package db

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

// ---- the invariant: the store's own writes never contend ----

// TestConcurrentWriteBatchesAllSucceed pins what the gate exists for. Before
// it, two of the store's own connections both issued BEGIN IMMEDIATE and the
// loser waited out the whole 5s busy_timeout and was then REFUSED: nine
// `store.db.write-batch` errors reading "database is locked (5) (SQLITE_BUSY)"
// during one re-ingestion, each a batch handed back to its caller's retry.
func TestConcurrentWriteBatchesAllSucceed(t *testing.T) {
	tests := []struct {
		name    string
		writers int
	}{
		{name: "a pair", writers: 2},
		{name: "one per producer the store really serves", writers: 8},
		{name: "a re-ingestion storm", writers: 32},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, s := newStore(t)
			errs := make([]error, test.writers)
			start := make(chan struct{})
			var wg sync.WaitGroup

			// Act: every writer is released from the same barrier, so their
			// transactions overlap rather than queue politely by arrival.
			for i := 0; i < test.writers; i++ {
				wg.Add(1)
				go func(i int) {
					defer wg.Done()
					<-start
					entry := pageEntry(fmt.Sprintf("w%d", i), fmt.Sprintf("u%d", i), "agent-1",
						frameItem(activityFrame("agent-1", fmt.Sprintf("act-%d", i), prose())))
					_, errs[i] = d.WriteBatch(ctx(), "test-producer", batch(entry), nil)
				}(i)
			}
			close(start)
			wg.Wait()

			// Assert
			for i, err := range errs {
				if err != nil {
					t.Fatalf("writer %d: WriteBatch = %v, want success", i, err)
				}
			}
			if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != test.writers {
				t.Fatalf("entry rows = %d, want %d", got, test.writers)
			}
			assertNoBusyRefusal(t, s)
		})
	}
}

// assertNoBusyRefusal fails if any record blames SQLite's busy arbitration.
// The gate's promise is not "the writes succeeded" but "no write ever saw a
// BUSY caused by a sibling", and a retry that quietly succeeded on a second
// attempt would satisfy the first without satisfying the second.
func assertNoBusyRefusal(t *testing.T, s *sink) {
	t.Helper()
	for _, record := range s.records(t) {
		message, _ := record["message"].(string)
		cause, _ := record["context"].(map[string]any)
		var causeText string
		if cause != nil {
			causeText, _ = cause["error_cause"].(string)
		}
		for _, text := range []string{message, causeText} {
			if strings.Contains(text, "database is locked") || strings.Contains(text, "SQLITE_BUSY") {
				t.Fatalf("a write met SQLite's busy arbitration, which the in-process gate must make impossible: %v\nlog was:\n%s", record, s.file.String())
			}
		}
	}
}

// ---- what a queued writer is told about its wait ----

// TestQueuedWriteBatchReportsItsQueueWaitAndStillSucceeds asserts the wait is
// MEASURED AND REPORTED rather than burned inside a busy handler. The clock is
// advanced from the queue hook, so the number is exact and nothing sleeps.
func TestQueuedWriteBatchReportsItsQueueWaitAndStillSucceeds(t *testing.T) {
	tests := []struct {
		name string
		wait time.Duration
	}{
		{name: "a brief turn behind a sibling", wait: 250 * time.Millisecond},
		{name: "longer than the busy_timeout that used to refuse it", wait: 7 * time.Second},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: a store that reports every statement, and a clock the
			// test moves by hand.
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, s := newReportingStore(t, clock)
			queued := make(chan struct{})
			var once sync.Once
			d.queuedForWrite = func() {
				once.Do(func() {
					clock.advance(test.wait)
					close(queued)
				})
			}

			// Act: hold the slot, let a second writer queue on it, then let go.
			release, err := d.acquireWrite(ctx())
			if err != nil {
				t.Fatalf("acquireWrite: %v", err)
			}
			done := make(chan error, 1)
			go func() {
				entry := pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-2", prose())))
				_, err := d.WriteBatch(ctx(), "test-producer", batch(entry), nil)
				done <- err
			}()
			<-queued
			release()
			if err := <-done; err != nil {
				t.Fatalf("the queued WriteBatch = %v, want success once its turn came", err)
			}

			// Assert
			wait, duration := slowQueryTiming(t, s, StatementWriteBatch)
			if wait != float64(test.wait.Milliseconds()) {
				t.Fatalf("lock_wait_ms = %v, want the %v the writer spent queued", wait, test.wait)
			}
			if duration < wait {
				t.Fatalf("duration_ms = %v is below lock_wait_ms = %v; the wait is a COMPONENT of the duration", duration, wait)
			}
		})
	}
}

// ---- a caller that hangs up while queued ----

// TestQueuedWriteBatchAnswersTheCallersCancellation pins the class boundary a
// queue creates: a wait that ends in the caller going away is NOT a storage
// failure, because the database was never touched and a retry would be a batch
// nobody is waiting for.
func TestQueuedWriteBatchAnswersTheCallersCancellation(t *testing.T) {
	tests := []struct {
		name string
		// cancelWhileQueued says whether the caller is cut off after it has
		// already blocked on the slot, or before it ever asked for one.
		cancelWhileQueued bool
	}{
		{name: "canceled while queued behind a sibling", cancelWhileQueued: true},
		{name: "canceled before the call was ever made", cancelWhileQueued: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)
			callCtx, cancel := context.WithCancel(context.Background())
			defer cancel()
			entry := pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-2", prose())))

			// Act
			var err error
			if test.cancelWhileQueued {
				queued := make(chan struct{})
				var once sync.Once
				d.queuedForWrite = func() { once.Do(func() { close(queued) }) }
				release, acquireErr := d.acquireWrite(ctx())
				if acquireErr != nil {
					t.Fatalf("acquireWrite: %v", acquireErr)
				}
				defer release()
				done := make(chan error, 1)
				go func() {
					_, err := d.WriteBatch(callCtx, "test-producer", batch(entry), nil)
					done <- err
				}()
				<-queued
				cancel()
				err = <-done
			} else {
				cancel()
				_, err = d.WriteBatch(callCtx, "test-producer", batch(entry), nil)
			}

			// Assert
			if !errors.Is(err, context.Canceled) {
				t.Fatalf("WriteBatch = %v, want the caller's own context.Canceled", err)
			}
			if errors.Is(err, ErrStorage) {
				t.Fatalf("WriteBatch = %v, want a cancellation rather than a storage failure the producer would retry", err)
			}
			if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != 0 {
				t.Fatalf("entry rows = %d, want 0: an abandoned batch writes nothing", got)
			}
		})
	}
}

// ---- the slot is always given back ----

// TestTheWriteSlotIsReleasedByEveryOutcome is the deadlock guard. A batch that
// fails INSIDE its transaction rolls back, and if it kept the slot on the way
// out the store would never write again.
func TestTheWriteSlotIsReleasedByEveryOutcome(t *testing.T) {
	tests := []struct {
		name string
		// firstFails says whether the batch ahead of the probe was refused
		// inside its own transaction rather than committed.
		firstFails bool
	}{
		{name: "after a committed batch", firstFails: false},
		{name: "after a batch refused inside its transaction", firstFails: true},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: an identity-changing upsert is refused by applyIdentityPolicy,
			// which runs INSIDE the transaction, so it exercises the rollback exit.
			d, _ := newStore(t)
			writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))
			if test.firstFails {
				bad := pageEntry("w2", "u1", "agent-1", frameItem(activityFrame("", "act-2", prose())))
				if _, err := d.WriteBatch(ctx(), "test-producer", batch(bad), nil); err == nil {
					t.Fatal("the arranged batch was expected to be refused")
				}
			}

			// Act: the probe can only begin if the slot came back.
			release, err := d.acquireWrite(ctx())

			// Assert
			if err != nil {
				t.Fatalf("acquireWrite after the preceding batch = %v, want the slot back", err)
			}
			release()
		})
	}
}

// ---- harness ----

// fakeClock is a monotonic clock a test moves by hand, so a duration is
// asserted exactly rather than waited out.
type fakeClock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *fakeClock) advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = c.now.Add(d)
}

// newReportingStore opens a store that reports every completed statement, so a
// timing record is reachable without contriving a slow one.
func newReportingStore(t *testing.T, clock *fakeClock) (*DB, *sink) {
	t.Helper()
	s, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")
	d, err := OpenWithOptions(path, log, Options{
		Now:        func() int64 { return testNow },
		Clock:      clock.Now,
		SlowQuery:  time.Nanosecond,
		BulkBase:   time.Nanosecond,
		BulkPerRow: time.Nanosecond,
	})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown
	return d, s
}
