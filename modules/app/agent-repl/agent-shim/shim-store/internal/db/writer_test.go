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
					_, errs[i] = d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(entry), nil)
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
			d.queuedForWrite = func(WriteClass) {
				once.Do(func() {
					clock.advance(test.wait)
					close(queued)
				})
			}

			// Act: hold the slot, let a second writer queue on it, then let go.
			release, err := d.acquireWrite(ctx(), WriteInteractive)
			if err != nil {
				t.Fatalf("acquireWrite: %v", err)
			}
			done := make(chan error, 1)
			go func() {
				entry := pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-2", prose())))
				_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(entry), nil)
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
				d.queuedForWrite = func(WriteClass) { once.Do(func() { close(queued) }) }
				release, acquireErr := d.acquireWrite(ctx(), WriteInteractive)
				if acquireErr != nil {
					t.Fatalf("acquireWrite: %v", acquireErr)
				}
				defer release()
				done := make(chan error, 1)
				go func() {
					_, err := d.WriteBatch(callCtx, "test-producer", WriteInteractive, batch(entry), nil)
					done <- err
				}()
				<-queued
				cancel()
				err = <-done
			} else {
				cancel()
				_, err = d.WriteBatch(callCtx, "test-producer", WriteInteractive, batch(entry), nil)
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
				if _, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(bad), nil); err == nil {
					t.Fatal("the arranged batch was expected to be refused")
				}
			}

			// Act: the probe can only begin if the slot came back.
			release, err := d.acquireWrite(ctx(), WriteInteractive)

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

// ---- the two-tier queue ----

// grantOrder holds the write slot, queues one writer per arrival IN THAT ORDER
// (each is confirmed queued before the next arrives), then lets go and returns
// the order the slot was granted in. Every grantee records itself BEFORE it
// releases, and the next grant happens inside that release, so the recorded
// order is the grant order exactly.
func grantOrder(t *testing.T, d *DB, arrivals []WriteClass) []string {
	t.Helper()
	queued := make(chan struct{})
	d.queuedForWrite = func(WriteClass) { queued <- struct{}{} }
	release, err := d.acquireWrite(ctx(), WriteInteractive)
	if err != nil {
		t.Fatalf("acquireWrite: %v", err)
	}
	order := make(chan string, len(arrivals))
	errs := make(chan error, len(arrivals))
	counts := map[WriteClass]int{}
	for _, class := range arrivals {
		counts[class]++
		name := fmt.Sprintf("%s%d", map[WriteClass]string{WriteInteractive: "I", WriteBulk: "B"}[class], counts[class])
		go func(class WriteClass, name string) {
			granted, err := d.acquireWrite(ctx(), class)
			if err != nil {
				errs <- err
				return
			}
			order <- name
			granted()
			errs <- nil
		}(class, name)
		<-queued
	}
	release()
	for range arrivals {
		if err := <-errs; err != nil {
			t.Fatalf("queued acquireWrite: %v", err)
		}
	}
	close(order)
	var got []string
	for name := range order {
		got = append(got, name)
	}
	return got
}

func interactiveRun(from, to int) []string {
	var out []string
	for i := from; i <= to; i++ {
		out = append(out, fmt.Sprintf("I%d", i))
	}
	return out
}

func repeatClass(class WriteClass, n int) []WriteClass {
	out := make([]WriteClass, n)
	for i := range out {
		out[i] = class
	}
	return out
}

// TestAQueuedInteractiveWriterIsTakenBeforeAQueuedBulkOne is the owner's rule
// at the scheduler: however long a bulk writer has been waiting, a released
// slot goes to a waiting interactive writer first.
func TestAQueuedInteractiveWriterIsTakenBeforeAQueuedBulkOne(t *testing.T) {
	tests := []struct {
		name     string
		arrivals []WriteClass
		want     []string
	}{
		{name: "bulk alone is taken when nothing interactive waits", arrivals: []WriteClass{WriteBulk}, want: []string{"B1"}},
		{name: "an interactive writer that arrived after a bulk one goes first",
			arrivals: []WriteClass{WriteBulk, WriteInteractive}, want: []string{"I1", "B1"}},
		{name: "every waiting interactive writer goes before two waiting bulk ones",
			arrivals: []WriteClass{WriteBulk, WriteBulk, WriteInteractive, WriteInteractive},
			want:     []string{"I1", "I2", "B1", "B2"}},
		{name: "each class is first-come first-served within itself",
			arrivals: []WriteClass{WriteInteractive, WriteBulk, WriteInteractive, WriteBulk},
			want:     []string{"I1", "I2", "B1", "B2"}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			got := grantOrder(t, d, test.arrivals)

			// Assert
			if strings.Join(got, ",") != strings.Join(test.want, ",") {
				t.Fatalf("grant order = %v, want %v", got, test.want)
			}
		})
	}
}

// TestBulkIsTakenAfterABurstOfInteractiveGrants is the fairness rule: once a
// bulk writer is waiting, it is granted the slot after at most
// InteractiveBurstBeforeBulk consecutive interactive grants, so a steady stream
// of interactive writes cannot starve bulk forever.
func TestBulkIsTakenAfterABurstOfInteractiveGrants(t *testing.T) {
	n := InteractiveBurstBeforeBulk
	tests := []struct {
		name     string
		arrivals []WriteClass
		want     []string
	}{
		{name: "a burst no longer than the rule never yields to bulk",
			arrivals: append([]WriteClass{WriteBulk}, repeatClass(WriteInteractive, n)...),
			want:     append(interactiveRun(1, n), "B1")},
		{name: "one interactive grant past the burst waits for bulk",
			arrivals: append([]WriteClass{WriteBulk}, repeatClass(WriteInteractive, n+2)...),
			want:     append(append(interactiveRun(1, n), "B1"), interactiveRun(n+1, n+2)...)},
		{name: "the burst restarts after each bulk grant",
			arrivals: append([]WriteClass{WriteBulk, WriteBulk}, repeatClass(WriteInteractive, 2*n+1)...),
			want: append(append(append(append(interactiveRun(1, n), "B1"), interactiveRun(n+1, 2*n)...), "B2"),
				interactiveRun(2*n+1, 2*n+1)...)},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			got := grantOrder(t, d, test.arrivals)

			// Assert
			if strings.Join(got, ",") != strings.Join(test.want, ",") {
				t.Fatalf("grant order = %v, want %v", got, test.want)
			}
		})
	}
}

// TestAcquireWriteRefusesAnUnsetClass: the writer never queues a write whose
// class was not stated, and never picks one for it.
func TestAcquireWriteRefusesAnUnsetClass(t *testing.T) {
	tests := []struct {
		name  string
		class WriteClass
	}{
		{name: "the zero value", class: WriteClassUnset},
		{name: "a value no class names", class: WriteClass(99)},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			release, err := d.acquireWrite(ctx(), test.class)

			// Assert
			if release != nil || RefusalSite(err) != SiteWriteClassUnset {
				t.Fatalf("acquireWrite(%v) = (release set %t, %v), want a %s refusal", test.class, release != nil, err, SiteWriteClassUnset)
			}
			probe, err := d.acquireWrite(ctx(), WriteInteractive)
			if err != nil {
				t.Fatalf("the refused call took the slot: acquireWrite = %v", err)
			}
			probe()
		})
	}
}

// TestAWithdrawnBulkWriterEndsTheBurstCount: a bulk writer that gives up while
// queued leaves no bulk waiting, so the next bulk writer starts a fresh burst
// rather than inheriting the streak its predecessor accumulated.
func TestAWithdrawnBulkWriterEndsTheBurstCount(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	queued := make(chan struct{})
	d.queuedForWrite = func(WriteClass) { queued <- struct{}{} }
	release, err := d.acquireWrite(ctx(), WriteInteractive)
	if err != nil {
		t.Fatalf("acquireWrite: %v", err)
	}
	callCtx, cancel := context.WithCancel(ctx())
	gaveUp := make(chan error, 1)
	go func() {
		_, err := d.acquireWrite(callCtx, WriteBulk)
		gaveUp <- err
	}()
	<-queued
	// As if interactive grants had already been made while it waited.
	d.writes.mu.Lock()
	d.writes.streak = InteractiveBurstBeforeBulk - 1
	d.writes.mu.Unlock()

	// Act
	cancel()
	err = <-gaveUp

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("withdrawn bulk writer = %v, want context.Canceled", err)
	}
	d.writes.mu.Lock()
	streak, bulk := d.writes.streak, len(d.writes.bulk)
	d.writes.mu.Unlock()
	if streak != 0 || bulk != 0 {
		t.Fatalf("after the withdrawal streak=%d bulk queued=%d, want 0 and 0", streak, bulk)
	}
	release()
}
