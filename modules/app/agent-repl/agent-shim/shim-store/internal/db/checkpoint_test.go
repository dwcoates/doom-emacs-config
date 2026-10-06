package db

import (
	"context"
	"encoding/binary"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/shim-store/internal/logging"
)

// ---- harness ----

// fakeTimers is the checkpoint job's idle timer, fired by hand. Every timer
// the job arms is handed to the test in order, so a test knows the job has
// finished reacting to a write the moment the timer it armed arrives.
type fakeTimers struct{ made chan *fakeTimer }

type fakeTimer struct {
	after time.Duration
	c     chan time.Time
}

func (t *fakeTimer) C() <-chan time.Time { return t.c }
func (t *fakeTimer) Stop() bool          { return true }
func (t *fakeTimer) fire()               { t.c <- time.Time{} }

func newFakeTimers() *fakeTimers { return &fakeTimers{made: make(chan *fakeTimer, 64)} }

func (f *fakeTimers) new(after time.Duration) checkpointTimer {
	t := &fakeTimer{after: after, c: make(chan time.Time, 1)}
	f.made <- t
	return t
}

// checkpointRun is one checkpoint the job reported.
type checkpointRun struct {
	result CheckpointResult
	err    error
}

// startCheckpoints runs the job on d with hand-fired timers, reporting every
// checkpoint it runs, and stops it at cleanup.
func startCheckpoints(t *testing.T, d *DB, policy CheckpointPolicy) (*fakeTimers, <-chan checkpointRun) {
	t.Helper()
	timers := newFakeTimers()
	runs := make(chan checkpointRun, 64)
	d.newCheckpointTimer = timers.new
	d.checkpointDone = func(result CheckpointResult, err error) { runs <- checkpointRun{result, err} }
	jobCtx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() {
		defer close(done)
		d.RunCheckpoints(jobCtx, policy)
	}()
	t.Cleanup(func() {
		cancel()
		<-done
	})
	return timers, runs
}

// writeOne commits one interactive page line.
func writeOne(t *testing.T, d *DB, id string) {
	t.Helper()
	entry := pageEntry("w-"+id, "u-"+id, "agent-1", frameItem(activityFrame("agent-1", "act-"+id, prose())))
	if _, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(entry), nil); err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
}

// checkpointRecords returns every normal-verbosity checkpoint record.
func checkpointRecords(t *testing.T, s *sink) []map[string]any {
	t.Helper()
	var out []map[string]any
	for _, record := range s.records(t) {
		if record["operation"] == CheckpointOperation && record["verbosity"] != "verbose" {
			out = append(out, record)
		}
	}
	return out
}

// failOnce makes the next checkpoint fail and every later one run for real.
func failOnce(d *DB) {
	var once sync.Once
	d.runCheckpoint = func(c context.Context) (int, int64, int64, error) {
		var err error
		once.Do(func() { err = errors.New("disk I/O error (10)") })
		if err != nil {
			return 0, 0, 0, err
		}
		return d.passiveCheckpoint(c)
	}
}

// walIndexBytes is a WAL-index header as SQLite lays it out.
func walIndexBytes(version uint32, isInit byte, frames, backfilled uint32) []byte {
	buf := make([]byte, walIndexReadSize)
	for _, at := range []int{0, walIndexHeaderSize} {
		binary.NativeEndian.PutUint32(buf[at:], version)
		buf[at+walIndexIsInitOffset] = isInit
		binary.NativeEndian.PutUint32(buf[at+walIndexMxFrame:], frames)
	}
	binary.NativeEndian.PutUint32(buf[walIndexBackfill:], backfilled)
	return buf
}

func openBytes(t *testing.T, content []byte) *os.File {
	t.Helper()
	path := filepath.Join(t.TempDir(), "index-shm")
	if err := os.WriteFile(path, content, 0o600); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	f, err := os.Open(path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { f.Close() }) //nolint:errcheck // best-effort test teardown
	return f
}

// ---- the WAL-index reading ----

func TestReadWALIndexReadsTheFrameCountAndTheBackfillMark(t *testing.T) {
	// Arrange
	f := openBytes(t, walIndexBytes(walIndexVersion, 1, 1200, 700))

	// Act
	index, err := readWALIndex(f)

	// Assert
	if err != nil {
		t.Fatalf("readWALIndex: %v", err)
	}
	if index.frames != 1200 || index.backfilled != 700 || index.pending() != 500 {
		t.Fatalf("index = %+v pending=%d, want frames=1200 backfilled=700 pending=500", index, index.pending())
	}
}

func TestReadWALIndexReadsTheLogsSalt(t *testing.T) {
	// Arrange: both header copies carry the same salt, as SQLite writes them.
	content := walIndexBytes(walIndexVersion, 1, 10, 0)
	for _, at := range []int{0, walIndexHeaderSize} {
		binary.NativeEndian.PutUint32(content[at+walIndexSalt:], 0xdeadbeef)
		binary.NativeEndian.PutUint32(content[at+walIndexSalt+4:], 7)
	}
	f := openBytes(t, content)

	// Act
	index, err := readWALIndex(f)

	// Assert
	if err != nil {
		t.Fatalf("readWALIndex: %v", err)
	}
	if index.salt != [2]uint32{0xdeadbeef, 7} {
		t.Fatalf("salt = %v, want [0xdeadbeef 7]", index.salt)
	}
}

func TestReadWALIndexRefusesAHeaderThatCannotBeTrusted(t *testing.T) {
	torn := walIndexBytes(walIndexVersion, 1, 10, 0)
	binary.NativeEndian.PutUint32(torn[walIndexHeaderSize+walIndexMxFrame:], 11)
	tests := []struct {
		name    string
		content []byte
		want    string
	}{
		{"the two header copies disagree", torn, "two copies disagree"},
		{"an unknown format version", walIndexBytes(1, 1, 10, 0), "version 1"},
		{"a header SQLite has not initialized", walIndexBytes(walIndexVersion, 0, 10, 0), "not initialized"},
		{"a file shorter than the header", []byte{1, 2, 3}, "reading the WAL-index header"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			f := openBytes(t, test.content)

			// Act
			_, err := readWALIndex(f)

			// Assert
			if err == nil || !strings.Contains(err.Error(), test.want) {
				t.Fatalf("readWALIndex = %v, want an error mentioning %q", err, test.want)
			}
		})
	}
}

func TestAWritersReleaseHandsTheWALReadingToTheCheckpointJob(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)

	// Act
	writeOne(t, d, "a")

	// Assert
	index, seen, err := d.walReading()
	if err != nil || !seen || index.pending() <= 0 {
		t.Fatalf("walReading = %+v seen=%v err=%v, want a reading with frames waiting", index, seen, err)
	}
	select {
	case <-d.wal.kick:
	default:
		t.Fatal("the release did not wake the checkpoint job")
	}
}

func TestAFailedWALReadingIsLoggedAtErrorAndStillWakesTheJob(t *testing.T) {
	// Arrange: a descriptor that can no longer be read.
	d, s := newFileStore(t)
	broken := openBytes(t, walIndexBytes(walIndexVersion, 1, 1, 0))
	broken.Close() //nolint:errcheck // closed on purpose: every read now fails
	d.wal.shm = broken
	t.Cleanup(func() { d.wal.shm = nil })

	// Act
	d.observeWAL()

	// Assert
	s.assertLogged(t, "error", "reading the WAL-index header failed")
	if _, _, err := d.walReading(); err == nil {
		t.Fatal("walReading carried no error after a failed read")
	}
	select {
	case <-d.wal.kick:
	default:
		t.Fatal("a failed reading did not wake the checkpoint job")
	}
}

// ---- one checkpoint ----

func TestCheckpointCopiesEveryWaitingFrame(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	writeOne(t, d, "a")

	// Act
	result, err := d.Checkpoint(ctx(), TriggerGrowth)

	// Assert
	if err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
	if result.Skipped || result.WALFrames == 0 || result.Checkpointed != result.WALFrames {
		t.Fatalf("result = %+v, want every frame checkpointed", result)
	}
}

func TestCheckpointIsLoggedWithItsPagesDurationAndClass(t *testing.T) {
	// Arrange
	d, s := newFileStore(t)
	writeOne(t, d, "a")

	// Act
	result, err := d.Checkpoint(ctx(), TriggerIdle)

	// Assert
	if err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
	records := checkpointRecords(t, s)
	if len(records) != 1 {
		t.Fatalf("got %d checkpoint records, want 1; log was:\n%s", len(records), s.file.String())
	}
	record := records[0]
	context, _ := record["context"].(map[string]any)
	if record["level"] != "info" || context["write_class"] != "bulk" || context["statement"] != StatementWALCheckpoint {
		t.Fatalf("record = %v, want an info record naming the bulk class and the wal_checkpoint family", record)
	}
	if rows, _ := context["rows"].(float64); int64(rows) != result.Checkpointed || rows == 0 {
		t.Fatalf("record rows = %v, want the %d pages copied", context["rows"], result.Checkpointed)
	}
	for _, key := range []string{"duration_ms", "lock_wait_ms", "exec_ms"} {
		if _, ok := context[key]; !ok {
			t.Fatalf("record has no %s: %v", key, record)
		}
	}
	if message, _ := record["message"].(string); !strings.Contains(message, "trigger=idle") || !strings.Contains(message, "mode=PASSIVE") {
		t.Fatalf("message = %q, want the trigger and the mode named", message)
	}
}

func TestCheckpointSkipsWhenNoFrameIsWaiting(t *testing.T) {
	// Arrange
	d, s := newFileStore(t)
	writeOne(t, d, "a")
	if _, err := d.Checkpoint(ctx(), TriggerGrowth); err != nil {
		t.Fatalf("first Checkpoint: %v", err)
	}
	ran := false
	d.runCheckpoint = func(context.Context) (int, int64, int64, error) {
		ran = true
		return 0, 0, 0, nil
	}

	// Act
	result, err := d.Checkpoint(ctx(), TriggerIdle)

	// Assert
	if err != nil || !result.Skipped || ran {
		t.Fatalf("second Checkpoint = %+v err=%v ran=%v, want a skip that runs no checkpoint", result, err, ran)
	}
	if n := len(checkpointRecords(t, s)); n != 1 {
		t.Fatalf("got %d normal checkpoint records, want only the first checkpoint's", n)
	}
}

func TestCheckpointQueuesInTheBulkTierWhileAnInteractiveWriterHoldsTheWriter(t *testing.T) {
	// Arrange: an interactive writer holds the one writer.
	d, _ := newFileStore(t)
	writeOne(t, d, "a")
	queued := make(chan WriteClass, 1)
	d.queuedForWrite = func(class WriteClass) { queued <- class }
	ran := make(chan struct{}, 1)
	d.runCheckpoint = func(c context.Context) (int, int64, int64, error) {
		ran <- struct{}{}
		return d.passiveCheckpoint(c)
	}
	release, err := d.acquireWrite(ctx(), WriteInteractive)
	if err != nil {
		t.Fatalf("acquireWrite: %v", err)
	}

	// Act
	done := make(chan error, 1)
	go func() {
		_, err := d.Checkpoint(ctx(), TriggerGrowth)
		done <- err
	}()
	class := <-queued

	// Assert: it queued as bulk, and had not run when it did.
	if class != WriteBulk {
		t.Fatalf("the checkpoint queued as %v, want bulk", class)
	}
	select {
	case <-ran:
		t.Fatal("the checkpoint ran while an interactive writer held the writer")
	default:
	}
	release()
	if err := <-done; err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
}

func TestAQueuedInteractiveWriterIsHandedTheWriterBeforeAQueuedCheckpoint(t *testing.T) {
	// Arrange: hold the writer, queue a checkpoint, then queue an interactive
	// write behind it.
	d, _ := newFileStore(t)
	writeOne(t, d, "a")
	queued := make(chan WriteClass, 2)
	d.queuedForWrite = func(class WriteClass) { queued <- class }
	var mu sync.Mutex
	var order []string
	d.runCheckpoint = func(c context.Context) (int, int64, int64, error) {
		mu.Lock()
		order = append(order, "checkpoint")
		mu.Unlock()
		return d.passiveCheckpoint(c)
	}
	d.transactionCommitted = func(class WriteClass) {
		mu.Lock()
		order = append(order, class.String())
		mu.Unlock()
	}
	release, err := d.acquireWrite(ctx(), WriteInteractive)
	if err != nil {
		t.Fatalf("acquireWrite: %v", err)
	}
	checkpointed := make(chan error, 1)
	go func() {
		_, err := d.Checkpoint(ctx(), TriggerGrowth)
		checkpointed <- err
	}()
	<-queued
	wrote := make(chan error, 1)
	go func() {
		entry := pageEntry("w-b", "u-b", "agent-1", frameItem(activityFrame("agent-1", "act-b", prose())))
		_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(entry), nil)
		wrote <- err
	}()
	<-queued

	// Act
	release()
	if err := <-wrote; err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	if err := <-checkpointed; err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}

	// Assert
	mu.Lock()
	defer mu.Unlock()
	if len(order) != 2 || order[0] != "interactive" || order[1] != "checkpoint" {
		t.Fatalf("the writer was handed out in order %v, want [interactive checkpoint]", order)
	}
}

func TestAFailedCheckpointIsLoggedAtErrorAndReturned(t *testing.T) {
	// Arrange
	d, s := newFileStore(t)
	writeOne(t, d, "a")
	failOnce(d)

	// Act
	_, err := d.Checkpoint(ctx(), TriggerGrowth)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("Checkpoint = %v, want a storage failure", err)
	}
	records := checkpointRecords(t, s)
	if len(records) != 1 || records[0]["level"] != "error" {
		t.Fatalf("records = %v, want exactly one error record", records)
	}
	context, _ := records[0]["context"].(map[string]any)
	if cause, _ := context["error"].(string); !strings.Contains(cause, "disk I/O error") {
		t.Fatalf("context error = %q, want the checkpoint's own failure", cause)
	}
}

func TestCheckpointFailsLoudlyWhenTheWALIndexCannotBeRead(t *testing.T) {
	// Arrange
	d, s := newFileStore(t)
	broken := openBytes(t, walIndexBytes(walIndexVersion, 1, 1, 0))
	broken.Close() //nolint:errcheck // closed on purpose: every read now fails
	d.wal.shm = broken
	t.Cleanup(func() { d.wal.shm = nil })

	// Act
	_, err := d.Checkpoint(ctx(), TriggerIdle)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("Checkpoint = %v, want a storage failure", err)
	}
	s.assertLogged(t, "error", "reading the WAL-index for the idle-triggered checkpoint")
}

func TestACheckpointThatCopiedNothingIsNarratedRatherThanRecorded(t *testing.T) {
	// Arrange: a reader pins every waiting frame, so the pass copies none.
	d, s := newFileStore(t)
	writeOne(t, d, "a")
	d.runCheckpoint = func(context.Context) (int, int64, int64, error) { return 0, 5, 0, nil }

	// Act
	result, err := d.Checkpoint(ctx(), TriggerGrowth)

	// Assert
	if err != nil || result.Checkpointed != 0 {
		t.Fatalf("Checkpoint = %+v err=%v, want a pass that copied nothing", result, err)
	}
	if records := checkpointRecords(t, s); len(records) != 0 {
		t.Fatalf("a pass that copied nothing wrote normal records: %v", records)
	}
	narrated := false
	for _, record := range s.records(t) {
		if record["operation"] == CheckpointOperation && record["verbosity"] == "verbose" {
			narrated = true
		}
	}
	if !narrated {
		t.Fatalf("a pass that copied nothing left no verbose record; log was:\n%s", s.file.String())
	}
}

func TestACheckpointWhoseCallerHungUpIsRecordedAsAbandoned(t *testing.T) {
	// Arrange
	d, s := newFileStore(t)
	canceled, cancel := context.WithCancel(ctx())
	cancel()

	// Act
	_, err := d.Checkpoint(canceled, TriggerIdle)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Checkpoint = %v, want the caller's cancellation", err)
	}
	s.assertLogged(t, "info", "abandoned")
	if errorsLogged := recordsAtLevel(t, s, "error"); len(errorsLogged) != 0 {
		t.Fatalf("a hung-up caller logged errors: %v", errorsLogged)
	}
}

// ---- the job's triggers ----

func TestTheJobCheckpointsOnceTheWALGrowsPastThePolicy(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	_, runs := startCheckpoints(t, d, CheckpointPolicy{Pages: 1})

	// Act
	writeOne(t, d, "a")

	// Assert
	run := <-runs
	if run.err != nil || run.result.Trigger != TriggerGrowth || run.result.Checkpointed == 0 {
		t.Fatalf("run = %+v, want a growth checkpoint that copied frames", run)
	}
}

func TestTheJobWaitsBelowTheGrowthThresholdAndArmsTheIdleTrigger(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	timers, runs := startCheckpoints(t, d, CheckpointPolicy{Pages: 1 << 30, Idle: 3 * time.Second})
	<-timers.made // armed at start

	// Act
	writeOne(t, d, "a")

	// Assert: the write re-armed the idle timer at the policy's interval, and
	// ran nothing.
	armed := <-timers.made
	if armed.after != 3*time.Second {
		t.Fatalf("idle timer armed for %v, want the policy's 3s", armed.after)
	}
	select {
	case run := <-runs:
		t.Fatalf("a checkpoint ran below the growth threshold: %+v", run)
	default:
	}
}

func TestTheJobCheckpointsWhenTheWriterGoesIdle(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	timers, runs := startCheckpoints(t, d, CheckpointPolicy{Pages: 1 << 30})
	<-timers.made
	writeOne(t, d, "a")
	idle := <-timers.made

	// Act
	idle.fire()

	// Assert
	run := <-runs
	if run.err != nil || run.result.Trigger != TriggerIdle || run.result.Checkpointed == 0 {
		t.Fatalf("run = %+v, want an idle checkpoint that copied frames", run)
	}
}

func TestTheJobRetriesAFailedGrowthCheckpointAtTheNextWrite(t *testing.T) {
	// Arrange
	d, s := newFileStore(t)
	failOnce(d)
	_, runs := startCheckpoints(t, d, CheckpointPolicy{Pages: 1})
	writeOne(t, d, "a")
	if first := <-runs; first.err == nil {
		t.Fatalf("first run = %+v, want the injected failure", first)
	}

	// Act
	writeOne(t, d, "b")

	// Assert
	retry := <-runs
	if retry.err != nil || retry.result.Trigger != TriggerGrowth || retry.result.Checkpointed == 0 {
		t.Fatalf("retry = %+v, want the growth checkpoint to succeed", retry)
	}
	s.assertLogged(t, "error", "running the growth-triggered checkpoint")
}

func TestTheJobRetriesAFailedIdleCheckpointAtTheNextIdle(t *testing.T) {
	// Arrange
	d, s := newFileStore(t)
	failOnce(d)
	timers, runs := startCheckpoints(t, d, CheckpointPolicy{Pages: 1 << 30})
	<-timers.made
	writeOne(t, d, "a")
	(<-timers.made).fire()
	if first := <-runs; first.err == nil {
		t.Fatalf("first run = %+v, want the injected failure", first)
	}

	// Act: the failure re-armed the idle trigger.
	(<-timers.made).fire()

	// Assert
	retry := <-runs
	if retry.err != nil || retry.result.Trigger != TriggerIdle || retry.result.Checkpointed == 0 {
		t.Fatalf("retry = %+v, want the idle checkpoint to succeed", retry)
	}
	s.assertLogged(t, "error", "running the idle-triggered checkpoint")
}

func TestTheJobArmsTheIdleTriggerWhenTheWALCannotBeRead(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	timers, runs := startCheckpoints(t, d, CheckpointPolicy{Pages: 1})
	<-timers.made
	broken := openBytes(t, walIndexBytes(walIndexVersion, 1, 1, 0))
	broken.Close() //nolint:errcheck // closed on purpose: every read now fails
	d.wal.shm = broken
	t.Cleanup(func() { d.wal.shm = nil })

	// Act
	writeOne(t, d, "a")

	// Assert: growth cannot be told, so the idle trigger is what retries.
	<-timers.made
	select {
	case run := <-runs:
		t.Fatalf("a checkpoint ran on a reading that failed: %+v", run)
	default:
	}
}

func TestTheJobRearmsTheIdleTriggerWhenAReaderCutACheckpointShort(t *testing.T) {
	// Arrange: the pass leaves half the frames behind a reader.
	d, _ := newFileStore(t)
	d.runCheckpoint = func(context.Context) (int, int64, int64, error) { return 0, 10, 5, nil }
	timers, runs := startCheckpoints(t, d, CheckpointPolicy{Pages: 1})
	<-timers.made

	// Act
	writeOne(t, d, "a")

	// Assert
	if run := <-runs; run.err != nil || run.result.Checkpointed != 5 {
		t.Fatalf("run = %+v, want the short pass", run)
	}
	<-timers.made
}

func TestTheRealIdleTimerIsATimeTimer(t *testing.T) {
	// Arrange
	timer := newRealTimer(time.Hour)

	// Act
	stopped := timer.Stop()

	// Assert
	if !stopped || timer.C() == nil {
		t.Fatalf("Stop = %v C = %v, want a live timer stopped before it fired", stopped, timer.C())
	}
}

func TestTheJobStopsWhenItsContextEnds(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	d.newCheckpointTimer = newFakeTimers().new
	jobCtx, cancel := context.WithCancel(ctx())
	done := make(chan struct{})
	go func() {
		defer close(done)
		d.RunCheckpoints(jobCtx, CheckpointPolicy{})
	}()

	// Act
	cancel()

	// Assert
	<-done
}

func TestCheckpointPolicyTakesTheDefaultsForZeroValues(t *testing.T) {
	// Arrange
	var policy CheckpointPolicy

	// Act
	resolved := policy.resolve()

	// Assert
	if resolved.Pages != DefaultCheckpointPages || resolved.Idle != DefaultCheckpointIdle {
		t.Fatalf("resolved = %+v, want the shipped defaults", resolved)
	}
}

// ---- the WAL pin ----

func TestReadWALIndexReadsEveryReaderMark(t *testing.T) {
	// Arrange
	content := walIndexBytes(walIndexVersion, 1, 1200, 700)
	for i, mark := range []uint32{0, 47481, 900, 0xffffffff, 12} {
		binary.NativeEndian.PutUint32(content[walIndexReadMark+4*i:], mark)
	}
	f := openBytes(t, content)

	// Act
	index, err := readWALIndex(f)

	// Assert
	if err != nil {
		t.Fatalf("readWALIndex: %v", err)
	}
	if index.readMarks != [walReadMarks]uint32{0, 47481, 900, 0xffffffff, 12} {
		t.Fatalf("readMarks = %v", index.readMarks)
	}
}

// holdReadSnapshot opens a read transaction and reads through it, so its
// snapshot is held until the returned function ends it.
func holdReadSnapshot(t *testing.T, d *DB) func() {
	t.Helper()
	tx, err := d.beginRead(ctx())
	if err != nil {
		t.Fatalf("beginRead: %v", err)
	}
	var n int
	if err := tx.QueryRowContext(ctx(), `SELECT COUNT(*) FROM entry`).Scan(&n); err != nil {
		t.Fatalf("SELECT: %v", err)
	}
	var once sync.Once
	end := func() { once.Do(func() { d.endTx(tx, logging.Fields{Operation: "store.db.test"}) }) }
	t.Cleanup(end)
	return end
}

// pinnedStore is a store whose WAL holds frames a reader's snapshot pins: the
// WAL was folded, a reader took its snapshot, and one more write followed.
func pinnedStore(t *testing.T, clock *fakeClock) (*DB, *sink, func()) {
	t.Helper()
	s, log := newSink(t)
	d, err := OpenWithOptions(filepath.Join(t.TempDir(), "store.db"), log, Options{Now: func() int64 { return testNow }, Clock: clock.Now, unsynced: true})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown
	writeOne(t, d, "a")
	if _, err := d.Checkpoint(ctx(), TriggerIdle); err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
	end := holdReadSnapshot(t, d)
	writeOne(t, d, "b")
	return d, s, end
}

func TestACheckpointAReaderPinsCopiesNothingAndReportsTheReadMarks(t *testing.T) {
	// Arrange
	d, _, _ := pinnedStore(t, &fakeClock{})

	// Act
	result, err := d.Checkpoint(ctx(), TriggerIdle)

	// Assert
	if err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
	if result.Copied != 0 || result.Checkpointed >= result.WALFrames {
		t.Fatalf("result = %+v, want nothing copied with frames waiting", result)
	}
	if len(result.ReadMarks) != walReadMarks {
		t.Fatalf("ReadMarks = %v, want all %d reader slots", result.ReadMarks, walReadMarks)
	}
}

func TestACheckpointThatCopiesReportsWhatItCopied(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	writeOne(t, d, "a")

	// Act
	result, err := d.Checkpoint(ctx(), TriggerIdle)

	// Assert
	if err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
	if result.Copied == 0 || result.Copied != result.Checkpointed || result.ReadMarks != nil {
		t.Fatalf("result = %+v, want every frame copied and no read marks taken", result)
	}
}

func TestTheWALPinWatch(t *testing.T) {
	pinned := CheckpointResult{WALFrames: 100, Checkpointed: 40, Copied: 0}
	copied := CheckpointResult{WALFrames: 100, Checkpointed: 100, Copied: 60}
	shortPass := CheckpointResult{WALFrames: 100, Checkpointed: 70, Copied: 30}
	skipped := CheckpointResult{WALFrames: 100, Checkpointed: 100, Skipped: true}
	start := time.Unix(1_700_000_000, 0)
	type step struct {
		result CheckpointResult
		at     time.Duration
		want   walPinEvent
		held   time.Duration
	}
	tests := []struct {
		name  string
		steps []step
	}{
		{"a checkpoint that copies is quiet", []step{{copied, 0, walPinQuiet, 0}}},
		{"the first pinned checkpoint opens a pin silently", []step{{pinned, 0, walPinQuiet, 0}}},
		{"a pin is reported once it outlasts the policy", []step{
			{pinned, 0, walPinQuiet, 0}, {pinned, time.Minute, walPinOutlasted, time.Minute}}},
		{"a pin short of the policy is not reported", []step{
			{pinned, 0, walPinQuiet, 0}, {pinned, time.Minute - time.Second, walPinQuiet, time.Minute - time.Second}}},
		{"a reported pin is reported only once", []step{
			{pinned, 0, walPinQuiet, 0}, {pinned, time.Minute, walPinOutlasted, time.Minute}, {pinned, 2 * time.Minute, walPinQuiet, 2 * time.Minute}}},
		{"a reported pin that copies again is released", []step{
			{pinned, 0, walPinQuiet, 0}, {pinned, time.Minute, walPinOutlasted, time.Minute}, {copied, 90 * time.Second, walPinReleased, 90 * time.Second}}},
		{"a partial copy ends the pin", []step{
			{pinned, 0, walPinQuiet, 0}, {pinned, time.Minute, walPinOutlasted, time.Minute}, {shortPass, 2 * time.Minute, walPinReleased, 2 * time.Minute}}},
		{"nothing left to copy ends the pin", []step{
			{pinned, 0, walPinQuiet, 0}, {pinned, time.Minute, walPinOutlasted, time.Minute}, {skipped, 2 * time.Minute, walPinReleased, 2 * time.Minute}}},
		{"an unreported pin ends silently", []step{
			{pinned, 0, walPinQuiet, 0}, {copied, time.Second, walPinQuiet, time.Second}}},
		{"a released pin starts over", []step{
			{pinned, 0, walPinQuiet, 0}, {copied, time.Second, walPinQuiet, time.Second},
			{pinned, time.Hour, walPinQuiet, 0}, {pinned, time.Hour + time.Minute, walPinOutlasted, time.Minute}}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			var watch walPinWatch
			for i, s := range test.steps {
				// Act
				event, held := watch.observe(s.result, start.Add(s.at), time.Minute)

				// Assert
				if event != s.want || held != s.held {
					t.Fatalf("step %d: observe = (%v, %v), want (%v, %v)", i, event, held, s.want, s.held)
				}
			}
		})
	}
}

func walPinRecords(t *testing.T, s *sink) []map[string]any {
	t.Helper()
	var out []map[string]any
	for _, record := range s.records(t) {
		if record["operation"] == WALPinOperation {
			out = append(out, record)
		}
	}
	return out
}

// runPinnedJob starts the job on a pinned store and runs its first idle
// checkpoint, which opens the pin. The store's pending write kicked the job
// before it started, so the job arms twice: at start, then for that kick.
func runPinnedJob(t *testing.T, clock *fakeClock) (*DB, *sink, func(), *fakeTimers, <-chan checkpointRun) {
	t.Helper()
	d, s, end := pinnedStore(t, clock)
	timers, runs := startCheckpoints(t, d, CheckpointPolicy{Pages: 1 << 30, PinWarnAfter: time.Minute})
	<-timers.made
	(<-timers.made).fire()
	if run := <-runs; run.err != nil || run.result.Copied != 0 {
		t.Fatalf("first run = %+v, want a pinned checkpoint", run)
	}
	return d, s, end, timers, runs
}

func TestTheJobWarnsOnceAPinOutlastsThePolicy(t *testing.T) {
	// Arrange
	clock := &fakeClock{}
	_, s, _, timers, runs := runPinnedJob(t, clock)
	clock.advance(time.Minute)

	// Act
	(<-timers.made).fire()
	<-runs

	// Assert
	records := walPinRecords(t, s)
	if len(records) != 1 || records[0]["level"] != "warn" {
		t.Fatalf("pin records = %v, want one warning; log was:\n%s", records, s.file.String())
	}
	s.assertLogged(t, "warn", "a reader has pinned the WAL for at least 1m0s")
}

func TestTheJobsPinWarningCarriesTheWALAndReadPoolState(t *testing.T) {
	// Arrange
	clock := &fakeClock{}
	_, s, _, timers, runs := runPinnedJob(t, clock)
	clock.advance(time.Minute)

	// Act
	(<-timers.made).fire()
	<-runs

	// Assert
	records := walPinRecords(t, s)
	if len(records) != 1 {
		t.Fatalf("pin records = %v, want one", records)
	}
	context, _ := records[0]["context"].(map[string]any)
	for _, key := range []string{"wal_frames", "wal_backfilled", "wal_read_marks", "read_pool_open", "read_pool_in_use", "read_pool_idle", "db"} {
		if _, ok := context[key]; !ok {
			t.Fatalf("pin warning has no %s: %v", key, context)
		}
	}
	if context["wal_pinned_for_ms"] != float64(time.Minute.Milliseconds()) {
		t.Fatalf("wal_pinned_for_ms = %v, want %d", context["wal_pinned_for_ms"], time.Minute.Milliseconds())
	}
	if context["read_pool_in_use"] != float64(1) {
		t.Fatalf("read_pool_in_use = %v, want the one connection the held read occupies", context["read_pool_in_use"])
	}
}

func TestTheJobRecordsTheEndOfAReportedPin(t *testing.T) {
	// Arrange
	clock := &fakeClock{}
	_, s, end, timers, runs := runPinnedJob(t, clock)
	clock.advance(time.Minute)
	(<-timers.made).fire()
	<-runs
	end()
	clock.advance(time.Second)

	// Act
	(<-timers.made).fire()
	<-runs

	// Assert
	records := walPinRecords(t, s)
	if len(records) != 2 || records[1]["level"] != "info" {
		t.Fatalf("pin records = %v, want the warning then an info release; log was:\n%s", records, s.file.String())
	}
	s.assertLogged(t, "info", "the WAL pin ended after at least 1m1s")
}

func TestTheJobIsSilentAboutAPinThatEndsInsideThePolicy(t *testing.T) {
	// Arrange
	clock := &fakeClock{}
	_, s, end, timers, runs := runPinnedJob(t, clock)
	end()
	clock.advance(time.Second)

	// Act
	(<-timers.made).fire()
	<-runs

	// Assert
	if records := walPinRecords(t, s); len(records) != 0 {
		t.Fatalf("pin records = %v, want none for a pin shorter than the policy", records)
	}
}

func TestTheWALPinWatchTracksAPinOpenedAtTheClocksZeroInstant(t *testing.T) {
	// Arrange
	var watch walPinWatch
	pinned := CheckpointResult{WALFrames: 100, Checkpointed: 40}
	watch.observe(pinned, time.Time{}, time.Minute)

	// Act
	event, held := watch.observe(pinned, time.Time{}.Add(time.Minute), time.Minute)

	// Assert
	if event != walPinOutlasted || held != time.Minute {
		t.Fatalf("observe = (%v, %v), want the pin reported after 1m", event, held)
	}
}

// ---- the checkpoint never holds the writer ----

// stallCheckpoints makes every checkpoint stop inside its pass, after its
// first WAL-index reading, until the returned release is called. stalled
// receives once per checkpoint that reached the pass.
func stallCheckpoints(t *testing.T, d *DB) (stalled <-chan struct{}, release func()) {
	t.Helper()
	reached := make(chan struct{}, 1)
	hold := make(chan struct{})
	d.runCheckpoint = func(c context.Context) (int, int64, int64, error) {
		reached <- struct{}{}
		<-hold
		return d.passiveCheckpoint(c)
	}
	var once sync.Once
	release = func() { once.Do(func() { close(hold) }) }
	t.Cleanup(release)
	return reached, release
}

// TestAReadAndAWriteCompleteWhileACheckpointIsStalled is the structural
// assertion behind the checkpoint connection. Each operation runs
// SYNCHRONOUSLY while a checkpoint is held inside its pass: if the pass ever
// holds the writer again, a write does not fail slowly, it never returns, and
// the package timeout says so.
func TestAReadAndAWriteCompleteWhileACheckpointIsStalled(t *testing.T) {
	tests := []struct {
		name string
		op   func(t *testing.T, d *DB)
	}{
		{
			name: "the sidecar's cursor read",
			op: func(t *testing.T, d *DB) {
				if _, err := d.Cursors(ctx(), nil); err != nil {
					t.Fatalf("Cursors while a checkpoint was stalled: %v", err)
				}
			},
		},
		{
			name: "an interactive write",
			op: func(t *testing.T, d *DB) {
				entry := pageEntry("w-i", "u-i", "agent-1", frameItem(activityFrame("agent-1", "act-i", prose())))
				if _, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(entry), nil); err != nil {
					t.Fatalf("interactive WriteBatch while a checkpoint was stalled: %v", err)
				}
			},
		},
		{
			name: "a bulk write",
			op: func(t *testing.T, d *DB) {
				entry := pageEntry("w-b", "u-b", "agent-1", frameItem(activityFrame("agent-1", "act-b", prose())))
				if _, err := d.WriteBatch(ctx(), "test-producer", WriteBulk, batch(entry), nil); err != nil {
					t.Fatalf("bulk WriteBatch while a checkpoint was stalled: %v", err)
				}
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: a checkpoint stalled inside its pass.
			d, _ := newFileStore(t)
			writeOne(t, d, "a")
			stalled, release := stallCheckpoints(t, d)
			done := make(chan error, 1)
			go func() {
				_, err := d.Checkpoint(ctx(), TriggerIdle)
				done <- err
			}()
			<-stalled

			// Act
			test.op(t, d)

			// Assert: the checkpoint was still inside its pass throughout,
			// and finishes once let go.
			select {
			case err := <-done:
				t.Fatalf("the checkpoint returned (%v) before it was released", err)
			default:
			}
			release()
			if err := <-done; err != nil {
				t.Fatalf("Checkpoint: %v", err)
			}
		})
	}
}

// TestTheCheckpointConnectionCheckpointsBesideAnOpenWriteTransaction pins the
// SQLite behavior the design rests on: the real PASSIVE checkpoint, on the
// checkpoint connection, completes while a write transaction holds the WAL
// write lock with its own uncommitted change.
func TestTheCheckpointConnectionCheckpointsBesideAnOpenWriteTransaction(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	writeOne(t, d, "a")
	tx, release, err := d.beginWrite(ctx(), WriteInteractive)
	if err != nil {
		t.Fatalf("beginWrite: %v", err)
	}
	defer release()
	defer tx.Rollback() //nolint:errcheck // the fixture write is never committed
	if _, err := tx.ExecContext(ctx(), `DELETE FROM cursor`); err != nil {
		t.Fatalf("the fixture write did not take the write lock: %v", err)
	}

	// Act
	busy, frames, checkpointed, err := d.passiveCheckpoint(ctx())

	// Assert
	if err != nil {
		t.Fatalf("passiveCheckpoint beside an open write: %v", err)
	}
	if busy != 0 || frames == 0 || checkpointed != frames {
		t.Fatalf("passiveCheckpoint = busy=%d frames=%d checkpointed=%d, want every committed frame copied", busy, frames, checkpointed)
	}
}

// TestACheckpointWhoseLogRestartedBeforeItsResultWasReadCountsTheWholeLog
// reproduces the one race a checkpoint off the writer meets: its pass copies
// every frame, a producer's commit restarts the log, and the pass then reads
// the backfill mark of the NEW log.
func TestACheckpointWhoseLogRestartedBeforeItsResultWasReadCountsTheWholeLog(t *testing.T) {
	// Arrange
	d, s := newFileStore(t)
	writeOne(t, d, "a")
	d.runCheckpoint = func(c context.Context) (int, int64, int64, error) {
		busy, frames, _, err := d.passiveCheckpoint(c)
		if err != nil {
			return 0, 0, 0, err
		}
		writeOne(t, d, "restarts-the-log")
		// The new log's backfill mark, as the racing pass would read it.
		return busy, frames, 0, nil
	}

	// Act
	result, err := d.Checkpoint(ctx(), TriggerGrowth)

	// Assert
	if err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
	if result.WALFrames == 0 || result.Checkpointed != result.WALFrames || result.Copied != result.WALFrames {
		t.Fatalf("result = %+v, want the whole log counted as copied", result)
	}
	if records := checkpointRecords(t, s); len(records) != 1 || records[0]["level"] != "info" {
		t.Fatalf("records = %v, want the one info record of a checkpoint that copied", records)
	}
}

// TestTheSecondWALReadingNoticesARestart pins the premise of the test above:
// the commit after a whole-log checkpoint really does restart the log, which
// is what the salt says.
func TestTheSecondWALReadingNoticesARestart(t *testing.T) {
	// Arrange
	d, _ := newFileStore(t)
	writeOne(t, d, "a")
	before, _, err := d.readWALUnderWriter(ctx())
	if err != nil {
		t.Fatalf("first reading: %v", err)
	}
	if _, _, _, err := d.passiveCheckpoint(ctx()); err != nil {
		t.Fatalf("passiveCheckpoint: %v", err)
	}
	writeOne(t, d, "b")

	// Act
	after, _, err := d.readWALUnderWriter(ctx())

	// Assert
	if err != nil {
		t.Fatalf("second reading: %v", err)
	}
	if after.salt == before.salt {
		t.Fatalf("salt %v did not change across a restart", after.salt)
	}
}

func TestCheckpointFailsLoudlyWhenTheSecondWALReadingFails(t *testing.T) {
	// Arrange: the pass itself breaks the descriptor every reading uses.
	d, s := newFileStore(t)
	writeOne(t, d, "a")
	d.runCheckpoint = func(c context.Context) (int, int64, int64, error) {
		busy, frames, checkpointed, err := d.passiveCheckpoint(c)
		d.wal.shm.Close() //nolint:errcheck // closed on purpose: the next read fails
		return busy, frames, checkpointed, err
	}
	t.Cleanup(func() { d.wal.shm = nil })

	// Act
	_, err := d.Checkpoint(ctx(), TriggerGrowth)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("Checkpoint = %v, want a storage failure", err)
	}
	s.assertLogged(t, "error", "reading the WAL-index after the growth-triggered checkpoint")
}

func TestACheckpointWhoseCallerHungUpDuringThePassIsRecordedAsAbandoned(t *testing.T) {
	// Arrange: the caller hangs up while the pass runs, before the second
	// reading queues.
	d, s := newFileStore(t)
	writeOne(t, d, "a")
	hungUp, cancel := context.WithCancel(ctx())
	defer cancel()
	d.runCheckpoint = func(c context.Context) (int, int64, int64, error) {
		busy, frames, checkpointed, err := d.passiveCheckpoint(c)
		cancel()
		return busy, frames, checkpointed, err
	}

	// Act
	_, err := d.Checkpoint(hungUp, TriggerIdle)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Checkpoint = %v, want the caller's cancellation", err)
	}
	s.assertLogged(t, "info", "abandoned")
	if errorsLogged := recordsAtLevel(t, s, "error"); len(errorsLogged) != 0 {
		t.Fatalf("a hung-up caller logged errors: %v", errorsLogged)
	}
}

func TestAWALReadingTakesTheWriterInTheBulkTierAndReportsItsWait(t *testing.T) {
	// Arrange: an interactive writer holds the writer, and the clock advances
	// while the reading waits.
	clock := &fakeClock{}
	_, log := newSink(t)
	d, err := OpenWithOptions(filepath.Join(t.TempDir(), "store.db"), log, Options{Now: func() int64 { return testNow }, Clock: clock.Now, unsynced: true})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown
	writeOne(t, d, "a")
	queued := make(chan WriteClass, 1)
	d.queuedForWrite = func(class WriteClass) { queued <- class }
	release, err := d.acquireWrite(ctx(), WriteInteractive)
	if err != nil {
		t.Fatalf("acquireWrite: %v", err)
	}
	type reading struct {
		index  walIndex
		waited time.Duration
		err    error
	}
	done := make(chan reading, 1)
	go func() {
		index, waited, err := d.readWALUnderWriter(ctx())
		done <- reading{index, waited, err}
	}()
	class := <-queued

	// Act
	clock.advance(40 * time.Millisecond)
	release()
	got := <-done

	// Assert
	if class != WriteBulk {
		t.Fatalf("the reading queued as %v, want bulk", class)
	}
	if got.err != nil || got.index.frames == 0 {
		t.Fatalf("readWALUnderWriter = %+v, want a reading of the written WAL", got)
	}
	if got.waited != 40*time.Millisecond {
		t.Fatalf("waited = %v, want the 40ms it queued", got.waited)
	}
}
