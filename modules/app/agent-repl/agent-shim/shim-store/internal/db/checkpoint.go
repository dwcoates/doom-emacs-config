package db

import (
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"os"
	"time"

	"agentrepl/shim-store/internal/logging"
)

// CHECKPOINTS ARE BULK WORK, AND THEY NEVER RUN INSIDE ANYBODY'S COMMIT.
//
// SQLite's default is an AUTOCHECKPOINT: the commit that carries the WAL past
// 1000 pages runs the checkpoint itself, synchronously, before COMMIT returns.
// So whichever writer happened to cross the line paid for copying every page
// every other writer had appended — about 4 MB and an fsync — and on the
// owner's store that was routinely an INTERACTIVE commit paying for the
// sidecar's bulk ingestion. The write DSN therefore carries
// `wal_autocheckpoint(0)`, and the checkpoint is this file's scheduled job
// instead: it takes the one writer through the BULK tier (writer.go), so an
// interactive write is always handed the writer ahead of it and never waits on
// more than the one checkpoint already running.
//
// WHAT TRIGGERS IT, and neither trigger is a guessed timer.
//
//   - GROWTH. Every writer's release reads the WAL-index header (see
//     readWALIndex) while it still holds the writer, and hands the reading to
//     the job. Once CheckpointPolicy.Pages frames have been appended since the
//     last checkpoint, the job runs. The threshold is the same 1000 pages the
//     autocheckpoint used, so the WAL keeps the size it always had between
//     checkpoints; only who pays for the checkpoint changed.
//   - IDLE. Once no writer has released for CheckpointPolicy.Idle and frames
//     are still waiting to be copied, the job runs, so a quiet store folds its
//     WAL back rather than carrying a partial one until the next burst. It is
//     re-armed by every release, so a live turn never looks idle.
//
// WHY PASSIVE, ALWAYS. A PASSIVE checkpoint copies what it can and NEVER waits
// on a reader: FULL, RESTART and TRUNCATE all run the busy handler until every
// reader has left the WAL, and they would do it while holding the one writer —
// up to the DSN's 5s busy_timeout of every interactive write queued behind a
// page repaint's snapshot. PASSIVE's duration is therefore exactly the copy,
// and the copy is bounded by the growth trigger. Frames a reader still pins are
// left for the next run. Shrinking the -wal file is not the checkpoint's job:
// `journal_size_limit` truncates it the next time the log restarts.
//
// WHY NO DEADLINE. SQLite has no page limit on a checkpoint, and an
// interrupted one keeps NONE of its progress: the backfill mark is advanced
// only after the whole pass has been copied and synced. A deadline would
// therefore turn a slow checkpoint into one that is retried forever against a
// WAL that only grows, which is the 119 MB -wal this job exists to end. The
// bound SQLite allows is the one used: PASSIVE never waits on a lock, and the
// growth trigger bounds how much there is to copy.

// CheckpointOperation is the operation every checkpoint record carries.
const CheckpointOperation = "store.db.wal-checkpoint"

// StatementWALCheckpoint is the statement family a checkpoint is timed as.
const StatementWALCheckpoint = "wal_checkpoint"

// DefaultCheckpointPages is the growth trigger: how many WAL frames may be
// appended since the last checkpoint before the job runs. It is SQLite's own
// autocheckpoint default, so the WAL between checkpoints is as large as it
// always was (~4 MB of 4 KiB pages) and one checkpoint copies at most that
// much plus whatever was appended while it queued in the bulk tier.
const DefaultCheckpointPages = 1000

// DefaultCheckpointIdle is the idle trigger: how long the writer must go
// unreleased, with frames still waiting, before the job runs. A live turn
// writes a batch per frame, far more often than this, so a turn in progress is
// never mistaken for a quiet store; a finished one is folded back two seconds
// after its last write.
const DefaultCheckpointIdle = 2 * time.Second

// DefaultPinWarnAfter is how long checkpoints may go on copying nothing while
// frames wait before the store says a reader is pinning its WAL. A read holds
// its snapshot only for the statements of one call; the slowest read seen on
// the owner's store, with the machine at a load average of 151, took 1.4s. A
// snapshot still held a minute on is no read in progress: it is a leak, and
// until it ends the WAL grows without bound and every read over it slows.
const DefaultPinWarnAfter = time.Minute

// CheckpointTrigger names why a checkpoint ran.
type CheckpointTrigger string

const (
	// TriggerGrowth is a checkpoint run because the WAL grew past the policy.
	TriggerGrowth CheckpointTrigger = "growth"
	// TriggerIdle is a checkpoint run because the writer went quiet with
	// frames still waiting.
	TriggerIdle CheckpointTrigger = "idle"
)

// CheckpointPolicy is the job's two triggers, and how long a pin may last
// before it is reported. Zero values take the defaults.
type CheckpointPolicy struct {
	Pages        int64
	Idle         time.Duration
	PinWarnAfter time.Duration
}

func (p CheckpointPolicy) resolve() CheckpointPolicy {
	if p.Pages <= 0 {
		p.Pages = DefaultCheckpointPages
	}
	if p.Idle <= 0 {
		p.Idle = DefaultCheckpointIdle
	}
	if p.PinWarnAfter <= 0 {
		p.PinWarnAfter = DefaultPinWarnAfter
	}
	return p
}

// CheckpointResult is what one checkpoint did.
type CheckpointResult struct {
	Trigger CheckpointTrigger
	// WALFrames is how many frames the WAL held when the checkpoint ran, and
	// Checkpointed how many of them are now copied into the database. Fewer
	// checkpointed than held means a reader still pins the rest.
	WALFrames    int64
	Checkpointed int64
	// Copied is how many frames THIS run moved. Zero with frames still waiting
	// means a reader's snapshot pins every one of them.
	Copied int64
	// ReadMarks are the WAL-index's reader marks as this run left them, read
	// only when it copied nothing: they say which snapshot the pinning reader
	// holds (mark 0 is a reader of the database file alone, opened while the
	// WAL was fully checkpointed).
	ReadMarks []uint32
	// Skipped is true when there was nothing to copy, so no checkpoint ran.
	Skipped bool
}

// walIndex is the part of the WAL-index header the job reads: how many frames
// the log holds, how many of them a checkpoint has already copied, and the
// frame each reader slot's snapshot ends at.
type walIndex struct {
	frames     uint32
	backfilled uint32
	readMarks  [walReadMarks]uint32
}

// pending is how many frames are waiting to be checkpointed.
func (w walIndex) pending() int64 { return int64(w.frames) - int64(w.backfilled) }

// The WAL-index layout (https://www.sqlite.org/walformat.html, "The WAL-Index
// Header"): two copies of a 48-byte header, then the checkpoint info whose
// first word is nBackfill. Every field is in the host's byte order, because the
// file is shared memory. It is SQLite's documented on-disk format, fixed so
// that processes of different SQLite versions can share one WAL.
const (
	walIndexHeaderSize   = 48
	walReadMarks         = 5
	walIndexReadSize     = 2*walIndexHeaderSize + 4 + 4*walReadMarks
	walIndexVersion      = 3007000
	walIndexIsInitOffset = 12
	walIndexMxFrame      = 16
	walIndexBackfill     = 2 * walIndexHeaderSize
	walIndexReadMark     = walIndexBackfill + 4
)

// readWALIndex reads the WAL-index header from the -shm file.
//
// IT IS READ UNDER THE WRITER, and that is what makes it exact. SQLite updates
// the header when a write commits and when a checkpoint finishes, and both of
// those only ever happen here while the one writer is held; the two header
// copies are still compared, as SQLite's own readers do, so an outside writer
// (a `sqlite3` shell) mid-update is reported rather than misread.
func readWALIndex(f *os.File) (walIndex, error) {
	var buf [walIndexReadSize]byte
	if _, err := f.ReadAt(buf[:], 0); err != nil {
		return walIndex{}, fmt.Errorf("reading the WAL-index header: %w", err)
	}
	first, second := buf[:walIndexHeaderSize], buf[walIndexHeaderSize:2*walIndexHeaderSize]
	if string(first) != string(second) {
		return walIndex{}, errors.New("the WAL-index header's two copies disagree — a writer outside this store is mid-commit")
	}
	order := binary.NativeEndian
	if v := order.Uint32(first[0:4]); v != walIndexVersion {
		return walIndex{}, fmt.Errorf("the WAL-index header has version %d, want %d", v, walIndexVersion)
	}
	if first[walIndexIsInitOffset] != 1 {
		return walIndex{}, errors.New("the WAL-index header is not initialized")
	}
	index := walIndex{
		frames:     order.Uint32(first[walIndexMxFrame : walIndexMxFrame+4]),
		backfilled: order.Uint32(buf[walIndexBackfill : walIndexBackfill+4]),
	}
	for i := range index.readMarks {
		at := walIndexReadMark + 4*i
		index.readMarks[i] = order.Uint32(buf[at : at+4])
	}
	return index, nil
}

// walWatch is the hand-off from every writer's release to the checkpoint job.
// `kick` is a one-slot signal and `last` the latest reading, so a burst of
// releases costs the job one wake, never a queue.
type walWatch struct {
	kick chan struct{}
	// last is written only under the writer and read by the job under mu.
	last    walIndex
	lastErr error
	seen    bool
	shm     *os.File
}

// observeWAL reads the WAL-index and hands it to the checkpoint job. It runs
// in every writer's release, still holding the writer, so no commit or
// checkpoint can be moving the header underneath it.
//
// It is live only once the database is fully open (finishOpen makes the
// signal), so the schema creation inside Open never opens the -shm descriptor
// of a file the nuke path may still unlink.
func (d *DB) observeWAL() {
	if d.wal.kick == nil {
		return
	}
	index, err := d.readWAL()
	if err != nil {
		d.recordWALProbeFailure(err)
		return
	}
	d.walMu.Lock()
	d.wal.last, d.wal.lastErr, d.wal.seen = index, nil, true
	d.walMu.Unlock()
	select {
	case d.wal.kick <- struct{}{}:
	default:
	}
}

// recordWALProbeFailure is the one record a failed WAL-index read gets. The
// job is still woken, so it sees the failure and retries at the next trigger
// instead of silently going without a reading.
func (d *DB) recordWALProbeFailure(err error) {
	d.log.Log(logging.Fields{Operation: CheckpointOperation, DatabasePath: d.path, Level: "error", ErrorCause: err.Error()},
		"reading the WAL-index header failed, so the checkpoint job cannot tell how far the WAL has grown: %v", err)
	d.walMu.Lock()
	d.wal.lastErr = err
	d.walMu.Unlock()
	select {
	case d.wal.kick <- struct{}{}:
	default:
	}
}

// walReading is the job's view of the latest release.
func (d *DB) walReading() (walIndex, bool, error) {
	d.walMu.Lock()
	defer d.walMu.Unlock()
	return d.wal.last, d.wal.seen, d.wal.lastErr
}

// readWAL reads the WAL-index through the store's one -shm descriptor, opened
// on first use. The caller holds the writer.
//
// THE DESCRIPTOR IS NEVER CLOSED WHILE SQLITE HOLDS THE FILE. SQLite locks the
// -shm with POSIX fcntl locks, and closing ANY descriptor a process holds on a
// file releases EVERY fcntl lock that process holds on it — so closing this one
// while a connection is open would silently drop SQLite's read marks and let a
// checkpoint overwrite pages a reader is still using. It is opened once and
// closed only by Close, after both pools have closed.
func (d *DB) readWAL() (walIndex, error) {
	if d.wal.shm == nil {
		f, err := os.Open(d.path + "-shm")
		if err != nil {
			return walIndex{}, fmt.Errorf("opening the WAL-index: %w", err)
		}
		d.wal.shm = f
	}
	return readWALIndex(d.wal.shm)
}

// Checkpoint runs one PASSIVE checkpoint as BULK work: it queues for the one
// writer in the bulk tier, reads the WAL-index under it, and copies whatever is
// waiting. Nothing waiting is a skip, not a checkpoint. A failure is recorded
// here, once, at error, and returned; the caller's cancellation is recorded at
// info, like every other abandoned statement.
func (d *DB) Checkpoint(ctx context.Context, trigger CheckpointTrigger) (result CheckpointResult, err error) {
	result.Trigger = trigger
	base := logging.Fields{Operation: CheckpointOperation, DatabasePath: d.path, WriteClass: WriteBulk.String()}

	started := d.mono()
	err = d.acquireSlot(ctx, WriteBulk)
	lockWait := d.mono().Sub(started)
	if err != nil {
		return result, d.refuse(base, err)
	}
	// THE CHECKPOINT'S OWN RELEASE DOES NOT WAKE THE JOB. The job learns what
	// this checkpoint did from its result; a wake from here would re-run a
	// failed checkpoint at once, in a loop, rather than at the next trigger.
	defer d.writes.release()

	before, err := d.readWAL()
	if err != nil {
		return result, d.refuse(base, storagef(err, "reading the WAL-index for the %s-triggered checkpoint", trigger))
	}
	if before.pending() <= 0 {
		result.Skipped = true
		result.WALFrames, result.Checkpointed = int64(before.frames), int64(before.backfilled)
		d.log.LogVerbose(base, "nothing to checkpoint trigger=%s wal_frames=%d checkpointed=%d", trigger, before.frames, before.backfilled)
		return result, nil
	}

	var busy int
	run := d.runCheckpoint
	if run == nil {
		run = d.passiveCheckpoint
	}
	busy, result.WALFrames, result.Checkpointed, err = run(ctx)
	if err != nil {
		return result, d.refuse(base, storagef(err, "running the %s-triggered checkpoint", trigger))
	}
	// COPIED IS THIS RUN'S WORK. The log may have restarted since the header
	// was read only if a checkpoint ran in between, and none can: this one
	// holds the writer.
	copied := result.Checkpointed - int64(before.backfilled)
	result.Copied = copied
	fields := base
	fields.Statement = StatementWALCheckpoint
	fields.LockWait = lockWait
	fields.Rows = copied
	d.observeQuery(StatementWALCheckpoint, "", fields, started, copied)
	fields.Duration = d.mono().Sub(started)
	fields.Exec = fields.Duration - lockWait
	message := "WAL checkpointed trigger=%s mode=PASSIVE wal_frames=%d checkpointed=%d copied=%d busy=%d duration_ms=%d lock_wait_ms=%d exec_ms=%d"
	args := []any{trigger, result.WALFrames, result.Checkpointed, copied, busy,
		fields.Duration.Milliseconds(), lockWait.Milliseconds(), fields.Exec.Milliseconds()}
	if copied <= 0 {
		// A reader pinned every waiting frame, so nothing moved. One such run
		// is narration, not an event: the next trigger tries again. The read
		// marks are taken now, still under the writer, so that if the pin
		// outlasts DefaultPinWarnAfter the job's record can say which snapshot
		// holds it (RunCheckpoints, walPinWatch).
		after, err := d.readWAL()
		if err != nil {
			return result, d.refuse(base, storagef(err, "reading the WAL-index after the %s-triggered checkpoint", trigger))
		}
		result.ReadMarks = after.readMarks[:]
		d.log.LogVerbose(fields, message, args...)
		return result, nil
	}
	d.log.Log(fields, message, args...)
	return result, nil
}

// passiveCheckpoint is the statement itself, on the write connection the
// caller holds through the bulk tier.
func (d *DB) passiveCheckpoint(ctx context.Context) (busy int, frames, checkpointed int64, err error) {
	err = d.sql.QueryRowContext(ctx, `PRAGMA wal_checkpoint(PASSIVE)`).Scan(&busy, &frames, &checkpointed)
	return busy, frames, checkpointed, err
}

// checkpointTimer is the one timer the job arms. It is an interface so a test
// fires the idle trigger by hand instead of waiting one out.
type checkpointTimer interface {
	C() <-chan time.Time
	Stop() bool
}

type realTimer struct{ t *time.Timer }

func (r realTimer) C() <-chan time.Time { return r.t.C }
func (r realTimer) Stop() bool          { return r.t.Stop() }

func newRealTimer(d time.Duration) checkpointTimer { return realTimer{time.NewTimer(d)} }

// RunCheckpoints is the resident store's checkpoint job. It runs until ctx
// ends; main.go stops it BEFORE closing the database.
//
// GROWTH is measured from the frames the log held at the last checkpoint that
// succeeded, so a checkpoint a reader cut short is not re-run on every commit:
// the job waits for another Pages frames, or for the store to go idle. When the
// log has restarted since (its frame count is below the mark), everything in it
// is new.
//
// A FAILED CHECKPOINT IS RETRIED AT THE NEXT TRIGGER. Its mark does not move,
// so the very next release re-runs it, and the idle timer is re-armed so a
// quiet store retries too. Nothing is dropped.
func (d *DB) RunCheckpoints(ctx context.Context, policy CheckpointPolicy) {
	policy = policy.resolve()
	newTimer := d.newCheckpointTimer
	if newTimer == nil {
		newTimer = newRealTimer
	}
	var idle checkpointTimer
	var idleC <-chan time.Time
	arm := func() {
		if idle != nil {
			idle.Stop()
		}
		idle = newTimer(policy.Idle)
		idleC = idle.C()
	}
	disarm := func() {
		if idle != nil {
			idle.Stop()
		}
		idle, idleC = nil, nil
	}
	defer disarm()

	var mark uint32
	var pin walPinWatch
	run := func(trigger CheckpointTrigger) {
		result, err := d.Checkpoint(ctx, trigger)
		if err == nil {
			d.reportWALPin(&pin, result, policy.PinWarnAfter)
		}
		if d.checkpointDone != nil {
			d.checkpointDone(result, err)
		}
		if err != nil {
			if isContextError(err) {
				return
			}
			arm()
			return
		}
		mark = uint32(result.WALFrames)
		if result.Checkpointed < result.WALFrames {
			arm()
			return
		}
		disarm()
	}

	// A store that has just come up may carry a WAL from before it: the idle
	// trigger is armed at once so that backlog is folded without waiting for a
	// write.
	arm()
	for {
		select {
		case <-ctx.Done():
			return
		case <-d.wal.kick:
			index, seen, probeErr := d.walReading()
			if probeErr != nil || !seen {
				// The reading failed, and was recorded where it failed. The
				// idle trigger is what retries, since growth cannot be told.
				arm()
				continue
			}
			grown := int64(index.frames)
			if index.frames >= mark {
				grown = int64(index.frames - mark)
			}
			if grown >= policy.Pages {
				run(TriggerGrowth)
				continue
			}
			if index.pending() > 0 {
				arm()
			} else {
				disarm()
			}
		case <-idleC:
			idle, idleC = nil, nil
			run(TriggerIdle)
		}
	}
}

// WALPinOperation is the operation of the records that open and close a pin.
const WALPinOperation = "store.db.wal-pin"

// walPinWatch follows one PIN: an unbroken run of checkpoints that each copied
// nothing while frames waited, which only a reader's snapshot can cause. It
// opens at the first such checkpoint and closes at the first one that copies
// anything, or finds nothing waiting.
//
// A PIN IS ONLY REPORTED ONCE IT OUTLASTS ANY READ. A checkpoint that meets a
// read in progress copies nothing and is followed, a moment later, by one
// that copies it all; that is SQLite working as designed. The owner's store
// instead ran four hours of checkpoints that each copied 0 of up to 47,506
// frames, and said so only at verbose level, so the WAL passed 137 MB and every
// read over it slowed with nothing in the log to say why.
type walPinWatch struct {
	open     bool
	since    time.Time
	reported bool
}

type walPinEvent int

const (
	walPinQuiet walPinEvent = iota
	// walPinOutlasted is the one report of a pin that has outlasted the policy.
	walPinOutlasted
	// walPinReleased closes a pin that was reported.
	walPinReleased
)

// observe folds one successful checkpoint into the watch and says whether it
// opened a report or closed one, with how long the pin had been held by then.
// The pin is measured from the first checkpoint that saw it, so the duration
// is a lower bound on how long the snapshot has been held.
func (w *walPinWatch) observe(result CheckpointResult, now time.Time, warnAfter time.Duration) (walPinEvent, time.Duration) {
	pinned := !result.Skipped && result.Copied <= 0 && result.Checkpointed < result.WALFrames
	if !pinned {
		if !w.open {
			return walPinQuiet, 0
		}
		held, reported := now.Sub(w.since), w.reported
		*w = walPinWatch{}
		if reported {
			return walPinReleased, held
		}
		return walPinQuiet, held
	}
	if !w.open {
		w.open, w.since = true, now
	}
	held := now.Sub(w.since)
	if !w.reported && held >= warnAfter {
		w.reported = true
		return walPinOutlasted, held
	}
	return walPinQuiet, held
}

// reportWALPin writes the pin's two records: a warning once it has outlasted
// warnAfter, since the WAL now grows until somebody finds the reader, and an
// info record when it ends. Both carry the WAL's state and the read pool's, so
// the log alone says how far behind the database file is, which snapshot is
// held, and whether the pool thinks any connection is still in use.
func (d *DB) reportWALPin(pin *walPinWatch, result CheckpointResult, warnAfter time.Duration) {
	event, held := pin.observe(result, d.mono(), warnAfter)
	if event == walPinQuiet {
		return
	}
	stats := d.read.Stats()
	fields := logging.Fields{
		Operation:    WALPinOperation,
		DatabasePath: d.path,
		WAL: &logging.WALState{
			Frames:        result.WALFrames,
			Backfilled:    result.Checkpointed,
			ReadMarks:     result.ReadMarks,
			PinnedFor:     held,
			ReadPoolOpen:  stats.OpenConnections,
			ReadPoolInUse: stats.InUse,
			ReadPoolIdle:  stats.Idle,
		},
	}
	if event == walPinOutlasted {
		fields.Level = "warn"
		d.log.Log(fields, "a reader has pinned the WAL for at least %s: every checkpoint copies nothing while %d frames wait, so the WAL grows until that snapshot ends wal_frames=%d backfilled=%d read_marks=%v read_pool_in_use=%d",
			held.Round(time.Second), result.WALFrames-result.Checkpointed, result.WALFrames, result.Checkpointed, result.ReadMarks, stats.InUse)
		return
	}
	fields.Level = "info"
	d.log.Log(fields, "the WAL pin ended after at least %s wal_frames=%d backfilled=%d",
		held.Round(time.Second), result.WALFrames, result.Checkpointed)
}
