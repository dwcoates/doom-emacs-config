// cycle.go is the sidecar's production cycle and the invariant that governs it.
//
// THE STORE-UNREACHABLE INVARIANT. Every production cycle BEGINS with a
// successful GetSidecarCursors. Any store error — a refused or unreachable
// cursor read, a refused or unreachable batch write — SUSPENDS ALL PRODUCTION
// until a full recover-cursors-then-rescan succeeds. While production is
// suspended the sidecar reads nothing, loudly.
//
// WHY IT IS STRUCTURAL RATHER THAN A RETRY AROUND A SPECIAL CASE. A tailer's
// read position can only ever come from a cursor the store handed us, because:
//
//   - `cursors` is nil unless a cycle recovered it, and only `recover` sets it;
//   - `rescan` is the only thing that builds a tailer, and it asserts that;
//   - suspension DROPS every tailer, so a resumed cycle rebuilds each one from
//     the position this cycle's store handed it — never from a remembered one;
//   - after construction a tailer advances only through Commit, which the poll
//     loop calls only on a durable WriteBatch success;
//   - every periodic action runs through `producing`, so nothing polls, sweeps
//     or rescans while production is suspended.
//
// Boot ordering is therefore irrelevant by construction: a store that starts
// late simply means the first cycle has not begun yet, which is the same state
// a store that dies mid-run puts the sidecar into, handled by the same code.
//
// WHAT THIS REPLACES, AND WHY IT MATTERS. Cursor recovery once ran exactly at
// boot and its failure path was a silent cold start: the sidecar logged
// "cursor recovery failed (starting cold)" and re-read every watched file from
// offset 0. That is a fallback masking a down dependency, and it re-ingested
// whole conversations in production. The honest behavior while the store is
// unreachable is producing NOTHING, loudly.
//
// "COLD" SURVIVES ONLY AS ITS TRUTHFUL CASE: a REACHED store that genuinely
// holds no cursor for a file. That is the newly-discovered-transcript backfill
// and it reads from offset 0 exactly as it should.
package main

import (
	"context"
	"fmt"
	"math/rand"
	"os"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/shim-claude-sidecar/internal/tail"
	"golang.org/x/sys/unix"
)

// The recovery ladder: an immediate first attempt, then 250ms doubling to a
// ceiling the ladder holds FOREVER. There is no attempt budget and no terminal
// state — production that is suspended is production that is being recovered,
// for as long as the store stays unreachable.
const (
	recoverBackoffMin = 250 * time.Millisecond
	recoverBackoffMax = 10 * time.Second
)

// recoverJitterFraction spreads each armed delay over ±20% of its backoff, so a
// fleet of sidecars that lost the same store does not rediscover it in lockstep.
const recoverJitterFraction = 0.2

// recoverTick is how often Run asks whether a recovery attempt is due.
//
// THE LADDER IS A DEADLINE THE LOOP COMPARES AGAINST, NOT AN EVENT SOMEONE MUST
// REMEMBER TO SCHEDULE. It used to be one re-armable timer, which made the whole
// ladder depend on a Stop/drain/Reset dance executing correctly on every
// transition: a single lost re-arm silenced recovery entirely, with no state
// left in the process saying a recovery was owed. That is exactly what a
// production store bounce produced. A deadline cannot be lost — the ticker that
// checks it runs unconditionally for the life of the process — so the worst a
// mishandled transition costs is one tick of latency.
const recoverTick = 50 * time.Millisecond

// rpcTimeout bounds one store call. A call that cannot finish inside it is a
// store that is not answering, which is a suspension like any other.
const rpcTimeout = 30 * time.Second

// watched is one file the sidecar is currently reading.
type watched struct {
	target discover.Target
	tailer *tail.Tailer
}

type sidecar struct {
	options Options
	store   *storeclient.Client
	disc    *discover.Discoverer
	tracker *stale.Tracker
	log     *logging.Bound

	watchers map[string]*watched // by resolved path
	owners   *ownerIndex
	held     *heldSpools

	// cursors is CYCLE-SCOPED: recovered as the first act of every production
	// cycle and dropped the moment production is suspended, so a tailer can
	// never be built from a stale — or absent — recovery.
	cursors map[string]*storev1.CursorState // by resolved path; nil while suspended
	// rewound remembers which files have had their one boot rewind, so the
	// bounded backward scan happens once per file per process rather than on
	// every reconnect.
	rewound map[string]bool

	nextAttemptAt  time.Time
	attempting     bool
	backoff        time.Duration
	attempts       int
	suspendedSince time.Time
	bootSwept      bool

	// now and jitter are the cycle's clock and its backoff spread, injectable so
	// the ladder is tested by advancing a fake clock rather than by waiting.
	now    func() time.Time
	jitter func(time.Duration) time.Duration
	// bootTimeMs is the machine boot time, injectable for the boot sweep's test.
	bootTimeMs func() int64
}

func newSidecar(options Options, log *logging.Bound) *sidecar {
	s := &sidecar{
		options:  options,
		store:    storeclient.New(options.StoreSocket, log.With(logging.Context{Component: "storeclient"})),
		disc:     discover.New(options.ConfigRoots, options.SpoolRoot, log.With(logging.Context{Component: "discover"})),
		tracker:  stale.New(options.Stale, log.With(logging.Context{Component: "stale"})),
		log:      log,
		watchers: map[string]*watched{},
		rewound:  map[string]bool{},
		// A fresh sidecar is simply a sidecar whose first cycle has not begun
		// yet, with its first attempt due immediately. That is all "boot" means.
		now:        time.Now,
		jitter:     jitterBackoff,
		bootTimeMs: bootTimeMillis,
	}
	s.owners = newOwnerIndex(log.With(logging.Context{Component: "owner"}))
	s.held = newHeldSpools(options.UnownedSpoolWindow, log.With(logging.Context{Component: "held"}))
	s.suspendedSince = s.now()
	s.nextAttemptAt = s.now()
	return s
}

// Run drives the cycle until a termination signal.
func (s *sidecar) Run(stop <-chan os.Signal) error {
	pollT := time.NewTicker(s.options.PollInterval)
	rescanT := time.NewTicker(s.options.RescanInterval)
	sweepT := time.NewTicker(s.options.RescanInterval)
	// The ladder's heartbeat. It runs for the life of the process, whatever
	// production is doing, so suspended production is always being recovered.
	attemptT := time.NewTicker(recoverTick)
	defer pollT.Stop()
	defer rescanT.Stop()
	defer sweepT.Stop()
	defer attemptT.Stop()

	for {
		select {
		case signal := <-stop:
			s.log.With(logging.Context{Operation: "shutdown"}).Log("received signal=%s; shutting the sidecar down", signal)
			s.store.Close()
			return nil
		case <-attemptT.C:
			s.attemptDue()
		case <-rescanT.C:
			s.producing(s.rescan)
		case <-pollT.C:
			s.producing(s.pollAll)
		case <-sweepT.C:
			s.producing(s.sweep)
		}
	}
}

// producing runs one unit of work, and only while production is live. EVERY
// periodic action goes through here, so "reads while the store is unreachable"
// is one decision in one place rather than a condition each caller remembers.
func (s *sidecar) producing(work func()) {
	if s.cursors != nil {
		work()
	}
}

// requireCursors asserts the invariant every production step depends on: no
// file is read, and no tailer is built, without this cycle's recovered cursors
// in hand. A violation is a bug in this cycle, so it fails hard rather than
// quietly cold-starting.
func (s *sidecar) requireCursors(what string) {
	if s.cursors == nil {
		panic(fmt.Sprintf("sidecar: %s with production suspended — a tailer's position may only come from a cursor this cycle's store handed us", what))
	}
}

// attemptDue runs the LADDER: it attempts recovery whenever production is
// suspended and the armed deadline has passed, and does nothing otherwise.
func (s *sidecar) attemptDue() {
	if s.cursors != nil || s.attempting || s.now().Before(s.nextAttemptAt) {
		return
	}
	s.attempt()
}

// attempt tries to begin a production cycle. It is SINGLE-FLIGHT so two ticks
// cannot race two cursor recoveries onto one cycle.
func (s *sidecar) attempt() {
	s.attempting = true
	defer func() { s.attempting = false }()
	if err := s.beginCycle(); err != nil {
		s.attempts++
		s.backoff = nextBackoff(s.backoff)
		delay := s.jitter(s.backoff)
		// Reading no files IS the whole file plane stopped, so each retry is
		// recorded — at verbose, because the WARNING that opened the suspension
		// already said the loud part once.
		s.log.With(logging.Context{Operation: "recover-cursors", Level: "warn"}).
			LogVerbose("recovery attempt %d failed, retrying in %s while reading no files: %v", s.attempts, delay, err)
		s.nextAttemptAt = s.now().Add(delay)
		return
	}
}

// beginCycle is the first act of every production cycle: recover the cursors,
// then build this cycle's tailers from them, and only then start reading. Any
// failure leaves production suspended with nothing read, which is the whole
// point — a cycle that could write but had no recovery state is exactly the
// cold start this design removes.
func (s *sidecar) beginCycle() error {
	ctx, cancel := context.WithTimeout(context.Background(), rpcTimeout)
	defer cancel()
	cursors, err := s.store.Cursors(ctx, "")
	if err != nil {
		// storeclient owns the causal record with its rpc and refusal detail.
		return fmt.Errorf("recovering cursors: %w", err)
	}
	s.cursors = indexCursorsByPath(cursors)
	s.log.With(logging.Context{Operation: "recover-cursors"}).Log(
		"production cycle begins: recovered %d cursor(s) from the store", len(s.cursors))

	// Reading may begin now, and not one statement earlier.
	s.rescan()

	// The boot sweep is about MACHINE boot rather than about this cycle, so it
	// runs once per process — but its conclusions are records, so it can only
	// run once there is a store to write them to.
	if !s.bootSwept {
		s.bootSwept = true
		s.emit("boot sweep", s.lostEntries(s.tracker.BootSweep(s.bootTimeMs(), s.now().UnixMilli())))
	}
	s.reportResumed()
	return nil
}

// suspend stops all production and arms an immediate recovery attempt. It is
// idempotent, because one poll pass can surface the same dead store through
// several failed writes.
func (s *sidecar) suspend(operation string, cause error) {
	if s.cursors == nil {
		return
	}
	s.cursors = nil
	// EVERY TAILER GOES WITH THE CURSORS. A tailer that outlived its cycle would
	// resume from a position the NEXT cycle's store never handed us, which is
	// the one thing the invariant forbids.
	s.watchers = map[string]*watched{}
	s.store.Close()
	s.suspendedSince = s.now()
	s.attempts = 0
	s.backoff = 0
	s.nextAttemptAt = s.now()
	// The record that OPENS the outage: every tail stops here and the file plane
	// produces nothing until the store answers again. The caller owns the causal
	// error, so this record owns only the transition.
	s.log.With(logging.Context{Operation: "production-suspended", Level: "warn"}).Log(
		"production suspended after operation=%s; reading no files until a full cursor recovery succeeds", operation)
	_ = cause
}

// noteStoreErr suspends production for any store error. There is no connection
// to consult any more: a refusal and a transport failure both mean this cycle
// cannot be trusted to have a store behind it, and the invariant's price for
// being wrong is re-ingesting a conversation.
func (s *sidecar) noteStoreErr(operation string, err error) {
	if err == nil {
		return
	}
	s.suspend(operation, err)
}

// reportResumed closes the outage window in the log. A cycle that began on its
// first attempt spent no time suspended and reports nothing.
func (s *sidecar) reportResumed() {
	if s.attempts == 0 {
		return
	}
	downMs := s.now().Sub(s.suspendedSince).Milliseconds()
	s.log.With(logging.Context{Operation: "production-resumed"}).Log(
		"production resumed: the store was unreachable for %dms across %d failed recovery attempt(s), during which no file was read",
		downMs, s.attempts)
	s.attempts = 0
}

// rescan discovers targets and creates a tailer for each new one, seeded from
// the cursor THIS cycle's store handed us.
//
// This is the ONLY place a tailer is ever built, which is why it asserts the
// invariant: a tailer built without a recovered cursor map starts at offset 0,
// and that silent cold start is the bug the whole cycle exists to prevent.
func (s *sidecar) rescan() {
	s.requireCursors("rescan")
	now := s.now()
	for _, target := range s.disc.Scan() {
		if _, ok := s.watchers[target.Path]; ok {
			continue
		}
		if target.MetaMissing {
			// discover.withMeta already stated this once; the target stays
			// discovered and is re-checked on the next rescan.
			continue
		}
		resolved, ok := s.resolveTarget(target, now)
		if !ok {
			continue
		}
		s.watch(resolved, now)
	}
}

// watch builds one tailer for a resolved target and starts reading it.
func (s *sidecar) watch(target discover.Target, now time.Time) {
	s.requireCursors("watch")
	bound := s.log.With(logging.Context{
		Component: "tail", Path: target.Path, TaskID: target.TaskID,
		VendorSessionID: target.SessionID, AgentID: target.AgentID,
	})
	ctx := &tail.Context{
		SessionID:         target.SessionID,
		Path:              target.Path,
		Kind:              target.Kind,
		AgentID:           target.AgentID,
		MainAgentID:       s.owners.mainAgentFor(target),
		SpawnBackgrounded: s.owners.spawnBackgrounded(target.TaskID),
		MetaPath:          target.MetaPath,
		ConfigRoots:       s.disc.ConfigRoots(),
		TaskID:            target.TaskID,
		SpoolDir:          target.SpoolDir,
		RunID:             target.RunID,
	}
	tailer := tail.New(target.Path, target.Codec(), s.newHandler(target.Kind, bound), ctx, bound)
	if cursor := s.cursors[target.Path]; cursor != nil {
		tailer.Restore(cursor)
		s.rewindOnce(target, tailer)
	}
	s.watchers[target.Path] = &watched{target: target, tailer: tailer}
	s.trackDetached(target, now)
	bound.With(logging.Context{Operation: "watch"}).Log("watching %s", target.Kind)
}

// rewindOnce applies the boot rewind: ONE bounded backward scan per file per
// process, moving a restored cursor back to the first record of the in-progress
// turn so the converter's in-memory joins re-warm over one re-read turn.
//
// Spools are never rewound: they carry no turns, and their deltas are already
// offset-carrying.
func (s *sidecar) rewindOnce(target discover.Target, tailer *tail.Tailer) {
	if s.rewound[target.Path] {
		return
	}
	s.rewound[target.Path] = true
	switch target.Kind {
	case tail.KindSessionTranscript, tail.KindAgentTranscript:
		tailer.RewindToTurnStart(tail.DefaultRewindWindow, tail.IsUserPromptRecord)
	default:
		s.log.With(logging.Context{Operation: "boot-rewind", Path: target.Path}).
			LogVerbose("no rewind for kind=%s: it carries no turns", target.Kind)
	}
}

// trackDetached starts the LOST policy's clock for a file that IS a detached
// run. A transcript is not one: it is an agent's own record, and its silence is
// not a conclusion about anything.
func (s *sidecar) trackDetached(target discover.Target, now time.Time) {
	if target.TaskID == "" || target.SessionID != "" {
		return
	}
	work := stale.Work{
		Path:          target.Path,
		TaskID:        target.TaskID,
		Kind:          target.Kind,
		OwnerAgentID:  s.owners.agentFor(target.TaskID),
		RunActivityID: s.owners.activityFor(target.TaskID),
	}
	if info, err := os.Stat(target.Path); err == nil {
		work.LastActivityMs = info.ModTime().UnixMilli()
	}
	s.tracker.Observe(work, now.UnixMilli())
}

// pollAll polls every watched file once, writes any batch, and commits the
// cursor only after that write was DURABLE. It is reachable only while
// production is live, so a file is never read without somewhere to put what it
// says.
func (s *sidecar) pollAll() {
	s.requireCursors("pollAll")
	nowMs := s.now().UnixMilli()
	for path, w := range s.watchers {
		result, err := w.tailer.Poll()
		if err != nil {
			s.pollFailed(path, w, err, nowMs)
			continue
		}
		if !result.Changed {
			continue
		}
		if err := s.writeBatch(result); err != nil {
			// Honest sad path: nothing was committed, so the cursor does not
			// move and the same durable bytes are re-read next cycle.
			s.log.With(logging.Context{
				Operation: "store-write", Path: path, TaskID: w.target.TaskID,
				Offset: logging.Off(result.Next.GetOffset()), Level: "error",
			}).Log("cursor not advanced after %d record(s): %v", len(result.Entries), err)
			// The write did not merely fail, it suspended production. Abandon
			// the pass rather than reading the rest with nowhere to put it.
			return
		}
		w.tailer.Commit(result)
		s.tracker.Activity(path, nowMs)
		s.log.With(logging.Context{
			Operation: "tail-pickup", Path: path, TaskID: w.target.TaskID,
			FileID: result.Next.GetFileId(), Offset: logging.Off(result.Next.GetOffset()),
		}).Log("picked up %d record(s) kind=%s", len(result.Entries), w.target.Kind)
	}
}

// pollFailed narrates one file's read failure and, for a file that vanished,
// starts the LOST policy's grace clock.
func (s *sidecar) pollFailed(path string, w *watched, err error, nowMs int64) {
	if os.IsNotExist(err) {
		s.tracker.MarkVanished(path, nowMs)
		delete(s.watchers, path)
		s.log.With(logging.Context{Operation: "file-vanished", Path: path, TaskID: w.target.TaskID, Level: "warn"}).
			Log("the watched file vanished; any bytes appended past the committed offset went with it")
		return
	}
	s.log.With(logging.Context{Operation: "poll", Path: path, TaskID: w.target.TaskID, Level: "error"}).
		Log("tail poll failed: %v", err)
}

// writeBatch sends one tailer batch — the records plus the cursor advance that
// must become durable WITH them.
//
// THE CURSOR RIDES WITH THE RECORDS ON PURPOSE. Split them and a crash between
// the two either loses records (cursor advanced first) or duplicates them
// (records first); only the second is survivable, by the deterministic write_id
// on every record, which is a recovery rather than a guarantee.
func (s *sidecar) writeBatch(result tail.PollResult) error {
	return s.storeWrite("tailer batch", &storev1.EntryBatch{
		Entries:       result.Entries,
		CursorAdvance: result.Next,
	})
}

// sweep states the LOST conclusions whose windows expired and writes their
// terminals.
func (s *sidecar) sweep() {
	s.requireCursors("sweep")
	s.emit("lost sweep", s.lostEntries(s.tracker.Sweep(s.now().UnixMilli())))
}

// emit writes inferred records as a single CURSOR-LESS batch: they were not
// read at a file position, so there is no reader position that becomes durable
// with them and nothing that could advance one wrongly.
func (s *sidecar) emit(what string, entries []*storev1.StoreEntry) {
	if len(entries) == 0 {
		return
	}
	if err := s.storeWrite(what, &storev1.EntryBatch{Entries: entries}); err != nil {
		s.log.With(logging.Context{Operation: "store-write", Level: "error"}).
			Log("%s write failed for %d record(s); production is suspended and the conclusions are restated on the next cycle: %v", what, len(entries), err)
	}
}

// storeWrite is the sidecar's ONLY path to the store. Routing every write
// through here is what makes an unreachable store impossible to miss.
func (s *sidecar) storeWrite(what string, batch *storev1.EntryBatch) error {
	ctx, cancel := context.WithTimeout(context.Background(), rpcTimeout)
	defer cancel()
	err := s.store.WriteBatch(ctx, batch)
	s.noteStoreErr(what, err)
	return err
}

// indexCursorsByPath keys recovered cursors by their file path for tailer
// restore. The store's own key is the file id; the path is what discovery hands
// back, and both are carried on every CursorState.
func indexCursorsByPath(cursors []*storev1.CursorState) map[string]*storev1.CursorState {
	out := make(map[string]*storev1.CursorState, len(cursors))
	for _, cursor := range cursors {
		if cursor.GetPath() != "" {
			out[discover.Normalize(cursor.GetPath())] = cursor
		}
	}
	return out
}

// jitterBackoff spreads d over ±recoverJitterFraction of itself. A zero delay
// (the immediate retry a fresh suspension arms) stays immediate.
func jitterBackoff(d time.Duration) time.Duration {
	if d <= 0 {
		return 0
	}
	spread := float64(d) * recoverJitterFraction
	return time.Duration(float64(d) - spread + 2*spread*rand.Float64())
}

// nextBackoff doubles d from recoverBackoffMin up to the recoverBackoffMax
// ceiling.
func nextBackoff(d time.Duration) time.Duration {
	if d == 0 {
		return recoverBackoffMin
	}
	d *= 2
	if d > recoverBackoffMax {
		return recoverBackoffMax
	}
	return d
}

// bootTimeMillis returns the machine boot time in unix millis, or 0 when it is
// unavailable (darwin/BSD kern.boottime).
func bootTimeMillis() int64 {
	tv, err := unix.SysctlTimeval("kern.boottime")
	if err != nil {
		return 0
	}
	return int64(tv.Sec)*1000 + int64(tv.Usec)/1000
}
