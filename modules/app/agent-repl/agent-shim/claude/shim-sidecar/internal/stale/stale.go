// Package stale is the sidecar's LOST policy.
//
// LOST IS ITS OWN WORD: it means "we stopped seeing it", never "we know it
// failed". The sidecar reads files; it has no view of process liveness, so the
// most it can ever say about a detached run whose file went quiet is HOW it
// stopped seeing it. That statement is the reader's to make LOUDLY — never to
// silently drop, and never to spell as a completion.
//
// THREE WAYS TO STOP SEEING A RUN, and the arm IS how we concluded it:
//
//  1. FileVanished — the run's file disappeared while the run was open. A grace
//     window absorbs the ordinary rename/replace race before the conclusion.
//  2. WentSilent   — the file is still there and has not grown for longer than
//     its kind's silence window.
//  3. SweptUp      — a run whose file has not been touched since before the
//     machine booted. Nothing survives a reboot. It is checked at boot AND on
//     every sweep, because a run discovered after the boot pass (a spool whose
//     hold expired, say) is exactly as dead as one that was open during it.
//
// IT RE-DERIVES FROM FILES AND CURSORS, AND ASKS THE RECORD WHAT SETTLED. What
// the sidecar knows is which files exist, when they were last written, and how
// far its cursors have read them — so that is what the policy's clocks are
// built out of. What it can NOT know from a file is that a run already settled
// before this process started: the terminator sits behind the committed cursor
// and is never read again. On 2026-09-30 a restarted sidecar re-tracked such a
// finished run from its quiet spool and concluded it LOST. So the caller asks
// the store (GetRunSettlements) before it observes a run, and a run the record
// holds as ended is settled here (SettledByRecord) instead of tracked. Inside
// one process every settle lands in one set, and observing a settled run
// panics: a settled run is never tracked again.
//
// IT MINTS NO RECORDS. A Lost is an OBSERVATION; turning one into the run's
// terminal frame is conversion, and conversion lives behind the handler seam.
// Keeping this package free of conversion is what lets the policy be tested for
// what it concludes rather than for what it emits.
package stale

import (
	"sort"
	"sync"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Default windows, overridable through Options.
const (
	DefaultGrace           = 30 * time.Second
	DefaultShellSilence    = 30 * time.Minute
	DefaultAgentSilence    = 60 * time.Minute
	DefaultWorkflowSilence = 60 * time.Minute
)

// Reason names HOW we stopped seeing a run.
type Reason string

const (
	ReasonFileVanished Reason = "file_vanished"
	ReasonWentSilent   Reason = "went_silent"
	ReasonSweptUp      Reason = "swept_up"
)

// Options tunes the windows. Zero values fall back to the defaults.
type Options struct {
	Grace           time.Duration
	ShellSilence    time.Duration
	AgentSilence    time.Duration
	WorkflowSilence time.Duration
}

// Work is one detached run the reader is watching, as re-derived from its file.
// Path is the run's identity here: it is what the reader actually observes, and
// it is already symlink-resolved by discovery, so one file cannot enter the
// tracker twice.
type Work struct {
	Path   string
	TaskID string
	Kind   tail.Kind
	// OwnerAgentID is the agent whose stream spawned the run, when it is known.
	OwnerAgentID string
	// RunActivityID is the spawning call's activity id — the handle a terminal
	// is keyed by. Empty until the spawn is observed.
	RunActivityID string
	// LastActivityMs is when the file was last known to have grown, seeded from
	// its mtime at first observation.
	LastActivityMs int64
}

// Lost is one concluded observation, handed to the caller to state and to turn
// into the run's terminal.
type Lost struct {
	Work
	Reason Reason
	// ObservedAtMs is when the conclusion was reached, NOT when the run ended:
	// we do not know when it ended, which is the whole point of the word.
	ObservedAtMs int64
	// Catchup says this conclusion is STARTUP BACKLOG rather than a
	// newly-arising condition: the run's last growth predates this sidecar's
	// own start, so it was already stale before we were watching and was
	// concluded once, long ago, by whoever was. `state` summarizes these
	// per class at INFO instead of stating each one, and it rides out to the
	// caller so the TERMINAL's own record can follow the same classification
	// — without it the flood this policy exists to stop simply reappeared one
	// layer down, as one `bash-lost` WARN per backlog run.
	Catchup bool
	// BenignEnd names WHY the file's disappearance took nothing with it, and
	// is empty when it may have. A whole tree removed on purpose
	// (BenignTreeRemoved), a spool that had already written its terminator
	// (BenignEnded), a file every byte of which was read and committed before
	// it went (BenignFullyRead) — in each the committed offset was the last
	// thing the file had, so the conclusion is stated at INFO. Only a
	// disappearance that could have taken bytes past the committed offset is
	// the loss this policy warns about. It changes NO conclusion and NO wire
	// arm: the run is still LOST and the arm is still file_vanished.
	BenignEnd string
}

// The BenignEnd vocabulary. Each names a way a vanished file can be known to
// have given up everything it ever had; the empty string is the genuine loss.
const (
	// BenignTreeRemoved: the DIRECTORY holding the file was gone too, so
	// nobody unlinked a file out from under the reader.
	BenignTreeRemoved = "tree_removed"
	// BenignEnded: the file was a spool that had already written its
	// terminator (`[exited with code N]`, `[killed]`), so the run it carried
	// was already concluded when the file went.
	BenignEnded = "ended"
	// BenignFullyRead: the committed offset equalled the size the last poll
	// observed, so every byte the file ever held is durable.
	BenignFullyRead = "fully_read"
)

type entry struct {
	work         Work
	vanishedAtMs int64  // 0 = the file is present
	benignEnd    string // why the vanish took nothing with it; "" = a genuine loss
}

// Tracker holds the open runs and concludes LOST. Safe for concurrent use: the
// poll loop and the sweep both touch it.
type Tracker struct {
	mu   sync.Mutex
	open map[string]*entry // by resolved path
	// settled is every path this process has seen settle — by a terminal read
	// off its own file, a transcript's conclusion, a stop, a LOST terminal made
	// durable, or the store already holding the run as ended. A SETTLED RUN IS
	// NEVER TRACKED AGAIN: observing one is an invariant violation and panics,
	// so the caller asks Settled first. Settle and SettledByRecord are the only
	// ways in, both through retireLocked.
	//
	// A SWEEP'S CONCLUSION IS NOT YET A SETTLE. Sweep stops tracking what it
	// concludes so it is not restated every pass, but the run is settled only
	// once the caller has made its LOST terminal durable and says so through
	// Settle: a conclusion whose write failed must be re-derived and restated
	// by the next cycle, which a settled run never would be.
	settled map[string]struct{}
	opt     Options
	log     *logging.Bound
	// bootUnknownSaid keeps the "no boot time" statement to once per process:
	// the sweep runs on a timer, and repeating it every tick would bury it.
	bootUnknownSaid bool
	// processStartMs is when this sidecar began producing. It is the boundary
	// between a run that was ALREADY stale before we started — backlog the first
	// sweep catches up on and states as ONE summary per class — and one that
	// went stale WHILE we watched, a newly-arising condition stated per item.
	// The clock is the run's own last-activity mtime, never our read time, so
	// this joins the same fact swept_up already reads (mtime < bootMs). Zero
	// means unset: nothing is treated as catch-up and the tracker states every
	// conclusion per item, exactly as it did before this policy existed.
	processStartMs int64
}

// New builds a Tracker.
func New(opt Options, log *logging.Bound) *Tracker {
	if opt.Grace == 0 {
		opt.Grace = DefaultGrace
	}
	if opt.ShellSilence == 0 {
		opt.ShellSilence = DefaultShellSilence
	}
	if opt.AgentSilence == 0 {
		opt.AgentSilence = DefaultAgentSilence
	}
	if opt.WorkflowSilence == 0 {
		opt.WorkflowSilence = DefaultWorkflowSilence
	}
	log.With(logging.Context{Operation: "stale-new"}).LogVerbose(
		"constructing lost tracker grace=%s shell_silence=%s agent_silence=%s workflow_silence=%s",
		opt.Grace, opt.ShellSilence, opt.AgentSilence, opt.WorkflowSilence)
	return &Tracker{open: map[string]*entry{}, settled: map[string]struct{}{}, opt: opt, log: log}
}

// Windows reports the windows this tracker actually runs with, defaults filled
// in. It exists so the flag wiring can be asserted where it lands rather than
// where it is parsed: a window that never reached the tracker is a flag that
// does nothing.
func (t *Tracker) Windows() Options { return t.opt }

// SetProcessStart records when this sidecar began producing, so the sweep can
// tell a run that was ALREADY stale before we started (backlog, summarized as
// startup catch-up) from one that went stale while we watched (stated per
// item). It is set ONCE, at the first production cycle; a later call is ignored
// so a store bounce that re-enters the first cycle cannot move the boundary
// forward and reclassify runs it once summarized.
func (t *Tracker) SetProcessStart(ms int64) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.processStartMs == 0 {
		t.processStartMs = ms
	}
}

// Observe records that a run's file is being watched. Re-observing a known run
// refreshes what the reader has since learned about it (its owner, its run
// handle) without disturbing its activity clock.
//
// A SETTLED RUN IS NEVER OBSERVED. Tracking it again is how a finished run came
// to be concluded LOST (2026-09-30), so a caller that reaches here with one has
// broken the invariant, and it panics rather than track it.
func (t *Tracker) Observe(work Work, nowMs int64) {
	if work.Path == "" {
		panic("stale: a tracked run must name its file")
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	if _, settled := t.settled[work.Path]; settled {
		panic("stale: observing a run that already settled: " + work.Path)
	}
	if existing, ok := t.open[work.Path]; ok {
		if work.OwnerAgentID != "" {
			existing.work.OwnerAgentID = work.OwnerAgentID
		}
		if work.RunActivityID != "" {
			existing.work.RunActivityID = work.RunActivityID
		}
		t.bound(existing.work).LogVerbose("re-observed an already tracked run")
		return
	}
	if work.LastActivityMs == 0 {
		work.LastActivityMs = nowMs
	}
	t.open[work.Path] = &entry{work: work}
	t.bound(work).Log("tracking detached run kind=%s last_activity_ms=%d", work.Kind, work.LastActivityMs)
}

// Activity records that the file grew, which is the only evidence of liveness a
// file reader has. It also clears a vanish: a file that came back was a rename
// race, not a disappearance.
func (t *Tracker) Activity(path string, nowMs int64) {
	t.mu.Lock()
	defer t.mu.Unlock()
	existing, ok := t.open[path]
	if !ok {
		return
	}
	existing.work.LastActivityMs = nowMs
	if existing.vanishedAtMs != 0 {
		existing.vanishedAtMs = 0
		t.bound(existing.work).Log("the vanished file is back and growing; its grace clock is cleared")
	}
}

// MarkVanished starts the grace clock for a file that disappeared.
//
// benignEnd names why the disappearance took nothing with it, or is empty when
// it may have. That is what separates an ordinary end from a file losing bytes
// under the reader, and it decides only the LEVEL of the records: a benign end
// is stated at INFO, anything else stays a WARNING. The conclusion itself and
// the wire arm are identical either way.
func (t *Tracker) MarkVanished(path string, nowMs int64, benignEnd string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	existing, ok := t.open[path]
	if !ok {
		return
	}
	if existing.vanishedAtMs != 0 {
		return
	}
	existing.vanishedAtMs = nowMs
	existing.benignEnd = benignEnd
	if benignEnd != "" {
		t.bound(existing.work).With(logging.Context{Reason: benignEnd}).Log(
			"the run's file vanished with nothing outstanding (%s), which is an ordinary end rather than a loss; the grace window of %s still decides the conclusion", benignEnd, t.opt.Grace)
		return
	}
	t.bound(existing.work).With(logging.Context{Level: "warn"}).Log(
		"the run's file vanished; the grace window of %s decides whether that is a rename race or a LOST run", t.opt.Grace)
}

// Settle records that a run ended on evidence — its own terminator, a
// transcript's conclusion, a stop, or a LOST the caller refused to restate —
// and stops tracking it. A settled run is never swept and never tracked again:
// LOST is only ever the answer for a run we stopped seeing, never for one we
// saw finish. It answers whether the run was being tracked.
//
// A RUN NOT BEING TRACKED IS STILL RECORDED AS SETTLED, so a file whose watcher
// is rebuilt later in this process is never tracked again either.
func (t *Tracker) Settle(path string) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	existing, ok := t.open[path]
	t.retireLocked(path)
	if !ok {
		t.log.With(logging.Context{Operation: "lost-policy", Path: path}).LogVerbose(
			"run recorded as settled; it was not being tracked, and it never will be")
		return false
	}
	t.bound(existing.work).Log("run settled by a terminal read from its own file; it can no longer be concluded LOST")
	return true
}

// SettledByRecord records that the store already holds a run as ended, so it
// is never tracked. It is the answer for a run that settled before this process
// started: its terminator sits behind the committed cursor and is never read
// again, so nothing on the file plane could say so.
//
// A RUN BEING TRACKED CANNOT BE SETTLED BY THE RECORD: the caller asks the
// store BEFORE it observes, and reaching here with an open run means it did
// not, which panics.
func (t *Tracker) SettledByRecord(work Work, endedAtMs int64) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if _, open := t.open[work.Path]; open {
		panic("stale: a tracked run cannot be settled by the record; the store is asked before a run is observed: " + work.Path)
	}
	t.retireLocked(work.Path)
	t.bound(work).Log("run already settled per the store (ended_at_ms=%d); it is not tracked and can never be concluded LOST", endedAtMs)
}

// Settled reports whether a path's run has settled in this process.
func (t *Tracker) Settled(path string) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	_, ok := t.settled[path]
	return ok
}

// retireLocked is the ONE way a run stops being tracked: it leaves `open` and
// joins `settled`, so it can never be observed again. Caller holds mu.
func (t *Tracker) retireLocked(path string) {
	delete(t.open, path)
	t.settled[path] = struct{}{}
}

// Open reports whether a path is still being tracked.
func (t *Tracker) Open(path string) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	_, ok := t.open[path]
	return ok
}

// Sweep concludes every run whose grace or silence window has expired, and
// stops tracking it. The conclusions are returned in path order so a sweep's
// records are stable across runs.
func (t *Tracker) Sweep(bootMs, nowMs int64) []Lost {
	t.mu.Lock()
	defer t.mu.Unlock()
	if bootMs <= 0 && !t.bootUnknownSaid {
		// BootSweep already said the loud part once; this records that the
		// sweep's boot arm is inert too, rather than leaving it unstated.
		t.bootUnknownSaid = true
		t.log.With(logging.Context{Operation: "lost-policy"}).LogVerbose(
			"boot time unavailable: the sweep's swept_up arm is inert and a pre-boot run can only be concluded by its silence")
	}
	var out []Lost
	for path, existing := range t.open {
		reason, concluded := t.conclude(existing, bootMs, nowMs)
		if !concluded {
			continue
		}
		delete(t.open, path)
		out = append(out, Lost{Work: existing.work, Reason: reason, ObservedAtMs: nowMs, BenignEnd: existing.benignEnd})
	}
	return t.state(out, nowMs)
}

// conclude decides whether one entry's window has expired. Caller holds mu.
//
// A VANISHED FILE IS JUDGED ONLY BY ITS GRACE WINDOW: that we watched it
// disappear is a better statement than any window could make, so nothing else
// is consulted until the grace decides between a rename race and a LOST run.
// Otherwise the BOOT rule comes first, because "the file predates the reboot"
// says HOW we know rather than merely that the file is quiet.
func (t *Tracker) conclude(e *entry, bootMs, nowMs int64) (Reason, bool) {
	if e.vanishedAtMs != 0 && nowMs-e.vanishedAtMs >= t.opt.Grace.Milliseconds() {
		return ReasonFileVanished, true
	}
	if e.vanishedAtMs != 0 {
		return "", false
	}
	if bootMs > 0 && e.work.LastActivityMs < bootMs {
		return ReasonSweptUp, true
	}
	if nowMs-e.work.LastActivityMs >= t.silence(e.work.Kind).Milliseconds() {
		return ReasonWentSilent, true
	}
	return "", false
}

// BootSweep concludes every tracked run whose file has not been written since
// before the machine booted. Nothing survives a reboot, so a run still open
// across one was never going to report again.
func (t *Tracker) BootSweep(bootMs, nowMs int64) []Lost {
	if bootMs <= 0 {
		// Without a boot time the sweep cannot run, and a pre-boot run stays
		// "running" forever in every reader downstream. That is a real loss, so
		// it is stated rather than passed over.
		t.log.With(logging.Context{Operation: "boot-sweep", Level: "warn"}).Log(
			"boot time unavailable: runs that predate the reboot cannot be swept and stay open")
		return nil
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	var out []Lost
	for path, existing := range t.open {
		if existing.work.LastActivityMs >= bootMs {
			continue
		}
		delete(t.open, path)
		out = append(out, Lost{Work: existing.work, Reason: ReasonSweptUp, ObservedAtMs: nowMs})
	}
	return t.state(out, nowMs)
}

// state logs each conclusion and returns them in a stable order. Caller holds mu.
//
// A CONCLUSION ABOUT A BACKLOG RUN IS CATCH-UP, NOT A NEW EVENT. A restarted
// sidecar re-derives every historical run from its files, and one that was
// already stale before we started (its last growth predates processStartMs) was
// concluded once, long ago; re-stating each of hundreds per restart is the same
// inverted-pyramid flood the discover-meta holds already fixed. So a run whose
// last activity predates the process start is accumulated per class and stated
// as ONE summary; only a run that went stale WHILE we watched is stated per
// item. Nothing is silenced: the totals ride the summary.
func (t *Tracker) state(out []Lost, nowMs int64) []Lost {
	sort.Slice(out, func(i, j int) bool { return out[i].Path < out[j].Path })
	catchup := map[Reason]*catchupCount{}
	for i := range out {
		lost := &out[i]
		if t.processStartMs != 0 && lost.LastActivityMs < t.processStartMs {
			lost.Catchup = true
			c := catchup[lost.Reason]
			if c == nil {
				c = &catchupCount{oldestMs: lost.LastActivityMs}
				catchup[lost.Reason] = c
			}
			c.add(lost.LastActivityMs)
			t.bound(lost.Work).With(logging.Context{Level: "debug", Reason: string(lost.Reason)}).LogVerbose(
				"run concluded LOST during startup catch-up reason=%s: it was already stale before this sidecar started, so it is summarized rather than stated on its own (last_activity_ms=%d observed_at_ms=%d)",
				lost.Reason, lost.LastActivityMs, nowMs)
			continue
		}
		if lost.BenignEnd != "" {
			// NOTHING WAS OUTSTANDING WHEN THE FILE WENT. The committed offset
			// was the last thing the file had — because the whole tree was
			// removed on purpose, because the spool had already written its
			// terminator, or because every byte was read and committed. The
			// conclusion is unchanged and the wire arm is unchanged — only the
			// level, because there is nothing here for an operator to act on.
			//
			// THE `reason` KEY STAYS THE WIRE ARM. `lost-terminal` joins to this
			// record on it (seam.go), so spelling a second vocabulary into the
			// same key would break the join that exists to explain a terminal.
			// The benign end is stated in the sentence and is filterable one
			// record earlier, on the `file-vanished` record that carries it in
			// `reason`.
			t.bound(lost.Work).With(logging.Context{Reason: string(lost.Reason)}).Log(
				"run concluded LOST reason=%s: its file vanished with nothing outstanding (%s), which is an ordinary end rather than a loss (last_activity_ms=%d observed_at_ms=%d)",
				lost.Reason, lost.BenignEnd, lost.LastActivityMs, nowMs)
			continue
		}
		if lost.Reason == ReasonWentSilent {
			// SILENCE IS NOT A FAULT, AND THIS READER CANNOT SAY OTHERWISE.
			//
			// The record's own sentence is the argument: "we stopped seeing
			// it, which is not a claim that it failed". A detached run that is
			// quiet and ALIVE is the ordinary shape of a poll loop — the
			// owner's own background shells sit in an `until` loop that prints
			// nothing between checks — and this package has no way to tell one
			// from a run that died. It was looked for: the vendor's launch
			// result carries `backgroundTaskId` and a cwd hint and NO pid, the
			// spool is a plain file with no heartbeat, nothing is written
			// beside it, and a terminator (`EXIT=`, `[exited with code N]`,
			// `[killed]`) is the only end signal the vendor ever writes. The
			// package doc's "it has no view of process liveness" is a fact
			// about the file plane, not a shortcut.
			//
			// So the conclusion stands — the run IS no longer being seen, the
			// wire arm is unchanged, the terminal is still minted — and only
			// the level moves, because a warn asks an operator to act on a
			// silent-but-healthy loop that needs nothing.
			//
			// A VANISHED FILE UNDER A STANDING DIRECTORY KEEPS ITS WARN. That
			// one is an unlink under the reader, which is a real anomaly about
			// the file plane rather than an inference about a process.
			t.bound(lost.Work).With(logging.Context{Reason: string(lost.Reason)}).Log(
				"run concluded LOST reason=%s: it stopped writing for longer than its silence window, which is not a claim that it failed and not a fault to act on — a detached run that is quiet and alive looks exactly like this from the file plane (last_activity_ms=%d observed_at_ms=%d)",
				lost.Reason, lost.LastActivityMs, nowMs)
			continue
		}
		t.bound(lost.Work).With(logging.Context{Level: "warn"}).Log(
			"run concluded LOST reason=%s: we stopped seeing it, which is not a claim that it failed (last_activity_ms=%d observed_at_ms=%d)",
			lost.Reason, lost.LastActivityMs, nowMs)
	}
	t.summarizeCatchup(catchup, nowMs)
	return out
}

// catchupCount is one stale class's running tally during a startup catch-up
// sweep: how many pre-existing runs it concluded and the oldest one's clock.
type catchupCount struct {
	count    int
	oldestMs int64
}

func (c *catchupCount) add(activityMs int64) {
	c.count++
	if activityMs < c.oldestMs {
		c.oldestMs = activityMs
	}
}

// summarizeCatchup states ONE INFORMATIONAL summary per stale class the sweep
// caught up on, naming the class, the count and the oldest run's age, so the
// owner sees "N runs concluded" without N lines. Caller holds mu. A sweep that
// caught nothing up states nothing.
//
// IT IS INFO, NOT WARN: a startup catch-up summary is an account of PRE-EXISTING
// backlog a restart re-derived, not a fault the owner must act on, so it must
// not trip a strict harvest. The per-item steady-state conclusions this
// summarizes AWAY stay at their own levels; only this roll-up is informational.
func (t *Tracker) summarizeCatchup(catchup map[Reason]*catchupCount, nowMs int64) {
	reasons := make([]Reason, 0, len(catchup))
	for reason := range catchup {
		reasons = append(reasons, reason)
	}
	sort.Slice(reasons, func(i, j int) bool { return reasons[i] < reasons[j] })
	for _, reason := range reasons {
		c := catchup[reason]
		age := time.Duration(nowMs-c.oldestMs) * time.Millisecond
		t.log.With(logging.Context{
			Operation: "catchup-summary", Level: "info",
			Reason: string(reason), Repeat: logging.Repeat(c.count),
		}).Log("startup catch-up concluded %d pre-existing run(s) LOST reason=%s; the oldest last grew %s ago — these predate this sidecar and are summarized here, not stated one by one",
			c.count, reason, age)
	}
}

// silence is the per-kind window a quiet file is given before it counts as
// silent. Caller holds mu.
func (t *Tracker) silence(kind tail.Kind) time.Duration {
	switch kind {
	case tail.KindShellSpool, tail.KindResidueSpool:
		return t.opt.ShellSilence
	case tail.KindWorkflowJournal:
		return t.opt.WorkflowSilence
	default:
		return t.opt.AgentSilence
	}
}

// bound builds the log context for one run. Caller holds mu.
func (t *Tracker) bound(work Work) *logging.Bound {
	return t.log.With(logging.Context{
		Operation:  "lost-policy",
		Path:       work.Path,
		TaskID:     work.TaskID,
		AgentID:    work.OwnerAgentID,
		ActivityID: work.RunActivityID,
	})
}
