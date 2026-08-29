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
//  3. SweptUp      — at boot, a run whose file has not been touched since before
//     the machine booted. Nothing survives a reboot.
//
// IT RE-DERIVES FROM FILES AND CURSORS, because there is nothing else left to
// derive from: the store holds no open-task snapshot for the sidecar
// (GetLiveWork is the shim's verb, and the sidecar's only recovery verb is
// GetSidecarCursors). What the sidecar knows is which files exist, when they
// were last written, and how far its cursors have read them — so that is what
// the policy is built out of.
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
}

type entry struct {
	work         Work
	vanishedAtMs int64 // 0 = the file is present
}

// Tracker holds the open runs and concludes LOST. Safe for concurrent use: the
// poll loop and the sweep both touch it.
type Tracker struct {
	mu   sync.Mutex
	open map[string]*entry // by resolved path
	opt  Options
	log  *logging.Bound
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
	return &Tracker{open: map[string]*entry{}, opt: opt, log: log}
}

// Windows reports the windows this tracker actually runs with, defaults filled
// in. It exists so the flag wiring can be asserted where it lands rather than
// where it is parsed: a window that never reached the tracker is a flag that
// does nothing.
func (t *Tracker) Windows() Options { return t.opt }

// Observe records that a run's file is being watched. Re-observing a known run
// refreshes what the reader has since learned about it (its owner, its run
// handle) without disturbing its activity clock.
func (t *Tracker) Observe(work Work, nowMs int64) {
	if work.Path == "" {
		panic("stale: a tracked run must name its file")
	}
	t.mu.Lock()
	defer t.mu.Unlock()
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
func (t *Tracker) MarkVanished(path string, nowMs int64) {
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
	t.bound(existing.work).With(logging.Context{Level: "warn"}).Log(
		"the run's file vanished; the grace window of %s decides whether that is a rename race or a LOST run", t.opt.Grace)
}

// Settle stops tracking a run whose terminal the reader actually READ (a
// spool's EXIT marker, say). A settled run is never swept: LOST is only ever
// the answer for a run we stopped seeing, never for one we saw finish.
func (t *Tracker) Settle(path string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	existing, ok := t.open[path]
	if !ok {
		return
	}
	delete(t.open, path)
	t.bound(existing.work).Log("run settled by a terminal read from its own file; it can no longer be concluded LOST")
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
func (t *Tracker) Sweep(nowMs int64) []Lost {
	t.mu.Lock()
	defer t.mu.Unlock()
	var out []Lost
	for path, existing := range t.open {
		reason, concluded := t.conclude(existing, nowMs)
		if !concluded {
			continue
		}
		delete(t.open, path)
		out = append(out, Lost{Work: existing.work, Reason: reason, ObservedAtMs: nowMs})
	}
	return t.state(out, nowMs)
}

// conclude decides whether one entry's window has expired. Caller holds mu.
func (t *Tracker) conclude(e *entry, nowMs int64) (Reason, bool) {
	if e.vanishedAtMs != 0 && nowMs-e.vanishedAtMs >= t.opt.Grace.Milliseconds() {
		return ReasonFileVanished, true
	}
	if e.vanishedAtMs != 0 {
		return "", false
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
func (t *Tracker) state(out []Lost, nowMs int64) []Lost {
	sort.Slice(out, func(i, j int) bool { return out[i].Path < out[j].Path })
	for _, lost := range out {
		t.bound(lost.Work).With(logging.Context{Level: "warn"}).Log(
			"run concluded LOST reason=%s: we stopped seeing it, which is not a claim that it failed (last_activity_ms=%d observed_at_ms=%d)",
			lost.Reason, lost.LastActivityMs, nowMs)
	}
	return out
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
