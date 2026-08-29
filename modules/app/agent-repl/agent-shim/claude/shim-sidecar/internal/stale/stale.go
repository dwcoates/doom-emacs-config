// Package stale implements the sidecar's completion-inference / staleness policy
// (design §7.4). No terminal markers exist on disk, so open detached tasks are
// resolved to the terminal status LOST — NEVER DONE — via three explicit,
// loud-logged inferences:
//
//  1. vanished-file: a watched task file disappears while its task is open →
//     after a grace period (default 30s) → LOST.
//  2. silence-timeout: no new bytes for a per-kind window (shell 30m, agent 60m,
//     workflow 60m) → LOST. The sidecar cannot observe stream-plane liveness, so
//     it emits LOST and lets the store/daemon reconcile against any stream
//     terminal (a real DONE from the stream already sits in the store).
//  3. boot-sweep: at startup, an open task whose started_at predates the current
//     boot time → LOST (nothing survives a reboot).
//
// LOST is a synthetic-plane inference and is never conflated with DONE.
package stale

import (
	"fmt"
	"sync"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Default grace / silence windows (§7.4), overridable via Options.
const (
	DefaultGrace           = 30 * time.Second
	DefaultShellSilence    = 30 * time.Minute
	DefaultAgentSilence    = 60 * time.Minute
	DefaultWorkflowSilence = 60 * time.Minute
)

// Options tunes the tracker's windows. Zero values fall back to the defaults.
type Options struct {
	Grace           time.Duration
	ShellSilence    time.Duration
	AgentSilence    time.Duration
	WorkflowSilence time.Duration
}

type task struct {
	id           string
	kind         tail.Kind
	session      string
	outputPath   string
	startedAtMs  int64
	lastActMs    int64
	vanishedAtMs int64 // 0 = file present
}

// taskKey is the store's lifecycle identity.  Task ids originate in vendor
// payloads and are only unique within a conversation; treating task_id alone
// as global made recovery reject two perfectly valid open tasks from separate
// sessions and kept the whole sidecar link down forever.
type taskKey struct {
	session string
	id      string
}

// Tracker tracks open detached tasks and infers LOST transitions. Safe for
// concurrent use (the poll loop and the sweep timer both touch it).
type Tracker struct {
	mu             sync.Mutex
	tasks          map[taskKey]*task
	restoreFailure string
	opt            Options
	log            *logging.Bound
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
	log.With(logging.Context{Operation: "stale-new"}).LogVerbose("constructing tracker grace=%s shell_silence=%s agent_silence=%s workflow_silence=%s", opt.Grace, opt.ShellSilence, opt.AgentSilence, opt.WorkflowSilence)
	return &Tracker{tasks: map[taskKey]*task{}, opt: opt, log: log}
}

// Open registers (or refreshes) an open task. startedAtMs is the launch/observed
// time; nowMs seeds last-activity.
func (t *Tracker) Open(id string, kind tail.Kind, session, outputPath string, startedAtMs, nowMs int64) {
	t.log.With(logging.Context{Operation: "stale-open", VendorSessionID: session, TaskID: id, Path: outputPath}).LogVerbose("open requested kind=%d started_at_ms=%d now_ms=%d", kind, startedAtMs, nowMs)
	if id == "" || session == "" {
		err := fmt.Sprintf("stale: task identity is required session=%q task_id=%q", session, id)
		// An incomplete identity cannot be routed to a session diagnostic.
		t.log.With(logging.Context{Operation: "stale-open", Path: outputPath, Level: "error"}).Log("%s", err)
		panic(err)
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	key := taskKey{session: session, id: id}
	if existing, ok := t.tasks[key]; ok {
		if outputPath != "" {
			existing.outputPath = outputPath
		}
		t.log.With(logging.Context{Operation: "stale-open", VendorSessionID: session, TaskID: id, Path: existing.outputPath}).LogVerbose("refreshed existing task")
		return
	}
	t.tasks[key] = &task{
		id: id, kind: kind, session: session, outputPath: outputPath,
		startedAtMs: startedAtMs, lastActMs: nowMs,
	}
	t.log.With(logging.Context{Operation: "stale-open", VendorSessionID: session, TaskID: id, Path: outputPath}).Log("tracking new task kind=%d", kind)
}

// Restore resets the in-memory tracker at the start of every established
// connection.
//
// IT HAS NOTHING LEFT TO RESTORE FROM, AND THE REASON IS A SCHEMA HOLE RATHER
// THAN A FAILURE. The store used to hand back an authoritative open-task set
// (agentshim.v1 CursorList.open_tasks, each an OpenTaskState). The redesigned
// contract deleted OpenTaskState AND CursorList: store.v1
// GetSidecarCursorsResponse returns cursors and nothing else, so the store no
// longer reports open tasks at all and there is no snapshot to key entries on.
//
// The consequences are stated rather than smoothed over, because every one of
// them is a real behavior change:
//
//   - A task open when this process restarts is not tracked, so it is never
//     LOST-swept. It sits in the feed as running until something else ends it.
//   - The boot sweep has nothing to sweep, so tasks killed by a reboot stay
//     running rather than being resolved to LOST.
//   - The spool-owner index cannot be seeded (see the sidecar's seedOwners), so
//     a live task's spool stays unattributed until the transcript that announced
//     it is re-read.
//
// The error return is KEPT rather than dropped as now-unreachable: the reset is
// still a step establishment must be able to refuse, and restoreError is still
// how a refusal reaches the link exactly once.
func (t *Tracker) Restore() error {
	t.log.With(logging.Context{Operation: "restore-open-tasks"}).LogVerbose("restore requested")
	// Loud, and at error level, because this is silent data loss in the user's
	// feed: work that was running is now untracked and will never be resolved to
	// a terminal status by this process.
	t.log.With(logging.Context{Operation: "restore-open-tasks", Level: "error"}).Log(
		"store.v1 reports no open tasks (OpenTaskState and CursorList were deleted with no successor); " +
			"tasks open across this restart are neither tracked nor swept, and their spools stay unattributed until their transcripts are re-read")
	t.mu.Lock()
	t.tasks = map[taskKey]*task{}
	t.restoreFailure = ""
	t.mu.Unlock()
	t.log.With(logging.Context{Operation: "restore-open-tasks"}).Log("open-task tracker reset; no persisted open tasks are recoverable")
	return nil
}

// restoreError retains one canonical record for an identical invalid snapshot.
// Establishment retries the same authoritative snapshot until the store changes;
// enqueuing the same session diagnostic on every retry would grow the outbox
// forever while the link is necessarily unable to flush it.
func (t *Tracker) restoreError(ctx logging.Context, err error) error {
	fingerprint := ctx.VendorSessionID + "\x00" + ctx.TaskID + "\x00" + err.Error()
	t.mu.Lock()
	repeated := t.restoreFailure == fingerprint
	if !repeated {
		t.restoreFailure = fingerprint
	}
	t.mu.Unlock()
	if !repeated {
		ctx.Operation = "restore-open-tasks"
		ctx.Level = "error"
		t.log.With(ctx).Log("recovery validation failed: %v", err)
	}
	return err
}

// Activity records that a task's file produced new bytes at nowMs and clears any
// vanish timer (the file is present again).
func (t *Tracker) Activity(session, id string, nowMs int64) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if tk, ok := t.tasks[taskKey{session: session, id: id}]; ok {
		tk.lastActMs = nowMs
		tk.vanishedAtMs = 0
	}
}

// MarkVanished notes that a task's file first went missing at nowMs (starts the
// grace clock). Repeated calls keep the earliest vanish time.
func (t *Tracker) MarkVanished(session, id string, nowMs int64) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if tk, ok := t.tasks[taskKey{session: session, id: id}]; ok && tk.vanishedAtMs == 0 {
		tk.vanishedAtMs = nowMs
	}
}

// MarkPresent clears a vanish timer (the file reappeared before grace elapsed).
func (t *Tracker) MarkPresent(session, id string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if tk, ok := t.tasks[taskKey{session: session, id: id}]; ok {
		tk.vanishedAtMs = 0
	}
}

// Close removes a task that reached a real terminal elsewhere (a TaskStop twin
// or a stream terminal); its slot is freed so it is never LOST-swept.
func (t *Tracker) Close(session, id string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	delete(t.tasks, taskKey{session: session, id: id})
}

// Open reports whether id is currently tracked (test/introspection helper).
func (t *Tracker) IsOpen(session, id string) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	_, ok := t.tasks[taskKey{session: session, id: id}]
	return ok
}

// Sweep evaluates every open task against the vanish-grace and silence windows,
// emits a LOST TaskEnded for each that crossed a threshold, and closes them.
func (t *Tracker) Sweep(nowMs int64) []*storev1.StoreEntry {
	t.log.With(logging.Context{Operation: "stale-sweep"}).LogVerbose("sweep requested now_ms=%d", nowMs)
	t.mu.Lock()
	defer t.mu.Unlock()
	var out []*storev1.StoreEntry
	for key, tk := range t.tasks {
		switch {
		case tk.vanishedAtMs != 0 && nowMs-tk.vanishedAtMs >= t.opt.Grace.Milliseconds():
			out = append(out, t.lost(tk, "vanished-file"))
			delete(t.tasks, key)
		case nowMs-tk.lastActMs >= t.silence(tk.kind).Milliseconds():
			out = append(out, t.lost(tk, "silence-timeout"))
			delete(t.tasks, key)
		}
	}
	t.log.With(logging.Context{Operation: "stale-sweep"}).LogVerbose("sweep complete inferred_lost=%d", len(out))
	return out
}

// BootSweep LOSTs every open task whose started_at predates bootMs (nothing
// survives a reboot). Run once at startup.
func (t *Tracker) BootSweep(bootMs, nowMs int64) []*storev1.StoreEntry {
	t.log.With(logging.Context{Operation: "stale-boot-sweep"}).LogVerbose("boot sweep requested boot_ms=%d now_ms=%d", bootMs, nowMs)
	t.mu.Lock()
	defer t.mu.Unlock()
	var out []*storev1.StoreEntry
	for key, tk := range t.tasks {
		if tk.startedAtMs > 0 && tk.startedAtMs < bootMs {
			out = append(out, t.lost(tk, "boot-sweep"))
			delete(t.tasks, key)
		}
	}
	t.log.With(logging.Context{Operation: "stale-boot-sweep"}).LogVerbose("boot sweep complete inferred_lost=%d", len(out))
	return out
}

// lost builds the LOST end-of-work record and loud-logs the transition.
//
// LOST IS ITS OWN OUTCOME AND IS NEVER FOLDED INTO FAILURE. We do not know that
// the work died; we know only that we cannot see it any more. The inference is
// carried so a reader can tell "we watched it exit" from "we stopped hearing
// from it", and so a wrong threshold is diagnosable rather than merely wrong.
func (t *Tracker) lost(tk *task, inference string) *storev1.StoreEntry {
	t.log.With(logging.Context{Operation: "infer-lost", TaskID: tk.id, VendorSessionID: tk.session, Level: "warn"}).
		Log("LOST kind=%d inference=%s; never reported as succeeded", tk.kind, inference)
	return convert.DetachedLost(convert.Attribution{
		SessionID:    tk.session,
		Path:         tk.outputPath,
		ProducedAtMs: nowMillis(),
	}, tk.id, inference)
}

func (t *Tracker) silence(k tail.Kind) time.Duration {
	switch k {
	case tail.KindShellSpool:
		return t.opt.ShellSilence
	case tail.KindWorkflowJournal:
		return t.opt.WorkflowSilence
	default:
		return t.opt.AgentSilence
	}
}

// nowMillis is overridable in tests.
var nowMillis = func() int64 { return time.Now().UnixMilli() }
