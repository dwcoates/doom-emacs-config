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

	corev1 "agentrepl/proto/agentshim/core/v1"
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
	t.log.With(logging.Context{Operation: "stale-open", Session: session, Task: id, Path: outputPath}).LogVerbose("open requested kind=%d started_at_ms=%d now_ms=%d", kind, startedAtMs, nowMs)
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
		t.log.With(logging.Context{Operation: "stale-open", Session: session, Task: id, Path: existing.outputPath}).LogVerbose("refreshed existing task")
		return
	}
	t.tasks[key] = &task{
		id: id, kind: kind, session: session, outputPath: outputPath,
		startedAtMs: startedAtMs, lastActMs: nowMs,
	}
	t.log.With(logging.Context{Operation: "stale-open", Session: session, Task: id, Path: outputPath}).Log("tracking new task kind=%d", kind)
}

// Restore replaces the in-memory tracker with the store's authoritative
// persisted open-task set. It validates the complete snapshot before mutation,
// so a malformed persisted lifecycle leaves the prior tracker intact and
// prevents the sidecar link from coming up.
func (t *Tracker) Restore(states []*corev1.OpenTaskState) error {
	t.log.With(logging.Context{Operation: "restore-open-tasks"}).LogVerbose("restore requested states=%d", len(states))
	restored := make(map[taskKey]*task, len(states))
	for _, state := range states {
		if state == nil || state.GetStarted() == nil {
			err := fmt.Errorf("stale: recovery contains an open task with no start event")
			return t.restoreError(logging.Context{}, err)
		}
		ev := state.GetStarted()
		ts := ev.GetTaskStarted()
		if ts == nil {
			err := fmt.Errorf("stale: recovery session=%s seq=%d is not TaskStarted", ev.GetSessionId(), ev.GetSeq())
			return t.restoreError(logging.Context{Session: ev.GetSessionId()}, err)
		}
		if ev.GetSessionId() == "" || ts.GetTaskId() == "" || ev.GetProducedAtMs() <= 0 || state.GetLastActivityAtMs() <= 0 {
			err := fmt.Errorf("stale: invalid recovered TaskStarted session=%q task_id=%q produced_at_ms=%d last_activity_at_ms=%d seq=%d",
				ev.GetSessionId(), ts.GetTaskId(), ev.GetProducedAtMs(), state.GetLastActivityAtMs(), ev.GetSeq())
			return t.restoreError(logging.Context{Session: ev.GetSessionId(), Task: ts.GetTaskId()}, err)
		}
		key := taskKey{session: ev.GetSessionId(), id: ts.GetTaskId()}
		if _, exists := restored[key]; exists {
			err := fmt.Errorf("stale: duplicate recovered task session=%q task_id=%q",
				ev.GetSessionId(), ts.GetTaskId())
			return t.restoreError(logging.Context{Session: ev.GetSessionId(), Task: ts.GetTaskId()}, err)
		}
		kind, err := coreTaskKindToTail(ts.GetKind())
		if err != nil {
			wrapped := fmt.Errorf("stale: invalid recovered TaskStarted session=%q task_id=%q: %w",
				ev.GetSessionId(), ts.GetTaskId(), err)
			return t.restoreError(logging.Context{Session: ev.GetSessionId(), Task: ts.GetTaskId()}, wrapped)
		}
		restored[key] = &task{
			id:          ts.GetTaskId(),
			kind:        kind,
			session:     ev.GetSessionId(),
			outputPath:  ts.GetOutputPath(),
			startedAtMs: ev.GetProducedAtMs(),
			lastActMs:   state.GetLastActivityAtMs(),
		}
	}
	t.mu.Lock()
	t.tasks = restored
	t.restoreFailure = ""
	t.mu.Unlock()
	t.log.With(logging.Context{Operation: "restore-open-tasks"}).Log(
		"restored %d authoritative open task(s) with persisted activity from store", len(restored))
	return nil
}

// restoreError retains one canonical record for an identical invalid snapshot.
// Establishment retries the same authoritative snapshot until the store changes;
// enqueuing the same session diagnostic on every retry would grow the outbox
// forever while the link is necessarily unable to flush it.
func (t *Tracker) restoreError(ctx logging.Context, err error) error {
	fingerprint := ctx.Session + "\x00" + ctx.Task + "\x00" + err.Error()
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
func (t *Tracker) Sweep(nowMs int64) []*corev1.Event {
	t.log.With(logging.Context{Operation: "stale-sweep"}).LogVerbose("sweep requested now_ms=%d", nowMs)
	t.mu.Lock()
	defer t.mu.Unlock()
	var out []*corev1.Event
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
func (t *Tracker) BootSweep(bootMs, nowMs int64) []*corev1.Event {
	t.log.With(logging.Context{Operation: "stale-boot-sweep"}).LogVerbose("boot sweep requested boot_ms=%d now_ms=%d", bootMs, nowMs)
	t.mu.Lock()
	defer t.mu.Unlock()
	var out []*corev1.Event
	for key, tk := range t.tasks {
		if tk.startedAtMs > 0 && tk.startedAtMs < bootMs {
			out = append(out, t.lost(tk, "boot-sweep"))
			delete(t.tasks, key)
		}
	}
	t.log.With(logging.Context{Operation: "stale-boot-sweep"}).LogVerbose("boot sweep complete inferred_lost=%d", len(out))
	return out
}

// lost builds the synthetic-plane LOST TaskEnded and loud-logs the transition.
func (t *Tracker) lost(tk *task, inference string) *corev1.Event {
	// A synthetic LOST is a terminal verdict the user READS: the task is drawn
	// as lost and never reaches DONE.
	t.log.With(logging.Context{Operation: "infer-lost", Task: tk.id, Session: tk.session, Level: "warn"}).Log("LOST kind=%d inference=%s never DONE", tk.kind, inference)
	return &corev1.Event{
		SessionId:    tk.session,
		Plane:        corev1.Plane_PLANE_SYNTHETIC,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: nowMillis(),
		DedupKey:     "task-lost:" + tk.id,
		Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{
			TaskId:     tk.id,
			Kind:       kindToTaskKind(tk.kind),
			Status:     corev1.TerminalStatus_TERMINAL_STATUS_LOST,
			OutputPath: tk.outputPath,
			Inference:  inference,
		}},
	}
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

func coreTaskKindToTail(k corev1.TaskKind) (tail.Kind, error) {
	switch k {
	case corev1.TaskKind_TASK_KIND_SHELL:
		return tail.KindShellSpool, nil
	case corev1.TaskKind_TASK_KIND_WORKFLOW:
		return tail.KindWorkflowJournal, nil
	case corev1.TaskKind_TASK_KIND_AGENT:
		return tail.KindAgentTranscript, nil
	default:
		return 0, fmt.Errorf("unsupported task kind %s", k)
	}
}

func kindToTaskKind(k tail.Kind) corev1.TaskKind {
	switch k {
	case tail.KindShellSpool:
		return corev1.TaskKind_TASK_KIND_SHELL
	case tail.KindWorkflowJournal:
		return corev1.TaskKind_TASK_KIND_WORKFLOW
	case tail.KindAgentTranscript:
		return corev1.TaskKind_TASK_KIND_AGENT
	default:
		return corev1.TaskKind_TASK_KIND_UNSPECIFIED
	}
}

// nowMillis is overridable in tests.
var nowMillis = func() int64 { return time.Now().UnixMilli() }
