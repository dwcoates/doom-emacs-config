// Package merge is the daemon's workspace-merge engine, ported from the
// Emacs producers in merge-handlers.el and worktree.el (the cherry-pick
// driver around agent-repl--merge-handler-cherry-pick /
// agent-repl--workspace-merge-do and the finalize tail around
// agent-repl--finalize-merged-workspace / agent-repl--tag-merge-completion).
//
// Responsibility (design §4.6, §9.3): run the cherry-pick driver via
// `git -C <dir>`, detect conflicts, finalize a merged workspace, and resume
// the cherry-pick after a human resolves a conflict. Emacs keeps only the
// reactive conflict-resolution UX (magit); state ownership lives here.
//
// Every merge-state transition (merging, merge_queued, merge_conflict,
// merge_failed, merged) is emitted through the StateSink defined in this
// file. The SSM binds a real StateSink at stitch; this module never imports
// internal/ssm and never writes to the shim-store — merge state is
// daemon-local by design (the shim ecosystem is agent-interaction-only).
package merge

import (
	"fmt"

	"claude-repld/internal/dlog"
)

// Phase is one merge-state transition the engine emits. The string values
// are the snake_case render-state vocabulary the SSM and the frontend.v1
// RenderState enum share (RENDER_STATE_MERGE_QUEUED etc., lowercased), so a
// transition recorded here resolves to the matching render state without a
// translation table.
type Phase string

const (
	// PhaseMerging: a cherry-pick is in flight for the workspace.
	PhaseMerging Phase = "merging"
	// PhaseMergeQueued: the merge is deferred because another cherry-pick is
	// already in flight against the same target worktree. The per-target
	// serialization queue itself is bound at stitch (the daemon orchestrator);
	// this module only emits the state so the transition is never invisible.
	PhaseMergeQueued Phase = "merge_queued"
	// PhaseMergeConflict: the cherry-pick left a conflict in the target tree.
	// The pick is LEFT IN TREE (never aborted) so a human can resolve it and
	// the resolve-and-continue handoff can resume it.
	PhaseMergeConflict Phase = "merge_conflict"
	// PhaseMergeFailed: git aborted the cherry-pick with no conflict to
	// resolve (the elisp silent-failure sentinel) or a precondition surfaced a
	// hard failure after state began transitioning.
	PhaseMergeFailed Phase = "merge_failed"
	// PhaseMerged: the workspace's commits landed on the target (or were
	// already incorporated); the trunk now carries the work.
	PhaseMerged Phase = "merged"
)

// validPhases gates emit against a typo'd Phase. All in-tree call sites use
// the constants above; this is loud belt-and-suspenders, never a fallback.
var validPhases = map[Phase]bool{
	PhaseMerging:       true,
	PhaseMergeQueued:   true,
	PhaseMergeConflict: true,
	PhaseMergeFailed:   true,
	PhaseMerged:        true,
}

// StateSink receives every merge-state transition the engine produces. The
// SSM implements it at stitch (an append to its state log); nothing here
// imports internal/ssm.
//
// RecordMergeTransition MUST surface any persistence failure as a returned
// error. The engine loud-logs the failure and propagates it — it never
// swallows a sink error and never falls back to a best-effort no-op.
type StateSink interface {
	RecordMergeTransition(ws string, phase Phase, cause string) error
}

// stateEmitter binds a StateSink to the engine's logger so every transition
// is loud-logged with workspace + phase + cause before it hits the sink, and
// a sink failure is loud-logged and returned rather than dropped.
type stateEmitter struct {
	sink StateSink
	logf dlog.Logf
}

// emit loud-logs the transition and records it through the sink. A sink
// error is loud-logged and returned; the caller aborts the operation on it.
func (e *stateEmitter) emit(ws string, phase Phase, cause string) error {
	if !validPhases[phase] {
		// Defensive: an unknown phase means a programming error at the call
		// site. Fail loudly rather than record a garbage state.
		e.logf("merge: REFUSING unknown phase {ws=%s phase=%s cause=%s}", ws, phase, cause)
		return fmt.Errorf("merge: unknown state phase %q for workspace %q", phase, ws)
	}
	e.logf("merge: state {ws=%s phase=%s cause=%s}", ws, phase, cause)
	if err := e.sink.RecordMergeTransition(ws, phase, cause); err != nil {
		e.logf("merge: state SINK FAILED {ws=%s phase=%s cause=%s}: %v", ws, phase, cause, err)
		return fmt.Errorf("merge: record %s transition for %q: %w", phase, ws, err)
	}
	return nil
}
