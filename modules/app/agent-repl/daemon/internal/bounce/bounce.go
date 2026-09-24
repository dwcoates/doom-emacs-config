// Package bounce is the vocabulary of the per-workspace BOUNCE REGISTRY: the
// request a rollout makes to replace what serves a workspace, and the decision
// the workspace's prompt queue answers it with.
//
// THE PROMPT QUEUE OWNS THE DECISION (owner design, 2026-09-23). The queue is
// what dispatches a workspace's turns, so it is the one place where "the
// workspace is free" and "bounce it now" can be decided without a queued
// prompt starting a turn in between: both happen under the queue's own
// per-workspace delivery lock, and a workspace it decides to bounce moves to
// DRAINING, where nothing is dispatched until the bounce has finished.
//
// A workspace with nothing in flight — no turn, no detached work (background
// subagents, background shells, monitors) — is bounced AT ONCE. One with work
// in flight is REGISTERED, and the registry is checked on the daemon's own
// freeness edges (a turn's end, a detached item's end), never by polling.
// QUEUED PROMPTS NEVER BLOCK A BOUNCE: they stay queued and are delivered to
// whatever serves the workspace once the bounce is done.
//
// It is a leaf of its own so the queue, which decides, and the rollout, which
// asks, share one spelling without importing each other.
package bounce

import (
	"context"

	"claude-repld/internal/ids"
)

// Func performs a bounce once the workspace is drained: it replaces what
// serves the workspace (a stale shim stood down and a fresh one resumed; a
// workspace transferred to a successor daemon). It runs on its own goroutine,
// OFF the queue's delivery lock, and nothing is dispatched to the workspace
// while it runs.
type Func func(ctx context.Context, ws ids.WorkspaceID) error

// Request asks the registry to bounce one workspace.
type Request struct {
	// Reason names why, for every record the bounce writes. REQUIRED.
	Reason string
	// Force bounces at once even with work in flight: a turn, and every
	// detached item that runs inside the shim's vendor child, end with it. A
	// forced deploy is the only thing that sets it.
	Force bool
	// Run performs the bounce. REQUIRED.
	Run Func
	// KeepDraining leaves the workspace DRAINING after a successful Run: the
	// workspace no longer belongs to this daemon (a handover transfer), and its
	// queued intake is the successor's to deliver.
	KeepDraining bool
	// Done, when set, is told how the bounce ended. It is called once, after
	// the workspace has left draining (or, with KeepDraining, after Run).
	Done func(error)
}

// Decision is what the registry did with a request.
type Decision struct {
	// Now reports that the bounce was started at once.
	Now bool
	// Forced reports that it was started at once over work in flight.
	Forced bool
	// AlreadyPending reports that a bounce was already registered or running
	// for the workspace; the request joined it rather than starting a second.
	AlreadyPending bool
	// TurnInFlight and DetachedWork are what a REGISTERED bounce waits on (and,
	// for a forced one, what it ended).
	TurnInFlight bool
	DetachedWork int
}
