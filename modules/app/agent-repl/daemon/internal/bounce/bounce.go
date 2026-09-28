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
	"errors"

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
	// ReplacesShim marks a bounce whose Run replaces the workspace's SHIM
	// with a fresh one (a stale build, the restart verb, a log at its
	// ceiling), as opposed to moving the workspace elsewhere (a handover
	// transfer). It decides what the shim DEPARTING under a registered bounce
	// means: the work the bounce waited on has ended either way, but a
	// replacement is only wanted while the workspace is open and the shim
	// died on its own. One whose session this daemon ended itself, or whose
	// workspace is closed, is UNREGISTERED instead (ErrUnregistered): the
	// next bring-up, if any, spawns the installed build anyway, and a relaunch
	// would revive a workspace nobody asked to be running.
	ReplacesShim bool
	// WaitFor is the gate the request waits on before it runs. The zero value
	// is GateFreeness. Only a MOVE (KeepDraining) may ask for
	// GateDispatchQuiet.
	WaitFor Gate
	// Done, when set, is told how the bounce ended. It is called once, after
	// the workspace has left draining (or, with KeepDraining, after Run), or
	// with ErrUnregistered when the registry dropped the bounce unrun.
	Done func(error)
}

// Gate is what a bounce waits on before the registry runs it.
//
// A HANDOVER NEVER WAITS ON WORK; A SHIM REPLACEMENT DOES (owner ruling,
// 2026-09-27). Replacing a shim ends everything that runs inside its vendor
// child, so it waits for the workspace to fall free. Moving a workspace to
// another daemon ends nothing: the shim keeps running, detached, and the
// daemon it moves to adopts it mid-turn. So a move waits only until no
// DELIVERY is in flight, which the registry's own per-workspace delivery lock
// already guarantees at the instant it decides.
type Gate int

// The gates.
const (
	// GateFreeness waits for no turn in flight and no live detached work: the
	// shim-replacement gate, and the zero value.
	GateFreeness Gate = iota
	// GateDispatchQuiet waits only for the delivery lock: no StartTurn is
	// mid-flight when the bounce is decided, and none can start after it.
	// Turns and detached work run on through it.
	GateDispatchQuiet
)

// String names a gate for the records.
func (g Gate) String() string {
	switch g {
	case GateFreeness:
		return "freeness"
	case GateDispatchQuiet:
		return "dispatch_quiet"
	default:
		return "unknown"
	}
}

// ErrHandedAcross is what Done is told for a shim REPLACEMENT a dispatch-quiet
// move carried to the daemon it took the workspace to: the move ran without
// waiting for the replacement's freeness, and the replacement runs on that
// daemon after its adoption, at ITS freeness (or at once, when forced). It is
// an outcome, not a failure.
var ErrHandedAcross = errors.New("bounce: handed across; the daemon the workspace moved to runs the replacement after its adoption")

// ErrMovedAway refuses a request against a workspace whose move has already
// SEALED what it carries to the next daemon: nothing asked of this daemon now
// can reach that daemon, so the caller asks the daemon the workspace moved to.
// The transport answers it as `transferring_away`.
var ErrMovedAway = errors.New("bounce: the workspace is moving to another daemon; ask the daemon it moved to")

// Handoff is what a workspace's prompt queue held ONLY IN MEMORY when a
// dispatch-quiet move sealed it: the part of the queue's state that is not a
// durable row and would otherwise die with this daemon while the work it
// orders is still running on the adopted shim. The move carries it to the
// daemon it takes the workspace to, which installs it before it dials the
// shim.
type Handoff struct {
	// Acts are the session acts queued behind the running work, in
	// submission order.
	Acts []HandoffAct `json:"acts,omitempty"`
	// Cut is the context cut (/clear, /compact) that IS the running turn, nil
	// when none is.
	Cut *HandoffCut `json:"cut,omitempty"`
	// Head is the held prompt an interjection moved to the SEMANTIC HEAD: its
	// interrupt was sent, and it is delivered first at the running turn's end.
	Head string `json:"head,omitempty"`
	// Interrupting reports the footer's waiting-interrupting status.
	Interrupting bool `json:"interrupting,omitempty"`
}

// HandoffAct is one queued session act.
type HandoffAct struct {
	Kind   string `json:"kind"`
	Value  string `json:"value,omitempty"`
	Turn   string `json:"turn,omitempty"`
	Origin int32  `json:"origin,omitempty"`
}

// HandoffCut is the running context cut: its turn and its command.
type HandoffCut struct {
	Turn    string `json:"turn"`
	Command int32  `json:"command"`
}

// Empty reports whether the handoff carries nothing.
func (h Handoff) Empty() bool {
	return len(h.Acts) == 0 && h.Cut == nil && h.Head == "" && !h.Interrupting
}

// ErrUnregistered is what Done is told for a registered bounce the registry
// DROPPED UNRUN: the shim it would have replaced departed, and nothing is
// left for it to replace (see Request.ReplacesShim). It is an outcome, not a
// failure.
var ErrUnregistered = errors.New("bounce: unregistered; the shim it would replace departed and nothing is left to replace")

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
