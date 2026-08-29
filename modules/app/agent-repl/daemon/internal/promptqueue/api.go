// Package promptqueue is the ONE delivery path for prompts.
//
// It owns holds, the classifier call, interjection, the parked ledger and the
// drain at turn end. It never imports the merge orchestrator or the drain
// controller; the three meet at the WSM lease and at the shim client. See
// ARCHITECTURE.md "promptqueue".
package promptqueue

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// Submission is one prompt entering the queue.
type Submission struct {
	// WS is the workspace it belongs to.
	WS ids.WorkspaceID
	// Turn is the turn the daemon minted for it.
	Turn ids.TurnID
	// Said is the composed prompt — one canonical form client to daemon to
	// tray to shim to record.
	Said *conversationv1.UserSaid
	// Origin is the send site, REQUIRED: an UNSPECIFIED origin is refused at
	// submission, and the accepted value is persisted onto the turn via
	// StartTurn.
	Origin conversationv1.PromptOrigin
	// Target, when set, is the bubble composer's addressed feed row — the
	// prompt goes to THAT agent through UpdateAgent.prompt.
	Target *feedid.Ref
}

// Disposition is what became of a submission. Exactly one field is set; a hold
// is an ANSWER, not a failure.
type Disposition struct {
	// Delivered reports the prompt went to the shim at once.
	Delivered bool
	// Held is why it was parked instead, nil when it was not.
	Held *wsm.HoldKind
	// RefusedArm names the refusal when the submission was rejected outright
	// (the merge lease's refuse policy), empty otherwise.
	RefusedArm string
}

// Act is a session-level action that travels the same one delivery path as a
// prompt, so it cannot overtake a queued prompt.
type Act struct {
	// Kind names it: "clear", "compact", "set_model", "set_permission_mode".
	Kind string
	// Value is the act's argument — the model id, the permission mode — empty
	// for the argument-less acts.
	Value string
}

// Queue is the delivery path's whole surface.
type Queue interface {
	// Submit runs one submission through recognition-free delivery: the lease
	// policy, the classifier, the hold decision, and the shim call. The
	// disposition is the answer.
	Submit(ctx context.Context, sub Submission) (Disposition, error)
	// Release delivers a held prompt now (UpdateHeldPrompt.release).
	Release(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
	// Drop discards a held prompt (UpdateHeldPrompt.drop).
	Drop(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
	// Accept flips a hold_for_turn_end verdict to accepted and re-pushes the
	// tray (UpdateHeldPrompt.accept). It is LEGAL ONLY on a hold_for_turn_end
	// verdict; every other hold refuses.
	Accept(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
	// SubmitSessionAct sends a session act down the same path.
	SubmitSessionAct(ctx context.Context, ws ids.WorkspaceID, act Act) error
	// OnTurnEnded is the LifecycleSink's turn end: pop the queue and deliver
	// the next prompt.
	OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose)
	// OnLeaseChanged re-evaluates every hold against the new lease policy.
	OnLeaseChanged(ws ids.WorkspaceID)
	// RestoreHolds reloads every standing hold at boot, ALL-OR-NOTHING: a
	// corrupt row fails the restore and nothing is loaded.
	RestoreHolds(ctx context.Context) error
}

// Deps are the queue's collaborators.
type Deps struct {
	// DB is the durable hold store and the lease's policy metadata.
	DB wsm.DB
	// Judge classifies interjection.
	Judge classifier.Judge
	// Feed mirrors an accepted prompt as a user_prompt row.
	Feed feed.Resolver
	// Holds is the tray the queue publishes its holds to.
	Holds holds.Resolver
	// Client resolves a workspace's shim client; the queue never dials one
	// itself.
	Client ClientFunc
	// Log is the queue's logger.
	Log dlog.Surfaces
}

// ClientFunc resolves a workspace's live shim client, reporting false when the
// workspace has none. It is injected so the queue does not own the fleet.
type ClientFunc func(ws ids.WorkspaceID) (Sender, bool)

// Sender is the slice of the shim client the queue uses. Keeping it narrow is
// what lets the queue be tested against a fake without a whole shim.
type Sender interface {
	// StartTurn opens a turn with its minted id and required origin.
	StartTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) error
	// PromptAgent delivers a bubble-composer prompt to one agent.
	PromptAgent(ctx context.Context, agent *conversationv1.AgentId, said *conversationv1.UserSaid) error
	// KillTurn interrupts the open turn for an interjection.
	KillTurn(ctx context.Context, turn ids.TurnID, force bool) error
}

// New builds the queue.
func New(deps Deps) (Queue, error) {
	return nil, notimpl.Err
}
