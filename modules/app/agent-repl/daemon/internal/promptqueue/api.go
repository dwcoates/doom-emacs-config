// Package promptqueue is the ONE delivery path for prompts.
//
// It owns holds, the classifier call, interjection, the parked ledger and the
// drain at turn end. It never imports the merge orchestrator or the drain
// controller; the three meet at the WSM lease and at the shim client. See
// ARCHITECTURE.md "promptqueue".
package promptqueue

import (
	"context"
	"errors"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// The typed refusals this package answers with. Each is a fact about the
// SUBMISSION, never about the queue's contents: a hold is an answer, not one of
// these.
//
// THE SERVER MAPS THESE ONTO THE LANDED ERROR ARMS, one for one, and this is
// the one place the mapping is written down:
//
//	ErrMerging           → SubmitPromptError.merging
//	ErrNoSession         → SubmitPromptError.no_session
//	ErrNoSuchHold        → UpdateHeldPromptError.no_such_hold
//	ErrAlreadyDelivered  → UpdateHeldPromptError.already_delivered
//	ErrAcceptNotApplicable → UpdateHeldPromptError.accept_not_applicable
//	ErrReleaseRefused    → UpdateHeldPromptError.release_refused
var (
	// ErrMerging is a submission that arrived AFTER a merge began. It is
	// refused outright rather than held: a merged workspace closes, so work a
	// post-merge-start prompt produced would be orphaned.
	ErrMerging = errors.New("promptqueue: a merge is in flight for this workspace")
	// ErrNoSession is a submission to a workspace with no live shim.
	ErrNoSession = errors.New("promptqueue: the workspace has no session to submit to")
	// ErrNoSuchHold names a turn the queue holds nothing under.
	ErrNoSuchHold = errors.New("promptqueue: no hold stands under that turn")
	// ErrAlreadyDelivered is an action on a hold that already went to the shim.
	ErrAlreadyDelivered = errors.New("promptqueue: that hold was already delivered")
	// ErrAcceptNotApplicable is an accept on a verdict other than
	// hold_for_turn_end. The landed contract makes accept legal only there.
	ErrAcceptNotApplicable = errors.New("promptqueue: accept is legal only on a hold_for_turn_end verdict")
	// ErrReleaseRefused is a force-through on an uninterruptible verdict or a
	// session_starting hold: there is nothing delivery could do yet.
	ErrReleaseRefused = errors.New("promptqueue: this hold cannot be released through")
)

// The session-act kinds the one delivery path carries. They are constants
// because internal/workspace spells the same two at its call sites and the two
// spellings must not drift.
const (
	// ActClear is /clear: the context cut with nothing left in its place.
	ActClear = "clear"
	// ActCompact is /compact: the context cut that summarizes.
	ActCompact = "compact"
	// ActSetModel is a model change, from /model <arg> or from the picker.
	ActSetModel = "set_model"
	// ActSetPermissionMode is a permission-mode change.
	ActSetPermissionMode = "set_permission_mode"
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

// Disposition is what became of a submission. A hold is an ANSWER, not a
// failure.
type Disposition struct {
	// Delivered reports the prompt went to the shim at once.
	Delivered bool
	// Held is the DAEMON-SIDE condition it was parked under, nil when no such
	// condition applies.
	Held *wsm.HoldKind
	// Classification is the verdict the prompt was parked under, nil when it
	// was not parked by one.
	//
	// SEAM ADDITION (recorded): the skeleton described a hold with `Held`
	// alone, but the two facts are orthogonal in wsm and a classification hold
	// carries no HoldKind at all — with `Held` as the only signal, a hold for
	// a running turn was indistinguishable from a delivery.
	Classification *wsm.Classification
	// RefusedArm names the refusal when the submission was rejected outright
	// (the merge lease's refuse policy), empty otherwise.
	RefusedArm string
}

// Parked reports that the submission was held rather than delivered or
// refused. It is the one place the three-way answer is read off the fields, so
// no caller re-derives it.
func (d Disposition) Parked() bool {
	return !d.Delivered && d.RefusedArm == ""
}

// Act is a session-level action that travels the same one delivery path as a
// prompt, so it cannot overtake a queued prompt.
type Act struct {
	// Kind names it: ActClear, ActCompact, ActSetModel, ActSetPermissionMode.
	Kind string
	// Value is the act's argument — the model id, the permission mode — empty
	// for the argument-less acts.
	Value string
	// Turn is the turn a context-cutting act runs as, minted by the caller so
	// SubmitPrompt can acknowledge it. Empty means the queue mints one, which
	// is what the picker-driven setter acts do.
	//
	// SEAM ADDITION (recorded): /clear and /compact reach the vendor as turns,
	// and SubmitPromptSuccess.turn is what the composer matches its own row
	// against.
	Turn ids.TurnID
	// Origin is the send site of a context-cutting act, which is delivered as
	// a turn and so needs the same durable origin a prompt does. It is unused
	// by the two setter acts.
	//
	// SEAM ADDITION (recorded): /clear and /compact reach the vendor as
	// StartTurn calls, and StartTurn.origin is required.
	Origin conversationv1.PromptOrigin
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
	// Footer carries the waiting-interrupting status, which fires the MOMENT
	// an interrupt registers rather than when the turn actually ends.
	Footer footer.Resolver
	// Holds is the tray the queue publishes its holds to.
	Holds holds.Resolver
	// Client resolves a workspace's shim client; the queue never dials one
	// itself.
	Client ClientFunc
	// Watcher resolves a workspace's session watcher, which is what answers
	// the in-flight turn and what an accepted turn is handed over to.
	Watcher WatcherFunc
	// ParkedRoute delivers a submission that arrived under a PARKED merge
	// lease to the resolution agent as guidance. It is a FUNCTION, not the
	// orchestrator, because the queue never imports merge.
	ParkedRoute ParkedRouter
	// DrainRefusals records each submission the drain lease refused or held.
	// The controller rate-limits its own record; the queue only reports.
	DrainRefusals RefusalNoter
	// OneShotFinish is the turn-terminal hook for a one-shot workspace's
	// finish action. It is a function because internal/workspace imports this
	// package, so this package cannot import it back.
	OneShotFinish FinishHook
	// StripSentinels removes the host's metaprompt sentinel spans from the
	// MIRRORED text; the full text stays on the durable record and on the
	// prompt the shim receives. nil is the identity.
	StripSentinels func(string) string
	// Now supplies the instants the queue stamps. nil means time.Now.
	Now func() time.Time
	// Log is the queue's logger.
	Log dlog.Surfaces
}

// ClientFunc resolves a workspace's live shim client, reporting false when the
// workspace has none. It is injected so the queue does not own the fleet.
type ClientFunc func(ws ids.WorkspaceID) (Sender, bool)

// Sender is the slice of the shim client the queue uses. Keeping it narrow is
// what lets the queue be tested against a fake without a whole shim.
type Sender interface {
	// StartTurn opens a turn with its minted id and required origin. It
	// answers the shim's success, whose `prompt` names the main agent and
	// whose `page` is the opening page.
	//
	// SEAM CHANGE (recorded): the skeleton returned only an error. The queue
	// must hand StartTurnSuccess.prompt.agent to the watcher's SetMainAgent
	// and the pair to OnTurnOpened — ARCHITECTURE names the queue as the
	// authoritative source of both — so the success has to come back here.
	StartTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) (*shimv1.StartTurnSuccess, error)
	// PromptAgent delivers a bubble-composer prompt to one agent.
	PromptAgent(ctx context.Context, agent *conversationv1.AgentId, said *conversationv1.UserSaid) error
	// KillTurn interrupts the open turn for an interjection.
	KillTurn(ctx context.Context, turn ids.TurnID, force bool) error
	// SetModel switches the session's model. It rides this interface because a
	// model change resolves at the turn boundary and respects the lease like
	// any other delivery.
	SetModel(ctx context.Context, model string) error
	// SetPermissionMode switches the session's permission mode, for the same
	// reason.
	SetPermissionMode(ctx context.Context, mode string) error
}

// WatcherFunc resolves a workspace's session watcher, reporting false when the
// workspace has none. sessionwatcher.Watcher satisfies Watcher.
type WatcherFunc func(ws ids.WorkspaceID) (Watcher, bool)

// Watcher is the slice of the session watcher the queue uses.
type Watcher interface {
	// TurnInFlight reports the open turn, nil when none is.
	TurnInFlight() *ids.TurnID
	// SetMainAgent names the session's main agent from an accepted turn.
	SetMainAgent(agent *conversationv1.AgentId)
	// OnTurnOpened hands an accepted turn over: the prompt as delivered and
	// the opening page StartTurnSuccess carried.
	OnTurnOpened(ws ids.WorkspaceID, prompt *conversationv1.AgentPrompt, page *conversationv1.HistoryPage)
}

// ParkedRouter delivers one parked submission to the resolution agent as
// guidance, addressed at the parked tab. It answers with the turn the guidance
// runs as. It matches merge.ParkedRouter so the wiring is a direct assignment.
type ParkedRouter func(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid) (ids.TurnID, error)

// RefusalNoter records one drain-refused submission. drain.Controller
// satisfies it.
type RefusalNoter interface {
	// NoteRefusal records that one submission was refused or held under the
	// drain lease.
	NoteRefusal(ws ids.WorkspaceID)
}

// FinishHook is the one-shot workspace's turn-terminal finish action.
// workspace.Verbs.OnOneShotTurnConcluded satisfies it.
type FinishHook func(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error

// New builds the queue.
func New(deps Deps) (Queue, error) {
	return newQueue(deps)
}
