// Package prompthandler is SubmitPrompt's body: command recognition, the feed
// mirror, and the forward into the queue.
//
// The CLIENT does not recognize slash commands — recognition is the daemon's,
// transparently, and the success oneof is where the answer forks. `/agents`
// and `/help` ARE recognized and never forwarded: they answer as a refusal
// with the add-support offer. See ARCHITECTURE.md's Q1/Q3 rulings.
package prompthandler

import (
	"context"
	"errors"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/wsm"
)

// The typed refusals this package answers with.
//
// THE SERVER MAPS THESE ONTO THE LANDED ERROR ARMS, one for one, and this is
// the one place the mapping is written down:
//
//	ErrOriginRequired      → Connect InvalidArgument (a validation failure, never an arm)
//	ErrFeedNotInWorkspace  → SubmitPromptError.feed_not_in_workspace
//	ErrDuplicateSubmission → no landed arm; see daemon/ERROR-ARMS.md
//
// `feed_undecodable` is deliberately NOT produced here: the server decodes a
// FeedId before it reaches this package, so a value that does not decode never
// becomes a handler call.
//
// The queue's own refusals (merging, no_session) travel through unchanged and
// are mapped where promptqueue documents them.
var (
	// ErrOriginRequired is a submission carrying PROMPT_ORIGIN_UNSPECIFIED. It
	// is refused HERE, before anything is minted or mirrored.
	ErrOriginRequired = errors.New("prompthandler: the prompt origin is required")
	// ErrFeedNotInWorkspace is an echoed FeedId belonging to another workspace.
	ErrFeedNotInWorkspace = errors.New("prompthandler: the addressed feed belongs to another workspace")
	// ErrDuplicateSubmission is a retried request whose idempotency key is
	// already claimed. The claim answers with the turn already minted, so the
	// retry is not a second turn.
	ErrDuplicateSubmission = errors.New("prompthandler: that idempotency key already claimed a turn")
)

// Recognition is what the daemon made of a submission.
type Recognition int

// The recognitions.
const (
	// RecognizedNone is an ordinary prompt: forward it to the queue.
	RecognizedNone Recognition = iota
	// RecognizedPanel is a command the daemon answers itself with a panel.
	RecognizedPanel
	// RecognizedRefused is a command the daemon recognizes and neither answers
	// nor forwards (`/agents`, `/help`, and any unsupported command).
	RecognizedRefused
	// RecognizedAct is a command the daemon carries down the queue's ONE
	// delivery path as a session act (`/clear`, `/compact`, `/model <arg>`).
	//
	// SEAM ADDITION (recorded): the skeleton had three recognitions, but the
	// session-acting commands are neither a panel nor a refusal nor an
	// ordinary prompt, and the two context cuts DO mint a turn while a model
	// change does not. See the report's escalation on the response arm.
	RecognizedAct
)

// Outcome is SubmitPrompt's answer, before it is encoded into the response
// oneof. Exactly one of the shapes is populated.
type Outcome struct {
	// Recognition says which shape this is.
	Recognition Recognition
	// Turn is the minted turn, set when Recognition is RecognizedNone and when
	// a RecognizedAct is a context cut (which the vendor runs as a turn).
	Turn ids.TurnID
	// Panel is the answered panel, set when Recognition is RecognizedPanel. It
	// is ALSO mirrored into the root feed as a non-durable row.
	Panel *agentreplv1.SubmitPromptCommandPanel
	// RefusedCommand is the literal command as typed, set when Recognition is
	// RecognizedRefused. Its refusal card is mirrored into the root feed too.
	RefusedCommand string
	// Act is the session act a RecognizedAct submitted, for the record.
	Act promptqueue.Act
	// Disposition is the queue's answer for a forwarded prompt.
	Disposition promptqueue.Disposition
}

// Handler is SubmitPrompt's body.
type Handler interface {
	// Submit runs one submission: recognize it, mirror what it produced into
	// the root feed, and forward an ordinary prompt to the queue. origin is
	// REQUIRED — an UNSPECIFIED origin is refused here, before anything is
	// minted or mirrored. feed, when set, addresses a subagent bubble's
	// composer; recognition is unchanged either way.
	Submit(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, idempotencyKey string, origin conversationv1.PromptOrigin, target *feedid.Ref) (Outcome, error)
	// Recognize reports what the daemon makes of a submission's text without
	// acting on it. It is exported so the recognition table has one home and
	// one test surface.
	Recognize(text string) (Recognition, string)
}

// Deps are the handler's collaborators.
type Deps struct {
	// Queue is where an ordinary prompt goes.
	Queue promptqueue.Queue
	// Feed mirrors panels, refusals and accepted prompts.
	Feed feed.Resolver
	// DB claims the client's idempotency key — so a retried request is not a
	// second turn — and resolves the workspace whose log sink the handler
	// writes to.
	DB wsm.DB
	// Panels resolves a recognized panel command. It is a FUNCTION because the
	// four panels are assembled from facts this package does not own (the
	// session's status, the tracker's checklist, the MCP roster, the context
	// tree), and the server that owns them wires it.
	//
	// SEAM ADDITION (recorded): the skeleton named no panel source, and a
	// panel command cannot be answered without one.
	Panels PanelFunc
	// MintTurn mints the turn the daemon acknowledges with. nil means
	// wsm.NewTurnID.
	MintTurn func() ids.TurnID
	// MovedOn, when set, is told each time the queue ACCEPTED a submission of
	// the workspace's own (a prompt or a session act): the workspace has moved
	// on, which is what retires a concluded merge's standing state
	// (merge.Orchestrator.RetireConcluded). nil tells nobody.
	MovedOn func(ctx context.Context, ws ids.WorkspaceID)
	// Log is the handler's logger.
	Log dlog.Surfaces
}

// PanelFunc resolves one recognized panel command for a workspace.
type PanelFunc func(ctx context.Context, ws ids.WorkspaceID, command conversationv1.SessionCommand) (*agentreplv1.SubmitPromptCommandPanel, error)

// New builds the handler.
func New(deps Deps) (Handler, error) {
	return newHandler(deps)
}
