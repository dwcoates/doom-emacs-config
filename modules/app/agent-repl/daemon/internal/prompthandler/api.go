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

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/feed"
)

// Recognition is what the daemon made of a submission.
type Recognition int

// The three recognitions.
const (
	// RecognizedNone is an ordinary prompt: forward it to the queue.
	RecognizedNone Recognition = iota
	// RecognizedPanel is a command the daemon answers itself with a panel.
	RecognizedPanel
	// RecognizedRefused is a command the daemon recognizes and neither answers
	// nor forwards (`/agents`, `/help`, and any unsupported command).
	RecognizedRefused
)

// Outcome is SubmitPrompt's answer, before it is encoded into the response
// oneof. Exactly one of the three shapes is populated.
type Outcome struct {
	// Recognition says which shape this is.
	Recognition Recognition
	// Turn is the minted turn, set when Recognition is RecognizedNone.
	Turn ids.TurnID
	// Panel is the answered panel, set when Recognition is RecognizedPanel. It
	// is ALSO mirrored into the root feed as a non-durable row.
	Panel *agentreplv1.SubmitPromptCommandPanel
	// RefusedCommand is the literal command as typed, set when Recognition is
	// RecognizedRefused. Its refusal card is mirrored into the root feed too.
	RefusedCommand string
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
	// Log is the handler's logger.
	Log dlog.Surfaces
}

// New builds the handler.
func New(deps Deps) (Handler, error) {
	return nil, notimpl.Err
}
