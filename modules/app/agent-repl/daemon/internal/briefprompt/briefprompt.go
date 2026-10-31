// Package briefprompt is the one way the daemon submits a prompt it composed
// from a brief to a workspace's agent under the hood: compose the brief, mint
// the submission's idempotency key, and submit it as a DEFERRED prompt of its
// origin, which waits behind a running turn and wakes an idle agent. Every
// notifier (merge-queue, workspace, derived-workspace and pull-request
// notices, the main sync's repair prompt) submits through it, so the three
// steps cannot drift apart between them.
package briefprompt

import (
	"context"
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/wsm"
)

// Composer composes and submits brief prompts.
type Composer struct {
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// MintKey mints an idempotency key for one submission.
	MintKey func(prefix string) (string, error)
}

// Step is the step of a submission that failed, so a caller records the
// failure in its own words.
type Step int

// The steps.
const (
	// StepCompose is the brief failing to load or splice.
	StepCompose Step = iota + 1
	// StepMint is the idempotency key failing to mint.
	StepMint
	// StepSubmit is the prompt handler refusing or failing the submission.
	StepSubmit
)

// Error is a submission that failed at Step.
type Error struct {
	Step Step
	Err  error
}

// Error is the cause's own text, so a caller's record names the cause as it
// did before the step was known.
func (e *Error) Error() string { return e.Err.Error() }

func (e *Error) Unwrap() error { return e.Err }

// StepOf answers the step ERR failed at; zero for an error no submission
// answered.
func StepOf(err error) Step {
	var e *Error
	if errors.As(err, &e) {
		return e.Step
	}
	return 0
}

// Submit composes BRIEF from VALUES and submits it to WS as a deferred prompt
// of ORIGIN, its key minted under KEY_PREFIX, answering the turn it was
// submitted as. A failure is an *Error naming its step.
func (c Composer) Submit(ctx context.Context, prompts prompthandler.Submitter, ws ids.WorkspaceID, brief string, values map[string]string, keyPrefix string, origin conversationv1.PromptOrigin) (ids.TurnID, error) {
	text, err := c.Briefs(brief, values)
	if err != nil {
		return "", &Error{Step: StepCompose, Err: err}
	}
	key, err := c.MintKey(keyPrefix)
	if err != nil {
		return "", &Error{Step: StepMint, Err: err}
	}
	outcome, err := prompts.Submit(ctx, ws, feed.SaidText(text), key, origin, nil, wsm.DeliveryDeferred, nil)
	if err != nil {
		return "", &Error{Step: StepSubmit, Err: err}
	}
	return outcome.Turn, nil
}
