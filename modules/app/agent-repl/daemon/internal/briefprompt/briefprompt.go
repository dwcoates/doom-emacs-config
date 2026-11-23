// Package briefprompt is the one way the daemon submits a prompt it composed
// from a brief to a workspace's agent under the hood: compose the brief, mint
// the submission's idempotency key, and submit it under its origin, which is
// delivered on arrival (promptqueue's arrival.go): it joins a running turn
// after its current tool call and wakes an idle agent. Every
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
	// Meter records every notice submitted, by its brief (the notice's
	// type) and its size. REQUIRED: every daemon notice to a workspace goes
	// through a Composer, so this is where they are all measured.
	Meter Meter
}

// Meter records the notices the daemon sends workspaces. Each call is an
// atomic add and never blocks the notice.
type Meter interface {
	// ObserveNotice records one notice submitted: its type (the brief it
	// was composed from) and its text's size in UTF-8 bytes.
	ObserveNotice(notice string, bytes int)
	// CountSuppressedNotice records one notice of a type the daemon is set
	// not to send, which was therefore never composed or submitted.
	CountSuppressedNotice(notice string)
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
	return c.SubmitAs(ctx, prompts, ws, brief, values, keyPrefix, origin, wsm.DeliveryDeferred)
}

// SubmitAs is Submit with the prompt's DELIVERY named: an ordinary prompt is
// classified against the running turn, which may interject it, where a
// deferred one waits for the turn's end. Only a notice whose content may be
// urgent (a Slack thread's digest) asks for ordinary delivery.
func (c Composer) SubmitAs(ctx context.Context, prompts prompthandler.Submitter, ws ids.WorkspaceID, brief string, values map[string]string, keyPrefix string, origin conversationv1.PromptOrigin, delivery wsm.Delivery) (ids.TurnID, error) {
	if c.Meter == nil {
		panic("briefprompt: a Composer submits with no Meter; every notice is measured")
	}
	text, err := c.Briefs(brief, values)
	if err != nil {
		return "", &Error{Step: StepCompose, Err: err}
	}
	key, err := c.MintKey(keyPrefix)
	if err != nil {
		return "", &Error{Step: StepMint, Err: err}
	}
	outcome, err := prompts.Submit(ctx, ws, feed.SaidText(text), key, origin, nil, delivery, nil)
	if err != nil {
		return "", &Error{Step: StepSubmit, Err: err}
	}
	c.Meter.ObserveNotice(brief, len(text))
	return outcome.Turn, nil
}
