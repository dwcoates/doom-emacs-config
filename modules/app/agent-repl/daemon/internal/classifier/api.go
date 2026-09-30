// Package classifier judges where an incoming prompt goes while a turn runs:
// it waits for the turn, joins it after its current tool call, or interrupts
// it.
//
// It is the daemon's OWN headless vendor run (R13), guarded by
// envc.VendorGuard.Check("classifier"); `-fake` uses a scripted keyword
// heuristic instead. The explicit-interrupt fast path bypasses the model
// entirely. See ARCHITECTURE.md "promptqueue".
package classifier

import (
	"context"
	"fmt"

	"claude-repld/internal/envc"
	"claude-repld/internal/headless"
)

// ExplicitInterrupts is the fast path: a prompt whose FIRST WORD is one of
// these interrupts without asking the model at all (project-lead ruling — the
// rule is first-word, not whole-prompt).
var ExplicitInterrupts = []string{"stop", "abort", "cancel", "halt", "wait"}

// Route is where the judge sends an incoming prompt relative to the running
// turn (owner ruling, 2026-09-30).
type Route int

const (
	// RouteQueue: the prompt is independent of the running turn and waits for
	// it to end.
	RouteQueue Route = iota
	// RouteAfterToolCall: the prompt adds to the running turn's work without
	// invalidating it, so it joins that turn after its current tool call and
	// nothing is interrupted. The common case, and the answer when unsure.
	RouteAfterToolCall
	// RouteInterrupt: the prompt makes the running work wrong or wasted, so
	// the turn is interrupted to deliver it.
	RouteInterrupt
)

// String names the route as the logs and the tray's evidence spell it.
func (r Route) String() string {
	switch r {
	case RouteQueue:
		return "queue"
	case RouteAfterToolCall:
		return "after_tool_call"
	case RouteInterrupt:
		return "interrupt"
	default:
		panic(fmt.Sprintf("classifier: route %d is not one of the three", int(r)))
	}
}

// Verdict is the judge's answer.
type Verdict struct {
	// Route is where the incoming prompt goes.
	Route Route
	// Reason is the judge's stated reason, kept as evidence on the held
	// prompt's record.
	Reason string
	// FastPath reports that the explicit-interrupt path decided and no model
	// ran.
	FastPath bool
}

// Judge decides where incoming goes relative to running. An error here is not a
// verdict: the caller holds the prompt with a classification_error rather than
// guessing either way.
type Judge interface {
	// Judge returns the verdict for one pair of prompts.
	Judge(ctx context.Context, running, incoming string) (Verdict, error)
}

// New builds the real judge: a headless vendor run, refused by guard when
// vendor calls are forbidden. runner is the shared internal/headless facility
// — which is where the binary is resolved, so the classifier can no longer be
// built with an empty one — and promptsDir is where the routing brief is read
// AT USE TIME.
//
// SEAM CHANGE (recorded): the skeleton's New took (guard, vendorBin). The
// routing question is `prompts/queue-routing-classifier.md`, read at use time
// like every other brief, so the judge needs the prompts directory too; there
// is no other way for it to reach one. A LATER SEAM CHANGE (recorded): the
// bare binary name became the shared headless runner, so the classifier and
// the workspace naming call share one exec site.
func New(guard envc.VendorGuard, runner headless.Runner, promptsDir string) (Judge, error) {
	return newVendorJudge(guard, runner, promptsDir), nil
}

// NewFake builds the `-fake` judge: the scripted keyword heuristic, which
// never invokes anything.
func NewFake() Judge {
	return fakeJudge{}
}
