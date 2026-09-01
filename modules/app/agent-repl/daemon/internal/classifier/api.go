// Package classifier judges whether an incoming prompt interrupts the running
// turn.
//
// It is the daemon's OWN headless vendor run (R13), guarded by
// envc.VendorGuard.Check("classifier"); `-fake` uses a scripted keyword
// heuristic instead. The explicit-interrupt fast path bypasses the model
// entirely. See ARCHITECTURE.md "promptqueue".
package classifier

import (
	"context"

	"claude-repld/internal/envc"
)

// ExplicitInterrupts is the fast path: a prompt that is exactly one of these
// words interrupts without asking the model at all.
var ExplicitInterrupts = []string{"stop", "abort", "cancel", "halt", "wait"}

// Verdict is the judge's answer.
type Verdict struct {
	// Interject reports whether the incoming prompt interrupts the running
	// turn.
	Interject bool
	// Reason is the judge's stated reason, kept as evidence on the held
	// prompt's record.
	Reason string
	// FastPath reports that the explicit-interrupt path decided and no model
	// ran.
	FastPath bool
}

// Judge decides whether incoming interrupts running. An error here is not a
// verdict: the caller holds the prompt with a classification_error rather than
// guessing either way.
type Judge interface {
	// Judge returns the verdict for one pair of prompts.
	Judge(ctx context.Context, running, incoming string) (Verdict, error)
}

// New builds the real judge: a headless vendor run, refused by guard when
// vendor calls are forbidden. vendorBin is the binary it runs and promptsDir
// is where the routing brief is read AT USE TIME.
//
// SEAM CHANGE (recorded): the skeleton's New took (guard, vendorBin). The
// routing question is `prompts/queue-routing-classifier.md`, read at use time
// like every other brief, so the judge needs the prompts directory too; there
// is no other way for it to reach one.
func New(guard envc.VendorGuard, vendorBin, promptsDir string) (Judge, error) {
	return newVendorJudge(guard, vendorBin, promptsDir), nil
}

// NewFake builds the `-fake` judge: the scripted keyword heuristic, which
// never invokes anything.
func NewFake() Judge {
	return fakeJudge{}
}
