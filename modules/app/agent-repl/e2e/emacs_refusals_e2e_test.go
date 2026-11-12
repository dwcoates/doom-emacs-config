// emacs_refusals_e2e_test.go — EMACS-LAYER-SPEC.md area I, "Host-side
// refusal messages" (scenarios 42-44).
//
// This area is Emacs-only by construction: the refusals here never reach the
// wire, so there is no frame for a Connect-dialing client to observe. They
// are `user-error` text and preserved editor state, and EMACS-LAYER-SPEC.md
// grants them the sanctioned exception to "never scrape human text where a
// variable exists" — the message IS the subject, so it is read deliberately
// and said so here.
//
// The standing rule these three share: undelivered user intent may never be
// silently discarded, and a unary call that cannot be made fails LOUDLY
// rather than being absorbed.
package e2e

import (
	"encoding/json"
	"testing"
)
