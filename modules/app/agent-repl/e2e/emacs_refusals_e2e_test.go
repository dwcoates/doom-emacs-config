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

// TestEmacsSubmitWithNoDaemonIsRefusedLoudly is scenario 42.
//
// A submission with no daemon behind it is a unary call that cannot be made.
// The contract has two halves and the second is the one that matters to the
// user: the failure is surfaced, AND THE USER'S WORDS ARE NOT LOST. Owner
// ruling (older, on master): a from-buffer send clears the composer
// OPTIMISTICALLY at dispatch, whether or not the daemon acks, so the
// composer is NOT where the words survive and this test does not read it.
// They survive in two places instead: the durable on-disk held-prompt
// ingress (owner ruling 2026-09-28: held prompts survive outages and
// restarts, and Emacs holds none in memory), which the daemon ingests once
// it serves; and the input history ring, from which the user can recall
// them.
func TestEmacsSubmitWithNoDaemonIsRefusedLoudly(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace, then no daemon. Emacs asks the daemon
	// to exit — it never kills one — and the link drops at the transport.
	w, e := emGHIWorld(t)
	ws, dir := emGHIRegister(t, e, w.Emacs.box, "repo-no-daemon-send")
	emGHISelect(t, e, dir, ws)
	emGHIOpenPanel(t, e)

	const draft = "the prompt that must survive the outage"
	buffer := e.EvalString(`(buffer-name (agent-repl--input-buffer ` + elispString(ws) + `))`)
	e.Eval(`(with-current-buffer (agent-repl--input-buffer ` + elispString(ws) + `)
                 (erase-buffer)
                 (insert ` + elispString(draft) + `)
                 t)`)

	e.Eval(`(agent-repl-frontend-daemon-stop)`)
	e.AwaitEvalFor(daemonStopBound, "the link to go down when the daemon exits",
		`(if (agent-repl-link-up-p) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	// Act, and read the ingress and the history IN THE SAME command-loop
	// iteration as the send: the no-connection branch of the submit runs
	// inline, and the reconnect loop may bring a daemon back at any later
	// instant and ingest what was held. One form makes the observation
	// race-free without a sleep and without pausing the module's own timers.
	got := e.EvalStrings(`(progn (agent-repl-send)
                                 (list (number-to-string (agent-repl-held-ingress-waiting ` + elispString(ws) + `))
                                       (with-current-buffer ` + elispString(buffer) + `
                                         (or (car agent-repl--input-history) ""))))`)
	if len(got) != 2 {
		t.Fatalf("the post-send readback answered %q, want the ingress count and the newest history entry", got)
	}

	// Assert: the prompt was HELD on disk, not dropped. Holding it is how
	// Emacs refuses loudly without discarding intent.
	if got[0] != "1" {
		t.Fatalf("%s of the workspace's prompts wait in the held-prompt ingress after a send with no daemon, want 1 held for the outage", got[0])
	}

	// Assert: the user's own words are recallable from the input history,
	// which the optimistic clear pushed them onto before erasing the
	// composer.
	if got[1] != draft {
		t.Fatalf("the newest input history entry is %q after the refused send, want the draft %q", got[1], draft)
	}
}
