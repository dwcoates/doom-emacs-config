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
	"os"
	"strings"
	"testing"
)

// emRFHostCountForm counts the workspaces Emacs holds daemon identities for.
// It stands in for "no wire call was made" in the two scenarios that refuse
// before dialing: every verb in this module resolves a ref and a connection
// out of this table on its way to the wire, so a refusal that nonetheless
// reached the daemon would have had to move it.
const emRFHostCountForm = `(hash-table-count agent-repl-host--by-name)`

// emRFCatch evaluates FORM and answers the error message it signalled, or
// the empty string when it completed. Reading the message is the sanctioned
// rendered-string exception for this area.
func emRFCatch(t *testing.T, e *Emacs, form string) string {
	t.Helper()
	return e.EvalString(`(condition-case err ` + form + `
                             (error (error-message-string err)))`)
}

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

// TestEmacsNoWorkspacesRegisteredRefusesThePicker is scenario 43.
//
// With an empty registry there is nothing to close, and the refusal is a
// `user-error` naming that fact — not a nil target quietly handed to the
// wire. The message is read as text deliberately: EMACS-LAYER-SPEC.md names
// this scenario as one of the two rendered-string assertions in the layer,
// because the message IS the contract.
func TestEmacsNoWorkspacesRegisteredRefusesThePicker(t *testing.T) {
	t.Parallel()
	// Arrange: a daemon, and NO workspace registered against it. This is a
	// fresh Emacs, so the registry is empty by construction rather than by
	// something the test cleared.
	_, e := emGHIWorld(t)
	if got := e.EvalInt(emGHIWorkspaceCountForm); got != 0 {
		t.Fatalf("the workspace registry holds %d entries before the act, want an empty registry", got)
	}
	before := e.EvalInt(emRFHostCountForm)

	// Act
	message := emRFCatch(t, e, `(agent-repl-close-workspace)`)

	// Assert: the refusal names the missing registry.
	const want = "No agent-repl workspaces registered"
	if !strings.Contains(message, want) {
		t.Fatalf("closing with an empty registry signalled %q, want a user-error containing %q", message, want)
	}

	// Assert: nothing reached the wire. Every verb resolves its ref and its
	// connection out of the host table on the way to the daemon, so a
	// refusal that had dialed anyway would have had to move it.
	if got := e.EvalInt(emRFHostCountForm); got != before {
		t.Fatalf("the host table holds %d entries after the refusal, want %d: the refusal made a wire call", got, before)
	}
}

// emGHIWorkspaceCountForm counts the workspace registry, for the assertions
// that care about emptiness rather than about names.
// Only entries carrying `:project-dir` are workspaces: persp-mode's own
// bookkeeping puts stub entries into the same table (the boot writes a
// `ws=none` stub for `:panels-were-visible`), and counting those would read
// a fresh Emacs as already holding a workspace.
const emGHIWorkspaceCountForm = `(let ((n 0))
   (maphash (lambda (_k v) (when (plist-get v :project-dir) (setq n (1+ n))))
            agent-repl--workspaces)
   n)`

// TestEmacsNukeConfirmsBeforeDestroying is scenario 44.
//
// Nuke is the ONE verb that destroys data — worktree and branch, both
// unrecoverable — so a declined confirmation must leave every one of them
// exactly where it was. The confirmation reader is stubbed for the duration
// of the single call, the standard ERT way, so the command still runs its
// own confirmation code path rather than being reached past.
func TestEmacsNukeConfirmsBeforeDestroying(t *testing.T) {
	t.Parallel()
	// Arrange
	w, e := emGHIWorld(t)
	ws, dir := emGHIRegister(t, e, w.Emacs.box, "repo-nuke-declined")
	emGHISelect(t, e, dir, ws)
	before := e.EvalInt(emRFHostCountForm)

	// Act: answer NO. `cl-letf` scopes the stub to this one call, so the
	// command's own `yes-or-no-p` call site is what runs.
	e.Eval(`(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                 (agent-repl-nuke-workspace ` + elispString(ws) + `)
                 t)`)

	// Assert: no wire call. A nuke that had been sent would have taken the
	// workspace's daemon identity with it.
	if got := e.EvalInt(emRFHostCountForm); got != before {
		t.Fatalf("the host table holds %d entries after a declined nuke, want %d: the nuke was sent anyway", got, before)
	}

	// Assert: the workspace is still registered, and still holds its
	// daemon-minted ref.
	if !e.EvalBool(`(and (agent-repl-host-ref ` + elispString(ws) + `) t)`) {
		t.Fatalf("the workspace %s lost its daemon ref after a declined nuke", ws)
	}

	// Assert: the WORKTREE is untouched, read from the filesystem rather
	// than from anything Emacs says about it. This is the fact the
	// confirmation exists to protect.
	if _, err := os.Stat(dir); err != nil {
		t.Fatalf("the worktree %s is gone after a declined nuke: %v", dir, err)
	}
}
