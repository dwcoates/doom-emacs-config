// emacs_frontendcapabilities_e2e_test.go — the gui frontend registry's
// CAPABILITY SLOTS, exercised against a real daemon through a real Emacs.
//
// Why this file exists at all: a registry slot naming a symbol nothing
// defines is invisible to every static gate the module has. The byte
// compiler is silenced by the `declare-function' beside it, and a unit test
// of the capability cannot fail for a capability that was never written. The
// defect surfaces only where these tests put it — in a running Emacs, on the
// registration the module actually loaded, reached through the dispatch a
// user's gesture reaches.
//
// 79faac1c7 left three such slots behind when it deleted frontend-client.el
// (`:cancel-detached-fn', `:durable-session-id-fn', `:adopt-session-fn'), and
// the resolution differs per slot: one has a real post-overhaul answer and is
// implemented, two have none and are deliberately UNSET. Both outcomes are
// asserted here, because "unset" is a contract too — the dispatch's loud nil
// is what a frontend without the capability owes its caller, and a crash is
// not a loud nil.
//
// These tests reuse area G/H/I's helpers (emacs_interrupt_e2e_test.go), which
// is where the shared registration/selection/submission acts for this suite
// live.
package e2e

import (
	"encoding/json"
	"testing"
)

// emCapGuiFrontendForm reads the REGISTERED gui frontend struct out of the
// live registry, which is the only honest subject here: a test that rebuilt
// its own struct would assert about a value the module never loaded.
const emCapGuiFrontendForm = `(agent-repl-frontend-get 'gui)`

// emCapUndefinedSlotsForm answers the names of every function-valued slot of
// the registered gui frontend whose value is NOT a live function. It walks
// the struct rather than naming slots, so a capability added later is covered
// the day it is added.
const emCapUndefinedSlotsForm = `(let (undefined)
  (dolist (slot (cdr (cl-struct-slot-info 'agent-repl-frontend)))
    (let* ((name (car slot))
           (value (cl-struct-slot-value 'agent-repl-frontend name ` + emCapGuiFrontendForm + `)))
      (when (and (string-suffix-p "-fn" (symbol-name name))
                 value
                 (not (functionp value)))
        (push (symbol-name name) undefined))))
  (sort undefined #'string<))`

// TestEmacsGuiRegistryNamesOnlyLiveCapabilities is the guard the three void
// slots defeated: every capability the loaded gui registration names must be
// a function IN THE EMACS THAT LOADED IT.
//
// It needs no workspace and no turn. The registration happens at module load,
// so a defect here exists before any user does anything — which is exactly
// why it went unnoticed until someone pressed a key.
func TestEmacsGuiRegistryNamesOnlyLiveCapabilities(t *testing.T) {
	t.Parallel()
	// Arrange: an Emacs with the module loaded. No daemon act is needed.
	w, e := emGHIWorld(t)
	_ = w

	// Act
	undefined := e.EvalStrings(emCapUndefinedSlotsForm)

	// Assert
	if len(undefined) != 0 {
		t.Fatalf("the gui registration names %v as capabilities, and none of them is a defined function; "+
			"each is a void function at the moment a user reaches it", undefined)
	}
}

// TestEmacsGuiCannotCancelDetachedWork asserts the CANCEL-DETACHED path.
//
// The gui leaves `:cancel-detached-fn' unset on purpose: stopping detached
// work is the `Interrupt' verb's `all_agents' target, `Interrupt' is a feed
// verb whose other targets are named by `frontend.v1.FeedId', and Emacs calls
// no interrupt rpc at all — the webapp footer owns the gesture, on the very
// page this frontend mounts.
//
// The assertion that matters is the second one. An unset slot is only correct
// if the DISPATCH survives it: it must answer a loud nil, never dispatch some
// other verb, and never die. Before the fix this same call was a void-function
// error on a real workspace.
func TestEmacsGuiCannotCancelDetachedWork(t *testing.T) {
	t.Parallel()
	// Arrange: a real registered, selected workspace — the dispatch resolves
	// the frontend through the workspace, so a bare symbol lookup would not
	// exercise the path a gesture takes.
	w, e := emGHIWorld(t)
	ws, dir := emGHIRegister(t, e, w.Emacs.box, "repo-cancel-detached")
	emGHISelect(t, e, dir, ws)

	// Assert: the capability is genuinely absent from the registration.
	if e.EvalBool(`(and (agent-repl-frontend-cancel-detached-fn ` + emCapGuiFrontendForm + `) t)`) {
		t.Fatal("the gui registration declares a cancel-detached capability; Emacs calls no interrupt rpc, " +
			"so the slot must stay unset and the dispatch must say so")
	}

	// Act / Assert: the dispatch answers nil rather than dying, and Emacs is
	// still answering afterwards.
	if e.EvalBool(`(and (agent-repl--frontend-dispatch-cancel-detached ` + elispString(ws) + `) t)`) {
		t.Fatalf("dispatching a detached-work cancel on %s reported a cancel; no frontend here can reach detached work", ws)
	}
	emGHIAssertResponsive(t, e, "a detached-work cancel dispatched at a frontend without the capability")
}

// TestEmacsGuiCannotAdoptANamedSession asserts the ADOPTION path.
//
// No post-overhaul verb binds a workspace to a vendor session uuid a client
// names: the daemon owns session identity and resumes a workspace's own
// conversation from its own record. So `:adopt-session-fn' is unset, and the
// accessor answering nil is the whole contract — there is no dispatcher to
// reach past it, and a caller that finds nil has been told the truth.
func TestEmacsGuiCannotAdoptANamedSession(t *testing.T) {
	t.Parallel()
	// Arrange
	w, e := emGHIWorld(t)
	_ = w

	// Act / Assert
	if e.EvalBool(`(and (agent-repl-frontend-adopt-session-fn ` + emCapGuiFrontendForm + `) t)`) {
		t.Fatal("the gui registration declares an adopt-session capability; nothing in the contract " +
			"binds a workspace to a client-named vendor session, so the slot must stay unset")
	}
}

// TestEmacsGuiDurableSessionIdNamesTheVendorConversation is the slot that DID
// have a post-overhaul answer, end to end.
//
// The durable id is the vendor conversation's uuid — the one a resume replays
// — and Emacs holds no session state to answer from. It comes down the host
// stream on the live arm's `vendor_info' oneof, so this reads the capability
// back off a real bring-up against the fake vendor: the id the daemon
// published, never the daemon-minted session token that rotates beside it,
// and the same id once a turn runs in that conversation.
//
// THE CONVERSATION'S ID EXISTS FROM THE BRING-UP, NOT FROM THE FIRST TURN.
// The shim pre-mints it and hands it to the vendor as `Options.sessionId`
// (agent-shim/claude/shim's src/engine/identity.ts), and since a1caff698 a
// look starts the session of an open workspace with none behind it, so the
// registration's own selection publishes the id within a few hundred
// milliseconds. An earlier shape of this test asserted "nil before any turn"
// right after the panel opened; that read raced the bring-up and failed
// whenever the session came up first (the capability answering nil with no
// live session is unit-covered in lisp/test-host.el).
func TestEmacsGuiDurableSessionIdNamesTheVendorConversation(t *testing.T) {
	t.Parallel()
	// Arrange: a registered, selected workspace with its panel open.
	w, e := emGHIWorld(t)
	ws, dir := emGHIRegister(t, e, w.Emacs.box, "repo-durable-session-id")
	emGHISelect(t, e, dir, ws)
	emGHIOpenPanel(t, e)

	durableForm := `(or (agent-repl--gui-durable-session-id ` + elispString(ws) + `) "")`

	// Act: the bring-up the selection started publishes the conversation.
	raw := e.AwaitEval("the workspace to name its vendor conversation", durableForm,
		func(raw json.RawMessage) bool {
			var got string
			return json.Unmarshal(raw, &got) == nil && got != ""
		})
	var durable string
	if err := json.Unmarshal(raw, &durable); err != nil {
		t.Fatalf("decode the durable session id for %s: %v", ws, err)
	}

	// Assert: the capability answers the id the host stream published, and
	// answers exactly it — a second source would be a second answer.
	if fromHost := e.EvalString(`(or (agent-repl-host-vendor-session-id ` + elispString(ws) + `) "")`); fromHost != durable {
		t.Fatalf("the gui capability answers %q and the host stream holds %q; the capability must have exactly one source", durable, fromHost)
	}

	// Assert: it is the VENDOR id, not the daemon-minted echo token that
	// rotates under one workspace. Confusing the two is the whole reason the
	// capability reads the vendor arm and nothing else.
	hostRefID := e.EvalString(`(or (plist-get (agent-repl-host-ref ` + elispString(ws) + `) :id) "")`)
	if durable == hostRefID {
		t.Fatalf("the durable session id equals the workspace ref id %q; it must be the vendor conversation's uuid", hostRefID)
	}

	// Act: a turn in that conversation, parked mid-turn so "running" is a
	// fact rather than a race.
	emGHISubmit(t, e, ws, emGHIParkedPrompt)
	emGHIAwaitStatus(t, e, ws, "the turn to be running in the conversation", emGHIRunningArms...)

	// Assert: the turn runs in the SAME conversation; the durable id does not
	// move under it.
	if got := e.EvalString(durableForm); got != durable {
		t.Fatalf("%s durably identifies %q while its first turn runs, want the bring-up's %q", ws, got, durable)
	}
}
