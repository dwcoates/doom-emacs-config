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
// THE CONVERSATION'S ID EXISTS FROM THE BRING-UP, NOT FROM THE TURN. The
// shim pre-mints it and hands it to the vendor as `Options.sessionId`
// (agent-shim/claude/shim's src/engine/identity.ts). Opening a workspace
// starts no shim (owner ruling, 2026-10-28), so the first prompt's bring-up
// is what publishes it; the capability answering nil with no live session is
// unit-covered in lisp/test-host.el.
func TestEmacsGuiDurableSessionIdNamesTheVendorConversation(t *testing.T) {
	t.Parallel()
	// Arrange: a registered, selected workspace with its panel open.
	w, e := emGHIWorld(t)
	ws, dir := emGHIRegister(t, e, w.Emacs.box, "repo-durable-session-id")
	emGHISelect(t, e, dir, ws)
	emGHIOpenPanel(t, e)

	durableForm := `(or (agent-repl--gui-durable-session-id ` + elispString(ws) + `) "")`

	// Act: a prompt brings the session up, parked mid-turn so "running" is a
	// fact rather than a race; its bring-up publishes the conversation.
	emGHISubmit(t, e, ws, emGHIParkedPrompt)
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

	emGHIAwaitStatus(t, e, ws, "the turn to be running in the conversation", emGHIRunningArms...)

	// Assert: the turn runs in the SAME conversation; the durable id does not
	// move under it.
	if got := e.EvalString(durableForm); got != durable {
		t.Fatalf("%s durably identifies %q while its first turn runs, want the bring-up's %q", ws, got, durable)
	}
}
