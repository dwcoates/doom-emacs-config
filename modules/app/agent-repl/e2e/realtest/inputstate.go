//go:build realtest

package realtest

import (
	"fmt"
	"strings"
	"time"
)

// THE EDITOR'S PENDING INPUT STATE, AND WHY A REALTEST HAS TO READ IT.
//
// Evil reads a key sequence across several command-loop turns. `d` is not a
// command, it is an OPERATOR: it puts evil into `operator' state and the next
// key is read as its motion, whatever that key happens to be bound to. Doom's
// leader works the same way — `SPC` alone only opens a prefix map and waits.
//
// So a HALF-DELIVERED SEQUENCE IS A TRAP THAT DOES NOT EXPIRE. There is no
// timeout on an operator or a prefix: the editor sits in that state until some
// key arrives, and the key that eventually arrives is consumed by it. The next
// real keystroke is then read as the missing half of somebody else's sequence.
//
// That is not hypothetical either. Realtest 4's first act pressed `s-}` and
// the switch was completely correct — right workspace, right record, cursor in
// the composer, right tab underlined — but `last-command` came back
// `evil-delete`. `evil-read-motion' performs no motion check: it accepts
// whatever the key is bound to and `call-interactively's it, so
// `agent-repl-switch-right' ran in full while the command loop attributed the
// turn to the operator that swallowed it. Every later act in the same run read
// the right command, because one chord is all a pending operator eats.
//
// The `d` was already standing in the editor when the run began. Realtests 4
// through 8 ADOPT a standing Emacs rather than starting one, so whatever the
// owner or a previous run left half-typed is inherited — and a leader sequence
// is sent one key at a time, each key its own activate-post-restore, so a
// dropped `SPC` in `SPC j d` leaves a bare `d` behind for the next run to
// inherit.
//
// TWO RULES FOLLOW, AND THIS FILE IS BOTH OF THEM:
//
//  1. A run CLEARS the editor's pending input before it presses anything it
//     intends to measure, with a real `<escape>` — the same key a user would
//     press, and the same key the chord prover already leads with.
//  2. A run LEAVES the editor clean, so the next run inherits nothing.
//
// Neither rule weakens an assertion. An inherited operator is REPORTED, not
// quietly swept up, and an `<escape>` that cannot clear one is a failure: it
// would mean a real key does not reach the keymap, which is the one thing a
// realtest exists to catch.

const (
	// wsActInputClearCeiling bounds how long a real `<escape>` may take to put
	// evil back in a state that consumes nothing.
	//
	// Leaving operator state is Emacs's own command loop dispatching one key
	// that has already been posted and acknowledged by the window server —
	// microseconds of work. The only latency to tolerate is the poll interval
	// below plus one emacsclient round trip, both sized here. Three seconds is
	// a small multiple of that, and it is the same bound the standing-minibuffer
	// dismissal gives its own chord for the same reason (minibuffer.go).
	wsActInputClearCeiling = 3 * time.Second

	// wsActInputClearPollInterval is how often the pending state is re-read
	// while waiting for the escape to land. Deliberately tight: this is one
	// keypress being dispatched, not a startup.
	wsActInputClearPollInterval = 100 * time.Millisecond
)

// wsActInputState is what the editor would do with the NEXT key it is sent.
type wsActInputState struct {
	// Evil is evil's current state name, e.g. "normal", "insert",
	// "operator". "none" when evil is not loaded at all.
	Evil string
	// Prefix is `prefix-arg' rendered, or "nil" when none stands. A standing
	// prefix argument is the other way a key is consumed as part of something
	// the run did not send.
	Prefix string
}

// Pending answers whether the next key would be eaten by something already
// standing rather than read on its own terms.
//
// EVIL'S `operator' STATE IS THE WHOLE POINT. It is the state a bare `d`
// leaves, and the next key becomes that operator's motion. A standing
// `prefix-arg' does the same to a numeric or `C-u` prefix. Every other evil
// state — normal, insert, visual, emacs — reads the next key as itself.
func (s wsActInputState) Pending() bool {
	return s.Evil == "operator" || (s.Prefix != "" && s.Prefix != "nil")
}

// String renders the state the way the manifest carries it.
func (s wsActInputState) String() string {
	return fmt.Sprintf("evil-state=%s prefix-arg=%s", s.Evil, s.Prefix)
}

// wsActPendingInputForm is the elisp that reads the pending input state.
//
// `evil-state' through `bound-and-true-p' so an editor without evil answers
// "none" rather than failing the probe: a realtest reporting "no evil" is
// worth more than a realtest reporting "the probe errored".
func wsActPendingInputForm() string {
	return `(format "%s` + stateFieldSep + `%s"
    (or (bound-and-true-p evil-state) "none")
    (format "%s" prefix-arg))`
}

// parseWsActInputState reads what wsActPendingInputForm answered.
//
// A wrong field count is an ERROR rather than a partial read. The two fields
// are joined with state.go's own unit separator precisely so a value that
// happens to contain a pipe or a comma cannot split into the wrong number of
// fields, and a probe that came back malformed has said nothing trustworthy
// about whether a key would be eaten.
func parseWsActInputState(raw string) (wsActInputState, error) {
	fields := strings.Split(raw, stateFieldSep)
	if len(fields) != 2 {
		return wsActInputState{}, fmt.Errorf(
			"the pending-input probe answered %q, which splits into %d fields rather than 2 (evil-state, prefix-arg)",
			raw, len(fields))
	}
	return wsActInputState{Evil: fields[0], Prefix: fields[1]}, nil
}

// wsActInputAlreadyCleanNote is what the run says when nothing was standing.
func wsActInputAlreadyCleanNote(why string, state wsActInputState) string {
	return fmt.Sprintf("the editor had no pending input before %s (%s), so the `<escape>` that cleared it "+
		"changed nothing", why, state)
}

// wsActInputInheritedNote is what the run says when something WAS standing and
// a real `<escape>` cleared it.
//
// IT IS A FINDING, not housekeeping. A pending operator eats the next real
// keystroke a user types just as surely as it eats the next chord this run
// sends, and the run that left it there — or the owner who half-typed it — is
// worth naming. What the note must never do is imply the run tolerated it: the
// state is reported, then cleared with a key a user could have pressed.
func wsActInputInheritedNote(why string, before, after wsActInputState) string {
	return fmt.Sprintf("INHERITED PENDING INPUT, CLEARED: the editor was standing at %s before %s, so the very "+
		"next key would have been consumed as part of a sequence this run never sent — an evil operator reads "+
		"the next key as its motion whatever that key is bound to, and runs adopt a standing Emacs rather than "+
		"starting one. A real `<escape>` put it back to %s", before, why, after)
}

// wsActInputNotClearedNote is what the run says when the escape did not take.
//
// This one is a failure and not a finding to carry: an `<escape>` that cannot
// leave operator state means a real key is not reaching the keymap, and every
// chord the run presses afterwards would be read as somebody else's motion.
func wsActInputNotClearedNote(why string, before, after wsActInputState) string {
	return fmt.Sprintf("PENDING INPUT COULD NOT BE CLEARED: the editor was standing at %s before %s and is "+
		"still at %s after a real `<escape>` (%s). Every chord pressed after this one would be consumed as a "+
		"motion rather than read on its own terms, so what follows would be measuring the wrong thing",
		before, why, after, wsActInputClearCeiling)
}
