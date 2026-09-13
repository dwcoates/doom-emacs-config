//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// A REQUESTED ACTIVATION IS NOT A FOCUS EDGE. The editor says whether one
// happened.
//
// The show phase exists to produce the thing the product's pre-creation drain
// waits on: Emacs becoming the focused application. The driver ASKS for that by
// activating the target, and on macOS 14 and later the ask is cooperative —
// `activate()` is a request the window server may decline, and the legacy
// `.activateIgnoringOtherApps` that used to override it is documented as having
// no effect. A declined request looks exactly like a granted one from the
// calling side: the helper exits 0, the key is posted, and (because a
// pid-addressed CGEvent reaches the process regardless) the key even ARRIVES
// and confirms. Nothing in the key-delivery reading can tell the two apart.
//
// So the focus edge is read from the same place the product reads it:
// `frame-focus-state` over every live frame, which is exactly what
// `agent-repl--emacs-focused-p` (lisp/notifications.el) scans and therefore
// exactly what `agent-repl--webview-precreate-hold-p` is holding on. If that
// says unfocused while the driver believes it activated Emacs, the activation
// was declined and no amount of waiting will drain the queue.
//
// THE 2026-09-12 EVENING FINDING, WHICH THIS FILE IS THE ANSWER TO. Every
// realtest reported `PANELS DID NOT PAINT` after three focus edges over two
// minutes, with zero key refusals and zero delivery failures, while the
// editor's own log repeated `precreate-parked reason=visible-unfocused`
// throughout. The driver's activation code was byte-for-byte the code that
// painted panels in 541ms earlier the same afternoon, so the driver was never
// the difference. What differed was the SESSION: behind a locked screen the
// window server grants activation to nobody, and a harness that cannot see the
// lock spends its whole ceiling waiting for an edge that cannot happen.
// keydriver.swift's `--session` is that reading, and the note below is how a
// run says so instead of blaming the paint.

// FocusState is what the editor said about its own desktop focus.
//
// The zero value is UNREAD on purpose: a press that never asked must not be
// recorded as one that was told Emacs is unfocused, which is the reading that
// condemns the whole show phase.
type FocusState int

const (
	// FocusUnread: nobody asked the editor.
	FocusUnread FocusState = iota
	// FocusFocused: Emacs holds desktop focus, so a real focus edge happened.
	FocusFocused
	// FocusUnfocused: Emacs does not hold desktop focus, so the activation
	// this press asked for was declined.
	FocusUnfocused
	// FocusUnanswered: the editor would not answer, which is never read as
	// either of the two above.
	FocusUnanswered
)

// FocusReading is one answer from the editor about its own focus.
type FocusReading struct {
	State FocusState
	// ProbeFailure names why the editor did not answer, empty when it did.
	ProbeFailure string
}

// Describe renders the reading in the words a manifest carries.
func (f FocusReading) Describe() string {
	switch f.State {
	case FocusFocused:
		return "emacsFocused=yes"
	case FocusUnfocused:
		return "emacsFocused=no"
	case FocusUnanswered:
		return "emacsFocused=unanswered (" + f.ProbeFailure + ")"
	default:
		return "emacsFocused=unread"
	}
}

// emacsFocusForm is the probe, and it MIRRORS THE PRODUCT'S OWN PREDICATE.
//
// `agent-repl--emacs-focused-p` scans `frame-focus-state` across every live
// frame rather than the selected one, and the pre-creation hold is built on
// that answer. Asking a different question — the selected frame, the window
// server's idea of frontmost — would let the harness call a focus edge real on
// a reading the product does not consult.
func emacsFocusForm() string {
	return `(if (seq-some #'frame-focus-state (frame-list)) "focused" "unfocused")`
}

// parseFocusReading reads what emacsFocusForm wrote.
//
// An answer that is neither word is UNANSWERED rather than unfocused: a probe
// whose shape changed must not silently start condemning every show phase.
func parseFocusReading(raw string) FocusReading {
	switch strings.TrimSpace(raw) {
	case "focused":
		return FocusReading{State: FocusFocused}
	case "unfocused":
		return FocusReading{State: FocusUnfocused}
	default:
		return FocusReading{
			State:        FocusUnanswered,
			ProbeFailure: fmt.Sprintf("the focus probe answered %q, which is neither focused nor unfocused", raw),
		}
	}
}

// ReadEmacsFocus asks the editor whether it holds desktop focus.
//
// Like ReadInputMark it never answers an error: an editor that would not answer
// has said nothing about focus, and that has to travel WITH the reading so no
// verdict is taken on the strength of it.
func ReadEmacsFocus(ctx context.Context, client *Client) FocusReading {
	raw, err := client.ReadString(ctx, emacsFocusForm())
	if err != nil {
		return FocusReading{
			State:        FocusUnanswered,
			ProbeFailure: fmt.Sprintf("(frame-focus-state) over (frame-list): %v", err),
		}
	}
	return parseFocusReading(raw)
}

// ScreenLock is whether the login window stands in front of the session.
type ScreenLock int

const (
	// ScreenLockUnknown: the session could not be read, which is never
	// reported as unlocked.
	ScreenLockUnknown ScreenLock = iota
	// ScreenLockUnlocked: the session is the owner's, and an activation
	// request can be granted.
	ScreenLockUnlocked
	// ScreenLockLocked: no application can be activated at all.
	ScreenLockLocked
)

// screenLockLine is what `keydriver --session` prints.
const screenLockPrefix = "screenLocked="

// parseScreenLock reads the helper's session line.
//
// Its own function so the spelling is testable without a window server: a
// helper that started answering something else must be an error, never a
// silent "unlocked".
func parseScreenLock(raw string) (ScreenLock, error) {
	line := strings.TrimSpace(raw)
	if !strings.HasPrefix(line, screenLockPrefix) {
		return ScreenLockUnknown, fmt.Errorf("the key helper's session reading answered %q, which does not "+
			"start with %q", line, screenLockPrefix)
	}
	switch strings.TrimPrefix(line, screenLockPrefix) {
	case "yes":
		return ScreenLockLocked, nil
	case "no":
		return ScreenLockUnlocked, nil
	case "unknown":
		return ScreenLockUnknown, nil
	default:
		return ScreenLockUnknown, fmt.Errorf("the key helper's session reading answered %q, which names no "+
			"lock state this harness knows", line)
	}
}

// ScreenLock asks the helper whether the screen is locked.
func (d *KeyDriver) ScreenLock(ctx context.Context) (ScreenLock, error) {
	if d.helper == "" {
		return ScreenLockUnknown, fmt.Errorf("the key helper has not been built; call Build first")
	}
	callCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, d.helper, "--session").CombinedOutput()
	if err != nil {
		return ScreenLockUnknown, fmt.Errorf("ask the key helper whether the screen is locked: %w; it said: %s",
			err, strings.TrimSpace(string(out)))
	}
	return parseScreenLock(string(out))
}

// noFocusEdgeNote is what a run says when it asked for focus and the editor
// never took it.
//
// Pure, for the same reason showPhaseNote is: the wording of a finding must be
// testable without a running editor, and this is the finding that decides
// whether an unpainted panel is the product's fault or the desktop's.
func noFocusEdgeNote(presses int, lock ScreenLock, lockErr error, focus FocusReading) string {
	head := fmt.Sprintf("NO REAL FOCUS EDGE WAS PRODUCED: the driver activated Emacs for %d keypress(es) and "+
		"the editor's own `frame-focus-state` never once said it held desktop focus (%s). The pre-creation "+
		"drain holds on exactly that reading, so no panel below could have painted and the unpainted panels "+
		"are NOT a product finding", presses, focus.Describe())
	switch {
	case lockErr != nil:
		return head + fmt.Sprintf(". Whether the screen was locked could not be read: %v", lockErr)
	case lock == ScreenLockLocked:
		return head + ". THE SCREEN WAS LOCKED. Behind the login window the window server grants activation " +
			"to nobody, so `activate()` is accepted and changes nothing while a pid-addressed CGEvent still " +
			"reaches the process — which is why key delivery stayed healthy throughout. There is no way to " +
			"focus an application behind a locked screen: unlock the session and re-run"
	case lock == ScreenLockUnlocked:
		return head + ". The screen was NOT locked, so the activation was requested on an unlocked session " +
			"and declined anyway: macOS 14 and later make activation cooperative, and a request from a " +
			"process that is not itself an active application can be refused outright. That is a harness " +
			"defect in the key driver, not a paint defect"
	default:
		return head + ". Whether the screen was locked is UNKNOWN — the session dictionary would not answer — " +
			"so this cannot yet be attributed to the lock or to a declined activation"
	}
}

// WHERE FOCUS IS SUPPOSED TO BE WHEN A PHASE OF PRESSES ENDS, WHICH DEPENDS ON
// WHO OWNS IT (owner ruling, 2026-09-13).
//
// Under the old policy every press restored the previously frontmost
// application, so the only correct reading at the end of a phase was "focus is
// exactly where it started" and anything else was a failure to restore. Under
// the sweep's policy focus was taken ONCE before the first realtest and goes
// back ONCE from bin/realtest.sh's EXIT trap, so the correct reading at the end
// of a phase is that EMACS still has it — and a phase that handed focus back
// would be the defect, because the next realtest's presses would each have to
// steal it again.
//
// The two readings are asked by the same helper so a run can never assert one
// policy's expectation while running under the other.

// emacsApplicationName is what `System Events` calls the editor, which is what
// FrontmostApp answers with.
const emacsApplicationName = "Emacs"

// focusAfterPressesNote judges where focus ended up after a phase that pressed
// keys, and answers whether that is a finding.
//
// Pure, for the same reason noFocusEdgeNote is: the wording of a finding must
// be testable without a window server, and this one decides whether a run
// disturbed the owner's desktop.
//
// A SWEEP THAT ENDS WITH FOCUS SOMEWHERE ELSE IS NOT A FAILURE. The owner may
// click away at any moment, and the policy's answer to that is the next press
// re-taking focus and saying so (refocusNote), not a red run. It is REPORTED so
// a reader of the manifest can see it happened.
func focusAfterPressesNote(what string, sweepHolds bool, before, after string) (string, bool) {
	if !sweepHolds {
		if before != after {
			return fmt.Sprintf("%s left focus on %q, not on %q where it started: with no sweep holding "+
				"focus the driver activates Emacs for each keypress and must restore the prior frontmost "+
				"app, and here it did not", what, after, before), true
		}
		return fmt.Sprintf("%s restored focus to %q after momentarily activating Emacs for each keypress",
			what, after), false
	}
	if after == emacsApplicationName {
		return fmt.Sprintf("%s left Emacs frontmost, which is the sweep's policy: focus was taken once "+
			"before the first realtest and goes back to %q once, from bin/realtest.sh's EXIT trap, however "+
			"the sweep ends", what, before), false
	}
	return fmt.Sprintf("%s ended with %q frontmost rather than Emacs, so something took focus back during "+
		"the run — most likely the owner clicking away. Not a finding against either system: the next press "+
		"re-takes focus and says so, and the sweep still hands focus to %q at its end", what, after, before), false
}
