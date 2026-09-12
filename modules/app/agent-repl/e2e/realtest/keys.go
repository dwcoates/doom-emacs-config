//go:build realtest

package realtest

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// REAL KEY EVENTS, NOT ELISP ACTS. Everything a realtest sends the editor is a
// key the owner could have pressed, delivered so that Emacs's own keymap is
// what resolves it. This is the driver; keydriver.swift is the mechanism and
// carries the reasoning for CGEventPostToPid.
//
// Realtest 1 has no acts — it only observes a startup — so the driver is
// PROVEN here rather than used: the run ends by pressing `s-}` and `M-2` once
// each, both harmless workspace switches, and reads `(recent-keys)` and
// `last-command` back to confirm Emacs's keymap resolved them. A driver that is
// only exercised by the realtest that first needs it is a driver that fails on
// the day it matters.
//
// THE ORDER OF ATTEMPTS IS FIXED and there is no third:
//
//  1. The Swift helper: it activates the target Emacs for the instant of the
//     keypress, posts CGEventPostToPid addressed to the Emacs pid, and restores
//     the previously frontmost application. A no-activation post reached nothing
//     in run 3 because a hidden background app has no key window for the event
//     to land on; keydriver.swift carries the full reasoning. The momentary
//     focus is the ONE place a realtest brings Emacs forward, and it is bounded
//     to the keypress and reversed immediately.
//  2. osascript / System Events `key code`, addressed to the Emacs process,
//     which delivers to whatever is frontmost and so also requires focus.
//
// If NEITHER works — or accessibility permission is missing — the run records
// exactly what failed and why and stops. It does NOT fall back to elisp: the
// owner rules on the alternative (docs/REALTEST-PLAN.md).

// Chord is one keystroke, spelled the way Emacs spells it and carrying the
// macOS virtual keycode that produces it.
//
// The two spellings are kept together deliberately. Emacs's own name is what
// the assertion reads back out of `(recent-keys)`, and the keycode is what the
// window server needs; a table that held only one of them would need the other
// derived at the call site, which is where a wrong keycode turns into "the
// binding is broken".
type Chord struct {
	// Emacs is the chord as `kbd` spells it, e.g. "s-}".
	Emacs string
	// Keycode is the macOS virtual keycode for the physical key, on a US
	// layout.
	Keycode int
	// Modifiers are the helper's modifier words.
	Modifiers []string
	// Why says what the chord does, so a reader knows why it is safe to send.
	Why string
	// Repeatable says a SECOND delivery of this chord is harmless.
	//
	// It gates the retry of a key the editor's own account says was dropped
	// (delivery.go). A retry is not free: a post that was dropped at dispatch
	// may still be sitting in the target's queue, so re-posting a key that
	// ADVANCES state — a workspace switch, a leader sequence, a letter that
	// completes a command — risks the act happening twice, and a run that
	// reports one act when two happened is lying in a worse direction than one
	// that reports a key as undelivered. Only keys that return the editor to a
	// resting state, where a second press is a no-op, are repeatable.
	//
	// The zero value is the safe one: a chord says nothing, and it is posted
	// exactly once.
	Repeatable bool
	// RepeatWhy says why this chord is, or is not, repeatable, so a reader of
	// a finding can check the judgement rather than take it.
	RepeatWhy string
}

// repeatWhy is RepeatWhy with the answer every chord that says nothing gives.
func (c Chord) repeatWhy() string {
	if c.RepeatWhy != "" {
		return c.RepeatWhy
	}
	return "it advances the editor's state, so a second delivery would not be a no-op"
}

// The chords the self-test presses. Both are harmless: they change which
// workspace is selected and nothing else, and the editor is left standing
// afterwards either way.
//
// On this Emacs build the Command key is `super` and the Option key is `meta`
// (the NS defaults, which ~/.config/doom does not override), so `s-}` is
// Command+Shift+] and `M-2` is Option+2.
var (
	// SwitchRight is `s-}`, bound to `agent-repl-switch-right`
	// (lisp/keybindings.el).
	SwitchRight = Chord{
		Emacs:     "s-}",
		Keycode:   30, // ]
		Modifiers: []string{"command", "shift"},
		Why:       "selects the next workspace tab; it changes the selection and nothing else",
	}
	// SwitchToSecond is `M-2`, bound to `agent-repl-switch-to-workspace-2`
	// through the numerals minor-mode map.
	SwitchToSecond = Chord{
		Emacs:     "M-2",
		Keycode:   19, // 2
		Modifiers: []string{"option"},
		Why:       "selects the second drawn workspace tab; it changes the selection and nothing else",
	}
)

// KeyDriver posts real key events to one process.
type KeyDriver struct {
	// Pid is the Emacs process.
	Pid int
	// Scratch is where the compiled helper lands.
	Scratch string
	// Client is the read-only channel to the same Emacs, and it is what turns
	// a post into a DELIVERY. With it, every press holds the target key until
	// Emacs's own account of its input shows the key arriving, and a key that
	// never arrives is reported as a harness failure instead of being waited
	// out downstream (delivery.go). Without it a press is posted blind, which
	// is the old behaviour and is kept only for a caller that has no channel
	// to the editor.
	Client *Client
	// Notes collects what the presses had to say about themselves — a retry, an
	// unconfirmable reading — for a caller to drain into its manifest. An
	// undelivered key is not here: that is the press's error.
	Notes []string
	// helper is the compiled keydriver path, once built.
	helper string
	// Method names which of the two mechanisms is in use, for the report.
	Method string
	// Trusted is whether this process holds accessibility trust.
	Trusted bool
}

// keyDriverSource is where keydriver.swift lives relative to this package.
// Located from the test's own working directory, which `go test` sets to the
// package directory.
const keyDriverSource = "keydriver.swift"

// Build compiles the helper and establishes which mechanism can be used.
//
// It returns an error naming EXACTLY what failed — a missing swiftc, a
// compile failure, missing accessibility trust — because that message is the
// evidence the owner rules on when key delivery turns out to be impossible.
func (d *KeyDriver) Build(ctx context.Context) error {
	if _, err := exec.LookPath("swiftc"); err != nil {
		return fmt.Errorf("swiftc is not available, so the CGEvent key helper cannot be compiled: %w", err)
	}
	source, err := filepath.Abs(keyDriverSource)
	if err != nil {
		return fmt.Errorf("locate %s: %w", keyDriverSource, err)
	}
	if _, err := os.Stat(source); err != nil {
		return fmt.Errorf("the key helper source is missing at %s: %w", source, err)
	}

	binary := filepath.Join(d.Scratch, "keydriver")
	buildCtx, cancel := context.WithTimeout(ctx, 2*time.Minute)
	defer cancel()
	out, err := exec.CommandContext(buildCtx, "swiftc", "-O", "-o", binary, source).CombinedOutput()
	if err != nil {
		return fmt.Errorf("compile the CGEvent key helper: %w; swiftc said:\n%s", err, string(out))
	}
	d.helper = binary

	checkCtx, cancel2 := context.WithTimeout(ctx, 30*time.Second)
	defer cancel2()
	checkOut, checkErr := exec.CommandContext(checkCtx, binary, "--check").CombinedOutput()
	d.Trusted = strings.TrimSpace(string(checkOut)) == "trusted"
	if !d.Trusted {
		// NOT a fallback to elisp, and not a silent retry. An untrusted
		// process's synthetic events are dropped by the window server without
		// an error, so a send that "worked" would prove nothing at all.
		return fmt.Errorf(
			"this process does not hold macOS accessibility trust, so CGEvent key posting would be silently dropped (helper said %q, exit %v). "+
				"Grant Accessibility to the terminal or agent process that runs bin/realtest.sh in System Settings > Privacy & Security > Accessibility, then re-run. "+
				"No elisp fallback is taken: the owner rules on the alternative",
			strings.TrimSpace(string(checkOut)), checkErr)
	}
	d.Method = "CGEventPostToPid via keydriver.swift (activates Emacs for the keypress, restores prior focus)"
	return nil
}

// Press posts one chord to the Emacs process and, when a Client is attached,
// does not answer until Emacs's own account says the key arrived.
func (d *KeyDriver) Press(ctx context.Context, chord Chord) error {
	_, err := d.PressWithReceipt(ctx, chord)
	return err
}

// DrainNotes takes what the presses have said about themselves and empties the
// list, so a caller can put them in its manifest without reporting any of them
// twice.
func (d *KeyDriver) DrainNotes() []string {
	notes := d.Notes
	d.Notes = nil
	return notes
}

// PressWithReceipt posts one chord and answers what is known about it.
//
// WITH A CLIENT IT CANNOT SILENTLY DROP. The helper holds the target key while
// this side reads `(recent-keys)` and `quit-flag`, so the window in which
// AppKit will route the queued event stays open for as long as the reading
// takes, and the press answers an error the moment Emacs's own marks say the
// key never entered its input. A repeatable chord is posted again first
// (delivery.go says which chords those are and why the rest are not).
//
// WITHOUT ONE IT IS THE OLD BLIND POST, kept for a caller with no channel to
// the editor, and the receipt says so rather than claiming a delivery nobody
// checked.
func (d *KeyDriver) PressWithReceipt(ctx context.Context, chord Chord) (DeliveryReceipt, error) {
	if d.helper == "" {
		return DeliveryReceipt{Chord: chord}, fmt.Errorf("the key helper has not been built; call Build first")
	}

	if d.Client == nil {
		out, err := d.post(ctx, chord, nil)
		receipt := DeliveryReceipt{
			Chord:    chord,
			Verdict:  DeliveryUndetermined,
			Reason:   "the key driver has no read channel to this Emacs, so nothing was read back",
			Attempts: 1,
			Helper:   out,
		}
		if err != nil {
			return receipt, err
		}
		return receipt, nil
	}

	attempts := 1
	if chord.Repeatable {
		attempts = keyDeliveryAttempts
	}

	receipt := DeliveryReceipt{Chord: chord}
	for attempt := 1; attempt <= attempts; attempt++ {
		before := ReadInputMark(ctx, d.Client)
		started := time.Now()

		var after InputMark
		verdict, reason := DeliveryUndetermined, "the helper never reported the key as posted"
		out, err := d.post(ctx, chord, func() {
			verdict, reason, after = d.confirm(ctx, before)
		})

		receipt = DeliveryReceipt{
			Chord:    chord,
			Verdict:  verdict,
			Reason:   reason,
			Attempts: attempt,
			Helper:   out,
			Before:   before,
			After:    after,
			Elapsed:  time.Since(started),
		}
		if err != nil {
			return receipt, err
		}
		if verdict != DeliveryAbsent {
			break
		}
	}

	if receipt.Verdict == DeliveryAbsent {
		return receipt, receipt.deliveryError()
	}
	if !receipt.Confirmed() {
		d.Notes = append(d.Notes, receipt.Note())
	}
	return receipt, nil
}

// confirm reads the editor until it accounts for the key, or until the ceiling.
//
// It runs while the helper is HOLDING the target key, which is the whole point:
// the queued event is routed to whatever is the key window when Emacs next
// looks, so the reading and the key-ness of the window have to overlap.
//
// It returns as soon as there is nothing left to learn — the key arrived, or
// the reading is one that more time cannot resolve — so the ordinary press
// gives the owner's focus back in milliseconds. A probe that would not answer
// is retried until the ceiling, because that one IS transient.
func (d *KeyDriver) confirm(ctx context.Context, before InputMark) (DeliveryVerdict, string, InputMark) {
	deadline := time.Now().Add(keyDeliveryConfirmCeiling)
	for {
		after := ReadInputMark(ctx, d.Client)
		verdict, reason := judgeDelivery(before, after)
		if verdict == DeliveryArrived {
			return verdict, reason, after
		}
		if verdict == DeliveryUndetermined && before.ProbeFailure == "" && after.ProbeFailure == "" {
			// The ring cannot distinguish this press, and it will not start
			// being able to. Holding focus out to the ceiling for a reading
			// that cannot change would cost the owner's desktop for nothing.
			return verdict, reason, after
		}
		if ctx.Err() != nil || !time.Now().Before(deadline) {
			return verdict, reason, after
		}
		select {
		case <-ctx.Done():
			return verdict, reason, after
		case <-time.After(keyDeliveryPollInterval):
		}
	}
}

// keyDriverArgs spells one press for the helper.
//
// Its own function so the spelling is testable without a window server: a
// `--hold` that went missing would put the harness back on the fixed-span
// handback that dropped keys, and nothing else would notice.
func keyDriverArgs(pid int, chord Chord, hold bool) []string {
	args := make([]string, 0, 4)
	if hold {
		args = append(args, fmt.Sprintf("--hold=%g", keyDeliveryHoldCeiling.Seconds()))
	}
	return append(args, fmt.Sprint(pid), fmt.Sprint(chord.Keycode), strings.Join(chord.Modifiers, ","))
}

// post runs the helper once.
//
// `held` is what makes the two modes one path: when it is non-nil the helper is
// asked to hold the target key, and `held` is called once the event has been
// posted and before focus is handed back. When it is nil the helper posts and
// restores focus on its own fixed span, which is the blind mode.
//
// EVERYTHING THE HELPER SAID TRAVELS BACK, including on the paths that fail
// early: its refusals are the evidence a key-delivery finding is made of.
func (d *KeyDriver) post(ctx context.Context, chord Chord, held func()) (string, error) {
	callCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()

	command := exec.CommandContext(callCtx, d.helper, keyDriverArgs(d.Pid, chord, held != nil)...)
	var errors bytes.Buffer
	command.Stderr = &errors

	stdin, err := command.StdinPipe()
	if err != nil {
		return "", fmt.Errorf("open the key helper's stdin for %s: %w", chord.Emacs, err)
	}
	stdout, err := command.StdoutPipe()
	if err != nil {
		return "", fmt.Errorf("open the key helper's stdout for %s: %w", chord.Emacs, err)
	}
	if err := command.Start(); err != nil {
		return "", fmt.Errorf("start the key helper for %s: %w", chord.Emacs, err)
	}

	lines := make(chan string, 8)
	go func() {
		defer close(lines)
		scanner := bufio.NewScanner(stdout)
		for scanner.Scan() {
			lines <- scanner.Text()
		}
	}()

	said := make([]string, 0, 4)
	posted := false
	for line := range lines {
		said = append(said, line)
		if line == keyDeliveryPostedLine {
			posted = true
			break
		}
	}
	if posted && held != nil {
		held()
		// The release is best-effort by design: the helper's own ceiling ends
		// the hold anyway, so a write that cannot land delays the focus
		// handback rather than losing it.
		_, _ = io.WriteString(stdin, "release\n")
	}
	_ = stdin.Close()
	for line := range lines {
		said = append(said, line)
	}

	waitErr := command.Wait()
	report := strings.TrimSpace(strings.Join(append(said, strings.TrimSpace(errors.String())), " | "))
	if waitErr != nil {
		return report, fmt.Errorf("post %s (keycode %d, %s) to pid %d: %w; the helper said: %s",
			chord.Emacs, chord.Keycode, strings.Join(chord.Modifiers, "+"), d.Pid, waitErr, report)
	}
	return report, nil
}

// PressViaSystemEvents is the second mechanism, and it is NOT equivalent.
//
// System Events' `key code` delivers to whatever is frontmost, so reaching
// Emacs with it means Emacs has to BE frontmost — which disturbs the owner and
// is the thing the whole launch path exists to avoid. It is implemented so the
// run can state whether it works at all, and it is never used silently: the
// caller records that focus had to move.
func PressViaSystemEvents(ctx context.Context, chord Chord) error {
	modifiers := make([]string, 0, len(chord.Modifiers))
	for _, name := range chord.Modifiers {
		switch name {
		case "command":
			modifiers = append(modifiers, "command down")
		case "shift":
			modifiers = append(modifiers, "shift down")
		case "option":
			modifiers = append(modifiers, "option down")
		case "control":
			modifiers = append(modifiers, "control down")
		default:
			return fmt.Errorf("unknown modifier %q for System Events", name)
		}
	}
	script := fmt.Sprintf(`tell application "System Events" to tell process "Emacs" to key code %d using {%s}`,
		chord.Keycode, strings.Join(modifiers, ", "))
	callCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, "osascript", "-e", script).CombinedOutput()
	if err != nil {
		return fmt.Errorf("send %s through System Events: %w; osascript said: %s",
			chord.Emacs, err, strings.TrimSpace(string(out)))
	}
	return nil
}

// RecentKeys reads Emacs's own record of the keys it received.
//
// `recent-keys` is the editor's account of its INPUT, which is the only thing
// that can distinguish "the chord arrived and the keymap resolved it" from "the
// command ran because something called it". It is rendered through `key-description`
// so it comes back as the same spelling `kbd` takes.
func RecentKeys(ctx context.Context, client *Client) (string, error) {
	return client.ReadString(ctx, `(key-description (recent-keys))`)
}

// LastCommand reads which command Emacs's command loop last dispatched.
func LastCommand(ctx context.Context, client *Client) (string, error) {
	raw, err := client.Read(ctx, `(format "%s" last-command)`)
	if err != nil {
		return "", err
	}
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return "", fmt.Errorf("the last-command probe answered %s: %w", raw, err)
	}
	return s, nil
}
