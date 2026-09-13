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
	// MarkFree says Emacs is NOT OBLIGED to leave an input mark for this
	// chord, so the absence of both marks says nothing about whether it
	// arrived.
	//
	// IT IS A PROPERTY OF THE CHORD AND THE MOMENT, NOT OF THE CHORD ALONE,
	// and the quit character is the whole reason the field exists. Where a
	// `C-g` lands while Emacs is BUSY — inside a command, with no key read
	// standing — `kbd_buffer_store_buffered_event` hands the `quit_char` to
	// `handle_interrupt` instead of storing it, so `read_char` never returns
	// it and `record_char` never records it, and the `quit-flag` it arms is
	// taken by whatever eventually notices. That press leaves no mark and can
	// only be judged by its effect.
	//
	// WHERE A KEY READ IS STANDING IT IS AN ORDINARY KEY, and this field must
	// be false for it. Inside a minibuffer read, `read_key_sequence` reads the
	// `C-g` as a key sequence like any other and dispatches it to
	// `abort-minibuffers` / `minibuffer-keyboard-quit`, so `record_char`
	// records it and `(recent-keys)` grows. The 2026-09-13 11:10 sweep proves
	// it from both sides in one run: realtest 5's ring ends
	// `... SPC <tab> n C-g <escape>` for a `C-g` that closed the prompt, while
	// realtests 6, 7 and 8 reported "the quit character leaves NO mark" for
	// three presses whose rings gained nothing at all — which, on the corrected
	// model, is a key that never arrived.
	//
	// A mark-free chord is judged by its EFFECT instead (DeliveryEffect), and
	// where no effect is supplied its delivery is UNDETERMINED and blamed on
	// nobody.
	MarkFree bool
	// MarkFreeWhy says why this chord leaves no mark, so a reader of a finding
	// can check the judgement rather than take it.
	MarkFreeWhy string
	// Interrupting says this chord is the QUIT CHARACTER, whose meaning
	// depends on what Emacs is doing at the instant it lands.
	//
	// THE HARNESS IS THE THING MOST LIKELY TO BE MAKING EMACS BUSY. Every
	// other key is queued while Emacs executes lisp and read as a key when it
	// next looks; a `quit_char` is not queued at all —
	// `kbd_buffer_store_buffered_event` hands it to `handle_interrupt`, so it
	// never reaches `read_key_sequence`, never reaches
	// `minibuffer-keyboard-quit`, and never reaches `record_char`. The prompt
	// stays up and the ring does not grow, which is indistinguishable from a
	// key that was dropped by the window server.
	//
	// AND THE CONFIRMATION USED TO BE WHAT MADE EMACS BUSY. `confirm` opened
	// its first emacsclient probe within a millisecond of the post and then
	// re-probed every 50ms, twice per turn once an effect was supplied, so the
	// editor was executing this harness's own lisp for most of the window in
	// which it had to read the key. A press of the same chord, to the same
	// pid, with nothing talking to Emacs, was recorded and dismissed the
	// prompt; six presses inside the 2026-09-13 12:13 sweep were not. The
	// difference was the probe traffic, and it is ours.
	//
	// So an interrupting chord gets a QUIET WINDOW: the editor is left alone
	// after the post for long enough to turn its run loop and read the key,
	// and re-read at a slower cadence afterwards. The hold is what keeps the
	// target's key window open across it (keydriver.swift).
	Interrupting bool
	// InterruptingWhy says why, so a reader of a finding can check the
	// judgement rather than take it.
	InterruptingWhy string
	// Recorded is how `(key-description (recent-keys))` SPELLS this chord
	// after Emacs has read it, where that is not `Emacs`.
	//
	// THE TWO SPELLINGS ARE NOT ALWAYS THE SAME KEY, and the 2026-09-13 sweep
	// is what that costs. `kbd` reads "TAB" as the ASCII character 9, and
	// `key-description` renders that character "TAB"; but the physical tab key
	// on a GUI (NS) build does not arrive as character 9 at all — it arrives
	// as the function key symbol `tab`, which `key-description` renders
	// `<tab>`. So a run that pressed keycode 48 and then asked whether the
	// ring "contains SPC TAB n" asked about a key sequence Emacs never
	// records, and reported `SPC TAB n` as uncreditable in realtest 5 and
	// `SPC TAB f` in realtest 7 while the ring plainly ended
	// `<escape> <escape> SPC <tab> n`.
	//
	// The credit check therefore compares against EMACS'S OWN SPELLING
	// (`SpellRecorded`) and the human-readable notes keep `Emacs`, which is
	// the spelling a reader would type. A chord that says nothing here is
	// recorded exactly as it is typed, which is the ordinary case.
	Recorded string
}

// recorded is Recorded with the answer every chord that says nothing gives:
// Emacs records it under the same name it is typed by.
func (c Chord) recorded() string {
	if c.Recorded != "" {
		return c.Recorded
	}
	return c.Emacs
}

// SpellRecorded renders a chord sequence the way `(recent-keys)` renders it
// once Emacs has read it.
//
// It is the ONLY spelling a `(recent-keys)` assertion may be written against.
// wsActSpell renders the same sequence the way a reader types it, and the two
// differ wherever a physical key arrives as a function key symbol rather than
// as the ASCII character `kbd` reads its name as.
func SpellRecorded(sequence []Chord) string {
	parts := make([]string, 0, len(sequence))
	for _, chord := range sequence {
		parts = append(parts, chord.recorded())
	}
	return strings.Join(parts, " ")
}

// markFreeWhy is MarkFreeWhy with the answer every mark-free chord that says
// nothing gives.
func (c Chord) markFreeWhy() string {
	if c.MarkFreeWhy != "" {
		return c.MarkFreeWhy
	}
	return "this chord is recorded as leaving no input mark, and no reason was given"
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
	// KeepFocus says THE SWEEP OWNS THE DESKTOP, so a press activates the
	// target and LEAVES it frontmost instead of restoring whatever was there
	// before (owner ruling, 2026-09-13; sweepfocus.go carries it).
	//
	// It is set from the environment bin/realtest.sh exports once its own
	// focus take has run, and never guessed: a press that kept focus while
	// nobody held the handback would leave the owner's desktop on Emacs after
	// the run, which is the one outcome this whole policy exists to avoid.
	KeepFocus bool
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
	d.Method = "CGEventPostToPid via keydriver.swift (activates Emacs for the keypress; " + d.focusPolicy() + ")"
	return nil
}

// Press posts one chord to the Emacs process and, when a Client is attached,
// does not answer until Emacs's own account says the key arrived.
func (d *KeyDriver) Press(ctx context.Context, chord Chord) error {
	_, err := d.PressWithReceipt(ctx, chord)
	return err
}

// PressWithEffect posts one chord and confirms it by the EFFECT it is pressed
// for, alongside Emacs's own input marks.
//
// It exists for the mark-free chords (Chord.MarkFree): a key Emacs is not
// obliged to record cannot be confirmed by the marks, and the thing the caller
// is about to look at anyway — the standing minibuffer closing — is the only
// account of that key there is. Supplying it here rather than checking it
// afterwards is what keeps the verdict SINGLE-VALUED: one observation settles
// both "did the key arrive" and "did it do what it is pressed for", so the run
// cannot report a delivery failure and a product finding about the same press.
//
// The effect is polled INSIDE the hold, so the target keeps its key window for
// exactly as long as the answer takes and no longer.
func (d *KeyDriver) PressWithEffect(ctx context.Context, chord Chord, effect *DeliveryEffect) (DeliveryReceipt, error) {
	return d.press(ctx, chord, effect)
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
	return d.press(ctx, chord, nil)
}

// press is the one press path; PressWithReceipt and PressWithEffect differ
// only in whether an effect is supplied to confirm a mark-free chord by.
func (d *KeyDriver) press(ctx context.Context, chord Chord, effect *DeliveryEffect) (DeliveryReceipt, error) {
	if d.helper == "" {
		return DeliveryReceipt{Chord: chord}, fmt.Errorf("the key helper has not been built; call Build first")
	}

	if d.Client == nil {
		out, err := d.post(ctx, chord, nil)
		d.noteRefocus(chord, out)
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
		// focus is read INSIDE the hold, which is the only window in which the
		// question means anything: the helper restores the previously frontmost
		// application the moment the hold ends, so a reading taken afterwards
		// would always say unfocused.
		var focus FocusReading
		observed := false
		verdict, reason := DeliveryUndetermined, "the helper never reported the key as posted"
		out, err := d.post(ctx, chord, func() {
			verdict, reason, after, observed = d.confirm(ctx, chord, before, effect)
			focus = ReadEmacsFocus(ctx, d.Client)
		})
		d.noteRefocus(chord, out)

		receipt = DeliveryReceipt{
			Chord:          chord,
			Verdict:        verdict,
			Reason:         reason,
			Attempts:       attempt,
			Helper:         out,
			Before:         before,
			After:          after,
			Elapsed:        time.Since(started),
			Focus:          focus,
			EffectObserved: observed,
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
// AN EFFECT, WHERE ONE IS SUPPLIED, IS THE STRONGEST ACCOUNT THERE IS and is
// asked first: a key whose effect has happened arrived, whatever the marks say
// or fail to say.
func (d *KeyDriver) confirm(ctx context.Context, chord Chord, before InputMark,
	effect *DeliveryEffect) (DeliveryVerdict, string, InputMark, bool) {
	deadline := time.Now().Add(keyDeliveryConfirmCeiling)
	var after InputMark
	effectFailure := ""

	// THE QUIET WINDOW COMES FIRST, AND ONLY FOR A CHORD THAT NEEDS IT. Probing
	// an editor that is about to read an ordinary key costs nothing; probing
	// one that is about to read a quit character turns that key into an
	// interrupt (Chord.Interrupting). So the editor is left alone for exactly
	// as long as reading the key takes it, and no chord that does not need it
	// pays a millisecond.
	if delay := confirmFirstProbeDelay(chord); delay > 0 {
		select {
		case <-ctx.Done():
			return DeliveryUndetermined, "the press was cancelled before the editor was read", after, false
		case <-time.After(delay):
		}
	}

	for {
		if effect != nil && effect.Observed != nil {
			happened, err := effect.Observed(ctx)
			switch {
			case err != nil:
				// NOT SWALLOWED. An effect probe that would not answer has said
				// nothing about the key, and reading its silence as "the effect
				// has not happened" is how a press that could not be judged
				// came back as one that was judged absent.
				effectFailure = err.Error()
			case happened:
				return DeliveryArrived, "the key's own effect happened: " + effect.What, after, true
			}
		}
		after = ReadInputMark(ctx, d.Client)
		verdict, reason := judgeWithEffectProbe(chord, before, after, effectFailure)
		if verdict == DeliveryArrived {
			return verdict, reason, after, false
		}
		if verdict == DeliveryUndetermined && before.ProbeFailure == "" && after.ProbeFailure == "" && effect == nil {
			// The ring cannot distinguish this press, and it will not start
			// being able to. Holding focus out to the ceiling for a reading
			// that cannot change would cost the owner's desktop for nothing.
			// WITH AN EFFECT TO WAIT ON IT IS A DIFFERENT QUESTION: that one
			// CAN change, and the hold is what keeps the target's key window
			// open long enough for it to.
			return verdict, reason, after, false
		}
		if ctx.Err() != nil || !time.Now().Before(deadline) {
			return verdict, reason, after, false
		}
		select {
		case <-ctx.Done():
			return verdict, reason, after, false
		case <-time.After(confirmPollInterval(chord)):
		}
	}
}

// keyDriverArgs spells one press for the helper.
//
// Its own function so the spelling is testable without a window server: a
// `--hold` that went missing would put the harness back on the fixed-span
// handback that dropped keys, and nothing else would notice.
//
// AND `--hold` CARRIES A SECOND MEANING THE HELPER RELIES ON: it says this
// side will read Emacs's marks back and report an undelivered key itself. That
// is what lets keydriver.swift treat its key-window reading as advisory and
// post anyway, instead of refusing a press on a prediction the editor is about
// to answer for real. It is therefore passed exactly when `Client` is attached
// — the same condition under which `confirm` runs — and never on a blind
// press, where the helper's refusal is the only thing standing between a
// dropped key and silence.
//
// `--keep-focus` IS THE SWEEP'S POLICY, NOT THE PRESS'S. It says the sweep took
// focus before its first realtest and will hand it back from its EXIT trap, so
// this press must not undo the steal one keystroke at a time. Its own function
// for the same reason the hold is: a flag that went missing would silently put
// the per-press flicker back, and only a test of the spelling would notice.
func keyDriverArgs(pid int, chord Chord, hold bool, keepFocus bool) []string {
	args := make([]string, 0, 5)
	if hold {
		args = append(args, fmt.Sprintf("--hold=%g", keyDeliveryHoldCeiling.Seconds()))
	}
	if keepFocus {
		args = append(args, keyDriverKeepFocusFlag)
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

	command := exec.CommandContext(callCtx, d.helper, keyDriverArgs(d.Pid, chord, held != nil, d.KeepFocus)...)
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

// keyDriverKeepFocusFlag is what tells the helper the sweep is holding focus.
const keyDriverKeepFocusFlag = "--keep-focus"

// keyDriverRefocusedMarker is what the helper's receipt says when it found the
// target NOT frontmost and had to take focus for this press.
const keyDriverRefocusedMarker = "refocused=yes"

// focusPolicy names, in the manifest's words, who hands focus back.
func (d *KeyDriver) focusPolicy() string {
	if d.KeepFocus {
		return "the SWEEP holds focus from its first press to its last and hands it back once at the end"
	}
	return "restores the previously frontmost application after each press"
}

// refocusNote is what a press says when it found Emacs not frontmost.
//
// A pure function of the chord and the helper's line, for the same reason every
// other note here is: the wording of a finding must be testable without a
// window server. It is a NOTE and never an error — re-taking focus is what the
// press is supposed to do, and the two reasons it happens (the owner clicked
// away, a realtest just relaunched Emacs) are both expected.
func refocusNote(chord Chord, helper string) string {
	return fmt.Sprintf("EMACS WAS NOT FRONTMOST WHEN %s WAS PRESSED, so the driver took focus for it again "+
		"and did NOT hand it back: the sweep holds focus from its first press to its last. Either the owner "+
		"clicked away mid-run, or this is the first press against an Emacs a realtest has just cold-started. "+
		"The helper said: %s", chord.Emacs, helper)
}

// noteRefocus records a re-take when the helper reported one.
func (d *KeyDriver) noteRefocus(chord Chord, helper string) {
	if strings.Contains(helper, keyDriverRefocusedMarker) {
		d.Notes = append(d.Notes, refocusNote(chord, helper))
	}
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
