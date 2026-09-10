//go:build realtest

package realtest

import (
	"context"
	"encoding/json"
	"fmt"
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
//  1. CGEventPostToPid through the Swift helper, addressed to the Emacs pid,
//     with no activation.
//  2. osascript / System Events `key code`, addressed to the Emacs process.
//
// If NEITHER works without focus — or accessibility permission is missing —
// the run records exactly what failed and why and stops. It does NOT fall back
// to elisp: the owner rules on the alternative (docs/REALTEST-PLAN.md).

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
	d.Method = "CGEventPostToPid via keydriver.swift (no activation)"
	return nil
}

// Press posts one chord to the Emacs process.
func (d *KeyDriver) Press(ctx context.Context, chord Chord) error {
	if d.helper == "" {
		return fmt.Errorf("the key helper has not been built; call Build first")
	}
	callCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, d.helper,
		fmt.Sprint(d.Pid), fmt.Sprint(chord.Keycode), strings.Join(chord.Modifiers, ",")).CombinedOutput()
	if err != nil {
		return fmt.Errorf("post %s (keycode %d, %s) to pid %d: %w; the helper said: %s",
			chord.Emacs, chord.Keycode, strings.Join(chord.Modifiers, "+"), d.Pid, err, strings.TrimSpace(string(out)))
	}
	return nil
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
