//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// WHERE DOES THE QUIT KEY GO? A DIAGNOSTIC CAPTURE, NOT A FIX.
//
// THE UNSOLVED FACT, as of the 2026-09-13 sweeps. In every sweep since 12:13
// the `C-g` pressed at a standing prompt in realtests 5 through 8 is posted
// with a CLEAN helper receipt — the window server accepted it, the target was
// held key, accessibility answered frontmost — twice per prompt, and:
//
//   - Emacs's `(recent-keys)` never gains it, and a `C-g` read by a standing
//     minibuffer read IS recorded (delivery.go carries the derivation);
//   - no quit reaches the command loop at all: the lead installed a
//     `command-error-function` for a solo realtest 5 run and it saw ZERO quit
//     signals;
//   - no deferred-quit record is written, so the product's own quit path never
//     ran either.
//
// AND THE SAME HELPER, THE SAME ARGUMENTS, THE SAME EMACS, PRESSED BY HAND at a
// timer-raised `read-string` dismisses the prompt and IS recorded — held or
// released. So the key is being lost somewhere between "a CGEvent accepted for
// this pid" and "the editor's input", and every reading this harness currently
// takes is on the wrong side of that gap.
//
// SO THIS FILE ADDS A CAPTURE, AND NOTHING ELSE. It does not change how the
// quit is pressed, judged or reported, and it does not touch the product. It
// takes ONE emacsclient eval immediately after the quit press's quiet window,
// and one BEFORE the press with the prompt standing, and writes both verbatim
// into the run directory and into the failure note. The next sweep then answers
// where the key went from evidence rather than from another theory:
//
//   - `(recent-keys)`, `last-input-event`, `last-event-frame`, `this-command`
//     and `real-last-command` say whether ANY input reached the editor in that
//     window, and which frame it was addressed to;
//   - `(selected-frame)`'s name and `(frame-parameter nil 'name)` say which
//     frame the eval itself is running against, which is the frame every other
//     probe here has silently assumed;
//   - `(active-minibuffer-window)`, its frame, `(minibuffer-depth)` and the
//     minibuffer contents say whether a read is standing and WHERE — a prompt
//     on a frame that is not the key window would explain a key that arrives
//     and does nothing;
//   - `quit-flag`, `inhibit-quit` and `unread-command-events` say whether a
//     quit was armed and swallowed, or is still queued unread;
//   - `(current-input-mode)` says whether the quit CHARACTER is still `C-g` and
//     whether interrupts are being read at all;
//   - the xwidget readings say whether the selected window is showing a webkit
//     view, which is the one buffer class on this build that hosts a native
//     subview capable of taking first responder away from the Emacs frame — a
//     key window whose first responder is a WKWebView is exactly the shape that
//     accepts a CGEvent and never reaches the keymap;
//   - `(frame-focus-state)` per frame says which frame the editor itself
//     believes holds desktop focus, which is what the product's own predicate
//     reads.
//
// THE CAPTURE IS ONE EVAL, AND ITS COST IS DELIBERATE. Probing the editor while
// a quit character is in flight is exactly what `Chord.Interrupting` and the
// quiet window exist to avoid, so this is taken AFTER the quiet window has
// elapsed — at the same instant the first mark probe would have run — and never
// twice.
//
// EVERY FIELD IS WRAPPED ON ITS OWN. A capture in which one form signals must
// still carry the other seventeen: the whole point is that nobody yet knows
// which reading is the interesting one.

const (
	// quitProbeFileName is where a run's captures land, inside the run
	// directory the whole sweep shares.
	quitProbeFileName = "quit-probe.txt"

	// quitProbeFieldSeparator joins a field's name to its value. A tab,
	// because every value is rendered with `%S` and a tab cannot appear
	// unescaped in that output.
	quitProbeFieldSeparator = "\t"

	// quitProbeRecentKeysTail is how much of the key ring is carried.
	//
	// The ring is `lossage-size` keys — 300 by default, a full screen of text
	// rendered — and the question here is only what arrived AROUND the press,
	// so the tail is what is read. Wide enough to hold the whole of a realtest's
	// longest chord sequence with room either side of it.
	quitProbeRecentKeysTail = 400
)

// quitProbeFields is what one capture reads, in the order the capture prints
// them.
//
// A TABLE RATHER THAN ONE LITERAL FORM, so a field can be tested for by name
// without matching elisp, and so the reason a field is here can sit beside it.
var quitProbeFields = []struct {
	// Name is what the capture calls the field.
	Name string
	// Form is the elisp that answers it.
	Form string
	// Why says what a reader learns from it.
	Why string
}{
	{
		Name: "recent-keys-tail",
		Form: fmt.Sprintf(`(let ((keys (key-description (recent-keys)))) `+
			`(if (> (length keys) %d) (substring keys %d) keys))`,
			quitProbeRecentKeysTail, -quitProbeRecentKeysTail),
		Why: "whether ANY key reached the editor's input around the press",
	},
	{Name: "last-input-event", Form: `last-input-event`, Why: "the last event the editor actually read"},
	{Name: "last-event-frame", Form: `last-event-frame`, Why: "which frame that event was addressed to"},
	{
		Name: "selected-frame-name",
		Form: `(frame-parameter (selected-frame) 'name)`,
		Why:  "which frame this eval is running against",
	},
	{
		Name: "nil-frame-name",
		Form: `(frame-parameter nil 'name)`,
		Why:  "the frame every other probe here silently means by `nil`",
	},
	{
		Name: "active-minibuffer-window",
		Form: `(active-minibuffer-window)`,
		Why:  "whether a minibuffer read is standing at all",
	},
	{
		Name: "active-minibuffer-frame",
		Form: `(and (active-minibuffer-window) (window-frame (active-minibuffer-window)))`,
		Why:  "WHICH frame is holding the prompt; a prompt off the key window explains a key that does nothing",
	},
	{Name: "minibuffer-depth", Form: `(minibuffer-depth)`, Why: "how many reads are nested"},
	{Name: "quit-flag", Form: `quit-flag`, Why: "whether a quit is armed and not yet taken"},
	{Name: "inhibit-quit", Form: `inhibit-quit`, Why: "whether a quit would be swallowed where it lands"},
	{
		Name: "unread-command-events",
		Form: `unread-command-events`,
		Why:  "whether the key is queued unread rather than lost",
	},
	{
		Name: "current-input-mode",
		Form: `(current-input-mode)`,
		Why:  "whether the quit CHARACTER is still C-g and interrupts are being read",
	},
	{Name: "this-command", Form: `this-command`, Why: "what the command loop is running now"},
	{Name: "real-last-command", Form: `real-last-command`, Why: "the last command the loop actually dispatched"},
	{
		Name: "minibuffer-contents",
		Form: `(if (active-minibuffer-window) ` +
			`(with-current-buffer (window-buffer (active-minibuffer-window)) (minibuffer-contents)) ` +
			`"no minibuffer standing")`,
		Why: "what has been typed into the standing prompt, if anything",
	},
	{
		Name: "xwidget-webkit-current-session",
		Form: `(if (fboundp 'xwidget-webkit-current-session) (xwidget-webkit-current-session) ` +
			`'no-xwidget-webkit-support)`,
		Why: "whether a webkit view is live in this Emacs at all",
	},
	{
		Name: "selected-window-buffer",
		Form: `(list (buffer-name (window-buffer (selected-window))) ` +
			`(buffer-local-value 'major-mode (window-buffer (selected-window))) ` +
			`(if (with-current-buffer (window-buffer (selected-window)) ` +
			`(derived-mode-p 'xwidget-webkit-mode)) 'xwidget 'not-xwidget))`,
		Why: "whether the selected window is showing an xwidget, whose native subview can hold first responder",
	},
	{
		Name: "frame-focus-states",
		Form: `(mapcar (lambda (f) (cons (frame-parameter f 'name) (frame-focus-state f))) (frame-list))`,
		Why:  "which frame the EDITOR believes holds desktop focus, per frame",
	},
}

// quitProbeForm is the one eval a capture takes.
//
// One form, not eighteen round trips: the whole capture has to describe ONE
// instant, and eighteen probes taken a round trip apart would straddle whatever
// the key was doing. Every field is wrapped in its own `condition-case`, so a
// form that signals carries its error in place of its value and the other
// seventeen still arrive — nobody yet knows which reading is the interesting
// one, so losing the capture over one of them would be losing the evidence.
func quitProbeForm() string {
	parts := make([]string, 0, len(quitProbeFields)*2+1)
	parts = append(parts, "(concat")
	for _, field := range quitProbeFields {
		parts = append(parts, fmt.Sprintf(
			` %q %q (condition-case quit-probe-error (format "%%S" %s) `+
				`(error (format "PROBE-ERROR %%S" quit-probe-error))) "\n"`,
			field.Name, quitProbeFieldSeparator, field.Form))
	}
	parts = append(parts, ")")
	return strings.Join(parts, "")
}

// QuitProbe is one capture of the editor's input state around the quit press.
type QuitProbe struct {
	// When names the moment, in the words the capture carries.
	When string
	// At is when the capture was taken.
	At time.Time
	// Raw is exactly what the editor answered, unparsed. It is carried
	// verbatim because the whole artifact is evidence the owner rules on and a
	// parse is a place for this harness to lose the field that mattered.
	Raw string
	// Failure names why the editor did not answer, empty when it did.
	Failure string
}

// ReadQuitProbe takes one capture.
//
// It never answers an error, for the same reason ReadInputMark does not: an
// editor that would not answer has said nothing, and that has to travel WITH
// the capture rather than replace it — a probe that failed is itself a reading
// about where the key went.
func ReadQuitProbe(ctx context.Context, client *Client, when string) QuitProbe {
	probe := QuitProbe{When: when, At: time.Now()}
	raw, err := client.ReadString(ctx, quitProbeForm())
	if err != nil {
		probe.Failure = err.Error()
		return probe
	}
	probe.Raw = raw
	return probe
}

// Render writes one capture the way both the file and the failure note carry
// it.
//
// Pure, for the same reason every note here is: the wording of evidence must be
// testable without a running editor.
func (p QuitProbe) Render() string {
	head := fmt.Sprintf("---- QUIT DIAGNOSTIC PROBE: %s (%s) ----",
		p.When, p.At.Format(time.RFC3339Nano))
	if p.Failure != "" {
		return head + "\nTHE EDITOR DID NOT ANSWER THIS CAPTURE: " + p.Failure + "\n"
	}
	body := p.Raw
	if !strings.HasSuffix(body, "\n") {
		body += "\n"
	}
	return head + "\n" + body
}

// QuitProbePath is where a run's captures land.
func QuitProbePath(runDir string) string {
	return filepath.Join(runDir, quitProbeFileName)
}

// AppendQuitProbes adds captures to the run's probe file.
//
// APPENDED, NEVER REWRITTEN. Realtests 5 through 8 each press the quit at more
// than one prompt and each runs in its own `go test` invocation over one shared
// run directory, so a writer that truncated would leave only the last press's
// capture — and the press that matters is not known in advance.
func AppendQuitProbes(runDir string, probes ...QuitProbe) (string, error) {
	path := QuitProbePath(runDir)
	if err := os.MkdirAll(runDir, 0o755); err != nil {
		return "", fmt.Errorf("make the run directory for the quit diagnostic probe: %w", err)
	}
	file, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return "", fmt.Errorf("open the quit diagnostic probe file %s: %w", path, err)
	}
	defer file.Close()
	for _, probe := range probes {
		if _, err := file.WriteString(probe.Render() + "\n"); err != nil {
			return "", fmt.Errorf("write a quit diagnostic capture to %s: %w", path, err)
		}
	}
	return path, nil
}

// quitProbeNote renders both captures for the failure note, VERBATIM.
//
// The whole capture goes in the note and not only a pointer to the file,
// because the note is what a reader of a failing run sees and the file is what
// they have to go and find. The plan's bar for this loop is "surface them to
// the owner, with evidence, verbatim" (docs/REALTEST-PLAN.md), and a path is
// not evidence.
func quitProbeNote(prompt string, before, after QuitProbe, path string) string {
	return fmt.Sprintf("QUIT DIAGNOSTIC CAPTURE for the `C-g` pressed at %q. This is EVIDENCE, not a verdict: "+
		"in every sweep since 2026-09-13 12:13 this press is posted with a clean helper receipt, twice, and "+
		"Emacs's ring never gains it, no quit reaches the command loop, and no deferred-quit record is "+
		"written — while the same helper with the same arguments against the same Emacs, pressed by hand at a "+
		"timer-raised read-string, dismisses the prompt and IS recorded. The two captures below bracket the "+
		"press, and the same bytes are in %s.\n\n%s\n%s",
		prompt, path, before.Render(), after.Render())
}
