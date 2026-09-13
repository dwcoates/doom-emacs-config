//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// THE SWEEP'S FOCUS IS STOLEN ONCE AND HANDED BACK ONCE (owner ruling,
// 2026-09-13).
//
// WHAT IT REPLACES. Every press used to activate Emacs, post its event, and
// reactivate whatever had been frontmost — so a sweep of eight realtests
// flickered the owner's desktop dozens of times, and nothing on the screen said
// whether a run was still going or had finished ten minutes ago.
//
// THE POLICY. The sweep brings Emacs forward before its first realtest and
// leaves it there; every press still verifies the target can receive a key and
// re-activates it when it cannot, but no press hands focus back. When the sweep
// ends — passing, failing, panicking or interrupted — the application that was
// frontmost before it started gets focus back. So the owner can watch the run,
// and the desktop coming back is how they know it is over.
//
// WHY THE TWO HALVES ARE SEPARATE HARNESS CHECKS. They belong to different
// processes: `bin/realtest.sh` runs the take before its first `go test` and the
// give-back from its EXIT trap, and each realtest is its own `go test`
// invocation in between. They are `go test -run` invocations rather than shell
// calling `swiftc` directly, for the same reason the leftover clean and the gap
// scan are: keydriver.swift is compiled in exactly one place (KeyDriver.Build),
// and a second spelling of that compile in shell is the drift this module
// spends its harness code avoiding.
//
// WHAT TRAVELS BETWEEN THEM IS A NAME, NOT A HANDLE. The taking process has
// exited by the time the handback runs, so the token is written to a file in
// the run directory: a bundle identifier where the application has one, its pid
// otherwise, and `none` when nothing was frontmost. A bundle identifier
// survives an application that relaunched between the two moments; a raw pid
// would hand focus to whatever inherited the number.

const (
	// sweepFocusEnv is what bin/realtest.sh sets to select which half of the
	// sweep's focus handling this invocation is.
	sweepFocusEnv = "AGENT_REPL_REALTEST_FOCUS"
	// sweepFocusTake and sweepFocusGiveBack are its two values.
	sweepFocusTake     = "take"
	sweepFocusGiveBack = "give-back"

	// sweepFocusHeldEnv says the SWEEP has taken focus and owns it until its
	// EXIT trap gives it back, so no press may hand it back per keystroke.
	//
	// It is set by bin/realtest.sh only once the take actually ran, so a run
	// that could not take focus falls back to the per-press restore rather
	// than leaving the owner's desktop parked on Emacs with nobody holding the
	// handback.
	sweepFocusHeldEnv = "AGENT_REPL_REALTEST_FOCUS_HELD"

	// sweepFocusTokenFile is where the take writes the token and the give-back
	// reads it, inside the run directory both invocations share.
	sweepFocusTokenFile = "sweep-focus.txt"

	// sweepFocusTookPrefix and sweepFocusGaveBackPrefix are what the helper
	// prints for each half.
	sweepFocusTookPrefix     = "keydriver-took:"
	sweepFocusGaveBackPrefix = "keydriver-gave-back:"

	// sweepFocusNoPrevious is the token for "nothing was frontmost".
	sweepFocusNoPrevious = "none"
)

// sweepHoldsFocus says the sweep took focus at its start and will give it back
// at its end, so a press must not restore focus itself.
func sweepHoldsFocus() bool {
	return os.Getenv(sweepFocusHeldEnv) == "1"
}

// SweepFocusTaken is what the take reported.
//
// Activated is the helper's own word — `yes`, `declined`, or `no` with a reason
// on the raw line — and it is carried rather than collapsed to a bool: a
// declined activation and an editor that was not running are different facts,
// and a sweep reports which it met instead of only that Emacs did not come
// forward.
type SweepFocusTaken struct {
	// Previous is the token naming where focus was before the sweep took it.
	Previous string
	// Activated is `yes`, `declined` or `no`.
	Activated string
	// Raw is the whole line, which is what a manifest carries.
	Raw string
}

// parseSweepFocusTake reads what `keydriver --take` printed.
//
// Its own function so the spelling is testable without a window server. A line
// that does not carry a previous-focus token is an ERROR rather than a token of
// `none`: `none` means the window server said nothing was frontmost, and a
// helper whose output shape changed must never be read as saying that — the
// handback would then quietly return focus to nobody and the owner would be
// left staring at Emacs.
func parseSweepFocusTake(raw string) (SweepFocusTaken, error) {
	line := sweepFocusLine(raw, sweepFocusTookPrefix)
	if line == "" {
		return SweepFocusTaken{}, fmt.Errorf("the key helper's focus take printed %q, which carries no %q line",
			strings.TrimSpace(raw), sweepFocusTookPrefix)
	}
	previous, ok := sweepFocusField(line, "previous=")
	if !ok {
		return SweepFocusTaken{}, fmt.Errorf("the key helper's focus take printed %q, which names no "+
			"previous=<token>, so there would be nothing to hand focus back to", line)
	}
	activated, ok := sweepFocusField(line, "activated=")
	if !ok {
		return SweepFocusTaken{}, fmt.Errorf("the key helper's focus take printed %q, which does not say "+
			"whether the activation was granted", line)
	}
	return SweepFocusTaken{Previous: previous, Activated: activated, Raw: line}, nil
}

// parseSweepFocusGiveBack reads what `keydriver --give-back` printed.
func parseSweepFocusGiveBack(raw string) (restored bool, line string, err error) {
	line = sweepFocusLine(raw, sweepFocusGaveBackPrefix)
	if line == "" {
		return false, "", fmt.Errorf("the key helper's focus handback printed %q, which carries no %q line",
			strings.TrimSpace(raw), sweepFocusGaveBackPrefix)
	}
	value, ok := sweepFocusField(line, "restored=")
	if !ok {
		return false, line, fmt.Errorf("the key helper's focus handback printed %q, which does not say "+
			"whether focus was restored", line)
	}
	switch value {
	case "yes":
		return true, line, nil
	case "no":
		return false, line, nil
	default:
		return false, line, fmt.Errorf("the key helper's focus handback said restored=%q, which is neither "+
			"yes nor no", value)
	}
}

// sweepFocusLine picks the helper's line out of whatever else was printed.
func sweepFocusLine(raw, prefix string) string {
	for _, line := range strings.Split(raw, "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, prefix) {
			return line
		}
	}
	return ""
}

// sweepFocusField reads one `name=value` out of a helper line.
//
// The value ends at the next space, which is why the helper's reasons are
// written as `reason=...` LAST on their line: a field that could contain a
// space would have to be quoted, and a quoting rule between two of our own
// processes is a parser waiting to disagree with its writer.
func sweepFocusField(line, name string) (string, bool) {
	index := strings.Index(line, name)
	if index < 0 {
		return "", false
	}
	value := line[index+len(name):]
	if space := strings.IndexByte(value, ' '); space >= 0 {
		value = value[:space]
	}
	if value == "" {
		return "", false
	}
	return value, true
}

// TakeSweepFocus brings the editor forward for the whole sweep and answers
// where focus was before.
//
// `pid` of zero means no editor is answering yet — the ordinary case for a
// sweep whose first realtest cold-starts one — and the take then only RECORDS
// where focus started. The first press against the new Emacs re-takes focus and
// says so, which is exactly the path a mid-sweep relaunch already takes.
func (d *KeyDriver) TakeSweepFocus(ctx context.Context, pid int) (SweepFocusTaken, error) {
	if d.helper == "" {
		return SweepFocusTaken{}, fmt.Errorf("the key helper has not been built; call Build first")
	}
	args := []string{"--take"}
	if pid > 0 {
		args = append(args, fmt.Sprint(pid))
	}
	out, err := exec.CommandContext(ctx, d.helper, args...).CombinedOutput()
	if err != nil {
		return SweepFocusTaken{}, fmt.Errorf("ask the key helper to take focus for the sweep: %w; it said: %s",
			err, strings.TrimSpace(string(out)))
	}
	return parseSweepFocusTake(string(out))
}

// GiveBackSweepFocus hands focus to the token the take recorded.
func (d *KeyDriver) GiveBackSweepFocus(ctx context.Context, token string) (bool, string, error) {
	if d.helper == "" {
		return false, "", fmt.Errorf("the key helper has not been built; call Build first")
	}
	out, err := exec.CommandContext(ctx, d.helper, "--give-back", token).CombinedOutput()
	if err != nil {
		return false, "", fmt.Errorf("ask the key helper to hand focus back to %s: %w; it said: %s",
			token, err, strings.TrimSpace(string(out)))
	}
	return parseSweepFocusGiveBack(string(out))
}

// SweepFocusTokenPath is where the token lives inside a run directory.
func SweepFocusTokenPath(runDir string) string {
	return filepath.Join(runDir, sweepFocusTokenFile)
}

// WriteSweepFocusToken records where focus started, for the handback that runs
// in a different process.
func WriteSweepFocusToken(runDir, token string) (string, error) {
	path := SweepFocusTokenPath(runDir)
	if err := os.MkdirAll(runDir, 0o755); err != nil {
		return "", fmt.Errorf("make the run directory for the sweep's focus token: %w", err)
	}
	if err := os.WriteFile(path, []byte(token+"\n"), 0o644); err != nil {
		return "", fmt.Errorf("write the sweep's focus token to %s: %w", path, err)
	}
	return path, nil
}

// ReadSweepFocusToken reads it back.
//
// A MISSING FILE IS AN ERROR, NOT A `none`. The handback runs from an EXIT
// trap, where every failure is easy to lose; a missing token means the take
// never ran or never wrote, and the sweep has to say so rather than quietly
// hand focus to nobody and leave the owner's desktop on Emacs.
func ReadSweepFocusToken(runDir string) (string, error) {
	path := SweepFocusTokenPath(runDir)
	raw, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("read the sweep's focus token from %s: %w", path, err)
	}
	token := strings.TrimSpace(string(raw))
	if token == "" {
		return "", fmt.Errorf("the sweep's focus token at %s is empty, so there is no application named to "+
			"hand focus back to", path)
	}
	return token, nil
}

// sweepFocusTakenNote renders the take in the words the run's output carries.
func sweepFocusTakenNote(taken SweepFocusTaken) string {
	switch taken.Activated {
	case "yes":
		return fmt.Sprintf("the sweep took focus: Emacs is frontmost for the whole run and focus goes back to "+
			"%s when the sweep ends, however it ends. %s", taken.Previous, taken.Raw)
	case "declined":
		return fmt.Sprintf("THE SWEEP ASKED FOR FOCUS AND THE WINDOW SERVER DECLINED: Emacs did not come "+
			"forward. macOS 14 and later make activation cooperative, and a locked screen grants it to "+
			"nobody. The presses re-ask and report what they get; focus still goes back to %s at the end. %s",
			taken.Previous, taken.Raw)
	default:
		return fmt.Sprintf("the sweep recorded where focus started (%s) and brought nothing forward: %s. The "+
			"first press against the editor takes focus and says so, and the sweep's end hands it back",
			taken.Previous, taken.Raw)
	}
}

// sweepFocusGaveBackNote renders the handback.
func sweepFocusGaveBackNote(token string, restored bool, line string) string {
	if restored {
		return fmt.Sprintf("the sweep handed focus back to %s, where it found it: %s", token, line)
	}
	return fmt.Sprintf("THE SWEEP COULD NOT HAND FOCUS BACK to %s, so the owner's desktop is not as this run "+
		"found it: %s", token, line)
}
