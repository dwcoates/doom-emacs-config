//go:build realtest

package realtest

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// The take's line names where focus started, which is the one fact the handback
// cannot do without.
func TestParseSweepFocusTakeReadsThePreviousApplication(t *testing.T) {
	raw := "keydriver-took: previous=bundle:com.google.Chrome target=421 activated=yes screenLocked=no\n"

	taken, err := parseSweepFocusTake(raw)

	if err != nil {
		t.Fatalf("parseSweepFocusTake(%q) = %v", raw, err)
	}
	if taken.Previous != "bundle:com.google.Chrome" {
		t.Errorf("previous = %q, want the bundle token the helper printed", taken.Previous)
	}
}

// A declined activation is carried as the word the helper used, not collapsed
// into "not taken": the sweep reports which of the two happened.
func TestParseSweepFocusTakeCarriesADeclinedActivation(t *testing.T) {
	raw := "keydriver-took: previous=pid:900 target=421 activated=declined screenLocked=no\n"

	taken, err := parseSweepFocusTake(raw)

	if err != nil {
		t.Fatalf("parseSweepFocusTake(%q) = %v", raw, err)
	}
	if taken.Activated != "declined" {
		t.Errorf("activated = %q, want %q", taken.Activated, "declined")
	}
}

// A line with no previous-focus token is an ERROR, never a token of `none`: the
// handback would otherwise return focus to nobody and leave the owner staring
// at Emacs.
func TestParseSweepFocusTakeRefusesALineWithNoPreviousToken(t *testing.T) {
	raw := "keydriver-took: target=421 activated=yes screenLocked=no\n"

	_, err := parseSweepFocusTake(raw)

	if err == nil {
		t.Fatalf("parseSweepFocusTake(%q) answered no error, so a sweep would hand focus to nobody", raw)
	}
}

// A helper whose output shape changed must be an error rather than a silent
// "nothing was frontmost".
func TestParseSweepFocusTakeRefusesAnUnknownLine(t *testing.T) {
	raw := "keydriver: something else entirely\n"

	_, err := parseSweepFocusTake(raw)

	if err == nil {
		t.Fatalf("parseSweepFocusTake(%q) answered no error", raw)
	}
}

// The handback's own line says whether focus actually landed.
func TestParseSweepFocusGiveBackReadsARestoredFocus(t *testing.T) {
	raw := "keydriver-gave-back: previous=bundle:com.google.Chrome restored=yes screenLocked=no\n"

	restored, _, err := parseSweepFocusGiveBack(raw)

	if err != nil {
		t.Fatalf("parseSweepFocusGiveBack(%q) = %v", raw, err)
	}
	if !restored {
		t.Errorf("restored = false for a line that says restored=yes")
	}
}

// An application that quit during the run is a handback that did not land, and
// the sweep has to be able to say so.
func TestParseSweepFocusGiveBackReadsAFocusThatDidNotLand(t *testing.T) {
	raw := "keydriver-gave-back: previous=bundle:com.google.Chrome restored=no reason=that application is no longer running\n"

	restored, _, err := parseSweepFocusGiveBack(raw)

	if err != nil {
		t.Fatalf("parseSweepFocusGiveBack(%q) = %v", raw, err)
	}
	if restored {
		t.Errorf("restored = true for a line that says restored=no")
	}
}

// A restored= value that is neither word must be an error, never a silent
// false: a handback reported as failed when it succeeded sends the owner
// looking for a desktop that is already back.
func TestParseSweepFocusGiveBackRefusesAnUnknownRestoredValue(t *testing.T) {
	raw := "keydriver-gave-back: previous=pid:900 restored=maybe\n"

	_, _, err := parseSweepFocusGiveBack(raw)

	if err == nil {
		t.Fatalf("parseSweepFocusGiveBack(%q) answered no error", raw)
	}
}

// The token survives one process ending and another starting, which is the
// whole reason it is a file.
func TestSweepFocusTokenRoundTrips(t *testing.T) {
	runDir := t.TempDir()

	if _, err := WriteSweepFocusToken(runDir, "bundle:com.google.Chrome"); err != nil {
		t.Fatalf("WriteSweepFocusToken: %v", err)
	}
	token, err := ReadSweepFocusToken(runDir)

	if err != nil {
		t.Fatalf("ReadSweepFocusToken: %v", err)
	}
	if token != "bundle:com.google.Chrome" {
		t.Errorf("token = %q, want the one that was written", token)
	}
}

// A missing token is an ERROR, not a `none`: it means the take never ran, and
// the trap that reads it has to say so rather than quietly hand focus nowhere.
func TestReadSweepFocusTokenRefusesAMissingFile(t *testing.T) {
	runDir := t.TempDir()

	_, err := ReadSweepFocusToken(runDir)

	if err == nil {
		t.Fatalf("ReadSweepFocusToken answered no error for a run directory with no token in it")
	}
}

// An empty token file is the same failure with a different spelling.
func TestReadSweepFocusTokenRefusesAnEmptyFile(t *testing.T) {
	runDir := t.TempDir()
	if err := os.WriteFile(filepath.Join(runDir, sweepFocusTokenFile), []byte("\n"), 0o644); err != nil {
		t.Fatalf("write the empty token: %v", err)
	}

	_, err := ReadSweepFocusToken(runDir)

	if err == nil {
		t.Fatalf("ReadSweepFocusToken answered no error for an empty token file")
	}
}

// The sweep holds focus only when it said so, so a Go test run outside
// bin/realtest.sh restores focus the old way rather than parking the owner's
// desktop on Emacs.
func TestSweepHoldsFocusOnlyWhenTheSweepSaysSo(t *testing.T) {
	tests := []struct {
		name  string
		value string
		want  bool
	}{
		{name: "the sweep took focus", value: "1", want: true},
		{name: "no sweep is holding it", value: "", want: false},
		{name: "anything else is not a hold", value: "0", want: false},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			t.Setenv(sweepFocusHeldEnv, test.value)

			if got := sweepHoldsFocus(); got != test.want {
				t.Errorf("sweepHoldsFocus() = %v, want %v for %s=%q", got, test.want, sweepFocusHeldEnv, test.value)
			}
		})
	}
}

// The take's note has to say where focus goes back to, because that is what the
// owner reads to know the run will give their desktop back.
func TestSweepFocusTakenNoteNamesWhereFocusGoesBack(t *testing.T) {
	taken := SweepFocusTaken{Previous: "bundle:com.google.Chrome", Activated: "yes", Raw: "keydriver-took: ..."}

	note := sweepFocusTakenNote(taken)

	if !strings.Contains(note, "bundle:com.google.Chrome") {
		t.Errorf("the take note does not name where focus goes back to: %s", note)
	}
}

// A declined activation reads as a declined activation, not as a taken focus.
func TestSweepFocusTakenNoteNamesADeclinedActivation(t *testing.T) {
	taken := SweepFocusTaken{Previous: "pid:900", Activated: "declined", Raw: "keydriver-took: ..."}

	note := sweepFocusTakenNote(taken)

	if !strings.Contains(note, "DECLINED") {
		t.Errorf("a declined activation does not read as one: %s", note)
	}
}

// A handback that did not land must read as a desktop this run changed and
// left, because nothing else in the output would say so.
func TestSweepFocusGaveBackNoteNamesADesktopLeftDisturbed(t *testing.T) {
	note := sweepFocusGaveBackNote("bundle:com.google.Chrome", false, "keydriver-gave-back: ...")

	if !strings.Contains(note, "COULD NOT HAND FOCUS BACK") {
		t.Errorf("a handback that did not land does not read as one: %s", note)
	}
}
