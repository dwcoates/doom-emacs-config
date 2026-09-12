//go:build realtest

package realtest

import (
	"errors"
	"strings"
	"testing"
)

func TestFocusReadingZeroValueIsUnread(t *testing.T) {
	// Arrange / Act
	var reading FocusReading

	// Assert
	if reading.State != FocusUnread {
		t.Errorf("a reading nobody took must be unread, so a press that never asked is not recorded as one "+
			"that was told Emacs is unfocused; it was %v", reading.State)
	}
}

func TestEmacsFocusFormScansEveryFrame(t *testing.T) {
	// Arrange / Act
	form := emacsFocusForm()

	// Assert
	if !strings.Contains(form, "(frame-list)") {
		t.Errorf("the probe must scan every frame, because that is what `agent-repl--emacs-focused-p` does "+
			"and therefore what the pre-creation hold is built on; the form was %q", form)
	}
}

func TestEmacsFocusFormReadsTheProductPredicatesAttribute(t *testing.T) {
	// Arrange / Act
	form := emacsFocusForm()

	// Assert
	if !strings.Contains(form, "frame-focus-state") {
		t.Errorf("the probe must read the same attribute the product's hold reads; the form was %q", form)
	}
}

func TestParseFocusReadingReadsFocused(t *testing.T) {
	// Arrange / Act
	reading := parseFocusReading("focused")

	// Assert
	if reading.State != FocusFocused {
		t.Errorf("`focused` is the editor saying a real focus edge happened; it read %v", reading.State)
	}
}

func TestParseFocusReadingReadsUnfocused(t *testing.T) {
	// Arrange / Act
	reading := parseFocusReading("unfocused")

	// Assert
	if reading.State != FocusUnfocused {
		t.Errorf("`unfocused` is the editor saying the activation was declined; it read %v", reading.State)
	}
}

func TestParseFocusReadingTreatsAnUnknownAnswerAsUnanswered(t *testing.T) {
	// Arrange / Act
	reading := parseFocusReading("nil")

	// Assert
	if reading.State != FocusUnanswered {
		t.Errorf("an answer that is neither word must never condemn a show phase as unfocused; it read %v",
			reading.State)
	}
}

func TestParseScreenLockReadsLocked(t *testing.T) {
	// Arrange / Act
	lock, err := parseScreenLock("screenLocked=yes\n")

	// Assert
	if err != nil || lock != ScreenLockLocked {
		t.Errorf("a locked session must be read as locked; it read %v with error %v", lock, err)
	}
}

func TestParseScreenLockReadsUnlocked(t *testing.T) {
	// Arrange / Act
	lock, err := parseScreenLock("screenLocked=no\n")

	// Assert
	if err != nil || lock != ScreenLockUnlocked {
		t.Errorf("an unlocked session must be read as unlocked; it read %v with error %v", lock, err)
	}
}

func TestParseScreenLockReadsUnknown(t *testing.T) {
	// Arrange / Act
	lock, err := parseScreenLock("screenLocked=unknown\n")

	// Assert
	if err != nil || lock != ScreenLockUnknown {
		t.Errorf("a session that would not answer must be unknown, never unlocked; it read %v with error %v",
			lock, err)
	}
}

func TestParseScreenLockRefusesAnUnrecognisedLine(t *testing.T) {
	// Arrange / Act
	_, err := parseScreenLock("trusted\n")

	// Assert
	if err == nil {
		t.Error("a helper that answered something else must be an error, never a silent unlocked reading")
	}
}

func TestParseScreenLockRefusesAnUnknownWord(t *testing.T) {
	// Arrange / Act
	_, err := parseScreenLock("screenLocked=maybe\n")

	// Assert
	if err == nil {
		t.Error("a lock word this harness does not know must be an error rather than a guess")
	}
}

func TestNoFocusEdgeNoteClearsTheProductWhenNoEdgeWasReal(t *testing.T) {
	// Arrange / Act
	note := noFocusEdgeNote(3, ScreenLockLocked, nil, FocusReading{State: FocusUnfocused})

	// Assert
	if !strings.Contains(note, "NOT a product finding") {
		t.Errorf("an unpainted panel behind a focus edge that never happened is the desktop's, not the "+
			"product's, and the note must say so; it said %q", note)
	}
}

func TestNoFocusEdgeNoteNamesTheLockedScreen(t *testing.T) {
	// Arrange / Act
	note := noFocusEdgeNote(3, ScreenLockLocked, nil, FocusReading{State: FocusUnfocused})

	// Assert
	if !strings.Contains(note, "THE SCREEN WAS LOCKED") {
		t.Errorf("the one state in which no activation can succeed must be named outright; it said %q", note)
	}
}

func TestNoFocusEdgeNoteBlamesTheDriverOnAnUnlockedSession(t *testing.T) {
	// Arrange / Act
	note := noFocusEdgeNote(3, ScreenLockUnlocked, nil, FocusReading{State: FocusUnfocused})

	// Assert
	if !strings.Contains(note, "harness defect") {
		t.Errorf("an activation declined on an unlocked session is the key driver's defect and the note must "+
			"say so; it said %q", note)
	}
}

func TestNoFocusEdgeNoteWithholdsBlameWhenTheLockIsUnknown(t *testing.T) {
	// Arrange / Act
	note := noFocusEdgeNote(3, ScreenLockUnknown, nil, FocusReading{State: FocusUnfocused})

	// Assert
	if !strings.Contains(note, "UNKNOWN") {
		t.Errorf("a lock state nobody read must not be attributed to either cause; it said %q", note)
	}
}

func TestNoFocusEdgeNoteCarriesTheLockReadingFailure(t *testing.T) {
	// Arrange / Act
	note := noFocusEdgeNote(1, ScreenLockUnknown, errors.New("the helper would not run"),
		FocusReading{State: FocusUnfocused})

	// Assert
	if !strings.Contains(note, "the helper would not run") {
		t.Errorf("a lock reading that failed must travel with the note rather than being swallowed; it said %q",
			note)
	}
}

func TestNoFocusEdgeNoteCarriesTheEditorsOwnReading(t *testing.T) {
	// Arrange / Act
	note := noFocusEdgeNote(2, ScreenLockLocked, nil, FocusReading{State: FocusUnanswered, ProbeFailure: "no channel"})

	// Assert
	if !strings.Contains(note, "no channel") {
		t.Errorf("the note is made of the editor's own answer and must carry why it did not give one; it said %q",
			note)
	}
}

func TestNoFocusEdgeNoteCountsThePresses(t *testing.T) {
	// Arrange / Act
	note := noFocusEdgeNote(3, ScreenLockLocked, nil, FocusReading{State: FocusUnfocused})

	// Assert
	if !strings.Contains(note, "3 keypress(es)") {
		t.Errorf("the note must say how many activations were requested; it said %q", note)
	}
}
