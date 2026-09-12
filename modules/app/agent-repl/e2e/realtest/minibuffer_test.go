//go:build realtest

package realtest

import (
	"strings"
	"testing"
)

func TestAbortMinibufferFormSchedulesRatherThanThrows(t *testing.T) {
	// Arrange / Act
	form := wsActAbortMinibufferForm()

	// Assert
	if !strings.Contains(form, "run-at-time 0 nil") {
		t.Errorf("the abort form must schedule the abort on a timer so the throw does not unwind the "+
			"emacsclient call that sent it; it is:\n%s", form)
	}
}

func TestAbortMinibufferFormPrefersAbortMinibuffers(t *testing.T) {
	// Arrange / Act
	form := wsActAbortMinibufferForm()

	// Assert
	if !strings.Contains(form, "(fboundp 'abort-minibuffers)") {
		t.Errorf("the abort form must prefer `abort-minibuffers`, which unwinds every minibuffer level; it is:\n%s", form)
	}
}

func TestAbortMinibufferFormFallsBackToAbortRecursiveEdit(t *testing.T) {
	// Arrange / Act
	form := wsActAbortMinibufferForm()

	// Assert
	if !strings.Contains(form, "(abort-recursive-edit)") {
		t.Errorf("the abort form must still work on an Emacs without `abort-minibuffers`; it is:\n%s", form)
	}
}

func TestDismissNoteSaysNothingWasStanding(t *testing.T) {
	// Arrange / Act
	note := wsActDismissNote(wsActDismissNothingStanding, "", 0)

	// Assert
	if !strings.Contains(note, "no minibuffer was standing") {
		t.Errorf("a run with no prompt up must say so plainly; it said %q", note)
	}
}

func TestDismissNoteNamesTheChordWhenTheChordWorked(t *testing.T) {
	// Arrange / Act
	note := wsActDismissNote(wsActDismissByChord, "Open workspace: ", 0)

	// Assert
	if !strings.Contains(note, "real `C-g`") || !strings.Contains(note, "Open workspace: ") {
		t.Errorf("a chord dismissal must name the chord and the prompt it closed; it said %q", note)
	}
}

func TestDismissNoteCallsTheEvalPathADeviation(t *testing.T) {
	// Arrange / Act
	note := wsActDismissNote(wsActDismissByEval, "Add project directory: ", 2)

	// Assert
	if !strings.Contains(note, "DEVIATION") {
		t.Errorf("the eval channel must be reported as a deviation rather than as a quiet success; it said %q", note)
	}
}

func TestDismissNoteCountsTheEvalAttempts(t *testing.T) {
	// Arrange / Act
	note := wsActDismissNote(wsActDismissByEval, "Add project directory: ", 2)

	// Assert
	if !strings.Contains(note, "2 attempt(s)") {
		t.Errorf("the eval channel must report how many aborts it took; it said %q", note)
	}
}

func TestDismissNoteSaysTheRunStopsWhenBothChannelsFailed(t *testing.T) {
	// Arrange / Act
	note := wsActDismissNote(wsActDismissFailed, "Repository: ", 3)

	// Assert
	if !strings.Contains(note, "the run stops here") {
		t.Errorf("a minibuffer neither channel could dismiss must say the run stops rather than continue; "+
			"it said %q", note)
	}
}

func TestDismissNoteNamesAnUnknownOutcome(t *testing.T) {
	// Arrange / Act
	note := wsActDismissNote(wsActDismissStage(99), "Repository: ", 0)

	// Assert
	if !strings.Contains(note, "unknown minibuffer dismissal outcome") {
		t.Errorf("an outcome nobody added a sentence for must be reported, not rendered as an empty note; "+
			"it said %q", note)
	}
}

func TestChordDismissCeilingIsFarBelowTheChordCeiling(t *testing.T) {
	// Arrange / Act / Assert
	//
	// The whole finding was a 30s wait per failed dismissal. The dismissal
	// bound must be a small fraction of the ceiling that produced it, or the
	// fix is only cosmetic.
	if wsActChordDismissCeiling*5 > wsActChordCeiling {
		t.Errorf("the dismissal bound %s is not meaningfully tighter than the chord ceiling %s that produced "+
			"the 30s stalls", wsActChordDismissCeiling, wsActChordCeiling)
	}
}
