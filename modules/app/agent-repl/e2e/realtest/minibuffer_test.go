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

// ---- The C-g press is judged by the marks Emacs is obliged to leave --------

func TestQuitEvidenceCountsAGrownRecentKeysAsArrival(t *testing.T) {
	// Arrange
	evidence := wsActQuitEvidence{KeysBefore: "SPC j m p", KeysAfter: "SPC j m p C-g"}

	// Act / Assert
	if !evidence.Arrived() {
		t.Errorf("a (recent-keys) that grew is Emacs saying it read the quit character, so the press arrived; "+
			"the evidence %+v was read as not having arrived", evidence)
	}
}

func TestQuitEvidenceCountsAnArmedQuitFlagAsArrival(t *testing.T) {
	// Arrange
	evidence := wsActQuitEvidence{KeysBefore: "SPC j m p", KeysAfter: "SPC j m p", QuitFlagArmed: true}

	// Act / Assert
	if !evidence.Arrived() {
		t.Errorf("an armed quit-flag is a quit Emacs took in and has not yet honoured, so the press arrived; "+
			"the evidence %+v was read as not having arrived", evidence)
	}
}

func TestQuitEvidenceCallsNeitherMarkANonArrival(t *testing.T) {
	// Arrange
	evidence := wsActQuitEvidence{KeysBefore: "SPC j m p", KeysAfter: "SPC j m p"}

	// Act / Assert
	if evidence.Arrived() {
		t.Errorf("an arriving quit character must leave (recent-keys) grown or quit-flag armed, so neither "+
			"mark is the key never entering Emacs's input; the evidence %+v was read as having arrived", evidence)
	}
}

func TestQuitEvidenceTreatsAFailedProbeAsArrival(t *testing.T) {
	// Arrange
	//
	// The conservative direction: an editor that would not answer has said
	// nothing, and a reading nobody got must never let a real product defect
	// be filed against the harness.
	evidence := wsActQuitEvidence{ProbeFailure: "quit-flag after the press: connection refused"}

	// Act / Assert
	if !evidence.Arrived() {
		t.Errorf("a probe that would not answer is not an absent mark; the evidence %+v was read as not "+
			"having arrived", evidence)
	}
}

func TestQuitFlagFormReadsTheFlagAsAWord(t *testing.T) {
	// Arrange / Act
	form := wsActQuitFlagForm()

	// Assert
	if !strings.Contains(form, "quit-flag") || !strings.Contains(form, `"armed"`) {
		t.Errorf("the quit-flag probe must read `quit-flag` and answer in a word an empty reply cannot pass "+
			"for; it is:\n%s", form)
	}
}

func TestDismissNoteBlamesTheHarnessWhenTheChordNeverArrived(t *testing.T) {
	// Arrange / Act
	note := wsActDismissNote(wsActDismissChordNeverArrived, "Repository: ", 1)

	// Assert
	if !strings.Contains(note, "NOT A PRODUCT FINDING") || !strings.Contains(note, "key driver") {
		t.Errorf("a `C-g` Emacs never saw is a defect in this harness's key driver and must say so rather "+
			"than accuse the editor; it said %q", note)
	}
}

func TestDismissNoteKeepsTheProductFindingWhenTheChordArrived(t *testing.T) {
	// Arrange / Act
	note := wsActDismissNote(wsActDismissByEval, "Repository: ", 1)

	// Assert
	if !strings.Contains(note, "PRODUCT FINDING") || !strings.Contains(note, "REACHED Emacs") {
		t.Errorf("a `C-g` that reached Emacs and left the prompt up is a product defect and must still be "+
			"reported as one; it said %q", note)
	}
}

func TestQuitEvidenceNoteRendersBothReadings(t *testing.T) {
	// Arrange
	evidence := wsActQuitEvidence{KeysBefore: "SPC j m p", KeysAfter: "SPC j m p", QuitFlagArmed: true}

	// Act
	note := wsActQuitEvidenceNote(evidence)

	// Assert
	if !strings.Contains(note, "quit-flag was armed") {
		t.Errorf("the evidence sentence must carry what the editor said about the flag so the verdict can be "+
			"checked rather than taken; it said %q", note)
	}
}

func TestQuitEvidenceNoteNamesAProbeFailure(t *testing.T) {
	// Arrange
	evidence := wsActQuitEvidence{ProbeFailure: "quit-flag after the press: connection refused"}

	// Act
	note := wsActQuitEvidenceNote(evidence)

	// Assert
	if !strings.Contains(note, "connection refused") {
		t.Errorf("a probe that would not answer must appear in the finding, not be smoothed over; it said %q", note)
	}
}
