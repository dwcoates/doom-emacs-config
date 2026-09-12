//go:build realtest

package realtest

import (
	"strings"
	"testing"
)

func TestPendingInputFormReadsEvilState(t *testing.T) {
	// Arrange / Act
	form := wsActPendingInputForm()

	// Assert
	if !strings.Contains(form, "(bound-and-true-p evil-state)") {
		t.Errorf("the pending-input probe must read evil's state, which is where a bare operator shows; it is:\n%s", form)
	}
}

func TestPendingInputFormReadsThePrefixArgument(t *testing.T) {
	// Arrange / Act
	form := wsActPendingInputForm()

	// Assert
	if !strings.Contains(form, "prefix-arg") {
		t.Errorf("a standing prefix argument eats the next key just as an operator does, so the probe must "+
			"read it; it is:\n%s", form)
	}
}

func TestPendingInputFormJoinsFieldsWithTheUnitSeparator(t *testing.T) {
	// Arrange / Act
	form := wsActPendingInputForm()

	// Assert
	if !strings.Contains(form, stateFieldSep) {
		t.Errorf("the probe must join its fields with the same unit separator every other snapshot read uses, "+
			"so a value containing a pipe cannot split into the wrong number of fields; it is:\n%s", form)
	}
}

func TestParseInputStateReadsBothFields(t *testing.T) {
	// Arrange / Act
	state, err := parseWsActInputState("operator" + stateFieldSep + "nil")

	// Assert
	if err != nil {
		t.Fatalf("a well-formed answer must parse: %v", err)
	}
	if state.Evil != "operator" || state.Prefix != "nil" {
		t.Errorf("the parsed state must carry both fields verbatim; it is %+v", state)
	}
}

func TestParseInputStateRejectsAMalformedAnswer(t *testing.T) {
	// Arrange / Act
	_, err := parseWsActInputState("operator")

	// Assert
	if err == nil {
		t.Error("a probe answer with the wrong field count has said nothing trustworthy and must be an error, " +
			"never a partial read")
	}
}

func TestOperatorStateIsPending(t *testing.T) {
	// Arrange / Act
	state := wsActInputState{Evil: "operator", Prefix: "nil"}

	// Assert
	if !state.Pending() {
		t.Error("evil `operator` state reads the next key as the operator's motion, so it is pending input")
	}
}

func TestNormalStateIsNotPending(t *testing.T) {
	// Arrange / Act
	state := wsActInputState{Evil: "normal", Prefix: "nil"}

	// Assert
	if state.Pending() {
		t.Error("normal state reads the next key as itself, so nothing is pending")
	}
}

func TestInsertStateIsNotPending(t *testing.T) {
	// Arrange / Act
	state := wsActInputState{Evil: "insert", Prefix: "nil"}

	// Assert
	if state.Pending() {
		t.Error("insert state self-inserts the next key rather than consuming it as part of a sequence")
	}
}

func TestAStandingPrefixArgumentIsPending(t *testing.T) {
	// Arrange / Act
	state := wsActInputState{Evil: "normal", Prefix: "4"}

	// Assert
	if !state.Pending() {
		t.Error("a standing prefix argument is consumed by the next key, so it is pending input")
	}
}

func TestAnEditorWithoutEvilIsNotPending(t *testing.T) {
	// Arrange / Act
	state := wsActInputState{Evil: "none", Prefix: "nil"}

	// Assert
	if state.Pending() {
		t.Error("an editor with no evil loaded has no operator to be standing in")
	}
}

func TestInputStateRendersBothFields(t *testing.T) {
	// Arrange / Act
	rendered := wsActInputState{Evil: "operator", Prefix: "nil"}.String()

	// Assert
	if !strings.Contains(rendered, "evil-state=operator") || !strings.Contains(rendered, "prefix-arg=nil") {
		t.Errorf("the manifest reader needs both fields named; it said %q", rendered)
	}
}

func TestAlreadyCleanNoteSaysNothingWasStanding(t *testing.T) {
	// Arrange / Act
	note := wsActInputAlreadyCleanNote("pressing s-}", wsActInputState{Evil: "normal", Prefix: "nil"})

	// Assert
	if !strings.Contains(note, "no pending input") {
		t.Errorf("the ordinary case must say plainly that nothing was cleared; it said %q", note)
	}
}

func TestInheritedNoteIsMarkedAsAFinding(t *testing.T) {
	// Arrange / Act
	note := wsActInputInheritedNote("pressing s-}",
		wsActInputState{Evil: "operator", Prefix: "nil"},
		wsActInputState{Evil: "normal", Prefix: "nil"})

	// Assert
	if !strings.Contains(note, "INHERITED PENDING INPUT") {
		t.Errorf("an inherited operator is a finding, never quiet housekeeping; it said %q", note)
	}
}

func TestInheritedNoteNamesTheStateItCleared(t *testing.T) {
	// Arrange / Act
	note := wsActInputInheritedNote("pressing s-}",
		wsActInputState{Evil: "operator", Prefix: "nil"},
		wsActInputState{Evil: "normal", Prefix: "nil"})

	// Assert
	if !strings.Contains(note, "evil-state=operator") {
		t.Errorf("the state before the escape is the finding and must appear verbatim; it said %q", note)
	}
}

func TestNotClearedNoteSaysWhatFollowsWouldBeMeaningless(t *testing.T) {
	// Arrange / Act
	note := wsActInputNotClearedNote("pressing s-}",
		wsActInputState{Evil: "operator", Prefix: "nil"},
		wsActInputState{Evil: "operator", Prefix: "nil"})

	// Assert
	if !strings.Contains(note, "PENDING INPUT COULD NOT BE CLEARED") {
		t.Errorf("an escape that cannot clear an operator means real keys are not reaching the keymap, and "+
			"the note must say so; it said %q", note)
	}
}

func TestNotClearedNoteNamesTheCeilingItWaited(t *testing.T) {
	// Arrange / Act
	note := wsActInputNotClearedNote("pressing s-}",
		wsActInputState{Evil: "operator", Prefix: "nil"},
		wsActInputState{Evil: "operator", Prefix: "nil"})

	// Assert
	if !strings.Contains(note, wsActInputClearCeiling.String()) {
		t.Errorf("a reader has to know how long the escape was given; it said %q", note)
	}
}
