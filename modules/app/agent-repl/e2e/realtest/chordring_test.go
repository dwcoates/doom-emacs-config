//go:build realtest

package realtest

import (
	"strings"
	"testing"
)

// The rings below are Emacs's own, taken from the run directory of the
// 2026-09-13 11:10 sweep (`probe-00.json` and the realtest 5 and 7 notes), so
// what is asserted is what the editor actually said.

func TestReadChordRingNamesTheForeignKeyThatAteTheLeader(t *testing.T) {
	// Arrange: realtest 8's whole ring, up to the chord that failed.
	keys := "<escape> s <escape> ' SPC j m p"

	// Act.
	reading, foreign := readChordRing(keys, "<escape>", "SPC j m p")

	// Assert.
	if reading != wsActRingForeignPreface {
		t.Errorf("the ring ends with the sequence preceded by `'`, not by the run's own escape, so it "+
			"carries input the run did not send; the reading was %v", reading)
	}
	if foreign != "'" {
		t.Errorf("the key that stood where the run's escape should be is `'` (evil-goto-mark, which reads "+
			"the next key as a mark name); the reading named %q", foreign)
	}
}

func TestReadChordRingAcceptsASequencePrecededByTheRunsOwnEscape(t *testing.T) {
	// Arrange.
	keys := "<escape> <escape> SPC <tab> n"

	// Act.
	reading, foreign := readChordRing(keys, "<escape>", "SPC <tab> n")

	// Assert.
	if reading != wsActRingAsPressed {
		t.Errorf("every key arrived in order behind the run's own escape, so the ring accuses nobody; the "+
			"reading was %v (%q)", reading, foreign)
	}
}

func TestReadChordRingReportsASequenceThatIsNotAtTheTail(t *testing.T) {
	// Arrange: the `n` never arrived.
	keys := "<escape> <escape> SPC <tab>"

	// Act.
	reading, _ := readChordRing(keys, "<escape>", "SPC <tab> n")

	// Assert.
	if reading != wsActRingSequenceMissing {
		t.Errorf("a ring that does not end with the sequence is a key that never arrived; the reading was %v",
			reading)
	}
}

func TestReadChordRingReadsAnEmptyRingAsASequenceThatNeverArrived(t *testing.T) {
	// Arrange.
	keys := ""

	// Act.
	reading, _ := readChordRing(keys, "<escape>", "SPC <tab> n")

	// Assert.
	if reading != wsActRingSequenceMissing {
		t.Errorf("an editor whose ring holds nothing has recorded no key of the sequence; the reading was %v",
			reading)
	}
}

func TestReadChordRingDoesNotInventForeignInputWhenTheEscapeAgedOut(t *testing.T) {
	// Arrange: the sequence IS the whole ring, so nothing precedes it to
	// compare — the escape was shifted out rather than replaced.
	keys := "SPC <tab> n"

	// Act.
	reading, _ := readChordRing(keys, "<escape>", "SPC <tab> n")

	// Assert.
	if reading != wsActRingAsPressed {
		t.Errorf("a missing preface is not a foreign one, and accusing the desktop on that reading is the "+
			"same sin as accusing the binding; the reading was %v", reading)
	}
}

func TestReadChordRingDoesNotMatchASequenceSpelledTheTypedWay(t *testing.T) {
	// Arrange: the mismatch the 2026-09-13 sweep actually printed — the ring
	// spells the tab key `<tab>` and the caller asked with "TAB".
	keys := "<escape> <escape> SPC <tab> n"

	// Act.
	reading, _ := readChordRing(keys, "<escape>", "SPC TAB n")

	// Assert.
	if reading != wsActRingSequenceMissing {
		t.Errorf("the ring never spells the physical tab key `TAB`, so a caller that asks in the typed "+
			"spelling must be told the sequence is not there rather than quietly matched; the reading was %v",
			reading)
	}
}

func TestRingNoteForForeignInputBlamesNeitherTheBindingNorTheDriver(t *testing.T) {
	// Arrange / Act
	note := wsActRingNote(wsActRingForeignPreface, "'", "SPC j m p", "<escape>")

	// Assert
	if !strings.Contains(note, "NEITHER THE BINDING NOR THE KEY DRIVER") {
		t.Errorf("a leader eaten by a key the run never sent is not the binding's defect and not the "+
			"driver's; it said %q", note)
	}
}

func TestRingNoteForForeignInputCarriesTheKeyItFound(t *testing.T) {
	// Arrange / Act
	note := wsActRingNote(wsActRingForeignPreface, "'", "SPC j m p", "<escape>")

	// Assert
	if !strings.Contains(note, "`'`") {
		t.Errorf("the finding is only actionable if it names the key that stood in the escape's place; it "+
			"said %q", note)
	}
}

func TestRingNoteForAMissingSequenceIsAHarnessFinding(t *testing.T) {
	// Arrange / Act
	note := wsActRingNote(wsActRingSequenceMissing, "", "SPC <tab> n", "<escape>")

	// Assert
	if !strings.Contains(note, "against this harness") {
		t.Errorf("a key that never arrived is this harness's key driver, not the editor; it said %q", note)
	}
}

func TestRingNoteForACleanRingKeepsTheFindingOnTheBinding(t *testing.T) {
	// Arrange / Act
	note := wsActRingNote(wsActRingAsPressed, "", "SPC <tab> n", "<escape>")

	// Assert
	if !strings.Contains(note, "about the binding") {
		t.Errorf("every key arrived and the command asked nothing, which is the one reading that IS the "+
			"binding's; it said %q", note)
	}
}
