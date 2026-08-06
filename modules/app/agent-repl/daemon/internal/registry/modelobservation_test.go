package registry

import (
	"strings"
	"testing"
)

// ModelObservation is the ordering the two writers of rec.Model lacked. These
// cover the admission rule itself; the write it gates is covered where the
// gate lives (server.RegistryRegistrar).

func TestModelObservationWithoutAGenerationOrdinalIsInvalid(t *testing.T) {
	// Arrange / Act / Assert — an observation that orders against nothing is
	// refused rather than given the benefit of the doubt.
	if (ModelObservation{Generation: "cg_a", StreamSeq: 9}).Valid() {
		t.Fatal("an observation with no generation ordinal must not be valid")
	}
}

func TestModelObservationWithAGenerationOrdinalIsValid(t *testing.T) {
	// Arrange / Act / Assert.
	if !(ModelObservation{Generation: "cg_a", GenOrdinal: 1}).Valid() {
		t.Fatal("an observation carrying a generation ordinal must be valid")
	}
}

func TestModelObservationSupersedesOnAHigherSeqWithinAGeneration(t *testing.T) {
	// Arrange.
	prev := ModelObservation{Generation: "cg_a", GenOrdinal: 1, StreamSeq: 9}
	next := ModelObservation{Generation: "cg_a", GenOrdinal: 1, StreamSeq: 10}

	// Act / Assert.
	if !next.Supersedes(prev) {
		t.Fatal("a later seq in the same generation must supersede")
	}
}

func TestModelObservationDoesNotSupersedeOnALowerSeq(t *testing.T) {
	// Arrange — F2's stale SystemInit: it rode an event the controller had
	// already consumed when the shim confirmed the newer selection.
	prev := ModelObservation{Generation: "cg_a", GenOrdinal: 1, StreamSeq: 12}
	stale := ModelObservation{Generation: "cg_a", GenOrdinal: 1, StreamSeq: 9}

	// Act / Assert.
	if stale.Supersedes(prev) {
		t.Fatal("an earlier seq must not supersede a newer confirmation")
	}
}

func TestModelObservationDoesNotSupersedeAnEqualToken(t *testing.T) {
	// Arrange — a re-delivered event carries the seq it originally carried, so
	// equal is a replay rather than a fresh report.
	same := ModelObservation{Generation: "cg_a", GenOrdinal: 1, StreamSeq: 12}

	// Act / Assert.
	if same.Supersedes(same) {
		t.Fatal("an equal token must not supersede; equal is a replay")
	}
}

func TestModelObservationFromAHigherGenerationSupersedesALowerSeq(t *testing.T) {
	// Arrange — a respawn restarts the file plane's seq space, so the new
	// controller's first init carries a low seq and must still win.
	prev := ModelObservation{Generation: "cg_a", GenOrdinal: 1, StreamSeq: 900}
	next := ModelObservation{Generation: "cg_b", GenOrdinal: 2, StreamSeq: 1}

	// Act / Assert.
	if !next.Supersedes(prev) {
		t.Fatal("a newer controller generation must supersede regardless of seq")
	}
}

func TestModelObservationFromARetiredGenerationDoesNotSupersede(t *testing.T) {
	// Arrange — a report left in flight by the controller this one replaced.
	prev := ModelObservation{Generation: "cg_b", GenOrdinal: 2, StreamSeq: 1}
	retired := ModelObservation{Generation: "cg_a", GenOrdinal: 1, StreamSeq: 900}

	// Act / Assert.
	if retired.Supersedes(prev) {
		t.Fatal("a retired generation's report must not supersede its replacement's")
	}
}

func TestModelObservationStringNamesBothOrderingComponents(t *testing.T) {
	// Arrange — the token is written into the daemon log on every refusal, so
	// an operator must be able to read WHICH controller and WHICH seq lost.
	o := ModelObservation{Generation: "cg_a", GenOrdinal: 3, StreamSeq: 12}

	// Act.
	got := o.String()

	// Assert.
	for _, want := range []string{"cg_a", "3", "12"} {
		if !strings.Contains(got, want) {
			t.Fatalf("String() = %q, want it to name %q", got, want)
		}
	}
}
