//go:build realtest

package realtest

import (
	"testing"
)

// The probe transport's unit tests. They assert the answer-file naming rather
// than run emacsclient, because a probe needs a live Emacs and this is the layer
// that must stay hermetic.

func TestProbeAnswerPathCapsTheFilesARunLeavesBehind(t *testing.T) {
	// Arrange: run 3 wrote one probe file per poll, 126 of them, and the run
	// directory could not be read. Act: name the same 126 probes.
	scratch := "/scratch"
	seen := make(map[string]bool)
	for seq := 1; seq <= 126; seq++ {
		seen[probeAnswerPath(scratch, seq)] = true
	}

	// Assert.
	if len(seen) > probeRingSize {
		t.Errorf("126 probes named %d distinct files, want at most the ring size %d", len(seen), probeRingSize)
	}
}

func TestProbeAnswerPathReusesASlotOnlyAfterTheRing(t *testing.T) {
	// Arrange/Act/Assert: consecutive probes must land on different files, or a
	// probe would read back the previous probe's answer. A name may only repeat
	// once the whole ring has been used.
	scratch := "/scratch"
	for seq := 1; seq <= 2*probeRingSize; seq++ {
		if probeAnswerPath(scratch, seq) == probeAnswerPath(scratch, seq+1) {
			t.Errorf("probes %d and %d share the answer file %q", seq, seq+1, probeAnswerPath(scratch, seq))
		}
		if probeAnswerPath(scratch, seq) != probeAnswerPath(scratch, seq+probeRingSize) {
			t.Errorf("probe %d and probe %d do not share a slot; the ring is not %d wide",
				seq, seq+probeRingSize, probeRingSize)
		}
	}
}
