//go:build realtest

package realtest

import (
	"context"
	"os"
	"path/filepath"
	"strings"
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

// A PROBE THAT NEVER WROTE MUST NOT READ AS ONE THAT DID. The ring reuses a
// slot every `probeRingSize` probes, so the file a probe is about to read
// already holds an earlier probe's answer; the stamp is the only thing that
// tells them apart.
func TestCheckProbeAnswerRejectsTheAnswerOfAnEarlierProbe(t *testing.T) {
	// Arrange
	stale := []byte(`{"ok":true,"seq":9,"value":"<escape> SPC j m p"}`)

	// Act
	_, err := checkProbeAnswer(17, "(recent-keys)", stale)

	// Assert
	if err == nil {
		t.Fatal("a probe read the answer of the probe that used the slot before it and called it its own")
	}
}

// The ordinary case: the answer this probe wrote is the answer it reads.
func TestCheckProbeAnswerTakesItsOwnAnswer(t *testing.T) {
	// Arrange
	fresh := []byte(`{"ok":true,"seq":17,"value":"<escape> SPC j m p C-g"}`)

	// Act
	value, err := checkProbeAnswer(17, "(recent-keys)", fresh)

	// Assert
	if err != nil || string(value) != `"<escape> SPC j m p C-g"` {
		t.Errorf("value = %s err = %v, want the probe's own answer", value, err)
	}
}

// A form that signalled in Emacs is still that probe's answer, and it must
// surface as the signal rather than as a stale-slot complaint.
func TestCheckProbeAnswerSurfacesAnEmacsSignal(t *testing.T) {
	// Arrange
	signalled := []byte(`{"ok":false,"seq":17,"error":"Quit"}`)

	// Act
	_, err := checkProbeAnswer(17, "(recent-keys)", signalled)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "Quit") {
		t.Errorf("err = %v, want the elisp signal carried out to the caller", err)
	}
}

// THE QUIT SIGNAL IS CAUGHT, which `error` alone does not do. A `C-g` taken as
// an interrupt quits whatever lisp is running, and while a press is being
// confirmed that lisp is this probe.
func TestProbeWrapperCatchesAQuitAsWellAsAnError(t *testing.T) {
	// Arrange & Act
	wrapper := probeWrapper("/tmp/answer.json", 3, "(recent-keys)")

	// Assert
	if !strings.Contains(wrapper, "(quit error)") {
		t.Errorf("the probe wrapper handles %q, so a quit character that interrupts a probe would unwind it "+
			"silently and the caller would read the ring slot's previous occupant", wrapper)
	}
}

// The stamp has to be in BOTH arms, or a probe that signalled would read as a
// stale slot rather than as the signal it was.
func TestProbeWrapperStampsBothArmsWithTheProbeSequence(t *testing.T) {
	// Arrange & Act
	wrapper := probeWrapper("/tmp/answer.json", 3, "(recent-keys)")

	// Assert
	if got := strings.Count(wrapper, "(cons 'seq 3)"); got != 2 {
		t.Errorf("the probe wrapper stamps the sequence %d time(s), want both the value arm and the error "+
			"arm: %s", got, wrapper)
	}
}

// THE SLOT IS EMPTIED BEFORE IT IS ASKED FOR, so a probe that dies leaves a
// missing file rather than somebody else's answer.
func TestReadClearsTheAnswerSlotBeforeAskingForIt(t *testing.T) {
	// Arrange
	scratch := t.TempDir()
	client := &Client{Socket: filepath.Join(scratch, "no-such-socket"), Scratch: scratch}
	slot := probeAnswerPath(scratch, 1)
	if err := os.WriteFile(slot, []byte(`{"ok":true,"seq":-1,"value":"stale"}`), 0o600); err != nil {
		t.Fatalf("plant a stale answer: %v", err)
	}

	// Act
	_, err := client.Read(context.Background(), "(recent-keys)")

	// Assert
	if err == nil {
		t.Fatal("a probe against a socket nobody is listening on answered")
	}
	if _, statErr := os.Stat(slot); !os.IsNotExist(statErr) {
		t.Errorf("the stale answer is still at %s after a probe that never wrote one (stat: %v)", slot, statErr)
	}
}
