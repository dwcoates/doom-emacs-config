package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalMergeworkspaceDir is the package these rules hold, under the daemon module's root.
const internalMergeworkspaceDir = "internal/mergeworkspace"

// TestSweepSitesShareSweepFailed fails a sweep step that records its failure
// by hand instead of through sweepFailed, which is what keeps a stand-down
// from reading as a failure.
func TestSweepSitesShareSweepFailed(t *testing.T) {
	sourcescan.InPackage(t, internalMergeworkspaceDir)
	// Arrange
	src, err := os.ReadFile("mergeworkspace.go")
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	text := string(src)
	start := strings.Index(text, "func (s *Service) Sweep(")
	end := strings.Index(text, "func (s *Service) inAnyQueue(")
	if start < 0 || end < start {
		t.Fatal("the sweep's functions were not found")
	}
	sweep := text[start:end]

	// Act
	byHand := strings.Count(sweep, "log.Error(op") + strings.Count(sweep, "log.Warn(op")
	shared := strings.Count(sweep, "sweepFailed(")

	// Assert
	if byHand != 0 || shared == 0 {
		t.Fatalf("the sweep records %d failures by hand and %d through sweepFailed", byHand, shared)
	}
}

// TestTheCompactionIsSpelledOnlyBySessionCommand pins that the merge
// workspace service spells no /compact by hand: sessioncommand.Literal reads
// it off the schema.
func TestTheCompactionIsSpelledOnlyBySessionCommand(t *testing.T) {
	sourcescan.InPackage(t, internalMergeworkspaceDir)
	// Act
	n := sourcescan.Count(t, internalMergeworkspaceDir, `"/compact"`)

	// Assert
	if n != 0 {
		t.Errorf("the service's sources spell /compact by hand %d times", n)
	}
}
