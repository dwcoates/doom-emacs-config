package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalDlogDir is the package these rules hold, under the daemon module's root.
const internalDlogDir = "internal/dlog"

func TestEveryRunLogRetentionFailureGoesThroughRetentionFailed(t *testing.T) {
	sourcescan.InPackage(t, internalDlogDir)
	// Act.
	viaHelper := sourcescan.Count(t, internalDlogDir, "s.retentionFailed(")
	byHand := sourcescan.Count(t, internalDlogDir, `Global().Error("daemon.dlog.retention_failed"`) +
		sourcescan.Count(t, internalDlogDir, `Global().Log(LevelError, "daemon.dlog.retention_failed"`)

	// Assert: the run log, a directory and the stdio log call the helper, and the only hand-written record is the helper's own.
	if viaHelper != 3 || byHand != 1 {
		t.Fatalf("s.retentionFailed( appears %d times and a hand-rolled record %d times; want 3 and 1 (the helper's own record)", viaHelper, byHand)
	}
}

// TestEveryWorkspaceEntryIsBuiltByTheOneConstructor fails a hand-built entry.
func TestEveryWorkspaceEntryIsBuiltByTheOneConstructor(t *testing.T) {
	sourcescan.InPackage(t, internalDlogDir)
	// Arrange.
	raw, err := os.ReadFile("surfaces.go")
	if err != nil {
		t.Fatalf("read surfaces.go: %v", err)
	}

	// Act.
	built := strings.Count(string(raw), "&workspaceSinks{")

	// Assert.
	if built != 1 {
		t.Fatalf("surfaces.go builds a workspace entry %d times, want once, in newWorkspaceEntryLocked", built)
	}
}

func TestEveryPeriodicSinkPassSnapshotsThroughOpenSinks(t *testing.T) {
	sourcescan.InPackage(t, internalDlogDir)
	// Act: the production call sites of the helper, and of the hand-rolled loop.
	viaHelper := sourcescan.Count(t, internalDlogDir, "range s.openSinks()")
	byHand := sourcescan.Count(t, internalDlogDir, "for name, sk := range ws.sinks {")

	// Assert: the scan and the retention pass, and the helper's own loop alone.
	if viaHelper != 2 || byHand != 1 {
		t.Fatalf("range s.openSinks() appears %d times and a hand-rolled sink loop %d times; want 2 and 1 (the helper's own)", viaHelper, byHand)
	}
}
