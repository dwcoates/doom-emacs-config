package daemonrules

import (
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalColdbacklogDir is the package these rules hold, under the daemon module's root.
const internalColdbacklogDir = "internal/coldbacklog"

func TestAGateIsDatedByGateLapsedAt(t *testing.T) {
	sourcescan.InPackage(t, internalColdbacklogDir)
	// Act
	shared := sourcescan.Count(t, internalColdbacklogDir, "cachelapse.GateLapsedAt(")
	own := sourcescan.Count(t, internalColdbacklogDir, "GetLastRequestAtMs()")

	// Assert
	if shared != 1 || own != 0 {
		t.Fatalf("cachelapse.GateLapsedAt( appears %d times and an own reading of the last request %d times; want the shared reading alone", shared, own)
	}
}
