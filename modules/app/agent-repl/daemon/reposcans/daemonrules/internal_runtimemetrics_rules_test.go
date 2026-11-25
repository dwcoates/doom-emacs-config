package daemonrules

import (
	"slices"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalRuntimemetricsDir is the package these rules hold, under the daemon module's root.
const internalRuntimemetricsDir = "internal/runtimemetrics"

func TestEveryReportedDurationIsCheckedByValidSeconds(t *testing.T) {
	sourcescan.InPackage(t, internalRuntimemetricsDir)
	// Act
	sites := sourcescan.Sites(t, internalRuntimemetricsDir, "math.IsNaN(")

	// Assert
	if want := []string{"latency.go"}; !slices.Equal(sites, want) {
		t.Fatalf("math.IsNaN( sites = %v, want %v: check a duration with validSeconds", sites, want)
	}
}
