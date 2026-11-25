package daemonrules

import (
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalProcmetricsDir is the package these rules hold, under the daemon module's root.
const internalProcmetricsDir = "internal/procmetrics"

// The host's CPU times are read by agentrepl/logging/hostcpu alone, the one
// reader testrun's CPU governor shares, so the two can never disagree on what
// the host's load is.
func TestTheHostsCPUTimesAreReadThroughHostcpu(t *testing.T) {
	sourcescan.InPackage(t, internalProcmetricsDir)
	// Act
	direct := sourcescan.Count(t, internalProcmetricsDir, "HOST_CPU_LOAD_INFO")
	shared := sourcescan.Count(t, internalProcmetricsDir, "hostcpu.Read()")

	// Assert
	if direct != 0 || shared != 1 {
		t.Fatalf("procmetrics reads HOST_CPU_LOAD_INFO %d times and hostcpu.Read() %d; want 0 and 1", direct, shared)
	}
}
