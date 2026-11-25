package testenv

import (
	"encoding/json"
	"fmt"

	"agentrepl/testrun/internal/proctable"
)

// PhasesOut names the file a test run gives each Go unit for the CPU its
// harness spent in phases it can tell apart from the tests' own work: each
// daemon's boot until it serves, and the test binary's setup before its
// first test and teardown after its last. The harness appends one line per
// measurement (RecordPhase) and the runner sums them by phase for the unit
// (ReadPhases). Unset, nothing is recorded.
const PhasesOut = "AGENT_REPL_TEST_PHASES_OUT"

// THE PHASE IS A CLOSED VOCABULARY, the label of the unit's phase series.
const (
	// PhaseDaemonBootShared is a suite-owned daemon's CPU (the canonical
	// daemon, a group's) from its start until it serves.
	PhaseDaemonBootShared = "daemon_boot_shared"
	// PhaseDaemonBootFresh is a daemon a test started for itself, from its
	// start until it serves.
	PhaseDaemonBootFresh = "daemon_boot_fresh"
	// PhaseHarnessSetup is the test binary's own CPU before its first test:
	// its TestMain's work up to the tests' run.
	PhaseHarnessSetup = "harness_setup"
	// PhaseHarnessTeardown is the test binary's own CPU after its last test:
	// the suite's cleanups, the shared daemons' stop.
	PhaseHarnessTeardown = "harness_teardown"
)

var phases = map[string]bool{
	PhaseDaemonBootShared: true, PhaseDaemonBootFresh: true,
	PhaseHarnessSetup: true, PhaseHarnessTeardown: true,
}

// phaseLine is one measurement in a PhasesOut file.
type phaseLine struct {
	Phase      string  `json:"phase"`
	CPUSeconds float64 `json:"cpu_seconds"`
}

// RecordPhase appends to the run's PhasesOut file that the unit spent
// CPUSECONDS in PHASE. It does nothing outside a run that asked. A phase
// outside the vocabulary is an error, so the series never grows a label.
func RecordPhase(getenv func(string) string, phase string, cpuSeconds float64) error {
	if !phases[phase] {
		return fmt.Errorf("testenv: %q is no phase of the vocabulary", phase)
	}
	path := getenv(PhasesOut)
	if path == "" {
		return nil
	}
	return appendJSONLine(path, "phase CPU", phaseLine{Phase: phase, CPUSeconds: cpuSeconds})
}

// ReadPhases answers a PhasesOut file's CPU, summed by phase. A file no
// harness wrote answers none; a line that is not a measurement of a known
// phase is an error.
func ReadPhases(path string) (map[string]float64, error) {
	out := map[string]float64{}
	ok, err := readJSONLines(path, "phase CPU", func(n int, line []byte) error {
		var l phaseLine
		if err := json.Unmarshal(line, &l); err != nil || !phases[l.Phase] {
			return fmt.Errorf("testenv: line %d of %s is no phase CPU: %q", n, path, line)
		}
		out[l.Phase] += l.CPUSeconds
		return nil
	})
	if err != nil || !ok {
		return nil, err
	}
	return out, nil
}

// ProcessCPU answers the CPU seconds process PID has used, its own and its
// reaped children's: what a harness reads of a daemon when it serves.
func ProcessCPU(pid int) (float64, error) { return proctable.ProcessCPU(pid) }
