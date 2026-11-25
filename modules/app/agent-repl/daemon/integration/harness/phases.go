package harness

import (
	"fmt"
	"os"
	"syscall"

	"agentrepl/testrun/testenv"
)

// THE HARNESS TELLS ITS OWN PHASES' CPU APART FROM THE TESTS' (testenv.PhasesOut),
// so a run can say how much of the suite's CPU goes to starting daemons and
// to the test binary's setup rather than to the cases themselves:
//
//   - every daemon StartDaemon brings up, its CPU (its own and its reaped
//     children's) from its start until its serving record, which the
//     harness awaits anyway (awaitServing): a suite-owned daemon's as
//     testenv.PhaseDaemonBootShared, a test's own as
//     testenv.PhaseDaemonBootFresh. What the daemon uses after that is its
//     serving, read as the daemon program's CPU less its boots.
//   - the test binary's own CPU before its first test (testenv.PhaseHarnessSetup)
//     and after its last (testenv.PhaseHarnessTeardown).
//
// It does nothing in a run that did not ask. A measurement that fails fails
// loudly: a daemon's in its test, the binary's in the process's exit code.

// phasesAsked reports whether the run asked for phase CPU.
func phasesAsked() bool { return os.Getenv(testenv.PhasesOut) != "" }

// recordPhase appends one phase measurement to the run's file.
func recordPhase(phase string, cpu float64) error { return testenv.RecordPhase(os.Getenv, phase, cpu) }

// recordDaemonBoot records the CPU daemon PID used until it served, read by
// CPU, under the shared or the fresh boot phase.
func recordDaemonBoot(pid int, shared bool, cpu func(int) (float64, error), record func(string, float64) error) error {
	phase := testenv.PhaseDaemonBootFresh
	if shared {
		phase = testenv.PhaseDaemonBootShared
	}
	used, err := cpu(pid)
	if err != nil {
		return fmt.Errorf("harness: read the CPU daemon %d used until it served, for %s: %w", pid, phase, err)
	}
	if err := record(phase, used); err != nil {
		return fmt.Errorf("harness: record daemon %d's %s of %.3fs: %w", pid, phase, used, err)
	}
	return nil
}

// recordBoot records this daemon's boot CPU, now that it serves, when the run
// asked.
func (d *Daemon) recordBoot() {
	if !phasesAsked() {
		return
	}
	if err := recordDaemonBoot(d.cmd.Process.Pid, d.suiteOwned, testenv.ProcessCPU, recordPhase); err != nil {
		d.t.Fatalf("%v", err)
	}
}

// phaseMeter measures the test binary's own CPU around its tests: start
// before the first, ran after the last, finish once the suite's fixtures
// ended.
type phaseMeter struct {
	self   func() (float64, error)
	record func(string, float64) error
	atRun  float64
	err    error
}

// newPhaseMeter is the meter of this process, or nil when the run did not
// ask.
func newPhaseMeter() *phaseMeter {
	if !phasesAsked() {
		return nil
	}
	return &phaseMeter{self: selfCPU, record: recordPhase}
}

// start records the binary's CPU so far as its setup.
func (p *phaseMeter) start() error {
	if p == nil {
		return nil
	}
	setup, err := p.self()
	if err != nil {
		return fmt.Errorf("harness: read the test binary's CPU before its first test: %w", err)
	}
	if err := p.record(testenv.PhaseHarnessSetup, setup); err != nil {
		return fmt.Errorf("harness: record the test binary's setup of %.3fs: %w", setup, err)
	}
	return nil
}

// ran notes the binary's CPU at the end of its last test; a failed read is
// answered by finish.
func (p *phaseMeter) ran() {
	if p == nil {
		return
	}
	p.atRun, p.err = p.self()
}

// finish records what the binary used since its last test as its teardown.
func (p *phaseMeter) finish() error {
	if p == nil {
		return nil
	}
	if p.err != nil {
		return fmt.Errorf("harness: read the test binary's CPU after its last test: %w", p.err)
	}
	end, err := p.self()
	if err != nil {
		return fmt.Errorf("harness: read the test binary's CPU after the suite's teardown: %w", err)
	}
	teardown := end - p.atRun
	if err := p.record(testenv.PhaseHarnessTeardown, teardown); err != nil {
		return fmt.Errorf("harness: record the test binary's teardown of %.3fs: %w", teardown, err)
	}
	return nil
}

// selfCPU is this process's own CPU seconds, user and system.
func selfCPU() (float64, error) {
	var ru syscall.Rusage
	if err := syscall.Getrusage(syscall.RUSAGE_SELF, &ru); err != nil {
		return 0, fmt.Errorf("getrusage: %w", err)
	}
	seconds := func(tv syscall.Timeval) float64 { return float64(tv.Sec) + float64(tv.Usec)/1e6 }
	return seconds(ru.Utime) + seconds(ru.Stime), nil
}
