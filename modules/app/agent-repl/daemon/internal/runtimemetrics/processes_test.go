package runtimemetrics

import (
	"os"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/procmetrics"
)

// selfOnly is a kernel holding only the test process, as the daemon.
type selfOnly struct{}

func (selfOnly) PIDs() ([]int, error) { return []int{os.Getpid()}, nil }
func (selfOnly) Identity(pid int) (procmetrics.Identity, error) {
	return procmetrics.Identity{PID: pid, PPID: 1, Name: "agent-repld", Responsible: pid, StartUnixMicros: 1}, nil
}
func (selfOnly) Usage(int) (procmetrics.Usage, error) {
	return procmetrics.Usage{CPUSeconds: 1, ResidentBytes: 2}, nil
}
func (selfOnly) Args(int) ([]string, error) { return nil, procmetrics.ErrGone }
func (selfOnly) Host(string) (procmetrics.Host, error) {
	return procmetrics.Host{MemoryTotalBytes: 8}, nil
}

func onAC() (bool, error) { return false, nil }

func TestRegisterProcessesServesTheDaemonsProcess(t *testing.T) {
	// Arrange
	s, err := Open(dlog.NewTestLogger(), t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)

	// Act
	err = s.RegisterProcesses(selfOnly{}, "/state/sock", t.TempDir(), onAC)

	// Assert
	if err != nil {
		t.Fatalf("RegisterProcesses: %v", err)
	}
	body := scrape(t, s)
	for _, want := range []string{
		`agentrepl_process_count{process="daemon",workspace=""} 1`,
		`agentrepl_host_memory_total_bytes 8`,
		`agentrepl_host_on_battery 0`,
	} {
		if !strings.Contains(body, want) {
			t.Fatalf("scrape lacks %s:\n%s", want, body)
		}
	}
}

func TestRegisterProcessesRefusesAnIncompleteCollector(t *testing.T) {
	// Arrange
	s, err := Open(dlog.NewTestLogger(), t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)

	// Act
	err = s.RegisterProcesses(selfOnly{}, "", t.TempDir(), onAC)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "are required") {
		t.Fatalf("err = %v, want the incomplete collector refused", err)
	}
}

func TestTheOffSurfaceRegistersNoProcesses(t *testing.T) {
	// Act
	err := (*Surface)(nil).RegisterProcesses(selfOnly{}, "", "", nil)

	// Assert
	if err != nil {
		t.Fatalf("the off surface refused: %v", err)
	}
}

func TestLatestAnswersTheRegisteredCollectorsScrape(t *testing.T) {
	// Arrange
	s, err := Open(dlog.NewTestLogger(), t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	if err := s.RegisterProcesses(selfOnly{}, "/state/sock", t.TempDir(), onAC); err != nil {
		t.Fatalf("RegisterProcesses: %v", err)
	}
	scrape(t, s)

	// Act
	snap, sampled := s.Latest()

	// Assert
	if _, daemon := snap.Footprints[procmetrics.Group{Process: procmetrics.ProcessDaemon}]; !sampled || !daemon {
		t.Fatalf("Latest = %+v (sampled %v), want the scrape's daemon group", snap, sampled)
	}
}

func TestLatestIsUnsampledWithNoCollector(t *testing.T) {
	// Arrange
	s, err := Open(dlog.NewTestLogger(), t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)

	// Act
	_, sampled := s.Latest()

	// Assert
	if sampled {
		t.Fatalf("Latest answered a sample with no collector registered")
	}
}

func TestTheOffSurfaceHasNoSample(t *testing.T) {
	// Arrange
	var s *Surface

	// Act
	_, sampled := s.Latest()

	// Assert
	if sampled {
		t.Fatalf("the off surface answered a sample")
	}
}
