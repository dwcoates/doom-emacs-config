//go:build darwin

package procmetrics

import (
	"errors"
	"os"
	"slices"
	"testing"

	"agent-repld/internal/sourcescan"
)

// The kernel reader is proven against this test process itself: the kernel
// is the one dependency it has, and reading our own record needs no fixture.

func TestKernelReadsThisProcessIdentity(t *testing.T) {
	// Act
	id, err := Kernel{}.Identity(os.Getpid())

	// Assert
	if err != nil {
		t.Fatalf("Identity(self): %v", err)
	}
	if id.PID != os.Getpid() || id.PPID != os.Getppid() || id.Name == "" || id.StartUnixMicros == 0 || id.Responsible == 0 {
		t.Fatalf("Identity(self) = %+v, want our pid, parent, a name, a start and a responsible process", id)
	}
}

func TestKernelReadsThisProcessUsage(t *testing.T) {
	// Act
	u, err := Kernel{}.Usage(os.Getpid())

	// Assert
	if err != nil {
		t.Fatalf("Usage(self): %v", err)
	}
	if u.CPUSeconds <= 0 || u.CPUSeconds > 3600 || u.ResidentBytes == 0 || u.FootprintBytes == 0 || u.Threads == 0 || u.OpenFDs == 0 {
		t.Fatalf("Usage(self) = %+v, want plausible CPU, memory, threads and descriptors", u)
	}
}

func TestKernelListsThisProcess(t *testing.T) {
	// Act
	pids, err := Kernel{}.PIDs()

	// Assert
	if err != nil || !slices.Contains(pids, os.Getpid()) {
		t.Fatalf("PIDs = (%d pids, %v), want this process among them", len(pids), err)
	}
}

func TestKernelReadsThisProcessArgv(t *testing.T) {
	// Act
	args, err := Kernel{}.Args(os.Getpid())

	// Assert
	if err != nil || !slices.Equal(args, os.Args) {
		t.Fatalf("Args(self) = (%q, %v), want %q", args, err, os.Args)
	}
}

func TestKernelAnswersGoneForAProcessThatDoesNotExist(t *testing.T) {
	// Arrange: pids are below 100000 on macOS (kern.maxproc bounds them).
	const absent = 99999

	// Act
	_, identityErr := Kernel{}.Identity(absent)
	_, usageErr := Kernel{}.Usage(absent)
	_, argsErr := Kernel{}.Args(absent)

	// Assert
	if !errors.Is(identityErr, ErrGone) || !errors.Is(usageErr, ErrGone) || !errors.Is(argsErr, ErrGone) {
		t.Fatalf("Identity = %v, Usage = %v, Args = %v, want ErrGone from all three", identityErr, usageErr, argsErr)
	}
}

func TestKernelReadsTheHost(t *testing.T) {
	// Act
	h, err := Kernel{}.Host(t.TempDir())

	// Assert
	if err != nil {
		t.Fatalf("Host: %v", err)
	}
	if h.CPUSeconds["idle"] <= 0 || h.MemoryTotalBytes == 0 || h.MemoryBytes["wired"] == 0 {
		t.Fatalf("Host = %+v, want idle CPU, total and wired memory", h)
	}
	if h.DiskTotalBytes == 0 || h.DiskFreeBytes > h.DiskTotalBytes {
		t.Fatalf("Host disk = free %d of %d", h.DiskFreeBytes, h.DiskTotalBytes)
	}
	if h.MemoryPressure != 1 && h.MemoryPressure != 2 && h.MemoryPressure != 4 {
		t.Fatalf("pressure = %d, want 1, 2 or 4", h.MemoryPressure)
	}
}

// The host's CPU times are read by agentrepl/logging/hostcpu alone, the one
// reader testrun's CPU governor shares, so the two can never disagree on what
// the host's load is.
func TestTheHostsCPUTimesAreReadThroughHostcpu(t *testing.T) {
	// Act
	direct := sourcescan.Count(t, "HOST_CPU_LOAD_INFO")
	shared := sourcescan.Count(t, "hostcpu.Read()")

	// Assert
	if direct != 0 || shared != 1 {
		t.Fatalf("procmetrics reads HOST_CPU_LOAD_INFO %d times and hostcpu.Read() %d; want 0 and 1", direct, shared)
	}
}

func TestKernelHostAnswersEveryCPUMode(t *testing.T) {
	// Act
	host, err := Kernel{}.Host(t.TempDir())

	// Assert
	if err != nil {
		t.Fatalf("Host: %v", err)
	}
	for _, mode := range []string{"user", "system", "idle", "nice"} {
		if _, ok := host.CPUSeconds[mode]; !ok {
			t.Fatalf("CPUSeconds = %v; want the %s mode", host.CPUSeconds, mode)
		}
	}
}
