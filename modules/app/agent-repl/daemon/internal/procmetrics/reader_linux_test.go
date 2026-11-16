//go:build linux

package procmetrics

import (
	"os"
	"slices"
	"testing"
)

// The Linux reader is proven against this test process itself, as the
// darwin one is: the kernel is its one dependency.

func TestKernelReadsThisProcessOnLinux(t *testing.T) {
	// Act
	id, idErr := Kernel{}.Identity(os.Getpid())
	u, usageErr := Kernel{}.Usage(os.Getpid())
	args, argsErr := Kernel{}.Args(os.Getpid())
	pids, pidsErr := Kernel{}.PIDs()

	// Assert
	if idErr != nil || id.PPID != os.Getppid() || id.Name == "" || id.StartUnixMicros == 0 {
		t.Fatalf("Identity(self) = (%+v, %v)", id, idErr)
	}
	if usageErr != nil || u.ResidentBytes == 0 || u.FootprintBytes == 0 || u.Threads == 0 || u.OpenFDs == 0 {
		t.Fatalf("Usage(self) = (%+v, %v)", u, usageErr)
	}
	if argsErr != nil || !slices.Equal(args, os.Args) {
		t.Fatalf("Args(self) = (%q, %v), want %q", args, argsErr, os.Args)
	}
	if pidsErr != nil || !slices.Contains(pids, os.Getpid()) {
		t.Fatalf("PIDs = (%d pids, %v), want this process among them", len(pids), pidsErr)
	}
}

func TestKernelReadsTheHostOnLinux(t *testing.T) {
	// Act
	h, err := Kernel{}.Host(t.TempDir())

	// Assert
	if err != nil || h.CPUSeconds["idle"] <= 0 || h.MemoryTotalBytes == 0 || h.MemoryBytes["free"] == 0 || h.DiskTotalBytes == 0 {
		t.Fatalf("Host = (%+v, %v)", h, err)
	}
}
