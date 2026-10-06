//go:build darwin

package vendortraffic

import (
	"os"
	"syscall"
	"testing"
	"time"
)

// TestKernelProcessesListsThisProcessInItsGroup reads the real process table
// (no network is touched): this test process is a member of its own group,
// with its parent, its command name and a start time in the past.
func TestKernelProcessesListsThisProcessInItsGroup(t *testing.T) {
	// Arrange.
	pgid, err := syscall.Getpgid(os.Getpid())
	if err != nil {
		t.Fatalf("Getpgid: %v", err)
	}

	// Act.
	members, err := KernelProcesses{}.Group(pgid)

	// Assert.
	if err != nil {
		t.Fatalf("Group: %v", err)
	}
	for _, p := range members {
		if p.PID != os.Getpid() {
			continue
		}
		if p.PPID != os.Getppid() || p.Name == "" || !p.Started.Before(time.Now()) {
			t.Fatalf("this process listed as %+v, want ppid %d, a name and a past start", p, os.Getppid())
		}
		return
	}
	t.Fatalf("group %d listed %+v without this process (%d)", pgid, members, os.Getpid())
}
