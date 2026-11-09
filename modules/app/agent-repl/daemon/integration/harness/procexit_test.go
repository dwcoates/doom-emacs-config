package harness

import (
	"agent-repld/internal/sourcescan"
	"context"
	"errors"
	"os/exec"
	"strings"
	"testing"
)

func TestAwaitProcessEndRefusesAProcessAlreadyReaped(t *testing.T) {
	// Arrange
	cmd := exec.Command("/bin/sh", "-c", "exit 0")
	if err := cmd.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}

	// Act
	_, err := AwaitProcessEnd(context.Background(), cmd.Process.Pid)

	// Assert
	if !errors.Is(err, errEndUnwatchable) {
		t.Fatalf("AwaitProcessEnd of a reaped pid = %v, want errEndUnwatchable", err)
	}
}

// TestHarnessTakesKernelEventsOnlyThroughKernelevent pins that the harness
// holds no kernel-event code of its own: every exit and path wait goes
// through internal/kernelevent, the one implementation production shares.
func TestHarnessTakesKernelEventsOnlyThroughKernelevent(t *testing.T) {
	// Arrange
	forbidden := []string{"unix.Kqueue(", "unix.Kevent(", "unix.PidfdOpen(", "unix.InotifyInit"}

	for _, f := range sourcescan.Production(t) {
		t.Run(f.Name, func(t *testing.T) {
			// Act, Assert
			for _, call := range forbidden {
				if strings.Contains(string(f.Source), call) {
					t.Errorf("%s calls %s; take the event through internal/kernelevent", f.Name, call)
				}
			}
		})
	}
}
