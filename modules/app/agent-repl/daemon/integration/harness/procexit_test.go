package harness

import (
	"context"
	"errors"
	"os/exec"
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
