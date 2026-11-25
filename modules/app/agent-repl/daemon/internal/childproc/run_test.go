package childproc

import (
	"path/filepath"
	"testing"
)

func TestRun(t *testing.T) {
	tests := []struct {
		name        string
		script      string
		wantStdout  string
		wantStderr  string
		wantRunErr  bool
		wantStarted int
	}{
		{name: "a child that succeeds answers both streams", script: `printf out; printf err >&2`, wantStdout: "out", wantStderr: "err", wantStarted: 1},
		{name: "a child that fails answers both streams and its exit", script: `printf out; printf boom >&2; exit 3`, wantStdout: "out", wantStderr: "boom", wantRunErr: true, wantStarted: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cmd := Command(Stable, "/bin/sh", "-c", tt.script)
			started := 0

			// Act
			stdout, stderr, runErr, err := Run(cmd, func() { started++ })

			// Assert
			if err != nil || stdout != tt.wantStdout || stderr != tt.wantStderr || (runErr != nil) != tt.wantRunErr || started != tt.wantStarted {
				t.Fatalf("Run = %q, %q, %v, %v (started %d); want %q, %q, run error %v, started %d",
					stdout, stderr, runErr, err, started, tt.wantStdout, tt.wantStderr, tt.wantRunErr, tt.wantStarted)
			}
		})
	}
}

func TestRunOfAChildThatNeverStartsAnswersItsStartError(t *testing.T) {
	// Arrange
	cmd := Command(Stable, filepath.Join(t.TempDir(), "absent"))
	started := 0

	// Act
	stdout, stderr, runErr, err := Run(cmd, func() { started++ })

	// Assert
	if err != nil || runErr == nil || cmd.Process != nil || stdout != "" || stderr != "" || started != 0 {
		t.Fatalf("Run = %q, %q, %v, %v (process %v, started %d); want the start error alone and started never called",
			stdout, stderr, runErr, err, cmd.Process, started)
	}
}

func TestRunAcceptsNoStartedCallback(t *testing.T) {
	// Arrange
	cmd := Command(Stable, "/bin/sh", "-c", "printf ok")

	// Act
	stdout, _, runErr, err := Run(cmd, nil)

	// Assert
	if err != nil || runErr != nil || stdout != "ok" {
		t.Fatalf("Run = %q, %v, %v; want %q", stdout, runErr, err, "ok")
	}
}

func TestRunRefusesAnOutputThatCannotBeCaptured(t *testing.T) {
	// Arrange: no temporary directory exists to hold the output files.
	t.Setenv("TMPDIR", filepath.Join(t.TempDir(), "absent"))
	cmd := Command(Stable, "/bin/sh", "-c", "printf ok")

	// Act
	_, _, runErr, err := Run(cmd, nil)

	// Assert
	if err == nil || runErr != nil || cmd.Process != nil {
		t.Fatalf("Run = %v, %v (process %v); want the capture refused and nothing started", runErr, err, cmd.Process)
	}
}
