package harness

import (
	"errors"
	"os/exec"
	"testing"
)

// runRecorder runs the fake with args and answers its exit status.
func runRecorder(t *testing.T, r *Recorder, args ...string) int {
	t.Helper()
	err := exec.Command(r.Path, args...).Run()
	var exit *exec.ExitError
	switch {
	case err == nil:
		return 0
	case errors.As(err, &exit):
		return exit.ExitCode()
	default:
		t.Fatalf("run %s: %v", r.Path, err)
		return -1
	}
}

func TestSetExitCodeForScriptsOneVerbAheadOfTheGeneralCode(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want int
	}{
		{name: "the scripted verb fails", args: []string{"kickstart", "-k", "gui/501/x"}, want: 1},
		{name: "another verb takes the general code", args: []string{"print", "gui/501/x"}, want: 0},
		{name: "no arguments take the general code", args: nil, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := NewRecorderExecutable(t, t.TempDir(), "launchctl")
			r.SetExitCodeFor("kickstart", 1)

			// Act
			got := runRecorder(t, r, tt.args...)

			// Assert
			if got != tt.want {
				t.Fatalf("exit = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestSetExitCodeForOutranksSetExitCode(t *testing.T) {
	// Arrange
	r := NewRecorderExecutable(t, t.TempDir(), "launchctl")
	r.SetExitCode(3)
	r.SetExitCodeFor("print", 0)

	// Act
	printed, kicked := runRecorder(t, r, "print", "x"), runRecorder(t, r, "kickstart", "x")

	// Assert
	if printed != 0 || kicked != 3 {
		t.Fatalf("exits = (%d, %d), want the verb's 0 and the general 3", printed, kicked)
	}
}

func TestInvocationsExceptSetsTheNamedVerbsAside(t *testing.T) {
	// Arrange
	r := NewRecorderExecutable(t, t.TempDir(), "launchctl")
	runRecorder(t, r, "print", "gui/501/a")
	runRecorder(t, r, "kickstart", "-k", "gui/501/a")
	runRecorder(t, r, "print", "gui/501/b")

	// Act
	got := r.InvocationsExcept("print")

	// Assert
	if len(got) != 1 || got[0].Argv[0] != "kickstart" {
		t.Fatalf("invocations = %+v, want only the kickstart", got)
	}
}
