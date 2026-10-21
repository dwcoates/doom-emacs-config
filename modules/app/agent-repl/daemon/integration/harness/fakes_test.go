package harness

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
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

func TestRecorderRecordsEveryArgumentVerbatim(t *testing.T) {
	cases := []struct {
		name string
		arg  string
	}{
		{"a double quote", `say "hi"`},
		{"a backslash", `C:\path\to`},
		{"a backslash before a quote", `end\"`},
		{"spaces", "print gui/501/com.example"},
		{"an empty argument", ""},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			// Arrange
			r := NewRecorderExecutable(t, t.TempDir(), "launchctl")

			// Act
			runRecorder(t, r, "print", c.arg)

			// Assert
			got := r.Invocations()
			if len(got) != 1 || len(got[0].Argv) != 2 || got[0].Argv[1] != c.arg {
				t.Fatalf("recorded %+v, want one invocation whose second argument is %q", got, c.arg)
			}
		})
	}
}

func TestTestAllScriptEnvNamesTheVariableTheMergeOrchestratorReads(t *testing.T) {
	// Act
	got := TestAllScriptEnv("/repo/bin/test-all.sh")

	// Assert
	if got != "AGENT_REPL_TEST_ALL_SCRIPT=/repo/bin/test-all.sh" {
		t.Fatalf("TestAllScriptEnv = %q, want the daemon's AGENT_REPL_TEST_ALL_SCRIPT entry", got)
	}
}

func TestNoIntegrationTestSpellsTheTestAllVariableByHand(t *testing.T) {
	// Arrange
	files, err := filepath.Glob(filepath.Join("..", "*.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("list the integration suite's files: %v (%d files)", err, len(files))
	}

	// Act
	var offenders []string
	for _, file := range files {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if strings.Contains(string(body), "AGENT_REPL_TEST_ALL_SCRIPT=") {
			offenders = append(offenders, file)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v spell the test-all variable by hand; use TestAllScriptEnv", offenders)
	}
}

func TestWriteExecutableWritesAnExecutable(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "bin", "hook.sh")

	// Act
	writeExecutable(t, path, "#!/bin/sh\n")

	// Assert
	info, err := os.Stat(path)
	if err != nil || info.Mode()&0o111 == 0 {
		t.Fatalf("stat = %v, %v; want an executable", info, err)
	}
}

// TestEveryHarnessExecutableIsWrittenThroughWriteExecutable pins that no
// harness source writes an executable by hand.
func TestEveryHarnessExecutableIsWrittenThroughWriteExecutable(t *testing.T) {
	// Arrange
	paths, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}
	var byHand []string

	// Act
	for _, path := range paths {
		if strings.HasSuffix(path, "_test.go") {
			continue
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}
		for i, line := range strings.Split(string(raw), "\n") {
			if strings.Contains(line, "os.WriteFile(") && strings.Contains(line, "0o755") {
				byHand = append(byHand, fmt.Sprintf("%s:%d", path, i+1))
			}
		}
	}

	// Assert
	if len(byHand) != 0 {
		t.Fatalf("executables written by hand at %v, want writeExecutable", byHand)
	}
}
