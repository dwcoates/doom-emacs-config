package command

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// The commands are this test binary re-executed in a helper mode, so no
// external program runs.
const helperEnv = "TESTRUN_COMMAND_HELPER"

func TestMain(m *testing.M) {
	switch os.Getenv(helperEnv) {
	case "":
		os.Exit(m.Run())
	case "pass":
		os.Stdout.WriteString("listed\n")
		os.Exit(0)
	case "fail":
		os.Stdout.WriteString("partial\n")
		os.Stderr.WriteString("the reason it failed\n")
		os.Exit(4)
	}
	os.Exit(2)
}

func helper(mode string) *exec.Cmd {
	cmd := exec.Command(os.Args[0])
	cmd.Env = append(os.Environ(), helperEnv+"="+mode)
	cmd.Dir = os.TempDir()
	return cmd
}

func TestOutput(t *testing.T) {
	tests := []struct {
		name    string
		cmd     func(t *testing.T) *exec.Cmd
		want    string
		wantErr []string
	}{
		{name: "stdout of a passing command", cmd: func(*testing.T) *exec.Cmd { return helper("pass") }, want: "listed\n"},
		{
			name:    "a failing command names itself, its directory, its status and its stderr",
			cmd:     func(*testing.T) *exec.Cmd { return helper("fail") },
			wantErr: []string{os.Args[0], "(in " + os.TempDir() + ")", "exit status 4", "the reason it failed"},
		},
		{
			name:    "a missing program is an error naming it",
			cmd:     func(t *testing.T) *exec.Cmd { return exec.Command(filepath.Join(t.TempDir(), "absent")) },
			wantErr: []string{"absent"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			out, err := Output(tt.cmd(t))

			// Assert
			if tt.wantErr != nil {
				if err == nil || out != nil {
					t.Fatalf("Output = %q, %v; want an error and no output", out, err)
				}
				for _, w := range tt.wantErr {
					if !strings.Contains(err.Error(), w) {
						t.Errorf("err = %v, want it to mention %q", err, w)
					}
				}
				return
			}
			if err != nil || string(out) != tt.want {
				t.Fatalf("Output = %q, %v; want %q", out, err, tt.want)
			}
		})
	}
}

func TestOutputRefusesACommandWhoseStderrIsTaken(t *testing.T) {
	// Arrange
	cmd := helper("pass")
	cmd.Stderr = os.Stderr

	// Act / Assert
	defer func() {
		if recover() == nil {
			t.Fatal("Output ran a command whose stderr it could not capture")
		}
	}()
	Output(cmd)
}

func TestVerdict(t *testing.T) {
	tests := []struct {
		name           string
		cmd            func(t *testing.T) *exec.Cmd
		wantStdout     string
		wantStderr     string
		wantExitedZero bool
		wantErr        string
	}{
		{name: "a passing command exited zero", cmd: func(*testing.T) *exec.Cmd { return helper("pass") }, wantStdout: "listed\n", wantExitedZero: true},
		{name: "a failing command is an answer, with both outputs", cmd: func(*testing.T) *exec.Cmd { return helper("fail") }, wantStdout: "partial\n", wantStderr: "the reason it failed\n"},
		{name: "a missing program is an error naming it", cmd: func(t *testing.T) *exec.Cmd { return exec.Command(filepath.Join(t.TempDir(), "absent")) }, wantErr: "absent"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			stdout, stderr, exitedZero, err := Verdict(tt.cmd(t))

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) || stdout != nil || stderr != nil {
					t.Fatalf("Verdict = %q, %q, %v; want an error naming %q", stdout, stderr, err, tt.wantErr)
				}
				return
			}
			if err != nil || string(stdout) != tt.wantStdout || string(stderr) != tt.wantStderr || exitedZero != tt.wantExitedZero {
				t.Fatalf("Verdict = %q, %q, %v, %v; want %q, %q, %v", stdout, stderr, exitedZero, err, tt.wantStdout, tt.wantStderr, tt.wantExitedZero)
			}
		})
	}
}

func TestVerdictRefusesACommandWhoseOutputIsTaken(t *testing.T) {
	// Arrange
	cmd := helper("pass")
	cmd.Stdout = os.Stdout

	// Act / Assert
	defer func() {
		if recover() == nil {
			t.Fatal("Verdict ran a command whose stdout it could not capture")
		}
	}()
	Verdict(cmd)
}
