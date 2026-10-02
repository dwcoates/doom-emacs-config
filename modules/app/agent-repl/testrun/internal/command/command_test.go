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
