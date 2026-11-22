package locktest

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"testing"
)

const helperEnv = "LOCKTEST_HELPER"

func TestLockHelperProcess(t *testing.T) {
	Run(helperEnv, map[string]Mode{
		"hold": func(string) error { return nil },
		"echo": func(path string) error {
			fmt.Printf("path %s\n", path)
			os.Exit(0)
			return nil
		},
		"fail": func(string) error { return errors.New("the mode failed") },
	})
}

func TestStartRunsTheNamedModeInAnotherProcess(t *testing.T) {
	cases := []struct {
		name     string
		mode     string
		wantLine string
		// wantOther is whether the line names another process's pid.
		wantOther bool
	}{
		{name: "a holding mode prints ready and its pid", mode: "hold", wantOther: true},
		{name: "a mode that prints its own line answers it", mode: "echo", wantLine: "path /p"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			p := Start(t, "TestLockHelperProcess", helperEnv, tc.mode, "/p")

			// Assert
			if tc.wantOther {
				if pid := p.Field(t, "ready"); pid == os.Getpid() || pid != p.Cmd.Process.Pid {
					t.Fatalf("ready %d, want the helper's pid %d", pid, p.Cmd.Process.Pid)
				}
				return
			}
			if p.Line != tc.wantLine {
				t.Fatalf("line = %q, want %q", p.Line, tc.wantLine)
			}
		})
	}
}

func TestAHelperWhoseModeFailsOrIsUnknownExitsWithStatus3(t *testing.T) {
	cases := []struct {
		name string
		mode string
	}{
		{name: "a mode that fails", mode: "fail"},
		{name: "a mode not given to Run", mode: "nonesuch"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd := exec.Command(os.Args[0], "-test.run=^TestLockHelperProcess$")
			cmd.Env = append(os.Environ(), helperEnv+"="+tc.mode, helperEnv+"_PATH=/p")

			// Act
			err := cmd.Run()

			// Assert
			var exit *exec.ExitError
			if !errors.As(err, &exit) || exit.ExitCode() != 3 {
				t.Fatalf("the helper's exit = %v, want status 3", err)
			}
		})
	}
}

func TestFieldAnswersTheIntegerAfterItsWord(t *testing.T) {
	cases := []struct {
		name    string
		line    string
		word    string
		want    int
		wantErr bool
	}{
		{name: "the word's integer", line: "boot 12 serving 34", word: "serving", want: 34},
		{name: "a word not named", line: "ready 12", word: "holder", wantErr: true},
		{name: "a word with no integer after it", line: "ready x", word: "ready", wantErr: true},
		{name: "a word last on the line", line: "ready", word: "ready", wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := field(tc.line, tc.word)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("field(%q, %q) = (%d, %v), want %d, error %v", tc.line, tc.word, got, err, tc.want, tc.wantErr)
			}
		})
	}
}
