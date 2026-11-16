package deploy

import (
	"context"
	"errors"
	"strings"
	"testing"
)

func TestRunToolAnswersTheCommandsOutputAndExit(t *testing.T) {
	// Arrange
	runner := &scriptedRunner{answers: map[string]runAnswer{"/bin/tool verb x": {out: "said", code: 3}}}

	// Act
	out, code, err := runTool(context.Background(), runner, "/d", "/bin/tool", "verb", "x")

	// Assert
	if err != nil || out != "said" || code != 3 {
		t.Fatalf("runTool = %q, %d, %v; want said, 3", out, code, err)
	}
	if runner.dirs[0] != "/d" {
		t.Fatalf("ran in %q, want /d", runner.dirs[0])
	}
}

func TestRunToolNamesACommandThatCouldNotRun(t *testing.T) {
	// Arrange
	runner := &scriptedRunner{answers: map[string]runAnswer{"/bin/tool verb": {err: errors.New("exec: not found")}}}

	// Act
	_, _, err := runTool(context.Background(), runner, "/", "/bin/tool", "verb")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "deploy: run /bin/tool verb") || !strings.Contains(err.Error(), "not found") {
		t.Fatalf("runTool = %v, want the command and its cause", err)
	}
}

func TestMustToolFailsANonZeroExitWithTheToolsWords(t *testing.T) {
	tests := []struct {
		name    string
		answer  runAnswer
		wantErr string
	}{
		{name: "a zero exit passes", answer: runAnswer{out: "fine"}},
		{name: "a non-zero exit names the tool by its base name, the verb, the code and the words", answer: runAnswer{out: " refused \n", code: 2}, wantErr: "deploy: tool verb x exited 2: refused"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &scriptedRunner{answers: map[string]runAnswer{"/bin/tool verb x": tc.answer}}

			// Act
			err := mustTool(context.Background(), runner, "/", "/bin/tool", "verb", "x")

			// Assert
			if tc.wantErr == "" {
				if err != nil {
					t.Fatalf("mustTool = %v, want nil", err)
				}
				return
			}
			if err == nil || err.Error() != tc.wantErr {
				t.Fatalf("mustTool = %v, want %q", err, tc.wantErr)
			}
		})
	}
}

// TestEveryServiceManagerReportsAFailedVerbTheSameWay: both managers' failed
// verbs go through toolExited, so a failure reads the same whichever
// platform it came from.
func TestEveryServiceManagerReportsAFailedVerbTheSameWay(t *testing.T) {
	failure := runAnswer{out: "refused", code: 1}
	tests := []struct {
		name string
		call func() error
		want string
	}{
		{
			name: "launchd",
			call: func() error {
				l, _ := newLaunchctl(map[string]runAnswer{"launchctl kickstart -k gui/501/x": failure})
				return l.Restart(context.Background(), "x")
			},
			want: toolExited("launchctl", []string{"kickstart", "-k", "gui/501/x"}, 1, "refused").Error(),
		},
		{
			name: "systemd",
			call: func() error {
				s, _ := newSystemdUser(map[string]runAnswer{"systemctl restart --user x.service": failure})
				return s.Restart(context.Background(), "x")
			},
			want: toolExited("systemctl", []string{"restart", "--user", "x.service"}, 1, "refused").Error(),
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := tc.call()

			// Assert
			if err == nil || err.Error() != tc.want {
				t.Fatalf("%s restart = %v, want %q", tc.name, err, tc.want)
			}
		})
	}
}

func TestScriptDefinerRunsDefineServiceInTheModuleRoot(t *testing.T) {
	const argv = "/m/bin/requirements.sh define-service com.agentrepl.tests"
	cases := []struct {
		name    string
		answer  runAnswer
		wantErr string
	}{
		{name: "a zero exit writes the definition", answer: runAnswer{code: 0}},
		{name: "a non-zero exit is a failure in the script's words", answer: runAnswer{out: "define-service: no service entry", code: 1},
			wantErr: "requirements.sh define-service com.agentrepl.tests exited 1: define-service: no service entry"},
		{name: "a script that could not run is a failure", answer: runAnswer{err: errors.New("exec: not found")},
			wantErr: "exec: not found"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &scriptedRunner{answers: map[string]runAnswer{argv: tc.answer}}

			// Act
			err := ScriptDefiner{ModuleRoot: "/m", Runner: runner}.Define(context.Background(), TestsLabel)

			// Assert
			if tc.wantErr == "" && err != nil || tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("Define = %v, want error containing %q", err, tc.wantErr)
			}
			if len(runner.calls) != 1 || strings.Join(runner.calls[0], " ") != argv || runner.dirs[0] != "/m" {
				t.Fatalf("ran %v in %v, want %q in /m", runner.calls, runner.dirs, argv)
			}
		})
	}
}
