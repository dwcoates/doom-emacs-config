package merge

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/scriptrunner"
)

func TestKeptRunFailureNamesHowTheScriptEnded(t *testing.T) {
	script := lifecycleconfig.ScriptHook("bin/x.sh", 7).GetScript()
	tests := []struct {
		name string
		ran  KeptRun
		want string
	}{
		{name: "an exit of zero", ran: KeptRun{Run: scriptrunner.Run{ExitCode: 0}}, want: ""},
		{name: "a script that could not be run", ran: KeptRun{Err: errors.New("no bash")}, want: "no bash"},
		{name: "a script killed at its timeout", ran: KeptRun{Run: scriptrunner.Run{ExitCode: -1, TimedOut: true}}, want: "it ran past its 7s timeout"},
		{name: "a non-zero exit", ran: KeptRun{Run: scriptrunner.Run{ExitCode: 3}}, want: "it exited 3"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.ran.Failure(script)

			// Assert
			if got != tt.want {
				t.Fatalf("failure = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestRunKeptScriptRunsTheScriptFromTheRepositoryBoundedByItsTimeout(t *testing.T) {
	// Arrange
	h := newHarness(t)
	script := lifecycleconfig.ScriptHook("bin/x.sh", 7).GetScript()

	// Act
	RunKeptScript(context.Background(), h.hooks, h.repoRoot, script, []string{"A=b"}, filepath.Join(t.TempDir(), "x.log"))

	// Assert
	runs := h.hooks.ran()
	want := hookRun{dir: h.repoRoot, argv: []string{"bash", filepath.Join(h.repoRoot, "bin/x.sh")}, env: []string{"A=b"}, timeout: 7 * time.Second}
	if len(runs) != 1 || runs[0].dir != want.dir || !slices.Equal(runs[0].argv, want.argv) || !slices.Equal(runs[0].env, want.env) || runs[0].timeout != want.timeout {
		t.Fatalf("runs = %+v, want one %+v", runs, want)
	}
}

func TestRunKeptScriptKeepsTheScriptsWholeOutput(t *testing.T) {
	// Arrange
	h := newHarness(t)
	script := lifecycleconfig.ScriptHook("bin/x.sh", 7).GetScript()
	h.hooks.outputs[filepath.Join(h.repoRoot, "bin/x.sh")] = "one\ntwo\n"
	path := filepath.Join(t.TempDir(), "x.log")

	// Act
	ran := RunKeptScript(context.Background(), h.hooks, h.repoRoot, script, nil, path)

	// Assert
	body, err := os.ReadFile(path)
	if ran.LogErr != nil || err != nil || string(body) != "one\ntwo\n" {
		t.Fatalf("log = %q, %v (log error %v), want the script's whole output", body, err, ran.LogErr)
	}
}

func TestRunKeptScriptReportsALogThatCannotBeWritten(t *testing.T) {
	// Arrange: the log's directory is a file.
	h := newHarness(t)
	script := lifecycleconfig.ScriptHook("bin/x.sh", 7).GetScript()
	blocker := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(blocker, nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	ran := RunKeptScript(context.Background(), h.hooks, h.repoRoot, script, nil, filepath.Join(blocker, "x.log"))

	// Assert
	if ran.LogErr == nil || ran.Err != nil || ran.Failure(script) != "" {
		t.Fatalf("run = %+v, want a script that ran and a log error", ran)
	}
}

func TestEveryScriptOutsideADeployRunRunsThroughRunKeptScript(t *testing.T) {
	// Arrange: a deploy's own hooks run in deploy.go, drawn as they write;
	// every other script run (a release, a drain, a finish hook) is
	// RunKeptScript's.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatal(err)
	}
	calls := map[string]int{}

	// Act
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatal(err)
		}
		if n := strings.Count(string(raw), "RunLinesWith("); n > 0 {
			calls[name] = n
		}
	}

	// Assert
	if len(calls) != 3 || calls["deploy.go"] != 1 || calls["keptscript.go"] != 1 || calls["api.go"] != 1 {
		t.Fatalf("RunLinesWith is called in %v, want once in deploy.go (a deploy's own hooks), once in keptscript.go (RunKeptScript) and its declaration in api.go", calls)
	}
}

func TestLifecycleLogPathIsAQueueLog(t *testing.T) {
	for _, stage := range []lifecycleconfig.Stage{lifecycleconfig.StageCreation, lifecycleconfig.StageFinish} {
		t.Run(stage.Key(), func(t *testing.T) {
			// Act
			path := LifecycleLogPath("/state", stage, time.Unix(0, 42), 3)

			// Assert
			if filepath.Dir(path) != LogsDir("/state") || !IsLog(filepath.Base(path)) || !strings.HasPrefix(filepath.Base(path), stage.Key()+"-") {
				t.Fatalf("LifecycleLogPath = %s, want a %s log among the queue's", path, stage.Key())
			}
		})
	}
}

func TestLifecycleLogPathPanicsForADeployStage(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("a deploy stage's lifecycle log path did not panic")
		}
	}()
	LifecycleLogPath("/state", lifecycleconfig.StagePreDeploy, time.Unix(0, 1), 0)
}
