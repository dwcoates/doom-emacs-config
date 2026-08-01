package reload

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"
)

// fakeScript writes an inert stand-in for bin/deploy-all.sh. NO test in this
// package ever runs the real one: it rebuilds and bounces the live stack.
func fakeScript(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "deploy-all.sh")
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o755); err != nil {
		t.Fatalf("write fake script: %v", err)
	}
	return path
}

// recordingLogf collects log records for assertion.
type recordingLogf struct {
	mu    sync.Mutex
	lines []string
}

func (r *recordingLogf) logf(format string, args ...any) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.lines = append(r.lines, format)
	_ = args
}

func (r *recordingLogf) contains(substr string) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return slices.ContainsFunc(r.lines, func(l string) bool { return strings.Contains(l, substr) })
}

func newTestScript(t *testing.T, script string) (*DetachedScript, string) {
	t.Helper()
	logDir := filepath.Join(t.TempDir(), "reload")
	s, err := NewDetachedScript(ScriptConfig{
		Script: script,
		Dir:    t.TempDir(),
		LogDir: logDir,
		Logf:   t.Logf,
		Now:    func() time.Time { return time.Unix(0, 0).UTC() },
	})
	if err != nil {
		t.Fatalf("NewDetachedScript: %v", err)
	}
	return s, logDir
}

// The deploy KILLS the daemon that spawned it, so the child must be in its own
// process group: a signal delivered to the daemon's group must not reach it.
// Asserted on the built command rather than by signalling anything.
func TestCommandPutsTheDeployInItsOwnProcessGroup(t *testing.T) {
	// Arrange.
	s, _ := newTestScript(t, fakeScript(t, "exit 0\n"))

	// Act.
	cmd := s.command(Plan{Components: []Component{ComponentDaemon}}, nil)

	// Assert.
	if cmd.SysProcAttr == nil || !cmd.SysProcAttr.Setpgid {
		t.Fatalf("command SysProcAttr = %+v, want Setpgid so the deploy survives the daemon it kills", cmd.SysProcAttr)
	}
}

// exec.CommandContext would bind the child to the merge coordinator's context,
// which is cancelled by the very shutdown the deploy causes.
func TestCommandIsNotBoundToAnyContext(t *testing.T) {
	// Arrange.
	s, _ := newTestScript(t, fakeScript(t, "exit 0\n"))

	// Act.
	cmd := s.command(Plan{Components: []Component{ComponentDaemon}}, nil)

	// Assert.
	if cmd.Cancel != nil {
		t.Fatalf("command carries a Cancel func, so the deploy would be killed by the shutdown it causes")
	}
}

func TestCommandPassesTheElispRangeWhenElispChanged(t *testing.T) {
	// Arrange.
	script := fakeScript(t, "exit 0\n")
	s, _ := newTestScript(t, script)
	plan := Plan{Components: []Component{ComponentDaemon, ComponentElisp}, Range: "aaa..bbb"}

	// Act.
	cmd := s.command(plan, nil)

	// Assert.
	if !slices.Equal(cmd.Args, []string{script, "--elisp", "aaa..bbb"}) {
		t.Fatalf("command Args = %v, want the elisp range passed through", cmd.Args)
	}
}

func TestCommandOmitsTheElispFlagWhenNoElispChanged(t *testing.T) {
	// Arrange.
	script := fakeScript(t, "exit 0\n")
	s, _ := newTestScript(t, script)
	plan := Plan{Components: []Component{ComponentDaemon}, Range: "aaa..bbb"}

	// Act.
	cmd := s.command(plan, nil)

	// Assert.
	if !slices.Equal(cmd.Args, []string{script}) {
		t.Fatalf("command Args = %v, want no elisp flag", cmd.Args)
	}
}

// An inherited GIT_DIR would retarget the deploy's own git work at whichever
// repository exported it.
func TestCommandStripsInheritedRepositoryBindings(t *testing.T) {
	// Arrange.
	t.Setenv("GIT_DIR", "/somewhere/.git")
	s, _ := newTestScript(t, fakeScript(t, "exit 0\n"))

	// Act.
	cmd := s.command(Plan{Components: []Component{ComponentDaemon}}, nil)

	// Assert.
	if slices.ContainsFunc(cmd.Env, func(e string) bool { return strings.HasPrefix(e, "GIT_DIR=") }) {
		t.Fatalf("command env kept GIT_DIR; the deploy would build against the leaking repository")
	}
}

func TestLaunchRecordsTheDeployOutputUnderTheStateRoot(t *testing.T) {
	// Arrange — an inert script that writes a recognizable marker and exits.
	s, logDir := newTestScript(t, fakeScript(t, "echo deploy-marker\n"))
	plan := Plan{Workspace: "/ws", Components: []Component{ComponentDaemon}, Range: "aaa..bbb"}

	// Act.
	if err := s.Launch(context.Background(), plan); err != nil {
		t.Fatalf("Launch: %v", err)
	}

	// Assert — the log file exists at the deterministic path the injected clock
	// produced; its content is written by the detached child, so only the
	// file's existence is asserted here.
	want := filepath.Join(logDir, "deploy-19700101T000000.000Z.log")
	if _, err := os.Stat(want); err != nil {
		t.Fatalf("stat redeploy log %s: %v", want, err)
	}
}

func TestLaunchSurfacesAStartFailure(t *testing.T) {
	// Arrange — a script path that does not exist.
	rec := &recordingLogf{}
	logDir := filepath.Join(t.TempDir(), "reload")
	s, err := NewDetachedScript(ScriptConfig{
		Script: filepath.Join(t.TempDir(), "absent-deploy-all.sh"),
		Dir:    t.TempDir(),
		LogDir: logDir,
		Logf:   rec.logf,
	})
	if err != nil {
		t.Fatalf("NewDetachedScript: %v", err)
	}

	// Act.
	err = s.Launch(context.Background(), Plan{Workspace: "/ws", Components: []Component{ComponentDaemon}})

	// Assert — surfaced to the caller AND in the canonical log.
	if err == nil {
		t.Fatalf("Launch() error = nil, want the spawn failure surfaced")
	}
	if !rec.contains("LAUNCH FAILED") {
		t.Fatalf("Launch left no canonical failure record; got %v", rec.lines)
	}
}

func TestLaunchSurfacesAnUnusableLogDirectory(t *testing.T) {
	// Arrange — a log dir path whose parent is a FILE, so MkdirAll must fail.
	blocker := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(blocker, []byte("x"), 0o644); err != nil {
		t.Fatalf("write blocker: %v", err)
	}
	rec := &recordingLogf{}
	s, err := NewDetachedScript(ScriptConfig{
		Script: fakeScript(t, "exit 0\n"),
		Dir:    t.TempDir(),
		LogDir: filepath.Join(blocker, "reload"),
		Logf:   rec.logf,
	})
	if err != nil {
		t.Fatalf("NewDetachedScript: %v", err)
	}

	// Act.
	err = s.Launch(context.Background(), Plan{Workspace: "/ws", Components: []Component{ComponentDaemon}})

	// Assert.
	if err == nil {
		t.Fatalf("Launch() error = nil, want the log-dir failure surfaced")
	}
	if !rec.contains("redeploy log dir FAILED") {
		t.Fatalf("Launch left no canonical failure record; got %v", rec.lines)
	}
}

func TestNewDetachedScriptRefusesAMissingDependency(t *testing.T) {
	tests := []struct {
		name string
		cfg  ScriptConfig
	}{
		{"no script", ScriptConfig{Dir: "/d", LogDir: "/l", Logf: t.Logf}},
		{"no dir", ScriptConfig{Script: "/s", LogDir: "/l", Logf: t.Logf}},
		{"no log dir", ScriptConfig{Script: "/s", Dir: "/d", Logf: t.Logf}},
		{"no logf", ScriptConfig{Script: "/s", Dir: "/d", LogDir: "/l"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			s, err := NewDetachedScript(tc.cfg)

			// Assert.
			if err == nil {
				t.Fatalf("NewDetachedScript(%+v) error = nil, want a refusal", tc.cfg)
			}
			if s != nil {
				t.Fatalf("NewDetachedScript returned %v alongside its refusal", s)
			}
		})
	}
}
