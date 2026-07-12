package remediation

import (
	"fmt"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"
)

// recorder is a StartFunc that records every launch instead of execing.
type recorder struct {
	mu    sync.Mutex
	argvs [][]string
	dirs  []string
	err   error
}

func (r *recorder) start(argv []string, dir string) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.err != nil {
		return r.err
	}
	r.argvs = append(r.argvs, argv)
	r.dirs = append(r.dirs, dir)
	return nil
}

func (r *recorder) count() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.argvs)
}

func newRunner(t *testing.T, rec *recorder, cfg Config) *Runner {
	t.Helper()
	if cfg.Dir == "" {
		cfg.Dir = "/repo"
	}
	cfg.Start = rec.start
	cfg.Logf = func(string, ...any) {}
	r, err := New(cfg, time.Now().Add(-90*time.Second))
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return r
}

func TestNewRejectsMissingConfig(t *testing.T) {
	tests := []struct {
		name string
		cfg  Config
	}{
		{"no dir", Config{Start: func([]string, string) error { return nil }, Logf: func(string, ...any) {}}},
		{"no start", Config{Dir: "/repo", Logf: func(string, ...any) {}}},
		{"no logf", Config{Dir: "/repo", Start: func([]string, string) error { return nil }}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			_, err := New(tc.cfg, time.Now())
			// Assert
			if err == nil {
				t.Fatalf("New(%+v) = nil error, want error", tc.cfg)
			}
		})
	}
}

func TestStartLaunchesAnalystInTheConfiguredDir(t *testing.T) {
	// Arrange
	rec := &recorder{}
	r := newRunner(t, rec, Config{Dir: "/checkout"})
	// Act
	started, err := r.Start("s_abc")
	// Assert
	if err != nil || !started {
		t.Fatalf("Start = (%v, %v), want (true, nil)", started, err)
	}
	if got := rec.dirs[0]; got != "/checkout" {
		t.Fatalf("analyst dir = %q, want /checkout", got)
	}
}

func TestStartUsesTheHeadlessOpusArgv(t *testing.T) {
	// Arrange
	rec := &recorder{}
	r := newRunner(t, rec, Config{Bin: "/usr/bin/claude"})
	// Act
	if _, err := r.Start("s_abc"); err != nil {
		t.Fatalf("Start: %v", err)
	}
	// Assert
	argv := rec.argvs[0]
	if argv[0] != "/usr/bin/claude" || argv[1] != "-p" {
		t.Fatalf("argv head = %v, want /usr/bin/claude -p <prompt>", argv[:2])
	}
	if !slices.Contains(argv, "--model") || !slices.Contains(argv, Model) {
		t.Fatalf("argv = %v, want --model %s", argv, Model)
	}
}

func TestStartCarriesTheConfiguredPermissionMode(t *testing.T) {
	// Arrange — the operator opted the headless analyst out of gating.
	rec := &recorder{}
	r := newRunner(t, rec, Config{PermissionMode: "bypassPermissions"})
	// Act
	if _, err := r.Start("s_abc"); err != nil {
		t.Fatalf("Start: %v", err)
	}
	// Assert
	argv := rec.argvs[0]
	mode := argv[slices.Index(argv, "--permission-mode")+1]
	if mode != "bypassPermissions" {
		t.Fatalf("permission mode = %q, want bypassPermissions", mode)
	}
}

func TestStartOmitsThePermissionFlagWhenUnconfigured(t *testing.T) {
	// Arrange — no mode configured means no loosening is assumed.
	rec := &recorder{}
	r := newRunner(t, rec, Config{})
	// Act
	if _, err := r.Start("s_abc"); err != nil {
		t.Fatalf("Start: %v", err)
	}
	// Assert
	if slices.Contains(rec.argvs[0], "--permission-mode") {
		t.Fatalf("argv = %v, want no --permission-mode", rec.argvs[0])
	}
}

func TestStartFallsBackToTheClaudeOnPath(t *testing.T) {
	// Arrange — no Bin configured.
	rec := &recorder{}
	r := newRunner(t, rec, Config{})
	// Act
	if _, err := r.Start("s_abc"); err != nil {
		t.Fatalf("Start: %v", err)
	}
	// Assert
	if got := rec.argvs[0][0]; got != DefaultBin {
		t.Fatalf("analyst bin = %q, want %q", got, DefaultBin)
	}
}

func TestStartCarriesTheSessionIdInThePrompt(t *testing.T) {
	// Arrange
	rec := &recorder{}
	r := newRunner(t, rec, Config{})
	// Act
	if _, err := r.Start("s_deadbeef"); err != nil {
		t.Fatalf("Start: %v", err)
	}
	// Assert
	if prompt := rec.argvs[0][2]; !strings.Contains(prompt, "s_deadbeef") {
		t.Fatalf("prompt does not name the lost session: %q", prompt)
	}
}

func TestStartDispatchesOnlyOneAnalystPerSession(t *testing.T) {
	// Arrange — every reconnecting tab funnels into the same id.
	rec := &recorder{}
	r := newRunner(t, rec, Config{})
	if _, err := r.Start("s_abc"); err != nil {
		t.Fatalf("first Start: %v", err)
	}
	// Act
	started, err := r.Start("s_abc")
	// Assert
	if err != nil {
		t.Fatalf("second Start: %v", err)
	}
	if started {
		t.Fatal("second Start reported a launch, want the dedupe no-op")
	}
	if rec.count() != 1 {
		t.Fatalf("launched %d analysts, want 1", rec.count())
	}
}

func TestStartDispatchesPerDistinctSession(t *testing.T) {
	// Arrange
	rec := &recorder{}
	r := newRunner(t, rec, Config{})
	// Act
	for _, id := range []string{"s_a", "s_b"} {
		if _, err := r.Start(id); err != nil {
			t.Fatalf("Start(%s): %v", id, err)
		}
	}
	// Assert
	if rec.count() != 2 {
		t.Fatalf("launched %d analysts, want 2", rec.count())
	}
}

func TestStartRejectsAnEmptySessionId(t *testing.T) {
	// Arrange
	rec := &recorder{}
	r := newRunner(t, rec, Config{})
	// Act
	_, err := r.Start("")
	// Assert
	if err == nil {
		t.Fatal("Start(\"\") = nil error, want error")
	}
}

func TestStartSurfacesASpawnFailure(t *testing.T) {
	// Arrange
	rec := &recorder{err: fmt.Errorf("exec: claude not found")}
	r := newRunner(t, rec, Config{})
	// Act
	started, err := r.Start("s_abc")
	// Assert
	if err == nil {
		t.Fatal("Start = nil error, want the spawn failure surfaced")
	}
	if started {
		t.Fatal("Start reported a launch despite the spawn failure")
	}
}

func TestPromptBriefsTheAnalystOnTheDaemonUptime(t *testing.T) {
	// Arrange / Act — a daemon that just booted is the restart signal.
	prompt := Prompt("s_abc", 3*time.Second)
	// Assert
	if !strings.Contains(prompt, "up 3s") {
		t.Fatalf("prompt omits the daemon uptime: %q", prompt)
	}
}

func TestPromptDemandsAResilienceWorkspace(t *testing.T) {
	// Arrange / Act
	prompt := Prompt("s_abc", time.Minute)
	// Assert — the workspace is the deliverable, not the diagnosis.
	if !strings.Contains(prompt, "create-or-update-workspace") {
		t.Fatalf("prompt does not ask for a workspace: %q", prompt)
	}
}
