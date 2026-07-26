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

func TestNewRefusesAnUngatedModeWithoutConsent(t *testing.T) {
	// Arrange — an ungated analyst approves its own tool calls against a real
	// checkout, unattended, so it takes an operator who said so.
	cfg := Config{
		Dir:            "/repo",
		PermissionMode: "bypassPermissions",
		Start:          func([]string, string) error { return nil },
		Logf:           func(string, ...any) {},
	}
	// Act
	_, err := New(cfg, time.Now())
	// Assert — refused at boot, never downgraded to a gated mode.
	if err == nil {
		t.Fatal("New(bypassPermissions, no consent) = nil error, want a refusal")
	}
}

func TestNewRefusalNamesTheCheckoutAtRisk(t *testing.T) {
	// Arrange
	cfg := Config{
		Dir:            "/home/u/config",
		PermissionMode: "bypassPermissions",
		Start:          func([]string, string) error { return nil },
		Logf:           func(string, ...any) {},
	}
	// Act
	_, err := New(cfg, time.Now())
	// Assert — the operator must be able to see WHAT would have been exposed.
	if err == nil || !strings.Contains(err.Error(), "/home/u/config") {
		t.Fatalf("err = %v, want it to name the remediation dir", err)
	}
}

func TestNewAcceptsAnUngatedModeWithConsent(t *testing.T) {
	// Arrange — the analyst cannot function gated (headless auto-deny), so
	// this is the configuration in which the feature actually works.
	rec := &recorder{}
	// Act
	r := newRunner(t, rec, Config{PermissionMode: "bypassPermissions", AllowUngated: true})
	// Assert
	if r == nil {
		t.Fatal("New with consent returned no runner")
	}
}

func TestNewNeedsNoConsentForAGatedMode(t *testing.T) {
	// Arrange
	rec := &recorder{}
	// Act
	r := newRunner(t, rec, Config{PermissionMode: "acceptEdits"})
	// Assert
	if r == nil {
		t.Fatal("New(acceptEdits) returned no runner")
	}
}

func TestNewRecordsTheUngatedAnalystAtBoot(t *testing.T) {
	// Arrange — the operator learns of an ungated analyst when the daemon
	// adopts the config, not when a session is finally lost.
	var lines []string
	cfg := Config{
		Dir:            "/repo",
		PermissionMode: "bypassPermissions",
		AllowUngated:   true,
		Start:          func([]string, string) error { return nil },
		Logf:           func(f string, a ...any) { lines = append(lines, fmt.Sprintf(f, a...)) },
	}
	// Act
	if _, err := New(cfg, time.Now()); err != nil {
		t.Fatalf("New: %v", err)
	}
	// Assert
	if len(lines) != 1 || !strings.Contains(lines[0], "UNGATED") {
		t.Fatalf("boot log = %v, want one UNGATED record", lines)
	}
}

func TestNewLogsNothingAtBootForAGatedAnalyst(t *testing.T) {
	// Arrange
	var lines []string
	cfg := Config{
		Dir:            "/repo",
		PermissionMode: "acceptEdits",
		Start:          func([]string, string) error { return nil },
		Logf:           func(f string, a ...any) { lines = append(lines, fmt.Sprintf(f, a...)) },
	}
	// Act
	if _, err := New(cfg, time.Now()); err != nil {
		t.Fatalf("New: %v", err)
	}
	// Assert
	if len(lines) != 0 {
		t.Fatalf("boot log = %v, want nothing for a gated analyst", lines)
	}
}

func TestStartRecordsTheUngatedDispatch(t *testing.T) {
	// Arrange — the spawn itself is named, with the consent that admitted it,
	// exactly as an ungated session create is.
	rec := &recorder{}
	var lines []string
	r, err := New(Config{
		Dir:            "/repo",
		PermissionMode: "bypassPermissions",
		AllowUngated:   true,
		Start:          rec.start,
		Logf:           func(f string, a ...any) { lines = append(lines, fmt.Sprintf(f, a...)) },
	}, time.Now())
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	// Act
	if _, err := r.Start("s_abc"); err != nil {
		t.Fatalf("Start: %v", err)
	}
	// Assert — the last line is the dispatch, and it carries the note.
	dispatch := lines[len(lines)-1]
	if !strings.Contains(dispatch, "UNGATED") || !strings.Contains(dispatch, "consent=true") {
		t.Fatalf("dispatch log = %q, want the ungated record with its consent", dispatch)
	}
}

func TestStartDispatchRecordIsPlainForAGatedAnalyst(t *testing.T) {
	// Arrange
	rec := &recorder{}
	var lines []string
	r, err := New(Config{
		Dir:            "/repo",
		PermissionMode: "acceptEdits",
		Start:          rec.start,
		Logf:           func(f string, a ...any) { lines = append(lines, fmt.Sprintf(f, a...)) },
	}, time.Now())
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	// Act
	if _, err := r.Start("s_abc"); err != nil {
		t.Fatalf("Start: %v", err)
	}
	// Assert
	if strings.Contains(lines[len(lines)-1], "UNGATED") {
		t.Fatalf("dispatch log = %q, want no ungated claim for a gated analyst", lines[len(lines)-1])
	}
}

func TestStartCarriesTheConfiguredPermissionMode(t *testing.T) {
	// Arrange — the operator opted the headless analyst out of gating.
	rec := &recorder{}
	r := newRunner(t, rec, Config{PermissionMode: "bypassPermissions", AllowUngated: true})
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
