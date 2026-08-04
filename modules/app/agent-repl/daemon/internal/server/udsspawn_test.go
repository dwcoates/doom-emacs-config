package server

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/shim"
)

// ---------------------------------------------------------------------------
// THE ONE UDS SPAWN (udsspawn.go).
//
// These pin the parts three hand-written closures used to each own privately:
// the canonical workspace the child runs in, the fd-3 contract derived from the
// log target rather than written down, and the hard failure a target closed
// underneath the daemon produces.
// ---------------------------------------------------------------------------

// spawnRecorder captures what the procedure asked the launcher for, and runs a
// trivial real process in the shim's place so the caller still receives a
// genuine *shim.Proc to reap.
type spawnRecorder struct {
	mu      sync.Mutex
	options []shim.Options
	events  []string
}

func (r *spawnRecorder) spawn(opts shim.Options) (*shim.Proc, error) {
	r.mu.Lock()
	r.options = append(r.options, opts)
	r.mu.Unlock()
	stand := opts
	stand.Argv = []string{"/bin/sh", "-c", "true"}
	return shim.Spawn(stand)
}

func (r *spawnRecorder) captured() []shim.Options {
	r.mu.Lock()
	defer r.mu.Unlock()
	return slices.Clone(r.options)
}

func (r *spawnRecorder) recordEvent(_ dlog.Level, _ dlog.Workspace, _, message string, _ map[string]any) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.events = append(r.events, message)
}

func (r *spawnRecorder) recorded() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return slices.Clone(r.events)
}

// newSpawnRig builds the procedure over a temp workspace and a real target
// manager, with the launcher and the recorder returned for assertions.
func newSpawnRig(t *testing.T, cwd string) (ShimSpawnFunc, *spawnRecorder, *dlog.TargetManager) {
	t.Helper()
	targets := dlog.NewTargetManager()
	t.Cleanup(func() { _ = targets.Close() })
	recorder := &spawnRecorder{}
	spawner, err := NewUDSSpawner(UDSSpawnConfig{
		Targets:    targets,
		Node:       "node",
		Script:     "shim.js",
		ShimSocket: filepath.Join(t.TempDir(), "shim.sock"),
		Logger:     func(dlog.Workspace, string) shim.Logger { return discardShimLogger{} },
		Event:      recorder.recordEvent,
		Spawn:      recorder.spawn,
	})
	if err != nil {
		t.Fatalf("NewUDSSpawner: %v", err)
	}
	_ = cwd
	return spawner, recorder, targets
}

type discardShimLogger struct{}

func (discardShimLogger) Log(string, ...any)        {}
func (discardShimLogger) LogVerbose(string, ...any) {}

// THE CHILD RUNS IN THE CANONICAL WORKSPACE, not in whatever alias the caller
// happened to hold: the workspace identity the log targets are keyed by is the
// resolved directory, and a child running under the alias would file its
// records under a different workspace than the daemon's.
func TestUDSSpawnRunsTheChildInTheCanonicalWorkspace(t *testing.T) {
	// Arrange.
	real := t.TempDir()
	alias := filepath.Join(t.TempDir(), "workspace-alias")
	if err := os.Symlink(real, alias); err != nil {
		t.Fatal(err)
	}
	spawner, recorder, _ := newSpawnRig(t, alias)
	canonical, err := dlog.WorkspaceFromDirectory(alias)
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	stop, err := spawner("s1", CreateOpts{CWD: alias, Model: "haiku"})
	if err != nil {
		t.Fatalf("spawn: %v", err)
	}
	if stop == nil {
		t.Fatal("spawn returned no stop func")
	}

	// Assert.
	captured := recorder.captured()
	if len(captured) != 1 {
		t.Fatalf("captured %d spawns, want 1", len(captured))
	}
	if captured[0].Dir != canonical.Directory {
		t.Fatalf("child Dir = %q, want the canonical workspace %q", captured[0].Dir, canonical.Directory)
	}
	if !slices.Contains(captured[0].Argv, canonical.Directory) || slices.Contains(captured[0].Argv, alias) {
		t.Fatalf("argv = %v, want the canonical directory and not the alias %q", captured[0].Argv, alias)
	}
}

// THE FD CONTRACT IS DERIVED FROM THE SLICE, so the flag's value and the
// descriptor the child actually inherits cannot disagree.
func TestUDSSpawnDerivesTheLogFDFromTheExtraFilesPosition(t *testing.T) {
	// Arrange.
	workspace := t.TempDir()
	spawner, recorder, _ := newSpawnRig(t, workspace)

	// Act.
	if _, err := spawner("s1", CreateOpts{CWD: workspace}); err != nil {
		t.Fatalf("spawn: %v", err)
	}

	// Assert.
	captured := recorder.captured()[0]
	if len(captured.ExtraFiles) != 1 {
		t.Fatalf("ExtraFiles = %v, want exactly the one log target", captured.ExtraFiles)
	}
	flag := slices.Index(captured.Argv, dlog.ChildLogFDFlag)
	if flag < 0 || flag+1 >= len(captured.Argv) {
		t.Fatalf("argv = %v, want a %s pair", captured.Argv, dlog.ChildLogFDFlag)
	}
	if captured.Argv[flag+1] != "3" {
		t.Fatalf("%s = %q, want 3 — the first extra file lands on descriptor 3", dlog.ChildLogFDFlag, captured.Argv[flag+1])
	}
}

// THE DOUBLE USE, proved: a borrowed target closed by force (which no ordinary
// caller can do, hence the test-only backdoor) makes the NEXT spawn fail hard
// with a diagnostic naming the closed target — never a child handed a dead
// descriptor — and leaves no process behind.
func TestUDSSpawnFailsHardOnAClosedLogTarget(t *testing.T) {
	// Arrange — one spawn to establish the target, then close it underneath.
	workspace := t.TempDir()
	spawner, recorder, targets := newSpawnRig(t, workspace)
	if _, err := spawner("s1", CreateOpts{CWD: workspace}); err != nil {
		t.Fatalf("first spawn: %v", err)
	}
	resolved, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		t.Fatal(err)
	}
	borrowed, err := targets.BorrowWorkspaceRuntime(resolved, dlog.RuntimeShim)
	if err != nil {
		t.Fatalf("borrow: %v", err)
	}
	if err := dlog.CloseBorrowedTargetForTest(borrowed); err != nil {
		t.Fatalf("force-close: %v", err)
	}
	before := len(recorder.captured())
	// Counted rather than sliced: the first spawn's reaper records its exit on
	// its own goroutine, so only the count of THIS message is stable here.
	spawnedRecords := func() int {
		count := 0
		for _, message := range recorder.recorded() {
			if message == "UDS shim spawned" {
				count++
			}
		}
		return count
	}
	spawnedBefore := spawnedRecords()

	// Act.
	stop, err := spawner("s2", CreateOpts{CWD: workspace})

	// Assert — the refusal names the closed target, and nothing was launched.
	if err == nil {
		t.Fatal("a spawn against a closed log target succeeded; the child would hold a dead descriptor")
	}
	if !strings.Contains(err.Error(), "CLOSED") || !strings.Contains(err.Error(), "shim log target") {
		t.Fatalf("error = %v, want the canonical closed-target diagnostic", err)
	}
	if stop != nil {
		t.Fatal("a failed spawn handed back a stop func")
	}
	if got := len(recorder.captured()); got != before {
		t.Fatalf("captured %d spawns after the failure, want %d — nothing may be launched", got, before)
	}
	if got := spawnedRecords(); got != spawnedBefore {
		t.Fatalf("spawned records = %d, want %d — a spawn that never happened must not be recorded as one", got, spawnedBefore)
	}
}

// THE WIRING IS VALIDATED AT CONSTRUCTION, so a missing collaborator is a boot
// failure rather than a nil-deref at the first bring-up.
func TestNewUDSSpawnerRefusesIncompleteWiring(t *testing.T) {
	complete := func() UDSSpawnConfig {
		return UDSSpawnConfig{
			Targets:    dlog.NewTargetManager(),
			Node:       "node",
			Script:     "shim.js",
			ShimSocket: "/tmp/shim.sock",
			Logger:     func(dlog.Workspace, string) shim.Logger { return discardShimLogger{} },
			Event:      func(dlog.Level, dlog.Workspace, string, string, map[string]any) {},
		}
	}
	cases := []struct {
		name    string
		break_  func(*UDSSpawnConfig)
		wantErr string
	}{
		{"no targets", func(c *UDSSpawnConfig) { c.Targets = nil }, "log target manager"},
		{"no node", func(c *UDSSpawnConfig) { c.Node = "" }, "node binary"},
		{"no script", func(c *UDSSpawnConfig) { c.Script = "" }, "shim entrypoint"},
		{"no socket", func(c *UDSSpawnConfig) { c.ShimSocket = "" }, "daemon shim socket"},
		{"no logger", func(c *UDSSpawnConfig) { c.Logger = nil }, "Logger factory"},
		{"no event recorder", func(c *UDSSpawnConfig) { c.Event = nil }, "Event recorder"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cfg := complete()
			tc.break_(&cfg)

			// Act.
			_, err := NewUDSSpawner(cfg)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("error = %v, want one naming %q", err, tc.wantErr)
			}
		})
	}
}
