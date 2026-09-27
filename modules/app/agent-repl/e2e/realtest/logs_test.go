//go:build realtest

package realtest

import (
	"os"
	"path/filepath"
	"testing"
)

func TestEnumerateNamesTheFiveCanonicalWorkspaceSinks(t *testing.T) {
	// Arrange: the logging contract's persistence layout is five links per
	// workspace, and a harvester that read four of them would be silently
	// blind to one whole runtime's records.
	dir := t.TempDir()
	env := Env{Workspaces: []Workspace{{ID: "ws1", Dir: dir, Name: "one"}}}

	// Act.
	sources, err := EnumerateSources(env)
	if err != nil {
		t.Fatalf("enumerate: %v", err)
	}

	// Assert.
	for _, sink := range []string{"emacs.log", "daemon.log", "shim.log", "webapp.log", "sidecar.log"} {
		want := filepath.Join(dir, ".claude", "emacs", sink)
		found := false
		for _, src := range sources {
			if src.Path == want {
				found = true
				if src.Workspace != "ws1" {
					t.Errorf("%s is attributed to %q, want the workspace that owns it", sink, src.Workspace)
				}
			}
		}
		if !found {
			t.Errorf("the enumeration does not name %s", want)
		}
	}
}

func TestEnumerateDoesNotNameTheWorkspaceSinkTargets(t *testing.T) {
	// Arrange: the per-workspace targets live under the state root, and every
	// one of them is already reachable through the link that names it.
	// Enumerating both would report every record twice.
	stateDir := t.TempDir()
	logs := filepath.Join(stateDir, "logs")
	if err := os.MkdirAll(logs, 0o755); err != nil {
		t.Fatalf("create %s: %v", logs, err)
	}
	target := filepath.Join(logs, "agent-repl-99808d49-daemon-311774699.log")
	if err := os.WriteFile(target, nil, 0o644); err != nil {
		t.Fatalf("write %s: %v", target, err)
	}
	if err := os.WriteFile(filepath.Join(logs, "daemon.run.log"), nil, 0o644); err != nil {
		t.Fatalf("write the daemon's global sink: %v", err)
	}

	// Act.
	sources, err := EnumerateSources(Env{StateDir: stateDir})
	if err != nil {
		t.Fatalf("enumerate: %v", err)
	}

	// Assert.
	for _, src := range sources {
		if src.Path == target {
			t.Errorf("the enumeration names a workspace sink TARGET (%s); it must be reached only through the canonical link", target)
		}
	}
	if len(sources) != 1 || sources[0].Name != "daemon.global" {
		t.Errorf("enumerated %+v, want only the daemon's global sink", sources)
	}
}

func TestEnumerateNamesTheDaemonsRotationSiblings(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	logs := filepath.Join(stateDir, "logs")
	if err := os.MkdirAll(logs, 0o755); err != nil {
		t.Fatalf("create %s: %v", logs, err)
	}
	for _, name := range []string{"daemon.run.log", "daemon.run.log.1", "daemon.run.log.5"} {
		if err := os.WriteFile(filepath.Join(logs, name), nil, 0o644); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
	}

	// Act.
	sources, err := EnumerateSources(Env{StateDir: stateDir})
	if err != nil {
		t.Fatalf("enumerate: %v", err)
	}

	// Assert.
	if len(sources) != 3 {
		t.Fatalf("enumerated %d source(s), want the log and its two rotation siblings: %+v", len(sources), sources)
	}
}

func TestEnumerateSeparatesTheServicesStderrFromTheirRecords(t *testing.T) {
	// Arrange: the two are read differently — one by timestamp and level, the
	// other by "anything at all is wrong" — so they cannot share a kind.
	cacheDir := t.TempDir()
	for _, name := range []string{
		"shim-store.log", "shim-store.log.1", "shim-store.err.log", "shim-store.out.log",
		"shim-claude-sidecar.log", "shim-claude-sidecar.err.log",
	} {
		if err := os.WriteFile(filepath.Join(cacheDir, name), nil, 0o644); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
	}

	// Act.
	sources, err := EnumerateSources(Env{CacheLogDir: cacheDir})
	if err != nil {
		t.Fatalf("enumerate: %v", err)
	}

	// Assert.
	kinds := map[string]SourceKind{}
	for _, src := range sources {
		kinds[filepath.Base(src.Path)] = src.Kind
	}
	for name, want := range map[string]SourceKind{
		"shim-store.log":              KindJSONL,
		"shim-store.log.1":            KindJSONL,
		"shim-store.err.log":          KindStderr,
		"shim-claude-sidecar.log":     KindJSONL,
		"shim-claude-sidecar.err.log": KindStderr,
	} {
		got, ok := kinds[name]
		if !ok {
			t.Errorf("the enumeration does not name %s", name)
			continue
		}
		if got != want {
			t.Errorf("%s is enumerated as %s, want %s", name, got, want)
		}
	}
	if _, named := kinds["shim-store.out.log"]; named {
		t.Errorf("the enumeration names shim-store.out.log, which is excluded deliberately")
	}
}

func TestEnumerateNamesTheModuleLogAndItsPreviousSibling(t *testing.T) {
	// Arrange.
	moduleLog := filepath.Join(t.TempDir(), "doom-agent-repl-501", "doom-agent-repl.log")

	// Act.
	sources, err := EnumerateSources(Env{ModuleLog: moduleLog})
	if err != nil {
		t.Fatalf("enumerate: %v", err)
	}

	// Assert: both, even though neither exists — a source snapshotted as
	// absent and read from zero is how a file the run itself created gets
	// harvested.
	if len(sources) != 2 {
		t.Fatalf("enumerated %d source(s), want the module log and its .prev sibling: %+v", len(sources), sources)
	}
	if sources[0].Path != moduleLog || sources[1].Path != moduleLog+".prev" {
		t.Errorf("enumerated %q and %q", sources[0].Path, sources[1].Path)
	}
}

func TestRealEnvResolvesTheModuleLogToTheDurableCentralSink(t *testing.T) {
	// Arrange: lisp/core.el's default is the state root's
	// logs/emacs.central.log; both earlier defaults are RETIRED and hold only
	// historical records.
	env := RealEnv("/Users/someone", nil)

	// Assert.
	want := "/Users/someone/.claude-emacs/logs/emacs.central.log"
	if env.ModuleLog != want {
		t.Errorf("the module log resolved to %q, want %q", env.ModuleLog, want)
	}
	if env.StateDir != "/Users/someone/.claude-emacs" {
		t.Errorf("the state root resolved to %q", env.StateDir)
	}
	if env.CacheLogDir != "/Users/someone/.cache/agent-repl/log" {
		t.Errorf("the service log directory resolved to %q", env.CacheLogDir)
	}
}
