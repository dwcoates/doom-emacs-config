//go:build realtest

package realtest

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestEnumerateNamesEveryCanonicalWorkspaceSinkAndItsWarningsTwin(t *testing.T) {
	// Arrange: the logging contract's persistence layout is five links per
	// workspace, each with its warnings twin, and a harvester that read nine
	// of them would be silently blind to one whole runtime's records.
	dir := t.TempDir()
	env := Env{Workspaces: []Workspace{{ID: "ws1", Dir: dir, Name: "one"}}}

	// Act.
	sources, err := EnumerateSources(env)
	if err != nil {
		t.Fatalf("enumerate: %v", err)
	}

	// Assert.
	for _, sink := range []string{
		"emacs.log", "daemon.log", "shim.log", "webapp.log", "sidecar.log",
		"emacs.warn.log", "daemon.warn.log", "shim.warn.log", "webapp.warn.log", "sidecar.warn.log",
	} {
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

	// Assert: all three, even though none exists — a source snapshotted as
	// absent and read from zero is how a file the run itself created gets
	// harvested.
	var paths []string
	for _, src := range sources {
		paths = append(paths, src.Path)
	}
	warnings := strings.TrimSuffix(moduleLog, ".log") + ".warn.log"
	if !slices.Equal(paths, []string{moduleLog, moduleLog + ".prev", warnings}) {
		t.Errorf("enumerated %q, want the module log, its .prev sibling and its warnings file", paths)
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

func TestEnumerateNamesTheGlobalWarningsFiles(t *testing.T) {
	// Arrange: every runtime writes its warn and error records only to its
	// log's warnings file, so a harvest blind to those reads no warning.
	stateDir, cacheDir := t.TempDir(), t.TempDir()
	logs := filepath.Join(stateDir, "logs")
	if err := os.MkdirAll(logs, 0o755); err != nil {
		t.Fatalf("create %s: %v", logs, err)
	}
	files := []string{
		filepath.Join(logs, "daemon.run.warn.log"),
		filepath.Join(logs, "daemon.run.warn.log.1"),
		filepath.Join(cacheDir, "shim-store.warn.log"),
		filepath.Join(cacheDir, "shim-claude-sidecar.warn.log"),
	}
	for _, file := range files {
		if err := os.WriteFile(file, nil, 0o644); err != nil {
			t.Fatalf("write %s: %v", file, err)
		}
	}

	// Act.
	sources, err := EnumerateSources(Env{StateDir: stateDir, CacheLogDir: cacheDir})
	if err != nil {
		t.Fatalf("enumerate: %v", err)
	}

	// Assert.
	for _, file := range files {
		if !slices.ContainsFunc(sources, func(src Source) bool { return src.Path == file && src.Kind == KindJSONL }) {
			t.Errorf("the enumeration does not name %s as records: %+v", file, sources)
		}
	}
}
