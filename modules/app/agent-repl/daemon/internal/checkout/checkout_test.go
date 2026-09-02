package checkout_test

import (
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/checkout"
)

// TestRootEnvironmentOverrideWins verifies that a set AGENT_REPL_CHECKOUT
// wins over the exePath walk, and is used verbatim (cleaned) without being
// validated against the marker.
func TestRootEnvironmentOverrideWins(t *testing.T) {
	// Arrange: an exePath that would otherwise resolve via the walk, and an
	// environment override pointing somewhere unrelated and unmarked.
	tmp := t.TempDir()
	moduleRoot := filepath.Join(tmp, "modules", "app", "agent-repl")
	if err := os.MkdirAll(filepath.Join(moduleRoot, "daemon"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	exePath := filepath.Join(moduleRoot, "daemon", "claude-repld")
	t.Setenv(checkout.Env, filepath.Join(tmp, "elsewhere", "..", "elsewhere-override"))

	// Act.
	got, err := checkout.Root(exePath)

	// Assert.
	if err != nil {
		t.Fatalf("Root() error = %v", err)
	}
	want := filepath.Clean(filepath.Join(tmp, "elsewhere", "..", "elsewhere-override"))
	if got != want {
		t.Fatalf("Root() = %q, want %q", got, want)
	}
}

// TestRootModuleRootAncestor verifies that when the executable lives beneath
// a "modules/app/agent-repl" directory, that directory itself is the root.
func TestRootModuleRootAncestor(t *testing.T) {
	// Arrange: <tmp>/modules/app/agent-repl/daemon/claude-repld.
	tmp := t.TempDir()
	moduleRoot := filepath.Join(tmp, "modules", "app", "agent-repl")
	daemonDir := filepath.Join(moduleRoot, "daemon")
	if err := os.MkdirAll(daemonDir, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	exePath := filepath.Join(daemonDir, "claude-repld")

	// Act.
	got, err := checkout.Root(exePath)

	// Assert.
	if err != nil {
		t.Fatalf("Root() error = %v", err)
	}
	if got != moduleRoot {
		t.Fatalf("Root() = %q, want %q", got, moduleRoot)
	}
}

// TestRootRepositoryRootAncestor verifies that when an ancestor of the
// executable contains "modules/app/agent-repl" as a subdirectory (rather
// than being that directory itself), the root is that subdirectory.
func TestRootRepositoryRootAncestor(t *testing.T) {
	// Arrange: <tmp>/repo/modules/app/agent-repl exists, and the executable
	// lives at <tmp>/repo/some/other/deploy/claude-repld, well outside it.
	tmp := t.TempDir()
	repoRoot := filepath.Join(tmp, "repo")
	moduleRoot := filepath.Join(repoRoot, "modules", "app", "agent-repl")
	if err := os.MkdirAll(moduleRoot, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	deployDir := filepath.Join(repoRoot, "some", "other", "deploy")
	if err := os.MkdirAll(deployDir, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	exePath := filepath.Join(deployDir, "claude-repld")

	// Act.
	got, err := checkout.Root(exePath)

	// Assert.
	if err != nil {
		t.Fatalf("Root() error = %v", err)
	}
	if got != moduleRoot {
		t.Fatalf("Root() = %q, want %q", got, moduleRoot)
	}
}

// TestRootWithNoMarkerNearTheExecutableFallsBack covers the binary built
// OUTSIDE the tree — `go build -o <tmp>`, which is what every test harness and
// every scratch build does. The executable's own ancestors name no checkout,
// so the path this package was compiled from answers instead of a refusal.
func TestRootWithNoMarkerNearTheExecutableFallsBack(t *testing.T) {
	// Arrange: a bare directory tree with no "modules/app/agent-repl" in it.
	tmp := t.TempDir()
	exePath := filepath.Join(tmp, "bin", "claude-repld")
	if err := os.MkdirAll(filepath.Dir(exePath), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	// Act.
	got, err := checkout.Root(exePath)

	// Assert.
	if err != nil {
		t.Fatalf("Root() = %v, want the compiled-in checkout", err)
	}
	if filepath.Base(got) != "agent-repl" {
		t.Fatalf("Root() = %q, want the agent-repl module root", got)
	}
}

// TestDerivedPaths verifies the three fixed paths joined beneath a root.
func TestDerivedPaths(t *testing.T) {
	root := filepath.FromSlash("/checkout/modules/app/agent-repl")

	tests := []struct {
		name string
		got  string
		want string
	}{
		{name: "shim main", got: checkout.ShimMain(root), want: filepath.Join(root, "agent-shim", "claude", "shim", "dist", "main.js")},
		{name: "webapp dist", got: checkout.WebappDist(root), want: filepath.Join(root, "webapp", "dist")},
		{name: "prompts dir", got: checkout.PromptsDir(root), want: filepath.Join(root, "prompts")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert.
			if tc.got != tc.want {
				t.Fatalf("got %q, want %q", tc.got, tc.want)
			}
		})
	}
}

// TestVocabDirIsBeneathTheProtoTree covers the one path the daemon cannot be
// given a flag for: the render vocabulary is shared with every other system
// and lives with the protos.
func TestVocabDirIsBeneathTheProtoTree(t *testing.T) {
	// Arrange
	root := filepath.Join("/checkout", "modules", "app", "agent-repl")

	// Act
	got := checkout.VocabDir(root)

	// Assert
	if want := filepath.Join(root, "proto", "vocab"); got != want {
		t.Fatalf("VocabDir = %q, want %q", got, want)
	}
}
