package checkout_test

import (
	"os"
	"path/filepath"
	"strings"
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

// TestRootNoMarkerReturnsError verifies that when no ancestor carries the
// marker and no environment override is set, Root fails loudly and names the
// executable path rather than guessing.
func TestRootNoMarkerReturnsError(t *testing.T) {
	// Arrange: a bare directory tree with no "modules/app/agent-repl"
	// anywhere in it.
	tmp := t.TempDir()
	exePath := filepath.Join(tmp, "bin", "claude-repld")
	if err := os.MkdirAll(filepath.Dir(exePath), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	// Act.
	got, err := checkout.Root(exePath)

	// Assert.
	if err == nil {
		t.Fatalf("Root() = %q, want an error", got)
	}
	if !strings.Contains(err.Error(), exePath) {
		t.Fatalf("Root() error %q does not name the executable path %q", err, exePath)
	}
	if !strings.Contains(err.Error(), checkout.Env) {
		t.Fatalf("Root() error %q does not name %s", err, checkout.Env)
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
