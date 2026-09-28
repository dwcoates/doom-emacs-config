package checkout

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
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
	t.Setenv(Env, filepath.Join(tmp, "elsewhere", "..", "elsewhere-override"))

	// Act.
	got, err := Root(exePath)

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
	got, err := Root(exePath)

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
	got, err := Root(exePath)

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
	got, err := Root(exePath)

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
		{name: "shim main", got: ShimMain(root), want: filepath.Join(root, "agent-shim", "claude", "shim", "dist", "main.js")},
		{name: "webapp dist", got: WebappDist(root), want: filepath.Join(root, "webapp", "dist")},
		{name: "prompts dir", got: PromptsDir(root), want: filepath.Join(root, "prompts")},
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
	got := VocabDir(root)

	// Assert
	if want := filepath.Join(root, "proto", "vocab"); got != want {
		t.Fatalf("VocabDir = %q, want %q", got, want)
	}
}

// TestShimBuildStampIsBesideTheCompiledEntryPoint covers the production source
// of SHIM_BUILD_SHA: the stamp the shim's build chain writes next to its
// bundle, which the daemon refuses to boot without.
func TestShimBuildStampIsBesideTheCompiledEntryPoint(t *testing.T) {
	// Arrange.
	root := filepath.FromSlash("/checkout/modules/app/agent-repl")

	// Act.
	got := ShimBuildStamp(root)

	// Assert.
	if want := filepath.Join(filepath.Dir(ShimMain(root)), ".built-sha"); got != want {
		t.Fatalf("ShimBuildStamp() = %q, want %q", got, want)
	}
}

// TestResolveRootRefusesWhenNothingNamesACheckout covers the loud refusal: no
// environment override, no marker above the executable, and a compiled-in
// path that names none either — which is what a binary copied off a machine
// whose checkout is gone looks like.
func TestResolveRootRefusesWhenNothingNamesACheckout(t *testing.T) {
	// Arrange.
	unsetEnv(t)
	exePath := filepath.Join(t.TempDir(), "bin", "claude-repld")

	// Act.
	got, err := resolveRoot(exePath, func() (string, bool) { return "", false })

	// Assert.
	if err == nil {
		t.Fatalf("resolveRoot() = %q, want a refusal rather than a guessed root", got)
	}
	for _, want := range []string{exePath, marker, Env} {
		if !strings.Contains(err.Error(), want) {
			t.Fatalf("err = %v, want it to name %q", err, want)
		}
	}
}

// TestResolveRootTakesTheCompiledFallbackOverTheRefusal pins that the last
// resort is consulted before the refusal, which is what makes a binary built
// with `go build -o <tmp>` resolvable at all.
func TestResolveRootTakesTheCompiledFallbackOverTheRefusal(t *testing.T) {
	// Arrange.
	unsetEnv(t)
	exePath := filepath.Join(t.TempDir(), "bin", "claude-repld")

	// Act.
	got, err := resolveRoot(exePath, func() (string, bool) { return "/built/from/here", true })

	// Assert.
	if err != nil || got != "/built/from/here" {
		t.Fatalf("resolveRoot() = (%q, %v), want the compiled-in root", got, err)
	}
}

// TestCompiledRootFromRefusesAnUnknownFrame covers the case runtime.Caller
// could not answer: no root, never a walk from an empty path.
func TestCompiledRootFromRefusesAnUnknownFrame(t *testing.T) {
	// Arrange, Act.
	got, ok := compiledRootFrom("", false)

	// Assert.
	if ok {
		t.Fatalf("compiledRootFrom() = (%q, true), want no root for an unknown frame", got)
	}
}

// TestCompiledRootFromRefusesACheckoutThatMoved covers the reason the walk
// stats at all: a checkout that moved after the binary was built leaves a
// compiled-in path that no longer exists.
func TestCompiledRootFromRefusesACheckoutThatMoved(t *testing.T) {
	// Arrange: a marked path under a directory nothing was ever created in.
	sourceFile := filepath.Join(t.TempDir(), "gone", "modules", "app", "agent-repl",
		"daemon", "internal", "checkout", "checkout.go")

	// Act.
	got, ok := compiledRootFrom(sourceFile, true)

	// Assert.
	if ok {
		t.Fatalf("compiledRootFrom() = (%q, true), want no root for a checkout that moved", got)
	}
}

// TestCompiledRootFromRefusesASourceInNoCheckout covers the walk running out
// of ancestors: a source path with no marker anywhere above it.
func TestCompiledRootFromRefusesASourceInNoCheckout(t *testing.T) {
	// Arrange.
	sourceFile := filepath.Join(t.TempDir(), "elsewhere", "checkout.go")

	// Act.
	got, ok := compiledRootFrom(sourceFile, true)

	// Assert.
	if ok {
		t.Fatalf("compiledRootFrom() = (%q, true), want no root outside any checkout", got)
	}
}

// TestCompiledRootAnswersThisPackagesOwnCheckout pins the production last
// resort against the tree the tests themselves were compiled from.
func TestCompiledRootAnswersThisPackagesOwnCheckout(t *testing.T) {
	// Arrange, Act.
	got, ok := compiledRoot()

	// Assert.
	if !ok {
		t.Fatal("compiledRoot() = (_, false), want this package's own checkout")
	}
	if filepath.Base(got) != "agent-repl" {
		t.Fatalf("compiledRoot() = %q, want the agent-repl module root", got)
	}
}

// unsetEnv removes AGENT_REPL_CHECKOUT for one test, restoring whatever the
// process had (set or unset) afterwards.
func unsetEnv(t *testing.T) {
	t.Helper()
	t.Setenv(Env, "restored-by-cleanup")
	if err := os.Unsetenv(Env); err != nil {
		t.Fatalf("Unsetenv(%s) = %v", Env, err)
	}
}

// TestRepoRoot verifies the repository root is the module root less the
// marker, and that a root not ending in the marker is refused.
func TestRepoRoot(t *testing.T) {
	tests := []struct {
		name    string
		root    string
		want    string
		wantErr bool
	}{
		{name: "a module root answers its repository", root: "/home/u/.config/doom/modules/app/agent-repl", want: "/home/u/.config/doom"},
		{name: "a trailing separator is cleaned first", root: "/repo/modules/app/agent-repl/", want: "/repo"},
		{name: "an unmarked root is refused", root: "/pinned", wantErr: true},
		{name: "a partial marker is refused", root: "/repo/app/agent-repl", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.root.

			// Act.
			got, err := RepoRoot(tt.root)

			// Assert.
			if tt.wantErr {
				if err == nil || !strings.Contains(err.Error(), tt.root) {
					t.Fatalf("RepoRoot(%q) = %q, %v; want an error naming the root", tt.root, got, err)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("RepoRoot(%q) = %q, %v; want %q", tt.root, got, err, tt.want)
			}
		})
	}
}
