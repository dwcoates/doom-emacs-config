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
	exePath := filepath.Join(moduleRoot, "daemon", "agent-repld")
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
	// Arrange: <tmp>/modules/app/agent-repl/daemon/agent-repld.
	tmp := t.TempDir()
	moduleRoot := filepath.Join(tmp, "modules", "app", "agent-repl")
	daemonDir := filepath.Join(moduleRoot, "daemon")
	if err := os.MkdirAll(daemonDir, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	exePath := filepath.Join(daemonDir, "agent-repld")

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
	// lives at <tmp>/repo/some/other/deploy/agent-repld, well outside it.
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
	exePath := filepath.Join(deployDir, "agent-repld")

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

// stampedTree builds a marked module root on disk and an executable path
// outside it, so only the stamp can name the checkout.
func stampedTree(t *testing.T) (root, exePath string) {
	t.Helper()
	unsetEnv(t)
	root = filepath.Join(t.TempDir(), "repo", "modules", "app", "agent-repl")
	if err := os.MkdirAll(root, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	return root, filepath.Join(t.TempDir(), "bin", "agent-repld")
}

// TestResolveRootTakesTheStampOverTheRefusal pins that the stamp is consulted
// before the refusal, which is what makes a staged deploy build, outside the
// tree, resolvable at all.
func TestResolveRootTakesTheStampOverTheRefusal(t *testing.T) {
	tests := []struct {
		name  string
		stamp func(root string) string
	}{
		{"the stamp as written", func(root string) string { return root }},
		{"a stamp with a trailing separator", func(root string) string { return root + "/" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root, exePath := stampedTree(t)

			// Act.
			got, err := resolveRoot(exePath, tt.stamp(root))

			// Assert.
			if err != nil || got != root {
				t.Fatalf("resolveRoot() = (%q, %v), want the stamped root %q", got, err, root)
			}
		})
	}
}

// TestResolveRootRefusesWhenNothingNamesACheckout covers every loud refusal:
// no environment override, no marker above the executable, and a stamp that
// names no usable checkout. Each refusal names the executable, the marker and
// the override, so the operator sees how to recover.
func TestResolveRootRefusesWhenNothingNamesACheckout(t *testing.T) {
	tests := []struct {
		name string
		// stamp is the stamp, given the marked root stampedTree made.
		stamp func(t *testing.T, root string) string
		// mentions are what the refusal must name besides exePath, marker and Env.
		mentions []string
	}{
		{"an unstamped binary", func(*testing.T, string) string { return "" }, []string{builtFromSymbol}},
		{"a stamp that is no module root", func(_ *testing.T, root string) string { return filepath.Dir(root) }, []string{"is not a"}},
		{"a checkout that moved after the build", func(_ *testing.T, root string) string {
			return filepath.Join(filepath.Dir(root), "gone", "modules", "app", "agent-repl")
		}, []string{"unusable", "no such file"}},
		{"a stamped root that is a file", func(t *testing.T, root string) string {
			file := filepath.Join(filepath.Dir(root), "file", "modules", "app", "agent-repl")
			if err := os.MkdirAll(filepath.Dir(file), 0o755); err != nil {
				t.Fatalf("MkdirAll: %v", err)
			}
			if err := os.WriteFile(file, nil, 0o644); err != nil {
				t.Fatalf("WriteFile: %v", err)
			}
			return file
		}, []string{"is not a directory"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root, exePath := stampedTree(t)

			stamp := tt.stamp(t, root)

			// Act.
			got, err := resolveRoot(exePath, stamp)

			// Assert.
			if err == nil {
				t.Fatalf("resolveRoot() = %q, want a refusal rather than a guessed root", got)
			}
			for _, want := range append([]string{exePath, marker, Env}, tt.mentions...) {
				if !strings.Contains(err.Error(), want) {
					t.Fatalf("err = %v, want it to name %q", err, want)
				}
			}
		})
	}
}

// TestBuiltFromLinkBindsTheStamp pins the -X binding's spelling to the
// variable it sets: a rename of the package or the variable fails here rather
// than leaving every deploy build silently unstamped.
func TestBuiltFromLinkBindsTheStamp(t *testing.T) {
	if got, want := BuiltFromLink("/r"), "agent-repld/internal/checkout.builtFrom=/r"; got != want {
		t.Fatalf("BuiltFromLink() = %q, want %q", got, want)
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
