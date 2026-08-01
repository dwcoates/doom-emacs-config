package gitexec

import (
	"context"
	"os"
	"slices"
	"strings"
	"testing"
)

// A Git hook exports its repository bindings into every child it runs, and an
// inherited binding retargets `git -C dir` at the HOOK'S repository — which has
// historically both hung the merge e2e suite and flipped core.bare on the live
// checkout. One test per binding: each is an independent leak.
func TestCommandStripsInheritedRepositoryBinding(t *testing.T) {
	tests := []struct {
		name    string
		binding string
		value   string
	}{
		{"git dir", "GIT_DIR", "/somewhere/.git"},
		{"index file", "GIT_INDEX_FILE", "/somewhere/.git/index"},
		{"work tree", "GIT_WORK_TREE", "/somewhere"},
		{"object directory", "GIT_OBJECT_DIRECTORY", "/somewhere/.git/objects"},
		{"common dir", "GIT_COMMON_DIR", "/somewhere/.git"},
		{"prefix", "GIT_PREFIX", "sub/dir/"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a hook-shaped environment.
			t.Setenv(tc.binding, tc.value)

			// Act.
			cmd := Command(context.Background(), t.TempDir(), "status")

			// Assert.
			if slices.ContainsFunc(cmd.Env, func(e string) bool { return strings.HasPrefix(e, tc.binding+"=") }) {
				t.Fatalf("Command env kept %s; git would target the hook's repository", tc.binding)
			}
		})
	}
}

func TestCommandKeepsUnrelatedEnvironment(t *testing.T) {
	// Arrange — an unrelated variable alongside a stripped one.
	t.Setenv("GIT_DIR", "/somewhere/.git")
	t.Setenv("GITEXEC_UNRELATED", "keep-me")

	// Act.
	cmd := Command(context.Background(), t.TempDir(), "status")

	// Assert.
	if !slices.Contains(cmd.Env, "GITEXEC_UNRELATED=keep-me") {
		t.Fatalf("Command env dropped an unrelated variable; only repository bindings may be stripped")
	}
}

func TestCommandKeepsPath(t *testing.T) {
	// Arrange — PATH is what makes `git` resolvable at all.
	t.Setenv("GIT_DIR", "/somewhere/.git")
	if os.Getenv("PATH") == "" {
		t.Skip("no PATH in this environment")
	}

	// Act.
	cmd := Command(context.Background(), t.TempDir(), "status")

	// Assert.
	if !slices.ContainsFunc(cmd.Env, func(e string) bool { return strings.HasPrefix(e, "PATH=") }) {
		t.Fatalf("Command env dropped PATH; git would be unresolvable")
	}
}

func TestCommandPutsDirAheadOfCallerArgs(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	cmd := Command(context.Background(), dir, "rev-parse", "--absolute-git-dir")

	// Assert — `-C dir` must precede the subcommand or git rejects it.
	want := []string{"git", "-C", dir, "rev-parse", "--absolute-git-dir"}
	if !slices.Equal(cmd.Args, want) {
		t.Fatalf("Command args = %v, want %v", cmd.Args, want)
	}
}

func TestStripEnvKeepsNameSubstringMatches(t *testing.T) {
	// Arrange — a variable whose name merely CONTAINS a stripped name must
	// survive; the boundary is the exact name plus '='.
	env := []string{"GIT_DIRECTIVE=keep", "GIT_DIR=/drop/.git"}

	// Act.
	got := StripEnv(env)

	// Assert.
	if !slices.Equal(got, []string{"GIT_DIRECTIVE=keep"}) {
		t.Fatalf("StripEnv(%v) = %v, want only GIT_DIRECTIVE kept", env, got)
	}
}
