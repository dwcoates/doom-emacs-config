package reload

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// deployedBinaryFixture lays out a checkout the way a deployed daemon sits in
// one: the marker script under modules/app/agent-repl/bin, and the binary well
// below it.
func deployedBinaryFixture(t *testing.T, repo string) string {
	t.Helper()
	writeFixtureFile(t, repo, DeployScriptRelPath, "#!/bin/sh\nexit 0\n")
	exe := filepath.Join(repo, filepath.FromSlash(stackPrefix+"daemon/bin/claude-repld"))
	if err := os.MkdirAll(filepath.Dir(exe), 0o755); err != nil {
		t.Fatalf("mkdir bin: %v", err)
	}
	if err := os.WriteFile(exe, []byte("binary"), 0o755); err != nil {
		t.Fatalf("write fake binary: %v", err)
	}
	return exe
}

func TestResolveSelfFindsTheCheckoutTheBinaryWasDeployedFrom(t *testing.T) {
	// Arrange.
	repo := newFixtureRepo(t)
	exe := deployedBinaryFixture(t, repo)

	// Act.
	self, ok, err := ResolveSelf(context.Background(), exe)

	// Assert.
	if err != nil {
		t.Fatalf("ResolveSelf: %v", err)
	}
	if !ok {
		t.Fatalf("ResolveSelf reported no checkout for a binary inside one")
	}
	if self.Root != repo {
		t.Fatalf("ResolveSelf Root = %s, want %s", self.Root, repo)
	}
	if self.ScriptPath() != filepath.Join(repo, filepath.FromSlash(DeployScriptRelPath)) {
		t.Fatalf("ResolveSelf ScriptPath = %s, want the checkout's deploy-all.sh", self.ScriptPath())
	}
}

// A `go test` binary lives in the build cache, and a hand-copied binary lives
// wherever it was copied. Neither has a checkout to redeploy, and reporting
// that fact is not an error.
func TestResolveSelfReportsNoCheckoutForABinaryOutsideOne(t *testing.T) {
	// Arrange.
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("canonicalize temp dir: %v", err)
	}
	exe := filepath.Join(dir, "claude-repld")
	if err := os.WriteFile(exe, []byte("binary"), 0o755); err != nil {
		t.Fatalf("write fake binary: %v", err)
	}

	// Act.
	self, ok, err := ResolveSelf(context.Background(), exe)

	// Assert.
	if err != nil {
		t.Fatalf("ResolveSelf() error = %v, want the not-deployed answer", err)
	}
	if ok {
		t.Fatalf("ResolveSelf reported a checkout (%+v) for a binary outside one", self)
	}
}

func TestResolveSelfSurfacesAnUnresolvableExecutablePath(t *testing.T) {
	// Arrange — a path that does not exist at all.
	missing := filepath.Join(t.TempDir(), "gone", "claude-repld")

	// Act.
	_, ok, err := ResolveSelf(context.Background(), missing)

	// Assert.
	if err == nil {
		t.Fatalf("ResolveSelf() error = nil, want the canonicalization failure surfaced")
	}
	if ok {
		t.Fatalf("ResolveSelf reported a checkout despite failing")
	}
	if !strings.Contains(err.Error(), "canonicalize executable") {
		t.Fatalf("ResolveSelf() error = %v, want it to name the failed step", err)
	}
}

func TestIdentifyRepoDistinguishesTwoRepositories(t *testing.T) {
	// Arrange.
	one := newFixtureRepo(t)
	two := newFixtureRepo(t)

	// Act.
	identOne, err := IdentifyRepo(context.Background(), one)
	if err != nil {
		t.Fatalf("IdentifyRepo(one): %v", err)
	}
	identTwo, err := IdentifyRepo(context.Background(), two)
	if err != nil {
		t.Fatalf("IdentifyRepo(two): %v", err)
	}

	// Assert.
	if identOne.CommonDir == identTwo.CommonDir {
		t.Fatalf("two unrelated repositories share the common dir %s", identOne.CommonDir)
	}
}

// Every linked worktree of one repository shares its common dir, which is what
// makes the common dir the repository identity and the toplevel the worktree
// identity.
func TestIdentifyRepoSharesTheCommonDirAcrossLinkedWorktrees(t *testing.T) {
	// Arrange.
	main := newFixtureRepo(t)
	linked, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("canonicalize temp dir: %v", err)
	}
	linked = filepath.Join(linked, "sibling")
	gitFixture(t, main, "worktree", "add", "-b", "sibling", linked, "main")

	// Act.
	identMain, err := IdentifyRepo(context.Background(), main)
	if err != nil {
		t.Fatalf("IdentifyRepo(main): %v", err)
	}
	identLinked, err := IdentifyRepo(context.Background(), linked)
	if err != nil {
		t.Fatalf("IdentifyRepo(linked): %v", err)
	}

	// Assert.
	if identMain.CommonDir != identLinked.CommonDir {
		t.Fatalf("linked worktree common dir = %s, want the main worktree's %s", identLinked.CommonDir, identMain.CommonDir)
	}
	if identMain.Toplevel == identLinked.Toplevel {
		t.Fatalf("linked worktree toplevel = %s, want it to differ from the main worktree's", identLinked.Toplevel)
	}
}

func TestIdentifyRepoSurfacesADirectoryThatIsNoRepository(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	_, err := IdentifyRepo(context.Background(), dir)

	// Assert.
	if err == nil {
		t.Fatalf("IdentifyRepo(%s) error = nil, want the git failure surfaced", dir)
	}
}

func TestIdentifyRepoRefusesAnEmptyDirectory(t *testing.T) {
	// Act.
	_, err := IdentifyRepo(context.Background(), "")

	// Assert.
	if err == nil {
		t.Fatalf("IdentifyRepo(\"\") error = nil, want a refusal")
	}
}
