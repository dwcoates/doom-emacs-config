package reload

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/gitexec"
)

// The fixtures here build REAL git repositories in temp dirs. The derivations
// under test read cherry-pick annotations and repository identities that only
// git itself produces faithfully, so a hand-rolled fake would test the fake.
//
// Nothing here ever touches the machine's own checkout, and nothing here runs
// bin/deploy-all.sh.

// gitFixture runs a git command against a fixture repository. It carries its
// own identity and disables the user's global and system config, so a
// developer's commit.gpgsign or init.defaultBranch cannot reach these repos.
func gitFixture(t *testing.T, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
	cmd.Env = append(gitexec.StripEnv(os.Environ()),
		"GIT_CONFIG_GLOBAL=/dev/null",
		"GIT_CONFIG_SYSTEM=/dev/null",
		"GIT_AUTHOR_NAME=reload fixture",
		"GIT_AUTHOR_EMAIL=reload@fixture.invalid",
		"GIT_COMMITTER_NAME=reload fixture",
		"GIT_COMMITTER_EMAIL=reload@fixture.invalid",
	)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v in %s: %v\n%s", args, dir, err, out)
	}
	return strings.TrimSpace(string(out))
}

// writeFixtureFile writes a repository-relative file, creating its parents.
func writeFixtureFile(t *testing.T, dir, rel, content string) {
	t.Helper()
	full := filepath.Join(dir, filepath.FromSlash(rel))
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		t.Fatalf("mkdir for %s: %v", rel, err)
	}
	if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", rel, err)
	}
}

// newFixtureRepo returns a fresh repository on branch `main` with one base
// commit. The path is canonicalized because macOS hands t.TempDir a /var path
// that is a symlink to /private/var, and every identity comparison under test
// is against a canonicalized path.
func newFixtureRepo(t *testing.T) string {
	t.Helper()
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("canonicalize temp dir: %v", err)
	}
	gitFixture(t, dir, "init", "-b", "main")
	writeFixtureFile(t, dir, "README.md", "base\n")
	gitFixture(t, dir, "add", "README.md")
	gitFixture(t, dir, "commit", "-m", "base commit")
	return dir
}

// commitOnBranch creates branch off `main`, commits files on it, and returns to
// `main`.
func commitOnBranch(t *testing.T, dir, branch, message string, files map[string]string) {
	t.Helper()
	gitFixture(t, dir, "checkout", "-b", branch, "main")
	for rel, content := range files {
		writeFixtureFile(t, dir, rel, content)
	}
	gitFixture(t, dir, "add", "-A")
	gitFixture(t, dir, "commit", "-m", message)
	gitFixture(t, dir, "checkout", "main")
}

// cherryPickBranch replays branch onto `main` exactly as merge.Driver does
// (`cherry-pick -x`), which is what leaves the annotation the landed-range
// derivation reads back.
func cherryPickBranch(t *testing.T, dir, branch string) {
	t.Helper()
	gitFixture(t, dir, "cherry-pick", "-x", "main.."+branch)
}

// mergedFixture is the ordinary arrangement: one branch's commits picked onto
// main, leaving the repository in the state a completed merge leaves it.
func mergedFixture(t *testing.T, branch string, files map[string]string) string {
	t.Helper()
	dir := newFixtureRepo(t)
	commitOnBranch(t, dir, branch, "work on "+branch, files)
	cherryPickBranch(t, dir, branch)
	return dir
}

// fixtureSelf is the SelfRepo identity of a fixture repository.
func fixtureSelf(t *testing.T, dir string) SelfRepo {
	t.Helper()
	ident, err := IdentifyRepo(context.Background(), dir)
	if err != nil {
		t.Fatalf("IdentifyRepo(%s): %v", dir, err)
	}
	return SelfRepo{Root: ident.Toplevel, CommonDir: ident.CommonDir}
}
