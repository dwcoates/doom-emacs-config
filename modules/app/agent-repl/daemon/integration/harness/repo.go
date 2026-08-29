package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// Repo is a fake git repository: a main worktree on `main` with one commit.
type Repo struct {
	// Dir is the main worktree.
	Dir string

	t *testing.T
}

// DefaultBranch is the branch every fake repository starts on.
const DefaultBranch = "main"

// NewRepo initializes a repository under the test's temp tree and commits a
// README on the default branch.
func NewRepo(t *testing.T) *Repo {
	t.Helper()
	return NewRepoAt(t, filepath.Join(t.TempDir(), "repo"))
}

// NewRepoAt initializes a repository at an exact path, for the tests that care
// where the workspace lives (the multi-repo root, the daemon's self repo).
func NewRepoAt(t *testing.T, dir string) *Repo {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	r := &Repo{Dir: dir, t: t}
	r.git("init", "--initial-branch="+DefaultBranch)
	r.git("config", "user.name", "Integration Harness")
	r.git("config", "user.email", "harness@example.invalid")
	r.git("config", "commit.gpgsign", "false")
	r.Commit("README.md", "fake repository\n")
	return r
}

// Commit writes a file and commits it, answering the new commit's sha.
func (r *Repo) Commit(file, content string) string {
	r.t.Helper()
	writeFile(r.t, filepath.Join(r.Dir, file), content)
	r.git("add", file)
	r.git("commit", "-m", "add "+file)
	return strings.TrimSpace(r.gitOut("rev-parse", "HEAD"))
}

// Branch creates and checks out a branch off the current head.
func (r *Repo) Branch(name string) {
	r.t.Helper()
	r.git("checkout", "-b", name)
}

// Checkout switches to an existing branch.
func (r *Repo) Checkout(name string) {
	r.t.Helper()
	r.git("checkout", name)
}

// Head is the current commit sha.
func (r *Repo) Head() string {
	r.t.Helper()
	return strings.TrimSpace(r.gitOut("rev-parse", "HEAD"))
}

// Worktrees lists every worktree path the repository knows, main included.
func (r *Repo) Worktrees() []string {
	r.t.Helper()
	var out []string
	for _, line := range strings.Split(r.gitOut("worktree", "list", "--porcelain"), "\n") {
		if path, ok := strings.CutPrefix(strings.TrimSpace(line), "worktree "); ok {
			out = append(out, path)
		}
	}
	return out
}

// Branches lists every local branch.
func (r *Repo) Branches() []string {
	r.t.Helper()
	var out []string
	for _, line := range strings.Split(r.gitOut("branch", "--format=%(refname:short)"), "\n") {
		if b := strings.TrimSpace(line); b != "" {
			out = append(out, b)
		}
	}
	return out
}

// HasBranch reports whether a local branch exists.
func (r *Repo) HasBranch(name string) bool {
	r.t.Helper()
	for _, b := range r.Branches() {
		if b == name {
			return true
		}
	}
	return false
}

// HasWorktree reports whether a worktree path is still registered.
func (r *Repo) HasWorktree(dir string) bool {
	r.t.Helper()
	want := filepath.Clean(dir)
	for _, w := range r.Worktrees() {
		if filepath.Clean(w) == want {
			return true
		}
	}
	return false
}

// LogSubjects lists the subjects reachable from a ref, newest first.
func (r *Repo) LogSubjects(ref string) []string {
	r.t.Helper()
	var out []string
	for _, line := range strings.Split(r.gitOut("log", "--format=%s", ref), "\n") {
		if s := strings.TrimSpace(line); s != "" {
			out = append(out, s)
		}
	}
	return out
}

func (r *Repo) git(args ...string) {
	r.t.Helper()
	r.gitOut(args...)
}

// gitOut runs git in the repository with the ambient git environment stripped,
// so a hook-leaked GIT_DIR can never reach it.
func (r *Repo) gitOut(args ...string) string {
	r.t.Helper()
	cmd := exec.Command("git", append([]string{"-C", r.Dir}, args...)...)
	cmd.Env = cleanGitEnv(os.Environ())
	out, err := cmd.CombinedOutput()
	if err != nil {
		r.t.Fatalf("harness: git %s in %s: %v\n%s", strings.Join(args, " "), r.Dir, err, out)
	}
	return string(out)
}

// gitEnvKeys are the variables that must never be inherited by a git child.
var gitEnvKeys = []string{
	"GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR",
	"GIT_PREFIX", "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
}

func cleanGitEnv(env []string) []string {
	out := make([]string, 0, len(env))
	for _, kv := range env {
		key, _, _ := strings.Cut(kv, "=")
		drop := false
		for _, bad := range gitEnvKeys {
			if key == bad {
				drop = true
				break
			}
		}
		if !drop {
			out = append(out, kv)
		}
	}
	return out
}

// AddWorktree creates a branch off the default branch and checks it out into a
// sibling worktree directory, answering that directory.
func (r *Repo) AddWorktree(name string) string {
	r.t.Helper()
	dir := filepath.Join(filepath.Dir(r.Dir), filepath.Base(r.Dir)+"-"+name)
	r.git("worktree", "add", "-b", name, dir, DefaultBranch)
	return dir
}

// CommitIn writes and commits a file inside one of the repository's worktrees.
func (r *Repo) CommitIn(worktree, file, content string) string {
	r.t.Helper()
	writeFile(r.t, filepath.Join(worktree, file), content)
	sub := &Repo{Dir: worktree, t: r.t}
	sub.git("add", file)
	sub.git("commit", "-m", "add "+file)
	return strings.TrimSpace(sub.gitOut("rev-parse", "HEAD"))
}
