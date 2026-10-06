package harness

import (
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"claude-repld/integration/fakegit"
)

// GIT IS NEVER CALLED DURING TESTING. A `Repo` here is a FAKE repository: a
// directory tree with a `.git` marker and a row in the fixture file that the
// scripted `git` on the daemon's PATH answers from. Nothing runs `git init`,
// nothing spawns the real binary, and every git fact the daemon reads —
// default branch, commits, worktree list, conflicts, landed ranges, changed
// paths, cleanliness — is fixture data this file writes.

// GitWorld is one test's fake git universe: the fixture file every scripted
// `git` invocation reads, plus the directory the repositories live under.
type GitWorld struct {
	// StateFile is the fixture file, named to children by FAKEGIT_STATE.
	StateFile string

	root string
	t    *testing.T
}

// worlds holds one world per test, so a repository can be built before or
// after the daemon that will read it.
var (
	worldsMu sync.Mutex
	worlds   = map[*testing.T]*GitWorld{}
)

// World answers this test's git world, minting it on first use.
func World(t *testing.T) *GitWorld {
	t.Helper()
	worldsMu.Lock()
	defer worldsMu.Unlock()
	if w, ok := worlds[t]; ok {
		return w
	}
	// The root is CANONICAL: on macOS t.TempDir() hands back a path under the
	// /tmp symlink, and a spawned child's own view of its cwd is the resolved
	// /private/tmp one. A fixture path a test compares a process's answer
	// against has to be the resolved form, or every such comparison fails on
	// the symlink alone.
	root := fakegit.Canon(t.TempDir())
	w := &GitWorld{StateFile: filepath.Join(root, "fakegit.json"), root: filepath.Join(root, "repos"), t: t}
	if err := os.MkdirAll(w.root, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", w.root, err)
	}
	worlds[t] = w
	t.Cleanup(func() {
		worldsMu.Lock()
		delete(worlds, t)
		worldsMu.Unlock()
	})
	return w
}

// DefaultBranch is the branch every fake repository starts on.
const DefaultBranch = "main"

// Repo is one fake repository's main worktree.
type Repo struct {
	// Dir is the main worktree.
	Dir string

	w *GitWorld
	t *testing.T
}

// NewRepo mints a fake repository under the test's own tree.
func NewRepo(t *testing.T) *Repo {
	t.Helper()
	w := World(t)
	return NewRepoAt(t, filepath.Join(w.root, "repo-"+mintName(w)))
}

// NewRepoAt mints a fake repository at an exact path, for the tests that care
// where the workspace lives (the multi-repo root, the daemon's self repo).
func NewRepoAt(t *testing.T, dir string) *Repo {
	t.Helper()
	w := World(t)
	if err := os.MkdirAll(filepath.Join(dir, ".git"), 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	r := &Repo{Dir: dir, w: w, t: t}
	r.edit(func(s *fakegit.State) {
		if s.Repo(dir) != nil {
			t.Fatalf("harness: %s is already a fake repository", dir)
		}
		repo := &fakegit.Repo{
			Dir:           dir,
			CommonDir:     filepath.Join(fakegit.Canon(dir), ".git"),
			DefaultBranch: DefaultBranch,
			BranchHeads:   map[string]string{},
		}
		repo.Worktrees = []*fakegit.Worktree{{Dir: dir, Branch: DefaultBranch}}
		repo.AddBranch(DefaultBranch, "")
		s.Repos = append(s.Repos, repo)
		c := s.AddCommit(repo, DefaultBranch, "add README.md", nil, []string{"README.md"})
		repo.Worktrees[0].Head = c.SHA
	})
	writeFile(t, filepath.Join(dir, "README.md"), "fake repository\n")
	// EVERY REPOSITORY STATES ITS OWN ONE-SHOT POLICY. A repository that
	// states none refuses a one-shot create outright (the owner's 2026-09-12
	// ruling), so a fixture repository carries a copy of the shipped corpus
	// as its policy exactly as a real repository's author would write one.
	CopyPrompts(t, r.PolicyDir())
	return r
}

// PolicyDir is the repository's own `.agent-repl/prompts`, where it states its
// one-shot and merge policy.
func (r *Repo) PolicyDir() string {
	return filepath.Join(r.Dir, ".agent-repl", "prompts")
}

// mintName answers a fresh repository directory name.
func mintName(w *GitWorld) string {
	entries, err := os.ReadDir(w.root)
	if err != nil {
		return "0"
	}
	return string(rune('a' + len(entries)))
}

// edit mutates the fixture file under the same lock the scripted `git` takes.
func (r *Repo) edit(fn func(*fakegit.State)) {
	r.t.Helper()
	r.w.edit(fn)
}

func (w *GitWorld) edit(fn func(*fakegit.State)) {
	w.t.Helper()
	if err := fakegit.WithLock(w.StateFile, func(s *fakegit.State) error {
		fn(s)
		return nil
	}); err != nil {
		w.t.Fatalf("harness: fakegit state: %v", err)
	}
}

// read answers a copy of the fixture file.
func (w *GitWorld) read() *fakegit.State {
	w.t.Helper()
	s, err := fakegit.LoadLocked(w.StateFile)
	if err != nil {
		w.t.Fatalf("harness: fakegit state: %v", err)
	}
	return s
}

// Calls lists every git invocation the daemon made, oldest first, so a test
// can assert on the conversation rather than only on its effects.
func (w *GitWorld) Calls() []fakegit.Call { return w.read().Calls }

// state answers this repository's fixture row.
func (r *Repo) state(s *fakegit.State) *fakegit.Repo {
	repo := s.Repo(r.Dir)
	if repo == nil {
		r.t.Fatalf("harness: %s is not a fake repository", r.Dir)
	}
	return repo
}

// CommitIn records a commit inside one of the repository's worktrees.
func (r *Repo) CommitIn(worktree, file, content string) string {
	r.t.Helper()
	writeFile(r.t, filepath.Join(worktree, file), content)
	var sha string
	r.edit(func(s *fakegit.State) {
		repo := r.state(s)
		wt := repo.Worktree(worktree)
		if wt == nil {
			r.t.Fatalf("harness: %s is not a worktree of %s", worktree, r.Dir)
		}
		c := s.AddCommit(repo, wt.Branch, "add "+file, []string{wt.Head}, []string{file})
		sha = c.SHA
	})
	return sha
}

// CommitWork records one commit of WORK on the branch checked out in the
// worktree at dir, as the workspace's agent would have made it, and answers
// its sha. A branch with nothing on it is already on its target, so its merge
// concludes at once with nothing to land (merge: "already on <target>"); a
// test that means to exercise a merge's phases commits work first. paths are
// what the commit touches, `work.txt` when none are named.
func CommitWork(t *testing.T, dir string, paths ...string) string {
	t.Helper()
	if len(paths) == 0 {
		paths = []string{"work.txt"}
	}
	for _, path := range paths {
		writeFile(t, filepath.Join(dir, path), "work\n")
	}
	var sha string
	World(t).edit(func(s *fakegit.State) {
		repo, wt := s.FindWorktree(dir)
		if repo == nil || wt == nil || wt.Branch == "" {
			t.Fatalf("harness: %s is no worktree with a branch checked out", dir)
		}
		sha = s.AddCommit(repo, wt.Branch, "the workspace's work", []string{wt.Head}, paths).SHA
	})
	return sha
}

// Branch creates a branch off the current head and checks it out in the main
// worktree.
func (r *Repo) Branch(name string) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		repo := r.state(s)
		wt := repo.Worktree(r.Dir)
		repo.AddBranch(name, wt.Head)
		wt.Branch = name
	})
}

// Checkout switches the main worktree to an existing branch.
func (r *Repo) Checkout(name string) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		repo := r.state(s)
		if !repo.HasBranch(name) {
			r.t.Fatalf("harness: %s has no branch %s", r.Dir, name)
		}
		wt := repo.Worktree(r.Dir)
		wt.Branch = name
		wt.Head = repo.BranchHeads[name]
	})
}

// Worktrees lists every worktree path the repository knows, main included.
func (r *Repo) Worktrees() []string {
	r.t.Helper()
	var out []string
	for _, wt := range r.state(r.w.read()).Worktrees {
		out = append(out, wt.Dir)
	}
	return out
}

// Branches lists every local branch.
func (r *Repo) Branches() []string {
	r.t.Helper()
	return r.state(r.w.read()).Branches
}

// HasBranch reports whether a local branch exists.
func (r *Repo) HasBranch(name string) bool {
	r.t.Helper()
	return r.state(r.w.read()).HasBranch(name)
}

// HasWorktree reports whether a worktree path is still registered.
func (r *Repo) HasWorktree(dir string) bool {
	r.t.Helper()
	return r.state(r.w.read()).Worktree(dir) != nil
}

// AddWorktree cuts a branch off the default branch into a sibling worktree
// directory, exactly as the scripted `git worktree add` would, and answers it.
func (r *Repo) AddWorktree(name string) string {
	r.t.Helper()
	dir := filepath.Join(filepath.Dir(r.Dir), filepath.Base(r.Dir)+"-"+name)
	r.edit(func(s *fakegit.State) {
		res := fakegit.Run(s, r.Dir, []string{"-C", r.Dir, "worktree", "add", "-b", name, dir, DefaultBranch})
		if res.Exit != 0 {
			r.t.Fatalf("harness: fake `worktree add %s`: %s", name, res.Stderr)
		}
	})
	return dir
}

// ScriptConflict makes the NEXT `merge --no-ff` of branch inside worktreeDir
// leave these paths conflicted instead of landing.
func (r *Repo) ScriptConflict(worktreeDir, branch string, paths ...string) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		s.Conflicts = append(s.Conflicts, &fakegit.Conflict{Dir: worktreeDir, Branch: branch, Paths: paths})
	})
}

// ScriptRebaseConflict makes replaying branch's commit at the 1-based place
// commit stop on these paths, the first time the merge queue's rebase meets it.
func (r *Repo) ScriptRebaseConflict(branch string, commit int, paths ...string) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		s.Conflicts = append(s.Conflicts, &fakegit.Conflict{Dir: r.Dir, Branch: branch, Paths: paths, RebaseCommit: commit})
	})
}

// ResolveConflicts clears the conflicted paths in one worktree, as an agent
// that resolved them and `git add`ed them leaves it.
func (r *Repo) ResolveConflicts(worktreeDir string) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		wt := r.state(s).Worktree(worktreeDir)
		if wt == nil {
			r.t.Fatalf("harness: %s is not a worktree of %s", worktreeDir, r.Dir)
		}
		wt.Conflicted = nil
	})
}

// RebaseInProgress reports whether a rebase stands in one worktree.
func (r *Repo) RebaseInProgress(worktreeDir string) bool {
	r.t.Helper()
	wt := r.state(r.w.read()).Worktree(worktreeDir)
	return wt != nil && wt.Rebase != nil
}

// BranchHead answers the commit a branch points at.
func (r *Repo) BranchHead(branch string) string {
	r.t.Helper()
	return r.state(r.w.read()).BranchHeads[branch]
}

// SetUpstream points origin's branch at a new commit on top of the local
// one, as a pull request merged upstream leaves it, and answers that commit.
func (r *Repo) SetUpstream(branch string) string {
	r.t.Helper()
	var sha string
	r.edit(func(s *fakegit.State) {
		repo := r.state(s)
		c := s.AddCommit(repo, "", "merged upstream", []string{repo.BranchHeads[branch]}, []string{"upstream.txt"})
		if repo.RemoteHeads == nil {
			repo.RemoteHeads = map[string]string{}
		}
		repo.RemoteHeads[branch] = c.SHA
		sha = c.SHA
	})
	return sha
}

// AddBranchWorktree adds a branch cut from the default branch, carrying one
// commit of work, checked out in a sibling worktree, and answers the worktree.
func (r *Repo) AddBranchWorktree(name string) string {
	r.t.Helper()
	dir := r.AddWorktree(name)
	CommitWork(r.t, dir)
	return dir
}

// AddBranch adds a branch cut from the default branch carrying one commit of
// work, checked out NOWHERE, and answers its head.
func (r *Repo) AddBranch(name string) string {
	r.t.Helper()
	var sha string
	r.edit(func(s *fakegit.State) {
		repo := r.state(s)
		base := repo.BranchHeads[DefaultBranch]
		repo.AddBranch(name, base)
		sha = s.AddCommit(repo, name, "the branch's work", []string{base}, []string{"branch.txt"}).SHA
	})
	return sha
}

// RemoveBranch deletes a branch from the repository, as a branch deleted
// after it was landed outside the daemon's own merge flow is.
func (r *Repo) RemoveBranch(name string) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		r.state(s).RemoveBranch(name)
	})
}

// ScriptFailure makes the NEXT git command matching this prefix fail.
func (r *Repo) ScriptFailure(dir string, exit int, stderr string, match ...string) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		s.Failures = append(s.Failures, &fakegit.Failure{Dir: dir, Match: match, Stderr: stderr, Exit: exit})
	})
}

// SetPaths records the paths a commit touched, which is what the rollout
// controller classifies subsystems from.
func (r *Repo) SetPaths(sha string, paths ...string) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		c := s.Commits[sha]
		if c == nil {
			r.t.Fatalf("harness: %s is not a commit in the fake world", sha)
		}
		c.Paths = paths
	})
}

// SetDirty scripts a worktree's cleanliness, which is what `status --porcelain`
// answers from. An unclean merge target is the state a restart refuses to
// resume a merge into.
func (r *Repo) SetDirty(worktreeDir string, dirty bool) {
	r.t.Helper()
	r.edit(func(s *fakegit.State) {
		wt := r.state(s).Worktree(worktreeDir)
		if wt == nil {
			r.t.Fatalf("harness: %s is not a worktree of %s", worktreeDir, r.Dir)
		}
		wt.Dirty = dirty
	})
}

// IdleWorktree makes a linked worktree look untouched since at, as the
// landed-worktree reaper reads it: the worktree's admin files (HEAD, index and
// the HEAD reflog, in the directory the scripted `rev-parse --absolute-git-dir`
// names) are written with that mtime. The fixture's commits are already years
// old, so their committer times say nothing newer.
func (r *Repo) IdleWorktree(dir string, at time.Time) {
	r.t.Helper()
	admin, ok := r.w.read().GitDirOf(dir)
	if !ok {
		r.t.Fatalf("harness: %s is not a fake worktree", dir)
	}
	for _, name := range []string{"HEAD", "index", filepath.Join("logs", "HEAD")} {
		path := filepath.Join(admin, name)
		writeFile(r.t, path, "fake admin file\n")
		if err := os.Chtimes(path, at, at); err != nil {
			r.t.Fatalf("harness: chtimes %s: %v", path, err)
		}
	}
}
