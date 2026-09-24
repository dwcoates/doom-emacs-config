package worktreereap

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// GIT IS NEVER CALLED: every git fact below is a scripted answer of fakeGit.
// The worktree and admin directories are plain temp directories, because the
// reaper reads their existence and their files' mtimes off the filesystem.

// now is every test's instant.
var now = time.Date(2026, 9, 24, 12, 0, 0, 0, time.UTC)

// longAgo is comfortably past the idle threshold.
var longAgo = now.Add(-72 * time.Hour)

// Shas the world scripts.
const (
	baseSHA  = "b000000000000000000000000000000000000001"
	baseTree = "7000000000000000000000000000000000000001"
)

// fakeGit answers the reaper's git from scripted tables, keyed by the
// directory or the head a call names. Every call is recorded in order.
type fakeGit struct {
	mu    sync.Mutex
	calls []string

	defaultBranch    map[string]string
	defaultBranchErr map[string]error
	worktrees        map[string][]gitclient.Worktree
	listErr          map[string]error
	admin            map[string]string
	adminErr         map[string]error
	committed        map[string]time.Time
	dirty            map[string]bool
	cleanErr         map[string]error
	merged           map[string]gitclient.MergeTreeOutcome
	mergeErr         map[string]error
	removeErr        map[string]error
	deleteErr        map[string]error
	pruneErr         error

	// entered and release, when set, hold DefaultBranch until the test
	// releases it: the rendezvous a concurrency test needs.
	entered chan struct{}
	release chan struct{}
}

func newFakeGit() *fakeGit {
	return &fakeGit{
		defaultBranch:    map[string]string{},
		defaultBranchErr: map[string]error{},
		worktrees:        map[string][]gitclient.Worktree{},
		listErr:          map[string]error{},
		admin:            map[string]string{},
		adminErr:         map[string]error{},
		committed:        map[string]time.Time{},
		dirty:            map[string]bool{},
		cleanErr:         map[string]error{},
		merged:           map[string]gitclient.MergeTreeOutcome{},
		mergeErr:         map[string]error{},
		removeErr:        map[string]error{},
		deleteErr:        map[string]error{},
	}
}

func (g *fakeGit) record(format string, args ...any) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, fmt.Sprintf(format, args...))
}

// called reports every recorded call.
func (g *fakeGit) called() []string {
	g.mu.Lock()
	defer g.mu.Unlock()
	return append([]string(nil), g.calls...)
}

// saw reports whether exactly this call was recorded.
func (g *fakeGit) saw(call string) bool {
	for _, c := range g.called() {
		if c == call {
			return true
		}
	}
	return false
}

func (g *fakeGit) DefaultBranch(_ context.Context, repoDir string) (string, error) {
	g.record("default_branch %s", repoDir)
	if g.entered != nil {
		g.entered <- struct{}{}
		<-g.release
	}
	if err := g.defaultBranchErr[repoDir]; err != nil {
		return "", err
	}
	if b, ok := g.defaultBranch[repoDir]; ok {
		return b, nil
	}
	return "main", nil
}

func (g *fakeGit) ResolveRef(_ context.Context, repoDir, ref string) (string, error) {
	g.record("resolve_ref %s %s", repoDir, ref)
	return baseSHA, nil
}

func (g *fakeGit) TreeOf(_ context.Context, dir, ref string) (string, error) {
	g.record("tree_of %s %s", dir, ref)
	return baseTree, nil
}

func (g *fakeGit) ListWorktrees(_ context.Context, repoDir string) ([]gitclient.Worktree, error) {
	g.record("list_worktrees %s", repoDir)
	if err := g.listErr[repoDir]; err != nil {
		return nil, err
	}
	return g.worktrees[repoDir], nil
}

func (g *fakeGit) PruneWorktrees(_ context.Context, repoDir string) error {
	g.record("prune %s", repoDir)
	return g.pruneErr
}

func (g *fakeGit) AdminDir(_ context.Context, worktreeDir string) (string, error) {
	g.record("admin_dir %s", worktreeDir)
	if err := g.adminErr[worktreeDir]; err != nil {
		return "", err
	}
	return g.admin[worktreeDir], nil
}

func (g *fakeGit) CommitterTime(_ context.Context, dir, ref string) (time.Time, error) {
	g.record("committer_time %s", ref)
	at, ok := g.committed[ref]
	if !ok {
		return time.Time{}, fmt.Errorf("fake git: no committer time scripted for %s", ref)
	}
	return at, nil
}

func (g *fakeGit) IsClean(_ context.Context, dir string) (bool, error) {
	g.record("is_clean %s", dir)
	if err := g.cleanErr[dir]; err != nil {
		return false, err
	}
	return !g.dirty[dir], nil
}

func (g *fakeGit) MergeTree(_ context.Context, dir, base, other string) (gitclient.MergeTreeOutcome, error) {
	g.record("merge_tree %s %s %s", dir, base, other)
	if err := g.mergeErr[other]; err != nil {
		return gitclient.MergeTreeOutcome{}, err
	}
	out, ok := g.merged[other]
	if !ok {
		return gitclient.MergeTreeOutcome{}, fmt.Errorf("fake git: no merge-tree scripted for %s", other)
	}
	return out, nil
}

func (g *fakeGit) RemoveCleanWorktree(_ context.Context, repoDir, worktreeDir string) error {
	g.record("remove %s", worktreeDir)
	return g.removeErr[worktreeDir]
}

func (g *fakeGit) DeleteBranchAt(_ context.Context, repoDir, branch, head string) error {
	g.record("delete_branch %s %s", branch, head)
	return g.deleteErr[branch]
}

// fakeRegistry is the state client's two reads.
type fakeRegistry struct {
	repos         []wsm.Repository
	workspaces    []wsm.Workspace
	reposErr      error
	workspacesErr error
}

func (r *fakeRegistry) ListRepositories(context.Context) ([]wsm.Repository, error) {
	return r.repos, r.reposErr
}

func (r *fakeRegistry) ListWorkspaces(context.Context) ([]wsm.Workspace, error) {
	return r.workspaces, r.workspacesErr
}

// fakeClock is a Clock the test drives: Now is fixed, and After hands the
// test each requested wait and a channel it fires by hand.
type fakeClock struct {
	now   time.Time
	asked chan time.Duration
	fire  chan time.Time
}

func newFakeClock() *fakeClock {
	return &fakeClock{now: now, asked: make(chan time.Duration, 16), fire: make(chan time.Time)}
}

func (c *fakeClock) Now() time.Time { return c.now }

func (c *fakeClock) After(d time.Duration) <-chan time.Time {
	c.asked <- d
	return c.fire
}

// world is one test's repositories, worktrees and reaper.
type world struct {
	t        *testing.T
	git      *fakeGit
	registry *fakeRegistry
	clock    *fakeClock
	log      *dlog.TestLogger
	live     []ids.WorkspaceID
	lockPath string
	root     string
}

func newWorld(t *testing.T) *world {
	t.Helper()
	root, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("canonicalizing the temp root: %v", err)
	}
	return &world{
		t:        t,
		git:      newFakeGit(),
		registry: &fakeRegistry{},
		clock:    newFakeClock(),
		log:      dlog.NewTestLogger(),
		lockPath: filepath.Join(root, "locks", "worktree-reap.lock"),
		root:     root,
	}
}

// addRepo registers a repository whose main worktree exists, and lists it as
// its own first worktree.
func (w *world) addRepo(name string) string {
	w.t.Helper()
	dir := w.mkdir(name)
	w.registry.repos = append(w.registry.repos, wsm.Repository{ID: ids.RepoID("repo-" + name), Dir: dir, Name: name})
	w.git.worktrees[dir] = []gitclient.Worktree{{Dir: dir, Head: baseSHA, Branch: "main"}}
	return dir
}

// tree describes one linked worktree to add. The zero value is a clean,
// idle, landed worktree on its own branch.
type tree struct {
	name     string
	branch   string
	detached bool
	head     string
	locked   bool
	prunable bool
	// missing leaves the directory uncreated.
	missing bool
	// touched is the admin files' mtime; zero is longAgo.
	touched time.Time
	// committed is the head's committer time; zero is longAgo.
	committed time.Time
	// outcome is merge-tree's answer; zero is the landed answer.
	outcome *gitclient.MergeTreeOutcome
}

// addTree adds a linked worktree to a repository and answers its directory.
func (w *world) addTree(repoDir string, spec tree) string {
	w.t.Helper()
	dir := filepath.Join(w.root, "trees", spec.name)
	if !spec.missing {
		w.mkdir(filepath.Join("trees", spec.name))
	}
	head := spec.head
	if head == "" {
		head = fmt.Sprintf("%040x", len(w.git.committed)+100)
	}
	branch := spec.branch
	if branch == "" && !spec.detached {
		branch = "feat/" + spec.name
	}
	w.git.worktrees[repoDir] = append(w.git.worktrees[repoDir], gitclient.Worktree{
		Dir: dir, Head: head, Branch: branch, Detached: spec.detached,
		Locked: spec.locked, Prunable: spec.prunable,
	})

	admin := w.mkdir(filepath.Join("admin", spec.name))
	touched := spec.touched
	if touched.IsZero() {
		touched = longAgo
	}
	w.writeAged(filepath.Join(admin, "HEAD"), touched)
	w.writeAged(filepath.Join(admin, "index"), touched)
	w.writeAged(filepath.Join(admin, "logs", "HEAD"), touched)
	w.git.admin[dir] = admin

	committed := spec.committed
	if committed.IsZero() {
		committed = longAgo
	}
	w.git.committed[head] = committed
	outcome := gitclient.MergeTreeOutcome{Tree: baseTree}
	if spec.outcome != nil {
		outcome = *spec.outcome
	}
	w.git.merged[head] = outcome
	return dir
}

// headOf answers the scripted head of a worktree.
func (w *world) headOf(repoDir, dir string) string {
	w.t.Helper()
	for _, wt := range w.git.worktrees[repoDir] {
		if wt.Dir == dir {
			return wt.Head
		}
	}
	w.t.Fatalf("no worktree %s in %s", dir, repoDir)
	return ""
}

// register records a workspace for a worktree.
func (w *world) register(repoDir, dir string, ws wsm.Workspace) wsm.Workspace {
	w.t.Helper()
	ws.Dir = dir
	if ws.ID == "" {
		ws.ID = ids.WorkspaceID(fmt.Sprintf("ws%014d", len(w.registry.workspaces)))
	}
	for _, repo := range w.registry.repos {
		if repo.Dir == repoDir {
			ws.Repo = repo.ID
		}
	}
	if ws.CreatedAt.IsZero() {
		ws.CreatedAt = longAgo
	}
	w.registry.workspaces = append(w.registry.workspaces, ws)
	return ws
}

func (w *world) mkdir(rel string) string {
	w.t.Helper()
	dir := filepath.Join(w.root, rel)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		w.t.Fatalf("mkdir %s: %v", dir, err)
	}
	return dir
}

func (w *world) writeAged(path string, at time.Time) {
	w.t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		w.t.Fatalf("mkdir %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte("x\n"), 0o644); err != nil {
		w.t.Fatalf("write %s: %v", path, err)
	}
	if err := os.Chtimes(path, at, at); err != nil {
		w.t.Fatalf("chtimes %s: %v", path, err)
	}
}

// reaper builds the reaper over the world.
func (w *world) reaper() *Reaper {
	w.t.Helper()
	r, err := New(w.deps())
	if err != nil {
		w.t.Fatalf("New: %v", err)
	}
	return r
}

func (w *world) deps() Deps {
	return Deps{
		Git:          w.git,
		Registry:     w.registry,
		LiveSessions: func() []ids.WorkspaceID { return w.live },
		Clock:        w.clock,
		LockPath:     w.lockPath,
		IdleAfter:    DefaultIdleAfter,
		StartDelay:   DefaultStartDelay,
		Every:        DefaultEvery,
		Log:          w.log,
	}
}

// sweep runs one sweep, failing on an error.
func (w *world) sweep() Report {
	w.t.Helper()
	report, err := w.reaper().Sweep(context.Background())
	if err != nil {
		w.t.Fatalf("Sweep: %v", err)
	}
	return report
}

// records answers every record at a level under an operation.
func (w *world) records(level, operation string) []dlog.Record {
	var out []dlog.Record
	for _, r := range w.log.Records() {
		if r.Level == level && r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
}

// assertNoErrors fails on any ERROR or WARN record.
func (w *world) assertNoWarnings() {
	w.t.Helper()
	for _, r := range w.log.Records() {
		if r.Level == "error" || r.Level == "warn" {
			w.t.Fatalf("unexpected %s record: %+v", r.Level, r)
		}
	}
}

// keptFor answers the reason a worktree's keep record gave.
func (w *world) keptFor(dir string) string {
	w.t.Helper()
	for _, r := range w.records("debug", opKeep) {
		if r.Context["worktree"] == dir {
			reason, _ := r.Context["reason"].(string)
			return reason
		}
	}
	return ""
}

var errScripted = errors.New("scripted failure")
