package merge

import (
	"agent-repld/internal/wsm"
	"context"
	"fmt"
	"slices"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/scriptrunner"
)

// This file tests the chain: chained rebasing, restacks, speculative gates and
// their governor, landing order, and a restart mid-chain. GIT IS NEVER CALLED:
// chainGit is an in-memory model of commits, branches and an interactive
// rebase, whose trees are the sets of patches a commit carries, so "the tree
// the gate passed" and "master after A landed plus B's commits" are facts a
// test can read.

// chainCommit is one modelled commit: its parents, the patch it carries (none
// for a merge commit) and its tree, the set of every patch it holds.
type chainCommit struct {
	parents []string
	patch   string
	tree    map[string]bool
	order   int
}

// chainRebase is one interactive rebase standing in a worktree.
type chainRebase struct {
	branch string
	head   string
	todo   []string
	// stopped is the conflict the current commit stopped on, resolved once
	// the rebase continues; rerere reports a recorded resolution reused.
	stopped *[2]string
	rerere  bool
}

// chainGit models the repository every chain test's merges share.
type chainGit struct {
	*fakeGit
	cmu       sync.Mutex
	commits   map[string]*chainCommit
	refs      map[string]string
	rebases   map[string]*chainRebase
	conflicts map[[2]string][]string
	recorded  map[[2]string]bool
	trees     map[string]string
	n         int
	landings  []string
	// rebaseHolds parks the next rebase started in a worktree; a stubborn
	// one ignores its caller's cancellation, as a git command mid-flight does.
	rebaseHolds holdSet
	stubborn    map[string]bool
	// cleanHolds parks the next cleanliness read in a worktree, ignoring its
	// caller's cancellation as a git command mid-flight does.
	cleanHolds holdSet
}

func newChainGit(inner *fakeGit) *chainGit {
	g := &chainGit{
		fakeGit: inner, commits: map[string]*chainCommit{}, refs: map[string]string{},
		rebases: map[string]*chainRebase{}, conflicts: map[[2]string][]string{}, recorded: map[[2]string]bool{},
		trees: map[string]string{}, stubborn: map[string]bool{},
	}
	root := g.mintLocked(nil, "base")
	g.refs["master"] = root
	return g
}

// mintLocked makes a commit on parents carrying patch.
func (g *chainGit) mintLocked(parents []string, patch string) string {
	g.n++
	sha := fmt.Sprintf("c%03d-%s", g.n, patch)
	tree := map[string]bool{}
	for _, p := range parents {
		for k := range g.commits[p].tree {
			tree[k] = true
		}
	}
	if patch != "" {
		tree[patch] = true
	}
	g.commits[sha] = &chainCommit{parents: parents, patch: patch, tree: tree, order: g.n}
	return sha
}

// branch forks a branch from master's first commit with one commit per patch.
func (g *chainGit) branch(name string, patches ...string) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	head := "c001-base"
	for _, p := range patches {
		head = g.mintLocked([]string{head}, p)
	}
	g.refs[name] = head
}

// commitOn adds one commit to a branch, as a session's repair does.
func (g *chainGit) commitOn(name, patch string) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	g.refs[name] = g.mintLocked([]string{g.refs[name]}, patch)
}

// conflict scripts replaying patch onto a base holding onto's patch as a
// conflict in files.
func (g *chainGit) conflict(patch, onto string, files ...string) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	g.conflicts[[2]string{patch, onto}] = files
}

func (g *chainGit) dirBranch(dir string) string {
	g.fakeGit.mu.Lock()
	defer g.fakeGit.mu.Unlock()
	if b, ok := g.fakeGit.branches[dir]; ok {
		return b
	}
	return "master"
}

func (g *chainGit) resolveLocked(dir, ref string) (string, error) {
	if ref == "HEAD" {
		if rb := g.rebases[dir]; rb != nil {
			return rb.head, nil
		}
		ref = g.dirBranch(dir)
	}
	ref = strings.TrimPrefix(ref, "refs/heads/")
	if sha, ok := g.refs[ref]; ok {
		return sha, nil
	}
	if _, ok := g.commits[ref]; ok {
		return ref, nil
	}
	return "", fmt.Errorf("chain git: unknown ref %q in %s", ref, dir)
}

func (g *chainGit) reachableLocked(sha string) map[string]bool {
	seen := map[string]bool{}
	stack := []string{sha}
	for len(stack) > 0 {
		c := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if seen[c] {
			continue
		}
		seen[c] = true
		stack = append(stack, g.commits[c].parents...)
	}
	return seen
}

func (g *chainGit) treeLocked(sha string) string {
	var patches []string
	for p := range g.commits[sha].tree {
		patches = append(patches, p)
	}
	sort.Strings(patches)
	return strings.Join(patches, "+")
}

// tree is the patch set a ref holds.
func (g *chainGit) tree(ref string) string {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	sha, err := g.resolveLocked("", ref)
	if err != nil {
		return "unknown:" + ref
	}
	return g.treeLocked(sha)
}

// ref answers a branch's tip.
func (g *chainGit) ref(name string) string {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	return g.refs[name]
}

// own answers the patches a branch carries that base does not.
func (g *chainGit) own(base, name string) []string {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	commits, _ := g.betweenLocked(base, g.refs[name])
	var out []string
	for _, c := range commits {
		out = append(out, g.commits[c.SHA].patch)
	}
	return out
}

func (g *chainGit) betweenLocked(base, tip string) ([]gitclient.Commit, error) {
	ofBase := g.reachableLocked(base)
	var out []gitclient.Commit
	for sha := range g.reachableLocked(tip) {
		if ofBase[sha] || len(g.commits[sha].parents) > 1 {
			continue
		}
		out = append(out, gitclient.Commit{SHA: sha, Subject: g.commits[sha].patch})
	}
	sort.Slice(out, func(i, j int) bool { return g.commits[out[i].SHA].order < g.commits[out[j].SHA].order })
	return out, nil
}

func (g *chainGit) ResolveRef(_ context.Context, dir, ref string) (string, error) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	return g.resolveLocked(dir, ref)
}

func (g *chainGit) IsAncestor(_ context.Context, dir, ancestor, descendant string) (bool, error) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	a, err := g.resolveLocked(dir, ancestor)
	if err != nil {
		return false, err
	}
	d, err := g.resolveLocked(dir, descendant)
	if err != nil {
		return false, err
	}
	return g.reachableLocked(d)[a], nil
}

func (g *chainGit) CommitsBetween(_ context.Context, dir, base, tip string) ([]gitclient.Commit, error) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	b, err := g.resolveLocked(dir, base)
	if err != nil {
		return nil, err
	}
	t, err := g.resolveLocked(dir, tip)
	if err != nil {
		return nil, err
	}
	return g.betweenLocked(b, t)
}

func (g *chainGit) TreeOf(_ context.Context, dir, ref string) (string, error) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	sha, err := g.resolveLocked(dir, ref)
	if err != nil {
		return "", err
	}
	return g.treeLocked(sha), nil
}

// holdRebase parks the next rebase started in dir until release closes;
// entered closes once it is parked.
func (g *chainGit) holdRebase(dir string) (entered, release chan struct{}) {
	return g.rebaseHolds.arm(dir)
}

// holdRebaseStubbornly is holdRebase for a rebase that ignores its caller's
// cancellation until release closes.
func (g *chainGit) holdRebaseStubbornly(dir string) (entered, release chan struct{}) {
	entered, release = g.holdRebase(dir)
	g.cmu.Lock()
	g.stubborn[dir] = true
	g.cmu.Unlock()
	return entered, release
}

// holdClean parks the next cleanliness read in dir until release closes;
// entered closes once it is parked. A rebase reads it first, so a held read
// stands the run after it read the target's tip and before its rebase began.
func (g *chainGit) holdClean(dir string) (entered, release chan struct{}) {
	return g.cleanHolds.arm(dir)
}

func (g *chainGit) IsClean(ctx context.Context, dir string) (bool, error) {
	if hold, held := g.cleanHolds.claim(dir); held {
		hold.parkUncancelled()
	}
	return g.fakeGit.IsClean(ctx, dir)
}

func (g *chainGit) StartRebase(ctx context.Context, dir, onto string, commits []string) (gitclient.RebaseStep, error) {
	hold, held := g.rebaseHolds.claim(dir)
	g.cmu.Lock()
	stubborn := g.stubborn[dir]
	delete(g.stubborn, dir)
	g.cmu.Unlock()
	if held && stubborn {
		hold.parkUncancelled()
	} else if held && !hold.park(ctx) {
		return gitclient.RebaseStep{}, fmt.Errorf("chain rebase: %w", ctx.Err())
	}
	g.cmu.Lock()
	defer g.cmu.Unlock()
	base, err := g.resolveLocked(dir, onto)
	if err != nil {
		return gitclient.RebaseStep{}, err
	}
	g.rebases[dir] = &chainRebase{branch: g.dirBranch(dir), head: base, todo: append([]string(nil), commits...)}
	return g.replayLocked(dir), nil
}

func (g *chainGit) AbortRebase(_ context.Context, dir string) error {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	if g.rebases[dir] == nil {
		return fmt.Errorf("chain git: no rebase stands in %s", dir)
	}
	delete(g.rebases, dir)
	return nil
}

func (g *chainGit) ContinueRebase(_ context.Context, dir string) (gitclient.RebaseStep, error) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	if g.rebases[dir] == nil {
		return gitclient.RebaseStep{}, fmt.Errorf("chain git: no rebase stands in %s", dir)
	}
	return g.replayLocked(dir), nil
}

// replayLocked replays the next commit: it stops on a scripted conflict the
// first time it meets one, answers a recorded one as rerere-resolved, and
// records a resolution as the rebase continues past it.
func (g *chainGit) replayLocked(dir string) gitclient.RebaseStep {
	rb := g.rebases[dir]
	patch := g.commits[rb.todo[0]].patch
	if rb.stopped == nil && !rb.rerere {
		for held := range g.commits[rb.head].tree {
			key := [2]string{patch, held}
			files, scripted := g.conflicts[key]
			if !scripted {
				continue
			}
			if g.recorded[key] {
				rb.rerere = true
				return gitclient.RebaseStep{Resolved: files}
			}
			rb.stopped = &key
			return gitclient.RebaseStep{Conflicted: files}
		}
	}
	if rb.stopped != nil {
		g.recorded[*rb.stopped] = true
	}
	rb.stopped, rb.rerere = nil, false
	rb.head = g.mintLocked([]string{rb.head}, patch)
	rb.todo = rb.todo[1:]
	if len(rb.todo) == 0 {
		g.refs[rb.branch] = rb.head
		delete(g.rebases, dir)
		return gitclient.RebaseStep{Done: true}
	}
	return gitclient.RebaseStep{}
}

func (g *chainGit) RebaseInProgress(_ context.Context, dir string) (bool, error) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	return g.rebases[dir] != nil, nil
}

func (g *chainGit) ConflictedFiles(context.Context, string) ([]string, error) { return nil, nil }

func (g *chainGit) AddDetachedWorktree(ctx context.Context, repoDir, worktreeDir, commit string) error {
	if err := g.fakeGit.AddDetachedWorktree(ctx, repoDir, worktreeDir, commit); err != nil {
		return err
	}
	g.cmu.Lock()
	defer g.cmu.Unlock()
	sha, err := g.resolveLocked(worktreeDir, commit)
	if err != nil {
		return err
	}
	g.trees[worktreeDir] = sha
	return nil
}

func (g *chainGit) MergeNoFF(_ context.Context, dir, source, _ string) (gitclient.MergeOutcome, error) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	head, err := g.resolveLocked(dir, source)
	if err != nil {
		return gitclient.MergeOutcome{}, err
	}
	merge := g.mintLocked([]string{g.trees[dir], head}, "")
	return gitclient.MergeOutcome{Landed: &gitclient.Commit{SHA: merge}}, nil
}

func (g *chainGit) FastForward(_ context.Context, _, commit string) error {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	if !g.reachableLocked(commit)[g.refs["master"]] {
		return fmt.Errorf("chain git: %s is no fast-forward of master", commit)
	}
	g.refs["master"] = commit
	g.landings = append(g.landings, commit)
	return nil
}

func (g *chainGit) LandedRange(_ context.Context, _, merge string) ([]gitclient.Commit, error) {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	parents := g.commits[merge].parents
	return g.betweenLocked(parents[0], parents[1])
}

// landedOwn answers the patches each landing brought onto master, in order.
func (g *chainGit) landedOwn() [][]string {
	g.cmu.Lock()
	defer g.cmu.Unlock()
	var out [][]string
	for _, merge := range g.landings {
		parents := g.commits[merge].parents
		commits, _ := g.betweenLocked(parents[0], parents[1])
		var patches []string
		for _, c := range commits {
			patches = append(patches, g.commits[c.SHA].patch)
		}
		out = append(out, patches)
	}
	return out
}

// chainRunner is the gate's script per worktree: a run passes unless
// scripted, and a run can be held inside its gate.
type chainRunner struct {
	mu      sync.Mutex
	results map[string][]scriptedRun
	holds   holdSet
	calls   map[string]int
	// running counts the runs under way now, and most the most that ever
	// were at once: one repository's gates never overlap (slot.go).
	running, most int
}

func newChainRunner() *chainRunner {
	return &chainRunner{results: map[string][]scriptedRun{}, calls: map[string]int{}}
}

// hold parks the next gate run in dir until release closes.
func (c *chainRunner) hold(dir string) (entered, release chan struct{}) {
	return c.holds.arm(dir)
}

// script queues the next run's answer in dir.
func (c *chainRunner) script(dir string, run scriptedRun) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.results[dir] = append(c.results[dir], run)
}

// overlapped answers the most runs ever under way at once.
func (c *chainRunner) overlapped() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.most
}

func (c *chainRunner) ran(dir string) int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.calls[dir]
}

func (c *chainRunner) RunLinesMeasured(ctx context.Context, dir string, argv []string, onLine func(string)) (scriptrunner.Run, error) {
	c.mu.Lock()
	c.calls[dir]++
	c.running++
	c.most = max(c.most, c.running)
	defer func() {
		c.mu.Lock()
		c.running--
		c.mu.Unlock()
	}()
	c.mu.Unlock()
	if hold, held := c.holds.claim(dir); held && !hold.park(ctx) {
		return scriptrunner.Run{}, fmt.Errorf("chain script: %w", ctx.Err())
	}
	c.mu.Lock()
	var run scriptedRun
	if rs := c.results[dir]; len(rs) > 0 {
		run = rs[0]
		c.results[dir] = rs[1:]
	}
	c.mu.Unlock()
	if onLine != nil && run.Output != "" {
		for _, line := range strings.Split(strings.TrimSuffix(run.Output, "\n"), "\n") {
			onLine(line)
		}
	}
	return scriptrunner.Run{Output: run.Output, ExitCode: run.Code}, run.Err
}

// chain is one chain test's world.
type chain struct {
	h      *harness
	git    *chainGit
	runner *chainRunner
	idle   chan ids.WorkspaceID
	// kicks latches a kick of the queue, which pumps nothing in the harness
	// (kick is a no-op without o.async): drain wakes on it as production's
	// pump is woken. One pending kick is all a wake needs, so a kick that
	// finds one pending is dropped rather than blocking the orchestrator.
	kicks    chan wsm.RepoKey
	dirs     map[ids.WorkspaceID]string
	branches map[ids.WorkspaceID]string
}

// newChain builds a harness of several merges over the modelled repository,
// with the harness's merge (ws-1, branch feature) carrying patch a1.
func newChain(t *testing.T) *chain {
	t.Helper()
	h := newHarness(t)
	h.emacsRepo()
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	c := &chain{h: h, git: newChainGit(h.git), runner: newChainRunner(),
		idle: make(chan ids.WorkspaceID, 1024), kicks: make(chan wsm.RepoKey, 1),
		dirs: map[ids.WorkspaceID]string{theWorkspace: h.sourceD}, branches: map[ids.WorkspaceID]string{theWorkspace: "feature"}}
	c.wire(h.o)
	c.git.branch("feature", "a1")
	return c
}

// wire points an orchestrator at the chain's world.
func (c *chain) wire(o *orchestrator) {
	o.deps.Git = c.git
	o.deps.TestRunner = c.runner
	o.onChainIdle = func(ws ids.WorkspaceID) { c.idle <- ws }
	o.onKick = func(repo wsm.RepoKey) {
		select {
		case c.kicks <- repo:
		default:
		}
	}
}

// queue puts another workspace's merge in line, its branch carrying patches.
func (c *chain) queue(t *testing.T, ws ids.WorkspaceID, patches ...string) {
	t.Helper()
	branch := "b-" + string(ws)
	c.branches[ws] = branch
	c.git.branch(branch, patches...)
	queueAnother(t, c.h, ws, string(ws))
	c.h.db.mu.Lock()
	c.dirs[ws] = c.h.db.workspaces[ws].Dir
	c.h.db.mu.Unlock()
}

// awaitIdle waits for ws to go idle with until true of the world.
func (c *chain) awaitIdle(t *testing.T, ws ids.WorkspaceID, until func() bool) {
	t.Helper()
	receiveUntil(t, c.idle, string(ws)+" going idle in the awaited state",
		func(got ids.WorkspaceID) bool { return got == ws && until() })
}

// onTip reports whether ws's branch stands on the tip of the branch of ahead.
func (c *chain) onTip(ws, ahead ids.WorkspaceID) func() bool {
	return func() bool {
		ok, _ := c.git.IsAncestor(context.Background(), "", c.git.ref(c.branches[ahead]), c.branches[ws])
		return ok
	}
}

// promote runs the pump once more: the merge behind the landed head is
// promoted and runs to its end.
func (c *chain) promote(t *testing.T) {
	t.Helper()
	if err := c.h.admit(context.Background()); err != nil {
		t.Fatalf("the promoted merge ended on: %v", err)
	}
}

// drain pumps until the queue is empty. Under the race to land (race.go) how
// many passes that takes depends on which merge wins each round, and a pass
// can find the queue's front still STARTING in its chain: its start runs on a
// goroutine of its own, and the harness's pump runs only when a test calls it
// (kick is a no-op without o.async), where production's pump is woken by that
// very admission. A pass that changes nothing therefore waits for a run to
// go idle -- the started merge, admitted and waiting to be promoted -- before
// pumping again, instead of spinning past the start (the "outlived its test"
// flake under load: the start's goroutine had not been scheduled yet). It also
// wakes on every kick, the edge that pumps production's queue: a pass that
// found a waiting member still in its background rebase (rebasewaiting.go)
// skipped it, and only that rebase's settling kick says it may be admitted
// now (2026-10-13: ws-d stood queued until the drain's bound, its settling
// kick lost).
func (c *chain) drain(t *testing.T) {
	t.Helper()
	bound := time.NewTimer(10 * time.Second)
	defer bound.Stop()
	for {
		queue, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
		if err != nil {
			t.Fatalf("reading the queue: %v", err)
		}
		if len(queue) == 0 {
			return
		}
		c.promote(t)
		after, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
		if err != nil {
			t.Fatalf("reading the queue: %v", err)
		}
		if !sameLine(queue, after) {
			continue
		}
		select {
		case <-c.idle:
		case <-c.kicks:
		case <-bound.C:
			t.Fatalf("the queue never drained: %v still in line", after)
		}
	}
}

// sameLine reports whether two reads of the queue hold the same entries in
// the same states.
func sameLine(a, b []wsm.MergeQueueEntry) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i].Workspace != b[i].Workspace || a[i].State != b[i].State {
			return false
		}
	}
	return true
}

func (c *chain) prompts(ws ids.WorkspaceID, origin conversationv1.PromptOrigin) int {
	c.h.queue.mu.Lock()
	defer c.h.queue.mu.Unlock()
	n := 0
	for _, sub := range c.h.queue.submissions {
		if sub.WS == ws && sub.Origin == origin {
			n++
		}
	}
	return n
}

func (c *chain) recorded(level, op, message string) bool {
	return loggedLike(c.h, logLike{level: level, op: op, text: message})
}

// broken scripts a gate whose command was not found.
var broken = scriptedRun{Output: "bash: test-all.sh: command not found\n", Code: 127}

// failing scripts a failing gate.
var failing = scriptedRun{Output: "[agent-repl-tests] ERROR: daemon failed after 4s with exit code 1\n", Code: 1}

func TestEveryGateNamesTheNicenessItsTreeRunsAt(t *testing.T) {
	// Arrange.
	tests := []struct {
		name        string
		metricsRoot string
		want        []string
	}{
		{name: "a daemon recording no metrics", want: []string{"env", "AGENT_REPL_TEST_NICENESS=10", "bash", "s"}},
		{name: "a daemon recording metrics", metricsRoot: "/m", want: []string{
			"env", "AGENT_REPL_TEST_NICENESS=10", "AGENT_REPL_TEST_METRICS_ROOT=/m", "AGENT_REPL_TEST_METRICS_TRIGGER=merge_queue", "bash", "s"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := gateArgv([]string{"bash", "s"}, tt.metricsRoot)

			// Assert.
			if !slices.Equal(got, tt.want) {
				t.Fatalf("argv = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestStandingFront(t *testing.T) {
	cases := []struct {
		name      string
		left      map[ids.WorkspaceID]bool
		line      []ids.WorkspaceID
		wantFront []ids.WorkspaceID
		wantLine  []ids.WorkspaceID
	}{
		{
			name:      "a chain whose members are all in line is answered as it is",
			line:      []ids.WorkspaceID{"ws-a", "ws-b", "ws-c"},
			wantFront: []ids.WorkspaceID{"ws-a", "ws-b"},
			wantLine:  []ids.WorkspaceID{"ws-a", "ws-b", "ws-c"},
		},
		{
			name:      "a member that left is out of the chain and of a line still holding its entry",
			left:      map[ids.WorkspaceID]bool{"ws-a": true},
			line:      []ids.WorkspaceID{"ws-a", "ws-b", "ws-c"},
			wantFront: []ids.WorkspaceID{"ws-b"},
			wantLine:  []ids.WorkspaceID{"ws-b", "ws-c"},
		},
		{
			name:      "a member that left is out of the chain once its entry is gone",
			left:      map[ids.WorkspaceID]bool{"ws-a": true},
			line:      []ids.WorkspaceID{"ws-b", "ws-c"},
			wantFront: []ids.WorkspaceID{"ws-b"},
			wantLine:  []ids.WorkspaceID{"ws-b", "ws-c"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the chain is ws-a then ws-b.
			chain := []*run{{ws: "ws-a", leftLine: tc.left["ws-a"]}, {ws: "ws-b", leftLine: tc.left["ws-b"]}}
			var line []wsm.MergeQueueEntry
			for _, ws := range tc.line {
				line = append(line, admittedEntry(ws))
			}

			// Act
			front, entries := standingFront(chain, line)

			// Assert
			var gotFront, gotLine []ids.WorkspaceID
			for _, r := range front {
				gotFront = append(gotFront, r.ws)
			}
			for _, e := range entries {
				gotLine = append(gotLine, e.Workspace)
			}
			if !slices.Equal(gotFront, tc.wantFront) || !slices.Equal(gotLine, tc.wantLine) {
				t.Fatalf("standingFront = %v, %v; want %v, %v", gotFront, gotLine, tc.wantFront, tc.wantLine)
			}
		})
	}
}

// TestTheChainCheckAgainstItsQueue covers extendOnce's check that its chain is
// the queue's front: a run whose merge is over left its line before its entry
// went, so a queue read on either side of that removal agrees with the chain,
// while a chain that truly stands elsewhere is still refused loudly.
func TestTheChainCheckAgainstItsQueue(t *testing.T) {
	cases := []struct {
		name      string
		chain     []ids.WorkspaceID
		leftLine  ids.WorkspaceID
		queue     []ids.WorkspaceID
		wantError bool
	}{
		{name: "a head that left, read before its entry went, is no defect", chain: []ids.WorkspaceID{"ws-a", "ws-b"}, leftLine: "ws-a", queue: []ids.WorkspaceID{"ws-a", "ws-b"}},
		{name: "a head that left, read after its entry went, is no defect", chain: []ids.WorkspaceID{"ws-a", "ws-b"}, leftLine: "ws-a", queue: []ids.WorkspaceID{"ws-b"}},
		{name: "a chain whose only member left has nothing to extend behind", chain: []ids.WorkspaceID{"ws-a"}, leftLine: "ws-a", queue: []ids.WorkspaceID{"ws-b"}},
		{name: "a chain standing behind an entry it does not hold is refused", chain: []ids.WorkspaceID{"ws-b"}, queue: []ids.WorkspaceID{"ws-a", "ws-b"}, wantError: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var entries []wsm.MergeQueueEntry
			for _, ws := range tc.queue {
				entries = append(entries, admittedEntry(ws))
			}
			h := resumeWorld(t, entries)
			standChain(t, h, tc.chain, tc.leftLine)

			// Act
			started := h.o.extendOnce(context.Background(), h.repoKey())

			// Assert
			if started {
				t.Fatal("extendOnce started a resumed run, want none")
			}
			if got := len(h.o.chains[h.repoKey()]); got != len(tc.chain) {
				t.Fatalf("the chain holds %d runs, want the %d it had: none of these chains is extended", got, len(tc.chain))
			}
			refused := loggedLike(h, logLike{level: "error", op: "daemon.merge.chain", text: "does not stand at the front of its queue"})
			if refused != tc.wantError {
				t.Fatalf("the chain was recorded as off its queue's front: %v, want %v", refused, tc.wantError)
			}
			if tc.wantError && !loggedLike(h, logLike{level: "error", op: "daemon.merge.chain", text: "does not stand at the front of its queue",
				context: map[string]string{"member": "ws-b", "place": "0", "entries": "2", "chain": "ws-b", "line": "ws-a(" + fmt.Sprint(wsm.MergeAdmitted) + ") ws-b(" + fmt.Sprint(wsm.MergeAdmitted) + ")"}}) {
				t.Fatal("the refusal did not name the member, its place, the whole chain and the whole line")
			}
		})
	}
}

// standChain stands CHAIN as h's chain of deploys running concurrent stages:
// each member joined, LEFT marked as having left its line, and the first the
// running head, which has read its repository's concurrent deploy config --
// the chain extendOnce checks against its queue.
func standChain(t *testing.T, h *harness, chain []ids.WorkspaceID, left ids.WorkspaceID) []*run {
	t.Helper()
	// A queue chains only behind a head whose deploy runs concurrent stages.
	h.mu.Lock()
	h.config = concurrentThrough("deploy")
	h.mu.Unlock()
	ctx, cancel := context.WithCancelCause(context.Background())
	t.Cleanup(func() { cancel(nil) })
	h.o.mu.Lock()
	defer h.o.mu.Unlock()
	var runs []*run
	for _, ws := range chain {
		r := h.o.newRun(ctx, cancel, h.repoKey(), ws, 1)
		r.joined, r.leftLine = true, ws == left
		runs = append(runs, r)
	}
	h.o.chains[h.repoKey()] = runs
	// The chain extends only behind its running head (extendOnce), so its
	// first member is the head running, which has read the repository's
	// deploy config.
	runs[0].target = deployTarget{cfg: concurrentThrough("deploy")}
	h.o.running[h.repoKey()] = runs[0]
	return runs
}

// headInGate runs the chain's head merge into its test gate and holds it
// there, answering the chain and the pump's result once the gate is entered.
func headInGate(t *testing.T) (*chain, <-chan error) {
	t.Helper()
	c := newChain(t)
	inGate, _ := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	done := admitAsync(c.h, context.Background())
	<-inGate
	return c, done
}
