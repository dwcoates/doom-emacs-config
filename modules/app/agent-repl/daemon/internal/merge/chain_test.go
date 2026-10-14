package merge

import (
	"agent-repld/internal/wsm"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"slices"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/cpugov"
	"agent-repld/internal/dlog"
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
	// rebaseHolds parks the next rebase started in a worktree.
	rebaseHolds map[string]chainHold
}

func newChainGit(inner *fakeGit) *chainGit {
	g := &chainGit{
		fakeGit: inner, commits: map[string]*chainCommit{}, refs: map[string]string{},
		rebases: map[string]*chainRebase{}, conflicts: map[[2]string][]string{}, recorded: map[[2]string]bool{},
		trees: map[string]string{}, rebaseHolds: map[string]chainHold{},
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
	g.cmu.Lock()
	defer g.cmu.Unlock()
	h := chainHold{entered: make(chan struct{}), release: make(chan struct{})}
	g.rebaseHolds[dir] = h
	return h.entered, h.release
}

func (g *chainGit) StartRebase(ctx context.Context, dir, onto string, commits []string) (gitclient.RebaseStep, error) {
	g.cmu.Lock()
	hold, held := g.rebaseHolds[dir]
	delete(g.rebaseHolds, dir)
	g.cmu.Unlock()
	if held {
		close(hold.entered)
		select {
		case <-hold.release:
		case <-ctx.Done():
			return gitclient.RebaseStep{}, fmt.Errorf("chain rebase: %w", ctx.Err())
		}
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
	holds   map[string][]chainHold
	calls   map[string]int
	// running counts the runs under way now, and most the most that ever
	// were at once: one repository's gates never overlap (testturn.go).
	running, most int
}

type chainHold struct{ entered, release chan struct{} }

func newChainRunner() *chainRunner {
	return &chainRunner{results: map[string][]scriptedRun{}, holds: map[string][]chainHold{}, calls: map[string]int{}}
}

// hold parks the next gate run in dir until release closes.
func (c *chainRunner) hold(dir string) (entered, release chan struct{}) {
	c.mu.Lock()
	defer c.mu.Unlock()
	h := chainHold{entered: make(chan struct{}), release: make(chan struct{})}
	c.holds[dir] = append(c.holds[dir], h)
	return h.entered, h.release
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
	var hold *chainHold
	if hs := c.holds[dir]; len(hs) > 0 {
		hold = &hs[0]
		c.holds[dir] = hs[1:]
	}
	c.mu.Unlock()
	if hold != nil {
		close(hold.entered)
		select {
		case <-hold.release:
		case <-ctx.Done():
			return scriptrunner.Run{}, fmt.Errorf("chain script: %w", ctx.Err())
		}
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

// decisionLog forwards to the harness's log and reports every governor
// decision as it is made.
type decisionLog struct {
	dlog.Logger
	decisions chan dlog.Context
}

func (d *decisionLog) Info(op, msg string, ctx dlog.Context) {
	d.Logger.Info(op, msg, ctx)
	if op == "daemon.cpugov.admit" {
		d.decisions <- ctx
	}
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
	kicks     chan wsm.RepoKey
	decisions chan dlog.Context
	dirs      map[ids.WorkspaceID]string
	branches  map[ids.WorkspaceID]string
	turns     *chainTurns
}

// chainTurns orders the gates of a chain's merges at their repository's test
// turn: a parked merge's next ask for the turn holds until it is released or
// its run is stopped, and every merge that joins the line for it is told.
type chainTurns struct {
	mu    sync.Mutex
	parks map[ids.WorkspaceID]chan struct{}
	lined chan ids.WorkspaceID
}

// ask is the orchestrator's onTestTurnAsk.
func (c *chainTurns) ask(ctx context.Context, ws ids.WorkspaceID) {
	c.mu.Lock()
	park, ok := c.parks[ws]
	delete(c.parks, ws)
	c.mu.Unlock()
	if !ok {
		return
	}
	select {
	case <-park:
	case <-ctx.Done():
	}
}

// parkTurn holds ws's next ask for its test turn, until the release it
// answers is called or its run is stopped.
func (c *chain) parkTurn(ws ids.WorkspaceID) (release func()) {
	park := make(chan struct{})
	c.turns.mu.Lock()
	c.turns.parks[ws] = park
	c.turns.mu.Unlock()
	return func() {
		c.turns.mu.Lock()
		delete(c.turns.parks, ws)
		c.turns.mu.Unlock()
		close(park)
	}
}

// awaitLined waits, bounded, for ws to join the line for its test turn.
func (c *chain) awaitLined(t *testing.T, ws ids.WorkspaceID) {
	t.Helper()
	bound := time.NewTimer(5 * time.Second)
	defer bound.Stop()
	for {
		select {
		case got := <-c.turns.lined:
			if got == ws {
				return
			}
		case <-bound.C:
			t.Fatalf("%s never joined the line for its test turn", ws)
		}
	}
}

// newChain builds a harness whose queue runs the production window over the
// modelled repository, with the harness's merge (ws-1, branch feature)
// carrying patch a1.
func newChain(t *testing.T) *chain {
	t.Helper()
	return newChainOfWidth(t, 0)
}

// newChainOfWidth is newChain with a window of width (0 is the default,
// cpugov.QueueWindow), for a test of a mechanism that needs more merges in
// the chain than the default window holds.
func newChainOfWidth(t *testing.T, width int) *chain {
	t.Helper()
	h := newHarness(t)
	h.emacsRepo()
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	c := &chain{h: h, git: newChainGit(h.git), runner: newChainRunner(),
		idle: make(chan ids.WorkspaceID, 1024), kicks: make(chan wsm.RepoKey, 1), decisions: make(chan dlog.Context, 1024),
		dirs: map[ids.WorkspaceID]string{theWorkspace: h.sourceD}, branches: map[ids.WorkspaceID]string{theWorkspace: "feature"},
		turns: &chainTurns{parks: map[ids.WorkspaceID]chan struct{}{}, lined: make(chan ids.WorkspaceID, 1024)}}
	cfg := cpugov.DefaultConfig()
	if width > 0 {
		cfg.MaxGates = width
	}
	gov, err := cpugov.New(cfg, h.sampler, h.sampler, h.clock, &decisionLog{Logger: h.logs.Global(), decisions: c.decisions})
	if err != nil {
		t.Fatalf("building the governor: %v", err)
	}
	gov.Observe(cpugov.Sample{Utilization: 0, CPUs: 16})
	h.governor = gov
	c.wire(h.o)
	h.o.window = width
	c.git.branch("feature", "a1")
	return c
}

// wire points an orchestrator at the chain's world.
func (c *chain) wire(o *orchestrator) {
	o.window = 0
	o.deps.Git = c.git
	o.deps.TestRunner = c.runner
	o.deps.Governor = c.h.governor
	o.onChainIdle = func(ws ids.WorkspaceID) { c.idle <- ws }
	o.onTestTurnAsk = c.turns.ask
	o.onTestTurnLine = func(ws ids.WorkspaceID) { c.turns.lined <- ws }
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
	bound := time.NewTimer(5 * time.Second)
	defer bound.Stop()
	for {
		select {
		case got := <-c.idle:
			if got == ws && until() {
				return
			}
		case <-bound.C:
			t.Fatalf("%s never went idle in the awaited state", ws)
		}
	}
}

// awaitDecision waits for the governor's decision about ws.
func (c *chain) awaitDecision(t *testing.T, ws ids.WorkspaceID, decision string) dlog.Context {
	t.Helper()
	bound := time.NewTimer(5 * time.Second)
	defer bound.Stop()
	for {
		select {
		case got := <-c.decisions:
			if got["workspace"] == string(ws) && got["decision"] == decision {
				return got
			}
		case <-bound.C:
			t.Fatalf("the governor never decided %s for %s", decision, ws)
		}
	}
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

func TestTheMergeBehindTheHeadRebasesOntoTheTargetsTip(t *testing.T) {
	// Arrange: the head holds in its gate; the merge behind it, rebased,
	// waits in line for the test turn.
	c := newChain(t)
	_, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())

	// Act.
	c.awaitLined(t, "ws-b")
	tree, own := c.git.tree("b-ws-b"), c.git.own(c.git.ref("master"), "b-ws-b")
	close(releaseHead)
	<-done
	c.drain(t)

	// Assert.
	if tree != "b1+base" || !slices.Equal(own, []string{"b1"}) {
		t.Fatalf("the merge behind the head holds %s carrying %v, want b1+base carrying [b1]: it stands on the target, not the head", tree, own)
	}
}

func TestTheMergeBehindTheHeadStartsWhileTheHeadsRebaseStands(t *testing.T) {
	// Arrange: master moved, so the head must rebase; it parks inside its
	// rebase, and the merge behind it holds in its gate.
	c := newChain(t)
	c.git.commitOn("master", "m1")
	headRebase, releaseHead := c.git.holdRebase(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	behindGate, releaseBehind := c.runner.hold(c.dirs["ws-b"])
	done := admitAsync(c.h, context.Background())
	<-headRebase

	// Act: the merge behind reaches its gate while the head's rebase stands.
	bound := time.NewTimer(5 * time.Second)
	defer bound.Stop()
	select {
	case <-behindGate:
	case <-bound.C:
		t.Fatal("the merge behind the head never reached its gate while the head's rebase stood: it waited on the head")
	}
	close(releaseHead)
	close(releaseBehind)
	if err := <-done; err != nil {
		t.Fatalf("the head's admission ended on: %v", err)
	}
	c.drain(t)

	// Assert: both landed, each with its own commits.
	if tree := c.git.tree("master"); tree != "a1+b1+base+m1" {
		t.Fatalf("master holds %s, want a1+b1+base+m1", tree)
	}
}

func TestTheRaceWinnerLandsTheTreeItTestedWithoutTestingAgain(t *testing.T) {
	// Arrange: the head holds in its gate; the merge behind it passes.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")

	// Act.
	c.behindWins(t)

	// Assert.
	if got := c.runner.ran(c.dirs["ws-b"]); got != 1 {
		t.Fatalf("the winner ran its gate %d times, want once: its pass stood", got)
	}
	if !c.recorded("info", "daemon.merge.speculative", "the speculative pass stands") {
		t.Fatal("the reused pass was not recorded")
	}
}

func TestAHeadThatLostTheRaceAndThenFailsLandsNothingOfItsOwn(t *testing.T) {
	// Arrange: the head holds in its first gate and breaks in the next.
	c := newChain(t)
	c.runner.script(c.dirs[theWorkspace], broken)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")

	// Act.
	c.behindWins(t)

	// Assert.
	if tree := c.git.tree("master"); tree != "b1+base" {
		t.Fatalf("master holds %s, want b1+base: the failed head's commits must not land", tree)
	}
}

func TestAConflictMetByTheHeadStartingOverIsResolvedByItsOwnSession(t *testing.T) {
	// Arrange: A's commit conflicts with B's; A holds in its first gate, so B
	// wins and A starts over on a target that holds b1.
	c := newChain(t)
	c.git.conflict("a1", "b1", "x.go")
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")

	// Act.
	c.behindWins(t)

	// Assert.
	if got := c.prompts(theWorkspace, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 1 {
		t.Fatalf("A's session got %d conflict prompts starting over on B's landing, want 1", got)
	}
	if got := c.prompts("ws-b", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 0 {
		t.Fatalf("B's session got %d conflict prompts, want none: B stood on the target", got)
	}
}

func TestASpeculativeGateWaitsForTheLoadToFallBelowTheBand(t *testing.T) {
	// Arrange: the host is inside the band; the head's ask for the test turn
	// is parked, so the merge behind it holds the turn and asks the governor.
	c := newChain(t)
	c.h.load(0.6)
	release := c.parkTurn(theWorkspace)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())
	deferred := c.awaitDecision(t, "ws-b", "defer")
	ranWhileDeferred := c.runner.ran(c.dirs["ws-b"])

	// Act: the load falls, so the merge behind runs, passes and overtakes.
	c.h.load(0.2)
	<-done
	release()
	c.drain(t)

	// Assert.
	if deferred["reason"] != "load_in_or_above_band" || ranWhileDeferred != 0 {
		t.Fatalf("deferred for %v with %d runs, want load_in_or_above_band and no run", deferred["reason"], ranWhileDeferred)
	}
	if got := c.runner.ran(c.dirs["ws-b"]); got != 1 {
		t.Fatalf("B ran its gate %d times, want once: once the load fell", got)
	}
}

func TestASpeculativeGateIsNeverAdmittedAboveTheBand(t *testing.T) {
	// Arrange: the head's ask for the test turn is parked, so the merge
	// behind it holds the turn and asks the governor.
	c := newChain(t)
	c.h.load(0.9)
	release := c.parkTurn(theWorkspace)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())

	// Act: the head asks once B is deferred; B gives it the turn.
	deferred := c.awaitDecision(t, "ws-b", "defer")
	release()
	<-done
	c.promote(t)

	// Assert.
	if deferred["reason"] != "load_in_or_above_band" {
		t.Fatalf("deferred for %v, want load_in_or_above_band", deferred["reason"])
	}
	if got := c.runner.ran(c.dirs["ws-b"]); got != 1 {
		t.Fatalf("B ran its gate %d times, want once: only at the head", got)
	}
	if !c.recorded("info", "daemon.merge.tests", "gives its test turn to its head") {
		t.Fatal("B holding the turn unadmitted did not record giving it to its head")
	}
}

func TestOnACPowerTheMergeBehindTheHeadGatesBesideItOnASaturatedHost(t *testing.T) {
	// Arrange: the host is saturated but on AC power; the head holds in its
	// gate.
	c := newChain(t)
	c.h.load(1)
	c.h.power(cpugov.PowerAC)
	inGate, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	behindGate, releaseBehind := c.runner.hold(c.dirs["ws-b"])
	done := admitAsync(c.h, context.Background())
	<-inGate

	// Act: the merge behind reaches its gate while the head's still runs.
	admitted := c.awaitDecision(t, "ws-b", "admit")
	<-behindGate
	close(releaseHead)
	close(releaseBehind)
	<-done
	c.drain(t)

	// Assert.
	if admitted["reason"] != "on_ac_power" {
		t.Fatalf("admitted for %v, want on_ac_power", admitted["reason"])
	}
}

func TestASpeculativeGateBelowTheBandIsAdmitted(t *testing.T) {
	// Arrange: the head's ask for the test turn is parked.
	c := newChain(t)
	release := c.parkTurn(theWorkspace)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())

	// Act.
	admitted := c.awaitDecision(t, "ws-b", "admit")
	<-done
	release()
	c.drain(t)

	// Assert.
	if admitted["reason"] != "below_band" {
		t.Fatalf("admitted for %v, want below_band", admitted["reason"])
	}
}

func TestTheHeadsGateRunsOnASaturatedHost(t *testing.T) {
	// Arrange.
	c := newChain(t)
	c.h.load(1)
	enqueue(t, c.h)

	// Act.
	if err := c.h.admit(context.Background()); err != nil {
		t.Fatalf("the head ended on: %v", err)
	}

	// Assert.
	if tree := c.git.tree("master"); tree != "a1+base" {
		t.Fatalf("master holds %s, want the head landed on a saturated host", tree)
	}
}

func TestAGateClaimsTheHostBudgetAtItsClassAndASpeculativeOneRunsNiced(t *testing.T) {
	// Arrange.
	tests := []struct {
		name        string
		speculative bool
		want        []string
	}{
		{name: "the head", want: []string{"env", "AGENT_REPL_SUITE_PRIORITY=merge", "bash", "s"}},
		{name: "behind the head", speculative: true, want: []string{"/usr/bin/nice", "-n", "12", "env", "AGENT_REPL_SUITE_PRIORITY=merge", "bash", "s"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := gateArgv([]string{"bash", "s"}, tt.speculative)

			// Assert.
			if !slices.Equal(got, tt.want) {
				t.Fatalf("argv = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestTheDefaultWindowHoldsTheHeadAndOneMergeBehindIt(t *testing.T) {
	// Arrange: three merges at the default window (owner ruling, 2026-10-12:
	// the top two only); the head holds in its gate and the merge behind it
	// waits in line for the test turn.
	c := newChain(t)
	_, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	c.queue(t, "ws-c", "c1")
	done := admitAsync(c.h, context.Background())
	c.awaitLined(t, "ws-b")

	// Act.
	_, cRuns := c.h.o.runFor("ws-c")
	cRuns = cRuns || c.h.o.inChain(c.h.repoKey(), "ws-c")
	cTree := c.git.tree("b-ws-c")
	close(releaseHead)
	<-done
	c.drain(t)

	// Assert.
	if cpugov.QueueWindow != 2 {
		t.Fatalf("cpugov.QueueWindow = %d, want 2", cpugov.QueueWindow)
	}
	if cRuns || cTree != "base+c1" {
		t.Fatalf("C ran=%v holding %s while third in line, want it untouched", cRuns, cTree)
	}
}

func TestARestartMidRaceResumesBothMergesAndLandsEachWithItsOwnCommits(t *testing.T) {
	// Arrange: the head holds in its gate and the merge behind it waits in
	// line for the test turn; the daemon exits mid-race.
	c := newChain(t)
	_, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	first := c.h.o
	done := admitAsync(c.h, context.Background())
	c.awaitLined(t, "ws-b")
	first.Drain(context.Background())
	close(releaseHead)
	<-done
	waitGone(t, first)
	next, err := newOrchestrator(c.h.deps())
	if err != nil {
		t.Fatalf("building the next daemon's orchestrator: %v", err)
	}
	c.wire(next)
	c.h.o = next

	// Act.
	if err := next.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}
	c.drain(t)

	// Assert.
	got := c.git.landedOwn()
	if len(got) != 2 {
		t.Fatalf("landings brought %v, want two", got)
	}
	own := map[string]bool{}
	for _, l := range got {
		own[strings.Join(l, ",")] = true
	}
	if !own["a1"] || !own["b1"] {
		t.Fatalf("landings brought %v, want one of [a1] and one of [b1]", got)
	}
	if tree := c.git.tree("master"); tree != "a1+b1+base" {
		t.Fatalf("master holds %s, want a1+b1+base", tree)
	}
}

// waitGone waits, bounded, for everything an orchestrator started to end.
func waitGone(t *testing.T, o *orchestrator) {
	t.Helper()
	gone := make(chan struct{})
	go func() { o.live.Wait(); close(gone) }()
	select {
	case <-gone:
	case <-time.After(5 * time.Second):
		t.Fatal("the exiting orchestrator's runs did not end")
	}
}

func TestASpeculativePassStartsTheFixingCountOver(t *testing.T) {
	// Arrange: the merge behind the head fails its first speculative gate,
	// is fixed, and passes the next.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	c.runner.script(c.dirs["ws-b"], failing)

	// Act.
	c.behindWins(t)

	// Assert: the record its pass wrote counts no fixing attempt.
	c.h.db.mu.Lock()
	defer c.h.db.mu.Unlock()
	fixed, passed := false, false
	for _, p := range c.h.db.progressHistory {
		if p.Workspace != "ws-b" {
			continue
		}
		var doc progressDoc
		if err := json.Unmarshal(p.Document, &doc); err != nil {
			t.Fatalf("decoding a record: %v", err)
		}
		fixed = fixed || doc.FixesUsed == 1
		if doc.Tested != nil {
			passed = true
			if doc.FixesUsed != 0 {
				t.Fatalf("the pass recorded %d fixing attempts, want the count started over", doc.FixesUsed)
			}
		}
	}
	if !fixed || !passed {
		t.Fatalf("fixed=%v passed=%v, want a fixing attempt and then a speculative pass", fixed, passed)
	}
}

func TestWatchLooksAgainAtEachSignalUntilItIsDone(t *testing.T) {
	// Arrange: a look that is done on its third reading.
	h := newHarness(t)
	ctx, cancel := context.WithCancelCause(context.Background())
	signals := make(chan chan struct{}, 3)
	looks := 0
	stop := h.o.watch(ctx, cancel, func() (bool, <-chan struct{}) {
		looks++
		if looks == 3 {
			return true, nil
		}
		ch := make(chan struct{})
		signals <- ch
		return false, ch
	})

	// Act.
	close(<-signals)
	close(<-signals)
	stop()

	// Assert.
	if looks != 3 {
		t.Fatalf("the watch looked %d times, want 3: once, then at each signal", looks)
	}
}

func TestStoppingAWatchEndsItAndCancelsItsWork(t *testing.T) {
	// Arrange: a look that never finishes.
	h := newHarness(t)
	ctx, cancel := context.WithCancelCause(context.Background())
	stop := h.o.watch(ctx, cancel, func() (bool, <-chan struct{}) { return false, make(chan struct{}) })

	// Act.
	stop()

	// Assert.
	if ctx.Err() == nil {
		t.Fatal("stopping the watch left its work uncancelled")
	}
}

func TestEveryOrchestratorWatchRunsThroughWatch(t *testing.T) {
	// Arrange.
	tests := []struct{ file, watcher string }{
		{file: "chain.go", watcher: "return r.o.watch(ctx, cancel,"},
		{file: "testturn.go", watcher: "return o.watch(ctx, cut,"},
	}
	for _, tt := range tests {
		body, err := os.ReadFile(tt.file)
		if err != nil {
			t.Fatalf("reading %s: %v", tt.file, err)
		}

		// Act / Assert.
		if !strings.Contains(string(body), tt.watcher) {
			t.Fatalf("%s's watch no longer runs through watch", tt.file)
		}
	}
	if body, _ := os.ReadFile("testturn.go"); strings.Contains(string(body), "go func()") {
		t.Fatal("testturn.go starts a watcher goroutine of its own instead of watch")
	}
}
