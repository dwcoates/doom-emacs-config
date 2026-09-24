package worktreereap

import (
	"context"
	"errors"
	"os"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// --- New ---------------------------------------------------------------------

func TestNewRefusesAMissingCollaboratorOrWindow(t *testing.T) {
	cases := []struct {
		name    string
		breakIt func(*Deps)
	}{
		{"git", func(d *Deps) { d.Git = nil }},
		{"registry", func(d *Deps) { d.Registry = nil }},
		{"live sessions", func(d *Deps) { d.LiveSessions = nil }},
		{"clock", func(d *Deps) { d.Clock = nil }},
		{"lock path", func(d *Deps) { d.LockPath = "" }},
		{"log", func(d *Deps) { d.Log = nil }},
		{"idle threshold", func(d *Deps) { d.IdleAfter = 0 }},
		{"start delay", func(d *Deps) { d.StartDelay = 0 }},
		{"cadence", func(d *Deps) { d.Every = -time.Hour }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			deps := newWorld(t).deps()
			tc.breakIt(&deps)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil {
				t.Fatalf("New with no %s = nil error, want a refusal", tc.name)
			}
		})
	}
}

// --- the landed rule ---------------------------------------------------------

func TestALandedIdleWorktreeIsRemoved(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "landed"})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 1 || report.Removed[0] != dir {
		t.Fatalf("Removed = %v, want [%s]", report.Removed, dir)
	}
	w.assertNoWarnings()
}

func TestALandedWorktreesBranchIsDeletedAtTheJudgedHead(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "landed"})

	// Act.
	w.sweep()

	// Assert.
	if want := "delete_branch feat/landed " + w.headOf(repo, dir); !w.git.saw(want) {
		t.Fatalf("git calls = %v, want %q", w.git.called(), want)
	}
}

func TestASquashLandedWorktreeIsRemoved(t *testing.T) {
	// Arrange: the branch's commits are NOT ancestors of the default branch
	// -- it landed as one squash commit -- so only the tree comparison can
	// tell it landed, and merge-tree answers the default branch's own tree.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "squashed", head: "5000000000000000000000000000000000000005",
		outcome: &gitclient.MergeTreeOutcome{Tree: baseTree}})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 1 || report.Removed[0] != dir {
		t.Fatalf("Removed = %v, want the squash-landed %s", report.Removed, dir)
	}
}

func TestTheLandedTestMergesTheHeadIntoTheDefaultBranchsCommit(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "landed"})

	// Act.
	w.sweep()

	// Assert.
	if want := "merge_tree " + repo + " " + baseSHA + " " + w.headOf(repo, dir); !w.git.saw(want) {
		t.Fatalf("git calls = %v, want %q", w.git.called(), want)
	}
}

func TestTheDefaultBranchIsTheOneTheRepositoryResolves(t *testing.T) {
	// Arrange: nothing hardcodes "master".
	w := newWorld(t)
	repo := w.addRepo("repo")
	w.git.defaultBranch[repo] = "trunk"
	w.addTree(repo, tree{name: "landed"})

	// Act.
	w.sweep()

	// Assert.
	if want := "resolve_ref " + repo + " refs/heads/trunk"; !w.git.saw(want) {
		t.Fatalf("git calls = %v, want %q", w.git.called(), want)
	}
}

func TestAnUnlandedWorktreeIsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "unlanded", outcome: &gitclient.MergeTreeOutcome{Tree: "9999999999999999999999999999999999999999"}})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepUnlanded {
		t.Fatalf("Removed = %v, kept for %q, want the unlanded tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestAWorktreeWhoseMergeConflictsIsKept(t *testing.T) {
	// Arrange: a conflict is conservatively NOT landed, whatever tree git
	// printed with it.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "conflicts", outcome: &gitclient.MergeTreeOutcome{Tree: baseTree, Conflicted: true}})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepConflicts {
		t.Fatalf("Removed = %v, kept for %q, want the conflicted tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestADirtyWorktreeIsKept(t *testing.T) {
	// Arrange: IsClean counts modified AND untracked content (gitclient's
	// own tests pin both), so either keeps the tree.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "dirty"})
	w.git.dirty[dir] = true

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepDirty {
		t.Fatalf("Removed = %v, kept for %q, want the dirty tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestADirtyWorktreeIsNeverMergeTested(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "dirty"})
	w.git.dirty[dir] = true

	// Act.
	w.sweep()

	// Assert.
	for _, call := range w.git.called() {
		if strings.HasPrefix(call, "merge_tree ") {
			t.Fatalf("git calls = %v, want no merge-tree for a dirty tree", w.git.called())
		}
	}
}

// --- the idle gate -----------------------------------------------------------

func TestARecentlyActiveLandedWorktreeIsKept(t *testing.T) {
	// Arrange: a tree cut an hour ago sits at the default branch and is
	// trivially landed; an agent may be about to work in it.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "fresh", touched: now.Add(-time.Hour)})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepActive {
		t.Fatalf("Removed = %v, kept for %q, want the fresh tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestAnActiveWorktreeIsNeverProbed(t *testing.T) {
	// Arrange: nothing this sweep runs may touch a tree somebody is using.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "fresh", touched: now.Add(-time.Hour)})

	// Act.
	w.sweep()

	// Assert.
	if w.git.saw("is_clean " + dir) {
		t.Fatalf("git calls = %v, want no status probe of an active tree", w.git.called())
	}
}

func TestTheIdleThresholdIsTheConfiguredOne(t *testing.T) {
	// Arrange: two hours idle clears a one-hour threshold.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "idle", touched: now.Add(-2 * time.Hour), committed: now.Add(-2 * time.Hour)})
	deps := w.deps()
	deps.IdleAfter = time.Hour
	r, err := New(deps)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	report, err := r.Sweep(context.Background())

	// Assert.
	if err != nil || len(report.Removed) != 1 || report.Removed[0] != dir {
		t.Fatalf("Sweep = (%v, %v), want %s removed under a one-hour threshold", report.Removed, err, dir)
	}
}

// --- the safety gates --------------------------------------------------------

func TestTheMainWorktreeIsNeverJudged(t *testing.T) {
	// Arrange: the main worktree sits on the default branch, idle and clean.
	w := newWorld(t)
	repo := w.addRepo("repo")

	// Act.
	report := w.sweep()

	// Assert.
	if report.Kept[keepMain] != 1 || report.Worktrees != 0 {
		t.Fatalf("report = %+v, want the main worktree counted as main and nothing judged", report)
	}
	if w.git.saw("admin_dir "+repo) || w.git.saw("is_clean "+repo) {
		t.Fatalf("git calls = %v, want the main worktree never probed", w.git.called())
	}
}

func TestALockedWorktreeIsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "locked", locked: true})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepLocked {
		t.Fatalf("Removed = %v, kept for %q, want the locked tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestAnOpenWorkspacesCheckoutIsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "open"})
	w.register(repo, dir, wsm.Workspace{Closed: false})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepOpenWorkspace {
		t.Fatalf("Removed = %v, kept for %q, want the open workspace's tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestAWorkspaceWithALiveSessionIsKept(t *testing.T) {
	// Arrange: a closed workspace whose session is still live.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "live"})
	ws := w.register(repo, dir, wsm.Workspace{Closed: true})
	w.live = []ids.WorkspaceID{ws.ID}

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepLiveSession {
		t.Fatalf("Removed = %v, kept for %q, want the live session's tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestTheBranchAnOpenWorkspaceWasCutFromIsKept(t *testing.T) {
	// Arrange: a nested workspace merges into its parent's worktree.
	w := newWorld(t)
	repo := w.addRepo("repo")
	parent := w.addTree(repo, tree{name: "parent"})
	child := w.addTree(repo, tree{name: "child", touched: now.Add(-time.Minute)})
	w.register(repo, child, wsm.Workspace{ParentBranch: "feat/parent"})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(parent) != keepParentBranch {
		t.Fatalf("Removed = %v, kept for %q, want the parent's tree kept", report.Removed, w.keptFor(parent))
	}
}

func TestAClosedIdleWorkspacesLandedCheckoutIsRemoved(t *testing.T) {
	// Arrange: a closed workspace with no live session is no longer in use.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "closed"})
	w.register(repo, dir, wsm.Workspace{Closed: true})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 1 || report.Removed[0] != dir {
		t.Fatalf("Removed = %v, want the closed workspace's landed tree removed", report.Removed)
	}
}

func TestAJustMergedWorkspaceIsNotIdle(t *testing.T) {
	// Arrange: the merge queue stamps merged_at, then closed, then removes
	// the tree itself; the stamp keeps the sweep out of that window.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "merging"})
	merged := now.Add(-time.Second)
	w.register(repo, dir, wsm.Workspace{Closed: true, MergedAt: &merged})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepActive {
		t.Fatalf("Removed = %v, kept for %q, want the just-merged tree left to the merge queue", report.Removed, w.keptFor(dir))
	}
}

func TestALinkedWorktreeOnTheDefaultBranchIsKept(t *testing.T) {
	// Arrange: removing it would delete the default branch.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "on-main", branch: "main"})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepDefaultBranch {
		t.Fatalf("Removed = %v, kept for %q, want the default branch's tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestAWorktreeOnAnUnbornBranchIsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "unborn", head: "0000000000000000000000000000000000000000"})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepNoCommit {
		t.Fatalf("Removed = %v, kept for %q, want the unborn tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestAWorktreeWhoseDirectoryIsMissingIsKept(t *testing.T) {
	// Arrange: git lists it (a lock or a moved directory) but it is not there.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "gone", missing: true})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || w.keptFor(dir) != keepMissing {
		t.Fatalf("Removed = %v, kept for %q, want the missing tree kept", report.Removed, w.keptFor(dir))
	}
}

func TestALandedDetachedWorktreeIsRemovedWithNoBranchToDelete(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "detached", detached: true})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 1 || report.Removed[0] != dir || report.BranchesDeleted != 0 {
		t.Fatalf("report = %+v, want the detached tree removed and no branch deleted", report)
	}
}

// --- prunable ----------------------------------------------------------------

func TestAPrunableWorktreeIsPrunedNotRemoved(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "stale", prunable: true, missing: true})

	// Act.
	report := w.sweep()

	// Assert.
	if !w.git.saw("prune "+repo) || w.git.saw("remove "+dir) || report.Pruned != 1 {
		t.Fatalf("git calls = %v, pruned %d, want one prune and no removal", w.git.called(), report.Pruned)
	}
	if len(w.records("info", opPrune)) != 1 {
		t.Fatalf("prune records = %v, want one INFO", w.records("info", opPrune))
	}
}

func TestARepositoryWithNothingPrunableIsNotPruned(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	w.addTree(repo, tree{name: "landed"})

	// Act.
	w.sweep()

	// Assert.
	if w.git.saw("prune " + repo) {
		t.Fatalf("git calls = %v, want no prune", w.git.called())
	}
}

func TestAPruneFailureIsRecordedAtErrorAndTheSweepGoesOn(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	w.addTree(repo, tree{name: "stale", prunable: true, missing: true})
	landed := w.addTree(repo, tree{name: "landed"})
	w.git.pruneErr = errScripted

	// Act.
	report := w.sweep()

	// Assert.
	if len(w.records("error", opPrune)) != 1 || len(report.Removed) != 1 || report.Removed[0] != landed {
		t.Fatalf("prune errors = %d, removed %v, want one ERROR and the landed tree still removed", len(w.records("error", opPrune)), report.Removed)
	}
}

// --- removal failures --------------------------------------------------------

func TestARemovalFailureIsRecordedAtErrorAndTheSweepGoesOn(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	refused := w.addTree(repo, tree{name: "a-refused"})
	next := w.addTree(repo, tree{name: "b-next"})
	w.git.removeErr[refused] = errScripted

	// Act.
	report := w.sweep()

	// Assert.
	if len(w.records("error", opRemove)) != 1 {
		t.Fatalf("remove errors = %v, want one ERROR", w.records("error", opRemove))
	}
	if len(report.Removed) != 1 || report.Removed[0] != next {
		t.Fatalf("Removed = %v, want the next tree %s still removed", report.Removed, next)
	}
}

func TestABranchIsNeverDeletedWhenItsWorktreeWasNotRemoved(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	refused := w.addTree(repo, tree{name: "refused"})
	w.git.removeErr[refused] = errScripted

	// Act.
	w.sweep()

	// Assert.
	for _, call := range w.git.called() {
		if strings.HasPrefix(call, "delete_branch ") {
			t.Fatalf("git calls = %v, want no branch deletion after a refused removal", w.git.called())
		}
	}
}

func TestTheBranchIsDeletedOnlyAfterTheRemoval(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "landed"})

	// Act.
	w.sweep()

	// Assert.
	calls := strings.Join(w.git.called(), "\n")
	removeAt := strings.Index(calls, "remove "+dir)
	deleteAt := strings.Index(calls, "delete_branch feat/landed")
	if removeAt < 0 || deleteAt < removeAt {
		t.Fatalf("git calls = %v, want the removal before the branch deletion", w.git.called())
	}
}

func TestABranchDeletionFailureIsRecordedAtErrorAndTheRemovalStands(t *testing.T) {
	// Arrange: the branch moved after it was judged.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "moved"})
	w.git.deleteErr["feat/moved"] = errScripted

	// Act.
	report := w.sweep()

	// Assert.
	if len(w.records("error", opBranch)) != 1 || len(report.Removed) != 1 || report.Removed[0] != dir || report.BranchesDeleted != 0 {
		t.Fatalf("report = %+v, branch errors %d, want the removal counted and the branch kept loudly", report, len(w.records("error", opBranch)))
	}
}

// --- logging -----------------------------------------------------------------

func TestARemovalIsRecordedAtInfoWithItsFacts(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "landed"})

	// Act.
	w.sweep()

	// Assert.
	records := w.records("info", opRemove)
	if len(records) != 1 {
		t.Fatalf("remove records = %v, want one INFO", records)
	}
	ctx := records[0].Context
	if ctx["worktree"] != dir || ctx["branch"] != "feat/landed" || ctx["head"] != w.headOf(repo, dir) || ctx["why"] == nil {
		t.Fatalf("remove record context = %v, want the path, branch, head and why", ctx)
	}
}

func TestTheSweepSummaryIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	w.addTree(repo, tree{name: "landed"})
	w.addTree(repo, tree{name: "unlanded", outcome: &gitclient.MergeTreeOutcome{Tree: "9999999999999999999999999999999999999999"}})

	// Act.
	w.sweep()

	// Assert.
	var summary map[string]any
	for _, r := range w.records("info", opSweep) {
		if r.Message == "the sweep finished" {
			summary = r.Context
		}
	}
	if summary == nil || summary["removed"] != 1 || summary["worktrees"] != 2 || summary["failures"] != 0 {
		t.Fatalf("summary = %v, want 2 judged, 1 removed, 0 failures", summary)
	}
}

// --- repository and registry failures ---------------------------------------

func TestARepositoryThatCannotBeSweptIsRecordedAndTheNextIsSwept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	broken := w.addRepo("a-broken")
	w.git.defaultBranchErr[broken] = errScripted
	healthy := w.addRepo("b-healthy")
	dir := w.addTree(healthy, tree{name: "landed"})

	// Act.
	report := w.sweep()

	// Assert.
	if len(w.records("error", opRepo)) != 1 || len(report.Removed) != 1 || report.Removed[0] != dir {
		t.Fatalf("repo errors %d, removed %v, want one ERROR and the healthy repository swept", len(w.records("error", opRepo)), report.Removed)
	}
}

func TestARepositoryWhoseListingFailsIsRecordedAtError(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	w.git.listErr[repo] = errScripted

	// Act.
	report := w.sweep()

	// Assert.
	if len(w.records("error", opRepo)) != 1 || report.Failures != 1 {
		t.Fatalf("repo errors %d, failures %d, want one", len(w.records("error", opRepo)), report.Failures)
	}
}

func TestARepositoryWhoseMainWorktreeIsGoneIsSkippedAtInfo(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	if err := os.RemoveAll(repo); err != nil {
		t.Fatalf("removing the repository: %v", err)
	}

	// Act.
	w.sweep()

	// Assert.
	if len(w.records("info", opRepo)) != 1 || len(w.git.called()) != 0 {
		t.Fatalf("repo records %v, git calls %v, want one INFO and no git", w.records("info", opRepo), w.git.called())
	}
	w.assertNoWarnings()
}

func TestARegistryThatCannotBeReadEndsTheSweepAtError(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.registry.reposErr = errScripted

	// Act.
	_, err := w.reaper().Sweep(context.Background())

	// Assert.
	if !errors.Is(err, errScripted) || len(w.records("error", opSweep)) != 1 {
		t.Fatalf("Sweep = %v, sweep errors %d, want the registry failure returned and recorded", err, len(w.records("error", opSweep)))
	}
}

func TestAWorktreeWhoseActivityCannotBeReadIsKeptAtError(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "unreadable"})
	w.git.adminErr[dir] = errScripted

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || len(w.records("error", opJudge)) != 1 {
		t.Fatalf("Removed = %v, judge errors %d, want the tree kept and one ERROR", report.Removed, len(w.records("error", opJudge)))
	}
}

func TestAMergeTreeFailureKeepsTheWorktreeAtError(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "unmergeable"})
	w.git.mergeErr[w.headOf(repo, dir)] = errScripted

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || len(w.records("error", opJudge)) != 1 {
		t.Fatalf("Removed = %v, judge errors %d, want the tree kept and one ERROR", report.Removed, len(w.records("error", opJudge)))
	}
}

func TestACancelledGitIsRecordedAtInfoNotError(t *testing.T) {
	// Arrange: the daemon's exit cancelling a git is not a failure.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "landed"})
	w.git.removeErr[dir] = &gitclient.Cancelled{Args: []string{"worktree", "remove"}, Dir: repo, Cause: context.Canceled}

	// Act.
	report := w.sweep()

	// Assert.
	if len(w.records("error", opRemove)) != 0 || report.Failures != 0 {
		t.Fatalf("remove errors %v, failures %d, want a cancellation recorded at INFO only", w.records("error", opRemove), report.Failures)
	}
}

func TestACancelledSweepStopsAndSaysSo(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.addRepo("repo")
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := w.reaper().Sweep(ctx)

	// Assert.
	if !errors.Is(err, context.Canceled) || len(w.git.called()) != 0 {
		t.Fatalf("Sweep = %v, git calls %v, want a cancelled sweep that ran no git", err, w.git.called())
	}
	w.assertNoWarnings()
}

// --- exclusivity -------------------------------------------------------------

func TestASweepIsRefusedWhileAnotherIsRunning(t *testing.T) {
	// Arrange: the first sweep is held inside its first git call.
	w := newWorld(t)
	w.addRepo("repo")
	w.git.entered = make(chan struct{})
	w.git.release = make(chan struct{})
	r := w.reaper()
	first := make(chan error, 1)
	go func() {
		_, err := r.Sweep(context.Background())
		first <- err
	}()
	<-w.git.entered

	// Act.
	_, err := r.Sweep(context.Background())
	close(w.git.release)

	// Assert.
	if !errors.Is(err, ErrSweepRunning) {
		t.Fatalf("the second Sweep = %v, want ErrSweepRunning", err)
	}
	if err := <-first; err != nil {
		t.Fatalf("the first Sweep = %v, want it to finish", err)
	}
}

func TestASweepHeldByAnotherProcessIsSkippedAtInfo(t *testing.T) {
	// Arrange: an flock on a second open file description conflicts exactly
	// as another process's would.
	w := newWorld(t)
	repo := w.addRepo("repo")
	w.addTree(repo, tree{name: "landed"})
	held, ok, err := acquireLock(w.lockPath)
	if err != nil || !ok {
		t.Fatalf("taking the lock = (%v, %v)", ok, err)
	}
	t.Cleanup(func() { held.release() })

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Removed) != 0 || len(w.git.called()) != 0 || len(w.records("info", opSweep)) != 1 {
		t.Fatalf("report %+v, git calls %v, want nothing swept and one INFO", report, w.git.called())
	}
}

func TestASweepLockThatCannotBeToldAboutIsAnError(t *testing.T) {
	// Arrange: the lock's directory is a regular file.
	w := newWorld(t)
	w.writeAged(w.root+"/locks", longAgo)

	// Act.
	_, err := w.reaper().Sweep(context.Background())

	// Assert.
	if err == nil {
		t.Fatal("Sweep = nil error, want the lock failure")
	}
	if len(w.records("error", opSweep)) != 1 {
		t.Fatalf("sweep errors = %v, want one ERROR", w.records("error", opSweep))
	}
}
