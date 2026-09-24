//go:build integration

package integration

import (
	"os"
	"testing"
	"time"

	"claude-repld/integration/harness"
)

// reaperEnv runs the landed-worktree reaper's schedule fast enough to observe:
// the first sweep a moment after boot, then one every 200ms, so a sweep runs
// after the test has registered its repository. The idle threshold stays the
// production 24h; the fixture's trees are aged past it.
var reaperEnv = []string{
	"AGENT_REPL_WORKTREE_REAP_START_DELAY=10ms",
	"AGENT_REPL_WORKTREE_REAP_EVERY=200ms",
}

// TestTheReaperRemovesALandedIdleWorktreeAndKeepsTheRest drives the whole
// sweep through the real daemon against the scripted git: a worktree whose
// branch is already on main is removed and its branch deleted, while an
// unlanded worktree and an open workspace's checkout are left alone.
func TestTheReaperRemovesALandedIdleWorktreeAndKeepsTheRest(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: reaperEnv})
	repo := harness.NewRepo(t)
	landed := repo.AddWorktree("landed")
	unlanded := repo.AddWorktree("unlanded")
	repo.CommitIn(unlanded, "work.txt", "unlanded work\n")
	open := repo.AddWorktree("open")
	idleSince := time.Now().Add(-72 * time.Hour)
	for _, dir := range []string{landed, unlanded, open} {
		repo.IdleWorktree(dir, idleSince)
	}
	harness.Register(t, d, open)

	// Act.
	removal := d.AwaitLogRecord(d.RunLogPath(), "the landed worktree's removal", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.worktreereap.remove" && r.Level == "info" && r.Context["worktree"] == landed
	})
	d.AwaitLogRecord(d.RunLogPath(), "the landed branch's deletion", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.worktreereap.branch" && r.Context["branch"] == "landed"
	})
	// THE SUMMARY NAMES WHY EACH OTHER TREE WAS KEPT, so a tree kept by a
	// failure (which the warning sweep would also catch) cannot pass as one
	// kept by its gate.
	d.AwaitLogRecord(d.RunLogPath(), "a sweep that kept the unlanded and the open trees by their gates", func(r harness.LogRecord) bool {
		kept, _ := r.Context["kept"].(map[string]any)
		return r.Operation == "daemon.worktreereap.sweep" && r.Message == "the sweep finished" &&
			kept["unlanded"] == float64(1) && kept["open_workspace"] == float64(1)
	})

	// Assert.
	if removal.Context["branch"] != "landed" || removal.Context["why"] == nil {
		t.Fatalf("removal record = %v, want the branch and why", removal.Context)
	}
	if _, err := os.Stat(landed); !os.IsNotExist(err) {
		t.Fatalf("the landed worktree %s is still on disk (%v)", landed, err)
	}
	if repo.HasWorktree(landed) || repo.HasBranch("landed") {
		t.Fatalf("the landed worktree's registration or branch survived: worktrees %v, branches %v", repo.Worktrees(), repo.Branches())
	}
	if !repo.HasWorktree(unlanded) || !repo.HasBranch("unlanded") {
		t.Fatalf("the unlanded worktree was touched: worktrees %v, branches %v", repo.Worktrees(), repo.Branches())
	}
	if !repo.HasWorktree(open) || !repo.HasBranch("open") {
		t.Fatalf("the open workspace's checkout was touched: worktrees %v, branches %v", repo.Worktrees(), repo.Branches())
	}
}
