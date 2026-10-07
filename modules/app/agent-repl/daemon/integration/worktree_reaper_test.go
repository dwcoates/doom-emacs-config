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

// TestTheReaperExpiresAWorktreeThatNeverLanded drives the expiry rule through
// the real daemon against the scripted git: in a repository worked in every 12
// hours for 20 days, a dirty unlanded worktree born 30 days ago is preserved
// under refs/agent-repl/reaped/ and removed with its branch kept, while one
// born 5 days ago and an open workspace's checkout are left alone.
func TestTheReaperExpiresAWorktreeThatNeverLanded(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{ExtraEnv: reaperEnv})
	repo := harness.NewRepo(t)
	now := time.Now()
	stale := repo.AddWorktree("stale")
	staleHead := repo.CommitIn(stale, "stale.txt", "never landed\n")
	repo.SetDirty(stale, true)
	young := repo.AddWorktree("young")
	repo.CommitIn(young, "young.txt", "not landed yet\n")
	for dir, born := range map[string]time.Time{stale: now.Add(-30 * 24 * time.Hour), young: now.Add(-5 * 24 * time.Hour)} {
		repo.IdleWorktree(dir, now.Add(-72*time.Hour))
		repo.BornWorktree(dir, born)
	}
	repo.ActiveEvery(now.Add(-20*24*time.Hour), now.Add(-time.Hour), 12*time.Hour)
	// An open workspace registers the repository with the daemon; its own
	// checkout is kept by its gate.
	open := repo.AddWorktree("open")
	repo.IdleWorktree(open, now.Add(-72*time.Hour))
	harness.Register(t, d, open)

	// Act.
	expiry := d.AwaitLogRecord(d.RunLogPath(), "the stale worktree's expiry", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.worktreereap.expire" && r.Level == "info" && r.Context["worktree"] == stale
	})
	d.AwaitLogRecord(d.RunLogPath(), "a sweep that kept the young tree as unlanded and the open one by its gate", func(r harness.LogRecord) bool {
		kept, _ := r.Context["kept"].(map[string]any)
		return r.Operation == "daemon.worktreereap.sweep" && r.Message == "the sweep finished" &&
			kept["unlanded"] == float64(1) && kept["open_workspace"] == float64(1) && r.Context["expired"] == float64(1)
	})

	// Assert.
	if _, err := os.Stat(stale); !os.IsNotExist(err) {
		t.Fatalf("the expired worktree %s is still on disk (%v)", stale, err)
	}
	if repo.HasWorktree(stale) || !repo.HasBranch("stale") {
		t.Fatalf("want the expired worktree unregistered and its branch kept: worktrees %v, branches %v", repo.Worktrees(), repo.Branches())
	}
	ref, _ := expiry.Context["preserved_ref"].(string)
	sha, found := repo.Ref(ref)
	if !found || sha != expiry.Context["preserved_sha"] {
		t.Fatalf("preservation ref %q = (%q, %v), refs %v, want the recorded commit", ref, sha, found, repo.Refs())
	}
	if parent := repo.ParentOf(sha); parent != staleHead {
		t.Fatalf("the preservation commit's parent = %q, want the stale head %q", parent, staleHead)
	}
	if !repo.HasWorktree(young) || !repo.HasWorktree(open) {
		t.Fatalf("the young or the open worktree was touched: worktrees %v", repo.Worktrees())
	}
}
