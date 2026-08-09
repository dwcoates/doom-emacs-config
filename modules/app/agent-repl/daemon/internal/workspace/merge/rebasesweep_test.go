package merge

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// THE BOOT SWEEP of rebase worktree DIRECTORIES. Its whole risk is the one
// judgement it makes — this directory belongs to nobody — so the tests are about
// what it keeps at least as much as about what it removes.

// arrangeOrphanDir plants a leftover rebase worktree parent in tmp and returns
// its path, mimicking the shape os.MkdirTemp + `git worktree add` leaves.
func arrangeOrphanDir(t *testing.T, tmp, suffix string) string {
	t.Helper()
	dir := filepath.Join(tmp, rebaseWorktreeMarker+suffix)
	if err := os.MkdirAll(filepath.Join(dir, rebaseWorktreeLeaf), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	return dir
}

func TestTheSweepRemovesADirectoryNoMergeOwns(t *testing.T) {
	// Arrange — a daemon died mid-merge and left its temp tree behind.
	tmp := t.TempDir()
	orphan := arrangeOrphanDir(t, tmp, "dead")
	e := newRootedDriver(t, &recordingSink{}, tmp)

	// Act
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert
	if _, err := os.Lstat(orphan); !os.IsNotExist(err) {
		t.Fatalf("Lstat(%s) = %v, want the orphan removed", orphan, err)
	}
}

func TestTheSweepKeepsTheWorktreeOfAConflictParkedMerge(t *testing.T) {
	// Arrange — the retained tree is a PARKED conflict's workbench: the
	// resolution turn is editing files in it right now, and removing it would
	// destroy a user's half-finished merge.
	tmp := t.TempDir()
	parked := arrangeOrphanDir(t, tmp, "parked")
	e := newRootedDriver(t, &recordingSink{}, tmp)

	// Act
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{Retained: []string{filepath.Join(parked, rebaseWorktreeLeaf)}}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert
	if _, err := os.Lstat(parked); err != nil {
		t.Fatalf("Lstat(%s) = %v, want the parked merge's worktree untouched", parked, err)
	}
}

func TestTheSweepSeparatesTheOrphansFromTheLiveOnes(t *testing.T) {
	// Arrange — both kinds in one temp dir, which is exactly how they sit live.
	tmp := t.TempDir()
	live := arrangeOrphanDir(t, tmp, "live")
	dead := arrangeOrphanDir(t, tmp, "dead")
	e := newRootedDriver(t, &recordingSink{}, tmp)

	// Act
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{Retained: []string{filepath.Join(live, rebaseWorktreeLeaf)}}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert
	if _, err := os.Lstat(live); err != nil {
		t.Fatalf("the live merge's worktree was swept: %v", err)
	}
	if _, err := os.Lstat(dead); !os.IsNotExist(err) {
		t.Fatalf("Lstat(%s) = %v, want the orphan removed", dead, err)
	}
}

func TestTheSweepLeavesDirectoriesThatAreNotRebaseWorktreesAlone(t *testing.T) {
	// Arrange — $TMPDIR is shared with the whole machine.
	tmp := t.TempDir()
	stranger := filepath.Join(tmp, "somebody-elses-work")
	if err := os.Mkdir(stranger, 0o755); err != nil {
		t.Fatalf("Mkdir: %v", err)
	}
	e := newRootedDriver(t, &recordingSink{}, tmp)

	// Act
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert
	if _, err := os.Lstat(stranger); err != nil {
		t.Fatalf("Lstat(%s) = %v, want an unrelated temp directory untouched", stranger, err)
	}
}

func TestTheSweepReportsOneSummaryLine(t *testing.T) {
	// Arrange — three orphans and one live tree. A line per directory is what a
	// sweep of hundreds must never produce.
	tmp := t.TempDir()
	live := arrangeOrphanDir(t, tmp, "live")
	for _, suffix := range []string{"a", "b", "c"} {
		arrangeOrphanDir(t, tmp, suffix)
	}
	log := &rebaseLog{}
	e := newLoggingDriverRooted(t, log, tmp)

	// Act
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{Retained: []string{filepath.Join(live, rebaseWorktreeLeaf)}}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert
	got := log.matching("orphaned rebase worktree sweep")
	if len(got) != 1 {
		t.Fatalf("summary lines = %v, want exactly one", got)
	}
	if !strings.Contains(got[0], "removed=3") || !strings.Contains(got[0], "kept=1") {
		t.Fatalf("summary = %q, want the counts of what was removed and what was kept", got[0])
	}
}

func TestTheSummaryNamesWhatItKept(t *testing.T) {
	// Arrange — the retention is the sweep's only judgement, so it is the one
	// thing the summary has to make checkable.
	tmp := t.TempDir()
	live := arrangeOrphanDir(t, tmp, "live")
	log := &rebaseLog{}
	e := newLoggingDriverRooted(t, log, tmp)

	// Act
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{Retained: []string{filepath.Join(live, rebaseWorktreeLeaf)}}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert
	got := log.matching("orphaned rebase worktree sweep")
	if len(got) != 1 || !strings.Contains(got[0], live) {
		t.Fatalf("summary = %v, want the retained directory %s named", got, live)
	}
}

func TestTheSweepPrunesTheRepositoryTheOrphanBelongedTo(t *testing.T) {
	// Arrange — a real registered worktree whose daemon then died: the
	// directory is a leftover AND the repository still lists it.
	tmp := t.TempDir()
	target := initTarget(t)
	orphan := filepath.Join(tmp, rebaseWorktreeMarker+"registered")
	work := filepath.Join(orphan, rebaseWorktreeLeaf)
	gitRun(t, target, "worktree", "add", "-q", "--detach", work)
	e := newRootedDriver(t, &recordingSink{}, tmp)

	// Act — the target is in the daemon's repository universe, which is what
	// permits removing a leftover that names it.
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{Repos: []string{target}}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert — a registration that outlives its directory is what grew this
	// repository's `git worktree list` to ~190 entries.
	if got := gitRun(t, target, "worktree", "list", "--porcelain"); strings.Contains(got, rebaseWorktreeMarker) {
		t.Fatalf("worktree list still carries the swept worktree:\n%s", got)
	}
}

func TestTheSweepNeverLooksOutsideItsInjectedRoot(t *testing.T) {
	// Arrange — the REAL process temp dir (stood in for here, and installed as
	// $TMPDIR so an implicit fallback would find it) holds a live daemon's
	// rebase worktree, while this driver's root is a directory of this test's
	// own. This is the production incident: a test's sweep read $TMPDIR and
	// deleted the tree a merge gate was running its suite inside.
	realTmp := t.TempDir()
	decoy := arrangeOrphanDir(t, realTmp, "live-production-merge")
	t.Setenv("TMPDIR", realTmp)
	root := t.TempDir()
	mine := arrangeOrphanDir(t, root, "mine")
	e := newRootedDriver(t, &recordingSink{}, root)

	// Act
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert — the decoy outside the injected root is untouched (the orphan
	// inside it is the proof the sweep did run).
	if _, err := os.Lstat(decoy); err != nil {
		t.Fatalf("Lstat(%s) = %v, want a directory outside the injected root untouched", decoy, err)
	}
	if _, err := os.Lstat(mine); !os.IsNotExist(err) {
		t.Fatalf("Lstat(%s) = %v, want the orphan inside the injected root removed", mine, err)
	}
}

func TestTheSweepKeepsALeftoverWhoseRepositoryItDoesNotManage(t *testing.T) {
	// Arrange — a leftover in this daemon's own root whose `.git` file names a
	// repository the daemon manages nothing for. It is another daemon's tree,
	// and it may be a LIVE one.
	tmp := t.TempDir()
	stranger := initTarget(t)
	orphan := filepath.Join(tmp, rebaseWorktreeMarker+"stranger")
	gitRun(t, stranger, "worktree", "add", "-q", "--detach", filepath.Join(orphan, rebaseWorktreeLeaf))
	log := &rebaseLog{}
	e := newLoggingDriverRooted(t, log, tmp)

	// Act — an empty repository universe: nothing here is ours.
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert — kept, and said so.
	if _, err := os.Lstat(orphan); err != nil {
		t.Fatalf("Lstat(%s) = %v, want a foreign repository's tree kept", orphan, err)
	}
	got := log.matching("orphaned rebase worktree sweep")
	if len(got) != 1 || !strings.Contains(got[0], "kept_unknown_repo=1") || !strings.Contains(got[0], orphan) {
		t.Fatalf("summary = %v, want one line counting and naming the kept unknown-repo leftover %s", got, orphan)
	}
}

func TestTheSweepRemovesALeftoverOfARepositoryARetainedTreeProvesIsOurs(t *testing.T) {
	// Arrange — two leftovers of ONE repository: a live merge's retained tree,
	// which is what identifies that repository as this daemon's, and a dead
	// daemon's orphan of the same repository.
	tmp := t.TempDir()
	target := initTarget(t)
	live := filepath.Join(tmp, rebaseWorktreeMarker+"live")
	gitRun(t, target, "worktree", "add", "-q", "--detach", filepath.Join(live, rebaseWorktreeLeaf))
	dead := filepath.Join(tmp, rebaseWorktreeMarker+"dead")
	gitRun(t, target, "worktree", "add", "-q", "--detach", filepath.Join(dead, rebaseWorktreeLeaf))
	e := newRootedDriver(t, &recordingSink{}, tmp)

	// Act
	if err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{Retained: []string{filepath.Join(live, rebaseWorktreeLeaf)}}); err != nil {
		t.Fatalf("SweepOrphanRebaseWorktrees: %v", err)
	}

	// Assert
	if _, err := os.Lstat(dead); !os.IsNotExist(err) {
		t.Fatalf("Lstat(%s) = %v, want the orphan of a repository a retained tree vouches for removed", dead, err)
	}
}

func TestTheSweepSurfacesADirectoryItCouldNotRemove(t *testing.T) {
	// Arrange — an orphan nothing may unlink.
	tmp := t.TempDir()
	orphan := arrangeOrphanDir(t, tmp, "stuck")
	if err := os.Chmod(orphan, 0o555); err != nil {
		t.Fatalf("Chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(orphan, 0o755) })
	e := newRootedDriver(t, &recordingSink{}, tmp)

	// Act
	err := e.SweepOrphanRebaseWorktrees(context.Background(), SweepScope{})

	// Assert — a leak the sweep could not clear is REPORTED, never counted as
	// swept.
	if err == nil || !strings.Contains(err.Error(), orphan) {
		t.Fatalf("sweep error = %v, want the undeletable orphan %s named", err, orphan)
	}
}

// --- the retention set --------------------------------------------------

func TestAConflictParkedRunRetainsItsRebaseWorktree(t *testing.T) {
	// Arrange — a merge parked on a conflict. Its tree is the resolution's
	// workbench, and the sweep learns that from here and nowhere else.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act — the park is established by the time the resolution is handed out.
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Assert
	got := h.coord.RetainedRebaseWorktrees()
	if len(got) != 1 || got[0] != testRebaseWorkDir {
		t.Fatalf("retained = %v, want the parked merge's worktree %s", got, testRebaseWorkDir)
	}
}

func TestARetiredRunNoLongerRetainsItsRebaseWorktree(t *testing.T) {
	// Arrange — one merge that reaches its terminal, and a second behind it
	// whose dispatch is the proof the first's runOne returned.
	h := newHarness(t)
	first, second := testRequest("a"), testRequest("b")
	if _, err := h.coord.Enqueue(context.Background(), first); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Act
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	if got := <-h.picker.merges; !sameRequest(got, second) {
		t.Fatalf("next merge = %+v, want %+v", got, second)
	}

	// Assert — a tree whose teardown has run must not go on protecting itself
	// from the sweep, or one failed removal would be retained forever.
	if got := h.coord.RetainedRebaseWorktrees(); len(got) != 0 {
		t.Fatalf("retained = %v, want nothing held by a retired run", got)
	}
}

func TestEvictionLeavesTheRunningMergesWorktreeStanding(t *testing.T) {
	// Arrange — the head is parked on a conflict and a second merge of the same
	// workspace waits behind it. An interrupt evicts only the WAITING one, which
	// never had a worktree to tear down.
	h := newHarness(t)
	first, second := testRequest("a"), testRequest("a")
	second.Name = "a2"
	if _, err := h.coord.Enqueue(context.Background(), first); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue(a2): %v", err)
	}
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Act
	evicted, err := h.coord.Evict(context.Background(), first.Workspace)
	if err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert — the parked tree still belongs to the merge that is holding it, so
	// a sweep racing the interrupt cannot take the resolution's workbench away.
	if evicted != 1 {
		t.Fatalf("evicted = %d, want only the waiting entry", evicted)
	}
	if got := h.coord.RetainedRebaseWorktrees(); len(got) != 1 || got[0] != testRebaseWorkDir {
		t.Fatalf("retained = %v, want the running merge's worktree %s still held", got, testRebaseWorkDir)
	}
}

func TestRebaseWorktreeRepoReadsTheRepositoryOffTheOrphan(t *testing.T) {
	tests := []struct {
		name    string
		gitFile string
		want    string
		wantOK  bool
	}{
		{
			name:    "the linked worktree git file git writes",
			gitFile: "gitdir: /Users/x/repo/.git/worktrees/rebase\n",
			want:    "/Users/x/repo",
			wantOK:  true,
		},
		{
			// A guessed repository would be handed to `git worktree prune`,
			// which is a mutation of somebody else's repository.
			name:    "anything that is not that exact shape",
			gitFile: "gitdir: /Users/x/repo/.git\n",
			wantOK:  false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			leaf := t.TempDir()
			if err := os.WriteFile(filepath.Join(leaf, ".git"), []byte(tt.gitFile), 0o644); err != nil {
				t.Fatalf("WriteFile: %v", err)
			}

			// Act
			got, ok := rebaseWorktreeRepo(leaf)

			// Assert
			if ok != tt.wantOK || (tt.wantOK && got != tt.want) {
				t.Fatalf("rebaseWorktreeRepo() = (%q, %v), want (%q, %v)", got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestRebaseWorktreeRepoReportsNothingForAnOrphanWithNoGitFile(t *testing.T) {
	// Arrange — the common leftover: an empty parent whose leaf is long gone.
	leaf := t.TempDir()

	// Act
	got, ok := rebaseWorktreeRepo(leaf)

	// Assert
	if ok {
		t.Fatalf("rebaseWorktreeRepo() = (%q, true), want no repository named", got)
	}
}
