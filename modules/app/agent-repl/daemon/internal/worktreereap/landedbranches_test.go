package worktreereap

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// landedHead is the tip every arranged landed branch points at.
const landedHead = "1a2d000000000000000000000000000000000001"

// landedWorkspace registers a MERGED workspace whose worktree is gone, on
// BRANCH, cut from PARENT, and makes its branch exist at landedHead.
func landedWorkspace(w *world, repoDir, name, branch, parent string) wsm.Workspace {
	w.t.Helper()
	merged := longAgo.Add(time.Hour)
	ws := w.register(repoDir, filepath.Join(w.root, "trees", name), wsm.Workspace{
		Name: name, Branch: branch, ParentBranch: parent, Closed: true, MergedAt: &merged,
	})
	w.git.branches[branch] = true
	w.git.branchHeads["refs/heads/"+branch] = landedHead
	return ws
}

// TestTheSweepDeletesTheBranchesLandedWorkspacesLeftBehind covers the healing
// pass (sweepLandedBranches): a merged workspace's branch whose tip is inside
// the branch it landed into is deleted at that tip, and every branch that might
// still hold or own work is kept.
func TestTheSweepDeletesTheBranchesLandedWorkspacesLeftBehind(t *testing.T) {
	cases := []struct {
		name        string
		arrange     func(w *world, repoDir string)
		wantDeleted bool
		wantInto    string
	}{
		{
			name: "a landed branch inside its parent is deleted",
			arrange: func(w *world, repoDir string) {
				landedWorkspace(w, repoDir, "inv", "ramdisk-leak-investigation", "master")
			},
			wantDeleted: true,
			wantInto:    "master",
		},
		{
			name: "a landed workspace with no recorded parent is judged against the default branch",
			arrange: func(w *world, repoDir string) {
				landedWorkspace(w, repoDir, "inv", "ramdisk-leak-investigation", "")
			},
			wantDeleted: true,
			wantInto:    "main",
		},
		{
			name: "a landed branch holding commits its parent does not is kept",
			arrange: func(w *world, repoDir string) {
				landedWorkspace(w, repoDir, "inv", "ramdisk-leak-investigation", "master")
				w.git.notInside[landedHead] = true
			},
		},
		{
			name: "a landed name reused by an unmerged workspace keeps its branch",
			arrange: func(w *world, repoDir string) {
				landedWorkspace(w, repoDir, "inv", "ramdisk-leak-investigation", "master")
				w.register(repoDir, filepath.Join(w.root, "trees", "inv-again"), wsm.Workspace{
					Name: "ramdisk-leak-investigation", Branch: "ramdisk-leak-investigation", ParentBranch: "master", Closed: true,
				})
			},
		},
		{
			name: "a landed branch a worktree has checked out is kept",
			arrange: func(w *world, repoDir string) {
				landedWorkspace(w, repoDir, "inv", "ramdisk-leak-investigation", "master")
				w.addTree(repoDir, tree{name: "holder", branch: "ramdisk-leak-investigation", touched: now})
			},
		},
		{
			name: "a landed branch that is already gone is left alone",
			arrange: func(w *world, repoDir string) {
				landedWorkspace(w, repoDir, "inv", "ramdisk-leak-investigation", "master")
				w.git.branches["ramdisk-leak-investigation"] = false
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			repoDir := w.addRepo("doom")
			tc.arrange(w, repoDir)

			// Act
			report := w.sweep()

			// Assert
			deleted := w.git.saw("delete_branch ramdisk-leak-investigation " + landedHead)
			if deleted != tc.wantDeleted {
				t.Fatalf("deleted = %v, want %v; calls %v", deleted, tc.wantDeleted, w.git.called())
			}
			if tc.wantDeleted {
				if report.BranchesDeleted != 1 {
					t.Fatalf("BranchesDeleted = %d, want 1", report.BranchesDeleted)
				}
				record, ok := dlog.FindRecord(w.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opLandedBranch,
					Message: "deleted the landed branch: its tip is inside the ref it landed into"})
				if !ok {
					t.Fatalf("no INFO %s record of the deletion: %+v", opLandedBranch, w.log.Records())
				}
				if record.Context["into"] != tc.wantInto || record.Context["branch"] != "ramdisk-leak-investigation" {
					t.Fatalf("the record's context is %v, want the branch into %s", record.Context, tc.wantInto)
				}
			}
			if report.Failures != 0 {
				t.Fatalf("Failures = %d, want 0: %+v", report.Failures, w.log.Records())
			}
		})
	}
}

// TestTheSweepCountsALandedBranchItCouldNotJudge covers a git failure in the
// healing pass: the branch is kept, the failure is counted and recorded once
// at ERROR, and the sweep goes on.
func TestTheSweepCountsALandedBranchItCouldNotJudge(t *testing.T) {
	// Arrange
	w := newWorld(t)
	repoDir := w.addRepo("doom")
	landedWorkspace(w, repoDir, "inv", "ramdisk-leak-investigation", "master")
	w.git.ancestorErr = errors.New("git could not answer")

	// Act
	report := w.sweep()

	// Assert
	if w.git.saw("delete_branch ramdisk-leak-investigation " + landedHead) {
		t.Fatal("a branch whose landing could not be judged was deleted")
	}
	if report.Failures != 1 || report.Repositories != 1 {
		t.Fatalf("Failures = %d, Repositories = %d; want 1 and 1", report.Failures, report.Repositories)
	}
	if got := dlog.FilterRecords(w.log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opLandedBranch}); len(got) != 1 {
		t.Fatalf("want exactly one ERROR %s record, got %+v", opLandedBranch, got)
	}
}

// TestADryRunSweepDeletesNoLandedBranch covers the dry run: the healing pass
// deletes, so a dry run asks git nothing about landed branches.
func TestADryRunSweepDeletesNoLandedBranch(t *testing.T) {
	// Arrange
	w := newWorld(t)
	repoDir := w.addRepo("doom")
	landedWorkspace(w, repoDir, "inv", "ramdisk-leak-investigation", "master")

	// Act
	if _, err := w.reaper().SweepWith(context.Background(), Options{DryRun: true}); err != nil {
		t.Fatalf("SweepWith: %v", err)
	}

	// Assert
	if w.git.saw("delete_branch ramdisk-leak-investigation "+landedHead) || w.git.saw("is_ancestor "+landedHead+" master") {
		t.Fatalf("a dry run judged or deleted a landed branch: %v", w.git.called())
	}
}
