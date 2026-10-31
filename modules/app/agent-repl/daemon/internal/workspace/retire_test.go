package workspace

import (
	"context"
	"errors"
	"os"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// landedAt is the instant every arranged landing landed at.
var landedAt = time.Unix(1_700_000_000, 0).UTC()

// landedRequester arranges a requester whose detached merge landed at its
// current HEAD, in a real directory, quiet and clean.
func landedRequester(t *testing.T, f *fixture) wsm.Workspace {
	t.Helper()
	ws := f.workspace("w1", t.TempDir())
	ws.Landed = &wsm.Landing{Head: "sha-of-HEAD", At: landedAt}
	ws.ParentBranch = "master"
	f.db.with(ws)
	return ws
}

// retireRecords answers the opRetire records at LEVEL naming WS, leaving out
// the roster republish's own record.
func retireRecords(f *fixture, level string, ws ids.WorkspaceID) []dlog.Record {
	var out []dlog.Record
	for _, r := range f.log.logger.Records() {
		if r.Operation == opRetire && r.Level == level && r.Context["workspace"] == string(ws) && r.Message != "republished the roster registry" {
			out = append(out, r)
		}
	}
	return out
}

// assertNothingRetired fails unless the requester's session, worktree and
// record were all left as they were.
func assertNothingRetired(t *testing.T, f *fixture, ws ids.WorkspaceID) {
	t.Helper()
	assertNothingTornDown(t, f, ws)
	if f.db.workspaces[ws].MergedAt != nil {
		t.Fatal("the requester was stamped merged")
	}
}

// assertNothingTornDown fails unless the requester's session and worktree
// were left alone and it was not closed.
func assertNothingTornDown(t *testing.T, f *fixture, ws ids.WorkspaceID) {
	t.Helper()
	if len(f.shim.killedSession) != 0 || len(f.fleet.stopped) != 0 {
		t.Fatalf("session killed %v, stopped %+v; want it left alone", f.shim.killedSession, f.fleet.stopped)
	}
	if len(f.git.removedClean) != 0 {
		t.Fatalf("removed %+v, want the worktree kept", f.git.removedClean)
	}
	if f.db.closedFlags[ws] {
		t.Fatal("the requester was closed")
	}
}

func TestRetireLandedRetiresAQuietCleanRequesterStillAtItsLanding(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := landedRequester(t, f)

	// Act
	kept, err := f.verbs.RetireLanded(context.Background(), ws.ID)

	// Assert
	if err != nil || kept != "" {
		t.Fatalf("RetireLanded = %q, %v; want a retirement", kept, err)
	}
	if len(f.shim.killedSession) != 1 || len(f.fleet.stopped) != 1 {
		t.Fatalf("session killed %v, stopped %+v; want it stopped once", f.shim.killedSession, f.fleet.stopped)
	}
	if len(f.git.removedClean) != 1 || f.git.removedClean[0].WorktreeDir != ws.Dir || f.git.removedClean[0].RepoDir != f.db.repositories[0].Dir {
		t.Fatalf("removed %+v, want the requester's worktree through the clean door", f.git.removedClean)
	}
	if !f.db.closedFlags[ws.ID] {
		t.Fatal("the retired requester was not closed")
	}
	if merged := f.db.workspaces[ws.ID].MergedAt; merged == nil || !merged.Equal(landedAt) {
		t.Fatalf("MergedAt = %v, want the landing's instant %v", merged, landedAt)
	}
	if len(retireRecords(f, dlog.LevelInfo, ws.ID)) != 1 {
		t.Fatalf("want one INFO %s record naming the requester, got %+v", opRetire, f.log.logger.Records())
	}
}

func TestRetireLandedRemovesTheWorktreeOnlyAfterTheSessionStopped(t *testing.T) {
	// Arrange: the removal fails, so what ran before it is what is left.
	f := newFixture(t)
	ws := landedRequester(t, f)
	f.git.removeCleanErr = errors.New("fatal: contains modified or untracked files")

	// Act
	_, err := f.verbs.RetireLanded(context.Background(), ws.ID)

	// Assert
	if err == nil {
		t.Fatal("RetireLanded succeeded with a refused removal")
	}
	if len(f.fleet.stopped) != 1 {
		t.Fatalf("stopped %+v, want the session stopped before the removal", f.fleet.stopped)
	}
	if f.db.workspaces[ws.ID].MergedAt != nil {
		t.Fatal("a requester whose worktree stayed was stamped merged")
	}
	if len(retireRecords(f, dlog.LevelError, ws.ID)) != 1 {
		t.Fatalf("want one ERROR %s record naming the requester, got %+v", opRetire, f.log.logger.Records())
	}
}

func TestRetireLandedKeepsARequesterThatMightHoldWhatDidNotLand(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture, ws wsm.Workspace)
		want    string
	}{
		{name: "no landing", want: RetireKeptNotLanded, arrange: func(t *testing.T, f *fixture, ws wsm.Workspace) {
			ws.Landed = nil
			f.db.with(ws)
		}},
		{name: "already merged", want: RetireKeptNotLanded, arrange: func(t *testing.T, f *fixture, ws wsm.Workspace) {
			at := landedAt
			ws.MergedAt = &at
			f.db.with(ws)
		}},
		{name: "an open child", want: RetireKeptParent, arrange: func(t *testing.T, f *fixture, ws wsm.Workspace) {
			f.db.with(wsm.Workspace{ID: "child", Dir: t.TempDir(), Repo: "other-repo", Parent: &ws.ID})
		}},
		{name: "an open workspace cut from its branch", want: RetireKeptParent, arrange: func(t *testing.T, f *fixture, ws wsm.Workspace) {
			f.db.with(wsm.Workspace{ID: "child", Dir: t.TempDir(), Repo: ws.Repo, ParentBranch: ws.Branch})
		}},
		{name: "a held prompt", want: RetireKeptNotQuiet, arrange: func(t *testing.T, f *fixture, ws wsm.Workspace) {
			f.db.held[ws.ID] = []wsm.HeldPrompt{{}}
		}},
		{name: "its directory gone", want: RetireKeptMissing, arrange: func(t *testing.T, f *fixture, ws wsm.Workspace) {
			if err := os.RemoveAll(ws.Dir); err != nil {
				t.Fatalf("RemoveAll: %v", err)
			}
		}},
		{name: "a dirty worktree", want: RetireKeptDirty, arrange: func(t *testing.T, f *fixture, ws wsm.Workspace) {
			f.git.dirty = true
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := landedRequester(t, f)
			tt.arrange(t, f, ws)

			// Act
			kept, err := f.verbs.RetireLanded(context.Background(), ws.ID)

			// Assert
			if err != nil || kept != tt.want {
				t.Fatalf("RetireLanded = %q, %v; want kept %q", kept, err, tt.want)
			}
			assertNothingTornDown(t, f, ws.ID)
		})
	}
}

func TestRetireLandedLeavesAClosedChildOutOfTheParentGate(t *testing.T) {
	// Arrange: the merge workspace itself is cut from the requester, and is
	// closed once its merge lands.
	f := newFixture(t)
	ws := landedRequester(t, f)
	f.db.with(wsm.Workspace{ID: "merge-ws", Dir: t.TempDir(), Repo: ws.Repo, Parent: &ws.ID, Closed: true})

	// Act
	kept, err := f.verbs.RetireLanded(context.Background(), ws.ID)

	// Assert
	if err != nil || kept != "" {
		t.Fatalf("RetireLanded = %q, %v; want a retirement", kept, err)
	}
}

func TestRetireLandedClearsTheLandingOfARequesterThatMovedPastIt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := landedRequester(t, f)
	f.git.heads = map[string]string{ws.Dir: "a-later-commit"}

	// Act
	kept, err := f.verbs.RetireLanded(context.Background(), ws.ID)

	// Assert
	if err != nil || kept != RetireKeptMovedOn {
		t.Fatalf("RetireLanded = %q, %v; want kept %q", kept, err, RetireKeptMovedOn)
	}
	if len(f.db.landedWrites) != 1 || f.db.landedWrites[0] != nil {
		t.Fatalf("landing writes = %+v, want the landing cleared once", f.db.landedWrites)
	}
	assertNothingRetired(t, f, ws.ID)
}

func TestRetireLandedFailsLoudlyAndKeepsTheRequesterWhenAReadFails(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture, ws wsm.Workspace)
	}{
		{name: "the workspaces", arrange: func(f *fixture, _ wsm.Workspace) {
			f.db.listWorkspacesErr = errors.New("database is locked")
		}},
		{name: "the HEAD", arrange: func(f *fixture, _ wsm.Workspace) {
			f.git.resolveErr = errors.New("fatal: not a git repository")
		}},
		{name: "the cleanliness", arrange: func(f *fixture, _ wsm.Workspace) {
			f.git.isCleanErr = errors.New("fatal: index file corrupt")
		}},
		{name: "the repository", arrange: func(f *fixture, _ wsm.Workspace) {
			f.db.listRepositoriesErr = errors.New("database is locked")
		}},
		{name: "the landing's clearing", arrange: func(f *fixture, ws wsm.Workspace) {
			f.git.heads = map[string]string{ws.Dir: "a-later-commit"}
			f.db.setLandedErr = errors.New("database is locked")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := landedRequester(t, f)
			tt.arrange(f, ws)

			// Act
			_, err := f.verbs.RetireLanded(context.Background(), ws.ID)

			// Assert
			if err == nil {
				t.Fatal("RetireLanded succeeded over a failed read")
			}
			if len(retireRecords(f, dlog.LevelError, ws.ID)) != 1 {
				t.Fatalf("want one ERROR %s record naming the requester, got %+v", opRetire, f.log.logger.Records())
			}
			assertNothingRetired(t, f, ws.ID)
		})
	}
}

func TestRetireLandedFailsLoudlyWhenTheMergedStampCannotBeWritten(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := landedRequester(t, f)
	f.db.setMergedErr = errors.New("database is locked")

	// Act
	_, err := f.verbs.RetireLanded(context.Background(), ws.ID)

	// Assert: the tree is gone and the workspace closed, so an open restores
	// the worktree from its kept branch rather than finding it merged.
	if err == nil {
		t.Fatal("RetireLanded succeeded with an unwritten merged stamp")
	}
	if len(retireRecords(f, dlog.LevelError, ws.ID)) != 1 {
		t.Fatalf("want one ERROR %s record naming the requester, got %+v", opRetire, f.log.logger.Records())
	}
	if f.db.workspaces[ws.ID].MergedAt != nil {
		t.Fatal("the requester reads merged after its stamp failed")
	}
}

func TestRetireLandedRefusesAWorkspaceThisDaemonDoesNotServe(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := landedRequester(t, f)
	f.owner.standing = StandingTransferringAway

	// Act
	_, err := f.verbs.RetireLanded(context.Background(), ws.ID)

	// Assert
	var refusal *Refusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmTransferringAway {
		t.Fatalf("RetireLanded = %v, want the transferring-away refusal", err)
	}
	assertNothingRetired(t, f, ws.ID)
}

// TestRetireLandedDeletesTheLandedBranch covers the branch going with the
// retired requester's worktree (landedbranch.Retire): deleted once its tip is
// inside the branch it landed into, kept when it holds anything that did not
// land.
func TestRetireLandedDeletesTheLandedBranch(t *testing.T) {
	cases := []struct {
		name        string
		offParent   bool
		wantDeleted bool
		wantLevel   string
		wantMessage string
	}{
		{
			name:        "a branch inside its parent is deleted at its tip",
			wantDeleted: true,
			wantLevel:   dlog.LevelInfo,
			wantMessage: "deleted the landed branch: its tip is inside the ref it landed into",
		},
		{
			name:        "a branch holding commits its parent does not is kept",
			offParent:   true,
			wantLevel:   dlog.LevelDebug,
			wantMessage: "kept the branch: it holds commits the ref it landed into does not",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := landedRequester(t, f)
			f.git.existingBranches = map[string]bool{ws.Branch: true}
			f.git.offPrimary = map[string]bool{"sha-of-refs/heads/" + ws.Branch: tc.offParent}

			// Act
			kept, err := f.verbs.RetireLanded(context.Background(), ws.ID)

			// Assert
			if err != nil || kept != "" {
				t.Fatalf("RetireLanded = %q, %v; want a retirement", kept, err)
			}
			var want []string
			if tc.wantDeleted {
				want = []string{ws.Branch + "@sha-of-refs/heads/" + ws.Branch}
			}
			if len(f.git.deletedBranches) != len(want) || (len(want) == 1 && f.git.deletedBranches[0] != want[0]) {
				t.Fatalf("deleted %v, want %v", f.git.deletedBranches, want)
			}
			record, ok := dlog.FindRecord(retireRecords(f, tc.wantLevel, ws.ID), dlog.RecordQuery{Message: tc.wantMessage})
			if !ok {
				t.Fatalf("no %s %s record %q: %+v", tc.wantLevel, opRetire, tc.wantMessage, f.log.logger.Records())
			}
			if record.Context["into"] != "master" || record.Context["branch"] != ws.Branch {
				t.Fatalf("the record's context is %v, want branch %s into master", record.Context, ws.Branch)
			}
		})
	}
}
