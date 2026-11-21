package wsm

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// theResolvedRebase is the rebase a conflict repair worked on, as the merge
// records it.
var theResolvedRebase = ResolvedRebase{Tip: "tip-sha", BranchHead: "head-sha", Turn: "turn-1"}

// conflictAsideFixture queues a merge, admits it and steps it aside for
// ASIDE's repair, with a second merge waiting behind it.
func conflictAsideFixture(t *testing.T, aside MergeAside) (*store, *dlog.TestLogger, RepoKey, WorkspaceID) {
	t.Helper()
	s, logs := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	head, behind := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, head.ID, instant)
	queued(t, s, repo, behind.ID, instant.Add(time.Minute))
	if err := s.AdmitMerge(ctx, repo, head.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	holdAsideLease(t, s, head.ID, aside)
	if _, err := s.StepAsideMerge(ctx, repo, head.ID, aside); err != nil {
		t.Fatalf("StepAsideMerge: %v", err)
	}
	return s, logs, repo, head.ID
}

// entryOf answers ws's entry in repo's queue.
func entryOf(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) MergeQueueEntry {
	t.Helper()
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, entry := range entries {
		if entry.Workspace == ws {
			return entry
		}
	}
	t.Fatalf("no entry of %s in %+v", ws, entries)
	return MergeQueueEntry{}
}

func TestReturnRemediatedMergeRecordsTheResolvedRebaseOnTheEntry(t *testing.T) {
	// Arrange
	s, _, repo, ws := conflictAsideFixture(t, MergeAsideConflicts)
	resolved := theResolvedRebase

	// Act
	_, err := returnRemediatedWith(context.Background(), s, repo, ws, nil, &resolved)

	// Assert
	if err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}
	entry := entryOf(t, s, repo, ws)
	if entry.State != MergeQueued || entry.Resolved == nil || *entry.Resolved != theResolvedRebase {
		t.Fatalf("entry = %+v (resolved %+v), want it waiting carrying %+v", entry, entry.Resolved, theResolvedRebase)
	}
}

func TestReturnRemediatedMergeWithNoResolvedRebaseLeavesTheEntryWithout(t *testing.T) {
	// Arrange
	s, _, repo, ws := conflictAsideFixture(t, MergeAsideConflicts)

	// Act
	_, err := returnRemediatedWith(context.Background(), s, repo, ws, nil, nil)

	// Assert
	if err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}
	if entry := entryOf(t, s, repo, ws); entry.Resolved != nil {
		t.Fatalf("resolved = %+v, want none", entry.Resolved)
	}
}

func TestReturnRemediatedMergeRefusesAResolvedRebaseFromAnyRepairButConflicts(t *testing.T) {
	// Arrange
	s, _, repo, ws := conflictAsideFixture(t, MergeAsideTests)
	resolved := theResolvedRebase

	// Act
	_, err := returnRemediatedWith(context.Background(), s, repo, ws, nil, &resolved)

	// Assert
	if !errors.Is(err, errResolvedRebaseAside) {
		t.Fatalf("ReturnRemediatedMerge = %v, want the refusal %v", err, errResolvedRebaseAside)
	}
	entry := entryOf(t, s, repo, ws)
	if entry.State != MergeRemediating || entry.Resolved != nil {
		t.Fatalf("entry = %+v (resolved %+v), want it still remediating with no resolved rebase", entry, entry.Resolved)
	}
}

func TestReturnRemediatedMergeRefusesAMalformedResolvedRebase(t *testing.T) {
	// Arrange
	s, logs, repo, ws := conflictAsideFixture(t, MergeAsideConflicts)
	resolved := ResolvedRebase{Tip: "tip-sha", Turn: "turn-1"}

	// Act
	_, err := returnRemediatedWith(context.Background(), s, repo, ws, nil, &resolved)

	// Assert
	if err == nil {
		t.Fatal("ReturnRemediatedMerge = nil, want a refusal of a resolved rebase with no branch head")
	}
	rec, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.return_remediated_merge"})
	if !ok || rec.Context["workspace"] != string(ws) || rec.Context["error"] == nil {
		t.Fatalf("ERROR record = %+v (found %v), want the workspace and the cause", rec, ok)
	}
	if entry := entryOf(t, s, repo, ws); entry.State != MergeRemediating {
		t.Fatalf("entry = %+v, want it still remediating", entry)
	}
}

func TestARequeueKeepsTheResolvedRebase(t *testing.T) {
	// Arrange: the merge returned waiting, carrying its resolved rebase, then
	// headed the queue: it took a lease and was admitted as a head is.
	s, _, repo, ws := conflictAsideFixture(t, MergeAsideConflicts)
	resolved := theResolvedRebase
	if _, err := returnRemediatedWith(context.Background(), s, repo, ws, nil, &resolved); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}
	if err := s.AdmitMerge(context.Background(), repo, ws); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	lease, err := s.AcquireLease(context.Background(), ws, HolderMerge, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	err = s.RequeuePausedMerge(context.Background(), repo, ws, lease.ID, "ledger-1")

	// Assert
	if err != nil {
		t.Fatalf("RequeuePausedMerge: %v", err)
	}
	entry := entryOf(t, s, repo, ws)
	if entry.State != MergeQueued || entry.Resolved == nil || *entry.Resolved != theResolvedRebase {
		t.Fatalf("entry = %+v (resolved %+v), want it waiting, still carrying %+v", entry, entry.Resolved, theResolvedRebase)
	}
}

func TestClearResolvedRebase(t *testing.T) {
	tests := []struct {
		name        string
		carries     bool
		wantCleared bool
	}{
		{name: "an entry carrying a resolved rebase has it cleared", carries: true, wantCleared: true},
		{name: "an entry carrying none answers it carried none", carries: false, wantCleared: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _, repo, ws := conflictAsideFixture(t, MergeAsideConflicts)
			var resolved *ResolvedRebase
			if tc.carries {
				r := theResolvedRebase
				resolved = &r
			}
			if _, err := returnRemediatedWith(context.Background(), s, repo, ws, nil, resolved); err != nil {
				t.Fatalf("ReturnRemediatedMerge: %v", err)
			}

			// Act
			cleared, err := s.ClearResolvedRebase(context.Background(), repo, ws)

			// Assert
			if err != nil || cleared != tc.wantCleared {
				t.Fatalf("ClearResolvedRebase = %v, %v, want %v", cleared, err, tc.wantCleared)
			}
			if entry := entryOf(t, s, repo, ws); entry.Resolved != nil {
				t.Fatalf("resolved = %+v, want none", entry.Resolved)
			}
		})
	}
}

func TestClearResolvedRebaseOfAWorkspaceWithNoEntryIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.ClearResolvedRebase(context.Background(), RepoKey(t.TempDir()), ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("ClearResolvedRebase = %v, want ErrNotFound", err)
	}
}

func TestAResolvedRebaseMissingAColumnIsADecodeError(t *testing.T) {
	// Arrange: a row naming a resolved tip with no branch head or turn.
	s, _, repo, ws := conflictAsideFixture(t, MergeAsideConflicts)
	if _, err := returnRemediatedWith(context.Background(), s, repo, ws, nil, nil); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}
	corrupt(t, s, `UPDATE merge_queue SET resolved_tip = 'tip-sha' WHERE workspace_id = ?`, string(ws))

	// Act
	_, err := s.MergeQueue(context.Background(), repo)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "resolved_tip" {
		t.Fatalf("MergeQueue = %v, want a DecodeError on resolved_tip", err)
	}
}
