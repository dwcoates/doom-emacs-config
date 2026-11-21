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

// standingKind is where a fixture's merge stands in its queue.
type standingKind int

const (
	standsAtWork standingKind = iota + 1
	standsWaiting
	standsAsideForTests
	standsAsideForConflicts
	standsAsideForWaitingConflicts
)

// resolvingFixture queues a head and a merge behind it, and answers the one
// standing as KIND says: the head at work, the merge behind it waiting, the
// head stepped aside for its tests or its conflicts, or the merge behind it
// stepped aside from its waiting place for its conflicts.
func resolvingFixture(t *testing.T, kind standingKind) (*store, *dlog.TestLogger, RepoKey, WorkspaceID) {
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
	stepAside := func(id WorkspaceID, aside MergeAside) {
		if aside != MergeAsideWaitingConflicts {
			holdAsideLease(t, s, id, aside)
		}
		if _, err := s.StepAsideMerge(ctx, repo, id, aside); err != nil {
			t.Fatalf("StepAsideMerge: %v", err)
		}
	}
	switch kind {
	case standsAtWork:
		return s, logs, repo, head.ID
	case standsWaiting:
		return s, logs, repo, behind.ID
	case standsAsideForTests:
		stepAside(head.ID, MergeAsideTests)
		return s, logs, repo, head.ID
	case standsAsideForConflicts:
		stepAside(head.ID, MergeAsideConflicts)
		return s, logs, repo, head.ID
	case standsAsideForWaitingConflicts:
		stepAside(behind.ID, MergeAsideWaitingConflicts)
		return s, logs, repo, behind.ID
	}
	t.Fatalf("standingKind(%d) names no standing", kind)
	return nil, nil, "", ""
}

// recordResolved records the resolved rebase R on ws's entry.
func recordResolved(t *testing.T, s *store, repo RepoKey, ws WorkspaceID, r ResolvedRebase) {
	t.Helper()
	if err := s.RecordResolvedRebase(context.Background(), repo, ws, r); err != nil {
		t.Fatalf("RecordResolvedRebase: %v", err)
	}
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

func TestResolvesConflicts(t *testing.T) {
	tests := []struct {
		name  string
		state MergeQueueState
		aside MergeAside
		want  bool
	}{
		{name: "the merge at work may resolve in place", state: MergeAdmitted, aside: MergeAsideNone, want: true},
		{name: "a merge aside for its conflicts resolves them", state: MergeRemediating, aside: MergeAsideConflicts, want: true},
		{name: "a waiting merge aside for its conflicts resolves them", state: MergeRemediating, aside: MergeAsideWaitingConflicts, want: true},
		{name: "a merge aside for its tests resolves no conflict", state: MergeRemediating, aside: MergeAsideTests},
		{name: "a merge waiting in line rebases git-only", state: MergeQueued, aside: MergeAsideNone},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := resolvesConflicts(tc.state, tc.aside)

			// Assert
			if got != tc.want {
				t.Fatalf("resolvesConflicts(%s, %s) = %v, want %v", tc.state, tc.aside, got, tc.want)
			}
		})
	}
}

func TestRecordResolvedRebaseRecordsItOnAMergeResolvingItsConflicts(t *testing.T) {
	tests := []struct {
		name string
		kind standingKind
	}{
		{name: "the merge at work, resolving in place", kind: standsAtWork},
		{name: "a merge stepped aside from the head for its conflicts", kind: standsAsideForConflicts},
		{name: "a merge stepped aside from its waiting place for its conflicts", kind: standsAsideForWaitingConflicts},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _, repo, ws := resolvingFixture(t, tc.kind)

			// Act
			err := s.RecordResolvedRebase(context.Background(), repo, ws, theResolvedRebase)

			// Assert
			if err != nil {
				t.Fatalf("RecordResolvedRebase: %v", err)
			}
			if entry := entryOf(t, s, repo, ws); entry.Resolved == nil || *entry.Resolved != theResolvedRebase {
				t.Fatalf("resolved = %+v, want %+v", entry.Resolved, theResolvedRebase)
			}
		})
	}
}

func TestRecordResolvedRebaseReplacesTheOneItCarried(t *testing.T) {
	// Arrange
	s, _, repo, ws := resolvingFixture(t, standsAtWork)
	recordResolved(t, s, repo, ws, theResolvedRebase)
	later := ResolvedRebase{Tip: "tip-sha", BranchHead: "head-sha", Turn: "turn-2"}

	// Act
	err := s.RecordResolvedRebase(context.Background(), repo, ws, later)

	// Assert
	if err != nil {
		t.Fatalf("RecordResolvedRebase: %v", err)
	}
	if entry := entryOf(t, s, repo, ws); entry.Resolved == nil || *entry.Resolved != later {
		t.Fatalf("resolved = %+v, want %+v", entry.Resolved, later)
	}
}

func TestRecordResolvedRebaseRefusals(t *testing.T) {
	tests := []struct {
		name     string
		kind     standingKind
		resolved ResolvedRebase
		wantErr  error
	}{
		{name: "a merge stepped aside for its tests is refused", kind: standsAsideForTests, resolved: theResolvedRebase, wantErr: errResolvedRebaseRefused},
		{name: "a merge waiting in line is refused", kind: standsWaiting, resolved: theResolvedRebase, wantErr: errResolvedRebaseRefused},
		{name: "a resolved rebase with no branch head is refused", kind: standsAtWork, resolved: ResolvedRebase{Tip: "tip-sha", Turn: "turn-1"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, logs, repo, ws := resolvingFixture(t, tc.kind)
			before := entryOf(t, s, repo, ws)

			// Act
			err := s.RecordResolvedRebase(context.Background(), repo, ws, tc.resolved)

			// Assert
			if err == nil || (tc.wantErr != nil && !errors.Is(err, tc.wantErr)) {
				t.Fatalf("RecordResolvedRebase = %v, want a refusal (%v)", err, tc.wantErr)
			}
			rec, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.record_resolved_rebase"})
			if !ok || rec.Context["workspace"] != string(ws) || rec.Context["resolved_tip"] != tc.resolved.Tip || rec.Context["error"] == nil {
				t.Fatalf("ERROR record = %+v (found %v), want the workspace, the resolved rebase and the cause", rec, ok)
			}
			after := entryOf(t, s, repo, ws)
			if after.Resolved != nil || after.State != before.State {
				t.Fatalf("entry = %+v (resolved %+v), want it unchanged from %+v", after, after.Resolved, before)
			}
		})
	}
}

func TestRecordResolvedRebaseOfAWorkspaceWithNoEntryIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.RecordResolvedRebase(context.Background(), RepoKey(t.TempDir()), ws.ID, theResolvedRebase)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RecordResolvedRebase = %v, want ErrNotFound", err)
	}
}

// TestAReturnKeepsTheResolvedRebaseItsEntryCarries is the 2026-11-20
// gh-workflow-run-subscribe loss: a waiting merge's resolving rebase was cut
// while its turn resolved it, and its return to the line must keep the fact.
func TestAReturnKeepsTheResolvedRebaseItsEntryCarries(t *testing.T) {
	tests := []struct {
		name      string
		kind      standingKind
		wantState MergeQueueState
	}{
		{name: "a merge back from the head's conflicts", kind: standsAsideForConflicts, wantState: MergeQueued},
		{name: "a waiting merge back from its conflicts", kind: standsAsideForWaitingConflicts, wantState: MergeQueued},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _, repo, ws := resolvingFixture(t, tc.kind)
			recordResolved(t, s, repo, ws, theResolvedRebase)

			// Act
			_, err := returnRemediated(context.Background(), s, repo, ws, nil)

			// Assert
			if err != nil {
				t.Fatalf("ReturnRemediatedMerge: %v", err)
			}
			entry := entryOf(t, s, repo, ws)
			if entry.State != tc.wantState || entry.Resolved == nil || *entry.Resolved != theResolvedRebase {
				t.Fatalf("entry = %+v (resolved %+v), want it %s, still carrying %+v", entry, entry.Resolved, tc.wantState, theResolvedRebase)
			}
		})
	}
}

func TestAReturnWithNoResolvedRebaseLeavesTheEntryWithout(t *testing.T) {
	// Arrange
	s, _, repo, ws := resolvingFixture(t, standsAsideForConflicts)

	// Act
	_, err := returnRemediated(context.Background(), s, repo, ws, nil)

	// Assert
	if err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}
	if entry := entryOf(t, s, repo, ws); entry.Resolved != nil {
		t.Fatalf("resolved = %+v, want none", entry.Resolved)
	}
}

func TestARequeueKeepsTheResolvedRebase(t *testing.T) {
	// Arrange: the merge returned waiting, carrying its resolved rebase, then
	// headed the queue: it took a lease and was admitted as a head is.
	s, _, repo, ws := resolvingFixture(t, standsAsideForConflicts)
	recordResolved(t, s, repo, ws, theResolvedRebase)
	if _, err := returnRemediated(context.Background(), s, repo, ws, nil); err != nil {
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
			s, _, repo, ws := resolvingFixture(t, standsAtWork)
			if tc.carries {
				recordResolved(t, s, repo, ws, theResolvedRebase)
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
	s, _, repo, ws := resolvingFixture(t, standsAtWork)
	corrupt(t, s, `UPDATE merge_queue SET resolved_tip = 'tip-sha' WHERE workspace_id = ?`, string(ws))

	// Act
	_, err := s.MergeQueue(context.Background(), repo)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "resolved_tip" {
		t.Fatalf("MergeQueue = %v, want a DecodeError on resolved_tip", err)
	}
}
