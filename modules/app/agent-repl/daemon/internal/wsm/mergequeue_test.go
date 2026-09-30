package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
	"time"
)

func TestAdmitMergeRefusesAnEntryNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.AdmitMerge(context.Background(), RepoKey(t.TempDir()), ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("AdmitMerge = %v, want ErrNotFound", err)
	}
}

func TestRemoveMergeQueueEntryRefusesAnEntryNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.RemoveMergeQueueEntry(context.Background(), RepoKey(t.TempDir()), ws.ID, "gone")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RemoveMergeQueueEntry = %v, want ErrNotFound", err)
	}
}

func TestMergeQueueReportsAnEmptyQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.MergeQueue(context.Background(), RepoKey(t.TempDir()))

	// Assert
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("queue = %+v, want none", got)
	}
}

func TestMergeQueuePausedRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())

	// Act
	if err := s.SetMergeQueuePaused(context.Background(), repo, true); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}
	paused, err := s.MergeQueuePaused(context.Background(), repo)

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePaused: %v", err)
	}
	if !paused {
		t.Fatalf("paused = false after SetMergeQueuePaused(true)")
	}
}

func TestMergeQueuePausedResumes(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if err := s.SetMergeQueuePaused(context.Background(), repo, true); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}

	// Act
	if err := s.SetMergeQueuePaused(context.Background(), repo, false); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}
	paused, err := s.MergeQueuePaused(context.Background(), repo)

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePaused: %v", err)
	}
	if paused {
		t.Fatalf("paused = true after resuming")
	}
}

func TestMergeQueuePausedIsFalseForAnUntouchedRepo(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	paused, err := s.MergeQueuePaused(context.Background(), RepoKey(t.TempDir()))

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePaused: %v", err)
	}
	if paused {
		t.Fatalf("paused = true for a repo that was never paused")
	}
}

// ownBranch is the source every test that is not about sources requests.
var ownBranch = MergeSource{Kind: MergeSourceOwnBranch}

// queued requests a workspace's merge and moves it into line, answering its
// place.
func queued(t *testing.T, s *store, repo RepoKey, id WorkspaceID, at time.Time) int {
	t.Helper()
	if err := s.RequestMerge(context.Background(), repo, id, ownBranch, at); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	position, err := s.QueueMerge(context.Background(), repo, id)
	if err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
	return position
}

func TestQueueMergeNumbersPlacesInOrder(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)

	// Act
	firstPosition := queued(t, s, repo, first.ID, instant)
	secondPosition := queued(t, s, repo, second.ID, instant.Add(time.Minute))

	// Assert
	if firstPosition != 1 || secondPosition != 2 {
		t.Fatalf("positions = %d and %d, want 1 and 2", firstPosition, secondPosition)
	}
}

func TestQueueMergePlacesARequestAtTheBackWhenItIsQueued(t *testing.T) {
	// Arrange: the first merge is requested BEFORE the second, but queued after.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	early := testWorkspace(t, s)
	late := testWorkspace(t, s)
	if err := s.RequestMerge(context.Background(), repo, early.ID, ownBranch, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	queued(t, s, repo, late.ID, instant.Add(time.Minute))

	// Act
	position, err := s.QueueMerge(context.Background(), repo, early.ID)

	// Assert
	if err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
	if position != 2 {
		t.Fatalf("position = %d, want 2: a merge's place is taken when it is queued", position)
	}
}

func TestQueueMergeCountsNoRequestInLine(t *testing.T) {
	// Arrange: a request ahead in sequence is in nobody's line.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	pending := testWorkspace(t, s)
	ws := testWorkspace(t, s)
	if err := s.RequestMerge(context.Background(), repo, pending.ID, ownBranch, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act
	position := queued(t, s, repo, ws.ID, instant.Add(time.Minute))

	// Assert
	if position != 1 {
		t.Fatalf("position = %d, want 1: a request is not in line", position)
	}
}

func TestQueueMergeRefusesAMergeThatWasNeverRequested(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.QueueMerge(context.Background(), RepoKey(t.TempDir()), ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("QueueMerge = %v, want ErrNotFound", err)
	}
}

func TestQueueMergeRefusesAMergeAlreadyInLine(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)

	// Act
	_, err := s.QueueMerge(context.Background(), repo, ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("QueueMerge of a queued merge = %v, want ErrNotFound", err)
	}
}

func TestRequestMergeRefusesADuplicate(t *testing.T) {
	tests := []struct {
		name  string
		queue bool
		want  MergeQueueState
	}{
		{name: "of a request", queue: false, want: MergeRequested},
		{name: "of a queued merge", queue: true, want: MergeQueued},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			repo := RepoKey(t.TempDir())
			ws := testWorkspace(t, s)
			if err := s.RequestMerge(context.Background(), repo, ws.ID, ownBranch, instant); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			if tt.queue {
				if _, err := s.QueueMerge(context.Background(), repo, ws.ID); err != nil {
					t.Fatalf("QueueMerge: %v", err)
				}
			}

			// Act
			err := s.RequestMerge(context.Background(), repo, ws.ID, ownBranch, instant)

			// Assert
			var refusal *MergeQueuedError
			if !errors.As(err, &refusal) {
				t.Fatalf("RequestMerge = %v, want a *MergeQueuedError", err)
			}
			if refusal.Position != 1 || refusal.State != tt.want {
				t.Fatalf("refusal = %+v, want place 1 in state %s", refusal, tt.want)
			}
			if !loggedOperation(log, "daemon.wsm.request_merge", "error") {
				t.Fatalf("the refusal was not logged at error: %v", log.Records())
			}
		})
	}
}

func TestRequestMergeKeysOneQueuePerRepoSpelling(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	base := t.TempDir()
	ws := testWorkspace(t, s)
	if err := s.RequestMerge(context.Background(), RepoKey(base), ws.ID, ownBranch, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act
	err := s.RequestMerge(context.Background(), RepoKey(base+string(filepath.Separator)), ws.ID, ownBranch, instant)

	// Assert
	var refusal *MergeQueuedError
	if !errors.As(err, &refusal) {
		t.Fatalf("RequestMerge with a second spelling = %v, want the duplicate refusal", err)
	}
}

func TestRequestMergeRefusesAnUnregisteredWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.RequestMerge(context.Background(), RepoKey(t.TempDir()), WorkspaceID("absent"), ownBranch, instant)

	// Assert
	if err == nil {
		t.Fatalf("RequestMerge for an unregistered workspace succeeded")
	}
}

func TestRequestMergeRefusesASourceThatContradictsItself(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "an undeclared kind", source: MergeSource{Kind: MergeSourceKind(9)}},
		{name: "keep_open off the own branch", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", KeepOpen: true}},
		{name: "a workspace source naming no workspace", source: MergeSource{Kind: MergeSourceWorkspace}},
		{name: "a branch source naming no branch", source: MergeSource{Kind: MergeSourceBranch}},
		{name: "an own branch naming a branch", source: MergeSource{Kind: MergeSourceOwnBranch, Branch: "b"}},
		{name: "merged upstream naming a workspace", source: MergeSource{Kind: MergeSourceMergedUpstream, Workspace: "w"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.RequestMerge(context.Background(), RepoKey(t.TempDir()), ws.ID, tt.source, instant)

			// Assert
			if err == nil {
				t.Fatalf("RequestMerge(%+v) succeeded, want a refusal", tt.source)
			}
			if !loggedOperation(log, "daemon.wsm.request_merge", "error") {
				t.Fatalf("the refusal was not logged at error: %v", log.Records())
			}
		})
	}
}

func TestMergeQueueRoundTripsEverySource(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "own branch", source: MergeSource{Kind: MergeSourceOwnBranch}},
		{name: "own branch kept open", source: MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true}},
		{name: "another workspace", source: MergeSource{Kind: MergeSourceWorkspace, Workspace: "0123456789abcdef"}},
		{name: "a branch", source: MergeSource{Kind: MergeSourceBranch, Branch: "agent-1a2b/fix"}},
		{name: "merged upstream", source: MergeSource{Kind: MergeSourceMergedUpstream}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			ws := testWorkspace(t, s)
			if err := s.RequestMerge(context.Background(), repo, ws.ID, tt.source, instant); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}

			// Act
			got, err := s.MergeQueue(context.Background(), repo)

			// Assert
			if err != nil {
				t.Fatalf("MergeQueue: %v", err)
			}
			if len(got) != 1 || got[0].Source != tt.source || got[0].State != MergeRequested {
				t.Fatalf("queue = %+v, want one requested entry with source %+v", got, tt.source)
			}
		})
	}
}

func TestAdmitMergeMarksTheRunningEntry(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)

	// Act
	if err := s.AdmitMerge(context.Background(), repo, ws.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 1 || got[0].State != MergeAdmitted {
		t.Fatalf("queue = %+v, want one admitted entry", got)
	}
}

func TestRemoveMergeQueueEntryRenumbersTheRest(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)
	queued(t, s, repo, first.ID, instant)
	queued(t, s, repo, second.ID, instant.Add(time.Minute))

	// Act
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, first.ID, "evicted"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 1 || got[0].Workspace != second.ID || got[0].Position != 1 {
		t.Fatalf("queue = %+v, want the survivor at position 1", got)
	}
}

func TestRemoveMergeQueueEntryLogsTheCause(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)

	// Act
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, ws.ID, "dequeued by the user"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Assert
	for _, record := range log.Records() {
		if record.Operation == "daemon.wsm.remove_merge_queue_entry" && record.Context["cause"] == "dequeued by the user" {
			return
		}
	}
	t.Fatalf("the removal's cause was not logged: %v", log.Records())
}

func TestAllMergeQueuesListsEveryRepoForTheBootReenqueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	firstRepo := RepoKey(t.TempDir())
	secondRepo := RepoKey(t.TempDir())
	queued(t, s, firstRepo, testWorkspace(t, s).ID, instant)
	queued(t, s, secondRepo, testWorkspace(t, s).ID, instant)

	// Act
	got, err := s.AllMergeQueues(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("AllMergeQueues: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("loaded %d queues, want 2", len(got))
	}
	for repo, entries := range got {
		if len(entries) != 1 || entries[0].Position != 1 {
			t.Fatalf("queue %q = %+v, want one entry at position 1", repo, entries)
		}
	}
}

func TestAllMergeQueuesFailsWholeOnAnUndecodableRow(t *testing.T) {
	tests := []struct {
		name  string
		query string
		field string
	}{
		{name: "an undeclared state", query: `UPDATE merge_queue SET state = 99 WHERE workspace_id = ?`, field: "state"},
		{name: "an undeclared source", query: `UPDATE merge_queue SET source_kind = 99 WHERE workspace_id = ?`, field: "source"},
		{name: "a source contradicting itself", query: `UPDATE merge_queue SET source_branch = 'b' WHERE workspace_id = ?`, field: "source"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			queued(t, s, RepoKey(t.TempDir()), ws.ID, instant)
			corrupt(t, s, tt.query, ws.ID)

			// Act
			got, err := s.AllMergeQueues(context.Background())

			// Assert
			var refusal *DecodeError
			if !errors.As(err, &refusal) || refusal.Table != "merge_queue" || refusal.Field != tt.field {
				t.Fatalf("AllMergeQueues = %v, want a *DecodeError naming merge_queue.%s", err, tt.field)
			}
			if got != nil {
				t.Fatalf("loaded %d queues alongside the refusal, want none", len(got))
			}
			if !loggedOperation(log, "daemon.wsm.all_merge_queues", "error") {
				t.Fatalf("the decode failure was not logged at error: %v", log.Records())
			}
		})
	}
}

func TestForgetDeletesAWorkspacesQueueEntry(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("queue = %+v after the nuke, want none", got)
	}
}
