package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"slices"
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
	position, err := s.QueueMerge(context.Background(), repo, id, NewLeaseID())
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
	position, err := s.QueueMerge(context.Background(), repo, early.ID, NewLeaseID())

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

// frontQueued requests a FRONT merge and moves it into line, answering its
// place.
func frontQueued(t *testing.T, s *store, repo RepoKey, id WorkspaceID, at time.Time) int {
	t.Helper()
	if err := s.RequestMerge(context.Background(), repo, id, MergeSource{Kind: MergeSourceOwnBranch, Front: true}, at); err != nil {
		t.Fatalf("RequestMerge(front): %v", err)
	}
	position, err := s.QueueMerge(context.Background(), repo, id, NewLeaseID())
	if err != nil {
		t.Fatalf("QueueMerge(front): %v", err)
	}
	return position
}

// queueOrder answers a repository's queue as its workspaces, in line order.
func queueOrder(t *testing.T, s *store, repo RepoKey) []WorkspaceID {
	t.Helper()
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	out := make([]WorkspaceID, 0, len(entries))
	for _, e := range entries {
		out = append(out, e.Workspace)
	}
	return out
}

func TestQueueMergePutsAFrontMergeAheadOfEveryWaitingMerge(t *testing.T) {
	// Arrange: two ordinary merges wait.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	a, b, f := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, a.ID, instant)
	queued(t, s, repo, b.ID, instant.Add(time.Minute))

	// Act
	position := frontQueued(t, s, repo, f.ID, instant.Add(2*time.Minute))

	// Assert
	if got, want := queueOrder(t, s, repo), []WorkspaceID{f.ID, a.ID, b.ID}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
	if position != 1 {
		t.Fatalf("position = %d, want 1", position)
	}
}

func TestQueueMergeNeverPutsAFrontMergeAheadOfTheAdmittedMerge(t *testing.T) {
	// Arrange: one merge runs and one waits.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	running, waiting, f := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, running.ID, instant)
	if err := s.AdmitMerge(context.Background(), repo, running.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	queued(t, s, repo, waiting.ID, instant.Add(time.Minute))

	// Act
	frontQueued(t, s, repo, f.ID, instant.Add(2*time.Minute))

	// Assert
	if got, want := queueOrder(t, s, repo), []WorkspaceID{running.ID, f.ID, waiting.ID}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestQueueMergeKeepsFrontMergesInArrivalOrder(t *testing.T) {
	// Arrange: an ordinary merge waits behind an earlier front merge.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ordinary, first, second := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, ordinary.ID, instant)
	frontQueued(t, s, repo, first.ID, instant.Add(time.Minute))

	// Act
	frontQueued(t, s, repo, second.ID, instant.Add(2*time.Minute))

	// Assert
	if got, want := queueOrder(t, s, repo), []WorkspaceID{first.ID, second.ID, ordinary.ID}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestQueueMergePutsAFrontMergeAtTheBackWhenNoOrdinaryMergeWaits(t *testing.T) {
	// Arrange: the only merge is running.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	running, f := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, running.ID, instant)
	if err := s.AdmitMerge(context.Background(), repo, running.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	frontQueued(t, s, repo, f.ID, instant.Add(time.Minute))

	// Assert
	if got, want := queueOrder(t, s, repo), []WorkspaceID{running.ID, f.ID}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestQueueMergeLeavesARequestBehindAFrontMergeOutOfLine(t *testing.T) {
	// Arrange: a request sits behind the waiting merge the front merge passes.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	waiting, pending, f := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, waiting.ID, instant)
	if err := s.RequestMerge(context.Background(), repo, pending.ID, ownBranch, instant.Add(time.Minute)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act
	frontQueued(t, s, repo, f.ID, instant.Add(2*time.Minute))

	// Assert: the shift moved the request's place, never its state.
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, e := range entries {
		if e.Workspace == pending.ID && e.State != MergeRequested {
			t.Fatalf("the request behind the shift is %s, want still requested", e.State)
		}
	}
}

func TestMergeQueueCarriesAFrontRequest(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)

	// Act
	if err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch, Front: true}, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Assert
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(entries) != 1 || !entries[0].Source.Front {
		t.Fatalf("queue = %+v, want one entry carrying Front", entries)
	}
}

func TestQueueMergePutsAFrontMergeRequestedBeforeAWaitingMergeAheadOfIt(t *testing.T) {
	// Arrange: the front merge is ASKED FOR before the ordinary one is queued,
	// so its request holds an earlier sequence than the merge it must pass.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	f, ordinary := testWorkspace(t, s), testWorkspace(t, s)
	if err := s.RequestMerge(context.Background(), repo, f.ID, MergeSource{Kind: MergeSourceOwnBranch, Front: true}, instant); err != nil {
		t.Fatalf("RequestMerge(front): %v", err)
	}
	queued(t, s, repo, ordinary.ID, instant.Add(time.Minute))

	// Act
	position, err := s.QueueMerge(context.Background(), repo, f.ID, NewLeaseID())

	// Assert
	if err != nil {
		t.Fatalf("QueueMerge(front): %v", err)
	}
	if got, want := queueOrder(t, s, repo), []WorkspaceID{f.ID, ordinary.ID}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
	if position != 1 {
		t.Fatalf("position = %d, want 1", position)
	}
}

func TestMoveMergeToFrontPlacesTheMerge(t *testing.T) {
	// Each case builds a queue from steps over workspaces named by letter,
	// then moves the one named "m" to the front.
	type step struct {
		ws   string
		kind string // "queued", "front" or "admitted"
	}
	tests := []struct {
		name      string
		steps     []step
		wantOrder []string
		wantMoved bool
	}{
		{
			name:      "ahead of every ordinary merge waiting",
			steps:     []step{{"a", "queued"}, {"b", "queued"}, {"m", "queued"}},
			wantOrder: []string{"m", "a", "b"},
			wantMoved: true,
		},
		{
			name:      "the merges behind it keep their places",
			steps:     []step{{"a", "queued"}, {"m", "queued"}, {"b", "queued"}},
			wantOrder: []string{"m", "a", "b"},
			wantMoved: true,
		},
		{
			name:      "never ahead of the admitted merge",
			steps:     []step{{"r", "admitted"}, {"a", "queued"}, {"m", "queued"}},
			wantOrder: []string{"r", "m", "a"},
			wantMoved: true,
		},
		{
			name:      "behind a merge already put at the front",
			steps:     []step{{"a", "queued"}, {"f", "front"}, {"m", "queued"}},
			wantOrder: []string{"f", "m", "a"},
			wantMoved: true,
		},
		{
			name:      "a merge with no ordinary merge ahead stays put",
			steps:     []step{{"r", "admitted"}, {"m", "queued"}, {"a", "queued"}},
			wantOrder: []string{"r", "m", "a"},
			wantMoved: false,
		},
		{
			name:      "a merge already at the front stays put",
			steps:     []step{{"a", "queued"}, {"m", "front"}},
			wantOrder: []string{"m", "a"},
			wantMoved: false,
		},
		{
			name:      "the admitted merge stays put",
			steps:     []step{{"m", "admitted"}, {"a", "queued"}},
			wantOrder: []string{"m", "a"},
			wantMoved: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := map[string]WorkspaceID{}
			for i, st := range tt.steps {
				id := testWorkspace(t, s).ID
				named[st.ws] = id
				at := instant.Add(time.Duration(i) * time.Minute)
				switch st.kind {
				case "front":
					frontQueued(t, s, repo, id, at)
				default:
					queued(t, s, repo, id, at)
				}
				if st.kind == "admitted" {
					if err := s.AdmitMerge(context.Background(), repo, id); err != nil {
						t.Fatalf("AdmitMerge: %v", err)
					}
				}
			}

			// Act
			moved, err := s.MoveMergeToFront(context.Background(), repo, named["m"])

			// Assert
			if err != nil {
				t.Fatalf("MoveMergeToFront: %v", err)
			}
			if moved != tt.wantMoved {
				t.Fatalf("moved = %v, want %v", moved, tt.wantMoved)
			}
			want := make([]WorkspaceID, 0, len(tt.wantOrder))
			for _, name := range tt.wantOrder {
				want = append(want, named[name])
			}
			if got := queueOrder(t, s, repo); !slices.Equal(got, want) {
				t.Fatalf("queue = %v, want %v", got, want)
			}
		})
	}
}

func TestMoveMergeToFrontMarksAWaitingMergeAFrontMerge(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)

	// Act
	if _, err := s.MoveMergeToFront(context.Background(), repo, ws.ID); err != nil {
		t.Fatalf("MoveMergeToFront: %v", err)
	}

	// Assert
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(entries) != 1 || !entries[0].Source.Front {
		t.Fatalf("queue = %+v, want one entry carrying Front", entries)
	}
}

func TestMoveMergeToFrontPutsARequestAtTheFrontOnceItIsQueued(t *testing.T) {
	// Arrange: an ordinary merge waits, and a merge is only requested.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ordinary, pending := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, ordinary.ID, instant)
	if err := s.RequestMerge(context.Background(), repo, pending.ID, ownBranch, instant.Add(time.Minute)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	moved, err := s.MoveMergeToFront(context.Background(), repo, pending.ID)
	if err != nil || moved {
		t.Fatalf("MoveMergeToFront = %v, %v; want unmoved", moved, err)
	}

	// Act
	if _, err := s.QueueMerge(context.Background(), repo, pending.ID, NewLeaseID()); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}

	// Assert
	if got, want := queueOrder(t, s, repo), []WorkspaceID{pending.ID, ordinary.ID}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestMoveMergeToFrontRefusesAWorkspaceNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)

	// Act
	_, err := s.MoveMergeToFront(context.Background(), repo, ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("MoveMergeToFront = %v, want ErrNotFound", err)
	}
}

func TestQueueMergeRefusesAMergeThatWasNeverRequested(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.QueueMerge(context.Background(), RepoKey(t.TempDir()), ws.ID, NewLeaseID())

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
	_, err := s.QueueMerge(context.Background(), repo, ws.ID, NewLeaseID())

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
				if _, err := s.QueueMerge(context.Background(), repo, ws.ID, NewLeaseID()); err != nil {
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

func TestRequestMergeKeepsTheBranchAWorkspaceSourceWasRequestedWith(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	source := MergeSource{Kind: MergeSourceOwnBranch, Branch: "renamed"}

	// Act
	err := s.RequestMerge(context.Background(), repo, ws.ID, source, instant)

	// Assert
	if err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(entries) != 1 || entries[0].Source != source {
		t.Fatalf("MergeQueue = %+v, %v, want the source with its branch", entries, err)
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
		{name: "a source contradicting itself", query: `UPDATE merge_queue SET source_kind = 2, source_branch = '' WHERE workspace_id = ?`, field: "source"},
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

func TestQueueMergeKeepsTheBubblesLedgerOnTheRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	if err := s.RequestMerge(context.Background(), repo, ws.ID, ownBranch, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	ledger := NewLeaseID()

	// Act
	if _, err := s.QueueMerge(context.Background(), repo, ws.ID, ledger); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
	entries, err := s.MergeQueue(context.Background(), repo)

	// Assert
	if err != nil || len(entries) != 1 || entries[0].Ledger != ledger {
		t.Fatalf("queue = %+v (%v), want the entry carrying ledger %s", entries, err, ledger)
	}
}

func TestQueueMergeRefusesAMergeWithNoLedger(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	if err := s.RequestMerge(context.Background(), repo, ws.ID, ownBranch, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act
	_, err := s.QueueMerge(context.Background(), repo, ws.ID, "")

	// Assert
	if err == nil {
		t.Fatal("QueueMerge put a merge in line with no ledger identity, want a refusal")
	}
	entries, _ := s.MergeQueue(context.Background(), repo)
	if len(entries) != 1 || entries[0].State != MergeRequested {
		t.Fatalf("queue = %+v, want the request left as it was", entries)
	}
}

func TestARequestedMergeCarriesNoLedger(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)

	// Act
	if err := s.RequestMerge(context.Background(), repo, ws.ID, ownBranch, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	entries, err := s.MergeQueue(context.Background(), repo)

	// Assert
	if err != nil || len(entries) != 1 || entries[0].Ledger != "" {
		t.Fatalf("queue = %+v (%v), want a request with no ledger yet", entries, err)
	}
}

// oneShotRequester registers a workspace with a creation job that is not yet a
// one-shot, the state a workspace announcing a one-shot merge starts from.
func oneShotRequester(t *testing.T, s *store) Workspace {
	t.Helper()
	ws := testWorkspace(t, s)
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, CreatedAt: instant}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}
	return ws
}

// markedOneShot reads whether the workspace's creation job is a one-shot.
func markedOneShot(t *testing.T, s *store, id WorkspaceID) bool {
	t.Helper()
	job, found, err := s.CreationJob(context.Background(), id)
	if err != nil || !found {
		t.Fatalf("CreationJob = %+v, %v, %v, want the recorded job", job, found, err)
	}
	return job.OneShot
}

func TestRequestOneShotMergeRecordsTheRequestAndMarksTheOneShot(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := oneShotRequester(t, s)

	// Act
	err := s.RequestOneShotMerge(context.Background(), repo, ws.ID, ownBranch, instant)

	// Assert
	if err != nil {
		t.Fatalf("RequestOneShotMerge: %v", err)
	}
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(entries) != 1 || entries[0].State != MergeRequested || entries[0].Source != ownBranch {
		t.Fatalf("MergeQueue = %+v, %v, want one requested own-branch merge", entries, err)
	}
	if !markedOneShot(t, s, ws.ID) {
		t.Fatalf("the requester was not marked a one-shot")
	}
	if record, err := s.Workspace(context.Background(), ws.ID); err != nil || !record.OneShot {
		t.Fatalf("Workspace = %+v, %v, want the roster's one-shot fact set", record, err)
	}
}

func TestRequestOneShotMergeRefusesASourceThatIsNoOneShot(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "kept open", source: MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true}},
		{name: "detached", source: MergeSource{Kind: MergeSourceOwnBranch, Detach: NewDetachedMergeID()}},
		{name: "another branch", source: MergeSource{Kind: MergeSourceBranch, Branch: "b"}},
		{name: "merged upstream", source: MergeSource{Kind: MergeSourceMergedUpstream}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			repo := RepoKey(t.TempDir())
			ws := oneShotRequester(t, s)

			// Act
			err := s.RequestOneShotMerge(context.Background(), repo, ws.ID, tt.source, instant)

			// Assert
			if err == nil {
				t.Fatalf("RequestOneShotMerge(%+v) succeeded, want a refusal", tt.source)
			}
			if !loggedOperation(log, "daemon.wsm.request_one_shot_merge", "error") {
				t.Fatalf("the refusal was not logged at error: %v", log.Records())
			}
			if entries, _ := s.MergeQueue(context.Background(), repo); len(entries) != 0 {
				t.Fatalf("MergeQueue = %+v, want nothing recorded", entries)
			}
			if markedOneShot(t, s, ws.ID) {
				t.Fatalf("a refused request marked the requester a one-shot")
			}
		})
	}
}

func TestRequestOneShotMergeRefusesARequesterWithNoCreationJobAndRecordsNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)

	// Act
	err := s.RequestOneShotMerge(context.Background(), repo, ws.ID, ownBranch, instant)

	// Assert
	if err == nil {
		t.Fatalf("RequestOneShotMerge for a workspace with no creation job succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.request_one_shot_merge", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
	if entries, _ := s.MergeQueue(context.Background(), repo); len(entries) != 0 {
		t.Fatalf("MergeQueue = %+v, want the request rolled back", entries)
	}
}

func TestRequestOneShotMergeRefusesADuplicateAndLeavesTheRequesterUnmarked(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := oneShotRequester(t, s)
	if err := s.RequestMerge(context.Background(), repo, ws.ID, ownBranch, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act
	err := s.RequestOneShotMerge(context.Background(), repo, ws.ID, ownBranch, instant)

	// Assert
	var refusal *MergeQueuedError
	if !errors.As(err, &refusal) {
		t.Fatalf("RequestOneShotMerge = %v, want a *MergeQueuedError", err)
	}
	if markedOneShot(t, s, ws.ID) {
		t.Fatalf("a refused duplicate marked the requester a one-shot")
	}
}

func TestIsOneShotAcceptsOnlyAnInPlaceOwnBranchThatClosesTheRequester(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
		want   bool
	}{
		{name: "own branch", source: ownBranch, want: true},
		{name: "own branch kept open", source: MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true}},
		{name: "own branch detached", source: MergeSource{Kind: MergeSourceOwnBranch, Detach: "d"}},
		{name: "another workspace", source: MergeSource{Kind: MergeSourceWorkspace, Workspace: "w"}},
		{name: "a branch", source: MergeSource{Kind: MergeSourceBranch, Branch: "b"}},
		{name: "merged upstream", source: MergeSource{Kind: MergeSourceMergedUpstream}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := tt.source.IsOneShot()

			// Assert.
			if got != tt.want {
				t.Fatalf("IsOneShot(%+v) = %v, want %v", tt.source, got, tt.want)
			}
		})
	}
}
