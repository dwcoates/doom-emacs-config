package wsm

import (
	"context"
	"database/sql"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
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

// moveStep is one entry a move test's queue is built from: the workspace's
// letter, and how it stands ("queued", "front", "admitted" or "requested").
type moveStep struct {
	ws   string
	kind string
}

// buildMoveQueue builds a queue from steps, in order, answering each letter's
// workspace id.
func buildMoveQueue(t *testing.T, s *store, repo RepoKey, steps []moveStep) map[string]WorkspaceID {
	t.Helper()
	named := map[string]WorkspaceID{}
	for i, st := range steps {
		id := testWorkspace(t, s).ID
		named[st.ws] = id
		at := instant.Add(time.Duration(i) * time.Minute)
		switch st.kind {
		case "front":
			frontQueued(t, s, repo, id, at)
		case "requested":
			if err := s.RequestMerge(context.Background(), repo, id, ownBranch, at); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
		case "queued", "admitted":
			queued(t, s, repo, id, at)
		default:
			t.Fatalf("unknown move step kind %q", st.kind)
		}
		if st.kind == "admitted" {
			if err := s.AdmitMerge(context.Background(), repo, id); err != nil {
				t.Fatalf("AdmitMerge: %v", err)
			}
		}
	}
	return named
}

// lettersOf spells the queue's order in the letters buildMoveQueue named.
func lettersOf(t *testing.T, s *store, repo RepoKey, named map[string]WorkspaceID) []string {
	t.Helper()
	letter := map[WorkspaceID]string{}
	for name, id := range named {
		letter[id] = name
	}
	var out []string
	for _, id := range queueOrder(t, s, repo) {
		out = append(out, letter[id])
	}
	return out
}

func TestMoveMergePlacesTheMerge(t *testing.T) {
	// Each case builds a queue, then moves the merge named "m".
	tests := []struct {
		name      string
		steps     []moveStep
		move      MergeMove
		wantOrder []string
		wantMoved bool
	}{
		{
			name:      "up past the waiting merge ahead of it",
			steps:     []moveStep{{"a", "queued"}, {"b", "queued"}, {"m", "queued"}},
			move:      MergeMoveUp,
			wantOrder: []string{"a", "m", "b"},
			wantMoved: true,
		},
		{
			name:      "down past the waiting merge behind it",
			steps:     []moveStep{{"m", "queued"}, {"a", "queued"}, {"b", "queued"}},
			move:      MergeMoveDown,
			wantOrder: []string{"a", "m", "b"},
			wantMoved: true,
		},
		{
			name:      "never up past the admitted merge",
			steps:     []moveStep{{"r", "admitted"}, {"m", "queued"}, {"a", "queued"}},
			move:      MergeMoveUp,
			wantOrder: []string{"r", "m", "a"},
			wantMoved: false,
		},
		{
			name:      "the first waiting merge stays put moving up",
			steps:     []moveStep{{"m", "queued"}, {"a", "queued"}},
			move:      MergeMoveUp,
			wantOrder: []string{"m", "a"},
			wantMoved: false,
		},
		{
			name:      "the last waiting merge stays put moving down",
			steps:     []moveStep{{"a", "queued"}, {"m", "queued"}},
			move:      MergeMoveDown,
			wantOrder: []string{"a", "m"},
			wantMoved: false,
		},
		{
			name:      "a request between them is passed over moving up",
			steps:     []moveStep{{"a", "queued"}, {"p", "requested"}, {"m", "queued"}},
			move:      MergeMoveUp,
			wantOrder: []string{"m", "p", "a"},
			wantMoved: true,
		},
		{
			name:      "a request behind it is no neighbor moving down",
			steps:     []moveStep{{"m", "queued"}, {"p", "requested"}},
			move:      MergeMoveDown,
			wantOrder: []string{"m", "p"},
			wantMoved: false,
		},
		{
			name:      "the admitted merge stays put",
			steps:     []moveStep{{"m", "admitted"}, {"a", "queued"}},
			move:      MergeMoveDown,
			wantOrder: []string{"m", "a"},
			wantMoved: false,
		},
		{
			name:      "a merge only requested stays put",
			steps:     []moveStep{{"a", "queued"}, {"m", "requested"}},
			move:      MergeMoveUp,
			wantOrder: []string{"a", "m"},
			wantMoved: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, tt.steps)

			// Act
			moved, err := s.MoveMerge(context.Background(), repo, named["m"], tt.move)

			// Assert
			if err != nil {
				t.Fatalf("MoveMerge: %v", err)
			}
			if moved != tt.wantMoved {
				t.Fatalf("moved = %v, want %v", moved, tt.wantMoved)
			}
			if got := lettersOf(t, s, repo, named); !slices.Equal(got, tt.wantOrder) {
				t.Fatalf("queue = %v, want %v", got, tt.wantOrder)
			}
		})
	}
}

func TestMoveMergeGivesTheMovedMergeThePassedMergesFrontMark(t *testing.T) {
	// Each case moves "m" past one neighbor and reads every entry's mark.
	tests := []struct {
		name      string
		steps     []moveStep
		move      MergeMove
		wantFront map[string]bool
	}{
		{
			name:      "an ordinary merge moved up past a front merge becomes a front merge",
			steps:     []moveStep{{"f", "front"}, {"m", "queued"}},
			move:      MergeMoveUp,
			wantFront: map[string]bool{"f": true, "m": true},
		},
		{
			name:      "a front merge moved down past an ordinary merge becomes ordinary",
			steps:     []moveStep{{"m", "front"}, {"a", "queued"}},
			move:      MergeMoveDown,
			wantFront: map[string]bool{"m": false, "a": false},
		},
		{
			name:      "an ordinary merge moved past an ordinary merge stays ordinary",
			steps:     []moveStep{{"a", "queued"}, {"m", "queued"}},
			move:      MergeMoveUp,
			wantFront: map[string]bool{"a": false, "m": false},
		},
		{
			name:      "a front merge moved past a front merge stays a front merge",
			steps:     []moveStep{{"f", "front"}, {"m", "front"}},
			move:      MergeMoveUp,
			wantFront: map[string]bool{"f": true, "m": true},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, tt.steps)

			// Act
			if _, err := s.MoveMerge(context.Background(), repo, named["m"], tt.move); err != nil {
				t.Fatalf("MoveMerge: %v", err)
			}

			// Assert
			entries, err := s.MergeQueue(context.Background(), repo)
			if err != nil {
				t.Fatalf("MergeQueue: %v", err)
			}
			for name, want := range tt.wantFront {
				for _, entry := range entries {
					if entry.Workspace == named[name] && entry.Source.Front != want {
						t.Fatalf("%s's front mark = %v, want %v", name, entry.Source.Front, want)
					}
				}
			}
		})
	}
}

func TestMoveMergeKeepsANewFrontMergeAheadOfAMovedOrdinaryMerge(t *testing.T) {
	// Arrange: a front merge moves down past an ordinary one, so the ordinary
	// merge now stands first.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	named := buildMoveQueue(t, s, repo, []moveStep{{"f", "front"}, {"a", "queued"}})
	if _, err := s.MoveMerge(context.Background(), repo, named["f"], MergeMoveDown); err != nil {
		t.Fatalf("MoveMerge: %v", err)
	}
	n := testWorkspace(t, s).ID
	named["n"] = n

	// Act
	frontQueued(t, s, repo, n, instant.Add(time.Hour))

	// Assert
	if got, want := lettersOf(t, s, repo, named), []string{"n", "a", "f"}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestMoveMergeRefusesAWorkspaceNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)

	// Act
	_, err := s.MoveMerge(context.Background(), repo, ws.ID, MergeMoveUp)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("MoveMerge = %v, want ErrNotFound", err)
	}
}

func TestMoveMergeRefusesAnUndeclaredMoveAndMovesNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repo := RepoKey(t.TempDir())
	named := buildMoveQueue(t, s, repo, []moveStep{{"a", "queued"}, {"m", "queued"}})

	// Act
	moved, err := s.MoveMerge(context.Background(), repo, named["m"], MergeMove(0))

	// Assert
	if err == nil || moved {
		t.Fatalf("MoveMerge(0) = %v, %v; want a refusal", moved, err)
	}
	if !loggedOperation(log, "daemon.wsm.move_merge", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
	if got, want := lettersOf(t, s, repo, named), []string{"a", "m"}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
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
			// The refusal is an answer the orchestrator turns into the
			// request's already-queued arm, so the store records it at debug
			// and never as an error.
			if !loggedOperation(log, "daemon.wsm.request_merge", "debug") || loggedOperation(log, "daemon.wsm.request_merge", "error") {
				t.Fatalf("the refusal was not logged at debug alone: %v", log.Records())
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
	if err != nil || len(entries) != 1 || !reflect.DeepEqual(entries[0].Source, source) {
		t.Fatalf("MergeQueue = %+v, %v, want the source with its branch", entries, err)
	}
}

func TestRequestMergeRefusesASourceThatContradictsItself(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "an undeclared kind", source: MergeSource{Kind: MergeSourceKind(9)}},
		{name: "the retired merged-upstream kind", source: MergeSource{Kind: MergeSourceKind(3)}},
		{name: "keep_open off the own branch", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", KeepOpen: true}},
		{name: "a workspace source naming no workspace", source: MergeSource{Kind: MergeSourceWorkspace}},
		{name: "a branch source naming no branch", source: MergeSource{Kind: MergeSourceBranch}},
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
			if len(got) != 1 || !reflect.DeepEqual(got[0].Source, tt.source) || got[0].State != MergeRequested {
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

func TestYieldMergeHeadPutsTheWinnerFirstAndKeepsTheLoserAdmitted(t *testing.T) {
	// Arrange: the head and the merge behind it are admitted; a third waits.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	loser, winner, waiting := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, loser.ID, instant)
	queued(t, s, repo, winner.ID, instant.Add(time.Minute))
	queued(t, s, repo, waiting.ID, instant.Add(2*time.Minute))
	for _, id := range []WorkspaceID{loser.ID, winner.ID} {
		if err := s.AdmitMerge(ctx, repo, id); err != nil {
			t.Fatalf("AdmitMerge: %v", err)
		}
	}

	// Act
	if err := s.YieldMergeHead(ctx, repo, loser.ID, winner.ID); err != nil {
		t.Fatalf("YieldMergeHead: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(ctx, repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 3 || got[0].Workspace != winner.ID || got[1].Workspace != loser.ID || got[2].Workspace != waiting.ID {
		t.Fatalf("queue = %+v, want winner, loser, waiting", got)
	}
	if got[0].State != MergeAdmitted || got[1].State != MergeAdmitted || got[2].State != MergeQueued {
		t.Fatalf("states = %v %v %v, want the winner and the suspended loser admitted, the third queued", got[0].State, got[1].State, got[2].State)
	}
}

func TestYieldMergeHeadRefusesAWinnerNotAdmitted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	loser, winner := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, loser.ID, instant)
	queued(t, s, repo, winner.ID, instant.Add(time.Minute))
	if err := s.AdmitMerge(ctx, repo, loser.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	err := s.YieldMergeHead(ctx, repo, loser.ID, winner.ID)

	// Assert
	if err == nil {
		t.Fatal("YieldMergeHead let a merge that was never admitted overtake the head")
	}
	got, readErr := s.MergeQueue(ctx, repo)
	if readErr != nil {
		t.Fatalf("MergeQueue: %v", readErr)
	}
	if got[0].Workspace != loser.ID || got[1].Workspace != winner.ID {
		t.Fatalf("queue = %+v, want it unchanged by the refused yield", got)
	}
}

func TestYieldMergeHeadAnswersAHeadThatLeftTheQueue(t *testing.T) {
	// Arrange: the head left its queue (it failed, landed or was abandoned)
	// before the merge behind it could record the race it won.
	s, log := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	loser, winner := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, loser.ID, instant)
	queued(t, s, repo, winner.ID, instant.Add(time.Minute))
	for _, id := range []WorkspaceID{loser.ID, winner.ID} {
		if err := s.AdmitMerge(ctx, repo, id); err != nil {
			t.Fatalf("AdmitMerge: %v", err)
		}
	}
	if err := s.RemoveMergeQueueEntry(ctx, repo, loser.ID, "failed"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Act
	err := s.YieldMergeHead(ctx, repo, loser.ID, winner.ID)

	// Assert
	if !errors.Is(err, ErrMergeHeadLeft) {
		t.Fatalf("YieldMergeHead = %v, want ErrMergeHeadLeft", err)
	}
	for _, rec := range log.Records() {
		if rec.Level == "error" {
			t.Fatalf("a head that left its queue was recorded at ERROR: %v", rec)
		}
	}
	got, readErr := s.MergeQueue(ctx, repo)
	if readErr != nil {
		t.Fatalf("MergeQueue: %v", readErr)
	}
	if len(got) != 1 || got[0].Workspace != winner.ID || got[0].State != MergeAdmitted {
		t.Fatalf("queue = %+v, want the winner alone, admitted", got)
	}
}

func TestYieldMergeHeadRefusesAWinnerNotInLineAsNoHeadThatLeft(t *testing.T) {
	// Arrange: the head is in line; the merge claiming to overtake it is not.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	loser, winner := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, loser.ID, instant)
	if err := s.AdmitMerge(ctx, repo, loser.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	err := s.YieldMergeHead(ctx, repo, loser.ID, winner.ID)

	// Assert
	if err == nil || errors.Is(err, ErrMergeHeadLeft) {
		t.Fatalf("YieldMergeHead = %v, want a refusal that is not ErrMergeHeadLeft", err)
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
	if err != nil || len(entries) != 1 || entries[0].State != MergeRequested || !reflect.DeepEqual(entries[0].Source, ownBranch) {
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
		{name: "detached", source: MergeSource{Kind: MergeSourceOwnBranch, Detach: NewDetachedMergeID(), DetachMaker: "maker"}},
		{name: "another branch", source: MergeSource{Kind: MergeSourceBranch, Branch: "b"}},
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
		{name: "own branch detached", source: MergeSource{Kind: MergeSourceOwnBranch, Detach: "d", DetachMaker: "maker"}},
		{name: "another workspace", source: MergeSource{Kind: MergeSourceWorkspace, Workspace: "w"}},
		{name: "a branch", source: MergeSource{Kind: MergeSourceBranch, Branch: "b"}},
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

func TestMergeQueueColumnsNameEveryColumnTheScanReads(t *testing.T) {
	// Arrange: a queue holding one request.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	if err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch, Branch: "feature"}, time.Unix(100, 0)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act: both readers select mergeQueueColumns and scan them.
	one, errOne := s.MergeQueue(context.Background(), repo)
	all, errAll := s.AllMergeQueues(context.Background())

	// Assert
	if errOne != nil || errAll != nil || len(one) != 1 || len(all[one[0].Repo]) != 1 {
		t.Fatalf("MergeQueue = %v, %v; AllMergeQueues = %v, %v; want the one request from each", one, errOne, all, errAll)
	}
}

// TestNoQueueReadSpellsOutItsColumns holds every merge_queue read to
// mergeQueueColumns: a read that lists its columns by hand can drift from the
// scan's order.
func TestNoQueueReadSpellsOutItsColumns(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("mergequeue.go")
	if err != nil {
		t.Fatalf("read mergequeue.go: %v", err)
	}

	// Act
	spelled := strings.Count(string(src), "source_detach_id, ledger_id")

	// Assert
	if spelled != 1 {
		t.Fatalf("the queue's column list is spelled %d times in mergequeue.go, want once (mergeQueueColumns)", spelled)
	}
}

func TestMergeMoveString(t *testing.T) {
	tests := []struct {
		move MergeMove
		want string
	}{
		{move: MergeMoveUp, want: "up"},
		{move: MergeMoveDown, want: "down"},
		{move: MergeMove(0), want: "MergeMove(0)"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := tt.move.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestPrioritizeMergePlacesTheMerge(t *testing.T) {
	// Each case builds a queue, then prioritizes the merge named "m".
	tests := []struct {
		name      string
		steps     []moveStep
		wantOrder []string
		wantMoved bool
	}{
		{
			name:      "to the front of the waiting merges, behind the admitted one",
			steps:     []moveStep{{"r", "admitted"}, {"a", "queued"}, {"b", "queued"}, {"m", "queued"}},
			wantOrder: []string{"r", "m", "a", "b"},
			wantMoved: true,
		},
		{
			name:      "ahead of a front merge",
			steps:     []moveStep{{"f", "front"}, {"a", "queued"}, {"m", "queued"}},
			wantOrder: []string{"m", "f", "a"},
			wantMoved: true,
		},
		{
			name:      "past a request standing between",
			steps:     []moveStep{{"a", "queued"}, {"p", "requested"}, {"m", "queued"}},
			wantOrder: []string{"m", "a", "p"},
			wantMoved: true,
		},
		{
			name:      "the first waiting merge stays put",
			steps:     []moveStep{{"r", "admitted"}, {"m", "queued"}, {"a", "queued"}},
			wantOrder: []string{"r", "m", "a"},
			wantMoved: false,
		},
		{
			name:      "the admitted merge stays put",
			steps:     []moveStep{{"m", "admitted"}, {"a", "queued"}},
			wantOrder: []string{"m", "a"},
			wantMoved: false,
		},
		{
			name:      "a merge only requested stays put",
			steps:     []moveStep{{"a", "queued"}, {"m", "requested"}},
			wantOrder: []string{"a", "m"},
			wantMoved: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, tt.steps)

			// Act
			moved, err := s.PrioritizeMerge(context.Background(), repo, named["m"])

			// Assert
			if err != nil {
				t.Fatalf("PrioritizeMerge: %v", err)
			}
			if moved != tt.wantMoved {
				t.Fatalf("moved = %v, want %v", moved, tt.wantMoved)
			}
			if got := lettersOf(t, s, repo, named); !slices.Equal(got, tt.wantOrder) {
				t.Fatalf("queue = %v, want %v", got, tt.wantOrder)
			}
		})
	}
}

func TestPrioritizeMergeMarksTheMovedMergeFront(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	named := buildMoveQueue(t, s, repo, []moveStep{{"a", "queued"}, {"m", "queued"}})

	// Act
	if _, err := s.PrioritizeMerge(context.Background(), repo, named["m"]); err != nil {
		t.Fatalf("PrioritizeMerge: %v", err)
	}

	// Assert
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, entry := range entries {
		want := entry.Workspace == named["m"]
		if entry.Source.Front != want {
			t.Fatalf("%s's front mark = %v, want %v", entry.Workspace, entry.Source.Front, want)
		}
	}
}

func TestPrioritizeMergeRefusesAWorkspaceNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)

	// Act
	_, err := s.PrioritizeMerge(context.Background(), repo, ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("PrioritizeMerge = %v, want ErrNotFound", err)
	}
}

func TestEntryPlaceReadsTheEntrysPlaceAndState(t *testing.T) {
	// Each case builds a queue and reads the place of the merge named "m".
	tests := []struct {
		name      string
		steps     []moveStep
		wantState MergeQueueState
	}{
		{name: "a waiting merge", steps: []moveStep{{"a", "queued"}, {"m", "queued"}}, wantState: MergeQueued},
		{name: "a front merge waits too", steps: []moveStep{{"m", "front"}}, wantState: MergeQueued},
		{name: "the admitted merge", steps: []moveStep{{"m", "admitted"}}, wantState: MergeAdmitted},
		{name: "a merge only requested", steps: []moveStep{{"m", "requested"}}, wantState: MergeRequested},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, tt.steps)
			key, err := normalizeRepoKey(repo)
			if err != nil {
				t.Fatalf("normalizeRepoKey: %v", err)
			}
			entries, err := s.MergeQueue(context.Background(), repo)
			if err != nil {
				t.Fatalf("MergeQueue: %v", err)
			}
			var wantSeq int64
			err = s.write(context.Background(), "daemon.wsm.test", nil, func(ctx context.Context, tx *sql.Tx) error {
				return tx.QueryRowContext(ctx, `SELECT seq FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, named["m"]).Scan(&wantSeq)
			})
			if err != nil || len(entries) == 0 {
				t.Fatalf("read the entry's seq: %v (%d entries)", err, len(entries))
			}

			// Act
			var seq int64
			var state MergeQueueState
			err = s.write(context.Background(), "daemon.wsm.test", nil, func(ctx context.Context, tx *sql.Tx) error {
				var err error
				seq, state, err = entryPlace(ctx, tx, key, named["m"])
				return err
			})

			// Assert
			if err != nil || seq != wantSeq || state != tt.wantState {
				t.Fatalf("entryPlace = %d, %v, %v; want %d, %v", seq, state, err, wantSeq, tt.wantState)
			}
		})
	}
}

func TestEntryPlaceRefusesAWorkspaceNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	key, err := normalizeRepoKey(repo)
	if err != nil {
		t.Fatalf("normalizeRepoKey: %v", err)
	}
	ws := testWorkspace(t, s)

	// Act
	err = s.write(context.Background(), "daemon.wsm.test", nil, func(ctx context.Context, tx *sql.Tx) error {
		_, _, err := entryPlace(ctx, tx, key, ws.ID)
		return err
	})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("entryPlace = %v, want ErrNotFound", err)
	}
}

func TestEveryReorderingReadsItsPlaceThroughEntryPlace(t *testing.T) {
	// Arrange
	code, err := os.ReadFile("mergequeue.go")
	if err != nil {
		t.Fatalf("read mergequeue.go: %v", err)
	}

	// Act
	reads := strings.Count(string(code), "SELECT state, seq FROM merge_queue WHERE repo_key = ? AND workspace_id = ?")
	calls := strings.Count(string(code), "entryPlace(ctx, tx, key, id)")

	// Assert: the helper's own query, and one call per reordering (move,
	// prioritize, and the yield's read of both racers).
	if reads != 1 || calls != 3 {
		t.Fatalf("state/seq reads = %d, entryPlace calls = %d; want 1 and 3", reads, calls)
	}
}

// admittedWithLease queues ws's merge, admits it, takes its merge lease and
// records its progress under that lease: the merge a restart took back but
// has not resumed yet.
func admittedWithLease(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) Lease {
	t.Helper()
	queuedWith(t, s, repo, ws, ownBranch)
	if err := s.AdmitMerge(context.Background(), repo, ws); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	lease, err := s.AcquireLease(context.Background(), ws, HolderMerge, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}
	putProgress(t, s, ws, lease.ID)
	return lease
}

func TestDropAdmittedMergeTakesTheEntryTheLeaseAndTheProgressTogether(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	lease := admittedWithLease(t, s, repo, ws.ID)

	// Act.
	err := s.DropAdmittedMerge(context.Background(), repo, ws.ID, lease.ID, "user_drop")

	// Assert.
	if err != nil {
		t.Fatalf("DropAdmittedMerge: %v", err)
	}
	entries, _ := s.MergeQueue(context.Background(), repo)
	_, held, _ := s.Lease(context.Background(), ws.ID)
	_, progress, _ := s.MergeProgressOf(context.Background(), ws.ID)
	if len(entries) != 0 || held || progress {
		t.Fatalf("after the drop: %d entries, lease held %v, progress %v; want none of them", len(entries), held, progress)
	}
}

func TestDropAdmittedMergeRefusesAMergeStillWaitingInLine(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, ownBranch)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	err = s.DropAdmittedMerge(context.Background(), repo, ws.ID, lease.ID, "user_drop")

	// Assert.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("DropAdmittedMerge = %v, want the refusal", err)
	}
	entries, _ := s.MergeQueue(context.Background(), repo)
	if _, held, _ := s.Lease(context.Background(), ws.ID); len(entries) != 1 || !held {
		t.Fatalf("after the refusal: %d entries, lease held %v; want both left", len(entries), held)
	}
}

func TestDropAdmittedMergeRefusesALeaseThatIsNotTheWorkspacesMergeLeaseAndKeepsTheEntry(t *testing.T) {
	// Arrange.
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	admittedWithLease(t, s, repo, ws.ID)

	// Act.
	err := s.DropAdmittedMerge(context.Background(), repo, ws.ID, NewLeaseID(), "user_drop")

	// Assert: the transaction rolled back the entry's removal.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("DropAdmittedMerge = %v, want the refusal", err)
	}
	entries, _ := s.MergeQueue(context.Background(), repo)
	if _, held, _ := s.Lease(context.Background(), ws.ID); len(entries) != 1 || !held {
		t.Fatalf("after the refusal: %d entries, lease held %v; want both left", len(entries), held)
	}
	if !loggedOperation(log, "daemon.wsm.drop_admitted_merge", "debug") {
		t.Fatalf("the refusal was not recorded: %v", log.Records())
	}
}
