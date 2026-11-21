package wsm

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/sourcescan"
	"context"
	"database/sql"
	"errors"
	"fmt"
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

func TestAdmitMergeRefusesAnEntryThatIsNotWaiting(t *testing.T) {
	tests := []struct {
		name  string
		state func(t *testing.T, s *store, repo RepoKey, id WorkspaceID)
		want  MergeQueueState
	}{
		{name: "an admitted entry", want: MergeAdmitted, state: func(t *testing.T, s *store, repo RepoKey, id WorkspaceID) {
			if err := s.AdmitMerge(context.Background(), repo, id); err != nil {
				t.Fatalf("AdmitMerge: %v", err)
			}
		}},
		{name: "a remediating entry", want: MergeRemediating, state: func(t *testing.T, s *store, repo RepoKey, id WorkspaceID) {
			if err := s.AdmitMerge(context.Background(), repo, id); err != nil {
				t.Fatalf("AdmitMerge: %v", err)
			}
			if _, err := s.StepAsideMerge(context.Background(), repo, id, MergeAsideTests); err != nil {
				t.Fatalf("StepAsideMerge: %v", err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repo := RepoKey(t.TempDir())
			ws := testWorkspace(t, s)
			queued(t, s, repo, ws.ID, instant)
			tt.state(t, s, repo, ws.ID)

			// Act
			err := s.AdmitMerge(context.Background(), repo, ws.ID)

			// Assert
			if err == nil || !strings.Contains(err.Error(), "while "+tt.want.String()) {
				t.Fatalf("AdmitMerge = %v, want a refusal naming the prior state %s", err, tt.want)
			}
			entries, readErr := s.MergeQueue(context.Background(), repo)
			if readErr != nil || len(entries) != 1 || entries[0].State != tt.want {
				t.Fatalf("queue = %+v, %v; want the entry left %s", entries, readErr, tt.want)
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.admit_merge"}); !ok {
				t.Fatalf("no ERROR daemon.wsm.admit_merge record: %+v", logs.Records())
			}
		})
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

// testPause is a pause record a writer may write: the user's, at instant.
func testPause() *MergeQueuePause {
	return &MergeQueuePause{At: instant, By: MergeQueuePauser{Kind: MergeQueuePauserUser}}
}

func TestMergeQueuePauseRoundTripsItsRecord(t *testing.T) {
	tests := []struct {
		name  string
		pause MergeQueuePause
	}{
		{name: "the user's pause", pause: MergeQueuePause{At: instant, By: MergeQueuePauser{Kind: MergeQueuePauserUser}}},
		{name: "a workspace's hard stop", pause: MergeQueuePause{At: instant.Add(time.Minute), By: MergeQueuePauser{Kind: MergeQueuePauserWorkspace, Name: "doom", Workspace: "w-doom"}, Hard: true}},
		{name: "a workspace's pause to land a change", pause: MergeQueuePause{At: instant, By: MergeQueuePauser{Kind: MergeQueuePauserWorkspace, Name: "doom", Workspace: "w-doom"}, ToLand: true}},
		{name: "the host's pause for its fault", pause: MergeQueuePause{At: instant, By: MergeQueuePauser{Kind: MergeQueuePauserHost, Cause: "too many open files in system"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			pause := tt.pause

			// Act
			if err := s.SetMergeQueuePaused(context.Background(), repo, &pause); err != nil {
				t.Fatalf("SetMergeQueuePaused: %v", err)
			}
			got, err := s.MergeQueuePause(context.Background(), repo)

			// Assert
			if err != nil {
				t.Fatalf("MergeQueuePause: %v", err)
			}
			if got == nil || !got.At.Equal(tt.pause.At) || got.By != tt.pause.By || got.Hard != tt.pause.Hard || got.ToLand != tt.pause.ToLand {
				t.Fatalf("pause = %+v, want %+v", got, tt.pause)
			}
		})
	}
}

func TestMergeQueuePausedReportsAPauseWithItsRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())

	// Act
	if err := s.SetMergeQueuePaused(context.Background(), repo, testPause()); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}
	paused, err := s.MergeQueuePaused(context.Background(), repo)

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePaused: %v", err)
	}
	if !paused {
		t.Fatalf("paused = false after a pause was written")
	}
}

func TestMergeQueuePauseReplacesTheRecordOfAStandingPause(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if err := s.SetMergeQueuePaused(context.Background(), repo, testPause()); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}
	hard := MergeQueuePause{At: instant.Add(time.Hour), By: MergeQueuePauser{Kind: MergeQueuePauserWorkspace, Name: "w", Workspace: "w-id"}, Hard: true}

	// Act
	if err := s.SetMergeQueuePaused(context.Background(), repo, &hard); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}
	got, err := s.MergeQueuePause(context.Background(), repo)

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePause: %v", err)
	}
	if got == nil || !got.At.Equal(hard.At) || got.By != hard.By || !got.Hard {
		t.Fatalf("pause = %+v, want %+v", got, hard)
	}
}

func TestMergeQueuePauseReadsAPauseWrittenBeforeItsRecordAsUnrecorded(t *testing.T) {
	// Arrange: the previous build's write names none of the record's columns.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	key, err := normalizeRepoKey(repo)
	if err != nil {
		t.Fatalf("normalizeRepoKey: %v", err)
	}
	corrupt(t, s, `INSERT INTO merge_queue_repos (repo_key, paused) VALUES (?, 1)`, string(key))

	// Act
	got, err := s.MergeQueuePause(context.Background(), repo)

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePause: %v", err)
	}
	if got == nil || !got.At.IsZero() || got.By.Kind != MergeQueuePauserUnrecorded || got.Hard || got.ToLand {
		t.Fatalf("pause = %+v, want an unrecorded pause not made to land", got)
	}
}

func TestMergeQueuePauseRefusesAnUndeclaredPauser(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	key, err := normalizeRepoKey(repo)
	if err != nil {
		t.Fatalf("normalizeRepoKey: %v", err)
	}
	corrupt(t, s, `INSERT INTO merge_queue_repos (repo_key, paused, paused_by) VALUES (?, 1, 'robot')`, string(key))

	// Act
	_, err = s.MergeQueuePause(context.Background(), repo)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "paused_by" {
		t.Fatalf("MergeQueuePause = %v, want a DecodeError on paused_by", err)
	}
}

func TestSetMergeQueuePausedRefusesAMalformedRecordAndWritesNothing(t *testing.T) {
	tests := []struct {
		name  string
		pause MergeQueuePause
	}{
		{name: "no instant", pause: MergeQueuePause{By: MergeQueuePauser{Kind: MergeQueuePauserUser}}},
		{name: "no pauser", pause: MergeQueuePause{At: instant}},
		{name: "a workspace with no name", pause: MergeQueuePause{At: instant, By: MergeQueuePauser{Kind: MergeQueuePauserWorkspace, Workspace: "w-id"}}},
		{name: "a workspace with no id", pause: MergeQueuePause{At: instant, By: MergeQueuePauser{Kind: MergeQueuePauserWorkspace, Name: "doom"}}},
		{name: "the host with no cause", pause: MergeQueuePause{At: instant, By: MergeQueuePauser{Kind: MergeQueuePauserHost}}},
		{name: "the host's pause made to land", pause: MergeQueuePause{At: instant, By: MergeQueuePauser{Kind: MergeQueuePauserHost, Cause: "no runner"}, ToLand: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			repo := RepoKey(t.TempDir())
			pause := tt.pause

			// Act
			err := s.SetMergeQueuePaused(context.Background(), repo, &pause)

			// Assert
			if err == nil {
				t.Fatal("SetMergeQueuePaused = nil, want a refusal")
			}
			if !loggedOperation(log, "daemon.wsm.set_merge_queue_paused", "error") {
				t.Fatalf("the refusal was not logged at error: %v", log.Records())
			}
			if paused, err := s.MergeQueuePaused(context.Background(), repo); err != nil || paused {
				t.Fatalf("MergeQueuePaused = %v, %v; want false", paused, err)
			}
		})
	}
}

func TestMergeQueuePausedResumes(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if err := s.SetMergeQueuePaused(context.Background(), repo, testPause()); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}

	// Act
	if err := s.SetMergeQueuePaused(context.Background(), repo, nil); err != nil {
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
		// wantResult is what the move answers, and wantBlocker the letter of
		// the admitted entry it names, for MergeMoveBlockedByAdmitted alone.
		wantResult  MergeMoveResult
		wantBlocker string
	}{
		{
			name:       "up past the waiting merge ahead of it",
			steps:      []moveStep{{"a", "queued"}, {"b", "queued"}, {"m", "queued"}},
			move:       MergeMoveUp,
			wantOrder:  []string{"a", "m", "b"},
			wantResult: MergeMoved,
		},
		{
			name:       "down past the waiting merge behind it",
			steps:      []moveStep{{"m", "queued"}, {"a", "queued"}, {"b", "queued"}},
			move:       MergeMoveDown,
			wantOrder:  []string{"a", "m", "b"},
			wantResult: MergeMoved,
		},
		{
			name:       "never up past the admitted merge",
			steps:      []moveStep{{"r", "admitted"}, {"m", "queued"}, {"a", "queued"}},
			move:       MergeMoveUp,
			wantOrder:  []string{"r", "m", "a"},
			wantResult: MergeMoveBlockedByAdmitted, wantBlocker: "r",
		},
		{
			name:       "the first waiting merge stays put moving up",
			steps:      []moveStep{{"m", "queued"}, {"a", "queued"}},
			move:       MergeMoveUp,
			wantOrder:  []string{"m", "a"},
			wantResult: MergeMoveAtEdge,
		},
		{
			name:       "the last waiting merge stays put moving down",
			steps:      []moveStep{{"a", "queued"}, {"m", "queued"}},
			move:       MergeMoveDown,
			wantOrder:  []string{"a", "m"},
			wantResult: MergeMoveAtEdge,
		},
		{
			name:       "a request between them is passed over moving up",
			steps:      []moveStep{{"a", "queued"}, {"p", "requested"}, {"m", "queued"}},
			move:       MergeMoveUp,
			wantOrder:  []string{"m", "p", "a"},
			wantResult: MergeMoved,
		},
		{
			name:       "a request behind it is no neighbor moving down",
			steps:      []moveStep{{"m", "queued"}, {"p", "requested"}},
			move:       MergeMoveDown,
			wantOrder:  []string{"m", "p"},
			wantResult: MergeMoveAtEdge,
		},
		{
			name:       "the admitted merge stays put",
			steps:      []moveStep{{"m", "admitted"}, {"a", "queued"}},
			move:       MergeMoveDown,
			wantOrder:  []string{"m", "a"},
			wantResult: MergeMoveNotWaiting,
		},
		{
			name:       "a merge only requested stays put",
			steps:      []moveStep{{"a", "queued"}, {"m", "requested"}},
			move:       MergeMoveUp,
			wantOrder:  []string{"a", "m"},
			wantResult: MergeMoveNotWaiting,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, tt.steps)

			// Act
			outcome, err := s.MoveMerge(context.Background(), repo, named["m"], tt.move)

			// Assert
			if err != nil {
				t.Fatalf("MoveMerge: %v", err)
			}
			if want := (MergeMoveOutcome{Result: tt.wantResult, Blocker: named[tt.wantBlocker]}); outcome != want {
				t.Fatalf("outcome = %+v, want %+v", outcome, want)
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
	outcome, err := s.MoveMerge(context.Background(), repo, named["m"], MergeMove(0))

	// Assert
	if err == nil || outcome.Moved() {
		t.Fatalf("MoveMerge(0) = %+v, %v; want a refusal", outcome, err)
	}
	if !loggedOperation(log, "daemon.wsm.move_merge", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
	if got, want := lettersOf(t, s, repo, named), []string{"a", "m"}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestMoveMergeToPlacesTheMerge(t *testing.T) {
	// Each case builds a queue, then moves "m" beside "x".
	tests := []struct {
		name      string
		steps     []moveStep
		side      MergeSide
		wantOrder []string
		wantMoved bool
	}{
		{
			name:      "ahead of a merge several places ahead",
			steps:     []moveStep{{"a", "queued"}, {"x", "queued"}, {"b", "queued"}, {"m", "queued"}},
			side:      MergeSideAheadOf,
			wantOrder: []string{"a", "m", "x", "b"},
			wantMoved: true,
		},
		{
			name:      "behind a merge several places behind",
			steps:     []moveStep{{"m", "queued"}, {"a", "queued"}, {"x", "queued"}, {"b", "queued"}},
			side:      MergeSideBehind,
			wantOrder: []string{"a", "x", "m", "b"},
			wantMoved: true,
		},
		{
			name:      "behind the last merge",
			steps:     []moveStep{{"m", "queued"}, {"a", "queued"}, {"x", "queued"}},
			side:      MergeSideBehind,
			wantOrder: []string{"a", "x", "m"},
			wantMoved: true,
		},
		{
			name:      "a merge already in that place stays put",
			steps:     []moveStep{{"m", "queued"}, {"x", "queued"}},
			side:      MergeSideAheadOf,
			wantOrder: []string{"m", "x"},
			wantMoved: false,
		},
		{
			name:      "the admitted merge and a request keep their places",
			steps:     []moveStep{{"r", "admitted"}, {"x", "queued"}, {"p", "requested"}, {"m", "queued"}},
			side:      MergeSideAheadOf,
			wantOrder: []string{"r", "m", "p", "x"},
			wantMoved: true,
		},
		{
			name:      "an admitted anchor is no place",
			steps:     []moveStep{{"x", "admitted"}, {"a", "queued"}, {"m", "queued"}},
			side:      MergeSideAheadOf,
			wantOrder: []string{"x", "a", "m"},
			wantMoved: false,
		},
		{
			name:      "a merge only requested stays put",
			steps:     []moveStep{{"x", "queued"}, {"m", "requested"}},
			side:      MergeSideAheadOf,
			wantOrder: []string{"x", "m"},
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
			moved, err := s.MoveMergeTo(context.Background(), repo, named["m"], named["x"], tt.side)

			// Assert
			if err != nil {
				t.Fatalf("MoveMergeTo: %v", err)
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

func TestMoveMergeToGivesTheMovedMergeTheAnchorsFrontMark(t *testing.T) {
	tests := []struct {
		name      string
		steps     []moveStep
		side      MergeSide
		wantFront bool
	}{
		{name: "an ordinary merge put ahead of a front merge becomes a front merge", steps: []moveStep{{"x", "front"}, {"a", "queued"}, {"m", "queued"}}, side: MergeSideAheadOf, wantFront: true},
		{name: "an ordinary merge put behind a front merge becomes a front merge", steps: []moveStep{{"x", "front"}, {"a", "queued"}, {"m", "queued"}}, side: MergeSideBehind, wantFront: true},
		{name: "a front merge put behind an ordinary merge becomes ordinary", steps: []moveStep{{"m", "front"}, {"a", "queued"}, {"x", "queued"}}, side: MergeSideBehind, wantFront: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, tt.steps)

			// Act
			if _, err := s.MoveMergeTo(context.Background(), repo, named["m"], named["x"], tt.side); err != nil {
				t.Fatalf("MoveMergeTo: %v", err)
			}

			// Assert
			entries, err := s.MergeQueue(context.Background(), repo)
			if err != nil {
				t.Fatalf("MergeQueue: %v", err)
			}
			for _, entry := range entries {
				if entry.Workspace == named["m"] && entry.Source.Front != tt.wantFront {
					t.Fatalf("m's front mark = %v, want %v", entry.Source.Front, tt.wantFront)
				}
			}
		})
	}
}

func TestMoveMergeToRefusesAMergeOrAnchorNotInTheQueue(t *testing.T) {
	tests := []struct {
		name   string
		moving string
		anchor string
	}{
		{name: "the moved merge", moving: "stranger", anchor: "x"},
		{name: "the anchor", moving: "m", anchor: "stranger"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, []moveStep{{"x", "queued"}, {"m", "queued"}})
			named["stranger"] = testWorkspace(t, s).ID

			// Act
			_, err := s.MoveMergeTo(context.Background(), repo, named[tt.moving], named[tt.anchor], MergeSideAheadOf)

			// Assert
			if !errors.Is(err, ErrNotFound) {
				t.Fatalf("MoveMergeTo = %v, want ErrNotFound", err)
			}
			if got, want := lettersOf(t, s, repo, named), []string{"x", "m"}; !slices.Equal(got, want) {
				t.Fatalf("queue = %v, want %v", got, want)
			}
		})
	}
}

func TestMoveMergeToRefusesAnUndeclaredSideAndMovesNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repo := RepoKey(t.TempDir())
	named := buildMoveQueue(t, s, repo, []moveStep{{"x", "queued"}, {"m", "queued"}})

	// Act
	moved, err := s.MoveMergeTo(context.Background(), repo, named["m"], named["x"], MergeSide(0))

	// Assert
	if err == nil || moved {
		t.Fatalf("MoveMergeTo(0) = %v, %v; want a refusal", moved, err)
	}
	if !loggedOperation(log, "daemon.wsm.move_merge_to", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
	if got, want := lettersOf(t, s, repo, named), []string{"x", "m"}; !slices.Equal(got, want) {
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
	spelled := strings.Count(string(src), "pick_started_at, pick_turn, slot, slot_since")

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
			name:      "past a request standing between, which keeps its place",
			steps:     []moveStep{{"a", "queued"}, {"p", "requested"}, {"m", "queued"}},
			wantOrder: []string{"m", "p", "a"},
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
	// move to a place, move to a named place (PlaceMerge), prioritize, a
	// merge stepping aside, a remediated
	// merge returning, the place a returned merge answers (returnPlaceOf),
	// and an admitted merge moving ahead of the merges it passed over) and
	// per state write guarded on its prior state (setMergeQueueState).
	if reads != 1 || calls != 9 {
		t.Fatalf("state/seq reads = %d, entryPlace calls = %d; want 1 and 9", reads, calls)
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

func TestReturnAdmittedMergeKeepsItsPlaceInLine(t *testing.T) {
	// Arrange: the head is admitted, two merges wait behind it.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	head, second, third := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, head.ID, instant)
	queued(t, s, repo, second.ID, instant.Add(time.Minute))
	queued(t, s, repo, third.ID, instant.Add(2*time.Minute))
	if err := s.AdmitMerge(ctx, repo, head.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	ledger := NewLeaseID()

	// Act
	if err := s.ReturnAdmittedMerge(ctx, repo, head.ID, ledger); err != nil {
		t.Fatalf("ReturnAdmittedMerge: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(ctx, repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 3 || got[0].Workspace != head.ID || got[0].State != MergeQueued || got[0].Ledger != ledger ||
		got[1].Workspace != second.ID || got[2].Workspace != third.ID {
		t.Fatalf("queue = %+v, want the returned head first, waiting under the new ledger, the rest in order", got)
	}
}

func TestReturnAdmittedMergeRefuses(t *testing.T) {
	cases := []struct {
		name   string
		admit  bool
		ledger LeaseID
		want   string
	}{
		{name: "a merge that is only waiting", admit: false, ledger: "lease-x", want: "admitted merge of workspace"},
		{name: "a return with no ledger identity", admit: true, ledger: "", want: "with its bubble's ledger identity"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			ws := testWorkspace(t, s)
			queued(t, s, repo, ws.ID, instant)
			if tc.admit {
				if err := s.AdmitMerge(ctx, repo, ws.ID); err != nil {
					t.Fatalf("AdmitMerge: %v", err)
				}
			}

			// Act
			err := s.ReturnAdmittedMerge(ctx, repo, ws.ID, tc.ledger)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("ReturnAdmittedMerge = %v, want an error containing %q", err, tc.want)
			}
			if _, found := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Operation: "daemon.wsm.return_admitted_merge"}); !found {
				t.Fatalf("records = %+v, want the refusal recorded", logs.Records())
			}
		})
	}
}

// behindQueued requests a merge placed directly behind AHEAD and moves it into
// line, answering its place.
func behindQueued(t *testing.T, s *store, repo RepoKey, id, ahead WorkspaceID, at time.Time) int {
	t.Helper()
	if err := s.RequestMerge(context.Background(), repo, id, MergeSource{Kind: MergeSourceOwnBranch, Behind: ahead}, at); err != nil {
		t.Fatalf("RequestMerge(behind): %v", err)
	}
	position, err := s.QueueMerge(context.Background(), repo, id, NewLeaseID())
	if err != nil {
		t.Fatalf("QueueMerge(behind): %v", err)
	}
	return position
}

// frontMarks answers each merge in line's front mark, in line order.
func frontMarks(t *testing.T, s *store, repo RepoKey) []bool {
	t.Helper()
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	out := make([]bool, 0, len(entries))
	for _, e := range entries {
		out = append(out, e.Source.Front)
	}
	return out
}

func TestQueueMergePutsABehindMergeDirectlyBehindItsNeighbor(t *testing.T) {
	cases := []struct {
		name      string
		arrange   func(t *testing.T, s *store, repo RepoKey, ws []WorkspaceID) WorkspaceID
		wantOrder func(ws []WorkspaceID, b WorkspaceID) []WorkspaceID
		wantMarks []bool
	}{
		{name: "behind a waiting ordinary merge, taking its ordinary mark",
			arrange: func(t *testing.T, s *store, repo RepoKey, ws []WorkspaceID) WorkspaceID {
				queued(t, s, repo, ws[0], instant)
				queued(t, s, repo, ws[1], instant.Add(time.Minute))
				queued(t, s, repo, ws[2], instant.Add(2*time.Minute))
				return ws[0]
			},
			wantOrder: func(ws []WorkspaceID, b WorkspaceID) []WorkspaceID { return []WorkspaceID{ws[0], b, ws[1], ws[2]} },
			wantMarks: []bool{false, false, false, false}},
		{name: "behind a waiting front merge, taking its front mark",
			arrange: func(t *testing.T, s *store, repo RepoKey, ws []WorkspaceID) WorkspaceID {
				queued(t, s, repo, ws[0], instant)
				frontQueued(t, s, repo, ws[1], instant.Add(time.Minute))
				frontQueued(t, s, repo, ws[2], instant.Add(2*time.Minute))
				return ws[1]
			},
			wantOrder: func(ws []WorkspaceID, b WorkspaceID) []WorkspaceID { return []WorkspaceID{ws[1], b, ws[2], ws[0]} },
			wantMarks: []bool{true, true, true, false}},
		{name: "behind the running merge, first in line ahead of front merges, marked front",
			arrange: func(t *testing.T, s *store, repo RepoKey, ws []WorkspaceID) WorkspaceID {
				queued(t, s, repo, ws[0], instant)
				if err := s.AdmitMerge(context.Background(), repo, ws[0]); err != nil {
					t.Fatalf("AdmitMerge: %v", err)
				}
				frontQueued(t, s, repo, ws[1], instant.Add(time.Minute))
				queued(t, s, repo, ws[2], instant.Add(2*time.Minute))
				return ws[0]
			},
			wantOrder: func(ws []WorkspaceID, b WorkspaceID) []WorkspaceID { return []WorkspaceID{ws[0], b, ws[1], ws[2]} },
			wantMarks: []bool{false, true, true, false}},
		{name: "behind the last merge waiting, at the back with its mark",
			arrange: func(t *testing.T, s *store, repo RepoKey, ws []WorkspaceID) WorkspaceID {
				queued(t, s, repo, ws[0], instant)
				queued(t, s, repo, ws[1], instant.Add(time.Minute))
				queued(t, s, repo, ws[2], instant.Add(2*time.Minute))
				return ws[2]
			},
			wantOrder: func(ws []WorkspaceID, b WorkspaceID) []WorkspaceID { return []WorkspaceID{ws[0], ws[1], ws[2], b} },
			wantMarks: []bool{false, false, false, false}},
		{name: "behind a merge that has left the line, at the head of the waiting line",
			arrange: func(t *testing.T, s *store, repo RepoKey, ws []WorkspaceID) WorkspaceID {
				queued(t, s, repo, ws[0], instant)
				queued(t, s, repo, ws[1], instant.Add(time.Minute))
				queued(t, s, repo, ws[2], instant.Add(2*time.Minute))
				if err := s.RemoveMergeQueueEntry(context.Background(), repo, ws[0], "merged"); err != nil {
					t.Fatalf("RemoveMergeQueueEntry: %v", err)
				}
				return ws[0]
			},
			wantOrder: func(ws []WorkspaceID, b WorkspaceID) []WorkspaceID { return []WorkspaceID{b, ws[1], ws[2]} },
			wantMarks: []bool{true, false, false}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			ws := []WorkspaceID{testWorkspace(t, s).ID, testWorkspace(t, s).ID, testWorkspace(t, s).ID}
			ahead := tc.arrange(t, s, repo, ws)
			b := testWorkspace(t, s).ID

			// Act
			behindQueued(t, s, repo, b, ahead, instant.Add(3*time.Minute))

			// Assert
			if got, want := queueOrder(t, s, repo), tc.wantOrder(ws, b); !slices.Equal(got, want) {
				t.Fatalf("queue = %v, want %v", got, want)
			}
			if got := frontMarks(t, s, repo); !slices.Equal(got, tc.wantMarks) {
				t.Fatalf("front marks = %v, want %v", got, tc.wantMarks)
			}
		})
	}
}

func TestABehindRequestCarriesItsAskUntilItIsPlaced(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	ahead, ws := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, ahead.ID, instant)
	if err := s.RequestMerge(ctx, repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch, Behind: ahead.ID}, instant.Add(time.Minute)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	requested, err := s.MergeQueue(ctx, repo)
	if err != nil || len(requested) != 2 || requested[1].Source.Behind != ahead.ID {
		t.Fatalf("MergeQueue = %+v, %v; want the request carrying Behind", requested, err)
	}

	// Act
	if _, err := s.QueueMerge(ctx, repo, ws.ID, NewLeaseID()); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}

	// Assert
	placed, err := s.MergeQueue(ctx, repo)
	if err != nil || len(placed) != 2 || placed[1].Source.Behind != "" {
		t.Fatalf("MergeQueue = %+v, %v; want the placed merge's ask spent", placed, err)
	}
}

func TestRequestMergeRefusesAMergePlacedFrontAndBehind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ahead, ws := testWorkspace(t, s), testWorkspace(t, s)

	// Act
	err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch, Front: true, Behind: ahead.ID}, instant)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "placed at the front or behind") {
		t.Fatalf("RequestMerge = %v, want the contradiction refused", err)
	}
}

// stepAsideFixture is a queue whose head, failed, stepped aside: the head
// remediating, a second merge now admitted at the front, a third waiting.
func stepAsideFixture(t *testing.T) (*store, RepoKey, WorkspaceID, WorkspaceID, WorkspaceID) {
	t.Helper()
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	failed, next, waiting := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, failed.ID, instant)
	queued(t, s, repo, next.ID, instant.Add(time.Minute))
	queued(t, s, repo, waiting.ID, instant.Add(2*time.Minute))
	if err := s.AdmitMerge(ctx, repo, failed.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	holdAsideLease(t, s, failed.ID, MergeAsideTests)
	if _, err := s.StepAsideMerge(ctx, repo, failed.ID, MergeAsideTests); err != nil {
		t.Fatalf("StepAsideMerge: %v", err)
	}
	if err := s.AdmitMerge(ctx, repo, next.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	return s, repo, failed.ID, next.ID, waiting.ID
}

func TestStepAsideMergeTakesTheHeadOutOfLine(t *testing.T) {
	// Arrange + Act
	s, repo, failed, next, waiting := stepAsideFixture(t)

	// Assert
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 3 || got[0].Workspace != next || got[1].Workspace != waiting || got[2].Workspace != failed || got[2].State != MergeRemediating {
		t.Fatalf("queue = %+v, want next, waiting, then the failed merge remediating", got)
	}
}

func TestStepAsideMergeRefusesAMergeNotAdmitted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)

	// Act
	_, err := s.StepAsideMerge(context.Background(), repo, ws.ID, MergeAsideTests)

	// Assert
	if err == nil {
		t.Fatal("StepAsideMerge of a waiting merge = nil, want a refusal")
	}
}

func TestReturnRemediatedMergePutsItAheadOfTheWaitingBehindTheWorkedOn(t *testing.T) {
	// Arrange
	s, repo, failed, next, waiting := stepAsideFixture(t)

	// Act
	if _, err := returnRemediated(context.Background(), s, repo, failed, nil); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 3 || got[0].Workspace != next || got[1].Workspace != failed || got[2].Workspace != waiting {
		t.Fatalf("queue = %+v, want the merge being worked on, the returned merge, then the waiting one", got)
	}
	if got[1].State != MergeQueued {
		t.Fatalf("the returned merge is %s, want waiting, as any merge in line", got[1].State)
	}
}

func TestReturnRemediatedMergeWithNobodyWaitingStandsBehindTheWorkedOn(t *testing.T) {
	// Arrange: the failed head stepped aside behind one merge, nobody waits.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	failed, next := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, failed.ID, instant)
	queued(t, s, repo, next.ID, instant.Add(time.Minute))
	if err := s.AdmitMerge(ctx, repo, failed.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	holdAsideLease(t, s, failed.ID, MergeAsideTests)
	if _, err := s.StepAsideMerge(ctx, repo, failed.ID, MergeAsideTests); err != nil {
		t.Fatalf("StepAsideMerge: %v", err)
	}
	if err := s.AdmitMerge(ctx, repo, next.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	if _, err := returnRemediated(ctx, s, repo, failed.ID, nil); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(ctx, repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 2 || got[0].Workspace != next.ID || got[1].Workspace != failed.ID {
		t.Fatalf("queue = %+v, want the merge being worked on, then the returned merge", got)
	}
}

func TestReturnRemediatedMergeStandsBehindAMergeNamedWorkedOnThatStillWaits(t *testing.T) {
	// Arrange: the failed head stepped aside; the merge behind it is being
	// taken up (named worked on) but its admission is not written yet.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	failed, next, last := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, failed.ID, instant)
	queued(t, s, repo, next.ID, instant.Add(time.Minute))
	queued(t, s, repo, last.ID, instant.Add(2*time.Minute))
	if err := s.AdmitMerge(ctx, repo, failed.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	holdAsideLease(t, s, failed.ID, MergeAsideTests)
	if _, err := s.StepAsideMerge(ctx, repo, failed.ID, MergeAsideTests); err != nil {
		t.Fatalf("StepAsideMerge: %v", err)
	}

	// Act
	if _, err := returnRemediated(ctx, s, repo, failed.ID, []WorkspaceID{next.ID}); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(ctx, repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 3 || got[0].Workspace != next.ID || got[1].Workspace != failed.ID || got[2].Workspace != last.ID {
		t.Fatalf("queue = %+v, want the merge being worked on, the returned merge, then the one waiting", got)
	}
}

// returnersFixture queues five merges, then admits and steps aside each
// merge named by ORDER for its reason in ASIDES, and returns the store, the
// repository, and the merges in their queued order.
func returnersFixture(t *testing.T, asides map[int]MergeAside, order []int) (*store, RepoKey, []WorkspaceID) {
	t.Helper()
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	var wss []WorkspaceID
	for i := 0; i < 5; i++ {
		ws := testWorkspace(t, s)
		queued(t, s, repo, ws.ID, instant.Add(time.Duration(i)*time.Minute))
		wss = append(wss, ws.ID)
	}
	for _, i := range order {
		if err := s.AdmitMerge(ctx, repo, wss[i]); err != nil {
			t.Fatalf("AdmitMerge: %v", err)
		}
		holdAsideLease(t, s, wss[i], asides[i])
		if _, err := s.StepAsideMerge(ctx, repo, wss[i], asides[i]); err != nil {
			t.Fatalf("StepAsideMerge: %v", err)
		}
	}
	return s, repo, wss
}

func TestReturnRemediatedMergePlacesEachReturnerAtTheFirstWaitingPlace(t *testing.T) {
	tests := []struct {
		name    string
		asides  map[int]MergeAside
		returns []int
		want    []int
	}{
		{
			name:    "a merge back from its tests goes ahead of an earlier conflict returner",
			asides:  map[int]MergeAside{0: MergeAsideConflicts, 1: MergeAsideTests},
			returns: []int{0, 1},
			want:    []int{2, 1, 0, 3, 4},
		},
		{
			name:    "a later conflict returner passes an earlier one",
			asides:  map[int]MergeAside{0: MergeAsideConflicts, 1: MergeAsideConflicts},
			returns: []int{1, 0},
			want:    []int{2, 0, 1, 3, 4},
		},
		{
			name:    "a later test returner placed at the front passes an earlier one",
			asides:  map[int]MergeAside{0: MergeAsideTests, 1: MergeAsideTests},
			returns: []int{1, 0},
			want:    []int{2, 0, 1, 3, 4},
		},
		{
			name:    "a conflict returner lands behind a test returner at the front and ahead of the waiting line",
			asides:  map[int]MergeAside{0: MergeAsideTests, 1: MergeAsideConflicts},
			returns: []int{0, 1},
			want:    []int{2, 0, 1, 3, 4},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: merges 0 and 1 step aside in turn, and merge 2 is
			// being worked on.
			s, repo, wss := returnersFixture(t, tt.asides, []int{0, 1})
			ctx := context.Background()
			if err := s.AdmitMerge(ctx, repo, wss[2]); err != nil {
				t.Fatalf("AdmitMerge: %v", err)
			}

			// Act
			for _, i := range tt.returns {
				if _, err := returnRemediated(ctx, s, repo, wss[i], []WorkspaceID{wss[2]}); err != nil {
					t.Fatalf("ReturnRemediatedMerge: %v", err)
				}
			}

			// Assert
			got, err := s.MergeQueue(ctx, repo)
			if err != nil {
				t.Fatalf("MergeQueue: %v", err)
			}
			var order []int
			for _, e := range got {
				for i, ws := range wss {
					if e.Workspace == ws {
						order = append(order, i)
					}
				}
			}
			if fmt.Sprint(order) != fmt.Sprint(tt.want) {
				t.Fatalf("queue order = %v, want %v", order, tt.want)
			}
		})
	}
}

func TestReturnRemediatedMergeNeverPassesAnAdmittedMergeThatNeverSteppedAside(t *testing.T) {
	// Arrange: merge 0 steps aside for its tests, merge 1 is admitted and is
	// not named worked on.
	s, repo, wss := returnersFixture(t, map[int]MergeAside{0: MergeAsideTests}, []int{0})
	ctx := context.Background()
	if err := s.AdmitMerge(ctx, repo, wss[1]); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	if _, err := returnRemediated(ctx, s, repo, wss[0], nil); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(ctx, repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if got[0].Workspace != wss[1] || got[1].Workspace != wss[0] {
		t.Fatalf("queue = %+v, want the admitted merge, then the returner", got)
	}
}

func TestReturnRemediatedMergeRefusesAMergeNotRemediating(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)

	// Act
	_, err := returnRemediated(context.Background(), s, repo, ws.ID, nil)

	// Assert
	if err == nil {
		t.Fatal("ReturnRemediatedMerge of a waiting merge = nil, want a refusal")
	}
}

// asideQueue is a queue of a head and four merges behind it, each admitted in
// turn so it can step aside from the head as its predecessor did.
type asideQueue struct {
	s    *store
	repo RepoKey
	ws   []WorkspaceID
}

func newAsideQueue(t *testing.T, n int) (*asideQueue, *dlog.TestLogger) {
	t.Helper()
	s, log := testStore(t)
	q := &asideQueue{s: s, repo: RepoKey(t.TempDir())}
	for i := 0; i < n; i++ {
		ws := testWorkspace(t, s).ID
		queued(t, s, q.repo, ws, instant.Add(time.Duration(i)*time.Minute))
		q.ws = append(q.ws, ws)
	}
	return q, log
}

// asideFromHead admits WS at the head and steps it aside for ASIDE,
// answering the slot it took.
func (q *asideQueue) asideFromHead(t *testing.T, ws WorkspaceID, aside MergeAside) MergeSlot {
	t.Helper()
	ctx := context.Background()
	if err := q.s.AdmitMerge(ctx, q.repo, ws); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	holdAsideLease(t, q.s, ws, aside)
	slot, err := q.s.StepAsideMerge(ctx, q.repo, ws, aside)
	if err != nil {
		t.Fatalf("StepAsideMerge: %v", err)
	}
	return slot
}

// slotOf reads WS's stamped slot.
func (q *asideQueue) slotOf(t *testing.T, ws WorkspaceID) MergeSlot {
	t.Helper()
	entries, err := q.s.MergeQueue(context.Background(), q.repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, e := range entries {
		if e.Workspace == ws {
			return e.Slot
		}
	}
	t.Fatalf("workspace %s has no entry", ws)
	return 0
}

func TestStepAsideMergeAnswersTheSlotItTook(t *testing.T) {
	cases := []struct {
		name   string
		before []MergeAside
		aside  MergeAside
		want   MergeSlot
	}{
		{name: "a merge fixing tests takes the free fixing slot", aside: MergeAsideTests, want: MergeSlotFixingTests},
		{name: "a merge resolving conflicts takes the free resolving slot", aside: MergeAsideConflicts, want: MergeSlotResolvingConflicts},
		{name: "a merge fixing tests awaits the fixing slot another holds", before: []MergeAside{MergeAsideTests}, aside: MergeAsideTests, want: MergeSlotAwaitingFixingTests},
		{name: "a merge resolving conflicts resolves beside another that resolves", before: []MergeAside{MergeAsideConflicts}, aside: MergeAsideConflicts, want: MergeSlotResolvingConflicts},
		{name: "a merge fixing tests takes its slot while another resolves conflicts", before: []MergeAside{MergeAsideConflicts}, aside: MergeAsideTests, want: MergeSlotFixingTests},
		{name: "a merge resolving conflicts takes its slot while another fixes tests", before: []MergeAside{MergeAsideTests}, aside: MergeAsideConflicts, want: MergeSlotResolvingConflicts},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			q, _ := newAsideQueue(t, len(tc.before)+2)
			for i, aside := range tc.before {
				q.asideFromHead(t, q.ws[i], aside)
			}

			// Act
			got := q.asideFromHead(t, q.ws[len(tc.before)], tc.aside)

			// Assert
			if got != tc.want {
				t.Fatalf("StepAsideMerge = %s, want %s", got, tc.want)
			}
			if stamped := q.slotOf(t, q.ws[len(tc.before)]); stamped != tc.want {
				t.Fatalf("the stamped slot = %s, want %s", stamped, tc.want)
			}
		})
	}
}

func TestReturnRemediatedMergeGrantsTheReleasedSlotToTheFirstAwaitingIt(t *testing.T) {
	// Arrange: three merges stepped aside for tests in turn, so the first
	// holds the fixing slot and the other two await it, in order.
	q, _ := newAsideQueue(t, 4)
	for _, ws := range q.ws[:3] {
		q.asideFromHead(t, ws, MergeAsideTests)
	}

	// Act
	if _, err := returnRemediated(context.Background(), q.s, q.repo, q.ws[0], nil); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}

	// Assert
	if got := q.slotOf(t, q.ws[1]); got != MergeSlotFixingTests {
		t.Fatalf("the first merge awaiting the slot = %s, want it granted the fixing slot", got)
	}
	if got := q.slotOf(t, q.ws[2]); got != MergeSlotAwaitingFixingTests {
		t.Fatalf("the second merge awaiting the slot = %s, want it still awaiting", got)
	}
}

func TestAWaitingMergeStepsAsideForItsBackgroundConflictAndReturnsWaiting(t *testing.T) {
	// Arrange: a head and a merge waiting behind it.
	q, _ := newAsideQueue(t, 3)
	ctx := context.Background()
	if err := q.s.AdmitMerge(ctx, q.repo, q.ws[0]); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	slot, err := q.s.StepAsideMerge(ctx, q.repo, q.ws[2], MergeAsideWaitingConflicts)
	if err != nil {
		t.Fatalf("StepAsideMerge: %v", err)
	}
	if _, err := returnRemediated(ctx, q.s, q.repo, q.ws[2], []WorkspaceID{q.ws[0]}); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}

	// Assert
	if slot != MergeSlotResolvingConflicts {
		t.Fatalf("StepAsideMerge = %s, want the resolving slot", slot)
	}
	got, err := q.s.MergeQueue(ctx, q.repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 3 || got[1].Workspace != q.ws[2] || got[1].State != MergeQueued || got[1].Aside != MergeAsideNone {
		t.Fatalf("queue = %+v, want the returned merge waiting first behind the head, with no aside", got)
	}
}

func TestStepAsideMergeRefusesAnAsideThatNamesNoRepair(t *testing.T) {
	// Arrange
	q, log := newAsideQueue(t, 1)
	ctx := context.Background()
	if err := q.s.AdmitMerge(ctx, q.repo, q.ws[0]); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	_, err := q.s.StepAsideMerge(ctx, q.repo, q.ws[0], MergeAsideNone)

	// Assert
	if err == nil {
		t.Fatal("StepAsideMerge for no repair = nil, want a refusal")
	}
	rec, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.step_aside_merge"})
	if !found || rec.Context["aside"] != "none" || rec.Context["workspace"] != string(q.ws[0]) {
		t.Fatalf("the refusal's record = %+v (found %v), want it at error naming the workspace and the aside", rec, found)
	}
	if got := q.slotOf(t, q.ws[0]); got != MergeSlotHead {
		t.Fatalf("the refused merge = %s, want it still heading its line", got)
	}
}

func TestAWaitingMergeCannotStepAsideForTheHeadsRepairs(t *testing.T) {
	// Arrange
	q, _ := newAsideQueue(t, 2)

	// Act
	_, err := q.s.StepAsideMerge(context.Background(), q.repo, q.ws[1], MergeAsideConflicts)

	// Assert
	if err == nil {
		t.Fatal("StepAsideMerge of a waiting merge for the head's conflicts = nil, want a refusal")
	}
	if got := q.slotOf(t, q.ws[1]); got != MergeSlotWaiting {
		t.Fatalf("the refused merge = %s, want it still waiting", got)
	}
}

// behindRequested requests a merge placed behind AHEAD by the name "ahead",
// as the merge package records a behind placement.
func behindRequested(t *testing.T, s *store, repo RepoKey, id, ahead WorkspaceID) {
	t.Helper()
	if err := s.RequestMerge(context.Background(), repo, id, MergeSource{Kind: MergeSourceOwnBranch, Behind: ahead, BehindName: "ahead"}, instant.Add(time.Minute)); err != nil {
		t.Fatalf("RequestMerge(behind): %v", err)
	}
}

// entryNamed answers ID's entry in REPO's queue.
func entryNamed(t *testing.T, s *store, repo RepoKey, id WorkspaceID) MergeQueueEntry {
	t.Helper()
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, entry := range entries {
		if entry.Workspace == id {
			return entry
		}
	}
	t.Fatalf("no entry for %s in %+v", id, entries)
	return MergeQueueEntry{}
}

func TestARequestPlacedBehindAMergeWaitsOnItFromTheRequestOn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ahead, ws := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, ahead.ID, instant)

	// Act
	behindRequested(t, s, repo, ws.ID, ahead.ID)
	requested := entryNamed(t, s, repo, ws.ID)
	if _, err := s.QueueMerge(context.Background(), repo, ws.ID, NewLeaseID()); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
	placed := entryNamed(t, s, repo, ws.ID)

	// Assert
	for _, entry := range []MergeQueueEntry{requested, placed} {
		if entry.WaitsOn != ahead.ID || entry.WaitsOnName != "ahead" || entry.WaitsOnLeft != "" || !entry.HeldBehind() {
			t.Fatalf("entry = %+v, want it held behind %s", entry, ahead.ID)
		}
	}
}

func TestRemovingTheMergeAheadSettlesTheMergesWaitingOnIt(t *testing.T) {
	tests := []struct {
		name       string
		cause      string
		wantLeft   string
		wantHeld   bool
		wantFailed bool
		wantLog    bool
	}{
		{name: "the merge ahead landed", cause: MergeLeftLanded, wantLeft: MergeLeftLanded, wantLog: true},
		{name: "the merge ahead failed", cause: "failed", wantLeft: "failed", wantFailed: true, wantLog: true},
		{name: "the merge ahead was evicted", cause: "evicted", wantLeft: "evicted", wantFailed: true, wantLog: true},
		{name: "the merge ahead is put straight back", cause: MergeLeftToReturn, wantHeld: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repo := RepoKey(t.TempDir())
			ahead, ws := testWorkspace(t, s), testWorkspace(t, s)
			queued(t, s, repo, ahead.ID, instant)
			behindRequested(t, s, repo, ws.ID, ahead.ID)

			// Act
			err := s.RemoveMergeQueueEntry(context.Background(), repo, ahead.ID, tt.cause)

			// Assert
			if err != nil {
				t.Fatalf("RemoveMergeQueueEntry: %v", err)
			}
			entry := entryNamed(t, s, repo, ws.ID)
			if entry.WaitsOnLeft != tt.wantLeft || entry.HeldBehind() != tt.wantHeld || entry.AheadFailed() != tt.wantFailed {
				t.Fatalf("entry = %+v; want left %q, held %v, failed %v", entry, tt.wantLeft, tt.wantHeld, tt.wantFailed)
			}
			record, logged := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Operation: "daemon.wsm.remove_merge_queue_entry",
				Message: "the merge left its queue; the merges placed behind it are settled by the cause it left for"})
			if logged != tt.wantLog || (logged && record.Context["settled"] != int64(1)) {
				t.Fatalf("settle record = %+v, %v; want logged %v with one merge settled", record, logged, tt.wantLog)
			}
		})
	}
}

func TestDroppingAnAdmittedMergeAheadSettlesTheMergesWaitingOnIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ahead, ws := testWorkspace(t, s), testWorkspace(t, s)
	lease := admittedWithLease(t, s, repo, ahead.ID)
	behindRequested(t, s, repo, ws.ID, ahead.ID)

	// Act
	err := s.DropAdmittedMerge(context.Background(), repo, ahead.ID, lease.ID, "failed")

	// Assert
	if err != nil {
		t.Fatalf("DropAdmittedMerge: %v", err)
	}
	if entry := entryNamed(t, s, repo, ws.ID); !entry.AheadFailed() || entry.WaitsOnLeft != "failed" {
		t.Fatalf("entry = %+v, want its merge ahead recorded as failed", entry)
	}
}

func TestRemovingAMergeWithNoCauseIsRefusedAndChangesNothing(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	repo := RepoKey(t.TempDir())
	ahead, ws := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, ahead.ID, instant)
	behindRequested(t, s, repo, ws.ID, ahead.ID)

	// Act
	err := s.RemoveMergeQueueEntry(context.Background(), repo, ahead.ID, "")

	// Assert
	if err == nil {
		t.Fatal("RemoveMergeQueueEntry with no cause succeeded")
	}
	if entries, _ := s.MergeQueue(context.Background(), repo); len(entries) != 2 || !entryNamed(t, s, repo, ws.ID).HeldBehind() {
		t.Fatalf("entries = %+v, want both merges standing and the wait unsettled", entries)
	}
	if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.remove_merge_queue_entry"}); !ok {
		t.Fatalf("records = %+v, want the refused removal at ERROR", logs.Records())
	}
}

func TestAWaitOnTheMergeAheadOutlivesTheStoreHandle(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	log := dlog.NewTestLogger()
	s := openStoreAt(t, path, log)
	repo := RepoKey(t.TempDir())
	ahead, ws := testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, ahead.ID, instant)
	behindRequested(t, s, repo, ws.ID, ahead.ID)
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, ahead.ID, "failed"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	reopened := openStoreAt(t, path, log)
	t.Cleanup(func() { reopened.Close() })
	entry := entryNamed(t, reopened, repo, ws.ID)

	// Assert
	if entry.WaitsOn != ahead.ID || entry.WaitsOnName != "ahead" || entry.WaitsOnLeft != "failed" {
		t.Fatalf("entry after the reopen = %+v, want the settled wait read back", entry)
	}
}

func TestAMergeQueueRowThatRecordsALeavingWithoutAWaitDoesNotDecode(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)
	if _, err := s.db().Exec(`UPDATE merge_queue SET waits_on_left = 'failed' WHERE workspace_id = ?`, ws.ID); err != nil {
		t.Fatalf("corrupting the row: %v", err)
	}

	// Act
	_, err := s.MergeQueue(context.Background(), repo)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "waits_on_left" {
		t.Fatalf("MergeQueue = %v, want a decode error on waits_on_left", err)
	}
}

func TestRequestMergeRefusesANameForAMergePlacedBehindNone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)

	// Act
	err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch, BehindName: "ahead"}, instant)

	// Assert
	if err == nil {
		t.Fatal("RequestMerge accepted a behind name with no merge behind")
	}
	if entries, _ := s.MergeQueue(context.Background(), repo); len(entries) != 0 {
		t.Fatalf("entries = %+v, want nothing recorded", entries)
	}
}

func TestAssignQueuePlacesReordersMergesAmongTheirOwnPlaces(t *testing.T) {
	// Arrange: three merges waiting, front first.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	var line []WorkspaceID
	for i := range 3 {
		ws := testWorkspace(t, s)
		queued(t, s, repo, ws.ID, instant.Add(time.Duration(i)*time.Second))
		line = append(line, ws.ID)
	}
	before, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatal(err)
	}
	key, err := normalizeRepoKey(repo)
	if err != nil {
		t.Fatal(err)
	}

	// Act: the last two trade places.
	err = s.writeQueue(context.Background(), "test", dlog.Context{}, key, func(ctx context.Context, tx *sql.Tx) error {
		places := []int64{}
		for _, ws := range line[1:] {
			seq, _, err := entryPlace(ctx, tx, key, ws)
			if err != nil {
				return err
			}
			places = append(places, seq)
		}
		return assignQueuePlaces(ctx, tx, key, places, []WorkspaceID{line[2], line[1]})
	})

	// Assert
	after, readErr := s.MergeQueue(context.Background(), repo)
	if err != nil || readErr != nil || len(after) != 3 || after[0].Workspace != line[0] || after[1].Workspace != line[2] || after[2].Workspace != line[1] {
		t.Fatalf("assignQueuePlaces = %v, queue %+v (%v), want the last two traded", err, after, readErr)
	}
	if len(before) != 3 {
		t.Fatalf("before = %+v", before)
	}
}

// THE QUEUE REORDERS THROUGH ONE HELPER: a hand-rolled seq swap elsewhere
// would skip the negative parking the unique (repo_key, seq) index needs.
func TestEveryQueueReorderGoesThroughAssignQueuePlaces(t *testing.T) {
	src, err := os.ReadFile("mergequeue.go")
	if err != nil {
		t.Fatal(err)
	}
	if n := strings.Count(string(src), "SET seq = -seq"); n != 1 {
		t.Fatalf("mergequeue.go parks seqs in %d places, want only assignQueuePlaces", n)
	}
}

func TestAnAdmissionPastWaitingMergesPutsTheAdmittedMergeAheadOfThem(t *testing.T) {
	tests := []struct {
		name  string
		admit int
		want  []int
	}{
		{name: "the first merge waiting keeps its place", admit: 0, want: []int{0, 1, 2}},
		{name: "a merge admitted past two goes ahead of them, which keep their order", admit: 2, want: []int{2, 0, 1}},
		{name: "a merge admitted past one passes nobody behind it", admit: 1, want: []int{1, 0, 2}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			var line []WorkspaceID
			for i := range 3 {
				ws := testWorkspace(t, s)
				queued(t, s, repo, ws.ID, instant.Add(time.Duration(i)*time.Second))
				line = append(line, ws.ID)
			}

			// Act
			err := s.AdmitMerge(context.Background(), repo, line[tt.admit])

			// Assert
			entries, readErr := s.MergeQueue(context.Background(), repo)
			if err != nil || readErr != nil || len(entries) != len(tt.want) {
				t.Fatalf("AdmitMerge = %v, queue = %+v (%v)", err, entries, readErr)
			}
			for i, want := range tt.want {
				if entries[i].Workspace != line[want] {
					t.Fatalf("place %d holds %s, want %s (queue %+v)", i, entries[i].Workspace, line[want], entries)
				}
			}
			if entries[0].State != MergeAdmitted {
				t.Fatalf("front = %+v, want the admitted merge", entries[0])
			}
		})
	}
}

func TestRequeuePausedMergePutsItBackInLineAndDropsItsLeaseAndProgress(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	lease := admittedWithLease(t, s, repo, ws.ID)

	// Act
	err := s.RequeuePausedMerge(context.Background(), repo, ws.ID, lease.ID, "ledger-1")

	// Assert
	entries, _ := s.MergeQueue(context.Background(), repo)
	_, held, _ := s.Lease(context.Background(), ws.ID)
	_, progress, _ := s.MergeProgressOf(context.Background(), ws.ID)
	if err != nil || len(entries) != 1 || entries[0].State != MergeQueued || entries[0].Ledger != "ledger-1" || held || progress {
		t.Fatalf("RequeuePausedMerge = %v; entries %+v, lease held %v, progress %v; want it waiting under its ledger with no lease or progress", err, entries, held, progress)
	}
}

func TestRequeuePausedMergeRefusesAMergeNotAdmittedAndChangesNothing(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, ownBranch)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	err = s.RequeuePausedMerge(context.Background(), repo, ws.ID, lease.ID, "ledger-1")

	// Assert
	entries, _ := s.MergeQueue(context.Background(), repo)
	_, held, _ := s.Lease(context.Background(), ws.ID)
	if !errors.Is(err, ErrNotFound) || len(entries) != 1 || entries[0].State != MergeQueued || !held {
		t.Fatalf("RequeuePausedMerge = %v; entries %+v, lease held %v; want a refusal that leaves both", err, entries, held)
	}
	// The store's write path records a write naming no such record; its
	// caller owns the error's account.
	if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Operation: "daemon.wsm.requeue_paused_merge", Message: "the write named no such record"}); !ok {
		t.Fatalf("no daemon.wsm.requeue_paused_merge record of the refusal: %+v", logs.Records())
	}
}

func TestNormalizedRepoKeyAnswersANormalizableKeyWithoutARecord(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	dir := t.TempDir()
	want, err := normalizeRepoKey(RepoKey(dir))
	if err != nil {
		t.Fatalf("normalizeRepoKey(%q): %v", dir, err)
	}

	// Act
	got, err := s.normalizedRepoKey("daemon.wsm.test_op", RepoKey(dir), dlog.Context{"repo": dir})

	// Assert
	if err != nil {
		t.Fatalf("normalizedRepoKey: %v", err)
	}
	if got != want {
		t.Errorf("normalizedRepoKey = %q, want %q", got, want)
	}
	if errs := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error"}); len(errs) != 0 {
		t.Errorf("error records = %+v, want none", errs)
	}
}

func TestNormalizedRepoKeyRecordsAKeyThatCannotBeNormalizedOnce(t *testing.T) {
	tests := []struct {
		name   string
		repo   RepoKey
		op     string
		fields dlog.Context
	}{
		{name: "empty key", repo: "", op: "daemon.wsm.empty_op", fields: dlog.Context{"repo": "", "workspace": "ws-1"}},
		{name: "empty key with other fields", repo: "", op: "daemon.wsm.other_op", fields: dlog.Context{"repo": "", "problems": 3}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			got, err := s.normalizedRepoKey(tt.op, tt.repo, tt.fields)

			// Assert
			if err == nil {
				t.Fatalf("normalizedRepoKey(%q) = %q, want an error", tt.repo, got)
			}
			if got != "" {
				t.Errorf("normalizedRepoKey key = %q, want empty", got)
			}
			errs := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error"})
			if len(errs) != 1 {
				t.Fatalf("error records = %+v, want exactly one", errs)
			}
			rec := errs[0]
			if rec.Operation != tt.op || rec.Message != "refused a merge queue key that cannot be normalized" {
				t.Errorf("record = %s %q, want %s with the refusal message", rec.Operation, rec.Message, tt.op)
			}
			for k, v := range tt.fields {
				if rec.Context[k] != v {
					t.Errorf("record context[%q] = %v, want %v", k, rec.Context[k], v)
				}
			}
		})
	}
}

// TestRepoKeyRefusalIsRecordedOnlyByTheHelper fails on a hand-rolled site: the
// refusal message exists once, inside normalizedRepoKey.
func TestRepoKeyRefusalIsRecordedOnlyByTheHelper(t *testing.T) {
	// Arrange
	const message = "refused a merge queue key that cannot be normalized"

	// Act
	sites := sourcescan.Sites(t, message)

	// Assert
	if want := []string{"mergequeue.go"}; !slices.Equal(sites, want) {
		t.Errorf("files recording %q = %v, want %v (only normalizedRepoKey records it)", message, sites, want)
	}
}

// holdAsideLease gives WS the merge lease and the progress record its run
// holds when it steps aside from the head for ASIDE, as the orchestrator's
// run holds them: the record names the step it repairs at.
func holdAsideLease(t *testing.T, s *store, ws WorkspaceID, aside MergeAside) LeaseID {
	t.Helper()
	lease, err := holdAsideLeaseErr(context.Background(), s, ws, aside)
	if err != nil {
		t.Fatalf("holding the merge lease of %s: %v", ws, err)
	}
	return lease
}

// holdAsideLeaseErr is holdAsideLease for a step that answers its error.
func holdAsideLeaseErr(ctx context.Context, s *store, ws WorkspaceID, aside MergeAside) (LeaseID, error) {
	step := "conflicts"
	if aside == MergeAsideTests {
		step = MergeStepFixes
	}
	held, err := s.AcquireLeaseAs(ctx, ws, NewLeaseID(), HolderMerge, PolicyHold)
	if err != nil {
		return "", err
	}
	doc := []byte(`{"step":"` + step + `"}`)
	return held.ID, s.PutMergeProgress(ctx, MergeProgress{Workspace: ws, Lease: held.ID, UpdatedAt: instant, Document: doc})
}

// returnRemediated returns WS to REPO's line with the merge lease it holds,
// none when it holds none, as the orchestrator does.
func returnRemediated(ctx context.Context, s *store, repo RepoKey, ws WorkspaceID, workedOn []WorkspaceID) (MergeReturn, error) {
	lease, held, err := s.Lease(ctx, ws)
	if err != nil {
		return MergeReturn{}, err
	}
	var id LeaseID
	if held && lease.Holder == HolderMerge {
		id = lease.ID
	}
	return s.ReturnRemediatedMerge(ctx, repo, ws, id, workedOn)
}

// returnedFixture is a queue of a head being worked on (merge 1), a merge
// back in line from fixing its tests (merge 0), and merges 2 and 3 waiting
// behind it, in that order.
func returnedFixture(t *testing.T) (*store, RepoKey, []WorkspaceID, MergeReturn) {
	t.Helper()
	s, repo, wss := returnersFixture(t, map[int]MergeAside{0: MergeAsideTests}, []int{0})
	ctx := context.Background()
	if err := s.AdmitMerge(ctx, repo, wss[1]); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	placed, err := returnRemediated(ctx, s, repo, wss[0], []WorkspaceID{wss[1]})
	if err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}
	return s, repo, wss, placed
}

// queueIndexes answers REPO's queue as indexes into WSS, front first, -1
// for a merge WSS does not name.
func queueIndexes(t *testing.T, s *store, repo RepoKey, wss []WorkspaceID) []int {
	t.Helper()
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	var order []int
	for _, e := range got {
		order = append(order, slices.Index(wss, e.Workspace))
	}
	return order
}

func TestReturnRemediatedMergeLandsATestReturnerAtTheFirstWaitingPlace(t *testing.T) {
	// Arrange + Act
	s, repo, wss, placed := returnedFixture(t)
	ctx := context.Background()

	// Assert
	t.Run("it stands next after the merge being worked on", func(t *testing.T) {
		if got, want := queueIndexes(t, s, repo, wss), []int{1, 0, 2, 3, 4}; !slices.Equal(got, want) {
			t.Fatalf("queue order = %v, want %v", got, want)
		}
	})
	t.Run("the return answers its place and the merge it stands behind", func(t *testing.T) {
		if placed.Position != 2 || placed.Behind != wss[1] || placed.Lease == "" {
			t.Fatalf("ReturnRemediatedMerge = %+v, want place 2 behind %s with its lease dropped", placed, wss[1])
		}
	})
	t.Run("it waits as an ordinary merge, holding no lease", func(t *testing.T) {
		got, err := s.MergeQueue(ctx, repo)
		if err != nil {
			t.Fatalf("MergeQueue: %v", err)
		}
		if got[1].State != MergeQueued || got[1].Source.Front || got[1].Aside != MergeAsideNone {
			t.Fatalf("returned entry = %+v, want waiting, ordinary and with no aside", got[1])
		}
		if _, held, err := s.Lease(ctx, wss[0]); err != nil || held {
			t.Fatalf("Lease = held %v, %v; want the merge lease dropped", held, err)
		}
	})
	t.Run("its progress record stays for its resume", func(t *testing.T) {
		record, found, err := s.MergeProgressOf(ctx, wss[0])
		if err != nil || !found || record.Lease != placed.Lease {
			t.Fatalf("MergeProgressOf = %+v, %v, %v; want the record under the dropped lease kept", record, found, err)
		}
	})
}

func TestAReturnerHoldsNoLeaseOnItsPlace(t *testing.T) {
	tests := []struct {
		name string
		act  func(t *testing.T, s *store, repo RepoKey, wss []WorkspaceID)
		want []int
	}{
		{
			name: "a later front placement goes ahead of it",
			act: func(t *testing.T, s *store, repo RepoKey, wss []WorkspaceID) {
				hotfix := testWorkspace(t, s).ID
				front := MergeSource{Kind: MergeSourceOwnBranch, Front: true}
				if err := s.RequestMerge(context.Background(), repo, hotfix, front, instant.Add(time.Hour)); err != nil {
					t.Fatalf("RequestMerge: %v", err)
				}
				if _, err := s.QueueMerge(context.Background(), repo, hotfix, NewLeaseID()); err != nil {
					t.Fatalf("QueueMerge: %v", err)
				}
			},
			want: []int{1, -1, 0, 2, 3, 4},
		},
		{
			name: "a user move up of the merge behind it passes it",
			act: func(t *testing.T, s *store, repo RepoKey, wss []WorkspaceID) {
				if outcome, err := s.MoveMerge(context.Background(), repo, wss[2], MergeMoveUp); err != nil || !outcome.Moved() {
					t.Fatalf("MoveMerge = %+v, %v; want moved", outcome, err)
				}
			},
			want: []int{1, 2, 0, 3, 4},
		},
		{
			name: "a user drag of a later merge ahead of it passes it",
			act: func(t *testing.T, s *store, repo RepoKey, wss []WorkspaceID) {
				if moved, err := s.MoveMergeTo(context.Background(), repo, wss[3], wss[0], MergeSideAheadOf); err != nil || !moved {
					t.Fatalf("MoveMergeTo = %v, %v; want moved", moved, err)
				}
			},
			want: []int{1, 3, 0, 2, 4},
		},
		{
			name: "a user prioritizing a later merge passes it",
			act: func(t *testing.T, s *store, repo RepoKey, wss []WorkspaceID) {
				if moved, err := s.PrioritizeMerge(context.Background(), repo, wss[4]); err != nil || !moved {
					t.Fatalf("PrioritizeMerge = %v, %v; want moved", moved, err)
				}
			},
			want: []int{1, 4, 0, 2, 3},
		},
		{
			name: "the returner itself moves down like any waiting merge",
			act: func(t *testing.T, s *store, repo RepoKey, wss []WorkspaceID) {
				if outcome, err := s.MoveMerge(context.Background(), repo, wss[0], MergeMoveDown); err != nil || !outcome.Moved() {
					t.Fatalf("MoveMerge = %+v, %v; want moved", outcome, err)
				}
			},
			want: []int{1, 2, 0, 3, 4},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, repo, wss, _ := returnedFixture(t)

			// Act
			tt.act(t, s, repo, wss)

			// Assert
			if got := queueIndexes(t, s, repo, wss); !slices.Equal(got, tt.want) {
				t.Fatalf("queue order = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAConflictReturnerPassesATestReturnerNoLongerAtTheFront(t *testing.T) {
	// Arrange: merge 0 is back from its tests, then moved down behind merge
	// 2; merge 3 steps aside for conflicts.
	s, repo, wss, _ := returnedFixture(t)
	ctx := context.Background()
	if outcome, err := s.MoveMerge(ctx, repo, wss[0], MergeMoveDown); err != nil || !outcome.Moved() {
		t.Fatalf("MoveMerge = %+v, %v; want moved", outcome, err)
	}
	if err := s.RemoveMergeQueueEntry(ctx, repo, wss[1], "landed"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	if err := s.AdmitMerge(ctx, repo, wss[2]); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	holdAsideLease(t, s, wss[2], MergeAsideConflicts)
	if _, err := s.StepAsideMerge(ctx, repo, wss[2], MergeAsideConflicts); err != nil {
		t.Fatalf("StepAsideMerge: %v", err)
	}
	if err := s.AdmitMerge(ctx, repo, wss[0]); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	if _, err := returnRemediated(ctx, s, repo, wss[2], []WorkspaceID{wss[0]}); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}

	// Assert: the test returner heads now, so the conflict returner takes the
	// first waiting place ahead of merge 3.
	if got, want := queueIndexes(t, s, repo, wss), []int{0, 2, 3, 4}; !slices.Equal(got, want) {
		t.Fatalf("queue order = %v, want %v", got, want)
	}
}

func TestAConflictReturnerTakesTheFrontMarkOfTheMergeItPasses(t *testing.T) {
	// Arrange: merge 0 steps aside for conflicts; merge 2 is put at the
	// front behind the head, merge 1.
	s, repo, wss := returnersFixture(t, map[int]MergeAside{0: MergeAsideConflicts}, []int{0})
	ctx := context.Background()
	if err := s.AdmitMerge(ctx, repo, wss[1]); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	if moved, err := s.PrioritizeMerge(ctx, repo, wss[3]); err != nil || !moved {
		t.Fatalf("PrioritizeMerge = %v, %v; want moved", moved, err)
	}

	// Act
	if _, err := returnRemediated(ctx, s, repo, wss[0], []WorkspaceID{wss[1]}); err != nil {
		t.Fatalf("ReturnRemediatedMerge: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(ctx, repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if got[1].Workspace != wss[0] || !got[1].Source.Front {
		t.Fatalf("queue = %+v, want the returner first among the waiting, marked front as the front merge it passed", got)
	}
}

func TestReturnRemediatedMergeRefusesAWrongLease(t *testing.T) {
	tests := []struct {
		name  string
		aside MergeAside
		lease func(held LeaseID) LeaseID
	}{
		{name: "a merge back from the head with no lease named", aside: MergeAsideTests, lease: func(LeaseID) LeaseID { return "" }},
		{name: "a merge back from the head naming a lease it does not hold", aside: MergeAsideConflicts, lease: func(LeaseID) LeaseID { return NewLeaseID() }},
		{name: "a merge back from its waiting place naming a lease", aside: MergeAsideWaitingConflicts, lease: func(LeaseID) LeaseID { return NewLeaseID() }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			q, _ := newAsideQueue(t, 3)
			ctx := context.Background()
			var held LeaseID
			if tt.aside == MergeAsideWaitingConflicts {
				if err := q.s.AdmitMerge(ctx, q.repo, q.ws[0]); err != nil {
					t.Fatalf("AdmitMerge: %v", err)
				}
				if _, err := q.s.StepAsideMerge(ctx, q.repo, q.ws[2], tt.aside); err != nil {
					t.Fatalf("StepAsideMerge: %v", err)
				}
			} else {
				q.asideFromHead(t, q.ws[2], tt.aside)
			}
			before, err := q.s.MergeQueue(ctx, q.repo)
			if err != nil {
				t.Fatalf("MergeQueue: %v", err)
			}

			// Act
			_, err = q.s.ReturnRemediatedMerge(ctx, q.repo, q.ws[2], tt.lease(held), nil)

			// Assert
			if err == nil {
				t.Fatal("ReturnRemediatedMerge = nil, want the wrong lease refused")
			}
			after, err := q.s.MergeQueue(ctx, q.repo)
			if err != nil {
				t.Fatalf("MergeQueue: %v", err)
			}
			if fmt.Sprint(after) != fmt.Sprint(before) {
				t.Fatalf("queue after the refusal = %+v, want it unchanged from %+v", after, before)
			}
		})
	}
}

func TestRemoveMergeQueueEntryDropsOnlyARecordNoLeaseHolds(t *testing.T) {
	// Arrange: merge 0 waits back in line with its record and no lease;
	// merge 1, the head, holds a lease and a record.
	s, repo, wss, _ := returnedFixture(t)
	ctx := context.Background()
	headLease := holdAsideLease(t, s, wss[1], MergeAsideTests)

	// Act
	if err := s.RemoveMergeQueueEntry(ctx, repo, wss[0], "taken off"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	if err := s.RemoveMergeQueueEntry(ctx, repo, wss[1], MergeLeftToReturn); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Assert
	if _, found, err := s.MergeProgressOf(ctx, wss[0]); err != nil || found {
		t.Fatalf("MergeProgressOf(returner) found %v, %v; want its unleased record gone with its entry", found, err)
	}
	if record, found, err := s.MergeProgressOf(ctx, wss[1]); err != nil || !found || record.Lease != headLease {
		t.Fatalf("MergeProgressOf(head) = %+v, %v, %v; want the leased record kept", record, found, err)
	}
}

// reorderWaitingOf runs reorderWaiting on the queue's waiting line in one
// queue write, answering whether the order changed.
func reorderWaitingOf(t *testing.T, s *store, repo RepoKey, id WorkspaceID, target int, front bool) bool {
	t.Helper()
	key, err := normalizeRepoKey(repo)
	if err != nil {
		t.Fatalf("normalizeRepoKey: %v", err)
	}
	var moved bool
	err = s.writeQueue(context.Background(), "test.reorder_waiting", dlog.Context{}, key, func(ctx context.Context, tx *sql.Tx) error {
		waiting, err := waitingInLine(ctx, tx, key)
		if err != nil {
			return err
		}
		moved, err = reorderWaiting(ctx, tx, key, waiting, id, target, front)
		return err
	})
	if err != nil {
		t.Fatalf("reorderWaiting: %v", err)
	}
	return moved
}

// frontMarkOf answers the front mark of ID's queue entry.
func frontMarkOf(t *testing.T, s *store, repo RepoKey, id WorkspaceID) bool {
	t.Helper()
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, entry := range entries {
		if entry.Workspace == id {
			return entry.Source.Front
		}
	}
	t.Fatalf("no queue entry for %s", id)
	return false
}

func TestReorderWaitingPutsTheMergeOnItsWaitingPlace(t *testing.T) {
	// Each case puts "m" on a waiting place of r (admitted), a, p (requested), b, m.
	tests := []struct {
		name      string
		target    int
		wantOrder []string
		wantMoved bool
	}{
		{name: "the head of the waiting line", target: 0, wantOrder: []string{"r", "m", "p", "a", "b"}, wantMoved: true},
		{name: "a middle place", target: 1, wantOrder: []string{"r", "a", "p", "m", "b"}, wantMoved: true},
		{name: "its own place", target: 2, wantOrder: []string{"r", "a", "p", "b", "m"}, wantMoved: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, []moveStep{{"r", "admitted"}, {"a", "queued"}, {"p", "requested"}, {"b", "queued"}, {"m", "queued"}})

			// Act
			moved := reorderWaitingOf(t, s, repo, named["m"], tt.target, false)

			// Assert
			if moved != tt.wantMoved {
				t.Fatalf("moved = %v, want %v", moved, tt.wantMoved)
			}
			if got := lettersOf(t, s, repo, named); !slices.Equal(got, tt.wantOrder) {
				t.Fatalf("queue = %v, want %v", got, tt.wantOrder)
			}
		})
	}
}

func TestReorderWaitingSetsTheFrontMarkOnlyWhenTheOrderChanges(t *testing.T) {
	tests := []struct {
		name      string
		target    int
		wantFront bool
	}{
		{name: "a move marks the merge as asked", target: 0, wantFront: true},
		{name: "a merge left on its place keeps its mark", target: 1, wantFront: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, []moveStep{{"a", "queued"}, {"m", "queued"}})

			// Act
			reorderWaitingOf(t, s, repo, named["m"], tt.target, true)

			// Assert
			if got := frontMarkOf(t, s, repo, named["m"]); got != tt.wantFront {
				t.Fatalf("m's front mark = %v, want %v", got, tt.wantFront)
			}
		})
	}
}

func TestReorderWaitingPanicsOnAPlaceOutsideTheWaitingLine(t *testing.T) {
	for _, target := range []int{-1, 2} {
		t.Run(fmt.Sprintf("place %d", target), func(t *testing.T) {
			// Arrange: the place is checked before the transaction is touched.
			waiting := []waitingRow{{workspace: "a", seq: 1}, {workspace: "m", seq: 2}}
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatalf("reorderWaiting(%d) did not panic", target)
				}
			}()

			// Act
			_, _ = reorderWaiting(context.Background(), nil, "", waiting, "m", target, false)
		})
	}
}

// TestEveryWaitingReorderPassesOnlyWaitingMerges pins that every store verb
// that moves a waiting merge reorders through reorderWaiting: each moves "m"
// to the head of r (admitted), a, p (requested), m, and each leaves the
// admitted merge and the request on their places.
func TestEveryWaitingReorderPassesOnlyWaitingMerges(t *testing.T) {
	tests := []struct {
		name string
		move func(s *store, repo RepoKey, named map[string]WorkspaceID) (bool, error)
	}{
		{name: "MoveMerge", move: func(s *store, repo RepoKey, named map[string]WorkspaceID) (bool, error) {
			outcome, err := s.MoveMerge(context.Background(), repo, named["m"], MergeMoveUp)
			return outcome.Moved(), err
		}},
		{name: "MoveMergeTo", move: func(s *store, repo RepoKey, named map[string]WorkspaceID) (bool, error) {
			return s.MoveMergeTo(context.Background(), repo, named["m"], named["a"], MergeSideAheadOf)
		}},
		{name: "PrioritizeMerge", move: func(s *store, repo RepoKey, named map[string]WorkspaceID) (bool, error) {
			return s.PrioritizeMerge(context.Background(), repo, named["m"])
		}},
		{name: "PlaceMerge", move: func(s *store, repo RepoKey, named map[string]WorkspaceID) (bool, error) {
			return s.PlaceMerge(context.Background(), repo, named["m"], MergePlace{Kind: MergePlaceFront})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, []moveStep{{"r", "admitted"}, {"a", "queued"}, {"p", "requested"}, {"m", "queued"}})

			// Act
			moved, err := tt.move(s, repo, named)

			// Assert
			if err != nil || !moved {
				t.Fatalf("move = %v, %v; want moved", moved, err)
			}
			if got, want := lettersOf(t, s, repo, named), []string{"r", "m", "p", "a"}; !slices.Equal(got, want) {
				t.Fatalf("queue = %v, want %v", got, want)
			}
		})
	}
}

func TestPlaceMergePlacesTheMerge(t *testing.T) {
	// Each case builds a queue, then places the merge named "m".
	tests := []struct {
		name      string
		steps     []moveStep
		place     func(named map[string]WorkspaceID) MergePlace
		wantOrder []string
		wantMoved bool
	}{
		{
			name:      "front passes every waiting merge, front merges included, but not the admitted one",
			steps:     []moveStep{{"r", "admitted"}, {"f", "front"}, {"a", "queued"}, {"m", "queued"}},
			place:     func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceFront} },
			wantOrder: []string{"r", "m", "f", "a"},
			wantMoved: true,
		},
		{
			name:      "back goes behind every waiting merge",
			steps:     []moveStep{{"m", "queued"}, {"a", "queued"}, {"b", "queued"}},
			place:     func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceBack} },
			wantOrder: []string{"a", "b", "m"},
			wantMoved: true,
		},
		{
			name:  "behind a waiting merge ahead of it",
			steps: []moveStep{{"a", "queued"}, {"x", "queued"}, {"b", "queued"}, {"m", "queued"}},
			place: func(named map[string]WorkspaceID) MergePlace {
				return MergePlace{Kind: MergePlaceBehind, Anchor: named["x"]}
			},
			wantOrder: []string{"a", "x", "m", "b"},
			wantMoved: true,
		},
		{
			name:  "behind a waiting merge behind it",
			steps: []moveStep{{"m", "queued"}, {"a", "queued"}, {"x", "queued"}, {"b", "queued"}},
			place: func(named map[string]WorkspaceID) MergePlace {
				return MergePlace{Kind: MergePlaceBehind, Anchor: named["x"]}
			},
			wantOrder: []string{"a", "x", "m", "b"},
			wantMoved: true,
		},
		{
			name:  "behind the running merge is the head of the waiting line",
			steps: []moveStep{{"x", "admitted"}, {"a", "queued"}, {"m", "queued"}},
			place: func(named map[string]WorkspaceID) MergePlace {
				return MergePlace{Kind: MergePlaceBehind, Anchor: named["x"]}
			},
			wantOrder: []string{"x", "m", "a"},
			wantMoved: true,
		},
		{
			name:  "behind itself stays put",
			steps: []moveStep{{"a", "queued"}, {"m", "queued"}},
			place: func(named map[string]WorkspaceID) MergePlace {
				return MergePlace{Kind: MergePlaceBehind, Anchor: named["m"]}
			},
			wantOrder: []string{"a", "m"},
			wantMoved: false,
		},
		{
			name:      "index 0 is the head of the waiting line",
			steps:     []moveStep{{"r", "admitted"}, {"a", "queued"}, {"b", "queued"}, {"m", "queued"}},
			place:     func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceIndex, Index: 0} },
			wantOrder: []string{"r", "m", "a", "b"},
			wantMoved: true,
		},
		{
			name:      "a middle index",
			steps:     []moveStep{{"m", "queued"}, {"a", "queued"}, {"b", "queued"}, {"c", "queued"}},
			place:     func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceIndex, Index: 2} },
			wantOrder: []string{"a", "b", "m", "c"},
			wantMoved: true,
		},
		{
			name:      "the last index",
			steps:     []moveStep{{"m", "queued"}, {"a", "queued"}, {"b", "queued"}},
			place:     func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceIndex, Index: 2} },
			wantOrder: []string{"a", "b", "m"},
			wantMoved: true,
		},
		{
			name:      "a merge already on its place stays put",
			steps:     []moveStep{{"a", "queued"}, {"m", "queued"}},
			place:     func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceIndex, Index: 1} },
			wantOrder: []string{"a", "m"},
			wantMoved: false,
		},
		{
			name:      "a request keeps its place",
			steps:     []moveStep{{"a", "queued"}, {"p", "requested"}, {"m", "queued"}},
			place:     func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceFront} },
			wantOrder: []string{"m", "p", "a"},
			wantMoved: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, tt.steps)

			// Act
			moved, err := s.PlaceMerge(context.Background(), repo, named["m"], tt.place(named))

			// Assert
			if err != nil {
				t.Fatalf("PlaceMerge: %v", err)
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

func TestPlaceMergeGivesTheMovedMergeTheFrontMarkOfItsPlace(t *testing.T) {
	tests := []struct {
		name      string
		steps     []moveStep
		place     MergePlace
		wantFront bool
	}{
		{name: "an ordinary merge put at the front becomes a front merge", steps: []moveStep{{"a", "queued"}, {"m", "queued"}}, place: MergePlace{Kind: MergePlaceFront}, wantFront: true},
		{name: "a front merge put at the back of ordinary merges becomes ordinary", steps: []moveStep{{"m", "front"}, {"a", "queued"}}, place: MergePlace{Kind: MergePlaceBack}, wantFront: false},
		{name: "an ordinary merge put directly behind a front merge becomes a front merge", steps: []moveStep{{"f", "front"}, {"g", "front"}, {"a", "queued"}, {"m", "queued"}}, place: MergePlace{Kind: MergePlaceIndex, Index: 1}, wantFront: true},
		{name: "a front merge put directly behind an ordinary merge becomes ordinary", steps: []moveStep{{"m", "front"}, {"a", "queued"}, {"b", "queued"}}, place: MergePlace{Kind: MergePlaceIndex, Index: 1}, wantFront: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, tt.steps)

			// Act
			if _, err := s.PlaceMerge(context.Background(), repo, named["m"], tt.place); err != nil {
				t.Fatalf("PlaceMerge: %v", err)
			}

			// Assert
			if got := frontMarkOf(t, s, repo, named["m"]); got != tt.wantFront {
				t.Fatalf("m's front mark = %v, want %v", got, tt.wantFront)
			}
		})
	}
}

func TestPlaceMergeRefusesAndMovesNothing(t *testing.T) {
	// Each case refuses a move on the queue x (admitted), a, p (requested), m.
	tests := []struct {
		name   string
		moving string
		place  func(named map[string]WorkspaceID) MergePlace
		want   func(err error) bool
	}{
		{name: "a workspace with no merge", moving: "stranger",
			place: func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceFront} },
			want:  func(err error) bool { return errors.Is(err, ErrNotFound) }},
		{name: "the running merge", moving: "x",
			place: func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceBack} },
			want:  func(err error) bool { return errors.Is(err, ErrMergeNotWaiting) }},
		{name: "a merge only requested", moving: "p",
			place: func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceFront} },
			want:  func(err error) bool { return errors.Is(err, ErrMergeNotWaiting) }},
		{name: "behind a workspace with no merge", moving: "m",
			place: func(named map[string]WorkspaceID) MergePlace {
				return MergePlace{Kind: MergePlaceBehind, Anchor: named["stranger"]}
			},
			want: func(err error) bool { return errors.Is(err, ErrAnchorNotInLine) }},
		{name: "behind a merge only requested", moving: "m",
			place: func(named map[string]WorkspaceID) MergePlace {
				return MergePlace{Kind: MergePlaceBehind, Anchor: named["p"]}
			},
			want: func(err error) bool { return errors.Is(err, ErrAnchorNotInLine) }},
		{name: "an index past the last waiting place", moving: "m",
			place: func(map[string]WorkspaceID) MergePlace { return MergePlace{Kind: MergePlaceIndex, Index: 2} },
			want: func(err error) bool {
				var outOfRange *MergePlaceOutOfRangeError
				return errors.As(err, &outOfRange) && outOfRange.Index == 2 && outOfRange.Waiting == 2
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			repo := RepoKey(t.TempDir())
			named := buildMoveQueue(t, s, repo, []moveStep{{"x", "admitted"}, {"a", "queued"}, {"p", "requested"}, {"m", "queued"}})
			named["stranger"] = testWorkspace(t, s).ID

			// Act
			moved, err := s.PlaceMerge(context.Background(), repo, named[tt.moving], tt.place(named))

			// Assert
			if moved || !tt.want(err) {
				t.Fatalf("PlaceMerge = %v, %v; want the refusal", moved, err)
			}
			if loggedOperation(log, "daemon.wsm.place_merge", "error") {
				t.Fatalf("a refusal was logged at error: %v", log.Records())
			}
			if got, want := lettersOf(t, s, repo, named), []string{"x", "a", "p", "m"}; !slices.Equal(got, want) {
				t.Fatalf("queue = %v, want %v", got, want)
			}
		})
	}
}

func TestPlaceMergeRefusesAnUndeclaredPlaceAndMovesNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repo := RepoKey(t.TempDir())
	named := buildMoveQueue(t, s, repo, []moveStep{{"a", "queued"}, {"m", "queued"}})

	// Act
	moved, err := s.PlaceMerge(context.Background(), repo, named["m"], MergePlace{})

	// Assert
	if err == nil || moved {
		t.Fatalf("PlaceMerge(unset) = %v, %v; want a refusal", moved, err)
	}
	if !loggedOperation(log, "daemon.wsm.place_merge", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
	if got, want := lettersOf(t, s, repo, named), []string{"a", "m"}; !slices.Equal(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}
