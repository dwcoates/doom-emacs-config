package wsm

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
)

// loggedAt reports whether LOG holds an ERROR record of OPERATION.
func loggedAt(log *dlog.TestLogger, operation string) bool {
	for _, record := range log.Records() {
		if record.Operation == operation && record.Level == "error" {
			return true
		}
	}
	return false
}

func TestTheMigrationRevealsEveryExistingMergeWorkspace(t *testing.T) {
	// Arrange: a layout-38 file holding a merge workspace forked from the
	// fixture's workspace.
	path := fixtureAt(t, 38)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, place, merge_of, merge_request_id)
		VALUES ('ws-m', 'repo-1', '/tmp/ws-m', 'feature-merge', 'feature-merge', 'master', 0, 0, 0, 1, 50, ?, 'req-1')`, string(fixtureWorkspaceID))

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("re-Open: %v", err)
	}
	defer handle.Close()
	merged, err := handle.Workspace(context.Background(), "ws-m")
	ordinary, err2 := handle.Workspace(context.Background(), fixtureWorkspaceID)

	// Assert
	if err != nil || err2 != nil {
		t.Fatalf("Workspace = %v / %v", err, err2)
	}
	if !merged.MergeRevealed {
		t.Fatal("the existing merge workspace migrated hidden, want revealed")
	}
	if ordinary.MergeRevealed {
		t.Fatal("an ordinary workspace migrated as a revealed merge workspace")
	}
}

func TestTheMigrationNamesAnAsyncRequestAnEarlierBuildRecorded(t *testing.T) {
	// Arrange: a layout-38 file holding a detached request of the fixture's
	// workspace.
	path := fixtureAt(t, 38)
	execRaw(t, path, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, source_keep_open, source_branch, source_detach_id)
		VALUES ('/tmp/repo-1', ?, 1, ?, 1, ?, 0, 'feature', 'req-1')`, string(fixtureWorkspaceID), int(MergeRequested), int(MergeSourceOwnBranch))

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("re-Open: %v", err)
	}
	defer handle.Close()
	queues, err := handle.AllMergeQueues(context.Background())
	var queue []MergeQueueEntry
	for _, entries := range queues {
		queue = append(queue, entries...)
	}

	// Assert
	if err != nil || len(queue) != 1 {
		t.Fatalf("AllMergeQueues = %+v, %v", queues, err)
	}
	if queue[0].Source.DetachName != "feature-merge" {
		t.Fatalf("DetachName = %q, want the name the earlier build forked under", queue[0].Source.DetachName)
	}
}

func TestRevealMergeWorkspaceRevealsAHiddenMergeWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	forked := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())

	// Act
	revealed, err := s.RevealMergeWorkspace(context.Background(), forked.ID)
	loaded, loadErr := s.Workspace(context.Background(), forked.ID)

	// Assert
	if err != nil || !revealed {
		t.Fatalf("RevealMergeWorkspace = %v, %v; want revealed", revealed, err)
	}
	if loadErr != nil || !loaded.MergeRevealed {
		t.Fatalf("Workspace = %+v, %v; want MergeRevealed", loaded, loadErr)
	}
}

func TestAMergeWorkspaceIsRegisteredHidden(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)

	// Act
	forked := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())
	loaded, err := s.Workspace(context.Background(), forked.ID)

	// Assert
	if err != nil || loaded.MergeRevealed {
		t.Fatalf("Workspace = %+v, %v; want a hidden merge workspace", loaded, err)
	}
}

func TestRevealMergeWorkspaceAnswersFalseForOneAlreadyRevealed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	forked := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())
	if _, err := s.RevealMergeWorkspace(context.Background(), forked.ID); err != nil {
		t.Fatalf("first reveal: %v", err)
	}

	// Act
	revealed, err := s.RevealMergeWorkspace(context.Background(), forked.ID)

	// Assert
	if err != nil || revealed {
		t.Fatalf("RevealMergeWorkspace = %v, %v; want no change", revealed, err)
	}
}

func TestRevealMergeWorkspaceRefusesAnOrdinaryWorkspace(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.RevealMergeWorkspace(context.Background(), ws.ID)
	loaded, loadErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no merge workspace") {
		t.Fatalf("RevealMergeWorkspace = %v, want the refusal", err)
	}
	if loadErr != nil || loaded.MergeRevealed {
		t.Fatalf("the refused reveal changed the row: %+v, %v", loaded, loadErr)
	}
	if !loggedAt(logs, "daemon.wsm.reveal_merge_workspace") {
		t.Fatal("the refusal was not recorded under the operation")
	}
}

func TestRevealMergeWorkspaceRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.RevealMergeWorkspace(context.Background(), "nobody")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RevealMergeWorkspace = %v, want ErrNotFound", err)
	}
}

func TestReservedMergeNamesAnswersEveryStandingAsyncRequest(t *testing.T) {
	// Arrange: one async request, and one sync request that reserves nothing.
	s, _ := testStore(t)
	asyncWS := testWorkspaceNamed(t, s, "a")
	syncWS := testWorkspaceNamed(t, s, "b")
	repo := RepoKey(t.TempDir())
	requestDetached(t, s, repo, asyncWS.ID, NewDetachedMergeID())
	if err := s.RequestMerge(context.Background(), repo, syncWS.ID, MergeSource{Kind: MergeSourceOwnBranch}, time.Unix(100, 0)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act
	got, err := s.ReservedMergeNames(context.Background())

	// Assert
	if err != nil || len(got) != 1 || got["sample"] != asyncWS.ID {
		t.Fatalf("ReservedMergeNames = %v, %v; want only the async request's name", got, err)
	}
}

func TestAHandOffReleasesTheRequestersReservation(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()
	requestDetached(t, s, repo, requester.ID, request)
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	if err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample", "", time.Unix(200, 0)); err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	got, err := s.ReservedMergeNames(context.Background())

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("ReservedMergeNames = %v, %v; want none once the merge workspace exists", got, err)
	}
}

func TestAHandOffCarriesThePickOntoTheMergeWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()
	requestDetached(t, s, repo, requester.ID, request)
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample", "take the fix", time.Unix(200, 0))
	queue, qerr := s.MergeQueue(context.Background(), repo)

	// Assert
	if err != nil || qerr != nil || len(queue) != 1 {
		t.Fatalf("hand-off = %v, queue = %+v, %v", err, queue, qerr)
	}
	if queue[0].Workspace != forked.ID || queue[0].Source.Pick != "take the fix" || queue[0].Source.Detach != "" {
		t.Fatalf("queue = %+v, want the merge workspace's in-place request carrying the pick", queue[0])
	}
}

// handedOffPick hands a pick request over to its merge workspace and answers
// the queue's key and the merge workspace.
func handedOffPick(t *testing.T, s *store) (RepoKey, Workspace) {
	t.Helper()
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()
	requestDetached(t, s, repo, requester.ID, request)
	forked := registerMergeWorkspace(t, s, requester.ID, request)
	if err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample", "take the fix", time.Unix(200, 0)); err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	return repo, forked
}

func TestStartMergePickRecordsTheTurnTheLedgerAndTheStart(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo, forked := handedOffPick(t, s)
	turn := NewTurnID()

	// Act
	err := s.StartMergePick(context.Background(), repo, forked.ID, "ledger-1", turn, time.Unix(300, 0))
	queue, qerr := s.MergeQueue(context.Background(), repo)

	// Assert
	if err != nil || qerr != nil || len(queue) != 1 {
		t.Fatalf("StartMergePick = %v, queue = %+v, %v", err, queue, qerr)
	}
	got := queue[0]
	if got.Ledger != "ledger-1" || got.Source.PickTurn != turn || got.Source.PickStartedAt == nil || !got.Source.PickStartedAt.Equal(time.Unix(300, 0)) {
		t.Fatalf("entry = %+v, want the ledger, the turn and the start", got)
	}
}

func TestStartMergePickRefusesAPickThatAlreadyBegan(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo, forked := handedOffPick(t, s)
	if err := s.StartMergePick(context.Background(), repo, forked.ID, "ledger-1", NewTurnID(), time.Unix(300, 0)); err != nil {
		t.Fatalf("first start: %v", err)
	}

	// Act
	err := s.StartMergePick(context.Background(), repo, forked.ID, "ledger-2", NewTurnID(), time.Unix(400, 0))
	queue, _ := s.MergeQueue(context.Background(), repo)

	// Assert
	if err == nil {
		t.Fatal("StartMergePick restarted a begun pick, want a refusal")
	}
	if queue[0].Ledger != "ledger-1" {
		t.Fatalf("the refused start rewrote the row: %+v", queue[0])
	}
}

func TestStartMergePickRefusesARequestThatIsNoPick(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	if err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch}, time.Unix(100, 0)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act
	err := s.StartMergePick(context.Background(), repo, ws.ID, "ledger-1", NewTurnID(), time.Unix(300, 0))

	// Assert
	if err == nil {
		t.Fatal("StartMergePick started a request that carries no pick, want a refusal")
	}
}

func TestStartMergePickRefusesAMissingLedgerOrTurn(t *testing.T) {
	tests := []struct {
		name   string
		ledger LeaseID
		turn   TurnID
	}{
		{name: "no ledger", ledger: "", turn: "t-1"},
		{name: "no turn", ledger: "ledger-1", turn: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repo, forked := handedOffPick(t, s)

			// Act
			err := s.StartMergePick(context.Background(), repo, forked.ID, tc.ledger, tc.turn, time.Unix(300, 0))
			queue, _ := s.MergeQueue(context.Background(), repo)

			// Assert
			if err == nil {
				t.Fatal("StartMergePick accepted a start with no ledger or turn")
			}
			if queue[0].Source.PickStartedAt != nil {
				t.Fatalf("the refused start wrote the row: %+v", queue[0])
			}
			if !loggedAt(logs, "daemon.wsm.start_merge_pick") {
				t.Fatal("the refusal was not recorded under the operation")
			}
		})
	}
}

func TestATurnMirrorRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := feedid.LeaseID("ledger-1")
	mirror := &MirrorAddress{
		Workspace: "requester",
		Feed:      feedid.Feed{Merge: &lease},
		Parent:    feedid.Ref{WS: "requester", Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "pick", Sub: "1"}},
	}
	turn := NewTurnID()

	// Act
	if err := s.PutTurn(context.Background(), Turn{ID: turn, Workspace: ws.ID, Text: "pick", Origin: "merge", Mirror: mirror, StartedAt: time.Unix(1, 0)}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	got, err := s.TurnMirrors(context.Background(), ws.ID, []TurnID{turn})

	// Assert
	if err != nil || got[turn] == nil {
		t.Fatalf("TurnMirrors = %v, %v; want the mirror", got, err)
	}
	if got[turn].Workspace != "requester" || *got[turn].Feed.Merge != lease || got[turn].Parent.Row != mirror.Parent.Row ||
		got[turn].Parent.WS != mirror.Parent.WS || *got[turn].Parent.Feed.Merge != lease {
		t.Fatalf("mirror = %+v, want %+v", got[turn], mirror)
	}
}

func TestATurnWithNoMirrorIsAbsentFromTurnMirrors(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := NewTurnID()
	if err := s.PutTurn(context.Background(), Turn{ID: turn, Workspace: ws.ID, Text: "x", Origin: "webapp", StartedAt: time.Unix(1, 0)}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	got, err := s.TurnMirrors(context.Background(), ws.ID, []TurnID{turn})

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("TurnMirrors = %v, %v; want none", got, err)
	}
}

func TestPutTurnRefusesAMirrorNamingNoWorkspace(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	turn := NewTurnID()

	// Act
	err := s.PutTurn(context.Background(), Turn{ID: turn, Workspace: ws.ID, Text: "x", Origin: "merge", Mirror: &MirrorAddress{Feed: feedid.Feed{Root: true}}, StartedAt: time.Unix(1, 0)})
	recorded, _ := s.RecordedTurns(context.Background(), ws.ID, []TurnID{turn})

	// Assert
	if err == nil {
		t.Fatal("PutTurn accepted a mirror naming no workspace")
	}
	if recorded[turn] {
		t.Fatal("the refused turn was recorded")
	}
	if !loggedAt(logs, "daemon.wsm.put_turn") {
		t.Fatal("the refusal was not recorded under the operation")
	}
}

func TestMergeSourceValidateRefusesInconsistentAsyncAndPickFields(t *testing.T) {
	started := time.Unix(1, 0)
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "detached with no name", source: MergeSource{Kind: MergeSourceOwnBranch, Detach: "d", DetachMaker: "maker"}},
		{name: "a name with no detach", source: MergeSource{Kind: MergeSourceOwnBranch, DetachName: "n"}},
		{name: "a pick on a branch source", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", Pick: "p"}},
		{name: "a pick kept open", source: MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true, Pick: "p"}},
		{name: "a begun pick with no turn", source: MergeSource{Kind: MergeSourceOwnBranch, Pick: "p", PickStartedAt: &started}},
		{name: "a turn with no start", source: MergeSource{Kind: MergeSourceOwnBranch, Pick: "p", PickTurn: "t"}},
		{name: "a begun pick with no pick", source: MergeSource{Kind: MergeSourceOwnBranch, PickStartedAt: &started, PickTurn: "t"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := tc.source.validate()

			// Assert
			if err == nil {
				t.Fatalf("validate accepted %+v", tc.source)
			}
		})
	}
}

func TestMergeHidden(t *testing.T) {
	landed := time.Unix(100, 0)
	origin := &MergeOrigin{Requester: "ws-r", Request: "req-1"}
	tests := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "an unrevealed, unmerged merge workspace", ws: Workspace{MergeOf: origin}, want: true},
		{name: "a revealed merge workspace", ws: Workspace{MergeOf: origin, MergeRevealed: true}, want: false},
		{name: "a landed merge workspace never revealed", ws: Workspace{MergeOf: origin, MergedAt: &landed}, want: false},
		{name: "an ordinary workspace", ws: Workspace{}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.ws.MergeHidden()

			// Assert
			if got != tt.want {
				t.Fatalf("MergeHidden() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestNoCallSiteSpellsOutMergeHidden holds every reader of a merge workspace's
// visibility to MergeHidden: a site that tests MergeRevealed by hand would drift
// from it (the landed case is the one a hand-rolled test forgets).
func TestNoCallSiteSpellsOutMergeHidden(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if strings.HasSuffix(path, filepath.Join("wsm", "syncasyncmerges.go")) || strings.HasSuffix(path, filepath.Join("wsm", "workspaces.go")) {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(src), ".MergeRevealed") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("these files read MergeRevealed by hand instead of MergeHidden: %v", offenders)
	}
}
