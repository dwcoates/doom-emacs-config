package wsm

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/semver"
	"agent-repld/internal/sourcescan"
)

// fullRequest is a request carrying every value a merge is asked for with,
// so a test can see each one kept off the queue and brought back.
var fullRequest = MergeSource{Kind: MergeSourceOwnBranch, Branch: "feature", ProfileGate: true, Version: &VersionIncrement{Part: semver.Minor, ChangelogLine: "Merges keep their requests"}}

// exits are the two store writes a merge leaves its queue through.
var exits = []struct {
	name string
	exit func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID, cause string) error
}{
	{name: "a waiting merge's removal", exit: func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID, cause string) error {
		queuedWith(t, s, repo, ws, fullRequest)
		return s.RemoveMergeQueueEntry(context.Background(), repo, ws, cause)
	}},
	{name: "an admitted merge's drop", exit: func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID, cause string) error {
		queuedWith(t, s, repo, ws, fullRequest)
		if err := s.AdmitMerge(context.Background(), repo, ws); err != nil {
			t.Fatalf("AdmitMerge: %v", err)
		}
		lease, err := s.AcquireLease(context.Background(), ws, HolderMerge, PolicyHold)
		if err != nil {
			t.Fatalf("AcquireLease: %v", err)
		}
		return s.DropAdmittedMerge(context.Background(), repo, ws, lease.ID, cause)
	}},
}

func TestAnExitShortOfALandingKeepsTheRequestOffTheQueue(t *testing.T) {
	for _, tt := range exits {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())

			// Act
			err := tt.exit(t, s, repo, ws.ID, "evicted")

			// Assert: off the queue, its request kept with its cause, the
			// workspace left open, and the keeping recorded.
			if err != nil {
				t.Fatalf("the exit: %v", err)
			}
			if entries, _ := s.MergeQueue(context.Background(), repo); len(entries) != 0 {
				t.Fatalf("after the exit the queue holds %+v, want nothing", entries)
			}
			rec, found, err := s.OffQueueMergeOf(context.Background(), ws.ID)
			if err != nil || !found {
				t.Fatalf("OffQueueMergeOf = %+v, %v, %v; want the request kept", rec, found, err)
			}
			if rec.Cause != "evicted" || rec.Adopted || !rec.RequestedAt.Equal(instant) {
				t.Fatalf("record = %+v, want cause evicted, not adopted, requested at %v", rec, instant)
			}
			if !reflect.DeepEqual(rec.Source, fullRequest) {
				t.Fatalf("kept request = %+v, want %+v", rec.Source, fullRequest)
			}
			if workspaceNow(t, s, ws.ID).Closed {
				t.Fatal("the exit closed the merge's workspace")
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo,
				Message: "the merge left its queue without landing; its request stands off the queue until it is requeued or discarded"}); !ok {
				t.Fatalf("the kept request was not recorded: %v", log.Records())
			}
		})
	}
}

func TestALandingEndsTheRequest(t *testing.T) {
	for _, tt := range exits {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())

			// Act
			err := tt.exit(t, s, repo, ws.ID, MergeLeftLanded)

			// Assert
			if err != nil {
				t.Fatalf("the exit: %v", err)
			}
			if rec, found, _ := s.OffQueueMergeOf(context.Background(), ws.ID); found {
				t.Fatalf("a landed merge kept its request: %+v", rec)
			}
		})
	}
}

func TestAClosedWorkspacesExitKeepsNoRequest(t *testing.T) {
	// Arrange: the workspace is closed before its merge is taken off, as a
	// kill closes it first.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, fullRequest)
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	err := s.RemoveMergeQueueEntry(context.Background(), repo, ws.ID, "workspace_closed")

	// Assert
	if err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	if rec, found, _ := s.OffQueueMergeOf(context.Background(), ws.ID); found {
		t.Fatalf("a closed workspace kept a request: %+v", rec)
	}
}

func TestAnExitWithNoCauseIsRefusedAndChangesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, fullRequest)

	// Act
	err := s.RemoveMergeQueueEntry(context.Background(), repo, ws.ID, "")

	// Assert
	if err == nil {
		t.Fatal("an exit with no cause was accepted")
	}
	if entries, _ := s.MergeQueue(context.Background(), repo); len(entries) != 1 {
		t.Fatalf("the queue holds %d entries, want the one kept", len(entries))
	}
	if _, found, _ := s.OffQueueMergeOf(context.Background(), ws.ID); found {
		t.Fatal("a refused exit kept a request off the queue")
	}
}

func TestARefusedExitKeepsNoRequest(t *testing.T) {
	tests := []struct {
		name string
		exit func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) error
	}{
		{name: "a removal of an entry not queued", exit: func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) error {
			return s.RemoveMergeQueueEntry(context.Background(), repo, ws, "evicted")
		}},
		{name: "a drop naming another lease", exit: func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) error {
			admittedWithLease(t, s, repo, ws)
			return s.DropAdmittedMerge(context.Background(), repo, ws, NewLeaseID(), "evicted")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())

			// Act
			err := tt.exit(t, s, repo, ws.ID)

			// Assert: refused, and the record's write rolled back with it.
			if !errors.Is(err, ErrNotFound) {
				t.Fatalf("the exit = %v, want the refusal", err)
			}
			if _, found, _ := s.OffQueueMergeOf(context.Background(), ws.ID); found {
				t.Fatal("a refused exit kept a request off the queue")
			}
		})
	}
}

// TestNoQueueRowIsDeletedOutsideTheOneTransition holds every exit to
// leaveQueueInTx: a statement deleting a queue row anywhere else could delete
// a request it did not land. The one other delete is the layout-128 move of
// async requests into their own table, which deletes only what it copied.
func TestNoQueueRowIsDeletedOutsideTheOneTransition(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "DELETE FROM merge_queue ")

	// Assert
	if want := []string{"asyncrequests.go", "offqueue.go"}; !slices.Equal(sites, want) {
		t.Fatalf("merge_queue rows are deleted in %v, want only the layout-128 move and the one transition (%v)", sites, want)
	}
}

// Every refusal of a workspace already on its queue is refuseQueued's, so a
// request and a requeue answer the same place in the same words.
func TestOneReadRefusesAWorkspaceAlreadyOnItsQueue(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "SELECT seq, state FROM merge_queue WHERE repo_key = ? AND workspace_id = ?")
	callers := sourcescan.Sites(t, "refuseQueued(ctx, tx, key, id)")

	// Assert
	if want := []string{"offqueue.go"}; !slices.Equal(sites, want) {
		t.Fatalf("a workspace's queue row is read for a refusal in %v, want only refuseQueued's %v", sites, want)
	}
	if want := []string{"mergequeue.go", "offqueue.go"}; !slices.Equal(callers, want) {
		t.Fatalf("refuseQueued is called in %v, want the request and the requeue (%v)", callers, want)
	}
}

// TestBothQueueReadersDecodeARequestThroughTheOneScan pins the shared
// requestScan: the queue's reader and the off-queue reader answer the same
// request for the same row.
func TestBothQueueReadersDecodeARequestThroughTheOneScan(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, fullRequest)
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(entries) != 1 {
		t.Fatalf("MergeQueue = %+v, %v", entries, err)
	}

	// Act
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, ws.ID, "failed"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	rec, _, err := s.OffQueueMergeOf(context.Background(), ws.ID)

	// Assert
	if err != nil || !reflect.DeepEqual(rec.Source, entries[0].Source) {
		t.Fatalf("off the queue the request reads %+v (%v), in line it read %+v", rec.Source, err, entries[0].Source)
	}
}

func TestOffQueueMergesListsOldestDepartureFirst(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	clock := &slotClock{at: instant}
	s.now = clock.now
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, second.ID, fullRequest)
	queuedWith(t, s, repo, first.ID, fullRequest)
	clock.at = instant
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, first.ID, "evicted"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	clock.at = instant.Add(time.Minute)
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, second.ID, "failed"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Act
	got, err := s.OffQueueMerges(context.Background())

	// Assert
	if err != nil || len(got) != 2 || got[0].Workspace != first.ID || got[1].Workspace != second.ID {
		t.Fatalf("OffQueueMerges = %+v, %v; want %s then %s", got, err, first.ID, second.ID)
	}
	if !got[0].LeftAt.Equal(instant) {
		t.Fatalf("LeftAt = %v, want %v", got[0].LeftAt, instant)
	}
}

func TestAWorkspacesCloseEndsItsRequestOffTheQueue(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, fullRequest)
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, ws.ID, "evicted"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Act
	err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	if rec, found, _ := s.OffQueueMergeOf(context.Background(), ws.ID); found {
		t.Fatalf("the close left the request standing: %+v", rec)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo,
		Message: "the close ended the workspace's merge request standing off its queue"}); !ok {
		t.Fatalf("the close's end of the request was not recorded: %v", log.Records())
	}
}

// offQueued puts WS's full request in line in REPO and takes it off, so it
// stands off its queue.
func offQueued(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) {
	t.Helper()
	queuedWith(t, s, repo, ws, fullRequest)
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, ws, "evicted"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
}

func TestRequeueOffQueueMergePutsTheSameRequestBackAsARequest(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	offQueued(t, s, repo, ws.ID)

	// Act
	source, err := s.RequeueOffQueueMerge(context.Background(), repo, ws.ID, RequeuePlacement{}, nil, "", instant.Add(time.Hour))

	// Assert: requested again, with every value it was asked for, and its
	// record gone.
	if err != nil {
		t.Fatalf("RequeueOffQueueMerge: %v", err)
	}
	if !reflect.DeepEqual(source, fullRequest) {
		t.Fatalf("requeued request = %+v, want %+v", source, fullRequest)
	}
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(entries) != 1 || entries[0].State != MergeRequested || !reflect.DeepEqual(entries[0].Source, fullRequest) {
		t.Fatalf("queue = %+v, %v; want the request, requested", entries, err)
	}
	if !entries[0].EnqueuedAt.Equal(instant) {
		t.Fatalf("EnqueuedAt = %v, want the original request's %v", entries[0].EnqueuedAt, instant)
	}
	if _, found, _ := s.OffQueueMergeOf(context.Background(), ws.ID); found {
		t.Fatal("the requeue left the record standing")
	}
}

func TestRequeueOffQueueMergeTakesTheNewPlacement(t *testing.T) {
	tests := []struct {
		name      string
		place     func(ahead WorkspaceID) RequeuePlacement
		wantFront bool
		wantAhead bool
	}{
		{name: "at the back", place: func(WorkspaceID) RequeuePlacement { return RequeuePlacement{} }},
		{name: "at the front", place: func(WorkspaceID) RequeuePlacement { return RequeuePlacement{Front: true} }, wantFront: true},
		{name: "behind a merge", place: func(ahead WorkspaceID) RequeuePlacement {
			return RequeuePlacement{Behind: ahead, BehindName: "ahead"}
		}, wantAhead: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			ahead := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			offQueued(t, s, repo, ws.ID)
			queued(t, s, repo, ahead.ID, instant)

			// Act
			source, err := s.RequeueOffQueueMerge(context.Background(), repo, ws.ID, tt.place(ahead.ID), nil, "", instant)

			// Assert
			if err != nil {
				t.Fatalf("RequeueOffQueueMerge: %v", err)
			}
			if source.Front != tt.wantFront || (source.Behind == ahead.ID) != tt.wantAhead {
				t.Fatalf("requeued placement front %v behind %q, want front %v behind ahead %v", source.Front, source.Behind, tt.wantFront, tt.wantAhead)
			}
		})
	}
}

func TestRequeueOffQueueMergeRefusals(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID)
		version *VersionIncrement
		want    func(error) bool
	}{
		{name: "no request off the queue", arrange: func(*testing.T, *store, RepoKey, WorkspaceID) {},
			want: func(err error) bool { return errors.Is(err, ErrNotFound) }},
		{name: "a merge standing in line", arrange: func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) {
			queued(t, s, repo, ws, instant)
		}, want: func(err error) bool { var q *MergeQueuedError; return errors.As(err, &q) }},
		{name: "a version for a request that recorded its own", arrange: func(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) {
			offQueued(t, s, repo, ws)
		}, version: &VersionIncrement{Part: semver.Patch, ChangelogLine: "another"},
			want: func(err error) bool { return errors.Is(err, ErrOffQueueVersionRecorded) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			tt.arrange(t, s, repo, ws.ID)
			before, _ := s.MergeQueue(context.Background(), repo)
			recBefore, foundBefore, _ := s.OffQueueMergeOf(context.Background(), ws.ID)

			// Act
			_, err := s.RequeueOffQueueMerge(context.Background(), repo, ws.ID, RequeuePlacement{}, tt.version, "", instant)

			// Assert: refused, and nothing changed.
			if !tt.want(err) {
				t.Fatalf("RequeueOffQueueMerge = %v, want the refusal", err)
			}
			after, _ := s.MergeQueue(context.Background(), repo)
			recAfter, foundAfter, _ := s.OffQueueMergeOf(context.Background(), ws.ID)
			if len(after) != len(before) || foundAfter != foundBefore || !reflect.DeepEqual(recAfter, recBefore) {
				t.Fatalf("the refusal changed the queue (%d -> %d) or the record (%v -> %v)", len(before), len(after), foundBefore, foundAfter)
			}
		})
	}
}

// A WORKSPACE HOLDS ONE MERGE, IN LINE OR OFF IT: a second request beside a
// request off its queue is refused in the transaction that would write it,
// whichever request verb asks.
func TestARequestBesideARequestOffItsQueueIsRefused(t *testing.T) {
	tests := []struct {
		name      string
		requester func(t *testing.T, s *store) WorkspaceID
		request   func(s *store, repo RepoKey, ws WorkspaceID) error
	}{
		{name: "a merge request", requester: func(t *testing.T, s *store) WorkspaceID { return testWorkspace(t, s).ID },
			request: func(s *store, repo RepoKey, ws WorkspaceID) error {
				return s.RequestMerge(context.Background(), repo, ws, ownBranch, instant.Add(time.Hour))
			}},
		{name: "a one-shot merge request", requester: func(t *testing.T, s *store) WorkspaceID { return oneShotRequester(t, s).ID },
			request: func(s *store, repo RepoKey, ws WorkspaceID) error {
				return s.RequestOneShotMerge(context.Background(), repo, ws, ownBranch, instant.Add(time.Hour))
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := tt.requester(t, s)
			repo := RepoKey(t.TempDir())
			offQueued(t, s, repo, ws)
			recBefore, _, _ := s.OffQueueMergeOf(context.Background(), ws)

			// Act
			err := tt.request(s, repo, ws)

			// Assert: refused with why it left, and nothing changed.
			var refusal *MergeOffQueueError
			if !errors.As(err, &refusal) || refusal.Workspace != ws || refusal.Cause != "evicted" {
				t.Fatalf("request = %v, want a *MergeOffQueueError naming the workspace and its cause", err)
			}
			if entries, _ := s.MergeQueue(context.Background(), repo); len(entries) != 0 {
				t.Fatalf("the refused request wrote a queue row: %+v", entries)
			}
			if recAfter, found, _ := s.OffQueueMergeOf(context.Background(), ws); !found || !reflect.DeepEqual(recAfter, recBefore) {
				t.Fatalf("the refused request changed the record: %+v -> %+v (found %v)", recBefore, recAfter, found)
			}
			// The refusal is an answer the orchestrator turns into the
			// request's merge-off-queue arm, so the store records it at debug
			// and never as an error.
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelDebug,
				Message: "the workspace's merge stands off its queue; the request was refused"}); !ok {
				t.Fatalf("the refusal was not recorded at debug: %v", log.Records())
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError}); ok {
				t.Fatalf("the refusal was recorded as an error: %v", log.Records())
			}
		})
	}
}

// Layout 134 ends a record that stood off its queue beside a queue row of
// the same workspace, and keeps every other record.
func TestTheMigrationEndsARecordBesideAQueueRow(t *testing.T) {
	tests := []struct {
		name     string
		arrange  string
		wantKept bool
	}{
		{name: "a record beside a queue row of its workspace", wantKept: false,
			arrange: `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, source_pre_deploy_hooks, source_post_deploy_hooks) VALUES ('/tmp/repo-1', 'ws-layout3', 1, 1, 1, 0, '[]', '[]')`},
		{name: "a record with no queue row", wantKept: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a layout-133 file whose one workspace holds a record
			// off its queue.
			path := fixtureAt(t, 133)
			execRaw(t, path, `INSERT INTO off_queue_merges (workspace_id, cause, left_at, adopted, enqueued_at, `+adoptedRequestColumns+`)
				VALUES (?, 'failed', 1, 0, 1, 0, 0, NULL, 'feature', NULL, NULL, NULL, 0, NULL, 0, NULL, NULL, NULL, '[]', '[]', NULL, 0)`, string(fixtureWorkspaceID))
			if tt.arrange != "" {
				execRaw(t, path, tt.arrange)
			}

			// Act
			handle, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			defer handle.Close()
			kept := rawScalar[int](t, path, `SELECT count(*) FROM off_queue_merges WHERE workspace_id = ?`, string(fixtureWorkspaceID))

			// Assert
			if (kept == 1) != tt.wantKept {
				t.Fatalf("kept %d records, want kept %v", kept, tt.wantKept)
			}
		})
	}
}

func TestRequeueOffQueueMergeGivesAnAdoptedRequestTheVersionSupplied(t *testing.T) {
	// Arrange: an adopted record, as the migration writes one.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	corrupt(t, s, `INSERT INTO off_queue_merges (workspace_id, cause, left_at, adopted, enqueued_at, `+adoptedRequestColumns+`)
		VALUES (?, 'adopted', 1, 1, 1, 0, 0, NULL, 'feature', NULL, NULL, NULL, 0, NULL, 0, NULL, NULL, NULL, '[]', '[]', NULL, 0)`, ws.ID)
	version := &VersionIncrement{Part: semver.Patch, ChangelogLine: "Adopted merges land again"}

	// Act
	source, err := s.RequeueOffQueueMerge(context.Background(), repo, ws.ID, RequeuePlacement{}, version, "", instant)

	// Assert
	if err != nil || !reflect.DeepEqual(source.Version, version) || source.Branch != "feature" {
		t.Fatalf("RequeueOffQueueMerge = %+v, %v; want the feature branch with the supplied version", source, err)
	}
}

func TestDiscardOffQueueMergeEndsTheRequest(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	offQueued(t, s, repo, ws.ID)

	// Act
	err := s.DiscardOffQueueMerge(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("DiscardOffQueueMerge: %v", err)
	}
	if _, found, _ := s.OffQueueMergeOf(context.Background(), ws.ID); found {
		t.Fatal("the discard left the request standing")
	}
	if workspaceNow(t, s, ws.ID).Closed {
		t.Fatal("discarding a workspace's own merge closed the workspace")
	}
	if !loggedOperation(log, "daemon.wsm.discard_off_queue_merge", "info") {
		t.Fatalf("the discard was not recorded: %v", log.Records())
	}
}

func TestDiscardOffQueueMergeRefusesAWorkspaceWithNoRequest(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.DiscardOffQueueMerge(context.Background(), ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("DiscardOffQueueMerge = %v, want ErrNotFound", err)
	}
}

// TestTheMigrationAdoptsOpenMergeWorkspacesNothingReferences covers the
// layout-129 adoption: only an open merge workspace with no queue row, no
// record and no async request standing for it is adopted.
func TestTheMigrationAdoptsOpenMergeWorkspacesNothingReferences(t *testing.T) {
	tests := []struct {
		name      string
		arrange   string
		wantAdopt bool
	}{
		{name: "an open merge workspace nothing references", wantAdopt: true},
		{name: "a merge workspace standing in its queue",
			arrange: `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind) VALUES ('/tmp/repo-1', 'ws-layout3', 1, 1, 1, 0)`},
		{name: "a closed merge workspace", arrange: `UPDATE workspaces SET closed = 1, closure_at = 1, closure_by = 'user'`},
		{name: "a merge workspace whose hand-off a crash interrupted",
			arrange: `INSERT INTO async_merge_requests (request_id, repo_key, requester_id, seq, requested_at, branch, name, maker, target_maker, compact_after, cleanup_worktree, front, pre_deploy_hooks, post_deploy_hooks)
				VALUES ('req-1', '/tmp/repo-1', 'ws-layout3', 1, 1, 'feature', 'sample', 'ws-layout3', 0, 0, 0, 0, '[]', '[]')`},
		{name: "a workspace that is no merge workspace", arrange: `UPDATE workspaces SET merge_of = NULL, merge_request_id = NULL`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a layout-128 file whose one workspace is a merge
			// workspace.
			path := fixtureAt(t, 128)
			execRaw(t, path, `UPDATE workspaces SET merge_of = id, merge_request_id = 'req-1'`)
			if tt.arrange != "" {
				execRaw(t, path, tt.arrange)
			}

			// Act
			handle, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			defer handle.Close()
			adopted := rawScalar[int](t, path, `SELECT count(*) FROM off_queue_merges WHERE workspace_id = ? AND adopted = 1 AND cause = 'adopted' AND source_branch = (SELECT branch FROM workspaces WHERE id = ?)`,
				string(fixtureWorkspaceID), string(fixtureWorkspaceID))

			// Assert
			if (adopted == 1) != tt.wantAdopt {
				t.Fatalf("adopted %d records, want adopted %v", adopted, tt.wantAdopt)
			}
		})
	}
}

func TestRequeueOffQueueMergeRecordsTheSessionsModelInTheSameWrite(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "h", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	offQueued(t, s, repo, ws.ID)

	// Act
	_, err := s.RequeueOffQueueMerge(context.Background(), repo, ws.ID, RequeuePlacement{}, nil, "claude-opus-5-5", instant)

	// Assert
	if err != nil {
		t.Fatalf("RequeueOffQueueMerge: %v", err)
	}
	if got := scalar[string](t, s, `SELECT model FROM sessions WHERE workspace_id = ?`, ws.ID); got != "claude-opus-5-5" {
		t.Fatalf("the session's model = %q, want the requeue's", got)
	}
}

func TestRequeueOffQueueMergeWithAModelAndNoSessionChangesNothing(t *testing.T) {
	// Arrange: a workspace with no session row.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	offQueued(t, s, repo, ws.ID)

	// Act
	_, err := s.RequeueOffQueueMerge(context.Background(), repo, ws.ID, RequeuePlacement{}, nil, "claude-opus-5-5", instant)

	// Assert: refused, the request still off its queue and none in line.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RequeueOffQueueMerge = %v, want the missing session refused", err)
	}
	if _, found, _ := s.OffQueueMergeOf(context.Background(), ws.ID); !found {
		t.Fatal("the refused requeue lost the request")
	}
	if entries, _ := s.MergeQueue(context.Background(), repo); len(entries) != 0 {
		t.Fatalf("the refused requeue left %d entries in line", len(entries))
	}
}
