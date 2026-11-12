package wsm

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// asyncSourceNamed is an async request of branch "feature" whose merge
// workspace is named NAME.
func asyncSourceNamed(name string) MergeSource {
	return MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: NewDetachedMergeID(), DetachMaker: "maker", DetachName: name}
}

func TestRequestAsyncMergeKeepsEveryRequestOfOneTurnInTheOrderAsked(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	first, second := asyncSourceNamed("first"), asyncSourceNamed("second")

	// Act: two requests of one turn, and an in-place merge of the same
	// requester beside them.
	errFirst := s.RequestAsyncMerge(context.Background(), repo, requester.ID, "turn-1", first, time.Unix(100, 0))
	errSecond := s.RequestAsyncMerge(context.Background(), repo, requester.ID, "turn-1", second, time.Unix(101, 0))
	errOwn := s.RequestMerge(context.Background(), repo, requester.ID, MergeSource{Kind: MergeSourceOwnBranch}, time.Unix(102, 0))

	// Assert
	if errFirst != nil || errSecond != nil || errOwn != nil {
		t.Fatalf("requests = %v, %v, %v; want every one recorded", errFirst, errSecond, errOwn)
	}
	requests := asyncRequestsOf(t, s, requester.ID)
	if len(requests) != 2 || requests[0].Source.Detach != first.Detach || requests[1].Source.Detach != second.Detach {
		t.Fatalf("AsyncMergeRequestsOf = %+v; want first then second", requests)
	}
	if requests[0].Turn != "turn-1" || requests[1].Turn != "turn-1" {
		t.Fatalf("turns = %q, %q; want the turn that asked for each", requests[0].Turn, requests[1].Turn)
	}
	if queue, err := s.MergeQueue(context.Background(), repo); err != nil || len(queue) != 1 || queue[0].Source.Detach != "" {
		t.Fatalf("MergeQueue = %+v, %v; want the requester's in-place merge alone", queue, err)
	}
}

func TestRequestAsyncMergeRefusesASourceThatIsNoAsyncRequest(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "an in-place merge", source: MergeSource{Kind: MergeSourceOwnBranch}},
		{name: "a detached own branch", source: MergeSource{Kind: MergeSourceOwnBranch, Branch: "b", Detach: "d", DetachMaker: "maker", DetachName: "n"}},
		{name: "a detached request with no maker", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: "d", DetachName: "n"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			requester := testWorkspace(t, s)

			// Act
			err := s.RequestAsyncMerge(context.Background(), RepoKey(t.TempDir()), requester.ID, "", tt.source, time.Unix(100, 0))

			// Assert: an error, recorded at ERROR with the request, and nothing written.
			if err == nil {
				t.Fatal("RequestAsyncMerge accepted a source that is no async request")
			}
			rec, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.request_async_merge"})
			if !ok || rec.Context["workspace"] != string(requester.ID) || rec.Context["error"] == nil {
				t.Fatalf("records = %+v, want the refusal at ERROR with the workspace and the cause", logs.Records())
			}
			if requests := asyncRequestsOf(t, s, requester.ID); len(requests) != 0 {
				t.Fatalf("AsyncMergeRequestsOf = %+v, want nothing recorded", requests)
			}
		})
	}
}

func TestRequestMergeRefusesAnAsyncRequest(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())

	// Act
	err := s.RequestMerge(context.Background(), repo, requester.ID, asyncSourceNamed("n"), time.Unix(100, 0))

	// Assert: an error, recorded at ERROR, and no queue row.
	if err == nil {
		t.Fatal("RequestMerge recorded an async request on the requester's queue row")
	}
	if !loggedError(logs.Records(), "daemon.wsm.request_merge") {
		t.Fatalf("records = %+v, want the refusal at ERROR", logs.Records())
	}
	if queue, err := s.MergeQueue(context.Background(), repo); err != nil || len(queue) != 0 {
		t.Fatalf("MergeQueue = %+v, %v; want no row", queue, err)
	}
}

func TestARequestersRemovalTakesItsAsyncRequests(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester, other := testWorkspace(t, s), testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	asyncRequest(t, s, repo, requester.ID, asyncSourceNamed("gone"))
	asyncRequest(t, s, repo, other.ID, asyncSourceNamed("kept"))

	// Act
	corrupt(t, s, `DELETE FROM workspaces WHERE id = ?`, requester.ID)

	// Assert
	requests, err := s.AllAsyncMergeRequests(context.Background())
	if err != nil || len(requests) != 1 || requests[0].Requester != other.ID {
		t.Fatalf("AllAsyncMergeRequests = %+v, %v; want the other requester's alone", requests, err)
	}
}

func TestDropAsyncMergeRequestSettlesTheMergesPlacedBehindIt(t *testing.T) {
	// Arrange: a request, and an in-place merge placed behind it.
	s, _ := testStore(t)
	requester, later := testWorkspace(t, s), testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := asyncSourceNamed("ahead")
	asyncRequest(t, s, repo, requester.ID, request)
	if err := s.RequestMerge(context.Background(), repo, later.ID, MergeSource{Kind: MergeSourceOwnBranch, Behind: WorkspaceID(request.Detach), BehindName: "ahead"}, time.Unix(150, 0)); err != nil {
		t.Fatalf("RequestMerge(later): %v", err)
	}

	// Act
	err := s.DropAsyncMergeRequest(context.Background(), repo, request.Detach, "detached_fork_failed")

	// Assert
	if err != nil {
		t.Fatalf("DropAsyncMergeRequest: %v", err)
	}
	if requests := asyncRequestsOf(t, s, requester.ID); len(requests) != 0 {
		t.Fatalf("AsyncMergeRequestsOf = %+v, want the request gone", requests)
	}
	if entry := entryNamed(t, s, repo, later.ID); entry.WaitsOnLeft != "detached_fork_failed" || !entry.AheadFailed() {
		t.Fatalf("later entry = %+v, want its wait failed by the drop's cause", entry)
	}
}

func TestDropAsyncMergeRequestOfARequestNoLongerStandingIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.DropAsyncMergeRequest(context.Background(), RepoKey(t.TempDir()), NewDetachedMergeID(), "detached_fork_failed")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("DropAsyncMergeRequest = %v, want ErrNotFound", err)
	}
}

func TestAMergeLeavingSettlesTheAsyncRequestsPlacedBehindIt(t *testing.T) {
	// Arrange: a merge in line, and an async request placed behind it.
	s, _ := testStore(t)
	ahead, requester := testWorkspace(t, s), testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queued(t, s, repo, ahead.ID, time.Unix(50, 0))
	source := asyncSourceNamed("behind")
	source.Behind, source.BehindName = ahead.ID, "ahead"
	asyncRequest(t, s, repo, requester.ID, source)

	// Act
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, ahead.ID, MergeLeftLanded, nil); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Assert
	requests := asyncRequestsOf(t, s, requester.ID)
	if len(requests) != 1 || requests[0].WaitsOn != ahead.ID || requests[0].WaitsOnLeft != MergeLeftLanded {
		t.Fatalf("AsyncMergeRequestsOf = %+v; want the request's wait settled as landed", requests)
	}
}

func TestAHandOffPointsARequestPlacedBehindTheRequestAtTheMergeWorkspace(t *testing.T) {
	// Arrange: two requests of one turn, the second placed behind the first.
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	first := asyncSourceNamed("first")
	second := asyncSourceNamed("second")
	second.Behind, second.BehindName = WorkspaceID(first.Detach), "first"
	asyncRequest(t, s, repo, requester.ID, first)
	asyncRequest(t, s, repo, requester.ID, second)
	forked := registerMergeWorkspace(t, s, requester.ID, first.Detach)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, first.Detach, forked.ID, "first", "", time.Unix(200, 0))

	// Assert
	if err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	requests := asyncRequestsOf(t, s, requester.ID)
	if len(requests) != 1 || requests[0].Source.Behind != forked.ID || requests[0].WaitsOn != forked.ID || requests[0].Source.BehindName != "first" {
		t.Fatalf("AsyncMergeRequestsOf = %+v; want the second placed behind, and waiting on, %s", requests, forked.ID)
	}
}

func TestTheMigrationMovesAPendingAsyncRequestOntoItsOwnRow(t *testing.T) {
	// Arrange: a layout-127 file holding the fixture workspace's detached
	// request on its own queue row, and another workspace's in-place merge.
	path := fixtureAt(t, 127)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, place)
		VALUES ('ws-own', 'repo-1', '/tmp/ws-own', 'own', 'own', 'master', 0, 0, 0, 1, 60)`)
	execRaw(t, path, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, source_keep_open, source_branch, source_detach_id, source_detach_name, source_detach_maker, source_pick, source_front, source_pre_deploy_hooks, source_post_deploy_hooks)
		VALUES ('/tmp/repo-1', ?, 1, ?, 7, ?, 0, 'feature', 'req-1', 'feature-merge', ?, 'rebase it', 1, '[]', '[]')`,
		string(fixtureWorkspaceID), int(MergeRequested), int(MergeSourceBranch), string(fixtureWorkspaceID))
	execRaw(t, path, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, source_keep_open, source_branch, source_pre_deploy_hooks, source_post_deploy_hooks)
		VALUES ('/tmp/repo-1', 'ws-own', 2, ?, 8, ?, 0, 'own', '[]', '[]')`, int(MergeRequested), int(MergeSourceOwnBranch))

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("re-Open: %v", err)
	}
	defer handle.Close()
	requests, err := handle.AllAsyncMergeRequests(context.Background())
	queues, qerr := handle.AllMergeQueues(context.Background())

	// Assert: the request stands on its own with no turn, carrying what it
	// asked for, and the queue keeps the in-place merge alone.
	if err != nil || len(requests) != 1 {
		t.Fatalf("AllAsyncMergeRequests = %+v, %v; want the one request", requests, err)
	}
	got := requests[0]
	if got.Requester != fixtureWorkspaceID || got.Source.Detach != "req-1" || got.Source.DetachName != "feature-merge" || got.Turn != "" ||
		got.Source.Pick != "rebase it" || !got.Source.Front || got.Source.Branch != "feature" || !got.RequestedAt.Equal(fromNanos(7)) {
		t.Fatalf("request = %+v; want the moved request as recorded", got)
	}
	var rows []MergeQueueEntry
	for _, entries := range queues {
		rows = append(rows, entries...)
	}
	if qerr != nil || len(rows) != 1 || rows[0].Workspace != "ws-own" {
		t.Fatalf("AllMergeQueues = %+v, %v; want the in-place merge alone", queues, qerr)
	}
}
