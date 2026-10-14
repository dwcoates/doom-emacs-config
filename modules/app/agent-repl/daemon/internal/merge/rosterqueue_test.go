package merge

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestTheOrchestratorWritesThroughTheRosterQueueDB(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)

	// Assert: every queue write the orchestrator makes republishes the
	// roster's queue, because there is no unwrapped DB for it to write through.
	if _, ok := h.o.deps.DB.(rosterQueueDB); !ok {
		t.Fatalf("the orchestrator's DB is %T, want rosterQueueDB", h.o.deps.DB)
	}
}

func TestAsyncMergesOnTheQueue(t *testing.T) {
	tests := []struct {
		name string
		act  func(t *testing.T, h *harness)
		want []ids.WorkspaceID
	}{
		{
			name: "a recorded async request names its requester before its hand-off",
			act: func(t *testing.T, h *harness) {
				h.inFlight = "turn-1"
				if err := requestDetached(t, h, RequestedByAgent); err != nil {
					t.Fatalf("Enqueue: %v", err)
				}
			},
			want: []ids.WorkspaceID{theWorkspace},
		},
		{
			name: "a handed-off async merge names the merge workspace's requester",
			act: func(t *testing.T, h *harness) {
				if err := requestDetached(t, h, RequestedByUser); err != nil {
					t.Fatalf("Enqueue: %v", err)
				}
			},
			want: []ids.WorkspaceID{theWorkspace},
		},
		{
			name: "a landed async merge names nobody",
			act: func(t *testing.T, h *harness) {
				h.emacsRepo()
				h.landsCleanly("abc123def4567")
				h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
				h.gatePasses("daemon")
				if err := requestDetached(t, h, RequestedByUser); err != nil {
					t.Fatalf("Enqueue: %v", err)
				}
				if err := h.admit(context.Background()); err != nil {
					t.Fatalf("the merge failed: %v", err)
				}
			},
			want: nil,
		},
		{
			name: "an evicted async merge names nobody",
			act: func(t *testing.T, h *harness) {
				if err := requestDetached(t, h, RequestedByUser); err != nil {
					t.Fatalf("Enqueue: %v", err)
				}
				if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
					t.Fatalf("Evict: %v", err)
				}
			},
			want: nil,
		},
		{
			name: "an async request whose fork failed names nobody",
			act: func(t *testing.T, h *harness) {
				h.detached.forkErr = errors.New("the worktree could not be made")
				if err := requestDetached(t, h, RequestedByUser); err != nil {
					t.Fatalf("Enqueue: %v", err)
				}
			},
			want: nil,
		},
		{
			name: "a sync merge names nobody",
			act: func(t *testing.T, h *harness) {
				if err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, By: RequestedByUser}); err != nil {
					t.Fatalf("Enqueue: %v", err)
				}
			},
			want: nil,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			tc.act(t, h)

			// Assert
			if got := h.sidebar.queueOf(h.repoKey()).AsyncRequesters; !slices.Equal(got, tc.want) {
				t.Fatalf("async merges = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAnUnreadableQueueTellsTheRosterNothingAndIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.queueReadErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues, want none", told)
	}
	rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.roster_queue")
	if !ok {
		t.Fatal("the unreadable queue was not recorded at ERROR")
	}
	if rec.Context["repo"] != string(h.repoKey()) || rec.Context["error"] != "database is locked" {
		t.Fatalf("record context = %v, want the repo and the cause", rec.Context)
	}
}

func TestAQueueReadCutByItsContextTellsTheRosterNothingAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.queueReadErr = context.Canceled
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues, want none", told)
	}
	if rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.roster_queue"); ok {
		t.Fatalf("the cut read was recorded at ERROR: %+v", rec)
	}
	rec, ok := h.recordFor(dlog.LevelInfo, "daemon.merge.roster_queue")
	if !ok || rec.Context["repo"] != string(h.repoKey()) || rec.Context["error"] != context.Canceled.Error() {
		t.Fatalf("record %+v (found %v), want the cut at INFO with the repo and the cause", rec, ok)
	}
}

func TestAPauseReadCutByItsContextTellsTheRosterNothingAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.pausedReadErr = context.Canceled
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues, want none", told)
	}
	if rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.roster_queue"); ok {
		t.Fatalf("the cut read was recorded at ERROR: %+v", rec)
	}
	rec, ok := h.recordFor(dlog.LevelInfo, "daemon.merge.roster_queue")
	if !ok || rec.Context["repo"] != string(h.repoKey()) || rec.Context["error"] != context.Canceled.Error() {
		t.Fatalf("record %+v (found %v), want the cut at INFO with the repo and the cause", rec, ok)
	}
}

func TestAnUnreadableQueuedWorkspaceTellsTheRosterNothingAndIsAnError(t *testing.T) {
	// Arrange: a queue row naming a workspace with no record.
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{{Repo: h.repoKey(), Workspace: "ws-gone", State: wsm.MergeQueued,
		Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}}}
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues, want none", told)
	}
	rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.roster_queue")
	if !ok {
		t.Fatal("the unreadable workspace was not recorded at ERROR")
	}
	if rec.Context["repo"] != string(h.repoKey()) || rec.Context["entries"] != 1 || rec.Context["error"] == nil {
		t.Fatalf("record context = %v, want the repo, the entry count and the cause", rec.Context)
	}
}

func TestAFailedQueueWriteTellsTheRosterNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: removing a row the queue does not hold fails.
	err := h.o.deps.DB.RemoveMergeQueueEntry(context.Background(), h.repoKey(), theWorkspace, "test")

	// Assert
	if err == nil {
		t.Fatal("RemoveMergeQueueEntry = nil, want the fake's refusal")
	}
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues after a failed write, want none", told)
	}
}

func TestASyncMergeIsOnItsRosterQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	enqueue(t, h)

	// Assert
	if got := h.sidebar.queueOf(h.repoKey()).Workspaces; !slices.Equal(got, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("roster queue workspaces = %v, want the merging workspace", got)
	}
}

func TestPausingTheQueueTellsTheRosterItIsPaused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)

	// Act
	if err := h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("Pause: %v", err)
	}

	// Assert
	got := h.sidebar.queueOf(h.repoKey())
	if !got.Paused || !slices.Equal(got.Workspaces, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("roster queue = %+v, want paused with the merging workspace on it", got)
	}
}

func TestResumingTheQueueTellsTheRosterItIsNoLongerPaused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	if err := h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("Pause: %v", err)
	}

	// Act
	if err := h.o.Unpause(context.Background(), nil); err != nil {
		t.Fatalf("Unpause: %v", err)
	}

	// Assert
	if got := h.sidebar.queueOf(h.repoKey()); got.Paused {
		t.Fatalf("roster queue = %+v, want it no longer paused", got)
	}
}

func TestPausingOneRepositoryTellsTheRosterOnlyThatQueueIsPaused(t *testing.T) {
	// Arrange: two repositories, each with a merge on its queue.
	h := newHarness(t)
	enqueue(t, h)
	other := wsm.RepoKey("/other/.git")
	h.db.mu.Lock()
	h.db.queues[other] = []wsm.MergeQueueEntry{{Repo: other, Workspace: theMergeWorkspace, State: wsm.MergeQueued,
		Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}}}
	h.db.mu.Unlock()
	h.registerRepo("target", h.targetD)
	h.registerRepo("other", "/other")
	h.o.publishRosterQueue(context.Background(), other)

	// Act
	if err := h.o.Pause(context.Background(), &RepositoryScope{ID: "target"}); err != nil {
		t.Fatalf("Pause: %v", err)
	}

	// Assert
	if !h.sidebar.queueOf(h.repoKey()).Paused {
		t.Fatal("the paused repository's roster queue is not paused")
	}
	if h.sidebar.queueOf(other).Paused {
		t.Fatal("the other repository's roster queue is paused")
	}
}

func TestAnUnreadablePauseTellsTheRosterNothingAndIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.pausedReadErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues, want none", told)
	}
	rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.roster_queue")
	if !ok {
		t.Fatal("the unreadable pause was not recorded at ERROR")
	}
	if rec.Context["repo"] != string(h.repoKey()) || rec.Context["error"] != "database is locked" {
		t.Fatalf("record context = %v, want the repo and the cause", rec.Context)
	}
}

func TestAFailedPauseWriteTellsTheRosterNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.pauseWriteErr = errors.New("database is read-only")
	h.db.mu.Unlock()

	// Act
	err := h.o.deps.DB.SetMergeQueuePaused(context.Background(), h.repoKey(), true)

	// Assert
	if err == nil {
		t.Fatal("SetMergeQueuePaused = nil, want the fake's refusal")
	}
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues after a failed pause write, want none", told)
	}
}
