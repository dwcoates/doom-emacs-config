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
				if _, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, By: RequestedByUser}); err != nil {
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

func TestTheRosterQueueStatesTheLineFrontFirst(t *testing.T) {
	// Arrange: the running merge, a waiting one, and one only requested.
	h := newHarness(t)
	h.register("ws-run", "run")
	h.register("ws-wait", "wait")
	h.register("ws-asked", "asked")
	h.db.mu.Lock()
	h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{
		{Repo: h.repoKey(), Workspace: "ws-run", State: wsm.MergeAdmitted, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
		{Repo: h.repoKey(), Workspace: "ws-asked", State: wsm.MergeRequested, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
		{Repo: h.repoKey(), Workspace: "ws-wait", State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
	}
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	got := h.sidebar.queueOf(h.repoKey())
	if want := []ids.WorkspaceID{"ws-run", "ws-wait"}; !slices.Equal(got.Line, want) {
		t.Fatalf("roster line = %v, want %v", got.Line, want)
	}
	if want := []ids.WorkspaceID{"ws-run", "ws-asked", "ws-wait"}; !slices.Equal(got.Workspaces, want) {
		t.Fatalf("roster workspaces = %v, want every row %v", got.Workspaces, want)
	}
}

func TestEveryWriteThatChangesTheLineRepublishesTheRosterQueue(t *testing.T) {
	own := wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}
	tests := []struct {
		name  string
		queue []wsm.MergeQueueEntry
		lease bool
		write func(h *harness, db wsm.DB) error
		want  []ids.WorkspaceID
	}{
		{
			name:  "a requested merge put in line",
			queue: []wsm.MergeQueueEntry{{Workspace: "ws-a", State: wsm.MergeQueued, Source: own}, {Workspace: "ws-b", State: wsm.MergeRequested, Source: own}},
			write: func(h *harness, db wsm.DB) error {
				_, err := db.QueueMerge(context.Background(), h.repoKey(), "ws-b", "lease-b")
				return err
			},
			want: []ids.WorkspaceID{"ws-a", "ws-b"},
		},
		{
			name:  "a waiting merge moved up",
			queue: []wsm.MergeQueueEntry{{Workspace: "ws-a", State: wsm.MergeQueued, Source: own}, {Workspace: "ws-b", State: wsm.MergeQueued, Source: own}},
			write: func(h *harness, db wsm.DB) error {
				_, err := db.MoveMerge(context.Background(), h.repoKey(), "ws-b", wsm.MergeMoveUp)
				return err
			},
			want: []ids.WorkspaceID{"ws-b", "ws-a"},
		},
		{
			name:  "a waiting merge put at the front",
			queue: []wsm.MergeQueueEntry{{Workspace: "ws-a", State: wsm.MergeQueued, Source: own}, {Workspace: "ws-b", State: wsm.MergeQueued, Source: own}},
			write: func(h *harness, db wsm.DB) error {
				_, err := db.PrioritizeMerge(context.Background(), h.repoKey(), "ws-b")
				return err
			},
			want: []ids.WorkspaceID{"ws-b", "ws-a"},
		},
		{
			name:  "the head yielded to the race's winner",
			queue: []wsm.MergeQueueEntry{{Workspace: "ws-a", State: wsm.MergeAdmitted, Source: own}, {Workspace: "ws-b", State: wsm.MergeAdmitted, Source: own}},
			write: func(h *harness, db wsm.DB) error {
				return db.YieldMergeHead(context.Background(), h.repoKey(), "ws-a", "ws-b")
			},
			want: []ids.WorkspaceID{"ws-b", "ws-a"},
		},
		{
			name:  "an admitted merge dropped",
			queue: []wsm.MergeQueueEntry{{Workspace: "ws-a", State: wsm.MergeAdmitted, Source: own}, {Workspace: "ws-b", State: wsm.MergeQueued, Source: own}},
			lease: true,
			write: func(h *harness, db wsm.DB) error {
				return db.DropAdmittedMerge(context.Background(), h.repoKey(), "ws-a", "lease-a", "test")
			},
			want: []ids.WorkspaceID{"ws-b"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.register("ws-a", "a")
			h.register("ws-b", "b")
			h.db.mu.Lock()
			for i := range tc.queue {
				tc.queue[i].Repo = h.repoKey()
			}
			h.db.queues[h.repoKey()] = tc.queue
			if tc.lease {
				h.db.leases["ws-a"] = wsm.Lease{ID: "lease-a", Holder: wsm.HolderMerge}
			}
			h.db.mu.Unlock()

			// Act
			if err := tc.write(h, h.o.deps.DB); err != nil {
				t.Fatalf("write: %v", err)
			}

			// Assert
			if told := h.sidebar.toldCount(); told != 1 {
				t.Fatalf("the roster was told %d queues, want exactly one", told)
			}
			if got := h.sidebar.queueOf(h.repoKey()).Line; !slices.Equal(got, tc.want) {
				t.Fatalf("roster line = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAMoveThatMovedNothingTellsTheRosterNothing(t *testing.T) {
	tests := []struct {
		name  string
		write func(h *harness, db wsm.DB) (bool, error)
	}{
		{"a move up from the front", func(h *harness, db wsm.DB) (bool, error) {
			return db.MoveMerge(context.Background(), h.repoKey(), "ws-a", wsm.MergeMoveUp)
		}},
		{"a prioritize of the front", func(h *harness, db wsm.DB) (bool, error) {
			return db.PrioritizeMerge(context.Background(), h.repoKey(), "ws-a")
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.register("ws-a", "a")
			h.db.mu.Lock()
			h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{{Repo: h.repoKey(), Workspace: "ws-a", State: wsm.MergeQueued,
				Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}}}
			h.db.mu.Unlock()

			// Act
			moved, err := tc.write(h, h.o.deps.DB)

			// Assert
			if err != nil || moved {
				t.Fatalf("write = %v, %v; want nothing moved", moved, err)
			}
			if told := h.sidebar.toldCount(); told != 0 {
				t.Fatalf("the roster was told %d queues, want none", told)
			}
		})
	}
}

func TestAFailedLineWriteTellsTheRosterNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.moveErr = errors.New("database is read-only")
	h.db.mu.Unlock()

	// Act
	_, err := h.o.deps.DB.MoveMerge(context.Background(), h.repoKey(), theWorkspace, wsm.MergeMoveUp)

	// Assert
	if err == nil {
		t.Fatal("MoveMerge = nil, want the fake's refusal")
	}
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues after a failed write, want none", told)
	}
}
