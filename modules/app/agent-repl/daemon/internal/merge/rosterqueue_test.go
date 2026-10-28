package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
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
	if err := h.o.Pause(context.Background(), nil, userPauser); err != nil {
		t.Fatalf("Pause: %v", err)
	}

	// Assert
	got := h.sidebar.queueOf(h.repoKey())
	if got.Pause == nil || !slices.Equal(got.Workspaces, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("roster queue = %+v, want paused with the merging workspace on it", got)
	}
}

func TestResumingTheQueueTellsTheRosterItIsNoLongerPaused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	if err := h.o.Pause(context.Background(), nil, userPauser); err != nil {
		t.Fatalf("Pause: %v", err)
	}

	// Act
	if err := h.o.Unpause(context.Background(), nil); err != nil {
		t.Fatalf("Unpause: %v", err)
	}

	// Assert
	if got := h.sidebar.queueOf(h.repoKey()); got.Pause != nil {
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
	if err := h.o.Pause(context.Background(), &RepositoryScope{ID: "target"}, userPauser); err != nil {
		t.Fatalf("Pause: %v", err)
	}

	// Assert
	if h.sidebar.queueOf(h.repoKey()).Pause == nil {
		t.Fatal("the paused repository's roster queue is not paused")
	}
	if h.sidebar.queueOf(other).Pause != nil {
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
	err := h.o.deps.DB.SetMergeQueuePaused(context.Background(), h.repoKey(), &wsm.MergeQueuePause{At: time.Now(), By: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserUser}})

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
	if want := []ids.WorkspaceID{"ws-run", "ws-wait"}; !slices.Equal(got.LineWorkspaces(), want) {
		t.Fatalf("roster line = %v, want %v", got.LineWorkspaces(), want)
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
			if got := h.sidebar.queueOf(h.repoKey()).LineWorkspaces(); !slices.Equal(got, tc.want) {
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

func TestTheRosterLineCreditsEachMergeAsTheRosterDoes(t *testing.T) {
	tests := []struct {
		name         string
		entry        wsm.MergeQueueEntry
		wantCredited ids.WorkspaceID
		wantAsync    bool
	}{
		{
			name:         "a workspace landing its own branch is credited to itself",
			entry:        wsm.MergeQueueEntry{Workspace: "ws-own", State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
			wantCredited: "ws-own",
		},
		{
			name:         "a merge workspace is credited to its requester",
			entry:        wsm.MergeQueueEntry{Workspace: "ws-merge", State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
			wantCredited: "ws-requester",
			wantAsync:    true,
		},
		{
			name:         "a detached request not yet handed off is credited to its own workspace",
			entry:        wsm.MergeQueueEntry{Workspace: "ws-own", State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Detach: "req-1"}},
			wantCredited: "ws-own",
			wantAsync:    true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.register("ws-own", "own")
			h.register("ws-requester", "requester")
			h.register("ws-merge", "land-requester")
			h.db.mu.Lock()
			merge := h.db.workspaces["ws-merge"]
			merge.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester"}
			h.db.workspaces["ws-merge"] = merge
			tc.entry.Repo = h.repoKey()
			h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{tc.entry}
			h.db.mu.Unlock()

			// Act
			h.o.publishRosterQueue(context.Background(), h.repoKey())

			// Assert
			line := h.sidebar.queueOf(h.repoKey()).Line
			if len(line) != 1 {
				t.Fatalf("roster line = %v, want one merge", line)
			}
			if line[0].Workspace != tc.entry.Workspace || line[0].Credited != tc.wantCredited || line[0].Async != tc.wantAsync {
				t.Fatalf("roster entry = %+v, want workspace %s credited to %s, async %v", line[0], tc.entry.Workspace, tc.wantCredited, tc.wantAsync)
			}
		})
	}
}

func TestTheRosterLineCarriesEachMergesPlaceInLine(t *testing.T) {
	// Arrange: the admitted front, and a waiting merge put at the front.
	h := newHarness(t)
	h.register("ws-a", "a")
	h.register("ws-b", "b")
	queuedA := time.UnixMilli(1_000)
	queuedB := time.UnixMilli(2_000)
	h.db.mu.Lock()
	h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{
		{Repo: h.repoKey(), Workspace: "ws-a", State: wsm.MergeAdmitted, EnqueuedAt: queuedA, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
		{Repo: h.repoKey(), Workspace: "ws-b", State: wsm.MergeQueued, EnqueuedAt: queuedB, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Front: true}},
	}
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	line := h.sidebar.queueOf(h.repoKey()).Line
	if len(line) != 2 {
		t.Fatalf("roster line = %v, want two merges", line)
	}
	if !line[0].Admitted || line[0].PlacedAtFront || !line[0].QueuedAt.Equal(queuedA) {
		t.Fatalf("front = %+v, want admitted, not placed at the front, queued at %v", line[0], queuedA)
	}
	if line[1].Admitted || !line[1].PlacedAtFront || !line[1].QueuedAt.Equal(queuedB) {
		t.Fatalf("waiting = %+v, want waiting, placed at the front, queued at %v", line[1], queuedB)
	}
}

func TestTheRosterLineRevealsTheBubbleOfEachMergeWithABubbleIdentity(t *testing.T) {
	tests := []struct {
		name   string
		ws     ids.WorkspaceID
		lease  ids.LeaseID
		wantAt ids.WorkspaceID
	}{
		{name: "a sync merge's bubble is on its own feed", ws: "ws-own", lease: "lease-own", wantAt: "ws-own"},
		{name: "an async merge's bubble is on its requester's feed", ws: "ws-merge", lease: "lease-merge", wantAt: "ws-requester"},
		{name: "a merge with no bubble identity reveals nothing", ws: "ws-own"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.register("ws-own", "own")
			h.register("ws-requester", "requester")
			h.register("ws-merge", "land-requester")
			h.db.mu.Lock()
			merge := h.db.workspaces["ws-merge"]
			merge.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester"}
			h.db.workspaces["ws-merge"] = merge
			h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{{Repo: h.repoKey(), Workspace: tc.ws, State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}}}
			h.db.mu.Unlock()
			if tc.lease != "" {
				h.o.mu.Lock()
				h.o.ledgerOf[tc.ws] = tc.lease
				h.o.mu.Unlock()
			}

			// Act
			h.o.publishRosterQueue(context.Background(), h.repoKey())

			// Assert
			got := h.sidebar.queueOf(h.repoKey()).Line[0].Reveal
			if tc.lease == "" {
				if got != nil {
					t.Fatalf("reveal = %v, want none", got)
				}
				return
			}
			if want := feedid.Encode(headRef(tc.wantAt, tc.lease)); got.GetValue() != want.GetValue() {
				t.Fatalf("reveal = %v, want the bubble head %v", got, want)
			}
		})
	}
}

func TestOnlyTheFrontOfTheRosterLineCarriesItsStages(t *testing.T) {
	// Arrange: both merges have a ledger with a live queue round.
	h := newHarness(t)
	h.register("ws-a", "a")
	h.register("ws-b", "b")
	started := time.UnixMilli(5_000)
	h.db.mu.Lock()
	h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{
		{Repo: h.repoKey(), Workspace: "ws-a", State: wsm.MergeAdmitted, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
		{Repo: h.repoKey(), Workspace: "ws-b", State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
	}
	h.db.ledger["ws-a"] = []wsm.MergeLedgerEntry{{Workspace: "ws-a", Lease: "lease-a", Intervals: []wsm.TabInterval{{Round: 1, Kind: TabQueue, StartedAt: started}}}}
	h.db.ledger["ws-b"] = []wsm.MergeLedgerEntry{{Workspace: "ws-b", Lease: "lease-b", Intervals: []wsm.TabInterval{{Round: 1, Kind: TabQueue, StartedAt: started}}}}
	h.db.mu.Unlock()
	h.o.mu.Lock()
	h.o.ledgerOf["ws-a"] = "lease-a"
	h.o.ledgerOf["ws-b"] = "lease-b"
	h.o.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	line := h.sidebar.queueOf(h.repoKey()).Line
	if len(line[0].Stages) != 1 || line[0].Stages[0].GetLabel() != "queue" || line[0].Stages[0].GetLive().GetStartedAtMs() != started.UnixMilli() {
		t.Fatalf("front stages = %v, want its live queue stage", line[0].Stages)
	}
	if len(line[1].Stages) != 0 {
		t.Fatalf("waiting stages = %v, want none", line[1].Stages)
	}
}

func TestAnUnreadableLedgerTellsTheRosterNothingAndIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.register("ws-a", "a")
	h.db.mu.Lock()
	h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{{Repo: h.repoKey(), Workspace: "ws-a", State: wsm.MergeAdmitted, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}}}
	h.db.ledgerReadErr = errors.New("database is locked")
	h.db.mu.Unlock()
	h.o.mu.Lock()
	h.o.ledgerOf["ws-a"] = "lease-a"
	h.o.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues, want none", told)
	}
	rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.roster_queue")
	if !ok {
		t.Fatal("the unreadable ledger was not recorded at ERROR")
	}
	if rec.Context["repo"] != string(h.repoKey()) || rec.Context["error"] == nil {
		t.Fatalf("record context = %v, want the repo and the cause", rec.Context)
	}
}

func TestAFailedMergeLeavesNothingStandingOnTheRosterQueue(t *testing.T) {
	// Arrange: the merge's conflict resolution gives up.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	h.git.conflicted = everyAttemptLeaves("a.go")
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}

	// Assert: the line is empty, so the Merge Queue section lists nothing for
	// the repository (owner request, 2026-10-27).
	if queue := h.sidebar.queueOf(h.repoKey()); len(queue.Line) != 0 || len(queue.Workspaces) != 0 {
		t.Fatalf("roster queue = %+v, want the failed merge gone from it", queue)
	}
}

func TestAPauseTellsTheRosterItsRecord(t *testing.T) {
	tests := []struct {
		name   string
		act    func(h *harness) error
		wantBy wsm.MergeQueuePauser
		hard   bool
	}{
		{
			name:   "the user's pause",
			act:    func(h *harness) error { return h.o.Pause(context.Background(), nil, userPauser) },
			wantBy: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserUser},
		},
		{
			name: "a workspace's pause names the workspace",
			act: func(h *harness) error {
				return h.o.Pause(context.Background(), nil, Pauser{By: RequestedByAgent, Asker: theWorkspace})
			},
			wantBy: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserWorkspace, Name: "ws-one"},
		},
		{
			name:   "a hard stop",
			act:    func(h *harness) error { return h.o.HardStop(context.Background(), nil, userPauser) },
			wantBy: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserUser},
			hard:   true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			enqueue(t, h)

			// Act
			if err := tt.act(h); err != nil {
				t.Fatalf("pause: %v", err)
			}

			// Assert
			pause := h.sidebar.queueOf(h.repoKey()).Pause
			if pause == nil || pause.By != tt.wantBy || pause.Hard != tt.hard || pause.At.IsZero() {
				t.Fatalf("roster pause = %+v, want by %+v, hard %v, at an instant", pause, tt.wantBy, tt.hard)
			}
		})
	}
}

func TestAHardStopOfAPausedQueueRecordsTheHardStop(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	if err := h.o.Pause(context.Background(), nil, userPauser); err != nil {
		t.Fatalf("Pause: %v", err)
	}

	// Act
	if err := h.o.HardStop(context.Background(), nil, Pauser{By: RequestedByAgent, Asker: theWorkspace}); err != nil {
		t.Fatalf("HardStop: %v", err)
	}

	// Assert
	pause := h.sidebar.queueOf(h.repoKey()).Pause
	if pause == nil || !pause.Hard || pause.By.Kind != wsm.MergeQueuePauserWorkspace {
		t.Fatalf("roster pause = %+v, want the workspace's hard stop", pause)
	}
}

func TestAPauseByAWorkspaceTheRegistryCannotReadIsRefusedAndPausesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)

	// Act
	err := h.o.Pause(context.Background(), nil, Pauser{By: RequestedByAgent, Asker: "ws-gone"})

	// Assert
	if err == nil {
		t.Fatal("Pause = nil, want the unreadable asker refused")
	}
	if paused, _ := h.db.MergeQueuePaused(context.Background(), h.repoKey()); paused {
		t.Fatal("the queue was paused")
	}
	rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.pause")
	if !ok || rec.Context["asker"] != "ws-gone" || rec.Context["error"] == nil {
		t.Fatalf("record = %+v (%v), want the asker and the cause at ERROR", rec, ok)
	}
}
