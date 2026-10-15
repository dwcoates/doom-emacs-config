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

func TestTheOrchestratorWritesThroughTheAsyncMergesDB(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)

	// Assert: every queue write the orchestrator makes republishes the async
	// set, because there is no unwrapped DB for it to write through.
	if _, ok := h.o.deps.DB.(asyncMergesDB); !ok {
		t.Fatalf("the orchestrator's DB is %T, want asyncMergesDB", h.o.deps.DB)
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
			if got := h.sidebar.asyncOf(h.repoKey()); !slices.Equal(got, tc.want) {
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
	h.o.publishAsyncMerges(context.Background(), h.repoKey())

	// Assert
	if told := len(h.sidebar.asyncTold); told != 0 {
		t.Fatalf("the roster was told %d async sets, want none", told)
	}
	rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.async_merges")
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
	h.o.publishAsyncMerges(context.Background(), h.repoKey())

	// Assert
	if told := len(h.sidebar.asyncTold); told != 0 {
		t.Fatalf("the roster was told %d async sets, want none", told)
	}
	if rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.async_merges"); ok {
		t.Fatalf("the cut read was recorded at ERROR: %+v", rec)
	}
	rec, ok := h.recordFor(dlog.LevelInfo, "daemon.merge.async_merges")
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
	h.o.publishAsyncMerges(context.Background(), h.repoKey())

	// Assert
	if told := len(h.sidebar.asyncTold); told != 0 {
		t.Fatalf("the roster was told %d async sets, want none", told)
	}
	rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.async_merges")
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
	if told := len(h.sidebar.asyncTold); told != 0 {
		t.Fatalf("the roster was told %d async sets after a failed write, want none", told)
	}
}
