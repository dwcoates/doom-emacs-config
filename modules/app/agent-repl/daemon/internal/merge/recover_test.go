package merge

import (
	"context"
	"errors"
	"os"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TestRecoverReEnqueuesWaitingMergesInOrder covers the durable queue's whole
// point: a bounce brings back exactly what was waiting, in the order it waited.
func TestRecoverReEnqueuesWaitingMergesInOrder(t *testing.T) {
	// Arrange: three merges queued before the restart.
	h := newHarness(t)
	order := []ids.WorkspaceID{"ws-1", "ws-2", "ws-3"}
	h.register("ws-2", "ws-two")
	h.register("ws-3", "ws-three")
	for _, ws := range order {
		enqueueWorkspace(t, h, ws)
	}

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	for i, entry := range entries {
		if entry.Workspace != order[i] {
			t.Fatalf("after recovery the queue is %+v, want %v", entries, order)
		}
	}
}

// TestRecoverResumesAnInFlightMergeOverACleanTarget covers the resumable case:
// nothing of the interrupted merge is half applied, so it can run again.
func TestRecoverResumesAnInFlightMergeOverACleanTarget(t *testing.T) {
	// Arrange: a merge that was admitted and whose lease survived the restart.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.git.clean = true

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || entries[0].State != wsm.MergeQueued {
		t.Fatalf("the recovered entry is %+v, want it waiting again", entries)
	}
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateQueued {
		t.Fatalf("the merge is %q, want it queued for another run", facts.State)
	}
}

// TestRecoverReleasesTheStuckLease covers the invariant a stuck lease would
// break: a lease nobody holds and nobody releases refuses that workspace's every
// prompt forever.
func TestRecoverReleasesTheStuckLease(t *testing.T) {
	// Arrange: a merge whose lease survived the restart.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); held {
		t.Fatal("a merge lease survived recovery")
	}
}

// TestRecoverRequeuesARecordlessMergeWhateverItsTargetHolds covers the
// owner's ruling: a merge's fate is never decided from what its target's
// working tree holds. An admitted merge with no progress record never took a
// step, so content in the target -- an untracked file, the owner's own edits
// -- is no evidence about it.
func TestRecoverRequeuesARecordlessMergeWhateverItsTargetHolds(t *testing.T) {
	// Arrange: an interrupted merge with no record, over a target whose
	// status would read unclean.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.git.clean = false

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateQueued {
		t.Fatalf("the merge is %q, want it queued for another run", facts.State)
	}
}

// TestRecoverNeverReadsTheTargetsStatus pins the same ruling at its source: the
// recovery asks git nothing about the target's working tree.
func TestRecoverNeverReadsTheTargetsStatus(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if slices.Contains(h.git.calls, "is_clean") {
		t.Fatalf("the recovery read the target's status: %v", h.git.calls)
	}
}

// TestRecoverLoudlyFailsWhenTheGeometryIsGone covers a workspace whose creation
// job no longer resolves: there is nothing left to resume into.
func TestRecoverLoudlyFailsWhenTheGeometryIsGone(t *testing.T) {
	// Arrange: an interrupted merge whose geometry was forgotten.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.db.mu.Lock()
	delete(h.db.jobs, theWorkspace)
	h.db.mu.Unlock()

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateFailed {
		t.Fatalf("the merge is %q, want it failed loudly", facts.State)
	}
}

// TestRecoverNeverLeavesAMergeInFlight covers the whole file's invariant: after
// recovery nothing is admitted-but-unowned.
func TestRecoverNeverLeavesAMergeInFlight(t *testing.T) {
	// Arrange: one resumable and one unresumable merge, in two repositories.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	enqueue(t, h)
	enqueueWorkspace(t, h, "ws-2")
	interruptMerge(t, h)
	h.git.clean = false

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	queues, _ := h.db.AllMergeQueues(context.Background())
	for repo, entries := range queues {
		for _, entry := range entries {
			if entry.State == wsm.MergeAdmitted {
				t.Fatalf("%s in %s is still admitted after recovery", entry.Workspace, repo)
			}
		}
	}
}

// enqueueWorkspace queues one named workspace's merge.
func enqueueWorkspace(t *testing.T, h *harness, ws ids.WorkspaceID) {
	t.Helper()
	if err := h.o.Enqueue(context.Background(), Request{Workspace: ws, By: RequestedByUser}); err != nil {
		t.Fatalf("enqueueing %s: %v", ws, err)
	}
}

// interruptMerge leaves the store looking the way a daemon that died mid-merge
// leaves it: the entry admitted and the lease still held.
func interruptMerge(t *testing.T, h *harness) {
	t.Helper()
	if err := h.db.AdmitMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("admitting: %v", err)
	}
	if _, err := h.db.AcquireLease(context.Background(), theWorkspace, wsm.HolderMerge, wsm.PolicyRefuse); err != nil {
		t.Fatalf("taking the lease: %v", err)
	}
}

// TestRecoverRefusesACorruptCreationJobRatherThanFailingTheMerge covers the
// corrupt-refuses-load ruling: a creation_jobs row that will not decode is
// state corruption, so the recovery FAILS THE BOOT instead of downgrading it
// into the ordinary "the geometry is gone" outcome and serving on.
func TestRecoverRefusesACorruptCreationJobRatherThanFailingTheMerge(t *testing.T) {
	// Arrange: an interrupted merge whose creation_jobs row will not decode.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.db.mu.Lock()
	h.db.jobDecodeErrs[theWorkspace] = &wsm.DecodeError{
		Table: "creation_jobs", Row: string(theWorkspace), Field: "actions_before",
		Err: errors.New("not valid json"),
	}
	h.db.mu.Unlock()

	// Act.
	err := h.o.Recover(context.Background())

	// Assert.
	if err == nil {
		t.Fatal("Recover over a corrupt creation_jobs row = nil, want a loud refusal")
	}
	var decodeErr *wsm.DecodeError
	if !errors.As(err, &decodeErr) {
		t.Fatalf("Recover error = %v, want it to carry the *wsm.DecodeError", err)
	}
}

// forgetJob removes a workspace's recorded merge geometry, which is what a nuke
// between the shutdown and the restart leaves behind: a durable queue entry for
// a workspace whose merge can no longer be described.
func forgetJob(h *harness, ws ids.WorkspaceID) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	delete(h.db.jobs, ws)
}

// TestRecoverAbandonsAWaitingMergeItCannotRequeueWithTheShutdownAsTheCause is
// the daemon-shutdown producer: the durable queue survives the shutdown, so the
// restart is where a merge that cannot go back gives up, and the cause names
// the shutdown rather than leaving the queue entry to fail at the front.
func TestRecoverAbandonsAWaitingMergeItCannotRequeueWithTheShutdownAsTheCause(t *testing.T) {
	// Arrange: a merge queued before the shutdown, whose geometry is gone.
	h := newHarness(t)
	enqueue(t, h)
	forgetJob(h, theWorkspace)
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	want := "the daemon shut down while this merge was waiting in the queue, and the restart could not put it back"
	if got := h.feed.lastAbandonedSummary(); got != want {
		t.Fatalf("the unrecoverable merge's summary = %q, want the shutdown's own cause", got)
	}
}

// TestRecoverTakesAnUnrequeueableMergeOffItsQueue covers the durable half: the
// entry is removed, so the queue behind it is not held by a merge that can
// never be admitted.
func TestRecoverTakesAnUnrequeueableMergeOffItsQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	forgetJob(h, theWorkspace)
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 0 {
		t.Fatalf("the queue after the recovery is %+v, want the unrequeueable merge gone", entries)
	}
}

// TestRecoverRefusesTheBootWhenAWaitingMergesJobRowWillNotDecode keeps the
// corruption arm distinct from the abandon: a half-written row is evidence,
// not a merge that legitimately lost its geometry.
func TestRecoverRefusesTheBootWhenAWaitingMergesJobRowWillNotDecode(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	h.db.mu.Lock()
	h.db.jobDecodeErrs[theWorkspace] = &wsm.DecodeError{Table: "creation_jobs", Row: string(theWorkspace), Field: "layout", Err: errors.New("bad json")}
	h.db.mu.Unlock()
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	recoverErr := fresh.Recover(context.Background())

	// Assert.
	var decodeErr *wsm.DecodeError
	if !errors.As(recoverErr, &decodeErr) {
		t.Fatalf("Recover = %v, want the boot refused with the decode error", recoverErr)
	}
	if got := h.feed.lastMergeErrorArm(); got != "" {
		t.Fatalf("the corrupt row drew the %q terminal, want the boot refused instead", got)
	}
}

// TestRecoverResubmitsATurnAMergeDisplacedAndNeverPutBack covers the boot
// recovery's whole purpose: a merge that died holding the user's turn does not
// keep it.
func TestRecoverResubmitsATurnAMergeDisplacedAndNeverPutBack(t *testing.T) {
	// Arrange: a turn left marked displaced by a merge that never released.
	h := newHarness(t)
	h.displaceTurn("carry on with the refactor")

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME); n != 1 {
		t.Fatalf("the boot resubmitted the displaced turn %d times, want exactly once", n)
	}
}

// TestRecoverResubmitsTheDisplacedTurnsOwnText covers what comes back: the
// words the user typed, not an empty prompt.
func TestRecoverResubmitsTheDisplacedTurnsOwnText(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	const text = "carry on with the refactor"
	h.displaceTurn(text)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	if len(h.queue.submissions) != 1 {
		t.Fatalf("the boot made %d submissions, want exactly the one resubmission", len(h.queue.submissions))
	}
	got := h.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if got != text {
		t.Fatalf("the resubmitted text = %q, want %q", got, text)
	}
}

// TestASecondBootDoesNotResubmitADisplacedTurnAgain covers the exactly-once
// edge the durable claim exists for: two boots, one resubmission.
func TestASecondBootDoesNotResubmitADisplacedTurnAgain(t *testing.T) {
	// Arrange: one displaced turn, already recovered by a first boot.
	h := newHarness(t)
	h.displaceTurn("carry on with the refactor")
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("the first Recover failed: %v", err)
	}

	// Act: a second boot over the same store.
	second, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}
	if err := second.Recover(context.Background()); err != nil {
		t.Fatalf("the second Recover failed: %v", err)
	}

	// Assert.
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME); n != 1 {
		t.Fatalf("two boots resubmitted the displaced turn %d times, want exactly once", n)
	}
}

// TestRecoverClaimsADisplacedTurnEvenWhenTheResubmissionFails covers the claim
// ordering: a submission that fails must not leave the mark standing for the
// next boot to fire again.
func TestRecoverClaimsADisplacedTurnEvenWhenTheResubmissionFails(t *testing.T) {
	// Arrange: a displaced turn and a queue that refuses.
	h := newHarness(t)
	h.displaceTurn("carry on with the refactor")
	h.queue.err = errors.New("the session is gone")

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if len(h.db.retired) != 1 {
		t.Fatalf("the displaced turns claimed = %v, want the one record claimed", h.db.retired)
	}
}

// TestRecoverWithNoDisplacedTurnsResubmitsNothing covers the ordinary boot,
// where no merge ever took a turn.
func TestRecoverWithNoDisplacedTurnsResubmitsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME); n != 0 {
		t.Fatalf("a boot with nothing displaced resubmitted %d turns, want none", n)
	}
}

// TestRecoverSweepsTheQueueTreesADeadRunLeft covers the queue's scratch trees
// across a crash: they are the queue's own, and the recovery removes them.
func TestRecoverSweepsTheQueueTreesADeadRunLeft(t *testing.T) {
	// Arrange: an interrupted merge whose run left two trees behind.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	lease := h.leaseID(t)
	var want []string
	for attempt := 1; attempt <= 2; attempt++ {
		dir := treeFor(h.stateDir, lease, attempt)
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("making a leftover tree: %v", err)
		}
		want = append(want, dir)
	}

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if !equal(h.git.removedWorktrees, want) {
		t.Fatalf("removed %v, want the dead run's trees %v", h.git.removedWorktrees, want)
	}
}

func TestRecoverReArmsARequestStillWaitingForItsTurn(t *testing.T) {
	// Arrange: a request recorded before the restart.
	h := newHarness(t)
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert: re-armed, not yet in line, reported nowhere.
	if state, _ := h.entryState(); state != wsm.MergeRequested {
		t.Fatalf("entry state = %v, want still requested", state)
	}
	if _, standing := h.o.Facts(theWorkspace); standing {
		t.Fatal("a re-armed request was reported")
	}
	h.runWait(t)
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("entry state after the wait = %v, want queued", state)
	}
}

func TestRecoverKeepsAResumedMergesSource(t *testing.T) {
	// Arrange: an admitted merge of another workspace's branch, interrupted.
	h := newHarness(t)
	h.registerOther(otherWorkspace, "ws-two", "other-branch")
	source := wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace}
	// The request is recorded with the branch checked out in the other worktree.
	recorded := source
	recorded.Branch = "other-branch"
	if err := h.request(t, source, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	interruptMerge(t, h)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || entries[0].Source != recorded || entries[0].State != wsm.MergeQueued {
		t.Fatalf("queue = %+v, want the merge back in line with its source", entries)
	}
}
