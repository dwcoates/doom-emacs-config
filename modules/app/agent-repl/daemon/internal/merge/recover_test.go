package merge

import (
	"context"
	"errors"
	"strings"
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

// TestRecoverPublishesAWaitingMergesFacts covers what a user sees after a
// bounce: their merge is still queued and still says where it sits.
func TestRecoverPublishesAWaitingMergesFacts(t *testing.T) {
	// Arrange: a queued merge and a fresh orchestrator over the same store.
	h := newHarness(t)
	enqueue(t, h)
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	facts, ok := fresh.Facts(theWorkspace)
	if !ok || facts.State != StateQueued || facts.QueuePosition != 1 {
		t.Fatalf("the recovered facts are %+v (present=%v), want queued at position 1", facts, ok)
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

// TestRecoverLoudlyFailsAnUncleanTarget covers the unresumable case: the daemon
// does not know what the dead run staged, and guessing would land a tree nobody
// reviewed.
func TestRecoverLoudlyFailsAnUncleanTarget(t *testing.T) {
	// Arrange: an interrupted merge over a dirty target.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.git.clean = false

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	facts, _ := h.o.Facts(theWorkspace)
	if facts.State != StateFailed {
		t.Fatalf("the merge is %q, want it failed loudly", facts.State)
	}
	if !strings.Contains(facts.Detail, "restart") {
		t.Fatalf("the failure reads %q, want it to name the restart", facts.Detail)
	}
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 0 {
		t.Fatalf("a loudly failed merge is still queued: %+v", entries)
	}
}

// TestRecoverLoudlyFailsWhenTheTargetCannotBeInspected covers the other
// unresumable case: "could not tell" is never read as resumable.
func TestRecoverLoudlyFailsWhenTheTargetCannotBeInspected(t *testing.T) {
	// Arrange: a target git cannot answer for.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.git.cleanErr = errors.New("the target is gone")

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateFailed {
		t.Fatalf("the merge is %q, want it failed loudly", facts.State)
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

// TestLastTabIsWhereAResumePicksUp covers what the ledger reconstructs: the tab
// a merge reached, which is all a replay needs and all it holds.
func TestLastTabIsWhereAResumePicksUp(t *testing.T) {
	// Arrange: a ledger whose last interval is the tests tab.
	h := newHarness(t)
	lease := wsm.NewLeaseID()
	if err := h.db.OpenMergeLedger(context.Background(), theWorkspace, lease); err != nil {
		t.Fatalf("opening the ledger: %v", err)
	}
	for _, kind := range []string{TabMerge, TabTests} {
		if err := h.db.RecordTabInterval(context.Background(), lease, wsm.TabInterval{Round: 1, Kind: kind}); err != nil {
			t.Fatalf("recording %s: %v", kind, err)
		}
	}

	// Act.
	got := h.o.lastTab(context.Background(), theWorkspace)

	// Assert.
	if got != TabTests {
		t.Fatalf("the last tab is %q, want %q", got, TabTests)
	}
}

// TestLastTabDefaultsToTheQueue covers a merge with no ledger yet: it never
// reached a phase, so the queue is where it resumes.
func TestLastTabDefaultsToTheQueue(t *testing.T) {
	// Arrange: a workspace with no ledger.
	h := newHarness(t)

	// Act.
	got := h.o.lastTab(context.Background(), theWorkspace)

	// Assert.
	if got != TabQueue {
		t.Fatalf("the last tab is %q, want %q", got, TabQueue)
	}
}

// enqueueWorkspace queues one named workspace's merge.
func enqueueWorkspace(t *testing.T, h *harness, ws ids.WorkspaceID) {
	t.Helper()
	if err := h.o.Enqueue(context.Background(), ws, RequestedByUser); err != nil {
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
