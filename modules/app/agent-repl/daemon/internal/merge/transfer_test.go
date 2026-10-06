package merge

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/wsm"
)

// heldInGateForTransfer runs the harness's merge into its test gate and
// answers the gate's release and the pump's end.
func heldInGateForTransfer(t *testing.T, h *harness) (chan struct{}, <-chan error) {
	t.Helper()
	h.emacsRepo()
	h.landsCleanly(mergeSHA)
	inGate, release := make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate
	return release, done
}

func TestATransferSuspendsTheRunningMergeAndKeepsWhatItsResumeNeeds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() { close(release) })

	// Act.
	err := h.o.SuspendForTransfer(context.Background(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("SuspendForTransfer = %v, want the merge suspended", err)
	}
	if err := <-done; err != nil {
		t.Fatalf("the pump was told %v, want no failure", err)
	}
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); !held {
		t.Fatal("the transfer released the merge's lease, want it left for the adopting daemon")
	}
	if step := recordedStep(t, h); step != TabTests {
		t.Fatalf("the record stands at %q, want %q", step, TabTests)
	}
	if record, found := recordWith(h, "info", "daemon.merge.transfer"); !found || record.Context["step"] != TabTests {
		t.Fatalf("the transfer's record is %+v (found %v), want the suspension naming its step", record, found)
	}
}

func TestATransferWaitsForTheMergesGitInFlight(t *testing.T) {
	// Arrange: a landing held inside its fast-forward.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly(mergeSHA)
	h.gatePasses("daemon")
	entered, release := h.git.hold("fast_forward")
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-entered
	h.o.onGitStopWait = func() { close(release) }

	// Act.
	err := h.o.SuspendForTransfer(context.Background(), theWorkspace)
	<-done

	// Assert.
	if err != nil || len(h.git.fastForwards) != 1 {
		t.Fatalf("SuspendForTransfer = %v with fast-forwards %v, want the one in flight finished", err, h.git.fastForwards)
	}
}

func TestAWorkspaceHandedAwayHasNoMergeAdmittedHere(t *testing.T) {
	// Arrange: a queued merge whose workspace moved.
	h := newHarness(t)
	enqueue(t, h)
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}

	// Act.
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil || ran {
		t.Fatalf("pumpOnce = (%v, %v), want nothing admitted for a workspace another daemon serves", ran, err)
	}
}

func TestAMergeAdmittedElsewhereHoldsItsRepositorysSlot(t *testing.T) {
	// Arrange: one merge admitted by another daemon, one queued behind it.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	enqueue(t, h)
	enqueueWorkspace(t, h, "ws-2")
	if err := h.db.AdmitMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act.
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil || ran {
		t.Fatalf("pumpOnce = (%v, %v), want nothing admitted behind a merge in flight elsewhere", ran, err)
	}
}

func TestATransferWithdrawsARequestWaitingForItsTurnLeavingItRecorded(t *testing.T) {
	// Arrange: a request waiting for its turn to end.
	h := newHarness(t)
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	h.o.awaitRequestingTurn(theWorkspace, h.repoKey())

	// Act.
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}

	// Assert.
	h.o.mu.Lock()
	_, waiting := h.o.requested[theWorkspace]
	h.o.mu.Unlock()
	if waiting {
		t.Fatal("the request's wait stands here after its workspace moved")
	}
	if state, _ := h.entryState(); state != wsm.MergeRequested {
		t.Fatalf("entry state = %v, want the request still recorded for the adopting daemon", state)
	}
}

func TestTheAdoptingDaemonResumesASuspendedMergeInTheSameBubble(t *testing.T) {
	// Arrange: a merge suspended for its transfer, and the successor.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() { close(release) })
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}
	<-done
	lease, _, _ := h.db.Lease(context.Background(), theWorkspace)
	h.runner.mu.Lock()
	h.runner.before = nil
	h.runner.mu.Unlock()
	h.gatePasses("daemon")
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true
	h.restart(t)

	// Act.
	if err := h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the resumed merge ended on: %v", err)
	}

	// Assert.
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed", facts.State, facts.Detail)
	}
	for _, id := range h.feed.headIDs() {
		if id != headIDOf(lease.ID) {
			t.Fatalf("a head was drawn as %s, want every head the transferred merge's own", id)
		}
	}
}

func TestATakenBackTransferResumesItsMergeHere(t *testing.T) {
	// Arrange: a merge suspended for a transfer that did not land.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() { close(release) })
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}
	<-done
	h.runner.mu.Lock()
	h.runner.before = nil
	h.runner.mu.Unlock()
	h.gatePasses("daemon")
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true

	// Act.
	if err := h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the resumed merge ended on: %v", err)
	}

	// Assert.
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed here", facts.State, facts.Detail)
	}
}

// queuedLedger reads the ledger identity the harness workspace's queued merge
// carries on its row.
func queuedLedger(t *testing.T, h *harness) wsm.LeaseID {
	t.Helper()
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	for _, entry := range entries {
		if entry.Workspace == theWorkspace {
			if entry.Ledger == "" {
				t.Fatal("the queued merge's row carries no ledger identity")
			}
			return entry.Ledger
		}
	}
	t.Fatal("the harness workspace has no queue entry")
	return ""
}

// headsSince answers the bubble heads pushed after the first n pushes.
func headsSince(h *harness, n int) []string {
	return h.feed.headIDs()[n:]
}

func TestARestartRedrawsAQueuedMergeInTheBubbleItWaitedIn(t *testing.T) {
	// Arrange: a merge waiting in a paused queue.
	h := newHarness(t)
	h.db.paused[h.repoKey()] = true
	enqueue(t, h)
	ledger := queuedLedger(t, h)
	before := len(h.feed.headIDs())
	h.restart(t)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	heads := headsSince(h, before)
	if len(heads) == 0 {
		t.Fatal("the restart drew no bubble for the queued merge")
	}
	for _, id := range heads {
		if id != headIDOf(ledger) {
			t.Fatalf("the restart drew %s, want the bubble the merge waited in, %s", id, headIDOf(ledger))
		}
	}
}

func TestAHandoverRedrawsAQueuedMergeInTheBubbleItWaitedIn(t *testing.T) {
	// Arrange: a merge waiting in a paused queue, its workspace handed over.
	h := newHarness(t)
	h.db.paused[h.repoKey()] = true
	enqueue(t, h)
	ledger := queuedLedger(t, h)
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}
	before := len(h.feed.headIDs())
	h.restart(t)

	// Act.
	if err := h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}

	// Assert.
	heads := headsSince(h, before)
	if len(heads) == 0 || heads[len(heads)-1] != headIDOf(ledger) {
		t.Fatalf("the successor drew %v, want the bubble the merge waited in, %s", heads, headIDOf(ledger))
	}
}

func TestAQueuedMergeAnEarlierBuildRecordedIsGivenABubble(t *testing.T) {
	// Arrange: a queued row with no ledger identity, as an earlier build
	// wrote it.
	h := newHarness(t)
	h.db.paused[h.repoKey()] = true
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	h.db.mu.Lock()
	h.db.queues[h.repoKey()][0].State = wsm.MergeQueued
	h.db.mu.Unlock()

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	if _, has := h.o.leaseOf(theWorkspace); !has {
		t.Fatal("the earlier build's queued merge was given no bubble")
	}
	if _, found := recordWith(h, "info", "daemon.merge.recover"); !found {
		t.Fatal("the fresh bubble was not recorded")
	}
}

func TestAMergeThatCannotBePutInLineHoldsNoBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.queueErr = errors.New("disk full")

	// Act.
	err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, By: RequestedByUser})

	// Assert.
	if err == nil {
		t.Fatal("Enqueue succeeded with the queue write failing, want the failure")
	}
	if _, has := h.o.leaseOf(theWorkspace); has {
		t.Fatal("a merge that never took its place still holds a bubble identity")
	}
}

// TestASuccessorAdoptingBehindAMergeRunningInTheIncumbentAdmitsNothingQuietly
// covers the two-daemon window of a handover: the successor takes a workspace
// whose repository's merge still runs in the incumbent. Its admission stops
// at the durable "admitted elsewhere" fact, before the repository's kernel
// lock is ever tried, so nothing is admitted and nothing is warned.
func TestASuccessorAdoptingBehindAMergeRunningInTheIncumbentAdmitsNothingQuietly(t *testing.T) {
	// Arrange: the incumbent's merge held in its gate, a second workspace's
	// merge queued behind it, and the successor over the same state.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() {
		close(release)
		<-done
	})
	enqueueWorkspace(t, h, "ws-2")
	successor, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	if err := successor.AdoptWorkspace(context.Background(), "ws-2"); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}
	ran, err := successor.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil || ran {
		t.Fatalf("pumpOnce = (%v, %v), want nothing admitted behind the incumbent's merge", ran, err)
	}
	if record, found := recordWith(h, "debug", "daemon.merge.admit"); !found || !strings.Contains(record.Message, "admitted elsewhere") {
		t.Fatalf("the admission's record is %+v (found %v), want it stopped at the merge admitted elsewhere", record, found)
	}
	for _, level := range []string{"warn", "error"} {
		if records := recordsAtLevel(h, level); len(records) > 0 {
			t.Fatalf("the successor wrote %s records %v, want none", level, records)
		}
	}
}
