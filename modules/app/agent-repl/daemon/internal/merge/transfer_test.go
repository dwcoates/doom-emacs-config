package merge

import (
	"context"
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
