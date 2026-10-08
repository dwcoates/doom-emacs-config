package merge

import (
	"context"
	"testing"
)

// waitingForFree admits the harness's merge into its wait for the workspace to
// fall free and answers the pump's end.
func waitingForFree(t *testing.T, h *harness) <-chan error {
	t.Helper()
	h.freeness.busy = true
	h.freeness.waiting = make(chan struct{})
	h.freeness.release = make(chan struct{})
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-h.freeness.waiting
	return done
}

// TestARestartSuspendsAMergeWaitingForItsWorkspace pins that a merge only
// waiting never blocks a restart: its wait lets go, its lease and record
// stand, and nothing of it is drawn.
func TestARestartSuspendsAMergeWaitingForItsWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	done := waitingForFree(t, h)

	// Act.
	err := h.o.SuspendForRestart(context.Background(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("SuspendForRestart = %v, want the waiting merge suspended", err)
	}
	if err := <-done; err != nil {
		t.Fatalf("the pump was told %v, want no failure", err)
	}
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); !held {
		t.Fatal("the restart released the merge's lease, want it holding the workspace's prompts")
	}
	if step := recordedStep(t, h); step != TabQueue {
		t.Fatalf("the record stands at %q, want %q", step, TabQueue)
	}
	if heads := h.feed.headIDs(); len(heads) != 0 {
		t.Fatalf("the suspended waiting merge drew %v, want no bubble", heads)
	}
}

// TestAMergeWaitingThroughARestartWaitsAgainOnTheFreshSession covers the
// resume: the merge waits for freeness afresh, draws nothing while it does,
// and is placed and lands once the fresh session is free.
func TestAMergeWaitingThroughARestartWaitsAgainOnTheFreshSession(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	done := waitingForFree(t, h)
	if err := h.o.SuspendForRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForRestart: %v", err)
	}
	<-done
	h.freeness.waiting = make(chan struct{})
	release := make(chan struct{})
	h.freeness.release = release
	h.emacsRepo()
	h.landsCleanly(mergeSHA)
	h.gatePasses("daemon")
	if err := h.o.ResumeAfterRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("ResumeAfterRestart: %v", err)
	}

	// Act.
	resumed := admitAsync(h, context.Background())
	<-h.freeness.waiting
	headsWhileWaiting := len(h.feed.headIDs())
	h.freeness.busy = false
	close(release)
	err := <-resumed

	// Assert.
	if err != nil {
		t.Fatalf("the resumed merge ended on: %v", err)
	}
	if headsWhileWaiting != 0 {
		t.Fatalf("the resumed merge drew %d head(s) while it waited again, want none", headsWhileWaiting)
	}
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed", facts.State, facts.Detail)
	}
}

// TestARestartSuspendsAStartedMergeAtAStoppingPoint pins the started merge:
// the restart suspends it exactly as a handover does, under its own record.
func TestARestartSuspendsAStartedMergeAtAStoppingPoint(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() { close(release) })

	// Act.
	err := h.o.SuspendForRestart(context.Background(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("SuspendForRestart = %v, want the merge suspended", err)
	}
	if err := <-done; err != nil {
		t.Fatalf("the pump was told %v, want no failure", err)
	}
	if record, found := recordWith(h, "info", "daemon.merge.restart"); !found || record.Context["step"] != TabTests {
		t.Fatalf("the restart's record is %+v (found %v), want the suspension naming its step", record, found)
	}
}

// TestAStartedMergeResumesAfterARestartInItsBubble covers the started
// merge's resume: it goes on at its step, in the same bubble.
func TestAStartedMergeResumesAfterARestartInItsBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() { close(release) })
	if err := h.o.SuspendForRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForRestart: %v", err)
	}
	<-done
	lease, _, _ := h.db.Lease(context.Background(), theWorkspace)
	h.runner.mu.Lock()
	h.runner.before = nil
	h.runner.mu.Unlock()
	h.gatePasses("daemon")
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true

	// Act.
	if err := h.o.ResumeAfterRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("ResumeAfterRestart: %v", err)
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
			t.Fatalf("a head was drawn as %s, want every head the merge's own", id)
		}
	}
}

// TestNoMergeIsAdmittedWhileItsWorkspaceRestarts pins the exclusion: between
// the suspension and the resume nothing of the workspace is admitted.
func TestNoMergeIsAdmittedWhileItsWorkspaceRestarts(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	if err := h.o.SuspendForRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForRestart: %v", err)
	}
	_ = h.admit(context.Background())

	// Assert.
	if _, running := h.o.runFor(theWorkspace); running {
		t.Fatal("a merge was admitted while its workspace restarts")
	}
}
