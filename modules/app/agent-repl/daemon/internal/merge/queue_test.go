package merge

import (
	"claude-repld/internal/wsm"
	"context"
	"errors"
	"testing"
)

// TestEvictRefusesAWorkspaceWithNothingQueued covers the refusal an operator
// gets for a merge that is not there.
func TestEvictRefusesAWorkspaceWithNothingQueued(t *testing.T) {
	// Arrange: a workspace with no queued merge.
	h := newHarness(t)

	// Act.
	err := h.o.Evict(context.Background(), theWorkspace)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmNoSuchQueuedMerge {
		t.Fatalf("Evict answered %v, want the %s refusal", err, ArmNoSuchQueuedMerge)
	}
}

// TestAdmissionTakesTheRepositoryLock covers the exclusivity: while a merge
// runs, the repository's lock is held so no second daemon admits from the same
// queue.
func TestAdmissionTakesTheRepositoryLock(t *testing.T) {
	// Arrange: a merge held inside its gate, so the run holds its slot when
	// the assert happens.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	inGate, release := holdTheGate(h)
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	_, taken, err := acquireRepoLock(h.o.lockDir, string(h.repoKey()))

	// Assert.
	close(release)
	<-done
	if err != nil {
		t.Fatalf("probing the repository lock errored: %v", err)
	}
	if taken {
		t.Fatal("the repository's queue lock was free while a merge was running")
	}
}

// holdTheGate holds the harness's next gate run until release is closed, and
// signals inGate once it is held: the rendezvous a test needs to act while a
// merge is in its long phase.
func holdTheGate(h *harness) (inGate, release chan struct{}) {
	inGate, release = make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		h.runner.mu.Lock()
		h.runner.before = nil
		h.runner.mu.Unlock()
		close(inGate)
		<-release
	}
	return inGate, release
}

// TestOneMergesFailureDoesNotStopTheQueueBehindIt covers the pump's own
// invariant: a run that ended on its own terminal has already reported itself,
// so the next queued merge still gets admitted.
func TestOneMergesFailureDoesNotStopTheQueueBehindIt(t *testing.T) {
	// Arrange: a merge git refuses to make.
	h := newHarness(t)
	h.emacsRepo()
	h.git.mergeErr = errors.New("refusing to merge unrelated histories")
	enqueue(t, h)

	// Act.
	err := h.admit(context.Background())

	// Assert: the pump reports no queue-level failure, so its loop goes on.
	if err != nil {
		t.Fatalf("pumpOnce after a merge that ended on its own terminal = %v, want no queue-level error", err)
	}
}

// TestEveryAbandonCauseResolvesItsOwnSentence pins the cause vocabulary: the
// wire has ONE abandoned arm for all four ends, so a cause with no sentence of
// its own would be an end a reader cannot tell from any other.
func TestEveryAbandonCauseResolvesItsOwnSentence(t *testing.T) {
	// Arrange.
	tests := []struct {
		name  string
		cause AbandonCause
		want  string
	}{
		{"user drop", CauseUserDrop, "the operator evicted this merge from the queue"},
		{"user dequeue", CauseUserDequeue, "the user released this merge's queue slot"},
		{"workspace closed", CauseWorkspaceClosed, "the workspace was closed while this merge was waiting in the queue"},
		{"daemon shutdown", CauseDaemonShutdown, "the daemon shut down while this merge was waiting in the queue, and the restart could not put it back"},
	}
	seen := map[string]AbandonCause{}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, declared := tt.cause.summary()

			// Assert.
			if !declared || got != tt.want {
				t.Fatalf("%s.summary() = %q, %v, want %q declared", tt.cause, got, declared, tt.want)
			}
			if other, clash := seen[got]; clash {
				t.Fatalf("%s draws the same sentence as %s, so the two ends read alike", tt.cause, other)
			}
			seen[got] = tt.cause
		})
	}
}

// TestClosingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause is the
// workspace-close producer: a merge whose workspace is torn down can never
// run, and the bubble says so rather than simply stopping.
func TestClosingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert.
	if got := h.feed.lastAbandonedSummary(); got != "the workspace was closed while this merge was waiting in the queue" {
		t.Fatalf("the closed workspace's merge summary = %q, want the close's own cause", got)
	}
}

// TestClosingAWorkspaceTakesItsMergeOffTheQueue covers the other half of the
// same close: a merge left queued behind a workspace that is gone would hold
// the repository's queue against a workspace nobody can merge.
func TestClosingAWorkspaceTakesItsMergeOffTheQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 0 {
		t.Fatalf("the queue after the close is %+v, want it empty", entries)
	}
}

// TestClosingAWorkspaceWithNoQueuedMergeAbandonsNothing is the no-op edge: the
// teardown verbs call this for every workspace, and most have no merge at all.
func TestClosingAWorkspaceWithNoQueuedMergeAbandonsNothing(t *testing.T) {
	// Arrange: a registered workspace that never enqueued a merge.
	h := newHarness(t)

	// Act.
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert.
	if got := h.feed.lastMergeErrorArm(); got != "" {
		t.Fatalf("a close with no queued merge drew the %q terminal, want no terminal at all", got)
	}
}

// queueAnother puts one more workspace's merge in line behind the harness's.
func queueAnother(t *testing.T, h *harness, ws wsm.WorkspaceID, name string) {
	t.Helper()
	h.registerOther(ws, name, "b-"+name)
	if err := h.o.Enqueue(context.Background(), Request{Workspace: ws, Source: ownBranch, By: RequestedByUser}); err != nil {
		t.Fatalf("Enqueue %s: %v", ws, err)
	}
}

// factsOf answers the last facts a workspace was told.
func (h *harness) factsOf(ws wsm.WorkspaceID) MergeFacts {
	facts, _ := h.o.Facts(ws)
	return facts
}

func TestEnqueuedCountsOnlyTheMergesWaiting(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")

	// Act.
	queueAnother(t, h, "ws-3", "three")

	// Assert.
	two, three := h.factsOf("ws-2"), h.factsOf("ws-3")
	if two.QueuePlace != 2 || two.QueueWaiting != 3 || three.QueuePlace != 3 || three.QueueWaiting != 3 {
		t.Fatalf("places = %d/%d and %d/%d, want 2/3 and 3/3", two.QueuePlace, two.QueueWaiting, three.QueuePlace, three.QueueWaiting)
	}
}

func TestARequestIsCountedAmongNoOnesWaiting(t *testing.T) {
	// Arrange: a request whose turn still runs.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act.
	queueAnother(t, h, "ws-2", "two")

	// Assert.
	if got := h.factsOf("ws-2"); got.QueuePlace != 1 || got.QueueWaiting != 1 {
		t.Fatalf("facts = %+v, want 1/1: a request is in nobody's line", got)
	}
}

func TestAWaitingMergesLineNamesTheMergeAheadAndItsStep(t *testing.T) {
	// Arrange: the harness's merge runs and is held inside its gate.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	inGate, release := holdTheGate(h)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	line := h.factsOf("ws-2").Line.GetEnqueued()
	close(release)
	<-done

	// Assert.
	if line.GetWorkspaceName() != "ws-one" || line.GetStep() != "testing" {
		t.Fatalf("enqueued line = %+v, want ws-one testing", line)
	}
}

// TestLeaveAdmissionReleasesTheDrainWaitingOnTheLastStep covers the release a
// drain waits on: the step that brings the count to zero closes the drain's
// channel, and a step that does not leaves it open.
func TestLeaveAdmissionReleasesTheDrainWaitingOnTheLastStep(t *testing.T) {
	tests := []struct {
		name        string
		inFlight    int
		wantRelease bool
	}{
		{name: "the last step", inFlight: 1, wantRelease: true},
		{name: "a step with another still in flight", inFlight: 2, wantRelease: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: steps in flight and a drain waiting on them.
			h := newHarness(t)
			idle := make(chan struct{})
			h.o.mu.Lock()
			h.o.admissions = tc.inFlight
			h.o.admissionsIdle = idle
			h.o.mu.Unlock()

			// Act.
			h.o.leaveAdmission()

			// Assert.
			released := false
			select {
			case <-idle:
				released = true
			default:
			}
			if released != tc.wantRelease {
				t.Fatalf("the drain released = %v, want %v", released, tc.wantRelease)
			}
		})
	}
}

// TestLeaveAdmissionWithNothingRegisteredPanics covers the accounting
// invariant: a leave with no step registered fails hard rather than driving
// the count negative.
func TestLeaveAdmissionWithNothingRegisteredPanics(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("leaveAdmission with nothing registered did not panic")
		}
	}()
	h.o.leaveAdmission()
}
