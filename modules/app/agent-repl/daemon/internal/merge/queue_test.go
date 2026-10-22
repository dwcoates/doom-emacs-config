package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"slices"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
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

// TestMoveRefusesAWorkspaceWithNothingQueued covers the refusal a move gets
// for a merge that is not there.
func TestMoveRefusesAWorkspaceWithNothingQueued(t *testing.T) {
	// Arrange: a workspace with no queued merge.
	h := newHarness(t)

	// Act.
	moved, err := h.o.Move(context.Background(), theWorkspace, wsm.MergeMoveUp)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmNoSuchQueuedMerge || moved {
		t.Fatalf("Move answered %v, %v; want the %s refusal", moved, err, ArmNoSuchQueuedMerge)
	}
}

// TestMoveRedrawsTheQueueInItsNewOrder covers the publication: every merge on
// the queue is told its new place, after a move either way.
func TestMoveRedrawsTheQueueInItsNewOrder(t *testing.T) {
	tests := []struct {
		name       string
		ws         wsm.WorkspaceID
		move       wsm.MergeMove
		wantPlaces map[wsm.WorkspaceID]int
	}{
		{name: "up", ws: "ws-3", move: wsm.MergeMoveUp,
			wantPlaces: map[wsm.WorkspaceID]int{theWorkspace: 1, "ws-3": 2, "ws-2": 3}},
		{name: "down", ws: theWorkspace, move: wsm.MergeMoveDown,
			wantPlaces: map[wsm.WorkspaceID]int{"ws-2": 1, theWorkspace: 2, "ws-3": 3}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: three merges wait.
			h := newHarness(t)
			enqueue(t, h)
			queueAnother(t, h, "ws-2", "two")
			queueAnother(t, h, "ws-3", "three")

			// Act.
			moved, err := h.o.Move(context.Background(), tt.ws, tt.move)

			// Assert.
			if err != nil || !moved {
				t.Fatalf("Move = %v, %v; want moved", moved, err)
			}
			for ws, want := range tt.wantPlaces {
				if got := h.factsOf(ws).QueuePlace; got != want {
					t.Fatalf("%s's place = %d, want %d", ws, got, want)
				}
			}
		})
	}
}

// TestMoveLeavesAMergeWithNoNeighborWhereItIs covers the success answer for
// a merge with no waiting merge on the side it is asked to move to.
func TestMoveLeavesAMergeWithNoNeighborWhereItIs(t *testing.T) {
	// Arrange: two merges wait.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")

	// Act.
	moved, err := h.o.Move(context.Background(), theWorkspace, wsm.MergeMoveUp)

	// Assert.
	if err != nil || moved {
		t.Fatalf("Move = %v, %v; want unmoved", moved, err)
	}
	if got := h.factsOf(theWorkspace); got.QueuePlace != 1 {
		t.Fatalf("place = %d, want 1", got.QueuePlace)
	}
}

func TestAWaitingMergeCountsFromWhenItJoinedTheQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	queueAnother(t, h, "ws-2", "two")

	// Assert.
	for _, ws := range []wsm.WorkspaceID{theWorkspace, "ws-2"} {
		if got, want := h.factsOf(ws).QueuedAt.UnixMilli(), queuedAtMS(t, h, ws); got != want {
			t.Fatalf("%s's facts are queued at %d, want its entry's %d", ws, got, want)
		}
	}
}

// TestMoveRecordsAFailedMoveAndRedrawsNothing covers the store failing the
// move: the error reaches the caller, is recorded at ERROR with its context,
// and no bubble is told a new place.
func TestMoveRecordsAFailedMoveAndRedrawsNothing(t *testing.T) {
	// Arrange: two merges wait, and the store fails every move.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")
	h.db.moveErr = errors.New("database is locked")

	// Act.
	moved, err := h.o.Move(context.Background(), "ws-2", wsm.MergeMoveUp)

	// Assert.
	if err == nil || moved {
		t.Fatalf("Move = %v, %v; want the store's failure", moved, err)
	}
	record, logged := h.recordFor("error", "daemon.merge.move")
	if !logged || record.Context["error"] != "database is locked" || record.Context["workspace"] != "ws-2" || record.Context["move"] != "up" {
		t.Fatalf("the failed move was not recorded at ERROR with its context: %+v", h.logs.Records())
	}
	if got := h.factsOf("ws-2"); got.QueuePlace != 2 {
		t.Fatalf("place = %d, want 2", got.QueuePlace)
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
	if _, err := h.o.Enqueue(context.Background(), Request{Workspace: ws, Source: ownBranch, By: RequestedByUser}); err != nil {
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

// lineOf answers two in-line entries queued at the given instants: the front
// (the harness workspace) and one waiting behind it.
func lineOf(frontMS, behindMS int64) []wsm.MergeQueueEntry {
	return []wsm.MergeQueueEntry{
		{Workspace: theWorkspace, Position: 1, State: wsm.MergeQueued, EnqueuedAt: time.UnixMilli(frontMS)},
		{Workspace: "ws-2", Position: 2, State: wsm.MergeQueued, EnqueuedAt: time.UnixMilli(behindMS)},
	}
}

// registerRunning makes a run the front of the harness repository's queue, as
// admission does.
func registerRunning(h *harness, r *run) {
	h.o.mu.Lock()
	defer h.o.mu.Unlock()
	h.o.running[r.repo] = r
	h.o.runsByWorkspace[r.ws] = r
}

// frontStandingOf resolves the queue's standings and answers the front's.
func frontStandingOf(t *testing.T, h *harness, entries []wsm.MergeQueueEntry) *frontendv1.FeedMergeQueueMerging {
	t.Helper()
	standings, err := h.o.queueStandings(entries)
	if err != nil {
		t.Fatalf("queueStandings: %v", err)
	}
	merging := standings[0].GetMerging()
	if merging == nil {
		t.Fatalf("the front's standing is %T, want merging", standings[0].GetStatus())
	}
	return merging
}

// TestAWaitingEntryCarriesItsQueuedTime covers a waiting entry's duration: it
// ticks from when the entry was queued.
func TestAWaitingEntryCarriesItsQueuedTime(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	standings, err := h.o.queueStandings(lineOf(1000, 2000))

	// Assert.
	if err != nil {
		t.Fatalf("queueStandings: %v", err)
	}
	if got := standings[1].GetWaiting().GetStageEnteredAtMs(); got != 2000 {
		t.Fatalf("the waiting entry entered its stage at %d, want its queued time 2000", got)
	}
}

// TestAFrontWithNoRunIsInItsQueueStageSinceQueued covers a front nothing runs
// yet (a paused queue): its stage is the queue, begun when it was queued.
func TestAFrontWithNoRunIsInItsQueueStageSinceQueued(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	merging := frontStandingOf(t, h, lineOf(1000, 2000))

	// Assert.
	if merging.GetActiveTab().GetText() != "queue" || merging.GetStageEnteredAtMs() != 1000 {
		t.Fatalf("the front stands %v since %d, want the queue since 1000", merging.GetActiveTab(), merging.GetStageEnteredAtMs())
	}
}

// TestAFrontStillInItsQueueRoundCountsFromItsQueuedTime covers an admitted
// front whose first step has not begun: the queue stage it is still in began
// when it was queued, the same start its own queue tab carries.
func TestAFrontStillInItsQueueRoundCountsFromItsQueuedTime(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := ledgeredRun(t, h)
	r.openTab(context.Background(), TabQueue)
	registerRunning(h, r)

	// Act.
	merging := frontStandingOf(t, h, lineOf(1000, 2000))

	// Assert.
	if merging.GetActiveTab().GetText() != "queue" || merging.GetStageEnteredAtMs() != 1000 {
		t.Fatalf("the front stands %v since %d, want the queue since 1000", merging.GetActiveTab(), merging.GetStageEnteredAtMs())
	}
}

// TestARunningFrontsStageIsItsActiveRound covers the front's progress: its
// active tab, since that tab's round began.
func TestARunningFrontsStageIsItsActiveRound(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := ledgeredRun(t, h)
	rebasing := r.openTab(context.Background(), TabRebasing)
	registerRunning(h, r)

	// Act.
	merging := frontStandingOf(t, h, lineOf(1000, 2000))

	// Assert.
	if merging.GetActiveTab().GetText() != "rebasing" || merging.GetStageEnteredAtMs() != rebasing.started.UnixMilli() {
		t.Fatalf("the front stands %v since %d, want rebasing since %d", merging.GetActiveTab(), merging.GetStageEnteredAtMs(), rebasing.started.UnixMilli())
	}
}

// TestTheFrontsStageStartsOverWhenItsActiveTabChanges covers the duration
// column's reset: a new active tab is a new stage, entered when that tab began.
func TestTheFrontsStageStartsOverWhenItsActiveTabChanges(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := ledgeredRun(t, h)
	registerRunning(h, r)
	r.openTab(context.Background(), TabRebasing)
	before := frontStandingOf(t, h, lineOf(1000, 2000)).GetStageEnteredAtMs()

	// Act.
	tests := r.openTab(context.Background(), TabTests)
	after := frontStandingOf(t, h, lineOf(1000, 2000))

	// Assert.
	if after.GetActiveTab().GetText() != "tests" || after.GetStageEnteredAtMs() != tests.started.UnixMilli() || after.GetStageEnteredAtMs() == before {
		t.Fatalf("after the tests tab opened the front stands %v since %d (before: %d), want tests since %d",
			after.GetActiveTab(), after.GetStageEnteredAtMs(), before, tests.started.UnixMilli())
	}
}

// TestAQueueEntryWithNoQueuedTimeIsRefused covers the invariant: an entry
// whose queued time is missing is an error, never a stage entered at zero.
func TestAQueueEntryWithNoQueuedTimeIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		entries []wsm.MergeQueueEntry
	}{
		{name: "the front", entries: []wsm.MergeQueueEntry{{Workspace: theWorkspace, State: wsm.MergeQueued}}},
		{name: "a waiting entry", entries: []wsm.MergeQueueEntry{
			{Workspace: theWorkspace, State: wsm.MergeQueued, EnqueuedAt: time.UnixMilli(1000)},
			{Workspace: "ws-2", State: wsm.MergeQueued},
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.o.queueStandings(tc.entries)

			// Assert.
			if err == nil {
				t.Fatal("queueStandings answered no error for an entry with no queued time")
			}
		})
	}
}

// TestQueueTabStateSpansTheWait covers the queue tab's badge: live from the
// queued time while the merge waits, settled from it to the run's start once
// admitted, and not drawn while an admission has no run registered yet.
func TestQueueTabStateSpansTheWait(t *testing.T) {
	tests := []struct {
		name      string
		state     wsm.MergeQueueState
		running   bool
		wantDrawn bool
		wantState func(r *run) tabState
	}{
		{name: "waiting", state: wsm.MergeQueued, wantDrawn: true,
			wantState: func(*run) tabState { return tabState{startedMS: 1000} }},
		{name: "admitted with its run", state: wsm.MergeAdmitted, running: true, wantDrawn: true,
			wantState: func(r *run) tabState { return tabState{startedMS: 1000, settled: true, endedMS: r.startedMS} }},
		{name: "admitted before its run registers", state: wsm.MergeAdmitted, wantDrawn: false,
			wantState: func(*run) tabState { return tabState{} }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			r := ledgeredRun(t, h)
			if tc.running {
				registerRunning(h, r)
			}
			entry := wsm.MergeQueueEntry{Workspace: theWorkspace, State: tc.state, EnqueuedAt: time.UnixMilli(1000)}

			// Act.
			state, drawn, err := h.o.queueTabState(entry)

			// Assert.
			if err != nil {
				t.Fatalf("queueTabState: %v", err)
			}
			if drawn != tc.wantDrawn || state != tc.wantState(r) {
				t.Fatalf("queueTabState = %+v drawn %v, want %+v drawn %v", state, drawn, tc.wantState(r), tc.wantDrawn)
			}
		})
	}
}

// TestAQueueTabWithNoQueuedTimeIsRefused covers the queue tab's side of the
// invariant: no start is an error, never a tab begun at zero.
func TestAQueueTabWithNoQueuedTimeIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, _, err := h.o.queueTabState(wsm.MergeQueueEntry{Workspace: theWorkspace, State: wsm.MergeQueued})

	// Assert.
	if err == nil {
		t.Fatal("queueTabState answered no error for an entry with no queued time")
	}
}

// TestRepublishingAQueueWithNoQueuedTimeFailsLoudly covers where the
// invariant violation surfaces: the republish answers the error, records it
// once at error with the repository, and draws no queue tab.
func TestRepublishingAQueueWithNoQueuedTimeFailsLoudly(t *testing.T) {
	// Arrange: a queued merge whose stored queued time is missing.
	h := newHarness(t)
	enqueue(t, h)
	h.db.mu.Lock()
	h.db.queues[h.repoKey()][0].EnqueuedAt = time.Time{}
	h.db.mu.Unlock()
	pushed := len(h.feed.tabs())

	// Act.
	err := h.o.republishQueue(context.Background(), h.repoKey())

	// Assert.
	if err == nil {
		t.Fatal("the republish answered no error")
	}
	record, logged := h.recordFor("error", "daemon.merge.queue")
	if !logged || record.Context["repo"] != string(h.repoKey()) || record.Context["error"] == nil {
		t.Fatalf("the error record is %+v (logged %v), want one naming the repository and the cause", record, logged)
	}
	if got := len(h.feed.tabs()); got != pushed {
		t.Fatalf("%d queue tabs were pushed by the failed republish, want none", got-pushed)
	}
}

// queuedAtMS answers when a workspace's merge was queued, as stored.
func queuedAtMS(t *testing.T, h *harness, ws wsm.WorkspaceID) int64 {
	t.Helper()
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	for _, entry := range h.db.queues[h.repoKey()] {
		if entry.Workspace == ws {
			return entry.EnqueuedAt.UnixMilli()
		}
	}
	t.Fatalf("%s is not queued", ws)
	return 0
}

// lastQueueTabOf answers the last queue tab pushed to one workspace's bubble.
func lastQueueTabOf(t *testing.T, h *harness, ws wsm.WorkspaceID) *frontendv1.FeedMergeTabQueue {
	t.Helper()
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	var found *frontendv1.FeedMergeTabQueue
	for _, row := range h.feed.rows {
		if queue := row.Row.GetMergeTab().GetQueue(); row.WS == ws && queue != nil {
			found = queue
		}
	}
	if found == nil {
		t.Fatalf("no queue tab was pushed to %s", ws)
	}
	return found
}

// TestAWaitingEntryKeepsItsQueuedTimeAcrossRepublishes covers the duration of
// an entry that does not move: a queue change elsewhere republishes the
// snapshot, and the entry still counts from when it was queued.
//
// The snapshot is read off the FRONT's bubble: only a merge that started has
// one (owner ruling, 2026-10-08).
func TestAWaitingEntryKeepsItsQueuedTimeAcrossRepublishes(t *testing.T) {
	// Arrange: the front started and is held inside its gate.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	inGate, release := holdTheGate(h)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	queueAnother(t, h, "ws-3", "three")
	got := lastQueueTabOf(t, h, theWorkspace).GetQueue().GetBehind()[0].GetWaiting().GetStageEnteredAtMs()
	close(release)
	<-done

	// Assert.
	if want := queuedAtMS(t, h, "ws-2"); got != want {
		t.Fatalf("ws-2 waits since %d after another merge queued, want its queued time %d", got, want)
	}
}

// TestAMergeInLineIsDrawnInABubblePinnedToTheFeedsBottom pins the placement
// rule: a merge waiting in line has its bubble from the moment it is queued,
// pinned to the feed's bottom until it resolves.
func TestAMergeInLineIsDrawnInABubblePinnedToTheFeedsBottom(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.paused[h.repoKey()] = true

	// Act.
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")

	// Assert.
	heads := h.feed.headPublications()
	drawn := map[wsm.WorkspaceID]bool{}
	for _, head := range heads {
		drawn[head.WS] = true
	}
	if !drawn[theWorkspace] || !drawn["ws-2"] {
		t.Fatalf("merges in line drew bubbles for %v, want both workspaces", drawn)
	}
	if unpinned := livePublishedUnpinned(heads); len(unpinned) != 0 {
		t.Fatalf("merges in line drew %v unpinned, want every live head pinned", unpinned)
	}
}

// TestTheSnapshotsFrontFollowsItsActiveTab covers the published snapshot end
// to end: a waiting workspace sees the front in its tests tab, since that
// round's ledger start.
func TestTheSnapshotsFrontFollowsItsActiveTab(t *testing.T) {
	// Arrange: the harness's merge runs and is held inside its gate.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	inGate, release := holdTheGate(h)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")
	done := admitAsync(h, context.Background())
	<-inGate

	// Act: the front's own bubble draws its standing; ws-2 has none until it
	// starts.
	front := lastQueueTabOf(t, h, theWorkspace).GetQueue().GetCurrent().GetStatus().(*frontendv1.FeedMergeQueueEntry_Merging).Merging
	running, _ := h.o.runFor(theWorkspace)
	tests := running.activeRound()
	close(release)
	<-done

	// Assert.
	if front.GetActiveTab().GetText() != "tests" || front.GetStageEnteredAtMs() != tests.started.UnixMilli() {
		t.Fatalf("the front's bubble draws it in %v since %d, want tests since %d", front.GetActiveTab(), front.GetStageEnteredAtMs(), tests.started.UnixMilli())
	}
}

// TestTheFrontsSettledQueueTabStaysFixed covers the queue tab's run time on
// the front's own bubble: every republish while the run moves on ships the
// same span, its queued time to its run's start.
func TestTheFrontsSettledQueueTabStaysFixed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.gatePasses("daemon")
	enqueue(t, h)
	queued := queuedAtMS(t, h, theWorkspace)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	spans := map[[2]int64]bool{}
	for _, row := range h.feed.rows {
		if settled := row.Row.GetMergeTab().GetQueue().GetSettled(); row.WS == theWorkspace && settled != nil {
			spans[[2]int64{settled.GetStartedAtMs(), settled.GetEndedAtMs()}] = true
		}
	}
	if len(spans) != 1 {
		t.Fatalf("the settled queue tab shipped spans %v, want exactly one", spans)
	}
	for span := range spans {
		if span[0] != queued || span[1] <= queued {
			t.Fatalf("the settled queue tab spans %v, want from the queued time %d to the run's later start", span, queued)
		}
	}
}

// lastHeadOf answers the newest merge head pushed to WS's bubble.
func lastHeadOf(t *testing.T, h *harness, ws wsm.WorkspaceID) *frontendv1.FeedMergeHead {
	t.Helper()
	return lastMergeOf(t, h, ws).GetHead()
}

// lastMergeOf answers the last merge head row pushed to WS, whole.
func lastMergeOf(t *testing.T, h *harness, ws wsm.WorkspaceID) *frontendv1.FeedMerge {
	t.Helper()
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	var found *frontendv1.FeedMerge
	for _, row := range h.feed.rows {
		if merge := row.Row.GetActivity().GetMerge(); row.WS == ws && merge.GetHead() != nil {
			found = merge
		}
	}
	if found == nil {
		t.Fatalf("no merge head was pushed to %s", ws)
	}
	return found
}

func TestAStartedMergesHeadClockRunsFromItsAdmissionAcrossQueueChanges(t *testing.T) {
	// Arrange: the harness clock moves on every read, so a head re-stamped
	// with the current time would show a later start. The merge started and
	// is held inside its gate.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	inGate, release := holdTheGate(h)
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate
	r, running := h.o.runFor(theWorkspace)
	if !running {
		t.Fatal("the merge held in its gate has no run registered")
	}

	// Act.
	queueAnother(t, h, "ws-2", "two")
	got := lastHeadOf(t, h, theWorkspace).GetRuntime().GetStartedAtMs()
	close(release)
	<-done

	// Assert.
	if got != r.startedMS {
		t.Fatalf("the head's clock starts at %d after another merge queued, want the admission %d", got, r.startedMS)
	}
}

func TestARunningMergesHeadClockRunsFromItsAdmissionNotItsQueuedTime(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.gatePasses("daemon")
	enqueue(t, h)
	queued := queuedAtMS(t, h, theWorkspace)

	// Act.
	if err := <-admitAsync(h, context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}

	// Assert: the admission is the queue tab's end, which the harness clock
	// puts strictly after the queued time.
	admitted := lastQueueTabOf(t, h, theWorkspace).GetSettled().GetEndedAtMs()
	if admitted <= queued {
		t.Fatalf("the queue tab ended at %d, want after the queued time %d", admitted, queued)
	}
	if got := lastHeadOf(t, h, theWorkspace).GetRuntime().GetStartedAtMs(); got != admitted {
		t.Fatalf("the running head's clock starts at %d, want the admission %d", got, admitted)
	}
}

func TestAWaitingMergesHeadClockRunsFromItsQueuedTime(t *testing.T) {
	// Arrange: a merge stands behind another, so it is never admitted.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	inGate, release := holdTheGate(h)
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	queueAnother(t, h, "ws-2", "two")
	got := lastHeadOf(t, h, "ws-2").GetRuntime().GetStartedAtMs()
	want := queuedAtMS(t, h, "ws-2")
	close(release)
	<-done

	// Assert.
	if got != want {
		t.Fatalf("the waiting head's clock starts at %d, want the queued time %d", got, want)
	}
}

// TestAMergeHeadsStageIsWhereItStandsInTheQueue covers the live head's badge
// arm (frontend.v1.FeedMergeUpdate.stage): the front, admitted and held in its
// gate, reads merging, and a merge queued behind it reads waiting.
func TestAMergeHeadsStageIsWhereItStandsInTheQueue(t *testing.T) {
	tests := []struct {
		name        string
		ws          wsm.WorkspaceID
		wantWaiting bool
	}{
		{name: "the front merges", ws: theWorkspace, wantWaiting: false},
		{name: "the merge behind waits", ws: "ws-2", wantWaiting: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.emacsRepo()
			h.landsCleanly("abc123def4567")
			inGate, release := holdTheGate(h)
			enqueue(t, h)
			done := admitAsync(h, context.Background())
			<-inGate

			// Act.
			queueAnother(t, h, "ws-2", "two")
			update := lastMergeOf(t, h, tc.ws).GetUpdate()
			close(release)
			<-done

			// Assert.
			if update == nil {
				t.Fatalf("the head of %s is not live", tc.ws)
			}
			if got := update.GetWaiting() != nil; got != tc.wantWaiting || (update.GetMerging() != nil) == tc.wantWaiting {
				t.Fatalf("the head of %s has stage %T, want waiting %v", tc.ws, update.GetStage(), tc.wantWaiting)
			}
		})
	}
}

// TestHeadClockMSFollowsTheEntrysStanding covers the head clock's origin and
// stage per standing: queued and waiting while it waits, the run's admission
// and merging once admitted, and no redraw while an admission has no run
// registered yet.
func TestHeadClockMSFollowsTheEntrysStanding(t *testing.T) {
	tests := []struct {
		name        string
		state       wsm.MergeQueueState
		running     bool
		wantClocked bool
		wantMS      func(r *run) int64
		wantStage   liveHead
	}{
		{name: "waiting", state: wsm.MergeQueued, wantClocked: true,
			wantMS: func(*run) int64 { return 1000 }, wantStage: headWaiting},
		{name: "admitted with its run", state: wsm.MergeAdmitted, running: true, wantClocked: true,
			wantMS: func(r *run) int64 { return r.startedMS }, wantStage: headMerging},
		{name: "admitted before its run registers", state: wsm.MergeAdmitted, wantClocked: false,
			wantMS: func(*run) int64 { return 0 }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			r := ledgeredRun(t, h)
			if tc.running {
				registerRunning(h, r)
			}
			entry := wsm.MergeQueueEntry{Workspace: theWorkspace, State: tc.state, EnqueuedAt: time.UnixMilli(1000)}

			// Act.
			ms, stage, clocked, err := h.o.headClockMS(entry)

			// Assert.
			if err != nil {
				t.Fatalf("headClockMS: %v", err)
			}
			if clocked != tc.wantClocked || ms != tc.wantMS(r) || stage != tc.wantStage {
				t.Fatalf("headClockMS = %d stage %d clocked %v, want %d stage %d clocked %v", ms, stage, clocked, tc.wantMS(r), tc.wantStage, tc.wantClocked)
			}
		})
	}
}

// TestQueueSpanOfFollowsTheEntrysStanding covers the shared wait resolver:
// queued alone while waiting, ended at the run's admission once admitted, and
// nothing while an admission has no run registered yet.
func TestQueueSpanOfFollowsTheEntrysStanding(t *testing.T) {
	tests := []struct {
		name      string
		state     wsm.MergeQueueState
		running   bool
		wantDrawn bool
		wantSpan  func(r *run) queueSpan
	}{
		{name: "waiting", state: wsm.MergeQueued, wantDrawn: true,
			wantSpan: func(*run) queueSpan { return queueSpan{queuedMS: 1000} }},
		{name: "admitted with its run", state: wsm.MergeAdmitted, running: true, wantDrawn: true,
			wantSpan: func(r *run) queueSpan { return queueSpan{queuedMS: 1000, admitted: true, admittedMS: r.startedMS} }},
		{name: "admitted before its run registers", state: wsm.MergeAdmitted, wantDrawn: false,
			wantSpan: func(*run) queueSpan { return queueSpan{} }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			r := ledgeredRun(t, h)
			if tc.running {
				registerRunning(h, r)
			}
			entry := wsm.MergeQueueEntry{Workspace: theWorkspace, State: tc.state, EnqueuedAt: time.UnixMilli(1000)}

			// Act.
			span, drawn, err := h.o.queueSpanOf(entry)

			// Assert.
			if err != nil {
				t.Fatalf("queueSpanOf: %v", err)
			}
			if drawn != tc.wantDrawn || span != tc.wantSpan(r) {
				t.Fatalf("queueSpanOf = %+v drawn %v, want %+v drawn %v", span, drawn, tc.wantSpan(r), tc.wantDrawn)
			}
		})
	}
}

// TestQueueSpanOfRefusesAnEntryWithNoQueuedTime covers the invariant in the
// shared resolver, admitted or not: no queued time is an error.
func TestQueueSpanOfRefusesAnEntryWithNoQueuedTime(t *testing.T) {
	for _, state := range []wsm.MergeQueueState{wsm.MergeQueued, wsm.MergeAdmitted} {
		t.Run(state.String(), func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			registerRunning(h, ledgeredRun(t, h))

			// Act.
			_, drawn, err := h.o.queueSpanOf(wsm.MergeQueueEntry{Workspace: theWorkspace, State: state})

			// Assert.
			if err == nil || drawn {
				t.Fatalf("queueSpanOf answered drawn %v, error %v, want an error and nothing drawn", drawn, err)
			}
		})
	}
}

// TestTheQueueTabAndTheHeadClockShareOneWait asserts both drawings read the
// one wait: a waiting head counts from the queue tab's start, an admitted one
// from the queue tab's end.
func TestTheQueueTabAndTheHeadClockShareOneWait(t *testing.T) {
	for _, state := range []wsm.MergeQueueState{wsm.MergeQueued, wsm.MergeAdmitted} {
		t.Run(state.String(), func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			registerRunning(h, ledgeredRun(t, h))
			entry := wsm.MergeQueueEntry{Workspace: theWorkspace, State: state, EnqueuedAt: time.UnixMilli(1000)}

			// Act.
			tab, _, tabErr := h.o.queueTabState(entry)
			clock, _, _, clockErr := h.o.headClockMS(entry)

			// Assert.
			if tabErr != nil || clockErr != nil {
				t.Fatalf("queueTabState: %v, headClockMS: %v", tabErr, clockErr)
			}
			want := tab.startedMS
			if tab.settled {
				want = tab.endedMS
			}
			if clock != want {
				t.Fatalf("the head clock counts from %d, want the queue tab's %+v boundary %d", clock, tab, want)
			}
		})
	}
}

// TestAWaitingHeadClockWithNoQueuedTimeIsRefused covers the head's side of
// the invariant: no queued time is an error, never a clock begun at zero.
func TestAWaitingHeadClockWithNoQueuedTimeIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, _, clocked, err := h.o.headClockMS(wsm.MergeQueueEntry{Workspace: theWorkspace, State: wsm.MergeQueued})

	// Assert.
	if err == nil || clocked {
		t.Fatalf("headClockMS answered clocked %v, error %v, want an error and no clock", clocked, err)
	}
}

// queueNames answers a queue snapshot's workspaces, ahead / current / behind.
func queueNames(q *frontendv1.FeedMergeQueue) (ahead []string, current string, behind []string) {
	for _, e := range q.GetAhead() {
		ahead = append(ahead, e.GetWorkspace().GetRef().GetId())
	}
	for _, e := range q.GetBehind() {
		behind = append(behind, e.GetWorkspace().GetRef().GetId())
	}
	return ahead, q.GetCurrent().GetWorkspace().GetRef().GetId(), behind
}

// threeInLine queues the harness's merge at the front and two more behind it.
func threeInLine(t *testing.T, h *harness) {
	t.Helper()
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")
	queueAnother(t, h, "ws-3", "three")
}

func TestAWaitingMergesQueueTabDropsTheFrontOnceItLands(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	threeInLine(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the front's merge failed: %v", err)
	}

	// Assert.
	ahead, current, behind := queueNames(lastQueueTabOf(t, h, "ws-2").GetQueue())
	if len(ahead) != 0 || current != "ws-2" || !slices.Equal(behind, []string{"ws-3"}) {
		t.Fatalf("ws-2 sees ahead %v, current %q, behind %v: want the landed front gone", ahead, current, behind)
	}
}

func TestAWaitingMergesQueueTabDropsTheFrontOnceItFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	threeInLine(t, h)
	h.runner.runs = nil
	h.gateBroken(1)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}

	// Assert.
	ahead, _, _ := queueNames(lastQueueTabOf(t, h, "ws-2").GetQueue())
	if len(ahead) != 0 {
		t.Fatalf("ws-2 sees %v ahead, want the failed front gone", ahead)
	}
}

func TestAWaitingMergesQueueTabDropsAMergeEvictedBehindIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	threeInLine(t, h)

	// Act.
	if err := h.o.Evict(context.Background(), "ws-3"); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	_, _, behind := queueNames(lastQueueTabOf(t, h, "ws-2").GetQueue())
	if len(behind) != 0 {
		t.Fatalf("ws-2 sees %v behind, want the evicted merge gone", behind)
	}
}

func TestAnEvictedMergesOwnQueueTabIsSettled(t *testing.T) {
	// Arrange: ws-3 waits, its queue tab live.
	h := newHarness(t)
	threeInLine(t, h)

	// Act.
	if err := h.o.Evict(context.Background(), "ws-3"); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	if tab := lastQueueTabOf(t, h, "ws-3"); tab.GetSettled() == nil {
		t.Fatalf("ws-3's queue tab = %v, want it settled: the merge left the queue", tab)
	}
}

func TestAnEvictedMergesOwnQueueTabShowsTheQueueItLeft(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	threeInLine(t, h)

	// Act.
	if err := h.o.Evict(context.Background(), "ws-3"); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	ahead, current, _ := queueNames(lastQueueTabOf(t, h, "ws-3").GetQueue())
	if current != "ws-3" || !slices.Equal(ahead, []string{string(theWorkspace), "ws-2"}) {
		t.Fatalf("ws-3 sees ahead %v, current %q: want the queue as it stood when it left", ahead, current)
	}
}

func TestALandedMergesOwnQueueTabShowsTheQueueItLeft(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	threeInLine(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the front's merge failed: %v", err)
	}

	// Assert.
	_, current, behind := queueNames(lastQueueTabOf(t, h, theWorkspace).GetQueue())
	if current != string(theWorkspace) || !slices.Equal(behind, []string{"ws-2", "ws-3"}) {
		t.Fatalf("the landed merge sees current %q, behind %v: want the queue it left", current, behind)
	}
}

func TestALeavingMergesQueueTabIsSettledAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	threeInLine(t, h)

	// Act.
	if err := h.o.Evict(context.Background(), "ws-3"); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	for _, rec := range h.logs.Records() {
		if rec.Level == "info" && rec.Operation == "daemon.merge.queue" && strings.Contains(rec.Message, "settled the leaving merge's queue tab") && rec.Context["workspace"] == "ws-3" {
			return
		}
	}
	t.Fatal("no INFO record says the leaving merge's queue tab was settled")
}

func TestAMergeInLineWithNoBubbleIdentityIsRecordedAtInfo(t *testing.T) {
	// Arrange: ws-2's ledger identity is gone from this daemon.
	h := newHarness(t)
	threeInLine(t, h)
	h.o.mu.Lock()
	delete(h.o.ledgerOf, "ws-2")
	h.o.mu.Unlock()

	// Act.
	if err := h.o.republishQueue(context.Background(), h.repoKey()); err != nil {
		t.Fatalf("republishQueue: %v", err)
	}

	// Assert.
	for _, rec := range h.logs.Records() {
		if rec.Level == "info" && strings.Contains(rec.Message, "have no bubble identity") && strings.Contains(fmt.Sprint(rec.Context["workspaces"]), "ws-2") {
			return
		}
	}
	t.Fatal("no INFO record names the merge whose queue tab cannot be redrawn")
}

// reorderingFor is a test reordering whose store answer is DO.
func reorderingFor(do func(wsm.RepoKey) (bool, error)) reordering {
	return reordering{
		op:      "daemon.merge.test_reorder",
		fields:  dlog.Context{"how": "test"},
		failed:  "could not reorder",
		unmoved: "stayed",
		moved:   "moved",
		do:      do,
	}
}

func TestReorderAnswersTheQueuesRefusalUnrecordedAndRunsNoStoreReorder(t *testing.T) {
	// Arrange: a workspace with nothing queued.
	h := newHarness(t)
	ran := false

	// Act.
	moved, err := h.o.reorder(context.Background(), theWorkspace, reorderingFor(func(wsm.RepoKey) (bool, error) {
		ran = true
		return true, nil
	}))

	// Assert.
	if refusal, refused := Refused(err); !refused || refusal.Arm != ArmNoSuchQueuedMerge || moved || ran {
		t.Fatalf("reorder = %v, %v (store ran %v); want the %s refusal and no store reorder", moved, err, ran, ArmNoSuchQueuedMerge)
	}
	if _, logged := h.recordFor("error", "daemon.merge.test_reorder"); logged {
		t.Fatal("the queue's refusal was recorded at ERROR")
	}
}

func TestReorderRecordsAStoreFailureOnceWithItsContext(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	moved, err := h.o.reorder(context.Background(), theWorkspace, reorderingFor(func(wsm.RepoKey) (bool, error) {
		return false, errors.New("database is locked")
	}))

	// Assert.
	if err == nil || moved {
		t.Fatalf("reorder = %v, %v; want the store's failure", moved, err)
	}
	record, logged := h.recordFor("error", "daemon.merge.test_reorder")
	if !logged || record.Context["error"] != "database is locked" || record.Context["how"] != "test" ||
		record.Context["workspace"] != string(theWorkspace) || record.Context["repo"] == nil {
		t.Fatalf("the failure was not recorded at ERROR with its context: %+v", h.logs.Records())
	}
}

func TestReorderRedrawsTheQueueOnlyWhenTheOrderChanged(t *testing.T) {
	tests := []struct {
		name      string
		moved     bool
		wantPlace int
	}{
		{name: "moved", moved: true, wantPlace: 1},
		{name: "unmoved", moved: false, wantPlace: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: two merges wait; the reorder swaps them in the store
			// only when it says it moved.
			h := newHarness(t)
			enqueue(t, h)
			queueAnother(t, h, "ws-2", "two")

			// Act.
			moved, err := h.o.reorder(context.Background(), "ws-2", reorderingFor(func(repo wsm.RepoKey) (bool, error) {
				if !tt.moved {
					return false, nil
				}
				return h.db.MoveMerge(context.Background(), repo, "ws-2", wsm.MergeMoveUp)
			}))

			// Assert.
			if err != nil || moved != tt.moved {
				t.Fatalf("reorder = %v, %v; want %v", moved, err, tt.moved)
			}
			if got := h.factsOf("ws-2").QueuePlace; got != tt.wantPlace {
				t.Fatalf("ws-2's drawn place = %d, want %d", got, tt.wantPlace)
			}
		})
	}
}

func TestEveryQueueReorderingRunsThroughReorder(t *testing.T) {
	// Arrange.
	var calls []string
	for _, file := range []string{"queue.go", "terminal.go"} {
		code, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		// Act.
		for _, call := range []string{"o.deps.DB.MoveMerge(", "o.deps.DB.PrioritizeMerge(", "o.reorder(ctx, ws, reordering{"} {
			for range strings.Count(string(code), call) {
				calls = append(calls, file+":"+call)
			}
		}
	}

	// Assert: each store reorder is called once, inside the reordering its
	// verb hands to reorder.
	want := []string{
		"queue.go:o.deps.DB.MoveMerge(", "queue.go:o.reorder(ctx, ws, reordering{",
		"terminal.go:o.deps.DB.PrioritizeMerge(", "terminal.go:o.reorder(ctx, ws, reordering{",
	}
	if !slices.Equal(calls, want) {
		t.Fatalf("reorder sites = %v, want %v", calls, want)
	}
}

func TestAKickOfASynchronousOrchestratorReachesItsKickSeam(t *testing.T) {
	// Arrange
	h := newHarness(t)
	var kicked []wsm.RepoKey
	h.o.onKick = func(repo wsm.RepoKey) { kicked = append(kicked, repo) }

	// Act
	h.o.kick(h.repoKey())

	// Assert
	if len(kicked) != 1 || kicked[0] != h.repoKey() {
		t.Fatalf("kicks seen = %v, want the one kick of %s", kicked, h.repoKey())
	}
}

// resumeWorld is a repository's queue after a restart: its entries as stored,
// and the merges the recovery has taken back so far.
func resumeWorld(t *testing.T, entries []wsm.MergeQueueEntry, resumed ...ids.WorkspaceID) *harness {
	t.Helper()
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.queues[h.repoKey()] = entries
	h.db.mu.Unlock()
	h.o.mu.Lock()
	for _, ws := range resumed {
		h.o.resumes[ws] = progressDoc{Repo: string(h.repoKey()), Step: TabTests}
	}
	h.o.mu.Unlock()
	return h
}

func admittedEntry(ws ids.WorkspaceID) wsm.MergeQueueEntry {
	return wsm.MergeQueueEntry{Workspace: ws, State: wsm.MergeAdmitted}
}

func TestAfterARestartTheFirstAdmittedMergeInQueueOrderResumesAsTheHead(t *testing.T) {
	// Arrange: both admitted merges taken back, the second one first.
	h := resumeWorld(t, []wsm.MergeQueueEntry{admittedEntry("ws-head"), admittedEntry("ws-behind")}, "ws-behind", "ws-head")

	// Act.
	ws, resuming, err := h.o.resumeIn(context.Background(), h.repoKey())

	// Assert.
	if err != nil || !resuming || ws != "ws-head" {
		t.Fatalf("resumeIn = (%s, %v, %v), want ws-head resumed as the head", ws, resuming, err)
	}
}

func TestAfterARestartNothingResumesAheadOfAHeadNotTakenBackYet(t *testing.T) {
	// Arrange: the head is mid-handover; only the merge behind it is back.
	h := resumeWorld(t, []wsm.MergeQueueEntry{admittedEntry("ws-head"), admittedEntry("ws-behind")}, "ws-behind")

	// Act.
	ws, resuming, err := h.o.resumeIn(context.Background(), h.repoKey())

	// Assert.
	if err != nil || resuming {
		t.Fatalf("resumeIn = (%s, %v, %v), want nothing resumed ahead of the head", ws, resuming, err)
	}
	if !loggedLike(h, logLike{level: "info", op: "daemon.merge.resume", text: "not taken back yet",
		context: map[string]string{"head": "ws-head", "waiting": "ws-behind"}}) {
		t.Fatal("the wait for the head's adoption was not recorded with the head and the merge waiting")
	}
}

func TestAfterARestartAHeadThatTheChainHoldsLeavesTheResumesBehindIt(t *testing.T) {
	// Arrange: the head already runs here in its chain.
	h := resumeWorld(t, []wsm.MergeQueueEntry{admittedEntry("ws-head"), admittedEntry("ws-behind")}, "ws-behind")
	ctx, cancel := context.WithCancelCause(context.Background())
	defer cancel(nil)
	h.o.mu.Lock()
	h.o.chains[h.repoKey()] = []*run{h.o.newRun(ctx, cancel, h.repoKey(), "ws-head", 1)}
	h.o.mu.Unlock()

	// Act.
	_, resuming, err := h.o.resumeIn(context.Background(), h.repoKey())

	// Assert.
	if err != nil || resuming {
		t.Fatalf("resumeIn = (%v, %v), want nothing resumed: the head runs in its chain", resuming, err)
	}
	if loggedLike(h, logLike{text: "not taken back yet"}) {
		t.Fatal("a head running here was recorded as not taken back")
	}
}

func TestAfterARestartAMergeStillWaitingInLineIsNoHead(t *testing.T) {
	// Arrange: a merge waiting in line stands ahead of the admitted one.
	h := resumeWorld(t, []wsm.MergeQueueEntry{{Workspace: "ws-queued", State: wsm.MergeQueued}, admittedEntry("ws-head")}, "ws-head")

	// Act.
	ws, resuming, err := h.o.resumeIn(context.Background(), h.repoKey())

	// Assert.
	if err != nil || !resuming || ws != "ws-head" {
		t.Fatalf("resumeIn = (%s, %v, %v), want ws-head: a waiting merge is nobody's head", ws, resuming, err)
	}
}

func TestAfterARestartAResumeWithNoAdmittedEntryIsStillHandedToItsRun(t *testing.T) {
	// Arrange: the resumed merge's entry is gone.
	h := resumeWorld(t, nil, "ws-gone")

	// Act.
	ws, resuming, err := h.o.resumeIn(context.Background(), h.repoKey())

	// Assert: its run reads the missing entry and ends on it, loudly.
	if err != nil || !resuming || ws != "ws-gone" {
		t.Fatalf("resumeIn = (%s, %v, %v), want ws-gone handed to its run", ws, resuming, err)
	}
}

func TestAfterARestartAQueueThatCannotBeReadResumesNothing(t *testing.T) {
	// Arrange.
	h := resumeWorld(t, []wsm.MergeQueueEntry{admittedEntry("ws-head")}, "ws-head")
	h.db.mu.Lock()
	h.db.queueReadErr = errors.New("the store is gone")
	h.db.mu.Unlock()

	// Act.
	_, resuming, err := h.o.resumeIn(context.Background(), h.repoKey())

	// Assert.
	if err == nil || resuming {
		t.Fatalf("resumeIn = (%v, %v), want the read's failure surfaced and nothing resumed", resuming, err)
	}
}

// pendingResume leaves the harness's merge taken back by the boot recovery
// and not resumed yet: admitted, its lease adopted, its record in o.resumes.
func pendingResume(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}
	return h
}

func TestEvictingAMergeTakenBackButNotResumedDropsItsEntryAndItsLeaseTogether(t *testing.T) {
	// Arrange.
	h := pendingResume(t)

	// Act.
	err := h.o.Evict(context.Background(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("Evict: %v", err)
	}
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	_, held, _ := h.db.Lease(context.Background(), theWorkspace)
	_, progress, _ := h.db.MergeProgressOf(context.Background(), theWorkspace)
	if len(entries) != 0 || held || progress {
		t.Fatalf("after the evict: %d entries, lease held %v, progress %v; want none of them", len(entries), held, progress)
	}
	h.o.mu.Lock()
	_, pending := h.o.resumes[theWorkspace]
	h.o.mu.Unlock()
	if pending {
		t.Fatal("the evicted merge is still handed to the pump to resume")
	}
}

func TestEvictingAMergeTakenBackButNotResumedEndsItsBubbleAsAMergeThatRan(t *testing.T) {
	// Arrange.
	h := pendingResume(t)

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	if got, want := h.feed.lastAbandonedSummary(), runningSummaries[CauseUserDrop]; got != want {
		t.Fatalf("the bubble ended %q, want %q", got, want)
	}
	if !recordedAt(h, "info", "dropped a merge a restart took back before it resumed: its entry, its lease and its progress record together") {
		t.Fatal("the drop was not recorded")
	}
}

func TestAFailedDropOfAMergeTakenBackLeavesItToResume(t *testing.T) {
	// Arrange.
	h := pendingResume(t)
	h.db.mu.Lock()
	h.db.dropAdmittedErr = errors.New("the store refused")
	h.db.mu.Unlock()

	// Act.
	err := h.o.Evict(context.Background(), theWorkspace)

	// Assert.
	if err == nil {
		t.Fatal("Evict succeeded over a refused drop")
	}
	h.o.mu.Lock()
	_, pending := h.o.resumes[theWorkspace]
	h.o.mu.Unlock()
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if !pending || len(entries) != 1 {
		t.Fatalf("after the refused drop: pending %v, %d entries; want the merge left to resume", pending, len(entries))
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.drop_pending_resume"})
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["lease"] == "" || rec.Context["error"] == nil {
		t.Fatalf("the refused drop was recorded as %+v, want an ERROR naming the workspace, its lease and the cause", rec)
	}
}

func TestAMergeTakenBackWhoseEntryCannotBeReadIsLeftToResume(t *testing.T) {
	// Arrange.
	h := pendingResume(t)
	h.db.mu.Lock()
	h.db.queueReadErr = errors.New("the store is gone")
	h.db.mu.Unlock()

	// Act.
	err := h.o.Evict(context.Background(), theWorkspace)

	// Assert.
	h.o.mu.Lock()
	_, pending := h.o.resumes[theWorkspace]
	h.o.mu.Unlock()
	if err == nil || !pending {
		t.Fatalf("Evict = %v, pending %v; want the read's failure and the merge left to resume", err, pending)
	}
}

func TestClosingTheWorkspaceOfAMergeTakenBackDropsItAsAnAdmittedMerge(t *testing.T) {
	// Arrange.
	h := pendingResume(t)

	// Act.
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert.
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); held {
		t.Fatal("the closed workspace's merge lease outlived its entry")
	}
}

func TestDroppingAnAdmittedMergeWithNoRunHereAsAWaitingOneIsRefused(t *testing.T) {
	// Arrange: an admitted entry another daemon drives.
	h := newHarness(t)
	enqueue(t, h)
	if err := h.db.AdmitMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatal(err)
	}

	// Act.
	err := h.o.Evict(context.Background(), theWorkspace)

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if err == nil || len(entries) != 1 {
		t.Fatalf("Evict = %v leaving %d entries, want the refusal and the entry kept", err, len(entries))
	}
	if !recordedAt(h, "error", "refused to drop an admitted merge as a waiting one") {
		t.Fatal("the refusal was not recorded")
	}
}

func TestAMergeTakenBackWithNoLeaseRecordedIsADefect(t *testing.T) {
	// Arrange.
	h := pendingResume(t)
	h.o.mu.Lock()
	doc := h.o.resumes[theWorkspace]
	delete(h.o.ledgerOf, theWorkspace)
	h.o.mu.Unlock()

	// Act / Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("a taken-back merge with no lease did not fail hard")
		}
	}()
	_ = h.o.dropPendingResume(context.Background(), theWorkspace, doc, CauseUserDrop)
}

// TestTheQueueTabNamesAnAsyncMergeForItselfAndOpensItsBubble covers the queue
// view: a sync merge's entry is its own workspace and carries no bubble; an
// async merge's is the hidden merge workspace, named for itself, and carries
// its bubble -- the requester and the head row on the requester's feed --
// which is what its click selects and opens.
func TestTheQueueTabNamesAnAsyncMergeForItselfAndOpensItsBubble(t *testing.T) {
	tests := []struct {
		name     string
		ws       ids.WorkspaceID
		ledgered bool
		observe  func(*frontendv1.FeedMergeQueueEntry) any
		want     func(merge wsm.Workspace) any
	}{
		{name: "a sync merge names its own workspace", ws: theWorkspace,
			observe: func(e *frontendv1.FeedMergeQueueEntry) any { return e.GetLabel().GetText() },
			want:    func(wsm.Workspace) any { return "requester" }},
		{name: "a sync merge carries no bubble", ws: theWorkspace,
			observe: func(e *frontendv1.FeedMergeQueueEntry) any { return e.GetBubble() == nil },
			want:    func(wsm.Workspace) any { return true }},
		{name: "an async merge names its merge for itself", ws: theMergeWorkspace, ledgered: true,
			observe: func(e *frontendv1.FeedMergeQueueEntry) any { return e.GetLabel().GetText() },
			want:    func(m wsm.Workspace) any { return m.Name }},
		{name: "an async merge's entry is its own workspace", ws: theMergeWorkspace, ledgered: true,
			observe: func(e *frontendv1.FeedMergeQueueEntry) any { return e.GetWorkspace().GetRef().GetId() },
			want:    func(wsm.Workspace) any { return string(theMergeWorkspace) }},
		{name: "an async merge's bubble stands in its requester", ws: theMergeWorkspace, ledgered: true,
			observe: func(e *frontendv1.FeedMergeQueueEntry) any { return e.GetBubble().GetRequester().GetId() },
			want:    func(wsm.Workspace) any { return string(theWorkspace) }},
		{name: "an async merge's bubble is its head row on the requester's feed", ws: theMergeWorkspace, ledgered: true,
			observe: func(e *frontendv1.FeedMergeQueueEntry) any { return e.GetBubble().GetRow().GetValue() },
			want: func(wsm.Workspace) any {
				return feedid.Encode(headRef(theWorkspace, "lease-1")).GetValue()
			}},
		{name: "an async merge with no bubble identity still names its requester", ws: theMergeWorkspace,
			observe: func(e *frontendv1.FeedMergeQueueEntry) any { return e.GetBubble().GetRequester().GetId() },
			want:    func(wsm.Workspace) any { return string(theWorkspace) }},
		{name: "an async merge with no bubble identity opens no row", ws: theMergeWorkspace,
			observe: func(e *frontendv1.FeedMergeQueueEntry) any { return e.GetBubble().GetRow() == nil },
			want:    func(wsm.Workspace) any { return true }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the requester, and the merge workspace forked for its async merge.
			h := newHarness(t)
			h.register(theWorkspace, "requester")
			merge := h.registerMergeWorkspace(theWorkspace, "req-1", "feature")
			if tt.ledgered {
				h.o.mu.Lock()
				h.o.ledgerOf[theMergeWorkspace] = "lease-1"
				h.o.mu.Unlock()
			}
			entries := []wsm.MergeQueueEntry{{Repo: h.repoKey(), Workspace: tt.ws, Position: 1, EnqueuedAt: h.clock()}}

			// Act.
			view, err := h.o.viewQueue(context.Background(), h.repoKey(), entries)

			// Assert.
			if err != nil {
				t.Fatalf("viewQueue: %v", err)
			}
			if got, want := tt.observe(view.snapshot(entries, tt.ws).GetCurrent()), tt.want(merge); got != want {
				t.Fatalf("got %v, want %v", got, want)
			}
		})
	}
}

// TestTheQueueTabIsNotDrawnWhenAnAsyncMergesRequesterCannotBeRead covers the
// failure: a requester the store cannot answer fails the drawing, rather than
// drawing an entry whose click would select the hidden merge workspace.
func TestTheQueueTabIsNotDrawnWhenAnAsyncMergesRequesterCannotBeRead(t *testing.T) {
	// Arrange: a merge workspace whose requester is not registered.
	h := newHarness(t)
	h.registerMergeWorkspace("ws-gone", "req-1", "feature")
	entries := []wsm.MergeQueueEntry{{Repo: h.repoKey(), Workspace: theMergeWorkspace, Position: 1, EnqueuedAt: h.clock()}}

	// Act.
	_, err := h.o.viewQueue(context.Background(), h.repoKey(), entries)

	// Assert.
	if err == nil {
		t.Fatal("viewQueue drew a merge whose requester could not be read")
	}
}

// TestADequeueDecidesAgainstASuspensionUnderOneLock covers every way an
// abandonment meets a suspension: decided first it ends the run; after a
// suspension this daemon takes back, it waits for that take-back; after one
// that hands the merge on, it is refused.
func TestADequeueDecidesAgainstASuspensionUnderOneLock(t *testing.T) {
	tests := []struct {
		name string
		// run registers a live run; suspended marks it suspended by it.
		run       bool
		suspended *suspension
		// away marks the workspace taken away by it.
		away *suspension
		// abandoned is an abandonment already on the run, recorded is one
		// already left for the take-back.
		abandoned, recorded AbandonCause
		wantErr             bool
		wantAbandoned       AbandonCause
		wantRecorded        AbandonCause
		wantRecord          string
	}{
		{name: "a run no suspension took is abandoned", run: true,
			wantAbandoned: CauseUserDrop, wantRecord: "abandoning a running merge"},
		{name: "the first abandonment of a run stands", run: true, abandoned: CauseWorkspaceClosed,
			wantAbandoned: CauseWorkspaceClosed, wantRecord: "abandoning a running merge"},
		{name: "a run a pause took is ended at its take-back", run: true, suspended: &suspendPause, away: &suspendPause,
			wantRecorded: CauseUserDrop, wantRecord: "a suspension took the merge first; the abandonment ends it at its take-back"},
		{name: "a merge whose run let go to a pause is ended at its take-back", away: &suspendPause,
			wantRecorded: CauseUserDrop, wantRecord: "a suspension took the merge first; the abandonment ends it at its take-back"},
		{name: "the first abandonment left for a take-back stands", away: &suspendPause, recorded: CauseWorkspaceClosed,
			wantRecorded: CauseWorkspaceClosed, wantRecord: "a suspension took the merge first; the abandonment ends it at its take-back"},
		{name: "a run the daemon's exit took is refused", run: true, suspended: &suspendExit,
			wantErr: true, wantRecord: "refused to abandon a merge a suspension handed on"},
		{name: "a run a transfer took is refused", run: true, suspended: &suspendTransfer, away: &suspendTransfer,
			wantErr: true, wantRecord: "refused to abandon a merge a suspension handed on"},
		{name: "a merge a transfer moved away is refused", away: &suspendTransfer,
			wantErr: true, wantRecord: "refused to abandon a merge a suspension handed on"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			var r *run
			if tc.run {
				r = liveRun(t, h, theWorkspace)
				close(r.finished)
			} else {
				forgetTakeBackAtCleanup(t, h, theWorkspace)
			}
			h.o.mu.Lock()
			if tc.suspended != nil {
				r.suspendLocked(*tc.suspended)
			}
			if tc.away != nil {
				h.o.takenAway[theWorkspace] = *tc.away
			}
			if tc.abandoned != "" {
				r.abandonedBy = tc.abandoned
			}
			if tc.recorded != "" {
				h.o.abandonsAtTakeBack[theWorkspace] = tc.recorded
			}
			h.o.mu.Unlock()

			// Act.
			err := h.o.dequeue(context.Background(), theWorkspace, CauseUserDrop)

			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("dequeue = %v, want an error %v", err, tc.wantErr)
			}
			h.o.mu.Lock()
			recorded := h.o.abandonsAtTakeBack[theWorkspace]
			var abandoned AbandonCause
			if r != nil {
				abandoned = r.abandonedBy
			}
			h.o.mu.Unlock()
			if abandoned != tc.wantAbandoned || recorded != tc.wantRecorded {
				t.Fatalf("abandoned %q and left for the take-back %q, want %q and %q", abandoned, recorded, tc.wantAbandoned, tc.wantRecorded)
			}
			if !loggedLike(h, logLike{level: "info", op: "daemon.merge.abandon", text: tc.wantRecord, context: map[string]string{"workspace": string(theWorkspace)}}) {
				t.Fatalf("no info record %q of the workspace: %v", tc.wantRecord, h.logs.Records())
			}
		})
	}
}

// TestADequeueOfARunSuspendedWithNoTakeBackRecordedPanics covers the
// invariant every suspension this daemon takes back keeps: it marks the
// workspace taken away.
func TestADequeueOfARunSuspendedWithNoTakeBackRecordedPanics(t *testing.T) {
	// Arrange: a run a pause took, with no take-back mark.
	h := newHarness(t)
	r := liveRun(t, h, theWorkspace)
	h.o.mu.Lock()
	r.suspendLocked(suspendPause)
	h.o.mu.Unlock()
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("dequeue coped with a suspended run that has no take-back")
		}
		h.o.mu.Lock()
		_, recorded := h.o.abandonsAtTakeBack[theWorkspace]
		h.o.mu.Unlock()
		if recorded {
			t.Fatal("the violated invariant left an abandonment behind")
		}
	}()

	// Act.
	_ = h.o.dequeue(context.Background(), theWorkspace, CauseUserDrop)
}

func TestAQueuedMergesLabelThatCannotBeReadIsRecordedByWhyItFailed(t *testing.T) {
	tests := []struct {
		name      string
		cancel    bool
		cause     error
		wantLevel string
	}{
		{name: "a read cut by its ended context is no failure", cancel: true, cause: context.Canceled, wantLevel: "info"},
		{name: "a read that failed on its own is an error", cause: errors.New("git broke"), wantLevel: "error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.git.mainWorktreeErr = tt.cause
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if tt.cancel {
				cancel()
			}
			entry := wsm.MergeQueueEntry{Workspace: theWorkspace, Source: wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "feature"}}

			// Act
			label := h.o.bubbleLabel(ctx, entry)

			// Assert
			if label != string(theWorkspace) {
				t.Fatalf("label = %q, want the workspace's id standing in", label)
			}
			recs := dlog.FilterRecords(h.logs.Records(), dlog.RecordQuery{Operation: "daemon.merge.queue"})
			if len(recs) != 1 || recs[0].Level != tt.wantLevel || recs[0].Context["workspace"] != string(theWorkspace) {
				t.Fatalf("records = %+v, want one %s record naming the workspace", recs, tt.wantLevel)
			}
		})
	}
}
