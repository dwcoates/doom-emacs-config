package merge

import (
	"context"
	"errors"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/wsm"
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
func TestAWaitingEntryKeepsItsQueuedTimeAcrossRepublishes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")

	// Act.
	queueAnother(t, h, "ws-3", "three")

	// Assert.
	got := lastQueueTabOf(t, h, theWorkspace).GetQueue().GetBehind()[0].GetWaiting().GetStageEnteredAtMs()
	if want := queuedAtMS(t, h, "ws-2"); got != want {
		t.Fatalf("ws-2 waits since %d after another merge queued, want its queued time %d", got, want)
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

	// Act.
	front := lastQueueTabOf(t, h, "ws-2").GetQueue().GetAhead()[0].GetMerging()
	running, _ := h.o.runFor(theWorkspace)
	tests := running.activeRound()
	close(release)
	<-done

	// Assert.
	if front.GetActiveTab().GetText() != "tests" || front.GetStageEnteredAtMs() != tests.started.UnixMilli() {
		t.Fatalf("ws-2 sees the front in %v since %d, want tests since %d", front.GetActiveTab(), front.GetStageEnteredAtMs(), tests.started.UnixMilli())
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
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	var found *frontendv1.FeedMergeHead
	for _, row := range h.feed.rows {
		if head := row.Row.GetActivity().GetMerge().GetHead(); row.WS == ws && head != nil {
			found = head
		}
	}
	if found == nil {
		t.Fatalf("no merge head was pushed to %s", ws)
	}
	return found
}

func TestAQueuedMergesHeadClockRunsFromItsQueuedTimeAcrossQueueChanges(t *testing.T) {
	// Arrange: the harness clock moves on every read, so a head re-stamped
	// with the current time would show a later start.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	queueAnother(t, h, "ws-2", "two")

	// Assert.
	got := lastHeadOf(t, h, theWorkspace).GetRuntime().GetStartedAtMs()
	if want := queuedAtMS(t, h, theWorkspace); got != want {
		t.Fatalf("the head's clock starts at %d after another merge queued, want the queued time %d", got, want)
	}
}

func TestARunningMergesHeadClockRunsFromItsQueuedTime(t *testing.T) {
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

	// Assert.
	if got := lastHeadOf(t, h, theWorkspace).GetRuntime().GetStartedAtMs(); got != queued {
		t.Fatalf("the running head's clock starts at %d, want the queued time %d", got, queued)
	}
}
