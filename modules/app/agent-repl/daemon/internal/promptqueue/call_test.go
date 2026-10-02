package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/classifier"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The tests of call.go: no queue lock is held across a call to the shim, and
// the claim a call stands on keeps the queue's order and its exactly-once
// delivery while the lock is free.

// lockFree reports whether the workspace's delivery lock can be taken now,
// and gives it straight back.
func lockFree(h *harness) bool {
	if !h.q.state(theWorkspace).drain.TryLock() {
		return false
	}
	h.q.state(theWorkspace).drain.Unlock()
	return true
}

// verdictsFree reports whether the workspace's verdict lock can be taken now,
// and gives it straight back.
func verdictsFree(h *harness) bool {
	if !h.q.state(theWorkspace).verdicts.TryLock() {
		return false
	}
	h.q.state(theWorkspace).verdicts.Unlock()
	return true
}

// bubbleSubmission is a prompt addressed to the subagent "sub-agent".
func bubbleSubmission(turn ids.TurnID) Submission {
	sub := submission(turn, "keep going")
	sub.Target = &feedid.Ref{
		WS:   theWorkspace,
		Feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "sub-agent"}},
		Row:  feedid.RowKey{Kind: feedid.KindActivity, ID: "spawn-1", Sub: "sub-agent"},
	}
	return sub
}

// heldByDrainLease records T1 under the drain lease and then lifts the lease,
// leaving a deliverable hold nothing is delivering.
func heldByDrainLease(t *testing.T, h *harness, turn ids.TurnID) {
	t.Helper()
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission(turn, "held")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
}

func setModel(value string) Act { return Act{Kind: ActSetModel, Value: value} }

func TestTheDeliveryLockIsFreeWhileTheShimIsCalled(t *testing.T) {
	tests := []struct {
		name string
		// run arranges the call and makes it, observing the lock with OBSERVE
		// from inside the call.
		run func(t *testing.T, h *harness, observe func())
	}{
		{"a submission's StartTurn", func(t *testing.T, h *harness, observe func()) {
			h.sender.startHook = observe
			if _, err := h.q.Submit(context.Background(), submission("t1", "go")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
		}},
		{"a turn end's StartTurn", func(t *testing.T, h *harness, observe func()) {
			running(t, h, "running-turn", "the running work")
			heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
			h.sender.startHook = observe
			h.watcher.idle()
			h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
		}},
		{"a context cut's StartTurn", func(t *testing.T, h *harness, observe func()) {
			h.sender.startHook = observe
			if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActCompact}); err != nil {
				t.Fatalf("SubmitSessionAct: %v", err)
			}
		}},
		{"a session act's SetModel", func(t *testing.T, h *harness, observe func()) {
			h.sender.callHook = func(string) { observe() }
			if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, setModel("opus")); err != nil {
				t.Fatalf("SubmitSessionAct: %v", err)
			}
		}},
		{"a bubble prompt's PromptAgent", func(t *testing.T, h *harness, observe func()) {
			h.sender.callHook = func(string) { observe() }
			if _, err := h.q.Submit(context.Background(), bubbleSubmission("b1")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
		}},
		{"a join's JoinRunningTurn", func(t *testing.T, h *harness, observe func()) {
			h.sender.callHook = func(string) { observe() }
			sentToJoin(t, h)
		}},
		{"a held prompt's revival", func(t *testing.T, h *harness, observe func()) {
			heldByDrainLease(t, h, "t1")
			h.clientReaped = true
			h.reviveHook = func() {
				observe()
				h.clientReaped = false
			}
			if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
				t.Fatalf("Release: %v", err)
			}
		}},
		{"a rollback's rewind", func(t *testing.T, h *harness, observe func()) {
			if err := h.q.RollBack(context.Background(), theWorkspace, instant, nil, func(context.Context) error {
				observe()
				return nil
			}); err != nil {
				t.Fatalf("RollBack: %v", err)
			}
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			observed, free := false, true

			// Act
			tc.run(t, h, func() {
				observed = true
				free = free && lockFree(h)
			})

			// Assert
			if !observed {
				t.Fatal("the call was never made")
			}
			if !free {
				t.Fatal("the delivery lock was held while the shim was called")
			}
		})
	}
}

func TestASubmissionDuringATurnStartIsHeldBehindThatTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.standOnOpening = true
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	var during Disposition
	var duringErr error
	h.sender.startHook = func() {
		h.sender.startHook = nil
		during, duringErr = h.q.Submit(context.Background(), submission("t2", "next"))
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "first")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if duringErr != nil || during.Delivered || during.Classification == nil {
		t.Fatalf("submission during the call = (%+v, %v), want held and judged behind the opening turn", during, duringErr)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want only the turn whose call stood", started)
	}
	if held := h.db.hold("t2"); held.Turn != "t2" || held.Tombstone != nil {
		t.Fatalf("hold of t2 = %+v, want it standing", held)
	}
}

func TestARepeatOfTheTurnBeingStartedIsAnsweredAsItsDelivery(t *testing.T) {
	// Arrange
	h := newHarness(t)
	var during Disposition
	h.sender.startHook = func() {
		h.sender.startHook = nil
		during, _ = h.q.Submit(context.Background(), submission("t1", "first"))
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "first")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if !during.Delivered || h.sender.startAttempts() != 1 {
		t.Fatalf("repeat = %+v after %d starts, want it answered as the one delivery", during, h.sender.startAttempts())
	}
}

func TestASubmissionDuringASessionActIsDeliveredWhenTheActSettles(t *testing.T) {
	// Arrange
	h := newHarness(t)
	var during Disposition
	h.sender.callHook = func(string) {
		h.sender.callHook = nil
		during, _ = h.q.Submit(context.Background(), submission("t2", "after the change"))
	}

	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, setModel("opus")); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Assert
	if during.Delivered || during.Classification != nil {
		t.Fatalf("submission during the act = %+v, want parked unjudged", during)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t2" {
		t.Fatalf("started = %v, want the parked prompt delivered once the act settled", started)
	}
	if held := h.db.hold("t2"); held.Tombstone == nil || held.Tombstone.Kind != tombstoneDelivered {
		t.Fatalf("hold of t2 = %+v, want it retired as delivered", held)
	}
}

func TestASessionActDuringATurnStartIsHeldBehindThatTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.standOnOpening = true
	var duringErr error
	h.sender.startHook = func() {
		h.sender.startHook = nil
		duringErr = h.q.SubmitSessionAct(context.Background(), theWorkspace, setModel("opus"))
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "first")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if duringErr != nil {
		t.Fatalf("SubmitSessionAct during the call: %v", duringErr)
	}
	if models := h.sender.modelsSet(); len(models) != 0 {
		t.Fatalf("models = %v, want the act held behind the turn the call opened", models)
	}
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil || len(standing) != 1 || standing[0].Act == nil {
		t.Fatalf("standing = (%+v, %v), want the act held", standing, err)
	}
}

func TestAPromptHeldBehindAFailedTurnStartIsDeliveredWhenItSettles(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.standOnOpening = true
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	calls := 0
	h.sender.startHook = func() {
		calls++
		if calls == 1 {
			h.sender.startErr = errors.New("the shim refused")
			if _, err := h.q.Submit(context.Background(), submission("t2", "next")); err != nil {
				t.Errorf("Submit during the call: %v", err)
			}
			return
		}
		h.sender.startErr = nil
	}

	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "first"))
	h.q.waitForClassifications()

	// Assert
	if err == nil {
		t.Fatal("Submit of t1 succeeded, want the refusal surfaced")
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t2" {
		t.Fatalf("started = %v, want the held prompt delivered once the refused start settled", started)
	}
	if held := h.db.hold("t2"); held.Tombstone == nil || held.Tombstone.Kind != tombstoneDelivered {
		t.Fatalf("hold of t2 = %+v, want it retired as delivered", held)
	}
}

func TestAPopDuringAShimCallIsDeferredToItsSettle(t *testing.T) {
	// Arrange: t1 stands deliverable but withheld by an edit; a rollback's
	// rewind is the call, and the edit's release during it would pop.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	h.watcher.idle()
	if err := h.q.BeginEdit(context.Background(), theWorkspace, "t1", func() bool { return true }); err != nil {
		t.Fatalf("BeginEdit: %v", err)
	}
	var startedDuring []ids.TurnID
	perform := func(context.Context) error {
		h.q.EditorGone(theWorkspace)
		startedDuring = h.sender.started()
		return nil
	}

	// Act
	if err := h.q.RollBack(context.Background(), theWorkspace, instant.Add(1), nil, perform); err != nil {
		t.Fatalf("RollBack: %v", err)
	}

	// Assert
	if len(startedDuring) != 0 {
		t.Fatalf("started during the rewind = %v, want the pop deferred", startedDuring)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want t1 delivered once the rewind settled", started)
	}
	if !hasRecord(h, "info", opCall, "a shim call is in flight on the workspace; the decision is taken again once it settles") {
		t.Fatalf("records = %+v, want the deferral on disk", h.log.Records())
	}
}

func TestABounceAskedDuringAShimCallIsDecidedWhenItSettles(t *testing.T) {
	tests := []struct {
		name string
		// request is the bounce asked while the call stands.
		request func(g *gate) bounce.Request
		// overAct makes the call a session act, which opens no turn.
		overAct bool
		started bool
	}{
		{"a dispatch-quiet move runs over the turn the call opened", func(g *gate) bounce.Request { return g.quietMoving("handover_transfer") }, false, true},
		{"a forced bounce runs over the turn the call opened", func(g *gate) bounce.Request { return g.request("build_stale", true) }, false, true},
		{"an unforced bounce waits for the turn the call opened", func(g *gate) bounce.Request { return g.request("build_stale", false) }, false, false},
		{"an unforced bounce runs once the act leaves the workspace free", func(g *gate) bounce.Request { return g.request("build_stale", false) }, true, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.watcher.standOnOpening = true
			g := newGate()
			var decision bounce.Decision
			ask := func() {
				var err error
				decision, err = h.q.RequestBounce(context.Background(), theWorkspace, tc.request(g))
				if err != nil {
					t.Errorf("RequestBounce: %v", err)
				}
			}

			// Act
			if tc.overAct {
				h.sender.callHook = func(string) { h.sender.callHook = nil; ask() }
				if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, setModel("opus")); err != nil {
					t.Fatalf("SubmitSessionAct: %v", err)
				}
			} else {
				h.sender.startHook = func() { h.sender.startHook = nil; ask() }
				if _, err := h.q.Submit(context.Background(), submission("t1", "go")); err != nil {
					t.Fatalf("Submit: %v", err)
				}
			}

			// Assert
			if decision.Now {
				t.Fatalf("decision = %+v, want it deferred while the call stood", decision)
			}
			if tc.started {
				g.awaitStart(t)
				g.finish(h, nil)
			} else if g.runs.Load() != 0 {
				t.Fatal("the bounce ran over work it waits for")
			}
		})
	}
}

func TestAReleaseDuringAShimCallIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	heldByDrainLease(t, h, "t2")
	var releaseErr error
	h.sender.startHook = func() {
		h.sender.startHook = nil
		releaseErr = h.q.Release(context.Background(), theWorkspace, "t2")
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "go")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if !errors.Is(releaseErr, ErrReleaseRefused) {
		t.Fatalf("Release during the call = %v, want ErrReleaseRefused", releaseErr)
	}
	if !hasRecord(h, "info", opRelease, "a delivery to the shim is in flight; the release is refused") {
		t.Fatalf("records = %+v, want the refusal on disk", h.log.Records())
	}
}

func TestAStepOnTheHoldBeingDeliveredIsRefusedAsBeingDelivered(t *testing.T) {
	tests := []struct {
		name string
		step func(h *harness) error
	}{
		{"a drop", func(h *harness) error { return h.q.Drop(context.Background(), theWorkspace, "t1") }},
		{"an edit", func(h *harness) error {
			return h.q.BeginEdit(context.Background(), theWorkspace, "t1", func() bool { return true })
		}},
		{"a fold into it", func(h *harness) error { return h.q.Fold(context.Background(), theWorkspace, "t2", "t1") }},
		{"a release of it", func(h *harness) error { return h.q.Release(context.Background(), theWorkspace, "t1") }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: t1 is the next to go when the running turn ends.
			h := newHarness(t)
			h.watcher.standOnOpening = true
			running(t, h, "running-turn", "the running work")
			heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
			heldPrompt(t, h, "t2", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
			var stepErr error
			h.sender.startHook = func() {
				h.sender.startHook = nil
				stepErr = tc.step(h)
			}

			// Act
			h.watcher.idle()
			h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

			// Assert
			if !errors.Is(stepErr, ErrBeingDelivered) {
				t.Fatalf("step during the delivery = %v, want it refused as being delivered", stepErr)
			}
			if !hasRecord(h, "info", opRelease, "the step is refused: the held prompt is being delivered to the shim") &&
				!hasRecord(h, "info", opDrop, "the step is refused: the held prompt is being delivered to the shim") &&
				!hasRecord(h, "info", opEditBegin, "the step is refused: the held prompt is being delivered to the shim") &&
				!hasRecord(h, "info", opFold, "the step is refused: the held prompt is being delivered to the shim") {
				t.Fatalf("records = %+v, want the refusal on disk at INFO", h.log.Records())
			}
			if held := h.db.hold("t1"); held.Tombstone == nil || held.Tombstone.Kind != tombstoneDelivered {
				t.Fatalf("hold of t1 = %+v, want it delivered once and retired as such", held)
			}
		})
	}
}

func TestARollbackClaimsTheHoldsItDrops(t *testing.T) {
	// Arrange
	h := newHarness(t)
	heldByDrainLease(t, h, "t1")
	var dropErr error

	// Act
	err := h.q.RollBack(context.Background(), theWorkspace, instant.Add(-1), []ids.TurnID{"t1"}, func(context.Context) error {
		dropErr = h.q.Drop(context.Background(), theWorkspace, "t1")
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if !errors.Is(dropErr, ErrBeingDelivered) {
		t.Fatalf("drop during the rewind = %v, want the claimed hold refused as being delivered", dropErr)
	}
	if held := h.db.hold("t1"); held.Tombstone == nil || held.Tombstone.Kind != tombstoneRolledBack {
		t.Fatalf("hold of t1 = %+v, want it retired by the rollback", held)
	}
}

func TestASecondShimCallWhileOneStandsFailsHard(t *testing.T) {
	// Arrange
	h := newHarness(t)
	log, err := h.q.logger(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("logger: %v", err)
	}
	d := h.q.lockDelivery(theWorkspace)
	var recovered any

	// Act
	func() {
		defer func() { recovered = recover() }()
		d.outside(shimCall{what: "start_turn", turn: "t1"}, log, func() {
			d.outside(shimCall{what: "session_act"}, log, func() {})
		})
	}()
	d.state.drain.Unlock()

	// Assert
	if recovered == nil {
		t.Fatal("a second call beside a standing one did not fail hard")
	}
	if !hasRecord(h, "error", opCall, "a second shim call was made while one stands") {
		t.Fatalf("records = %+v, want the violation on disk", h.log.Records())
	}
	if _, standing := h.q.standingCall(theWorkspace); standing {
		t.Fatal("a call still stands after the failed one unwound")
	}
}

func TestABounceStartedWhileAShimCallStandsFailsHard(t *testing.T) {
	// Arrange
	h := newHarness(t)
	log, err := h.q.logger(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("logger: %v", err)
	}
	d := h.q.lockDelivery(theWorkspace)
	d.state.bounce = &pendingBounce{}
	d.state.bounce.register(newGate().request("build_stale", false))
	var recovered any

	// Act
	func() {
		defer func() { recovered = recover() }()
		d.outside(shimCall{what: "start_turn", turn: "t1"}, log, func() {
			d.state.drain.Lock()
			defer d.state.drain.Unlock()
			h.q.startBounceLocked(d, log)
		})
	}()
	d.state.drain.Unlock()

	// Assert
	if recovered == nil {
		t.Fatal("a bounce started beside a standing call did not fail hard")
	}
	if !hasRecord(h, "error", opBounce, "a bounce was started while a shim call stands") {
		t.Fatalf("records = %+v, want the violation on disk", h.log.Records())
	}
}

func TestARedriveForAnUnresolvableWorkspaceLeavesTheParkedPromptHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.callHook = func(string) {
		h.sender.callHook = nil
		if _, err := h.q.Submit(context.Background(), submission("t2", "after the change")); err != nil {
			t.Errorf("Submit: %v", err)
		}
		h.db.mu.Lock()
		h.db.workspaceErr = errors.New("the state store is gone")
		h.db.mu.Unlock()
	}

	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, setModel("opus")); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Assert
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want nothing delivered for a workspace that cannot be resolved", started)
	}
	if held := h.db.hold("t2"); held.Turn != "t2" || held.Tombstone != nil {
		t.Fatalf("hold of t2 = %+v, want it standing", held)
	}
	if !hasRecord(h, "error", opSubmit, "could not resolve the workspace") {
		t.Fatalf("records = %+v, want the unresolvable workspace on disk", h.log.Records())
	}
}

func TestARedriveThatCannotDeliverRecordsItAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.callHook = func(string) {
		h.sender.callHook = nil
		if _, err := h.q.Submit(context.Background(), submission("t2", "after the change")); err != nil {
			t.Errorf("Submit: %v", err)
		}
		h.sender.startErr = errors.New("the shim refused")
	}

	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, setModel("opus")); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Assert
	if !hasRecord(h, "error", opCall, "the hold deferred behind the shim call was not delivered") {
		t.Fatalf("records = %+v, want the failed delivery on disk", h.log.Records())
	}
	if held := h.db.hold("t2"); held.Tombstone != nil {
		t.Fatalf("hold of t2 = %+v, want it standing for the next attempt", held)
	}
}

func TestAParkedBubblePromptIsDeliveredToItsAgentWhileATurnRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.standOnOpening = true
	var during Disposition
	h.sender.startHook = func() {
		h.sender.startHook = nil
		during, _ = h.q.Submit(context.Background(), bubbleSubmission("b1"))
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "go")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if during.Delivered {
		t.Fatalf("bubble prompt during the call = %+v, want it parked", during)
	}
	if len(h.sender.agents) != 1 || h.sender.agents[0].GetValue() != "sub-agent" {
		t.Fatalf("agents = %v, want the parked prompt delivered to its agent once the call settled", h.sender.agents)
	}
	if held := h.db.hold("b1"); held.Tombstone == nil || held.Tombstone.Kind != tombstoneDelivered {
		t.Fatalf("hold of b1 = %+v, want it retired as delivered", held)
	}
}

func TestAParkedBubblePromptItsAgentRefusesStaysHeldAndIsRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.standOnOpening = true
	h.sender.promptErr = errors.New("no such agent")
	h.sender.startHook = func() {
		h.sender.startHook = nil
		if _, err := h.q.Submit(context.Background(), bubbleSubmission("b1")); err != nil {
			t.Errorf("Submit: %v", err)
		}
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "go")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if !hasRecord(h, "error", opCall, "a prompt parked behind the shim call was not delivered to its agent; it stays held") {
		t.Fatalf("records = %+v, want the refusal on disk", h.log.Records())
	}
	if held := h.db.hold("b1"); held.Tombstone != nil {
		t.Fatalf("hold of b1 = %+v, want it standing", held)
	}
}

func TestAPromptParkedBehindAnActIsJudgedAgainstTheTurnThePopOpens(t *testing.T) {
	// Arrange: a held model change and a held prompt wait for the running turn.
	h := newHarness(t)
	h.watcher.standOnOpening = true
	running(t, h, "running-turn", "the running work")
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, setModel("opus")); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	h.sender.callHook = func(string) {
		h.sender.callHook = nil
		if _, err := h.q.Submit(context.Background(), submission("t2", "during the change")); err != nil {
			t.Errorf("Submit: %v", err)
		}
	}

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	h.q.waitForClassifications()

	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want t1, in order, after the act", started)
	}
	if held := h.db.hold("t2"); held.Tombstone != nil || held.Classification == nil {
		t.Fatalf("hold of t2 = %+v, want it standing and judged against t1", held)
	}
}

func TestAJoinIsBlockedWhileAShimCallStands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).call = &shimCall{what: "start_turn", turn: "t9", opensTurn: true}
	h.q.mu.Unlock()

	// Act
	reason, blocked := h.q.joinBlocked(theWorkspace, wsm.HeldPrompt{Turn: "t1"}, "running-turn")

	// Assert
	if !blocked || reason != "a delivery to the shim (start_turn) is in flight, so the prompt waits for the running turn to end" {
		t.Fatalf("joinBlocked = (%q, %v), want it blocked by the call", reason, blocked)
	}
}

func TestAVerdictAgainstAPromptBeingDeliveredInterruptsItRatherThanFoldingIntoIt(t *testing.T) {
	// Arrange: t1 stands, claimed by the call delivering it; t2 was judged to
	// interrupt it.
	h := newHarness(t)
	heldByDrainLease(t, h, "t1")
	heldByDrainLease(t, h, "t2")
	ahead := h.db.hold("t1")
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).call = &shimCall{what: "start_turn", turn: "t1", opensTurn: true, holds: []ids.TurnID{"t1"}}
	h.q.mu.Unlock()
	log, err := h.q.logger(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("logger: %v", err)
	}
	verdict := wsm.Classification{Arm: wsm.ArmInterject, Reason: "it changes the work", At: instant}

	// Act
	h.q.settleQueued(context.Background(), submission("t2", "held"), ahead, h.q.contentEpoch(theWorkspace, "t2"), verdict, classifier.RouteInterrupt, log)

	// Assert
	if h.db.coalescences != 0 {
		t.Fatalf("coalescences = %d, want none into a prompt already on its way", h.db.coalescences)
	}
	if killed := h.sender.killed(); len(killed) != 1 || killed[0] != "t1" {
		t.Fatalf("killed = %v, want the turn being started interrupted", killed)
	}
}

func TestTheVerdictLockIsFreeWhileAnInterjectionsInterruptIsSent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	free, observed := true, false
	h.sender.callHook = func(call string) {
		if call == "KillTurn" {
			observed = true
			free = free && verdictsFree(h)
		}
	}

	// Act
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it changes the work"})

	// Assert
	if !observed || !free {
		t.Fatalf("interrupt sent = %v, verdict lock free = %v; want it sent with the lock free", observed, free)
	}
}

func TestTheDeliveryLockIsFreeWhileAReleasesInterruptIsSent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	free, observed := true, false
	h.sender.callHook = func(call string) {
		if call == "KillTurn" {
			observed = true
			free = free && lockFree(h)
		}
	}

	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}

	// Assert
	if !observed || !free {
		t.Fatalf("interrupt sent = %v, delivery lock free = %v; want it sent with the lock free", observed, free)
	}
}
