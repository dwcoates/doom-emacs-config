package sessioncontroller

import (
	"context"
	"errors"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// --- Manager.CancelDetachedAgents ------------------------------------------
//
// The command exists because an Interrupt structurally cannot reach detached
// work: it ends the TURN, and detached agents have outlived their turn by
// definition. These pin the routing, the settlement that follows a successful
// stop, and the refusals.

// cancelledOutcome is a shim verdict naming the tasks it stopped.
func cancelledOutcome(taskIDs ...string) *corev1.DetachedCancelOutcome {
	return &corev1.DetachedCancelOutcome{Outcome: &corev1.DetachedCancelOutcome_Cancelled{
		Cancelled: &corev1.DetachedAgentsCancelled{TaskIds: taskIDs},
	}}
}

// liveManagerWithDetachedAgent brings a session up, opens a bubble for one
// detached agent, and returns the manager alongside its client and pusher.
func liveManagerWithDetachedAgent(t *testing.T, taskID string) (*Manager, func() *fakeClient, *fakePusher) {
	t.Helper()
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ws", "test-request", "hello", "", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	d, err := m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	push, ok := d.consumer.push.(*fakePusher)
	if !ok {
		t.Fatalf("consumer pusher = %T, want *fakePusher", d.consumer.push)
	}
	if _, err := d.consumer.bubbles.observeTaskStarted(&corev1.TaskStarted{
		TaskId: taskID, Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_" + taskID, Description: "fan out",
	}, 10); err != nil {
		t.Fatalf("observeTaskStarted: %v", err)
	}
	return m, lastClient, push
}

func TestCancelDetachedAgentsRequiresALiveSession(t *testing.T) {
	// Arrange: no session behind the workspace.
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{}}, &fakeSpawner{})

	// Act
	_, err := m.CancelDetachedAgents(context.Background(), "ws", "fe-1")

	// Assert: refused rather than recovered. Detached agents live inside the
	// vendor process the shim holds, so no shim means nothing detached — and
	// spawning one to ask would create the very session it was meant to stop
	// work in.
	if err == nil {
		t.Fatal("cancel with no live session must error")
	}
}

func TestCancelDetachedAgentsCarriesTheCommandRequestID(t *testing.T) {
	// Arrange
	m, lastClient, _ := liveManagerWithDetachedAgent(t, "task_1")
	lastClient().detachedCancelOutcome = cancelledOutcome("task_1")

	// Act
	if _, err := m.CancelDetachedAgents(context.Background(), "ws", "fe-7"); err != nil {
		t.Fatalf("cancel: %v", err)
	}

	// Assert: the wire travels under a daemon-minted control id, so the
	// caller's own id is the only thing that joins the exchange to the click.
	if got := lastClient().detachedCancelOrigins; len(got) != 1 || got[0] != "fe-7" {
		t.Fatalf("cancel origins = %v, want [fe-7]", got)
	}
}

func TestCancelDetachedAgentsSettlesTheStoppedAgentsBubble(t *testing.T) {
	// Arrange
	m, lastClient, push := liveManagerWithDetachedAgent(t, "task_1")
	lastClient().detachedCancelOutcome = cancelledOutcome("task_1")

	// Act
	if _, err := m.CancelDetachedAgents(context.Background(), "ws", "fe-1"); err != nil {
		t.Fatalf("cancel: %v", err)
	}

	// Assert: the bubble reaches a terminal state on the ack, so the feed and
	// the footer stop showing live work the daemon has already stopped.
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.bubbles) == 0 {
		t.Fatal("cancel pushed no async delta; the agent's bubble is still rendering as live")
	}
	last := push.bubbles[len(push.bubbles)-1]
	if len(last.GetUpdates()) != 1 {
		t.Fatalf("updates = %d, want 1", len(last.GetUpdates()))
	}
	if last.GetUpdates()[0].GetLiveness().GetLiveness().GetSettled().GetKilled() == nil {
		t.Fatalf("settled arm = %+v, want killed", last.GetUpdates()[0].GetLiveness().GetLiveness().GetSettled().GetOutcome())
	}
}

func TestCancelDetachedAgentsSettlesNothingWhenNothingWasRunning(t *testing.T) {
	// Arrange: the shim answers that it had nothing detached.
	m, lastClient, push := liveManagerWithDetachedAgent(t, "task_1")
	lastClient().detachedCancelOutcome = &corev1.DetachedCancelOutcome{
		Outcome: &corev1.DetachedCancelOutcome_NothingRunning{NothingRunning: &corev1.NoDetachedAgentsRunning{}},
	}
	push.mu.Lock()
	before := len(push.bubbles)
	push.mu.Unlock()

	// Act
	outcome, err := m.CancelDetachedAgents(context.Background(), "ws", "fe-1")

	// Assert: the verdict is RETURNED, not interpreted here — and nothing is
	// settled, because no agent was stopped.
	if err != nil {
		t.Fatalf("cancel err = %v, want nil; the refusal ruling belongs to the command layer", err)
	}
	if outcome.GetNothingRunning() == nil {
		t.Fatalf("outcome = %+v, want nothing_running", outcome)
	}
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.bubbles) != before {
		t.Fatalf("async pushes = %d, want %d: a cancel that stopped nothing must settle nothing", len(push.bubbles), before)
	}
}

func TestCancelDetachedAgentsSettlesNothingWhenTheWireFailed(t *testing.T) {
	// Arrange: the shim never answered.
	m, lastClient, push := liveManagerWithDetachedAgent(t, "task_1")
	lastClient().detachedCancelErr = errors.New("shim link is down")
	push.mu.Lock()
	before := len(push.bubbles)
	push.mu.Unlock()

	// Act
	_, err := m.CancelDetachedAgents(context.Background(), "ws", "fe-1")

	// Assert: nothing is known to have stopped, so nothing is settled. A
	// bubble closed on a failed cancel would report stopped work that is still
	// running — the one error a user cannot correct by waiting.
	if err == nil {
		t.Fatal("cancel err = nil, want the wire failure")
	}
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.bubbles) != before {
		t.Fatalf("async pushes = %d, want %d", len(push.bubbles), before)
	}
}

func TestCancelDetachedAgentsSettlesOnlyTheAgentsTheShimStopped(t *testing.T) {
	// Arrange: two bubbles open, but the shim reports only one stopped —
	// the partial-stop case.
	m, lastClient, push := liveManagerWithDetachedAgent(t, "task_1")
	d, err := m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	if _, err := d.consumer.bubbles.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_2", Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_task_2",
	}, 10); err != nil {
		t.Fatalf("observeTaskStarted: %v", err)
	}
	lastClient().detachedCancelOutcome = cancelledOutcome("task_1")

	// Act
	if _, err := m.CancelDetachedAgents(context.Background(), "ws", "fe-1"); err != nil {
		t.Fatalf("cancel: %v", err)
	}

	// Assert: the agent that refused the stop is STILL RUNNING, and settling
	// its bubble would tell the user otherwise.
	push.mu.Lock()
	defer push.mu.Unlock()
	last := push.bubbles[len(push.bubbles)-1]
	if len(last.GetUpdates()) != 1 {
		t.Fatalf("updates = %d, want 1: only the stopped agent's bubble may settle", len(last.GetUpdates()))
	}
}

func TestDetachedCancelSettleUsesTheNewestSeenSeq(t *testing.T) {
	// Arrange: the cancel arrives on a control path, with no store event and
	// therefore no seq of its own.
	m, lastClient, push := liveManagerWithDetachedAgent(t, "task_1")
	lastClient().detachedCancelOutcome = cancelledOutcome("task_1")
	d, err := m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	want := d.consumer.newestRetainedSeq()

	// Act
	if _, err := m.CancelDetachedAgents(context.Background(), "ws", "fe-1"); err != nil {
		t.Fatalf("cancel: %v", err)
	}

	// Assert: through_seq is the client's replay cursor, so inventing a number
	// ahead of the stream would move it past events it never received.
	push.mu.Lock()
	defer push.mu.Unlock()
	if got := push.bubbles[len(push.bubbles)-1].GetThroughSeq(); got != want {
		t.Fatalf("through_seq = %d, want the newest seen %d", got, want)
	}
}

// A settlement the store refuses must not read as a settled bubble. This drives
// the refusal through the same verdict the cancel path builds.
func TestADetachedCancelSettlementRefusalIsNotAnUpdate(t *testing.T) {
	// Arrange: a verdict with no terminal status and no exit code, which
	// SettleAsyncBubble refuses rather than resolving to a confident "done".
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_1",
	}, 10); err != nil {
		t.Fatal(err)
	}

	// Act
	ups, err := s.settleCancelledTasks([]string{"task_1"}, frontend.AsyncVerdict{AtMs: 20})

	// Assert
	if err == nil {
		t.Fatal("an unresolvable settlement must be refused, not written")
	}
	if len(ups) != 0 {
		t.Fatalf("updates = %d, want 0: a refused settle must not push a settled-looking bubble", len(ups))
	}
}

// EVERY control-plane route goes through the shared helper. A hand-rolled
// settle push that classified its own gaps would pass its own tests and lose
// the failure card, so the shape is asserted rather than assumed: both callers
// produce the same push for the same input.
func TestBothControlSettleRoutesShareTheSameShape(t *testing.T) {
	// Arrange
	c := &consumer{workspace: "/ws", sessionID: "s1", logf: func(string, ...any) {}}
	ups := []*frontendv1.AsyncBubbleUpdate{{BubbleId: "b1"}}
	gap := &frontend.AsyncGapError{BubbleID: "b1", Detail: "the bubble is gone"}

	// Act
	window := c.windowSettlePush(ups, gap, "where")
	cancel := c.detachedCancelSettlePush(ups, gap, "where")

	// Assert
	if len(window.Updates) != len(cancel.Updates) || len(window.Faults) != len(cancel.Faults) {
		t.Fatalf("window push (updates=%d faults=%d) and cancel push (updates=%d faults=%d) diverged",
			len(window.Updates), len(window.Faults), len(cancel.Updates), len(cancel.Faults))
	}
}
