// A DECLINE IS A STOP (permdecline.go), proved from both directions: the user
// answering "no" to a parked question, and the user typing a prompt instead of
// answering one. Both must end the turn and leave nothing travelling to the
// agent; a grant must do neither.
package sessioncontroller

import (
	"context"
	"errors"
	"runtime"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
)

// liveManagerWithParkedPermission brings a session up (the submit is what
// creates the controller a stop can reach) and parks a permission waiter on it,
// returning the manager, the last fake client, and the parked answer channel.
//
// The waiter is parked through the registry directly rather than through
// permHandler: the handler's own behavior is covered by the permission-item
// tests, and what these need is the rendezvous a real canUseTool leaves behind.
func liveManagerWithParkedPermission(t *testing.T, permissionID string) (*Manager, *fakeClient, <-chan *corev1.PermissionResponse) {
	t.Helper()
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.SubmitPrompt(context.Background(), "ws", "fe-bringup", "hello", "", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt (bring-up): %v", err)
	}
	ch, release := m.reg.await(permissionID, "ws")
	t.Cleanup(release)
	return m, lastClient(), ch
}

// awaitDecision reads the parked waiter's response, failing the test when none
// arrives. The channel is buffered by the registry, so a resolved waiter is
// already readable and this never sleeps on the happy path.
func awaitDecision(t *testing.T, ch <-chan *corev1.PermissionResponse) *corev1.PermissionResponse {
	t.Helper()
	select {
	case resp := <-ch:
		return resp
	case <-time.After(time.Second):
		t.Fatal("the parked permission was never resolved")
		return nil
	}
}

func TestDeclinedPermissionStopsTheTurn(t *testing.T) {
	// Arrange — a live session with a question parked on it.
	m, fc, ch := liveManagerWithParkedPermission(t, "perm-1")
	before := fc.interruptCount()

	// Act — the user answers no.
	if err := m.AnswerPermission(context.Background(), "ws", "fe-decline", "perm-1", false, "no thanks", nil); err != nil {
		t.Fatalf("AnswerPermission(deny): %v", err)
	}

	// Assert — the turn was stopped, under the frontend command's own id.
	if got := fc.interruptCount() - before; got != 1 {
		t.Fatalf("interrupts issued by the decline = %d, want 1 (a decline means what an interrupt means)", got)
	}
	if origins := fc.interruptOriginIDs(); len(origins) == 0 || origins[len(origins)-1] != "fe-decline" {
		t.Fatalf("interrupt origin ids = %v, want the decline command's own id last", origins)
	}
	if decision := awaitDecision(t, ch).GetDecision(); decision != corev1.PermissionDecision_PERMISSION_DECISION_DENY {
		t.Fatalf("decision = %v, want DENY", decision)
	}
}

func TestDeclinedPermissionRecordsItsReason(t *testing.T) {
	// Arrange.
	m, _, ch := liveManagerWithParkedPermission(t, "perm-2")

	// Act.
	if err := m.AnswerPermission(context.Background(), "ws", "fe-decline", "perm-2", false, "not that directory", nil); err != nil {
		t.Fatalf("AnswerPermission(deny): %v", err)
	}

	// Assert — the reason survives on the resolution the frontends render.
	if msg := awaitDecision(t, ch).GetDenyMessage(); msg != "not that directory" {
		t.Fatalf("deny message = %q, want the user's recorded reason", msg)
	}
}

func TestGrantedPermissionStopsNothing(t *testing.T) {
	// Arrange — the other answer, which leaves the turn running.
	m, fc, ch := liveManagerWithParkedPermission(t, "perm-3")
	before := fc.interruptCount()

	// Act.
	if err := m.AnswerPermission(context.Background(), "ws", "fe-allow", "perm-3", true, "", nil); err != nil {
		t.Fatalf("AnswerPermission(allow): %v", err)
	}

	// Assert.
	if got := fc.interruptCount() - before; got != 0 {
		t.Fatalf("interrupts issued by a grant = %d, want 0", got)
	}
	if decision := awaitDecision(t, ch).GetDecision(); decision != corev1.PermissionDecision_PERMISSION_DECISION_ALLOW {
		t.Fatalf("decision = %v, want ALLOW", decision)
	}
}

func TestDeclineOfAnAlreadyResolvedPermissionStopsNothing(t *testing.T) {
	// Arrange — a stale or duplicate answer: nothing is parked under this id.
	m, fc, _ := liveManagerWithParkedPermission(t, "perm-4")
	before := fc.interruptCount()

	// Act.
	err := m.AnswerPermission(context.Background(), "ws", "fe-decline", "ghost", false, "", nil)

	// Assert — loud, and no turn ended on the strength of an answer that
	// released nothing.
	if err == nil || !strings.Contains(err.Error(), "no pending permission") {
		t.Fatalf("AnswerPermission(stale deny) = %v, want a loud stale-answer error", err)
	}
	if got := fc.interruptCount() - before; got != 0 {
		t.Fatalf("interrupts issued by a stale decline = %d, want 0", got)
	}
}

func TestDeclineReportsAnUndeliverableStop(t *testing.T) {
	// Arrange — the shim cannot be reached, so the decline cannot be delivered.
	m, fc, ch := liveManagerWithParkedPermission(t, "perm-5")
	fc.interruptOutcome = corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED

	// Act.
	err := m.AnswerPermission(context.Background(), "ws", "fe-decline", "perm-5", false, "", nil)

	// Assert — the failure is the caller's, never swallowed into a silent
	// "declined" the user would read as a stop that happened.
	if !errors.Is(err, errclass.ErrInterruptUndelivered) {
		t.Fatalf("AnswerPermission(deny) over a failed stop = %v, want ErrInterruptUndelivered", err)
	}
	if decision := awaitDecision(t, ch).GetDecision(); decision != corev1.PermissionDecision_PERMISSION_DECISION_DENY {
		t.Fatalf("decision = %v, want DENY (the daemon still recorded the answer)", decision)
	}
}

func TestPromptSubmittedOverAParkedPermissionDeclinesIt(t *testing.T) {
	// Arrange — a question parked, and the user types instead of answering.
	m, fc, ch := liveManagerWithParkedPermission(t, "perm-6")
	before := fc.interruptCount()

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "fe-prompt", "do this instead", "", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert — declined, stopped, and the prompt still went through.
	resp := awaitDecision(t, ch)
	if resp.GetDecision() != corev1.PermissionDecision_PERMISSION_DECISION_DENY {
		t.Fatalf("decision = %v, want DENY", resp.GetDecision())
	}
	if resp.GetDenyMessage() != promptSupersededDenyMessage {
		t.Fatalf("deny message = %q, want the superseded-by-prompt reason", resp.GetDenyMessage())
	}
	if got := fc.interruptCount() - before; got != 1 {
		t.Fatalf("interrupts issued by the superseding prompt = %d, want 1", got)
	}
}

func TestPromptSubmittedOverAParkedPermissionRunsWhenTheStoppedTurnEnds(t *testing.T) {
	// Arrange — the question is parked inside a live turn, which is the only
	// state it can be parked in, so the superseding prompt necessarily lands
	// behind the turn its own decline just stopped.
	m, fc, _ := liveManagerWithParkedPermission(t, "perm-9")
	sent := len(fc.promptTexts())

	// Act — the prompt, then the end of the turn the decline stopped.
	if err := m.SubmitPrompt(context.Background(), "ws", "fe-prompt", "do this instead", "", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	queued := queuedTexts(t, m, "ws")
	d, err := m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	m.onTurnBoundary(d, false, m.now())

	// Assert — held as the paused queue's one deliverable, then delivered.
	if len(queued) != 1 || queued[0] != "do this instead" {
		t.Fatalf("queued prompts after the decline = %v, want the user's prompt held behind the stopped turn", queued)
	}
	waitForPromptText(t, fc, sent, "do this instead")
}

// queuedTexts returns the texts the workspace's queue holds, read under the
// manager mutex (the deliver and classify goroutines mutate entries under it).
func queuedTexts(t *testing.T, m *Manager, workspace string) []string {
	t.Helper()
	d, err := m.existing(workspace)
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	out := make([]string, 0, len(d.queue.entries))
	for _, e := range d.queue.entries {
		out = append(out, e.text)
	}
	return out
}

// waitForPromptText waits for the client to receive text as a prompt beyond the
// first `sent` it had already received. Delivery runs on its own goroutine off
// the turn boundary, so the wait is on the observable itself rather than on a
// duration.
func waitForPromptText(t *testing.T, fc *fakeClient, sent int, text string) {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	for {
		for _, got := range fc.promptTexts()[sent:] {
			if got == text {
				return
			}
		}
		if time.Now().After(deadline) {
			t.Fatalf("forwarded prompts = %v, want %q delivered once the stopped turn ended", fc.promptTexts(), text)
		}
		runtime.Gosched()
	}
}

func TestPromptSubmittedWithNothingParkedStopsNothing(t *testing.T) {
	// Arrange — the ordinary case: an unconditional stop here would interrupt
	// every prompt the user ever sends.
	m, lastClient := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "fe-1", "hello", "", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if got := lastClient().interruptCount(); got != 0 {
		t.Fatalf("interrupts issued by an ordinary prompt = %d, want 0", got)
	}
}

func TestPromptOverAParkedPermissionIsRefusedWhenTheStopFails(t *testing.T) {
	// Arrange — the decline's stop cannot be delivered.
	m, fc, _ := liveManagerWithParkedPermission(t, "perm-7")
	fc.interruptOutcome = corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED
	sent := len(fc.promptTexts())

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "fe-prompt", "do this instead", "", testPromptOrigin)

	// Assert — the prompt is NOT submitted past a stop that did not happen: it
	// would land behind a turn the user believes they ended.
	if err == nil {
		t.Fatal("SubmitPrompt over an undeliverable decline must fail loudly")
	}
	if got := len(fc.promptTexts()) - sent; got != 0 {
		t.Fatalf("prompts forwarded after a failed decline = %d, want 0", got)
	}
}

func TestPromptOverSeveralParkedPermissionsDeclinesAllOfThemAndStopsOnce(t *testing.T) {
	// Arrange — two concurrent questions, one prompt.
	m, fc, first := liveManagerWithParkedPermission(t, "perm-8a")
	second, release := m.reg.await("perm-8b", "ws")
	defer release()
	before := fc.interruptCount()

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "fe-prompt", "never mind", "", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert — both released, one stop (a turn cannot be ended twice).
	for name, ch := range map[string]<-chan *corev1.PermissionResponse{"perm-8a": first, "perm-8b": second} {
		if decision := awaitDecision(t, ch).GetDecision(); decision != corev1.PermissionDecision_PERMISSION_DECISION_DENY {
			t.Errorf("%s decision = %v, want DENY", name, decision)
		}
	}
	if got := fc.interruptCount() - before; got != 1 {
		t.Fatalf("interrupts issued for two declines = %d, want 1", got)
	}
}

func TestInterruptReleasesTheWorkspacesParkedPermissions(t *testing.T) {
	// Arrange — a question parked when a typed stop lands. The shim force-denies
	// it on its own side, so a daemon that kept the rendezvous would show the
	// question as still pending forever.
	m, _, ch := liveManagerWithParkedPermission(t, "perm-10")

	// Act — the ordinary user-commanded stop, not a decline.
	if err := m.Interrupt(context.Background(), "ws", "fe-interrupt"); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert — released, and recorded as declined by the stop.
	resp := awaitDecision(t, ch)
	if resp.GetDecision() != corev1.PermissionDecision_PERMISSION_DECISION_DENY {
		t.Fatalf("decision = %v, want DENY", resp.GetDecision())
	}
	if resp.GetDenyMessage() != stoppedDenyMessage {
		t.Fatalf("deny message = %q, want the stopped-by-the-user reason", resp.GetDenyMessage())
	}
	if pending := m.reg.idsForWorkspace("ws"); len(pending) != 0 {
		t.Fatalf("pending permissions after the stop = %v, want none", pending)
	}
}

func TestInterruptLeavesAnotherWorkspacesParkedPermissionAlone(t *testing.T) {
	// Arrange — one workspace's stop must never answer another's question.
	m, _, _ := liveManagerWithParkedPermission(t, "perm-11")
	_, release := m.reg.await("perm-elsewhere", "other-ws")
	defer release()

	// Act.
	if err := m.Interrupt(context.Background(), "ws", "fe-interrupt"); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert.
	if pending := m.reg.idsForWorkspace("other-ws"); len(pending) != 1 {
		t.Fatalf("other workspace's pending permissions = %v, want its own question untouched", pending)
	}
}

func TestDeclinedPermissionSendsNoAnswerToTheShim(t *testing.T) {
	// Arrange — the handler is what decides what reaches the wire; a decline's
	// only delivery is the stop, so it must answer with nothing.
	ph, reg, push := newTestPermHandler()
	req := &corev1.PermissionRequest{RequestId: "r-decline", ToolName: "Bash"}

	// Act.
	done := make(chan *corev1.PermissionResponse, 1)
	go func() { done <- ph.HandlePermission("s1", req) }()
	waitForPermWaiter(reg, "ws", "r-decline")
	if err := reg.answerDecline("r-decline", "no"); err != nil {
		t.Fatalf("answerDecline: %v", err)
	}

	// Assert — nothing to send, and the denial still recorded for the frontends.
	if resp := <-done; resp != nil {
		t.Fatalf("HandlePermission returned %v for a decline, want nil (the stop is the delivery)", resp)
	}
	if msg := lastPermissionDenyMessage(push, "r-decline"); msg != "no" {
		t.Fatalf("recorded deny message = %q, want %q", msg, "no")
	}
}
