package sessioncontroller

import (
	"context"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/workspace/merge"
)

// newMergeResolveHarness brings a session up for "ws" with the merge lease
// HELD, which is the only state a merge prompt is admissible in.
func newMergeResolveHarness(t *testing.T) *queueHarness {
	t.Helper()
	h := newQueueHarness(t, nil)
	h.applier.mergeLeases = map[string]bool{"ws": true}
	return h
}

// submitHook makes the fake shim announce the prompt it was handed, so a test
// observes the submit rather than waiting for it.
func submitHook(h *queueHarness) <-chan string {
	got := make(chan string, 4)
	h.client.mu.Lock()
	h.client.onSubmit = func() { got <- "submitted" }
	h.client.mu.Unlock()
	return got
}

func TestMergeResolutionReturnsWhenItsOwnTurnEnds(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)

	// Act.
	go func() {
		done <- h.m.SubmitMergePromptAwaitingTurn(context.Background(), "ws", "req-1", "resolve it", "")
	}()
	<-submitted
	d := h.controller()
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})
	h.m.onTurnEvent(d, false, "t1", turnOutcome{})

	// Assert.
	if err := <-done; err != nil {
		t.Fatalf("SubmitMergePromptAwaitingTurn = %v, want nil", err)
	}
}

func TestMergeResolutionIgnoresAnotherTurnsEnd(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)
	go func() {
		done <- h.m.SubmitMergePromptAwaitingTurn(context.Background(), "ws", "req-1", "resolve it", "")
	}()
	<-submitted
	d := h.controller()
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})

	// Act — a different turn ends.
	h.m.onTurnEvent(d, false, "t2", turnOutcome{})

	// Assert — the wait stands until ITS turn ends.
	select {
	case err := <-done:
		t.Fatalf("SubmitMergePromptAwaitingTurn returned %v on another turn's end", err)
	default:
	}
	h.m.onTurnEvent(d, false, "t1", turnOutcome{})
	if err := <-done; err != nil {
		t.Fatalf("SubmitMergePromptAwaitingTurn = %v, want nil", err)
	}
}

func TestMergeResolutionIgnoresATurnThatStartedBeforeItArmed(t *testing.T) {
	// Arrange — the user's turn was interrupted for the lease, and its
	// TurnEnded is still in flight when the resolution prompt goes out.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)
	go func() {
		done <- h.m.SubmitMergePromptAwaitingTurn(context.Background(), "ws", "req-1", "resolve it", "")
	}()
	<-submitted
	d := h.controller()

	// Act — the older turn's end arrives with no start observed after arming.
	h.m.onTurnEvent(d, false, "t0", turnOutcome{})

	// Assert — it wakes nothing, and the resolution's own turn still does.
	select {
	case err := <-done:
		t.Fatalf("SubmitMergePromptAwaitingTurn returned %v on the interrupted turn's end", err)
	default:
	}
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})
	h.m.onTurnEvent(d, false, "t1", turnOutcome{})
	if err := <-done; err != nil {
		t.Fatalf("SubmitMergePromptAwaitingTurn = %v, want nil", err)
	}
}

func TestMergeResolutionWithNoLiveSessionIsAnError(t *testing.T) {
	// Arrange — nothing was ever brought up for this workspace.
	m := &Manager{logf: func(string, ...any) {}}

	// Act.
	err := m.SubmitMergePromptAwaitingTurn(context.Background(), "ws", "req-1", "resolve it", "")

	// Assert.
	if err == nil {
		t.Fatal("SubmitMergePromptAwaitingTurn with no live session = nil, want an error")
	}
	if !strings.Contains(err.Error(), "no live session") {
		t.Fatalf("error = %q, want it to name the absent session", err)
	}
}

func TestMergeResolutionSurfacesASubmitRefusal(t *testing.T) {
	// Arrange — the lease is NOT held, so the merge submit is refused.
	h := newQueueHarness(t, nil)

	// Act.
	err := h.m.SubmitMergePromptAwaitingTurn(context.Background(), "ws", "req-1", "resolve it", "")

	// Assert — the refusal is surfaced, and no waiter is left behind.
	if err == nil {
		t.Fatal("SubmitMergePromptAwaitingTurn with no lease = nil, want a refusal")
	}
	if !strings.Contains(err.Error(), "NO merge lease held") {
		t.Fatalf("error = %q, want the merge-lease refusal", err)
	}
	d := h.controller()
	h.m.mu.Lock()
	waiters := len(d.turnWaiters)
	h.m.mu.Unlock()
	if waiters != 0 {
		t.Fatalf("turn waiters after a refused submit = %d, want 0", waiters)
	}
}

func TestMergeResolutionTurnThatNeverEndsIsAnError(t *testing.T) {
	// Arrange — a resolution turn that never reports its end.
	h := newMergeResolveHarness(t)
	h.m.cfg.MergeResolutionTurnBound = 20 * time.Millisecond

	// Act.
	err := h.m.ResolveMergeConflict(context.Background(), merge.ConflictResolution{
		Workspace: "ws", RequestID: "req-1", ConflictCommit: "abc1234",
		SourceBranch: "feature/a", TargetDir: "/target",
	})

	// Assert — reported, never reported as a completed resolution.
	if err == nil {
		t.Fatal("ResolveMergeConflict() = nil, want the unfinished-turn error")
	}
	if !strings.Contains(err.Error(), "never reported its end") {
		t.Fatalf("error = %q, want it to name the turn that never ended", err)
	}
}

func TestResolveMergeConflictSubmitsThePromptAndCompletesOnItsTurn(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)

	// Act.
	go func() {
		done <- h.m.ResolveMergeConflict(context.Background(), merge.ConflictResolution{
			Workspace: "ws", RequestID: "req-1", ConflictCommit: "abc1234",
			SourceBranch: "feature/a", TargetDir: "/target",
		})
	}()
	<-submitted
	d := h.controller()
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})
	h.m.onTurnEvent(d, false, "t1", turnOutcome{})

	// Assert — the agent was told what to resolve, under a merge origin.
	if err := <-done; err != nil {
		t.Fatalf("ResolveMergeConflict() = %v, want nil", err)
	}
	h.client.mu.Lock()
	prompts := append([]string(nil), h.client.prompts...)
	origins := append([]string(nil), h.client.origins...)
	h.client.mu.Unlock()
	if len(prompts) != 1 || !strings.Contains(prompts[0], "abc1234") {
		t.Fatalf("prompts = %q, want one naming the conflicting commit", prompts)
	}
	if len(origins) != 1 || !strings.HasPrefix(origins[0], "merge:") {
		t.Fatalf("origins = %q, want a merge-shaped provenance", origins)
	}
}

func TestResolveMergeConflictRejectsAnIncompleteResolution(t *testing.T) {
	tests := []struct {
		name string
		res  merge.ConflictResolution
		want string
	}{
		{name: "no workspace", res: merge.ConflictResolution{RequestID: "req-1"}, want: "workspace"},
		{name: "no request id", res: merge.ConflictResolution{Workspace: "ws"}, want: "request id"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m := &Manager{logf: func(string, ...any) {}}

			// Act.
			err := m.ResolveMergeConflict(context.Background(), tc.res)

			// Assert.
			if err == nil {
				t.Fatalf("ResolveMergeConflict(%+v) = nil, want an error", tc.res)
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %q, want it to name the missing %s", err, tc.want)
			}
		})
	}
}

// --- test-failure resolution --------------------------------------------

func TestResolveMergeTestFailureSubmitsThePromptAndCompletesOnItsTurn(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)

	// Act.
	go func() {
		done <- h.m.ResolveMergeTestFailure(context.Background(), merge.TestFailureResolution{
			Workspace: "ws", RequestID: "req-1", FailingCommit: "abc1234",
			SourceBranch: "feature/a", TargetDir: "/target", FailureTail: "FAIL: agent-repl-suite",
		})
	}()
	<-submitted
	d := h.controller()
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})
	h.m.onTurnEvent(d, false, "t1", turnOutcome{})

	// Assert — the agent was told which commit broke what, under a merge origin.
	if err := <-done; err != nil {
		t.Fatalf("ResolveMergeTestFailure() = %v, want nil", err)
	}
	h.client.mu.Lock()
	prompts := append([]string(nil), h.client.prompts...)
	origins := append([]string(nil), h.client.origins...)
	h.client.mu.Unlock()
	if len(prompts) != 1 || !strings.Contains(prompts[0], "abc1234") || !strings.Contains(prompts[0], "FAIL: agent-repl-suite") {
		t.Fatalf("prompts = %q, want one naming the failing commit and its output", prompts)
	}
	if len(origins) != 1 || !strings.HasPrefix(origins[0], "merge:") {
		t.Fatalf("origins = %q, want a merge-shaped provenance", origins)
	}
}

func TestResolveMergeTestFailureRejectsAnIncompleteResolution(t *testing.T) {
	tests := []struct {
		name string
		res  merge.TestFailureResolution
		want string
	}{
		{name: "no workspace", res: merge.TestFailureResolution{RequestID: "req-1"}, want: "workspace"},
		{name: "no request id", res: merge.TestFailureResolution{Workspace: "ws"}, want: "request id"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m := &Manager{logf: func(string, ...any) {}}

			// Act.
			err := m.ResolveMergeTestFailure(context.Background(), tc.res)

			// Assert.
			if err == nil {
				t.Fatalf("ResolveMergeTestFailure(%+v) = nil, want an error", tc.res)
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %q, want it to name the missing %s", err, tc.want)
			}
		})
	}
}

func TestResolveMergeTestFailureTurnThatNeverEndsIsAnError(t *testing.T) {
	// Arrange — a fix turn that never reports its end.
	h := newMergeResolveHarness(t)
	h.m.cfg.MergeResolutionTurnBound = 20 * time.Millisecond

	// Act.
	err := h.m.ResolveMergeTestFailure(context.Background(), merge.TestFailureResolution{
		Workspace: "ws", RequestID: "req-1", FailingCommit: "abc1234",
		SourceBranch: "feature/a", TargetDir: "/target", FailureTail: "boom",
	})

	// Assert — reported, never reported as a completed fix.
	if err == nil {
		t.Fatal("ResolveMergeTestFailure() = nil, want the unfinished-turn error")
	}
	if !strings.Contains(err.Error(), "never reported its end") {
		t.Fatalf("error = %q, want it to name the turn that never ended", err)
	}
}

// --- a turn that ENDED BADLY ---------------------------------------------

// THE GUARANTEE: an errored turn is reported as an error, not as a completed
// action. "The turn ended" is not "the turn worked", and a merge action is a
// turn the run demanded: reporting an errored before-action as complete lands a
// cherry-pick plan the action was meant to change, and reporting an errored
// after-action as complete never reaches after_action_error at all.
func TestAMergeTurnThatEndsInErrorIsReportedAsAFailure(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)

	// Act — the turn ends, carrying its own failure.
	go func() {
		done <- h.m.RunMergeBeforeAction(context.Background(), merge.BeforeAction{
			Workspace: "ws", RequestID: "req-1", Prompt: "run the pre-merge checks",
		})
	}()
	<-submitted
	d := h.controller()
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})
	h.m.onTurnEvent(d, false, "t1", turnOutcome{isError: true, stopReason: "error_during_execution"})

	// Assert.
	err := <-done
	if err == nil {
		t.Fatal("RunMergeBeforeAction() = nil for a turn that ended in error: the failure was swallowed")
	}
	if !strings.Contains(err.Error(), "error_during_execution") {
		t.Fatalf("error = %q, want it to carry the turn's own stop reason", err)
	}
}

// The mirror edge: a turn that ends CLEANLY is still reported as complete, so
// the check above cannot have been bought by failing every action.
func TestAMergeTurnThatEndsCleanlyIsReportedAsComplete(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)

	// Act.
	go func() {
		done <- h.m.RunMergeBeforeAction(context.Background(), merge.BeforeAction{
			Workspace: "ws", RequestID: "req-1", Prompt: "run the pre-merge checks",
		})
	}()
	<-submitted
	d := h.controller()
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})
	h.m.onTurnEvent(d, false, "t1", turnOutcome{stopReason: "end_turn"})

	// Assert.
	if err := <-done; err != nil {
		t.Fatalf("RunMergeBeforeAction() = %v, want nil for a turn that ended cleanly", err)
	}
}

// --- the after-action ----------------------------------------------------

// THE GUARANTEE: the after-action's errored turn is reported as an error too.
// merge.Coordinator carries it onto the terminal merged status, so softening it
// here would leave that status blank about a nudge that never fired.
func TestAMergeAfterActionTurnThatEndsInErrorIsReportedAsAFailure(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)

	// Act.
	go func() {
		done <- h.m.RunMergeAfterAction(context.Background(), merge.AfterAction{
			Workspace: "ws", RequestID: "req-1", Prompt: "run the release checklist",
		})
	}()
	<-submitted
	d := h.controller()
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})
	h.m.onTurnEvent(d, false, "t1", turnOutcome{isError: true, stopReason: "error_during_execution"})

	// Assert.
	err := <-done
	if err == nil {
		t.Fatal("RunMergeAfterAction() = nil for a turn that ended in error: the failure was swallowed")
	}
	if !strings.Contains(err.Error(), "error_during_execution") {
		t.Fatalf("error = %q, want it to carry the turn's own stop reason", err)
	}
}

// The mirror edge: a clean after-action turn reports complete.
func TestAMergeAfterActionTurnThatEndsCleanlyIsReportedAsComplete(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)
	submitted := submitHook(h)
	done := make(chan error, 1)

	// Act.
	go func() {
		done <- h.m.RunMergeAfterAction(context.Background(), merge.AfterAction{
			Workspace: "ws", RequestID: "req-1", Prompt: "run the release checklist",
		})
	}()
	<-submitted
	d := h.controller()
	h.m.onTurnEvent(d, true, "t1", turnOutcome{})
	h.m.onTurnEvent(d, false, "t1", turnOutcome{stopReason: "end_turn"})

	// Assert.
	if err := <-done; err != nil {
		t.Fatalf("RunMergeAfterAction() = %v, want nil for a turn that ended cleanly", err)
	}
}

// An after-action with no prompt is refused before anything is submitted: a
// turn carrying nothing would take the user's session and say nothing in it.
func TestAMergeAfterActionWithNoPromptIsRefused(t *testing.T) {
	// Arrange.
	h := newMergeResolveHarness(t)

	// Act.
	err := h.m.RunMergeAfterAction(context.Background(), merge.AfterAction{Workspace: "ws", RequestID: "req-1"})

	// Assert.
	if err == nil {
		t.Fatal("RunMergeAfterAction() = nil for an action with no prompt, want a refusal")
	}
}
