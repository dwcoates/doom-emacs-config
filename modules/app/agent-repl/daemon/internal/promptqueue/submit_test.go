package promptqueue

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/wsm"
)

func TestSubmitRefusesAnUnspecifiedOrigin(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sub := submission("t1", "hello")
	sub.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED
	// Act
	_, err := h.q.Submit(context.Background(), sub)
	// Assert
	if err == nil {
		t.Fatal("a submission with no origin must be refused")
	}
	if len(h.sender.started()) != 0 {
		t.Fatal("nothing may reach the shim before the origin is checked")
	}
}

func TestSubmitRefusesUnderTheMergeLease(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyRefuse)
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if !errors.Is(err, ErrMerging) {
		t.Fatalf("err = %v, want ErrMerging", err)
	}
	if got.RefusedArm != ArmMerging {
		t.Fatalf("arm = %q, want %q", got.RefusedArm, ArmMerging)
	}
}

func TestSubmitHoldsUnderTheDrainLease(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !got.Parked() || got.Held == nil || *got.Held != wsm.HoldShutdown {
		t.Fatalf("disposition = %+v, want a shutdown hold", got)
	}
}

func TestSubmitNotesEveryDrainRefusedSubmission(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if h.drain.count() != 1 {
		t.Fatalf("drain refusals noted = %d, want 1", h.drain.count())
	}
}

func TestSubmitRefusesADrainLeaseWithNoScheduleInForce(t *testing.T) {
	// Arrange: a drain lease stands but nothing is scheduled — the hold has no
	// schedule to wait on, and the queue never invents one.
	h := newHarness(t)
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err == nil {
		t.Fatal("a shutdown hold with no schedule in force must be surfaced")
	}
}

func TestSubmitHoldsUnderARestartLeaseAsABuildRefresh(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderRestart, wsm.PolicyHold)
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Held == nil || *got.Held != wsm.HoldBuildRefresh {
		t.Fatalf("disposition = %+v, want a build-refresh hold", got)
	}
}

func TestSubmitRoutesAParkedLeaseToTheResolutionAgent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyParked)
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "fix the conflict this way"))
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !got.Delivered {
		t.Fatalf("disposition = %+v, want a delivered disposition", got)
	}
	if len(h.parked) != 1 {
		t.Fatalf("parked routes = %d, want 1", len(h.parked))
	}
	if len(h.sender.started()) != 0 {
		t.Fatal("a parked submission never opens a turn of its own")
	}
}

func TestSubmitSurfacesAParkedRouteFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyParked)
	h.parkedErr = errors.New("the resolution agent is gone")
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "guidance"))
	// Assert
	if err == nil {
		t.Fatal("a parked route's failure must be surfaced, never swallowed")
	}
}

func TestSubmitRefusesAWorkspaceWithNoSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if !errors.Is(err, ErrNoSession) {
		t.Fatalf("err = %v, want ErrNoSession", err)
	}
}

func TestSubmitDeliversWhenNothingIsRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !got.Delivered {
		t.Fatalf("disposition = %+v, want delivered", got)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the minted turn", started)
	}
}

func TestSubmitHoldsWhenATurnIsRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.running("running-turn")
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "a follow-up"))
	h.q.waitForClassifications()
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !got.Parked() {
		t.Fatalf("disposition = %+v, want a hold", got)
	}
	if len(h.sender.started()) != 0 {
		t.Fatal("a held prompt never reaches the shim")
	}
}

func TestSubmitAnswersAHoldWithTheClassifyingVerdict(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.running("running-turn")
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "a follow-up"))
	h.q.waitForClassifications()
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Classification == nil || got.Classification.Arm != wsm.ArmClassifying {
		t.Fatalf("disposition = %+v, want the classifying verdict", got)
	}
}

func TestSubmitMirrorsAHeldPromptToTheTrayOnly(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.running("running-turn")
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if len(h.feed.mirrored()) != 0 {
		t.Fatal("a held prompt earns no feed row")
	}
	if len(h.holds.last()) != 1 {
		t.Fatalf("tray = %v, want the held prompt", h.holds.last())
	}
}

func TestSubmitRefusesALeasePolicyItDoesNotKnow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.LeasePolicy(99))
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err == nil {
		t.Fatal("an unknown lease policy must be surfaced rather than defaulted")
	}
}

// TestSubmitRevivesAParkedWorkspaceRatherThanRefusingIt pins the hibernation
// revival: a session hibernated by the idle sweep is idle, not dead, and the
// prompt is what brings it back.
func TestSubmitRevivesAParkedWorkspaceRatherThanRefusingIt(t *testing.T) {
	// Arrange: no session, and the revival makes one.
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() { h.noSession = false }

	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "wake up"))

	// Assert
	if err != nil {
		t.Fatalf("Submit onto a parked workspace = %v, want the revival to deliver it", err)
	}
	if !got.Delivered {
		t.Fatalf("disposition = %+v, want the prompt delivered after the revival", got)
	}
	if h.revivals != 1 {
		t.Fatalf("revivals = %d, want exactly one", h.revivals)
	}
}

// TestSubmitSurfacesAFailedRevival is the other edge: a workspace that will not
// come back fails the submission loudly rather than refusing it as "no
// session", which would read as an ordinary state.
func TestSubmitSurfacesAFailedRevival(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveErr = errors.New("the shim would not spawn")

	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "wake up"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the shim would not spawn") {
		t.Fatalf("Submit = %v, want the revival's own failure surfaced", err)
	}
}
