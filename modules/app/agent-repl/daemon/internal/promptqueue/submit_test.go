package promptqueue

import (
	"context"
	"errors"
	"reflect"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

func TestQueueStateTransitionsRecordTheirBeforeAndAfter(t *testing.T) {
	tests := []struct {
		name   string
		state  string
		before any
		after  any
		act    func(*harness, dlog.Logger) func()
	}{
		{
			name: "a bring-up enters flight", state: "bring_ups", before: 0, after: 1,
			act: func(h *harness, log dlog.Logger) func() {
				h.q.noteBringUp("ws-1", 1, log)
				return func() { h.q.noteBringUp("ws-1", -1, log) }
			},
		},
		{
			name: "a bring-up leaves flight", state: "bring_ups", before: 1, after: 0,
			act: func(h *harness, log dlog.Logger) func() {
				h.q.state("ws-1").bringUps = 1
				h.q.noteBringUp("ws-1", -1, log)
				return func() {}
			},
		},
		{
			name: "a background revival starts", state: "reviving", before: false, after: true,
			act: func(h *harness, log dlog.Logger) func() {
				entered := make(chan struct{})
				release := make(chan struct{})
				h.noSession = true
				h.reviveHook = func() {
					close(entered)
					<-release
					h.noSession = false
				}
				h.q.reviveInBackground(context.Background(), "ws-1", log)
				<-entered
				return func() { close(release); h.waitRevivals() }
			},
		},
		{
			name: "a background revival finishes", state: "reviving", before: true, after: false,
			act: func(h *harness, log dlog.Logger) func() {
				h.noSession = true
				h.reviveHook = func() { h.noSession = false }
				h.q.reviveInBackground(context.Background(), "ws-1", log)
				h.waitRevivals()
				return func() {}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			log, err := h.q.logger(context.Background(), "ws-1")
			if err != nil {
				t.Fatalf("logger: %v", err)
			}
			beforeRecords := len(h.log.Records())

			// Act.
			cleanup := tt.act(h, log)
			defer cleanup()

			// Assert.
			for _, record := range h.log.Records()[beforeRecords:] {
				if record.Level == "debug" && record.Operation == "daemon.promptqueue.state_transition" &&
					record.Context["state"] == tt.state && reflect.DeepEqual(record.Context["before"], tt.before) &&
					reflect.DeepEqual(record.Context["after"], tt.after) {
					return
				}
			}
			t.Fatalf("records = %+v, want %s before=%v after=%v", h.log.Records()[beforeRecords:], tt.state, tt.before, tt.after)
		})
	}
}

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

func TestSubmitRefusesUnderARetiredParkedMergeLease(t *testing.T) {
	// Arrange: a lease row an older build wrote parked.
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyParked)
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if !errors.Is(err, ErrMerging) || got.RefusedArm != ArmMerging {
		t.Fatalf("Submit = (%+v, %v), want the merging refusal", got, err)
	}
	if len(h.sender.started()) != 0 {
		t.Fatal("a submission under a retired parked lease reached the shim")
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

func TestSubmitRefusesAWorkspaceWithNoSessionAndNoRevivalWired(t *testing.T) {
	// Arrange
	h := newHarnessWithoutRevival(t)
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

	// Assert: the rpc answers AT ONCE under the revival-pending hold -- the
	// bring-up gates on the shim's diagnostics and is never awaited inside the
	// request -- and the revival then delivers the prompt.
	if err != nil {
		t.Fatalf("Submit onto a parked workspace = %v, want it held pending the revival", err)
	}
	if got.Held == nil || *got.Held != wsm.HoldReconnect {
		t.Fatalf("disposition = %+v, want the revival-pending hold", got)
	}
	h.waitRevivals()
	if h.revivals != 1 {
		t.Fatalf("revivals = %d, want exactly one", h.revivals)
	}
	if len(h.sender.started()) != 1 {
		t.Fatalf("started turns = %d, want the revived prompt delivered", len(h.sender.started()))
	}
}

// TestRevivingReportsTheRevivalAcrossItsBringUp pins that the idle sweep's
// revival answer is raised while the revival's bring-up runs.
func TestRevivingReportsTheRevivalAcrossItsBringUp(t *testing.T) {
	// Arrange: a bring-up the test holds open.
	h := newHarness(t)
	h.noSession = true
	entered := make(chan struct{})
	release := make(chan struct{})
	h.reviveHook = func() { close(entered); <-release }
	t.Cleanup(func() { close(release); h.waitRevivals() })
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	<-entered

	// Act
	got := h.q.Reviving(theWorkspace)

	// Assert
	if !got {
		t.Fatal("Reviving = false during the bring-up, want true")
	}
}

// TestRevivingReportsTheRevivalUntilItsPromptIsDelivered pins that the answer
// stays raised while the revived prompt is being handed to the new shim: the
// session's record still reads idle there, and only the turn the delivery
// starts claims it.
func TestRevivingReportsTheRevivalUntilItsPromptIsDelivered(t *testing.T) {
	// Arrange: the revival makes a session, and the delivery observes the flag.
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() { h.noSession = false }
	var duringDelivery bool
	h.sender.startHook = func() { duringDelivery = h.q.Reviving(theWorkspace) }

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.waitRevivals()

	// Assert
	if !duringDelivery {
		t.Fatal("Reviving = false while the revived prompt was delivered, want true")
	}
}

// TestRevivingIsLoweredOnceTheRevivalHasDelivered pins that a finished revival
// no longer defers the sweep.
func TestRevivingIsLoweredOnceTheRevivalHasDelivered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() { h.noSession = false }
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.waitRevivals()

	// Act
	got := h.q.Reviving(theWorkspace)

	// Assert
	if got {
		t.Fatal("Reviving = true after the revival delivered, want false")
	}
}

// TestSubmitAnswersARevivalAtOnceRatherThanAwaitingTheBringUp is the rpc-shape
// half: the minted turn comes back before the revival has run at all.
func TestSubmitAnswersARevivalAtOnceRatherThanAwaitingTheBringUp(t *testing.T) {
	// Arrange: a bring-up that never finishes while the test looks at the
	// answer, exactly as a shim withholding its diagnostics does.
	h := newHarness(t)
	h.noSession = true
	release := make(chan struct{})
	h.reviveHook = func() { <-release }
	t.Cleanup(func() { close(release); h.waitRevivals() })

	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "wake up"))

	// Assert
	if err != nil {
		t.Fatalf("Submit during a bring-up = %v, want an answer", err)
	}
	if got.Held == nil || *got.Held != wsm.HoldReconnect {
		t.Fatalf("disposition = %+v, want the revival-pending hold", got)
	}
}

// TestARevivalPendingHoldRaisesTheRostersTurnAtAcceptance pins that the roster
// takes the accepted turn when the rpc mints it, not when the revival delivers
// it: between the shim's SessionStarted and the hold's release the roster
// otherwise reads a live idle session and paints `ready` over a workspace whose
// prompt the daemon has already taken.
func TestARevivalPendingHoldRaisesTheRostersTurnAtAcceptance(t *testing.T) {
	// Arrange: a bring-up that has not finished while the test looks.
	h := newHarness(t)
	h.noSession = true
	release := make(chan struct{})
	h.reviveHook = func() { <-release }
	t.Cleanup(func() { close(release); h.waitRevivals() })

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit during a bring-up = %v, want an answer", err)
	}

	// Assert: the roster holds a prompt turn before any delivery happened.
	turns := h.sidebar.rosterTurns()
	if len(turns) != 1 || turns[0] == nil {
		t.Fatalf("roster turns = %+v, want exactly one accepted prompt turn at acceptance", turns)
	}
	if turns[0].Act != footer.ActPrompt {
		t.Fatalf("roster turn act = %d, want a prompt", turns[0].Act)
	}
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started turns = %d, want none while the bring-up still runs", len(got))
	}
}

// TestAFailedRevivalClearsTheRostersTurn pins the exit: a failed revival takes
// the roster's turn down, so the row falls back to the route's own fault arm
// rather than standing at `submitting` for a prompt that is waiting, not
// running.
func TestAFailedRevivalClearsTheRostersTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveErr = errors.New("the shim would not spawn")

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit = %v, want the submission held pending the revival", err)
	}
	h.waitRevivals()

	// Assert: accepted, then cleared, in that order.
	turns := h.sidebar.rosterTurns()
	if len(turns) != 2 || turns[0] == nil || turns[1] != nil {
		t.Fatalf("roster turns = %+v, want the accepted turn followed by its clearing", turns)
	}
}

// TestSubmitSurfacesAFailedRevival is the other edge: a workspace that will not
// come back answers the submission with the revival-pending hold, off the
// request path, and the failure is surfaced by the revival's own record.
func TestSubmitSurfacesAFailedRevival(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveErr = errors.New("the shim would not spawn")

	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "wake up"))
	h.waitRevivals()

	// Assert
	if err != nil {
		t.Fatalf("Submit = %v, want the submission held pending the revival", err)
	}
	if got.Held == nil || *got.Held != wsm.HoldReconnect {
		t.Fatalf("disposition = %+v, want the revival-pending hold", got)
	}
}

// TestAFailedRevivalKeepsItsReconnectHolds pins daemon_hold.proto's settled
// exit for HeldPromptReconnectHold: "A FAILED BRING-UP NEVER DROPS THESE
// ENTRIES." The prompt waits for whatever brings a session up next.
func TestAFailedRevivalKeepsItsReconnectHolds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveErr = errors.New("the shim would not spawn")

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit = %v, want the submission held pending the revival", err)
	}
	h.waitRevivals()

	// Assert
	standing, err := h.db.HeldPrompts(context.Background(), "ws-1")
	if err != nil {
		t.Fatalf("reading the standing holds: %v", err)
	}
	if len(standing) != 1 || standing[0].Hold == nil || *standing[0].Hold != wsm.HoldReconnect {
		t.Fatalf("standing holds = %+v, want t1 still held under the reconnect hold", standing)
	}
	if retired := h.db.retired("t1"); retired != nil {
		t.Fatalf("tombstone = %+v, want none: a failed bring-up drops nothing", retired)
	}
}

// TestAFailedRevivalRecordsTheHoldsItLeavesStanding pins that the kept holds
// are LOUD: the record names how many prompts wait and why.
func TestAFailedRevivalRecordsTheHoldsItLeavesStanding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveErr = errors.New("the shim would not spawn")

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit = %v, want the submission held pending the revival", err)
	}
	h.waitRevivals()

	// Assert
	for _, rec := range h.log.Records() {
		if rec.Level == dlog.LevelWarn && rec.Operation == opSubmit && rec.Context["held"] == 1 {
			return
		}
	}
	t.Fatalf("records = %+v, want a WARN naming the one prompt the failed revival leaves held", h.log.Records())
}

// unroutableSurfaces is dlog.Surfaces whose per-workspace sink never opens, so
// a test can drive the one condition production hits when a workspace's
// directory is a scratch path or a deleted worktree.
type unroutableSurfaces struct {
	*dlog.TestSurfaces
}

func (unroutableSurfaces) Workspace(string) (dlog.Logger, error) {
	return nil, errors.New("the workspace owns no durable log sink")
}

func (s unroutableSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	return s.TestSurfaces.Global().With(dlog.Context{dlog.KeyUnroutableWorkspace: dir})
}

// TestSubmitSurvivesAWorkspaceThatOwnsNoLogSink pins that A PROMPT IS NEVER
// LOST OVER ITS OWN LOGGING. Resolving a named workspace's sink is a TOTAL
// function: a directory that cannot host one routes the records to the central
// sink, and the submission goes through.
func TestSubmitSurvivesAWorkspaceThatOwnsNoLogSink(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.q.deps.Log = unroutableSurfaces{h.log}

	// Act.
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert.
	if err != nil {
		t.Fatalf("Submit() = %v, want the prompt delivered despite the unroutable sink", err)
	}
	if len(h.sender.started()) != 1 {
		t.Fatalf("started = %d prompts, want the one submitted", len(h.sender.started()))
	}
}

// TestSubmitStillRefusesAWorkspaceTheStateStoreWillNotName pins the error that
// REMAINS: an unknown workspace has nothing to submit to.
func TestSubmitStillRefusesAWorkspaceTheStateStoreWillNotName(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	sub := submission("t1", "hello")
	sub.WS = "a-workspace-nobody-registered"

	// Act.
	_, err := h.q.Submit(context.Background(), sub)

	// Assert.
	if err == nil {
		t.Fatal("Submit(unknown workspace) = nil error, want the refusal surfaced")
	}
}

// ---- the cold gate refuses by its own name -------------------------------
//
// Owner's report, 2026-09-14: three prompts to a workspace parked at its cold
// gate were answered `no_session`, about a session that was up and serving.
// A gate is a fact of its own and it is named as one.

func TestSubmitRefusesAColdGatedWorkspaceByTheGatesOwnName(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.coldGate = "the conversation is cold at 101600 context tokens"
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if !errors.Is(err, ErrColdGate) {
		t.Fatalf("err = %v, want ErrColdGate", err)
	}
}

func TestSubmitsColdGateRefusalIsNotTheNoSessionRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.coldGate = "the conversation is cold at 101600 context tokens"
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if errors.Is(err, ErrNoSession) {
		t.Fatalf("err = %v, want a cold gate refusal and NOT no_session", err)
	}
}

func TestSubmitsColdGateRefusalCarriesTheGatesOwnSentence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.coldGate = "the conversation is cold at 101600 context tokens"
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	var refusal *ColdGateRefusal
	if !errors.As(err, &refusal) || refusal.Detail != h.coldGate {
		t.Fatalf("err = %v, want the gate's own sentence carried", err)
	}
}

func TestSubmitIsUnaffectedWhenNoColdGateStands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err != nil || !got.Delivered {
		t.Fatalf("Submit = (%+v, %v), want an ordinary delivery", got, err)
	}
}

func TestSubmitDoesNotReviveAColdGatedWorkspace(t *testing.T) {
	// Arrange. A gated workspace's shim is UP; a revival would be a bring-up
	// of a session that already exists and is refusing on purpose.
	h := newHarness(t)
	h.coldGate = "the conversation is cold at 101600 context tokens"
	// Act
	_, _ = h.q.Submit(context.Background(), submission("t1", "hello"))
	h.waitRevivals()
	// Assert
	if h.revivals != 0 {
		t.Fatalf("revivals = %d, want none for a gated workspace", h.revivals)
	}
}

// cutRecordedBeforeTheWatcherKnows records a context cut as runContextCut does
// in its first step, before the watcher has been told of the turn.
func cutRecordedBeforeTheWatcherKnows(h *harness) {
	h.beginCut("cut-1", conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	h.watcher.idle()
}

func TestSubmitStartsNothingBesideACutTheWatcherHasNotYetSeen(t *testing.T) {
	// Arrange
	h := newHarness(t)
	cutRecordedBeforeTheWatcherKnows(h)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want the prompt held behind the recorded act", started)
	}
}

func TestSubmitHoldsBehindACutTheWatcherHasNotYetSeenAsUninterruptible(t *testing.T) {
	// Arrange
	h := newHarness(t)
	cutRecordedBeforeTheWatcherKnows(h)
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "a follow-up"))
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if got.Classification == nil || got.Classification.Arm != wsm.ArmUninterruptibleTurn {
		t.Fatalf("classification = %+v, want uninterruptible_turn", got.Classification)
	}
}

// TestSubmitOfTheTurnAlreadyInFlightStartsNothing pins the re-drive of a claim
// whose original DID reach the shim: the retry comes back under the same turn
// id, finds that turn running, and is answered as the delivery the original
// was -- never held behind itself, never started a second time -- with the
// repeat recorded at ERROR.
func TestSubmitOfTheTurnAlreadyInFlightStartsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "t1", "hello")
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !got.Delivered || got.Parked() {
		t.Fatalf("disposition = %+v, want delivered and not held", got)
	}
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want nothing started again", started)
	}
	held, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(held) != 0 {
		t.Fatalf("held = %+v, want the turn never queued behind itself", held)
	}
	if !logged(h.log.Records(), "error", opSubmit, repeatedStartMessage) {
		t.Fatalf("the repeated start was not recorded at error: %v", h.log.Records())
	}
}

// repeatedStartMessage is the record a submission of the turn in flight
// writes.
const repeatedStartMessage = "a submission repeated the turn already in flight; it is answered as the delivery the original was and nothing is started again"

// TestARevivedSessionThatHasSinceDepartedIsNotAFailedRevival is the race the
// integration suite caught: the shim a revival brought up died and was reaped
// before the revival read the workspace's client back.
func TestARevivedSessionThatHasSinceDepartedIsNotAFailedRevival(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() {
		h.noSession = false
		h.clientReaped = true
		h.watcher.depart(sessionwatcher.Departure{Cause: "exit"})
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit = %v, want the submission held pending the revival", err)
	}
	h.waitRevivals()

	// Assert
	if logged(h.log.Records(), "error", opSubmit, "the revival reported success but the workspace still has no session") {
		t.Fatalf("a revival whose session came up and died was recorded as a failed revival")
	}
	if !logged(h.log.Records(), "info", opSubmit, "the revived session came up and has since departed; its departure decides what follows") {
		t.Fatalf("records = %+v, want the departure named at INFO", h.log.Records())
	}
}

func TestARevivedSessionThatHasSinceDepartedKeepsItsHolds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() {
		h.noSession = false
		h.clientReaped = true
		h.watcher.depart(sessionwatcher.Departure{Cause: "exit"})
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit = %v, want the submission held pending the revival", err)
	}
	h.waitRevivals()

	// Assert
	standing, err := h.db.HeldPrompts(context.Background(), "ws-1")
	if err != nil {
		t.Fatalf("reading the standing holds: %v", err)
	}
	if len(standing) != 1 {
		t.Fatalf("standing holds = %+v, want the prompt still held for the next bring-up", standing)
	}
}

func TestARevivalThatLeavesNoSessionAtAllIsRecordedAtError(t *testing.T) {
	// Arrange: the revival answers success and nothing serves the workspace.
	h := newHarness(t)
	h.noSession = true

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit = %v, want the submission held pending the revival", err)
	}
	h.waitRevivals()

	// Assert
	if !logged(h.log.Records(), "error", opSubmit, "the revival reported success but the workspace still has no session") {
		t.Fatalf("records = %+v, want the lying revival at ERROR", h.log.Records())
	}
}

// ---- a re-driven refusal is the re-driver's to record ---------------------
//
// The held-prompt ingress retries a refused entry on a backoff for as long as
// the standing condition lasts, and records the refusal itself once per entry
// and refusal kind. Recorded at the arm's own level here as well, one merge
// produced the same WARN on every retry of every waiting prompt.

func TestSubmitRecordsAStandingRefusalAtItsLevelOrDebugForARedrive(t *testing.T) {
	const (
		merging   = "the submission is refused: a merge is in flight"
		coldGate  = "the session is parked at its cold gate, so the submission is refused by the gate's own name"
		noSession = "the workspace has no session to submit to"
		noWatcher = "the workspace has no session watcher"
	)
	tests := []struct {
		name      string
		arrange   func(t *testing.T) *harness
		redrive   bool
		message   string
		wantLevel string
	}{
		{name: "a live merge refusal warns", arrange: mergingHarness, message: merging, wantLevel: "warn"},
		{name: "a re-driven merge refusal is debug", arrange: mergingHarness, redrive: true, message: merging, wantLevel: "debug"},
		{name: "a live cold gate refusal is info", arrange: coldGatedHarness, message: coldGate, wantLevel: "info"},
		{name: "a re-driven cold gate refusal is debug", arrange: coldGatedHarness, redrive: true, message: coldGate, wantLevel: "debug"},
		{name: "a live no-session refusal warns", arrange: sessionlessHarness, message: noSession, wantLevel: "warn"},
		{name: "a re-driven no-session refusal is debug", arrange: sessionlessHarness, redrive: true, message: noSession, wantLevel: "debug"},
		{name: "a live missing-watcher refusal warns", arrange: watcherlessHarness, message: noWatcher, wantLevel: "warn"},
		{name: "a re-driven missing-watcher refusal is debug", arrange: watcherlessHarness, redrive: true, message: noWatcher, wantLevel: "debug"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := tc.arrange(t)
			ctx := context.Background()
			if tc.redrive {
				ctx = WithRedrive(ctx)
			}

			// Act
			_, err := h.q.Submit(ctx, submission("t1", "hello"))

			// Assert
			if err == nil {
				t.Fatal("Submit = nil error, want the refusal")
			}
			if !logged(h.log.Records(), tc.wantLevel, opSubmit, tc.message) {
				t.Fatalf("records = %+v, want %q at %s", h.log.Records(), tc.message, tc.wantLevel)
			}
		})
	}
}

func mergingHarness(t *testing.T) *harness {
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyRefuse)
	return h
}

func coldGatedHarness(t *testing.T) *harness {
	h := newHarness(t)
	h.coldGate = "the conversation is cold"
	return h
}

func sessionlessHarness(t *testing.T) *harness {
	h := newHarnessWithoutRevival(t)
	h.noSession = true
	return h
}

// watcherlessHarness has a shim client and no session watcher: the reaped-shim
// window the second no-session arm answers.
func watcherlessHarness(t *testing.T) *harness {
	h := newHarness(t)
	deps := h.q.deps
	deps.Watcher = func(ids.WorkspaceID) (Watcher, bool) { return nil, false }
	q, err := newQueue(deps)
	if err != nil {
		t.Fatalf("newQueue: %v", err)
	}
	h.q = q
	return h
}
