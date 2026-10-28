package workspace

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/rollout"
	"agent-repld/internal/wsm"
)

func TestRestartDelegatesToTheRelaunchEngine(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	f.rollout.awaitRelaunch(t)
	if got := f.rollout.relaunchCalls(); len(got) != 1 || got[0].Reason != rollout.ReasonRestartVerb {
		t.Fatalf("relaunches = %+v, want one restart-verb relaunch", got)
	}
}

func TestRestartForceEndsTheRunningTurnFirst(t *testing.T) {
	// Arrange: a restart is immediate, and the running turn is ended with no
	// gentleness before the bounce.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	if len(f.shim.killedTurns) != 1 || !f.shim.killedTurns[0].Force {
		t.Fatalf("killed turns = %+v, want one forced kill", f.shim.killedTurns)
	}
}

// TestRestartKillStatesNoCommand pins that a restart's kill states no HOW:
// whether it is a user stop is not settled, so it relays none.
func TestRestartKillStatesNoCommand(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	if len(f.shim.killedTurns) != 1 || f.shim.killedTurns[0].CommandedBy != nil {
		t.Fatalf("killed turns = %+v, want one kill with no commanded_by", f.shim.killedTurns)
	}
}

func TestRestartWithNoTurnKillsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	if len(f.shim.killedTurns) != 0 {
		t.Fatalf("killed turns = %+v, want none", f.shim.killedTurns)
	}
}

func TestRestartPushesTheWebappReloadAfterTheRelaunch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	f.rollout.awaitRelaunch(t)
	if got := f.rollout.reloadCalls(); len(got) != 1 {
		t.Fatalf("webapp reloads = %v, want exactly one", got)
	}
}

// TestRestartRecordsARelaunchFailure covers the record a relaunch failure
// leaves: ERROR, and no reload.
func TestRestartRecordsARelaunchFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = errors.New("the shim would not stand down")

	// Act.
	_ = f.verbs.Restart(context.Background(), "w1")

	// Assert.
	//
	// THE RECORD IS AWAITED, NOT READ ONCE. The fake fires its relaunch signal
	// from INSIDE RelaunchShim, so `awaitRelaunch` returns while the engine's
	// goroutine is still on its way to the log line this test is about; a
	// single read of the buffer saw an empty one whenever that goroutine lost
	// the race.
	f.rollout.awaitRelaunch(t)
	awaitRecord(t, f, "error", opRestart)
	if got := f.rollout.reloadCalls(); len(got) != 0 {
		t.Fatalf("webapp reloads = %v, want none after a failed relaunch", got)
	}
}

func TestAHandedAcrossRestartIsRecordedAsAnOutcome(t *testing.T) {
	// Arrange: the restart raced a handover's move and was carried with it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = bounce.ErrHandedAcross

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert: the fake signals only once the completion has run.
	f.rollout.awaitRelaunch(t)
	const want = "the restart was handed to the daemon the workspace moved to, which runs it after its adoption"
	found := false
	for _, r := range f.log.logger.Records() {
		if r.Operation != opRestart {
			continue
		}
		if r.Level == dlog.LevelError || r.Level == dlog.LevelWarn {
			t.Fatalf("a handed-across restart was recorded at %s: %q", r.Level, r.Message)
		}
		if r.Level == dlog.LevelInfo && r.Message == want {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want %q at INFO", f.log.logger.Records(), want)
	}
}

func TestARestartOfAWorkspaceWhoseMoveSealedIsRefusedAsMovedAway(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.refuseErr = bounce.ErrMovedAway

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert: the refusal carries the moved-away cause the transport answers
	// as transferring_away, and it is not recorded as a failure.
	if !errors.Is(err, bounce.ErrMovedAway) {
		t.Fatalf("Restart = %v, want ErrMovedAway", err)
	}
	for _, r := range f.log.logger.Records() {
		if r.Operation == opRestart && (r.Level == dlog.LevelError || r.Level == dlog.LevelWarn) {
			t.Fatalf("a moved-away restart was recorded at %s: %q", r.Level, r.Message)
		}
	}
}

func TestAnUnregisteredRestartIsRecordedAsAnOutcome(t *testing.T) {
	// Arrange: the shim departs before the registered restart is taken.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = bounce.ErrUnregistered

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert: the fake signals only once the completion has run.
	f.rollout.awaitRelaunch(t)
	const want = "the restart was unregistered: the shim it would replace departed and nothing is left to replace"
	found := false
	for _, r := range f.log.logger.Records() {
		if r.Operation != opRestart {
			continue
		}
		if r.Level == dlog.LevelError || r.Level == dlog.LevelWarn {
			t.Fatalf("an unregistered restart was recorded at %s: %q", r.Level, r.Message)
		}
		if r.Level == dlog.LevelInfo && r.Message == want {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want %q at INFO", f.log.logger.Records(), want)
	}
	if got := f.rollout.reloadCalls(); len(got) != 0 {
		t.Fatalf("webapp reloads = %v, want none for a restart that replaced nothing", got)
	}
}

// A FAILED TURN KILL NEVER STOPS THE RESTART: it is recorded at ERROR and the
// forced bounce, which ends the turn with the shim, proceeds.
func TestRestartProceedsPastAFailedTurnKill(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	f.shim.killTurnErr = errors.New("no turn open")

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Restart = %v, want the restart accepted past the failed kill", err)
	}
	f.rollout.awaitRelaunch(t)
	awaitRecord(t, f, "error", opRestart)
}

// A TURN KILL THE VENDOR NEVER ANSWERS is bounded: the restart proceeds to its
// forced bounce once the bound ends the call.
func TestRestartProceedsPastATurnKillThatHangs(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	f.shim.killTurnHangs = true
	f.verbs.(*verbs).restartStopBound = time.Millisecond

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Restart = %v, want the restart accepted past the hung kill", err)
	}
	f.rollout.awaitRelaunch(t)
	if got := f.rollout.relaunchCalls(); len(got) != 1 {
		t.Fatalf("relaunches = %+v, want the forced bounce after the bounded kill", got)
	}
}

func TestRestartAsksForAForcedBounce(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	f.rollout.awaitRelaunch(t)
	if got := f.rollout.relaunchCalls(); len(got) != 1 || !got[0].Force {
		t.Fatalf("relaunches = %+v, want one forced bounce", got)
	}
}

func TestRestartEndsTheVendorStartRunFirst(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.vendorRunning = true

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	if got := f.fleet.vendorCancels; len(got) != 1 || got[0] != "w1" {
		t.Fatalf("vendor-start cancellations = %v, want one for w1", got)
	}
}

// A WORKSPACE WITH NO SHIM UP IS RESTARTED BY STARTING ITS SESSION at once
// (owner ruling, 2026-10-28); there is nothing for a bounce to replace.
func TestRestartOfAWorkspaceWithNoShimStartsItsSession(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	if err != nil || len(f.fleet.started) != 1 || f.fleet.started[0] != "w1" {
		t.Fatalf("Restart = %v after starts %v, want w1's session started", err, f.fleet.started)
	}
	if got := f.rollout.relaunchCalls(); len(got) != 0 {
		t.Fatalf("relaunches = %+v, want no bounce of a shim that is not up", got)
	}
}

func TestARestartWhoseStartFailsAnswersRelaunchFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false
	f.fleet.startErr = errFake

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	asRefusal(t, err, "relaunch_failed")
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opRestart}); !ok {
		t.Fatalf("records = %+v, want the failed start at ERROR", f.log.logger.Records())
	}
}

// TestRestartEmptiesNoFeed pins the scope of the feed reset: a restart resumes
// the SAME conversation, so its rows are still the conversation's and replaying
// onto them is an upsert. Only a BIND — the one verb that changes which
// conversation a workspace runs — empties the feed.
func TestRestartEmptiesNoFeed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}
	f.rollout.awaitRelaunch(t)

	// Assert.
	if len(f.feed.resets) != 0 {
		t.Fatalf("feed resets = %+v, want none: a restart keeps the conversation's rows", f.feed.resets)
	}
}

// The registry keeps a deferred bounce's Done for the rerun, so a deferral
// told to the restart is its contract broken: recorded at ERROR, and never
// followed by the reload a finished restart pushes.
func TestADeferralToldToTheRestartIsRecordedAsAContractBreach(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = bounce.ErrDeferred

	// Act.
	_ = f.verbs.Restart(context.Background(), "w1")

	// Assert: the fake signals only once the completion has run.
	f.rollout.awaitRelaunch(t)
	const want = "the bounce registry told the restart a deferral; it owes only the rerun's outcome"
	found := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opRestart && r.Level == dlog.LevelError && r.Message == want {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want %q at ERROR", f.log.logger.Records(), want)
	}
	if got := f.rollout.reloadCalls(); len(got) != 0 {
		t.Fatalf("webapp reloads = %v, want none for a restart that did not finish", got)
	}
}

// TestARestartAnswersAFailedRelaunchAsRelaunchFailed pins the answer: the
// restart's caller is told the relaunch failed, with its cause (owner ruling,
// 2026-10-08), never a success for a restart that did not happen.
func TestARestartAnswersAFailedRelaunchAsRelaunchFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = errors.New("could not take the restart-pending hold")

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Arm != "relaunch_failed" || refusal.Fields["detail"] != "could not take the restart-pending hold" {
		t.Fatalf("Restart = %v, want the relaunch_failed refusal carrying the cause", err)
	}
}

// TestARestartAnswersADeferralAsRelaunchFailed covers the contract breach: a
// deferral is no restart that happened.
func TestARestartAnswersADeferralAsRelaunchFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = bounce.ErrDeferred

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	if refusal, ok := AsRefusal(err); !ok || refusal.Arm != "relaunch_failed" {
		t.Fatalf("Restart = %v, want the relaunch_failed refusal", err)
	}
}

// TestARestartAnswersOnlyOnceItHappened pins the order: the reload a finished
// restart pushes has been pushed by the time the restart answers.
func TestARestartAnswersOnlyOnceItHappened(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Restart = %v, want nil", err)
	}
	if got := f.rollout.reloadCalls(); len(got) != 1 {
		t.Fatalf("webapp reloads when the restart answered = %v, want the one a finished restart pushes", got)
	}
}

// TestARestartWhoseCallerLeavesAnswersTheCallersEnd covers a caller that
// stops waiting: the answer is the context's end, recorded at ERROR.
func TestARestartWhoseCallerLeavesAnswersTheCallersEnd(t *testing.T) {
	// Arrange: a bounce that never finishes.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.hold = make(chan struct{})
	t.Cleanup(func() { close(f.rollout.hold) })
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := f.verbs.Restart(ctx, "w1")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Restart = %v, want the caller's end", err)
	}
	awaitRecord(t, f, "error", opRestart)
}

// TestAnUnregisteredRestartAnswersSuccess covers nothing left to replace: the
// restart's purpose already holds.
func TestAnUnregisteredRestartAnswersSuccess(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = bounce.ErrUnregistered

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Restart = %v, want nil", err)
	}
}

// TestARestartSuspendsTheWorkspacesMergeBeforeTheBounceAndResumesItAfter pins
// the order (owner ruling, 2026-10-08): the merge lets go before the shim is
// replaced and is taken back once the restart finished.
func TestARestartSuspendsTheWorkspacesMergeBeforeTheBounceAndResumesItAfter(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Restart = %v, want nil", err)
	}
	if got := f.merge.steps; len(got) != 2 || got[0] != "suspend:w1" || got[1] != "resume:w1" {
		t.Fatalf("merge steps = %v, want the suspension then the take-back", got)
	}
}

// TestARestartTakesTheMergeBackAfterAFailedRelaunch pins the take-back on
// every outcome: a failed relaunch leaves no merge suspended for good.
func TestARestartTakesTheMergeBackAfterAFailedRelaunch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = errors.New("the prelaunch failed")

	// Act.
	_ = f.verbs.Restart(context.Background(), "w1")

	// Assert.
	if got := f.merge.steps; len(got) != 2 || got[1] != "resume:w1" {
		t.Fatalf("merge steps = %v, want the merge taken back", got)
	}
}

// TestARestartWhoseMergeWillNotLetGoIsRefused covers the merge that does not
// reach a stopping point: the restart bounces nothing, says why, and takes the
// merge back.
func TestARestartWhoseMergeWillNotLetGoIsRefused(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.merge.suspendErr = errors.New("the merge did not let go within 7s")

	// Act.
	err := f.verbs.Restart(context.Background(), "w1")

	// Assert.
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Arm != "relaunch_failed" {
		t.Fatalf("Restart = %v, want the relaunch_failed refusal", err)
	}
	if got := f.rollout.relaunchCalls(); len(got) != 0 {
		t.Fatalf("relaunches = %+v, want none when the merge would not let go", got)
	}
	if got := f.merge.steps; len(got) != 2 || got[1] != "resume:w1" {
		t.Fatalf("merge steps = %v, want the merge taken back", got)
	}
	awaitRecord(t, f, "error", opRestart)
}

// TestRelaunchFailedCarriesItsDetail pins the one refusal's shape every
// relaunch_failed site shares.
func TestRelaunchFailedCarriesItsDetail(t *testing.T) {
	// Arrange: nothing beyond the inputs.

	// Act.
	refusal := relaunchFailed("w1", "the shim died")

	// Assert.
	if refusal.Rpc != "RestartWorkspace" || refusal.Arm != "relaunch_failed" || refusal.Fields["detail"] != "the shim died" {
		t.Fatalf("refusal = %+v, want RestartWorkspace's relaunch_failed carrying the detail", refusal)
	}
}
