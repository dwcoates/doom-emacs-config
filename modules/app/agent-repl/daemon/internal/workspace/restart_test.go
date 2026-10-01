package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/rollout"
	"claude-repld/internal/wsm"
)

func TestRestartDelegatesToTheRelaunchEngine(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1", false); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	f.rollout.awaitRelaunch(t)
	if got := f.rollout.relaunchCalls(); len(got) != 1 || got[0].Reason != rollout.ReasonRestartVerb {
		t.Fatalf("relaunches = %+v, want one restart-verb relaunch", got)
	}
}

func TestRestartWithoutForceKillsNoTurn(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1", false); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	if len(f.shim.killedTurns) != 0 {
		t.Fatalf("killed turns = %+v, want none without force", f.shim.killedTurns)
	}
}

func TestRestartWithForceEndsTheRunningTurnFirst(t *testing.T) {
	// Arrange: the relaunch engine waits for freeness, so a wedged turn must go
	// first or the wait never completes.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1", true); err != nil {
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
	if err := f.verbs.Restart(context.Background(), "w1", true); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	if len(f.shim.killedTurns) != 1 || f.shim.killedTurns[0].CommandedBy != nil {
		t.Fatalf("killed turns = %+v, want one kill with no commanded_by", f.shim.killedTurns)
	}
}

func TestRestartWithForceAndNoTurnKillsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1", true); err != nil {
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
	if err := f.verbs.Restart(context.Background(), "w1", false); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	f.rollout.awaitRelaunch(t)
	if got := f.rollout.reloadCalls(); len(got) != 1 {
		t.Fatalf("webapp reloads = %v, want exactly one", got)
	}
}

// TestRestartRecordsARelaunchFailure covers where a relaunch failure now goes.
// The verb ACCEPTS and the engine runs behind it -- it waits for freeness,
// forever if need be -- so nobody is waiting on the failure to be returned;
// it is recorded at ERROR instead, and the relaunch's own fault carries it.
func TestRestartRecordsARelaunchFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = errors.New("the shim would not stand down")

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1", false); err != nil {
		t.Fatalf("Restart: %v", err)
	}

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
	if err := f.verbs.Restart(context.Background(), "w1", true); err != nil {
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
	err := f.verbs.Restart(context.Background(), "w1", false)

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
	if err := f.verbs.Restart(context.Background(), "w1", false); err != nil {
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

func TestRestartSurfacesAForcedTurnKillFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	f.shim.killTurnErr = errors.New("no turn open")

	// Act.
	err := f.verbs.Restart(context.Background(), "w1", true)

	// Assert.
	if err == nil {
		t.Fatal("Restart(force) = nil error, want the turn-kill failure surfaced")
	}
	if len(f.rollout.relaunches) != 0 {
		t.Fatalf("relaunches = %+v, want none after a failed force-end", f.rollout.relaunches)
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
	if err := f.verbs.Restart(context.Background(), "w1", false); err != nil {
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
	if err := f.verbs.Restart(context.Background(), "w1", false); err != nil {
		t.Fatalf("Restart: %v", err)
	}

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
