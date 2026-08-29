package workspace

import (
	"context"
	"errors"
	"testing"

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
	if len(f.rollout.relaunches) != 1 || f.rollout.relaunches[0].Reason != rollout.ReasonRestartVerb {
		t.Fatalf("relaunches = %+v, want one restart-verb relaunch", f.rollout.relaunches)
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
	if len(f.rollout.reloads) != 1 {
		t.Fatalf("webapp reloads = %v, want exactly one", f.rollout.reloads)
	}
}

func TestRestartSurfacesARelaunchFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = errors.New("the shim would not stand down")

	// Act.
	err := f.verbs.Restart(context.Background(), "w1", false)

	// Assert.
	if err == nil {
		t.Fatal("Restart() = nil error, want the relaunch failure surfaced")
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
