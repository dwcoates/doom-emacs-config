package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/shimclient"
)

// The bring-up on a daemon that has begun standing down. Measured at realtest
// 2026-09-13T18:32:16: an announcement landed between the shutdown's latch and
// the process's exit, the bring-up spawned anyway, the supervisor refused the
// spawn, and the register recorded three loud records plus a workspace fault
// for a state the daemon had decided on purpose.

// TestStartSpawnsNothingWhileTheDaemonIsStandingDown is the fix at its source:
// the bring-up reads the latch before it probes or spawns.
func TestStartSpawnsNothingWhileTheDaemonIsStandingDown(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.supervisor.BeginStandDown()

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := f.supervisor.spawnAttempts; got != 0 {
		t.Fatalf("spawn attempts = %d, want 0: a daemon that is leaving asks for no process", got)
	}
}

// TestStartRefusesWithTheSpawnFailedArmWhileTheDaemonIsStandingDown pins that
// the caller still gets the landed arm it already read for this state, so the
// transport's answer is unchanged by the earlier refusal.
func TestStartRefusesWithTheSpawnFailedArmWhileTheDaemonIsStandingDown(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.supervisor.BeginStandDown()

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	asRefusal(t, err, ArmSpawnFailed)
}

// TestStartCarriesTheStandingDownSentinelWhileTheDaemonIsStandingDown is what
// lets every relaying caller tell a departing daemon from a shim that would
// not come up.
func TestStartCarriesTheStandingDownSentinelWhileTheDaemonIsStandingDown(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.supervisor.BeginStandDown()

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if !errors.Is(err, shimclient.ErrStandingDown) {
		t.Fatalf("Start error = %v, want it to carry shimclient.ErrStandingDown", err)
	}
}

// TestStartOpensNoFaultWhileTheDaemonIsStandingDown is the record half:
// nothing is wrong with this workspace, and the successor revives it from the
// same record, so a `shim_start_failed` fault would be a standing lie.
func TestStartOpensNoFaultWhileTheDaemonIsStandingDown(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.supervisor.BeginStandDown()

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := len(f.db.dbFaults); got != 0 {
		t.Fatalf("faults opened = %d, want 0 for a bring-up refused by the daemon's own departure", got)
	}
}

// TestStartRecordsNoErrorWhileTheDaemonIsStandingDown is the level the finding
// is about: a refusal the daemon decided on purpose is INFO, never ERROR.
func TestStartRecordsNoErrorWhileTheDaemonIsStandingDown(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.supervisor.BeginStandDown()

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" {
			t.Fatalf("a bring-up refused by the daemon's own departure recorded %q at ERROR", r.Message)
		}
	}
}

// TestStartStillSpawnsWhenTheDaemonIsNotStandingDown is the guard's other
// half, and the reason it reads the latch rather than assuming it: an ordinary
// bring-up is untouched.
func TestStartStillSpawnsWhenTheDaemonIsNotStandingDown(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if got := f.supervisor.spawnAttempts; got != 1 {
		t.Fatalf("spawn attempts = %d, want exactly 1", got)
	}
}
