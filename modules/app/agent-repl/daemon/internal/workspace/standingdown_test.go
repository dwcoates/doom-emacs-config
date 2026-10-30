package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/shimclient"
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

// The bring-up of a workspace this daemon does not serve. Measured 2026-10-30
// 17:49:27: the outgoing daemon of a long handover took a prompt for a
// workspace its successor had created, revived it, and adopted the
// successor's live shim.

// TestStartBringsUpNoShimForAWorkspaceTheSuccessorServes is the guard at its
// source: neither the lock nor the socket is probed, and nothing is spawned.
func TestStartBringsUpNoShimForAWorkspaceTheSuccessorServes(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.standing = StandingTransferringAway

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	asRefusal(t, err, ArmTransferringAway)
	if f.probes != 0 || f.supervisor.spawnAttempts != 0 {
		t.Fatalf("probes = %d, spawn attempts = %d, want 0 and 0: the successor's shim is not this daemon's to adopt", f.probes, f.supervisor.spawnAttempts)
	}
}

// TestStartRecordsTheSuccessorsWorkspaceRefusalAtInfo is its canonical record:
// the refusal is the daemon's decision, so INFO with the standing named.
func TestStartRecordsTheSuccessorsWorkspaceRefusalAtInfo(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.standing = StandingTransferringAway

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	var found bool
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" {
			t.Fatalf("the refused bring-up recorded %q at ERROR", r.Message)
		}
		if r.Message == "no shim is brought up: the workspace is the successor's, not this daemon's" {
			found = r.Level == "info" && r.Context["standing"] == "transferring_away" && r.Context["workspace"] == string(ws.ID)
		}
	}
	if !found {
		t.Fatal("no INFO record naming the transferring_away workspace")
	}
}

// TestStartRefusesWhenTheServingStandingCannotBeRead covers the standing's
// error: nothing is brought up on a standing the daemon could not determine,
// and the failure is recorded.
func TestStartRefusesWhenTheServingStandingCannotBeRead(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.standingErr = errors.New("unrecognized standing")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil || f.supervisor.spawnAttempts != 0 {
		t.Fatalf("Start = %v, spawn attempts = %d, want an error and no spawn", err, f.supervisor.spawnAttempts)
	}
	var logged bool
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" && r.Message == "could not read the serving standing; nothing is brought up" {
			logged = true
		}
	}
	if !logged {
		t.Fatal("no ERROR record of the unreadable standing")
	}
}
