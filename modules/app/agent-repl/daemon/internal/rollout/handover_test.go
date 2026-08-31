package rollout

import (
	"context"
	"testing"
	"time"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// runHandover starts Handover, waits for every workspace's transfer push, then
// expires the adoption windows so the flow can reach its exit. The clock is
// driven rather than waited out: nothing in this package sleeps.
func runHandover(t *testing.T, h *harness, workspaces int) error {
	t.Helper()
	done := make(chan error, 1)
	go func() { done <- h.c.Handover(context.Background()) }()
	for range workspaces {
		h.clock.awaitArmed(t, adoptionWindow)
	}
	h.clock.Fire(adoptionWindow)
	select {
	case err := <-done:
		return err
	case <-time.After(10 * time.Second):
		t.Fatalf("Handover never returned")
		return nil
	}
}

// The windows the harness wires, named so a test fires the one it means.
const (
	adoptionWindow = 30 * time.Second
	holdoutCadence = 10 * time.Minute
)

func TestHandoverSpawnsTheSuccessorWithThisDaemonsAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	told := h.spawner.Told()
	if len(told) != 1 || told[0] != "127.0.0.1:7777" {
		t.Fatalf("the successor was told %v, want this daemon's own address", told)
	}
}

func TestHandoverAnnouncesTheSuccessorsAddressAndTheSelfMergeCause(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	sent := h.announcer.Sent()
	if len(sent) != 1 {
		t.Fatalf("shutdown announcements = %d, want 1", len(sent))
	}
	if sent[0].GetAddress() != "127.0.0.1:7788" {
		t.Fatalf("address = %q, want the successor's", sent[0].GetAddress())
	}
	if sent[0].GetCause().GetSelfMergeRollout() == nil {
		t.Fatalf("cause = %v, want the self_merge_rollout arm", sent[0].GetCause())
	}
}

func TestTheAnnouncementCarriesTheBoundedOutageAndItsMintedInstant(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	sent := h.announcer.Sent()[0]
	if sent.GetExpectedOutageMs() != 5000 {
		t.Fatalf("expected_outage_ms = %d, want the wired 5s", sent.GetExpectedOutageMs())
	}
	if sent.GetMintedAtMs() != instant.UnixMilli() {
		t.Fatalf("minted_at_ms = %d, want the clock's instant %d", sent.GetMintedAtMs(), instant.UnixMilli())
	}
}

func TestHandoverNeverAnnouncesWhenTheSuccessorDoesNotComeUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.spawner.err = errFake

	// Act
	err := h.c.Handover(context.Background())

	// Assert
	if err == nil {
		t.Fatalf("Handover succeeded with no successor")
	}
	if len(h.announcer.Sent()) != 0 {
		t.Fatalf("a stand-down was announced with no successor to dial")
	}
}

func TestATransferGoesFreenessThenQuiesceThenDetachThenTransferred(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	taken := h.order.Taken()
	quiesce, detach := indexOf(taken, "quiesce"), indexOf(taken, "detach")
	if quiesce < 0 || detach < 0 || quiesce > detach {
		t.Fatalf("steps = %v, want quiesce before detach", taken)
	}
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != ws || calls[0].Kind != "transferred" {
		t.Fatalf("pushes = %+v, want one transferred for %s after the detach", calls, ws)
	}
}

func TestATransferDetachesTheShimRatherThanKillingIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	shim := h.fleet.live[ws]

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if !shim.Detached() {
		t.Fatalf("the workspace's shim was not detached")
	}
	if len(shim.KillRequests()) != 0 || len(shim.ForceKills()) != 0 {
		t.Fatalf("the handover killed a shim; it keeps running and keeps its kernel lock")
	}
}

func TestATransferReleasesServingOwnership(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}
	owner, err := h.db.Serving(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if owner != nil {
		t.Fatalf("serving owner = %q after the transfer, want released", *owner)
	}
}

func TestTheTransferredPushCarriesTheSuccessorsAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if got := h.pusher.Calls()[0].Address; got != "127.0.0.1:7788" {
		t.Fatalf("transferred address = %q, want the successor's", got)
	}
}

func TestTheParticipantSnapshotIsTakenAtTheAnnouncement(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.participants.Set(ws, Participants{Host: true, Web: true})

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if got := h.c.ExpectedParticipants(ws); got != 2 {
		t.Fatalf("expected participants = %d, want the two streams held at announcement", got)
	}
}

func TestAHeadlessWorkspaceExpectsNoParticipants(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if got := h.c.ExpectedParticipants(ws); got != 0 {
		t.Fatalf("expected participants = %d, want none for a headless workspace", got)
	}
}

func TestTheIntentManifestNamesEverySessionsPidAndParticipants(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.participants.Set(ws, Participants{Host: true})
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}
	m, found, err := ReadManifest(h.c.deps.IntentManifest)

	// Assert
	if err != nil || !found {
		t.Fatalf("ReadManifest found = %v err = %v", found, err)
	}
	if len(m.Sessions) != 1 {
		t.Fatalf("manifest sessions = %d, want 1", len(m.Sessions))
	}
	got := m.Sessions[0]
	if got.Workspace != ws || got.Dir != record.Dir || got.ShimPID != 4242 {
		t.Fatalf("manifest session = %+v, want %s at %s on pid 4242", got, ws, record.Dir)
	}
	if got.Intent != IntentPreserve {
		t.Fatalf("intent = %q, want %q: a handover kills nothing", got.Intent, IntentPreserve)
	}
	if !got.ExpectedHost || got.ExpectedWeb {
		t.Fatalf("expected participants = host %v web %v, want host only", got.ExpectedHost, got.ExpectedWeb)
	}
}

func TestHandoverExitsAfterTheLastTransfer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 2); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	taken := h.order.Taken()
	exit := indexOf(taken, "exit")
	if exit < 0 {
		t.Fatalf("steps = %v, want the orderly exit", taken)
	}
	if exit != len(taken)-1 {
		t.Fatalf("steps = %v, want the exit last", taken)
	}
}

func TestHandoverIgnoresAWorkspaceAnotherDaemonServes(t *testing.T) {
	// Arrange
	h := newHarness(t)
	mine, _ := h.workspace(t)
	theirs, _ := h.workspace(t)
	if err := h.db.ClaimServing(context.Background(), theirs, ids.InstanceID("some-other-daemon")); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != mine {
		t.Fatalf("pushes = %+v, want only the workspace this daemon serves", calls)
	}
}

func TestAnExpiredAdoptionWindowRecordsTheWorkspacesOwnFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}
	faults, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultAdoptionExpired})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(faults) != 1 {
		t.Fatalf("adoption-expiry faults = %d, want the workspace's own one", len(faults))
	}
}

func TestAnAdoptionThatLandedRecordsNoFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	successor := ids.InstanceID("daemon-successor")

	// Act
	done := make(chan error, 1)
	go func() { done <- h.c.Handover(context.Background()) }()
	h.clock.awaitArmed(t, adoptionWindow)
	if err := h.db.ClaimServing(context.Background(), ws, successor); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	h.clock.Fire(adoptionWindow)
	if err := <-done; err != nil {
		t.Fatalf("Handover: %v", err)
	}
	faults, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultAdoptionExpired})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(faults) != 0 {
		t.Fatalf("adoption-expiry faults = %d, want none once the successor claimed the workspace", len(faults))
	}
}

func TestANeverFreeWorkspaceIsWaitedOnForeverAndNamedOnACadence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.Handover(context.Background()) }()
	<-h.freeness.calls
	// Two holdout cadences pass with the workspace still busy; the wait must
	// still be standing after each.
	h.clock.awaitArmed(t, holdoutCadence)
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)
	warns := levelRecords(records(h.log, opTransfer), "warn")
	close(gate)
	h.clock.awaitArmed(t, adoptionWindow)
	h.clock.Fire(adoptionWindow)
	if err := <-done; err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if len(warns) < 2 {
		t.Fatalf("holdout warnings = %d, want one per cadence while the workspace stayed busy", len(warns))
	}
	for _, warn := range warns {
		if warn.Context["cadence"] != holdoutCadence.String() {
			t.Fatalf("holdout warning cadence = %v, want the ten-minute ruling", warn.Context["cadence"])
		}
	}
}

func TestANeverFreeWorkspaceIsNeverInterruptedToHurryIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	shim := h.fleet.live[ws]
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.Handover(context.Background()) }()
	<-h.freeness.calls
	h.clock.awaitArmed(t, holdoutCadence)
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)
	killed := len(shim.KillRequests()) + len(shim.ForceKills())
	close(gate)
	h.clock.awaitArmed(t, adoptionWindow)
	h.clock.Fire(adoptionWindow)
	<-done

	// Assert
	if killed != 0 {
		t.Fatalf("kill calls while waiting = %d, want none: nothing is interrupted to hurry a holdout", killed)
	}
}
