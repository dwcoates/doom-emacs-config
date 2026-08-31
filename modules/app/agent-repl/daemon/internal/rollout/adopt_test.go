package rollout

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
)

// arm writes an intent manifest naming one workspace with the given expected
// participants, then Joins — the joining daemon's whole boot half.
func arm(t *testing.T, h *harness, ws ids.WorkspaceID, expected Participants) {
	t.Helper()
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if err := h.c.writeManifest(context.Background(), Manifest{
		Daemon:    ids.InstanceID("daemon-outgoing-previous"),
		Successor: "127.0.0.1:7788",
		WrittenAt: instant,
		Sessions: []ManifestSession{{
			Workspace:       ws,
			Dir:             record.Dir,
			ShimPID:         4242,
			VendorSessionID: "vendor-1",
			Intent:          IntentPreserve,
			ExpectedHost:    expected.Host,
			ExpectedWeb:     expected.Web,
		}},
	}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}
}

func TestJoinAdoptsAHeadlessWorkspaceAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 1 || got[0] != ws {
		t.Fatalf("adoptions = %v, want the headless workspace adopted at boot", got)
	}
}

func TestJoinWaitsForTheParticipantsOfANonHeadlessWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	arm(t, h, ws, Participants{Host: true})

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want none until the host participant calls", got)
	}
}

func TestJoinDoesNothingWithNoManifest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want none: this daemon is not joining anything", got)
	}
}

func TestAdoptHostCompletesASingleHostRendezvous(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true})

	// Act
	err := h.c.AdoptHost(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("AdoptHost: %v", err)
	}
	if got := h.fleet.Adoptions(); len(got) != 1 {
		t.Fatalf("adoptions = %v, want the workspace adopted once its one participant called", got)
	}
}

func TestTheRendezvousCompletesOnlyWhenEveryExpectedParticipantHasCalled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true, Web: true})

	// Act
	first := h.c.AdoptHost(context.Background(), ws)
	second := h.c.AdoptWeb(context.Background(), ws)

	// Assert
	if !errors.Is(first, ErrNotYetAdopted) {
		t.Fatalf("the first call answered %v, want not_yet_adopted", first)
	}
	if second != nil {
		t.Fatalf("the second call answered %v, want success", second)
	}
}

func TestAdoptWebAnswersNoTransferAnnouncedOnAnOrdinaryPageBoot(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	err := h.c.AdoptWeb(context.Background(), ws)

	// Assert
	if !errors.Is(err, ErrNoTransferAnnounced) {
		t.Fatalf("AdoptWeb answered %v, want no_transfer_announced", err)
	}
}

func TestTheOrdinaryPageBootIsRecordedAtInfoAndNeverAsAWarning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	_ = h.c.AdoptWeb(context.Background(), ws)

	// Assert
	if warns := levelRecords(records(h.log, opAdoptWeb), "warn"); len(warns) != 0 {
		t.Fatalf("warnings = %+v, want none: every non-handover page boot makes this call", warns)
	}
	if infos := levelRecords(records(h.log, opAdoptWeb), "info"); len(infos) != 1 {
		t.Fatalf("info records = %d, want the one INFO the ruling allows", len(infos))
	}
}

func TestAnUnexpectedParticipantIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true})

	// Act
	err := h.c.AdoptWeb(context.Background(), ws)

	// Assert
	if !errors.Is(err, ErrParticipantNotExpected) {
		t.Fatalf("AdoptWeb answered %v, want participant_not_expected", err)
	}
}

func TestACallOnAnAlreadyAdoptedWorkspaceSucceedsAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true})
	if err := h.c.AdoptHost(context.Background(), ws); err != nil {
		t.Fatalf("AdoptHost: %v", err)
	}

	// Act
	err := h.c.AdoptHost(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("the second call answered %v, want success", err)
	}
	if got := h.fleet.Adoptions(); len(got) != 1 {
		t.Fatalf("adoptions = %v, want the workspace adopted exactly once", got)
	}
}

func TestAdoptionClaimsServingOwnershipForThisDaemon(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	arm(t, h, ws, Participants{})
	owner, err := h.db.Serving(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if owner == nil || *owner != selfInstance {
		t.Fatalf("serving owner = %v, want this daemon", owner)
	}
}

func TestAdoptionDrainsTheHeldIntakeBeforePublishingViews(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	taken := h.order.Taken()
	drain, publish := indexOf(taken, "drain_intake"), indexOf(taken, "publish_views")
	if drain < 0 || publish < 0 || drain > publish {
		t.Fatalf("steps = %v, want the held intake drained before the fresh views", taken)
	}
}

func TestAdoptionAdoptsTheRunningShimBeforeClaimingServing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	taken := h.order.Taken()
	if adopt := indexOf(taken, "adopt"); adopt != 0 {
		t.Fatalf("steps = %v, want the running shim adopted first", taken)
	}
}

func TestDaemonAddrIsWrittenOnlyWhenEveryWorkspaceIsOwned(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first, _ := h.workspace(t)
	second, _ := h.workspace(t)
	firstRecord, _ := h.db.Workspace(context.Background(), first)
	secondRecord, _ := h.db.Workspace(context.Background(), second)
	if err := h.c.writeManifest(context.Background(), Manifest{
		Daemon: ids.InstanceID("daemon-outgoing-previous"), WrittenAt: instant,
		Sessions: []ManifestSession{
			{Workspace: first, Dir: firstRecord.Dir, Intent: IntentPreserve, ExpectedHost: true},
			{Workspace: second, Dir: secondRecord.Dir, Intent: IntentPreserve, ExpectedHost: true},
		},
	}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}

	// Act
	if err := h.c.AdoptHost(context.Background(), first); err != nil {
		t.Fatalf("AdoptHost(first): %v", err)
	}
	h.mu.Lock()
	afterFirst := h.addrWrites
	h.mu.Unlock()
	if err := h.c.AdoptHost(context.Background(), second); err != nil {
		t.Fatalf("AdoptHost(second): %v", err)
	}
	h.mu.Lock()
	afterSecond := h.addrWrites
	h.mu.Unlock()

	// Assert
	if afterFirst != 0 {
		t.Fatalf("daemon.addr writes after the first adoption = %d, want none", afterFirst)
	}
	if afterSecond != 1 {
		t.Fatalf("daemon.addr writes after the last adoption = %d, want 1", afterSecond)
	}
}

func TestAdoptionWarnsWhenNoShimHoldsTheWorkspaceLock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, dir := h.workspace(t)
	h.mu.Lock()
	h.lockStates[dir] = sessionlock.StateFree
	h.mu.Unlock()

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	warns := levelRecords(records(h.log, opAdopt), "warn")
	if len(warns) != 1 {
		t.Fatalf("adoption warnings = %d, want one naming the free lock", len(warns))
	}
}

func TestAdoptionSurfacesAFailedShimAdoption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.adoptErr[ws] = errFake
	arm(t, h, ws, Participants{Host: true})

	// Act
	err := h.c.AdoptHost(context.Background(), ws)

	// Assert
	if err == nil {
		t.Fatalf("AdoptHost succeeded with no shim to adopt")
	}
}

func TestExpectedParticipantsIsZeroForAWorkspaceWithNoTransfer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	got := h.c.ExpectedParticipants(ws)

	// Assert
	if got != 0 {
		t.Fatalf("expected participants = %d, want none with no transfer announced", got)
	}
}
