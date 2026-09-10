package rollout

import (
	"context"
	"errors"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/dlog"
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

func TestTransferAcceptsServingOwnershipThatAlreadyMoved(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	successor := ids.InstanceID("daemon-successor")
	if err := h.db.ClaimServing(context.Background(), ws, successor); err != nil {
		t.Fatalf("ClaimServing(successor): %v", err)
	}
	var windows sync.WaitGroup

	// Act.
	err = h.c.transfer(context.Background(), record, "127.0.0.1:7788", Participants{}, &windows)
	windows.Wait()

	// Assert.
	if err != nil {
		t.Fatalf("transfer after the successor claimed serving = %v, want success", err)
	}
	infos := levelRecords(records(h.log, opTransfer), "info")
	found := false
	for _, record := range infos {
		if record.Message == "the successor already owns the workspace" && record.Context["owner"] == string(successor) {
			found = true
		}
	}
	if !found {
		t.Fatalf("INFO records = %+v, want the already-owned transition", infos)
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

// TestTheManifestNamesAWorkspaceWithNoShimAsNoSession covers the write site of
// the bounce accounting: a workspace registered and never opened has no
// process to preserve, so calling its entry `preserve` would make its free
// lock read on the successor as a session that silently died — a fault raised
// on the most ordinary handover there is.
func TestTheManifestNamesAWorkspaceWithNoShimAsNoSession(t *testing.T) {
	// Arrange: a registered workspace whose session record carries no shim pid.
	h := newHarness(t)
	ws, dir := h.workspace(t)
	if err := h.db.SetShimPID(context.Background(), ws, nil); err != nil {
		t.Fatalf("SetShimPID: %v", err)
	}

	// Act
	m := h.c.manifest(context.Background(), "127.0.0.1:1", []wsm.Workspace{{ID: ws, Dir: dir}},
		map[ids.WorkspaceID]Participants{})

	// Assert
	if len(m.Sessions) != 1 {
		t.Fatalf("manifest sessions = %d, want the workspace's entry (it arms the rendezvous)", len(m.Sessions))
	}
	if got := m.Sessions[0].Intent; got != IntentNoSession {
		t.Fatalf("intent = %s, want %s", got, IntentNoSession)
	}
}

// TestTheManifestNamesAWorkspaceWithALiveShimAsPreserve is the other side: a
// running shim IS handed over alive, and its free lock on the successor is the
// genuine evidence that it died.
func TestTheManifestNamesAWorkspaceWithALiveShimAsPreserve(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, dir := h.workspace(t)

	// Act
	m := h.c.manifest(context.Background(), "127.0.0.1:1", []wsm.Workspace{{ID: ws, Dir: dir}},
		map[ids.WorkspaceID]Participants{})

	// Assert
	if len(m.Sessions) != 1 || m.Sessions[0].Intent != IntentPreserve {
		t.Fatalf("manifest sessions = %+v, want one preserve entry", m.Sessions)
	}
	if m.Sessions[0].ShimPID == 0 {
		t.Fatalf("shim pid = 0, want the running shim's pid on a preserve entry")
	}
}

// A MERGED WORKSPACE IS THE CASE THESE COVER. The merge removes the worktree
// and leaves the registry row, so the handover cannot transfer the workspace —
// the successor cannot even resolve a log sink for a directory that is gone.
// Its SHIM is still running, though, and this daemon is the only thing that
// knows the process exists.

// untransferableWorkspace is a workspace this daemon serves whose worktree has
// been removed under it, exactly as a merge's terminal removes it.
func untransferableWorkspace(t *testing.T, h *harness) *fakeShim {
	t.Helper()
	ws, dir := h.workspace(t)
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the merged workspace's worktree: %v", err)
	}
	return h.fleet.live[ws]
}

// TestHandoverStandsDownAWorkspaceItCannotTransfer asserts the obligation the
// split creates: a served workspace that is not handed over does not simply
// stay behind, because nothing after this daemon knows its shim exists.
func TestHandoverStandsDownAWorkspaceItCannotTransfer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	merged := untransferableWorkspace(t, h)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	kills := merged.ForceKills()
	if len(kills) != 1 {
		t.Fatalf("the untransferred workspace's shim took %d kills, want 1", len(kills))
	}
	if !kills[0].Force {
		t.Fatalf("kill = %+v, want a forced one: this daemon is already exiting", kills[0])
	}
}

// TestHandoverEndsAnUntransferredSessionBeforeStoppingItsProcess asserts the
// order: the shim writes its own terminals as the session ends, and a signal
// alone gives it no chance to.
func TestHandoverEndsAnUntransferredSessionBeforeStoppingItsProcess(t *testing.T) {
	// Arrange
	h := newHarness(t)
	merged := untransferableWorkspace(t, h)

	// Act
	if err := runHandover(t, h, 0); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if len(merged.KillRequests()) != 1 {
		t.Fatalf("the untransferred workspace's session took %d kill calls, want 1", len(merged.KillRequests()))
	}
	taken := h.order.Taken()
	kill, force := indexOf(taken, "kill_session"), indexOf(taken, "force_kill")
	if kill < 0 || force < 0 || kill > force {
		t.Fatalf("steps = %v, want the session ended before the process was stopped", taken)
	}
}

// TestHandoverStillExitsWhenAnUntransferredShimWillNotGo asserts the failure is
// RECORDED and stepped over: an exit skipped over a leaked shim leaks the
// daemon too.
func TestHandoverStillExitsWhenAnUntransferredShimWillNotGo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	merged := untransferableWorkspace(t, h)
	merged.SetForceError(errors.New("the kernel will not let it go"))

	// Act
	err := runHandover(t, h, 0)

	// Assert
	if err != nil {
		t.Fatalf("Handover: %v, want the exit to happen anyway", err)
	}
	if !loggedError(h.log, opHandover, "will outlive this daemon") {
		t.Fatalf("no error record says the shim will outlive this daemon: %v", h.log.Records())
	}
}

// TestHandoverNeverStopsAShimItTransferred asserts the other half of the same
// accounting: a transferred shim is the successor's to adopt, and stopping it
// would take the workspace's kernel lock down with it.
func TestHandoverNeverStopsAShimItTransferred(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	transferred := h.fleet.live[ws]

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if kills := transferred.ForceKills(); len(kills) != 0 {
		t.Fatalf("a transferred shim took %d kills, want none: it is the successor's to adopt", len(kills))
	}
}

// loggedError reports whether an ERROR record under an operation carries the
// substring.
func loggedError(log *dlog.TestSurfaces, operation, substr string) bool {
	for _, rec := range records(log, operation) {
		if rec.Level == dlog.LevelError && strings.Contains(rec.Message, substr) {
			return true
		}
	}
	return false
}
