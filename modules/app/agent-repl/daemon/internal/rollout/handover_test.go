package rollout

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// runHandover starts an unforced HandOver, waits for every workspace's
// transfer push, expires the adoption windows, and waits for the flow's exit.
// The clock is driven rather than waited out: nothing in this package sleeps.
func runHandover(t *testing.T, h *harness, workspaces int) error {
	t.Helper()
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		return err
	}
	for range workspaces {
		h.clock.awaitArmed(t, adoptionWindow)
	}
	// THE SUCCESSOR ADOPTS every workspace the handover transferred. Left
	// unadopted, an expired window takes the workspace back and the handover
	// never exits (see the reclaim tests).
	h.successorAdopts(t)
	h.clock.Fire(adoptionWindow)
	awaitExit(t, h)
	h.registry.wait()
	return nil
}

// awaitExit blocks until the handover reaches its orderly exit.
func awaitExit(t *testing.T, h *harness) {
	t.Helper()
	select {
	case <-h.exits:
	case <-time.After(10 * time.Second):
		t.Fatalf("the handover never exited; steps %v", h.order.Taken())
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
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("HandOver succeeded with no successor")
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

// TestATransferWhoseHandOverFailsPushesNoTransferNotice pins the refusal arm:
// a shim the fleet could not hand over is neither detached nor announced, so
// the workspace stays served here rather than half-transferred.
func TestATransferWhoseHandOverFailsPushesNoTransferNotice(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	shim := h.fleet.live[ws]
	h.fleet.handOverErr[ws] = errors.New("the watches would not close")

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert
	if shim.Detached() {
		t.Fatal("a shim whose hand-over failed was detached")
	}
	if calls := h.pusher.Calls(); len(calls) != 0 {
		t.Fatalf("pushes = %+v, want no transfer notice for a failed hand-over", calls)
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
	// Arrange: another daemon serves `theirs`, and THIS daemon holds no
	// session for it. (One this daemon holds a live session for is its own
	// whatever the row says; see TestHandoverHandsOverALiveSessionWhoseRowNamesAnotherInstance.)
	h := newHarness(t)
	mine, _ := h.workspace(t)
	theirs, _ := h.workspace(t)
	if err := h.db.ClaimServing(context.Background(), theirs, ids.InstanceID("some-other-daemon")); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	h.fleet.mu.Lock()
	delete(h.fleet.live, theirs)
	h.fleet.mu.Unlock()

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

// A COLD-STARTED DAEMON'S LIVE SESSIONS ARE ITS OWN. The three below pin the
// fix for the 2026-09-24 orphaning: a live session whose serving row names a
// dead instance was silently skipped, and its shim was left serving nobody.

// foreignLiveSession arranges a workspace this daemon holds a live session for
// while its serving row names another instance, as a cold-started daemon's
// pre-fix bring-up left it.
func foreignLiveSession(t *testing.T, h *harness) ids.WorkspaceID {
	t.Helper()
	ws, _ := h.workspace(t)
	if err := h.db.ClaimServing(context.Background(), ws, ids.InstanceID("a-dead-instance")); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	return ws
}

func TestHandoverHandsOverALiveSessionWhoseRowNamesAnotherInstance(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := foreignLiveSession(t, h)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != ws {
		t.Fatalf("pushes = %+v, want the live session transferred", calls)
	}
}

func TestHandoverStatesALiveSessionWithAForeignServingRowAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	foreignLiveSession(t, h)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	violations := 0
	for _, rec := range h.log.Records() {
		if rec.Level == dlog.LevelError && rec.Operation == opHandover && strings.Contains(rec.Message, "the live session is the truth") {
			violations++
		}
	}
	if violations != 1 {
		t.Fatalf("invariant-violation ERROR records = %d, want exactly one", violations)
	}
}

func TestHandoverReleasesTheReclaimedRowOfALiveSession(t *testing.T) {
	// Arrange: the successor waits for THIS daemon's release, so the row must
	// name this daemon before the transfer releases it.
	h := newHarness(t)
	ws := foreignLiveSession(t, h)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if owner != nil {
		t.Fatalf("serving owner = %q, want the row released for the successor", *owner)
	}
}

func TestHandoverSkipsASessionlessForeignWorkspaceAtDebug(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	theirs, _ := h.workspace(t)
	if err := h.db.ClaimServing(context.Background(), theirs, ids.InstanceID("some-other-daemon")); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	h.fleet.mu.Lock()
	delete(h.fleet.live, theirs)
	h.fleet.mu.Unlock()

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	skips := 0
	for _, rec := range h.log.Records() {
		if rec.Operation == opHandover && rec.Context["workspace"] == string(theirs) && strings.Contains(rec.Message, "it is not handed over") {
			if rec.Level != dlog.LevelDebug {
				t.Fatalf("skip record level = %q, want debug", rec.Level)
			}
			skips++
		}
	}
	if skips != 1 {
		t.Fatalf("skip records for the foreign workspace = %d, want exactly one", skips)
	}
}

// expireAdoption runs a handover of one workspace nobody adopts and fires its
// adoption window, joining the handover to its end.
func expireAdoption(t *testing.T, h *harness) {
	t.Helper()
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	h.clock.Fire(adoptionWindow)
	h.registry.wait()
	h.c.handoverDone.Wait()
}

// The reclaim tests are invariant D, the 2026-09-27 incident's second half:
// five transferred workspaces nobody adopted kept their quiesce holds and no
// daemon served them. An expired window now TAKES THE WORKSPACE BACK.

func TestAnExpiredAdoptionWindowTakesTheWorkspaceBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	expireAdoption(t, h)

	// Assert
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil || owner == nil || *owner != selfInstance {
		t.Fatalf("serving owner after the expiry = %v (%v), want this daemon again", owner, err)
	}
	if standing := h.c.Standing(ws); standing != StandingOwned {
		t.Fatalf("standing = %v, want owned: the workspace is served here again", standing)
	}
}

func TestAnExpiredAdoptionWindowReleasesTheTransfersHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	expireAdoption(t, h)

	// Assert
	if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
		t.Fatalf("Lease after the expiry = (held %v, %v), want the quiesce hold released", held, err)
	}
}

func TestAnExpiredAdoptionWindowReattachesTheDetachedShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	expireAdoption(t, h)

	// Assert
	if adoptions := h.fleet.Adoptions(); len(adoptions) != 1 || adoptions[0] != ws {
		t.Fatalf("adoptions = %v, want the detached shim re-attached here", adoptions)
	}
}

func TestAnExpiredAdoptionWindowRecordsTheWorkspacesOwnFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	expireAdoption(t, h)
	faults, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultAdoptionExpired})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(faults) != 1 {
		t.Fatalf("adoption-expiry faults = %d, want the workspace's own one", len(faults))
	}
	if !loggedError(h.log, opAdoption, "this daemon took it back") {
		t.Fatalf("records = %+v, want the reclaim stated at ERROR", h.log.Records())
	}
}

func TestAHandoverWithAReclaimedWorkspaceDoesNotExit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	expireAdoption(t, h)

	// Assert
	select {
	case <-h.exits:
		t.Fatalf("the daemon exited with a workspace it took back")
	default:
	}
	if !loggedError(h.log, opHandover, "were reclaimed and are served here again") {
		t.Fatalf("records = %+v, want the unfinished handover at ERROR", h.log.Records())
	}
}

// TestAWorkspaceTheSuccessorClaimedIsNotTakenBack pins the arbitration: a
// successor that claimed the row keeps the workspace, however late.
func TestAWorkspaceTheSuccessorClaimedIsNotTakenBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	successor := ids.InstanceID("daemon-successor")
	if err := h.db.ClaimServing(context.Background(), ws, successor); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}

	// Act
	reclaimed, err := h.c.reclaim(context.Background(), ws, "", true, dlog.Context{})

	// Assert
	if err != nil || reclaimed {
		t.Fatalf("reclaim = (%v, %v), want (false, nil): the successor won the row", reclaimed, err)
	}
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil || owner == nil || *owner != successor {
		t.Fatalf("serving owner = %v (%v), want the successor kept", owner, err)
	}
	h.clock.Fire(adoptionWindow)
	awaitExit(t, h)
}

// The failed-transfer tests are invariant A's handover half: a transfer that
// fails after its quiesce releases the hold it took and serves the workspace
// here again.

func TestATransferWhoseShimWillNotDetachReleasesItsHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.handOverErr[ws] = errFake

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	h.c.handoverDone.Wait()

	// Assert
	if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
		t.Fatalf("Lease after the failed transfer = (held %v, %v), want the quiesce hold released", held, err)
	}
}

func TestATransferWhoseServingReleaseFailsTakesTheWorkspaceBack(t *testing.T) {
	// Arrange
	fail := &atomic.Bool{}
	h := newHarness(t, func(d *Deps) { d.DB = releaseFailingDB{DB: d.DB, fail: fail} })
	ws, _ := h.workspace(t)
	fail.Store(true)

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	h.c.handoverDone.Wait()

	// Assert
	if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
		t.Fatalf("Lease after the failed transfer = (held %v, %v), want the quiesce hold released", held, err)
	}
	if adoptions := h.fleet.Adoptions(); len(adoptions) != 1 || adoptions[0] != ws {
		t.Fatalf("adoptions = %v, want the detached shim re-attached here", adoptions)
	}
	if standing := h.c.Standing(ws); standing != StandingOwned {
		t.Fatalf("standing = %v, want owned", standing)
	}
}

// releaseFailingDB refuses ReleaseServing while fail is set.
type releaseFailingDB struct {
	wsm.DB
	fail *atomic.Bool
}

func (d releaseFailingDB) ReleaseServing(ctx context.Context, ws wsm.WorkspaceID, instance wsm.InstanceID) error {
	if d.fail.Load() {
		return errFake
	}
	return d.DB.ReleaseServing(ctx, ws, instance)
}

func TestAnAdoptionThatLandedRecordsNoFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	successor := ids.InstanceID("daemon-successor")

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	if err := h.db.ClaimServing(context.Background(), ws, successor); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	h.clock.Fire(adoptionWindow)
	awaitExit(t, h)
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
	err = h.c.transfer(context.Background(), record, &handoverPlan{successor: "127.0.0.1:7788"}, Participants{}, &windows)
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

	// Act: two holdout cadences pass with the workspace still busy.
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, holdoutCadence)
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)
	warns := levelRecords(records(h.log, opHandover), "warn")
	h.registry.free(ws)
	h.clock.awaitArmed(t, adoptionWindow)
	h.successorAdopts(t)
	h.clock.Fire(adoptionWindow)
	awaitExit(t, h)

	// Assert
	if len(warns) < 2 {
		t.Fatalf("holdout warnings = %d, want one per cadence while the workspace stayed busy", len(warns))
	}
	for _, warn := range warns {
		if warn.Context["cadence"] != holdoutCadence.String() {
			t.Fatalf("holdout warning cadence = %v, want the ten-minute ruling", warn.Context["cadence"])
		}
		holdouts, _ := warn.Context["holdouts"].([]string)
		if len(holdouts) != 1 || holdouts[0] != string(ws) {
			t.Fatalf("holdouts = %v, want the busy workspace named", warn.Context["holdouts"])
		}
	}
}

func TestANeverFreeWorkspaceIsNeverInterruptedToHurryIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	shim := h.fleet.live[ws]
	h.freeness.SetFree(ws, false)

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, holdoutCadence)
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)
	killed := len(shim.KillRequests()) + len(shim.ForceKills())
	detached := shim.Detached()
	h.registry.free(ws)
	h.clock.awaitArmed(t, adoptionWindow)
	h.successorAdopts(t)
	h.clock.Fire(adoptionWindow)
	awaitExit(t, h)

	// Assert
	if killed != 0 || detached {
		t.Fatalf("kills %d, detached %v while waiting; want nothing touched until the workspace fell free", killed, detached)
	}
}

func TestABusyWorkspaceDoesNotDelayAFreeOneBehindIt(t *testing.T) {
	// Arrange: two workspaces, the first busy.
	h := newHarness(t)
	busy, _ := h.workspace(t)
	free, _ := h.workspace(t)
	h.freeness.SetFree(busy, false)

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	h.registry.wait()

	// Assert: the free workspace moved while the busy one is still registered.
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != free {
		t.Fatalf("pushes = %+v, want only the free workspace transferred", calls)
	}
	if !h.registry.Pending(busy) {
		t.Fatalf("the busy workspace's transfer is not registered")
	}
}

func TestAForcedHandoverTransfersEveryWorkspaceNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	accepted, err := h.c.HandOver(context.Background(), true)
	if err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	h.successorAdopts(t)
	h.clock.Fire(adoptionWindow)
	awaitExit(t, h)

	// Assert
	if !accepted.Forced || accepted.Busy != 1 {
		t.Fatalf("acceptance = %+v, want forced with the one busy workspace counted", accepted)
	}
	requests := h.registry.Requests()
	if len(requests) != 1 || !requests[0].Req.Force || !requests[0].Req.KeepDraining {
		t.Fatalf("requests = %+v, want one forced, kept-draining transfer", requests)
	}
	m, found, err := ReadManifest(h.c.deps.IntentManifest)
	if err != nil || !found || !m.Forced {
		t.Fatalf("manifest forced = %v (found %v, err %v), want the successor told the handover was forced", m.Forced, found, err)
	}
}

func TestAHandoverWithATransferTheRegistryRefusedDoesNotExit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.registry.err = errFake

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert
	select {
	case <-h.exits:
		t.Fatalf("the daemon exited with a workspace it still serves")
	default:
	}
	if !loggedError(h.log, opTransfer, "the bounce registry refused the workspace's transfer") {
		t.Fatalf("records = %+v, want the refusal at ERROR", h.log.Records())
	}
}

func TestAHandoverWithAFailedTransferDoesNotExit(t *testing.T) {
	// Arrange: the quiesce fails, so the transfer does.
	h := newHarness(t, func(d *Deps) {
		d.Quiesce = func(context.Context, ids.WorkspaceID) (ids.LeaseID, error) { return "", errFake }
	})
	h.workspace(t)

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	h.c.handoverDone.Wait()

	// Assert
	select {
	case <-h.exits:
		t.Fatalf("the daemon exited with a workspace whose transfer failed")
	default:
	}
	if !loggedError(h.log, opHandover, "the handover cannot finish") {
		t.Fatalf("records = %+v, want the unfinished handover at ERROR", h.log.Records())
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

// loggedErrorWith reports an ERROR under operation whose message holds substr
// and whose cause holds cause.
func loggedErrorWith(log *dlog.TestSurfaces, operation, substr, cause string) bool {
	for _, rec := range records(log, operation) {
		got, _ := rec.Context["cause"].(string)
		if rec.Level == dlog.LevelError && strings.Contains(rec.Message, substr) && strings.Contains(got, cause) {
			return true
		}
	}
	return false
}

// Quiesced answers the workspaces whose intake was quiesced, in order.
func (h *harness) Quiesced() []ids.WorkspaceID {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]ids.WorkspaceID(nil), h.quiesced...)
}

// Drained answers the workspaces whose held intake was drained, in order.
func (h *harness) Drained() []ids.WorkspaceID {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]ids.WorkspaceID(nil), h.drained...)
}

// listFailingDB is the state client with its workspace listing refusable, so
// the handover's `served` step fails AFTER the successor is up.
type listFailingDB struct {
	wsm.DB
	fail *atomic.Bool
}

func (d listFailingDB) ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error) {
	if d.fail.Load() {
		return nil, errFake
	}
	return d.DB.ListWorkspaces(ctx)
}

// failureAfterSpawn is one way a handover fails once its successor exists:
// how to cause it, how to heal it, and the ERROR that names it.
type failureAfterSpawn struct {
	name string
	// deps wires the failure into the controller's collaborators.
	deps func(t *testing.T, fail *atomic.Bool) func(*Deps)
	// arm causes the failure on the harness before the handover.
	arm func(t *testing.T, h *harness, fail *atomic.Bool)
	// heal undoes it, so the next handover can succeed.
	heal func(t *testing.T, h *harness, fail *atomic.Bool)
	// logged is the handover's own ERROR for the failure.
	logged string
}

// failuresAfterSpawn is every failure path between the spawn and the
// acceptance.
func failuresAfterSpawn() []failureAfterSpawn {
	noDeps := func(*testing.T, *atomic.Bool) func(*Deps) { return func(*Deps) {} }
	return []failureAfterSpawn{
		{
			name: "the successor started and never reported",
			deps: noDeps,
			arm: func(_ *testing.T, h *harness, _ *atomic.Bool) {
				h.spawner.err = errFake
				h.spawner.startedThenFailed = true
			},
			heal: func(_ *testing.T, h *harness, _ *atomic.Bool) {
				h.spawner.err = nil
				h.spawner.startedThenFailed = false
			},
			logged: "the successor did not come up",
		},
		{
			name: "the successor exited before it answered a health probe",
			deps: noDeps,
			arm: func(_ *testing.T, h *harness, _ *atomic.Bool) {
				h.spawner.readyErr = &SuccessorExitedError{PID: fakeSuccessorPID, Exit: "exit status 1"}
			},
			heal: func(_ *testing.T, h *harness, _ *atomic.Bool) {
				h.spawner.readyErr = nil
			},
			logged: "the successor never proved it was serving",
		},
		{
			name: "listing what is served failed",
			deps: func(_ *testing.T, fail *atomic.Bool) func(*Deps) {
				return func(d *Deps) { d.DB = listFailingDB{DB: d.DB, fail: fail} }
			},
			arm:    func(_ *testing.T, _ *harness, fail *atomic.Bool) { fail.Store(true) },
			heal:   func(_ *testing.T, _ *harness, fail *atomic.Bool) { fail.Store(false) },
			logged: "could not list what this daemon serves",
		},
		{
			name: "the manifest could not be written after the announcement",
			deps: noDeps,
			arm: func(t *testing.T, h *harness, _ *atomic.Bool) {
				// A regular file where the manifest's directory should be makes
				// the write fail at its MkdirAll.
				blocker := filepath.Join(t.TempDir(), "not-a-dir")
				if err := os.WriteFile(blocker, nil, 0o644); err != nil {
					t.Fatalf("write the blocker: %v", err)
				}
				h.c.deps.IntentManifest = filepath.Join(blocker, "manifest.json")
			},
			heal: func(t *testing.T, h *harness, _ *atomic.Bool) {
				h.c.deps.IntentManifest = filepath.Join(t.TempDir(), "intent", "manifest.json")
			},
			logged: "the intent manifest could not be written after the announcement",
		},
	}
}

// TestASuccessorThatDiesBeforeItIsReadyIsHandedNothing is invariant C, the
// 2026-09-27 incident's first half: the successor reported its address and
// exited before it could serve. Nothing may be quiesced, announced or asked
// of the bounce registry, no lease may be taken, and the ERROR names the
// successor's exit.
func TestASuccessorThatDiesBeforeItIsReadyIsHandedNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.spawner.readyErr = &SuccessorExitedError{PID: fakeSuccessorPID, Exit: "exit status 1"}

	// Act
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	var exited *SuccessorExitedError
	if !errors.As(err, &exited) {
		t.Fatalf("HandOver = %v, want the successor's exit", err)
	}
	if len(h.announcer.Sent()) != 0 || len(h.registry.Requests()) != 0 || len(h.Quiesced()) != 0 {
		t.Fatalf("announced %d, transfers asked %d, quiesced %v: want nothing touched",
			len(h.announcer.Sent()), len(h.registry.Requests()), h.Quiesced())
	}
	if _, held, dbErr := h.db.Lease(context.Background(), ws); dbErr != nil || held {
		t.Fatalf("Lease after the abandoned handover = (held %v, %v), want none", held, dbErr)
	}
	if !loggedErrorWith(h.log, opHandover, "the successor never proved it was serving", "exit status 1") {
		t.Fatalf("records = %+v, want the ERROR naming the successor's exit", h.log.Records())
	}
}

func TestAHandoverThatFailsAfterTheSpawnStopsItsSuccessorAndReleasesTheSlot(t *testing.T) {
	for _, tc := range failuresAfterSpawn() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			fail := &atomic.Bool{}
			h := newHarness(t, tc.deps(t, fail))
			h.workspace(t)
			tc.arm(t, h, fail)

			// Act
			_, err := h.c.HandOver(context.Background(), false)

			// Assert
			if err == nil {
				t.Fatalf("HandOver succeeded through the failure")
			}
			if live := h.spawner.Live(); live != 0 {
				t.Fatalf("successors still standing = %d, want the abandoned one stopped", live)
			}
			if _, rolling := h.c.RollingOut(); rolling {
				t.Fatalf("the handover is still in flight after it was abandoned")
			}
			if !loggedError(h.log, opHandover, tc.logged) {
				t.Fatalf("records = %+v, want %q at ERROR", h.log.Records(), tc.logged)
			}
		})
	}
}

func TestTheDeployAfterAFailedHandoverNeverHasTwoSuccessors(t *testing.T) {
	for _, tc := range failuresAfterSpawn() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			fail := &atomic.Bool{}
			h := newHarness(t, tc.deps(t, fail))
			ws, _ := h.workspace(t)
			h.freeness.SetFree(ws, false)
			tc.arm(t, h, fail)
			if _, err := h.c.HandOver(context.Background(), false); err == nil {
				t.Fatalf("the first HandOver succeeded through the failure")
			}
			tc.heal(t, h, fail)

			// Act
			_, err := h.c.HandOver(context.Background(), false)

			// Assert
			if err != nil {
				t.Fatalf("second HandOver: %v, want it accepted", err)
			}
			if told := h.spawner.Told(); len(told) != 2 {
				t.Fatalf("spawns = %d, want the failed one and its replacement", len(told))
			}
			if live := h.spawner.Live(); live != 1 {
				t.Fatalf("successors standing = %d, want exactly the second handover's", live)
			}
		})
	}
}

func TestAnAbandonedHandoverDisarmsTheRendezvousItsAnnouncementArmed(t *testing.T) {
	// Arrange: the manifest fails after the announcement armed the rendezvous.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	var manifestFailure failureAfterSpawn
	for _, f := range failuresAfterSpawn() {
		if f.name == "the manifest could not be written after the announcement" {
			manifestFailure = f
		}
	}
	manifestFailure.arm(t, h, &atomic.Bool{})

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err == nil {
		t.Fatalf("HandOver succeeded with an unwritable manifest")
	}

	// Assert
	if n := h.c.ExpectedParticipants(ws); n != 0 {
		t.Fatalf("expected participants = %d after the abandon, want the rendezvous disarmed", n)
	}
	h.c.mu.Lock()
	_, armed := h.c.rendezvous[ws]
	h.c.mu.Unlock()
	if armed {
		t.Fatalf("the abandoned handover left its rendezvous armed")
	}
}

func TestAHandoverWhoseSuccessorWillNotStopStaysInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.spawner.err = errFake
	h.spawner.startedThenFailed = true
	h.spawner.stopErr = errors.New("the successor was killed but not reaped")
	if _, err := h.c.HandOver(context.Background(), false); err == nil {
		t.Fatalf("the first HandOver succeeded with a successor that never reported")
	}
	h.spawner.err = nil
	h.spawner.startedThenFailed = false

	// Act
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	var inFlight *ErrAlreadyRollingOut
	if !errors.As(err, &inFlight) {
		t.Fatalf("err = %v, want *ErrAlreadyRollingOut while the unstopped successor may still run", err)
	}
	if told := h.spawner.Told(); len(told) != 1 {
		t.Fatalf("spawns = %d, want no second successor beside the one that would not stop", len(told))
	}
	if !loggedError(h.log, opHandover, "could not be stopped") {
		t.Fatalf("records = %+v, want the unstoppable successor at ERROR", h.log.Records())
	}
}

func TestASpawnThatStartedNothingIsNotStopped(t *testing.T) {
	// Arrange: the binary would not start, so there is no process to own.
	h := newHarness(t)
	h.workspace(t)
	h.spawner.err = errFake

	// Act
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("HandOver succeeded with no successor")
	}
	if stops := h.spawner.Stops(); stops != 0 {
		t.Fatalf("stops = %d, want none: nothing was started", stops)
	}
	if _, rolling := h.c.RollingOut(); rolling {
		t.Fatalf("a handover that started nothing still holds the slot")
	}
}

// TestAHandoverRemovesAStaleManifestBeforeItsSuccessorBoots pins that a
// joining successor can never read an EARLIER bounce's manifest as this
// handover's intent: it starts looking the moment it boots, before the
// incumbent has written anything.
func TestAHandoverRemovesAStaleManifestBeforeItsSuccessorBoots(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.participants.Set(ws, Participants{Host: true})
	if err := h.c.writeManifest(context.Background(), Manifest{
		Daemon: ids.InstanceID("daemon-long-gone"), WrittenAt: instant,
	}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}
	var presentAtSpawn atomic.Bool
	h.spawner.onSpawn = func() {
		_, err := os.Stat(h.c.deps.IntentManifest)
		presentAtSpawn.Store(err == nil)
	}

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if presentAtSpawn.Load() {
		t.Fatalf("the stale intent manifest was still on disk when the successor was spawned")
	}
}
