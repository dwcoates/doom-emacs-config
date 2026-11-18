package rollout

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// runHandover starts an unforced HandOver, waits for every workspace's
// transfer push, expires the adoption windows, and waits for the flow's exit.
// The clock is driven rather than waited out: nothing in this package sleeps.
func runHandover(t *testing.T, h *harness, workspaces int) error {
	t.Helper()
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
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
	_, err := h.c.HandOver(context.Background(), unforcedAsk)

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
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
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

func TestHandoverDrainsItsMergesBeforeItsExit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	taken := h.order.Taken()
	drain, exit := indexOf(taken, "drain_merges"), indexOf(taken, "exit")
	if drain < 0 || exit < 0 || drain > exit {
		t.Fatalf("steps = %v, want the merges drained before the orderly exit", taken)
	}
}

func TestAHandoverThatDoesNotExitDrainsNoMerge(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.registry.err = errFake

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert
	if indexOf(h.order.Taken(), "drain_merges") >= 0 {
		t.Fatalf("steps = %v, want no merge drained by a daemon that keeps serving", h.order.Taken())
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
// adoption window, returning once the take-back has finished and the
// workspace's next offer is waiting out its pacing delay.
func expireAdoption(t *testing.T, h *harness) {
	t.Helper()
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	h.clock.Fire(adoptionWindow)
	h.registry.wait()
	// The first re-offer's pacing delay is one adoption window.
	h.clock.awaitArmed(t, adoptionWindow)
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

func TestAnExpiredAdoptionWindowEndsTheTransfersKeptDrain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	expireAdoption(t, h)

	// Assert
	if ended := h.registry.EndedDrains(); len(ended) != 1 || ended[0] != ws {
		t.Fatalf("ended drains = %v, want the taken-back workspace's dispatch resumed", ended)
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

// TestAnExpiredAdoptionWindowRetractsTheTransferNotice pins that a take-back
// withdraws the notice that the workspace moved, after it was pushed: left
// standing, it was replayed to the page reloaded back here, and it swallowed
// the next transfer's identical notice (2026-10-12).
func TestAnExpiredAdoptionWindowRetractsTheTransferNotice(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	expireAdoption(t, h)

	// Assert
	var kinds []string
	for _, call := range h.pusher.Calls() {
		if call.WS == ws {
			kinds = append(kinds, call.Kind)
		}
	}
	if len(kinds) != 2 || kinds[0] != "transferred" || kinds[1] != "retract_transferred" {
		t.Fatalf("pushes for the workspace = %v, want [transferred retract_transferred]", kinds)
	}
}

// The re-offer tests are the 2026-10-12 incident: an expired window's
// take-back used to end the handover -- "not exiting" -- and the incumbent
// served the workspace alone for nine hours while the successor waited on its
// exit. A workspace taken back on an expired window is now offered again.

func TestAHandoverWaitingToReofferATakenBackWorkspaceDoesNotExit(t *testing.T) {
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
	if !recordWithMessage(h, opHandover, "info", "the workspace was taken back after its adoption window expired; it is offered to the successor again") {
		t.Fatalf("records = %+v, want the re-offer stated at INFO", h.log.Records())
	}
}

func TestAnUncleanTakeBackIsNotOfferedAgainAndHoldsTheHandover(t *testing.T) {
	// Arrange: the take-back's re-attach of the shim fails.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	setAdoptErr(h.fleet, ws, errors.New("the shim would not answer the re-attach"))
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)

	// Act
	h.clock.Fire(adoptionWindow)
	h.registry.wait()
	h.c.handoverDone.Wait()

	// Assert
	if !loggedError(h.log, opHandover, "were reclaimed and are served here again") {
		t.Fatalf("records = %+v, want the unfinished handover at ERROR", h.log.Records())
	}
	if recordWithMessage(h, opHandover, "info", "the workspace was taken back after its adoption window expired; it is offered to the successor again") {
		t.Fatal("a workspace taken back uncleanly was offered again")
	}
}

// reofferOnce fires the pacing delay expireAdoption left armed and waits for
// the next offer's own adoption window.
func reofferOnce(t *testing.T, h *harness) {
	t.Helper()
	h.clock.Fire(adoptionWindow)
	h.clock.awaitArmed(t, adoptionWindow)
	h.registry.wait()
}

func TestATakenBackWorkspaceIsTransferredAgainAfterItsDelay(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	expireAdoption(t, h)

	// Act
	reofferOnce(t, h)

	// Assert
	var kinds []string
	for _, call := range h.pusher.Calls() {
		if call.WS == ws {
			kinds = append(kinds, call.Kind)
		}
	}
	if len(kinds) != 3 || kinds[2] != "transferred" {
		t.Fatalf("pushes for the workspace = %v, want a fresh transfer notice after the retraction", kinds)
	}
}

func TestATakenBackColdGatedWorkspaceIsOfferedAgain(t *testing.T) {
	// Arrange: a parked shim runs no session, so it never re-announces facts.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.coldGates[ws] = &conversationv1.SessionCold{ContextTokens: 77}
	h.fleet.factsErr[ws] = context.DeadlineExceeded

	// Act
	expireAdoption(t, h)

	// Assert
	if !hasRecord(h, opHandover, "info", "the workspace was taken back after its adoption window expired; it is offered to the successor again") {
		t.Fatalf("records = %+v, want the cold-gated workspace offered again", records(h.log, opHandover))
	}
}

func TestATakenBackColdGatedWorkspaceWaitsForNoSessionFacts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.coldGates[ws] = &conversationv1.SessionCold{ContextTokens: 77}
	h.fleet.factsErr[ws] = context.DeadlineExceeded

	// Act
	expireAdoption(t, h)

	// Assert
	if hasRecord(h, opTransfer, "error", "the re-attached shim never re-announced its session facts; this daemon cannot see its turn in flight, and the workspace is drawn degraded until it does") {
		t.Fatalf("records = %+v, want no wait on facts a parked shim never announces", records(h.log, opTransfer))
	}
}

func TestAReofferThatLandsLetsTheHandoverExit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	expireAdoption(t, h)
	reofferOnce(t, h)
	lease, held, err := h.db.Lease(context.Background(), ws)
	if err != nil || !held {
		t.Fatalf("Lease = (%v, %v), want the re-offer's hold", held, err)
	}

	// Act: the successor adopts the second offer.
	if err := h.db.ClaimServing(context.Background(), ws, "daemon-successor"); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	if err := h.db.ReleaseLease(context.Background(), lease.ID); err != nil {
		t.Fatalf("ReleaseLease: %v", err)
	}

	// Assert
	awaitExit(t, h)
}

func TestEachExpiredReofferDoublesTheWaitBeforeTheNext(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	expireAdoption(t, h)
	reofferOnce(t, h)

	// Act: the second offer's window expires too.
	h.clock.Fire(adoptionWindow)
	h.registry.wait()

	// Assert
	h.clock.awaitArmed(t, 2*adoptionWindow)
}

// TestAWorkspaceTheSuccessorClaimedKeepsItsTransferNotice pins that a reclaim
// the successor's claim won retracts nothing: the workspace did move.
func TestAWorkspaceTheSuccessorClaimedKeepsItsTransferNotice(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	if err := h.db.ClaimServing(context.Background(), ws, ids.InstanceID("daemon-successor")); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}

	// Act
	if _, err := h.c.reclaim(context.Background(), ws, "", true, true, nil, dlog.Context{}); err != nil {
		t.Fatalf("reclaim: %v", err)
	}

	// Assert
	for _, call := range h.pusher.Calls() {
		if call.WS == ws && call.Kind == "retract_transferred" {
			t.Fatalf("pushes = %v, want no retraction of a transfer the successor won", h.pusher.Calls())
		}
	}
}

// TestAWorkspaceTheSuccessorClaimedIsNotTakenBack pins the arbitration: a
// successor that claimed the row keeps the workspace, however late.
func TestAWorkspaceTheSuccessorClaimedIsNotTakenBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	successor := ids.InstanceID("daemon-successor")
	if err := h.db.ClaimServing(context.Background(), ws, successor); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}

	// Act
	reclaimed, err := h.c.reclaim(context.Background(), ws, "", true, true, nil, dlog.Context{})

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
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	// The take-back is done once its re-offer waits out its pacing delay.
	h.clock.awaitArmed(t, adoptionWindow)

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
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	// The take-back is done once its re-offer waits out its pacing delay.
	h.clock.awaitArmed(t, adoptionWindow)

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

func TestATransferTakenBackGivesTheFleetItsWorkspaceBack(t *testing.T) {
	// Arrange: the release fails after HandOver, so the transfer is taken back.
	fail := &atomic.Bool{}
	h := newHarness(t, func(d *Deps) { d.DB = releaseFailingDB{DB: d.DB, fail: fail} })
	ws, _ := h.workspace(t)
	fail.Store(true)

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	// The take-back is done once its re-offer waits out its pacing delay.
	h.clock.awaitArmed(t, adoptionWindow)

	// Assert: a start on it serves it here again.
	h.fleet.mu.Lock()
	reclaimed := append([]ids.WorkspaceID(nil), h.fleet.reclaimed...)
	h.fleet.mu.Unlock()
	if len(reclaimed) != 1 || reclaimed[0] != ws {
		t.Fatalf("reclaimed = %v, want the taken-back workspace given back to the fleet", reclaimed)
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
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
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

// TestTheIncumbentDoesNotExitBeforeTheSuccessorDrainsTheHandedHold pins the
// ordering the successor's adoption rests on: it claims the serving row BEFORE
// it dials the shim and drains the handover hold, so a window that ended at the
// claim let this daemon exit mid-adoption, and its state handle's close
// released the hold under the successor's drain ("wsm: not found"). The window
// now ends only once the hold this transfer handed over is gone.
func TestTheIncumbentDoesNotExitBeforeTheSuccessorDrainsTheHandedHold(t *testing.T) {
	// Arrange
	var ws ids.WorkspaceID
	probe := &servingProbeDB{looks: make(chan struct{}, 64)}
	heldAtExit := make(chan bool, 1)
	h := newHarness(t, func(d *Deps) {
		probe.DB = d.DB
		d.DB = probe
		d.Exit = func(ctx context.Context) error {
			_, held, err := probe.DB.Lease(ctx, ws)
			if err != nil {
				t.Errorf("Lease at the exit: %v", err)
			}
			heldAtExit <- held
			return nil
		}
	})
	ws, _ = h.workspace(t)
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.clock.awaitArmed(t, adoptionWindow)
	hold, held, err := h.db.Lease(context.Background(), ws)
	if err != nil || !held {
		t.Fatalf("Lease after the transfer = (held %v, %v), want the handed quiesce hold", held, err)
	}

	// Act: the successor claims the row, and the window looks at it -- at the
	// claim's own commit -- while the hold it has yet to drain still stands.
	probe.armed.Store(true)
	if err := h.db.ClaimServing(context.Background(), ws, ids.InstanceID("daemon-successor")); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	select {
	case <-probe.looks:
	case held := <-heldAtExit:
		t.Fatalf("the incumbent exited after the successor's claim with the hold standing = %v, want it to wait for the drain", held)
	case <-time.After(10 * time.Second):
		t.Fatalf("the adoption window never looked at the claimed workspace")
	}
	if err := h.db.ReleaseLease(context.Background(), hold.ID); err != nil {
		t.Fatalf("ReleaseLease (the successor's drain): %v", err)
	}

	// Assert
	select {
	case held := <-heldAtExit:
		if held {
			t.Fatalf("the incumbent exited with the handed hold still standing")
		}
	case <-time.After(10 * time.Second):
		t.Fatalf("the incumbent never exited after the successor drained the hold; steps %v", h.order.Taken())
	}
}

// TestAnAdoptionCommittedAsTheWindowFirstLooksStillEndsIt pins the order the
// adoption window follows kernelevent.Follow's contract in: it subscribes to
// the shared state's changes BEFORE its first look. Looking first, a successor
// whose claim and drain committed between that look and the subscription was
// never seen, so the window held the incumbent's exit for its whole length
// (TestWebappLayer/WebappLayerRestartHandover, "daemon did not exit" under a
// loaded merge gate, 2026-11-10).
func TestAnAdoptionCommittedAsTheWindowFirstLooksStillEndsIt(t *testing.T) {
	// Arrange: the successor's claim and drain commit inside the window's
	// first look, after the look has read the row unclaimed.
	probe := &adoptDuringLookDB{successor: ids.InstanceID("daemon-successor")}
	h := newHarness(t, func(d *Deps) {
		probe.DB = d.DB
		d.DB = probe
	})
	h.workspace(t)

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Assert: the commit ends the window, with no expiry fired.
	awaitExit(t, h)
	if !probe.adopted.Load() {
		t.Fatalf("the incumbent exited before the successor adopted")
	}
}

// adoptDuringLookDB commits the successor's adoption, its claim and the drain
// of the handed hold, inside the first serving-ownership read that finds the
// row released with the hold still standing: the adoption window's first
// look. The read still answers what it found before the commit.
type adoptDuringLookDB struct {
	wsm.DB
	successor wsm.InstanceID
	adopted   atomic.Bool
}

func (d *adoptDuringLookDB) Serving(ctx context.Context, ws wsm.WorkspaceID) (*wsm.InstanceID, error) {
	owner, err := d.DB.Serving(ctx, ws)
	if err != nil || owner != nil || d.adopted.Load() {
		return owner, err
	}
	hold, held, err := d.DB.Lease(ctx, ws)
	if err != nil || !held {
		return owner, err
	}
	if err := d.DB.ReleaseLease(ctx, hold.ID); err != nil {
		return nil, err
	}
	if err := d.DB.ClaimServing(ctx, ws, d.successor); err != nil {
		return nil, err
	}
	d.adopted.Store(true)
	return owner, nil
}

// servingProbeDB signals every serving-ownership read once armed, so a test
// knows the adoption window has looked at a claim it made.
type servingProbeDB struct {
	wsm.DB
	armed atomic.Bool
	looks chan struct{}
}

func (d *servingProbeDB) Serving(ctx context.Context, ws wsm.WorkspaceID) (*wsm.InstanceID, error) {
	owner, err := d.DB.Serving(ctx, ws)
	if d.armed.Load() {
		select {
		case d.looks <- struct{}{}:
		default:
		}
	}
	return owner, err
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
	err = h.c.transfer(context.Background(), record, &handoverPlan{successor: "127.0.0.1:7788"}, &windows)
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

func TestAHandoverAsksEveryTransferForTheDispatchQuietGate(t *testing.T) {
	// Arrange: two workspaces, the first busy.
	h := newHarness(t)
	busy, _ := h.workspace(t)
	h.workspace(t)
	h.freeness.SetFree(busy, false)

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert: a handover never waits on work, busy or not.
	requests := h.registry.Requests()
	if len(requests) != 2 {
		t.Fatalf("requests = %+v, want one transfer per workspace", requests)
	}
	for _, call := range requests {
		if call.Req.WaitFor != bounce.GateDispatchQuiet {
			t.Fatalf("transfer of %s waits for %s, want dispatch_quiet", call.WS, call.Req.WaitFor)
		}
	}
}

func TestAForcedHandoverTransfersEveryWorkspaceNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	accepted, err := h.c.HandOver(context.Background(), forcedAsk)
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
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
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
	// Arrange: the transfer fails and its take-back does too.
	h := newHarness(t)
	h.workspace(t)
	failTransferUncleanly(h)

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
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
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *harness, ws ids.WorkspaceID)
	}{
		{
			name: "a session row with no pid and no shim held",
			arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
				if err := h.db.SetShimPID(context.Background(), ws, nil); err != nil {
					t.Fatalf("SetShimPID: %v", err)
				}
				delete(h.fleet.live, ws)
			},
		},
		{
			// The row keeps the pid of a shim that left by a path other than
			// the park: the same dead pids were re-read as preserved at every
			// handover.
			name: "a session row whose pid names a shim the fleet no longer holds",
			arrange: func(_ *testing.T, h *harness, ws ids.WorkspaceID) {
				delete(h.fleet.live, ws)
			},
		},
		{
			name: "a shim the fleet holds that has died",
			arrange: func(_ *testing.T, h *harness, ws ids.WorkspaceID) {
				h.fleet.live[ws].Die()
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, dir := h.workspace(t)
			tc.arrange(t, h, ws)

			// Act
			m := h.c.manifest(context.Background(), "127.0.0.1:1", []wsm.Workspace{{ID: ws, Dir: dir}},
				map[ids.WorkspaceID]Participants{})

			// Assert
			if len(m.Sessions) != 1 {
				t.Fatalf("manifest sessions = %d, want the workspace's entry (it arms the rendezvous)", len(m.Sessions))
			}
			if got := m.Sessions[0]; got.Intent != IntentNoSession || got.ShimPID != 0 {
				t.Fatalf("entry = %+v, want %s with no pid", got, IntentNoSession)
			}
		})
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

// TestHandoverNeverTransfersAClosedWorkspace asserts the defense in depth
// behind wsm.MarkClosed's release: a closed row still naming this daemon (the
// stale record 498b3b658c074bf4 carried, 2026-09-27) is never transferred.
func TestHandoverNeverTransfersAClosedWorkspace(t *testing.T) {
	tests := []struct {
		name         string
		close        bool
		live         bool
		wantTransfer bool
		wantKills    int
	}{
		{name: "an open served workspace is transferred", close: false, live: true, wantTransfer: true, wantKills: 0},
		{name: "a closed row with a stale serving claim is not transferred", close: true, live: false, wantTransfer: false, wantKills: 0},
		{name: "a closed row with a live session is stood down, not transferred", close: true, live: true, wantTransfer: false, wantKills: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			h := newHarness(t)
			ws, _ := h.workspace(t)
			shim := h.fleet.live[ws]
			if tt.close {
				if err := h.db.MarkClosed(ctx, ws, wsm.Closure{At: time.Now(), By: wsm.CloseByUser}); err != nil {
					t.Fatalf("MarkClosed: %v", err)
				}
				// The stale record: a closed row a leaked path left served.
				if err := h.db.ClaimServing(ctx, ws, selfInstance); err != nil {
					t.Fatalf("ClaimServing: %v", err)
				}
			}
			if !tt.live {
				delete(h.fleet.live, ws)
			}
			adoptions := 0
			if tt.wantTransfer {
				adoptions = 1
			}

			// Act
			if err := runHandover(t, h, adoptions); err != nil {
				t.Fatalf("Handover: %v", err)
			}

			// Assert
			transferred := false
			for _, call := range h.registry.Requests() {
				if call.WS == ws && call.Req.Reason == string(ReasonHandoverTransfer) {
					transferred = true
				}
			}
			if transferred != tt.wantTransfer {
				t.Fatalf("transferred = %v, want %v; requests %+v", transferred, tt.wantTransfer, h.registry.Requests())
			}
			if got := len(shim.ForceKills()); got != tt.wantKills {
				t.Fatalf("force kills = %d, want %d", got, tt.wantKills)
			}
		})
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

// DrainedHolds answers the hold each drain was handed, in drain order.
func (h *harness) DrainedHolds() []*ids.LeaseID {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]*ids.LeaseID(nil), h.drainedHolds...)
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
	_, err := h.c.HandOver(context.Background(), unforcedAsk)

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
			_, err := h.c.HandOver(context.Background(), unforcedAsk)

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
			if _, err := h.c.HandOver(context.Background(), unforcedAsk); err == nil {
				t.Fatalf("the first HandOver succeeded through the failure")
			}
			tc.heal(t, h, fail)

			// Act
			_, err := h.c.HandOver(context.Background(), unforcedAsk)

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
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err == nil {
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
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err == nil {
		t.Fatalf("the first HandOver succeeded with a successor that never reported")
	}
	h.spawner.err = nil
	h.spawner.startedThenFailed = false

	// Act
	_, err := h.c.HandOver(context.Background(), unforcedAsk)

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
	_, err := h.c.HandOver(context.Background(), unforcedAsk)

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

// TestATransferSuspendsTheWorkspacesMergeBeforeItsShimMoves pins the merge's
// move: it drives the workspace's own session, so it stops while the shim is
// still this daemon's.
func TestATransferSuspendsTheWorkspacesMergeBeforeItsShimMoves(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	taken := h.order.Taken()
	quiesce, suspend, detach := indexOf(taken, "quiesce"), indexOf(taken, "suspend_merge"), indexOf(taken, "detach")
	if quiesce < 0 || suspend < 0 || detach < 0 || quiesce > suspend || suspend > detach {
		t.Fatalf("steps = %v, want the merge suspended after the quiesce and before the detach", taken)
	}
	if calls := h.merges.called(); len(calls) != 1 || calls[0] != "suspend "+string(ws) {
		t.Fatalf("merge calls = %v, want one suspension of %s", calls, ws)
	}
}

// TestATransferWhoseMergeWillNotSuspendIsTakenBack covers the refusal: a merge
// still driving the session is never handed over under it, and the workspace
// is served here again with its merges taken back.
func TestATransferWhoseMergeWillNotSuspendIsTakenBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	shim := h.fleet.live[ws]
	h.merges.suspendErr = errors.New("the merge did not let go")

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert
	if shim.Detached() {
		t.Fatal("a workspace whose merge would not suspend had its shim detached")
	}
	if calls := h.merges.called(); len(calls) != 2 || calls[1] != "adopt "+string(ws) {
		t.Fatalf("merge calls = %v, want the suspension then this daemon taking the merges back", calls)
	}
	if !loggedError(h.log, opTransfer, "could not suspend the workspace's merge") {
		t.Fatalf("records = %+v, want the refused suspension at ERROR", h.log.Records())
	}
}

// failTransferUncleanly makes every transfer fail and its take-back fail too:
// the merge will not suspend, and this daemon cannot take its merges back.
func failTransferUncleanly(h *harness) {
	h.merges.suspendErr = errors.New("the merge did not let go")
	h.merges.adoptErr = errors.New("the merges could not be taken back")
}

// setSuspendErr sets what the merge suspension answers from now on.
func setSuspendErr(h *harness, err error) {
	h.merges.mu.Lock()
	defer h.merges.mu.Unlock()
	h.merges.suspendErr = err
}

// handOverWithAWholeFailure starts a handover whose one transfer fails with
// the workspace taken back whole, and waits for the re-offer's pacing delay.
func handOverWithAWholeFailure(t *testing.T, h *harness) {
	t.Helper()
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	h.clock.awaitArmed(t, adoptionWindow)
}

// The whole-failure tests are the 2026-10-26 incident: a waiting merge's
// rebase let go of its transfer 0.3s past the bound, the transfer failed with
// the workspace taken back whole, and the handover ended there -- the
// incumbent kept serving it while the successor kept the other 58, and a
// fresh Emacs was refused every workspace. A transfer that fails whole is
// now offered again, as an expired adoption window's is.

func TestATransferWhoseMergeWillNotSuspendIsOfferedAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	setSuspendErr(h, errors.New("the merge did not let go"))

	// Act
	handOverWithAWholeFailure(t, h)

	// Assert
	if !hasRecord(h, opHandover, "info", reofferAfterFailedTransfer) {
		t.Fatalf("records = %+v, want the workspace offered again", records(h.log, opHandover))
	}
}

func TestATransferWhoseQuiesceFailsIsOfferedAgain(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) {
		d.Quiesce = func(context.Context, ids.WorkspaceID) (ids.LeaseID, error) { return "", errFake }
	})
	h.workspace(t)

	// Act
	handOverWithAWholeFailure(t, h)

	// Assert
	if !hasRecord(h, opHandover, "info", reofferAfterFailedTransfer) {
		t.Fatalf("records = %+v, want the workspace offered again", records(h.log, opHandover))
	}
}

func TestATransferThatFailsWholeDoesNotEndTheHandover(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	setSuspendErr(h, errors.New("the merge did not let go"))

	// Act
	handOverWithAWholeFailure(t, h)

	// Assert
	if loggedError(h.log, opHandover, "the handover cannot finish") {
		t.Fatalf("records = %+v, want the handover still converging", h.log.Records())
	}
}

func TestATransferThatFailsWholeIsLoggedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	setSuspendErr(h, errors.New("the merge did not let go"))

	// Act
	handOverWithAWholeFailure(t, h)

	// Assert
	if !loggedErrorWith(h.log, opTransfer, "it is served here whole and is offered to the successor again", "the merge did not let go") {
		t.Fatalf("records = %+v, want the failed transfer at ERROR with its cause", records(h.log, opTransfer))
	}
}

func TestATransferAtFreenessThatFailsWholeIsOfferedAgainAfterALongerWait(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	setSuspendErr(h, errors.New("the merge did not let go"))
	handOverWithAWholeFailure(t, h)

	// Act: the offer at freeness fails whole too.
	h.clock.Fire(adoptionWindow)
	h.registry.wait()

	// Assert
	h.clock.awaitArmed(t, 2*adoptionWindow)
}

func TestATransferOfferedAgainAfterAWholeFailureLandsAndTheHandoverExits(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	setSuspendErr(h, errors.New("the merge did not let go"))
	handOverWithAWholeFailure(t, h)
	setSuspendErr(h, nil)
	reofferOnce(t, h)
	lease, held, err := h.db.Lease(context.Background(), ws)
	if err != nil || !held {
		t.Fatalf("Lease = (%v, %v), want the re-offer's hold", held, err)
	}

	// Act: the successor adopts the second offer.
	if err := h.db.ClaimServing(context.Background(), ws, "daemon-successor"); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	if err := h.db.ReleaseLease(context.Background(), lease.ID); err != nil {
		t.Fatalf("ReleaseLease: %v", err)
	}

	// Assert
	awaitExit(t, h)
}

// workspaceRecords narrows captured records to those written to one
// workspace's own sink under one message.
func workspaceRecords(log *dlog.TestSurfaces, dir, message string) []dlog.Record {
	var out []dlog.Record
	for _, rec := range records(log, opTransfer) {
		if rec.Message == message && rec.Context[dlog.KeyWorkspaceDir] == dir {
			out = append(out, rec)
		}
	}
	return out
}

const (
	handingOverMessage = "a deploy found this daemon's build older, so it is handing the workspace to its successor; both daemons write for the workspace until the transfer lands"
	handedOverMessage  = "the workspace is the successor's now; any later record from this daemon about it is the predecessor winding down, not the workspace's status"
)

func TestATransferTellsTheWorkspacesOwnLogWhyItIsHandedOver(t *testing.T) {
	// Arrange
	h := newHarness(t)
	_, dir := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert: on the workspace's sink, naming the successor by address and pid.
	got := workspaceRecords(h.log, dir, handingOverMessage)
	if len(got) != 1 {
		t.Fatalf("hand-over records on %s = %+v, want exactly one", dir, got)
	}
	if got[0].Context["successor"] != "127.0.0.1:7788" || got[0].Context["successor_pid"] != fakeSuccessorPID {
		t.Fatalf("context = %+v, want the successor's address and pid", got[0].Context)
	}
}

func TestATransferTellsTheWorkspacesOwnLogWhenTheSuccessorOwnsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	_, dir := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if got := workspaceRecords(h.log, dir, handedOverMessage); len(got) != 1 {
		t.Fatalf("handed-over records on %s = %+v, want exactly one", dir, got)
	}
}

func TestAFailedTransferNeverTellsTheWorkspacesLogTheSuccessorOwnsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, dir := h.workspace(t)
	h.fleet.handOverErr[ws] = errors.New("the watches would not close")

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert: the hand-over began, but the workspace stayed here.
	if got := workspaceRecords(h.log, dir, handingOverMessage); len(got) != 1 {
		t.Fatalf("hand-over records = %+v, want the attempt recorded", got)
	}
	if got := workspaceRecords(h.log, dir, handedOverMessage); len(got) != 0 {
		t.Fatalf("handed-over records = %+v, want none for a transfer that failed", got)
	}
}

func TestARestartWritesNoHandOverRecordToTheWorkspacesLog(t *testing.T) {
	// Arrange: a restart has no successor to hand the workspace to.
	h := newHarness(t)
	_, dir := h.workspace(t)

	// Act
	runRestart(t, h)

	// Assert
	if got := workspaceRecords(h.log, dir, handingOverMessage); len(got) != 0 {
		t.Fatalf("hand-over records = %+v, want none for a restart", got)
	}
}

func TestRendezvousClosed(t *testing.T) {
	closed := make(chan struct{})
	close(closed)
	tests := []struct {
		name string
		done <-chan struct{}
		want bool
	}{
		{name: "a closed rendezvous", done: closed, want: true},
		{name: "an open rendezvous", done: make(chan struct{}), want: false},
		{name: "no rendezvous", done: nil, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := rendezvousClosed(tc.done)

			// Assert
			if got != tc.want {
				t.Fatalf("rendezvousClosed = %v, want %v", got, tc.want)
			}
		})
	}
}

// manifestSessionsDB counts the manifest's session reads and can fail the bulk
// one.
type manifestSessionsDB struct {
	wsm.DB
	single, bulk *int
	err          error
}

func (d manifestSessionsDB) Session(ctx context.Context, ws ids.WorkspaceID) (wsm.Session, bool, error) {
	*d.single++
	return d.DB.Session(ctx, ws)
}

func (d manifestSessionsDB) Sessions(ctx context.Context) ([]wsm.Session, error) {
	*d.bulk++
	if d.err != nil {
		return nil, d.err
	}
	return d.DB.Sessions(ctx)
}

// TestTheManifestReadsEverySessionAtOnce pins the manifest onto the shared
// bulk index: it is written at the handover's busiest instant, when a read per
// workspace queued each behind every other reader.
func TestTheManifestReadsEverySessionAtOnce(t *testing.T) {
	// Arrange
	var single, bulk int
	h := newHarness(t)
	h.c.deps.DB = manifestSessionsDB{DB: h.c.deps.DB, single: &single, bulk: &bulk}
	var workspaces []wsm.Workspace
	for range 3 {
		ws, dir := h.workspace(t)
		workspaces = append(workspaces, wsm.Workspace{ID: ws, Dir: dir})
	}

	// Act
	m := h.c.manifest(context.Background(), "127.0.0.1:1", workspaces, map[ids.WorkspaceID]Participants{})

	// Assert
	if bulk != 1 || single != 0 {
		t.Fatalf("session reads: bulk = %d, single = %d; want one bulk read and no single reads", bulk, single)
	}
	for _, entry := range m.Sessions {
		if entry.Intent != IntentPreserve {
			t.Fatalf("manifest sessions = %+v, want every live shim preserved", m.Sessions)
		}
	}
}

// TestAManifestWhoseSessionReadFailsRecordsItOnce covers the read's failure:
// recorded at ERROR once for the whole manifest, and no entry claims a
// session the daemon could not read.
func TestAManifestWhoseSessionReadFailsRecordsItOnce(t *testing.T) {
	// Arrange
	var single, bulk int
	h := newHarness(t)
	h.c.deps.DB = manifestSessionsDB{DB: h.c.deps.DB, single: &single, bulk: &bulk, err: errFake}
	var workspaces []wsm.Workspace
	for range 2 {
		ws, dir := h.workspace(t)
		workspaces = append(workspaces, wsm.Workspace{ID: ws, Dir: dir})
	}

	// Act
	m := h.c.manifest(context.Background(), "127.0.0.1:1", workspaces, map[ids.WorkspaceID]Participants{})

	// Assert
	if len(m.Sessions) != len(workspaces) {
		t.Fatalf("manifest sessions = %d, want an entry per workspace (each arms the rendezvous)", len(m.Sessions))
	}
	// The live shims are the fleet's to say, so each is still preserved; the
	// unread session lends no entry its vendor session.
	for _, entry := range m.Sessions {
		if entry.Intent != IntentPreserve || entry.VendorSessionID != "" {
			t.Fatalf("manifest sessions = %+v, want each live shim preserved and no entry claiming an unread session", m.Sessions)
		}
	}
	var failures int
	for _, rec := range h.log.Records() {
		if rec.Level == "error" && rec.Operation == opManifest && rec.Message == "could not read the sessions for the intent manifest" {
			failures++
			if rec.Context["workspaces"] != len(workspaces) {
				t.Fatalf("failure context = %+v, want the workspace count", rec.Context)
			}
		}
	}
	if failures != 1 {
		t.Fatalf("manifest read failures recorded = %d, want exactly one; records = %+v", failures, h.log.Records())
	}
}

// THE TAKE-BACK CYCLE IS BOUNDED. On 2026-10-30 one workspace was taken back
// and offered again six times, and neither daemon could finish. After
// maxReoffers offers the incumbent surrenders it session-less, records the
// end of the cycle as the workspace's fault, and exits.
func TestATakeBackCycleEndsInASurrenderAfterItsLastOffer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	expireAdoption(t, h)

	// Act: every offer's window expires in turn.
	pacing := adoptionWindow
	for offer := 1; offer <= maxReoffers; offer++ {
		h.clock.Fire(pacing)
		h.clock.awaitArmed(t, adoptionWindow)
		h.registry.wait()
		h.clock.Fire(adoptionWindow)
		h.registry.wait()
		if offer < maxReoffers {
			pacing *= 2
			h.clock.awaitArmed(t, pacing)
		}
	}

	// Assert
	awaitExit(t, h)
	faults, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultAdoptionExpired})
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	surrendered := false
	for _, f := range faults {
		if f.Evidence["disposition"] == "surrendered_sessionless" && f.Evidence["offers"] == fmt.Sprint(maxReoffers) {
			surrendered = true
		}
	}
	if !surrendered {
		t.Fatalf("faults = %+v, want the cycle's end recorded with its offers", faults)
	}
	if !loggedError(h.log, opHandover, "the successor did not adopt the workspace in every offer; standing its session down and handing it over without one") {
		t.Fatalf("records = %+v, want the surrender stated at ERROR", h.log.Records())
	}
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil || owner != nil {
		t.Fatalf("Serving = (%v, %v), want the row released for the successor", owner, err)
	}
}

// openAdoptionFault is the one writer of the workspace's adoption fault, for
// an expired window and for a surrendered take-back cycle alike.
func TestOpenAdoptionFaultRecordsTheWorkspacesFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	h.c.openAdoptionFault(context.Background(), ws, opHandover, "the detail", map[string]string{"k": "v"}, dlog.Context{})

	// Assert
	faults, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultAdoptionExpired})
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(faults) != 1 || faults[0].Detail != "the detail" || faults[0].Evidence["k"] != "v" {
		t.Fatalf("faults = %+v, want the one fault with its detail and evidence", faults)
	}
}

func TestOpenAdoptionFaultThatCannotBeWrittenSaysSoAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if err := h.db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	h.c.openAdoptionFault(context.Background(), ws, opHandover, "the detail", nil, dlog.Context{"workspace": string(ws)})

	// Assert
	if !loggedError(h.log, opHandover, "could not record the workspace's failed adoption") {
		t.Fatalf("records = %+v, want the failed write stated at ERROR", h.log.Records())
	}
}

// A WORKSPACE TAKEN BACK WHOLE IS OFFERED AGAIN AT THE HANDOVER'S OWN GATE.
// Offered at freeness it waited for every lease, and a merge held by a
// hard-stopped queue never let go (2026-11-10: a handover held open for over
// an hour by one such workspace).
func TestATakenBackWorkspaceIsOfferedAgainAtDispatchQuiet(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	expireAdoption(t, h)

	// Act
	reofferOnce(t, h)

	// Assert
	h.registry.mu.Lock()
	defer h.registry.mu.Unlock()
	var offers []bounce.Gate
	for _, call := range h.registry.requests {
		if call.WS == ws && call.Req.Reason == string(ReasonHandoverTransfer) {
			offers = append(offers, call.Req.WaitFor)
		}
	}
	if len(offers) != 2 || offers[1] != bounce.GateDispatchQuiet {
		t.Fatalf("offers = %v, want the first transfer and its re-offer, both at dispatch quiet", offers)
	}
}

// ---- the rollout order and the page loads (owner ruling, 2026-11-13) -------

// foldRepositoryOf folds the sidebar section of WS's repository.
func foldRepositoryOf(t *testing.T, h *harness, ws ids.WorkspaceID) {
	t.Helper()
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if err := h.db.SetRepositoryFolded(context.Background(), record.Repo, true); err != nil {
		t.Fatalf("SetRepositoryFolded: %v", err)
	}
}

// foldSubsectionOf files WS in a subsection of its repository and folds it.
func foldSubsectionOf(t *testing.T, h *harness, ws ids.WorkspaceID) {
	t.Helper()
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	sub, err := h.db.CreateSubsection(context.Background(), record.Repo, "folded")
	if err != nil {
		t.Fatalf("CreateSubsection: %v", err)
	}
	if err := h.db.PlaceWorkspaces(context.Background(), []ids.WorkspaceID{ws}, &sub.ID, wsm.AtEnd{}); err != nil {
		t.Fatalf("PlaceWorkspaces: %v", err)
	}
	if err := h.db.SetSubsectionFolded(context.Background(), sub.ID, true); err != nil {
		t.Fatalf("SetSubsectionFolded: %v", err)
	}
}

func TestFoldedRows(t *testing.T) {
	tests := []struct {
		name string
		fold func(t *testing.T, h *harness, ws ids.WorkspaceID)
		want bool
	}{
		{name: "a workspace in an unfolded repository and no subsection is unfolded", fold: func(*testing.T, *harness, ids.WorkspaceID) {}, want: false},
		{name: "a workspace whose repository is folded is folded", fold: foldRepositoryOf, want: true},
		{name: "a workspace whose subsection is folded is folded", fold: foldSubsectionOf, want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			tc.fold(t, h, ws)
			record, err := h.db.Workspace(context.Background(), ws)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}

			// Act
			got, err := h.c.foldedRows(context.Background(), []wsm.Workspace{record})

			// Assert
			if err != nil || got[ws] != tc.want {
				t.Fatalf("foldedRows = (%v, %v), want folded %v", got, err, tc.want)
			}
		})
	}
}

func TestUnfoldedFirst(t *testing.T) {
	a, b, c, d := wsm.Workspace{ID: "a"}, wsm.Workspace{ID: "b"}, wsm.Workspace{ID: "c"}, wsm.Workspace{ID: "d"}
	tests := []struct {
		name   string
		folded map[ids.WorkspaceID]bool
		want   []ids.WorkspaceID
	}{
		{name: "nothing folded keeps the order", folded: nil, want: []ids.WorkspaceID{"a", "b", "c", "d"}},
		{name: "the folded follow every unfolded, each group in order", folded: map[ids.WorkspaceID]bool{"a": true, "c": true}, want: []ids.WorkspaceID{"b", "d", "a", "c"}},
		{name: "everything folded keeps the order", folded: map[ids.WorkspaceID]bool{"a": true, "b": true, "c": true, "d": true}, want: []ids.WorkspaceID{"a", "b", "c", "d"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := unfoldedFirst([]wsm.Workspace{a, b, c, d}, tc.folded)

			// Assert
			var names []ids.WorkspaceID
			for _, ws := range got {
				names = append(names, ws.ID)
			}
			if !slices.Equal(names, tc.want) {
				t.Fatalf("order = %v, want %v", names, tc.want)
			}
		})
	}
}

func TestAFoldedWorkspaceIsTransferredOnlyOnceEveryUnfoldedTransferHasRun(t *testing.T) {
	// Arrange: the folded workspace is registered FIRST, so only the fold
	// puts it second.
	h := newHarness(t)
	folded, _ := h.workspace(t)
	unfolded, _ := h.workspace(t)
	foldRepositoryOf(t, h, folded)
	var foldedAskedAtUnfoldedSeal []bool
	h.registry.onSeal = func(ws ids.WorkspaceID) {
		if ws != unfolded {
			return
		}
		asked := false
		for _, call := range h.registry.requests {
			asked = asked || call.WS == folded
		}
		foldedAskedAtUnfoldedSeal = append(foldedAskedAtUnfoldedSeal, asked)
	}

	// Act
	handOverUntilWindow(t, h)
	h.registry.awaitRequest(t, func(c registryCall) bool { return c.WS == folded })

	// Assert
	h.registry.mu.Lock()
	defer h.registry.mu.Unlock()
	if len(foldedAskedAtUnfoldedSeal) != 1 || foldedAskedAtUnfoldedSeal[0] {
		t.Fatalf("folded asked at the unfolded transfer's seal = %v, want asked only after it", foldedAskedAtUnfoldedSeal)
	}
}

func TestAFoldedTransferDoesNotWaitOnAnUnfoldedOneRegisteredBehindItsWork(t *testing.T) {
	// Arrange: every request is registered unrun, as one behind its work is.
	h := newHarness(t)
	folded, _ := h.workspace(t)
	unfolded, _ := h.workspace(t)
	foldRepositoryOf(t, h, folded)
	h.registry.park = true

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	call := h.registry.awaitRequest(t, func(c registryCall) bool { return c.WS == folded })

	// Assert
	if call.WS != folded || !h.registry.Pending(unfolded) {
		t.Fatalf("requests %+v, want the folded transfer asked while the unfolded one waits", h.registry.Requests())
	}
}

func TestTheTransferNoticeSaysWhetherTheHandoverLoadsThePage(t *testing.T) {
	tests := []struct {
		name string
		web  bool
		fold func(t *testing.T, h *harness, ws ids.WorkspaceID)
		want bool
	}{
		{name: "a page loaded at the announcement, unfolded, is reloaded", web: true, fold: func(*testing.T, *harness, ids.WorkspaceID) {}, want: true},
		{name: "no page loaded at the announcement is never loaded", web: false, fold: func(*testing.T, *harness, ids.WorkspaceID) {}, want: false},
		{name: "a folded workspace's page is never loaded", web: true, fold: foldRepositoryOf, want: false},
		{name: "a workspace in a folded subsection has its page never loaded", web: true, fold: foldSubsectionOf, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			h.participants.Set(ws, Participants{Host: true, Web: tc.web})
			tc.fold(t, h, ws)

			// Act
			handOverUntilWindow(t, h)

			// Assert
			var got []bool
			for _, call := range h.pusher.Calls() {
				if call.WS == ws && call.Kind == "transferred" {
					got = append(got, call.ReloadPage)
				}
			}
			if len(got) != 1 || got[0] != tc.want {
				t.Fatalf("transferred reload_page = %v, want [%v]", got, tc.want)
			}
		})
	}
}

func TestAHandoverWhoseFoldsCannotBeReadIsAbandonedBeforeTheAnnouncement(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.db = arrangementFailing{DB: h.db}
	h.c.deps.DB = h.db

	// Act
	_, err := h.c.HandOver(context.Background(), unforcedAsk)

	// Assert
	if err == nil {
		t.Fatalf("HandOver succeeded, want it abandoned")
	}
	if !hasRecord(h, opHandover, "error", "could not read which sidebar rows are folded; the handover is abandoned before anything was announced") {
		t.Fatalf("records = %+v, want the abandonment at ERROR", records(h.log, opHandover))
	}
	if len(h.announcer.Sent()) != 0 {
		t.Fatalf("the stand-down was announced, want nothing announced")
	}
}

// arrangementFailing is a state handle whose arrangement read fails.
type arrangementFailing struct{ wsm.DB }

func (arrangementFailing) ReadArrangement(context.Context) (wsm.Arrangement, error) {
	return wsm.Arrangement{}, errors.New("the arrangement read failed")
}

// yieldsInto wires the boot claim's yield to count on YIELDED, failing with
// ERR when set.
func yieldsInto(yielded chan<- struct{}, err error) func(d *Deps) {
	return func(d *Deps) {
		d.YieldBootClaim = func() error {
			yielded <- struct{}{}
			return err
		}
	}
}

func TestAHandoverYieldsTheBootClaimOnceItsTransferPassIsOver(t *testing.T) {
	// Arrange: one workspace, whose adoption window the yield never waits on.
	yielded := make(chan struct{}, 1)
	h := newHarness(t, yieldsInto(yielded, nil))
	h.workspace(t)

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	<-yielded

	// Assert: the yield comes before this daemon's exit, and is said.
	select {
	case <-h.exits:
		t.Fatal("the daemon exited before it yielded; the yield must not wait for the handover's end")
	default:
	}
	awaitRecord(t, h, opHandover, "the transfer pass is over: the successor takes over now, and this daemon serves what is still busy here until each workspace is free to move")
}

func TestAHandoverWhoseYieldFailsSaysSo(t *testing.T) {
	// Arrange
	yielded := make(chan struct{}, 1)
	h := newHarness(t, yieldsInto(yielded, errors.New("the boot lock could not be released")))
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	<-yielded

	// Assert
	awaitRecord(t, h, opHandover, "the transfer pass is over but the boot claim could not be yielded; the successor takes over at this daemon's exit")
}

func TestARestartNeverYieldsTheBootClaim(t *testing.T) {
	// Arrange
	yielded := make(chan struct{}, 1)
	h := newHarness(t, yieldsInto(yielded, nil))
	h.workspace(t)

	// Act
	runRestart(t, h)

	// Assert
	select {
	case <-yielded:
		t.Fatal("a restart yielded the boot claim; its replacement takes it at the exit")
	default:
	}
}

// announced records each move announced to a newer daemon.
type announced struct {
	mu    sync.Mutex
	moves []string
	err   error
}

// newestIs wires daemon.addr to name NEWEST, and the announcements into A.
func newestIs(newest string, a *announced) func(d *Deps) {
	return func(d *Deps) {
		d.NewestDaemon = func() (string, error) { return newest, nil }
		d.AnnounceHanded = func(ctx context.Context, address string, ws wsm.Workspace, outgoing ids.InstanceID, expected Participants) error {
			a.mu.Lock()
			defer a.mu.Unlock()
			a.moves = append(a.moves, fmt.Sprintf("%s->%s by %s", ws.ID, address, outgoing))
			return a.err
		}
	}
}

func TestAMoveGoesToTheNewestDaemon(t *testing.T) {
	tests := []struct {
		name         string
		newest       string
		wantAnnounce bool
		wantTo       string
	}{
		{name: "a newer daemon took over since the handover began", newest: "127.0.0.1:9999", wantAnnounce: true, wantTo: "127.0.0.1:9999"},
		{name: "the advertisement still names this daemon", newest: "127.0.0.1:7777"},
		{name: "the advertisement names no daemon", newest: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			a := &announced{}
			h := newHarness(t, newestIs(tt.newest, a))
			ws, _ := h.workspace(t)

			// Act
			handOverUntilWindow(t, h)

			// Assert: the move is told to the newest only when it is newer.
			a.mu.Lock()
			moves := slices.Clone(a.moves)
			a.mu.Unlock()
			if tt.wantAnnounce != (len(moves) == 1) {
				t.Fatalf("announcements = %v, want one %v", moves, tt.wantAnnounce)
			}
			want := tt.wantTo
			if want == "" {
				want = h.spawner.address
			}
			var to string
			for _, c := range h.pusher.Calls() {
				if c.WS == ws && c.Kind == "transferred" {
					to = c.Address
				}
			}
			if to != want {
				t.Fatalf("transferred to %q, want %q", to, want)
			}
		})
	}
}

func TestAMoveTheNewestDaemonCannotBeToldOfStaysServedHere(t *testing.T) {
	// Arrange
	a := &announced{err: ErrNotNewest}
	h := newHarness(t, newestIs("127.0.0.1:9999", a))
	ws, _ := h.workspace(t)

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Assert: nothing of the workspace moved, and the failure is said.
	awaitRecord(t, h, opTransfer, "the newest daemon could not be told of the move; the workspace stays served here and is offered again")
	for _, c := range h.pusher.Calls() {
		if c.WS == ws && c.Kind == "transferred" {
			t.Fatalf("pushes = %+v, want no transfer announced", h.pusher.Calls())
		}
	}
}

func TestAcceptHandedArmsTheRendezvousForTheMove(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	err := h.c.AcceptHanded(context.Background(), ws, "elder", Participants{Host: true})

	// Assert: the workspace is the elder's until it is adopted here.
	if err != nil {
		t.Fatalf("AcceptHanded = %v", err)
	}
	h.c.mu.Lock()
	e, armed := h.c.rendezvous[ws]
	h.c.unlock()
	if !armed || e.outgoing != "elder" || !e.expected.Host || e.expected.Web {
		t.Fatalf("rendezvous = %+v (armed %v), want it armed from elder expecting the host", e, armed)
	}
	if h.c.ServesWorkspace(ws) || h.c.Standing(ws) != StandingNotYetAdopted {
		t.Fatalf("serves %v, standing %v; want the workspace not yet adopted", h.c.ServesWorkspace(ws), h.c.Standing(ws))
	}
}

func TestAcceptHandedRefusesWhileThisDaemonHandsOverItself(t *testing.T) {
	// Arrange: this daemon hands over to a newer one.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Act
	err := h.c.AcceptHanded(context.Background(), ws, "elder", Participants{})

	// Assert
	if !errors.Is(err, ErrNotNewest) {
		t.Fatalf("AcceptHanded = %v, want ErrNotNewest", err)
	}
}

func TestAHandoverWithASharedAddressTellsClientsOnlyTheSharedAddress(t *testing.T) {
	// Arrange: the daemons share one address through their socket holder.
	h := newHarness(t)
	h.sharedAddress = "127.0.0.1:41000"
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert: the announcement and every transfer push name the shared
	// address, and the forwarding target is still the successor's own.
	sent := h.announcer.Sent()
	if len(sent) != 1 || sent[0].GetAddress() != "127.0.0.1:41000" {
		t.Fatalf("announcements = %v, want the shared address", sent)
	}
	pushed := 0
	for _, call := range h.pusher.Calls() {
		if call.Kind != "transferred" {
			continue
		}
		pushed++
		if call.Address != "127.0.0.1:41000" {
			t.Fatalf("a transfer push named %q, want the shared address", call.Address)
		}
	}
	if pushed == 0 {
		t.Fatal("no transfer was pushed")
	}
	if got := h.c.SuccessorAddress(); got != "127.0.0.1:7788" {
		t.Fatalf("the forwarding target = %q, want the successor's own address", got)
	}
}

func TestNewRefusesWithoutTheSharedAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.c.deps
	deps.SharedAddress = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "shared address") {
		t.Fatalf("New = %v, want the missing shared address refused", err)
	}
}
