package rollout

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"

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

// TestEveryExpectedParticipantSucceedsTogether pins the rendezvous's meaning:
// the participants call CONCURRENTLY and "all calls succeed together", so the
// caller that arrives first waits for the one that completes it rather than
// being told not_yet_adopted. That answer is reserved for a caller whose own
// context expires first, which the next test covers.
func TestEveryExpectedParticipantSucceedsTogether(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true, Web: true})

	// Act
	var wg sync.WaitGroup
	var hostErr, webErr error
	wg.Add(2)
	go func() { defer wg.Done(); hostErr = h.c.AdoptHost(context.Background(), ws) }()
	go func() { defer wg.Done(); webErr = h.c.AdoptWeb(context.Background(), ws) }()
	wg.Wait()

	// Assert
	if hostErr != nil {
		t.Fatalf("AdoptHost answered %v, want success", hostErr)
	}
	if webErr != nil {
		t.Fatalf("AdoptWeb answered %v, want success", webErr)
	}
}

// TestAnAdoptCallerThatGivesUpFirstAnswersNotYetAdopted covers the one case
// the arm is for: the other participant has not called, and this caller's own
// context expired. It is the retry-with-backoff answer.
func TestAnAdoptCallerThatGivesUpFirstAnswersNotYetAdopted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true, Web: true})
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the host calls and nothing else ever will.
	done := make(chan error, 1)
	go func() { done <- h.c.AdoptHost(ctx, ws) }()
	cancel()

	// Assert
	select {
	case err := <-done:
		if !errors.Is(err, ErrNotYetAdopted) {
			t.Fatalf("AdoptHost answered %v, want not_yet_adopted", err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("AdoptHost never answered after its context was cancelled")
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

// TestAdoptionOfAFreeLockDialsNoShim pins the never-opened workspace: its lock
// is free because no process was ever spawned for it, so it transfers on its
// WSM facts alone. Dialing a shim that does not exist would fail the adoption
// of a workspace in no trouble at all, and the incumbent would then wait out an
// adoption window for it.
func TestAdoptionOfAFreeLockDialsNoShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, dir := h.workspace(t)
	h.mu.Lock()
	h.lockStates[dir] = sessionlock.StateFree
	h.mu.Unlock()

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("shim adoptions = %v, want none: no process holds the lock", got)
	}
	if warns := levelRecords(records(h.log, opAdopt), "warn"); len(warns) != 0 {
		t.Fatalf("adoption warnings = %v, want none: a never-opened workspace is an ordinary transfer", warns)
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

func TestASecondManifestReadDoesNotReArmAnAdoptedRendezvous(t *testing.T) {
	// Arrange — one host participant adopts the workspace, then the joining
	// daemon reads the same stand-down manifest again, as its awaiting poll
	// does when the manifest was already on disk.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true})
	if err := h.c.AdoptHost(context.Background(), ws); err != nil {
		t.Fatalf("AdoptHost: %v", err)
	}

	// Act
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join (second manifest read): %v", err)
	}

	// Assert — a recovered page's own boot adopt succeeds at once against the
	// workspace that is already adopted.
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	if err := h.c.AdoptWeb(ctx, ws); err != nil {
		t.Fatalf("AdoptWeb after a second manifest read = %v, want the adopted workspace to succeed at once", err)
	}
}

func TestASecondManifestReadKeepsAPartialRendezvousLedger(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true, Web: true})
	h.c.mu.Lock()
	first := h.c.rendezvous[ws]
	first.hostCalled = true
	h.c.mu.Unlock()

	// Act
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join (second manifest read): %v", err)
	}

	// Assert
	h.c.mu.Lock()
	again := h.c.rendezvous[ws]
	h.c.mu.Unlock()
	if again != first || !again.hostCalled {
		t.Fatalf("rendezvous entry was re-armed (same=%v, host_called=%v), want the ledger kept", again == first, again.hostCalled)
	}
}

// writeHeadlessManifest lays down an intent manifest naming one workspace that
// nobody holds the streams of, WITHOUT joining from it.
func writeHeadlessManifest(t *testing.T, h *harness, ws ids.WorkspaceID) {
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
			Workspace: ws, Dir: record.Dir, ShimPID: 4242,
			VendorSessionID: "vendor-1", Intent: IntentPreserve,
		}},
	}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}
}

// TestHeadlessWorkspaceArmedByAnEarlierReadIsStillAdopted pins the case that
// stalled a handover: a participant's own adopt call re-reads the manifest and
// arms every session in it, so the joining poll that follows adds nothing. A
// headless workspace has NO participant to adopt it, so if the join skipped
// what it did not itself arm, nobody ever adopted it and the outgoing daemon
// waited out the entire adoption window.
func TestHeadlessWorkspaceArmedByAnEarlierReadIsStillAdopted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	writeHeadlessManifest(t, h, ws)
	if err := h.c.armFromManifest(); err != nil {
		t.Fatalf("armFromManifest: %v", err)
	}

	// Act
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 1 || got[0] != ws {
		t.Fatalf("adoptions = %v, want the headless workspace adopted despite the earlier arming", got)
	}
}

// TestHeadlessWorkspaceIsAdoptedOnceAcrossTwoReads pins the other side: the
// manifest is read more than once by design, and each read must not re-adopt
// what an earlier one already took.
func TestHeadlessWorkspaceIsAdoptedOnceAcrossTwoReads(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	writeHeadlessManifest(t, h, ws)
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}

	// Act
	if _, err := h.c.joinFromManifest(context.Background()); err != nil {
		t.Fatalf("joinFromManifest: %v", err)
	}

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 1 {
		t.Fatalf("adoptions = %v, want exactly one across the two manifest reads", got)
	}
}
