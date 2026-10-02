package rollout

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"slices"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/wsm"
)

// writeHandoverManifest writes the incumbent's intent manifest naming one
// workspace with the given expected participants. It is separate from `arm`
// because the manifest's ARRIVAL is an edge in its own right: the incumbent
// announces the successor's address before it writes the manifest, so a test
// that forces that ordering has to place the write itself.
func writeHandoverManifest(t *testing.T, h *harness, ws ids.WorkspaceID, expected Participants) {
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
}

// arm writes an intent manifest naming one workspace with the given expected
// participants, then Joins — the joining daemon's whole boot half.
func arm(t *testing.T, h *harness, ws ids.WorkspaceID, expected Participants) {
	t.Helper()
	writeHandoverManifest(t, h, ws, expected)
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

func TestAdoptionWaitsForTheIncumbentServingRelease(t *testing.T) {
	tests := []struct {
		name     string
		outgoing ids.InstanceID
	}{
		{name: "an early participant call waits on the serving latch", outgoing: "daemon-outgoing-previous"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: the participant calls the successor while the manifest's
			// incumbent still owns the workspace, exactly as a shutdown
			// announcement can race a busy incumbent's freeness gate.
			h := newHarness(t)
			ws, _ := h.workspace(t)
			arm(t, h, ws, Participants{Host: true})
			if err := h.db.ClaimServing(context.Background(), ws, test.outgoing); err != nil {
				t.Fatalf("ClaimServing(outgoing): %v", err)
			}

			// Act: call adoption before the incumbent releases its serving
			// claim.
			done := make(chan error, 1)
			go func() { done <- h.c.AdoptHost(context.Background(), ws) }()
			h.clock.awaitArmed(t, manifestPoll)

			// Assert: no shim adoption starts on timing alone. It begins only
			// after the serving row becomes the durable released latch and the
			// test advances the controller's observation clock.
			if got := h.fleet.Adoptions(); len(got) != 0 {
				t.Fatalf("adoptions before serving release = %v, want none", got)
			}
			if err := h.db.ReleaseServing(context.Background(), ws, test.outgoing); err != nil {
				t.Fatalf("ReleaseServing(outgoing): %v", err)
			}
			h.clock.Fire(manifestPoll)
			if err := <-done; err != nil {
				t.Fatalf("AdoptHost after serving release: %v", err)
			}
			if got := h.fleet.Adoptions(); len(got) != 1 || got[0] != ws {
				t.Fatalf("adoptions after serving release = %v, want %q once", got, ws)
			}
			infos := levelRecords(records(h.log, opAdopt), "info")
			waits := 0
			for _, record := range infos {
				if record.Message == "waiting for the incumbent to release serving ownership at freeness" {
					waits++
				}
			}
			if waits != 1 {
				t.Fatalf("adoption INFO records = %+v, want the serving-release wait named once", infos)
			}
		})
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

// TestAdoptionClaimsServingBeforeDialingTheShim pins the arbitration order
// (invariant D): the incumbent's reclaim of an expired window and this
// adoption race for one released row, and only the side whose claim stood may
// dial the shim -- so the claim is taken first. It replaces the earlier pin
// of the opposite order, which let both daemons hold a client for one shim.
func TestAdoptionClaimsServingBeforeDialingTheShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	var ownerAtDial *ids.InstanceID
	h.fleet.onAdopt = func(adopted ids.WorkspaceID) {
		owner, err := h.db.Serving(context.Background(), adopted)
		if err != nil {
			t.Errorf("Serving: %v", err)
		}
		ownerAtDial = owner
	}

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if ownerAtDial == nil || *ownerAtDial != selfInstance {
		t.Fatalf("serving owner when the shim was dialed = %v, want this daemon's claim already standing", ownerAtDial)
	}
}

// servingBlindDB answers every serving read as released, so an adoption
// passes its wait for the incumbent's release while the row itself names the
// incumbent: the instant between that wait and the claim, frozen.
type servingBlindDB struct{ wsm.DB }

func (servingBlindDB) Serving(context.Context, wsm.WorkspaceID) (*wsm.InstanceID, error) {
	return nil, nil
}

func TestAnAdoptionTheIncumbentTookBackDialsNothing(t *testing.T) {
	// Arrange: the incumbent took the workspace back after the adoption saw
	// it released.
	h := newHarness(t, func(d *Deps) { d.DB = servingBlindDB{DB: d.DB} })
	ws, _ := h.workspace(t)
	if err := h.db.ClaimServing(context.Background(), ws, ids.InstanceID("daemon-outgoing-previous")); err != nil {
		t.Fatalf("ClaimServing(incumbent): %v", err)
	}

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want no dial of a shim the incumbent took back", got)
	}
	if got := h.Drained(); len(got) != 0 {
		t.Fatalf("drained = %v, want the incumbent's hold left to the incumbent", got)
	}
	if !loggedError(h.log, opAdopt, "the incumbent took the workspace back") {
		t.Fatalf("records = %+v, want the lost claim at ERROR", h.log.Records())
	}
}

func TestAnAdoptionWhoseShimWillNotAnswerStillDrainsTheIntake(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.adoptErr[ws] = errFake

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.Drained(); len(got) != 1 || got[0] != ws {
		t.Fatalf("drained = %v, want the claimed workspace's hold drained though its shim did not answer", got)
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

// ---------------------------------------------------------------------------
// A headless adopt that FAILS is retried.
// ---------------------------------------------------------------------------

// setAdoptErr makes the fleet's next Adopt of WS fail with ERR, or succeed when
// ERR is nil. It lives here rather than in the shared harness because the retry
// is the only thing that turns an adopt failure on and off mid-test.
func setAdoptErr(f *fakeFleet, ws ids.WorkspaceID, err error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if err == nil {
		delete(f.adoptErr, ws)
		return
	}
	f.adoptErr[ws] = err
}

// harnessSignallingAdoptions is a harness that announces every completed
// adoption on ADOPTED.
//
// PUBLISHVIEWS IS THE COMPLETION EDGE. It is the last thing `adopt` does before
// it marks the workspace owned, so a test synchronizes on it rather than
// polling the fleet's adoption list.
func harnessSignallingAdoptions(t *testing.T, adopted chan<- ids.WorkspaceID) *harness {
	t.Helper()
	return newHarness(t, func(d *Deps) {
		inner := d.PublishViews
		d.PublishViews = func(ctx context.Context, published ids.WorkspaceID) error {
			err := inner(ctx, published)
			adopted <- published
			return err
		}
	})
}

// TestHeadlessAdoptFailureIsRetriedUntilItAdopts is the defect this retry
// exists for.
//
// A headless workspace has no participant whose own adopt call would come
// round, `awaitManifest` stops the moment a manifest is read, and
// `armFromManifest` arms without adopting — so before this retry a SINGLE
// transient failure stranded the workspace on the incumbent, which then waited
// out its whole 30s adoption window. Observed as a handover that took 32s and
// 42s under load against ~2s healthy.
func TestHeadlessAdoptFailureIsRetriedUntilItAdopts(t *testing.T) {
	// Arrange: the first adopt fails; the workspace is otherwise ordinary.
	adopted := make(chan ids.WorkspaceID, 4)
	h := harnessSignallingAdoptions(t, adopted)
	ws, _ := h.workspace(t)
	setAdoptErr(h.fleet, ws, errors.New("the shim was not reachable yet"))
	writeHeadlessManifest(t, h, ws)
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)

	// Act: the join adopts nothing, and the retry window opens.
	if err := h.c.Join(ctx); err != nil {
		t.Fatalf("Join: %v", err)
	}
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want none while the adopt is failing", got)
	}
	h.clock.awaitArmed(t, headlessRetryInitial)
	setAdoptErr(h.fleet, ws, nil)
	h.clock.Fire(headlessRetryInitial)

	// Assert
	select {
	case got := <-adopted:
		if got != ws {
			t.Fatalf("the retry adopted %q, want the headless workspace %q", got, ws)
		}
	case <-time.After(10 * time.Second):
		t.Fatalf("the headless workspace was never adopted on a retry")
	}
}

// TestHeadlessAdoptRetryBacksOffWhileItKeepsFailing pins the cadence: a
// workspace that will never adopt must not turn the adoption window into a
// retry-per-millisecond log flood, so each failure doubles the wait.
func TestHeadlessAdoptRetryBacksOffWhileItKeepsFailing(t *testing.T) {
	// Arrange: an adopt that never succeeds.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	setAdoptErr(h.fleet, ws, errors.New("the shim is gone"))
	writeHeadlessManifest(t, h, ws)
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)

	// Act
	if err := h.c.Join(ctx); err != nil {
		t.Fatalf("Join: %v", err)
	}
	h.clock.awaitArmed(t, headlessRetryInitial)
	h.clock.Fire(headlessRetryInitial)

	// Assert: the second window is twice the first.
	h.clock.awaitArmed(t, 2*headlessRetryInitial)
}

// TestReclaimHeadlessRefusesAWorkspaceAnotherAdopterHolds is why the retry can
// never adopt behind somebody else: it re-claims before every attempt, and the
// claim is one-at-a-time.
func TestReclaimHeadlessRefusesAWorkspaceAnotherAdopterHolds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	writeHeadlessManifest(t, h, ws)
	if err := h.c.armFromManifest(); err != nil {
		t.Fatalf("armFromManifest: %v", err)
	}
	if claimed := h.c.claimHeadless(); len(claimed) != 1 || claimed[0] != ws {
		t.Fatalf("claimHeadless = %v, want the armed headless workspace", claimed)
	}

	// Act
	got := h.c.reclaimHeadless(ws)

	// Assert
	if got {
		t.Fatalf("reclaimHeadless took a claim another adopter already holds")
	}
}

// TestReclaimHeadlessRefusesAnAlreadyAdoptedWorkspace pins the other stand-down
// edge: a released claim on a workspace that has since adopted must not be
// taken again.
func TestReclaimHeadlessRefusesAnAlreadyAdoptedWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	writeHeadlessManifest(t, h, ws)
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}
	if got := h.fleet.Adoptions(); len(got) != 1 {
		t.Fatalf("adoptions = %v, want the headless workspace adopted at boot", got)
	}
	h.c.releaseHeadless(ws)

	// Act
	got := h.c.reclaimHeadless(ws)

	// Assert
	if got {
		t.Fatalf("reclaimHeadless took a claim on a workspace that has already adopted")
	}
}

// TestAnAdoptionOutLivesTheCallerThatCompletedIt pins the one edge that cost
// the handover its whole adoption window: the participant whose call satisfied
// the rendezvous gives up MID-ADOPTION, and the takeover must finish anyway.
//
// Emacs's adopt is an ordinary unary call under a 10s client timeout while the
// steps of `adopt' are writes against a SQLite handle the outgoing daemon is
// still writing; on a loaded box the client expires first. Run on the caller's
// own context, the adoption then stopped half-done and settled the rendezvous
// failed, and nothing anywhere tried again -- so the incumbent waited out its
// full 30s window before exiting, which is the promotion Emacs never made.
func TestAnAdoptionOutLivesTheCallerThatCompletedIt(t *testing.T) {
	// Arrange: an adoption that is INSIDE its drain step when the caller
	// gives up, and that reports the context it was actually handed.
	entered := make(chan struct{})
	release := make(chan struct{})
	var seen error
	h := newHarness(t, func(deps *Deps) {
		deps.DrainIntake = func(ctx context.Context, _ ids.WorkspaceID) error {
			close(entered)
			<-release
			seen = ctx.Err()
			return ctx.Err()
		}
	})
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true})
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.AdoptHost(ctx, ws) }()
	<-entered
	cancel()
	close(release)
	err := <-done

	// Assert
	if err != nil {
		t.Fatalf("AdoptHost = %v, want the adoption to complete after the caller gave up", err)
	}
	if seen != nil {
		t.Fatalf("the adoption ran under a context the caller cancelled (%v), want one it cannot cancel", seen)
	}
	if got := h.publishedWorkspaces(); len(got) != 1 || got[0] != ws {
		t.Fatalf("published views = %v, want the adoption to have finished for %q", got, ws)
	}
}

// TestAnAdoptCallThatArrivesBeforeTheManifestWaitsForTheArm is the ordering
// the shutdown announcement makes reachable: `Handover` announces the
// successor's address FIRST and writes the intent manifest afterwards
// (handover.go, `ShutdownAnnounced` then `writeManifest`), so a participant
// that dials the announced address at once can reach a successor that has been
// told nothing yet. The call is HELD for the arm, the way adoption is held for
// the incumbent's serving release one step later — not refused.
func TestAnAdoptCallThatArrivesBeforeTheManifestWaitsForTheArm(t *testing.T) {
	// Arrange: a successor in joining mode whose incumbent has announced but
	// has not written the manifest yet.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}
	held := h.clock.armedSignal(manifestPoll)

	// Act: the host participant adopts on the address it was just handed.
	done := make(chan error, 1)
	go func() { done <- h.c.AdoptHost(context.Background(), ws) }()

	// Assert: the call is held on the arm poll rather than answered.
	select {
	case err := <-done:
		t.Fatalf("AdoptHost answered %v before the incumbent armed the rendezvous, want the call held for the arm", err)
	case <-held:
	}

	// Act: the incumbent's manifest lands and the successor's next poll comes
	// round.
	writeHandoverManifest(t, h, ws, Participants{Host: true})
	h.clock.Fire(manifestPoll)

	// Assert: the held call completes the rendezvous and adopts.
	if err := <-done; err != nil {
		t.Fatalf("AdoptHost after the manifest arrived = %v, want success", err)
	}
	if got := h.fleet.Adoptions(); len(got) != 1 || got[0] != ws {
		t.Fatalf("adoptions = %v, want %q adopted once the manifest armed it", got, ws)
	}
}

// TestAnAdoptCallPastTheAdoptionWindowIsStillRefused is the other side of the
// same bound: the wait is the handover's own AdoptionWindow, and a manifest
// that never arrives still answers no_transfer_announced.
func TestAnAdoptCallPastTheAdoptionWindowIsStillRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}
	bounded := h.clock.armedSignal(adoptionWindow)

	// Act
	done := make(chan error, 1)
	go func() { done <- h.c.AdoptHost(context.Background(), ws) }()
	select {
	case err := <-done:
		t.Fatalf("AdoptHost answered %v before its bound was armed, want the call held", err)
	case <-bounded:
	}
	h.clock.Fire(adoptionWindow)

	// Assert
	if err := <-done; !errors.Is(err, ErrNoTransferAnnounced) {
		t.Fatalf("AdoptHost after the adoption window = %v, want no_transfer_announced", err)
	}
}

// TestAnAdoptCallForAWorkspaceAnArrivedManifestDoesNotNameIsRefusedAtOnce
// keeps the wait from swallowing the ordinary answer: once the manifest is in
// hand the transfer set is known, so a workspace it does not name is refused
// immediately rather than held for a bound.
func TestAnAdoptCallForAWorkspaceAnArrivedManifestDoesNotNameIsRefusedAtOnce(t *testing.T) {
	// Arrange: the manifest arrived and names a DIFFERENT workspace.
	h := newHarness(t)
	transferred, _ := h.workspace(t)
	untouched, _ := h.workspace(t)
	arm(t, h, transferred, Participants{Host: true})

	// Act
	err := h.c.AdoptWeb(context.Background(), untouched)

	// Assert
	if !errors.Is(err, ErrNoTransferAnnounced) {
		t.Fatalf("AdoptWeb answered %v, want no_transfer_announced", err)
	}
	for _, waited := range h.clock.Waits() {
		if waited == adoptionWindow {
			t.Fatalf("the refusal armed the %s adoption window, want it answered at once", adoptionWindow)
		}
	}
}

func TestAdvertiseDelay(t *testing.T) {
	cases := []struct {
		name  string
		retry int
		want  time.Duration
	}{
		{name: "the first retry waits one manifest poll", retry: 1, want: manifestPoll},
		{name: "each further retry doubles the wait", retry: 3, want: 4 * manifestPoll},
		{name: "the wait never passes the ceiling", retry: 7, want: advertiseBackoffCeiling},
		{name: "a long run of retries cannot overflow past the ceiling", retry: 500, want: advertiseBackoffCeiling},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the table row.

			// Act
			got := advertiseDelay(tc.retry)

			// Assert
			if got != tc.want {
				t.Fatalf("advertiseDelay(%d) = %s, want %s", tc.retry, got, tc.want)
			}
		})
	}
}

// scriptedAddrWrites is a WriteDaemonAddr that answers each call from a
// script, advancing the clock by `advance` per call. Calls past the script's
// end fail the test.
type scriptedAddrWrites struct {
	t       *testing.T
	clock   *fakeClock
	advance time.Duration
	mu      sync.Mutex
	script  []error
	calls   int
}

func (s *scriptedAddrWrites) write(context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.clock.advance(s.advance)
	if s.calls >= len(s.script) {
		s.t.Errorf("daemon.addr write %d is past the %d scripted", s.calls+1, len(s.script))
		return errors.New("unscripted")
	}
	err := s.script[s.calls]
	s.calls++
	return err
}

func (s *scriptedAddrWrites) count() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.calls
}

var errHeldClaim = fmt.Errorf("%w: daemon.lock", daemonaddr.ErrClaimed)

// TestTheAdvertiseRetryBacksOffWhileTheClaimIsHeld drives the successor's
// daemon.addr retry on the fake clock: every wait it arms is fired, and the
// waits, the writes and the ERROR records are counted.
func TestTheAdvertiseRetryBacksOffWhileTheClaimIsHeld(t *testing.T) {
	errGone := errors.New("the state root is gone")
	cases := []struct {
		name       string
		script     []error
		advance    time.Duration
		wantWaits  []time.Duration
		wantErrors int
	}{
		{
			name:      "a held claim is retried on a doubling wait until it is released",
			script:    []error{errHeldClaim, errHeldClaim, nil},
			wantWaits: []time.Duration{manifestPoll, 2 * manifestPoll, 4 * manifestPoll},
		},
		{
			name:       "a failure that is not a held claim ends the retry at ERROR",
			script:     []error{errHeldClaim, errGone},
			wantWaits:  []time.Duration{manifestPoll, 2 * manifestPoll},
			wantErrors: 1,
		},
		{
			name:       "a claim held past its bound is reported once and still retried",
			script:     []error{errHeldClaim, errHeldClaim, errHeldClaim, errHeldClaim, nil},
			advance:    advertiseRefusedBound / 3,
			wantWaits:  []time.Duration{manifestPoll, 2 * manifestPoll, 4 * manifestPoll, 8 * manifestPoll, 16 * manifestPoll},
			wantErrors: 1,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var writes *scriptedAddrWrites
			h := newHarness(t, func(d *Deps) {
				d.WriteDaemonAddr = func(ctx context.Context) error { return writes.write(ctx) }
			})
			writes = &scriptedAddrWrites{t: t, clock: h.clock, advance: tc.advance, script: tc.script}
			done := make(chan struct{})

			// Act
			go func() {
				defer close(done)
				h.c.retryAdvertise(context.Background(), nil)
			}()
			for _, wait := range tc.wantWaits {
				h.clock.awaitArmed(t, wait)
				h.clock.Fire(wait)
			}
			<-done

			// Assert
			if got := writes.count(); got != len(tc.script) {
				t.Fatalf("daemon.addr writes = %d, want %d", got, len(tc.script))
			}
			if got := len(levelRecords(records(h.log, opAdopt), "error")); got != tc.wantErrors {
				t.Fatalf("ERROR records = %d, want %d", got, tc.wantErrors)
			}
		})
	}
}

// TestTheAdvertiseRetryEndsWithTheDaemonsLifetime pins that the retry is not
// a goroutine that outlives the daemon's serving lifetime.
func TestTheAdvertiseRetryEndsWithTheDaemonsLifetime(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) {
		d.WriteDaemonAddr = func(context.Context) error { return errHeldClaim }
	})
	lifetime, end := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() {
		defer close(done)
		h.c.retryAdvertise(lifetime, nil)
	}()
	h.clock.awaitArmed(t, manifestPoll)

	// Act
	end()

	// Assert: the retry returns without its wait ever being fired.
	<-done
}

// TestAdvertiseDoesNotRetryAFailureThatIsNotAHeldClaim pins the first
// attempt's own refusal: nothing is armed and the failure is an ERROR.
func TestAdvertiseDoesNotRetryAFailureThatIsNotAHeldClaim(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) {
		d.WriteDaemonAddr = func(context.Context) error { return errors.New("the state root is gone") }
	})

	// Act
	h.c.advertise(context.Background(), nil)

	// Assert
	if waits := h.clock.Waits(); len(waits) != 0 {
		t.Fatalf("waits armed = %v, want none for a failure that is not a held claim", waits)
	}
	if got := len(levelRecords(records(h.log, opAdopt), "error")); got != 1 {
		t.Fatalf("ERROR records = %d, want 1", got)
	}
}

// joinedWithOneOfTwo is a successor that joined with a manifest naming only
// FIRST: SECOND is registered but was never handed over (a closed workspace).
func joinedWithOneOfTwo(t *testing.T, h *harness) (first, second ids.WorkspaceID) {
	t.Helper()
	first, _ = h.workspace(t)
	second, _ = h.workspace(t)
	firstRecord, _ := h.db.Workspace(context.Background(), first)
	if err := h.c.writeManifest(context.Background(), Manifest{
		Daemon: ids.InstanceID("daemon-outgoing-previous"), WrittenAt: instant,
		Sessions: []ManifestSession{
			{Workspace: first, Dir: firstRecord.Dir, Intent: IntentPreserve, ExpectedHost: true},
		},
	}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}
	if err := h.c.Join(context.Background()); err != nil {
		t.Fatalf("Join: %v", err)
	}
	return first, second
}

func TestAWorkspaceNeverHandedOverIsNotYetAdoptedWhileTheOutgoingDaemonLives(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) {
		d.WriteDaemonAddr = func(context.Context) error { return daemonaddr.ErrClaimed }
	})
	_, second := joinedWithOneOfTwo(t, h)

	// Act
	standing := h.c.Standing(second)

	// Assert
	if standing != StandingNotYetAdopted {
		t.Fatalf("standing = %v, want not_yet_adopted while another daemon may still serve it", standing)
	}
}

func TestAWorkspaceNeverHandedOverIsOwnedOnceTheSuccessorAdvertises(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first, second := joinedWithOneOfTwo(t, h)

	// Act: the one handed-over workspace is adopted, which advertises.
	if err := h.c.AdoptHost(context.Background(), first); err != nil {
		t.Fatalf("AdoptHost: %v", err)
	}

	// Assert
	if standing := h.c.Standing(second); standing != StandingOwned {
		t.Fatalf("standing = %v, want owned: no other daemon is left to serve it", standing)
	}
}

func TestTheSuccessorTakesOverOnceTheIncumbentLetsGoOfTheClaim(t *testing.T) {
	// Arrange: the claim is held once, then released by the incumbent's exit.
	var writes *scriptedAddrWrites
	h := newHarness(t, func(d *Deps) {
		d.WriteDaemonAddr = func(ctx context.Context) error { return writes.write(ctx) }
	})
	writes = &scriptedAddrWrites{t: t, clock: h.clock, script: []error{errHeldClaim, nil}}
	_, second := joinedWithOneOfTwo(t, h)

	// Act: Join's own watcher retries on its backoff.
	h.clock.awaitArmed(t, incumbentExitPollInitial)
	h.clock.Fire(incumbentExitPollInitial)
	h.clock.awaitArmed(t, 2*incumbentExitPollInitial)
	h.clock.Fire(2 * incumbentExitPollInitial)
	<-h.c.tookOverSignal()

	// Assert
	if standing := h.c.Standing(second); standing != StandingOwned {
		t.Fatalf("standing = %v, want owned once daemon.addr was written", standing)
	}
}

func TestAWorkspaceWhoseHandoverNeverFinishedIsOwnedAfterTheTakeover(t *testing.T) {
	// Arrange: FIRST was being handed over, and nobody ever adopted it — its
	// participants never called (closed mid-handover).
	var writes *scriptedAddrWrites
	h := newHarness(t, func(d *Deps) {
		d.WriteDaemonAddr = func(ctx context.Context) error { return writes.write(ctx) }
	})
	writes = &scriptedAddrWrites{t: t, clock: h.clock, script: []error{nil}}
	first, _ := joinedWithOneOfTwo(t, h)

	// Act: the incumbent is gone at the watcher's first look.
	h.clock.awaitArmed(t, incumbentExitPollInitial)
	h.clock.Fire(incumbentExitPollInitial)
	<-h.c.tookOverSignal()
	h.c.stragglerAdoptions.Wait()

	// Assert: it is this daemon's, never not_yet_adopted again.
	if standing := h.c.Standing(first); standing != StandingOwned {
		t.Fatalf("standing = %v, want the unfinished workspace owned after the takeover", standing)
	}
}

func TestTheTakeoverDoesNotWaitForEveryRendezvous(t *testing.T) {
	// Arrange: the one handed-over workspace is never adopted.
	var writes *scriptedAddrWrites
	h := newHarness(t, func(d *Deps) {
		d.WriteDaemonAddr = func(ctx context.Context) error { return writes.write(ctx) }
	})
	writes = &scriptedAddrWrites{t: t, clock: h.clock, script: []error{nil}}
	joinedWithOneOfTwo(t, h)

	// Act
	h.clock.awaitArmed(t, incumbentExitPollInitial)
	h.clock.Fire(incumbentExitPollInitial)
	<-h.c.tookOverSignal()
	h.c.stragglerAdoptions.Wait()

	// Assert: daemon.addr was written although no rendezvous completed.
	if got := writes.count(); got != 1 {
		t.Fatalf("daemon.addr writes = %d, want 1", got)
	}
}

func TestTheSuccessorBouncesAnAdoptedShimOnAnOlderBuild(t *testing.T) {
	// Arrange: two live shims report their builds as they are attached — one
	// an older build, one the installed build — before either is adopted.
	h := newHarness(t)
	stale, current := joinedWithOneOfTwo(t, h)
	h.fleet.live[current] = newFakeShim(4343, h.order)
	h.fleet.live[stale].Reap() // the old process exits when it is stood down
	h.c.ShimReported(stale, "0ldbu1ld")
	h.c.ShimReported(current, "installed-build")
	h.c.staleChecks.Wait()

	// Act: adopting the handed-over workspace makes it this daemon's to bounce.
	if err := h.c.AdoptHost(context.Background(), stale); err != nil {
		t.Fatalf("AdoptHost: %v", err)
	}
	h.registry.wait()

	// Assert
	if indexOf(h.order.Taken(), "resume") < 0 {
		t.Fatalf("steps = %v, want the stale shim relaunched onto the installed build", h.order.Taken())
	}
	h.fleet.mu.Lock()
	_, touched := h.fleet.prelaunched[current]
	h.fleet.mu.Unlock()
	if touched {
		t.Fatalf("the shim already on the installed build was prelaunched; it must be left alone")
	}
}

func TestAReportBeforeTheAdoptionIsNotActedOn(t *testing.T) {
	// Arrange: a joining successor hears a stale build from a shim it has not
	// adopted yet.
	h := newHarness(t)
	stale, _ := joinedWithOneOfTwo(t, h)

	// Act
	h.c.ShimReported(stale, "0ldbu1ld")
	h.c.staleChecks.Wait()

	// Assert
	if got := h.registry.Requests(); len(got) != 0 {
		t.Fatalf("requests = %+v, want no bounce before the adoption", got)
	}
}

func TestTheSuccessorChecksEveryLiveShimOnlyOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first, _ := joinedWithOneOfTwo(t, h)

	// Act: the takeover is announced twice (a retried advertise landing late).
	if err := h.c.AdoptHost(context.Background(), first); err != nil {
		t.Fatalf("AdoptHost: %v", err)
	}
	h.c.becomeIncumbent(nil)
	h.registry.wait()

	// Assert
	checks := 0
	for _, r := range records(h.log, opStaleness) {
		if r.Message == "judged every adopted shim against the installed build" {
			checks++
		}
	}
	if checks != 1 {
		t.Fatalf("fleet checks = %d, want exactly 1", checks)
	}
}

func TestIncumbentExitDelay(t *testing.T) {
	cases := []struct {
		attempt int
		want    time.Duration
	}{
		{1, 150 * time.Millisecond},
		{2, 300 * time.Millisecond},
		{5, 2400 * time.Millisecond},
		{9, 2400 * time.Millisecond},
	}
	for _, tc := range cases {
		// Act + Assert
		if got := incumbentExitDelay(tc.attempt); got != tc.want {
			t.Fatalf("incumbentExitDelay(%d) = %v, want %v", tc.attempt, got, tc.want)
		}
	}
}

// awaitRecord waits, bounded, for a record the controller writes as it takes a
// decision, which is how a test knows a concurrent caller has reached it.
func awaitRecord(t *testing.T, h *harness, operation, message string) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for time.Now().Before(deadline) {
		for _, rec := range records(h.log, operation) {
			if rec.Message == message {
				return
			}
		}
		runtime.Gosched()
	}
	t.Fatalf("no %s record %q was written", operation, message)
}

// TestALateCallerOfASatisfiedRendezvousDoesNotAdoptASecondTime is the forced
// interleaving the webapp layer's restart handover lost at random: the page's
// own AdoptWebWorkspace reaches the successor while the adoption the test's
// call started is still waiting on the serving release. Two adoptions ran,
// both drained the handover hold, and the second's release met "not found".
func TestALateCallerOfASatisfiedRendezvousDoesNotAdoptASecondTime(t *testing.T) {
	// Arrange: the first caller's adoption is parked on the serving latch.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Web: true})
	const outgoing = ids.InstanceID("daemon-outgoing-previous")
	if err := h.db.ClaimServing(context.Background(), ws, outgoing); err != nil {
		t.Fatalf("ClaimServing(outgoing): %v", err)
	}
	first := make(chan error, 1)
	go func() { first <- h.c.AdoptWeb(context.Background(), ws) }()
	h.clock.awaitArmed(t, manifestPoll)

	// Act: a second web call arrives, then the incumbent lets go.
	second := make(chan error, 1)
	go func() { second <- h.c.AdoptWeb(context.Background(), ws) }()
	awaitRecord(t, h, opAdoptWeb, "the rendezvous is satisfied and its adoption is already running; waiting on it")
	if err := h.db.ReleaseServing(context.Background(), ws, outgoing); err != nil {
		t.Fatalf("ReleaseServing(outgoing): %v", err)
	}
	h.clock.Fire(manifestPoll)

	// Assert.
	if err := <-first; err != nil {
		t.Fatalf("the first AdoptWeb = %v, want success", err)
	}
	if err := <-second; err != nil {
		t.Fatalf("the second AdoptWeb = %v, want the first's success", err)
	}
	drains := 0
	for _, step := range h.order.Taken() {
		if step == "drain_intake" {
			drains++
		}
	}
	if drains != 1 {
		t.Fatalf("the held intake was drained %d time(s), want once", drains)
	}
}

// TestAFailedAdoptionMayBeRunAgainByALaterCaller: the latch forbids only a
// concurrent second run; a later call after a failure still runs the adoption.
func TestAFailedAdoptionMayBeRunAgainByALaterCaller(t *testing.T) {
	// Arrange: the first adoption fails dialing the shim.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.adoptErr[ws] = errFake
	arm(t, h, ws, Participants{Host: true})
	if err := h.c.AdoptHost(context.Background(), ws); err == nil {
		t.Fatal("precondition: the first adoption succeeded")
	}

	// Act.
	h.c.mu.Lock()
	adopting := h.c.rendezvous[ws].adopting
	h.c.mu.Unlock()

	// Assert.
	if adopting {
		t.Fatal("a failed adoption left the latch closed; no later caller could run it again")
	}
}

// TestAJoiningDaemonRetiresTheManifestItArmedFrom pins the successor's half of
// consuming a manifest: once it has armed from it and recorded what it names,
// no later boot of the same state root reads it again.
func TestAJoiningDaemonRetiresTheManifestItArmedFrom(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	arm(t, h, ws, Participants{Host: true})

	// Assert
	if manifestExists(t, h) {
		t.Fatalf("the intent manifest is still on disk after the joining daemon armed from it")
	}
}

func TestAJoiningDaemonKeepsTheRendezvousItArmedAfterRetiringTheManifest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	arm(t, h, ws, Participants{Host: true})

	// Assert
	if got := h.c.rendezvousSize(); got != 1 {
		t.Fatalf("armed workspaces = %d, want the one the retired manifest named", got)
	}
}

func TestAHandoverAdoptionClaimsOnTheJoiningReadOnlyHandle(t *testing.T) {
	// Arrange: the incumbent has released the transferred workspace, and this
	// successor still holds the read-only handle it joined with.
	h := newHarness(t)
	first, _ := joinedWithOneOfTwo(t, h)
	if err := h.db.ReleaseServing(context.Background(), first, selfInstance); err != nil {
		t.Fatalf("ReleaseServing: %v", err)
	}
	h.joiningHandle(t)

	// Act
	if err := h.c.AdoptHost(context.Background(), first); err != nil {
		t.Fatalf("AdoptHost: %v", err)
	}

	// Assert
	owner, err := h.db.Serving(context.Background(), first)
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if owner == nil || *owner != selfInstance {
		t.Fatalf("serving owner = %q, want this daemon once the adoption promoted its handle", ownerText(owner))
	}
}

// A DIALED ADOPTION IS A HEALTHY ATTACH (health/lifetime.go), so the faults
// whose lifetime ends there close, and the others stand.
func TestADialedAdoptionClosesTheHealthyAttachsFaults(t *testing.T) {
	tests := []struct {
		name   string
		kind   string
		closes bool
	}{
		{"an undetermined bounce", health.KindBounceUnknown, true},
		{"an expired adoption window", health.KindAdoptionWindowExpired, true},
		{"a refused resume waits for a started session", health.KindResumeFailed, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			if _, err := h.db.OpenFault(context.Background(), wsm.Fault{Workspace: &ws, Kind: tt.kind}); err != nil {
				t.Fatalf("OpenFault: %v", err)
			}

			// Act
			arm(t, h, ws, Participants{})
			open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: tt.kind})

			// Assert
			if err != nil {
				t.Fatalf("OpenFaults: %v", err)
			}
			if closed := len(open) == 0; closed != tt.closes {
				t.Fatalf("%s closed = %v, want %v", tt.kind, closed, tt.closes)
			}
		})
	}
}

// freeLockAdoption arranges a handed-over workspace whose lock reads free: the
// outgoing daemon's shim had died, or was never spawned.
func freeLockAdoption(t *testing.T, h *harness) ids.WorkspaceID {
	t.Helper()
	ws, dir := h.workspace(t)
	h.mu.Lock()
	h.lockStates[dir] = sessionlock.StateFree
	h.mu.Unlock()
	h.fleet.mu.Lock()
	delete(h.fleet.live, ws)
	h.fleet.mu.Unlock()
	return ws
}

func TestAnAdoptionOfAFreeLockStartsTheWorkspacesSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := freeLockAdoption(t, h)

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.Started(); !slices.Equal(got, []ids.WorkspaceID{ws}) {
		t.Fatalf("started = %v, want the adopted session-less workspace started", got)
	}
}

func TestAnAdoptionOfAFreeLockRaisesTheMarkerBeforeItPublishes(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := freeLockAdoption(t, h)

	// Act
	arm(t, h, ws, Participants{})
	h.Started()

	// Assert
	taken := h.order.Taken()
	raised := slices.Index(taken, "bringing_up:true")
	published := slices.Index(taken, "publish_views")
	started := slices.Index(taken, "start_session")
	if raised < 0 || published < 0 || started < 0 || raised > published || published > started {
		t.Fatalf("steps = %v, want the marker raised, then the views published, then the session started", taken)
	}
}

func TestAnAdoptionOfAHeldLockStartsNoSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none: the adopted shim is the session", got)
	}
}

func TestAnAdoptionOfAClosedFreeLockStartsNoSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := freeLockAdoption(t, h)
	if err := h.db.SetClosed(context.Background(), ws, true); err != nil {
		t.Fatalf("SetClosed: %v", err)
	}

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none for a closed workspace", got)
	}
}

func TestAnAdoptionWhoseViewsFailLowersTheMarkerAndStartsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) {
		d.PublishViews = func(context.Context, ids.WorkspaceID) error { return errFake }
	})
	ws := freeLockAdoption(t, h)

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none for an adoption that failed", got)
	}
	want := []markerEdge{{ws: ws, up: true}, {ws: ws, up: false}}
	if got := h.Marker(); !slices.Equal(got, want) {
		t.Fatalf("marker edges = %v, want the raise taken back", got)
	}
}
