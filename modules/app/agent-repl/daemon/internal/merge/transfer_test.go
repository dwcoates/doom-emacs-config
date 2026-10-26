package merge

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/encoding/prototext"

	"agent-repld/internal/dlog"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// heldInGateForTransfer runs the harness's merge into its test gate and
// answers the gate's release and the pump's end.
func heldInGateForTransfer(t *testing.T, h *harness) (chan struct{}, <-chan error) {
	t.Helper()
	h.emacsRepo()
	h.landsCleanly(mergeSHA)
	inGate, release := make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate
	return release, done
}

func TestATransferSuspendsTheRunningMergeAndKeepsWhatItsResumeNeeds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() { close(release) })

	// Act.
	err := h.o.SuspendForTransfer(context.Background(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("SuspendForTransfer = %v, want the merge suspended", err)
	}
	if err := <-done; err != nil {
		t.Fatalf("the pump was told %v, want no failure", err)
	}
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); !held {
		t.Fatal("the transfer released the merge's lease, want it left for the adopting daemon")
	}
	if step := recordedStep(t, h); step != TabTests {
		t.Fatalf("the record stands at %q, want %q", step, TabTests)
	}
	if record, found := recordWith(h, "info", "daemon.merge.transfer"); !found || record.Context["step"] != TabTests {
		t.Fatalf("the transfer's record is %+v (found %v), want the suspension naming its step", record, found)
	}
}

func TestATransferWaitsForTheMergesGitInFlight(t *testing.T) {
	// Arrange: a landing held inside its fast-forward.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly(mergeSHA)
	h.gatePasses("daemon")
	entered, release := h.git.hold("fast_forward")
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-entered
	h.o.onGitStopWait = func() { close(release) }

	// Act.
	err := h.o.SuspendForTransfer(context.Background(), theWorkspace)
	<-done

	// Assert.
	if err != nil || len(h.git.fastForwards) != 1 {
		t.Fatalf("SuspendForTransfer = %v with fast-forwards %v, want the one in flight finished", err, h.git.fastForwards)
	}
}

func TestAWorkspaceHandedAwayHasNoMergeAdmittedHere(t *testing.T) {
	// Arrange: a queued merge whose workspace moved.
	h := newHarness(t)
	enqueue(t, h)
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}

	// Act.
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil || ran {
		t.Fatalf("pumpOnce = (%v, %v), want nothing admitted for a workspace another daemon serves", ran, err)
	}
}

func TestAMergeAdmittedElsewhereHoldsItsRepositorysSlot(t *testing.T) {
	// Arrange: one merge admitted by another daemon, one queued behind it.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	enqueue(t, h)
	enqueueWorkspace(t, h, "ws-2")
	if err := h.db.AdmitMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act.
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil || ran {
		t.Fatalf("pumpOnce = (%v, %v), want nothing admitted behind a merge in flight elsewhere", ran, err)
	}
}

func TestATransferWithdrawsARequestWaitingForItsTurnLeavingItRecorded(t *testing.T) {
	// Arrange: a request waiting for its turn to end.
	h := newHarness(t)
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	h.o.awaitRequestingTurn(theWorkspace, h.repoKey())

	// Act.
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}

	// Assert.
	h.o.mu.Lock()
	_, waiting := h.o.requested[theWorkspace]
	h.o.mu.Unlock()
	if waiting {
		t.Fatal("the request's wait stands here after its workspace moved")
	}
	if state, _ := h.entryState(); state != wsm.MergeRequested {
		t.Fatalf("entry state = %v, want the request still recorded for the adopting daemon", state)
	}
}

func TestTheAdoptingDaemonResumesASuspendedMergeInTheSameBubble(t *testing.T) {
	// Arrange: a merge suspended for its transfer, and the successor.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() { close(release) })
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}
	<-done
	lease, _, _ := h.db.Lease(context.Background(), theWorkspace)
	h.runner.mu.Lock()
	h.runner.before = nil
	h.runner.mu.Unlock()
	h.gatePasses("daemon")
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true
	h.restart(t)

	// Act.
	if err := h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the resumed merge ended on: %v", err)
	}

	// Assert.
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed", facts.State, facts.Detail)
	}
	for _, id := range h.feed.headIDs() {
		if id != headIDOf(lease.ID) {
			t.Fatalf("a head was drawn as %s, want every head the transferred merge's own", id)
		}
	}
}

func TestATakenBackTransferResumesItsMergeHere(t *testing.T) {
	// Arrange: a merge suspended for a transfer that did not land.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() { close(release) })
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}
	<-done
	h.runner.mu.Lock()
	h.runner.before = nil
	h.runner.mu.Unlock()
	h.gatePasses("daemon")
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true

	// Act.
	if err := h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the resumed merge ended on: %v", err)
	}

	// Assert.
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed here", facts.State, facts.Detail)
	}
}

// queuedLedger reads the ledger identity the harness workspace's queued merge
// carries on its row.
func queuedLedger(t *testing.T, h *harness) wsm.LeaseID {
	t.Helper()
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	for _, entry := range entries {
		if entry.Workspace == theWorkspace {
			if entry.Ledger == "" {
				t.Fatal("the queued merge's row carries no ledger identity")
			}
			return entry.Ledger
		}
	}
	t.Fatal("the harness workspace has no queue entry")
	return ""
}

// headsSince answers the bubble heads pushed after the first n pushes.
func headsSince(h *harness, n int) []string {
	return h.feed.headIDs()[n:]
}

// A queued merge has its bubble, pinned to the feed's bottom (owner ruling,
// 2026-10-10); a restart and a handover draw it again under the identity it
// was queued under, still pinned.

// queuedHeadsAre fails unless HEADS is a non-empty run of LEDGER's head, every
// one published pinned.
func queuedHeadsAre(t *testing.T, heads []synthesized, ledger wsm.LeaseID) {
	t.Helper()
	if len(heads) == 0 {
		t.Fatal("no bubble was drawn for the queued merge")
	}
	for _, head := range heads {
		if id := prototext.Format(head.Row.GetId()); id != headIDOf(ledger) || !head.Pinned {
			t.Fatalf("drew %s (pinned %v), want the bubble the merge waited in, %s, pinned", id, head.Pinned, headIDOf(ledger))
		}
	}
}

func TestARestartRedrawsAQueuedMergeInTheBubbleItWaitedIn(t *testing.T) {
	// Arrange: a merge waiting in a paused queue.
	h := newHarness(t)
	h.db.paused[h.repoKey()] = true
	enqueue(t, h)
	ledger := queuedLedger(t, h)
	before := len(h.feed.headIDs())
	h.restart(t)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	queuedHeadsAre(t, h.feed.headPublications()[before:], ledger)
	if got, ok := h.o.leaseOf(theWorkspace); !ok || got != ledger {
		t.Fatalf("the recovered merge's identity = %q (%v), want %q", got, ok, ledger)
	}
}

func TestAHandoverRedrawsAQueuedMergeInTheBubbleItWaitedIn(t *testing.T) {
	// Arrange: a merge waiting in a paused queue, its workspace handed over.
	h := newHarness(t)
	h.db.paused[h.repoKey()] = true
	enqueue(t, h)
	ledger := queuedLedger(t, h)
	if err := h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}
	before := len(h.feed.headIDs())
	h.restart(t)

	// Act.
	if err := h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}

	// Assert.
	queuedHeadsAre(t, h.feed.headPublications()[before:], ledger)
	if got, ok := h.o.leaseOf(theWorkspace); !ok || got != ledger {
		t.Fatalf("the adopted merge's identity = %q (%v), want %q", got, ok, ledger)
	}
}

func TestAQueuedMergeAnEarlierBuildRecordedIsGivenABubble(t *testing.T) {
	// Arrange: a queued row with no ledger identity, as an earlier build
	// wrote it.
	h := newHarness(t)
	h.db.paused[h.repoKey()] = true
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	h.db.mu.Lock()
	h.db.queues[h.repoKey()][0].State = wsm.MergeQueued
	h.db.mu.Unlock()

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	if _, has := h.o.leaseOf(theWorkspace); !has {
		t.Fatal("the earlier build's queued merge was given no bubble")
	}
	if _, found := recordWith(h, "info", "daemon.merge.recover"); !found {
		t.Fatal("the fresh bubble was not recorded")
	}
}

func TestAMergeThatCannotBePutInLineHoldsNoBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.queueErr = errors.New("disk full")

	// Act.
	_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, By: RequestedByUser})

	// Assert.
	if err == nil {
		t.Fatal("Enqueue succeeded with the queue write failing, want the failure")
	}
	if _, has := h.o.leaseOf(theWorkspace); has {
		t.Fatal("a merge that never took its place still holds a bubble identity")
	}
}

// TestASuccessorAdoptingBehindAMergeRunningInTheIncumbentAdmitsNothingQuietly
// covers the two-daemon window of a handover: the successor takes a workspace
// whose repository's merge still runs in the incumbent. Its admission stops
// at the durable "admitted elsewhere" fact, before the repository's kernel
// lock is ever tried, so nothing is admitted and nothing is warned.
func TestASuccessorAdoptingBehindAMergeRunningInTheIncumbentAdmitsNothingQuietly(t *testing.T) {
	// Arrange: the incumbent's merge held in its gate, a second workspace's
	// merge queued behind it, and the successor over the same state.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	release, done := heldInGateForTransfer(t, h)
	t.Cleanup(func() {
		close(release)
		<-done
	})
	enqueueWorkspace(t, h, "ws-2")
	successor, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	if err := successor.AdoptWorkspace(context.Background(), "ws-2"); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}
	ran, err := successor.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil || ran {
		t.Fatalf("pumpOnce = (%v, %v), want nothing admitted behind the incumbent's merge", ran, err)
	}
	if record, found := recordWith(h, "debug", "daemon.merge.admit"); !found || !strings.Contains(record.Message, "admitted elsewhere") {
		t.Fatalf("the admission's record is %+v (found %v), want it stopped at the merge admitted elsewhere", record, found)
	}
	for _, level := range []string{"warn", "error"} {
		if records := recordsAtLevel(h, level); len(records) > 0 {
			t.Fatalf("the successor wrote %s records %v, want none", level, records)
		}
	}
}

func TestTheFirstSuspensionOfARunStands(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := &run{o: h.o}
	h.o.mu.Lock()
	r.suspendLocked(suspendPause)

	// Act: the daemon's exit arrives under the paused merge.
	r.suspendLocked(suspendExit)
	h.o.mu.Unlock()

	// Assert.
	why, suspended := r.suspendedFor()
	if !suspended || why.by != suspendPause.by {
		t.Fatalf("suspendedFor = %q, %v; want the pause that took the run", why.by, suspended)
	}
}

func TestARunNoSuspensionTookIsNotSuspended(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := &run{o: h.o}

	// Act.
	_, suspended := r.suspendedFor()

	// Assert.
	if suspended {
		t.Fatal("suspendedFor reports a suspension no one made")
	}
}

func TestASuspensionLeavesAnAbandonedRunToItsEnd(t *testing.T) {
	// Arrange: the run was abandoned before the pause reached it, and has
	// ended.
	h := newHarness(t)
	r := liveRun(t, h, theWorkspace)
	h.o.mu.Lock()
	r.abandonedBy = CauseUserDrop
	h.o.mu.Unlock()
	close(r.finished)

	// Act.
	err := h.o.suspendFor(context.Background(), theWorkspace, suspendPause)

	// Assert.
	if err != nil {
		t.Fatalf("suspendFor = %v, want the abandoned run left to its end", err)
	}
	if _, suspended := r.suspendedFor(); suspended {
		t.Fatal("the pause took a run that was already abandoned")
	}
	if !loggedLike(h, logLike{level: "info", op: "daemon.merge.pause", text: "the merge was abandoned before the suspension took it",
		context: map[string]string{"workspace": string(theWorkspace), "cause": string(CauseUserDrop)}}) {
		t.Fatalf("no record of the abandoned run left alone: %v", h.logs.Records())
	}
}

func TestLetGoBound(t *testing.T) {
	tests := []struct {
		name       string
		background bool
		want       time.Duration
	}{
		{name: "a run at the front lets go within the drain's bound", background: false, want: 2 * time.Second},
		{name: "a waiting member's rebase lets go through git, within both bounds", background: true, want: 12 * time.Second},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.o.drainBound, h.o.gitBound = 2*time.Second, 10*time.Second
			r := liveRun(t, h, theWorkspace)
			r.background = tc.background

			// Act
			got := h.o.letGoBound(r)

			// Assert
			if got != tc.want {
				t.Fatalf("letGoBound = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestATransferWaitsPastTheDrainBoundForAWaitingMembersRebaseToLetGo(t *testing.T) {
	// Arrange: the drain's bound is gone at once; the run lets go only once
	// the test has seen it cut, as a rebase abort does after the cut.
	h := newHarness(t)
	h.o.drainBound, h.o.gitBound = time.Nanosecond, time.Minute
	r := liveRun(t, h, theWorkspace)
	r.background = true
	go func() {
		<-r.ctx.Done()
		close(r.finished)
	}()

	// Act
	err := h.o.suspendFor(context.Background(), theWorkspace, suspendTransfer)

	// Assert
	if err != nil {
		t.Fatalf("suspendFor = %v, want the waiting member's rebase let go", err)
	}
}

func TestASuspensionRefusesAnAbandonedRunThatDoesNotEndWithinItsBound(t *testing.T) {
	// Arrange: the abandoned run never ends.
	h := newHarness(t)
	h.o.drainBound = time.Millisecond
	r := liveRun(t, h, theWorkspace)
	h.o.mu.Lock()
	r.abandonedBy = CauseUserDrop
	h.o.mu.Unlock()

	// Act.
	err := h.o.suspendFor(context.Background(), theWorkspace, suspendPause)

	// Assert.
	if err == nil {
		t.Fatal("suspendFor answered nil for an abandoned run that never ended")
	}
	if _, suspended := r.suspendedFor(); suspended {
		t.Fatal("the refused pause still took the run")
	}
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.pause", text: "an abandoned merge did not end before the suspension's bound",
		context: map[string]string{"workspace": string(theWorkspace), "error": err.Error()}}) {
		t.Fatalf("no error record of the refusal: %v", h.logs.Records())
	}
}

func TestATakeBackWhoseHeldAbandonmentFailsAnswersTheFailure(t *testing.T) {
	// Arrange: a pause took the workspace, an abandonment is left for its
	// take-back, and the merge has no queue entry left to drop.
	h := newHarness(t)
	h.o.mu.Lock()
	h.o.takenAway[theWorkspace] = suspendPause
	h.o.abandonsAtTakeBack[theWorkspace] = CauseUserDrop
	h.o.mu.Unlock()

	// Act.
	err := h.o.AdoptWorkspace(context.Background(), theWorkspace)

	// Assert.
	if err == nil {
		t.Fatal("AdoptWorkspace answered nil though the abandonment it held could not end the merge")
	}
	h.o.mu.Lock()
	_, away := h.o.takenAway[theWorkspace]
	_, held := h.o.abandonsAtTakeBack[theWorkspace]
	h.o.mu.Unlock()
	if away || held {
		t.Fatalf("the take-back left the mark (%v) or the abandonment (%v) behind", away, held)
	}
	if !loggedLike(h, logLike{level: "info", op: "daemon.merge.adopt", text: "an abandonment that reached the merge while a suspension held it",
		context: map[string]string{"workspace": string(theWorkspace), "cause": string(CauseUserDrop)}}) {
		t.Fatalf("no record of the abandonment applied: %v", h.logs.Records())
	}
}

func TestAwaitOwnEnd(t *testing.T) {
	says := ownEnd{refusal: "merge: %s outlived %s", late: "the run outlived the bound", ended: "the run ended"}
	tests := []struct {
		name    string
		ended   bool
		wantErr bool
		level   string
		message string
	}{
		{name: "a run that ends is forgotten", ended: true, level: "info", message: says.ended},
		{name: "a run that outlives the bound refuses the suspension", wantErr: true, level: "error", message: says.late},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.o.drainBound = time.Millisecond
			h.o.mu.Lock()
			h.o.ledgerOf[theWorkspace] = "lease-own-end"
			h.o.mu.Unlock()
			done := make(chan struct{})
			if tc.ended {
				close(done)
			}

			// Act.
			err := h.o.awaitOwnEnd(h.logs.Global(), "daemon.merge.pause", theWorkspace, dlog.Context{"workspace": string(theWorkspace)}, done, says)

			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("awaitOwnEnd = %v, want an error %v", err, tc.wantErr)
			}
			if err != nil && !strings.Contains(err.Error(), string(theWorkspace)+" outlived "+time.Millisecond.String()) {
				t.Fatalf("the refusal %q does not name the workspace and the bound", err)
			}
			h.o.mu.Lock()
			_, remembered := h.o.ledgerOf[theWorkspace]
			h.o.mu.Unlock()
			if remembered == tc.ended {
				t.Fatalf("the merge's ledger identity remembered = %v, want it forgotten only once the run ended", remembered)
			}
			want := map[string]string{"workspace": string(theWorkspace)}
			if err != nil {
				want["error"] = err.Error()
			}
			if !loggedLike(h, logLike{level: tc.level, op: "daemon.merge.pause", text: tc.message, context: want}) {
				t.Fatalf("no %s record %q: %v", tc.level, tc.message, h.logs.Records())
			}
		})
	}
}

// TestASuspensionAwaitsEveryOwnEndThroughAwaitOwnEnd holds suspendFor's runs
// that end by themselves -- in their terminal, or abandoned -- to the one
// bounded wait, so neither drifts into a hand-rolled copy.
func TestASuspensionAwaitsEveryOwnEndThroughAwaitOwnEnd(t *testing.T) {
	// Arrange.
	var body string
	for _, f := range sourcescan.Production(t) {
		src := string(f.Source)
		start := strings.Index(src, "\nfunc (o *orchestrator) suspendFor(")
		if start < 0 {
			continue
		}
		end := strings.Index(src[start+1:], "\nfunc ")
		body = src[start : start+1+end]
	}
	if body == "" {
		t.Fatal("suspendFor was not found")
	}

	// Act.
	calls := strings.Count(body, "o.awaitOwnEnd(")
	handRolled := strings.Count(body, "case <-mark.done") + strings.Count(body, "o.forgetMoved(ws)\n\t\tlog.Info(op, \"the merge was abandoned")

	// Assert.
	if calls != 2 || handRolled != 0 {
		t.Fatalf("suspendFor waits through awaitOwnEnd %d times with %d hand-rolled waits, want 2 and none", calls, handRolled)
	}
}
