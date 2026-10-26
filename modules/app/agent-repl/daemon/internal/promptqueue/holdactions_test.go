package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/classifier"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// heldPrompt parks one prompt behind a running turn and returns the harness.
func heldPrompt(t *testing.T, h *harness, turn string, verdict classifier.Verdict) {
	t.Helper()
	h.judge.verdict = verdict
	if _, err := h.q.Submit(context.Background(), submission(idsTurn(turn), "the held prompt")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
}

func TestReleaseDeliversAtOnceWhenNothingIsRunning(t *testing.T) {
	// Arrange: the prompt was held by the drain lease, which has since lifted.
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the released prompt", started)
	}
}

func TestReleaseRetiresTheHoldItDelivered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if h.db.hold("t1").Tombstone == nil {
		t.Fatal("a delivered hold must be retired")
	}
}

func TestReleaseSendsThePromptToJoinTheRunningTurnAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if got := h.sender.joins; len(got) != 1 || got[0] != "t1" {
		t.Fatalf("joins = %v, want t1 sent to join the running turn", got)
	}
}

func TestReleaseIntoARunningTurnInterruptsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want nothing interrupted", killed)
	}
}

func TestReleaseIntoARunningTurnRetiresTheHoldAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if held := h.db.hold("t1"); held.Tombstone == nil || held.Tombstone.Kind != tombstoneDelivered {
		t.Fatalf("hold = %+v, want it retired as delivered before the running turn ends", held)
	}
}

func TestReleaseIntoARunningTurnDrawsThePromptsRowAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	before := len(h.feed.mirrored())
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if got := len(h.feed.mirrored()); got != before+1 {
		t.Fatalf("mirrored rows = %d, want the released prompt's row drawn (was %d)", got, before)
	}
}

func TestReleaseJoinsBesideAJudgedJoinAlreadyWaiting(t *testing.T) {
	// Arrange: t1 was judged to join and waits; t2 is held behind it.
	h := newHarness(t)
	sentToJoin(t, h)
	heldPrompt(t, h, "t2", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t2"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if got := h.sender.joins; len(got) != 2 || got[1] != "t2" {
		t.Fatalf("joins = %v, want t2 sent beside t1", got)
	}
}

func TestReleaseThatCannotJoinIsRefusedAndSaysWhyOnTheVerdict(t *testing.T) {
	// Arrange: the running turn is one the vendor started, which is never joined.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	h.watcher.refuseJoins = true
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("err = %v, want ErrReleaseRefused", err)
	}
	if !logged(h.log.Records(), "info", opRelease, "the released prompt could not join the running turn; the release is refused") {
		t.Fatalf("records = %+v, want the refusal on disk", h.log.Records())
	}
	if c := verdictOf(t, h, "t1"); c.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("verdict = %+v, want the prompt waiting with the join's reason", c)
	}
}

func TestReleaseOfAHeldSessionActInterruptsTheRunningTurn(t *testing.T) {
	// Arrange: an act runs as a turn of its own and joins nothing.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	putHeld(t, h, wsm.HeldPrompt{Turn: "a1", Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "opus"}})
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "a1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if killed := h.sender.killed(); len(killed) != 1 || killed[0] != "running-turn" {
		t.Fatalf("killed = %v, want the running turn", killed)
	}
	if len(h.sender.joins) != 0 {
		t.Fatalf("joins = %v, want the act sent to join nothing", h.sender.joins)
	}
}

func TestReleaseWhileAnInterjectionIsUnderWay(t *testing.T) {
	tests := []struct {
		name    string
		release ids.TurnID
		wantErr error
		message string
	}{
		{
			name:    "of the interjecting prompt is answered as already going",
			release: "t1",
			wantErr: nil,
			message: "the prompt is already interjecting; the interrupted turn's end delivers it",
		},
		{
			name:    "of another prompt is refused, so the interjection goes first",
			release: "t2",
			wantErr: ErrReleaseRefused,
			message: "an interjection is interrupting the running turn and goes first; the release is refused",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: t1 interjects the running turn; t2 is held behind it.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "supersedes"})
			heldPrompt(t, h, "t2", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
			// Act
			err := h.q.Release(context.Background(), theWorkspace, tc.release)
			// Assert
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("err = %v, want %v", err, tc.wantErr)
			}
			if len(h.sender.joins) != 0 {
				t.Fatalf("joins = %v, want nothing sent ahead of the interjection", h.sender.joins)
			}
			if !logged(h.log.Records(), "info", opRelease, tc.message) {
				t.Fatalf("records = %+v, want %q on disk", h.log.Records(), tc.message)
			}
		})
	}
}

func TestReleaseRefusesAnUninterruptibleHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActClear}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	running(t, h, "running-turn", "/clear")
	heldPrompt(t, h, "t1", classifier.Verdict{})
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("err = %v, want ErrReleaseRefused", err)
	}
}

func TestReleaseRefusesAHoldWaitingOnASessionComingUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderHibernate, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("err = %v, want ErrReleaseRefused", err)
	}
}

func TestReleaseReportsAnUnknownHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "never-held")
	// Assert
	if !errors.Is(err, ErrNoSuchHold) {
		t.Fatalf("err = %v, want ErrNoSuchHold", err)
	}
}

func TestDropRetiresTheHoldDurablyFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})
	// Act
	if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Drop: %v", err)
	}
	// Assert
	tomb := h.db.hold("t1").Tombstone
	if tomb == nil || tomb.Kind != tombstoneDropped {
		t.Fatalf("tombstone = %+v, want a dropped tombstone", tomb)
	}
}

func TestDropRefusesWhenTheDurableDropFails(t *testing.T) {
	// Arrange: a failed drop refuses the cancel rather than reporting a prompt
	// gone that is still standing.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})
	h.db.tombstoneErr = errors.New("the database is read-only")
	// Act
	err := h.q.Drop(context.Background(), theWorkspace, "t1")
	// Assert
	if err == nil {
		t.Fatal("a failed durable drop must refuse the cancel")
	}
}

func TestDropRepublishesTheTray(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})
	before := h.holds.pushCount()
	// Act
	if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Drop: %v", err)
	}
	// Assert
	if h.holds.pushCount() <= before {
		t.Fatal("the tray must be republished after a drop")
	}
	if len(h.holds.last()) != 0 {
		t.Fatalf("tray = %v, want the dropped prompt gone", h.holds.last())
	}
}

func TestReleasingAHibernationsHoldRevivesTheParkedSessionToDeliverIt(t *testing.T) {
	// Arrange: the prompt is held by the hibernation lease, then the sweep
	// releases it with the session still parked.
	h := newHarness(t)
	h.lease(wsm.HolderHibernate, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.noSession = true
	h.reviveHook = func() { h.noSession = false }
	h.clearLease()

	// Act
	h.watcher.idle()
	h.q.OnLeaseChanged(theWorkspace)
	// The lease change no longer revives in-line: a lease ending with no
	// session up hands the surviving session_starting holds to the same
	// background revival a fresh submission takes, so the join is the
	// queue's own revival WaitGroup rather than a wait on the call's return.
	h.waitRevivals()

	// Assert
	if h.revivals != 1 {
		t.Fatalf("revivals = %d, want exactly one", h.revivals)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the held prompt delivered after the revival", started)
	}
}

// TestReleasingDuringTheBringUpALeaseEndingStartedIsRefused pins the window
// that produced the wrong arm: a hibernation's lease ends while the workspace
// it parked still has no shim, and a release landing inside the bring-up that
// follows must answer the domain-correct release_refused -- never the untyped
// "no session" about a workspace that is in fact still coming up.
func TestReleasingDuringTheBringUpALeaseEndingStartedIsRefused(t *testing.T) {
	// Arrange: the prompt is held by the hibernation lease, the lease is
	// cleared with the session still parked, and the revival is pinned open.
	h := newHarness(t)
	h.lease(wsm.HolderHibernate, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "wake up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.noSession = true
	entered, proceed := make(chan struct{}), make(chan struct{})
	h.reviveHook = func() {
		close(entered)
		<-proceed
		h.noSession = false
	}
	h.clearLease()
	h.watcher.idle()
	go h.q.OnLeaseChanged(theWorkspace)
	<-entered

	// Act: a force-through lands while the bring-up is still running.
	err := h.q.Release(context.Background(), theWorkspace, "t1")

	// Assert
	close(proceed)
	h.waitRevivals()
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("Release during the bring-up = %v, want ErrReleaseRefused", err)
	}
}

// TestReleaseIsRefusedWhileASessionActRuns pins that a force-through takes the
// interject path, and that path cannot target a running /compact: a prompt
// held before the act began keeps its place and no interrupt is sent.
func TestReleaseIsRefusedWhileASessionActRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	h.beginCut("cut-1", conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("Release = %v, want ErrReleaseRefused", err)
	}
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want no interrupt of the running session act", killed)
	}
}

func TestReleaseIsRefusedWhileACutTheWatcherHasNotYetSeenRuns(t *testing.T) {
	// Arrange: the prompt was held by the drain lease, which has since lifted.
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	h.beginCut("cut-1", conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	h.watcher.idle()
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("Release = %v, want ErrReleaseRefused", err)
	}
}

func TestSupersedeRetiresAStandingHoldAsSuperseded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})

	// Act
	retired, err := h.q.Supersede(context.Background(), theWorkspace, "t1")

	// Assert
	if err != nil || !retired {
		t.Fatalf("Supersede = (%v, %v), want (true, nil)", retired, err)
	}
	if tomb := h.db.hold("t1").Tombstone; tomb == nil || tomb.Kind != tombstoneSuperseded {
		t.Fatalf("tombstone = %+v, want a superseded tombstone", tomb)
	}
	if !logged(h.log.Records(), "info", opSupersede, "the held prompt was dropped") {
		t.Fatalf("records = %+v, want the retirement recorded at info under %s", h.log.Records(), opSupersede)
	}
}

func TestSupersedeAnswersFalseForAPromptAlreadyTold(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
	}{
		{
			name: "delivered",
			arrange: func(t *testing.T, h *harness) {
				running(t, h, "running-turn", "the running work")
				heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})
				if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
					t.Fatalf("Drop: %v", err)
				}
			},
		},
		{
			name:    "never held",
			arrange: func(t *testing.T, h *harness) {},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(t, h)

			// Act
			retired, err := h.q.Supersede(context.Background(), theWorkspace, "t1")

			// Assert
			if err != nil || retired {
				t.Fatalf("Supersede = (%v, %v), want (false, nil)", retired, err)
			}
			if !logged(h.log.Records(), "debug", opSupersede, "the prompt is no longer held; it was told, so nothing is superseded") {
				t.Fatalf("records = %+v, want the told prompt recorded at debug", h.log.Records())
			}
		})
	}
}

func TestSupersedeLeavesAPromptBeingDelivered(t *testing.T) {
	// Arrange: a rollback claims the hold for its call, as a delivery does.
	h := newHarness(t)
	heldByDrainLease(t, h, "t1")
	var retired bool
	var supersedeErr error

	// Act
	err := h.q.RollBack(context.Background(), theWorkspace, instant.Add(-1), []ids.TurnID{"t1"}, func(context.Context) error {
		retired, supersedeErr = h.q.Supersede(context.Background(), theWorkspace, "t1")
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if supersedeErr != nil || retired {
		t.Fatalf("Supersede during the call = (%v, %v), want (false, nil)", retired, supersedeErr)
	}
	if !logged(h.log.Records(), "debug", opSupersede, "the prompt is being delivered to the shim, so nothing is superseded") {
		t.Fatalf("records = %+v, want the claimed prompt recorded at debug", h.log.Records())
	}
}

func TestSupersedeFailsLoudlyAndRetiresNothing(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{
			name:    "the holds cannot be read",
			arrange: func(h *harness) { h.db.heldErr = errors.New("disk gone") },
			message: "could not read the holds to supersede a prompt; it stands",
		},
		{
			name:    "the retirement cannot be written",
			arrange: func(h *harness) { h.db.tombstoneErr = errors.New("the database is read-only") },
			message: "the drop was refused: the hold could not be retired",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})
			tt.arrange(h)

			// Act
			retired, err := h.q.Supersede(context.Background(), theWorkspace, "t1")

			// Assert
			if err == nil || retired {
				t.Fatalf("Supersede = (%v, %v), want (false, an error)", retired, err)
			}
			h.db.heldErr = nil
			if tomb := h.db.hold("t1").Tombstone; tomb != nil {
				t.Fatalf("tombstone = %+v, want the prompt still standing", tomb)
			}
			if !logged(h.log.Records(), "error", opSupersede, tt.message) {
				t.Fatalf("records = %+v, want %q at error", h.log.Records(), tt.message)
			}
		})
	}
}

func TestALeaseChangeIsPassedToTheLeaseWatcher(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "after the holds are re-judged", arrange: func(*harness) {}},
		{name: "even when the holds cannot be read", arrange: func(h *harness) { h.db.heldErr = errors.New("the store is gone") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(h)

			// Act
			h.q.OnLeaseChanged(theWorkspace)

			// Assert
			if got := h.leases.told(); len(got) != 1 || got[0] != theWorkspace {
				t.Fatalf("watcher told %v, want the one change of %s", got, theWorkspace)
			}
		})
	}
}
