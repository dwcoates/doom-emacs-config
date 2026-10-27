package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/classifier"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE 2026-10-10 INCIDENT, deterministically: a prompt is held for the revival
// of a workspace with no session, the revival parks the session at its cold
// gate, and the prompt must wait at the gate (never delivered, never stranded,
// never an error) until the gate's answer settles it (gatefate_test.go): a
// prompt still on the gate's hold when the session comes up is delivered.

// gateDetail is the standing gate's own account in these tests.
const gateDetail = "the conversation is cold at 90,000 context tokens"

// parkAtGate states what a revival that parks at its cold gate leaves: a shim
// client, no started session, no watcher, and the gate standing.
func parkAtGate(h *harness) {
	// A GATE RUNS NO SHIM until it is answered (owner ruling, 2026-10-27):
	// its shim is stood down, so no client answers either.
	h.noSession = true
	h.notStarted = true
	h.noWatcher = true
	h.coldGate = gateDetail
}

// answerGate states what the gate's answer leaves: the gate gone and the
// session up and watched.
func answerGate(h *harness) {
	h.coldGate = ""
	h.noSession = false
	h.notStarted = false
	h.noWatcher = false
}

// revivedIntoTheGate submits t1 to a workspace with no session whose revival
// parks at the cold gate, and joins the revival.
func revivedIntoTheGate(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() { parkAtGate(h) }
	if _, err := h.q.Submit(context.Background(), submission("t1", "the owner's prompt")); err != nil {
		t.Fatalf("Submit = %v, want the prompt held for the revival", err)
	}
	h.waitRevivals()
	return h
}

// strandedRow records the row an older build left behind: a standing prompt
// with no hold and no verdict.
func strandedRow(t *testing.T, h *harness, turn ids.TurnID) {
	t.Helper()
	if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{
		Workspace: theWorkspace, Turn: turn, Said: userSaid("stranded"),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT.String(), QueuedAt: instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
}

// heldUnder reports whether the turn's standing hold is under KIND.
func heldUnder(h *harness, turn ids.TurnID, kind wsm.HoldKind) bool {
	got := h.db.hold(turn)
	return got.Tombstone == nil && got.Hold != nil && *got.Hold == kind
}

// errorRecords answers every ERROR record.
func errorRecords(records []dlog.Record) []dlog.Record {
	var out []dlog.Record
	for _, r := range records {
		if r.Level == "error" {
			out = append(out, r)
		}
	}
	return out
}

func TestAColdGatedRevivalHoldsItsPromptAtTheGate(t *testing.T) {
	// Arrange / Act
	h := revivedIntoTheGate(t)

	// Assert
	if !heldUnder(h, "t1", wsm.HoldColdGate) {
		t.Fatalf("hold = %+v, want t1 held at the cold gate", h.db.hold("t1"))
	}
}

func TestAColdGatedRevivalStartsNoTurn(t *testing.T) {
	// Arrange / Act
	h := revivedIntoTheGate(t)

	// Assert
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing sent into a session parked at its gate", got)
	}
}

func TestAColdGatedRevivalRecordsNoError(t *testing.T) {
	// Arrange / Act
	h := revivedIntoTheGate(t)

	// Assert
	if got := errorRecords(h.log.Records()); len(got) != 0 {
		t.Fatalf("error records = %+v, want none: a gate is a designed state", got)
	}
}

func TestAColdGatedRevivalNeverSaysTheVendorServes(t *testing.T) {
	// Arrange / Act
	h := revivedIntoTheGate(t)

	// Assert
	if logged(h.log.Records(), "info", opSubmit, "the session is up and the vendor serves it; released the prompts held after reconnect") {
		t.Fatal("the release claimed a served session for one parked at its cold gate")
	}
}

func TestAColdGatedRevivalNeverSaysItRevivedTheSession(t *testing.T) {
	// Arrange / Act
	h := revivedIntoTheGate(t)

	// Assert
	if logged(h.log.Records(), "info", opSubmit, "revived the workspace's session for the submission") {
		t.Fatal("the revival claimed a session it parked at its cold gate")
	}
}

func TestAColdGatedRevivalSaysItParkedAtTheGate(t *testing.T) {
	// Arrange / Act
	h := revivedIntoTheGate(t)

	// Assert
	if !logged(h.log.Records(), "info", opSubmit, "the revival brought the shim up parked at its cold gate; the prompts wait for the gate's answer") {
		t.Fatalf("records = %+v, want the park said at INFO", h.log.Records())
	}
}

func TestAColdGatedRevivalPublishesTheColdGateHold(t *testing.T) {
	// Arrange / Act
	h := revivedIntoTheGate(t)

	// Assert
	last := h.holds.last()
	if len(last) != 1 || last[0].Hold == nil || *last[0].Hold != wsm.HoldColdGate {
		t.Fatalf("last tray = %+v, want t1 drawn at the cold gate", last)
	}
}

func TestTheSessionUpEdgeDeliversAPromptStillHeldAtTheGate(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	answerGate(h)

	// Act: the answer's session-up edge.
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want t1 delivered once the gate is answered", got)
	}
}

func TestTheSessionUpEdgeRetiresTheDeliveredGateHold(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	answerGate(h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.db.hold("t1").Tombstone; got == nil || got.Kind != tombstoneDelivered {
		t.Fatalf("tombstone = %+v, want t1 retired as delivered", got)
	}
}

func TestASessionUpReleaseIntoAShimWithNoStartedSessionKeepsTheReconnectHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.notStarted = true
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if !heldUnder(h, "t1", wsm.HoldReconnect) || len(h.sender.started()) != 0 {
		t.Fatalf("hold = %+v, started = %v, want t1 still waiting under the reconnect hold", h.db.hold("t1"), h.sender.started())
	}
}

func TestASessionUpReleaseDrivesAStrandedUnheldPrompt(t *testing.T) {
	// Arrange: the row the incident left, on a session that serves.
	h := newHarness(t)
	strandedRow(t, h, "t1")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want the stranded prompt delivered", got)
	}
}

func TestASessionUpReleaseHoldsAStrandedPromptAtTheStandingGate(t *testing.T) {
	// Arrange: the row the incident left, on the session parked at its gate.
	h := newHarness(t)
	parkAtGate(h)
	strandedRow(t, h, "t1")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if !heldUnder(h, "t1", wsm.HoldColdGate) {
		t.Fatalf("hold = %+v, want the stranded prompt held at the cold gate", h.db.hold("t1"))
	}
}

func TestASessionUpReleaseLeavesABuildRefreshHoldStanding(t *testing.T) {
	// Arrange: a hold of another condition behind the gate.
	h := newHarness(t)
	parkAtGate(h)
	strandedRow(t, h, "t1")
	refresh := wsm.HoldBuildRefresh
	if err := h.db.UpdateHeldPromptHold(context.Background(), "t1", &refresh, ""); err != nil {
		t.Fatalf("UpdateHeldPromptHold: %v", err)
	}

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if !heldUnder(h, "t1", wsm.HoldBuildRefresh) {
		t.Fatalf("hold = %+v, want the build-refresh hold untouched", h.db.hold("t1"))
	}
}

func TestAPopIntoAParkedSessionHoldsTheEntryAtTheGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	parkAtGate(h)
	strandedRow(t, h, "t1")
	d := h.q.lockDelivery(theWorkspace)

	// Act
	delivered, err := h.q.popAndDeliver(context.Background(), d, h.log.Global())
	d.unlock()

	// Assert
	if delivered || err != nil || !heldUnder(h, "t1", wsm.HoldColdGate) {
		t.Fatalf("pop = (%v, %v), hold = %+v, want nothing delivered and t1 held at the gate", delivered, err, h.db.hold("t1"))
	}
}

func TestAPopIntoAShimWithNoStartedSessionHoldsTheEntryForTheReconnect(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.notStarted = true
	strandedRow(t, h, "t1")
	d := h.q.lockDelivery(theWorkspace)

	// Act
	delivered, err := h.q.popAndDeliver(context.Background(), d, h.log.Global())
	d.unlock()

	// Assert
	if delivered || err != nil || !heldUnder(h, "t1", wsm.HoldReconnect) {
		t.Fatalf("pop = (%v, %v), hold = %+v, want nothing delivered and t1 held for the reconnect", delivered, err, h.db.hold("t1"))
	}
}

func TestADeliveryThatFindsNoWatcherPutsTheEntryBackOnTheReconnectHold(t *testing.T) {
	// Arrange: a shim and a started session with no watcher, which nothing
	// explains: the defect the ERROR still names.
	h := newHarness(t)
	h.noWatcher = true
	strandedRow(t, h, "t1")
	d := h.q.lockDelivery(theWorkspace)

	// Act
	_, _ = h.q.deliverHeld(context.Background(), d, h.db.hold("t1"), h.log.Global())
	d.unlock()

	// Assert
	if !heldUnder(h, "t1", wsm.HoldReconnect) {
		t.Fatalf("hold = %+v, want t1 back on the reconnect hold", h.db.hold("t1"))
	}
}

func TestADeliveryThatFindsNoWatcherStillAnswersNoSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noWatcher = true
	strandedRow(t, h, "t1")
	d := h.q.lockDelivery(theWorkspace)

	// Act
	_, err := h.q.deliverHeld(context.Background(), d, h.db.hold("t1"), h.log.Global())
	d.unlock()

	// Assert
	if !errors.Is(err, ErrNoSession) {
		t.Fatalf("deliverHeld = %v, want ErrNoSession still surfaced", err)
	}
}

func TestADeliveryWhoseRevivalParksAtTheGateHoldsTheEntryThere(t *testing.T) {
	// Arrange: a hibernated workspace's held prompt, released by its lease.
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() { parkAtGate(h) }
	strandedRow(t, h, "t1")
	d := h.q.lockDelivery(theWorkspace)

	// Act
	sent, err := h.q.deliverHeld(context.Background(), d, h.db.hold("t1"), h.log.Global())
	d.unlock()

	// Assert
	if sent || err != nil || !heldUnder(h, "t1", wsm.HoldColdGate) {
		t.Fatalf("deliverHeld = (%v, %v), hold = %+v, want t1 held at the gate with no error", sent, err, h.db.hold("t1"))
	}
}

func TestReleaseRefusesAPromptHeldAtTheColdGate(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)

	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")

	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("Release = %v, want refused while the gate stands", err)
	}
}

func TestALeaseEndingOverAParkedSessionHoldsItsPromptsAtTheGate(t *testing.T) {
	// Arrange: a merge-held prompt on a session parked at its gate.
	h := newHarness(t)
	parkAtGate(h)
	h.lease(wsm.HolderMerge, wsm.PolicyHold)
	strandedRow(t, h, "t1")
	merge := wsm.HoldMerge
	if err := h.db.UpdateHeldPromptHold(context.Background(), "t1", &merge, ""); err != nil {
		t.Fatalf("UpdateHeldPromptHold: %v", err)
	}
	h.clearLease()

	// Act
	h.q.OnLeaseChanged(theWorkspace)

	// Assert
	if !heldUnder(h, "t1", wsm.HoldColdGate) || len(h.sender.started()) != 0 {
		t.Fatalf("hold = %+v, started = %v, want t1 held at the gate and nothing sent", h.db.hold("t1"), h.sender.started())
	}
}

func TestASessionUpReleaseClassifiesAStrandedPromptBehindTheOneItDelivers(t *testing.T) {
	// Arrange: two rows the incident's build left, on a session that serves.
	h := newHarness(t)
	h.watcher.standOnOpening = true
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	strandedRow(t, h, "t1")
	strandedRow(t, h, "t2")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)
	h.q.waitForClassifications()

	// Assert: t1 runs, and t2 is judged against it rather than left unjudged.
	got := h.db.hold("t2")
	if got.Classification == nil || got.Classification.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("t2 = %+v, want it classified against the turn t1 started", got)
	}
}

func TestALeaseEvaluationThatChangesNothingDeliversAStrandedPrompt(t *testing.T) {
	// Arrange: the row the incident left, on a session that serves, reached by
	// a handover's drain (OnLeaseChanged with no lease standing).
	h := newHarness(t)
	strandedRow(t, h, "t1")

	// Act
	h.q.OnLeaseChanged(theWorkspace)

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want the stranded prompt delivered", got)
	}
}

func TestALeaseEvaluationThatChangesNothingDeliversNothingBesideARunningTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.running("t0")
	strandedRow(t, h, "t1")

	// Act
	h.q.OnLeaseChanged(theWorkspace)

	// Assert
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing beside the running turn", got)
	}
}

// ---- the facts come first (2026-10-10, post-merge) --------------------------
//
// A boot adopts a shim mid-turn and releases its holds before the watcher has
// taken the session facts up, so TurnInFlight answers nil for the turn the
// shim is running. The release and the idle pop must deliver nothing on that
// nil, and must take their step again from the facts-in edge.

// adoptedBeforeFacts is a session that serves whose facts are not yet in, with
// a stranded row standing.
func adoptedBeforeFacts(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.watcher.factsPending = true
	strandedRow(t, h, "t1")
	return h
}

func TestASessionUpReleaseBeforeTheFactsDeliversNothing(t *testing.T) {
	// Arrange
	h := adoptedBeforeFacts(t)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing before the facts say whether a turn runs", got)
	}
}

func TestASessionUpReleaseWhoseFactsNameARunningTurnDeliversNothingBesideIt(t *testing.T) {
	// Arrange
	h := adoptedBeforeFacts(t)
	h.q.ReleaseReconnectHolds(theWorkspace)
	adopted := idsTurn("t0")

	// Act: the facts arrive naming the adopted shim's running turn.
	h.watcher.takeFacts(&adopted)
	h.q.releasing.Wait()

	// Assert
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing beside the adopted turn", got)
	}
}

func TestASessionUpReleaseWhoseFactsNameNoTurnDeliversTheStrandedPrompt(t *testing.T) {
	// Arrange
	h := adoptedBeforeFacts(t)
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Act: the facts arrive with nothing in flight.
	h.watcher.takeFacts(nil)
	h.q.releasing.Wait()

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want the stranded prompt delivered once the facts are in", got)
	}
}

func TestAnIdlePopBeforeTheFactsDeliversNothing(t *testing.T) {
	// Arrange
	h := adoptedBeforeFacts(t)

	// Act: a boot's orphan-lease release reaches the holds through here.
	h.q.OnLeaseChanged(theWorkspace)

	// Assert
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing before the facts say whether a turn runs", got)
	}
}

func TestAnIdlePopWhoseFactsNameARunningTurnDeliversNothingBesideIt(t *testing.T) {
	// Arrange
	h := adoptedBeforeFacts(t)
	h.q.OnLeaseChanged(theWorkspace)
	adopted := idsTurn("t0")

	// Act
	h.watcher.takeFacts(&adopted)
	h.q.releasing.Wait()

	// Assert
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing beside the adopted turn", got)
	}
}

func TestASessionUpReleaseNeverDrivesARestoredDeferredPrompt(t *testing.T) {
	tests := []struct {
		name string
		row  func(h *harness)
	}{
		{name: "a deferred prompt restored with no hold and no verdict", row: func(h *harness) {
			putRow(t, h, wsm.HeldPrompt{Delivery: wsm.DeliveryDeferred})
		}},
		{name: "a deferred prompt restored with its turn-end verdict", row: func(h *harness) {
			putRow(t, h, wsm.HeldPrompt{Delivery: wsm.DeliveryDeferred,
				Classification: &wsm.Classification{Arm: wsm.ArmHoldForTurnEnd, At: instant}})
		}},
		{name: "an ordinary prompt restored with its turn-end verdict", row: func(h *harness) {
			putRow(t, h, wsm.HeldPrompt{Classification: &wsm.Classification{Arm: wsm.ArmHoldForTurnEnd, At: instant}})
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a session that serves, facts in, nothing running.
			h := newHarness(t)
			tc.row(h)

			// Act
			h.q.ReleaseReconnectHolds(theWorkspace)

			// Assert
			if got := h.sender.started(); len(got) != 0 {
				t.Fatalf("started = %v, want the prompt left for the turn's end", got)
			}
		})
	}
}

// putRow records t1 as a standing row shaped by OVER's delivery and verdict.
func putRow(t *testing.T, h *harness, over wsm.HeldPrompt) {
	t.Helper()
	over.Workspace, over.Turn, over.Said, over.QueuedAt = theWorkspace, "t1", userSaid("restored"), instant
	over.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT.String()
	if err := h.db.PutHeldPrompt(context.Background(), over); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
}

func TestANoticeSentToAGateWithNoShimIsHeldThereWithNoRevival(t *testing.T) {
	// Arrange: the gate stands and runs no shim; a notice waits out a gate.
	h := newHarness(t)
	parkAtGate(h)
	sub := submission("t1", "a merge landed")
	sub.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE

	// Act
	_, err := h.q.Submit(context.Background(), sub)

	// Assert
	if err != nil || !heldUnder(h, "t1", wsm.HoldColdGate) || h.revivals != 0 {
		t.Fatalf("Submit = %v, hold = %+v, revivals = %d; want t1 held at the gate and nothing revived", err, h.db.hold("t1"), h.revivals)
	}
}
