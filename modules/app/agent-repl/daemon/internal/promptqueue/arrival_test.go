package promptqueue

import (
	"context"
	"errors"
	"go/ast"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/classifier"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/wsm"
)

// arrivalOrigins are every origin delivered on arrival.
func arrivalOrigins() []conversationv1.PromptOrigin {
	var out []conversationv1.PromptOrigin
	for value := range conversationv1.PromptOrigin_name {
		if origin := conversationv1.PromptOrigin(value); promptorigin.DeliveredOnArrival(origin) {
			out = append(out, origin)
		}
	}
	return out
}

// arrivalOf is a prompt of ORIGIN, named by its sender when an agent sent it.
func arrivalOf(turn ids.TurnID, origin conversationv1.PromptOrigin) Submission {
	if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT {
		return agentSubmission(turn, "a word for the agent")
	}
	sub := submission(turn, "a word for the agent")
	sub.Origin = origin
	sub.Delivery = wsm.DeliveryDeferred
	return sub
}

// notice is a merge queue notice, the arrival every wait test sends.
func notice(turn ids.TurnID) Submission {
	return arrivalOf(turn, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE)
}

// noHolds fails the test when anything stands in the held prompts.
func noHolds(t *testing.T, h *harness) {
	t.Helper()
	held, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil || len(held) != 0 {
		t.Fatalf("held = %+v, %v; want nothing held", held, err)
	}
}

// waitsUnder fails the test unless TURN alone waits, under REASON.
func waitsUnder(t *testing.T, h *harness, turn ids.TurnID, reason wsm.ArrivalWaitReason) {
	t.Helper()
	waits := h.db.arrivalWaits(t)
	if len(waits) != 1 || waits[0].Turn != turn || waits[0].Reason != reason || waits[0].Why == "" {
		t.Fatalf("waits = %+v, want %s alone waiting on %s", waits, turn, reason)
	}
}

// noWaits fails the test when any arrival wait stands.
func noWaits(t *testing.T, h *harness) {
	t.Helper()
	if waits := h.db.arrivalWaits(t); len(waits) != 0 {
		t.Fatalf("waits = %+v, want none", waits)
	}
}

// started reports whether TURN was started as the session's turn.
func started(h *harness, turn ids.TurnID) bool {
	for _, s := range h.sender.started() {
		if s == turn {
			return true
		}
	}
	return false
}

// ---- delivered when it arrives ----------------------------------------

func TestAPromptDeliveredOnArrivalStartsAnIdleSessionsTurn(t *testing.T) {
	for _, origin := range arrivalOrigins() {
		t.Run(origin.String(), func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			got, err := h.q.Submit(context.Background(), arrivalOf("t1", origin))

			// Assert
			if err != nil || !got.Delivered || got.Waiting != nil || !started(h, "t1") {
				t.Fatalf("Submit = (%+v, %v), started %v; want t1 started at once", got, err, h.sender.started())
			}
			noHolds(t, h)
			noWaits(t, h)
		})
	}
}

func TestAPromptDeliveredOnArrivalJoinsTheRunningTurnUnjudged(t *testing.T) {
	for _, origin := range arrivalOrigins() {
		t.Run(origin.String(), func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")

			// Act
			got, err := h.q.Submit(context.Background(), arrivalOf("t1", origin))
			h.q.waitForClassifications()

			// Assert
			if err != nil || !got.Delivered || len(h.sender.joins) != 1 || h.sender.joins[0] != "t1" {
				t.Fatalf("Submit = (%+v, %v), joins %v; want t1 sent to join the running turn", got, err, h.sender.joins)
			}
			if asked := h.judge.questions(); len(asked) != 0 {
				t.Fatalf("judged = %v, want the classifier never asked", asked)
			}
			noHolds(t, h)
			noWaits(t, h)
		})
	}
}

func TestAJoinedArrivalRecordsItsTurnWithItsSender(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")

	// Act
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	record, ok := h.db.startedTurn("t1")
	if !ok || record.SentBy == nil || *record.SentBy != theSender {
		t.Fatalf("turn = (%+v, %v), want it recorded with sender %s", record, ok, theSender)
	}
}

// ---- the waits a running turn names ------------------------------------

func TestAnArrivalWaitsOutARunningContextCutAndGoesAtItsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.beginCut("cut-turn", conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	got, err := h.q.Submit(context.Background(), notice("t1"))
	if err != nil || got.Waiting == nil || *got.Waiting != wsm.WaitTurnEnd {
		t.Fatalf("Submit = (%+v, %v), want t1 waiting on the turn's end", got, err)
	}
	waitsUnder(t, h, "t1", wsm.WaitTurnEnd)

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "cut-turn", wsm.CloseCompleted)

	// Assert
	if !started(h, "t1") {
		t.Fatalf("started = %v, want t1 started at the cut's end", h.sender.started())
	}
	noHolds(t, h)
	noWaits(t, h)
}

func TestAnArrivalWaitsOutATurnTheVendorStartedAndGoesAtItsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "vendor-turn", "the agent's own work")
	h.watcher.refuseJoins = true
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	waitsUnder(t, h, "t1", wsm.WaitTurnEnd)

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "vendor-turn", wsm.CloseCompleted)

	// Assert
	if !started(h, "t1") || len(h.sender.joins) != 0 {
		t.Fatalf("started = %v, joins = %v; want t1 started at the turn's end, never joined", h.sender.started(), h.sender.joins)
	}
	noWaits(t, h)
}

func TestAnArrivalAnOpenAskRefusesWaitsForTheTurnsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.sender.joinErr = ErrAskOpen

	// Act
	_, err := h.q.Submit(context.Background(), notice("t1"))

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	waitsUnder(t, h, "t1", wsm.WaitTurnEnd)
	if waits := h.db.arrivalWaits(t); waits[0].Why != askOpenReason {
		t.Fatalf("why = %q, want %q", waits[0].Why, askOpenReason)
	}
	noHolds(t, h)
}

func TestAnArrivalTheShimRefusesToJoinWaitsForTheTurnsEndAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.sender.joinErr = errors.New("the shim fell over")

	// Act
	_, err := h.q.Submit(context.Background(), notice("t1"))

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	waitsUnder(t, h, "t1", wsm.WaitTurnEnd)
	if _, found := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opArrival,
		Message: "the shim refused the prompt sent to join the running turn; it waits for it to end"}); !found {
		t.Fatalf("no ERROR record of the refusal; records %+v", h.log.Records())
	}
}

func TestAnArrivalTheVendorAlreadyRecordedIsRetiredWithNoWait(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.sender.recorded = map[ids.TurnID]bool{"t1": true}

	// Act
	got, err := h.q.Submit(context.Background(), notice("t1"))

	// Assert
	if err != nil || !got.Delivered {
		t.Fatalf("Submit = (%+v, %v), want it answered delivered", got, err)
	}
	if how, closed := h.db.closedTurns["t1"]; !closed || how != wsm.CloseWithdrawn {
		t.Fatalf("close = (%s, %v), want t1 closed withdrawn", how.String(), closed)
	}
	noWaits(t, h)
}

// ---- the waits the session names ---------------------------------------

func TestAnArrivalToAWorkspaceWithNoShimRevivesItAndGoesWhenItComesUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() { h.noSession = false }

	// Act
	got, err := h.q.Submit(context.Background(), notice("t1"))
	h.waitRevivals()

	// Assert
	if err != nil || got.Waiting == nil || *got.Waiting != wsm.WaitSession {
		t.Fatalf("Submit = (%+v, %v), want t1 waiting on the session", got, err)
	}
	if h.revivals != 1 || !started(h, "t1") {
		t.Fatalf("revivals = %d, started = %v; want one revival delivering t1", h.revivals, h.sender.started())
	}
	noHolds(t, h)
	noWaits(t, h)
}

func TestAnArrivalToASessionNotStartedWaitsAndGoesWhenItComesUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.notStarted = true
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	waitsUnder(t, h, "t1", wsm.WaitSession)

	// Act
	h.notStarted = false
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if !started(h, "t1") {
		t.Fatalf("started = %v, want t1 started once the session serves", h.sender.started())
	}
	noWaits(t, h)
}

func TestAnArrivalTheShimRefusesForHavingNoSessionWaitsOnTheSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = ErrShimHasNoSession

	// Act
	got, err := h.q.Submit(context.Background(), notice("t1"))

	// Assert
	if err != nil || got.Waiting == nil || *got.Waiting != wsm.WaitSession {
		t.Fatalf("Submit = (%+v, %v), want t1 waiting on the session", got, err)
	}
	waitsUnder(t, h, "t1", wsm.WaitSession)
	noHolds(t, h)
}

func TestANoticeAtAColdGateWaitsOutTheGateWithNoRevival(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.coldGate = "the conversation is cold at 101600 context tokens"

	// Act
	got, err := h.q.Submit(context.Background(), notice("t1"))
	h.waitRevivals()

	// Assert
	if err != nil || got.Waiting == nil || *got.Waiting != wsm.WaitColdGate || h.revivals != 0 || len(h.sender.started()) != 0 {
		t.Fatalf("Submit = (%+v, %v), revivals %d, started %v; want t1 waiting on the gate, nothing revived or started", got, err, h.revivals, h.sender.started())
	}
	waitsUnder(t, h, "t1", wsm.WaitColdGate)
}

func TestAnAgentsPromptAtAColdGateIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.coldGate = "the conversation is cold at 101600 context tokens"

	// Act
	_, err := h.q.Submit(context.Background(), agentSubmission("t1", "hello"))

	// Assert
	var refusal *ColdGateRefusal
	if !errors.As(err, &refusal) {
		t.Fatalf("Submit = %v, want the cold gate's refusal", err)
	}
	noWaits(t, h)
	noHolds(t, h)
}

func TestAWaitingAgentsPromptAGateMeetsWaitsOutTheGate(t *testing.T) {
	// Arrange: the agent's prompt waits on a session that then parks at its
	// gate; its sender is no longer listening, so it is not refused.
	h := newHarness(t)
	h.notStarted = true
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.notStarted = false
	h.coldGate = "the conversation is cold at 101600 context tokens"

	// Act
	h.q.driveArrivals(context.Background(), theWorkspace, arrivalSessionUp, h.log.Global())

	// Assert
	waitsUnder(t, h, "t1", wsm.WaitColdGate)
}

func TestTheGatesAnswerDrivesTheWaitsAtTheGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.coldGate = "the conversation is cold at 101600 context tokens"
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.coldGate = ""

	// Act
	if err := h.q.ColdGateAnswered(context.Background(), theWorkspace, GateSend); err != nil {
		t.Fatalf("ColdGateAnswered: %v", err)
	}

	// Assert
	if !started(h, "t1") {
		t.Fatalf("started = %v, want t1 delivered once the gate was answered", h.sender.started())
	}
	noWaits(t, h)
}

func TestAnArrivalWaitsOutTheVendorAndGoesWhenItServes(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.footer.standVendorBlock("a usage limit")
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	waitsUnder(t, h, "t1", wsm.WaitVendor)

	// Act
	vendorServes(h)

	// Assert
	if !started(h, "t1") {
		t.Fatalf("started = %v, want t1 started once the vendor serves", h.sender.started())
	}
	noWaits(t, h)
}

// ---- the waits a lease names -------------------------------------------

func TestAHoldingLeaseNamesTheArrivalsWait(t *testing.T) {
	tests := []struct {
		holder wsm.LeaseHolder
		want   wsm.ArrivalWaitReason
	}{
		{holder: wsm.HolderDrain, want: wsm.WaitShutdown},
		{holder: wsm.HolderRestart, want: wsm.WaitBounce},
		{holder: wsm.HolderHibernate, want: wsm.WaitSession},
		{holder: wsm.HolderMerge, want: wsm.WaitMerge},
	}
	for _, tt := range tests {
		t.Run(tt.want.String(), func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
			h.lease(tt.holder, wsm.PolicyHold)

			// Act
			_, err := h.q.Submit(context.Background(), notice("t1"))

			// Assert
			if err != nil {
				t.Fatalf("Submit: %v", err)
			}
			waitsUnder(t, h, "t1", tt.want)
			noHolds(t, h)
		})
	}
}

func TestAMergesReleaseDeliversTheArrivalWaitingOnIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	h.clearLease()
	h.q.OnLeaseChanged(theWorkspace)

	// Assert
	if !started(h, "t1") {
		t.Fatalf("started = %v, want t1 started once the merge released its lease", h.sender.started())
	}
	noWaits(t, h)
}

func TestARefusingLeaseRefusesAnArrival(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyRefuse)

	// Act
	got, err := h.q.Submit(context.Background(), notice("t1"))

	// Assert
	if !errors.Is(err, ErrMerging) || got.RefusedArm != ArmMerging {
		t.Fatalf("Submit = (%+v, %v), want the merge's refusal", got, err)
	}
	noWaits(t, h)
}

func TestTheBootDeliversAnArrivalThatWaitedOutThePredecessorsExit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()

	// Act
	err := h.q.DriveArrivalWaits(context.Background())

	// Assert
	if err != nil || !started(h, "t1") {
		t.Fatalf("DriveArrivalWaits = %v, started %v; want t1 started", err, h.sender.started())
	}
	noWaits(t, h)
}

func TestTheBootsDriveFailsLoudlyWhenTheWaitsCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.waitsErr = errors.New("the state store is gone")

	// Act
	err := h.q.DriveArrivalWaits(context.Background())

	// Assert
	if err == nil {
		t.Fatalf("DriveArrivalWaits = nil, want the failed read")
	}
	if _, found := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opArrival,
		Message: "the boot could not read the arrival waits; none is driven"}); !found {
		t.Fatalf("no ERROR record of the failed read; records %+v", h.log.Records())
	}
}

// ---- the shim call's wait ----------------------------------------------

func TestAnArrivalBehindAStandingCallGoesAtItsSettle(t *testing.T) {
	// Arrange: a user's prompt's StartTurn stands while the notice arrives.
	h := newHarness(t)
	h.watcher.standOnOpening = true
	var during Disposition
	h.sender.startHook = func() {
		h.sender.startHook = nil
		var err error
		if during, err = h.q.Submit(context.Background(), notice("t2")); err != nil {
			t.Errorf("Submit(t2): %v", err)
		}
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "the user's work")); err != nil {
		t.Fatalf("Submit(t1): %v", err)
	}

	// Assert: it waited on the call, and the call's settle sent it to join
	// the turn the call opened.
	if during.Waiting == nil || *during.Waiting != wsm.WaitShimCall {
		t.Fatalf("during the call = %+v, want t2 waiting on the call", during)
	}
	if len(h.sender.joins) != 1 || h.sender.joins[0] != "t2" {
		t.Fatalf("joins = %v, want t2 joining the turn the call opened", h.sender.joins)
	}
	noWaits(t, h)
}

// ---- recording a wait --------------------------------------------------

func TestAnArrivalWhoseWaitCannotBeRecordedFailsLoudly(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.notStarted = true
	h.db.putWaitErr = errors.New("the state store is gone")

	// Act
	_, err := h.q.Submit(context.Background(), notice("t1"))

	// Assert
	if err == nil {
		t.Fatalf("Submit = nil, want the failed record surfaced")
	}
	if _, found := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opArrival,
		Message: "could not record the wait of a prompt delivered on arrival"}); !found {
		t.Fatalf("no ERROR record of the failed wait; records %+v", h.log.Records())
	}
	noHolds(t, h)
	if len(h.sender.started()) != 0 {
		t.Fatalf("started = %v, want nothing", h.sender.started())
	}
}

func TestADriveThatCannotReadTheWaitsRecordsItAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.waitsErr = errors.New("the state store is gone")

	// Act
	h.q.driveArrivals(context.Background(), theWorkspace, arrivalSessionUp, h.log.Global())

	// Assert
	if _, found := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opArrival,
		Message: "could not read the arrival waits the event ends; they wait for the next one"}); !found {
		t.Fatalf("no ERROR record of the failed read; records %+v", h.log.Records())
	}
}

func TestAWaitingArrivalTheShimRefusesKeepsItsWait(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.notStarted = true
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.notStarted = false
	h.sender.startErr = errors.New("the shim fell over")

	// Act
	h.q.driveArrivals(context.Background(), theWorkspace, arrivalSessionUp, h.log.Global())

	// Assert
	waitsUnder(t, h, "t1", wsm.WaitSession)
	if _, found := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opArrival,
		Message: "the waiting prompt could not be delivered; its wait stands for the next event"}); !found {
		t.Fatalf("no ERROR record of the failed delivery; records %+v", h.log.Records())
	}
}

// ---- superseding a waiting notice --------------------------------------

func TestSupersedeEndsAWaitingNotice(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.notStarted = true
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	superseded, err := h.q.Supersede(context.Background(), theWorkspace, "t1")

	// Assert
	if err != nil || !superseded {
		t.Fatalf("Supersede = (%v, %v), want the waiting notice superseded", superseded, err)
	}
	noWaits(t, h)
}

func TestSupersedeLeavesAToldNoticeAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	superseded, err := h.q.Supersede(context.Background(), theWorkspace, "t1")

	// Assert
	if err != nil || superseded {
		t.Fatalf("Supersede = (%v, %v), want a told notice left alone", superseded, err)
	}
}

// ---- a withdrawn join ---------------------------------------------------

func TestAnArrivalTheVendorGivesBackWaitsForTheTurnsEndUnderANewTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	h.q.OnTurnEnded(theWorkspace, "t1", wsm.CloseWithdrawn)

	// Assert
	waits := h.db.arrivalWaits(t)
	if len(waits) != 1 || waits[0].Turn == "t1" || waits[0].Reason != wsm.WaitTurnEnd || waits[0].Why != askOpenReason ||
		saidText(waits[0].Said) != "a word for the agent" {
		t.Fatalf("waits = %+v, want the given-back notice waiting on the turn's end under a new turn", waits)
	}
	noHolds(t, h)
}

// ---- which event ends which wait ---------------------------------------

func TestWhichEventEndsWhichWait(t *testing.T) {
	all := []wsm.ArrivalWaitReason{wsm.WaitSession, wsm.WaitColdGate, wsm.WaitBounce, wsm.WaitShutdown,
		wsm.WaitMerge, wsm.WaitTurnEnd, wsm.WaitVendor, wsm.WaitShimCall}
	tests := []struct {
		edge arrivalEdge
		ends []wsm.ArrivalWaitReason
	}{
		{edge: arrivalSessionUp, ends: all},
		{edge: arrivalBounceEnded, ends: all},
		{edge: arrivalBoot, ends: all},
		{edge: arrivalTurnEnded, ends: []wsm.ArrivalWaitReason{wsm.WaitTurnEnd}},
		{edge: arrivalLeaseChanged, ends: []wsm.ArrivalWaitReason{wsm.WaitMerge, wsm.WaitBounce, wsm.WaitSession, wsm.WaitShutdown}},
		{edge: arrivalGateAnswered, ends: []wsm.ArrivalWaitReason{wsm.WaitColdGate}},
		{edge: arrivalCallSettled, ends: []wsm.ArrivalWaitReason{wsm.WaitShimCall}},
	}
	for _, tt := range tests {
		t.Run(tt.edge.String(), func(t *testing.T) {
			want := map[wsm.ArrivalWaitReason]bool{}
			for _, r := range tt.ends {
				want[r] = true
			}
			for _, r := range all {
				// Act, Assert
				if got := tt.edge.ends(r); got != want[r] {
					t.Errorf("%s ends %s = %v, want %v", tt.edge, r, got, want[r])
				}
			}
		})
	}
}

func TestAJoiningArrivalKeepsNoUsersPromptFromTheClassifier(t *testing.T) {
	// Arrange: a notice joins the running turn and the vendor has not yet
	// folded it in.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), notice("t1")); err != nil {
		t.Fatalf("Submit(t1): %v", err)
	}
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t2", "the user's next words")); err != nil {
		t.Fatalf("Submit(t2): %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if asked := h.judge.questions(); len(asked) != 1 || asked[0][0] != "the running work" {
		t.Fatalf("judged = %v, want the user's prompt judged against the running turn", asked)
	}
}

// THE LEASE'S HOLDER IS READ ONCE: the arrival path names its wait from the
// hold kind holdForLease projects, so no other function switches on a holder.
func TestOnlyHoldForLeaseReadsALeasesHolder(t *testing.T) {
	// Arrange
	fset, funcs := productionFuncs(t)

	// Act
	var found []string
	for _, fn := range funcs {
		if fn.Name.Name == "holdForLease" || fn.Name.Name == "holderName" || fn.Name.Name == "noteDrainRefusal" || fn.Name.Name == "exemptFromLease" {
			continue
		}
		ast.Inspect(fn, func(n ast.Node) bool {
			if clause, ok := n.(*ast.CaseClause); ok {
				for _, e := range clause.List {
					if sel, ok := e.(*ast.SelectorExpr); ok && sel.Sel.Name == "HolderMerge" {
						found = append(found, fset.Position(sel.Pos()).String()+" in "+fn.Name.Name)
					}
				}
			}
			return true
		})
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("these functions switch on a lease's holder instead of reading holdForLease's hold kind: %v", found)
	}
}

// ---- an agent's prompt -------------------------------------------------

func TestAnAgentsPromptJoiningTheRunningTurnInterruptsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "urgent"}

	// Act
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "the build is green")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if killed := h.sender.killed(); len(killed) != 0 || askedCount(h) != 0 {
		t.Fatalf("killed = %v, asked %d times; want nothing interrupted and nothing judged", killed, askedCount(h))
	}
}

func TestASecondAgentPromptJoinsBesideTheFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "the build is green")); err != nil {
		t.Fatalf("Submit(t1): %v", err)
	}

	// Act
	if _, err := h.q.Submit(context.Background(), agentSubmission("t2", "and the lint is clean")); err != nil {
		t.Fatalf("Submit(t2): %v", err)
	}

	// Assert
	if joins := h.sender.joins; len(joins) != 2 || joins[0] != "t1" || joins[1] != "t2" {
		t.Fatalf("joins = %v, want both agent prompts sent to join", joins)
	}
}

func TestAnAgentsPromptBehindAHeldUsersPromptStillJoinsUnjudged(t *testing.T) {
	// Arrange: a user's prompt waits held, ruled to queue.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	asked := askedCount(h)

	// Act
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "the build is green")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if joins := h.sender.joins; len(joins) != 1 || joins[0] != "t1" || askedCount(h) != asked {
		t.Fatalf("joins = %v, asked %d more times; want t1 joined unjudged", joins, askedCount(h)-asked)
	}
}

func TestADeferredAgentPromptWaitsForTheRunningTurnsEndAndThenRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	sub := agentSubmission("t1", "after this, run the tests")
	sub.Delivery = wsm.DeliveryDeferred
	if _, err := h.q.Submit(context.Background(), sub); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if len(h.sender.joins) != 0 {
		t.Fatalf("joins = %v, want the deferred prompt not joined", h.sender.joins)
	}
	waitsUnder(t, h, "t1", wsm.WaitTurnEnd)

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if !started(h, "t1") {
		t.Fatalf("started = %v, want t1 run as its own turn", h.sender.started())
	}
	noHolds(t, h)
	noWaits(t, h)
}

func TestAUsersPromptBehindARunningTurnIsStillClassified(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "an unrelated question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if got := askedCount(h); got != 1 {
		t.Fatalf("the judge was asked %d times, want the user's prompt classified once", got)
	}
}
