package sessioncontroller

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/ssm"
)

// ---------------------------------------------------------------------------
// THE INVARIANT: nothing about a turn's accounting can decide whether that turn
// is over.
//
// Reproduced here is the exact production interleaving of session
// s_f223cd698d687299: a usage correction for msg_011CdswkkMZyUqE6amdCWSjJ at
// 18:14:30.105, the terminal result 52ms later at 18:14:30.157, and under the
// old code a hold that then waited for a `TurnEnded` the shim did not produce
// for ten minutes. The turn stayed open, the workspace rendered `thinking`, and
// the user's answer sat in a map.
// ---------------------------------------------------------------------------

// productionAPIMessageID is the response the observed correction named. Using
// the real id keeps the test and the incident log searchable as one thing.
const productionAPIMessageID = "msg_011CdswkkMZyUqE6amdCWSjJ"

// settledTurnRig drives one turn to its terminal result through the ordinary
// consumer paths, with the correction placed on whichever side of the result
// the case is about.
type settledTurnRig struct {
	// turnID is the turn this rig drives. It is a field rather than a constant
	// because a re-driven turn is named by the resumption that owed it.
	turnID   string
	consumer *consumer
	applier  *fakeApplier
	push     *fakePusher
	prog     *fakeProgress
	store    *settlingTurnAccountingStore
	logs     *levelSplitLogs
}

// newSettledTurnRig admits a turn, delivers one assistant response, and returns
// the rig ready for the result. It does NOT deliver the correction: each test
// decides where that lands.
func newSettledTurnRig(t *testing.T) *settledTurnRig {
	t.Helper()
	rig := &settledTurnRig{
		turnID:  "t",
		applier: &fakeApplier{},
		push:    &fakePusher{},
		prog:    &fakeProgress{},
		store:   newSettlingTurnAccountingStore(),
		logs:    &levelSplitLogs{},
	}
	rig.consumer = newConsumer("ws", "s1", rig.push, rig.applier, rig.prog, newFakeClearCompactStore(),
		rig.store, rig.logs.logf, nil, nil, nil, nil, nil)
	rig.consumer.warnf = rig.logs.warnf
	// The authoritative query the turn runs under. Every account-usage
	// observation is validated against it, so a rig without one could not
	// deliver the end boundary the stamp depends on.
	if err := rig.consumer.Apply(&corev1.Event{Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "q",
		Event:           &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{}},
	}}}); err != nil {
		t.Fatalf("admit the query: %v", err)
	}
	if err := rig.consumer.Apply(&corev1.Event{
		Seq: 1, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t",
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}},
	}); err != nil {
		t.Fatalf("admit the turn: %v", err)
	}
	if err := rig.consumer.Consume(snapshotAssistantEvent(t, productionAPIMessageID, 563)); err != nil {
		t.Fatalf("deliver the assistant response: %v", err)
	}
	return rig
}

// correct delivers the vendor's message_delta correction for the turn's one
// response.
func (r *settledTurnRig) correct(t *testing.T, outputTokens int64) {
	t.Helper()
	if err := r.consumer.Consume(messageDeltaUsageEvent(t, productionAPIMessageID, &datav1.ApiUsage{InputTokens: 33, OutputTokens: outputTokens})); err != nil {
		t.Fatalf("deliver the message_delta correction: %v", err)
	}
}

// endBoundary delivers the turn's END-BOUNDARY account-usage observation, the
// sample the stamp's reconciliation is measured against. It rides its own frame
// beside the shim's `TurnEnded`, so it lands after the vendor's result.
func (r *settledTurnRig) endBoundary(t *testing.T) {
	t.Helper()
	if err := r.consumer.Apply(&corev1.Event{
		Seq: 3, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: r.turnID,
		Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: usageObservation(r.turnID, false)},
	}); err != nil {
		t.Fatalf("deliver the turn-end usage observation: %v", err)
	}
}

// reDriveRequestID is a re-drive's request id, minted exactly as
// turnresumption.go mints one. The turn it names is invisible to the user and
// entirely ordinary to the accounting ledger.
var reDriveRequestID = resumptionRequestID("ws", "t-interrupted", 1786320000000)

// newReDrivenTurnRig is the same rig over a turn the daemon re-drove after a
// bounce rather than one the user typed.
func newReDrivenTurnRig(t *testing.T) *settledTurnRig {
	t.Helper()
	rig := &settledTurnRig{
		turnID:  reDriveRequestID,
		applier: &fakeApplier{},
		push:    &fakePusher{},
		prog:    &fakeProgress{},
		store:   newSettlingTurnAccountingStore(),
		logs:    &levelSplitLogs{},
	}
	rig.consumer = newConsumer("ws", "s1", rig.push, rig.applier, rig.prog, newFakeClearCompactStore(),
		rig.store, rig.logs.logf, nil, nil, nil, nil, nil)
	rig.consumer.warnf = rig.logs.warnf
	if err := rig.consumer.Apply(&corev1.Event{Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "q",
		Event:           &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{}},
	}}}); err != nil {
		t.Fatalf("admit the query: %v", err)
	}
	if err := rig.consumer.Apply(&corev1.Event{
		Seq: 1, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: reDriveRequestID,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: reDriveRequestID}},
	}); err != nil {
		t.Fatalf("admit the re-driven turn: %v", err)
	}
	return rig
}

// result delivers the vendor's terminal result, which is the turn's own end.
func (r *settledTurnRig) result(t *testing.T) {
	t.Helper()
	ev := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{
			Usage: &datav1.Usage{InputTokens: 33, OutputTokens: 720},
		}},
	})
	ev.RequestId = r.turnID
	if err := r.consumer.Consume(ev); err != nil {
		t.Fatalf("deliver the terminal result: %v", err)
	}
}

// THE PRODUCTION WEDGE, END TO END. Each case delivers the same two facts in a
// different order and asserts the same outcome: the turn's durable claim is
// closed by the result, with no `TurnEnded` anywhere in the run.
func TestATerminalResultSettlesItsTurnUnderEitherCorrectionOrdering(t *testing.T) {
	tests := []struct {
		name            string
		correctionFirst bool
	}{
		{name: "the observed interleaving: correction 52ms before the result", correctionFirst: true},
		{name: "the ordinary interleaving: correction after the result", correctionFirst: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			rig := newSettledTurnRig(t)

			// Act.
			if tc.correctionFirst {
				rig.correct(t, 720)
				rig.result(t)
			} else {
				rig.result(t)
				rig.correct(t, 720)
			}

			// Assert.
			if got := rig.applier.synthesizedTurnCloses(); len(got) != 1 || got[0] != ssm.TurnCloseTerminalResult {
				t.Fatalf("synthesized closes = %v, want the turn settled from its own terminal result", got)
			}
		})
	}
}

// The turn's answer reaches the feed on the result's own arrival. Under the old
// code this push was what the accounting hold withheld.
func TestATerminalResultReachesTheConversationOnArrival(t *testing.T) {
	// Arrange — the observed interleaving, with the correction already on file.
	rig := newSettledTurnRig(t)
	rig.correct(t, 720)
	before := len(rig.push.conversationDeltas())

	// Act.
	rig.result(t)

	// Assert.
	if got := len(rig.push.conversationDeltas()); got != before+1 {
		t.Fatalf("conversation deltas = %d, want %d — the turn's answer must be published by the result itself", got, before+1)
	}
}

// THE ABOLISHED GATE, NAMED. The old log line is the signature of the wedge and
// must not come back: no path may report a terminal result as held.
func TestNoTerminalResultIsEverReportedAsHeldForAccounting(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	rig.correct(t, 720)

	// Act.
	rig.result(t)

	// Assert.
	record := strings.Join(append(append([]string(nil), rig.logs.info...), rig.logs.warn...), "\n")
	if strings.Contains(record, "held for accounting") {
		t.Fatalf("log = %v, want no record of a terminal result held behind its accounting", record)
	}
}

// A turn whose accounting store REFUSES every write still ends. The stamp is
// the only casualty, which is the whole point of the separation.
func TestATerminalResultSettlesItsTurnEvenWhenTheAccountingStoreRefuses(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	rig.consumer.accountingStore = failingTurnAccountingStore{err: errors.New("accounting store is unwritable")}
	rig.correct(t, 720)

	// Act.
	rig.result(t)

	// Assert.
	if got := rig.applier.synthesizedTurnCloses(); len(got) != 1 {
		t.Fatalf("synthesized closes = %v, want the turn settled despite an unwritable accounting store", got)
	}
}

// The stamp settles as soon as its last dependency lands, with no second
// authority asked. Here the end boundary is the last one.
func TestATerminalResultWithEveryDependencyOnFileSettlesItsStamp(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	rig.correct(t, 720)

	// Act.
	rig.result(t)
	rig.endBoundary(t)

	// Assert.
	if got := rig.store.turnsRecorded(); len(got) != 1 || got[0] != "t" {
		t.Fatalf("settlements = %v, want the turn's stamp settled once every dependency was on file", got)
	}
}

// A turn still owed a correction settles its TURN and not its STAMP. The two
// outcomes have to be separable, or the separation is nominal.
func TestATerminalResultAwaitingACorrectionSettlesTheTurnButNotTheStamp(t *testing.T) {
	// Arrange — no correction is ever delivered.
	rig := newSettledTurnRig(t)

	// Act.
	rig.result(t)

	// Assert.
	if got := rig.applier.synthesizedTurnCloses(); len(got) != 1 {
		t.Fatalf("synthesized closes = %v, want the turn settled from its result", got)
	}
	if got := rig.store.turnsRecorded(); len(got) != 0 {
		t.Fatalf("settlements = %v, want the stamp outstanding while a response is still uncorrected", got)
	}
}

// THE LATE UPDATE. A correction for a response of an ALREADY-SETTLED turn
// revises the settled record rather than being dropped, which is what the old
// ledger did when it cleared itself at commit.
func TestACorrectionArrivingAfterTheStampSettledRevisesIt(t *testing.T) {
	// Arrange — the turn settles with one figure on file.
	rig := newSettledTurnRig(t)
	rig.correct(t, 720)
	rig.result(t)
	rig.endBoundary(t)

	// Act — the vendor reports a different final figure for the same response.
	rig.correct(t, 4407)

	// Assert.
	recorded := rig.store.turnsRecorded()
	if len(recorded) != 2 {
		t.Fatalf("settlements = %v, want the settled stamp re-persisted with the late correction", recorded)
	}
	if got := lastResponseOutputTokens(t, rig.store, "t"); got != 4407 {
		t.Fatalf("settled response output_tokens = %d, want the vendor's late figure", got)
	}
}

// The late revision is RECORDED. A figure that changes after the fact with no
// account of why is a figure nobody can reconcile against the log.
func TestALateCorrectionRecordsItsRevision(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	rig.correct(t, 720)
	rig.result(t)
	rig.endBoundary(t)

	// Act.
	rig.correct(t, 4407)

	// Assert.
	if !strings.Contains(strings.Join(rig.logs.info, "\n"), "accounting stamp REVISED LATE") {
		t.Fatalf("log = %v, want the late revision recorded", rig.logs.info)
	}
}

// The shim's own `TurnEnded`, arriving after the result already settled the
// turn, is admitted as the replay it is — and does NOT re-settle the stamp,
// which would file a collision report about the ordinary path.
func TestTheShimsLaterTurnEndedIsAdmittedWithoutResettlingTheStamp(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	rig.correct(t, 720)
	rig.result(t)
	rig.endBoundary(t)

	// Act.
	err := rig.consumer.Apply(&corev1.Event{
		Seq: 2, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t",
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}},
	})

	// Assert.
	if err != nil {
		t.Fatalf("Apply error = %v, want the shim's own end admitted over a turn its result already settled", err)
	}
	if got := rig.store.turnsRecorded(); len(got) != 1 {
		t.Fatalf("settlements = %v, want exactly one — the second authority names a turn already stamped", got)
	}
}

// A DEGRADED TURN STILL ENDS. An accounting rejection renders as INVALID
// ACCOUNTING, loudly, and the guard is untouched — what it may never do is keep
// the turn open.
func TestAnInvalidAccountingVerdictDoesNotKeepTheTurnOpen(t *testing.T) {
	// Arrange — no usage boundaries were ever sampled, which resolves invalid.
	rig := newSettledTurnRig(t)
	rig.correct(t, 720)

	// Act.
	rig.result(t)
	rig.endBoundary(t)

	// Assert.
	settled := settlementFor(t, rig.store, "t")
	if settled.GetInvalid() == nil {
		t.Fatalf("verdict = %+v, want the unsampled turn still reported INVALID ACCOUNTING", settled.GetVerdict())
	}
	if got := rig.applier.synthesizedTurnCloses(); len(got) != 1 {
		t.Fatalf("synthesized closes = %v, want the turn settled despite its invalid accounting", got)
	}
}

// ONE TURN, ONE END ANNOUNCEMENT. The vendor's result and the shim's
// `TurnEnded` are two statements of the same end, and the daemon's turn
// machinery — the queue's drain, a keep-alive ping's claim, the idle clock —
// must hear it once. A second edge lands after the NEXT turn has taken the
// record and retires a claim that turn owns.
func TestATurnEndReachesTheQueueExactlyOnce(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	ends := 0
	rig.consumer.onTurn = func(active bool, _ int64) {
		if !active {
			ends++
		}
	}
	rig.correct(t, 720)

	// Act — the result settles the turn, then the shim announces the same end.
	rig.result(t)
	if err := rig.consumer.Apply(&corev1.Event{
		Seq: 2, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t",
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}},
	}); err != nil {
		t.Fatalf("Apply the shim's own end: %v", err)
	}

	// Assert.
	if ends != 1 {
		t.Fatalf("turn-end announcements = %d, want exactly 1 for one turn's end", ends)
	}
}

// The keep-alive clock is stamped from that one announcement too, so an idle
// measurement cannot be restarted by the shim's echo of an end already taken.
func TestTheIdleClockIsStampedOncePerTurnEnd(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	stamps := 0
	rig.consumer.onTurnEnded = func(int64) { stamps++ }
	rig.correct(t, 720)

	// Act.
	rig.result(t)
	if err := rig.consumer.Apply(&corev1.Event{
		Seq: 2, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t",
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}},
	}); err != nil {
		t.Fatalf("Apply the shim's own end: %v", err)
	}

	// Assert.
	if stamps != 1 {
		t.Fatalf("idle-clock stamps = %d, want exactly 1 for one turn's end", stamps)
	}
}

// THE PRESENCE CHECK STILL BITES. A turn the daemon has attributed a terminal
// result to must really have emitted one, and a push that says otherwise is a
// misattributed accounting rather than a cosmetic disagreement.
func TestAnAttributedTerminalWithNoResultItemKillsTheDaemon(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	defer func() {
		if got := recover(); got == nil || !strings.Contains(fmt.Sprint(got), "had no result item") {
			t.Fatalf("recover = %v, want the misattributed terminal fatal", got)
		}
	}()

	// Act — an ordinary assistant emission, attributed as this turn's terminal.
	rig.consumer.pushConversationAttributed(mainAssistantEvent(t, 4, "a-not-a-result", "an ordinary answer"), true, "t")

	// Assert — the deferred recover above.
	t.Fatal("an event attributed as a terminal result but carrying none was published")
}

// THE E2E REGRESSION. A session outliving a shim generation renumbers its
// stream from one, so a later ordinary event reuses the seq an earlier terminal
// result held. The attribution travels with the call rather than with the
// coordinate, so the reused seq inherits nothing.
func TestASeqReusedByALaterGenerationIsNotTreatedAsTerminal(t *testing.T) {
	// Arrange — the turn's result settles at its seq.
	rig := newSettledTurnRig(t)
	rig.correct(t, 720)
	rig.result(t)

	// Act — the next generation's event lands on the same coordinate.
	reused := snapshotAssistantEvent(t, "msg_next_generation", 11)
	reused.Seq = rig.consumer.terminalSeqByTurn["t"]
	rig.consumer.pushConversation(reused, true)

	// Assert — reaching here at all is the assertion; the old seq index panicked.
	if got := len(rig.push.conversationDeltas()); got == 0 {
		t.Fatal("conversation deltas = 0, want the reused-seq event published like any other")
	}
}

// settlementFor reads one turn's settled record out of the store.
func settlementFor(t *testing.T, store *settlingTurnAccountingStore, turnID string) *frontendv1.TurnAccounting {
	t.Helper()
	settlements, err := store.List("s1")
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	for _, settlement := range settlements {
		if settlement.GetTurnId() == turnID {
			return settlement
		}
	}
	t.Fatalf("no settlement for turn %q", turnID)
	return nil
}

// lastResponseOutputTokens reads the settled record's one response figure.
func lastResponseOutputTokens(t *testing.T, store *settlingTurnAccountingStore, turnID string) int64 {
	t.Helper()
	responses := settlementFor(t, store, turnID).GetResponses()
	if len(responses) != 1 {
		t.Fatalf("responses = %d, want the turn's single response", len(responses))
	}
	return responses[0].GetUsage().GetOutputTokens()
}

// ---------------------------------------------------------------------------
// THE COMPOSITION: a displaced turn and a finished one settle differently.
//
// Two orthogonal mechanisms meet on one event. A bounce interrupts the turn in
// flight and records what the successor daemon owes (turnresumption.go); the
// SDK unwinds and emits a `result`. That result is not the turn arriving at its
// own end, and settling from it would record a turn the daemon stopped as one
// that finished.
// ---------------------------------------------------------------------------

// A turn a teardown is stopping is NOT settled from the result its own
// interrupt provoked. The stop's close is what attributes it.
func TestATerminalResultDuringATeardownStopDoesNotSettleTheTurn(t *testing.T) {
	// Arrange — the teardown's pre-interrupt latch, set exactly where the
	// teardown sets it: before the interrupt goes out.
	rig := newSettledTurnRig(t)
	rig.consumer.noteTurnStopInFlight("t")
	rig.correct(t, 720)

	// Act — the interrupt's result arrives.
	rig.result(t)

	// Assert.
	if got := rig.applier.synthesizedTurnCloses(); len(got) != 0 {
		t.Fatalf("synthesized closes = %v, want none — this turn was displaced by a stop, not finished", got)
	}
}

// THE ANSWER IS STILL PUBLISHED. Whatever the turn produced before the bounce
// landed on it is the user's, and withholding it would be the wedge again in a
// different coat.
func TestATerminalResultDuringATeardownStopStillReachesTheConversation(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	rig.consumer.noteTurnStopInFlight("t")
	before := len(rig.push.conversationDeltas())

	// Act.
	rig.result(t)

	// Assert.
	if got := len(rig.push.conversationDeltas()); got != before+1 {
		t.Fatalf("conversation deltas = %d, want %d — a stopped turn's output is still the user's", got, before+1)
	}
}

// The turn-end announcement is withheld too, so the queue's drain and the idle
// clock hear the stop's own boundary rather than one that would tell them the
// turn ended of its own accord.
func TestATerminalResultDuringATeardownStopAnnouncesNoTurnEnd(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	ends := 0
	rig.consumer.onTurn = func(active bool, _ int64) {
		if !active {
			ends++
		}
	}
	rig.consumer.noteTurnStopInFlight("t")

	// Act.
	rig.result(t)

	// Assert.
	if ends != 0 {
		t.Fatalf("turn-end announcements = %d, want none for a turn a teardown is stopping", ends)
	}
}

// A DIFFERENT TURN'S STOP IS NOT THIS TURN'S. The latch is per turn, so a stop
// recorded for an earlier turn cannot keep a later one from settling.
func TestATerminalResultForAnUnstoppedTurnStillSettles(t *testing.T) {
	// Arrange.
	rig := newSettledTurnRig(t)
	rig.consumer.noteTurnStopInFlight("t-some-other-turn")
	rig.correct(t, 720)

	// Act.
	rig.result(t)

	// Assert.
	if got := rig.applier.synthesizedTurnCloses(); len(got) != 1 {
		t.Fatalf("synthesized closes = %v, want the turn settled from its own terminal result", got)
	}
}

// THE RE-DRIVEN TURN IS A REAL TURN. Its prompt is daemon-hidden, but nothing
// about the accounting ledger is: the turn a resumption re-drives settles a
// stamp on exactly the terms any other turn does.
func TestAReDrivenTurnSettlesItsAccountingLikeAnyOther(t *testing.T) {
	// Arrange — a turn whose request id is a re-drive's.
	rig := newReDrivenTurnRig(t)

	// Act.
	rig.result(t)
	rig.endBoundary(t)

	// Assert.
	if got := rig.store.turnsRecorded(); len(got) != 1 || got[0] != reDriveRequestID {
		t.Fatalf("settlements = %v, want the re-driven turn's own stamp settled", got)
	}
}
