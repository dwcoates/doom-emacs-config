package shimclient

import (
	"errors"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// fakeOpenClaims answers the durable open-claim question.
type fakeOpenClaims struct {
	ids []string
	err error
}

func (f *fakeOpenClaims) ActiveTurnIDs(string, string) ([]string, error) { return f.ids, f.err }

// pinClient is a Client wired only for the pin bookkeeping under test.
func pinClient(t *testing.T, claims OpenTurnClaims) *Client {
	t.Helper()
	return &Client{cfg: Config{SessionID: "s1", Workspace: "/ws", OpenTurnClaims: claims},
		logf: func(string, ...any) {}}
}

// THE UNPINNED-END REGRESSION. The pin set keeps a turn's start and end atomic:
// the durable cursor may only advance while nothing is pinned. Discarding the
// set on reconnect declared "nothing is in flight", which let the cursor move
// past a start whose end had not arrived — the end then replayed alone and was
// rejected as naming an unpinned accounting turn.
func TestPinsAreRebuiltFromTheDurableLedger(t *testing.T) {
	// Arrange — the ledger still holds one open claim.
	c := pinClient(t, &fakeOpenClaims{ids: []string{"turn-open"}})

	// Act.
	pinned := c.reconstructPinnedTurns()

	// Assert.
	if _, ok := pinned["turn-open"]; !ok {
		t.Fatalf("pins = %v, want the open claim rebuilt so the cursor stays held behind it", pinned)
	}
}

// A ledger read failure yields an EMPTY set, never a guess: inventing pins from
// a failed read would hold the cursor behind turns that may not exist.
func TestAFailedLedgerReadRebuildsNoPins(t *testing.T) {
	// Arrange.
	c := pinClient(t, &fakeOpenClaims{err: errors.New("db gone")})

	// Act / Assert.
	if got := c.reconstructPinnedTurns(); len(got) != 0 {
		t.Fatalf("pins = %v, want none after a failed read", got)
	}
}

// With no durable authority wired the behavior is the pre-existing one, so the
// change cannot alter a deployment that never supplied it.
func TestNoDurableAuthorityRebuildsNoPins(t *testing.T) {
	// Arrange.
	c := pinClient(t, nil)

	// Act / Assert.
	if got := c.reconstructPinnedTurns(); len(got) != 0 {
		t.Fatalf("pins = %v, want none without an OpenTurnClaims", got)
	}
}

// THE FROZEN-CURSOR REGRESSION, the mirror of the first. Only a stream
// TurnEnded released a pin, so a turn the daemon closed itself kept its pin
// forever and the cursor could never advance past it again.
func TestASynthesizedCloseReleasesItsPin(t *testing.T) {
	// Arrange.
	c := pinClient(t, nil)
	c.pinnedAccountingTurns = map[string]struct{}{"turn-a": {}, "turn-b": {}}

	// Act.
	c.UnpinAccountingTurn("turn-a")

	// Assert.
	if _, ok := c.pinnedAccountingTurns["turn-a"]; ok {
		t.Fatal("the synthesized close left its pin standing; the cursor would freeze here forever")
	}
	if _, ok := c.pinnedAccountingTurns["turn-b"]; !ok {
		t.Fatal("an unrelated turn's pin was released; only the closed turn frees its hold")
	}
}

// A close naming a turn this client never pinned is a no-op, not a fault.
func TestUnpinningAnUnknownTurnIsANoOp(t *testing.T) {
	// Arrange.
	c := pinClient(t, nil)
	c.pinnedAccountingTurns = map[string]struct{}{"turn-a": {}}

	// Act.
	c.UnpinAccountingTurn("never-pinned")

	// Assert.
	if len(c.pinnedAccountingTurns) != 1 {
		t.Fatalf("pins = %v, want the known pin untouched", c.pinnedAccountingTurns)
	}
}

// --- turn ends from a retired query vs genuine inconsistency ------------------

// endEvent is a TurnEnded at SEQ for TURNID, produced by ENVELOPEQUERY.
func endEvent(seq uint64, turnID, envelopeQuery string) *corev1.Event {
	return &corev1.Event{
		SessionId: "vendor", Seq: seq, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId:       turnID,
		QueryInstanceId: envelopeQuery,
		Payload:         &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: turnID}},
	}
}

// (b) THE xfq REGRESSION. A turn ended by a query that has since been retired
// replays its end. There is no start still owed and nothing to keep atomic, but
// the end was rejected as a protocol violation -- terminal, and the reason the
// workspace could not open. It is the same mistake the accounting reducer made
// when it judged retired rows by live identity.
func TestAnEndFromARetiredQueryIsNotAViolation(t *testing.T) {
	// Arrange -- bound to the live query; the end names a different one.
	c := pinClient(t, &fakeOpenClaims{})
	c.liveQueryInstanceID = "live-query"
	c.pinnedAccountingTurns = map[string]struct{}{}

	// Act.
	err := c.validateDurableCursorTransition(endEvent(9, "long-finished-turn", "retired-query"))

	// Assert.
	if err != nil {
		t.Fatalf("end from a retired query = %v, want it accepted as history", err)
	}
}

// (b) And it mutates nothing: history is admitted, never recorded.
func TestAnEndFromARetiredQueryMutatesNoPins(t *testing.T) {
	// Arrange.
	c := pinClient(t, &fakeOpenClaims{})
	c.liveQueryInstanceID = "live-query"
	c.pinnedAccountingTurns = map[string]struct{}{"turn-live": {}}

	// Act.
	if err := c.validateDurableCursorTransition(endEvent(9, "long-finished-turn", "retired-query")); err != nil {
		t.Fatalf("end from a retired query = %v", err)
	}

	// Assert.
	if len(c.pinnedAccountingTurns) != 1 {
		t.Fatalf("pins = %v, want the live pin untouched by a historical end", c.pinnedAccountingTurns)
	}
}

// (a) THE GENUINE INCONSISTENCY STAYS FATAL, COMPLETELY UNCHANGED: an unpinned
// end produced by the LIVE query means the pin was lost underneath us.
func TestAnUnpinnedEndFromTheLiveQueryStaysFatal(t *testing.T) {
	// Arrange.
	c := pinClient(t, &fakeOpenClaims{})
	c.liveQueryInstanceID = "live-query"
	c.pinnedAccountingTurns = map[string]struct{}{}

	// Act.
	err := c.validateDurableCursorTransition(endEvent(9, "turn-live", "live-query"))

	// Assert.
	if err == nil {
		t.Fatal("an unpinned end from the live query was accepted; a lost pin is real corruption")
	}
}

// (c) EMPTY FAILS CLOSED: an end from a producer that predates the field keeps
// precisely the strict behavior it had before provenance existed.
func TestAnUnstampedUnpinnedEndStaysFatal(t *testing.T) {
	// Arrange.
	c := pinClient(t, &fakeOpenClaims{})
	c.liveQueryInstanceID = "live-query"
	c.pinnedAccountingTurns = map[string]struct{}{}

	// Act.
	err := c.validateDurableCursorTransition(endEvent(9, "unknown-turn", ""))

	// Assert.
	if err == nil {
		t.Fatal("an unstamped unpinned end was waved through; empty must be treated as live")
	}
}

// (d) THE STARTUP CASE. A record the session itself wrote before its
// subscription could exist is served back during catch-up, but it names the
// query running right now, so it is LIVE on one comparison -- and therefore
// still gets the full, fatal check rather than a history exemption.
func TestAnEndDeliveredDuringCatchUpFromTheLiveQueryIsStillChecked(t *testing.T) {
	// Arrange -- seq 1, beneath everything, exactly how catch-up delivers it.
	c := pinClient(t, &fakeOpenClaims{})
	c.liveQueryInstanceID = "live-query"
	c.pinnedAccountingTurns = map[string]struct{}{}

	// Act.
	err := c.validateDurableCursorTransition(endEvent(1, "turn-live", "live-query"))

	// Assert -- delivery order bought it no exemption.
	if err == nil {
		t.Fatal("an end catch-up-delivered from the LIVE query was treated as history")
	}
}

// An UNBOUND client has nothing to compare against, so the check stays strict.
func TestWithoutABoundQueryTheCheckStaysStrict(t *testing.T) {
	// Arrange -- no hello query bound.
	c := pinClient(t, nil)
	c.pinnedAccountingTurns = map[string]struct{}{}

	// Act.
	err := c.validateDurableCursorTransition(endEvent(9, "unknown-turn", "retired-query"))

	// Assert.
	if err == nil {
		t.Fatal("the strict check was dropped for a client with no bound query")
	}
}
