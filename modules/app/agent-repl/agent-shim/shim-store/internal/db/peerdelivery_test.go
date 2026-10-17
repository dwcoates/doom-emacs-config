package db

import (
	"errors"
	"path/filepath"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// deliveredLine is the RECIPIENT's end of a cross-session message: the page
// line of an inter-session peer message in `book` stating `delivery`, keyed on
// `uuid` as both planes key it.
func deliveredLine(uuid, book, delivery string) *storev1.StoreEntry {
	item := &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_PeerMessage{PeerMessage: &conversationv1.PeerMessage{
		Agent: &conversationv1.AgentId{Value: book},
		Body:  "hi",
		Id:    uuid,
		Kind: &conversationv1.PeerMessage_InterSession{InterSession: &conversationv1.PeerMessageInterSession{
			DeliveryId: &conversationv1.PeerDeliveryId{Value: delivery},
		}},
	}}}
	return pageEntry("w-"+uuid, "peer:"+uuid, book, item)
}

// sentLine is the SENDER's end of a cross-session message: the settled
// SendMessage unit `call` in `book`, reaching another session under `delivery`.
func sentLine(call, book, delivery string) *storev1.StoreEntry {
	send := &conversationv1.AgentSendMessage{Result: &conversationv1.AgentSendMessage_Success{Success: &conversationv1.AgentSendMessageSuccess{
		AddressedTo: "fix-flaky-3a",
		Recipient: &conversationv1.AgentSendMessageSuccess_PeerSession{PeerSession: &conversationv1.AgentSendMessagePeerSession{
			DeliveryId: &conversationv1.PeerDeliveryId{Value: delivery},
		}},
	}}}
	return pageEntry("w-"+call, "act:"+call, book, frameItem(activityFrame(book, call, send)))
}

func TestTheDeliveryIndexHoldsTheRecipientsEnd(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))

	// Assert
	if got := scalar[string](t, d, `SELECT end_kind || '/' || delivery_id FROM peer_delivery WHERE position = ?`, positionOf(t, result)); got != "recipient/m-1" {
		t.Fatalf("peer_delivery = %q, want recipient/m-1", got)
	}
}

func TestTheDeliveryIndexHoldsTheSendersEnd(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, sentLine("toolu_send", "session-a", "m-1"))

	// Assert
	if got := scalar[string](t, d, `SELECT end_kind || '/' || delivery_id FROM peer_delivery WHERE position = ?`, positionOf(t, result)); got != "sender/m-1" {
		t.Fatalf("peer_delivery = %q, want sender/m-1", got)
	}
}

func TestTheDeliveryIndexHoldsNoPeerMessageThatStatesNoDelivery(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, deliveredLine("u1", "session-b", ""))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM peer_delivery`); got != 0 {
		t.Fatalf("peer_delivery rows = %d, want none for a message stating no delivery", got)
	}
}

func TestPeerDeliveryAnswersBothEnds(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, sentLine("toolu_send", "session-a", "m-1"), deliveredLine("u1", "session-b", "m-1"))

	// Act
	sender, recipient, err := d.PeerDelivery(ctx(), "m-1")

	// Assert
	if err != nil || sender != "session-a" || recipient != "session-b" {
		t.Fatalf("PeerDelivery = (%q, %q, %v), want (session-a, session-b, nil)", sender, recipient, err)
	}
}

func TestPeerDeliveryAnswersAnEndNoSessionHoldsAsEmpty(t *testing.T) {
	// Arrange: only the sender's end is recorded, as for a send to an outside session.
	d, _ := newStore(t)
	writeOK(t, d, sentLine("toolu_send", "session-a", "m-1"))

	// Act
	sender, recipient, err := d.PeerDelivery(ctx(), "m-1")

	// Assert
	if err != nil || sender != "session-a" || recipient != "" {
		t.Fatalf("PeerDelivery = (%q, %q, %v), want (session-a, \"\", nil)", sender, recipient, err)
	}
}

func TestPeerDeliveryAnswersTheRootOfASubagentBook(t *testing.T) {
	// Arrange: the send was made by a subagent, spawned by the main agent.
	d, _ := newStore(t)
	writeOK(t, d, sentLine("toolu_send", "toolu_child", "m-1"))
	if _, err := d.sql.ExecContext(ctx(), `UPDATE agent SET spawned_by_agent = 'session-a' WHERE agent_id = 'toolu_child'`); err != nil {
		t.Fatalf("stamping the lineage: %v", err)
	}

	// Act
	sender, _, err := d.PeerDelivery(ctx(), "m-1")

	// Assert
	if err != nil || sender != "session-a" {
		t.Fatalf("PeerDelivery sender = (%q, %v), want the lineage root session-a", sender, err)
	}
}

func TestPeerDeliveryRefusesAnEndHeldByTwoSessions(t *testing.T) {
	// Arrange: one delivery filed as received by two sessions breaks the index's invariant.
	d, _ := newStore(t)
	writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))
	writeOK(t, d, deliveredLine("u2", "session-c", "m-1"))

	// Act
	_, _, err := d.PeerDelivery(ctx(), "m-1")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("PeerDelivery = %v, want a storage failure", err)
	}
}

func TestPeerDeliveryRecordsTheAmbiguousEndAtError(t *testing.T) {
	// Arrange
	d, logs := newStore(t)
	writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))
	writeOK(t, d, deliveredLine("u2", "session-c", "m-1"))

	// Act
	_, _, _ = d.PeerDelivery(ctx(), "m-1")

	// Assert
	logs.assertLogged(t, "error", "the recipient end of delivery m-1 is held by 2 sessions (session-b, session-c)")
}

func TestPeerDeliveryRefusesAnEmptyDelivery(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, _, err := d.PeerDelivery(ctx(), "")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("PeerDelivery(\"\") = %v, want an invalid request", err)
	}
}

// prePeerDeliveryDatabase writes a database carrying both ends of one delivery
// but NOT peer_delivery — the shape a store had before the index — and returns
// its path, closed.
func prePeerDeliveryDatabase(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	d, err := OpenWithOptions(path, log, Options{unsynced: true, Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	writeOK(t, d, sentLine("toolu_send", "session-a", "m-1"), deliveredLine("u1", "session-b", "m-1"))
	if _, err := d.sql.Exec(`DROP TABLE peer_delivery`); err != nil {
		t.Fatalf("dropping peer_delivery: %v", err)
	}
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	return path
}

func TestOpenIndexesBothStoredEndsWhenItBuildsTheDeliveryIndex(t *testing.T) {
	// Arrange
	path := prePeerDeliveryDatabase(t)
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{unsynced: true})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert
	if got := scalar[string](t, d, `SELECT group_concat(end_kind || '/' || book_agent_id, ',') FROM (SELECT * FROM peer_delivery ORDER BY end_kind)`); got != "recipient/session-b,sender/session-a" {
		t.Fatalf("peer_delivery = %q, want both stored ends indexed", got)
	}
}

func TestThePeerDeliveryStatementSeeksTheDeliveryIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, peerDeliveryBooksSQL, "m-1", kindPageLine)

	// Assert
	assertNoTableScan(t, "the books holding a delivery", plan)
}
