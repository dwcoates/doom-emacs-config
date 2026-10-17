package db

import (
	"errors"
	"path/filepath"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// deliveredLine is the page line of an inter-session peer message in `book`
// stating `delivery`, keyed on `uuid` as both planes key it.
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

func TestTheDeliveryIndexHoldsADeliveredPeerMessage(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))

	// Assert
	if got := scalar[string](t, d, `SELECT delivery_id FROM peer_delivery WHERE position = ?`, positionOf(t, result)); got != "m-1" {
		t.Fatalf("peer_delivery id = %q, want m-1", got)
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

func TestPeerRecipientAnswersTheSessionHoldingTheDelivery(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))

	// Act
	session, found, err := d.PeerRecipient(ctx(), "m-1")

	// Assert
	if err != nil || !found || session != "session-b" {
		t.Fatalf("PeerRecipient = (%q, %t, %v), want (session-b, true, nil)", session, found, err)
	}
}

func TestPeerRecipientAnswersTheRootOfASubagentBook(t *testing.T) {
	// Arrange: the delivery is filed in a subagent's book, spawned by the main agent.
	d, _ := newStore(t)
	writeOK(t, d, deliveredLine("u1", "toolu_child", "m-1"))
	if _, err := d.sql.ExecContext(ctx(), `UPDATE agent SET spawned_by_agent = 'session-b' WHERE agent_id = 'toolu_child'`); err != nil {
		t.Fatalf("stamping the lineage: %v", err)
	}

	// Act
	session, found, err := d.PeerRecipient(ctx(), "m-1")

	// Assert
	if err != nil || !found || session != "session-b" {
		t.Fatalf("PeerRecipient = (%q, %t, %v), want the lineage root (session-b, true, nil)", session, found, err)
	}
}

func TestPeerRecipientAnswersNotFoundForADeliveryNoSessionHolds(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))

	// Act
	session, found, err := d.PeerRecipient(ctx(), "m-2")

	// Assert
	if err != nil || found || session != "" {
		t.Fatalf("PeerRecipient = (%q, %t, %v), want not found", session, found, err)
	}
}

func TestPeerRecipientRefusesADeliveryHeldByTwoSessions(t *testing.T) {
	// Arrange: one delivery filed in two sessions breaks the index's invariant.
	d, _ := newStore(t)
	writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))
	writeOK(t, d, deliveredLine("u2", "session-c", "m-1"))

	// Act
	_, found, err := d.PeerRecipient(ctx(), "m-1")

	// Assert
	if found || !errors.Is(err, ErrStorage) {
		t.Fatalf("PeerRecipient = (found %t, %v), want a storage failure", found, err)
	}
}

func TestPeerRecipientRecordsTheAmbiguousDeliveryAtError(t *testing.T) {
	// Arrange
	d, logs := newStore(t)
	writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))
	writeOK(t, d, deliveredLine("u2", "session-c", "m-1"))

	// Act
	_, _, _ = d.PeerRecipient(ctx(), "m-1")

	// Assert
	logs.assertLogged(t, "error", "delivery m-1 is held by 2 sessions (session-b, session-c)")
}

func TestPeerRecipientRefusesAnEmptyDelivery(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, _, err := d.PeerRecipient(ctx(), "")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("PeerRecipient(\"\") = %v, want an invalid request", err)
	}
}

// prePeerDeliveryDatabase writes a database carrying one delivered peer
// message but NOT peer_delivery — the shape a store had before the index — and
// returns its path, closed.
func prePeerDeliveryDatabase(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	d, err := OpenWithOptions(path, log, Options{unsynced: true, Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	writeOK(t, d, deliveredLine("u1", "session-b", "m-1"))
	if _, err := d.sql.Exec(`DROP TABLE peer_delivery`); err != nil {
		t.Fatalf("dropping peer_delivery: %v", err)
	}
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	return path
}

func TestOpenIndexesEveryStoredDeliveryWhenItBuildsTheDeliveryIndex(t *testing.T) {
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
	if got := scalar[string](t, d, `SELECT book_agent_id || '/' || delivery_id FROM peer_delivery`); got != "session-b/m-1" {
		t.Fatalf("peer_delivery = %q, want the stored delivery indexed", got)
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
