package server

import (
	"bytes"
	"io"
	"testing"
	"time"

	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/wire"
	_ "modernc.org/sqlite"
)

// seedOwnedRecord writes one record BELONGING to the named message through the
// store's own socket.
//
// It no longer reaches into the database to stamp ownership: the column is read
// off `ExternalEntry.message` at write time, so an ordinary write through the
// production path is enough.
func seedOwnedRecord(t *testing.T, h *harness, session, owner string) {
	t.Helper()
	conn := h.dial(t)
	send(t, conn, storeWrite(userSaid(session, owner)))
	awaitWrite(t, conn)
	conn.Close()
}

func TestMessagePageRequestIsServedOverTheSocket(t *testing.T) {
	// Arrange
	h := start(t, 8, testLogger())
	seedOwnedRecord(t, h, "s1", "m1")

	// Act: one request frame, routed by its Any type-URL alone.
	conn := h.dial(t)
	send(t, conn, &protocolv1.MessagePageRequest{
		RequestId: "r1",
		SessionId: "s1",
		Anchor:    &protocolv1.MessagePageRequest_Head{Head: &protocolv1.MessagePageHead{}},
	})
	conn.SetReadDeadline(time.Now().Add(5 * time.Second))
	msg := recv(t, conn)

	// Assert
	page, ok := msg.(*protocolv1.MessagePage)
	if !ok {
		t.Fatalf("reply is %T, want *protocolv1.MessagePage", msg)
	}
	if page.GetRequestId() != "r1" || page.GetMessage_1().GetMessageId() != "m1" {
		t.Fatalf("page=%v, want request_id=r1 carrying message m1", page)
	}
}

func TestMessagePageCarriesTheExternalHalfOfEachRecord(t *testing.T) {
	// Arrange: a page is read BY a consumer, and the internal half exists
	// precisely so a consumer cannot read it.
	h := start(t, 8, testLogger())
	seedOwnedRecord(t, h, "s1", "m1")

	// Act
	conn := h.dial(t)
	send(t, conn, &protocolv1.MessagePageRequest{
		RequestId: "r1",
		SessionId: "s1",
		Anchor:    &protocolv1.MessagePageRequest_Head{Head: &protocolv1.MessagePageHead{}},
	})
	conn.SetReadDeadline(time.Now().Add(5 * time.Second))
	page := recv(t, conn).(*protocolv1.MessagePage)

	// Assert
	records := page.GetMessage_1().GetRecords()
	if len(records) != 1 {
		t.Fatalf("message carried %d records, want 1", len(records))
	}
	if records[0].GetMessage().GetMessageId() != "m1" || records[0].GetSessionId() != "s1" {
		t.Fatalf("record = %+v, want the external half of m1 in s1", records[0])
	}
}

func TestMessagePageRequestWithNoSessionIsRefusedWithoutAPage(t *testing.T) {
	// Arrange: a page missing its messages is indistinguishable from the top of
	// the conversation, so a request the store cannot route is answered with
	// no page at all rather than an empty one.
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}))

	// Act
	conn := h.dial(t)
	send(t, conn, &protocolv1.MessagePageRequest{
		RequestId: "r1",
		Anchor:    &protocolv1.MessagePageRequest_Head{Head: &protocolv1.MessagePageHead{}},
	})
	conn.SetReadDeadline(time.Now().Add(time.Second))
	_, err := wire.ReadAny(conn)

	// Assert
	if err == nil {
		t.Fatal("a sessionless page request unexpectedly received a page")
	}
	if err := h.srv.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	<-h.done
	if _, found := findLoggedRecord(t, logs.Bytes(), "message-page", "error"); !found {
		t.Fatalf("page refusal log missing: %s", logs.String())
	}
}

func TestSubscribeStillReplaysAfterTheMessagePageDoorOpens(t *testing.T) {
	// Arrange: the bounded page is ADDITIVE. The old read path must keep
	// working until a later branch closes it deliberately.
	h := start(t, 8, testLogger())
	seedOwnedRecord(t, h, "s1", "m1")

	// Act
	sub := h.dial(t)
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 0})
	sub.SetReadDeadline(time.Now().Add(5 * time.Second))
	delivery := recvDelivery(t, sub)
	recvSubscriptionReady(t, sub)

	// Assert
	if delivery.GetStored().GetSeq() != 1 {
		t.Fatalf("replayed seq=%d, want 1", delivery.GetStored().GetSeq())
	}
}
