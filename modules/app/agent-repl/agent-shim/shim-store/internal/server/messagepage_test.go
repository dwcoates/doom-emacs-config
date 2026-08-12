package server

import (
	"bytes"
	"database/sql"
	"io"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/wire"
	_ "modernc.org/sqlite"
)

// seedOwnedRecord writes one PERSISTENT record through the store and stamps its
// owning message on the stored row. Writing ownership at record-write time has
// its own owner; this suite needs only that the served page reflects the
// column, so it seeds the column directly.
func seedOwnedRecord(t *testing.T, h *harness, session, owner string) {
	t.Helper()
	conn := h.dial(t)
	send(t, conn, write(vAssistantStream(t, session, owner)))
	recvAck(t, conn)
	conn.Close()

	raw, err := sql.Open("sqlite", "file:"+h.dbPath)
	if err != nil {
		t.Fatalf("raw open: %v", err)
	}
	defer raw.Close()
	if _, err := raw.Exec(
		`UPDATE event SET top_level_message_id = ? WHERE session_id = ? AND uuid = ?`,
		owner, session, owner); err != nil {
		t.Fatalf("stamping ownership: %v", err)
	}
}

func TestMessagePageRequestIsServedOverTheSocket(t *testing.T) {
	// Arrange
	h := start(t, 8, testLogger())
	seedOwnedRecord(t, h, "s1", "m1")

	// Act: one request frame, routed by its Any type-URL alone.
	conn := h.dial(t)
	send(t, conn, &corev1.MessagePageRequest{
		RequestId: "r1",
		SessionId: "s1",
		Anchor:    &corev1.MessagePageRequest_Head{Head: &corev1.MessagePageHead{}},
	})
	conn.SetReadDeadline(time.Now().Add(5 * time.Second))
	msg := recv(t, conn)

	// Assert
	page, ok := msg.(*corev1.MessagePage)
	if !ok {
		t.Fatalf("reply is %T, want *corev1.MessagePage", msg)
	}
	if page.GetRequestId() != "r1" || page.GetMessage_1().GetMessageId() != "m1" {
		t.Fatalf("page=%v, want request_id=r1 carrying message m1", page)
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
	send(t, conn, &corev1.MessagePageRequest{
		RequestId: "r1",
		Anchor:    &corev1.MessagePageRequest_Head{Head: &corev1.MessagePageHead{}},
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
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})
	sub.SetReadDeadline(time.Now().Add(5 * time.Second))
	ev := recvEvent(t, sub)
	recvSubscriptionReady(t, sub)

	// Assert
	if ev.GetSeq() != 1 {
		t.Fatalf("replayed seq=%d, want 1", ev.GetSeq())
	}
}
