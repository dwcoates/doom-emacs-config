package shimclient

import (
	"errors"
	"net"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

func TestReplayContinuationFromLastSeq(t *testing.T) {
	// Arrange: the daemon has durably seen through seq 4; on attach it must
	// Subscribe{from_seq:4} and the shim replays 5..9.
	h := newHarness()
	h.seq.SetLastSeq("sess-1", 4)
	gotFrom := make(chan uint64, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		sub := fakeServerHandshake(t, conn, "sess-1", "1", false)
		gotFrom <- sub.GetFromSeq()
		for seq := uint64(5); seq <= 9; seq++ {
			mustWriteMsg(t, conn, persistentTurnEnd("sess-1", seq))
		}
		_, _ = readMsg(conn)
	})
	_, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act / Assert
	select {
	case from := <-gotFrom:
		if from != 4 {
			t.Fatalf("Subscribe from_seq: got %d want 4", from)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("no Subscribe observed")
	}
	for want := uint64(5); want <= 9; want++ {
		if got := recvEvent(t, h.state.ch).GetSeq(); got != want {
			t.Fatalf("replayed seq: got %d want %d", got, want)
		}
	}
	if last := h.seq.LastSeq("sess-1"); last != 9 {
		t.Fatalf("seq store: got %d want 9", last)
	}
}

func TestSeqRegressionIsDetected(t *testing.T) {
	// Arrange: a client (no live connection needed; exercise the demux logic
	// directly) that has advanced to seq 5.
	h := newHarness()
	c := New(h.config(t, "sess-1", "/unused.sock"))
	if err := c.dispatchEvent(persistentTurnEnd("sess-1", 5)); err != nil {
		t.Fatalf("first event should be accepted: %v", err)
	}
	<-h.state.ch // drain

	// Act: a lower seq is a protocol violation.
	err := c.dispatchEvent(persistentTurnEnd("sess-1", 3))

	// Assert
	if !errors.Is(err, ErrSeqRegression) {
		t.Fatalf("want ErrSeqRegression, got %v", err)
	}
}

func TestEphemeralSeqZeroDoesNotAdvanceHighWater(t *testing.T) {
	// Arrange
	h := newHarness()
	c := New(h.config(t, "sess-1", "/unused.sock"))
	if err := c.dispatchEvent(persistentTurnEnd("sess-1", 7)); err != nil {
		t.Fatalf("persistent event: %v", err)
	}
	<-h.state.ch

	// Act: an ephemeral ContentDelta (seq 0) must not regress or advance.
	ephemeral := &corev1.Event{
		SessionId: "sess-1",
		Class:     corev1.EventClass_EVENT_CLASS_EPHEMERAL,
		Payload:   &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "u1"}},
	}
	err := c.dispatchEvent(ephemeral)

	// Assert
	if err != nil {
		t.Fatalf("ephemeral event should not error: %v", err)
	}
	<-h.frame.ch // routed to frame sink
	if last := h.seq.LastSeq("sess-1"); last != 7 {
		t.Fatalf("ephemeral must not touch high-water: got %d want 7", last)
	}
}

func TestEventRouting(t *testing.T) {
	vendorAny, err := anypb.New(&corev1.TurnEnded{StopReason: "vendor-wrapped"})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	tests := []struct {
		name string
		ev   *corev1.Event
		want string // "state" | "frame" | "degraded"
	}{
		{
			name: "session started to state sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{}}},
			want: "state",
		},
		{
			name: "turn started to state sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}}},
			want: "state",
		},
		{
			name: "task started to state sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "a1"}}},
			want: "state",
		},
		{
			name: "content delta to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "u"}}},
			want: "frame",
		},
		{
			name: "message latency to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_MessageLatency{MessageLatency: &corev1.MessageLatency{Uuid: "m", TtftMs: 865}}},
			want: "frame",
		},
		{
			name: "heartbeat progress to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_HeartbeatProgress{HeartbeatProgress: &corev1.HeartbeatProgress{ToolUseId: "t"}}},
			want: "frame",
		},
		{
			name: "vendor payload to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_Vendor{Vendor: vendorAny}},
			want: "frame",
		},
		{
			name: "unparsed to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_Unparsed{Unparsed: &corev1.UnparsedEvent{Producer: "claude-shim"}}},
			want: "frame",
		},
		{
			name: "degraded state to degraded reporter",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{Component: "store-client"}}},
			want: "degraded",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness()
			c := New(h.config(t, "s", "/unused.sock"))

			// Act
			if err := c.dispatchEvent(tt.ev); err != nil {
				t.Fatalf("dispatchEvent: %v", err)
			}

			// Assert
			switch tt.want {
			case "state":
				assertRecv(t, h.state.ch)
			case "frame":
				assertRecv(t, h.frame.ch)
			case "degraded":
				select {
				case <-h.deg.ds:
				case <-time.After(time.Second):
					t.Fatal("degraded reporter never called")
				}
			}
		})
	}
}

func assertRecv(t *testing.T, ch chan *corev1.Event) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(time.Second):
		t.Fatal("expected sink never received the event")
	}
}
