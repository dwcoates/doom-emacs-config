package shimclient

import (
	"context"
	"errors"
	"net"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// replayRig stands a client up against a fake shim that hands every inbound
// ReplayRequest to `serve`, so a test scripts the shim's half of the exchange.
type replayRig struct {
	client *Client
	h      *harness
	stop   func()
	// requests carries each ReplayRequest the fake shim received.
	requests chan *corev1.ReplayRequest
}

func newReplayRig(t *testing.T, serve func(conn net.Conn, req *corev1.ReplayRequest)) *replayRig {
	t.Helper()
	h := newHarness()
	requests := make(chan *corev1.ReplayRequest, 8)
	path := startFakeShim(t, func(conn net.Conn) {
		fakeServerHandshake(t, conn, "s1", "1", false)
		for {
			m, err := wire.ReadAny(conn)
			if err != nil {
				return
			}
			req, ok := m.(*corev1.ReplayRequest)
			if !ok {
				continue // heartbeats and control traffic: not this test's business
			}
			requests <- req
			if serve != nil {
				serve(conn, req)
			}
		}
	})
	c := New(h.config(t, "s1", path))
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() { defer close(done); _ = c.Run(ctx) }()
	if err := c.AwaitReady(ctx); err != nil {
		cancel()
		t.Fatalf("AwaitReady: %v", err)
	}
	stop := func() {
		cancel()
		<-done
	}
	t.Cleanup(stop)
	return &replayRig{client: c, h: h, stop: stop, requests: requests}
}

// replayEvent wraps a store event as the shim would send it back.
func replayEvent(requestID string, seq uint64) *corev1.ReplayEvent {
	return &corev1.ReplayEvent{
		RequestId: requestID,
		Event: &corev1.Event{
			SessionId: "vendor-uuid",
			Seq:       seq,
			Payload:   &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "historical"}},
		},
	}
}

func TestReplaySendsAReplayRequestNotASubscribe(t *testing.T) {
	// Arrange — a Subscribe would MOVE the standing subscription; the whole
	// design turns on this being a different message.
	rig := newReplayRig(t, func(conn net.Conn, req *corev1.ReplayRequest) {
		mustWriteMsg(t, conn, &corev1.ReplayDone{RequestId: req.GetRequestId()})
	})
	// Act
	if _, err := rig.client.Replay(context.Background(), 0, 10, 100, func(*corev1.Event) {}); err != nil {
		t.Fatalf("Replay: %v", err)
	}
	// Assert
	select {
	case req := <-rig.requests:
		if req.GetFromSeq() != 0 || req.GetToSeq() != 10 || req.GetMaxEvents() != 100 {
			t.Fatalf("ReplayRequest = %+v, want from=0 to=10 max=100", req)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("the shim never received a ReplayRequest")
	}
}

func TestReplayStreamsEventsToTheCallersSink(t *testing.T) {
	// Arrange
	rig := newReplayRig(t, func(conn net.Conn, req *corev1.ReplayRequest) {
		mustWriteMsg(t, conn, replayEvent(req.GetRequestId(), 1))
		mustWriteMsg(t, conn, replayEvent(req.GetRequestId(), 2))
		mustWriteMsg(t, conn, &corev1.ReplayDone{RequestId: req.GetRequestId(), Delivered: 2})
	})
	var got []uint64
	// Act
	res, err := rig.client.Replay(context.Background(), 0, 10, 0, func(ev *corev1.Event) {
		got = append(got, ev.GetSeq())
	})
	// Assert
	if err != nil {
		t.Fatalf("Replay: %v", err)
	}
	if len(got) != 2 || got[0] != 1 || got[1] != 2 || res.Delivered != 2 {
		t.Fatalf("streamed %v (delivered=%d), want [1 2]", got, res.Delivered)
	}
}

func TestReplayedEventsNeverReachTheStateSink(t *testing.T) {
	// Arrange — THE structural guarantee. The replayed event carries a
	// TaskStarted payload, which on the LIVE path routes to the SSM. Arriving
	// as a ReplayEvent, the read loop's type switch has nowhere to put it but
	// the replay registry.
	rig := newReplayRig(t, func(conn net.Conn, req *corev1.ReplayRequest) {
		mustWriteMsg(t, conn, replayEvent(req.GetRequestId(), 1))
		mustWriteMsg(t, conn, &corev1.ReplayDone{RequestId: req.GetRequestId(), Delivered: 1})
	})
	// Act
	if _, err := rig.client.Replay(context.Background(), 0, 10, 0, func(*corev1.Event) {}); err != nil {
		t.Fatalf("Replay: %v", err)
	}
	// Assert
	select {
	case ev := <-rig.h.state.ch:
		t.Fatalf("a replayed event reached the SSM sink: seq=%d", ev.GetSeq())
	default:
	}
}

func TestReplayedEventsNeverReachTheFrameSink(t *testing.T) {
	// Arrange — the frame sink is what feeds conversation, progress, and the
	// retained ring on the live path. Replayed history routes to the caller's
	// own sink instead, so the driver can render it WITHOUT the other planes.
	rig := newReplayRig(t, func(conn net.Conn, req *corev1.ReplayRequest) {
		mustWriteMsg(t, conn, replayEvent(req.GetRequestId(), 1))
		mustWriteMsg(t, conn, &corev1.ReplayDone{RequestId: req.GetRequestId(), Delivered: 1})
	})
	// Act
	if _, err := rig.client.Replay(context.Background(), 0, 10, 0, func(*corev1.Event) {}); err != nil {
		t.Fatalf("Replay: %v", err)
	}
	// Assert
	select {
	case ev := <-rig.h.frame.ch:
		t.Fatalf("a replayed event reached the frame sink: seq=%d", ev.GetSeq())
	default:
	}
}

func TestReplayedEventsNeverAdvanceLastSeenSeq(t *testing.T) {
	// Arrange — the high-water mark is the daemon's LIVE consumption position.
	// Advancing it from replayed history would skip live events on the next
	// reattach.
	rig := newReplayRig(t, func(conn net.Conn, req *corev1.ReplayRequest) {
		mustWriteMsg(t, conn, replayEvent(req.GetRequestId(), 9999))
		mustWriteMsg(t, conn, &corev1.ReplayDone{RequestId: req.GetRequestId(), Delivered: 1})
	})
	// Act
	if _, err := rig.client.Replay(context.Background(), 0, 10, 0, func(*corev1.Event) {}); err != nil {
		t.Fatalf("Replay: %v", err)
	}
	// Assert
	if got := rig.h.seq.LastSeq("s1"); got != 0 {
		t.Fatalf("last_seen_seq = %d after a replay, want 0", got)
	}
}

func TestReplayReportsTruncationFromTheShim(t *testing.T) {
	// Arrange
	rig := newReplayRig(t, func(conn net.Conn, req *corev1.ReplayRequest) {
		mustWriteMsg(t, conn, &corev1.ReplayDone{
			RequestId: req.GetRequestId(), Truncated: true, Reason: "hit the cap", Delivered: 5,
		})
	})
	// Act
	res, err := rig.client.Replay(context.Background(), 0, 10, 0, func(*corev1.Event) {})
	// Assert
	if err != nil {
		t.Fatalf("Replay: %v", err)
	}
	if !res.Truncated || res.Reason != "hit the cap" {
		t.Fatalf("result = %+v, want truncated with the shim's reason", res)
	}
}

func TestReplayFailsWithNoLiveShimConnection(t *testing.T) {
	// Arrange — the shim IS the session's transport, and there is deliberately
	// no second route to its history.
	c := New(Config{SessionID: "s1", Logf: shimclientTestLogf(t)})
	// Act
	_, err := c.Replay(context.Background(), 0, 10, 0, func(*corev1.Event) {})
	// Assert
	if !errors.Is(err, ErrReplayNotConnected) {
		t.Fatalf("err = %v, want ErrReplayNotConnected", err)
	}
}

func TestReplayRejectsANilSink(t *testing.T) {
	// Arrange
	c := New(Config{SessionID: "s1", Logf: shimclientTestLogf(t)})
	// Act
	_, err := c.Replay(context.Background(), 0, 10, 0, nil)
	// Assert
	if err == nil {
		t.Fatal("Replay with no sink must error")
	}
}

func TestReplayHonorsTheCallersDeadline(t *testing.T) {
	// Arrange — a shim that accepts the request and never completes it.
	rig := newReplayRig(t, nil)
	ctx, cancel := context.WithTimeout(context.Background(), 150*time.Millisecond)
	defer cancel()
	// Act
	res, err := rig.client.Replay(ctx, 0, 10, 0, func(*corev1.Event) {})
	// Assert
	if err == nil {
		t.Fatal("an expired replay must report why it stopped")
	}
	if !res.Truncated {
		t.Fatalf("result = %+v, want truncated on a deadline", res)
	}
}

func TestReplayFailsWhenTheShimConnectionDrops(t *testing.T) {
	// Arrange — a replay whose shim went away is never going to finish;
	// leaving the caller blocked would be worse than telling it.
	rig := newReplayRig(t, func(conn net.Conn, _ *corev1.ReplayRequest) {
		conn.Close()
	})
	// Act
	res, _ := rig.client.Replay(context.Background(), 0, 10, 0, func(*corev1.Event) {})
	// Assert
	if !res.Truncated || !strings.Contains(res.Reason, "connection closed") {
		t.Fatalf("result = %+v, want truncated naming the lost connection", res)
	}
}

func TestReplayReportsALostLinkAsItsOwnError(t *testing.T) {
	// Arrange — the shim bounces the daemon link deliberately when the vendor
	// rotates its session uuid. Reporting that as the shim's own truncation
	// verdict is what turned a rotation into a failure card with nothing behind
	// it, so a lost link is its own error and the caller decides what it means.
	rig := newReplayRig(t, func(conn net.Conn, _ *corev1.ReplayRequest) {
		conn.Close()
	})
	// Act
	_, err := rig.client.Replay(context.Background(), 0, 10, 0, func(*corev1.Event) {})
	// Assert
	if !errors.Is(err, ErrReplayLinkLost) {
		t.Fatalf("err = %v, want ErrReplayLinkLost", err)
	}
}

func TestReplayEventForAnUnknownRequestIsDropped(t *testing.T) {
	// Arrange — a frame arriving for a replay nobody is waiting on (a late one
	// after the caller gave up) must be DROPPED, never redirected into the live
	// path. Its payload is a TaskStarted, which on the live path would reach
	// the SSM, so a leak would show up loudly.
	h := newHarness()
	ready := make(chan struct{})
	path := startFakeShim(t, func(conn net.Conn) {
		fakeServerHandshake(t, conn, "s1", "1", false)
		mustWriteMsg(t, conn, replayEvent("no-such-request", 42))
		// A live event AFTER the stray one: its arrival at the state sink is
		// the synchronization point, so the assertion never races the demux.
		mustWriteMsg(t, conn, &corev1.Event{
			SessionId: "vendor-uuid", Seq: 1,
			Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}},
		})
		close(ready)
		for {
			if _, err := wire.ReadAny(conn); err != nil {
				return
			}
		}
	})
	c := New(h.config(t, "s1", path))
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	go func() { _ = c.Run(ctx) }()

	// Act
	<-ready
	live := recvEvent(t, h.state.ch)

	// Assert — the live event arrived, and it is the ONLY thing that did.
	if live.GetSeq() != 1 {
		t.Fatalf("state sink got seq=%d, want the live event (seq 1)", live.GetSeq())
	}
	select {
	case ev := <-h.state.ch:
		t.Fatalf("a stray replay event reached the SSM sink: seq=%d", ev.GetSeq())
	default:
	}
}
