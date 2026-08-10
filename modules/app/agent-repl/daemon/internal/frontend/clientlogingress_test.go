package frontend

import (
	"testing"
	"time"
)

func TestReadLoopAcksAClientLogWithoutOccupyingItsWorkspaceLane(t *testing.T) {
	// Arrange: one workspace's client_log write is parked. If the record still
	// rode that workspace's lane, the lane's executor would be inside it and
	// the prompt behind it could not run.
	h := newLaneHandler()
	entered := make(chan string, 2)
	release := make(chan struct{})
	h.log = func(rid string) error {
		entered <- rid
		<-release
		return nil
	}
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)
	t.Cleanup(func() { close(release) })

	// Act.
	harness.send(clientLogCmdFor("log", "/ws/a"))
	select {
	case <-entered:
	case <-time.After(laneTestDeadline):
		t.Fatal("the client_log write never started")
	}
	harness.send(submitCmd("prompt", "/ws/a"))

	// Assert: the client_log is acked at ingress, and the prompt on the SAME
	// workspace runs while that write is still parked.
	first := harness.nextAck()
	if first.GetRequestId() != "log" || !first.GetOk() {
		t.Fatalf("first ack = %q ok=%v, want an ok ingress ack for the client_log", first.GetRequestId(), first.GetOk())
	}
	second := harness.nextAck()
	if second.GetRequestId() != "prompt" || !second.GetOk() {
		t.Fatalf("second ack = %q ok=%v, want the prompt answered while the client_log write is parked",
			second.GetRequestId(), second.GetOk())
	}
}

func TestReadLoopAnswersAnOpenWorkspaceDuringAClientLogFlood(t *testing.T) {
	// Arrange: the production shape — a webview flooding client_log while an
	// open_workspace for a DIFFERENT workspace arrives behind the flood. The
	// flood's writes are parked, so nothing about this passes on timing: the
	// open is either answered while every record is still unwritten, or it is
	// coupled to them.
	const flood = 4096
	h := newLaneHandler()
	release := make(chan struct{})
	h.log = func(string) error {
		<-release
		return nil
	}
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)
	t.Cleanup(func() { close(release) })

	// Act.
	for i := 0; i < flood; i++ {
		harness.send(clientLogCmdFor("log-"+itoa(i), "/ws/flooder"))
	}
	harness.send(openCmd("open", "/ws/other"))

	// Assert: the open's ack arrives, and it arrives without the flood being
	// released.
	for {
		ack := harness.nextAck()
		if ack.GetRequestId() == "open" {
			if !ack.GetOk() {
				t.Fatalf("open ack = nack %q, want ok", ack.GetError())
			}
			return
		}
	}
}

func TestReadLoopKeepsClientLogOffTheLaneDepthAccounting(t *testing.T) {
	// Arrange: a lane's queue is what the read loop hands it, and client_log is
	// no longer handed to it at all.
	lane := &commandLane{key: "/ws/a", ready: make(chan struct{}, 1)}

	// Act.
	_, queued := lane.push(laneItem{ticket: &commandTicket{cmd: submitCmd("prompt", "/ws/a")}})

	// Assert.
	if queued.depth != 1 {
		t.Fatalf("lane depth = %d, want the one interactive command", queued.depth)
	}
}
