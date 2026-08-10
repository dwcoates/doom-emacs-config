package frontend

import (
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// clientLogCmdFor is a workspace-addressed client log, which is what a webview
// actually sends: the record names the workspace it came from, so it lands on
// that workspace's lane rather than the global one.
func clientLogCmdFor(requestID, workspace string) *frontendv1.FrontendCommand {
	cmd := clientLogCmd(requestID)
	cmd.Workspace = workspace
	return cmd
}

func TestCommandLaneServesAnInteractiveCommandBeforeQueuedClientLogs(t *testing.T) {
	// Arrange: a burst of console records queued ahead of a prompt, which is
	// the shape that drove one connection's queue to thousands of entries.
	s := newLaneServer(t, newLaneHandler())
	lane := &commandLane{key: "/ws/a", ready: make(chan struct{}, 1)}
	lane.push(laneItem{ticket: s.newCommandTicket(nil, clientLogCmdFor("log1", "/ws/a"), time.Now(), 1)})
	lane.push(laneItem{ticket: s.newCommandTicket(nil, clientLogCmdFor("log2", "/ws/a"), time.Now(), 2)})
	lane.push(laneItem{ticket: s.newCommandTicket(nil, submitCmd("prompt", "/ws/a"), time.Now(), 3)})

	// Act.
	served := drainLane(t, lane, 3)

	// Assert.
	if strings.Join(served, ",") != "prompt,log1,log2" {
		t.Fatalf("serve order = %v, want the prompt ahead of both queued client logs", served)
	}
}

func TestCommandLaneKeepsQueuedClientLogsInEmissionOrder(t *testing.T) {
	// Arrange: deferring evidence must not scramble it — the daemon's log is
	// read as a sequence.
	s := newLaneServer(t, newLaneHandler())
	lane := &commandLane{key: "/ws/a", ready: make(chan struct{}, 1)}
	for i, rid := range []string{"log1", "log2", "log3"} {
		lane.push(laneItem{ticket: s.newCommandTicket(nil, clientLogCmdFor(rid, "/ws/a"), time.Now(), int64(i))})
	}

	// Act.
	served := drainLane(t, lane, 3)

	// Assert.
	if strings.Join(served, ",") != "log1,log2,log3" {
		t.Fatalf("serve order = %v, want emission order", served)
	}
}

func TestCommandLaneKeepsInteractiveCommandsInArrivalOrderAcrossAClientLog(t *testing.T) {
	// Arrange: the low-priority class may only defer client logs. Interactive
	// commands keep their order among themselves, client log or not.
	s := newLaneServer(t, newLaneHandler())
	lane := &commandLane{key: "/ws/a", ready: make(chan struct{}, 1)}
	lane.push(laneItem{ticket: s.newCommandTicket(nil, openCmd("open", "/ws/a"), time.Now(), 1)})
	lane.push(laneItem{ticket: s.newCommandTicket(nil, clientLogCmdFor("log", "/ws/a"), time.Now(), 2)})
	lane.push(laneItem{ticket: s.newCommandTicket(nil, submitCmd("prompt", "/ws/a"), time.Now(), 3)})

	// Act.
	served := drainLane(t, lane, 3)

	// Assert.
	if strings.Join(served, ",") != "open,prompt,log" {
		t.Fatalf("serve order = %v, want open before prompt with the client log last", served)
	}
}

func TestCommandLaneCloseReportsQueuedClientLogsAsStillOwed(t *testing.T) {
	// Arrange: a deferred client log was still read off the socket, so the
	// connection's teardown owes it an answer like any other command.
	s := newLaneServer(t, newLaneHandler())
	lane := &commandLane{key: "/ws/a", ready: make(chan struct{}, 1)}
	lane.push(laneItem{ticket: s.newCommandTicket(nil, clientLogCmdFor("log", "/ws/a"), time.Now(), 1)})

	// Act.
	pending := lane.close()

	// Assert.
	if pending != 1 {
		t.Fatalf("pending at close = %d, want the queued client log counted", pending)
	}
}

func TestCommandLanesNeverPreemptAnExecutingClientLog(t *testing.T) {
	// Arrange: the client log is INSIDE the runner before the prompt is
	// submitted, so only a preemption could let the prompt finish first.
	var mu sync.Mutex
	var ran []string
	executing := make(chan struct{})
	release := make(chan struct{})
	s := newLaneServer(t, newLaneHandler())
	lanes := newCommandLanes(testLogf(t), testLogf(t), func(ticket *commandTicket) {
		if ticket.cmd.GetClientLog() != nil {
			close(executing)
			<-release
		}
		mu.Lock()
		ran = append(ran, ticket.cmd.GetRequestId())
		mu.Unlock()
	}, func(*commandTicket) { t.Error("nothing was coalesced, want no supersede answer") })

	// Act.
	lanes.submit(s.newCommandTicket(nil, clientLogCmdFor("log", "/ws/a"), time.Now(), 1))
	select {
	case <-executing:
	case <-time.After(laneTestDeadline):
		t.Fatal("the client log never started running")
	}
	lanes.submit(s.newCommandTicket(nil, submitCmd("prompt", "/ws/a"), time.Now(), 2))
	close(release)
	lanes.close()

	// Assert.
	mu.Lock()
	defer mu.Unlock()
	if strings.Join(ran, ",") != "log,prompt" {
		t.Fatalf("completion order = %v, want the executing client log to finish first", ran)
	}
}

// drainLane serves COUNT entries off a lane and returns their request ids.
func drainLane(t *testing.T, lane *commandLane, count int) []string {
	t.Helper()
	var served []string
	for i := 0; i < count; i++ {
		item, ok, _ := lane.next()
		if !ok {
			t.Fatalf("lane ran dry after %d of %d entries", i, count)
		}
		served = append(served, item.ticket.cmd.GetRequestId())
	}
	return served
}
