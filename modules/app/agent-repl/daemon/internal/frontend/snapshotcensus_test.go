package frontend

import (
	"fmt"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
)

// ---------------------------------------------------------------------------
// WHO ASKED FOR THIS SNAPSHOT, AND WHY.
//
// Every full state snapshot the daemon assembles used to be logged by the state
// provider, which is handed no client and named none, under one heading:
// "connect snapshot". A three-hour daemon lifetime wrote 5350 of those lines
// with only 95 client connects behind them — the other 5255 were GUI streams
// renewing their freshness lease on a timer — and the log could not tell the
// two apart. A reconnect storm and a lease cadence are different faults with
// different remedies, so the record names the client and the phase.
// ---------------------------------------------------------------------------

// captureLogs is a Logf that keeps every rendered line for assertion.
type captureLogs struct {
	mu    sync.Mutex
	lines []string
}

func (c *captureLogs) logf(format string, args ...any) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.lines = append(c.lines, fmt.Sprintf(format, args...))
}

// matching returns every captured line containing want.
func (c *captureLogs) matching(want string) []string {
	c.mu.Lock()
	defer c.mu.Unlock()
	var out []string
	for _, line := range c.lines {
		if strings.Contains(line, want) {
			out = append(out, line)
		}
	}
	return out
}

func TestTheLeaseSnapshotCensusNamesItsClientAndPhase(t *testing.T) {
	// Arrange — one GUI stream, the client class whose lease renewals produced
	// the churn that read as reconnects.
	logs := &captureLogs{}
	s := New(Config{
		Logf: logs.logf, LogVerbosef: func(string, ...any) {},
		State:   staticState{snap: sampleSnapshot()},
		Handler: &mockHandler{},
	})
	cl := &client{
		id: 64, out: newOutbox(4), done: make(chan struct{}),
		scope: &Scope{Workspace: "w1", SessionID: "s1"}, kind: ClientKindGUIStream,
	}
	s.clients[cl] = struct{}{}

	// Act.
	if !s.renewSnapshotLease(cl) {
		t.Fatal("renewSnapshotLease = false, want a live lease")
	}

	// Assert.
	got := logs.matching("state snapshot served")
	if len(got) != 1 {
		t.Fatalf("census lines = %v, want exactly one", got)
	}
	for _, want := range []string{"client_id=64", "kind=gui_stream", "phase=lease", "workspaces=1"} {
		if !strings.Contains(got[0], want) {
			t.Fatalf("census line %q is missing %q", got[0], want)
		}
	}
}

func TestTheConnectSnapshotCensusNamesItsClientAndPhase(t *testing.T) {
	// Arrange.
	logs := &captureLogs{}
	s := New(Config{
		Logf: logs.logf, LogVerbosef: func(string, ...any) {},
		State:   staticState{snap: sampleSnapshot()},
		Handler: &mockHandler{},
	})
	defer s.Close()

	// Act.
	conn, _ := dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIObserver)
	defer conn.Close()

	// Assert — a real connect is distinguishable from a lease renewal by the
	// phase alone, which is the distinction the old line could not carry.
	got := logs.matching("state snapshot served")
	if len(got) != 1 {
		t.Fatalf("census lines = %v, want exactly one", got)
	}
	for _, want := range []string{"kind=gui_observer", "phase=connect"} {
		if !strings.Contains(got[0], want) {
			t.Fatalf("census line %q is missing %q", got[0], want)
		}
	}
}

// ---------------------------------------------------------------------------
// AN ACK IS NEVER STARVED BEHIND SNAPSHOT SERVING.
//
// Assembling a snapshot walks every workspace of daemon-wide state, and the
// daemon assembles roughly one per second across its GUI streams' leases. An
// openWorkspace whose ack had to wait out an assembly in progress would blow
// the client's ten-second ack deadline and open a `client.command_unacked'
// failure card for a command the daemon had not refused.
// ---------------------------------------------------------------------------

// blockingState is a StateProvider whose assembly parks until it is released.
// It stands in for a full 150-workspace assembly without costing one, and it
// makes "an assembly is in progress" an observable instant rather than a race.
type blockingState struct {
	entered chan struct{}
	release chan struct{}
	snap    *frontendv1.StateSnapshot
	// once guards entered, because the connect that starts the test's client
	// also assembles a snapshot and only one assembly may be the blocked one.
	once sync.Once
	// blocking is closed by the test once the connect snapshot is past, so the
	// connect itself is served without parking.
	blocking chan struct{}
}

func (b *blockingState) Snapshot() *frontendv1.StateSnapshot {
	select {
	case <-b.blocking:
		b.once.Do(func() { close(b.entered) })
		<-b.release
	default:
	}
	return b.snap
}

func TestAnOpenWorkspaceAckIsNotStarvedBehindSnapshotServing(t *testing.T) {
	// Arrange — a connected client, and a second assembly parked mid-flight.
	state := &blockingState{
		entered: make(chan struct{}), release: make(chan struct{}),
		blocking: make(chan struct{}), snap: sampleSnapshot(),
	}
	handler := &mockHandler{}
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: state, Handler: handler,
	})
	defer s.Close()
	conn, _ := dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIStream)
	defer conn.Close()
	close(state.blocking)

	// A second client's connect is the assembly that parks. The dial itself
	// returns on the WebSocket upgrade, which the daemon writes before it
	// assembles anything, so the park happens server-side while this goroutine
	// carries on.
	dialRaw(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIObserver)
	<-state.entered

	// Act — the command arrives while that assembly holds.
	data, err := protojson.Marshal(&frontendv1.FrontendCommand{
		RequestId: "open-1", Workspace: "w1",
		Command: &frontendv1.FrontendCommand_OpenWorkspace{OpenWorkspace: &frontendv1.OpenWorkspaceCmd{}},
	})
	if err != nil {
		t.Fatalf("marshal command: %v", err)
	}
	if err := conn.WriteMessage(websocket.TextMessage, data); err != nil {
		t.Fatalf("ws write: %v", err)
	}
	ack := readWSFrame(t, conn).GetCommandAck()

	// Assert — the ack came back with the assembly still parked. Releasing it
	// only afterwards is what makes this a starvation test rather than a race:
	// a daemon that served the ack behind the snapshot would deadlock here.
	if ack.GetRequestId() != "open-1" {
		t.Fatalf("ack request_id = %q, want open-1", ack.GetRequestId())
	}
	if !ack.GetOk() {
		t.Fatalf("ack = %+v, want ok", ack)
	}
	close(state.release)
}
