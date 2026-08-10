package frontend

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
)

// laneTestDeadline bounds how long a test waits for a frame that a correct
// implementation produces immediately. It is a FAILURE deadline, never a
// synchronization device: every ordering in these tests is established by
// channels, and this only converts a hang into a named failure.
const laneTestDeadline = 5 * time.Second

// scriptedConn feeds a read loop a scripted command sequence and captures the
// frames the connection writes back.
type scriptedConn struct {
	cmds   chan *frontendv1.FrontendCommand
	writes chan []byte

	eofOnce   sync.Once
	closeOnce sync.Once
	closed    chan struct{}
}

func newScriptedConn() *scriptedConn {
	return &scriptedConn{
		cmds:   make(chan *frontendv1.FrontendCommand, 32),
		writes: make(chan []byte, 32),
		closed: make(chan struct{}),
	}
}

func (c *scriptedConn) readCommand() (*frontendv1.FrontendCommand, error) {
	cmd, ok := <-c.cmds
	if !ok {
		return nil, io.EOF
	}
	return cmd, nil
}

func (c *scriptedConn) writeFrame(data []byte, _ func()) error {
	c.writes <- append([]byte(nil), data...)
	return nil
}

func (c *scriptedConn) close(closeCause) error {
	c.closeOnce.Do(func() { close(c.closed) })
	return nil
}

// laneHarness runs one client's read and write loops over a scripted
// connection, which is exactly the production wiring minus the accept path.
type laneHarness struct {
	t    *testing.T
	conn *scriptedConn
	cl   *client
	done chan struct{}
}

func newLaneHarness(t *testing.T, s *Server) *laneHarness {
	t.Helper()
	c := newScriptedConn()
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	s.mu.Lock()
	s.nextClientID++
	cl.id = s.nextClientID
	s.clients[cl] = struct{}{}
	s.mu.Unlock()

	h := &laneHarness{t: t, conn: c, cl: cl, done: make(chan struct{})}
	go s.writeLoop(c, cl)
	go func() {
		defer close(h.done)
		s.readLoop(c, cl)
	}()
	t.Cleanup(func() {
		h.eof()
		select {
		case <-h.done:
		case <-time.After(laneTestDeadline):
			t.Fatal("read loop did not finish after EOF")
		}
	})
	return h
}

func (h *laneHarness) send(cmd *frontendv1.FrontendCommand) {
	h.conn.cmds <- cmd
}

// eof ends the scripted command stream, which is how a connection closes.
func (h *laneHarness) eof() {
	h.conn.eofOnce.Do(func() { close(h.conn.cmds) })
}

func sprintfLane(format string, args ...any) string { return fmt.Sprintf(format, args...) }

var errLaneRefused = errors.New("frontend test: the handler refused this command")

// nextAck returns the request id of the next CommandAck the connection wrote,
// skipping any other frame.
func (h *laneHarness) nextAck() *frontendv1.CommandAck {
	h.t.Helper()
	for {
		select {
		case data := <-h.conn.writes:
			frame := &frontendv1.FrontendFrame{}
			if err := protojson.Unmarshal(data, frame); err != nil {
				h.t.Fatalf("decode written frame: %v", err)
			}
			if ack := frame.GetCommandAck(); ack != nil {
				return ack
			}
		case <-time.After(laneTestDeadline):
			h.t.Fatal("no CommandAck was written before the failure deadline")
			return nil
		}
	}
}

// laneHandler is a CommandHandler whose OpenWorkspace and SubmitPrompt are
// scriptable per workspace, with a concurrency census the tests assert on.
type laneHandler struct {
	*mockHandler
	// open, when non-nil, replaces OpenWorkspace's body.
	open func(workspace string) error
	// submit, when non-nil, replaces SubmitPrompt's body.
	submit func(workspace, requestID string) error
	// log, when non-nil, replaces ClientLog's body. It is how a test parks a
	// telemetry write to prove nothing interactive is behind it.
	log func(requestID string) error

	mu      sync.Mutex
	order   []string
	live    map[string]int
	maxLive map[string]int
}

func newLaneHandler() *laneHandler {
	return &laneHandler{
		mockHandler: &mockHandler{},
		live:        map[string]int{},
		maxLive:     map[string]int{},
	}
}

func (h *laneHandler) enter(workspace, requestID string) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.order = append(h.order, requestID)
	h.live[workspace]++
	if h.live[workspace] > h.maxLive[workspace] {
		h.maxLive[workspace] = h.live[workspace]
	}
}

func (h *laneHandler) exit(workspace string) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.live[workspace]--
}

func (h *laneHandler) entryOrder() []string {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]string(nil), h.order...)
}

func (h *laneHandler) peakConcurrency(workspace string) int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.maxLive[workspace]
}

func (h *laneHandler) OpenWorkspace(_ context.Context, ws, rid string, _ *frontendv1.OpenWorkspaceCmd) error {
	h.enter(ws, rid)
	defer h.exit(ws)
	if h.open != nil {
		return h.open(ws)
	}
	return nil
}

func (h *laneHandler) SubmitPrompt(_ context.Context, ws, rid string, _ *frontendv1.SubmitPromptCmd) error {
	h.enter(ws, rid)
	defer h.exit(ws)
	if h.submit != nil {
		return h.submit(ws, rid)
	}
	return nil
}

func (h *laneHandler) ClientLog(_ context.Context, ws, rid string, _ *frontendv1.ClientLogCmd) error {
	h.enter(ws, rid)
	defer h.exit(ws)
	if h.log != nil {
		return h.log(rid)
	}
	return nil
}

func openCmd(requestID, workspace string) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID, Workspace: workspace,
		Command: &frontendv1.FrontendCommand_OpenWorkspace{OpenWorkspace: &frontendv1.OpenWorkspaceCmd{}},
	}
}

func submitCmd(requestID, workspace string) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID, Workspace: workspace,
		Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{Text: "hi"}},
	}
}

func clientLogCmd(requestID string) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID,
		Command:   &frontendv1.FrontendCommand_ClientLog{ClientLog: &frontendv1.ClientLogCmd{Message: "m"}},
	}
}

func newLaneServer(t *testing.T, h CommandHandler) *Server {
	t.Helper()
	return New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: h,
	})
}

func TestLaneKeyRoutesCommandsToTheirSerializationDomain(t *testing.T) {
	tests := []struct {
		name string
		cmd  *frontendv1.FrontendCommand
		want string
	}{
		{name: "workspace command takes its workspace lane", cmd: openCmd("r1", "/ws/a"), want: "/ws/a"},
		{name: "workspace-less command takes the global lane", cmd: clientLogCmd("r2"), want: globalLaneKey},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := laneKey(tc.cmd)

			// Assert.
			if got != tc.want {
				t.Fatalf("laneKey = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestReadLoopRunsDifferentWorkspacesConcurrently(t *testing.T) {
	// Arrange: each open reports its arrival and then waits for every other
	// open to have arrived. Only genuine overlap can satisfy that barrier, so
	// the structure — not a clock — is what proves concurrency.
	const workspaces = 4
	h := newLaneHandler()
	arrived := make(chan string, workspaces)
	release := make(chan struct{})
	h.open = func(ws string) error {
		arrived <- ws
		<-release
		return nil
	}
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act.
	for _, ws := range []string{"/ws/a", "/ws/b", "/ws/c", "/ws/d"} {
		harness.send(openCmd("r-"+ws, ws))
	}

	// Assert: all four are inside the handler at once.
	seen := map[string]bool{}
	for i := 0; i < workspaces; i++ {
		select {
		case ws := <-arrived:
			seen[ws] = true
		case <-time.After(laneTestDeadline):
			t.Fatalf("only %d of %d opens were running concurrently: %v", len(seen), workspaces, seen)
		}
	}
	close(release)
	if len(seen) != workspaces {
		t.Fatalf("concurrent opens = %v, want %d distinct workspaces", seen, workspaces)
	}
	for i := 0; i < workspaces; i++ {
		if ack := harness.nextAck(); !ack.GetOk() {
			t.Fatalf("ack %s = nack %q, want ok", ack.GetRequestId(), ack.GetError())
		}
	}
}

func TestReadLoopKeepsOneWorkspacesCommandsInArrivalOrder(t *testing.T) {
	// Arrange: three commands for ONE workspace, each announcing its entry.
	h := newLaneHandler()
	h.open = func(string) error { return nil }
	h.submit = func(_, rid string) error { return nil }
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act.
	harness.send(openCmd("r1", "/ws/a"))
	harness.send(submitCmd("r2", "/ws/a"))
	harness.send(submitCmd("r3", "/ws/a"))

	// Assert: entry order and ack order both follow arrival, and the lane
	// never ran two of them at once.
	var acks []string
	for i := 0; i < 3; i++ {
		acks = append(acks, harness.nextAck().GetRequestId())
	}
	want := []string{"r1", "r2", "r3"}
	if strings.Join(h.entryOrder(), ",") != strings.Join(want, ",") {
		t.Fatalf("handler entry order = %v, want %v", h.entryOrder(), want)
	}
	if strings.Join(acks, ",") != strings.Join(want, ",") {
		t.Fatalf("ack order = %v, want %v", acks, want)
	}
	if peak := h.peakConcurrency("/ws/a"); peak != 1 {
		t.Fatalf("peak concurrency on one workspace lane = %d, want 1", peak)
	}
}

func TestReadLoopAcksAWorkspaceWhileAnotherBringUpIsBlocked(t *testing.T) {
	// Arrange: /ws/slow's open never returns until the test releases it.
	h := newLaneHandler()
	blocked := make(chan struct{})
	release := make(chan struct{})
	h.open = func(ws string) error {
		if ws == "/ws/slow" {
			close(blocked)
			<-release
		}
		return nil
	}
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act: the fast workspace's command is sent only once the slow bring-up is
	// demonstrably inside the handler, so the ack below cannot predate it.
	harness.send(openCmd("slow", "/ws/slow"))
	select {
	case <-blocked:
	case <-time.After(laneTestDeadline):
		t.Fatal("the slow bring-up never started")
	}
	harness.send(submitCmd("fast", "/ws/fast"))

	// Assert: the fast workspace is answered while the slow one is still held.
	ack := harness.nextAck()
	if ack.GetRequestId() != "fast" || !ack.GetOk() {
		t.Fatalf("first ack = %s ok=%v, want fast ok", ack.GetRequestId(), ack.GetOk())
	}
	close(release)
	if ack := harness.nextAck(); ack.GetRequestId() != "slow" {
		t.Fatalf("second ack = %s, want slow", ack.GetRequestId())
	}
}

func TestReadLoopAcksAGlobalCommandWhileABringUpIsBlocked(t *testing.T) {
	// Arrange: the global lane must never queue behind a workspace bring-up —
	// the roster publish and the daemon-global controls ride it.
	h := newLaneHandler()
	blocked := make(chan struct{})
	release := make(chan struct{})
	h.open = func(string) error {
		close(blocked)
		<-release
		return nil
	}
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act.
	harness.send(openCmd("slow", "/ws/slow"))
	select {
	case <-blocked:
	case <-time.After(laneTestDeadline):
		t.Fatal("the slow bring-up never started")
	}
	harness.send(clientLogCmd("global"))

	// Assert.
	ack := harness.nextAck()
	if ack.GetRequestId() != "global" || !ack.GetOk() {
		t.Fatalf("first ack = %s ok=%v, want global ok", ack.GetRequestId(), ack.GetOk())
	}
	close(release)
	if ack := harness.nextAck(); ack.GetRequestId() != "slow" {
		t.Fatalf("second ack = %s, want slow", ack.GetRequestId())
	}
}

func TestReadLoopStillNacksAFailedCommand(t *testing.T) {
	// Arrange.
	h := newLaneHandler()
	h.open = func(string) error { return errLaneRefused }
	s := newLaneServer(t, h)
	harness := newLaneHarness(t, s)

	// Act.
	harness.send(openCmd("r1", "/ws/a"))

	// Assert: a lane-run failure surfaces exactly as an inline one did.
	ack := harness.nextAck()
	if ack.GetOk() {
		t.Fatal("ack ok = true, want the handler's refusal")
	}
	if !strings.Contains(ack.GetError(), errLaneRefused.Error()) {
		t.Fatalf("ack error = %q, want it to carry %q", ack.GetError(), errLaneRefused)
	}
	if ack.GetFailure() == nil {
		t.Fatal("ack failure classification is nil, want the classified refusal")
	}
}

func TestCommandLanesRunEveryReadCommandBeforeCloseReturns(t *testing.T) {
	// Arrange: a command queued behind a running one when the connection ends
	// is still owed an answer — it was already taken off the socket.
	var mu sync.Mutex
	var ran []string
	started := make(chan struct{})
	release := make(chan struct{})
	s := newLaneServer(t, newLaneHandler())
	lanes := newCommandLanes(testLogf(t), testLogf(t), func(ticket *commandTicket) {
		mu.Lock()
		ran = append(ran, ticket.cmd.GetRequestId())
		first := len(ran) == 1
		mu.Unlock()
		if first {
			close(started)
			<-release
		}
	}, func(*commandTicket) { t.Error("nothing was coalesced, want no supersede answer") })

	// Act.
	lanes.submit(s.newCommandTicket(nil, openCmd("r1", "/ws/a"), time.Now(), 1))
	<-started
	lanes.submit(s.newCommandTicket(nil, openCmd("r2", "/ws/a"), time.Now(), 2))
	close(release)
	lanes.close()

	// Assert.
	mu.Lock()
	defer mu.Unlock()
	if strings.Join(ran, ",") != "r1,r2" {
		t.Fatalf("commands run = %v, want r1,r2", ran)
	}
}

func TestCommandLanesFailHardOnASubmitAfterClose(t *testing.T) {
	// Arrange: only the read loop submits, and only before it closes the
	// lanes, so a late submit is a violated invariant rather than a condition
	// to cope with.
	var logs []string
	var ran []string
	s := newLaneServer(t, newLaneHandler())
	lanes := newCommandLanes(
		func(format string, args ...any) { logs = append(logs, sprintfLane(format, args...)) },
		testLogf(t),
		func(ticket *commandTicket) { ran = append(ran, ticket.cmd.GetRequestId()) },
		func(*commandTicket) { t.Fatal("nothing was coalesced, want no supersede answer") },
	)
	lanes.close()

	// Act.
	recovered := func() (v any) {
		defer func() { v = recover() }()
		lanes.submit(s.newCommandTicket(nil, openCmd("late", "/ws/a"), time.Now(), 1))
		return nil
	}()

	// Assert: loud panic carrying the offending identity, and nothing run
	// outside a lane's ordering.
	panicked, _ := recovered.(string)
	if !strings.Contains(panicked, `request_id="late"`) {
		t.Fatalf("recover = %v, want a panic naming the late command", recovered)
	}
	if len(ran) != 0 {
		t.Fatalf("commands run = %v, want none after close", ran)
	}
	if len(logs) != 1 || !strings.Contains(logs[0], "AFTER the connection's lanes closed") {
		t.Fatalf("logs = %v, want one loud post-close line", logs)
	}
}
