package frontend

import (
	"bufio"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
)

// shortSock returns a unix-socket path short enough for the platform's
// sun_path limit (~104 bytes on darwin), which t.TempDir()'s long names blow.
func shortSock(t *testing.T, name string) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "fe")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return filepath.Join(dir, name)
}

func testLogf(t *testing.T) func(string, ...any) {
	t.Helper()
	return func(format string, args ...any) { t.Logf(format, args...) }
}

// staticState is a fixed StateProvider for tests.
type staticState struct{ snap *frontendv1.StateSnapshot }

func (s staticState) Snapshot() *frontendv1.StateSnapshot {
	if s.snap == nil {
		return &frontendv1.StateSnapshot{}
	}
	return s.snap
}

type sequenceState struct {
	mu    sync.Mutex
	snaps []*frontendv1.StateSnapshot
}

func (s *sequenceState) Snapshot() *frontendv1.StateSnapshot {
	s.mu.Lock()
	defer s.mu.Unlock()
	if len(s.snaps) == 0 {
		panic("sequenceState exhausted")
	}
	snapshot := s.snaps[0]
	s.snaps = s.snaps[1:]
	return snapshot
}

func sampleSnapshot() *frontendv1.StateSnapshot {
	return &frontendv1.StateSnapshot{
		Workspaces: []*frontendv1.WorkspaceState{
			{Workspace: "w1", SessionId: "s1", State: frontendv1.RenderState_RENDER_STATE_IDLE},
		},
	}
}

// mustPop takes the next queued payload off a client's outbox, failing the
// test when nothing is waiting.
func mustPop(t *testing.T, cl *client) []byte {
	t.Helper()
	data, ok := cl.out.pop()
	if !ok {
		t.Fatalf("client %d outbox is empty, want a queued frame", cl.id)
	}
	return data
}

func newTestServer(t *testing.T, buf int) (*Server, *mockHandler) {
	t.Helper()
	h := &mockHandler{}
	s := New(Config{Logf: testLogf(t), LogVerbosef: testLogf(t), State: staticState{snap: sampleSnapshot()}, Handler: h, BufSize: buf})
	return s, h
}

func TestRenewSnapshotLeaseEnqueuesRevisionedCurrentState(t *testing.T) {
	var verbose []string
	h := &mockHandler{}
	s := New(Config{
		Logf: testLogf(t),
		LogVerbosef: func(format string, args ...any) {
			verbose = append(verbose, fmt.Sprintf(format, args...))
		},
		State: staticState{snap: &frontendv1.StateSnapshot{
			Workspaces: []*frontendv1.WorkspaceState{
				{Workspace: "/w", SessionId: "s1", ControllerGenerationId: "g1", AtMs: 42, CauseSeq: 7},
			},
		}},
		Handler: h,
	})
	cl := &client{
		id: 9, out: newOutbox(1), done: make(chan struct{}),
		scope: &Scope{Workspace: "/w", SessionID: "s1"}, kind: ClientKindGUIStream,
	}
	s.clients[cl] = struct{}{}

	if !s.renewSnapshotLease(cl) {
		t.Fatal("renewSnapshotLease = false, want live lease")
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(mustPop(t, cl), frame); err != nil {
		t.Fatalf("decode lease snapshot: %v", err)
	}
	state := frame.GetSnapshot().GetWorkspaces()[0]
	if state.GetAtMs() != 42 || state.GetControllerGenerationId() != "g1" || state.GetCauseSeq() != 7 {
		t.Fatalf("lease revision = %+v, want g1/42/7", state)
	}
	if len(verbose) != 1 || !strings.Contains(verbose[0], "revisions=/w:g1:42:7") {
		t.Fatalf("verbose lease log = %v, want revision identity", verbose)
	}
}

func TestRenewSnapshotLeaseHardDisconnectsAFullQueue(t *testing.T) {
	var logs []string
	s := New(Config{
		Logf: func(format string, args ...any) {
			logs = append(logs, fmt.Sprintf(format, args...))
		},
		LogVerbosef: testLogf(t),
		State:       staticState{snap: sampleSnapshot()},
		Handler:     &mockHandler{},
	})
	cl := &client{
		id: 10, out: newOutbox(1), done: make(chan struct{}),
		scope: &Scope{Workspace: "w1", SessionID: "s1"}, kind: ClientKindGUIStream,
	}
	cl.out.push(outFrame{data: []byte("occupied")})
	s.clients[cl] = struct{}{}

	if s.renewSnapshotLease(cl) {
		t.Fatal("renewSnapshotLease = true, want full-queue detach")
	}
	select {
	case <-cl.done:
	default:
		t.Fatal("full lease queue did not close client")
	}
	if len(logs) != 2 || !strings.Contains(logs[0], "snapshot lease queue full") || !strings.Contains(logs[1], "client disconnected") {
		t.Fatalf("lease failure logs = %v, want queue-full then disconnect identities", logs)
	}
}

func TestRenewSnapshotLeaseRetriesCaptureOlderThanDeliveredState(t *testing.T) {
	var verbose []string
	provider := &sequenceState{snaps: []*frontendv1.StateSnapshot{
		{Workspaces: []*frontendv1.WorkspaceState{{Workspace: "/w", SessionId: "s1", AtMs: 41}}},
		{Workspaces: []*frontendv1.WorkspaceState{{Workspace: "/w", SessionId: "s1", AtMs: 42}}},
	}}
	s := New(Config{
		Logf: testLogf(t),
		LogVerbosef: func(format string, args ...any) {
			verbose = append(verbose, fmt.Sprintf(format, args...))
		},
		State: provider, Handler: &mockHandler{},
	})
	cl := &client{
		id: 12, out: newOutbox(1), done: make(chan struct{}),
		scope: &Scope{Workspace: "/w", SessionID: "s1"}, kind: ClientKindGUIStream,
	}
	s.clients[cl] = struct{}{}
	s.latestWorkspaceAt["/w"] = 42

	if !s.renewSnapshotLease(cl) {
		t.Fatal("renewSnapshotLease = false, want retry then renewal")
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(mustPop(t, cl), frame); err != nil {
		t.Fatalf("decode renewed snapshot: %v", err)
	}
	if got := frame.GetSnapshot().GetWorkspaces()[0].GetAtMs(); got != 42 {
		t.Fatalf("renewed snapshot at_ms = %d, want 42", got)
	}
	if len(verbose) != 2 || !strings.Contains(verbose[0], "snapshot lease retry") || !strings.Contains(verbose[1], "snapshot lease renewed") {
		t.Fatalf("verbose logs = %v, want retry then renewal", verbose)
	}
}

func TestRenewSnapshotLeaseHardDisconnectsMarshalFailure(t *testing.T) {
	var logs []string
	s := New(Config{
		Logf: func(format string, args ...any) {
			logs = append(logs, fmt.Sprintf(format, args...))
		},
		LogVerbosef: testLogf(t),
		State: staticState{snap: &frontendv1.StateSnapshot{Workspaces: []*frontendv1.WorkspaceState{
			{Workspace: string([]byte{0xff}), SessionId: "s1", AtMs: 1},
		}}},
		Handler: &mockHandler{},
	})
	cl := &client{id: 11, out: newOutbox(1), done: make(chan struct{}), kind: ClientKindGUIStream}
	s.clients[cl] = struct{}{}

	if s.renewSnapshotLease(cl) {
		t.Fatal("renewSnapshotLease = true, want marshal-failure detach")
	}
	select {
	case <-cl.done:
	default:
		t.Fatal("marshal failure did not close client")
	}
	if len(logs) != 2 || !strings.Contains(logs[0], "snapshot lease marshal failed") || !strings.Contains(logs[1], "client disconnected") {
		t.Fatalf("lease failure logs = %v, want marshal-failure then disconnect identities", logs)
	}
}

// --- UDS: snapshot on connect + command ack correlation ---------------------

func TestServeUDSSnapshotThenCommandAck(t *testing.T) {
	// Arrange.
	s, h := newTestServer(t, 0)
	l, err := net.Listen("unix", shortSock(t, "f.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	go func() { _ = s.Serve(l) }()
	defer s.Close()

	conn, err := net.Dial("unix", l.Addr().String())
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	defer conn.Close()
	r := bufio.NewReader(conn)

	// Act 1: read the connect snapshot (must arrive first).
	snapFrame := readFrame(t, r)
	// Assert 1.
	if snapFrame.GetSnapshot() == nil {
		t.Fatalf("first frame was not a snapshot: %v", snapFrame)
	}
	if got := snapFrame.GetSnapshot().GetWorkspaces()[0].GetWorkspace(); got != "w1" {
		t.Errorf("snapshot workspace = %q, want w1", got)
	}

	// Act 2: send a command, read the ack.
	writeCmd(t, conn, &frontendv1.FrontendCommand{
		RequestId: "c1", Workspace: "w1",
		Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{Text: "hi"}},
	})
	ackFrame := readFrame(t, r)

	// Assert 2.
	ack := ackFrame.GetCommandAck()
	if ack == nil {
		t.Fatalf("expected command_ack frame, got %v", ackFrame)
	}
	if ack.GetRequestId() != "c1" || !ack.GetOk() {
		t.Errorf("ack = %v, want request_id=c1 ok=true", ack)
	}
	if h.called != "submit_prompt" {
		t.Errorf("handler called %q, want submit_prompt", h.called)
	}
}

// --- UDS: ServeUDS binds, removes a stale socket, and serves ----------------

func TestServeUDSRemovesStaleSocketAndServes(t *testing.T) {
	// Arrange: a leftover regular file at the socket path.
	sock := shortSock(t, "stale.sock")
	if f, err := net.Listen("unix", sock); err == nil {
		_ = f.Close() // leaves a stale socket file behind
	}
	s, _ := newTestServer(t, 0)
	go func() { _ = s.ServeUDS(sock) }()
	defer s.Close()

	// Act: connect once ServeUDS has bound (busy-poll, no sleep-based sync).
	conn := retryDialUnix(t, sock)
	defer conn.Close()
	snapFrame := readFrame(t, bufio.NewReader(conn))

	// Assert.
	if snapFrame.GetSnapshot() == nil {
		t.Fatalf("expected snapshot after ServeUDS connect, got %v", snapFrame)
	}
}

// --- WS: snapshot on connect + command ack ----------------------------------

func TestServeWSSnapshotThenCommandAck(t *testing.T) {
	// Arrange.
	s, h := newTestServer(t, 0)
	defer s.Close()
	httpSrv := httptest.NewServer(http.HandlerFunc(s.ServeWS))
	defer httpSrv.Close()

	wsURL := "ws" + strings.TrimPrefix(httpSrv.URL, "http")
	conn, _, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	defer conn.Close()

	// Act 1: snapshot first.
	snapFrame := readWSFrame(t, conn)
	if snapFrame.GetSnapshot() == nil {
		t.Fatalf("first WS frame was not a snapshot: %v", snapFrame)
	}

	// Act 2: command + ack.
	cmdData, err := protojson.Marshal(&frontendv1.FrontendCommand{
		RequestId: "cws", Command: &frontendv1.FrontendCommand_Interrupt{Interrupt: &frontendv1.InterruptCmd{}},
	})
	if err != nil {
		t.Fatalf("marshal cmd: %v", err)
	}
	if err := conn.WriteMessage(websocket.TextMessage, cmdData); err != nil {
		t.Fatalf("ws write: %v", err)
	}
	ackFrame := readWSFrame(t, conn)

	// Assert.
	if ackFrame.GetCommandAck().GetRequestId() != "cws" {
		t.Errorf("ack request_id = %q, want cws", ackFrame.GetCommandAck().GetRequestId())
	}
	if h.called != "interrupt" {
		t.Errorf("handler called %q, want interrupt", h.called)
	}
}

func TestHostOnlyWorkspaceWorkReachesUDSButNotGUI(t *testing.T) {
	// Arrange.  Both connections are observers, so this proves delivery is
	// controlled by ClientKind alone.
	h := &mockHandler{}
	s := New(Config{Logf: testLogf(t), LogVerbosef: testLogf(t), State: staticState{snap: &frontendv1.StateSnapshot{
		WorkspaceAvailable: []*frontendv1.WorkspaceAvailable{{JobId: "job-1", FinalName: "new"}},
		HostActions:        []*frontendv1.HostAction{{ActionId: "action-1"}},
	}}, Handler: h})
	defer s.Close()

	l, err := net.Listen("unix", shortSock(t, "host.sock"))
	if err != nil {
		t.Fatalf("listen host UDS: %v", err)
	}
	go func() { _ = s.Serve(l) }()
	httpSrv := httptest.NewServer(http.HandlerFunc(s.ServeWS))
	defer httpSrv.Close()

	host, err := net.Dial("unix", l.Addr().String())
	if err != nil {
		t.Fatalf("dial host UDS: %v", err)
	}
	defer host.Close()
	guiURL := "ws" + strings.TrimPrefix(httpSrv.URL, "http")
	gui, _, err := websocket.DefaultDialer.Dial(guiURL, nil)
	if err != nil {
		t.Fatalf("dial GUI: %v", err)
	}
	defer gui.Close()

	// Act: the connect snapshots carry durable host work.
	hostSnap := readFrame(t, bufio.NewReader(host)).GetSnapshot()
	guiSnap := readWSFrame(t, gui).GetSnapshot()

	// Assert: only the UDS host sees either host-only snapshot collection.
	if len(hostSnap.GetWorkspaceAvailable()) != 1 || len(hostSnap.GetHostActions()) != 1 {
		t.Fatalf("host snapshot lost durable work: %+v", hostSnap)
	}
	if len(guiSnap.GetWorkspaceAvailable()) != 0 || len(guiSnap.GetHostActions()) != 0 {
		t.Fatalf("GUI snapshot leaked host work: %+v", guiSnap)
	}

	// Act / Assert: push fanout applies the same capability filter.
	s.PushWorkspaceAvailable(&frontendv1.WorkspaceAvailable{JobId: "job-2"})
	if got := readFrame(t, bufio.NewReader(host)); got.GetWorkspaceAvailable().GetJobId() != "job-2" {
		t.Fatalf("host live frame = %+v, want workspaceAvailable job-2", got)
	}
	if err := gui.SetReadDeadline(time.Now().Add(100 * time.Millisecond)); err != nil {
		t.Fatalf("set GUI deadline: %v", err)
	}
	if _, _, err := gui.ReadMessage(); err == nil {
		t.Fatal("GUI received host-only workspaceAvailable frame")
	}
}

func TestGUIRejectsHostOnlyCommand(t *testing.T) {
	// Arrange.
	s, h := newTestServer(t, 0)
	defer s.Close()
	httpSrv := httptest.NewServer(http.HandlerFunc(s.ServeWS))
	defer httpSrv.Close()
	guiURL := "ws" + strings.TrimPrefix(httpSrv.URL, "http")
	gui, _, err := websocket.DefaultDialer.Dial(guiURL, nil)
	if err != nil {
		t.Fatalf("dial GUI: %v", err)
	}
	defer gui.Close()
	_ = readWSFrame(t, gui) // connect snapshot

	// Act.
	data, err := protojson.Marshal(&frontendv1.FrontendCommand{
		RequestId: "materialized-from-gui",
		Command:   &frontendv1.FrontendCommand_WorkspaceMaterialized{WorkspaceMaterialized: &frontendv1.WorkspaceMaterializedCmd{JobId: "job-nope"}},
	})
	if err != nil {
		t.Fatalf("marshal materialized: %v", err)
	}
	if err := gui.WriteMessage(websocket.TextMessage, data); err != nil {
		t.Fatalf("write materialized: %v", err)
	}
	ack := readWSFrame(t, gui).GetCommandAck()

	// Assert.
	if ack.GetOk() || !strings.Contains(ack.GetError(), "host-only command") {
		t.Fatalf("GUI host-only ack = %+v, want loud host-only refusal", ack)
	}
	if h.called != "" {
		t.Fatalf("GUI host-only command reached handler %q", h.called)
	}
}

// TestServeWSScopedCommandStrictRoutesCommand proves the per-session /stream
// surface (a SCOPED connection) accepts an inbound FrontendCommand protojson
// frame with a nil translator (command-strict) and routes it through the SAME
// shared handler, delivering the ack — so the webapp can go command-strict.
func TestServeWSScopedCommandStrictRoutesCommand(t *testing.T) {
	// Arrange.
	s, h := newTestServer(t, 0)
	defer s.Close()
	httpSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// nil translator = command-strict.
		s.ServeWSScoped(w, r, Scope{Workspace: "ws"}, ClientKindGUIObserver, nil)
	}))
	defer httpSrv.Close()

	wsURL := "ws" + strings.TrimPrefix(httpSrv.URL, "http")
	conn, _, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	defer conn.Close()

	// Act: snapshot first, then a FrontendCommand protojson frame.
	if snap := readWSFrame(t, conn); snap.GetSnapshot() == nil {
		t.Fatalf("first WS frame was not a snapshot: %v", snap)
	}
	cmdData, err := protojson.Marshal(&frontendv1.FrontendCommand{
		Workspace: "ws", RequestId: "cscoped",
		Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{Text: "hi"}},
	})
	if err != nil {
		t.Fatalf("marshal cmd: %v", err)
	}
	if err := conn.WriteMessage(websocket.TextMessage, cmdData); err != nil {
		t.Fatalf("ws write: %v", err)
	}
	ackFrame := readWSFrame(t, conn)

	// Assert.
	if ackFrame.GetCommandAck().GetRequestId() != "cscoped" {
		t.Errorf("ack request_id = %q, want cscoped", ackFrame.GetCommandAck().GetRequestId())
	}
	if h.called != "submit_prompt" {
		t.Errorf("handler called %q, want submit_prompt", h.called)
	}
}

// --- Broadcast reaches a connected client -----------------------------------

func TestBroadcastReachesClient(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	l, err := net.Listen("unix", shortSock(t, "b.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	go func() { _ = s.Serve(l) }()
	defer s.Close()
	conn, err := net.Dial("unix", l.Addr().String())
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	defer conn.Close()
	r := bufio.NewReader(conn)
	_ = readFrame(t, r) // consume the snapshot

	// Act: broadcast a workspace-state delta.
	// (Poll clientCount so the broadcast lands after registration completed;
	// registration happens synchronously in the accept goroutine.)
	waitClientCount(t, s, 1)
	s.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "w1", State: frontendv1.RenderState_RENDER_STATE_THINKING})

	// Assert.
	got := readFrame(t, r)
	if got.GetWorkspaceState().GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Errorf("broadcast frame = %v, want a THINKING workspace_state", got)
	}
}

func TestPushWorkspaceAvailableReportsZeroWhenOnlyGUIClientsAreConnected(t *testing.T) {
	// Arrange: a GUI stream is barred from host-only frames, so a fan-out of
	// GUI clients alone is a fan-out to nobody. This is the exact shape a
	// daemon that bounced out from under a running Emacs is left in.
	s, _ := newTestServer(t, 8)
	gui := newClient(s.bufSize, nil, ClientKindGUIStream)
	s.mu.Lock()
	s.clients[gui] = struct{}{}
	s.mu.Unlock()

	// Act.
	delivered := s.PushWorkspaceAvailable(&frontendv1.WorkspaceAvailable{JobId: "job-1", FinalName: "fresh"})

	// Assert.
	if delivered != 0 {
		t.Fatalf("delivered = %d, want 0 (a GUI stream never receives host-only work)", delivered)
	}
}

func TestPushWorkspaceAvailableReportsTheHostClientsItReached(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 8)
	host := newClient(s.bufSize, nil, ClientKindHost)
	gui := newClient(s.bufSize, nil, ClientKindGUIStream)
	s.mu.Lock()
	s.clients[host] = struct{}{}
	s.clients[gui] = struct{}{}
	s.mu.Unlock()

	// Act.
	delivered := s.PushWorkspaceAvailable(&frontendv1.WorkspaceAvailable{JobId: "job-1", FinalName: "fresh"})

	// Assert: the host counts, the GUI stream does not.
	if delivered != 1 {
		t.Fatalf("delivered = %d, want 1", delivered)
	}
}

// --- Slow-consumer hard disconnect (white-box, deterministic) ---------------

func TestSlowConsumerHardDisconnect(t *testing.T) {
	// Arrange: a registered client whose writer never drains, buffer size 2.
	s, _ := newTestServer(t, 2)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	s.mu.Lock()
	s.clients[cl] = struct{}{}
	s.mu.Unlock()

	// Act 1: fill the buffer exactly — still connected.
	s.enqueue(cl, outFrame{data: []byte("a")})
	s.enqueue(cl, outFrame{data: []byte("b")})
	// Assert 1.
	if s.clientCount() != 1 {
		t.Fatalf("client should remain connected with a full-but-not-overflowed buffer, count=%d", s.clientCount())
	}

	// Act 2: one more overflows the bounded buffer.
	s.enqueue(cl, outFrame{data: []byte("c")})

	// Assert 2: hard disconnect.
	if s.clientCount() != 0 {
		t.Fatalf("slow consumer should be disconnected, count=%d", s.clientCount())
	}
	select {
	case <-cl.done:
	default:
		t.Fatal("client done channel was not closed on disconnect")
	}
}

func TestSlowConsumerDisconnectReportsTheCompactionItAttempted(t *testing.T) {
	// Arrange: a client whose queue is full of frames nothing may replace.
	var logs []string
	s := New(Config{
		Logf:        func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) },
		LogVerbosef: testLogf(t),
		State:       staticState{snap: sampleSnapshot()},
		Handler:     &mockHandler{},
		BufSize:     2,
	})
	cl := newClient(s.bufSize, nil, ClientKindHost)
	s.clients[cl] = struct{}{}
	s.enqueue(cl, outFrame{data: []byte("a")})
	s.enqueue(cl, outFrame{data: []byte("b")})

	// Act.
	s.enqueue(cl, outFrame{data: []byte("c")})

	// Assert: the disconnect line says compaction ran and freed nothing.
	if len(logs) == 0 || !strings.Contains(logs[0], "after compacting 0 superseded frames") {
		t.Fatalf("disconnect logs = %v, want the compaction attempt reported", logs)
	}
}

// --- Slow-consumer compaction -----------------------------------------------

func TestDeliverCompactsSupersededWorkspaceStatesForASlowConsumer(t *testing.T) {
	// Arrange: a registered client whose writer never drains, buffer size 3,
	// already holding one irreplaceable frame.
	s, _ := newTestServer(t, 3)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	s.mu.Lock()
	s.clients[cl] = struct{}{}
	s.mu.Unlock()
	s.enqueue(cl, outFrame{data: []byte("irreplaceable")})

	// Act: far more revisions of one workspace's state than the queue holds.
	for _, at := range []int64{1, 2, 3, 4, 5} {
		s.PushWorkspaceState(&frontendv1.WorkspaceState{
			Workspace: "w1", SessionId: "s1", AtMs: at,
			State: frontendv1.RenderState_RENDER_STATE_THINKING,
		})
	}

	// Assert: the connection survived, the irreplaceable frame is still first,
	// the newest revision is queued and no stale revision is.
	if s.clientCount() != 1 {
		t.Fatalf("client count = %d, want the slow consumer kept via compaction", s.clientCount())
	}
	if got := string(mustPop(t, cl)); got != "irreplaceable" {
		t.Fatalf("first queued frame = %q, want the irreplaceable one preserved", got)
	}
	var revisions []int64
	for {
		data, ok := cl.out.pop()
		if !ok {
			break
		}
		frame := &frontendv1.FrontendFrame{}
		if err := protojson.Unmarshal(data, frame); err != nil {
			t.Fatalf("decode compacted state: %v", err)
		}
		revisions = append(revisions, frame.GetWorkspaceState().GetAtMs())
	}
	if len(revisions) == 0 || revisions[len(revisions)-1] != 5 {
		t.Fatalf("queued revisions = %v, want the newest revision 5 last", revisions)
	}
	for _, at := range revisions {
		if at < 4 {
			t.Fatalf("queued revisions = %v, want every superseded revision compacted away", revisions)
		}
	}
}

func TestDeliverNeverCompactsConversationDeltasForASlowConsumer(t *testing.T) {
	// Arrange: a client whose buffer holds exactly two conversation deltas.
	s, _ := newTestServer(t, 2)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	s.mu.Lock()
	s.clients[cl] = struct{}{}
	s.mu.Unlock()
	s.PushConversationDelta(&frontendv1.ConversationDelta{Workspace: "w1", SessionId: "s1", ThroughSeq: 1})
	s.PushConversationDelta(&frontendv1.ConversationDelta{Workspace: "w1", SessionId: "s1", ThroughSeq: 2})

	// Act: a third append-semantic frame with no room and nothing to compact.
	s.PushConversationDelta(&frontendv1.ConversationDelta{Workspace: "w1", SessionId: "s1", ThroughSeq: 3})

	// Assert: the disconnect path still fires rather than dropping content.
	if s.clientCount() != 0 {
		t.Fatalf("client count = %d, want the hard disconnect to still fire", s.clientCount())
	}
	select {
	case <-cl.done:
	default:
		t.Fatal("client done channel was not closed on disconnect")
	}
}

func TestDeliverKeepsEveryFrameForAFastConsumer(t *testing.T) {
	// Arrange: a client with headroom for every frame.
	s, _ := newTestServer(t, 8)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	s.mu.Lock()
	s.clients[cl] = struct{}{}
	s.mu.Unlock()

	// Act: three revisions of the same workspace state.
	for _, at := range []int64{1, 2, 3} {
		s.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "w1", SessionId: "s1", AtMs: at})
	}

	// Assert: nothing was coalesced — a consumer keeping up sees every frame.
	var revisions []int64
	for {
		data, ok := cl.out.pop()
		if !ok {
			break
		}
		frame := &frontendv1.FrontendFrame{}
		if err := protojson.Unmarshal(data, frame); err != nil {
			t.Fatalf("decode state: %v", err)
		}
		revisions = append(revisions, frame.GetWorkspaceState().GetAtMs())
	}
	if len(revisions) != 3 || revisions[0] != 1 || revisions[1] != 2 || revisions[2] != 3 {
		t.Fatalf("delivered revisions = %v, want every revision in order", revisions)
	}
}

// --- protojson round-trip stability -----------------------------------------

func TestProtojsonRoundTripStability(t *testing.T) {
	tests := []struct {
		name  string
		frame *frontendv1.FrontendFrame
	}{
		{"snapshot", SnapshotFrame(sampleSnapshot())},
		{"workspace_state", WorkspaceStateFrame(&frontendv1.WorkspaceState{Workspace: "w", State: frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT, CauseSeq: 12})},
		{"session_view", SessionViewFrame(&frontendv1.SessionView{Workspace: "w", Model: "m", TotalTokens: 9, TotalCostUsd: 1.5})},
		{"conversation_delta", ConversationDeltaFrame(&frontendv1.ConversationDelta{
			Workspace: "w", SessionId: "s", ThroughSeq: 4,
			Items: []*frontendv1.ConversationItem{{
				Uuid: "u1", TsMs: 5,
				Item: &frontendv1.ConversationItem_AssistantMessage{AssistantMessage: &datav1.ApiAssistantMessage{
					Content: []*datav1.ContentBlock{
						{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hi"}}},
					},
				}},
			}},
		})},
		{"typing_delta", TypingDeltaFrame(&frontendv1.TypingDelta{
			Workspace: "w",
			Delta:     &corev1.ContentDelta{Uuid: "u", BlockIndex: 0, Delta: &corev1.ContentDelta_Text{Text: "ab"}},
		})},
		{"session_init", SessionInitViewFrame(&frontendv1.SessionInitView{
			Workspace: "w", SessionId: "s",
			Init: &datav1.SystemInit{Model: "claude-x"},
		})},
		{"task_catalog", TaskCatalogFrame(&frontendv1.TaskCatalog{Workspace: "w", Tasks: []*frontendv1.TaskEntry{{TaskId: "t", Kind: "agent", Status: "running"}}})},
		{"command_ack", CommandAckFrame(&frontendv1.CommandAck{RequestId: "r", Ok: true})},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act: marshal then unmarshal.
			data, err := marshalFrame(tc.frame)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			out := &frontendv1.FrontendFrame{}
			if err := protojson.Unmarshal(data, out); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			// Assert: unmarshal(marshal(x)) == x.
			if !proto.Equal(tc.frame, out) {
				t.Errorf("round-trip mismatch\n in: %v\nout: %v", tc.frame, out)
			}
		})
	}
}

// --- New: required-dependency guards ----------------------------------------

func TestNewPanicsOnMissingDeps(t *testing.T) {
	cases := []struct {
		name string
		cfg  Config
	}{
		{"no logf", Config{LogVerbosef: func(string, ...any) {}, State: staticState{}, Handler: &mockHandler{}}},
		{"no verbose logf", Config{Logf: func(string, ...any) {}, State: staticState{}, Handler: &mockHandler{}}},
		{"no state", Config{Logf: func(string, ...any) {}, LogVerbosef: func(string, ...any) {}, Handler: &mockHandler{}}},
		{"no handler", Config{Logf: func(string, ...any) {}, LogVerbosef: func(string, ...any) {}, State: staticState{}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			defer func() {
				if r := recover(); r == nil {
					t.Errorf("expected panic for %s", tc.name)
				}
			}()
			New(tc.cfg)
		})
	}
}

// --- helpers ----------------------------------------------------------------

func readFrame(t *testing.T, r *bufio.Reader) *frontendv1.FrontendFrame {
	t.Helper()
	line, err := r.ReadBytes('\n')
	if err != nil {
		t.Fatalf("read frame: %v", err)
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(line, frame); err != nil {
		t.Fatalf("unmarshal frame: %v", err)
	}
	return frame
}

func readWSFrame(t *testing.T, conn *websocket.Conn) *frontendv1.FrontendFrame {
	t.Helper()
	_, data, err := conn.ReadMessage()
	if err != nil {
		t.Fatalf("ws read: %v", err)
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(data, frame); err != nil {
		t.Fatalf("unmarshal ws frame: %v", err)
	}
	return frame
}

func writeCmd(t *testing.T, conn net.Conn, cmd *frontendv1.FrontendCommand) {
	t.Helper()
	data, err := protojson.Marshal(cmd)
	if err != nil {
		t.Fatalf("marshal cmd: %v", err)
	}
	if _, err := conn.Write(append(data, '\n')); err != nil {
		t.Fatalf("write cmd: %v", err)
	}
}

// retryDialUnix busy-polls until the socket is connectable (no sleep-based
// synchronization) or the attempt budget is exhausted.
func retryDialUnix(t *testing.T, path string) net.Conn {
	t.Helper()
	for i := 0; i < 100000; i++ {
		if conn, err := net.Dial("unix", path); err == nil {
			return conn
		}
		runtime.Gosched()
	}
	t.Fatalf("could not connect to %s", path)
	return nil
}

// waitClientCount busy-polls until the server reports the expected client count.
func waitClientCount(t *testing.T, s *Server, want int) {
	t.Helper()
	for i := 0; i < 100000; i++ {
		if s.clientCount() == want {
			return
		}
		runtime.Gosched()
	}
	t.Fatalf("client count never reached %d (last=%d)", want, s.clientCount())
}

// --- QueueClassification wire encoding ---------------------------------------

// TestQueueClassificationWireNames pins the protojson encoding of every
// QueueClassification the daemon sends. PENDING is a REAL verdict ("the
// classifier is running"), so it must appear on the wire; before
// QUEUE_CLASSIFICATION_UNSPECIFIED took number 0, PENDING WAS 0 and protojson
// omitted it, making "classifier running" indistinguishable from "field never
// populated". The webapp decoder rejects an absent classification precisely
// because that ambiguity is now gone.
func TestQueueClassificationWireNames(t *testing.T) {
	tests := []struct {
		name string
		cls  frontendv1.QueueClassification
		want string
	}{
		{"pending", frontendv1.QueueClassification_QUEUE_CLASSIFICATION_PENDING, "QUEUE_CLASSIFICATION_PENDING"},
		{"interject", frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT, "QUEUE_CLASSIFICATION_INTERJECT"},
		{"hold", frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD, "QUEUE_CLASSIFICATION_HOLD"},
		{"error", frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR, "QUEUE_CLASSIFICATION_ERROR"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			frame := QueueViewFrame(&frontendv1.QueueView{
				Workspace: "w", SessionId: "s",
				Entries: []*frontendv1.QueueEntry{{Id: "q1", Classification: tc.cls}},
			})
			// Act
			data, err := marshalFrame(frame)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			// Assert
			if !strings.Contains(string(data), tc.want) {
				t.Fatalf("wire form %s does not carry %q", data, tc.want)
			}
			out := &frontendv1.FrontendFrame{}
			if err := protojson.Unmarshal(data, out); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			if !proto.Equal(frame, out) {
				t.Errorf("round-trip mismatch\n in: %v\nout: %v", frame, out)
			}
		})
	}
}

// levelSplitServer builds a Server whose info and warn channels are separate
// sinks, so a test can assert WHICH channel a record took. A refused command
// indexed at info sits beside the per-command chatter and is invisible to a
// level filter.
func levelSplitServer(t *testing.T, info, warn *[]string) *Server {
	t.Helper()
	return New(Config{
		Logf:        func(format string, args ...any) { *info = append(*info, fmt.Sprintf(format, args...)) },
		Warnf:       func(format string, args ...any) { *warn = append(*warn, fmt.Sprintf(format, args...)) },
		LogVerbosef: testLogf(t),
		State:       staticState{snap: sampleSnapshot()},
		Handler:     &mockHandler{},
	})
}

func hostOnlyCommandFromGUI() (*client, *frontendv1.FrontendCommand) {
	cl := &client{id: 1, out: newOutbox(4), done: make(chan struct{}), kind: ClientKindGUIStream}
	cmd := &frontendv1.FrontendCommand{
		RequestId: "materialized-from-gui",
		Command:   &frontendv1.FrontendCommand_WorkspaceMaterialized{WorkspaceMaterialized: &frontendv1.WorkspaceMaterializedCmd{JobId: "job-nope"}},
	}
	return cl, cmd
}

func TestHostOnlyCommandRefusalTakesTheWarnChannel(t *testing.T) {
	// Arrange.
	var info, warn []string
	s := levelSplitServer(t, &info, &warn)
	cl, cmd := hostOnlyCommandFromGUI()

	// Act.
	s.dispatchClientCommand(cl, cmd)

	// Assert.
	if !strings.Contains(strings.Join(warn, "\n"), "host-only command rejected") {
		t.Fatalf("warn = %v, want the refusal at warn", warn)
	}
}

func TestHostOnlyCommandRefusalLeavesNothingOnTheInfoChannel(t *testing.T) {
	// Arrange.
	var info, warn []string
	s := levelSplitServer(t, &info, &warn)
	cl, cmd := hostOnlyCommandFromGUI()

	// Act.
	s.dispatchClientCommand(cl, cmd)

	// Assert.
	if strings.Contains(strings.Join(info, "\n"), "host-only command rejected kind=") {
		t.Fatalf("info = %v, want the refusal off the info channel entirely", info)
	}
}

func TestHostOnlyCommandRefusalStillRecordsWithNoWarnChannelWired(t *testing.T) {
	// Arrange -- an unwired warn channel must lose the SEVERITY, never the
	// record.
	var info []string
	s := New(Config{
		Logf:        func(format string, args ...any) { info = append(info, fmt.Sprintf(format, args...)) },
		LogVerbosef: testLogf(t),
		State:       staticState{snap: sampleSnapshot()},
		Handler:     &mockHandler{},
	})
	cl, cmd := hostOnlyCommandFromGUI()

	// Act.
	s.dispatchClientCommand(cl, cmd)

	// Assert.
	if !strings.Contains(strings.Join(info, "\n"), "host-only command rejected kind=") {
		t.Fatalf("info = %v, want the refusal still recorded through Logf", info)
	}
}

// TestQueueClassificationUnspecifiedIsOmittedOnTheWire pins the fact the
// webapp's strict decoder rests on: UNSPECIFIED is the proto3 zero, so
// protojson drops the field entirely. Absent and UNSPECIFIED are therefore the
// SAME wire fact, and the decoder is right to reject both identically. The
// daemon never produces this frame; the test exists so the equivalence cannot
// silently stop holding.
func TestQueueClassificationUnspecifiedIsOmittedOnTheWire(t *testing.T) {
	// Arrange
	frame := QueueViewFrame(&frontendv1.QueueView{
		Workspace: "w", SessionId: "s",
		Entries: []*frontendv1.QueueEntry{{
			Id:             "q1",
			Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_UNSPECIFIED,
		}},
	})
	// Act
	data, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	// Assert
	if strings.Contains(string(data), "classification") {
		t.Fatalf("UNSPECIFIED must be omitted by protojson, but the wire form carries it: %s", data)
	}
}
