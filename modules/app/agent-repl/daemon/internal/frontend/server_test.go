package frontend

import (
	"bufio"
	"errors"
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
	f, ok := cl.out.pop()
	if !ok {
		t.Fatalf("client %d outbox is empty, want a queued frame", cl.id)
	}
	return f.data
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

// TestSlowConsumerHardDisconnect covers the NON-HOST policy: a flat bound with
// no elastic region, so the first frame past it severs the connection. The host
// is deliberately excluded — it has its own elastic policy, covered separately.
func TestSlowConsumerHardDisconnect(t *testing.T) {
	// Arrange: a registered client whose writer never drains, buffer size 2.
	s, _ := newTestServer(t, 2)
	cl := newClient(s.bufSize, nil, ClientKindGUIStream)
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
	cl := newClient(s.bufSize, nil, ClientKindGUIStream)
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
		f, ok := cl.out.pop()
		if !ok {
			break
		}
		frame := &frontendv1.FrontendFrame{}
		if err := protojson.Unmarshal(f.data, frame); err != nil {
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
	cl := newClient(s.bufSize, nil, ClientKindGUIStream)
	s.mu.Lock()
	s.clients[cl] = struct{}{}
	s.mu.Unlock()
	s.PushConversationDelta(&frontendv1.ConversationDelta{Workspace: "w1", Fence: "s1", ThroughSeq: 1})
	s.PushConversationDelta(&frontendv1.ConversationDelta{Workspace: "w1", Fence: "s1", ThroughSeq: 2})

	// Act: a third append-semantic frame with no room and nothing to compact.
	s.PushConversationDelta(&frontendv1.ConversationDelta{Workspace: "w1", Fence: "s1", ThroughSeq: 3})

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
		f, ok := cl.out.pop()
		if !ok {
			break
		}
		frame := &frontendv1.FrontendFrame{}
		if err := protojson.Unmarshal(f.data, frame); err != nil {
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
			Workspace: "w", Fence: "s", ThroughSeq: 4,
			Items: []*frontendv1.ConversationItem{{
				Uuid: "u1", TsMs: 5,
				Item: &frontendv1.ConversationItem_Agent{Agent: &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_Response{Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{
					Content: []*datav1.ContentBlock{
						{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hi"}}},
					},
				}}}}},
			}},
		})},
		{"typing_delta", TypingDeltaFrame(&frontendv1.TypingDelta{
			Workspace: "w",
			Delta:     &corev1.ContentDelta{Uuid: "u", BlockIndex: 0, Delta: &corev1.ContentDelta_Text{Text: "ab"}},
		})},
		{"session_init", SessionInitViewFrame(&frontendv1.SessionInitView{
			Workspace: "w", Fence: "s",
			Init: &datav1.SystemInit{Model: "claude-x"},
		})},
		{"task_catalog", TaskCatalogFrame(&frontendv1.TaskCatalog{Workspace: "w", Tasks: []*frontendv1.TaskEntry{{TaskId: "t", Kind: &frontendv1.TaskEntry_Agent{Agent: &frontendv1.TaskKindAgent{}}, Status: &frontendv1.TaskEntry_Running{Running: &frontendv1.TaskStatusRunning{}}}}})},
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
		name  string
		entry *frontendv1.QueueEntry
		want  string
	}{
		{"pending", &frontendv1.QueueEntry{Id: "q1", Classification: &frontendv1.QueueEntry_Pending{
			Pending: &frontendv1.QueueClassificationPending{}}}, "pending"},
		{"interject", &frontendv1.QueueEntry{Id: "q1", Classification: &frontendv1.QueueEntry_Interject{
			Interject: &frontendv1.QueueClassificationInterject{}}}, "interject"},
		{"hold", &frontendv1.QueueEntry{Id: "q1", Classification: &frontendv1.QueueEntry_HoldForTurnEnd{
			HoldForTurnEnd: &frontendv1.QueueClassificationHold{}}}, "holdForTurnEnd"},
		{"error", &frontendv1.QueueEntry{Id: "q1", Classification: &frontendv1.QueueEntry_Error{
			Error: &frontendv1.QueueClassificationError{}}}, "error"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			frame := QueueViewFrame(&frontendv1.QueueView{
				Workspace: "w", Fence: "s",
				Entries: []*frontendv1.QueueEntry{tc.entry},
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

// TestAnUnclassifiedEntryCarriesNoVerdictOnTheWire pins the fact the webapp's
// strict decoder rests on: an entry with no classification arm serializes
// NONE of the four verdict names, so "nothing decided this" is detectable
// rather than indistinguishable from a real verdict. It is the guarantee the
// oneof replaced the enum's UNSPECIFIED-vs-absent equivalence with. The daemon
// never produces this frame; the test exists so the property cannot silently
// stop holding.
func TestAnUnclassifiedEntryCarriesNoVerdictOnTheWire(t *testing.T) {
	// Arrange
	frame := QueueViewFrame(&frontendv1.QueueView{
		Workspace: "w", Fence: "s",
		Entries: []*frontendv1.QueueEntry{{Id: "q1"}},
	})
	// Act
	data, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	// Assert
	for _, verdict := range []string{"pending", "interject", "holdForTurnEnd", `"error"`} {
		if strings.Contains(string(data), verdict) {
			t.Fatalf("an entry with no classification arm serialized %s: %s", verdict, data)
		}
	}
}

// --- Host elasticity ---------------------------------------------------------

// newSeveritySplitServer builds a server whose WARN and INFO channels are
// captured separately, so a record's severity is assertable.
func newSeveritySplitServer(t *testing.T, buf int) (*Server, *[]string, *[]string) {
	t.Helper()
	var info, warn []string
	s := New(Config{
		Logf:        func(format string, args ...any) { info = append(info, fmt.Sprintf(format, args...)) },
		Warnf:       func(format string, args ...any) { warn = append(warn, fmt.Sprintf(format, args...)) },
		LogVerbosef: testLogf(t),
		State:       staticState{snap: sampleSnapshot()},
		Handler:     &mockHandler{},
		BufSize:     buf,
	})
	return s, &info, &warn
}

// registerClient adds cl to the fan-out set (test aid).
func registerClient(s *Server, cl *client) {
	s.mu.Lock()
	s.clients[cl] = struct{}{}
	s.mu.Unlock()
}

// TestClientOutboxPolicyFollowsKind pins WHICH connections get headroom: the
// host alone, decided at accept from its kind and never reassigned.
func TestClientOutboxPolicyFollowsKind(t *testing.T) {
	tests := []struct {
		name     string
		kind     ClientKind
		wantHard int
	}{
		{name: "host absorbs a blocked event loop", kind: ClientKindHost, wantHard: 8 * hostBufferElasticity},
		{name: "gui stream keeps the flat bound", kind: ClientKindGUIStream, wantHard: 8},
		{name: "gui bootstrap keeps the flat bound", kind: ClientKindGUIBootstrap, wantHard: 8},
		{name: "gui observer keeps the flat bound", kind: ClientKindGUIObserver, wantHard: 8},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			cl := newClient(8, nil, tc.kind)

			// Assert.
			if cl.out.capacity() != 8 {
				t.Fatalf("soft bound = %d, want 8 for every kind", cl.out.capacity())
			}
			if cl.out.ceiling() != tc.wantHard {
				t.Fatalf("hard ceiling = %d, want %d", cl.out.ceiling(), tc.wantHard)
			}
		})
	}
}

// TestHostSurvivesTransientBusynessWithoutEviction is the live regression: an
// Emacs host that blocks its event loop long enough to overrun the soft bound,
// then drains, must keep its connection.
func TestHostSurvivesTransientBusynessWithoutEviction(t *testing.T) {
	// Arrange: a host whose soft bound is 4 and whose ceiling is therefore 64.
	s, _, warn := newSeveritySplitServer(t, 4)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	registerClient(s, cl)

	// Act: far more irreplaceable frames than the soft bound while Emacs is
	// blocked, then Emacs wakes up and drains everything.
	for i := 0; i < 40; i++ {
		s.enqueue(cl, outFrame{data: []byte(fmt.Sprintf("conv%d", i))})
	}
	drained := len(drain(cl.out))

	// Assert.
	if s.clientCount() != 1 {
		t.Fatalf("client count = %d, want the busy host still connected", s.clientCount())
	}
	if drained != 40 {
		t.Fatalf("drained = %d, want all 40 frames absorbed", drained)
	}
	if len(*warn) != 0 {
		t.Fatalf("warn log = %v, want no degradation record for transient busyness", *warn)
	}
}

// TestWedgedHostIsStillEvicted keeps the load-shedding coverage: a host that
// never reads must still be cut loose so the daemon cannot queue forever.
func TestWedgedHostIsStillEvicted(t *testing.T) {
	// Arrange: a host that never drains a single frame.
	s, _, _ := newSeveritySplitServer(t, 2)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	registerClient(s, cl)

	// Act: push past the ceiling with nothing compactible.
	for i := 0; i <= s.bufSize*hostBufferElasticity; i++ {
		s.enqueue(cl, outFrame{data: []byte(fmt.Sprintf("conv%d", i))})
	}

	// Assert.
	if s.clientCount() != 0 {
		t.Fatalf("client count = %d, want the wedged host disconnected", s.clientCount())
	}
	select {
	case <-cl.done:
	default:
		t.Fatal("client done channel was not closed on eviction")
	}
}

// TestHostEvictionRecordsAtWarnWithKindAndBufferStats covers the severity
// asymmetry: Emacs warns about its own link going down, so the daemon's record
// of the same event is a warning too, and it names what it gave up on.
func TestHostEvictionRecordsAtWarnWithKindAndBufferStats(t *testing.T) {
	// Arrange.
	s, _, warn := newSeveritySplitServer(t, 2)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	registerClient(s, cl)

	// Act.
	for i := 0; i <= s.bufSize*hostBufferElasticity; i++ {
		s.enqueue(cl, outFrame{data: []byte(fmt.Sprintf("conv%d", i))})
	}

	// Assert: exactly one warn record, naming kind, bounds and reason.
	if len(*warn) != 1 {
		t.Fatalf("warn log = %v, want exactly one host eviction record", *warn)
	}
	for _, want := range []string{"(host)", "soft 2", "hard 32", "after compacting 0 superseded frames", "reason=hard_ceiling"} {
		if !strings.Contains((*warn)[0], want) {
			t.Fatalf("warn record = %q, want it to name %q", (*warn)[0], want)
		}
	}
}

// TestNonHostEvictionStaysAtInfo pins the other half of that asymmetry: shedding
// a backgrounded webview is the contract working, not a degradation.
func TestNonHostEvictionStaysAtInfo(t *testing.T) {
	// Arrange.
	s, info, warn := newSeveritySplitServer(t, 2)
	cl := newClient(s.bufSize, nil, ClientKindGUIStream)
	registerClient(s, cl)

	// Act.
	for i := 0; i < 3; i++ {
		s.enqueue(cl, outFrame{data: []byte(fmt.Sprintf("conv%d", i))})
	}

	// Assert.
	if len(*warn) != 0 {
		t.Fatalf("warn log = %v, want a GUI eviction to stay off the warn channel", *warn)
	}
	if len(*info) == 0 || !strings.Contains((*info)[0], "(gui_stream)") {
		t.Fatalf("info log = %v, want the eviction recorded at info naming the kind", *info)
	}
}

// TestHostStalledEvictionRecordsItsStallDuration covers the second eviction
// reason: a host under its ceiling that has drained nothing for the whole grace
// period is wedged, and the record says so.
func TestHostStalledEvictionRecordsItsStallDuration(t *testing.T) {
	// Arrange: a host over its soft bound with a manually advanced clock.
	s, _, warn := newSeveritySplitServer(t, 2)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	clock := newFakeClock()
	cl.out.now = clock.Now
	registerClient(s, cl)
	s.enqueue(cl, outFrame{data: []byte("a")})
	s.enqueue(cl, outFrame{data: []byte("b")})
	s.enqueue(cl, outFrame{data: []byte("c")})

	// Act.
	clock.advance(hostStallGrace + time.Second)
	s.enqueue(cl, outFrame{data: []byte("d")})

	// Assert.
	if s.clientCount() != 0 {
		t.Fatalf("client count = %d, want the stalled host disconnected", s.clientCount())
	}
	if len(*warn) != 1 || !strings.Contains((*warn)[0], "reason=stalled") {
		t.Fatalf("warn log = %v, want a stalled-reason eviction record", *warn)
	}
	if !strings.Contains((*warn)[0], "stalled_for_ms=31000") {
		t.Fatalf("warn record = %q, want the stall duration reported", (*warn)[0])
	}
}

// --- the session-publication latch's frame coverage -------------------------
//
// frameSessionIdentity decides which frames the materialization latch may hold
// back. The SNAPSHOT side already latches the three resolved views, so a family
// missing from that switch is a place where the push and the snapshot disagree
// about whether a workspace exists yet. One test per family, because each is its
// own arm and its own way to be forgotten.

func TestFrameSessionIdentityScopesTheTopbarToItsWorkspace(t *testing.T) {
	// Arrange
	frame := TopbarViewFrame(&frontendv1.TopbarView{Workspace: "/ws"})

	// Act
	workspace, sessionID, scoped := frameSessionIdentity(frame)

	// Assert
	if !scoped || workspace != "/ws" || sessionID != "" {
		t.Fatalf("frameSessionIdentity(topbar) = (%q, %q, %t), want (\"/ws\", \"\", true): the snapshot latches topbars, so an unlatched push tells a client about a workspace the snapshot then refuses to name",
			workspace, sessionID, scoped)
	}
}

func TestFrameSessionIdentityScopesTheTokenBreakdownToItsWorkspace(t *testing.T) {
	// Arrange
	frame := TokenBreakdownViewFrame(&frontendv1.TokenBreakdownView{Workspace: "/ws"})

	// Act
	workspace, sessionID, scoped := frameSessionIdentity(frame)

	// Assert
	if !scoped || workspace != "/ws" || sessionID != "" {
		t.Fatalf("frameSessionIdentity(token_breakdown) = (%q, %q, %t), want (\"/ws\", \"\", true)",
			workspace, sessionID, scoped)
	}
}

func TestFrameSessionIdentityScopesTheWorkspaceGateToItsWorkspace(t *testing.T) {
	// Arrange
	frame := WorkspaceGateViewFrame(&frontendv1.WorkspaceGateView{Workspace: "/ws"})

	// Act
	workspace, sessionID, scoped := frameSessionIdentity(frame)

	// Assert
	if !scoped || workspace != "/ws" || sessionID != "" {
		t.Fatalf("frameSessionIdentity(workspace_gate) = (%q, %q, %t), want (\"/ws\", \"\", true)",
			workspace, sessionID, scoped)
	}
}

// --- which lane a command's answer takes ------------------------------------

// popFrames drains a client's outbox into decoded frames, oldest first.
func popFrames(t *testing.T, cl *client) []*frontendv1.FrontendFrame {
	t.Helper()
	var frames []*frontendv1.FrontendFrame
	for {
		f, ok := cl.out.pop()
		if !ok {
			return frames
		}
		frame := &frontendv1.FrontendFrame{}
		if err := protojson.Unmarshal(f.data, frame); err != nil {
			t.Fatalf("decode queued frame: %v", err)
		}
		frames = append(frames, frame)
	}
}

func TestCommandAckOvertakesAQueuedBulkBacklog(t *testing.T) {
	// Arrange: the incident's shape — a connect-sized bulk backlog already
	// queued when a command finishes.
	s, _ := newTestServer(t, 16)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	for range 4 {
		s.enqueue(cl, outFrame{data: mustMarshal(t, SnapshotFrame(sampleSnapshot()))})
	}

	// Act.
	s.processCommand(s.newCommandTicket(cl, openCmd("r-1", "/ws/a"), time.Now(), s.inflight.Add(1)))

	// Assert: the ack is the FIRST frame out, so the client's wait is bounded
	// by write throughput rather than by the backlog's depth.
	frames := popFrames(t, cl)
	if len(frames) != 5 {
		t.Fatalf("queued frames = %d, want the backlog plus one ack", len(frames))
	}
	if frames[0].GetCommandAck().GetRequestId() != "r-1" {
		t.Fatalf("first frame = %v, want the ack for r-1", frames[0].GetFrame())
	}
}

func TestResyncAckStillFollowsItsSnapshot(t *testing.T) {
	// Arrange: the ONE ordering pair the control lane must not break. A
	// resync's answer IS the snapshot, so an ack that arrived first would
	// report the client current while it still held the state it asked to
	// replace.
	s, _ := newTestServer(t, 16)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	cmd := &frontendv1.FrontendCommand{
		RequestId: "r-resync", Workspace: "/ws/a",
		Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{}},
	}

	// Act.
	s.processCommand(s.newCommandTicket(cl, cmd, time.Now(), s.inflight.Add(1)))

	// Assert.
	frames := popFrames(t, cl)
	if len(frames) != 2 {
		t.Fatalf("queued frames = %d, want the snapshot and its ack", len(frames))
	}
	if frames[0].GetSnapshot() == nil {
		t.Fatalf("first frame = %v, want the resync snapshot", frames[0].GetFrame())
	}
	if frames[1].GetCommandAck().GetRequestId() != "r-resync" {
		t.Fatalf("second frame = %v, want the resync ack behind its snapshot", frames[1].GetFrame())
	}
}

func TestAResyncAckDoesNotOvertakeTheDeltasQueuedBeforeIt(t *testing.T) {
	// Arrange: a bulk backlog, then a resync. The snapshot and the ack both
	// belong behind that backlog, because a snapshot adopted ahead of the
	// deltas queued before it would have the client apply stale deltas onto
	// fresh state.
	s, _ := newTestServer(t, 16)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	s.enqueue(cl, outFrame{data: mustMarshal(t, ConversationDeltaFrame(&frontendv1.ConversationDelta{Workspace: "/ws/a"}))})
	cmd := &frontendv1.FrontendCommand{
		RequestId: "r-resync", Workspace: "/ws/a",
		Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{}},
	}

	// Act.
	s.processCommand(s.newCommandTicket(cl, cmd, time.Now(), s.inflight.Add(1)))

	// Assert.
	frames := popFrames(t, cl)
	if len(frames) != 3 {
		t.Fatalf("queued frames = %d, want the delta, the snapshot and the ack", len(frames))
	}
	if frames[0].GetConversationDelta() == nil {
		t.Fatalf("first frame = %v, want the delta queued before the resync", frames[0].GetFrame())
	}
	if frames[1].GetSnapshot() == nil || frames[2].GetCommandAck() == nil {
		t.Fatalf("frames = %v/%v, want the snapshot then its ack", frames[1].GetFrame(), frames[2].GetFrame())
	}
}

// mustMarshal encodes a frame the way the delivery path does.
func mustMarshal(t *testing.T, frame *frontendv1.FrontendFrame) []byte {
	t.Helper()
	data, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshal frame: %v", err)
	}
	return data
}

// ---------------------------------------------------------------------------
// The blocked-writer alarm
// ---------------------------------------------------------------------------

// capturedLogf records every line the server logs, so a test can assert on the
// record itself rather than on the fact that something was logged somewhere.
type capturedLogf struct {
	mu    sync.Mutex
	lines []string
	saw   chan string
}

func newCapturedLogf() *capturedLogf {
	return &capturedLogf{saw: make(chan string, 64)}
}

func (c *capturedLogf) logf(format string, args ...any) {
	line := fmt.Sprintf(format, args...)
	c.mu.Lock()
	c.lines = append(c.lines, line)
	c.mu.Unlock()
	select {
	case c.saw <- line:
	default:
	}
}

// awaitLine blocks until a logged line contains want, so a test rendezvouses
// with the alarm rather than guessing when it fired.
func (c *capturedLogf) awaitLine(t *testing.T, want string) string {
	t.Helper()
	deadline := time.After(ticketTestDeadline)
	for {
		select {
		case line := <-c.saw:
			if strings.Contains(line, want) {
				return line
			}
		case <-deadline:
			c.mu.Lock()
			defer c.mu.Unlock()
			t.Fatalf("no logged line contained %q; saw %q", want, c.lines)
			return ""
		}
	}
}

func (c *capturedLogf) contains(want string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, line := range c.lines {
		if strings.Contains(line, want) {
			return true
		}
	}
	return false
}

// newStallServer builds a server whose blocked-writer alarm fires after
// deadline, with every log line captured.
func newStallServer(t *testing.T, log *capturedLogf, deadline time.Duration) *Server {
	t.Helper()
	return New(Config{
		Logf: log.logf, LogVerbosef: log.logf,
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		AckWarnThreshold: time.Hour, AckDeadline: deadline,
	})
}

func TestWriterBlockedInOneWriteAnnouncesItselfWhileItIsBlocked(t *testing.T) {
	// Arrange: a consumer that has stopped reading, which is what Emacs looks
	// like while it blocks its event loop restoring workspaces. The writer
	// parks inside writeFrame and nothing queued behind it can move.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Millisecond)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	c := newGatedConn()
	go s.writeLoop(c, cl)

	// Act.
	s.enqueue(cl, outFrame{data: []byte(`{"bulk":true}`)})
	c.awaitWrite(t)

	// Assert: the fault is named WHILE it is happening, and it names the
	// consumer rather than the commands that were waiting on it.
	line := log.awaitLine(t, "OUTBOUND WRITER BLOCKED")
	if !strings.Contains(line, "client_kind=host") {
		t.Fatalf("blocked record does not name the consumer: %q", line)
	}
	c.release <- nil
	c.close()
}

func TestWriterBlockedRecordNamesTheLaneTheHeldFrameLeftBy(t *testing.T) {
	// Arrange: control-lane priority cannot preempt a write already in
	// progress, so which lane the blocking frame was on is load-bearing.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Millisecond)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	c := newGatedConn()
	go s.writeLoop(c, cl)

	// Act.
	s.enqueue(cl, outFrame{control: true, data: []byte(`{"ack":true}`)})
	c.awaitWrite(t)

	// Assert.
	line := log.awaitLine(t, "OUTBOUND WRITER BLOCKED")
	if !strings.Contains(line, "lane=control") {
		t.Fatalf("blocked record does not name the lane: %q", line)
	}
	c.release <- nil
	c.close()
}

func TestWriterStallReportsItsResolution(t *testing.T) {
	// Arrange: a stall announced with no end is indistinguishable from a
	// daemon that exited still blocked.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Millisecond)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	c := newGatedConn()
	go s.writeLoop(c, cl)
	s.enqueue(cl, outFrame{data: []byte(`{"bulk":true}`)})
	c.awaitWrite(t)
	log.awaitLine(t, "OUTBOUND WRITER BLOCKED")

	// Act: the consumer reads again.
	c.release <- nil

	// Assert.
	line := log.awaitLine(t, "outbound writer UNBLOCKED")
	if !strings.Contains(line, "ok=true") {
		t.Fatalf("resolution does not report the write's outcome: %q", line)
	}
	c.close()
}

func TestWriterStallResolutionReportsAFailedWrite(t *testing.T) {
	// Arrange: a stall that ends in a broken connection must not read as a
	// delivery — the frame never reached the socket.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Millisecond)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	c := newGatedConn()
	go s.writeLoop(c, cl)
	s.enqueue(cl, outFrame{data: []byte(`{"bulk":true}`)})
	c.awaitWrite(t)
	log.awaitLine(t, "OUTBOUND WRITER BLOCKED")

	// Act.
	c.release <- errors.New("frontend test: consumer went away")

	// Assert.
	line := log.awaitLine(t, "outbound writer UNBLOCKED")
	if !strings.Contains(line, "ok=false") {
		t.Fatalf("resolution reports a failed write as delivered: %q", line)
	}
	c.close()
}

func TestAWriteThatCompletesPromptlyIsNotAnnouncedAtAll(t *testing.T) {
	// Arrange: the alarm is for a stall. A healthy consumer must produce no
	// record at all, or the signal is worthless at the rate a boot writes.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Hour)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	c := newGatedConn()
	go s.writeLoop(c, cl)

	// Act: two frames written and released without delay.
	s.enqueue(cl, outFrame{data: []byte(`{"bulk":true}`)})
	c.awaitWrite(t)
	c.release <- nil
	s.enqueue(cl, outFrame{data: []byte(`{"bulk":false}`)})
	c.awaitWrite(t)
	c.release <- nil

	// Assert.
	if log.contains("OUTBOUND WRITER BLOCKED") {
		t.Fatal("a prompt write was announced as a stall")
	}
	c.close()
}
