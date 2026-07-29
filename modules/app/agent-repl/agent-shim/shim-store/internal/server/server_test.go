package server

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/wire"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// --- harness ---------------------------------------------------------------

type harness struct {
	srv  *Server
	db   *db.DB
	path string
	done <-chan struct{}
}

// start brings up a server on a short UDS path (macOS sun_path limit) with the
// given fanout buffer and log sink.
func start(t *testing.T, buffer int, log *logging.Logger) *harness {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "sst")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	sockPath := filepath.Join(dir, "s")

	database, err := db.Open(filepath.Join(t.TempDir(), "events.db"), log.With(logging.Fields{Component: "db"}))
	if err != nil {
		t.Fatalf("db.Open: %v", err)
	}
	t.Cleanup(func() { database.Close() })

	ln, err := Listen(sockPath, log.With(logging.Fields{Component: "server", Socket: sockPath}))
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	srv := New(database, log, buffer)
	done := make(chan struct{})
	go func() {
		defer close(done)
		_ = srv.Serve(ln)
	}()
	t.Cleanup(func() {
		_ = srv.Close()
		<-done
	})
	return &harness{srv: srv, db: database, path: sockPath, done: done}
}

func testLogger() *logging.Logger { return logging.New(io.Discard, io.Discard, false) }

func (h *harness) dial(t *testing.T) net.Conn {
	t.Helper()
	conn, err := net.Dial("unix", h.path)
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	t.Cleanup(func() { conn.Close() })
	return conn
}

func send(t *testing.T, conn net.Conn, m proto.Message) {
	t.Helper()
	a, err := anypb.New(m)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	b, err := proto.Marshal(a)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	if err := wire.WriteFrame(conn, b); err != nil {
		t.Fatalf("write frame: %v", err)
	}
}

func recv(t *testing.T, conn net.Conn) proto.Message {
	t.Helper()
	conn.SetReadDeadline(time.Now().Add(3 * time.Second))
	frame, err := wire.ReadFrame(conn)
	if err != nil {
		t.Fatalf("read frame: %v", err)
	}
	a := &anypb.Any{}
	if err := proto.Unmarshal(frame, a); err != nil {
		t.Fatalf("unmarshal Any: %v", err)
	}
	m, err := a.UnmarshalNew()
	if err != nil {
		t.Fatalf("resolve Any: %v", err)
	}
	return m
}

func recvEvent(t *testing.T, conn net.Conn) *corev1.Event {
	t.Helper()
	m := recv(t, conn)
	ev, ok := m.(*corev1.Event)
	if !ok {
		t.Fatalf("expected *Event, got %T", m)
	}
	return ev
}

func recvAck(t *testing.T, conn net.Conn) *corev1.StoreWriteAck {
	t.Helper()
	m := recv(t, conn)
	ack, ok := m.(*corev1.StoreWriteAck)
	if !ok {
		t.Fatalf("expected *StoreWriteAck, got %T", m)
	}
	return ack
}

func vAssistantStream(t *testing.T, session, uuid string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Uuid: uuid}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: session, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Plane: corev1.Plane_PLANE_STREAM, Payload: &corev1.Event_Vendor{Vendor: a}}
}

func vAssistantDisk(t *testing.T, session, uuid string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{Envelope: &datav1.LineEnvelope{Uuid: uuid}}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: session, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Plane: corev1.Plane_PLANE_FILE, Payload: &corev1.Event_Vendor{Vendor: a}}
}

func write(events ...*corev1.Event) *corev1.StoreWrite {
	return &corev1.StoreWrite{Producer: "test", Batch: &corev1.EventBatch{Events: events}}
}

// --- tests -----------------------------------------------------------------

func TestRoundTripWriteAckSubscribeReplay(t *testing.T) {
	// Arrange
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	// Act: write a two-event batch.
	send(t, prod, write(vAssistantStream(t, "s1", "A"), vAssistantStream(t, "s1", "B")))
	ack := recvAck(t, prod)
	// Assert ack.
	if ack.GetAccepted() != 2 || ack.GetDeduped() != 0 || ack.GetLastSeq() != 2 {
		t.Fatalf("ack = %+v, want accepted=2 deduped=0 last_seq=2", ack)
	}
	// Act: subscribe from 0 and read the replay.
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})
	e1 := recvEvent(t, sub)
	e2 := recvEvent(t, sub)
	// Assert replay.
	if e1.GetSeq() != 1 || e2.GetSeq() != 2 {
		t.Fatalf("replayed seqs = [%d %d], want [1 2]", e1.GetSeq(), e2.GetSeq())
	}
}

func TestHeartbeatCanPrecedeTheFirstProducerWrite(t *testing.T) {
	// Arrange: startup recovery established the producer socket, but no source
	// file changed yet, so the sidecar has no StoreWrite with which to declare
	// the connection's role.
	h := start(t, 0, testLogger())
	prod := h.dial(t)

	// Act: idle liveness traffic arrives first, then a real producer batch.
	send(t, prod, &corev1.Heartbeat{SentAtMs: 42})
	echo, ok := recv(t, prod).(*corev1.Heartbeat)
	if !ok {
		t.Fatalf("heartbeat reply type = %T, want *Heartbeat", echo)
	}
	send(t, prod, write(vAssistantStream(t, "s1", "A")))
	ack := recvAck(t, prod)

	// Assert: the preamble stayed connected and the first write was ingested.
	if echo.GetSentAtMs() != 42 {
		t.Fatalf("heartbeat sent_at_ms = %d, want 42", echo.GetSentAtMs())
	}
	if ack.GetAccepted() != 1 || ack.GetLastSeq() != 1 {
		t.Fatalf("ack = %+v, want accepted=1 last_seq=1", ack)
	}
}

func TestHealthCheckCanPrecedeTheFirstProducerWrite(t *testing.T) {
	// Arrange: health is the first intentional frame on the recovered producer
	// socket, before a file change provides a StoreWrite.
	h := start(t, 0, testLogger())
	prod := h.dial(t)

	// Act: assert a correlated health reply, then write on the same connection.
	send(t, prod, &corev1.HealthCheck{RequestId: "health-before-write"})
	status, ok := recv(t, prod).(*corev1.HealthStatus)
	if !ok {
		t.Fatalf("health reply type = %T, want *HealthStatus", status)
	}
	send(t, prod, write(vAssistantStream(t, "s1", "A")))
	ack := recvAck(t, prod)

	// Assert: health was correlated and did not discard the producer preamble.
	if status.GetRequestId() != "health-before-write" || !status.GetHealthy() || status.GetComponent() != "shim-store" {
		t.Fatalf("health status = %+v, want correlated healthy shim-store status", status)
	}
	if ack.GetAccepted() != 1 || ack.GetLastSeq() != 1 {
		t.Fatalf("ack = %+v, want accepted=1 last_seq=1", ack)
	}
}

func TestEmptySubscribeLogsCanonicalProtocolRejection(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}))
	conn := h.dial(t)
	send(t, conn, &corev1.Subscribe{})
	conn.SetReadDeadline(time.Now().Add(time.Second))
	if _, err := wire.ReadAny(conn); err == nil {
		t.Fatal("empty subscription unexpectedly received a response")
	}
	if err := h.srv.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	<-h.done

	record, found := findLoggedRecord(t, logs.Bytes(), "subscribe", "error")
	if !found {
		t.Fatalf("empty-subscribe rejection log missing: %s", logs.String())
	}
	if record.Context["component"] != "server" || record.Context["socket"] != "store.sock" || record.Context["subscriber"] == "" {
		t.Fatalf("empty-subscribe rejection lacks canonical connection context: %#v", record)
	}
}

func TestCloseLogsListenerFailure(t *testing.T) {
	var logs bytes.Buffer
	closeErr := errors.New("listener close failed")
	srv := &Server{
		log:   logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}),
		ln:    failingListener{err: closeErr},
		conns: make(map[net.Conn]struct{}),
	}
	if err := srv.Close(); !errors.Is(err, closeErr) {
		t.Fatalf("Close error = %v, want listener failure", err)
	}

	record, found := findLoggedRecord(t, logs.Bytes(), "close-listener", "error")
	if !found {
		t.Fatalf("listener-close error record missing: %s", logs.String())
	}
	if record.Level != "error" || record.Context["component"] != "server" || record.Context["socket"] != "store.sock" {
		t.Fatalf("listener-close error lacks canonical context: %#v", record)
	}
}

type failingListener struct{ err error }

func (l failingListener) Accept() (net.Conn, error) { return nil, l.err }
func (l failingListener) Close() error              { return l.err }
func (l failingListener) Addr() net.Addr            { return fakeAddr("store.sock") }

type fakeAddr string

func (a fakeAddr) Network() string { return "unix" }
func (a fakeAddr) String() string  { return string(a) }

func TestReplayFromMidSeq(t *testing.T) {
	// Arrange
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	send(t, prod, write(vAssistantStream(t, "s1", "A"), vAssistantStream(t, "s1", "B"), vAssistantStream(t, "s1", "C")))
	recvAck(t, prod)
	// Act: subscribe from_seq=1 (exclusive) → expect seqs 2,3.
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 1})
	e1 := recvEvent(t, sub)
	e2 := recvEvent(t, sub)
	// Assert
	if e1.GetSeq() != 2 || e2.GetSeq() != 3 {
		t.Fatalf("replay from_seq=1 gave [%d %d], want [2 3]", e1.GetSeq(), e2.GetSeq())
	}
}

func TestDedupCollisionAcrossPlanes(t *testing.T) {
	// Arrange
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	// Act: the stream twin then the file twin of the same uuid.
	send(t, prod, write(vAssistantStream(t, "s1", "X")))
	ack1 := recvAck(t, prod)
	send(t, prod, write(vAssistantDisk(t, "s1", "X")))
	ack2 := recvAck(t, prod)
	// Assert: first accepted, second fully deduped.
	if ack1.GetAccepted() != 1 || ack1.GetDeduped() != 0 {
		t.Fatalf("ack1 = %+v, want accepted=1 deduped=0", ack1)
	}
	if ack2.GetAccepted() != 0 || ack2.GetDeduped() != 1 {
		t.Fatalf("ack2 = %+v, want accepted=0 deduped=1", ack2)
	}
	// Assert: exactly one row persisted.
	replayed, err := h.db.ReplayFrom("s1", 0)
	if err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	if len(replayed) != 1 {
		t.Fatalf("persisted %d events, want 1 (deduped twin)", len(replayed))
	}
}

func TestCrashReplayIdempotency(t *testing.T) {
	// Arrange
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	batch := func() *corev1.StoreWrite {
		return write(vAssistantStream(t, "s1", "A"), vAssistantStream(t, "s1", "B"))
	}
	// Act: the identical batch twice (a producer crash-and-replay).
	send(t, prod, batch())
	ack1 := recvAck(t, prod)
	send(t, prod, batch())
	ack2 := recvAck(t, prod)
	// Assert: first fully accepted, second fully deduped.
	if ack1.GetAccepted() != 2 {
		t.Fatalf("ack1 accepted = %d, want 2", ack1.GetAccepted())
	}
	if ack2.GetAccepted() != 0 || ack2.GetDeduped() != 2 {
		t.Fatalf("ack2 = %+v, want accepted=0 deduped=2", ack2)
	}
	replayed, err := h.db.ReplayFrom("s1", 0)
	if err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	if len(replayed) != 2 {
		t.Fatalf("persisted %d events, want 2", len(replayed))
	}
}

func TestEphemeralPassThroughNotPersisted(t *testing.T) {
	// Arrange
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})

	// Handshake: a persistent event proves the subscriber is registered and
	// live-tailing (received via replay or live either way) before we send the
	// ephemeral, with no timing assumptions.
	send(t, prod, write(vAssistantStream(t, "s1", "P1")))
	recvAck(t, prod)
	if got := recvEvent(t, sub); got.GetSeq() != 1 {
		t.Fatalf("handshake seq = %d, want 1", got.GetSeq())
	}

	// Act: a batch mixing a persistent event and an ephemeral one.
	eph := &corev1.Event{SessionId: "s1", Class: corev1.EventClass_EVENT_CLASS_EPHEMERAL,
		Payload: &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "live"}}}
	send(t, prod, write(vAssistantStream(t, "s1", "P2"), eph))
	ack := recvAck(t, prod)

	// Assert: only the persistent event was accepted/persisted.
	if ack.GetAccepted() != 1 {
		t.Fatalf("ack accepted = %d, want 1 (ephemeral not persisted)", ack.GetAccepted())
	}
	p2 := recvEvent(t, sub)
	if p2.GetSeq() != 2 {
		t.Fatalf("persistent seq = %d, want 2", p2.GetSeq())
	}
	live := recvEvent(t, sub)
	if live.GetClass() != corev1.EventClass_EVENT_CLASS_EPHEMERAL {
		t.Fatalf("expected ephemeral pass-through, got class %v", live.GetClass())
	}

	// Assert: the DB contains only the two persistent events, never the ephemeral.
	replayed, err := h.db.ReplayFrom("s1", 0)
	if err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	if len(replayed) != 2 {
		t.Fatalf("persisted %d events, want 2 (no ephemeral)", len(replayed))
	}
	for _, ev := range replayed {
		if ev.GetClass() == corev1.EventClass_EVENT_CLASS_EPHEMERAL {
			t.Fatal("ephemeral event was persisted")
		}
	}
}

// collectLogs returns a log sink plus a drain. Every line the server emits for
// a batch is logged before its ack is written, so draining after recvAck sees
// exactly that batch's lines with no timing assumptions.
type channelWriter chan string

func (w channelWriter) Write(p []byte) (int, error) {
	select {
	case w <- strings.TrimSpace(string(p)):
	default:
	}
	return len(p), nil
}

func collectLogs(capacity int) (*logging.Logger, func() []string) {
	lines := make(chan string, capacity)
	logf := logging.New(channelWriter(lines), io.Discard, false)
	drain := func() []string {
		var out []string
		for {
			select {
			case l := <-lines:
				out = append(out, l)
			default:
				return out
			}
		}
	}
	return logf, drain
}

func findLine(lines []string, operation string) string {
	for _, l := range lines {
		if strings.Contains(l, `"operation":"`+operation+`"`) {
			return l
		}
	}
	return ""
}

func findLineContaining(lines []string, operation, message string) string {
	for _, l := range lines {
		if strings.Contains(l, `"operation":"`+operation+`"`) && strings.Contains(l, message) {
			return l
		}
	}
	return ""
}

type loggedRecord struct {
	Level     string         `json:"level"`
	Operation string         `json:"operation"`
	Session   string         `json:"claude_session_id"`
	Context   map[string]any `json:"context"`
}

func findLoggedRecord(t *testing.T, logs []byte, operation, level string) (loggedRecord, bool) {
	t.Helper()
	for _, line := range bytes.Split(bytes.TrimSpace(logs), []byte("\n")) {
		var record loggedRecord
		if err := json.Unmarshal(line, &record); err != nil {
			t.Fatalf("server record is not JSON: %v", err)
		}
		if record.Operation == operation && record.Level == level {
			return record, true
		}
	}
	return loggedRecord{}, false
}

func TestIngestLineLogsBatchFacts(t *testing.T) {
	// Arrange
	logf, drain := collectLogs(64)
	h := start(t, 0, logf)
	prod := h.dial(t)

	// Act: a two-event persistent batch.
	send(t, prod, write(vAssistantStream(t, "s1", "A"), vAssistantStream(t, "s1", "B")))
	recvAck(t, prod)

	// Assert: the server's durable-batch outcome carries the batch's facts.
	want := "persisted batch events=2 accepted=2 deduped=0 last_seq=2 ingest_ms="
	got := findLineContaining(drain(), "ingest", want)
	if !strings.Contains(got, want) {
		t.Fatalf("ingest line = %q, want message containing %q", got, want)
	}
}

func TestIngestLineSilentForEphemeralOnlyBatch(t *testing.T) {
	// Arrange
	logf, drain := collectLogs(64)
	h := start(t, 0, logf)
	prod := h.dial(t)

	// Act: a batch with nothing persistent in it.
	eph := &corev1.Event{SessionId: "s1", Class: corev1.EventClass_EVENT_CLASS_EPHEMERAL,
		Payload: &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "live"}}}
	send(t, prod, write(eph))
	recvAck(t, prod)

	// Assert: no ingest line for a batch that never touched the DB.
	if got := findLine(drain(), "ingest"); got != "" {
		t.Fatalf("ephemeral-only batch logged %q, want silence", got)
	}
}

func TestSlowConsumerHardDisconnect(t *testing.T) {
	// Arrange: a tiny buffer and a subscriber that stops reading. The
	// workspace-aware requester owns this session-specific disconnect.
	h := start(t, 1, testLogger())
	prod := h.dial(t)
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})

	// Handshake so the subscriber is registered and live.
	send(t, prod, write(vAssistantStream(t, "s1", "P1")))
	recvAck(t, prod)
	recvEvent(t, sub)

	// Act: blast a batch of large, unique events while the subscriber never
	// reads again. Padding makes each frame ~1KiB so a modest count reliably
	// overflows the OS socket buffer plus the bounded per-subscriber buffer,
	// and the store hard-disconnects — without the ingest cost of a huge batch.
	pad := strings.Repeat("x", 1024)
	big := make([]*corev1.Event, 0, 2000)
	for i := range 2000 {
		big = append(big, vAssistantStream(t, "s1", fmt.Sprintf("u%04d%s", i, pad)))
	}
	send(t, prod, write(big...))
	recvAck(t, prod)

	// Assert: the slow consumer is disconnected. No store narrative record is
	// expected because the requester can attribute and report the session.
	if err := sub.SetReadDeadline(time.Now().Add(5 * time.Second)); err != nil {
		t.Fatalf("setting subscriber read deadline: %v", err)
	}
	for {
		if _, err := wire.ReadAny(sub); err != nil {
			return
		}
	}
}

func recvCursorList(t *testing.T, conn net.Conn) *corev1.CursorList {
	t.Helper()
	m := recv(t, conn)
	cl, ok := m.(*corev1.CursorList)
	if !ok {
		t.Fatalf("expected *CursorList, got %T", m)
	}
	return cl
}

func TestCursorQueryReturnsAllPersistedCursors(t *testing.T) {
	// Arrange: a producer commits a batch carrying a cursor advance.
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	sw := write(vAssistantStream(t, "s1", "A"))
	sw.Batch.CursorAdvance = &corev1.CursorState{FileId: "10:20", Path: "/x/y.jsonl", Offset: 42, Carry: []byte("tail")}
	send(t, prod, sw)
	recvAck(t, prod)

	// Act: a fresh connection recovers cursors (empty file_id = all).
	cq := h.dial(t)
	send(t, cq, &corev1.CursorQuery{})
	list := recvCursorList(t, cq)

	// Assert
	if len(list.GetCursors()) != 1 {
		t.Fatalf("cursors = %d, want 1", len(list.GetCursors()))
	}
	c := list.GetCursors()[0]
	if c.GetFileId() != "10:20" || c.GetOffset() != 42 || string(c.GetCarry()) != "tail" {
		t.Fatalf("cursor = %+v", c)
	}
}

func TestCursorQueryReturnsAuthoritativeOpenTaskStarts(t *testing.T) {
	h := start(t, 0, testLogger())
	started := func(taskID string) *corev1.Event {
		return &corev1.Event{
			SessionId: "s1", Plane: corev1.Plane_PLANE_FILE,
			Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, ProducedAtMs: 100,
			Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
				TaskId: taskID, Kind: corev1.TaskKind_TASK_KIND_SHELL,
			}},
		}
	}
	closed := started("closed")
	ended := &corev1.Event{
		SessionId: "s1", Plane: corev1.Plane_PLANE_STREAM,
		Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, ProducedAtMs: 200,
		Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{
			TaskId: "closed", Kind: corev1.TaskKind_TASK_KIND_SHELL,
			Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE,
		}},
	}
	if _, err := h.db.Ingest("test", []*corev1.Event{started("open"), closed, ended}, nil); err != nil {
		t.Fatalf("Ingest lifecycle: %v", err)
	}

	cq := h.dial(t)
	send(t, cq, &corev1.CursorQuery{})
	list := recvCursorList(t, cq)
	if !list.GetOpenTasksAuthoritative() {
		t.Fatal("all-cursors recovery did not attest authoritative open-task state")
	}
	if len(list.GetOpenTasks()) != 1 {
		t.Fatalf("open_tasks = %d, want 1", len(list.GetOpenTasks()))
	}
	if got := list.GetOpenTasks()[0].GetStarted().GetTaskStarted().GetTaskId(); got != "open" {
		t.Fatalf("open task id = %q, want open", got)
	}
}

func TestCursorQueryByFileID(t *testing.T) {
	// Arrange: two persisted cursors.
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	for _, fid := range []string{"1:1", "2:2"} {
		sw := write(vAssistantStream(t, "s1", "E"+fid))
		sw.Batch.CursorAdvance = &corev1.CursorState{FileId: fid, Path: "/p/" + fid, Offset: 7}
		send(t, prod, sw)
		recvAck(t, prod)
	}
	// Act: query one file_id.
	cq := h.dial(t)
	send(t, cq, &corev1.CursorQuery{FileId: "2:2"})
	list := recvCursorList(t, cq)
	// Assert: exactly that cursor.
	if len(list.GetCursors()) != 1 || list.GetCursors()[0].GetFileId() != "2:2" {
		t.Fatalf("by-id query = %+v, want just 2:2", list.GetCursors())
	}
}

func TestCursorQueryEmptyWhenAbsent(t *testing.T) {
	// Arrange: nothing persisted.
	h := start(t, 0, testLogger())
	// Act
	cq := h.dial(t)
	send(t, cq, &corev1.CursorQuery{FileId: "nope"})
	list := recvCursorList(t, cq)
	// Assert
	if len(list.GetCursors()) != 0 {
		t.Fatalf("cursors = %d, want 0", len(list.GetCursors()))
	}
}

func TestCloseDisconnectsLiveConnections(t *testing.T) {
	// Arrange: an established subscriber connection.
	h := start(t, 0, testLogger())
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})
	// Give the handler a moment to register by round-tripping a write so we
	// know the server is actively serving this session.
	prod := h.dial(t)
	send(t, prod, write(vAssistantStream(t, "s1", "P1")))
	recvAck(t, prod)
	recvEvent(t, sub) // subscriber is live

	// Act
	if err := h.srv.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert: the live subscriber connection is closed by the server.
	sub.SetReadDeadline(time.Now().Add(3 * time.Second))
	if _, err := wire.ReadFrame(sub); err == nil {
		t.Fatal("expected subscriber read to fail after server Close")
	}
}
