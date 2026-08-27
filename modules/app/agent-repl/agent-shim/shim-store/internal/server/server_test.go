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

	storev1 "agentrepl/proto/store/v1"
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

	dbPath := filepath.Join(t.TempDir(), "entries.db")
	database, err := db.Open(dbPath, log.With(logging.Fields{Component: "db"}))
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

// seedCursor commits a reader position through the db layer directly.
//
// NOT OVER THE WIRE, because there is no write barrier left. The producer
// connection used to be probed with a correlated HealthCheck to establish that
// an earlier write had been fully processed; ConnectionHeartbeat and
// HealthCheck are both deleted and WriteBatchResponse is not served, so the
// producer stream is write-only with nothing to synchronize on. Seeding through
// the db layer is synchronous by construction rather than by a timing
// assumption.
func (h *harness) seedCursor(t *testing.T, c *storev1.CursorState) {
	t.Helper()
	if _, err := h.db.Ingest("test", &storev1.EntryBatch{CursorAdvance: c}); err != nil {
		t.Fatalf("seeding cursor: %v", err)
	}
}

func sendMsg(conn net.Conn, m proto.Message) error {
	a, err := anypb.New(m)
	if err != nil {
		return fmt.Errorf("anypb.New: %w", err)
	}
	b, err := proto.Marshal(a)
	if err != nil {
		return fmt.Errorf("marshal: %w", err)
	}
	if err := wire.WriteFrame(conn, b); err != nil {
		return fmt.Errorf("write frame: %w", err)
	}
	return nil
}

func recvMsg(conn net.Conn) (proto.Message, error) {
	if err := conn.SetReadDeadline(time.Now().Add(10 * time.Second)); err != nil {
		return nil, fmt.Errorf("set read deadline: %w", err)
	}
	frame, err := wire.ReadFrame(conn)
	if err != nil {
		return nil, fmt.Errorf("read frame: %w", err)
	}
	a := &anypb.Any{}
	if err := proto.Unmarshal(frame, a); err != nil {
		return nil, fmt.Errorf("unmarshal Any: %w", err)
	}
	m, err := a.UnmarshalNew()
	if err != nil {
		return nil, fmt.Errorf("resolve Any: %w", err)
	}
	return m, nil
}

func send(t *testing.T, conn net.Conn, m proto.Message) {
	t.Helper()
	if err := sendMsg(conn, m); err != nil {
		t.Fatalf("send: %v", err)
	}
}

func recv(t *testing.T, conn net.Conn) proto.Message {
	t.Helper()
	m, err := recvMsg(conn)
	if err != nil {
		t.Fatalf("recv: %v", err)
	}
	return m
}

func recvCursors(t *testing.T, conn net.Conn) *storev1.GetSidecarCursorsResponse {
	t.Helper()
	m := recv(t, conn)
	resp, ok := m.(*storev1.GetSidecarCursorsResponse)
	if !ok {
		t.Fatalf("expected *GetSidecarCursorsResponse, got %T", m)
	}
	return resp
}

// --- record fixtures -------------------------------------------------------

// streamEntry is the minimum a stored record carries: the plane that observed
// it. Every batch built on it is refused by the db layer, which is the point of
// the producer tests below.
func streamEntry() *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane: &storev1.Plane{Plane: &storev1.Plane_Stream{Stream: &storev1.PlaneStream{}}},
	}
}

func writeBatch(entries ...*storev1.StoreEntry) *storev1.WriteBatchRequest {
	return &storev1.WriteBatchRequest{Producer: "test", Batch: &storev1.EntryBatch{Entries: entries}}
}

// --- log helpers -----------------------------------------------------------

type channelWriter chan string

func (w channelWriter) Write(p []byte) (int, error) {
	select {
	case w <- strings.TrimSpace(string(p)):
	default:
	}
	return len(p), nil
}

// drain returns every record the sink holds without blocking.
func drain(lines <-chan string) []string {
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

// awaitLine blocks until the log sink emits a record for `operation`.
//
// IT IS THE ONLY BARRIER LEFT ON A PRODUCER CONNECTION. The write half is
// write-only — WriteBatchResponse is declared but not served, and the
// correlated HealthCheck the old suite used as a barrier is deleted — so no
// reply frame can prove the server has taken the connection. The canonical
// record is emitted from handleConn AFTER trackConn and the handler's
// wg.Add(1), so receiving it establishes that Close will find the connection.
// A channel receive, never a sleep.
// It returns every record it consumed, the awaited one last.
func awaitLine(t *testing.T, lines <-chan string, operation string) []string {
	t.Helper()
	var seen []string
	deadline := time.After(10 * time.Second)
	for {
		select {
		case l := <-lines:
			seen = append(seen, l)
			if strings.Contains(l, `"operation":"`+operation+`"`) {
				return seen
			}
		case <-deadline:
			t.Fatalf("timed out waiting for a %q record; saw %v", operation, seen)
		}
	}
}

func findLine(lines []string, operation string) string {
	for _, l := range lines {
		if strings.Contains(l, `"operation":"`+operation+`"`) {
			return l
		}
	}
	return ""
}

type loggedRecord struct {
	Level     string         `json:"level"`
	Operation string         `json:"operation"`
	Message   string         `json:"message"`
	Session   string         `json:"claude_session_id"`
	Context   map[string]any `json:"context"`
}

func findLoggedRecord(t *testing.T, lines []string, operation, level string) (loggedRecord, bool) {
	t.Helper()
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var record loggedRecord
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("server record is not JSON: %v (%s)", err, line)
		}
		if record.Operation == operation && record.Level == level {
			return record, true
		}
	}
	return loggedRecord{}, false
}

// splitLines adapts a buffered sink to findLoggedRecord.
func splitLines(logs []byte) []string {
	var out []string
	for _, l := range bytes.Split(bytes.TrimSpace(logs), []byte("\n")) {
		out = append(out, string(l))
	}
	return out
}

// --- connection classification ---------------------------------------------

func TestUnrecognizedFirstFrameIsRefusedLoudly(t *testing.T) {
	// Arrange: a frame that is not one of the three the store still speaks.
	lines := make(chan string, 64)
	h := start(t, 0, logging.New(channelWriter(lines), io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}))
	conn := h.dial(t)

	// Act
	send(t, conn, &storev1.ReadAgentPageRequest{})
	collected := awaitLine(t, lines, "classify-connection")

	// Assert
	record, found := findLoggedRecord(t, collected, "classify-connection", "error")
	if !found {
		t.Fatalf("unclassified-frame record missing: %v", collected)
	}
	if !strings.Contains(record.Message, "expected WriteBatchRequest") {
		t.Fatalf("refusal message = %q, want it to name the frames the store accepts", record.Message)
	}
}

// --- the subscription refusal ----------------------------------------------

func TestWatchAgentSessionIsRefusedRatherThanRegistered(t *testing.T) {
	// Arrange: a registered subscription that can never deliver a line is a
	// silent empty feed, which is the failure this refusal exists to prevent.
	h := start(t, 0, testLogger())
	sub := h.dial(t)

	// Act
	send(t, sub, &storev1.WatchAgentSessionRequest{Watch: &storev1.AgentSessionToken{Value: "tok"}})

	// Assert: the connection ends rather than standing open with no tail.
	if err := sub.SetReadDeadline(time.Now().Add(5 * time.Second)); err != nil {
		t.Fatalf("setting read deadline: %v", err)
	}
	if _, err := wire.ReadAny(sub); err == nil {
		t.Fatal("a refused watch left the connection open, which reads as a live subscription")
	}
}

func TestWatchAgentSessionRefusalRegistersNoSubscriber(t *testing.T) {
	// Arrange
	h := start(t, 0, testLogger())
	sub := h.dial(t)

	// Act
	send(t, sub, &storev1.WatchAgentSessionRequest{})
	if err := sub.SetReadDeadline(time.Now().Add(5 * time.Second)); err != nil {
		t.Fatalf("setting read deadline: %v", err)
	}
	_, _ = wire.ReadAny(sub)

	// Assert: the fan-out registry is untouched, so nothing can leak a
	// subscriber the store cannot serve.
	if got := h.srv.fan.subscriberCount(""); got != 0 {
		t.Fatalf("subscriberCount = %d, want 0 after a refused watch", got)
	}
}

func TestWatchAgentSessionRefusalIsLoggedAtError(t *testing.T) {
	// Arrange
	lines := make(chan string, 64)
	h := start(t, 0, logging.New(channelWriter(lines), io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}))
	conn := h.dial(t)

	// Act
	send(t, conn, &storev1.WatchAgentSessionRequest{})
	collected := awaitLine(t, lines, "subscribe")

	// Assert
	record, found := findLoggedRecord(t, collected, "subscribe", "error")
	if !found {
		t.Fatalf("watch refusal record missing: %v", collected)
	}
	if !strings.Contains(record.Message, "REFUSED") || record.Context["component"] != "server" || record.Context["subscriber"] == "" {
		t.Fatalf("watch refusal lacks canonical connection context: %#v", record)
	}
}

// --- the producer side ------------------------------------------------------

func TestRejectedBatchDropsTheProducerConnection(t *testing.T) {
	// Arrange: every batch carrying records is refused by the db layer while
	// record persistence is unreconciled.
	//
	// THE CONNECTION DROP IS THE WHOLE SIGNAL. WriteBatchResponse exists in the
	// schema but is not served, so silence would be indistinguishable from
	// success and the producer would go on writing into a store discarding its
	// work.
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server"}))
	bad := h.dial(t)

	// Act
	send(t, bad, writeBatch(streamEntry()))

	// Assert: the connection ends rather than staying open in false health.
	if err := bad.SetReadDeadline(time.Now().Add(5 * time.Second)); err != nil {
		t.Fatalf("setting read deadline: %v", err)
	}
	if _, err := wire.ReadAny(bad); err == nil {
		t.Fatal("a rejected batch left the producer connection open, which is indistinguishable from success")
	}

	// Assert: the refusal was loud. The server is stopped first so the log sink
	// has one writer, then this reader — no concurrent access, and no timing
	// assumption either.
	if err := h.srv.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	<-h.done
	record, found := findLoggedRecord(t, splitLines(logs.Bytes()), "store-write", "error")
	if !found {
		t.Fatalf("rejected-batch record missing: %s", logs.String())
	}
	if !strings.Contains(record.Message, "REJECTED") {
		t.Fatalf("rejection message = %q, want it to state the refusal", record.Message)
	}
}

func TestEmptyBatchLogsNoIngestLine(t *testing.T) {
	// Arrange: a batch with neither records nor a cursor advance never touches
	// the database, so it must not narrate one.
	lines := make(chan string, 64)
	h := start(t, 0, logging.New(channelWriter(lines), io.Discard, true))
	prod := h.dial(t)

	// Act
	send(t, prod, writeBatch())
	awaitLine(t, lines, "ingest-classify")
	if err := h.srv.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	<-h.done

	// Assert: the classify line fired, so the batch was processed; no `ingest`
	// line followed it, because nothing touched the database.
	if got := findLine(drain(lines), "ingest"); got != "" {
		t.Fatalf("empty batch logged %q, want silence", got)
	}
}

// --- sidecar cursor recovery -------------------------------------------------

func TestCursorQueryReturnsAllPersistedCursors(t *testing.T) {
	// Arrange
	h := start(t, 0, testLogger())
	h.seedCursor(t, &storev1.CursorState{FileId: "10:20", Path: "/x/y.jsonl", Offset: 42, Carry: []byte("tail")})

	// Act: an unset file_id asks for every cursor.
	cq := h.dial(t)
	send(t, cq, &storev1.GetSidecarCursorsRequest{})
	resp := recvCursors(t, cq)

	// Assert
	cursors := resp.GetSuccess().GetCursors()
	if len(cursors) != 1 {
		t.Fatalf("cursors = %d, want 1 (result=%T)", len(cursors), resp.GetResult())
	}
	c := cursors[0]
	if c.GetFileId() != "10:20" || c.GetOffset() != 42 || string(c.GetCarry()) != "tail" {
		t.Fatalf("cursor = %+v", c)
	}
}

func TestCursorQueryByFileID(t *testing.T) {
	// Arrange: two persisted cursors.
	h := start(t, 0, testLogger())
	h.seedCursor(t, &storev1.CursorState{FileId: "1:1", Path: "/p/1", Offset: 7})
	h.seedCursor(t, &storev1.CursorState{FileId: "2:2", Path: "/p/2", Offset: 7})

	// Act
	cq := h.dial(t)
	send(t, cq, &storev1.GetSidecarCursorsRequest{FileId: proto.String("2:2")})
	resp := recvCursors(t, cq)

	// Assert
	cursors := resp.GetSuccess().GetCursors()
	if len(cursors) != 1 || cursors[0].GetFileId() != "2:2" {
		t.Fatalf("by-id query = %+v, want just 2:2", cursors)
	}
}

func TestCursorQueryEmptyWhenAbsent(t *testing.T) {
	// Arrange: nothing persisted. An empty set is a legitimate answer — a fresh
	// store has no cursors — so it is the SUCCESS arm, not the failure arm.
	h := start(t, 0, testLogger())

	// Act
	cq := h.dial(t)
	send(t, cq, &storev1.GetSidecarCursorsRequest{FileId: proto.String("nope")})
	resp := recvCursors(t, cq)

	// Assert
	if resp.GetSuccess() == nil {
		t.Fatalf("result = %T, want the success arm for an absent cursor", resp.GetResult())
	}
	if len(resp.GetSuccess().GetCursors()) != 0 {
		t.Fatalf("cursors = %d, want 0", len(resp.GetSuccess().GetCursors()))
	}
}

func TestCursorQueryFailureIsAnsweredRatherThanSwallowed(t *testing.T) {
	// Arrange: a closed database, so the query fails at the driver. A sidecar
	// that cannot tell "no cursors" from "could not read cursors" resumes every
	// tailed file from zero.
	h := start(t, 0, testLogger())
	if err := h.db.Close(); err != nil {
		t.Fatalf("closing db: %v", err)
	}

	// Act
	cq := h.dial(t)
	send(t, cq, &storev1.GetSidecarCursorsRequest{})
	resp := recvCursors(t, cq)

	// Assert
	if resp.GetFailure() == nil {
		t.Fatalf("result = %T, want the failure arm", resp.GetResult())
	}
	if resp.GetFailure().GetDetail() == "" {
		t.Fatal("failure carries no detail, so the sidecar's log cannot say why")
	}
}

// --- lifecycle ---------------------------------------------------------------

func TestCloseLogsListenerFailure(t *testing.T) {
	// Arrange
	var logs bytes.Buffer
	closeErr := errors.New("listener close failed")
	srv := &Server{
		log:   logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}),
		ln:    failingListener{err: closeErr},
		conns: make(map[net.Conn]struct{}),
	}

	// Act
	err := srv.Close()

	// Assert
	if !errors.Is(err, closeErr) {
		t.Fatalf("Close error = %v, want listener failure", err)
	}
	record, found := findLoggedRecord(t, splitLines(logs.Bytes()), "close-listener", "error")
	if !found {
		t.Fatalf("listener-close error record missing: %s", logs.String())
	}
	if record.Level != "error" || record.Context["component"] != "server" || record.Context["socket"] != "store.sock" {
		t.Fatalf("listener-close error lacks canonical context: %#v", record)
	}
}

func TestCloseDisconnectsLiveConnections(t *testing.T) {
	// Arrange: a producer connection the server has demonstrably taken.
	lines := make(chan string, 64)
	h := start(t, 0, logging.New(channelWriter(lines), io.Discard, false))
	prod := h.dial(t)
	send(t, prod, writeBatch())
	awaitLine(t, lines, "classify-connection")

	// Act
	if err := h.srv.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert
	if err := prod.SetReadDeadline(time.Now().Add(3 * time.Second)); err != nil {
		t.Fatalf("setting read deadline: %v", err)
	}
	if _, err := wire.ReadFrame(prod); err == nil {
		t.Fatal("expected the producer read to fail after server Close")
	}
}

type failingListener struct{ err error }

func (l failingListener) Accept() (net.Conn, error) { return nil, l.err }
func (l failingListener) Close() error              { return l.err }
func (l failingListener) Addr() net.Addr            { return fakeAddr("store.sock") }

type fakeAddr string

func (a fakeAddr) Network() string { return "unix" }
func (a fakeAddr) String() string  { return string(a) }
