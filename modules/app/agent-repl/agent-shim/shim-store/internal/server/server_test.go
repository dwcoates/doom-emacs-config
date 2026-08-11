package server

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
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

// sendMsg / recvMsg are the GOROUTINE-SAFE halves of the framing helpers: they
// return errors instead of calling t.Fatalf, which a non-test goroutine must
// never do. The concurrency tests below drive producers from their own
// goroutines and so cannot use the t-bound wrappers.
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

func recvEvent(t *testing.T, conn net.Conn) *corev1.Event {
	t.Helper()
	m := recv(t, conn)
	ev, ok := m.(*corev1.Event)
	if !ok {
		t.Fatalf("expected *Event, got %T", m)
	}
	return ev
}

func recvSubscriptionReady(t *testing.T, conn net.Conn) {
	t.Helper()
	if _, ok := recv(t, conn).(*corev1.Heartbeat); !ok {
		t.Fatal("subscription readiness frame is not a Heartbeat")
	}
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

func collectStoredReplay(t *testing.T, database *db.DB, session string, fromSeq uint64) []*corev1.Event {
	t.Helper()
	var events []*corev1.Event
	if _, err := database.ReplayFrom(context.Background(), session, fromSeq, func(ev *corev1.Event) error {
		events = append(events, ev)
		return nil
	}); err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	return events
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
	recvSubscriptionReady(t, sub)
	// Assert replay.
	if e1.GetSeq() != 1 || e2.GetSeq() != 2 {
		t.Fatalf("replayed seqs = [%d %d], want [1 2]", e1.GetSeq(), e2.GetSeq())
	}
}

func TestSubscribeReadyProvesRegistrationBeforeAnImmediateProducerWrite(t *testing.T) {
	// Arrange: an empty store makes readiness the first subscriber frame.
	h := start(t, 0, testLogger())
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})

	// Act: the readiness frame is the registration barrier, then another socket
	// writes without any delay.
	recvSubscriptionReady(t, sub)
	prod := h.dial(t)
	send(t, prod, write(vAssistantStream(t, "s1", "after-ready")))
	if ack := recvAck(t, prod); ack.GetAccepted() != 1 {
		t.Fatalf("write ack accepted = %d, want 1", ack.GetAccepted())
	}

	// Assert: the event cannot have overtaken subscriber registration.
	if ev := recvEvent(t, sub); ev.GetSeq() != 1 {
		t.Fatalf("live event seq = %d, want 1", ev.GetSeq())
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
	recvSubscriptionReady(t, sub)
	// Assert
	if e1.GetSeq() != 2 || e2.GetSeq() != 3 {
		t.Fatalf("replay from_seq=1 gave [%d %d], want [2 3]", e1.GetSeq(), e2.GetSeq())
	}
}

func TestLargeReplayStreamsInOrderWithBoundedProgressLogs(t *testing.T) {
	// Arrange: one batch near the observed incident scale's first progress
	// boundary. The store must emit the first row before advancing through the
	// query and must not emit one diagnostic per row.
	const eventCount = 513
	logf, drain := collectLogs(128, true)
	h := start(t, 0, logf)
	prod := h.dial(t)
	events := make([]*corev1.Event, 0, eventCount)
	for i := range eventCount {
		events = append(events, vAssistantStream(t, "s1", fmt.Sprintf("replay-%04d", i)))
	}
	send(t, prod, write(events...))
	recvAck(t, prod)

	// Act
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})
	for want := uint64(1); want <= eventCount; want++ {
		if got := recvEvent(t, sub).GetSeq(); got != want {
			t.Fatalf("streamed seq=%d, want=%d", got, want)
		}
	}
	recvSubscriptionReady(t, sub)
	// A live event proves serveSubscriber finished the replay and entered its
	// tail loop, so the completion record is present without a timing sleep.
	send(t, prod, write(vAssistantStream(t, "s1", "tail-proof")))
	recvAck(t, prod)
	if got := recvEvent(t, sub).GetSeq(); got != eventCount+1 {
		t.Fatalf("tail proof seq=%d, want=%d", got, eventCount+1)
	}

	// Assert
	lines := drain()
	if got := findLineContaining(lines, "subscribe-replay-progress", "delivered=512 first_seq=1 last_seq=512"); got == "" {
		t.Fatalf("bounded replay progress record missing from %d log lines", len(lines))
	}
	if got := findLineContaining(lines, "subscribe-replay", "delivered=513 first_seq=1 last_seq=513 query_ms="); got == "" {
		t.Fatalf("replay completion range and timing missing from %d log lines", len(lines))
	}
	progressRecords := 0
	for _, line := range lines {
		if strings.Contains(line, `"operation":"subscribe-replay-progress"`) {
			progressRecords++
		}
	}
	if progressRecords != 2 {
		t.Fatalf("progress records=%d, want 2 at delivered=1 and delivered=512", progressRecords)
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
	replayed := collectStoredReplay(t, h.db, "s1", 0)
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
	replayed := collectStoredReplay(t, h.db, "s1", 0)
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
	recvSubscriptionReady(t, sub)

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
	replayed := collectStoredReplay(t, h.db, "s1", 0)
	if len(replayed) != 2 {
		t.Fatalf("persisted %d events, want 2 (no ephemeral)", len(replayed))
	}
	for _, ev := range replayed {
		if ev.GetClass() == corev1.EventClass_EVENT_CLASS_EPHEMERAL {
			t.Fatal("ephemeral event was persisted")
		}
	}
}

// --- publish-order (seq inversion) ----------------------------------------
//
// THE INCIDENT THESE COVER. Seq assignment was always serialized (BEGIN
// IMMEDIATE), but the fan-out publish ran after the transaction on the
// producer's own goroutine holding nothing. Two producers on one session could
// therefore commit as N-then-N+1 and publish as N+1-then-N. The daemon reads a
// non-increasing seq on a session as a terminal protocol violation and kills the
// session, mid-turn — seen twice on 2026-07-29 (seq=642 after 647, and seq=1043
// after 1044).

// concurrentProducer drives one producer connection from its own goroutine.
//
// IT PIPELINES DELIBERATELY: every batch is written without waiting for its ack,
// and a second goroutine drains the acks. Ack-per-batch would defeat the whole
// test — the ack is written AFTER the fan-out publish, so a producer that waits
// for it is serialized against its own publish and can never be mid-region while
// another producer publishes. Pipelining is what keeps both of the store's
// handler goroutines deep in ingestAndFan at the same time, which is the
// condition the production inversion needed.
func concurrentProducer(conn net.Conn, batches []*corev1.StoreWrite, ready *sync.WaitGroup, start <-chan struct{}) error {
	drained := make(chan error, 1)
	go func() {
		for i := range batches {
			m, err := recvMsg(conn)
			if err != nil {
				drained <- fmt.Errorf("batch %d ack: %w", i, err)
				return
			}
			if _, ok := m.(*corev1.StoreWriteAck); !ok {
				drained <- fmt.Errorf("batch %d: ack type = %T, want *StoreWriteAck", i, m)
				return
			}
		}
		drained <- nil
	}()

	ready.Done()
	<-start
	for i, batch := range batches {
		if err := sendMsg(conn, batch); err != nil {
			return fmt.Errorf("batch %d: %w", i, err)
		}
	}
	return <-drained
}

// runProducersConcurrently releases every producer at once from a channel
// barrier and waits for all of them. No sleeps: `ready` proves each goroutine
// reached the barrier, closing `start` releases them together, and `wg` bounds
// the act.
func runProducersConcurrently(t *testing.T, fns ...func(*sync.WaitGroup, <-chan struct{}) error) {
	t.Helper()
	var ready, done sync.WaitGroup
	ready.Add(len(fns))
	done.Add(len(fns))
	start := make(chan struct{})
	errs := make([]error, len(fns))
	for i, fn := range fns {
		go func() {
			defer done.Done()
			errs[i] = fn(&ready, start)
		}()
	}
	ready.Wait() // every producer is at the barrier
	close(start) // release them together
	done.Wait()
	for i, err := range errs {
		if err != nil {
			t.Fatalf("producer %d: %v", i, err)
		}
	}
}

// registerSubscriber opens a subscription from 0 and proves it is REGISTERED and
// live-tailing by round-tripping one persistent event through it. Returns the
// subscriber conn and the seq that handshake consumed, so a caller can assert
// only on what follows. No timing assumptions.
func registerSubscriber(t *testing.T, h *harness, session string) (net.Conn, uint64) {
	t.Helper()
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: session, FromSeq: 0})
	recvSubscriptionReady(t, sub)
	prod := h.dial(t)
	send(t, prod, write(vAssistantStream(t, session, "handshake")))
	if ack := recvAck(t, prod); ack.GetAccepted() != 1 {
		t.Fatalf("handshake ack accepted = %d, want 1", ack.GetAccepted())
	}
	ev := recvEvent(t, sub)
	if ev.GetSeq() == 0 {
		t.Fatal("handshake event arrived with seq=0")
	}
	return sub, ev.GetSeq()
}

// watchSeqOrder drains the subscriber CONCURRENTLY with the producers, checking
// monotonicity as each event lands, and returns a join func yielding the first
// violation (nil if none).
//
// Draining concurrently is not an optimization, it is what makes the test valid.
// Buffering every event to assert afterwards caps the batch count at the fanout
// buffer, and overrunning that buffer HARD-DISCONNECTS the subscriber
// (fanout.publish's slow-consumer path) — which surfaces as an EOF read error
// that looks like a failure but proves nothing about ordering. Reading as they
// arrive decouples volume from the buffer, and volume is what makes the
// inversion reproducible.
//
// `wantPersistent` counts only seq-bearing events; ephemerals are passed over
// (they carry no seq and so cannot violate the ordering).
func watchSeqOrder(sub net.Conn, floor uint64, wantPersistent int) func() error {
	result := make(chan error, 1)
	go func() {
		last := floor
		for seen := 0; seen < wantPersistent; {
			m, err := recvMsg(sub)
			if err != nil {
				result <- fmt.Errorf("after %d/%d persistent events: %w", seen, wantPersistent, err)
				return
			}
			ev, ok := m.(*corev1.Event)
			if !ok {
				result <- fmt.Errorf("after %d persistent events: frame type = %T, want *Event", seen, m)
				return
			}
			if ev.GetClass() == corev1.EventClass_EVENT_CLASS_EPHEMERAL {
				continue
			}
			if ev.GetSeq() <= last {
				result <- fmt.Errorf("persistent event %d: seq %d did not increase past %d — publish order inverted", seen, ev.GetSeq(), last)
				return
			}
			last = ev.GetSeq()
			seen++
		}
		result <- nil
	}()
	return func() error { return <-result }
}

// oneEventBatches builds n single-event batches from a per-index event factory.
func oneEventBatches(n int, event func(i int) *corev1.Event) []*corev1.StoreWrite {
	batches := make([]*corev1.StoreWrite, n)
	for i := range n {
		batches[i] = write(event(i))
	}
	return batches
}

func TestConcurrentProducersOnOneSessionPublishInSeqOrder(t *testing.T) {
	// Arrange: one session, two producers on different planes with distinct
	// dedup identities — the shim's stream plane and the sidecar's file plane,
	// which is exactly the pair that collided in production. Distinct uuids mean
	// nothing dedups, so every event is assigned a seq and must be published.
	// The fanout buffer is set well above the event count so a slow-consumer
	// disconnect can never masquerade as an ordering failure; the buffer is not
	// what is under test here.
	const perProducer = 1500
	h := start(t, 4*perProducer, testLogger())
	sub, handshakeSeq := registerSubscriber(t, h, "s1")

	streamConn, diskConn := h.dial(t), h.dial(t)
	streamBatches := oneEventBatches(perProducer, func(i int) *corev1.Event {
		return vAssistantStream(t, "s1", fmt.Sprintf("stream-%d", i))
	})
	diskBatches := oneEventBatches(perProducer, func(i int) *corev1.Event {
		return vAssistantDisk(t, "s1", fmt.Sprintf("disk-%d", i))
	})

	// Assert (armed first): the subscriber's stream is STRICTLY INCREASING. This
	// is the daemon's own invariant — dispatchEvent treats any non-increasing seq
	// on a session as a terminal protocol violation — checked off the same wire
	// the daemon reads.
	joinWatcher := watchSeqOrder(sub, handshakeSeq, 2*perProducer)

	// Act: both producers write the same session at once.
	runProducersConcurrently(t,
		func(ready *sync.WaitGroup, start <-chan struct{}) error {
			return concurrentProducer(streamConn, streamBatches, ready, start)
		},
		func(ready *sync.WaitGroup, start <-chan struct{}) error {
			return concurrentProducer(diskConn, diskBatches, ready, start)
		},
	)

	if err := joinWatcher(); err != nil {
		t.Fatal(err)
	}
}

func TestRejectedBatchReleasesTheIngestLock(t *testing.T) {
	// Arrange: a batch the db layer refuses outright. A persistent event with no
	// session_id is rejected inside Ingest (it has no seq space to belong to), so
	// ingestAndFan returns down its error branch — the one path that must still
	// release the lock it took on the way in.
	h := start(t, 0, testLogger())
	bad := h.dial(t)

	// Act: the rejected batch, then an ordinary batch on a DIFFERENT producer
	// connection — the real hazard is a leaked lock wedging every OTHER producer.
	send(t, bad, write(&corev1.Event{
		Class:   corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Plane:   corev1.Plane_PLANE_STREAM,
		Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{}},
	}))
	rejected := recvAck(t, bad)

	good := h.dial(t)
	send(t, good, write(vAssistantStream(t, "s1", "after-rejection")))
	served := recvAck(t, good) // a leaked lock hangs here until the read deadline

	// Assert: the rejection was reported loudly, and the store still serves. The
	// error branch is unchanged; only the unlock was added.
	if rejected.GetError() == "" {
		t.Fatal("rejected batch acked with an empty error; the loud rejection path changed")
	}
	if rejected.GetAccepted() != 0 {
		t.Fatalf("rejected ack accepted = %d, want 0", rejected.GetAccepted())
	}
	if served.GetAccepted() != 1 || served.GetLastSeq() != 1 {
		t.Fatalf("post-rejection ack = %+v, want accepted=1 last_seq=1", served)
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

func collectLogs(capacity int, verbose bool) (*logging.Logger, func() []string) {
	lines := make(chan string, capacity)
	logf := logging.New(channelWriter(lines), io.Discard, verbose)
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
	Message   string         `json:"message"`
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

func TestIngestVerboseLineLogsBatchFacts(t *testing.T) {
	// Arrange
	logf, drain := collectLogs(64, true)
	h := start(t, 0, logf)
	prod := h.dial(t)

	// Act: a two-event persistent batch.
	send(t, prod, write(vAssistantStream(t, "s1", "A"), vAssistantStream(t, "s1", "B")))
	recvAck(t, prod)

	// Assert: the server's durable-batch outcome carries the batch's facts.
	want := "persisted batch events=2 accepted=2 deduped=0 replayed=0 last_seq=2 ingest_ms="
	got := findLineContaining(drain(), "ingest", want)
	if !strings.Contains(got, want) {
		t.Fatalf("ingest line = %q, want message containing %q", got, want)
	}
}

func TestIngestVerboseLineSilentForEphemeralOnlyBatch(t *testing.T) {
	// Arrange
	logf, drain := collectLogs(64, true)
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

func TestIngestSuccessSilentWhenVerboseDisabled(t *testing.T) {
	logf, drain := collectLogs(64, false)
	h := start(t, 0, logf)
	prod := h.dial(t)

	send(t, prod, write(vAssistantStream(t, "s1", "A")))
	recvAck(t, prod)

	if got := findLine(drain(), "ingest"); got != "" {
		t.Fatalf("non-verbose ingest success logged %q, want silence", got)
	}
}

// --- subscriber termination -------------------------------------------------
//
// These fixtures hold the exact producer-side transition with a hook owned by
// the subscriber state machine.  A test only advances a gate after it has
// observed the preceding transition, so no outcome relies on a scheduler race
// or a duration being long enough.

type subscriberTerminalCapture struct {
	records chan subscriberTerminalRecord
}

func newSubscriberTerminalCapture() *subscriberTerminalCapture {
	return &subscriberTerminalCapture{records: make(chan subscriberTerminalRecord, 2)}
}

func (c *subscriberTerminalCapture) hook(record subscriberTerminalRecord) {
	c.records <- record
}

func (c *subscriberTerminalCapture) await(t *testing.T) subscriberTerminalRecord {
	t.Helper()
	select {
	case record := <-c.records:
		return record
	case <-time.After(time.Second):
		t.Fatal("subscriber terminal record was not emitted")
		return subscriberTerminalRecord{}
	}
}

func (c *subscriberTerminalCapture) assertExactlyOne(t *testing.T) {
	t.Helper()
	select {
	case extra := <-c.records:
		t.Fatalf("extra subscriber terminal record = %+v", extra)
	default:
	}
}

type subscriberGate struct {
	reached chan struct{}
	release chan struct{}
	once    sync.Once
}

func newSubscriberGate() *subscriberGate {
	return &subscriberGate{reached: make(chan struct{}), release: make(chan struct{})}
}

func (g *subscriberGate) wait() {
	g.once.Do(func() { close(g.reached) })
	<-g.release
}

func (g *subscriberGate) await(t *testing.T) {
	t.Helper()
	select {
	case <-g.reached:
	case <-time.After(time.Second):
		t.Fatal("subscriber gate was not reached")
	}
}

func (g *subscriberGate) open() { close(g.release) }

// nthSubscriberGate blocks one selected replay row without changing the
// preceding rows.  The counter runs only in serveSubscriber's replay owner.
type nthSubscriberGate struct {
	want    int
	seen    int
	mu      sync.Mutex
	blocked *subscriberGate
}

func (g *nthSubscriberGate) wait() {
	g.mu.Lock()
	g.seen++
	block := g.seen == g.want
	g.mu.Unlock()
	if block {
		g.blocked.wait()
	}
}

// writeFaultConn fails its next socket write after the caller releases the
// gate.  Reads remain delegated to the pipe, allowing the terminal owner to
// close the server side and prove the reader suppresses its self-close error.
type writeFaultConn struct {
	net.Conn
	gate      *subscriberGate
	err       error
	once      sync.Once
	readError chan struct{}
	readOnce  sync.Once
}

type readFaultConn struct {
	net.Conn
	release <-chan struct{}
	err     error
	noticed chan<- struct{}
	once    sync.Once
}

func (c *readFaultConn) Read([]byte) (int, error) {
	<-c.release
	c.once.Do(func() { close(c.noticed) })
	return 0, c.err
}

func (c *writeFaultConn) Write(p []byte) (int, error) {
	c.gate.wait()
	fail := false
	c.once.Do(func() { fail = true })
	if fail {
		return 0, c.err
	}
	return c.Conn.Write(p)
}

func (c *writeFaultConn) Read(p []byte) (int, error) {
	n, err := c.Conn.Read(p)
	if err != nil && c.readError != nil {
		c.readOnce.Do(func() { close(c.readError) })
	}
	return n, err
}

func seedSubscriberReplay(t *testing.T, h *harness, session string, count int) {
	t.Helper()
	events := make([]*corev1.Event, 0, count)
	for i := range count {
		events = append(events, vAssistantStream(t, session, fmt.Sprintf("terminal-%d", i)))
	}
	if _, err := h.db.Ingest("subscriber-terminal-test", events, nil); err != nil {
		t.Fatalf("seed replay: %v", err)
	}
}

func installSubscriberHooks(s *Server, capture *subscriberTerminalCapture, replayHook, tailHook func()) {
	s.subscriberHooksMu.Lock()
	s.subscriberHooks = subscriberHooks{
		onTerminal:      capture.hook,
		beforeReplayRow: replayHook,
		beforeTailWrite: tailHook,
	}
	s.subscriberHooksMu.Unlock()
}

func serveSubscriberAsync(s *Server, conn net.Conn, sub *corev1.Subscribe) <-chan struct{} {
	conn = &onceConn{Conn: conn}
	s.trackConn(conn)
	done := make(chan struct{})
	go func() {
		defer close(done)
		defer s.untrackConn(conn)
		s.serveSubscriber(conn, sub)
	}()
	return done
}

func awaitSubscriberDone(t *testing.T, done <-chan struct{}) {
	t.Helper()
	select {
	case <-done:
	case <-time.After(time.Second):
		t.Fatal("subscriber did not stop")
	}
}

func assertSubscriberTerminalLog(t *testing.T, logs []byte, want subscriberTerminalRecord, wantCause bool) {
	t.Helper()
	var terminals []loggedRecord
	for _, line := range bytes.Split(bytes.TrimSpace(logs), []byte("\n")) {
		var record loggedRecord
		if err := json.Unmarshal(line, &record); err != nil {
			t.Fatalf("server record is not JSON: %v", err)
		}
		if record.Operation == "subscribe-terminal" {
			terminals = append(terminals, record)
		}
		if record.Operation == "subscriber-read" && record.Level == "error" {
			t.Fatalf("self-close was logged as a subscriber-read error: %#v", record)
		}
	}
	if len(terminals) != 1 {
		t.Fatalf("terminal records = %d, want 1; logs=%s", len(terminals), logs)
	}
	record := terminals[0]
	if record.Session != want.SessionID || record.Context["subscriber"] != want.Peer {
		t.Fatalf("terminal record identity = session %q peer %#v, want session %q peer %q: %#v", record.Session, record.Context["subscriber"], want.SessionID, want.Peer, record)
	}
	for key, wantValue := range map[string]any{
		"terminal_owner":   want.Owner,
		"terminal_reason":  string(want.Reason),
		"replay_from_seq":  float64(want.FromSeq),
		"replay_first_seq": float64(want.FirstReplaySeq),
		"replay_last_seq":  float64(want.LastReplaySeq),
		"delivered":        float64(want.Delivered),
	} {
		if got := record.Context[key]; got != wantValue {
			t.Fatalf("terminal context[%q] = %#v, want %#v; record=%#v", key, got, wantValue, record)
		}
	}
	if wantCause && record.Context["error"] == "" {
		t.Fatalf("terminal record omitted loud error cause: %#v", record)
	}
	if !wantCause {
		if got, exists := record.Context["error"]; exists {
			t.Fatalf("expected terminal record included error %#v: %#v", got, record)
		}
	}
}

func TestSubscriberCloseBeforeFirstReplayRowTerminatesOnce(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false))
	seedSubscriberReplay(t, h, "close-before-replay", 1)
	capture, gate := newSubscriberTerminalCapture(), newSubscriberGate()
	installSubscriberHooks(h.srv, capture, gate.wait, nil)
	serverConn, clientConn := net.Pipe()
	t.Cleanup(func() { _ = clientConn.Close() })
	done := serveSubscriberAsync(h.srv, serverConn, &corev1.Subscribe{SessionId: "close-before-replay"})

	gate.await(t)
	if err := clientConn.Close(); err != nil {
		t.Fatalf("client close: %v", err)
	}
	record := capture.await(t)
	gate.open()
	awaitSubscriberDone(t, done)
	if record.Reason != subscriptionTerminalReason("client-eof") || record.Delivered != 0 {
		t.Fatalf("terminal record = %+v, want client EOF before replay delivery", record)
	}
	capture.assertExactlyOne(t)
	assertSubscriberTerminalLog(t, logs.Bytes(), record, false)
}

func TestSubscriberCloseMidReplayTerminatesOnce(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false))
	seedSubscriberReplay(t, h, "close-mid-replay", 2)
	capture, gate := newSubscriberTerminalCapture(), newSubscriberGate()
	secondRow := &nthSubscriberGate{want: 2, blocked: gate}
	installSubscriberHooks(h.srv, capture, secondRow.wait, nil)
	serverConn, clientConn := net.Pipe()
	t.Cleanup(func() { _ = clientConn.Close() })
	done := serveSubscriberAsync(h.srv, serverConn, &corev1.Subscribe{SessionId: "close-mid-replay"})
	if ev := recvEvent(t, clientConn); ev.GetSeq() != 1 {
		t.Fatalf("first replay seq = %d, want 1", ev.GetSeq())
	}
	gate.await(t)
	if err := clientConn.Close(); err != nil {
		t.Fatalf("client close: %v", err)
	}
	record := capture.await(t)
	gate.open()
	awaitSubscriberDone(t, done)
	if record.Reason != subscriptionTerminalReason("client-eof") || record.Delivered != 1 {
		t.Fatalf("terminal record = %+v, want client EOF after one replay row", record)
	}
	capture.assertExactlyOne(t)
	assertSubscriberTerminalLog(t, logs.Bytes(), record, false)
}

func TestSubscriberCloseDuringLiveTailTerminatesOnce(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false))
	capture, gate := newSubscriberTerminalCapture(), newSubscriberGate()
	installSubscriberHooks(h.srv, capture, nil, gate.wait)
	serverConn, clientConn := net.Pipe()
	t.Cleanup(func() { _ = clientConn.Close() })
	done := serveSubscriberAsync(h.srv, serverConn, &corev1.Subscribe{SessionId: "close-live-tail"})
	recvSubscriptionReady(t, clientConn)
	h.srv.fan.publish(vAssistantStream(t, "close-live-tail", "tail"))

	gate.await(t)
	if err := clientConn.Close(); err != nil {
		t.Fatalf("client close: %v", err)
	}
	record := capture.await(t)
	gate.open()
	awaitSubscriberDone(t, done)
	if record.Reason != subscriptionTerminalReason("client-eof") || record.Delivered != 0 {
		t.Fatalf("terminal record = %+v, want client EOF in live tail", record)
	}
	capture.assertExactlyOne(t)
	assertSubscriberTerminalLog(t, logs.Bytes(), record, false)
}

func TestSubscriberReplayWriteFailureIsLoudAndDoesNotSelfReportReaderClose(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false))
	seedSubscriberReplay(t, h, "replay-write-failure", 1)
	capture, gate := newSubscriberTerminalCapture(), newSubscriberGate()
	installSubscriberHooks(h.srv, capture, nil, nil)
	serverPipe, clientConn := net.Pipe()
	t.Cleanup(func() { _ = clientConn.Close() })
	injected := errors.New("injected replay write failure")
	done := serveSubscriberAsync(h.srv, &writeFaultConn{Conn: serverPipe, gate: gate, err: injected}, &corev1.Subscribe{SessionId: "replay-write-failure"})

	gate.await(t)
	gate.open()
	record := capture.await(t)
	awaitSubscriberDone(t, done)
	if record.Reason != subscriptionTerminalReason("replay-failure") || !errors.Is(record.Cause, injected) {
		t.Fatalf("terminal record = %+v, want loud replay write failure", record)
	}
	capture.assertExactlyOne(t)
	assertSubscriberTerminalLog(t, logs.Bytes(), record, true)
}

func TestSubscriberSimultaneousReadAndReplayWriteFailureTerminatesOnce(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false))
	seedSubscriberReplay(t, h, "simultaneous-read-write", 1)
	capture, gate := newSubscriberTerminalCapture(), newSubscriberGate()
	installSubscriberHooks(h.srv, capture, nil, nil)
	serverPipe, clientConn := net.Pipe()
	t.Cleanup(func() { _ = clientConn.Close() })
	readError := make(chan struct{})
	done := serveSubscriberAsync(h.srv, &writeFaultConn{
		Conn: serverPipe, gate: gate, err: errors.New("injected concurrent replay write failure"), readError: readError,
	}, &corev1.Subscribe{SessionId: "simultaneous-read-write"})

	gate.await(t)
	if err := clientConn.Close(); err != nil {
		t.Fatalf("client close: %v", err)
	}
	select {
	case <-readError:
	case <-time.After(time.Second):
		t.Fatal("subscriber read failure was not observed before write release")
	}
	record := capture.await(t)
	gate.open()
	awaitSubscriberDone(t, done)
	if record.Reason != subscriptionTerminalReason("client-eof") {
		t.Fatalf("terminal record = %+v, want reader-owned client EOF", record)
	}
	capture.assertExactlyOne(t)
	assertSubscriberTerminalLog(t, logs.Bytes(), record, false)
}

func TestSubscriberStoreShutdownTerminatesOnce(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false))
	capture, gate := newSubscriberTerminalCapture(), newSubscriberGate()
	installSubscriberHooks(h.srv, capture, nil, gate.wait)
	serverConn, clientConn := net.Pipe()
	t.Cleanup(func() { _ = clientConn.Close() })
	done := serveSubscriberAsync(h.srv, serverConn, &corev1.Subscribe{SessionId: "store-shutdown"})
	recvSubscriptionReady(t, clientConn)
	h.srv.fan.publish(vAssistantStream(t, "store-shutdown", "tail"))
	gate.await(t)

	// Server.Close owns this connection because the fixture registered it before
	// starting the subscriber.  The gated write makes shutdown occur during the
	// live-tail socket transition rather than at an arbitrary time.
	if err := h.srv.Close(); err != nil {
		t.Fatalf("store shutdown: %v", err)
	}
	gate.open()
	record := capture.await(t)
	awaitSubscriberDone(t, done)
	if record.Reason != subscriptionTerminalReason("server-shutdown") {
		t.Fatalf("terminal record = %+v, want server shutdown", record)
	}
	capture.assertExactlyOne(t)
	assertSubscriberTerminalLog(t, logs.Bytes(), record, false)
}

func TestSubscriberClientResetTerminatesOnce(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false))
	capture := newSubscriberTerminalCapture()
	readRelease, readNoticed := make(chan struct{}), make(chan struct{})
	installSubscriberHooks(h.srv, capture, nil, nil)
	serverPipe, clientConn := net.Pipe()
	t.Cleanup(func() { _ = clientConn.Close() })
	done := serveSubscriberAsync(h.srv, &readFaultConn{
		Conn: serverPipe, release: readRelease, err: syscall.ECONNRESET, noticed: readNoticed,
	}, &corev1.Subscribe{SessionId: "client-reset"})
	recvSubscriptionReady(t, clientConn)

	close(readRelease)
	select {
	case <-readNoticed:
	case <-time.After(time.Second):
		t.Fatal("injected reset was not read")
	}
	record := capture.await(t)
	awaitSubscriberDone(t, done)
	if record.Reason != subscriptionTerminalReason("client-reset") || record.Owner != "reader" || record.Cause != nil {
		t.Fatalf("terminal record = %+v, want client reset", record)
	}
	capture.assertExactlyOne(t)
	assertSubscriberTerminalLog(t, logs.Bytes(), record, false)
}

func TestSubscriberSlowConsumerTerminatesOnce(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 1, logging.New(&logs, io.Discard, false))
	capture, gate := newSubscriberTerminalCapture(), newSubscriberGate()
	installSubscriberHooks(h.srv, capture, nil, gate.wait)
	serverConn, clientConn := net.Pipe()
	t.Cleanup(func() { _ = clientConn.Close() })
	done := serveSubscriberAsync(h.srv, serverConn, &corev1.Subscribe{SessionId: "slow-consumer"})
	recvSubscriptionReady(t, clientConn)

	h.srv.fan.publish(vAssistantStream(t, "slow-consumer", "one"))
	gate.await(t)
	h.srv.fan.publish(vAssistantStream(t, "slow-consumer", "two"))
	h.srv.fan.publish(vAssistantStream(t, "slow-consumer", "three"))
	gate.open()
	record := capture.await(t)
	awaitSubscriberDone(t, done)
	if record.Reason != subscriptionTerminalReason("slow-consumer") || record.Owner != "fanout" {
		t.Fatalf("terminal record = %+v, want slow-consumer", record)
	}
	capture.assertExactlyOne(t)
	assertSubscriberTerminalLog(t, logs.Bytes(), record, false)
}

func TestSlowConsumerHardDisconnect(t *testing.T) {
	// Arrange: a tiny buffer and a subscriber that stops reading. The
	// workspace-aware requester owns this session-specific disconnect.
	h := start(t, 1, testLogger())
	prod := h.dial(t)
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})
	recvSubscriptionReady(t, sub)

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
	recvSubscriptionReady(t, sub)
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

// --- idempotent replay by write identity ------------------------------------

// identified stamps a producer write identity on a stream event, which is what
// makes re-delivering it a no-op rather than a second row.
func identified(ev *corev1.Event, writeID string) *corev1.Event {
	ev.WriteId = writeID
	return ev
}

func TestReplayedBatchIsAbsorbedWithoutDuplicateRows(t *testing.T) {
	// Arrange: a batch the store has already ingested. A producer whose ack was
	// lost to a store bounce cannot tell that from a batch that never arrived,
	// so it resends — here, on a fresh producer connection, exactly as a relink
	// does.
	h := start(t, 0, testLogger())
	first := h.dial(t)
	send(t, first, write(
		identified(vAssistantStream(t, "s1", "A"), "w-1"),
		identified(vAssistantStream(t, "s1", "B"), "w-2"),
	))
	if ack := recvAck(t, first); ack.GetAccepted() != 2 {
		t.Fatalf("first ack = %+v, want accepted=2", ack)
	}
	first.Close()

	// Act
	second := h.dial(t)
	send(t, second, write(
		identified(vAssistantStream(t, "s1", "A"), "w-1"),
		identified(vAssistantStream(t, "s1", "B"), "w-2"),
	))
	ack := recvAck(t, second)

	// Assert
	if ack.GetAccepted() != 0 || ack.GetDeduped() != 2 {
		t.Fatalf("replay ack = %+v, want accepted=0 deduped=2", ack)
	}
	if rows := collectStoredReplay(t, h.db, "s1", 0); len(rows) != 2 {
		t.Fatalf("store holds %d rows after a replayed batch, want 2", len(rows))
	}
}

func TestReplayedBatchIsNotFannedOutASecondTime(t *testing.T) {
	// Arrange: a live subscriber that has already been handed the batch.
	h := start(t, 0, testLogger())
	sub := h.dial(t)
	send(t, sub, &corev1.Subscribe{SessionId: "s1", FromSeq: 0})
	recvSubscriptionReady(t, sub)
	prod := h.dial(t)
	send(t, prod, write(identified(vAssistantStream(t, "s1", "A"), "w-1")))
	recvAck(t, prod)
	if got := recvEvent(t, sub); got.GetSeq() != 1 {
		t.Fatalf("first delivery seq = %d, want 1", got.GetSeq())
	}

	// Act: replay the delivered batch, then write a genuinely new event. The new
	// event is the barrier — no sleep is needed, because the subscriber's NEXT
	// frame is either the duplicate (a failure) or the new event (a pass).
	send(t, prod, write(identified(vAssistantStream(t, "s1", "A"), "w-1")))
	recvAck(t, prod)
	send(t, prod, write(identified(vAssistantStream(t, "s1", "C"), "w-2")))
	recvAck(t, prod)

	// Assert
	next := recvEvent(t, sub)
	if next.GetWriteId() != "w-2" || next.GetSeq() != 2 {
		t.Fatalf("subscriber's next frame = write_id=%q seq=%d, want the NEW event w-2 at seq 2 — the replay was re-delivered",
			next.GetWriteId(), next.GetSeq())
	}
}
