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
	"sync/atomic"
	"syscall"
	"testing"
	"time"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/wire"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// --- harness ---------------------------------------------------------------

type harness struct {
	srv *Server
	db  *db.DB
	// dbPath is the record database's own path, so a test can open a second
	// raw handle and seed columns whose production writer is owned elsewhere.
	dbPath string
	path   string
	done   <-chan struct{}
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
	return &harness{srv: srv, db: database, dbPath: dbPath, path: sockPath, done: done}
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

func recvDelivery(t *testing.T, conn net.Conn) *protocolv1.EntryDelivery {
	t.Helper()
	m := recv(t, conn)
	delivery, ok := m.(*protocolv1.EntryDelivery)
	if !ok {
		t.Fatalf("expected *EntryDelivery, got %T", m)
	}
	return delivery
}

func recvSubscriptionReady(t *testing.T, conn net.Conn) {
	t.Helper()
	if _, ok := recv(t, conn).(*protocolv1.ConnectionHeartbeat); !ok {
		t.Fatal("subscription readiness frame is not a ConnectionHeartbeat")
	}
}

// probeID mints a unique correlation id per awaitWrite call.
var probeID atomic.Uint64

// awaitWrite is the write barrier every producer-side test needs, and it exists
// because THERE IS NO ACK. `StoreWriteAck` was retired and `StoreEntryWrite`
// has no reply, so a test cannot wait on the write itself.
//
// It works because serveProducer reads and dispatches this connection's frames
// on ONE goroutine, in order: a HealthStatus for a probe sent after a write can
// only be produced once that write has been fully processed. That is a
// structural ordering fact about the handler, not a timing assumption.
func awaitWrite(t *testing.T, conn net.Conn) {
	t.Helper()
	id := fmt.Sprintf("probe-%d", probeID.Add(1))
	send(t, conn, &protocolv1.HealthCheck{RequestId: id})
	status, ok := recv(t, conn).(*protocolv1.HealthStatus)
	if !ok {
		t.Fatalf("write barrier reply type = %T, want *HealthStatus", status)
	}
	if status.GetRequestId() != id {
		t.Fatalf("write barrier reply request_id = %q, want %q", status.GetRequestId(), id)
	}
}

func collectStoredReplay(t *testing.T, database *db.DB, session string, fromSeq uint64) []*protocolv1.EntryDelivery {
	t.Helper()
	var deliveries []*protocolv1.EntryDelivery
	if _, err := database.ReplayFrom(context.Background(), session, fromSeq, func(delivery *protocolv1.EntryDelivery) error {
		deliveries = append(deliveries, delivery)
		return nil
	}); err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	return deliveries
}

// --- record fixtures -------------------------------------------------------

func streamPlane() *agentshimv1.InternalEntry {
	return &agentshimv1.InternalEntry{
		Plane: &agentshimv1.Plane{Plane: &agentshimv1.Plane_Stream{Stream: &agentshimv1.PlaneStream{}}},
	}
}

// turnBegan builds a bookkeeping record naming a turn, which is what the
// concurrency and ordering tests need: cheap, session-scoped, and carrying a
// value a test can tell one record from another by.
func turnBegan(session, turnID string) *agentshimv1.Entry {
	return &agentshimv1.Entry{
		Internal: streamPlane(),
		External: &protocolv1.ExternalEntry{
			SessionId: session,
			Entry: &protocolv1.ExternalEntry_Bookkeeping{Bookkeeping: &protocolv1.BookkeepingEntry{
				Kind: &protocolv1.BookkeepingEntry_TurnBegan{TurnBegan: &protocolv1.TurnBegan{TurnId: turnID}},
			}},
		},
	}
}

// userSaid builds a record belonging to a message, which is what a page counts.
func userSaid(session, messageID string) *agentshimv1.Entry {
	return &agentshimv1.Entry{
		Internal: streamPlane(),
		External: &protocolv1.ExternalEntry{
			SessionId: session,
			Entry: &protocolv1.ExternalEntry_Message{Message: &conversationv1.MessageEntry{
				MessageId:         messageID,
				TopLevelMessageId: messageID,
				Parent:            &conversationv1.MessageParent{Parent: &conversationv1.MessageParent_Root{Root: &conversationv1.MessageParentRoot{}}},
				Author:            &conversationv1.MessageAuthor{Author: &conversationv1.MessageAuthor_User{User: &conversationv1.AuthorUser{}}},
				Payload:           &conversationv1.MessageEntry_UserSaid{UserSaid: &conversationv1.UserSaid{}},
			}},
		},
	}
}

func identified(entry *agentshimv1.Entry, writeID string) *agentshimv1.Entry {
	entry.Internal.WriteId = writeID
	return entry
}

func storeWrite(entries ...*agentshimv1.Entry) *agentshimv1.StoreEntryWrite {
	return &agentshimv1.StoreEntryWrite{Producer: "test", Batch: &agentshimv1.EntryBatch{Entries: entries}}
}

// storedDelivery is the envelope the store publishes, built directly for the
// fan-out tests that drive publish() without going through ingest.
func storedDelivery(session string, seq uint64) *protocolv1.EntryDelivery {
	return &protocolv1.EntryDelivery{
		Delivery: &protocolv1.EntryDelivery_Stored{Stored: &protocolv1.StoredEntryDelivery{
			Seq:   seq,
			Entry: turnBegan(session, fmt.Sprintf("t-%d", seq)).GetExternal(),
		}},
	}
}

// deliveryTurn names the turn a delivered bookkeeping record carries, which is
// how a test tells two deliveries apart now that write_id is internal and never
// crosses the wire.
func deliveryTurn(delivery *protocolv1.EntryDelivery) string {
	return delivery.GetStored().GetEntry().GetBookkeeping().GetTurnBegan().GetTurnId()
}

// --- tests -----------------------------------------------------------------

func TestRoundTripWriteSubscribeReplay(t *testing.T) {
	// Arrange
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	// Act: write a two-record batch.
	send(t, prod, storeWrite(turnBegan("s1", "A"), turnBegan("s1", "B")))
	awaitWrite(t, prod)
	// Act: subscribe from 0 and read the replay.
	sub := h.dial(t)
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 0})
	d1 := recvDelivery(t, sub)
	d2 := recvDelivery(t, sub)
	recvSubscriptionReady(t, sub)
	// Assert
	if d1.GetStored().GetSeq() != 1 || d2.GetStored().GetSeq() != 2 {
		t.Fatalf("replayed seqs = [%d %d], want [1 2]", d1.GetStored().GetSeq(), d2.GetStored().GetSeq())
	}
}

func TestSubscriberReceivesTheExternalHalfOnly(t *testing.T) {
	// Arrange: a record whose internal half carries a write identity the daemon
	// must never see.
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	send(t, prod, storeWrite(identified(turnBegan("s1", "A"), "w-secret")))
	awaitWrite(t, prod)

	// Act
	sub := h.dial(t)
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 0})
	delivery := recvDelivery(t, sub)

	// Assert: what arrives is the delivery envelope wrapping the external half.
	// The internal half has no field on this type to occupy, so the guarantee is
	// the shape's rather than a filter's — and the write identity is nowhere in
	// the bytes on the wire.
	frame, err := proto.Marshal(delivery)
	if err != nil {
		t.Fatalf("marshal delivery: %v", err)
	}
	if bytes.Contains(frame, []byte("w-secret")) {
		t.Fatal("the producer's write identity crossed the shim→daemon wire")
	}
	if delivery.GetStored().GetEntry().GetSessionId() != "s1" {
		t.Fatalf("delivered session = %q, want s1", delivery.GetStored().GetEntry().GetSessionId())
	}
}

func TestEveryRecordInABatchIsPersisted(t *testing.T) {
	// Arrange: there is no live-only class of record any more. EventClass was
	// retired and no message on the WRITE surface can say "hand this over
	// without storing it", so every entry in an EntryBatch is a durable write.
	h := start(t, 0, testLogger())
	prod := h.dial(t)

	// Act
	send(t, prod, storeWrite(turnBegan("s1", "A"), userSaid("s1", "m-1"), turnBegan("s1", "B")))
	awaitWrite(t, prod)

	// Assert
	if rows := collectStoredReplay(t, h.db, "s1", 0); len(rows) != 3 {
		t.Fatalf("store holds %d records, want 3 — every record in a batch is durable", len(rows))
	}
}

func TestSubscribeReadyProvesRegistrationBeforeAnImmediateProducerWrite(t *testing.T) {
	// Arrange: an empty store makes readiness the first subscriber frame.
	h := start(t, 0, testLogger())
	sub := h.dial(t)
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 0})

	// Act: the readiness frame is the registration barrier, then another socket
	// writes without any delay.
	recvSubscriptionReady(t, sub)
	prod := h.dial(t)
	send(t, prod, storeWrite(turnBegan("s1", "after-ready")))
	awaitWrite(t, prod)

	// Assert: the record cannot have overtaken subscriber registration.
	if seq := recvDelivery(t, sub).GetStored().GetSeq(); seq != 1 {
		t.Fatalf("live delivery seq = %d, want 1", seq)
	}
}

func TestHeartbeatCanPrecedeTheFirstProducerWrite(t *testing.T) {
	// Arrange: startup recovery established the producer socket, but no source
	// file changed yet, so the sidecar has no StoreEntryWrite with which to
	// declare the connection's role.
	h := start(t, 0, testLogger())
	prod := h.dial(t)

	// Act: idle liveness traffic arrives first, then a real producer batch.
	send(t, prod, &protocolv1.ConnectionHeartbeat{SentAtMs: 42})
	echo, ok := recv(t, prod).(*protocolv1.ConnectionHeartbeat)
	if !ok {
		t.Fatalf("heartbeat reply type = %T, want *ConnectionHeartbeat", echo)
	}
	send(t, prod, storeWrite(turnBegan("s1", "A")))
	awaitWrite(t, prod)

	// Assert: the preamble stayed connected and the first write was ingested.
	if echo.GetSentAtMs() != 42 {
		t.Fatalf("heartbeat sent_at_ms = %d, want 42", echo.GetSentAtMs())
	}
	if rows := collectStoredReplay(t, h.db, "s1", 0); len(rows) != 1 {
		t.Fatalf("store holds %d records, want 1", len(rows))
	}
}

func TestHealthCheckCanPrecedeTheFirstProducerWrite(t *testing.T) {
	// Arrange: health is the first intentional frame on the recovered producer
	// socket, before a file change provides a StoreEntryWrite.
	h := start(t, 0, testLogger())
	prod := h.dial(t)

	// Act: assert a correlated health reply, then write on the same connection.
	send(t, prod, &protocolv1.HealthCheck{RequestId: "health-before-write"})
	status, ok := recv(t, prod).(*protocolv1.HealthStatus)
	if !ok {
		t.Fatalf("health reply type = %T, want *HealthStatus", status)
	}
	send(t, prod, storeWrite(turnBegan("s1", "A")))
	awaitWrite(t, prod)

	// Assert: health was correlated and did not discard the producer preamble.
	if status.GetRequestId() != "health-before-write" || !status.GetHealthy() || status.GetComponent() != "shim-store" {
		t.Fatalf("health status = %+v, want correlated healthy shim-store status", status)
	}
	if rows := collectStoredReplay(t, h.db, "s1", 0); len(rows) != 1 {
		t.Fatalf("store holds %d records, want 1", len(rows))
	}
}

func TestEmptySubscribeLogsCanonicalProtocolRejection(t *testing.T) {
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}))
	conn := h.dial(t)
	send(t, conn, &protocolv1.Subscribe{})
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
	send(t, prod, storeWrite(turnBegan("s1", "A"), turnBegan("s1", "B"), turnBegan("s1", "C")))
	awaitWrite(t, prod)
	// Act: subscribe from_seq=1 (exclusive) → expect seqs 2,3.
	sub := h.dial(t)
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 1})
	d1 := recvDelivery(t, sub)
	d2 := recvDelivery(t, sub)
	recvSubscriptionReady(t, sub)
	// Assert
	if d1.GetStored().GetSeq() != 2 || d2.GetStored().GetSeq() != 3 {
		t.Fatalf("replay from_seq=1 gave [%d %d], want [2 3]", d1.GetStored().GetSeq(), d2.GetStored().GetSeq())
	}
}

func TestLargeReplayStreamsInOrderWithBoundedProgressLogs(t *testing.T) {
	// Arrange: one batch near the observed incident scale's first progress
	// boundary. The store must emit the first row before advancing through the
	// query and must not emit one diagnostic per row.
	const recordCount = 513
	logf, drain := collectLogs(128, true)
	h := start(t, 0, logf)
	prod := h.dial(t)
	entries := make([]*agentshimv1.Entry, 0, recordCount)
	for i := range recordCount {
		entries = append(entries, turnBegan("s1", fmt.Sprintf("replay-%04d", i)))
	}
	send(t, prod, storeWrite(entries...))
	awaitWrite(t, prod)

	// Act
	sub := h.dial(t)
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 0})
	for want := uint64(1); want <= recordCount; want++ {
		if got := recvDelivery(t, sub).GetStored().GetSeq(); got != want {
			t.Fatalf("streamed seq=%d, want=%d", got, want)
		}
	}
	recvSubscriptionReady(t, sub)
	// A live record proves serveSubscriber finished the replay and entered its
	// tail loop, so the completion record is present without a timing sleep.
	send(t, prod, storeWrite(turnBegan("s1", "tail-proof")))
	awaitWrite(t, prod)
	if got := recvDelivery(t, sub).GetStored().GetSeq(); got != recordCount+1 {
		t.Fatalf("tail proof seq=%d, want=%d", got, recordCount+1)
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

func TestCrashReplayIdempotency(t *testing.T) {
	// Arrange: a producer whose write outcome it never learned — which, with no
	// ack on the surface at all, is now EVERY write — cannot tell a batch that
	// landed from one that never arrived, so it resends.
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	replayable := func() *agentshimv1.StoreEntryWrite {
		return storeWrite(
			identified(turnBegan("s1", "A"), "w-1"),
			identified(turnBegan("s1", "B"), "w-2"),
		)
	}

	// Act: the identical batch twice.
	send(t, prod, replayable())
	awaitWrite(t, prod)
	send(t, prod, replayable())
	awaitWrite(t, prod)

	// Assert: the write identity made the repeat a no-op, not two more rows.
	if rows := collectStoredReplay(t, h.db, "s1", 0); len(rows) != 2 {
		t.Fatalf("store holds %d rows after a replayed batch, want 2", len(rows))
	}
}

func TestReplayedBatchIsNotFannedOutASecondTime(t *testing.T) {
	// Arrange: a live subscriber that has already been handed the batch.
	h := start(t, 0, testLogger())
	sub := h.dial(t)
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 0})
	recvSubscriptionReady(t, sub)
	prod := h.dial(t)
	send(t, prod, storeWrite(identified(turnBegan("s1", "A"), "w-1")))
	awaitWrite(t, prod)
	if got := recvDelivery(t, sub); got.GetStored().GetSeq() != 1 {
		t.Fatalf("first delivery seq = %d, want 1", got.GetStored().GetSeq())
	}

	// Act: replay the delivered batch, then write a genuinely new record. The
	// new record is the barrier — no sleep is needed, because the subscriber's
	// NEXT frame is either the duplicate (a failure) or the new one (a pass).
	send(t, prod, storeWrite(identified(turnBegan("s1", "A"), "w-1")))
	awaitWrite(t, prod)
	send(t, prod, storeWrite(identified(turnBegan("s1", "C"), "w-2")))
	awaitWrite(t, prod)

	// Assert. The turn id is what distinguishes them: write_id is INTERNAL and
	// never crosses this wire, so a test cannot look for it here either.
	next := recvDelivery(t, sub)
	if deliveryTurn(next) != "C" || next.GetStored().GetSeq() != 2 {
		t.Fatalf("subscriber's next frame = turn=%q seq=%d, want the NEW record C at seq 2 — the replay was re-delivered",
			deliveryTurn(next), next.GetStored().GetSeq())
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
// IT PIPELINES BY CONSTRUCTION NOW. The write direction has no reply at all, so
// a producer cannot serialize itself against its own publish even if it wanted
// to — which is exactly the condition the production inversion needed, and it
// is now the only condition available. One probe at the END is the completion
// barrier: serveProducer dispatches this connection's frames in order, so its
// reply proves every preceding batch was processed.
func concurrentProducer(conn net.Conn, batches []*agentshimv1.StoreEntryWrite, ready *sync.WaitGroup, start <-chan struct{}) error {
	ready.Done()
	<-start
	for i, batch := range batches {
		if err := sendMsg(conn, batch); err != nil {
			return fmt.Errorf("batch %d: %w", i, err)
		}
	}
	id := fmt.Sprintf("drain-%d", probeID.Add(1))
	if err := sendMsg(conn, &protocolv1.HealthCheck{RequestId: id}); err != nil {
		return fmt.Errorf("completion probe: %w", err)
	}
	m, err := recvMsg(conn)
	if err != nil {
		return fmt.Errorf("completion probe reply: %w", err)
	}
	status, ok := m.(*protocolv1.HealthStatus)
	if !ok {
		return fmt.Errorf("completion probe reply type = %T, want *HealthStatus", m)
	}
	if status.GetRequestId() != id {
		return fmt.Errorf("completion probe reply request_id = %q, want %q", status.GetRequestId(), id)
	}
	return nil
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
// live-tailing by round-tripping one record through it. Returns the subscriber
// conn and the seq that handshake consumed, so a caller can assert only on what
// follows. No timing assumptions.
func registerSubscriber(t *testing.T, h *harness, session string) (net.Conn, uint64) {
	t.Helper()
	sub := h.dial(t)
	send(t, sub, &protocolv1.Subscribe{SessionId: session, FromSeq: 0})
	recvSubscriptionReady(t, sub)
	prod := h.dial(t)
	send(t, prod, storeWrite(turnBegan(session, "handshake")))
	awaitWrite(t, prod)
	delivery := recvDelivery(t, sub)
	if delivery.GetStored().GetSeq() == 0 {
		t.Fatal("handshake delivery arrived with seq=0")
	}
	return sub, delivery.GetStored().GetSeq()
}

// watchSeqOrder drains the subscriber CONCURRENTLY with the producers, checking
// monotonicity as each record lands, and returns a join func yielding the first
// violation (nil if none).
//
// Draining concurrently is not an optimization, it is what makes the test valid.
// Buffering every record to assert afterwards caps the batch count at the fanout
// buffer, and overrunning that buffer HARD-DISCONNECTS the subscriber
// (fanout.publish's slow-consumer path) — which surfaces as an EOF read error
// that looks like a failure but proves nothing about ordering. Reading as they
// arrive decouples volume from the buffer, and volume is what makes the
// inversion reproducible.
func watchSeqOrder(sub net.Conn, floor uint64, want int) func() error {
	result := make(chan error, 1)
	go func() {
		last := floor
		for seen := 0; seen < want; {
			m, err := recvMsg(sub)
			if err != nil {
				result <- fmt.Errorf("after %d/%d records: %w", seen, want, err)
				return
			}
			delivery, ok := m.(*protocolv1.EntryDelivery)
			if !ok {
				result <- fmt.Errorf("after %d records: frame type = %T, want *EntryDelivery", seen, m)
				return
			}
			seq := delivery.GetStored().GetSeq()
			if seq <= last {
				result <- fmt.Errorf("record %d: seq %d did not increase past %d — publish order inverted", seen, seq, last)
				return
			}
			last = seq
			seen++
		}
		result <- nil
	}()
	return func() error { return <-result }
}

// oneEntryBatches builds n single-record batches from a per-index factory.
func oneEntryBatches(n int, entry func(i int) *agentshimv1.Entry) []*agentshimv1.StoreEntryWrite {
	batches := make([]*agentshimv1.StoreEntryWrite, n)
	for i := range n {
		batches[i] = storeWrite(entry(i))
	}
	return batches
}

func TestConcurrentProducersOnOneSessionPublishInSeqOrder(t *testing.T) {
	// Arrange: one session, two producers — the shim's stream plane and the
	// sidecar's file plane, which is exactly the pair that collided in
	// production. The fanout buffer is set well above the record count so a
	// slow-consumer disconnect can never masquerade as an ordering failure; the
	// buffer is not what is under test here.
	const perProducer = 1500
	h := start(t, 4*perProducer, testLogger())
	sub, handshakeSeq := registerSubscriber(t, h, "s1")

	streamConn, fileConn := h.dial(t), h.dial(t)
	streamBatches := oneEntryBatches(perProducer, func(i int) *agentshimv1.Entry {
		return turnBegan("s1", fmt.Sprintf("stream-%d", i))
	})
	fileBatches := oneEntryBatches(perProducer, func(i int) *agentshimv1.Entry {
		entry := turnBegan("s1", fmt.Sprintf("file-%d", i))
		entry.Internal.Plane = &agentshimv1.Plane{Plane: &agentshimv1.Plane_File{File: &agentshimv1.PlaneFile{}}}
		return entry
	})

	// Assert (armed first): the subscriber's stream is STRICTLY INCREASING. This
	// is the daemon's own invariant — it treats any non-increasing seq on a
	// session as a terminal protocol violation — checked off the same wire the
	// daemon reads.
	joinWatcher := watchSeqOrder(sub, handshakeSeq, 2*perProducer)

	// Act: both producers write the same session at once.
	runProducersConcurrently(t,
		func(ready *sync.WaitGroup, start <-chan struct{}) error {
			return concurrentProducer(streamConn, streamBatches, ready, start)
		},
		func(ready *sync.WaitGroup, start <-chan struct{}) error {
			return concurrentProducer(fileConn, fileBatches, ready, start)
		},
	)

	if err := joinWatcher(); err != nil {
		t.Fatal(err)
	}
}

func TestRejectedBatchDropsTheProducerConnection(t *testing.T) {
	// Arrange: a batch the db layer refuses outright — a record with no
	// session_id, which has no seq space to belong to.
	//
	// THE CONNECTION DROP IS THE WHOLE SIGNAL, and that is the cost of the
	// missing ack. The store cannot tell the producer its batch was refused,
	// so silence would be indistinguishable from success and the producer
	// would go on writing into a store discarding its work.
	var logs bytes.Buffer
	h := start(t, 0, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server"}))
	bad := h.dial(t)

	// Act
	send(t, bad, storeWrite(turnBegan("", "no-session")))

	// Assert: the connection ends rather than staying open in false health.
	if err := bad.SetReadDeadline(time.Now().Add(5 * time.Second)); err != nil {
		t.Fatalf("setting read deadline: %v", err)
	}
	if _, err := wire.ReadAny(bad); err == nil {
		t.Fatal("a rejected batch left the producer connection open, which is indistinguishable from success")
	}

	// Assert: the refusal was loud, and it says why the producer cannot be told.
	// The server is stopped first so the log sink has one writer, then this
	// reader — no concurrent access, and no timing assumption either.
	if err := h.srv.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	<-h.done
	record, found := findLoggedRecord(t, logs.Bytes(), "store-write", "error")
	if !found {
		t.Fatalf("rejected-batch record missing: %s", logs.String())
	}
	if !strings.Contains(record.Message, "REJECTED") || !strings.Contains(record.Message, "no ack") {
		t.Fatalf("rejection message = %q, want it to state the refusal and the missing ack", record.Message)
	}
}

func TestRejectedBatchReleasesTheIngestLock(t *testing.T) {
	// Arrange: the error branch of ingestAndFan is the one path that must still
	// release the lock it took on the way in. A leaked lock wedges every OTHER
	// producer, which is the real hazard.
	h := start(t, 0, testLogger())
	bad := h.dial(t)

	// Act: the rejected batch, then an ordinary batch on a DIFFERENT connection.
	send(t, bad, storeWrite(turnBegan("", "no-session")))
	good := h.dial(t)
	send(t, good, storeWrite(turnBegan("s1", "after-rejection")))
	awaitWrite(t, good) // a leaked lock hangs here until the read deadline

	// Assert: the store still serves.
	if rows := collectStoredReplay(t, h.db, "s1", 0); len(rows) != 1 {
		t.Fatalf("store holds %d records after a rejection, want 1", len(rows))
	}
}

// collectLogs returns a log sink plus a drain. Every line the server emits for
// a batch is logged before the barrier probe is answered, so draining after
// awaitWrite sees exactly that batch's lines with no timing assumptions.
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

	// Act: a two-record batch.
	send(t, prod, storeWrite(turnBegan("s1", "A"), turnBegan("s1", "B")))
	awaitWrite(t, prod)

	// Assert: the server's durable-batch outcome carries the batch's facts.
	want := "persisted batch entries=2 accepted=2 replayed=0 unconverted=0 last_seq=2 ingest_ms="
	got := findLineContaining(drain(), "ingest", want)
	if !strings.Contains(got, want) {
		t.Fatalf("ingest line = %q, want message containing %q", got, want)
	}
}

func TestIngestVerboseLineSilentForAnEmptyBatch(t *testing.T) {
	// Arrange
	logf, drain := collectLogs(64, true)
	h := start(t, 0, logf)
	prod := h.dial(t)

	// Act: a batch with neither records nor a cursor advance.
	send(t, prod, storeWrite())
	awaitWrite(t, prod)

	// Assert: no ingest line for a batch that never touched the DB.
	if got := findLine(drain(), "ingest"); got != "" {
		t.Fatalf("empty batch logged %q, want silence", got)
	}
}

func TestIngestSuccessSilentWhenVerboseDisabled(t *testing.T) {
	logf, drain := collectLogs(64, false)
	h := start(t, 0, logf)
	prod := h.dial(t)

	send(t, prod, storeWrite(turnBegan("s1", "A")))
	awaitWrite(t, prod)

	if got := findLine(drain(), "ingest"); got != "" {
		t.Fatalf("non-verbose ingest success logged %q, want silence", got)
	}
}

func TestReplayedBatchIsReportedAtNormalVerbosity(t *testing.T) {
	// Arrange: a replay is rare by construction and says the write identity
	// held, so it is a normal-log fact rather than narration.
	logf, drain := collectLogs(64, false)
	h := start(t, 0, logf)
	prod := h.dial(t)
	send(t, prod, storeWrite(identified(turnBegan("s1", "A"), "w-1")))
	awaitWrite(t, prod)

	// Act
	send(t, prod, storeWrite(identified(turnBegan("s1", "A"), "w-1")))
	awaitWrite(t, prod)

	// Assert
	if got := findLineContaining(drain(), "ingest", "REPLAYED batch absorbed idempotently"); got == "" {
		t.Fatal("a replayed batch was not reported at normal verbosity")
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
	entries := make([]*agentshimv1.Entry, 0, count)
	for i := range count {
		entries = append(entries, turnBegan(session, fmt.Sprintf("terminal-%d", i)))
	}
	if _, err := h.db.Ingest("subscriber-terminal-test", &agentshimv1.EntryBatch{Entries: entries}); err != nil {
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

func serveSubscriberAsync(s *Server, conn net.Conn, sub *protocolv1.Subscribe) <-chan struct{} {
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
	done := serveSubscriberAsync(h.srv, serverConn, &protocolv1.Subscribe{SessionId: "close-before-replay"})

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
	done := serveSubscriberAsync(h.srv, serverConn, &protocolv1.Subscribe{SessionId: "close-mid-replay"})
	if seq := recvDelivery(t, clientConn).GetStored().GetSeq(); seq != 1 {
		t.Fatalf("first replay seq = %d, want 1", seq)
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
	done := serveSubscriberAsync(h.srv, serverConn, &protocolv1.Subscribe{SessionId: "close-live-tail"})
	recvSubscriptionReady(t, clientConn)
	h.srv.fan.publish(storedDelivery("close-live-tail", 1))

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
	done := serveSubscriberAsync(h.srv, &writeFaultConn{Conn: serverPipe, gate: gate, err: injected}, &protocolv1.Subscribe{SessionId: "replay-write-failure"})

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
	}, &protocolv1.Subscribe{SessionId: "simultaneous-read-write"})

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
	done := serveSubscriberAsync(h.srv, serverConn, &protocolv1.Subscribe{SessionId: "store-shutdown"})
	recvSubscriptionReady(t, clientConn)
	h.srv.fan.publish(storedDelivery("store-shutdown", 1))
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
	}, &protocolv1.Subscribe{SessionId: "client-reset"})
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
	done := serveSubscriberAsync(h.srv, serverConn, &protocolv1.Subscribe{SessionId: "slow-consumer"})
	recvSubscriptionReady(t, clientConn)

	h.srv.fan.publish(storedDelivery("slow-consumer", 1))
	gate.await(t)
	h.srv.fan.publish(storedDelivery("slow-consumer", 2))
	h.srv.fan.publish(storedDelivery("slow-consumer", 3))
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
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 0})
	recvSubscriptionReady(t, sub)

	// Handshake so the subscriber is registered and live.
	send(t, prod, storeWrite(turnBegan("s1", "P1")))
	awaitWrite(t, prod)
	recvDelivery(t, sub)

	// Act: blast a batch of large records while the subscriber never reads
	// again. Padding makes each frame ~1KiB so a modest count reliably overflows
	// the OS socket buffer plus the bounded per-subscriber buffer, and the store
	// hard-disconnects — without the ingest cost of a huge batch.
	pad := strings.Repeat("x", 1024)
	big := make([]*agentshimv1.Entry, 0, 2000)
	for i := range 2000 {
		big = append(big, turnBegan("s1", fmt.Sprintf("u%04d%s", i, pad)))
	}
	send(t, prod, storeWrite(big...))

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

func recvCursorList(t *testing.T, conn net.Conn) *agentshimv1.CursorList {
	t.Helper()
	m := recv(t, conn)
	cl, ok := m.(*agentshimv1.CursorList)
	if !ok {
		t.Fatalf("expected *CursorList, got %T", m)
	}
	return cl
}

func TestCursorQueryReturnsAllPersistedCursors(t *testing.T) {
	// Arrange: a producer commits a batch carrying a cursor advance.
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	sw := storeWrite(turnBegan("s1", "A"))
	sw.Batch.CursorAdvance = &agentshimv1.CursorState{FileId: "10:20", Path: "/x/y.jsonl", Offset: 42, Carry: []byte("tail")}
	send(t, prod, sw)
	awaitWrite(t, prod)

	// Act: a fresh connection recovers cursors (empty file_id = all).
	cq := h.dial(t)
	send(t, cq, &agentshimv1.CursorQuery{})
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

func TestCursorQueryDeclaresOpenTasksUnanswerable(t *testing.T) {
	// Arrange: `OpenTaskState.started` carried the TaskStarted event that opened
	// the task and is retired, so the message can say when a task was last
	// active but not WHICH task it is. The store also has nothing left to derive
	// one from.
	//
	// The reply must SAY it cannot answer rather than return an empty set that
	// reads as "no tasks are open" — which is exactly the distinction
	// open_tasks_authoritative exists to make.
	h := start(t, 0, testLogger())

	// Act
	cq := h.dial(t)
	send(t, cq, &agentshimv1.CursorQuery{})
	list := recvCursorList(t, cq)

	// Assert
	if list.GetOpenTasksAuthoritative() {
		t.Fatal("the store attested authoritative open-task state it cannot compute")
	}
	if len(list.GetOpenTasks()) != 0 {
		t.Fatalf("open_tasks = %d, want 0 — no identity-less entries may be invented", len(list.GetOpenTasks()))
	}
}

func TestCursorQueryByFileID(t *testing.T) {
	// Arrange: two persisted cursors.
	h := start(t, 0, testLogger())
	prod := h.dial(t)
	for _, fid := range []string{"1:1", "2:2"} {
		sw := storeWrite(turnBegan("s1", "E"+fid))
		sw.Batch.CursorAdvance = &agentshimv1.CursorState{FileId: fid, Path: "/p/" + fid, Offset: 7}
		send(t, prod, sw)
		awaitWrite(t, prod)
	}
	// Act: query one file_id.
	cq := h.dial(t)
	send(t, cq, &agentshimv1.CursorQuery{FileId: "2:2"})
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
	send(t, cq, &agentshimv1.CursorQuery{FileId: "nope"})
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
	send(t, sub, &protocolv1.Subscribe{SessionId: "s1", FromSeq: 0})
	recvSubscriptionReady(t, sub)
	// Round-trip a write so we know the server is actively serving this session.
	prod := h.dial(t)
	send(t, prod, storeWrite(turnBegan("s1", "P1")))
	awaitWrite(t, prod)
	recvDelivery(t, sub) // subscriber is live

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
