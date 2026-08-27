// write_test.go covers the sidecar's own write path against a socket that
// merely DRAINS frames.
//
// That is enough, and it is enough for a reason worth stating: the schema
// retired StoreWriteAck without a successor, so a write is one-way and there is
// no reply for a fake store to have to imitate. What these tests can still pin
// is everything on this side of the socket — that the cursor rides with the
// records it was read at, that a diagnostic queued while the link was down is
// carried by the next batch, and that a failed write leaves the queue exactly as
// it found it.
package main

import (
	"io"
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
	"agentrepl/wire"
)

// drainingStore accepts one connection and decodes every frame it is sent,
// recording them for assertion. It replies to nothing, which is exactly what the
// real write path now expects.
type drainingStore struct {
	sock string

	mu       sync.Mutex
	received []*storev1.WriteBatchRequest
	done     chan struct{}
}

func newDrainingStore(t *testing.T) *drainingStore {
	t.Helper()
	// A unix socket path is capped at ~104 bytes by the kernel, well under what
	// t.TempDir() produces from a long test name — hence a short temp root of
	// our own rather than the usual helper.
	dir, err := os.MkdirTemp("", "sc")
	if err != nil {
		t.Fatalf("creating socket dir: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	sock := filepath.Join(dir, "s.sock")
	listener, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatalf("listening on %s: %v", sock, err)
	}
	store := &drainingStore{sock: sock, done: make(chan struct{})}
	go func() {
		defer close(store.done)
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		for {
			msg, err := wire.ReadAny(conn)
			if err != nil {
				return
			}
			write, ok := msg.(*storev1.WriteBatchRequest)
			if !ok {
				continue
			}
			store.mu.Lock()
			store.received = append(store.received, write)
			store.mu.Unlock()
		}
	}()
	t.Cleanup(func() {
		listener.Close()
		<-store.done
	})
	return store
}

// writes returns the frames the store has decoded so far, waiting until at least
// `want` have arrived rather than sleeping for them.
func (s *drainingStore) writes(t *testing.T, want int) []*storev1.WriteBatchRequest {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	for {
		s.mu.Lock()
		got := append([]*storev1.WriteBatchRequest(nil), s.received...)
		s.mu.Unlock()
		if len(got) >= want {
			return got
		}
		if time.Now().After(deadline) {
			t.Fatalf("store received %d frame(s), want at least %d", len(got), want)
		}
	}
}

// connectedSidecar builds a sidecar whose producer connection is established
// against a draining store, without going through establish (which would also
// demand a cursor recovery this fake does not answer).
func connectedSidecar(t *testing.T) (*sidecar, *drainingStore, func() []string) {
	t.Helper()
	store := newDrainingStore(t)
	logf, read := capturingLog()
	s := newSidecar(store.sock, nil, t.TempDir(), logf)
	if err := s.store.Connect(); err != nil {
		t.Fatalf("connect: %v", err)
	}
	t.Cleanup(func() { s.store.Close() })
	s.link = linkUp
	s.cursors = map[string]*storev1.CursorState{}
	return s, store, read
}

func testEntry(session string) *storev1.StoreEntry {
	return convert.ProducerDiagnostic(
		convert.Attribution{SessionID: session, ProducedAtMs: 1},
		"test:"+session, "test-op", "detail")
}

// The cursor rides with the records ON PURPOSE: split them and a crash between
// the two either loses records or duplicates them.
func TestABatchCarriesItsCursorAdvanceWithItsRecords(t *testing.T) {
	// Arrange.
	s, store, _ := connectedSidecar(t)
	res := tail.PollResult{
		Entries: []*storev1.StoreEntry{testEntry("s1")},
		Next:    &storev1.CursorState{FileId: "1:2", Path: "/t/a.jsonl", Offset: 128},
	}

	// Act.
	if err := s.writeBatch(res); err != nil {
		t.Fatalf("writeBatch: %v", err)
	}

	// Assert.
	got := store.writes(t, 1)[0]
	if got.GetProducer() != convert.Producer {
		t.Fatalf("producer = %q, want %q", got.GetProducer(), convert.Producer)
	}
	if len(got.GetBatch().GetEntries()) != 1 {
		t.Fatalf("entries = %d, want 1", len(got.GetBatch().GetEntries()))
	}
	if got.GetBatch().GetCursorAdvance().GetOffset() != 128 {
		t.Fatalf("cursor advance = %d, want it riding with the records", got.GetBatch().GetCursorAdvance().GetOffset())
	}
}

// A diagnostic queued while the link was down rides out on the next tail batch,
// which is what keeps the outbox from needing a write path of its own.
func TestQueuedDiagnosticsRideOutOnTheNextBatch(t *testing.T) {
	// Arrange.
	s, store, _ := connectedSidecar(t)
	s.diagnostics.enqueue(logging.Diagnostic{
		Timestamp: time.UnixMilli(1), PID: 1, Level: "warn", Verbosity: "normal",
		Operation: "sidecar.tail.poll", Message: "something", Session: "s1",
	})

	// Act.
	if err := s.writeBatch(tail.PollResult{Entries: []*storev1.StoreEntry{testEntry("s1")}}); err != nil {
		t.Fatalf("writeBatch: %v", err)
	}

	// Assert.
	got := store.writes(t, 1)[0]
	if len(got.GetBatch().GetEntries()) != 2 {
		t.Fatalf("entries = %d, want the record plus the queued diagnostic", len(got.GetBatch().GetEntries()))
	}
	if queued := len(s.diagnostics.snapshot()); queued != 0 {
		t.Fatalf("diagnostics still queued after a successful batch: %d", queued)
	}
}

// A write that fails must leave the outbox exactly as it found it, so the retry
// reuses the same record and its write identity rather than minting a second.
func TestAFailedBatchLeavesTheDiagnosticQueueIntact(t *testing.T) {
	// Arrange — connected, then the socket is closed underneath the write.
	s, _, _ := connectedSidecar(t)
	s.diagnostics.enqueue(logging.Diagnostic{
		Timestamp: time.UnixMilli(1), PID: 1, Level: "warn", Verbosity: "normal",
		Operation: "sidecar.tail.poll", Message: "something", Session: "s1",
	})
	retained := s.diagnostics.snapshot()[0]
	s.store.Close()

	// Act.
	err := s.writeBatch(tail.PollResult{Entries: []*storev1.StoreEntry{testEntry("s1")}})

	// Assert.
	if err == nil {
		t.Fatal("a write against a closed connection reported success")
	}
	queued := s.diagnostics.snapshot()
	if len(queued) != 1 || queued[0] != retained {
		t.Fatalf("queue after a failed write = %#v, want the exact retained record", queued)
	}
}

// Inferred records — a LOST sweep, an outage report — go out as a CURSOR-LESS
// batch, because they were not read at a file position and there is no reader
// position that could become durable with them.
func TestInferredRecordsAreWrittenWithoutACursorAdvance(t *testing.T) {
	// Arrange.
	s, store, _ := connectedSidecar(t)

	// Act.
	s.emit([]*storev1.StoreEntry{testEntry("s1")})

	// Assert.
	got := store.writes(t, 1)[0]
	if got.GetBatch().GetCursorAdvance() != nil {
		t.Fatal("an inferred record carried a cursor advance it was never read at")
	}
}

func TestEmittingNothingWritesNothing(t *testing.T) {
	// Arrange.
	s, store, _ := connectedSidecar(t)

	// Act.
	s.emit(nil)

	// Assert.
	store.mu.Lock()
	defer store.mu.Unlock()
	if len(store.received) != 0 {
		t.Fatalf("an empty emit wrote %d frame(s)", len(store.received))
	}
}

// A failed emit is reported per session it concerns, so the failure is visible
// from inside the workspace whose records were lost.
// A failed emit is still REPORTED, loudly, for every record it lost.
//
// IT IS NO LONGER REPORTED PER SESSION. The fanout read the session off
// protocol.v1 ExternalEntry.session_id, a field store.v1 StoreEntry does not
// carry in any form — so the attribution was deleted out from under the report,
// not the report itself. Losing the report entirely would be the failure this
// covers; losing the session name is the schema gap it now documents.
func TestAFailedEmitIsStillReported(t *testing.T) {
	// Arrange.
	s, _, read := connectedSidecar(t)
	s.store.Close()

	// Act.
	s.emit([]*storev1.StoreEntry{testEntry("s1"), testEntry("s2")})

	// Assert.
	if got := linesContaining(read(), "inferred record write failed"); len(got) != 1 {
		t.Fatalf("failure lines = %v, want exactly one report for the failed batch", got)
	}
}

// Each watched file kind gets the reader that understands its format, and a kind
// with no reader is a bug in discovery rather than a file to skip.
func TestEveryWatchedKindHasItsOwnReader(t *testing.T) {
	// Arrange.
	s, _, _ := connectedSidecar(t)
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})

	kinds := []tail.Kind{
		tail.KindSessionTranscript,
		tail.KindAgentTranscript,
		tail.KindWorkflowJournal,
		tail.KindShellSpool,
	}

	// Act / Assert.
	for _, kind := range kinds {
		if s.newHandler(kind, log) == nil {
			t.Fatalf("kind %d has no reader", kind)
		}
	}
	defer func() {
		if recover() == nil {
			t.Fatal("an unsupported kind was silently given a reader")
		}
	}()
	s.newHandler(tail.Kind(99), log)
}
