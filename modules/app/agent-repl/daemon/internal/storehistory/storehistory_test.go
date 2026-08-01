package storehistory

import (
	"context"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// THE DURABLE ROUTE TO A CONVERSATION.
//
// These tests stand a real store-shaped UDS server up and read it exactly as
// the daemon does: one Subscribe frame keyed by the VENDOR session uuid, then
// framed Events until a bound trips.

// fakeStore is a store-shaped server: it accepts one connection, reads the
// Subscribe frame, and writes back a canned event list.
type fakeStore struct {
	t  *testing.T
	ln net.Listener

	mu sync.Mutex
	// subscribed is the Subscribe frame the reader sent.
	subscribed *corev1.Subscribe
	// events are written in order once the subscription arrives.
	events []*corev1.Event
	// closeAfter closes the connection once every event is written, standing
	// in for a store that goes away mid-subscription.
	closeAfter bool
	// preamble is written before the events, standing in for any non-Event
	// frame that can share the subscription.
	preamble *corev1.Heartbeat
}

// newFakeStore listens on a short /tmp path: a t.TempDir()-derived socket path
// exceeds the 104-byte sun_path limit on macOS and fails to bind.
func newFakeStore(t *testing.T, events []*corev1.Event, closeAfter bool) *fakeStore {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "storehistory-")
	if err != nil {
		t.Fatalf("temp socket dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	ln, err := net.Listen("unix", filepath.Join(dir, "store.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { _ = ln.Close() })
	s := &fakeStore{t: t, ln: ln, events: events, closeAfter: closeAfter}
	go s.serve()
	return s
}

func (s *fakeStore) path() string { return s.ln.Addr().String() }

func (s *fakeStore) serve() {
	conn, err := s.ln.Accept()
	if err != nil {
		return
	}
	msg, err := wire.ReadAny(conn)
	if err != nil {
		_ = conn.Close()
		return
	}
	sub, ok := msg.(*corev1.Subscribe)
	if !ok {
		_ = conn.Close()
		return
	}
	s.mu.Lock()
	s.subscribed = sub
	events, closeAfter, preamble := s.events, s.closeAfter, s.preamble
	s.mu.Unlock()
	if preamble != nil {
		if err := wire.WriteAny(conn, preamble); err != nil {
			_ = conn.Close()
			return
		}
	}
	for _, ev := range events {
		if ev.GetSeq() != 0 && ev.GetSeq() <= sub.GetFromSeq() {
			continue
		}
		if err := wire.WriteAny(conn, ev); err != nil {
			_ = conn.Close()
			return
		}
	}
	if closeAfter {
		_ = conn.Close()
		return
	}
	// Otherwise hold the connection open, as a live-tailing store does. The
	// reader's idle window is what ends the replay.
	<-make(chan struct{})
}

func (s *fakeStore) subscription() *corev1.Subscribe {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.subscribed
}

// event is a seq-bearing store row. The payload kind is irrelevant here: this
// package moves rows, and translation happens a layer up.
func event(seq uint64) *corev1.Event {
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq}
}

// newReader builds a Reader over path with a fixed vendor resolution.
func newReader(t *testing.T, path string, lines *[]string) *Reader {
	t.Helper()
	var mu sync.Mutex
	return &Reader{
		Socket: path,
		Vendor: func(string) (string, bool) { return "vendor-uuid", true },
		Idle:   400 * time.Millisecond,
		Logf: func(f string, a ...any) {
			mu.Lock()
			*lines = append(*lines, fmt.Sprintf(f, a...))
			mu.Unlock()
		},
	}
}

// collect runs one replay and returns the seqs it delivered.
func collect(t *testing.T, r *Reader, fromSeq, toSeq uint64, maxEvents uint32) ([]uint64, Result, error) {
	t.Helper()
	var seqs []uint64
	res, err := r.ReplayHistory(context.Background(), "/ws", "s1", fromSeq, toSeq, maxEvents,
		func(ev *corev1.Event) { seqs = append(seqs, ev.GetSeq()) })
	return seqs, res, err
}

// --- the happy path ---------------------------------------------------------

func TestAReplayDeliversEveryPersistedEvent(t *testing.T) {
	// Arrange.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1), event(2), event(3)}, false)
	r := newReader(t, store.path(), &logged)

	// Act.
	seqs, _, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}
	if len(seqs) != 3 || seqs[0] != 1 || seqs[2] != 3 {
		t.Fatalf("delivered seqs = %v, want [1 2 3]", seqs)
	}
}

func TestADrainedReplayIsCompleteRatherThanTruncated(t *testing.T) {
	// Arrange — quiet on a workspace with no producer IS the end of history.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1)}, false)
	r := newReader(t, store.path(), &logged)

	// Act.
	_, res, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}
	if res.Truncated {
		t.Fatalf("a drained replay reported truncated=%v reason=%q", res.Truncated, res.Reason)
	}
}

func TestAReplaySubscribesUnderTheVendorSessionUuid(t *testing.T) {
	// Arrange — the store keys its seq space, dedup index, and fan-out on the
	// vendor uuid; any other id subscribes to a channel nothing publishes to.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1)}, false)
	r := newReader(t, store.path(), &logged)

	// Act.
	if _, _, err := collect(t, r, 0, 0, 0); err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}

	// Assert.
	if got := store.subscription().GetSessionId(); got != "vendor-uuid" {
		t.Fatalf("subscribed session_id = %q, want the vendor uuid", got)
	}
}

func TestAReplayCarriesTheCallersExclusiveFromSeq(t *testing.T) {
	// Arrange.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1), event(2), event(3)}, false)
	r := newReader(t, store.path(), &logged)

	// Act.
	seqs, _, err := collect(t, r, 2, 0, 0)

	// Assert.
	if err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}
	if got := store.subscription().GetFromSeq(); got != 2 {
		t.Fatalf("subscribed from_seq = %d, want 2", got)
	}
	if len(seqs) != 1 || seqs[0] != 3 {
		t.Fatalf("delivered seqs = %v, want [3]", seqs)
	}
}

func TestAReplayStopsAtItsExclusiveUpperBound(t *testing.T) {
	// Arrange — to_seq is the first seq the caller already covers.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1), event(2), event(3)}, false)
	r := newReader(t, store.path(), &logged)

	// Act.
	seqs, res, err := collect(t, r, 0, 3, 0)

	// Assert.
	if err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}
	if len(seqs) != 2 || seqs[1] != 2 {
		t.Fatalf("delivered seqs = %v, want [1 2]", seqs)
	}
	if res.Truncated {
		t.Fatalf("reaching to_seq reported truncated: %q", res.Reason)
	}
}

func TestAnEphemeralEventIsNotServedAsHistory(t *testing.T) {
	// Arrange — seq 0 is fanned to live subscribers and never persisted, so it
	// sits outside the seq space every floor and replay mark counts in.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(0), event(1)}, false)
	r := newReader(t, store.path(), &logged)

	// Act.
	seqs, _, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}
	if len(seqs) != 1 || seqs[0] != 1 {
		t.Fatalf("delivered seqs = %v, want [1]", seqs)
	}
}

// --- bounds and failures ----------------------------------------------------

func TestATrippedEventCapIsReportedAsTruncated(t *testing.T) {
	// Arrange — a cap that trips is never a quiet short answer.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1), event(2), event(3)}, false)
	r := newReader(t, store.path(), &logged)

	// Act.
	seqs, res, err := collect(t, r, 0, 0, 2)

	// Assert.
	if err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}
	if len(seqs) != 2 {
		t.Fatalf("delivered seqs = %v, want 2 events", seqs)
	}
	if !res.Truncated || !strings.Contains(res.Reason, "event cap") {
		t.Fatalf("result = %+v, want a truncated event-cap result", res)
	}
}

func TestAStoreThatClosesTheSubscriptionTruncatesTheReplay(t *testing.T) {
	// Arrange — whatever arrived is real, but it is not provably all of it.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1)}, true)
	r := newReader(t, store.path(), &logged)

	// Act.
	_, res, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}
	if !res.Truncated || !strings.Contains(res.Reason, "closed the subscription") {
		t.Fatalf("result = %+v, want a truncated closed-subscription result", res)
	}
}

func TestAnUnreachableStoreIsALoudError(t *testing.T) {
	// Arrange — silence would be indistinguishable from an empty conversation.
	var logged []string
	r := newReader(t, "/tmp/storehistory-nonexistent-abcdef.sock", &logged)

	// Act.
	_, _, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err == nil {
		t.Fatal("an unreachable store returned no error")
	}
}

func TestAnUnreachableStoreIsLoggedWithItsCause(t *testing.T) {
	// Arrange.
	var logged []string
	r := newReader(t, "/tmp/storehistory-nonexistent-abcdef.sock", &logged)

	// Act.
	_, _, _ = collect(t, r, 0, 0, 0)

	// Assert.
	for _, l := range logged {
		if strings.Contains(l, "durable history UNREADABLE") && strings.Contains(l, "dial failed") {
			return
		}
	}
	t.Fatalf("no loud unreadable-store line; lines=%v", logged)
}

func TestASessionWithNoVendorUuidCannotBeLocated(t *testing.T) {
	// Arrange — the store files a conversation's seq space under its vendor
	// uuid, so without one there is nothing to ask for.
	var logged []string
	store := newFakeStore(t, nil, false)
	r := newReader(t, store.path(), &logged)
	r.Vendor = func(string) (string, bool) { return "", false }

	// Act.
	_, _, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err == nil {
		t.Fatal("a session with no vendor uuid returned no error")
	}
	if !strings.Contains(err.Error(), "vendor session uuid") {
		t.Fatalf("error = %v, want it to name the missing vendor session uuid", err)
	}
}

func TestAReaderWithNoSocketConfiguredRefuses(t *testing.T) {
	// Arrange.
	var logged []string
	r := newReader(t, "", &logged)

	// Act.
	_, _, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err == nil {
		t.Fatal("a reader with no store socket returned no error")
	}
}

func TestAReaderWithNoLoggerRefuses(t *testing.T) {
	// Arrange — this path is the only account of a replay served without a
	// shim, so a silent one would be unreviewable.
	r := &Reader{Socket: "/tmp/x.sock", Vendor: func(string) (string, bool) { return "v", true }}

	// Act.
	_, err := r.ReplayHistory(context.Background(), "/ws", "s1", 0, 0, 0, func(*corev1.Event) {})

	// Assert.
	if err == nil {
		t.Fatal("a reader with no logger returned no error")
	}
}

func TestAReplayWithNoEventSinkRefuses(t *testing.T) {
	// Arrange.
	var logged []string
	r := newReader(t, "/tmp/x.sock", &logged)

	// Act.
	_, err := r.ReplayHistory(context.Background(), "/ws", "s1", 0, 0, 0, nil)

	// Assert.
	if err == nil {
		t.Fatal("a replay with no event sink returned no error")
	}
}

func TestAReaderWithNoVendorResolverRefuses(t *testing.T) {
	// Arrange.
	var logged []string
	r := newReader(t, "/tmp/x.sock", &logged)
	r.Vendor = nil

	// Act.
	_, _, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err == nil {
		t.Fatal("a reader with no vendor resolver returned no error")
	}
}

func TestANonEventStoreFrameIsSkippedRatherThanServed(t *testing.T) {
	// Arrange — this path serves persisted conversation history only.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1)}, false)
	store.preamble = &corev1.Heartbeat{SentAtMs: 1}
	r := newReader(t, store.path(), &logged)

	// Act.
	seqs, _, err := collect(t, r, 0, 0, 0)

	// Assert.
	if err != nil {
		t.Fatalf("ReplayHistory: %v", err)
	}
	if len(seqs) != 1 || seqs[0] != 1 {
		t.Fatalf("delivered seqs = %v, want [1]", seqs)
	}
}

func TestACancelledReplayReportsTheCancellation(t *testing.T) {
	// Arrange — the context owns the connection, so a cancelled resync
	// unblocks a read parked on a store that stopped answering.
	var logged []string
	store := newFakeStore(t, []*corev1.Event{event(1)}, false)
	r := newReader(t, store.path(), &logged)
	r.Idle = time.Minute
	ctx, cancel := context.WithCancel(context.Background())

	// Act — cancel from the sink, the moment the first event lands.
	_, err := r.ReplayHistory(ctx, "/ws", "s1", 0, 0, 0, func(*corev1.Event) { cancel() })
	defer cancel()

	// Assert.
	if err == nil {
		t.Fatal("a cancelled replay returned no error")
	}
	if !strings.Contains(err.Error(), "cancelled") {
		t.Fatalf("error = %v, want it to name the cancellation", err)
	}
}

func TestDefaultSocketPathIsTheLaunchdStoreSingleton(t *testing.T) {
	// Arrange — the same path a shim defaults to when spawned without
	// --store-socket.
	home, err := os.UserHomeDir()
	if err != nil {
		t.Skipf("no home dir: %v", err)
	}

	// Act.
	got, err := DefaultSocketPath()

	// Assert.
	if err != nil {
		t.Fatalf("DefaultSocketPath: %v", err)
	}
	if want := filepath.Join(home, ".cache", "agent-repl", "sock", "store.sock"); got != want {
		t.Fatalf("DefaultSocketPath = %q, want %q", got, want)
	}
}
