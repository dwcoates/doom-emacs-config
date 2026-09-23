package server

import (
	"bytes"
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-store/internal/logging"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
)

func TestMain(m *testing.M) {
	// Nothing in this package can reach a vendor, and the suite states so
	// rather than relying on that remaining true.
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	os.Exit(m.Run())
}

// ---- test doubles ----

// syncBuffer is a log sink safe to read from the test goroutine while server
// goroutines write to it.
type syncBuffer struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (b *syncBuffer) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.Write(p)
}

func (b *syncBuffer) String() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.String()
}

type logRecord struct {
	Operation string         `json:"operation"`
	Level     string         `json:"level"`
	Message   string         `json:"message"`
	RequestID string         `json:"request_id"`
	Context   map[string]any `json:"context"`
}

func records(t *testing.T, sink *syncBuffer) []logRecord {
	t.Helper()
	var out []logRecord
	for _, line := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		if line == "" {
			continue
		}
		var rec logRecord
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("decode log line %q: %v", line, err)
		}
		out = append(out, rec)
	}
	return out
}

// closeOrFail closes c and fails the test if the close fails. A subject's own
// close is part of what it observes: a stream, body or listener that will not
// close cleanly is a fault the subject would otherwise hide.
func closeOrFail(t testing.TB, c io.Closer) {
	t.Helper()
	if err := c.Close(); err != nil {
		t.Errorf("closing %T: %v", c, err)
	}
}

func findRecord(t *testing.T, sink *syncBuffer, operation, level string) (logRecord, bool) {
	t.Helper()
	for _, rec := range records(t, sink) {
		if rec.Operation == operation && rec.Level == level {
			return rec, true
		}
	}
	return logRecord{}, false
}

// fakeStore is the storage layer as internal/server needs it: every answer is
// staged by the test, and the two gates make the replay-to-live handoff
// observable without a single sleep.
type fakeStore struct {
	mu sync.Mutex

	writeResult WriteResult
	writeErr    error
	writes      []string

	opened  OpenedPage
	openErr error

	page    *storev1.ReadAgentPageSuccess
	pageErr error

	since    []LineWritten
	sinceErr error
	// sinceEntered is closed the first time LinesSince is called, which is the
	// signal that the handler has already subscribed to the fan-out.
	sinceEntered chan struct{}
	// sinceRelease, when non-nil, blocks LinesSince until the test closes it.
	sinceRelease chan struct{}

	bashRun    BashRunReplay
	bashRunErr error
	// bashRunEntered is closed the first time BashRun is called, which is the
	// signal that the handler has already subscribed to the bash fan-out.
	bashRunEntered chan struct{}
	// bashRunRelease, when non-nil, blocks BashRun until the test closes it.
	bashRunRelease chan struct{}

	live    *storev1.GetLiveWorkSuccess
	liveErr error
	// liveFor is the session the last LiveWork call was scoped to, and
	// liveAsked whether one was made at all.
	liveFor   string
	liveAsked bool

	cursors      []*storev1.CursorState
	cursorsErr   error
	cursorsFor   *string
	cursorsScope bool

	// the residue shape catalog, and what the last listing asked for
	shapes         []*storev1.ResidueShapeRow
	shapesErr      error
	shapesKind     *string
	shapesLimit    uint32
	shapesExample  bool
	shapesRequests int
	// shapesWritten is every observation list the server handed WriteBatch.
	shapesWritten [][]*storev1.ShapeObservation

	closed bool
}

// scopedLiveWork is a GetLiveWork request naming a session, which every
// request must: the store never answers one unscoped.
func scopedLiveWork() *storev1.GetLiveWorkRequest {
	return &storev1.GetLiveWorkRequest{Session: &conversationv1.AgentId{Value: "main-1"}}
}

func newFakeStore() *fakeStore {
	return &fakeStore{
		opened:         OpenedPage{Page: &storev1.AgentSessionPage{Boundary: &storev1.AgentSessionPage_Floor{Floor: &storev1.ReadAgentPageFloor{}}}},
		page:           &storev1.ReadAgentPageSuccess{Boundary: &storev1.ReadAgentPageSuccess_Floor{Floor: &storev1.ReadAgentPageFloor{}}},
		live:           &storev1.GetLiveWorkSuccess{},
		sinceEntered:   make(chan struct{}),
		bashRunEntered: make(chan struct{}),
	}
}

func (f *fakeStore) WriteBatch(_ context.Context, producer string, _ *storev1.EntryBatch, shapes []*storev1.ShapeObservation) (WriteResult, error) {
	f.mu.Lock()
	f.writes = append(f.writes, producer)
	f.shapesWritten = append(f.shapesWritten, shapes)
	result, err := f.writeResult, f.writeErr
	f.mu.Unlock()
	return result, err
}

func (f *fakeStore) OpenPage(context.Context, string, uint32, *storev1.StoreItemPointer) (OpenedPage, error) {
	return f.opened, f.openErr
}

func (f *fakeStore) ReadPage(context.Context, string, uint32, *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error) {
	return f.page, f.pageErr
}

func (f *fakeStore) LinesSince(context.Context, string, uint64) ([]LineWritten, error) {
	f.mu.Lock()
	select {
	case <-f.sinceEntered:
	default:
		close(f.sinceEntered)
	}
	release := f.sinceRelease
	f.mu.Unlock()
	if release != nil {
		<-release
	}
	return f.since, f.sinceErr
}

func (f *fakeStore) BashRun(context.Context, string) (BashRunReplay, error) {
	f.mu.Lock()
	select {
	case <-f.bashRunEntered:
	default:
		close(f.bashRunEntered)
	}
	release := f.bashRunRelease
	f.mu.Unlock()
	if release != nil {
		<-release
	}
	return f.bashRun, f.bashRunErr
}

func (f *fakeStore) LiveWork(_ context.Context, session string) (*storev1.GetLiveWorkSuccess, error) {
	f.mu.Lock()
	f.liveAsked = true
	f.liveFor = session
	f.mu.Unlock()
	return f.live, f.liveErr
}

func (f *fakeStore) Cursors(_ context.Context, fileID *string) ([]*storev1.CursorState, error) {
	f.mu.Lock()
	f.cursorsScope = true
	f.cursorsFor = fileID
	f.mu.Unlock()
	return f.cursors, f.cursorsErr
}

func (f *fakeStore) ResidueShapes(_ context.Context, kind *string, limit uint32, includeExample bool) ([]*storev1.ResidueShapeRow, error) {
	f.mu.Lock()
	f.shapesRequests++
	f.shapesKind, f.shapesLimit, f.shapesExample = kind, limit, includeExample
	f.mu.Unlock()
	return f.shapes, f.shapesErr
}

func (f *fakeStore) Close() error {
	f.closed = true
	return nil
}

// ---- harness ----

type harness struct {
	server *Server
	client storev1connect.ShimStoreClient
	// stream is the h2c client the watch tests use, so streaming is exercised
	// on the same cleartext HTTP/2 the store's socket offers.
	stream storev1connect.ShimStoreClient
	url    string
	logs   *syncBuffer
	http   *http.Client
}

// h2cClient dials cleartext HTTP/2 with prior knowledge: no TLS anywhere, which
// is what the store's unix socket offers.
func h2cClient() *http.Client {
	return &http.Client{Transport: &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, network, addr)
		},
	}}
}

// newHarness mounts the service on an HTTP/1.1 test server, which is the
// transport a Connect stream uses over the store's unix socket too.
func newHarness(t *testing.T, store Store, watchBuffer int) *harness {
	t.Helper()
	sink := &syncBuffer{}
	log := logging.New(sink, io.Discard, true)
	srv := New(store, log, watchBuffer)
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	t.Cleanup(func() {
		// 1s: an in-process httptest.Server over a fake Store shuts down in
		// single-digit milliseconds even under -race; see flush_test.go's
		// openBound for the same package-level basis.
		ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
		defer cancel()
		if err := srv.Shutdown(ctx); err != nil {
			t.Errorf("shutdown: %v", err)
		}
	})
	return &harness{
		server: srv,
		client: storev1connect.NewShimStoreClient(ts.Client(), ts.URL),
		stream: storev1connect.NewShimStoreClient(h2cClient(), ts.URL),
		url:    ts.URL,
		logs:   sink,
		http:   ts.Client(),
	}
}

func agentID(value string) *conversationv1.AgentId {
	return &conversationv1.AgentId{Value: value}
}

func line(agent, pointer string, seq uint64) LineWritten {
	return LineWritten{
		AgentID: agent,
		Line: &storev1.StoreLineAt{
			At:   &storev1.StoreItemPointer{Value: pointer},
			Line: &storev1.StorePageLine{PageAgentId: agentID(agent)},
		},
		WriteSeq: seq,
	}
}

func validEntry(writeID, upsertKey string) *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane:     &storev1.Plane{Plane: &storev1.Plane_Stream{Stream: &storev1.PlaneStream{}}},
		WriteId:   writeID,
		UpsertKey: upsertKey,
		Entry: &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{
			AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{ServeableFrame: &storev1.StorePageLine{PageAgentId: agentID("a1")}},
		}},
	}
}

func attributedEntry(writeID, upsertKey, topLevel, book string) *storev1.StoreEntry {
	entry := validEntry(writeID, upsertKey)
	entry.GetAgentUpdate().TopLevel = agentID(topLevel)
	entry.GetAgentUpdate().GetServeableFrame().PageAgentId = agentID(book)
	return entry
}

func writeOne(t *testing.T, h *harness) {
	t.Helper()
	res, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "claude-shim:test",
		Batch:    &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}},
	}))
	if err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("WriteBatch = %v, want the success arm", res.Msg.GetResult())
	}
}

func openSession(t *testing.T, h *harness, agent string) string {
	t.Helper()
	res, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID(agent), PageSize: 10,
	}))
	if err != nil {
		t.Fatalf("OpenAgentSession: %v", err)
	}
	success := res.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenAgentSession = %v, want the success arm", res.Msg.GetResult())
	}
	return success.GetWatch().GetValue()
}

// ---- WriteBatch ----

func TestBatchAttributionUsesSingularKeysForOneAgent(t *testing.T) {
	// Arrange.
	batch := &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		attributedEntry("w1", "u1", "agent-1", "agent-1"),
	}}

	// Act.
	got := batchAttribution(batch)

	// Assert.
	if got.AgentID != "agent-1" || got.BookAgentID != "agent-1" {
		t.Fatalf("batchAttribution = %+v, want singular agent and book", got)
	}
	if len(got.AgentIDs) != 0 || len(got.BookAgentIDs) != 0 {
		t.Fatalf("batchAttribution = %+v, want no aggregate keys", got)
	}
}

func TestBatchAttributionUsesSortedAggregateKeysForAMixedBatch(t *testing.T) {
	// Arrange.
	batch := &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		attributedEntry("w1", "u1", "agent-b", "book-b"),
		attributedEntry("w2", "u2", "agent-a", "book-a"),
	}}

	// Act.
	got := batchAttribution(batch)

	// Assert.
	if got.AgentID != "" || got.BookAgentID != "" {
		t.Fatalf("batchAttribution = %+v, want no arbitrary singular identity", got)
	}
	if strings.Join(got.AgentIDs, ",") != "agent-a,agent-b" {
		t.Fatalf("agent ids = %q, want deterministic aggregate attribution", got.AgentIDs)
	}
	if strings.Join(got.BookAgentIDs, ",") != "book-a,book-b" {
		t.Fatalf("book ids = %q, want deterministic aggregate attribution", got.BookAgentIDs)
	}
}

func TestBatchAttributionDoesNotSynthesizeAnAgentFromTheBook(t *testing.T) {
	// Arrange.
	batch := &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		attributedEntry("w1", "u1", "", "book-1"),
	}}

	// Act.
	got := batchAttribution(batch)

	// Assert.
	if got.AgentID != "" || len(got.AgentIDs) != 0 {
		t.Fatalf("batchAttribution = %+v, want no invented agent identity", got)
	}
	if got.BookAgentID != "book-1" {
		t.Fatalf("book agent id = %q, want book-1", got.BookAgentID)
	}
}

func TestWriteBatchSuccessArmOnACommittedBatch(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.writeResult = WriteResult{Written: 1}
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "claude-shim:s1",
		Batch:    &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch = %v, want nil", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want the success arm", res.Msg.GetResult())
	}
}

func TestWriteBatchSuccessArmCarriesLegacyBookConflictSkips(t *testing.T) {
	// Arrange. The batch committed its new entry and skipped one legacy
	// book-conflict; the skip rides the SUCCESS arm, never a failure.
	store := newFakeStore()
	store.writeResult = WriteResult{Written: 1, Skipped: []SkippedEntry{
		{UpsertKey: "u1", FromBook: "agent-1", ToBook: "agent-2"},
	}}
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "shim-claude-sidecar",
		Batch:    &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch = %v, want nil", err)
	}
	success := res.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("result = %v, want the success arm", res.Msg.GetResult())
	}
	if len(success.GetSkipped()) != 1 {
		t.Fatalf("skipped = %d, want 1", len(success.GetSkipped()))
	}
	if got := success.GetSkipped()[0]; got.GetUpsertKey() != "u1" || got.GetFromBook() != "agent-1" || got.GetToBook() != "agent-2" {
		t.Fatalf("skipped[0] = {%s %s %s}, want {u1 agent-1 agent-2}", got.GetUpsertKey(), got.GetFromBook(), got.GetToBook())
	}
}

func TestWriteBatchAnswersAnAbsorbedReplayWithTheSameSuccessArm(t *testing.T) {
	// Arrange. Every write_id landed before; nothing new was written.
	store := newFakeStore()
	store.writeResult = WriteResult{Absorbed: 3}
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "claude-shim:s1",
		Batch:    &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch = %v, want nil", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want the success arm for an absorbed replay", res.Msg.GetResult())
	}
}

func TestWriteBatchMapsAStorageFailureToTheFailureArm(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.writeErr = fmt.Errorf("%w: disk is gone", ErrStorage)
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "claude-shim:s1",
		Batch:    &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}},
	}))

	// Assert. A refusal is an HTTP 200 with the typed arm, never a Connect error.
	if err != nil {
		t.Fatalf("WriteBatch = %v, want nil (a refusal is not a transport error)", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
	// internal/db logged this failure already; the server must not record it a
	// second time, only trace that it answered the failure arm.
	if _, ok := findRecord(t, h.logs, "store.rpc.write-batch", "error"); ok {
		t.Fatalf("records = %+v, want no second error record for a db failure", records(t, h.logs))
	}
	var found bool
	for _, rec := range records(t, h.logs) {
		if rec.Operation == "store.rpc.write-batch" && rec.Level == "debug" && rec.Context["refusal_site"] == SiteDatabaseFailure {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("records = %+v, want a verbose trace at site %q", records(t, h.logs), SiteDatabaseFailure)
	}
}

func TestWriteBatchRefusesARequestNamingNoProducer(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Batch: &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}},
	}))

	// Assert. The store must not be touched by a refused request.
	if err != nil {
		t.Fatalf("WriteBatch = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
	if len(store.writes) != 0 {
		t.Fatalf("store writes = %d, want 0: validation runs before the store is touched", len(store.writes))
	}
}

func TestWriteBatchRefusalIsLoggedAtItsSite(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	if _, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "claude-shim:s1",
		Batch:    &storev1.EntryBatch{},
	})); err != nil {
		t.Fatalf("WriteBatch = %v, want nil", err)
	}

	// Assert.
	rec, ok := findRecord(t, h.logs, "store.rpc.write-batch", "warn")
	if !ok || rec.Context["refusal_site"] != SiteBatchEmpty {
		t.Fatalf("records = %+v, want a warn record at site %q", records(t, h.logs), SiteBatchEmpty)
	}
}

// ---- OpenAgentSession ----

func TestOpenAgentSessionAnswersAPageAndAToken(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.opened = OpenedPage{Page: &storev1.AgentSessionPage{
		Lines:    []*storev1.StoreLineAt{line("a1", "p1", 7).Line},
		Boundary: &storev1.AgentSessionPage_Floor{Floor: &storev1.ReadAgentPageFloor{}},
	}, PinSeq: 7}
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID("a1"), PageSize: 10,
	}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}
	success := res.Msg.GetSuccess()
	if success == nil || len(success.GetPage().GetLines()) != 1 || success.GetWatch().GetValue() == "" {
		t.Fatalf("result = %v, want one page line and a minted token", res.Msg.GetResult())
	}
}

func TestOpenAgentSessionRecordsCarryTheAgentAndBook(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.opened = OpenedPage{Page: &storev1.AgentSessionPage{}, PinSeq: 7}
	h := newHarness(t, store, 0)

	// Act.
	if _, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID("agent-1"), PageSize: 10,
	})); err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}

	// Assert.
	found := false
	for _, rec := range records(t, h.logs) {
		if rec.Operation != "store.rpc.open-agent-session" {
			continue
		}
		found = true
		if rec.Context["agent_id"] != "agent-1" || rec.Context["book_agent_id"] != "agent-1" {
			t.Fatalf("record = %+v, want request-scoped agent and book", rec)
		}
	}
	if !found {
		t.Fatal("no open-agent-session record found")
	}
}

// openPageOnly runs the one-shot open a caller uses when no watch will follow,
// and asserts the answer carries no token.
func openPageOnly(t *testing.T, h *harness, agent string) {
	t.Helper()
	res, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID(agent), PageSize: 10, PageOnly: true,
	}))
	if err != nil {
		t.Fatalf("OpenAgentSession: %v", err)
	}
	success := res.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenAgentSession = %v, want the success arm", res.Msg.GetResult())
	}
	if success.GetWatch() != nil {
		t.Fatalf("watch = %v, want UNSET for a page-only open", success.GetWatch())
	}
}

// TestOpenAgentSessionMintsNoTokenForAPageOnlyRead: the caller said no watch
// follows, so there is nothing to mint and nothing to answer with.
func TestOpenAgentSessionMintsNoTokenForAPageOnlyRead(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.opened = OpenedPage{Page: &storev1.AgentSessionPage{
		Lines:    []*storev1.StoreLineAt{line("a1", "p1", 7).Line},
		Boundary: &storev1.AgentSessionPage_Floor{Floor: &storev1.ReadAgentPageFloor{}},
	}, PinSeq: 7}
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID("a1"), PageSize: 10, PageOnly: true,
	}))

	// Assert. The page is served in full; only the token is withheld.
	if err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}
	success := res.Msg.GetSuccess()
	if success == nil || len(success.GetPage().GetLines()) != 1 {
		t.Fatalf("result = %v, want one page line", res.Msg.GetResult())
	}
	if success.GetWatch() != nil {
		t.Fatalf("watch = %v, want UNSET for a page-only open", success.GetWatch())
	}
}

// TestOpenAgentSessionRetainsTheTokenAnOrdinaryOpenMinted: the ordinary open is
// untouched — a watch is coming, so the registry holds the token for it.
func TestOpenAgentSessionRetainsTheTokenAnOrdinaryOpenMinted(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	token := openSession(t, h, "a1")

	// Assert.
	if token == "" {
		t.Fatalf("watch token = %q, want a minted token", token)
	}
	if got := h.server.tokens.outstanding(); got != 1 {
		t.Fatalf("outstanding = %d, want 1", got)
	}
}

// TestWatchRefusesTheBookOfAPageOnlyOpen: a page-only open leaves the caller
// with no token, so the watch it cannot address meets the ordinary refusal
// rather than any arm of its own.
func TestWatchRefusesTheBookOfAPageOnlyOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)
	openPageOnly(t, h, "a1")

	// Act.
	err := startWatch(h, context.Background(), "").refusal(t)

	// Assert.
	if connect.CodeOf(err) != connect.CodeNotFound {
		t.Fatalf("code = %v (err %v), want %v", connect.CodeOf(err), err, connect.CodeNotFound)
	}
	rec, ok := findRecord(t, h.logs, "store.rpc.watch-agent-session", "warn")
	if !ok || rec.Context["refusal_site"] != SiteTokenEmpty {
		t.Fatalf("records = %+v, want a warn record at site %q", records(t, h.logs), SiteTokenEmpty)
	}
}

// TestOutstandingTokensIsZeroAfterAOneTurnSessionsPageOnlyOpens: the two
// one-shot reads a single turn performs — the opening page and the teardown's
// book head — leave the registry exactly as they found it.
func TestOutstandingTokensIsZeroAfterAOneTurnSessionsPageOnlyOpens(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	openPageOnly(t, h, "a1")
	openPageOnly(t, h, "a1")

	// Assert.
	if got := h.server.tokens.outstanding(); got != 0 {
		t.Fatalf("outstanding = %d, want 0", got)
	}
}

func TestOpenAgentSessionRefusesAnAgentIdWithNoValue(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	res, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID(""), PageSize: 10,
	}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
}

// TestWriteBatchRecordsAStorageRefusalAgainstItsProcedure: a request the
// storage layer refuses is a refusal of THIS call, and the only record that can
// name the procedure is written here.
func TestWriteBatchRecordsAStorageRefusalAgainstItsProcedure(t *testing.T) {
	// Arrange. The malformed part is inside the envelope this layer keeps
	// opaque, so the storage layer is what refuses it.
	store := newFakeStore()
	store.writeErr = fmt.Errorf("%w: entries[0].agent_update sets no `agent_info` arm", ErrInvalid)
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "claude-shim:test",
		Batch:    &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
	rec, ok := findRecord(t, h.logs, "store.rpc.write-batch", "warn")
	if !ok {
		t.Fatalf("records = %+v, want the refusal recorded at warn", records(t, h.logs))
	}
	if rec.Context["rpc"] != storev1connect.ShimStoreWriteBatchProcedure {
		t.Errorf("rpc = %v, want %q", rec.Context["rpc"], storev1connect.ShimStoreWriteBatchProcedure)
	}
	if rec.Context["refusal_site"] != SiteStoreRefusedRequest {
		t.Errorf("refusal_site = %v, want %q", rec.Context["refusal_site"], SiteStoreRefusedRequest)
	}
}

func TestOpenAgentSessionMapsAStalePointerToTheFailureArm(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.openErr = fmt.Errorf("%w: known_through names no position in this book", ErrStalePointer)
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID("a1"), PageSize: 10, KnownThrough: &storev1.StoreItemPointer{Value: "p9"},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
	// A REFUSED REQUEST IS THIS LAYER'S RECORD TO WRITE, at warn: the storage
	// layer's own trace names a statement and a table and ties the refusal to
	// nothing, while only this layer knows the rpc, the request id and the
	// producer it belongs to. It is a `warn` and never an `error`, because a
	// pointer that has moved is an ordinary race whose recovery is a repaint.
	rec, ok := findRecord(t, h.logs, "store.rpc.open-agent-session", "warn")
	if !ok || rec.Context["refusal_site"] != SiteStalePointer {
		t.Fatalf("records = %+v, want one warn record at site %q", records(t, h.logs), SiteStalePointer)
	}
	if _, isError := findRecord(t, h.logs, "store.rpc.open-agent-session", "error"); isError {
		t.Fatalf("a stale pointer produced an error record: %+v", records(t, h.logs))
	}
}

// TestOpenAgentSessionMapsAnUnknownAgentToItsOwnArm: the storage layer's
// ErrUnknownAgent becomes unknown_agent on the wire and never invalid_request,
// because the request was well formed and respelling it cannot help.
func TestOpenAgentSessionMapsAnUnknownAgentToItsOwnArm(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.openErr = fmt.Errorf("%w: agent \"ghost\" names no book of this store", ErrUnknownAgent)
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID("ghost"), PageSize: 10,
	}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}
	if res.Msg.GetFailure().GetUnknownAgent() == nil {
		t.Fatalf("failure kind = %v, want unknown_agent", res.Msg.GetFailure().GetKind())
	}
}

// TestOpenAgentSessionRecordsAnUnknownAgentAtInfoWithBothRefusalKeys: a refused
// request belongs to the CALL, so this layer writes the one normal-level record
// — and for this class it is an `info`, because "no such book" is the verb's
// ordinary answer rather than a report that something is wrong. The record is
// still a normal-level one carrying both refusal keys, so an operator counting
// refusals loses nothing.
func TestOpenAgentSessionRecordsAnUnknownAgentAtInfoWithBothRefusalKeys(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.openErr = fmt.Errorf("%w: agent \"ghost\" names no book of this store", ErrUnknownAgent)
	h := newHarness(t, store, 0)

	// Act.
	if _, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID("ghost"), PageSize: 10,
	})); err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}

	// Assert.
	rec, ok := findRecord(t, h.logs, "store.rpc.open-agent-session", "info")
	if !ok || rec.Context["refusal_site"] != SiteUnknownAgent || rec.Context["refusal_kind"] != "unknown_agent" {
		t.Fatalf("records = %+v, want one info record at site %q and kind %q", records(t, h.logs), SiteUnknownAgent, "unknown_agent")
	}
}

// TestOpenAgentSessionDoesNotRecordAnUnknownAgentAtWarnOrError: the whole point
// of the class carrying its own level. An open against an agent whose first row
// has not landed yet is a request the consumer makes on every cold bring-up, and
// a store that wrote a warning for it made every healthy run look degraded.
func TestOpenAgentSessionDoesNotRecordAnUnknownAgentAtWarnOrError(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.openErr = fmt.Errorf("%w: agent \"ghost\" names no book of this store", ErrUnknownAgent)
	h := newHarness(t, store, 0)

	// Act.
	if _, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID("ghost"), PageSize: 10,
	})); err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}

	// Assert.
	for _, level := range []string{"warn", "error"} {
		if _, loud := findRecord(t, h.logs, "store.rpc.open-agent-session", level); loud {
			t.Fatalf("an unknown agent produced a %s record: %+v", level, records(t, h.logs))
		}
	}
}

func TestOpenAgentSessionServesAnEmptyBookAsALegalPage(t *testing.T) {
	// Arrange. An agent the store knows but that has no rows is an empty book,
	// not an unknown agent; the fake answers the empty page the db would.
	store := newFakeStore()
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.OpenAgentSession(context.Background(), connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID("fresh"), PageSize: 10,
	}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenAgentSession = %v, want nil", err)
	}
	success := res.Msg.GetSuccess()
	if success == nil || len(success.GetPage().GetLines()) != 0 || success.GetWatch().GetValue() == "" {
		t.Fatalf("result = %v, want an empty page with a valid token", res.Msg.GetResult())
	}
}

// ---- WatchAgentSession ----

// watcher runs one WatchAgentSession call off the test goroutine.
//
// THE CALL ITSELF BLOCKS UNTIL THE SERVER WRITES RESPONSE HEADERS, which a
// Connect server stream does on its first frame. A test that must observe a
// STANDING watch — one that has subscribed but has nothing to deliver yet —
// therefore cannot wait on the call; it waits on the fake store's LinesSince
// gate instead, which is entered strictly after the subscription.
type watcher struct {
	streamc chan *connect.ServerStreamForClient[storev1.WatchAgentSessionResponse]
	errc    chan error
}

func startWatch(h *harness, ctx context.Context, token string) *watcher {
	w := &watcher{
		streamc: make(chan *connect.ServerStreamForClient[storev1.WatchAgentSessionResponse], 1),
		errc:    make(chan error, 1),
	}
	go func() {
		stream, err := h.stream.WatchAgentSession(ctx, connect.NewRequest(&storev1.WatchAgentSessionRequest{
			Watch: &storev1.AgentSessionToken{Value: token},
		}))
		if err != nil {
			w.errc <- err
			return
		}
		w.streamc <- stream
	}()
	return w
}

// open blocks until the stream is readable and fails if it was refused.
func (w *watcher) open(t *testing.T) *connect.ServerStreamForClient[storev1.WatchAgentSessionResponse] {
	t.Helper()
	select {
	case stream := <-w.streamc:
		t.Cleanup(func() { closeOrFail(t, stream) })
		return stream
	case err := <-w.errc:
		t.Fatalf("WatchAgentSession = %v, want a stream", err)
		return nil
	}
}

// refusal blocks until the watch ends and returns why.
func (w *watcher) refusal(t *testing.T) error {
	t.Helper()
	select {
	case err := <-w.errc:
		return err
	case stream := <-w.streamc:
		defer closeOrFail(t, stream)
		for stream.Receive() {
		}
		return stream.Err()
	}
}

func TestWatchDeliversALineWrittenAfterThePin(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	h := newHarness(t, store, 0)
	token := openSession(t, h, "a1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w := startWatch(h, ctx, token)
	<-store.sinceEntered

	// Act. The write lands after the subscription, so it can only arrive live.
	store.mu.Lock()
	store.writeResult = WriteResult{Written: 1, Lines: []LineWritten{line("a1", "p5", 5)}}
	store.mu.Unlock()
	writeOne(t, h)

	// Assert.
	stream := w.open(t)
	if !stream.Receive() {
		t.Fatalf("Receive = false, want a line: %v", stream.Err())
	}
	if got := stream.Msg().GetLine().GetAt().GetValue(); got != "p5" {
		t.Fatalf("pointer = %q, want %q", got, "p5")
	}
}

func TestWatchNeverDeliversALineAtOrBelowThePin(t *testing.T) {
	// Arrange. The page was read at write ordinal 5.
	store := newFakeStore()
	store.opened = OpenedPage{Page: &storev1.AgentSessionPage{}, PinSeq: 5}
	h := newHarness(t, store, 0)
	token := openSession(t, h, "a1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w := startWatch(h, ctx, token)
	<-store.sinceEntered

	// Act. Ordinal 5 is already in the page; ordinal 6 is not.
	store.mu.Lock()
	store.writeResult = WriteResult{Written: 2, Lines: []LineWritten{line("a1", "p5", 5), line("a1", "p6", 6)}}
	store.mu.Unlock()
	writeOne(t, h)

	// Assert. The first frame delivered is the one above the pin.
	stream := w.open(t)
	if !stream.Receive() {
		t.Fatalf("Receive = false, want a line: %v", stream.Err())
	}
	if got := stream.Msg().GetLine().GetAt().GetValue(); got != "p6" {
		t.Fatalf("first delivered pointer = %q, want %q", got, "p6")
	}
}

func TestWatchNeverDeliversALineOfAnotherBook(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	h := newHarness(t, store, 0)
	token := openSession(t, h, "a1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w := startWatch(h, ctx, token)
	<-store.sinceEntered

	// Act. One batch carrying another agent's line and then this agent's.
	store.mu.Lock()
	store.writeResult = WriteResult{Written: 2, Lines: []LineWritten{line("other", "px", 1), line("a1", "p2", 2)}}
	store.mu.Unlock()
	writeOne(t, h)

	// Assert.
	stream := w.open(t)
	if !stream.Receive() {
		t.Fatalf("Receive = false, want a line: %v", stream.Err())
	}
	if got := stream.Msg().GetLine().GetAt().GetValue(); got != "p2" {
		t.Fatalf("first delivered pointer = %q, want %q", got, "p2")
	}
}

func TestWatchDeliversALineWrittenBetweenOpenAndWatchExactlyOnce(t *testing.T) {
	// Arrange. The watch is held inside LinesSince — i.e. after it subscribed —
	// while the very line the replay is about to return is also published live.
	// That overlap is what the handoff must collapse.
	store := newFakeStore()
	store.since = []LineWritten{line("a1", "p1", 1)}
	store.sinceRelease = make(chan struct{})
	h := newHarness(t, store, 0)
	token := openSession(t, h, "a1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w := startWatch(h, ctx, token)
	<-store.sinceEntered

	store.mu.Lock()
	store.writeResult = WriteResult{Written: 1, Lines: []LineWritten{line("a1", "p1", 1)}}
	store.mu.Unlock()
	writeOne(t, h)
	close(store.sinceRelease)

	// Act. A later, distinct line proves nothing sat between the two.
	stream := w.open(t)
	if !stream.Receive() {
		t.Fatalf("Receive = false, want the replayed line: %v", stream.Err())
	}
	if got := stream.Msg().GetLine().GetAt().GetValue(); got != "p1" {
		t.Fatalf("first pointer = %q, want %q", got, "p1")
	}
	store.mu.Lock()
	store.writeResult = WriteResult{Written: 1, Lines: []LineWritten{line("a1", "p2", 2)}}
	store.mu.Unlock()
	writeOne(t, h)

	// Assert.
	if !stream.Receive() {
		t.Fatalf("Receive = false, want the second line: %v", stream.Err())
	}
	if got := stream.Msg().GetLine().GetAt().GetValue(); got != "p2" {
		t.Fatalf("second pointer = %q, want %q (p1 was delivered twice)", got, "p2")
	}
}

func TestWatchTokenIsSingleUse(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	h := newHarness(t, store, 0)
	token := openSession(t, h, "a1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	startWatch(h, ctx, token)
	<-store.sinceEntered

	// Act. The same token, a second time.
	err := startWatch(h, context.Background(), token).refusal(t)

	// Assert.
	if connect.CodeOf(err) != connect.CodeNotFound {
		t.Fatalf("code = %v (err %v), want %v", connect.CodeOf(err), err, connect.CodeNotFound)
	}
}

func TestWatchRefusesATokenThisStoreNeverMinted(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	err := startWatch(h, context.Background(), "0123456789abcdef").refusal(t)

	// Assert. There is no failure arm on this rpc by design: the refusal is the
	// transport's CodeNotFound and the caller re-opens.
	if connect.CodeOf(err) != connect.CodeNotFound {
		t.Fatalf("code = %v (err %v), want %v", connect.CodeOf(err), err, connect.CodeNotFound)
	}
	rec, ok := findRecord(t, h.logs, "store.rpc.watch-agent-session", "warn")
	if !ok || rec.Context["refusal_site"] != SiteUnknownWatchToken {
		t.Fatalf("records = %+v, want a warn record at site %q", records(t, h.logs), SiteUnknownWatchToken)
	}
}

func TestWatchRefusesATokenWithNoValue(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	err := startWatch(h, context.Background(), "").refusal(t)

	// Assert.
	if connect.CodeOf(err) != connect.CodeNotFound {
		t.Fatalf("code = %v (err %v), want %v", connect.CodeOf(err), err, connect.CodeNotFound)
	}
	rec, ok := findRecord(t, h.logs, "store.rpc.watch-agent-session", "warn")
	if !ok || rec.Context["refusal_site"] != SiteTokenEmpty {
		t.Fatalf("records = %+v, want a warn record at site %q", records(t, h.logs), SiteTokenEmpty)
	}
}

func TestWatchOverflowEndsTheStreamAndLogsAWarning(t *testing.T) {
	// Arrange. A one-frame buffer, and the watch held inside LinesSince so it
	// cannot drain while the writer publishes.
	store := newFakeStore()
	store.sinceRelease = make(chan struct{})
	h := newHarness(t, store, 1)
	token := openSession(t, h, "a1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w := startWatch(h, ctx, token)
	<-store.sinceEntered

	// Act.
	store.mu.Lock()
	store.writeResult = WriteResult{Written: 3, Lines: []LineWritten{
		line("a1", "p1", 1), line("a1", "p2", 2), line("a1", "p3", 3),
	}}
	store.mu.Unlock()
	writeOne(t, h)
	close(store.sinceRelease)

	// Assert.
	err := w.refusal(t)
	if connect.CodeOf(err) != connect.CodeResourceExhausted {
		t.Fatalf("code = %v (err %v), want %v", connect.CodeOf(err), err, connect.CodeResourceExhausted)
	}
	if _, ok := findRecord(t, h.logs, "store.fanout.overflow", "warn"); !ok {
		t.Fatalf("records = %+v, want a store.fanout.overflow warning", records(t, h.logs))
	}
}

func TestWatchMapsAReplayFailureToATransportError(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.sinceErr = fmt.Errorf("%w: read failed", ErrStorage)
	h := newHarness(t, store, 0)
	token := openSession(t, h, "a1")

	// Act.
	err := startWatch(h, context.Background(), token).refusal(t)

	// Assert.
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("code = %v (err %v), want %v", connect.CodeOf(err), err, connect.CodeInternal)
	}
}

func TestShutdownEndsAStandingWatchCleanly(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	h := newHarness(t, store, 0)
	token := openSession(t, h, "a1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w := startWatch(h, ctx, token)
	<-store.sinceEntered

	// Act. 1s: same in-process fake-Store basis as the package's other
	// shutdown bounds.
	shutdownCtx, shutdownCancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer shutdownCancel()
	if err := h.server.Shutdown(shutdownCtx); err != nil {
		t.Fatalf("Shutdown = %v, want nil", err)
	}

	// Assert. The stream ends without an error: the store concluded nothing, it
	// simply stopped.
	if err := w.refusal(t); err != nil {
		t.Fatalf("stream error = %v, want a clean end", err)
	}
}

// ---- ReadAgentPage ----

func TestReadAgentPageServesAPage(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.page = &storev1.ReadAgentPageSuccess{
		Lines: []*storev1.StoreLineAt{{
			At:   &storev1.StoreItemPointer{Value: "sip1-8"},
			Line: &storev1.StorePageLine{PageAgentId: agentID("a1")},
		}},
		Boundary: &storev1.ReadAgentPageSuccess_Floor{Floor: &storev1.ReadAgentPageFloor{}},
	}
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.ReadAgentPage(context.Background(), connect.NewRequest(&storev1.ReadAgentPageRequest{
		Book: agentID("a1"), PageSize: 10, After: &storev1.StoreItemPointer{Value: "p9"},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ReadAgentPage = %v, want nil", err)
	}
	success := res.Msg.GetSuccess()
	if success == nil || len(success.GetLines()) != 1 {
		t.Fatalf("result = %v, want one line", res.Msg.GetResult())
	}
	// A CONTINUATION LINE CARRIES ITS OWN POINTER, so a reader never mints a
	// placeholder mark for a page it walked to.
	if got := success.GetLines()[0].GetAt().GetValue(); got != "sip1-8" {
		t.Fatalf("line pointer = %q, want the position the store served", got)
	}
}

func TestReadAgentPageRecordsCarryTheAgentAndBook(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	if _, err := h.client.ReadAgentPage(context.Background(), connect.NewRequest(&storev1.ReadAgentPageRequest{
		Book: agentID("agent-1"), PageSize: 10, After: &storev1.StoreItemPointer{Value: "p9"},
	})); err != nil {
		t.Fatalf("ReadAgentPage = %v, want nil", err)
	}

	// Assert.
	found := false
	for _, rec := range records(t, h.logs) {
		if rec.Operation != "store.rpc.read-agent-page" {
			continue
		}
		found = true
		if rec.Context["agent_id"] != "agent-1" || rec.Context["book_agent_id"] != "agent-1" {
			t.Fatalf("record = %+v, want request-scoped agent and book", rec)
		}
	}
	if !found {
		t.Fatal("no read-agent-page record found")
	}
}

func TestReadAgentPageRefusesAMissingAfterPointer(t *testing.T) {
	// Arrange. This verb only walks older; the first page is the open's answer.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	res, err := h.client.ReadAgentPage(context.Background(), connect.NewRequest(&storev1.ReadAgentPageRequest{
		Book: agentID("a1"), PageSize: 10,
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ReadAgentPage = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
}

func TestReadAgentPageMapsAStalePointerToTheFailureArm(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.pageErr = fmt.Errorf("%w: p9 is not in this book", ErrStalePointer)
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.ReadAgentPage(context.Background(), connect.NewRequest(&storev1.ReadAgentPageRequest{
		Book: agentID("a1"), PageSize: 10, After: &storev1.StoreItemPointer{Value: "p9"},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ReadAgentPage = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
}

// ---- GetLiveWork ----

func TestGetLiveWorkServesTheOpenObligations(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.live = &storev1.GetLiveWorkSuccess{LiveAgents: []*conversationv1.AgentId{agentID("a1")}}
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.GetLiveWork(context.Background(), connect.NewRequest(scopedLiveWork()))

	// Assert.
	if err != nil {
		t.Fatalf("GetLiveWork = %v, want nil", err)
	}
	if success := res.Msg.GetSuccess(); success == nil || len(success.GetLiveAgents()) != 1 {
		t.Fatalf("result = %v, want one live agent", res.Msg.GetResult())
	}
}

func TestGetLiveWorkMapsAStorageFailureToTheStorageFailureArm(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.liveErr = fmt.Errorf("%w: scan failed", ErrStorage)
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.GetLiveWork(context.Background(), connect.NewRequest(scopedLiveWork()))

	// Assert.
	if err != nil {
		t.Fatalf("GetLiveWork = %v, want nil", err)
	}
	if res.Msg.GetFailure().GetStorageFailure() == nil {
		t.Fatalf("result = %v, want the storage_failure arm", res.Msg.GetResult())
	}
}

func TestGetLiveWorkMapsANilAnswerToTheStorageFailureArm(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.live = nil
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.GetLiveWork(context.Background(), connect.NewRequest(scopedLiveWork()))

	// Assert.
	if err != nil {
		t.Fatalf("GetLiveWork = %v, want nil", err)
	}
	if res.Msg.GetFailure().GetStorageFailure() == nil {
		t.Fatalf("result = %v, want the storage_failure arm", res.Msg.GetResult())
	}
	if _, ok := findRecord(t, h.logs, "store.rpc.get-live-work", "error"); !ok {
		t.Fatalf("records = %+v, want the error record this layer owns", records(t, h.logs))
	}
}

func TestGetLiveWorkScopesTheStoreReadToTheRequestedSession(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	h := newHarness(t, store, 0)

	// Act.
	if _, err := h.client.GetLiveWork(context.Background(), connect.NewRequest(scopedLiveWork())); err != nil {
		t.Fatalf("GetLiveWork = %v, want nil", err)
	}

	// Assert.
	store.mu.Lock()
	defer store.mu.Unlock()
	if store.liveFor != "main-1" {
		t.Fatalf("store read scoped to %q, want main-1", store.liveFor)
	}
}

func TestGetLiveWorkRefusesARequestNamingNoSession(t *testing.T) {
	tests := []struct {
		name    string
		request *storev1.GetLiveWorkRequest
	}{
		{name: "session unset", request: &storev1.GetLiveWorkRequest{}},
		{name: "session with an empty value", request: &storev1.GetLiveWorkRequest{Session: &conversationv1.AgentId{}}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange. The store is shared by every session on the host, so an
			// unscoped answer would hand this caller everyone's obligations.
			store := newFakeStore()
			h := newHarness(t, store, 0)

			// Act.
			res, err := h.client.GetLiveWork(context.Background(), connect.NewRequest(test.request))

			// Assert.
			if err != nil {
				t.Fatalf("GetLiveWork = %v, want nil", err)
			}
			invalid := res.Msg.GetFailure().GetInvalidRequest()
			if invalid == nil || invalid.GetField() != "session" {
				t.Fatalf("result = %v, want invalid_request naming session", res.Msg.GetResult())
			}
			store.mu.Lock()
			asked := store.liveAsked
			store.mu.Unlock()
			if asked {
				t.Fatal("the store was queried for a refused request")
			}
			rec, ok := findRecord(t, h.logs, "store.rpc.get-live-work", "warn")
			if !ok {
				t.Fatalf("records = %+v, want one warn refusal record", records(t, h.logs))
			}
			if rec.Context["refusal_site"] != SiteSessionEmpty || rec.Context["refusal_kind"] != "invalid_request" {
				t.Fatalf("refusal context = %v, want site %q kind invalid_request", rec.Context, SiteSessionEmpty)
			}
		})
	}
}

// ---- ListResidueShapes ----

func TestListResidueShapesServesAnEmptyCatalogAsSuccess(t *testing.T) {
	// Arrange. A store that has observed no unstored residue has no shapes.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	res, err := h.client.ListResidueShapes(context.Background(), connect.NewRequest(&storev1.ListResidueShapesRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("ListResidueShapes = %v, want nil", err)
	}
	if success := res.Msg.GetSuccess(); success == nil || len(success.GetShapes()) != 0 {
		t.Fatalf("result = %v, want an empty success", res.Msg.GetResult())
	}
}

func TestListResidueShapesPassesTheKindFilterThrough(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	h := newHarness(t, store, 0)
	kind := "unparsed"

	// Act.
	if _, err := h.client.ListResidueShapes(context.Background(),
		connect.NewRequest(&storev1.ListResidueShapesRequest{Kind: &kind})); err != nil {
		t.Fatalf("ListResidueShapes = %v, want nil", err)
	}

	// Assert.
	if store.shapesKind == nil || *store.shapesKind != kind {
		t.Fatalf("store filtered on %v, want %q", store.shapesKind, kind)
	}
}

func TestListResidueShapesPassesTheExampleOptInThrough(t *testing.T) {
	// Arrange. The example is the one field carrying raw vendor bytes.
	store := newFakeStore()
	h := newHarness(t, store, 0)

	// Act.
	if _, err := h.client.ListResidueShapes(context.Background(),
		connect.NewRequest(&storev1.ListResidueShapesRequest{IncludeExample: true})); err != nil {
		t.Fatalf("ListResidueShapes = %v, want nil", err)
	}

	// Assert.
	if !store.shapesExample {
		t.Fatal("the store was asked to withhold the example, want the opt-in passed through")
	}
}

func TestListResidueShapesRefusesAPresentButEmptyKind(t *testing.T) {
	// Arrange. Absence is spelled by omitting the field, never by "".
	store := newFakeStore()
	h := newHarness(t, store, 0)
	empty := ""

	// Act.
	res, err := h.client.ListResidueShapes(context.Background(),
		connect.NewRequest(&storev1.ListResidueShapesRequest{Kind: &empty}))

	// Assert.
	if err != nil {
		t.Fatalf("ListResidueShapes = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
	if store.shapesRequests != 0 {
		t.Fatal("the store was queried for a refused request")
	}
}

func TestListResidueShapesMapsAStorageFailureToTheFailureArm(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.shapesErr = fmt.Errorf("%w: select failed", ErrStorage)
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.ListResidueShapes(context.Background(), connect.NewRequest(&storev1.ListResidueShapesRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("ListResidueShapes = %v, want nil", err)
	}
	if res.Msg.GetFailure().GetStorageFailure() == nil {
		t.Fatalf("result = %v, want the storage_failure arm", res.Msg.GetResult())
	}
}

func TestWriteBatchHandsTheShapeObservationsToTheStore(t *testing.T) {
	// Arrange. The catalog rides the write so it commits in its transaction.
	store := newFakeStore()
	h := newHarness(t, store, 0)
	shape := &storev1.ShapeObservation{
		ShapeHash: "h1", Kind: "unparsed", KeyStructure: "{a:string}", SeenMs: 1000,
	}

	// Act.
	if _, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "sidecar",
		Batch:    &storev1.EntryBatch{},
		Shapes:   []*storev1.ShapeObservation{shape},
	})); err != nil {
		t.Fatalf("WriteBatch = %v, want nil", err)
	}

	// Assert.
	if len(store.shapesWritten) != 1 || len(store.shapesWritten[0]) != 1 ||
		store.shapesWritten[0][0].GetShapeHash() != "h1" {
		t.Fatalf("shapes handed to the store = %v, want the one observation", store.shapesWritten)
	}
}

func TestWriteBatchRefusesAShapeObservationWithNoHash(t *testing.T) {
	// Arrange. shape_hash is the catalog's primary key.
	store := newFakeStore()
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: "sidecar",
		Batch:    &storev1.EntryBatch{},
		Shapes:   []*storev1.ShapeObservation{{Kind: "unparsed", KeyStructure: "{a:string}", SeenMs: 1000}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch = %v, want nil", err)
	}
	if res.Msg.GetFailure().GetInvalidRequest() == nil {
		t.Fatalf("result = %v, want the invalid_request arm", res.Msg.GetResult())
	}
	if len(store.writes) != 0 {
		t.Fatal("the store was written to for a refused request")
	}
}

// ---- GetSidecarCursors ----

func TestGetSidecarCursorsServesAnEmptySetAsSuccess(t *testing.T) {
	// Arrange. A fresh store has no cursors and every file starts from zero.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	res, err := h.client.GetSidecarCursors(context.Background(), connect.NewRequest(&storev1.GetSidecarCursorsRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("GetSidecarCursors = %v, want nil", err)
	}
	if success := res.Msg.GetSuccess(); success == nil || len(success.GetCursors()) != 0 {
		t.Fatalf("result = %v, want an empty success", res.Msg.GetResult())
	}
}

func TestGetSidecarCursorsScopesToOneFileWhenAsked(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.cursors = []*storev1.CursorState{{FileId: "1:2", Path: "/t.jsonl", Offset: 12}}
	h := newHarness(t, store, 0)
	fileID := "1:2"

	// Act.
	res, err := h.client.GetSidecarCursors(context.Background(), connect.NewRequest(&storev1.GetSidecarCursorsRequest{FileId: &fileID}))

	// Assert.
	if err != nil {
		t.Fatalf("GetSidecarCursors = %v, want nil", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want the success arm", res.Msg.GetResult())
	}
	if store.cursorsFor == nil || *store.cursorsFor != fileID {
		t.Fatalf("store scoped to %v, want %q", store.cursorsFor, fileID)
	}
}

func TestGetSidecarCursorsRefusesAPresentButEmptyFileId(t *testing.T) {
	// Arrange. Absence is spelled by omitting the field, never by "".
	store := newFakeStore()
	h := newHarness(t, store, 0)
	empty := ""

	// Act.
	res, err := h.client.GetSidecarCursors(context.Background(), connect.NewRequest(&storev1.GetSidecarCursorsRequest{FileId: &empty}))

	// Assert.
	if err != nil {
		t.Fatalf("GetSidecarCursors = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
	if store.cursorsScope {
		t.Fatal("the store was queried for a refused request")
	}
}

func TestGetSidecarCursorsMapsAStorageFailureToTheFailureArm(t *testing.T) {
	// Arrange.
	store := newFakeStore()
	store.cursorsErr = fmt.Errorf("%w: select failed", ErrStorage)
	h := newHarness(t, store, 0)

	// Act.
	res, err := h.client.GetSidecarCursors(context.Background(), connect.NewRequest(&storev1.GetSidecarCursorsRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("GetSidecarCursors = %v, want nil", err)
	}
	if res.Msg.GetFailure() == nil {
		t.Fatalf("result = %v, want the failure arm", res.Msg.GetResult())
	}
}

// ---- GetWorkflow ----

func TestGetWorkflowAnswersTheNotImplementedFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	res, err := h.client.GetWorkflow(context.Background(), connect.NewRequest(&storev1.GetWorkflowRequest{
		Work: &conversationv1.DetachedWorkId{Value: "wf1"},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("GetWorkflow = %v, want nil", err)
	}
	failure := res.Msg.GetFailure()
	if failure == nil || !strings.Contains(failure.GetDetail(), "not implemented") {
		t.Fatalf("result = %v, want the not-implemented failure arm", res.Msg.GetResult())
	}
	rec, ok := findRecord(t, h.logs, "store.rpc.get-workflow", "warn")
	if !ok || rec.Context["refusal_site"] != SiteWorkflowNotImplemented {
		t.Fatalf("records = %+v, want a warn record at site %q", records(t, h.logs), SiteWorkflowNotImplemented)
	}
}

// ---- transport ----

func TestServesOverHTTP11(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	res, err := h.client.GetLiveWork(context.Background(), connect.NewRequest(scopedLiveWork()))

	// Assert.
	if err != nil {
		t.Fatalf("GetLiveWork over HTTP/1.1 = %v, want nil", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want the success arm", res.Msg.GetResult())
	}
}

func TestServesOverH2CWithPriorKnowledge(t *testing.T) {
	// Arrange. No TLS anywhere: the h2c wrapper is what makes an HTTP/2
	// prior-knowledge client work on the same cleartext socket.
	sink := &syncBuffer{}
	srv := New(newFakeStore(), logging.New(sink, io.Discard, true), 0)
	ts := httptest.NewServer(srv.Handler())
	defer ts.Close()
	client := h2cClient()

	// Act.
	res, err := storev1connect.NewShimStoreClient(client, ts.URL).GetLiveWork(
		context.Background(), connect.NewRequest(scopedLiveWork()))

	// Assert.
	if err != nil {
		t.Fatalf("GetLiveWork over h2c = %v, want nil", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want the success arm", res.Msg.GetResult())
	}
}

func TestServesTheJSONCodec(t *testing.T) {
	// Arrange. A plain POST with a JSON body is the shape a curl probe uses.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	res, err := h.http.Post(h.url+storev1connect.ShimStoreGetLiveWorkProcedure, "application/json", strings.NewReader(`{"session":{"value":"main-1"}}`))
	if err != nil {
		t.Fatalf("POST: %v", err)
	}
	defer closeOrFail(t, res.Body)
	body, err := io.ReadAll(res.Body)
	if err != nil {
		t.Fatalf("read body: %v", err)
	}

	// Assert.
	if res.StatusCode != http.StatusOK {
		t.Fatalf("status = %d body = %s, want 200", res.StatusCode, body)
	}
	var decoded map[string]any
	if err := json.Unmarshal(body, &decoded); err != nil {
		t.Fatalf("decode %s: %v", body, err)
	}
	if _, ok := decoded["success"]; !ok {
		t.Fatalf("body = %s, want a success arm", body)
	}
}

func TestServesOverAUnixSocket(t *testing.T) {
	// Arrange. macOS caps sun_path at ~104 bytes, so the path is short by
	// construction rather than under t.TempDir().
	sink := &syncBuffer{}
	log := logging.New(sink, io.Discard, true)
	path := shortSocketPath(t)
	ln, err := Listen(path, log)
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	srv := New(newFakeStore(), log, 0)
	go func() { _ = srv.Serve(ln) }()
	t.Cleanup(func() {
		// 1s: an in-process httptest.Server over a fake Store shuts down in
		// single-digit milliseconds even under -race; see flush_test.go's
		// openBound for the same package-level basis.
		ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
		defer cancel()
		if err := srv.Shutdown(ctx); err != nil {
			t.Errorf("shutdown: %v", err)
		}
	})
	client := &http.Client{Transport: &http.Transport{
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, "unix", path)
		},
	}}

	// Act.
	res, err := storev1connect.NewShimStoreClient(client, "http://store").GetLiveWork(
		context.Background(), connect.NewRequest(scopedLiveWork()))

	// Assert.
	if err != nil {
		t.Fatalf("GetLiveWork over the unix socket = %v, want nil", err)
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want the success arm", res.Msg.GetResult())
	}
}

func TestRequestIdHeaderIsCarriedIntoTheLog(t *testing.T) {
	// Arrange.
	h := newHarness(t, newFakeStore(), 0)
	req := connect.NewRequest(&storev1.GetWorkflowRequest{Work: &conversationv1.DetachedWorkId{Value: "wf1"}})
	req.Header().Set(RequestIDHeader, "req-42")

	// Act.
	if _, err := h.client.GetWorkflow(context.Background(), req); err != nil {
		t.Fatalf("GetWorkflow = %v, want nil", err)
	}

	// Assert.
	rec, ok := findRecord(t, h.logs, "store.rpc.get-workflow", "warn")
	if !ok || rec.RequestID != "req-42" {
		t.Fatalf("record = %+v, want request_id %q", rec, "req-42")
	}
}

func TestNewPanicsWithoutAStore(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("New with a nil store returned, want a panic")
		}
	}()

	// Act.
	New(nil, logging.New(&syncBuffer{}, io.Discard, false), 0)
}

func TestARefusalRecordNamesBothTheSiteAndTheWireArm(t *testing.T) {
	// Arrange. THE SITE IS NOT THE ARM: several sites map to one arm, so a
	// reader triaging refusals needs the site to find the check that said no
	// and the kind to know whether the caller could ever have retried.
	tests := []struct {
		name      string
		operation string
		call      func(*harness)
		wantSite  string
		wantKind  string
	}{
		{
			name:      "an empty batch is invalid_request",
			operation: "store.rpc.write-batch",
			wantSite:  SiteBatchEmpty,
			wantKind:  "invalid_request",
			call: func(h *harness) {
				h.client.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{ //nolint:errcheck // the refusal is the subject
					Producer: "claude-shim:s1",
					Batch:    &storev1.EntryBatch{},
				}))
			},
		},
		{
			name:      "workflow is not_implemented",
			operation: "store.rpc.get-workflow",
			wantSite:  SiteWorkflowNotImplemented,
			wantKind:  "not_implemented",
			call: func(h *harness) {
				h.client.GetWorkflow(context.Background(), connect.NewRequest(&storev1.GetWorkflowRequest{ //nolint:errcheck // the refusal is the subject
					Work: &conversationv1.DetachedWorkId{Value: "work-1"},
				}))
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, newFakeStore(), 0)

			// Act.
			tc.call(h)

			// Assert.
			rec, ok := findRecord(t, h.logs, tc.operation, "warn")
			if !ok {
				t.Fatalf("records = %+v, want a warn record at %q", records(t, h.logs), tc.operation)
			}
			if rec.Context["refusal_site"] != tc.wantSite {
				t.Errorf("refusal_site = %v, want %q", rec.Context["refusal_site"], tc.wantSite)
			}
			if rec.Context["refusal_kind"] != tc.wantKind {
				t.Errorf("refusal_kind = %v, want %q", rec.Context["refusal_kind"], tc.wantKind)
			}
		})
	}
}
