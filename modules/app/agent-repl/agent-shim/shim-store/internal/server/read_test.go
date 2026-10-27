package server

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net/http/httptest"
	"testing"
	"time"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	storereadv1 "agentrepl/proto/storeread/v1"
	"agentrepl/proto/storeread/v1/storereadv1connect"
	"agentrepl/shim-store/internal/logging"
)

// fakeReadStore answers every read with one scripted page or error and
// records how it was asked.
type fakeReadStore struct {
	opening *storev1.AgentSessionPage
	page    *storev1.ReadAgentPageSuccess
	err     error
	// called names the read asked and its arguments.
	called string
}

func (f *fakeReadStore) NewestPage(_ context.Context, agentID string, stated bool) (*storev1.AgentSessionPage, error) {
	f.called = fmt.Sprintf("newest %s stated=%t", agentID, stated)
	return f.opening, f.err
}

func (f *fakeReadStore) PageBefore(_ context.Context, agentID string, after *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error) {
	f.called = fmt.Sprintf("before %s %s", agentID, after.GetValue())
	return f.page, f.err
}

func (f *fakeReadStore) PageThrough(_ context.Context, agentID string, throughAtMs int64) (*storev1.ReadAgentPageSuccess, error) {
	f.called = fmt.Sprintf("through %s %d", agentID, throughAtMs)
	return f.page, f.err
}

type readHarness struct {
	client storereadv1connect.StoreReadClient
	// producer is the producers' client aimed at the read-only socket's handler.
	producer storev1connect.ShimStoreClient
	logs     *syncBuffer
}

func newReadHarness(t *testing.T, store ReadStore) *readHarness {
	t.Helper()
	sink := &syncBuffer{}
	srv := NewRead(store, logging.New(sink, io.Discard, true))
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	t.Cleanup(func() {
		// 1s: an in-process httptest.Server with no standing stream shuts down
		// in single-digit milliseconds, as newHarness's does.
		ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
		defer cancel()
		if err := srv.Shutdown(ctx); err != nil {
			t.Errorf("shutdown: %v", err)
		}
	})
	return &readHarness{
		client:   storereadv1connect.NewStoreReadClient(ts.Client(), ts.URL),
		producer: storev1connect.NewShimStoreClient(ts.Client(), ts.URL),
		logs:     sink,
	}
}

func storeLine(at string) *storev1.StoreLineAt {
	return &storev1.StoreLineAt{
		At:   &storev1.StoreItemPointer{Value: at},
		Line: &storev1.StorePageLine{AgentItem: &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_AgentPrompt{AgentPrompt: &conversationv1.AgentPrompt{}}}},
	}
}

func book(value string) *conversationv1.AgentId { return &conversationv1.AgentId{Value: value} }

func newest(stated bool) *storereadv1.ReadHistoryPageRequest_Newest {
	n := &storereadv1.ReadHistoryPageNewest{KnownAs: &storereadv1.ReadHistoryPageNewest_Named{Named: &storereadv1.ReadHistoryPageNewestNamed{}}}
	if stated {
		n.KnownAs = &storereadv1.ReadHistoryPageNewest_Stated{Stated: &storereadv1.ReadHistoryPageNewestStated{}}
	}
	return &storereadv1.ReadHistoryPageRequest_Newest{Newest: n}
}

func TestReadHistoryPageAsksTheReadThePositionNames(t *testing.T) {
	floorOpening := &storev1.AgentSessionPage{Lines: []*storev1.StoreLineAt{storeLine("p-2")}, Boundary: &storev1.AgentSessionPage_Floor{Floor: &storev1.ReadAgentPageFloor{}}}
	floorPage := &storev1.ReadAgentPageSuccess{Lines: []*storev1.StoreLineAt{storeLine("p-2")}, Boundary: &storev1.ReadAgentPageSuccess_Floor{Floor: &storev1.ReadAgentPageFloor{}}}
	tests := []struct {
		name     string
		position any
		want     string
	}{
		{name: "the newest page of a stated book", position: newest(true), want: "newest agent-1 stated=true"},
		{name: "the newest page of a named book", position: newest(false), want: "newest agent-1 stated=false"},
		{name: "the page below a served pointer", position: &storereadv1.ReadHistoryPageRequest_After{After: &conversationv1.HistoryPointer{Value: "p-9"}}, want: "before agent-1 p-9"},
		{name: "the book as it stood at an instant", position: &storereadv1.ReadHistoryPageRequest_Through{Through: &conversationv1.ConversationThrough{AtMs: 1800}}, want: "through agent-1 1800"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := &fakeReadStore{opening: floorOpening, page: floorPage}
			h := newReadHarness(t, store)
			req := &storereadv1.ReadHistoryPageRequest{Book: book("agent-1")}
			switch p := tt.position.(type) {
			case *storereadv1.ReadHistoryPageRequest_Newest:
				req.Position = p
			case *storereadv1.ReadHistoryPageRequest_After:
				req.Position = p
			case *storereadv1.ReadHistoryPageRequest_Through:
				req.Position = p
			}

			// Act
			resp, err := h.client.ReadHistoryPage(context.Background(), connect.NewRequest(req))

			// Assert
			if err != nil {
				t.Fatalf("ReadHistoryPage: %v", err)
			}
			if store.called != tt.want {
				t.Fatalf("read asked = %q, want %q", store.called, tt.want)
			}
			page := resp.Msg.GetSuccess().GetPage()
			if len(page.GetEntries()) != 1 || page.GetEntries()[0].GetAt().GetValue() != "p-2" || page.GetFloor() == nil {
				t.Fatalf("page = %v, want the one line at the floor", page)
			}
		})
	}
}

func TestReadHistoryPageRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name      string
		req       *storereadv1.ReadHistoryPageRequest
		wantField string
	}{
		{name: "no book", req: &storereadv1.ReadHistoryPageRequest{Position: newest(true)}, wantField: "book"},
		{name: "no position", req: &storereadv1.ReadHistoryPageRequest{Book: book("agent-1")}, wantField: "position"},
		{
			name:      "a newest page saying nothing of how the book is known",
			req:       &storereadv1.ReadHistoryPageRequest{Book: book("agent-1"), Position: &storereadv1.ReadHistoryPageRequest_Newest{Newest: &storereadv1.ReadHistoryPageNewest{}}},
			wantField: "newest.known_as",
		},
		{
			name:      "an empty pointer",
			req:       &storereadv1.ReadHistoryPageRequest{Book: book("agent-1"), Position: &storereadv1.ReadHistoryPageRequest_After{After: &conversationv1.HistoryPointer{}}},
			wantField: "after",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := &fakeReadStore{}
			h := newReadHarness(t, store)

			// Act
			resp, err := h.client.ReadHistoryPage(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("ReadHistoryPage: %v", err)
			}
			if got := resp.Msg.GetFailure().GetInvalidRequest().GetField(); got != tt.wantField {
				t.Fatalf("failure = %v, want invalid_request on %q", resp.Msg.GetFailure(), tt.wantField)
			}
			if store.called != "" {
				t.Fatalf("read asked = %q, want none for a refused request", store.called)
			}
		})
	}
}

func TestReadHistoryPageAnswersAFailedReadInItsArm(t *testing.T) {
	tests := []struct {
		name    string
		store   *fakeReadStore
		wantArm string
	}{
		{name: "an unknown agent", store: &fakeReadStore{err: fmt.Errorf("no book: %w", ErrUnknownAgent)}, wantArm: "unknown_agent"},
		{name: "a stale pointer", store: &fakeReadStore{err: fmt.Errorf("moved: %w", ErrStalePointer)}, wantArm: "stale_pointer"},
		{name: "a database failure", store: &fakeReadStore{err: errors.New("disk I/O error")}, wantArm: "storage_failure"},
		{name: "a page that cannot be drawn", store: &fakeReadStore{opening: &storev1.AgentSessionPage{}}, wantArm: "storage_failure"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newReadHarness(t, tt.store)
			req := &storereadv1.ReadHistoryPageRequest{Book: book("agent-1"), Position: newest(false)}

			// Act
			resp, err := h.client.ReadHistoryPage(context.Background(), connect.NewRequest(req))

			// Assert
			if err != nil {
				t.Fatalf("ReadHistoryPage: %v", err)
			}
			failure := resp.Msg.GetFailure()
			got := ""
			switch {
			case failure.GetUnknownAgent() != nil:
				got = "unknown_agent"
			case failure.GetStalePointer() != nil:
				got = "stale_pointer"
			case failure.GetStorageFailure() != nil:
				got = "storage_failure"
			}
			if got != tt.wantArm {
				t.Fatalf("failure = %v, want the %s arm", failure, tt.wantArm)
			}
		})
	}
}

func TestReadHistoryPageRecordsAnUndrawablePageAtError(t *testing.T) {
	// Arrange
	h := newReadHarness(t, &fakeReadStore{opening: &storev1.AgentSessionPage{}})
	req := &storereadv1.ReadHistoryPageRequest{Book: book("agent-1"), Position: newest(false)}

	// Act
	if _, err := h.client.ReadHistoryPage(context.Background(), connect.NewRequest(req)); err != nil {
		t.Fatalf("ReadHistoryPage: %v", err)
	}

	// Assert
	if _, ok := findRecord(t, h.logs, opReadHistoryPage, "error"); !ok {
		t.Fatalf("logs = %s, want the undrawable page at error", h.logs.String())
	}
}

// TestTheReadOnlySocketServesNoProducerVerb: the read-only face's handler
// mounts StoreRead alone, so a producer's call reaching it — a write above
// all — finds no such procedure.
func TestTheReadOnlySocketServesNoProducerVerb(t *testing.T) {
	// Arrange
	h := newReadHarness(t, &fakeReadStore{})

	// Act
	_, err := h.producer.WriteBatch(context.Background(), connect.NewRequest(&storev1.WriteBatchRequest{}))

	// Assert
	if connect.CodeOf(err) != connect.CodeNotFound && connect.CodeOf(err) != connect.CodeUnimplemented {
		t.Fatalf("WriteBatch on the read-only handler err = %v (code %v), want no such procedure", err, connect.CodeOf(err))
	}
}
