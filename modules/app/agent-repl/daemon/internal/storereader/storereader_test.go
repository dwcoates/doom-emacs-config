package storereader

import (
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	storereadv1 "agentrepl/proto/storeread/v1"
	"agentrepl/proto/storeread/v1/storereadv1connect"
)

// fakeStore is a read-only store answering one scripted response and keeping
// the request it was asked.
type fakeStore struct {
	storereadv1connect.UnimplementedStoreReadHandler
	mu     sync.Mutex
	answer *storereadv1.ReadHistoryPageResponse
	asked  *storereadv1.ReadHistoryPageRequest
}

func (f *fakeStore) ReadHistoryPage(_ context.Context, req *connect.Request[storereadv1.ReadHistoryPageRequest]) (*connect.Response[storereadv1.ReadHistoryPageResponse], error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.asked = req.Msg
	return connect.NewResponse(f.answer), nil
}

// serve binds STORE on the read-only socket beside a store socket in a fresh
// directory, and answers that store socket's path.
func serve(t *testing.T, store *fakeStore) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "sr")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	storeSocket := filepath.Join(dir, "store.sock")
	ln, err := net.Listen("unix", filepath.Join(dir, "store-read.sock"))
	if err != nil {
		t.Fatal(err)
	}
	mux := http.NewServeMux()
	mux.Handle(storereadv1connect.NewStoreReadHandler(store))
	srv := &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{})}
	go func() { _ = srv.Serve(ln) }()
	t.Cleanup(func() { _ = srv.Close() })
	return storeSocket
}

func success(page *conversationv1.HistoryPage) *storereadv1.ReadHistoryPageResponse {
	return &storereadv1.ReadHistoryPageResponse{Result: &storereadv1.ReadHistoryPageResponse_Success{Success: &storereadv1.ReadHistoryPageSuccess{Page: page}}}
}

func floorPage() *conversationv1.HistoryPage {
	return &conversationv1.HistoryPage{Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}}
}

func TestReaderAsksThePositionItWasGiven(t *testing.T) {
	stated := &storereadv1.ReadHistoryPageNewest{KnownAs: &storereadv1.ReadHistoryPageNewest_Stated{Stated: &storereadv1.ReadHistoryPageNewestStated{}}}
	named := &storereadv1.ReadHistoryPageNewest{KnownAs: &storereadv1.ReadHistoryPageNewest_Named{Named: &storereadv1.ReadHistoryPageNewestNamed{}}}
	after := &conversationv1.HistoryPointer{Value: "p-9"}
	tests := []struct {
		name string
		read func(r *Reader) (*conversationv1.HistoryPage, error)
		want *storereadv1.ReadHistoryPageRequest
	}{
		{
			name: "the newest page of a stated book",
			read: func(r *Reader) (*conversationv1.HistoryPage, error) { return r.Newest(context.Background(), "book-1", true) },
			want: &storereadv1.ReadHistoryPageRequest{Book: &conversationv1.AgentId{Value: "book-1"}, Position: &storereadv1.ReadHistoryPageRequest_Newest{Newest: stated}},
		},
		{
			name: "the newest page of a named book",
			read: func(r *Reader) (*conversationv1.HistoryPage, error) { return r.Newest(context.Background(), "book-1", false) },
			want: &storereadv1.ReadHistoryPageRequest{Book: &conversationv1.AgentId{Value: "book-1"}, Position: &storereadv1.ReadHistoryPageRequest_Newest{Newest: named}},
		},
		{
			name: "the page before a pointer",
			read: func(r *Reader) (*conversationv1.HistoryPage, error) { return r.Before(context.Background(), "book-1", after) },
			want: &storereadv1.ReadHistoryPageRequest{Book: &conversationv1.AgentId{Value: "book-1"}, Position: &storereadv1.ReadHistoryPageRequest_After{After: after}},
		},
		{
			name: "the book as it stood at an instant",
			read: func(r *Reader) (*conversationv1.HistoryPage, error) { return r.Through(context.Background(), "book-1", 1800) },
			want: &storereadv1.ReadHistoryPageRequest{Book: &conversationv1.AgentId{Value: "book-1"}, Position: &storereadv1.ReadHistoryPageRequest_Through{Through: &conversationv1.ConversationThrough{AtMs: 1800}}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := &fakeStore{answer: success(floorPage())}
			r := New(serve(t, store))

			// Act
			page, err := tt.read(r)

			// Assert
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			if !proto.Equal(page, floorPage()) {
				t.Fatalf("page = %v, want the floor page served", page)
			}
			if !proto.Equal(store.asked, tt.want) {
				t.Fatalf("asked %v, want %v", store.asked, tt.want)
			}
		})
	}
}

func TestReaderNamesARefusalByItsArm(t *testing.T) {
	tests := []struct {
		name    string
		failure *storereadv1.ReadHistoryPageFailure
		want    string
	}{
		{name: "invalid request", failure: &storereadv1.ReadHistoryPageFailure{Kind: &storereadv1.ReadHistoryPageFailure_InvalidRequest{InvalidRequest: &storereadv1.ReadHistoryPageInvalidRequest{Field: "book"}}}, want: ArmInvalidRequest},
		{name: "unknown agent", failure: &storereadv1.ReadHistoryPageFailure{Kind: &storereadv1.ReadHistoryPageFailure_UnknownAgent{UnknownAgent: &storereadv1.ReadHistoryPageUnknownAgent{}}}, want: ArmUnknownAgent},
		{name: "stale pointer", failure: &storereadv1.ReadHistoryPageFailure{Kind: &storereadv1.ReadHistoryPageFailure_StalePointer{StalePointer: &storereadv1.ReadHistoryPageStalePointer{}}}, want: ArmStalePointer},
		{name: "storage failure", failure: &storereadv1.ReadHistoryPageFailure{Kind: &storereadv1.ReadHistoryPageFailure_StorageFailure{StorageFailure: &storereadv1.ReadHistoryPageStorageFailure{}}}, want: ArmStorageFailure},
		{name: "an arm this schema does not name", failure: &storereadv1.ReadHistoryPageFailure{}, want: ArmUnnamed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := New(serve(t, &fakeStore{answer: &storereadv1.ReadHistoryPageResponse{Result: &storereadv1.ReadHistoryPageResponse_Failure{Failure: tt.failure}}}))

			// Act
			_, err := r.Newest(context.Background(), "book-1", false)

			// Assert
			var refusal *Refusal
			if !errors.As(err, &refusal) || refusal.Arm != tt.want {
				t.Fatalf("err = %v, want a refusal named %s", err, tt.want)
			}
			if IsUnknownAgent(err) != (tt.want == ArmUnknownAgent) {
				t.Fatalf("IsUnknownAgent = %v for the %s arm", IsUnknownAgent(err), tt.want)
			}
		})
	}
}

func TestReaderReportsAnUnansweredRead(t *testing.T) {
	tests := []struct {
		name   string
		socket func(t *testing.T) string
	}{
		{name: "no store serves the read-only socket", socket: func(t *testing.T) string { return filepath.Join(t.TempDir(), "store.sock") }},
		{name: "the store answered neither a page nor a refusal", socket: func(t *testing.T) string {
			return serve(t, &fakeStore{answer: &storereadv1.ReadHistoryPageResponse{}})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := New(tt.socket(t))

			// Act
			_, err := r.Newest(context.Background(), "book-1", true)

			// Assert
			if !errors.Is(err, ErrUnanswered) {
				t.Fatalf("err = %v, want ErrUnanswered", err)
			}
		})
	}
}
