package zettelclient

import (
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	zettelv1 "agentrepl/proto/zettel/v1"
	"agentrepl/proto/zettel/v1/zettelv1connect"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	"agent-repld/internal/wsm"
)

// fakeZettel answers each call with what the test set.
type fakeZettel struct {
	zettelv1connect.UnimplementedZettelHandler
	record    *zettelv1.RecordExchangeResponse
	thread    *zettelv1.GetThreadResponse
	sightings *zettelv1.RecordSightingsResponse
	search    *zettelv1.SearchThreadsResponse
	deleted   *zettelv1.DeleteThreadResponse
	err       error
	got       *zettelv1.RecordExchangeRequest
}

func (f *fakeZettel) RecordExchange(_ context.Context, req *connect.Request[zettelv1.RecordExchangeRequest]) (*connect.Response[zettelv1.RecordExchangeResponse], error) {
	f.got = req.Msg
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.record), nil
}

func (f *fakeZettel) GetThread(context.Context, *connect.Request[zettelv1.GetThreadRequest]) (*connect.Response[zettelv1.GetThreadResponse], error) {
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.thread), nil
}

func (f *fakeZettel) RecordSightings(context.Context, *connect.Request[zettelv1.RecordSightingsRequest]) (*connect.Response[zettelv1.RecordSightingsResponse], error) {
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.sightings), nil
}

func (f *fakeZettel) SearchThreads(context.Context, *connect.Request[zettelv1.SearchThreadsRequest]) (*connect.Response[zettelv1.SearchThreadsResponse], error) {
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.search), nil
}

func (f *fakeZettel) DeleteThread(context.Context, *connect.Request[zettelv1.DeleteThreadRequest]) (*connect.Response[zettelv1.DeleteThreadResponse], error) {
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.deleted), nil
}

// serve runs the fake on a private unix socket and answers a Client of it, so
// the client's own dialing is what the test drives.
func serve(t *testing.T, f *fakeZettel) Client {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "zc")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	sock := filepath.Join(dir, "z.sock")
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	mux := http.NewServeMux()
	path, handler := zettelv1connect.NewZettelHandler(f)
	mux.Handle(path, handler)
	srv := &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{})}
	go func() { _ = srv.Serve(ln) }()
	t.Cleanup(func() { _ = srv.Close() })
	return New(sock)
}

func question() wsm.ThreadQuestion {
	return wsm.ThreadQuestion{Text: "What are quarks?", Parent: &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_ThreadStart{ThreadStart: &zettelv1.NodeParentThreadStart{}}}}
}

func TestRecordExchangeSendsTheQuestionAndAnswersTheNode(t *testing.T) {
	// Arrange
	f := &fakeZettel{record: &zettelv1.RecordExchangeResponse{Result: &zettelv1.RecordExchangeResponse_Success{
		Success: &zettelv1.RecordExchangeSuccess{Node: &zettelv1.Node{Id: &zettelv1.NodeId{Id: "n1"}}},
	}}}
	c := serve(t, f)

	// Act
	node, err := c.RecordExchange(context.Background(), "ws1", "turn1", question(), "Elementary particles.")

	// Assert
	if err != nil || node.GetId().GetId() != "n1" {
		t.Fatalf("node = %v, err = %v", node, err)
	}
	if f.got.GetThread().GetId() != "ws1" || f.got.GetTurn().GetTurnId() != "turn1" || f.got.GetQuestion().GetText() != "What are quarks?" ||
		f.got.GetAnswerMarkdown() != "Elementary particles." || f.got.GetParent().GetThreadStart() == nil {
		t.Errorf("request = %v, want every field carried", f.got)
	}
}

func TestFailureArmsBecomeRefusals(t *testing.T) {
	cases := []struct {
		name string
		f    *fakeZettel
		call func(c Client) error
		want string
	}{
		{"record: unknown parent", &fakeZettel{record: &zettelv1.RecordExchangeResponse{Result: &zettelv1.RecordExchangeResponse_Failure{
			Failure: &zettelv1.RecordExchangeFailure{Detail: "no parent", Kind: &zettelv1.RecordExchangeFailure_UnknownParent{UnknownParent: &zettelv1.RecordExchangeUnknownParent{}}},
		}}}, func(c Client) error {
			_, err := c.RecordExchange(context.Background(), "ws1", "turn1", question(), "a")
			return err
		}, "unknown_parent"},
		{"record: thread already started", &fakeZettel{record: &zettelv1.RecordExchangeResponse{Result: &zettelv1.RecordExchangeResponse_Failure{
			Failure: &zettelv1.RecordExchangeFailure{Detail: "started", Kind: &zettelv1.RecordExchangeFailure_ThreadAlreadyStarted{ThreadAlreadyStarted: &zettelv1.RecordExchangeThreadAlreadyStarted{}}},
		}}}, func(c Client) error {
			_, err := c.RecordExchange(context.Background(), "ws1", "turn1", question(), "a")
			return err
		}, "thread_already_started"},
		{"thread: storage failure", &fakeZettel{thread: &zettelv1.GetThreadResponse{Result: &zettelv1.GetThreadResponse_Failure{
			Failure: &zettelv1.GetThreadFailure{Detail: "disk", Kind: &zettelv1.GetThreadFailure_StorageFailure{StorageFailure: &zettelv1.GetThreadStorageFailure{}}},
		}}}, func(c Client) error {
			_, err := c.Thread(context.Background(), "ws1")
			return err
		}, "storage_failure"},
		{"sightings: unknown node", &fakeZettel{sightings: &zettelv1.RecordSightingsResponse{Result: &zettelv1.RecordSightingsResponse_Failure{
			Failure: &zettelv1.RecordSightingsFailure{Detail: "nope", Kind: &zettelv1.RecordSightingsFailure_UnknownNode{UnknownNode: &zettelv1.RecordSightingsUnknownNode{}}},
		}}}, func(c Client) error {
			return c.RecordSightings(context.Background(), []*zettelv1.NodeId{{Id: "x"}})
		}, "unknown_node"},
		{"thread: no outcome set", &fakeZettel{thread: &zettelv1.GetThreadResponse{}}, func(c Client) error {
			_, err := c.Thread(context.Background(), "ws1")
			return err
		}, "unset"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			c := serve(t, tc.f)

			// Act
			err := tc.call(c)

			// Assert
			var refusal *RefusedError
			if !errors.As(err, &refusal) || refusal.Kind != tc.want {
				t.Errorf("err = %v, want a %s refusal", err, tc.want)
			}
		})
	}
}

func TestATransportFailureIsUnavailable(t *testing.T) {
	cases := []struct {
		name string
		c    func(t *testing.T) Client
	}{
		{"no socket", func(t *testing.T) Client { return New(filepath.Join(t.TempDir(), "absent.sock")) }},
		{"the handler errors", func(t *testing.T) Client {
			return serve(t, &fakeZettel{err: connect.NewError(connect.CodeInternal, errors.New("boom"))})
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			c := tc.c(t)

			// Act
			_, err := c.Thread(context.Background(), "ws1")

			// Assert
			if !errors.Is(err, ErrUnavailable) {
				t.Errorf("err = %v, want ErrUnavailable", err)
			}
		})
	}
}

func TestThreadAnswersTheNodes(t *testing.T) {
	// Arrange
	c := serve(t, &fakeZettel{thread: &zettelv1.GetThreadResponse{Result: &zettelv1.GetThreadResponse_Success{
		Success: &zettelv1.GetThreadSuccess{Nodes: []*zettelv1.Node{{Id: &zettelv1.NodeId{Id: "n1"}}, {Id: &zettelv1.NodeId{Id: "n2"}}}},
	}}})

	// Act
	nodes, err := c.Thread(context.Background(), "ws1")

	// Assert
	if err != nil || len(nodes) != 2 {
		t.Errorf("nodes = %v, err = %v", nodes, err)
	}
}

func TestSearchThreadsAnswersTheMatches(t *testing.T) {
	// Arrange
	match := &zettelv1.SearchThreadsMatch{Thread: &zettelv1.ThreadId{Id: "t1"}, Node: &zettelv1.NodeId{Id: "n1"}}
	c := serve(t, &fakeZettel{search: &zettelv1.SearchThreadsResponse{Result: &zettelv1.SearchThreadsResponse_Success{
		Success: &zettelv1.SearchThreadsSuccess{Matches: []*zettelv1.SearchThreadsMatch{match}},
	}}})

	// Act
	got, err := c.SearchThreads(context.Background(), "sky")

	// Assert
	if err != nil || len(got) != 1 || got[0].GetThread().GetId() != "t1" {
		t.Fatalf("SearchThreads = %v, %v; want the one match", got, err)
	}
}

func TestDeleteThreadAnswersTheNodesDeleted(t *testing.T) {
	// Arrange
	c := serve(t, &fakeZettel{deleted: &zettelv1.DeleteThreadResponse{Result: &zettelv1.DeleteThreadResponse_Success{
		Success: &zettelv1.DeleteThreadSuccess{NodesDeleted: 3},
	}}})

	// Act
	got, err := c.DeleteThread(context.Background(), "t1")

	// Assert
	if err != nil || got != 3 {
		t.Fatalf("DeleteThread = %d, %v; want 3", got, err)
	}
}

func TestSearchAndDeleteMapEveryOutcome(t *testing.T) {
	tests := []struct {
		name     string
		f        *fakeZettel
		wantKind string
		wantDown bool
	}{
		{name: "search: invalid request", wantKind: "invalid_request", f: &fakeZettel{search: &zettelv1.SearchThreadsResponse{Result: &zettelv1.SearchThreadsResponse_Failure{
			Failure: &zettelv1.SearchThreadsFailure{Kind: &zettelv1.SearchThreadsFailure_InvalidRequest{InvalidRequest: &zettelv1.SearchThreadsInvalidRequest{Field: "query"}}},
		}}}},
		{name: "search: unarmed", wantKind: "unset", f: &fakeZettel{search: &zettelv1.SearchThreadsResponse{}}},
		{name: "search: transport", wantDown: true, f: &fakeZettel{err: connect.NewError(connect.CodeUnavailable, errors.New("down"))}},
		{name: "delete: storage failure", wantKind: "storage_failure", f: &fakeZettel{deleted: &zettelv1.DeleteThreadResponse{Result: &zettelv1.DeleteThreadResponse_Failure{
			Failure: &zettelv1.DeleteThreadFailure{Kind: &zettelv1.DeleteThreadFailure_StorageFailure{StorageFailure: &zettelv1.DeleteThreadStorageFailure{}}},
		}}}},
		{name: "delete: unarmed", wantKind: "unset", f: &fakeZettel{deleted: &zettelv1.DeleteThreadResponse{}}},
		{name: "delete: transport", wantDown: true, f: &fakeZettel{err: connect.NewError(connect.CodeUnavailable, errors.New("down"))}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			c := serve(t, tt.f)

			// Act
			var err error
			if strings.HasPrefix(tt.name, "search") {
				_, err = c.SearchThreads(context.Background(), "sky")
			} else {
				_, err = c.DeleteThread(context.Background(), "t1")
			}

			// Assert
			if tt.wantDown {
				if !errors.Is(err, ErrUnavailable) {
					t.Fatalf("err = %v, want ErrUnavailable", err)
				}
				return
			}
			var refused *RefusedError
			if !errors.As(err, &refused) || refused.Kind != tt.wantKind {
				t.Fatalf("err = %v, want a refusal of kind %s", err, tt.wantKind)
			}
		})
	}
}

func TestEveryZettelFailureHasTheShapeRefusedReads(t *testing.T) {
	// Arrange: every message of zettel.v1 named <Rpc>Failure.
	file := zettelv1.File_zettel_v1_service_proto
	var failures []string
	for i := 0; i < file.Services().Get(0).Methods().Len(); i++ {
		out := file.Services().Get(0).Methods().Get(i).Output()
		result := out.Oneofs().ByName("result")
		for j := 0; result != nil && j < result.Fields().Len(); j++ {
			if f := result.Fields().Get(j); f.Name() == "failure" {
				failures = append(failures, string(f.Message().FullName()))

				// Assert: refused reads `detail` and the `kind` oneof.
				if f.Message().Oneofs().ByName("kind") == nil || f.Message().Fields().ByName("detail") == nil {
					t.Errorf("%s has no `kind` oneof or no `detail`", f.Message().FullName())
				}
			}
		}
	}
	if len(failures) != file.Services().Get(0).Methods().Len() {
		t.Fatalf("failures = %v, want one per rpc", failures)
	}
}
