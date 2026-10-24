package zettelclient

import (
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"path/filepath"
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
