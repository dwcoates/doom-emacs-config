package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"path/filepath"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	zettelv1 "agentrepl/proto/zettel/v1"
	"agentrepl/zettel/internal/graph"
	"agentrepl/zettel/internal/zlog"

	"connectrpc.com/connect"
)

type fixture struct {
	srv *Server
	db  *graph.DB
	log *bytes.Buffer
}

func newFixture(t *testing.T) fixture {
	t.Helper()
	var buf bytes.Buffer
	log := zlog.New(&buf, &buf, sharedlogging.FixedWindow(sharedlogging.LevelDebug), time.Now)
	n := 0
	db, err := graph.Open(context.Background(), filepath.Join(t.TempDir(), "z.db"), log, graph.Options{
		NewID: func() string { n++; return fmt.Sprintf("id%d", n) },
	})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = db.Close() })
	return fixture{srv: New(db, log), db: db, log: &buf}
}

func ctx() context.Context { return context.Background() }

func threadStart() *zettelv1.NodeParent {
	return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_ThreadStart{ThreadStart: &zettelv1.NodeParentThreadStart{}}}
}

func begin(thread, turn, question string, parent *zettelv1.NodeParent) *connect.Request[zettelv1.BeginExchangeRequest] {
	return connect.NewRequest(&zettelv1.BeginExchangeRequest{
		Thread: &zettelv1.ThreadId{Id: thread}, Turn: &zettelv1.ExchangeTurn{TurnId: turn},
		Question: &zettelv1.NodeQuestion{Text: question}, Parent: parent,
	})
}

// answered drives one exchange to a node through the handlers.
func (f fixture) answered(t *testing.T, thread, turn string, parent *zettelv1.NodeParent) *zettelv1.Node {
	t.Helper()
	b, err := f.srv.BeginExchange(ctx(), begin(thread, turn, "q "+turn, parent))
	if err != nil || b.Msg.GetSuccess() == nil {
		t.Fatalf("begin: %v %v", err, b.Msg)
	}
	c, err := f.srv.CompleteExchange(ctx(), connect.NewRequest(&zettelv1.CompleteExchangeRequest{
		Exchange: b.Msg.GetSuccess().GetExchange(), AnswerMarkdown: "a " + turn,
	}))
	if err != nil || c.Msg.GetSuccess() == nil {
		t.Fatalf("complete: %v %v", err, c.Msg)
	}
	return c.Msg.GetSuccess().GetNode()
}

func TestBeginExchangeArms(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest]
		want    func(*zettelv1.BeginExchangeFailure) bool
	}{
		{"an unset thread", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			return begin("", "turn", "q", threadStart())
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "thread" }},
		{"a blank turn", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			return begin("t", " ", "q", threadStart())
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "turn" }},
		{"a blank question", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			return begin("t", "turn", "", threadStart())
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "question" }},
		{"an unset parent", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			return begin("t", "turn", "q", nil)
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "parent" }},
		{"a zero revision", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			return begin("t", "turn", "q", &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
				Node: &zettelv1.NodeId{Id: "n"}, Revision: &zettelv1.RevisionNumber{},
			}}})
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "parent.node.revision" }},
		{"a blank quote", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			return begin("t", "turn", "q", &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
				Node: &zettelv1.NodeId{Id: "n"}, Revision: &zettelv1.RevisionNumber{Number: 1}, Quote: &zettelv1.NodeQuote{},
			}}})
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "parent.node.quote.text" }},
		{"an unknown parent", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			return begin("t", "turn", "q", &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
				Node: &zettelv1.NodeId{Id: "nope"}, Revision: &zettelv1.RevisionNumber{Number: 1},
			}}})
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetUnknownParent() != nil }},
		{"a second thread start", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			f.answered(t, "t", "turn1", threadStart())
			return begin("t", "turn2", "q", threadStart())
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetThreadAlreadyStarted() != nil }},
		{"a turn begun twice", func(t *testing.T, f fixture) *connect.Request[zettelv1.BeginExchangeRequest] {
			if _, err := f.srv.BeginExchange(ctx(), begin("t", "turn1", "q", threadStart())); err != nil {
				t.Fatal(err)
			}
			return begin("t2", "turn1", "q", threadStart())
		}, func(f *zettelv1.BeginExchangeFailure) bool { return f.GetTurnAlreadyBegun().GetExisting().GetId() != "" }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			req := tc.arrange(t, f)
			f.log.Reset()

			// Act
			resp, err := f.srv.BeginExchange(ctx(), req)

			// Assert
			if err != nil {
				t.Fatalf("transport error %v, want a failure arm", err)
			}
			if failure := resp.Msg.GetFailure(); failure == nil || !tc.want(failure) || failure.GetDetail() == "" {
				t.Fatalf("response = %v, want the refusal arm with a detail", resp.Msg)
			}
			if !strings.Contains(f.log.String(), `"message":"refused a request"`) {
				t.Errorf("the refusal was not logged: %q", f.log.String())
			}
		})
	}
}

func TestExchangeRoundTripThroughTheHandlers(t *testing.T) {
	// Arrange
	f := newFixture(t)
	root := f.answered(t, "t", "turn1", threadStart())
	quoted := &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
		Node: root.GetId(), Revision: root.GetAnswer().GetNumber(),
		Quote: &zettelv1.NodeQuote{Text: "a", QuestionContext: &zettelv1.NodeQuoteContext{Text: "a turn1"}},
	}}}
	child := f.answered(t, "t", "turn2", quoted)
	pend, _ := f.srv.BeginExchange(ctx(), begin("t", "turn3", "in flight", &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
		Node: child.GetId(), Revision: &zettelv1.RevisionNumber{Number: 1},
	}}}))

	// Act
	resp, err := f.srv.GetThread(ctx(), connect.NewRequest(&zettelv1.GetThreadRequest{Thread: &zettelv1.ThreadId{Id: "t"}}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("GetThread: %v %v", err, resp.Msg)
	}
	s := resp.Msg.GetSuccess()
	if len(s.GetNodes()) != 2 || len(s.GetPending()) != 1 {
		t.Fatalf("nodes=%d pending=%d, want 2 and 1", len(s.GetNodes()), len(s.GetPending()))
	}
	got := s.GetNodes()[1]
	if got.GetParent().GetNode().GetQuote().GetQuestionContext().GetText() != "a turn1" || got.GetAnswer().GetMarkdown() != "a turn2" {
		t.Errorf("child = %v, want its quote, context and answer served", got)
	}
	if s.GetNodes()[0].GetParent().GetThreadStart() == nil {
		t.Errorf("root parent = %v, want thread_start", s.GetNodes()[0].GetParent())
	}
	if s.GetPending()[0].GetId().GetId() != pend.Msg.GetSuccess().GetExchange().GetId() || s.GetPending()[0].GetTurn().GetTurnId() != "turn3" {
		t.Errorf("pending = %v, want the exchange in flight", s.GetPending()[0])
	}
}

func TestCompleteAndAbandonArms(t *testing.T) {
	cases := []struct {
		name string
		act  func(f fixture) (any, error)
		want func(any) bool
	}{
		{"complete with no exchange", func(f fixture) (any, error) {
			r, err := f.srv.CompleteExchange(ctx(), connect.NewRequest(&zettelv1.CompleteExchangeRequest{AnswerMarkdown: "a"}))
			return r.Msg, err
		}, func(m any) bool {
			return m.(*zettelv1.CompleteExchangeResponse).GetFailure().GetInvalidRequest().GetField() == "exchange"
		}},
		{"complete with a blank answer", func(f fixture) (any, error) {
			r, err := f.srv.CompleteExchange(ctx(), connect.NewRequest(&zettelv1.CompleteExchangeRequest{Exchange: &zettelv1.ExchangeId{Id: "x"}}))
			return r.Msg, err
		}, func(m any) bool {
			return m.(*zettelv1.CompleteExchangeResponse).GetFailure().GetInvalidRequest().GetField() == "answer_markdown"
		}},
		{"complete an unknown exchange", func(f fixture) (any, error) {
			r, err := f.srv.CompleteExchange(ctx(), connect.NewRequest(&zettelv1.CompleteExchangeRequest{Exchange: &zettelv1.ExchangeId{Id: "x"}, AnswerMarkdown: "a"}))
			return r.Msg, err
		}, func(m any) bool { return m.(*zettelv1.CompleteExchangeResponse).GetFailure().GetUnknownExchange() != nil }},
		{"abandon with no exchange", func(f fixture) (any, error) {
			r, err := f.srv.AbandonExchange(ctx(), connect.NewRequest(&zettelv1.AbandonExchangeRequest{}))
			return r.Msg, err
		}, func(m any) bool {
			return m.(*zettelv1.AbandonExchangeResponse).GetFailure().GetInvalidRequest().GetField() == "exchange"
		}},
		{"abandon an unknown exchange", func(f fixture) (any, error) {
			r, err := f.srv.AbandonExchange(ctx(), connect.NewRequest(&zettelv1.AbandonExchangeRequest{Exchange: &zettelv1.ExchangeId{Id: "x"}}))
			return r.Msg, err
		}, func(m any) bool { return m.(*zettelv1.AbandonExchangeResponse).GetFailure().GetUnknownExchange() != nil }},
		{"abandon a real exchange", func(f fixture) (any, error) {
			b, _ := f.srv.BeginExchange(ctx(), begin("t", "turn", "q", threadStart()))
			r, err := f.srv.AbandonExchange(ctx(), connect.NewRequest(&zettelv1.AbandonExchangeRequest{Exchange: b.Msg.GetSuccess().GetExchange()}))
			return r.Msg, err
		}, func(m any) bool { return m.(*zettelv1.AbandonExchangeResponse).GetSuccess() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			msg, err := tc.act(f)

			// Assert
			if err != nil || !tc.want(msg) {
				t.Errorf("response = %v, err = %v", msg, err)
			}
		})
	}
}

func TestRecordSightingsArms(t *testing.T) {
	cases := []struct {
		name  string
		nodes func(known string) []*zettelv1.NodeId
		want  func(*zettelv1.RecordSightingsResponse) bool
	}{
		{"none named", func(string) []*zettelv1.NodeId { return nil },
			func(r *zettelv1.RecordSightingsResponse) bool { return r.GetFailure().GetInvalidRequest().GetField() == "nodes" }},
		{"an empty id", func(string) []*zettelv1.NodeId { return []*zettelv1.NodeId{{Id: ""}} },
			func(r *zettelv1.RecordSightingsResponse) bool { return r.GetFailure().GetInvalidRequest().GetField() == "nodes" }},
		{"an unknown node", func(known string) []*zettelv1.NodeId { return []*zettelv1.NodeId{{Id: known}, {Id: "nope"}} },
			func(r *zettelv1.RecordSightingsResponse) bool { return r.GetFailure().GetUnknownNode().GetNode().GetId() == "nope" }},
		{"a known node", func(known string) []*zettelv1.NodeId { return []*zettelv1.NodeId{{Id: known}} },
			func(r *zettelv1.RecordSightingsResponse) bool { return r.GetSuccess() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			known := f.answered(t, "t", "turn", threadStart()).GetId().GetId()

			// Act
			resp, err := f.srv.RecordSightings(ctx(), connect.NewRequest(&zettelv1.RecordSightingsRequest{Nodes: tc.nodes(known)}))

			// Assert
			if err != nil || !tc.want(resp.Msg) {
				t.Errorf("response = %v, err = %v", resp.Msg, err)
			}
		})
	}
}

func TestGetThreadRefusesAnUnsetThread(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	resp, err := f.srv.GetThread(ctx(), connect.NewRequest(&zettelv1.GetThreadRequest{}))

	// Assert
	if err != nil || resp.Msg.GetFailure().GetInvalidRequest().GetField() != "thread" {
		t.Errorf("response = %v, err = %v", resp.Msg, err)
	}
}

func TestStorageFailureAnswersTheStorageArm(t *testing.T) {
	// Arrange
	f := newFixture(t)
	if err := f.db.Close(); err != nil {
		t.Fatal(err)
	}

	// Act
	resp, err := f.srv.ListPendingExchanges(ctx(), connect.NewRequest(&zettelv1.ListPendingExchangesRequest{}))

	// Assert
	if err != nil || resp.Msg.GetFailure().GetStorageFailure() == nil {
		t.Fatalf("response = %v, err = %v, want the storage arm", resp.Msg, err)
	}
	var found bool
	for _, line := range strings.Split(strings.TrimSpace(f.log.String()), "\n") {
		var rec map[string]any
		if json.Unmarshal([]byte(line), &rec) == nil && rec["operation"] == "zettel.exchange.list" && rec["level"] == "error" {
			found = true
		}
	}
	if !found {
		t.Errorf("no error record for the storage failure: %q", f.log.String())
	}
}
