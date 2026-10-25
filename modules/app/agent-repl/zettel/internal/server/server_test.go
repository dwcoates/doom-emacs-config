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

func record(thread, turn, question string, parent *zettelv1.NodeParent) *connect.Request[zettelv1.RecordExchangeRequest] {
	return connect.NewRequest(&zettelv1.RecordExchangeRequest{
		Thread: &zettelv1.ThreadId{Id: thread}, Turn: &zettelv1.ExchangeTurn{TurnId: turn},
		Question: &zettelv1.NodeQuestion{Text: question}, Parent: parent, AnswerMarkdown: "a " + turn,
	})
}

// answered records one exchange through the handler.
func (f fixture) answered(t *testing.T, thread, turn string, parent *zettelv1.NodeParent) *zettelv1.Node {
	t.Helper()
	r, err := f.srv.RecordExchange(ctx(), record(thread, turn, "q "+turn, parent))
	if err != nil || r.Msg.GetSuccess() == nil {
		t.Fatalf("record: %v %v", err, r.Msg)
	}
	return r.Msg.GetSuccess().GetNode()
}

func TestRecordExchangeArms(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest]
		want    func(*zettelv1.RecordExchangeFailure) bool
	}{
		{"an unset thread", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			return record("", "turn", "q", threadStart())
		}, func(f *zettelv1.RecordExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "thread" }},
		{"a blank turn", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			return record("t", " ", "q", threadStart())
		}, func(f *zettelv1.RecordExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "turn" }},
		{"a blank question", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			return record("t", "turn", "", threadStart())
		}, func(f *zettelv1.RecordExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "question" }},
		{"a blank answer", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			r := record("t", "turn", "q", threadStart())
			r.Msg.AnswerMarkdown = ""
			return r
		}, func(f *zettelv1.RecordExchangeFailure) bool {
			return f.GetInvalidRequest().GetField() == "answer_markdown"
		}},
		{"an unset parent", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			return record("t", "turn", "q", nil)
		}, func(f *zettelv1.RecordExchangeFailure) bool { return f.GetInvalidRequest().GetField() == "parent" }},
		{"a zero revision", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			return record("t", "turn", "q", &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
				Node: &zettelv1.NodeId{Id: "n"}, Revision: &zettelv1.RevisionNumber{},
			}}})
		}, func(f *zettelv1.RecordExchangeFailure) bool {
			return f.GetInvalidRequest().GetField() == "parent.node.revision"
		}},
		{"a blank quote", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			return record("t", "turn", "q", &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
				Node: &zettelv1.NodeId{Id: "n"}, Revision: &zettelv1.RevisionNumber{Number: 1}, Quote: &zettelv1.NodeQuote{},
			}}})
		}, func(f *zettelv1.RecordExchangeFailure) bool {
			return f.GetInvalidRequest().GetField() == "parent.node.quote.text"
		}},
		{"an unknown parent", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			return record("t", "turn", "q", &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
				Node: &zettelv1.NodeId{Id: "nope"}, Revision: &zettelv1.RevisionNumber{Number: 1},
			}}})
		}, func(f *zettelv1.RecordExchangeFailure) bool { return f.GetUnknownParent() != nil }},
		{"a second thread start", func(t *testing.T, f fixture) *connect.Request[zettelv1.RecordExchangeRequest] {
			f.answered(t, "t", "turn1", threadStart())
			return record("t", "turn2", "q", threadStart())
		}, func(f *zettelv1.RecordExchangeFailure) bool { return f.GetThreadAlreadyStarted() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			req := tc.arrange(t, f)
			f.log.Reset()

			// Act
			resp, err := f.srv.RecordExchange(ctx(), req)

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

func TestRecordedNodesServeThroughGetThread(t *testing.T) {
	// Arrange
	f := newFixture(t)
	root := f.answered(t, "t", "turn1", threadStart())
	quoted := &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
		Node: root.GetId(), Revision: root.GetAnswer().GetNumber(),
		Quote: &zettelv1.NodeQuote{Text: "a", QuestionContext: &zettelv1.NodeQuoteContext{Text: "a turn1"}},
	}}}
	f.answered(t, "t", "turn2", quoted)

	// Act
	resp, err := f.srv.GetThread(ctx(), connect.NewRequest(&zettelv1.GetThreadRequest{Thread: &zettelv1.ThreadId{Id: "t"}}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("GetThread: %v %v", err, resp.Msg)
	}
	nodes := resp.Msg.GetSuccess().GetNodes()
	if len(nodes) != 2 {
		t.Fatalf("nodes=%d, want 2", len(nodes))
	}
	got := nodes[1]
	if got.GetParent().GetNode().GetQuote().GetQuestionContext().GetText() != "a turn1" || got.GetAnswer().GetMarkdown() != "a turn2" || got.GetTurn().GetTurnId() != "turn2" {
		t.Errorf("child = %v, want its quote, context, answer and turn served", got)
	}
	if nodes[0].GetParent().GetThreadStart() == nil {
		t.Errorf("root parent = %v, want thread_start", nodes[0].GetParent())
	}
}

func TestRecordExchangeAnswersTheExistingNodeForARecordedTurn(t *testing.T) {
	// Arrange
	f := newFixture(t)
	first := f.answered(t, "t", "turn1", threadStart())

	// Act
	resp, err := f.srv.RecordExchange(ctx(), record("t", "turn1", "again", threadStart()))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetNode().GetId().GetId() != first.GetId().GetId() {
		t.Errorf("response = %v, err = %v, want the first node", resp.Msg, err)
	}
}

func TestRecordSightingsArms(t *testing.T) {
	cases := []struct {
		name  string
		nodes func(known string) []*zettelv1.NodeId
		want  func(*zettelv1.RecordSightingsResponse) bool
	}{
		{"none named", func(string) []*zettelv1.NodeId { return nil },
			func(r *zettelv1.RecordSightingsResponse) bool {
				return r.GetFailure().GetInvalidRequest().GetField() == "nodes"
			}},
		{"an empty id", func(string) []*zettelv1.NodeId { return []*zettelv1.NodeId{{Id: ""}} },
			func(r *zettelv1.RecordSightingsResponse) bool {
				return r.GetFailure().GetInvalidRequest().GetField() == "nodes"
			}},
		{"an unknown node", func(known string) []*zettelv1.NodeId { return []*zettelv1.NodeId{{Id: known}, {Id: "nope"}} },
			func(r *zettelv1.RecordSightingsResponse) bool {
				return r.GetFailure().GetUnknownNode().GetNode().GetId() == "nope"
			}},
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
	resp, err := f.srv.GetThread(ctx(), connect.NewRequest(&zettelv1.GetThreadRequest{Thread: &zettelv1.ThreadId{Id: "t"}}))

	// Assert
	if err != nil || resp.Msg.GetFailure().GetStorageFailure() == nil {
		t.Fatalf("response = %v, err = %v, want the storage arm", resp.Msg, err)
	}
	var found bool
	for _, line := range strings.Split(strings.TrimSpace(f.log.String()), "\n") {
		var rec map[string]any
		if json.Unmarshal([]byte(line), &rec) == nil && rec["operation"] == "zettel.thread.read" && rec["level"] == "error" {
			found = true
		}
	}
	if !found {
		t.Errorf("no error record for the storage failure: %q", f.log.String())
	}
}

func TestSearchThreadsArms(t *testing.T) {
	cases := []struct {
		name  string
		query string
		want  func(*zettelv1.SearchThreadsResponse) bool
	}{
		{"a blank query", "  ", func(r *zettelv1.SearchThreadsResponse) bool {
			return r.GetFailure().GetInvalidRequest().GetField() == "query"
		}},
		{"a matching query", "turn1", func(r *zettelv1.SearchThreadsResponse) bool {
			m := r.GetSuccess().GetMatches()
			return len(m) == 1 && m[0].GetThread().GetId() == "t" && m[0].GetNode().GetId() == "id1" &&
				len(m[0].GetSnippet().GetParts()) > 0
		}},
		{"a query nothing says", "nowhere", func(r *zettelv1.SearchThreadsResponse) bool {
			return r.GetSuccess() != nil && len(r.GetSuccess().GetMatches()) == 0
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.answered(t, "t", "turn1", threadStart())

			// Act
			resp, err := f.srv.SearchThreads(ctx(), connect.NewRequest(&zettelv1.SearchThreadsRequest{Query: tc.query}))

			// Assert
			if err != nil || !tc.want(resp.Msg) {
				t.Errorf("response = %v, err = %v", resp.Msg, err)
			}
		})
	}
}

func TestMatchToProtoMarksEachPart(t *testing.T) {
	// Act
	got := MatchToProto(graph.Match{Thread: "t", Node: "n", Snippet: []graph.SnippetPart{{Text: "a "}, {Text: "b", Hit: true}}})

	// Assert
	parts := got.GetSnippet().GetParts()
	if len(parts) != 2 || parts[0].GetPlain() != "a " || parts[1].GetHit() != "b" {
		t.Fatalf("parts = %v, want a plain run then a hit", parts)
	}
}

func TestDeleteThreadArms(t *testing.T) {
	cases := []struct {
		name   string
		thread *zettelv1.ThreadId
		want   func(*zettelv1.DeleteThreadResponse) bool
	}{
		{"an unset thread", nil, func(r *zettelv1.DeleteThreadResponse) bool {
			return r.GetFailure().GetInvalidRequest().GetField() == "thread"
		}},
		{"a thread with a node", &zettelv1.ThreadId{Id: "t"}, func(r *zettelv1.DeleteThreadResponse) bool {
			return r.GetSuccess().GetNodesDeleted() == 1
		}},
		{"a thread zettel holds nothing of", &zettelv1.ThreadId{Id: "other"}, func(r *zettelv1.DeleteThreadResponse) bool {
			return r.GetSuccess() != nil && r.GetSuccess().GetNodesDeleted() == 0
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.answered(t, "t", "turn1", threadStart())

			// Act
			resp, err := f.srv.DeleteThread(ctx(), connect.NewRequest(&zettelv1.DeleteThreadRequest{Thread: tc.thread}))

			// Assert
			if err != nil || !tc.want(resp.Msg) {
				t.Errorf("response = %v, err = %v", resp.Msg, err)
			}
		})
	}
}

func TestSearchAndDeleteAnswerTheStorageArm(t *testing.T) {
	// Arrange
	f := newFixture(t)
	if err := f.db.Close(); err != nil {
		t.Fatal(err)
	}

	// Act
	search, serr := f.srv.SearchThreads(ctx(), connect.NewRequest(&zettelv1.SearchThreadsRequest{Query: "sky"}))
	del, derr := f.srv.DeleteThread(ctx(), connect.NewRequest(&zettelv1.DeleteThreadRequest{Thread: &zettelv1.ThreadId{Id: "t"}}))

	// Assert
	if serr != nil || search.Msg.GetFailure().GetStorageFailure() == nil {
		t.Errorf("search = %v, err = %v, want the storage arm", search.Msg, serr)
	}
	if derr != nil || del.Msg.GetFailure().GetStorageFailure() == nil {
		t.Errorf("delete = %v, err = %v, want the storage arm", del.Msg, derr)
	}
}
