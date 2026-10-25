package chat

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	zettelv1 "agentrepl/proto/zettel/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

const thr = ids.WorkspaceID("thread-1")

// fakeZettel is zettel in memory: RecordExchange is idempotent by turn, as the
// service's is.
type fakeZettel struct {
	nodes     []*zettelv1.Node
	loadErr   error
	recordErr error
	sightErr  error
	sighted   [][]string
	now       int64
	deleteErr error
	deleted   []ids.WorkspaceID
	matches   []*zettelv1.SearchThreadsMatch
	searchErr error
}

func (f *fakeZettel) RecordExchange(_ context.Context, thread ids.WorkspaceID, turn ids.TurnID, q wsm.ThreadQuestion, answer string) (*zettelv1.Node, error) {
	if f.recordErr != nil {
		return nil, f.recordErr
	}
	for _, n := range f.nodes {
		if n.GetTurn().GetTurnId() == string(turn) {
			return n, nil
		}
	}
	f.now++
	n := &zettelv1.Node{
		Id: &zettelv1.NodeId{Id: fmt.Sprintf("n%d", len(f.nodes)+1)}, Thread: &zettelv1.ThreadId{Id: string(thread)},
		Parent: q.Parent, Question: &zettelv1.NodeQuestion{Text: q.Text},
		Answer:      &zettelv1.NodeRevision{Number: &zettelv1.RevisionNumber{Number: 1}, Markdown: answer},
		CreatedAtMs: f.now, Sightings: &zettelv1.NodeSightings{}, Turn: &zettelv1.ExchangeTurn{TurnId: string(turn)},
	}
	f.nodes = append(f.nodes, n)
	return n, nil
}

func (f *fakeZettel) Thread(context.Context, ids.WorkspaceID) ([]*zettelv1.Node, error) {
	if f.loadErr != nil {
		return nil, f.loadErr
	}
	return append([]*zettelv1.Node(nil), f.nodes...), nil
}

func (f *fakeZettel) RecordSightings(_ context.Context, nodes []*zettelv1.NodeId) error {
	if f.sightErr != nil {
		return f.sightErr
	}
	var named []string
	for _, n := range nodes {
		named = append(named, n.GetId())
	}
	f.sighted = append(f.sighted, named)
	return nil
}

func (f *fakeZettel) SearchThreads(_ context.Context, _ string) ([]*zettelv1.SearchThreadsMatch, error) {
	return f.matches, f.searchErr
}

func (f *fakeZettel) DeleteThread(_ context.Context, thread ids.WorkspaceID) (uint32, error) {
	if f.deleteErr != nil {
		return 0, f.deleteErr
	}
	f.deleted = append(f.deleted, thread)
	n := uint32(len(f.nodes))
	f.nodes = nil
	return n, nil
}

// seed adds a node asked of parent ("" for the root) with sightings seen.
func (f *fakeZettel) seed(parent string, seen uint64) string {
	f.now++
	id := fmt.Sprintf("n%d", len(f.nodes)+1)
	n := &zettelv1.Node{
		Id: &zettelv1.NodeId{Id: id}, Thread: &zettelv1.ThreadId{Id: string(thr)}, Parent: parentNode(parent, ""),
		Question:    &zettelv1.NodeQuestion{Text: "q " + id},
		Answer:      &zettelv1.NodeRevision{Number: &zettelv1.RevisionNumber{Number: 1}, Markdown: "a " + id},
		CreatedAtMs: f.now, Sightings: &zettelv1.NodeSightings{Count: seen}, Turn: &zettelv1.ExchangeTurn{TurnId: "turn-" + id},
	}
	f.nodes = append(f.nodes, n)
	return id
}

func parentNode(parent, quote string) *zettelv1.NodeParent {
	if parent == "" {
		return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_ThreadStart{ThreadStart: &zettelv1.NodeParentThreadStart{}}}
	}
	of := &zettelv1.NodeParentNode{Node: &zettelv1.NodeId{Id: parent}, Revision: &zettelv1.RevisionNumber{Number: 1}}
	if quote != "" {
		of.Quote = &zettelv1.NodeQuote{Text: quote}
	}
	return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: of}}
}

// fakeTurns is the prompt records.
type fakeTurns struct {
	turns   map[ids.TurnID]wsm.Turn
	open    []wsm.Turn
	held    []wsm.HeldPrompt
	turnErr error
	openErr error
}

func (f *fakeTurns) Turn(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) (wsm.Turn, error) {
	if f.turnErr != nil {
		return wsm.Turn{}, f.turnErr
	}
	return f.turns[turn], nil
}

func (f *fakeTurns) OpenTurns(context.Context, ids.WorkspaceID) ([]wsm.Turn, error) {
	return f.open, f.openErr
}

func (f *fakeTurns) HeldPrompts(context.Context, ids.WorkspaceID) ([]wsm.HeldPrompt, error) {
	return f.held, nil
}

// ask opens a turn asking text of parent.
func (f *fakeTurns) ask(turn ids.TurnID, text, parent, quote string) {
	t := wsm.Turn{ID: turn, StartedAt: time.UnixMilli(int64(len(f.open))), Question: &wsm.ThreadQuestion{Text: text, Parent: parentNode(parent, quote)}}
	f.turns[turn] = t
	f.open = append(f.open, t)
}

// close closes every open turn, as the records say once the turn ends.
func (f *fakeTurns) close() { f.open = nil }

type fixture struct {
	t      *testing.T
	zettel *fakeZettel
	turns  *fakeTurns
	log    *dlog.TestLogger
	svc    *Service
}

func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{t: t, zettel: &fakeZettel{}, turns: &fakeTurns{turns: map[ids.TurnID]wsm.Turn{}}, log: dlog.NewTestLogger()}
	f.svc = New(Deps{
		Zettel: f.zettel, Turns: f.turns, Context: context.Background(),
		Log: func(ids.WorkspaceID) dlog.Logger { return f.log },
		Go:  func(job func()) { job() },
	})
	return f
}

func (f *fixture) open() {
	f.t.Helper()
	if err := f.svc.Open(context.Background(), thr); err != nil {
		f.t.Fatalf("Open: %v", err)
	}
}

func (f *fixture) view() *frontendv1.ChatView {
	f.t.Helper()
	v, ok := f.svc.Topic(thr).Latest()
	if !ok {
		f.t.Fatal("no view was published")
	}
	return v
}

// path is the shown exchanges' node ids.
func (f *fixture) path() []string {
	f.t.Helper()
	var out []string
	for _, ex := range f.view().GetConversation().GetExchanges() {
		out = append(out, ex.GetNode().GetId())
	}
	return out
}

func (f *fixture) exchange(id string) *frontendv1.ChatExchange {
	f.t.Helper()
	for _, ex := range f.view().GetConversation().GetExchanges() {
		if ex.GetNode().GetId() == id {
			return ex
		}
	}
	f.t.Fatalf("node %s is not shown", id)
	return nil
}

func (f *fixture) records(level, op string) int {
	n := 0
	for _, r := range f.log.Records() {
		if r.Level == level && r.Operation == op {
			n++
		}
	}
	return n
}

func equal(a, b []string) bool { return fmt.Sprint(a) == fmt.Sprint(b) }

func TestAnEmptyThreadDrawsItsHint(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	f.open()

	// Assert
	if got := f.view().GetEmpty().GetHint().GetText(); got != EmptyHint {
		t.Fatalf("hint = %q, want %q", got, EmptyHint)
	}
}

func TestTheShownPathPicksTheLeastSeenChild(t *testing.T) {
	cases := []struct {
		name string
		seen [2]uint64
		want string
	}{
		{name: "the older child seen less", seen: [2]uint64{0, 3}, want: "n2"},
		{name: "the newer child seen less", seen: [2]uint64{3, 0}, want: "n3"},
		{name: "a tie goes to the newest", seen: [2]uint64{1, 1}, want: "n3"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			root := f.zettel.seed("", 0)
			f.zettel.seed(root, tc.seen[0])
			f.zettel.seed(root, tc.seen[1])

			// Act
			f.open()

			// Assert
			if got := f.path(); !equal(got, []string{root, tc.want}) {
				t.Fatalf("path = %v, want [%s %s]", got, root, tc.want)
			}
		})
	}
}

func TestAPickHoldsForTheVisitWhateverIsSeen(t *testing.T) {
	// Arrange
	f := newFixture(t)
	root := f.zettel.seed("", 0)
	f.zettel.seed(root, 0)
	f.zettel.seed(root, 0)
	f.open()
	if err := f.svc.Sightings(context.Background(), thr, []NodeID{"n3", "n3"}); err != nil {
		t.Fatalf("Sightings: %v", err)
	}

	// Act
	f.open()

	// Assert
	if got := f.path(); !equal(got, []string{"n1", "n3"}) {
		t.Fatalf("path = %v, want the visit's pick n3 to stand", got)
	}
}

func TestSwitchingASiblingMovesAndWraps(t *testing.T) {
	cases := []struct {
		name  string
		way   Direction
		want  string
		label string
	}{
		{name: "next wraps to the first", way: Next, want: "n2", label: "1 of 2"},
		{name: "previous moves to the older", way: Previous, want: "n2", label: "1 of 2"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			root := f.zettel.seed("", 0)
			f.zettel.seed(root, 0)
			f.zettel.seed(root, 0)
			f.open()

			// Act
			err := f.svc.SwitchSibling(context.Background(), thr, "n3", tc.way)

			// Assert
			if err != nil {
				t.Fatalf("SwitchSibling: %v", err)
			}
			if got := f.path(); !equal(got, []string{root, tc.want}) {
				t.Fatalf("path = %v, want [%s %s]", got, root, tc.want)
			}
			if got := f.exchange(tc.want).GetSiblings().GetPosition().GetLabel(); got != tc.label {
				t.Fatalf("label = %q, want %q", got, tc.label)
			}
		})
	}
}

func TestLeavingABranchEndsItsVisit(t *testing.T) {
	// Arrange: n1 has children n2, n3; n3 has children n4, n5. n3 is shown,
	// and its pick n5 is switched to n4.
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.zettel.seed("n1", 0)
	f.zettel.seed("n1", 0)
	f.zettel.seed("n3", 0)
	f.zettel.seed("n3", 0)
	f.open()
	ctx := context.Background()
	if err := f.svc.SwitchSibling(ctx, thr, "n5", Previous); err != nil {
		t.Fatal(err)
	}

	// Act: leave n3 and come back.
	if err := f.svc.SwitchSibling(ctx, thr, "n3", Next); err != nil {
		t.Fatal(err)
	}
	if err := f.svc.SwitchSibling(ctx, thr, "n2", Next); err != nil {
		t.Fatal(err)
	}

	// Assert: n3's visit started again, so its pick is decided afresh.
	if got := f.path(); !equal(got, []string{"n1", "n3", "n5"}) {
		t.Fatalf("path = %v, want [n1 n3 n5]", got)
	}
}

func TestAGestureNamingAnUnknownNodeIsRefused(t *testing.T) {
	cases := []struct {
		name string
		act  func(f *fixture) error
	}{
		{name: "switch", act: func(f *fixture) error { return f.svc.SwitchSibling(context.Background(), thr, "nope", Next) }},
		{name: "select", act: func(f *fixture) error {
			return f.svc.Select(context.Background(), thr, &Highlight{Node: "nope", Quote: &zettelv1.NodeQuote{Text: "x"}})
		}},
		{name: "sightings", act: func(f *fixture) error { return f.svc.Sightings(context.Background(), thr, []NodeID{"n1", "nope"}) }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.zettel.seed("", 0)
			f.open()

			// Act
			err := tc.act(f)

			// Assert
			if !errors.Is(err, ErrUnknownNode) {
				t.Fatalf("err = %v, want ErrUnknownNode", err)
			}
			if len(f.zettel.sighted) != 0 {
				t.Fatalf("sightings were recorded: %v", f.zettel.sighted)
			}
		})
	}
}

func TestAskDecidesWhereTheQuestionIsAskedFrom(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
		node    string
		quote   string
		start   bool
		wantErr error
	}{
		{name: "an empty thread asks from the start", start: true},
		{name: "the end of the shown path", arrange: func(f *fixture) {
			f.zettel.seed("", 0)
			f.zettel.seed("n1", 0)
		}, node: "n2"},
		{name: "the held highlight", arrange: func(f *fixture) {
			f.zettel.seed("", 0)
			f.zettel.seed("n1", 0)
			f.open()
			_ = f.svc.Select(context.Background(), thr, &Highlight{Node: "n1", Quote: &zettelv1.NodeQuote{Text: "words"}})
		}, node: "n1", quote: "words"},
		{name: "a first question in flight refuses", arrange: func(f *fixture) {
			f.turns.ask("turn-1", "first?", "", "")
		}, wantErr: ErrAwaitingFirstAnswer},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			if tc.arrange != nil {
				tc.arrange(f)
			}
			f.open()

			// Act
			parent, err := f.svc.Ask(context.Background(), thr)

			// Assert
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("err = %v, want %v", err, tc.wantErr)
			}
			if tc.wantErr != nil {
				return
			}
			if got := parent.GetThreadStart() != nil; got != tc.start {
				t.Fatalf("thread start = %v, want %v", got, tc.start)
			}
			if got := parent.GetNode().GetNode().GetId(); got != tc.node {
				t.Fatalf("asked of %q, want %q", got, tc.node)
			}
			if got := parent.GetNode().GetQuote().GetText(); got != tc.quote {
				t.Fatalf("quote = %q, want %q", got, tc.quote)
			}
		})
	}
}

func TestTheSubmittedQuestionConsumesTheHighlightItQuoted(t *testing.T) {
	cases := []struct {
		name     string
		reselect string
		consumed bool
	}{
		{name: "the highlight asked with is consumed", consumed: true},
		{name: "a highlight made since stands", reselect: "other words"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.zettel.seed("", 0)
			f.open()
			ctx := context.Background()
			_ = f.svc.Select(ctx, thr, &Highlight{Node: "n1", Quote: &zettelv1.NodeQuote{Text: "words"}})
			asked, err := f.svc.Ask(ctx, thr)
			if err != nil {
				t.Fatal(err)
			}
			if tc.reselect != "" {
				_ = f.svc.Select(ctx, thr, &Highlight{Node: "n1", Quote: &zettelv1.NodeQuote{Text: tc.reselect}})
			}

			// Act
			if err := f.svc.Asked(ctx, thr, asked); err != nil {
				t.Fatal(err)
			}

			// Assert
			h := f.exchange("n1").GetAnswer().GetHighlight()
			if consumed := h == nil; consumed != tc.consumed {
				t.Fatalf("highlight = %v, want consumed %v", h, tc.consumed)
			}
		})
	}
}

func TestAQuestionInFlightGoesFromWaitingToANode(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.zettel.seed("n1", 0)
	f.open()
	f.turns.ask("turn-9", "and then?", "n2", "")
	if err := f.svc.Asked(context.Background(), thr, nil); err != nil {
		t.Fatal(err)
	}
	pending := func() *frontendv1.ChatPending { return f.view().GetConversation().GetPending() }

	// Act and assert, edge by edge.
	if pending().GetAnswer().GetWaiting() == nil || pending().GetQuestion().GetText().GetText() != "and then?" {
		t.Fatalf("pending = %v, want the question waiting", pending())
	}
	f.svc.AnswerProse(thr, "turn-9", "Then")
	if got := pending().GetAnswer().GetStreaming().GetMarkdown().GetText(); got != "Then" {
		t.Fatalf("streaming = %q, want %q", got, "Then")
	}
	f.svc.TurnAnswered(thr, "turn-9", "Then this.")
	f.turns.close()
	f.svc.TurnEnded(thr, "turn-9")
	if p := pending(); p != nil {
		t.Fatalf("pending = %v, want none once recorded", p)
	}
	if got := f.path(); !equal(got, []string{"n1", "n2", "n3"}) {
		t.Fatalf("path = %v, want the new answer shown below n2", got)
	}
	if got := f.exchange("n3").GetAnswer().GetMarkdown().GetText(); got != "Then this." {
		t.Fatalf("answer = %q", got)
	}
}

func TestAQuestionInFlightEndsThePathAtItsParent(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.zettel.seed("n1", 0)
	f.open()
	f.turns.ask("turn-9", "back up?", "n1", "the quoted words")

	// Act
	if err := f.svc.Asked(context.Background(), thr, nil); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := f.path(); !equal(got, []string{"n1"}) {
		t.Fatalf("path = %v, want [n1]", got)
	}
	if got := f.view().GetConversation().GetPending().GetQuestion().GetQuote().GetText(); got != "the quoted words" {
		t.Fatalf("pending quote = %q", got)
	}
}

func TestATurnEndedUnansweredIsNoLongerInFlight(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.open()
	f.turns.ask("turn-9", "why?", "n1", "")
	_ = f.svc.Asked(context.Background(), thr, nil)

	// Act: the feed sees the end before the records do.
	f.svc.TurnEnded(thr, "turn-9")

	// Assert
	if p := f.view().GetConversation().GetPending(); p != nil {
		t.Fatalf("pending = %v, want none", p)
	}
}

func TestAHeldQuestionIsInFlightWaiting(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.turns.held = []wsm.HeldPrompt{{
		Turn: "turn-h", Ask: &wsm.ThreadAsk{Parent: parentNode("n1", "")},
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "held?"}}},
		}}},
	}}

	// Act
	f.open()

	// Assert
	p := f.view().GetConversation().GetPending()
	if p.GetQuestion().GetText().GetText() != "held?" || p.GetAnswer().GetWaiting() == nil {
		t.Fatalf("pending = %v, want the held question waiting", p)
	}
}

func TestAnAnsweredTurnThatAskedNoQuestionRecordsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.turns.turns["turn-x"] = wsm.Turn{ID: "turn-x"}

	// Act
	f.svc.TurnAnswered("agent-ws", "turn-x", "an agent's answer")

	// Assert
	if len(f.zettel.nodes) != 0 {
		t.Fatalf("nodes = %v, want none", f.zettel.nodes)
	}
	if f.records("error", "daemon.chat.record_failed") != 0 {
		t.Fatal("a turn with no question logged a failure")
	}
}

func TestAFailedRecordIsLoggedAndNoLongerDrawn(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
	}{
		{name: "zettel refuses", arrange: func(f *fixture) { f.zettel.recordErr = errors.New("zettel down") }},
		{name: "the turn record is unreadable", arrange: func(f *fixture) { f.turns.turnErr = errors.New("db gone") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.zettel.seed("", 0)
			f.open()
			f.turns.ask("turn-9", "why?", "n1", "")
			_ = f.svc.Asked(context.Background(), thr, nil)
			tc.arrange(f)

			// Act
			f.svc.TurnAnswered(thr, "turn-9", "Because.")
			f.svc.TurnEnded(thr, "turn-9")

			// Assert
			if f.records("error", "daemon.chat.record_failed") != 1 {
				t.Fatalf("records = %+v, want one record_failed error", f.log.Records())
			}
			if p := f.view().GetConversation().GetPending(); p != nil {
				t.Fatalf("pending = %v, want none", p)
			}
			if len(f.path()) != 1 {
				t.Fatalf("path = %v, want only the root", f.path())
			}
		})
	}
}

func TestAnUnreadableGraphIsLoggedAndRefused(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.loadErr = errors.New("zettel down")

	// Act
	err := f.svc.Open(context.Background(), thr)

	// Assert
	if err == nil {
		t.Fatal("Open answered no error")
	}
	if f.records("error", "daemon.chat.load_failed") != 1 {
		t.Fatalf("records = %+v, want one load_failed error", f.log.Records())
	}
	if _, ok := f.svc.Topic(thr).Latest(); ok {
		t.Fatal("a view was published for an unread graph")
	}
}

func TestUnreadablePromptRecordsAreLoggedAndRefused(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.turns.openErr = errors.New("db gone")

	// Act
	err := f.svc.Open(context.Background(), thr)

	// Assert
	if err == nil || f.records("error", "daemon.chat.refresh_failed") != 1 {
		t.Fatalf("err = %v, records = %+v, want a refresh_failed error", err, f.log.Records())
	}
}

func TestSightingsAreRecordedAllOrNothing(t *testing.T) {
	cases := []struct {
		name    string
		failing bool
	}{
		{name: "recorded"},
		{name: "zettel refuses", failing: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.zettel.seed("", 0)
			f.open()
			if tc.failing {
				f.zettel.sightErr = errors.New("zettel down")
			}

			// Act
			err := f.svc.Sightings(context.Background(), thr, []NodeID{"n1"})

			// Assert
			if tc.failing {
				if err == nil || f.records("error", "daemon.chat.sightings_failed") != 1 {
					t.Fatalf("err = %v, want a logged failure", err)
				}
				return
			}
			if err != nil || !equal(f.zettel.sighted[0], []string{"n1"}) {
				t.Fatalf("err = %v, sighted = %v", err, f.zettel.sighted)
			}
		})
	}
}

func TestAnAnswerMarksTheWordsItsChildrenQuotedAndTheHighlight(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.zettel.nodes = append(f.zettel.nodes, &zettelv1.Node{
		Id: &zettelv1.NodeId{Id: "n2"}, Parent: parentNode("n1", "quoted words"), Question: &zettelv1.NodeQuestion{Text: "q"},
		Answer: &zettelv1.NodeRevision{Number: &zettelv1.RevisionNumber{Number: 1}, Markdown: "a"}, Turn: &zettelv1.ExchangeTurn{TurnId: "t2"},
	})
	f.open()

	// Act
	_ = f.svc.Select(context.Background(), thr, &Highlight{Node: "n1", Quote: &zettelv1.NodeQuote{Text: "held words"}})

	// Assert
	answer := f.exchange("n1").GetAnswer()
	if len(answer.GetQuoteMarks()) != 1 || answer.GetQuoteMarks()[0].GetText() != "quoted words" {
		t.Fatalf("quote marks = %v", answer.GetQuoteMarks())
	}
	if answer.GetHighlight().GetText() != "held words" {
		t.Fatalf("highlight = %v", answer.GetHighlight())
	}
	if got := f.exchange("n2").GetQuestion().GetQuote().GetText(); got != "quoted words" {
		t.Fatalf("question quote = %q", got)
	}
}

func TestDeleteDestroysTheGraphAndForgetsTheThread(t *testing.T) {
	// Arrange: an opened thread whose view is held.
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.open()
	held := f.svc.Topic(thr)

	// Act
	err := f.svc.Delete(context.Background(), thr)

	// Assert
	if err != nil || len(f.zettel.deleted) != 1 || f.zettel.deleted[0] != thr {
		t.Fatalf("Delete = %v, deleted = %v; want the thread's graph deleted", err, f.zettel.deleted)
	}
	if f.svc.Topic(thr) == held {
		t.Fatal("the thread's state outlived its delete")
	}
	if f.records("info", "daemon.chat.deleted") != 1 {
		t.Fatalf("records = %+v, want the delete at info", f.log.Records())
	}
}

func TestAFailedDeleteForgetsNothingAndIsRecorded(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.open()
	held := f.svc.Topic(thr)
	f.zettel.deleteErr = errors.New("zettelclient: DeleteThread: unavailable")

	// Act
	err := f.svc.Delete(context.Background(), thr)

	// Assert
	if err == nil {
		t.Fatal("Delete = nil, want zettel's failure")
	}
	if f.svc.Topic(thr) != held {
		t.Fatal("a failed delete forgot the thread")
	}
	if f.records("error", "daemon.chat.delete_failed") != 1 {
		t.Fatalf("records = %+v, want the failure at error", f.log.Records())
	}
}

func TestSearchAnswersWhatZettelAnswers(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.matches = []*zettelv1.SearchThreadsMatch{{Thread: &zettelv1.ThreadId{Id: "t1"}}}
	f.zettel.searchErr = errors.New("down")

	// Act
	got, err := f.svc.Search(context.Background(), "sky")

	// Assert
	if len(got) != 1 || err == nil || err.Error() != "down" {
		t.Fatalf("Search = %v, %v; want zettel's answer passed through", got, err)
	}
}
