package chat

import (
	"slices"

	frontendv1 "agentrepl/proto/frontend/v1"
	zettelv1 "agentrepl/proto/zettel/v1"

	"agent-repld/internal/dlog"
)

// THE VIEW: composed from the thread's state alone, under Service.mu, and
// published whole on every change.

// pendingQuestion is the question in flight.
type pendingQuestion struct {
	text   string
	parent *zettelv1.NodeParent
	answer *frontendv1.ChatPendingAnswer
}

// publish composes the thread's view and publishes it. Called under s.mu.
func (s *Service) publish(t *thread) {
	if !t.loaded {
		return
	}
	pending, inFlight := t.pending()
	var stop NodeID
	if inFlight {
		stop = NodeID(pending.parent.GetNode().GetNode().GetId())
		if stop != "" && t.nodes[stop] == nil {
			s.deps.Log(t.id).Error("daemon.chat.pending_parent_unknown",
				"the question in flight was asked of a node the thread does not hold; it is drawn below the shown path",
				dlog.Context{"thread": string(t.id), "parent": string(stop)})
			stop = ""
		}
	}
	t.path = t.walk(stop)
	if len(t.path) == 0 && !inFlight {
		t.topic.Publish(&frontendv1.ChatView{State: &frontendv1.ChatView_Empty{Empty: &frontendv1.ChatViewEmpty{
			Hint: &frontendv1.ChatViewEmptyHint{Text: EmptyHint},
		}}})
		return
	}
	conversation := &frontendv1.ChatViewConversation{}
	for _, id := range t.path {
		conversation.Exchanges = append(conversation.Exchanges, t.exchange(id))
	}
	if inFlight {
		conversation.Pending = &frontendv1.ChatPending{Question: question(pending.text, pending.parent), Answer: pending.answer}
	}
	t.topic.Publish(&frontendv1.ChatView{State: &frontendv1.ChatView_Conversation{Conversation: conversation}})
}

// pending answers the thread's question in flight: the oldest turn in flight
// not yet a node and not ended unanswered, else the oldest held question.
func (t *thread) pending() (pendingQuestion, bool) {
	for _, turn := range t.inFlight() {
		if t.recordedTurn(turn.ID) {
			continue
		}
		recording, answered := t.answered[turn.ID]
		if t.ended[turn.ID] && !answered {
			continue
		}
		answer := recording.markdown
		if !answered {
			answer = t.prose[turn.ID]
		}
		return pendingQuestion{text: turn.Question.Text, parent: turn.Question.Parent, answer: pendingAnswer(answer)}, true
	}
	if len(t.held) > 0 {
		h := t.held[0]
		return pendingQuestion{text: saidText(h), parent: h.Ask.Parent, answer: pendingAnswer("")}, true
	}
	return pendingQuestion{}, false
}

// pendingAnswer is an answer in flight: waiting until it has text.
func pendingAnswer(markdown string) *frontendv1.ChatPendingAnswer {
	if markdown == "" {
		return &frontendv1.ChatPendingAnswer{Progress: &frontendv1.ChatPendingAnswer_Waiting{Waiting: &frontendv1.ChatPendingAnswerWaiting{}}}
	}
	return &frontendv1.ChatPendingAnswer{Progress: &frontendv1.ChatPendingAnswer_Streaming{Streaming: &frontendv1.ChatPendingAnswerStreaming{
		Markdown: &frontendv1.ChatAnswerMarkdown{Text: markdown},
	}}}
}

// walk composes the shown path from the root down. Below each node the child
// picked for this visit is shown; a node with no pick yet gets its least-seen
// child (the newest among ties). When stop is set the path is the way to stop
// and ends there, because the question in flight is drawn below it. Picks of
// nodes that left the path are forgotten: their visit is over.
func (t *thread) walk(stop NodeID) []NodeID {
	roots := t.children("")
	if len(roots) == 0 {
		return nil
	}
	way := map[NodeID]bool{}
	for at := stop; at != ""; at = parentOf(t.nodes[at]) {
		way[at] = true
	}
	path := []NodeID{roots[0]}
	for cur := roots[0]; cur != stop; {
		kids := t.children(cur)
		if len(kids) == 0 {
			break
		}
		next := NodeID("")
		switch {
		case stop != "":
			for _, k := range kids {
				if way[k] {
					next = k
				}
			}
			t.picks[cur] = next
		case t.picks[cur] != "" && slices.Contains(kids, t.picks[cur]):
			next = t.picks[cur]
		default:
			next = t.leastSeen(kids)
			t.picks[cur] = next
		}
		path = append(path, next)
		cur = next
	}
	onPath := map[NodeID]bool{}
	for _, id := range path[:len(path)-1] {
		onPath[id] = true
	}
	for parent := range t.picks {
		if !onPath[parent] {
			delete(t.picks, parent)
		}
	}
	return path
}

// leastSeen is the child seen fewest times, the newest among ties.
func (t *thread) leastSeen(kids []NodeID) NodeID {
	best := kids[0]
	for _, k := range kids[1:] {
		if t.nodes[k].GetSightings().GetCount() <= t.nodes[best].GetSightings().GetCount() {
			best = k
		}
	}
	return best
}

// exchange draws one node on the path.
func (t *thread) exchange(id NodeID) *frontendv1.ChatExchange {
	node := t.nodes[id]
	ex := &frontendv1.ChatExchange{
		Node:     node.GetId(),
		Question: question(node.GetQuestion().GetText(), node.GetParent()),
		Answer:   &frontendv1.ChatAnswer{Markdown: &frontendv1.ChatAnswerMarkdown{Text: node.GetAnswer().GetMarkdown()}},
	}
	if parent := parentOf(node); parent != "" {
		if siblings := t.children(parent); len(siblings) > 1 {
			ex.Siblings = &frontendv1.ChatSiblings{Position: &frontendv1.ChatSiblingsPosition{Label: siblingLabel(indexOf(siblings, id), len(siblings))}}
		}
	}
	marked := map[string]bool{}
	for _, child := range t.children(id) {
		quote := t.nodes[child].GetParent().GetNode().GetQuote().GetText()
		if quote != "" && !marked[quote] {
			marked[quote] = true
			ex.Answer.QuoteMarks = append(ex.Answer.QuoteMarks, &frontendv1.ChatAnswerQuoteMark{Text: quote})
		}
	}
	if h := t.highlight; h != nil && h.Node == id {
		ex.Answer.Highlight = &frontendv1.ChatAnswerHighlight{Text: h.Quote.GetText()}
	}
	return ex
}

// question draws a question bubble.
func question(text string, parent *zettelv1.NodeParent) *frontendv1.ChatQuestion {
	q := &frontendv1.ChatQuestion{Text: &frontendv1.ChatQuestionText{Text: text}}
	if quote := parent.GetNode().GetQuote().GetText(); quote != "" {
		q.Quote = &frontendv1.ChatQuestionQuote{Text: quote}
	}
	return q
}
