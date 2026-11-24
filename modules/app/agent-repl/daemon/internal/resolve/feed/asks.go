package feed

import (
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// AsksStanding answers every ask whose card stands open on any of the
// workspace's feeds, oldest first, each as the start its card was drawn from
// under the agent that asked (sessionwatcher.FeedSink.AsksStanding). The
// session watcher reads it to settle, as abandoned, every one of them the
// session's facts no longer name open.
//
// THE START IS RESTATED FROM WHAT WAS SERVED: a question carries the batch its
// card drew, so an abandoned settle built from it restates that batch; a
// consent ask carries its identity alone, which is also the gated call's.
func (r *resolver) AsksStanding(ws ids.WorkspaceID) []*conversationv1.SessionOpenAsk {
	r.prime(ws)
	r.mu.Lock()
	defer r.mu.Unlock()
	open := openAsks(r.state(ws))
	asks := make([]*conversationv1.SessionOpenAsk, 0, len(open))
	for _, a := range open {
		switch a.kind {
		case AskPermission:
			asks = append(asks, &conversationv1.SessionOpenAsk{
				Agent: a.agent,
				Ask: &conversationv1.SessionOpenAsk_Permission{Permission: &conversationv1.AgentPermission{
					Id:     &conversationv1.AgentPermissionId{Value: a.id},
					Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{}},
				}},
			})
		case AskQuestion:
			asks = append(asks, &conversationv1.SessionOpenAsk{
				Agent: a.agent,
				Ask: &conversationv1.SessionOpenAsk_Question{Question: &conversationv1.AgentQuestion{
					Id:     &conversationv1.AgentQuestionId{Value: a.id},
					Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{Batch: a.batch}},
				}},
			})
		}
	}
	return asks
}

// openAsk is one ask whose card stands open on the workspace's feeds.
type openAsk struct {
	kind AskKind
	id   string
	// opened is the workspace's ask sequence at the card's first draw.
	opened uint64
	// agent is who asked.
	agent *conversationv1.AgentId
	// at is where the card is drawn.
	at placement
	// batch is a question's batch as served; nil for a consent ask.
	batch *conversationv1.AgentQuestionBatch
	// answerable is whether an answer can go to it now: a question batch
	// claimed by a delivery in flight, or not yet drawn, is not.
	answerable bool
}

// openAsks is every ask whose card stands open, oldest first: THE ONE RULE for
// which asks are open, read by the answer routing (oldestStandingAsk) and the
// session watcher's reconciliation (AsksStanding) alike.
func openAsks(s *wsState) []openAsk {
	var open []openAsk
	for id, p := range s.permissionRows {
		if p.card.GetOpen() == nil {
			continue
		}
		open = append(open, openAsk{kind: AskPermission, id: id, opened: p.opened, agent: p.agent, at: p.feed, answerable: true})
	}
	for id, q := range s.questionAsks {
		if !q.open {
			continue
		}
		open = append(open, openAsk{kind: AskQuestion, id: id, opened: q.opened, agent: q.agent, at: q.at, batch: q.batch,
			answerable: !q.delivering && q.row != nil})
	}
	sort.Slice(open, func(i, j int) bool { return open[i].opened < open[j].opened })
	return open
}
