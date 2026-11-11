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
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	type standing struct {
		opened uint64
		ask    *conversationv1.SessionOpenAsk
	}
	var found []standing
	for id, p := range s.permissionRows {
		if p.card.GetOpen() == nil {
			continue
		}
		found = append(found, standing{opened: p.opened, ask: &conversationv1.SessionOpenAsk{
			Agent: p.agent,
			Ask: &conversationv1.SessionOpenAsk_Permission{Permission: &conversationv1.AgentPermission{
				Id:     &conversationv1.AgentPermissionId{Value: id},
				Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{}},
			}},
		}})
	}
	for id, q := range s.questionAsks {
		if !q.open {
			continue
		}
		found = append(found, standing{opened: q.opened, ask: &conversationv1.SessionOpenAsk{
			Agent: q.agent,
			Ask: &conversationv1.SessionOpenAsk_Question{Question: &conversationv1.AgentQuestion{
				Id:     &conversationv1.AgentQuestionId{Value: id},
				Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{Batch: q.batch}},
			}},
		}})
	}
	sort.Slice(found, func(i, j int) bool { return found[i].opened < found[j].opened })
	asks := make([]*conversationv1.SessionOpenAsk, 0, len(found))
	for _, f := range found {
		asks = append(asks, f.ask)
	}
	return asks
}
