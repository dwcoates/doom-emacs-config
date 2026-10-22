package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// subagentModel is the model element a subagent's card draws: the model its
// latest response named (latestmodel.Ledger), nil before any has named one.
func (s *wsState) subagentModel(created *conversationv1.AgentId) *frontendv1.FeedSubagentModel {
	name := s.agentModel.Of(created.GetValue())
	if name == "" {
		return nil
	}
	return &frontendv1.FeedSubagentModel{Name: name}
}
