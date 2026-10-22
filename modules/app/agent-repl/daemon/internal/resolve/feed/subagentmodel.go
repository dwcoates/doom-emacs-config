package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// A SUBAGENT'S MODEL IS WHAT ITS OWN RESPONSES NAME.
//
// conversation.v1.AgentActivity.model rides the unit carrying each API
// response's usage, so it is read here exactly as usage is read in usage.go:
// filed by the agent that produced it, never by the bubble of the unit it rode
// on. The card draws the LATEST model the agent named; a run whose model
// changed mid-way draws the one now answering. Nothing is guessed before the
// first response names one: the spawn's requested_model is what was ASKED for
// and is unset whenever the agent's definition chose.

// recordModel files the model one of AGENT's API responses named, reporting
// whether the agent's latest model moved. A response naming none files nothing.
func (s *wsState) recordModel(agent *conversationv1.AgentId, model *conversationv1.AgentModel) bool {
	name := model.GetName()
	id := agent.GetValue()
	if name == "" || id == "" || s.agentModel[id] == name {
		return false
	}
	s.agentModel[id] = name
	return true
}

// subagentModel is the model element a subagent's card draws, nil before any
// of its responses has named one.
func (s *wsState) subagentModel(created *conversationv1.AgentId) *frontendv1.FeedSubagentModel {
	name, ok := s.agentModel[created.GetValue()]
	if !ok || created.GetValue() == "" {
		return nil
	}
	return &frontendv1.FeedSubagentModel{Name: name}
}
