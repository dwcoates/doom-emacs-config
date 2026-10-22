// Package latestmodel files the model each agent's LATEST API response named
// (conversation.v1.AgentActivity.model): what a subagent's footer row and feed
// card draw beside its label. ONE LEDGER, daemon-wide, because the footer and
// the feed reading "the subagent's model" by two rules would eventually draw
// two different models for one run the owner reads in both places.
//
// The model rides the unit carrying each API response's usage, so it is filed
// by the agent that produced it, never by the unit it rode on. A response that
// names no model files nothing: nothing is guessed before a response names
// one, since a spawn's requested model is what was ASKED for and is unset
// whenever the agent's own definition chose.
package latestmodel

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// Ledger is each agent's latest model, by agent id.
type Ledger map[string]string

// Record files the model one of AGENT's API responses named, reporting
// whether the agent's latest model moved.
func (l Ledger) Record(agent *conversationv1.AgentId, model *conversationv1.AgentModel) bool {
	name := model.GetName()
	id := agent.GetValue()
	if name == "" || id == "" || l[id] == name {
		return false
	}
	l[id] = name
	return true
}

// Of is AGENT's latest model, "" before any of its responses has named one.
func (l Ledger) Of(agent string) string {
	return l[agent]
}
