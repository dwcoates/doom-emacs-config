package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// HOW FULL THE CONTEXT WAS BEFORE A PROMPT (agentrepl.v1 RollbackPromptEntry
// .context_tokens).
//
// Every usage-carrying unit of the MAIN agent files a reading: every input
// token of that API response (cache reads, cache writes and uncached input,
// which together are the context the request carried), keyed at the place its
// entry sits in the conversation. A prompt's figure is the newest reading
// placed before the prompt's row and after the conversation's newest context
// cut. Readings are compared by PLACE, never by arrival, because history is
// loaded newest page first: a figure snapshotted on arrival would take a later
// page's reading for an earlier prompt.

// contextReading is one API response's context size, where it sits.
type contextReading struct {
	// key is the response entry's order base (entryBase): it compares with
	// row order keys, so "before a prompt" is a string comparison.
	key string
	// turn is the turn the response belongs to, so a rolled-back turn's
	// readings are never taken for the conversation that replaced it.
	turn ids.TurnID
	// tokens is the response's whole input.
	tokens uint64
}

// contextTokensOf is every input token USAGE carried.
func contextTokensOf(usage *conversationv1.TokenUsage) uint64 {
	return usage.GetInputHits().GetRead() + usage.GetInputMisses().GetWritten() + usage.GetInputMisses().GetUnwritten()
}

// recordContextReading files UNIT's reading when AGENT is the main agent. A
// unit's later frames restate its usage, so the reading is replaced, never
// added to. An entry with no usable place cannot be ordered against a prompt,
// so it files nothing; its row's own placement already records the fault.
func (r *resolver) recordContextReading(s *wsState, unit string, agent *conversationv1.AgentId, usage *conversationv1.TokenUsage) {
	if !s.isMainAgent(agent) {
		return
	}
	place := s.entryPlace
	if place == nil || place.GetAtMs() <= 0 {
		r.logger(s.id).Debug("daemon.feed.context_reading_unplaced",
			"a main-agent response stated usage with no conversation place; no context reading was filed for it",
			dlog.Context{"unit": unit})
		return
	}
	var turn ids.TurnID
	if t := s.rowTurn(); t != nil {
		turn = *t
	}
	s.contextReadings[unit] = contextReading{
		key:    entryBase(orderClass(s.plane), place),
		turn:   turn,
		tokens: contextTokensOf(usage),
	}
}

// contextBefore answers the newest reading placed after AFTER and before
// BEFORE (order keys; AFTER empty means no lower bound), skipping readings of
// rolled-back turns. False when there is none.
func contextBefore(s *wsState, after, before string) (uint64, bool) {
	var best contextReading
	found := false
	for _, reading := range s.contextReadings {
		if reading.key >= before || (after != "" && reading.key <= after) || s.rolledBack[reading.turn] {
			continue
		}
		if !found || reading.key > best.key {
			best, found = reading, true
		}
	}
	return best.tokens, found
}
