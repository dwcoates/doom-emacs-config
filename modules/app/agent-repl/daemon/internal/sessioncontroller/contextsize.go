package sessioncontroller

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/tokenusage"
)

// contextsize.go — HOW BIG THE STANDING CONVERSATION IS.
//
// THE MEASURE IS ONE API CALL'S INPUT, NEVER A TURN'S. The two are different
// questions and only one of them is the context window's occupancy. A terminal
// result's usage is the SUM over every model call the turn made, so a turn that
// ran thirty tool round trips counts the standing prefix thirty times: the
// warm-compaction floor used to read that sum, and it was measuring how much
// work the turn did rather than how full the context was. Live sessions reported
// figures in the tens of millions of tokens against a two-hundred-thousand-token
// window, which is why the floor never once declined anything.
//
// One assistant response's Messages-API usage is the honest figure. Its input
// buckets are settled at message_start and name exactly the prompt that ONE
// request presented — the whole standing conversation as the model saw it, and
// nothing more. Summing its three input buckets is therefore the occupancy, and
// the last such response's figure is the occupancy right now.
//
// A SUBAGENT'S RESPONSE MEASURES A DIFFERENT CONVERSATION. A subagent runs its
// own context, which is neither the session's nor a part of it, so its usage is
// not a measurement of the conversation a compaction would rewrite. Only
// main-agent records are read here, and the actor is already classified on the
// durable record (frontend.SetTokenUtilizationActor).
//
// A HISTORICAL RECORD IS NOT A MEASUREMENT OF NOW. Replayed transcript rows
// arrive at the same consumer and describe how big the conversation was at some
// past instant. The caller admits only live evidence for that reason (sinks.go).

// mainAgentContextUsage reduces one token-utilization record to the CANONICAL
// shape a context measurement is read off, and reports whether the record is one
// the measurement may be taken from at all.
//
// IT RETURNS THE SHAPE RATHER THAN THE NUMBER, which is turnResultCost's own
// discipline: the size and the bucket breakdown a log line justifies it with are
// two accessors on one reduction (internal/tokenusage), so converting twice —
// once to decide and once to explain — cannot produce two different answers
// about one response.
//
// The `ok` return is FALSE for a record that measures something other than this
// conversation's standing context rather than zero-valued, because zero is
// itself a meaningful answer elsewhere ("the vendor reported nothing") and
// collapsing the two would let a subagent's response read as an empty session.
//
// An error is returned only for a counter the vendor reported negative, which
// the canonical shape refuses to convert. The caller surfaces it rather than
// recording a figure near 2^64 as the conversation's size.
func mainAgentContextUsage(record *frontendv1.TokenUtilization) (usage *frontendv1.TokenUsage, ok bool, err error) {
	if record == nil {
		return nil, false, nil
	}
	if record.GetMainAgent() == nil {
		return nil, false, nil
	}
	vendor := record.GetUsage()
	if vendor == nil {
		return nil, false, nil
	}
	canonical, err := tokenusage.FromVendorUsage(vendor)
	if err != nil {
		return nil, false, err
	}
	return canonical, true, nil
}

// noteMainAgentContextSize remembers how big the standing conversation is as of
// one live main-agent response, which is the only figure the warm-compaction
// floor has to judge "worth compacting" by.
//
// IT IS NOT MONOTONIC, deliberately. A compaction's whole purpose is to make the
// conversation smaller, and a floor judged against a high-water mark would keep
// reading a compacted session as large forever. The latest measurement is the
// current one.
//
// A ZERO IS NOT RECORDED. A response reporting no input at all measured nothing;
// writing it would turn a known-large session into an unknown one, which the
// eligibility check reads as "do not compact". Keeping the last real measurement
// is the honest answer for a response that reported none.
func (m *Manager) noteMainAgentContextSize(d *sessionController, record *frontendv1.TokenUtilization) {
	if d == nil {
		return
	}
	usage, ok, err := mainAgentContextUsage(record)
	if err != nil {
		m.errorf("session-controller: conversation size REJECTED ws=%q session=%s api_message_id=%s error=%v — the vendor reported a negative token counter, so the warm-compaction floor keeps the previous measurement rather than judging against a fabricated one",
			d.workspace, d.sessionID, record.GetApiMessageId(), err)
		return
	}
	if !ok {
		return
	}
	tokens := tokenusage.ContextInput(usage)
	if tokens <= 0 {
		return
	}
	m.mu.Lock()
	before := d.lastContextInputTokens
	d.lastContextInputTokens = tokens
	m.mu.Unlock()
	if before == tokens {
		return
	}
	m.logf("session-controller: conversation size OBSERVED ws=%q session=%s api_message_id=%s model=%s before=%d after=%d warm_compaction_floor=%d %s",
		d.workspace, d.sessionID, record.GetApiMessageId(), record.GetModel(), before, tokens,
		keepalive.WarmCompactMinContextTokens, tokenusage.Breakdown(usage))
}
