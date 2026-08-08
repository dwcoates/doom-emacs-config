package sessioncontroller

import (
	"fmt"

	datav1 "agentrepl/proto/agentshim/data/v1"

	"claude-repld/internal/keepalive"
)

// turnresultcost.go — ONE REDUCTION of one turn's terminal result usage, read
// by three different decisions.
//
// The keep-alive's cold-ping hibernation, the daemon compaction's cold-read
// alarm, and the warm-compaction size floor all want a figure from the same
// result. Reducing the usage once and handing all three the same value is what
// makes it impossible for them to disagree about whether a single turn was
// cold — the same reason keepalive.UncachedInputTokens is shared with the
// progress footer rather than restated there.
//
// THE FIGURES COME FROM THE RESULT'S OWN USAGE AND NOWHERE ELSE. A compaction's
// synthetic assistant record carries a zero usage by construction, so a
// measurement taken from the assistant plane would report every compaction as
// free — including the 1.5-million-token one that motivated the alarm.

// turnResultCost is what ONE terminal result measured about the turn the
// accounting reducer attributed it to.
type turnResultCost struct {
	// turnID names the turn this belongs to. Every consumer matches on it, so a
	// late result for an earlier turn cannot fill a later one's measurement.
	turnID string
	// uncachedInputTokens is what the turn fed the model NEW: input_tokens plus
	// cache_creation_input_tokens, the two buckets processed fresh at full
	// price. keepalive.UncachedInputTokens states the disjoint-sum contract this
	// relies on.
	uncachedInputTokens int64
	// contextInputTokens is the TOTAL input the request presented — the two
	// uncached buckets plus the cache read. It is the size of the standing
	// conversation as the model saw it, which is what the warm-compaction floor
	// judges "is this conversation even big enough to be worth compacting"
	// against. Cache reads are INCLUDED here and excluded above, and the
	// difference is deliberate: a cached token is cheap NOW but is still a token
	// a cold revival would re-ingest at full price later.
	contextInputTokens int64
	// inputTokens, cacheCreationInputTokens and cacheReadInputTokens are the raw
	// buckets, carried so a report can show the arithmetic that produced a
	// verdict rather than only the verdict.
	inputTokens              int64
	cacheCreationInputTokens int64
	cacheReadInputTokens     int64
}

// newTurnResultCost reduces one result's usage.
//
// A nil usage is not reachable from the one caller (which checks), and a zero
// usage reduces to zeroes rather than an error: "the result reported nothing"
// is a state every consumer here already distinguishes from "no result
// arrived", and it does so by whether it was called at all.
func newTurnResultCost(turnID string, usage *datav1.Usage) turnResultCost {
	in, create, read := usage.GetInputTokens(), usage.GetCacheCreationInputTokens(), usage.GetCacheReadInputTokens()
	return turnResultCost{
		turnID:                   turnID,
		uncachedInputTokens:      keepalive.UncachedInputTokens(in, create),
		contextInputTokens:       in + create + read,
		inputTokens:              in,
		cacheCreationInputTokens: create,
		cacheReadInputTokens:     read,
	}
}

// noteTurnResultCost is the ONE fan-out of one reduced result to every decision
// that reads it. Called on the shim read-loop goroutine with no manager mutex
// held, which is the obligation the consumer hook carries.
//
// THE ORDER IS DELIBERATE: the size is remembered FIRST, because it is a fact
// about the session that the two verdicts below do not change, and a verdict
// that logged before the figure it was taken beside was recorded would leave a
// reader correlating two lines to reconstruct one instant.
func (m *Manager) noteTurnResultCost(d *sessionController, cost turnResultCost) {
	if d == nil {
		return
	}
	m.noteContextSize(d, cost)
	m.noteKeepAlivePingCost(d, cost)
	m.noteDaemonCompactionCost(d, cost)
}

// noteContextSize remembers how big the standing conversation was as of this
// result, which is the only figure the warm-compaction floor has to judge
// "worth compacting" by.
//
// A ZERO TOTAL IS NOT RECORDED. A result reporting no input at all is not
// evidence that the conversation is empty — it is a result that measured
// nothing — and writing it would turn a known-large session into an unknown
// one, which the eligibility check reads as "do not compact". Keeping the last
// real measurement is the honest answer for a turn that reported none.
func (m *Manager) noteContextSize(d *sessionController, cost turnResultCost) {
	if cost.contextInputTokens <= 0 {
		return
	}
	m.mu.Lock()
	before := d.lastContextInputTokens
	d.lastContextInputTokens = cost.contextInputTokens
	m.mu.Unlock()
	if before == cost.contextInputTokens {
		return
	}
	m.logf("session-controller: conversation size OBSERVED ws=%q session=%s turn_id=%s before=%d after=%d warm_compaction_floor=%d %s",
		d.workspace, d.sessionID, cost.turnID, before, cost.contextInputTokens,
		keepalive.WarmCompactMinContextTokens, cost.breakdown())
}

// breakdown renders the raw buckets and the two derived figures for a log line
// or a failure card's source detail.
//
// IT NAMES EVERY BUCKET, including the ones a verdict did not use. A reader
// asked to believe that a compaction read the conversation cold needs to see
// that the cache read was near zero while creation was enormous; a bare
// "uncached_input_tokens=1500000" is the conclusion without the evidence.
func (c turnResultCost) breakdown() string {
	return fmt.Sprintf("input_tokens=%d cache_creation_input_tokens=%d cache_read_input_tokens=%d uncached_input_tokens=%d context_input_tokens=%d",
		c.inputTokens, c.cacheCreationInputTokens, c.cacheReadInputTokens,
		c.uncachedInputTokens, c.contextInputTokens)
}
