package sessioncontroller

import (
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/tokenusage"
)

// turnresultcost.go — ONE REDUCTION of one turn's terminal result usage, read
// by three different decisions.
//
// The keep-alive's cold-ping hibernation, the daemon compaction's cold-read
// alarm, and the warm-compaction size floor all want a figure from the same
// result. Reducing the usage once and handing all three the same value is what
// makes it impossible for them to disagree about whether a single turn was
// cold — the same reason the canonical shape's accessors live in one package
// (internal/tokenusage) rather than being restated at each decision.
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
	// usage is the turn's canonical economics, and it is the WHOLE measurement:
	// the expensive sum, the context size, and the raw buckets a report shows to
	// justify a verdict are all accessors on it (internal/tokenusage) rather
	// than fields spread out here. Carrying the message instead of a handful of
	// pre-reduced numbers is what stops a consumer from reaching for one bucket
	// where the sum was meant.
	usage *frontendv1.TokenUsage
}

// expensiveInputTokens is what the turn fed the model NEW: both cache misses,
// the buckets processed fresh at full price.
func (c turnResultCost) expensiveInputTokens() int64 { return tokenusage.ExpensiveInput(c.usage) }

// contextInputTokens is the TOTAL input the request presented — both misses
// plus the cache hit. It is the size of the standing conversation as the model
// saw it, which is what the warm-compaction floor judges "is this conversation
// even big enough to be worth compacting" against. Cache reads are INCLUDED
// here and excluded above, and the difference is deliberate: a cached token is
// cheap NOW but is still a token a cold revival would re-ingest at full price
// later.
func (c turnResultCost) contextInputTokens() int64 { return tokenusage.ContextInput(c.usage) }

// newTurnResultCost converts one result's vendor usage at the boundary.
//
// A nil usage is not reachable from the one caller (which checks), and a zero
// usage reduces to zeroes rather than an error: "the result reported nothing"
// is a state every consumer here already distinguishes from "no result
// arrived", and it does so by whether it was called at all. A usage the vendor
// reported with a NEGATIVE counter is a different matter and is surfaced: the
// canonical shape is unsigned, so converting one would report a turn costing
// nearly 2^64 tokens to the hibernation policy and the tripwire alike.
func newTurnResultCost(turnID string, usage *datav1.Usage) (turnResultCost, error) {
	canonical, err := tokenusage.FromResultUsage(usage)
	if err != nil {
		return turnResultCost{}, err
	}
	return turnResultCost{turnID: turnID, usage: canonical}, nil
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
	context := cost.contextInputTokens()
	if context <= 0 {
		return
	}
	m.mu.Lock()
	before := d.lastContextInputTokens
	d.lastContextInputTokens = context
	m.mu.Unlock()
	if before == context {
		return
	}
	m.logf("session-controller: conversation size OBSERVED ws=%q session=%s turn_id=%s before=%d after=%d warm_compaction_floor=%d %s",
		d.workspace, d.sessionID, cost.turnID, before, context,
		keepalive.WarmCompactMinContextTokens, cost.breakdown())
}

// breakdown renders every bucket and both derived figures for a log line or a
// failure card's source detail, through the canonical shape's one renderer.
//
// IT NAMES EVERY BUCKET, including the ones a verdict did not use. A reader
// asked to believe that a compaction read the conversation cold needs to see
// that the cache read was near zero while the written miss was enormous; a bare
// "uncached_input_tokens=1500000" is the conclusion without the evidence.
func (c turnResultCost) breakdown() string { return tokenusage.Breakdown(c.usage) }
