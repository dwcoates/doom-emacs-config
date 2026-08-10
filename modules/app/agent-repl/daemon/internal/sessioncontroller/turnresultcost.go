package sessioncontroller

import (
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

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

// THERE IS DELIBERATELY NO CONTEXT-SIZE ACCESSOR HERE. A result's usage is the
// SUM over every model call the turn made, so adding the cache read back in
// would produce the standing prefix counted once per round trip rather than the
// conversation's size. The size is measured from a single response instead
// (contextsize.go); this shape answers only what the turn PAID.

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
// THE CONVERSATION'S SIZE IS NOT ONE OF THEM. It was, and that was the defect: a
// result's usage sums every model call the turn made, so it measured the turn's
// work rather than the context's occupancy. The size is taken from one live
// main-agent response instead (contextsize.go), which is the same question asked
// of evidence that can answer it.
func (m *Manager) noteTurnResultCost(d *sessionController, cost turnResultCost) {
	if d == nil {
		return
	}
	m.noteKeepAlivePingCost(d, cost)
	m.noteDaemonCompactionCost(d, cost)
}

// breakdown renders every bucket and both derived figures for a log line or a
// failure card's source detail, through the canonical shape's one renderer.
//
// IT NAMES EVERY BUCKET, including the ones a verdict did not use. A reader
// asked to believe that a compaction read the conversation cold needs to see
// that the cache read was near zero while the written miss was enormous; a bare
// "uncached_input_tokens=1500000" is the conclusion without the evidence.
func (c turnResultCost) breakdown() string { return tokenusage.Breakdown(c.usage) }
