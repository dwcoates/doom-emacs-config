package sessioncontroller

import (
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/tokenusage"
)

// keepalivecold.go — THE PING THAT CAME BACK COLD.
//
// keepalive.Config.Evaluate is a PREDICTION and can be nothing else. Its one
// input is the durable last-turn-end instant, so the most it can ever say is
// "the cache SHOULD still be warm". A shim respawn, a resumed session, or a
// rewritten prompt prefix destroys the cache without moving that instant, and
// the policy then goes on pinging a cache that is not there — paying a full
// context re-ingest, on the user's budget, to refresh nothing.
//
// A ping that came back having paid for the whole conversation is that same
// question OBSERVED. It is the only direct evidence the feature ever produces
// about its own premise, and it says the premise was false. So it OVERRULES the
// prediction — but only about PINGING. The session STOPS BEING PINGED and stays
// up; it is not slept.
//
// IT USED TO HIBERNATE HERE, WITH CAUSE cache_expired, and that was the same
// conflation the policy ladder made in its own cold-cache arm. The evidence is
// about cost — a dozen tokens of prompt billed for the whole conversation — and
// cost is a reason to stop spending, not a reason to tear a session down. A
// hibernation taken here landed at roughly the ping window, about an HOUR, under
// a configured six-hour idle cutoff; the user's contract is that nothing sleeps
// before that cutoff, and this was one of the two routes breaking it. The idle
// cutoff is now the only threshold that sleeps anything.
//
// WHY THE TRIGGER LIVES HERE AND NOT IN THE PROGRESS RESOLVER. progress.Manager
// reduces the same result first, for the footer's expensive-turn alert, and it
// is where the "came back cold" line has always been logged. It is also a VIEW
// PRODUCER: it holds no hibernation claim, no shim, and no session lifecycle,
// and a resolver that could stop a session would be a second lifecycle
// authority beside this one, refreshed on its own triggers and answerable to
// nobody. The session controller already owns the keep-alive claim, the one
// hibernation transition, and the stop causes, so the FACT is routed here and
// the DECISION is taken where the transition already lives.
//
// WHY THE ORDERING IS STRUCTURAL RATHER THAN TIMED. The ping's result lands on
// the shim demux goroutine and does one thing: LATCH the measurement. The only
// reader of that latch is the ping's turn-end boundary, which by construction
// cannot run before the turn has ended — so the verdict is never taken while the
// turn it was measured from is still live.

// keepAlivePingMeasurement is what ONE in-flight keep-alive ping measured about
// the cache it was sent to refresh. Read and written only under Manager.mu.
type keepAlivePingMeasurement struct {
	// turnID names the ping this measurement belongs to. Every read matches on
	// it, so a late result for an earlier ping cannot fill a later ping's
	// measurement with a cost that was never its own.
	turnID string
	// elapsedMs is how long the session had been quiet when the ping was
	// SUBMITTED, measured from the durable last-turn-end.
	//
	// IT IS REMEMBERED, NEVER RE-DERIVED. The ping's own turn end stamps that
	// durable instant to now, so a figure taken after the turn would report ~0
	// for a session that had in fact been quiet for an hour — and this is the
	// figure the durable HibernationDetail carries, which is the same discipline
	// keepalive.Decision states for every other measured field.
	elapsedMs int64
	// ttlMs is the cache lifetime that elapsed was believed to be inside. It is
	// the threshold the account reports having been wrong about.
	ttlMs int64
	// submittedAtMs is when this ping was claimed, on the daemon's own clock.
	//
	// IT IS WHAT THE DEADLINE IS MEASURED FROM (keepalivedeadline.go). A ping's
	// turn end is the only thing that clears its claim, so a ping whose end never
	// arrives holds the claim, declines every later ping, parks real prompts
	// behind it and reads as a live turn to hibernation and to every restart
	// guard — for as long as the daemon lives. The instant is remembered here,
	// with the claim it belongs to, because that is the only record of when the
	// ping began that survives to be compared against.
	submittedAtMs int64
	// restartGraceAtSubmitMs is the restart epoch's accumulated grace at the
	// instant this ping was claimed (restartepoch.go).
	//
	// IT IS A MARK, NOT A FLAG. The deadline extends by the grace accrued SINCE
	// this value, so a ping submitted after a bounce is owed nothing and one
	// submitted before it is owed exactly the window it lived through. Storing
	// the total at submit is what makes that subtraction possible without the
	// ping having to know a bounce happened.
	restartGraceAtSubmitMs int64
	// usage is what the ping's terminal result actually paid, in the canonical
	// shape. The verdict reads the expensive sum off it rather than being handed
	// a pre-reduced number, so the one place a bucket could be substituted for
	// the sum does not exist.
	usage *frontendv1.TokenUsage
	// resultObserved separates "the result reported nothing" from "no result
	// arrived at all". A ping whose turn ended without a terminal result is not
	// evidence of a warm cache and must not be read as one.
	resultObserved bool
}

// measureKeepAlivePing takes the elapsed a ping is about to be submitted
// against, before it is submitted.
//
// CALLED WITH NO MANAGER MUTEX HELD. The durable read may reach the legacy
// stamper, which lives outside this package, and the ping's whole submit path
// is careful never to call out under the mutex every prompt submission takes.
//
// A session with no durable instant to measure from yields a zero elapsed
// rather than a guess. That is the same answer every other evaluator gives for
// an undated session, and the cold-ping verdict does not depend on it: the
// evidence is what the ping PAID, not how long it had been quiet.
func (m *Manager) measureKeepAlivePing(workspace, turnID string) keepAlivePingMeasurement {
	cfg := m.keepAliveConfig()
	measurement := keepAlivePingMeasurement{
		turnID: turnID,
		ttlMs:  int64(cfg.CacheTTL / time.Millisecond),
		// STAMPED BEFORE ANY OF THE READS BELOW CAN FAIL. The deadline must
		// cover a ping whose measurement could not be completed just as much as
		// one whose could — an unmeasurable session is not a licence to hold a
		// claim forever — so this is the one field taken unconditionally.
		submittedAtMs: m.now(),
		// Taken on the same unconditional terms and for the same reason: a ping
		// whose measurement could not be completed is owed the bounce's grace
		// exactly as much as one whose could.
		restartGraceAtSubmitMs: m.restartGraceMark(),
	}
	if m.cfg.Hibernations == nil {
		return measurement
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return measurement
	}
	lastEndMs, ok := m.durableLastTurnEnd(sessionID, workspace)
	if !ok || lastEndMs <= 0 {
		return measurement
	}
	measurement.elapsedMs = m.now() - lastEndMs
	return measurement
}

// noteKeepAlivePingCost latches what a turn's terminal result paid, when that
// turn is the ping now in flight.
//
// TWO INDEPENDENT FACTS MUST AGREE before anything is written: the accounting
// reducer attributed this result to turnID, and the manager's own claim says
// turnID is the ping running right now. Either one alone would be an
// attribution by elimination — "nothing else was running, so it must be the
// ping" — and a cost measurement that decides whether to stop a session is not
// a thing to infer from an absence.
//
// Every other turn's cost is simply not this function's business: an expensive
// USER turn is a cost report, not evidence that a cache died, and the footer
// already reports it (progress.applyResultCostLocked).
func (m *Manager) noteKeepAlivePingCost(d *sessionController, cost turnResultCost) {
	if d == nil || cost.turnID == "" {
		return
	}
	m.mu.Lock()
	measurement := d.keepAlivePing
	if measurement == nil || measurement.turnID != cost.turnID || d.keepAliveTurnID != cost.turnID {
		m.mu.Unlock()
		return
	}
	measurement.usage = cost.usage
	measurement.resultObserved = true
	elapsedMs, ttlMs := measurement.elapsedMs, measurement.ttlMs
	m.mu.Unlock()
	m.logf("session-controller: keep-alive ping COST OBSERVED ws=%q session=%s turn_id=%s threshold=%d elapsed_ms=%d ttl_ms=%d %s — the figure is latched and read at the ping's own turn end, which is the only place it can be acted on without racing the turn's teardown",
		d.workspace, d.sessionID, cost.turnID, m.keepAliveConfig().UncachedCostAlertTokens, elapsedMs, ttlMs, cost.breakdown())
}

// actOnColdKeepAlivePing takes the ping's verdict on the cache at the boundary
// that has just ended the ping's turn. Called with the manager mutex RELEASED,
// on the shim read-loop goroutine.
//
// promptsWaiting says the ping's end is about to release real prompts that were
// held behind it.
func (m *Manager) actOnColdKeepAlivePing(d *sessionController, measurement *keepAlivePingMeasurement, promptsWaiting bool) {
	if measurement == nil || !measurement.resultObserved {
		return
	}
	cfg := m.keepAliveConfig()
	if !cfg.CameBackCold(measurement.usage) {
		return
	}
	if promptsWaiting {
		// THE USER IS ALREADY BACK. Prompts held behind the ping are about to be
		// released and delivered, and their own turn is what re-warms the cache.
		// Hibernating here would stop the shim out from under work the user is
		// waiting on, and the revival gate would then refuse the very prompts the
		// rewind was on its way to deliver. The finding is still reported: a cold
		// ping is a fact about the policy whether or not it is acted on.
		m.logf("session-controller: cache keep-alive came back COLD but prompts are WAITING ws=%q session=%s turn_id=%s uncached_input_tokens=%d threshold=%d — the held prompts are released instead and their turn re-warms the cache, so no hibernation is taken and the user is not sent to the revival gate mid-work",
			d.workspace, d.sessionID, measurement.turnID, tokenusage.ExpensiveInput(measurement.usage), cfg.UncachedCostAlertTokens)
		return
	}
	m.latchColdKeepAlivePing(d, *measurement)
}

// coldCacheVerdict is what ONE cold keep-alive ping proved about the prompt
// cache behind a session, kept so no later ping pays the same price to learn it
// again. Read and written only under Manager.mu.
type coldCacheVerdict struct {
	// turnID names the ping that measured it.
	turnID string
	// elapsedMs is how long the session had been quiet when that ping was
	// submitted, taken at the submit and never re-derived: the ping's own turn
	// end stamps the durable last-turn-end to now.
	elapsedMs int64
	// ttlMs is the cache lifetime the measurement disproved.
	ttlMs int64
	// uncachedInputTokens is what the ping actually paid — a dozen tokens of
	// prompt billed for the whole conversation, which IS the evidence.
	uncachedInputTokens int64
	// atMs is when the verdict was taken, on the daemon's own clock.
	atMs int64
}

// latchColdKeepAlivePing records the verdict a cold ping has proved and STOPS
// PINGING this session — it does not sleep it.
//
// IT USED TO HIBERNATE, AND THAT WAS THE SAME CONFLATION THE POLICY LADDER MADE.
// A ping that came back cold is direct evidence that the cache this session was
// being kept warm for is gone, and the whole argument built on that evidence was
// about COST: refreshing a dead cache pays a full context re-ingest for nobody.
// That supports declining the ping. It does not support tearing the session
// down, and a session torn down here slept at roughly ONE HOUR — the ping window
// — against a configured six-hour idle cutoff. The user's contract is that
// nothing sleeps before the cutoff, and this was one of the two routes breaking
// it.
//
// SO THE FINDING NOW COSTS THE USER NOTHING FURTHER AND TAKES NOTHING AWAY. The
// latch declines every later ping (keepAliveEligibleLocked, `cache_proven_cold`)
// until real work rebuilds the prefix, and the session stays up until the idle
// cutoff reaps it exactly as an un-pinged cold-cached session does.
//
// A SECOND VERDICT DOES NOT OVERWRITE THE FIRST. The first is the one that
// stopped the pinging, and its measurement is the one an operator diagnoses
// from; a later one could only be a ping the latch failed to decline, which is
// worth saying out loud rather than quietly recording.
func (m *Manager) latchColdKeepAlivePing(d *sessionController, measurement keepAlivePingMeasurement) {
	cfg := m.keepAliveConfig()
	verdict := coldCacheVerdict{
		turnID:              measurement.turnID,
		elapsedMs:           measurement.elapsedMs,
		ttlMs:               measurement.ttlMs,
		uncachedInputTokens: tokenusage.ExpensiveInput(measurement.usage),
		atMs:                m.now(),
	}
	m.mu.Lock()
	previous := d.cacheProvenCold
	if previous == nil {
		d.cacheProvenCold = &verdict
	}
	m.mu.Unlock()
	if previous != nil {
		m.logf("session-controller: INVARIANT VIOLATION — a SECOND cold keep-alive ping ran on ws=%q session=%s turn_id=%s while the verdict from turn_id=%s (at_ms=%d) should have declined it; the earlier verdict is kept and the later ping paid %d uncached input tokens for a finding the daemon already had",
			d.workspace, d.sessionID, verdict.turnID, previous.turnID, previous.atMs, verdict.uncachedInputTokens)
		return
	}
	m.logf("session-controller: CACHE KEEP-ALIVE CAME BACK COLD ws=%q session=%s turn_id=%s uncached_input_tokens=%d threshold=%d elapsed_ms=%d ttl_ms=%d — the ping is a dozen tokens of prompt and it paid for the whole conversation, so the cache it was sent to refresh was already gone. NO HIBERNATION IS TAKEN: a dead cache is a reason to stop spending on it, not a reason to tear the session down, and the idle cutoff (%s) is the only threshold that sleeps anything. Further pings for this session are declined until real work rebuilds the prefix",
		d.workspace, d.sessionID, verdict.turnID, verdict.uncachedInputTokens,
		cfg.UncachedCostAlertTokens, verdict.elapsedMs, verdict.ttlMs, cfg.IdleCutoff)
}

// retireColdCacheVerdictLocked drops a session's cold-cache verdict because real
// work is about to rebuild the prefix it was about. Caller holds m.mu.
//
// A SESSION WITH NO VERDICT IS UNTOUCHED AND SILENT: this runs on every prompt
// submission, and a line per prompt for a condition that almost never holds
// would drown the one that matters.
func (m *Manager) retireColdCacheVerdictLocked(d *sessionController, why string) {
	verdict := d.cacheProvenCold
	if verdict == nil {
		return
	}
	d.cacheProvenCold = nil
	m.logf("session-controller: cold-cache verdict RETIRED ws=%q session=%s by=%s turn_id=%s uncached_input_tokens=%d — real work is being submitted and its turn rebuilds the prompt cache the verdict was about, so keep-alive pings are eligible again",
		d.workspace, d.sessionID, why, verdict.turnID, verdict.uncachedInputTokens)
}
