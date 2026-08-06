package sessioncontroller

import (
	"time"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/registry"
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
// prediction: the session hibernates with cause cache_expired — the same cause
// the predicted branch already spells, recorded through the same one transition
// — and the user meets the revival gate that already exists, which offers
// "compact first" or "resume as-is".
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
// WHY THE ORDERING IS STRUCTURAL RATHER THAN TIMED. Hibernation stops the shim
// through the settled-gated teardown, which refuses a live turn outright, so a
// hibernation racing the ping's own teardown would simply fail. Nothing here
// waits for that race to resolve. The ping's result lands on the shim demux
// goroutine and does one thing: LATCH the measurement. The only reader of that
// latch is the ping's turn-end boundary, which by construction cannot run
// before the turn has ended. There is no window in which a hibernation and a
// live ping turn coexist, because the thing that takes the hibernation is the
// turn's own end.

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
	// uncachedInputTokens is what the ping's terminal result actually paid.
	uncachedInputTokens int64
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
func (m *Manager) noteKeepAlivePingCost(d *sessionController, turnID string, uncachedInputTokens int64) {
	if d == nil || turnID == "" {
		return
	}
	m.mu.Lock()
	measurement := d.keepAlivePing
	if measurement == nil || measurement.turnID != turnID || d.keepAliveTurnID != turnID {
		m.mu.Unlock()
		return
	}
	measurement.uncachedInputTokens = uncachedInputTokens
	measurement.resultObserved = true
	elapsedMs, ttlMs := measurement.elapsedMs, measurement.ttlMs
	m.mu.Unlock()
	m.logf("session-controller: keep-alive ping COST OBSERVED ws=%q session=%s turn_id=%s uncached_input_tokens=%d threshold=%d elapsed_ms=%d ttl_ms=%d — the figure is latched and read at the ping's own turn end, which is the only place it can be acted on without racing the turn's teardown",
		d.workspace, d.sessionID, turnID, uncachedInputTokens, m.keepAliveConfig().UncachedCostAlertTokens, elapsedMs, ttlMs)
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
	if !cfg.CameBackCold(measurement.uncachedInputTokens) {
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
			d.workspace, d.sessionID, measurement.turnID, measurement.uncachedInputTokens, cfg.UncachedCostAlertTokens)
		return
	}
	// DISPATCHED OFF THE SHIM READ LOOP. The transition stops and reaps the shim,
	// which cannot be done from the goroutine reading that shim's stream — the
	// same reason the ping's rewind aftermath is dispatched here.
	go m.hibernateOnColdKeepAlivePing(d, *measurement)
}

// hibernateOnColdKeepAlivePing records the sleep a cold ping has proved is
// owed, through the ONE transition every other cause takes.
//
// It builds no new cause and no new frontend message. registry's
// HibernationCauseCacheExpired already means exactly this — the prompt cache
// this session was being kept warm for is gone — and everything downstream of
// it already exists: the detail is persisted by the transition, mapped to
// HibernationDetail_CacheExpired by the server, and rendered by the webapp as
// the revival gate whose two buttons are "compact first" and "resume as-is".
//
// A FAILURE IS SURFACED, NEVER ABSORBED. There is no retry and no second route:
// the cache is provably gone, and if the sleep could not be recorded then the
// session stays awake with no record of it, which the log line says in those
// words rather than leaving a reader to infer it from silence.
func (m *Manager) hibernateOnColdKeepAlivePing(d *sessionController, measurement keepAlivePingMeasurement) {
	cfg := m.keepAliveConfig()
	account := registry.HibernationDetail{
		Cause:   registry.HibernationCauseCacheExpired,
		SinceMs: m.now(),
		// THE MEASURED FIGURES, both of them taken at the ping's submit and
		// carried here rather than re-derived from a clock that has since moved.
		ElapsedMs: measurement.elapsedMs,
		TTLMs:     measurement.ttlMs,
	}
	m.logf("session-controller: CACHE KEEP-ALIVE CAME BACK COLD ws=%q session=%s turn_id=%s uncached_input_tokens=%d threshold=%d elapsed_ms=%d ttl_ms=%d — the ping is a dozen tokens of prompt and it paid for the whole conversation, so the cache it was sent to refresh was already gone; the policy's prediction is overruled by its own measurement and the session hibernates with cause %s so the user is offered a compaction",
		d.workspace, d.sessionID, measurement.turnID, measurement.uncachedInputTokens,
		cfg.UncachedCostAlertTokens, measurement.elapsedMs, measurement.ttlMs, keepalive.CauseCacheExpired)
	if err := m.hibernateWithCause(d.workspace, account, evidenceObserved); err != nil {
		m.logf("session-controller: COLD KEEP-ALIVE HIBERNATION FAILED ws=%q session=%s turn_id=%s uncached_input_tokens=%d threshold=%d elapsed_ms=%d ttl_ms=%d error=%v — the prompt cache is provably gone and the session stays AWAKE with nothing recording it, so the next prompt pays a full context re-ingest without the user ever being offered the compaction",
			d.workspace, d.sessionID, measurement.turnID, measurement.uncachedInputTokens,
			cfg.UncachedCostAlertTokens, measurement.elapsedMs, measurement.ttlMs, err)
	}
}
