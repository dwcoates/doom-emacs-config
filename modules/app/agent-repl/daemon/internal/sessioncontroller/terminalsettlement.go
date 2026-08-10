package sessioncontroller

import (
	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/ssm"
)

// terminalsettlement.go — TURN SETTLEMENT IS DECOUPLED FROM ACCOUNTING
// ENRICHMENT, and this file is where the two are held apart.
//
// # The wedge this closes
//
// A turn's terminal result used to be PARKED — "terminal result held for
// accounting" — and published only when the turn's accounting settled, which
// only a `TurnEnded` off the stream could trigger. That made two unrelated
// things one thing: whether the user sees the answer and whether the token
// ledger is complete. Session s_f223cd698d687299 paid for it. The result
// arrived at 18:14:30.157, the hold went on in the same millisecond, and the
// `TurnEnded` it waited for did not arrive until the shim's own ten-minute
// turn-lifecycle watchdog produced one at 18:24:30. For ten minutes the answer
// was in the daemon's hands and the workspace rendered `thinking`.
//
// # The separation
//
//   - THE TURN END IS PUBLISHED IMMEDIATELY AND UNCONDITIONALLY. The vendor's
//     `result` message IS the end of the turn — nothing further is coming from
//     the query that produced it — so the result reaches the conversation and
//     the durable claim is closed from that same evidence, with no accounting
//     precondition of any kind. Nothing about the token ledger can gate it,
//     because nothing about the token ledger is consulted.
//
//   - THE ACCOUNTING STAMP IS ENRICHED AFTERWARDS, asynchronously, and may be
//     revised late. It is released by a LEVEL-TRIGGERED predicate over the
//     corrections ledger (accountinghold.go) rather than by an event wakeup, so
//     the ordering that wedged production — a correction 52ms AHEAD of the hold
//     that waits for it — is not merely handled but unrepresentable.
//
// The accounting hold still exists. It holds the STAMP. It never again holds
// the turn.
//
// # What is deliberately unchanged
//
// Every accounting-correctness guard is untouched: an unattributed response, a
// malformed usage observation and a divergent persisted settlement all still
// reject, still degrade loudly, and still render as INVALID ACCOUNTING. The
// change is only that none of them can decide whether a turn is over.

// settleTurnOnTerminalResult is the whole of the terminal-result path: settle,
// publish, then enrich.
//
// THE ORDER IS THE CONTRACT.
//
//   - THE TURN STATE SETTLES FIRST, and nothing about it is conditional on the
//     accounting. Settling ahead of the push makes the wedge's rendering
//     unrepresentable in BOTH directions: a frontend that holds the answer
//     necessarily already holds the settled state, so an answer can never be
//     drawn beneath a `thinking` workspace. Anything reading the daemon's own
//     turn record — the queue's drain, the keep-alive clock, the idle sweep —
//     gets the same guarantee: observing the result implies the end was
//     recorded.
//   - THE ANSWER IS PUBLISHED UNCONDITIONALLY, on every path, including the one
//     where the settlement above failed.
//   - THE ENRICHMENT HOLD IS INSTALLED LAST, over a ledger it reads at install
//     time, so it cannot be waiting on anything that already happened.
func (c *consumer) settleTurnOnTerminalResult(turnID string, ev *corev1.Event) {
	c.noteTerminalResult(turnID, ev)
	c.settleTurnStateOnTerminalResult(turnID, ev)
	c.pushConversationAttributed(ev, true, turnID)
	c.logf("session-controller: terminal result PUBLISHED session=%s turn_id=%s seq=%d — the vendor's result is the turn's own end, so it reaches the conversation now rather than behind an accounting settlement",
		c.sessionID, turnID, ev.GetSeq())
	c.installAccountingEnrichment(turnID)
}

// settleTurnStateOnTerminalResult closes the durable turn claim the result just
// ended, and delivers the turn-end boundary that closure implies.
//
// THE LATER `TurnEnded` IS NOT LOST AND IS NOT A CONTRADICTION. recordTurnEnd
// admits an end naming a claim the daemon already synthesized a close for as an
// idempotent REPLAY, and a replayed end still notifies: the queue drain, the
// keep-alive clock and every turn waiter run off the real boundary exactly as
// they did before, carrying the outcome only the shim's event knows. What
// changes is that none of them — and none of the user's rendering — has to WAIT
// for it.
//
// A FAILURE HERE IS LOUD AND SETTLES NOTHING SILENTLY. The answer is published
// regardless, so a claim this could not close leaves the workspace in the state
// it was in before, which the `TurnEnded` still cures when it arrives.
//
// BOTH HALVES SETTLE IN ONE CALL. The durable claim and the session-status axis
// are one fact read two ways, and settling either alone reproduces the wedge:
// a retired claim under a `thinking` axis renders a working workspace over a
// finished turn, and a painted axis over a standing claim holds every queued
// prompt behind an end that is not coming. SettleTurnFromTerminalResult retires
// the claims, paints the axis and hands the re-derived state to the frontends
// that are already drawing the old one.
func (c *consumer) settleTurnStateOnTerminalResult(turnID string, ev *corev1.Event) {
	// A DISPLACED TURN IS NOT A FINISHED ONE. A teardown that is about to take
	// this shim away interrupts the turn first, and the result that interrupt
	// provokes is the SDK unwinding rather than the turn arriving at its own
	// end. Settling from it would record a turn the daemon stopped as one that
	// finished, over a resumption the successor daemon owes the user
	// (turnresumption.go). The stop's own path closes the claim.
	if c.turnStopInFlight(turnID) {
		c.logf("session-controller: terminal result turn-state settlement DECLINED session=%s ws=%s turn_id=%s seq=%d decision=stop_in_flight — a teardown is stopping this shim over this turn, so the stop's own close attributes it rather than the result the interrupt provoked",
			c.sessionID, c.workspace, turnID, ev.GetSeq())
		return
	}
	settled, err := c.ssm.SettleTurnFromTerminalResult(c.workspace, c.sessionID, c.push.PushWorkspaceState)
	if err != nil {
		c.warn("session-controller: terminal result turn-state settlement FAILED session=%s ws=%s turn_id=%s seq=%d cause=%s: %v — the turn's result is published, but its durable claim may outlive it until the shim's own `TurnEnded` arrives",
			c.sessionID, c.workspace, turnID, ev.GetSeq(), ssm.TurnCloseTerminalResult, err)
		return
	}
	if !settled {
		c.logf("session-controller: terminal result turn-state settlement NOT NEEDED session=%s turn_id=%s seq=%d — the axis already reads settled and the ledger holds no claim for this session, so the turn's own end got here first",
			c.sessionID, turnID, ev.GetSeq())
		return
	}
	c.logf("session-controller: terminal result SETTLED THE TURN session=%s ws=%s turn_id=%s seq=%d cause=%s — the vendor reported this turn's result, which is its end, so the claim ledger and the render state are both settled from that evidence rather than held open for a second announcement of the same fact",
		c.sessionID, c.workspace, turnID, ev.GetSeq(), ssm.TurnCloseTerminalResult)
	boundary := c.boundaryInstant(ev)
	// THE QUEUE'S BOUNDARY MOVES WITH THE LEDGER'S. The drain's hold, the
	// keep-alive claim and the turn record are all statements about whether a
	// turn is running, and the durable claim above has just said it is not.
	// Leaving this edge on the shim's later `TurnEnded` would hold prompts
	// behind a claim the ledger no longer has.
	//
	// IT IS ANNOUNCED ONCE. The shim's `TurnEnded` for this same turn is a
	// replay of this very end, and a second announcement lands after the NEXT
	// turn has taken the daemon's turn record — retiring, for instance, a
	// keep-alive ping's claim on behalf of a turn that ended before it began.
	if !c.claimTurnEndAnnouncement(turnID) {
		return
	}
	if c.onTurn != nil {
		c.onTurn(false, boundary)
	}
	// THE KEEP-ALIVE CLOCK STARTS AT THE END THE VENDOR REPORTED. Every idle
	// decision — the ping, the warm compaction, the hibernation cutoff — is a
	// time-since check against the last turn end, so it has to measure from the
	// boundary that actually ended the turn rather than from the shim's later
	// announcement of it. The shim's `TurnEnded` re-stamps the same turn when it
	// lands, which moves the mark by the announcement's own latency and no more.
	if c.onTurnEnded != nil {
		c.onTurnEnded(boundary)
	}
}

// installAccountingEnrichment registers this turn's stamp against the
// corrections ledger.
//
// The dependency set is the turn's own responses plus its end-boundary usage
// observation, and the predicate is evaluated inside Install against the ledger
// as it stands — so a turn whose dependencies all arrived before the result
// settles its stamp synchronously, on this call, and a turn still owed one
// settles the instant that one is filed. Neither path involves an event wakeup
// that could be missed.
func (c *consumer) installAccountingEnrichment(turnID string) {
	awaited := c.accounting.enrichmentDependencies(turnID)
	c.accounting.corrections.Install(turnID, awaited, c.releaseAccountingEnrichment)
	if pending := c.accounting.corrections.Awaiting(turnID); len(pending) > 0 {
		c.logf("session-controller: accounting stamp AWAITING CORRECTIONS session=%s turn_id=%s responses=%d awaiting=%v — the turn itself is already settled and published; only its token stamp is outstanding",
			c.sessionID, turnID, len(awaited), pending)
	}
}

// releaseAccountingEnrichment is the corrections ledger's callback: the turn's
// stamp may now be resolved, or a correction has arrived for one already
// resolved.
func (c *consumer) releaseAccountingEnrichment(turnID string, late bool) {
	if late {
		c.reviseSettledTurnAccounting(turnID)
		return
	}
	if err := c.settleTurnAccounting(turnID); err != nil {
		c.warn("session-controller: ACCOUNTING DEGRADED session=%s ws=%s turn_id=%s — the turn's accounting stamp could not settle, but the turn itself is settled and its result is published: %v",
			c.sessionID, c.workspace, turnID, err)
	}
}

// reviseSettledTurnAccounting applies a correction that arrived AFTER its turn's
// stamp had already settled.
//
// A late correction is an ordinary vendor outcome rather than a race: the
// message_delta carrying a response's final usage has no ordering contract with
// anything, so it may land after the turn it belongs to is over. The old ledger
// cleared its corrections at settlement and simply lost it. Here the durable
// record is re-derived, re-persisted and re-published, so the figure the user
// reads is the vendor's final one.
//
// THE STORE STAYS AUTHORITATIVE. The revision is computed FROM the persisted
// settlement, never recomputed from a reducer entry that may already be
// retired, and a persistence failure is surfaced rather than papered over.
func (c *consumer) reviseSettledTurnAccounting(turnID string) {
	if c.accounting.hasTurn(turnID) {
		// The stamp has not settled yet, so there is nothing to revise: the
		// settling pass reads this same ledger and will pick the correction up.
		return
	}
	persisted, found, err := c.persistedTurnAccounting(turnID)
	if err != nil {
		c.warn("session-controller: LATE ACCOUNTING CORRECTION UNAPPLIED session=%s ws=%s turn_id=%s decision=persisted_lookup_failed — a response's final usage arrived after this turn settled and the persisted settlement could not be read to revise it: %v",
			c.sessionID, c.workspace, turnID, err)
		return
	}
	if !found {
		c.warn("session-controller: LATE ACCOUNTING CORRECTION UNAPPLIED session=%s ws=%s turn_id=%s decision=no_persisted_settlement — a response's final usage arrived after this turn settled, and the turn has no persisted settlement to revise",
			c.sessionID, c.workspace, turnID)
		return
	}
	if !c.accounting.corrections.ApplyTo(persisted) {
		return
	}
	revised, err := c.accountingStore.Record(c.sessionID, persisted)
	if err != nil {
		c.warn("session-controller: LATE ACCOUNTING CORRECTION NOT PERSISTED session=%s ws=%s turn_id=%s — a response's final usage arrived after this turn settled and the revised settlement could not be written, so the stored figures stay as first settled: %v",
			c.sessionID, c.workspace, turnID, err)
		return
	}
	c.logf("session-controller: accounting stamp REVISED LATE session=%s ws=%s turn_id=%s responses=%d — a response's final token usage arrived after this turn had already settled, so the settled stamp is corrected rather than the correction being dropped",
		c.sessionID, c.workspace, turnID, len(revised.GetResponses()))
	c.publishTurnAccountingStamp(turnID, revised)
}
