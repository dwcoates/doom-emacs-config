package sessioncontroller

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/daemonturn"
	"claude-repld/internal/keepalive"
)

// warmcompact.go — COMPACTING WHILE THE CACHE IS STILL WARM.
//
// A compaction reads the ENTIRE conversation. That read is either the cheapest
// turn of the session or the most expensive one, and nothing decides which
// except whether the vendor prompt cache is alive when it runs. The daemon used
// to compact at REVIVAL only, which is by construction after the cache expired:
// one live revival compaction read 1.5 MILLION uncached input tokens.
//
// So the compaction is moved to the last moment at which the cache is still
// provably alive — keepalive.WarmCompactMargin ahead of the last instant the
// keep-alive policy would submit anything — and everything downstream follows
// from the conversation simply being smaller from then on: the remaining pings
// are cheaper, hibernation proceeds unchanged, and a revival finds a conversation
// that has already been compacted and needs no compact-first read at all.
//
// IT REUSES THE COMPACTION MACHINERY THE REVIVAL ALREADY HAS. The same
// `/compact` text, the same forwardPrompt funnel, the same session-command
// recognition that keeps it out of the conversation feed. The only thing this
// file invents is WHEN, which is the whole feature.
//
// THE ELIGIBILITY IS THE KEEP-ALIVE PING'S, PLUS A SIZE FLOOR. A compaction is
// far more disruptive than a ping — it rewrites the conversation the user is
// working in — so the bar is not lower than the ping's anywhere, and it is
// higher in the two places that matter: a conversation too small to be worth
// compacting is left alone, and one attempt is allowed per cache window.

// ErrWarmCompactNotEligible reports a warm compaction declined because the
// session is not in a state that may be compacted. It is a NORMAL outcome, not
// a failure — the great majority of ticks in the window decline, because the
// first one takes the attempt and the rest see its anchor — so callers log it
// verbosely and move on. Modeled on ErrKeepAliveNotEligible for that reason.
var ErrWarmCompactNotEligible = errors.New("session-controller: the session is not eligible for a warm compaction")

// newWarmCompactRequestID mints the identity ONE warm compaction submits under.
// Prefixed like every other daemon-minted turn id so a turn id read in a log
// line says what it belongs to without a lookup, and randomized because a
// session is warm-compacted once per cache window for as long as it lives —
// the durable turn ledger refuses a second start under a name it already holds.
//
// An entropy failure is surfaced rather than papered over with a weaker id,
// which is newReviveCompactRequestID's discipline.
func newWarmCompactRequestID(sessionID string) (string, error) {
	var raw [8]byte
	if _, err := rand.Read(raw[:]); err != nil {
		return "", fmt.Errorf("session-controller: mint warm compaction request id for session %s: %w", sessionID, err)
	}
	return daemonturn.WarmCompactPrefix + sessionID + ":" + hex.EncodeToString(raw[:]), nil
}

// warmCompactEligibleLocked reports whether d may be warm-compacted right now,
// and why not when it may not. Caller holds m.mu.
//
// anchorTurnEndMs is the durable last-turn-end the policy took its decision
// against, and it is what makes the attempt exactly-once per cache window.
//
// THE REFUSALS ARE NOT INTERCHANGEABLE and none is redundant:
//
//   - a HIBERNATED session is outside the keep-alive loop entirely, so reaching
//     here means the one transition's construction failed — an invariant
//     violation, stated as one, exactly as the ping's check states it;
//   - a LIVE TURN means real work is running, and a compaction would rewrite the
//     conversation out from under it;
//   - QUEUED OR HELD PROMPTS mean real work is waiting; compacting first would
//     answer the user's prompt against a summary they never asked for;
//   - a KEEP-ALIVE IN FLIGHT owns the session's machine-generated turn already;
//   - a MERGE LEASE means somebody else owns the shim;
//   - a COMPACTION ALREADY CLAIMED is this decision already acted on;
//   - the SAME ANCHOR is this decision already TAKEN, whether or not it
//     succeeded. A warm compaction that failed falls back to today's behavior
//     for this cache window rather than being retried every sweep tick against
//     a cache that is meanwhile dying;
//   - an ALREADY-COMPACTED CONVERSATION is one this would compact a SECOND
//     time. The anchor above cannot see this: it makes the attempt exactly-once
//     per cache window, which says nothing about a compaction some OTHER path
//     ran — a compact-first revival, a `/compact` the user typed, the vendor's
//     own auto-compaction. The gate is the durable fact all of them close
//     (ssm/compactiongate.go), and a keep-alive ping does not re-open it,
//     precisely so that an idle session's pings cannot make its conversation
//     look freshly spoken-to;
//   - a SMALL CONVERSATION is not worth a full-history model call, and an
//     UNMEASURED one is an unknown rather than a small one. The size is one live
//     main-agent response's own input and is therefore the context window's
//     occupancy, not the turn's total spend (contextsize.go).
//
// A GATE READ FAILURE DECLINES. It is returned as an error rather than folded
// into a refusal reason because it is a genuine failure rather than a state,
// and the caller reports it as one — but the outcome is the same as any other
// refusal, because a compaction submitted on an unread gate is exactly the
// duplicate the gate exists to prevent.
func (m *Manager) warmCompactEligibleLocked(d *sessionController, anchorTurnEndMs int64) (ok bool, why string, err error) {
	if detail, asleep := m.hibernatedLocked(d.sessionID); asleep {
		m.logf("session-controller: INVARIANT VIOLATION — a warm-compaction eligibility check reached a HIBERNATED session ws=%q session=%s cause=%s; hibernation and keep-alive-stop are one transition, so this combination should be unreachable",
			d.workspace, d.sessionID, detail.Cause)
		return false, "hibernated", nil
	}
	if d.turn.active() {
		return false, "turn_active", nil
	}
	if len(d.queue.entries) > 0 {
		return false, "prompts_queued", nil
	}
	if m.keepAliveHoldTurnLocked(d) != "" {
		return false, "keep_alive_in_flight", nil
	}
	if m.cfg.SSM.MergeLeaseHeld(d.workspace) {
		return false, "merge_lease_held", nil
	}
	if d.daemonCompaction != nil {
		return false, "compaction_in_flight", nil
	}
	if d.warmCompactAnchorMs != 0 && d.warmCompactAnchorMs == anchorTurnEndMs {
		return false, "already_attempted_this_cache_window", nil
	}
	redundant, gate, err := m.compactionRedundant(d.workspace)
	if err != nil {
		return false, "compaction_gate_unreadable", err
	}
	if redundant {
		return false, "already_compacted:" + compactionRedundantDetail(gate), nil
	}
	if d.lastContextInputTokens <= 0 {
		return false, "context_size_unknown", nil
	}
	if d.lastContextInputTokens < keepalive.WarmCompactMinContextTokens {
		return false, "context_below_floor", nil
	}
	return true, "", nil
}

// SubmitWarmCompaction submits one pre-expiry compaction for workspace,
// anchored on the durable last-turn-end the policy decided against.
//
// It is an ORDINARY forwardPrompt of the same `/compact` text a compact-first
// revival submits. It earns no conversation bubble, because sessioncommand.go
// recognizes `/compact` as a session command and promptdispatch.go withholds
// receipts for those — so the daemon's own compaction does not render as a
// prompt the user typed, which is the same guarantee the keep-alive window
// gives the ping.
//
// THE WHOLE DECISION HAPPENS UNDER THE MANAGER MUTEX, for SubmitKeepAlivePing's
// reason: that mutex is the one every prompt submission takes, so "this session
// is idle, unqueued, awake and big enough" and "submit the compaction" are one
// act with respect to a user prompt arriving. Read the conditions, release,
// then submit would let a prompt land in the gap and be answered against a
// summary of the conversation it was written into.
//
// Returns ErrWarmCompactNotEligible when the re-check under the mutex declines.
func (m *Manager) SubmitWarmCompaction(ctx context.Context, workspace string, anchorTurnEndMs int64) (turnID string, err error) {
	// THE KEEP-ALIVE DEBT IS SETTLED FIRST, for the compact-first revival's
	// reason and with the same permanence: a compaction reads the whole
	// conversation and writes back a SUMMARY of it, so keep-alive turns standing
	// at the transcript tail when it runs are folded into the conversation
	// itself, where no later rewind can reach them.
	//
	// It runs OUTSIDE the mutex and BEFORE the eligibility read on purpose: it
	// may bounce the shim, and the decision below re-reads everything under the
	// mutex anyway. A ping in flight makes the settle decline and the
	// eligibility check refuse, which is the same answer reached twice.
	m.settleKeepAliveResidue(ctx, workspace, "warm-compaction")

	m.mu.Lock()
	d, live := m.byWS[workspace]
	if !live {
		m.mu.Unlock()
		return "", fmt.Errorf("%w: workspace %q has no live session controller", ErrWarmCompactNotEligible, workspace)
	}
	eligible, why, gateErr := m.warmCompactEligibleLocked(d, anchorTurnEndMs)
	if !eligible {
		sessionID, contextTokens := d.sessionID, d.lastContextInputTokens
		m.mu.Unlock()
		if gateErr != nil {
			// LOUDER THAN AN ORDINARY DECLINE, because it is not one. Every other
			// refusal is a state the daemon read successfully and decided against;
			// this is the daemon unable to tell whether it is about to compact the
			// same conversation twice, and declining on that ignorance.
			m.errorf("session-controller: warm compaction DECLINED ON AN UNREADABLE GATE ws=%q session=%s reason=%s error=%v — the daemon cannot tell whether this conversation has already been compacted, so it does not compact it; the cache window falls back to keep-alive pings",
				workspace, sessionID, why, gateErr)
			return "", fmt.Errorf("%w: workspace %q (%s): %w", ErrWarmCompactNotEligible, workspace, why, gateErr)
		}
		m.logf("session-controller: warm compaction declined ws=%q session=%s reason=%s context_input_tokens=%d floor=%d anchor_turn_end_ms=%d",
			workspace, sessionID, why, contextTokens, keepalive.WarmCompactMinContextTokens, anchorTurnEndMs)
		return "", fmt.Errorf("%w: workspace %q (%s)", ErrWarmCompactNotEligible, workspace, why)
	}
	sessionID := d.sessionID
	contextTokens := d.lastContextInputTokens
	turnID, err = newWarmCompactRequestID(sessionID)
	if err != nil {
		m.mu.Unlock()
		return "", err
	}
	// THE ANCHOR IS RECORDED BEFORE THE SUBMIT AND IS NEVER ROLLED BACK. It is
	// what makes the attempt exactly-once per cache window, and an attempt that
	// FAILED is still an attempt: retrying it on the next sweep tick would burn
	// the remaining margin re-attempting the same failing thing while the cache
	// it was racing dies anyway. The fallback for a failed warm compaction is
	// today's behavior — pings, then hibernation, then a compact-first revival —
	// and that fallback is only reachable if this window does not keep retrying.
	d.warmCompactAnchorMs = anchorTurnEndMs
	// THE CLAIM IS TAKEN UNDER THE SAME ACQUISITION that decided to take it, so
	// there is no instant in which the compaction is in flight and nothing says
	// so — and it is what the cold-read alarm matches this turn's result against.
	if err := m.claimDaemonCompactionLocked(d, daemonCompaction{
		turnID: turnID, kind: compactionWarm, anchorTurnEndMs: anchorTurnEndMs,
	}); err != nil {
		m.mu.Unlock()
		m.errorf("session-controller: warm compaction NOT CLAIMED ws=%q session=%s turn_id=%s error=%v — nothing was submitted; the cache window falls back to keep-alive pings and today's revival compaction",
			workspace, sessionID, turnID, err)
		return "", err
	}
	m.mu.Unlock()

	m.logf("session-controller: warm compaction SUBMITTING ws=%q session=%s turn_id=%s context_input_tokens=%d floor=%d anchor_turn_end_ms=%d — the prompt cache is still alive with %s of margin, so the whole-conversation read this costs is served from cache rather than re-ingested at full price after it expires",
		workspace, sessionID, turnID, contextTokens, keepalive.WarmCompactMinContextTokens, anchorTurnEndMs, keepalive.WarmCompactMargin)

	// PROMPT_ORIGIN_USER_SENT is the revival compaction's own origin and is kept
	// here deliberately: `/compact` is a session command, so it earns no bubble
	// on any origin, and inventing a second origin for the identical submission
	// would give the two compactions different accounting for no difference in
	// what they do.
	if err := m.forwardPrompt(ctx, d, turnID, compactCommandText,
		daemonturn.WarmCompactPrefix+sessionID, "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, submitterWarmCompaction); err != nil {
		// THE CLAIM IS RELEASED, THE ANCHOR IS NOT. A surviving claim would hand
		// the next turn's result to a compaction that never ran; a released anchor
		// would retry the failure every tick. The lifecycle is untouched either
		// way, which is the point: the keep-alive policy's own arms still run for
		// this session on the next tick and the session still hibernates on time.
		m.mu.Lock()
		m.releaseDaemonCompactionLocked(d, turnID)
		m.mu.Unlock()
		m.errorf("session-controller: warm compaction SUBMIT FAILED ws=%q session=%s turn_id=%s error=%v — the claim is released and the attempt is NOT retried in this cache window; the session falls back to today's behavior (keep-alive pings, then hibernation, then a compact-first revival that pays the full-context read), and the cold-read alarm will report that read if it happens",
			workspace, sessionID, turnID, err)
		return "", err
	}
	m.logf("session-controller: warm compaction SUBMITTED ws=%q session=%s turn_id=%s", workspace, sessionID, turnID)
	return turnID, nil
}
