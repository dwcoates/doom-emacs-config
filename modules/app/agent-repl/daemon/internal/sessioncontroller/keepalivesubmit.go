package sessioncontroller

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/keepalive"
)

// keepalivesubmit.go — SUBMITTING THE PING, and the check-and-act that decides
// whether to.
//
// THE WHOLE DECISION HAPPENS UNDER THE MANAGER MUTEX. That mutex is per-FLEET
// (a sessionController has none of its own) and is the same one every prompt
// submission takes, so "is this session idle, unqueued, awake and unleased" and
// "submit the ping" are one atomic act with respect to a user prompt arriving.
// Read the conditions, release, then submit would be a check-then-act: a user
// prompt could land in the gap and the ping would interrupt the very turn it
// just observed was not running.
//
// The sweep callback re-reads state under the mutex AFTER acquiring it rather
// than trusting whatever the caller saw, following the late-fire discipline the
// repull and context-cut callbacks already use: a tick's observation is stale
// by the time it can be acted on.

// ErrKeepAliveNotEligible reports a ping declined because the session is not in
// a state that may be pinged. It is a NORMAL outcome, not a failure — the great
// majority of ticks decline — so callers log it verbosely and move on.
var ErrKeepAliveNotEligible = errors.New("session-controller: the session is not eligible for a cache keep-alive ping")

// newKeepAliveTurnID mints the ping's request id, which becomes its turn_id.
// It is prefixed so a turn id read anywhere — a log line, a queue hold, a
// dropped-turn list in a rewind — says what it belongs to without a lookup.
func newKeepAliveTurnID() string {
	var b [12]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("session-controller: crypto/rand failed: %v", err))
	}
	return "ka_" + hex.EncodeToString(b[:])
}

// keepAliveEligibleLocked reports whether d may be pinged right now, and why
// not when it may not. Caller holds m.mu.
//
// THE FOUR REFUSALS ARE NOT INTERCHANGEABLE and none is redundant:
//
//   - a HIBERNATED session is outside the loop entirely; reaching here at all
//     would mean the one transition's construction had failed, so it is stated
//     as an invariant violation rather than an ordinary decline;
//   - a LIVE TURN means the cache is being refreshed by real work already, and
//     a ping would be a second turn racing it;
//   - QUEUED OR HELD PROMPTS mean real work is waiting for the session; pinging
//     would put a machine-generated turn ahead of the user's own;
//   - a MERGE LEASE means somebody else owns the shim (guardMergeLease refuses
//     it again at the submit, which is the backstop, not the reason).
func (m *Manager) keepAliveEligibleLocked(d *sessionController) (ok bool, why string) {
	if detail, asleep := m.hibernatedLocked(d.sessionID); asleep {
		m.logf("session-controller: INVARIANT VIOLATION — a keep-alive eligibility check reached a HIBERNATED session ws=%q session=%s cause=%s; hibernation and keep-alive-stop are one transition, so this combination should be unreachable",
			d.workspace, d.sessionID, detail.Cause)
		return false, "hibernated"
	}
	if d.turn.active() {
		return false, "turn_active"
	}
	if len(d.queue.entries) > 0 {
		return false, "prompts_queued"
	}
	if d.keepAliveTurnID != "" {
		return false, "keep_alive_in_flight"
	}
	if m.cfg.SSM.MergeLeaseHeld(d.workspace) {
		return false, "merge_lease_held"
	}
	return true, ""
}

// SubmitKeepAlivePing submits one cache keep-alive ping for workspace.
//
// It is an ORDINARY SubmitPrompt in every respect the vendor can see — same
// control frame, same turn lifecycle — and differs only in its attribution:
// PROMPT_ORIGIN_CACHE_KEEP_ALIVE, which is what every consumer excludes on.
// Making it a special control message instead would have meant the vendor did
// not refresh the cache, which is the one thing the ping is for.
//
// Returns ErrKeepAliveNotEligible when the re-check under the mutex declines.
func (m *Manager) SubmitKeepAlivePing(ctx context.Context, workspace string) (turnID string, err error) {
	m.mu.Lock()
	d, live := m.byWS[workspace]
	if !live {
		m.mu.Unlock()
		return "", fmt.Errorf("%w: workspace %q has no live session controller", ErrKeepAliveNotEligible, workspace)
	}
	eligible, why := m.keepAliveEligibleLocked(d)
	if !eligible {
		m.mu.Unlock()
		m.logf("session-controller: keep-alive ping declined ws=%q session=%s reason=%s", workspace, d.sessionID, why)
		return "", fmt.Errorf("%w: workspace %q (%s)", ErrKeepAliveNotEligible, workspace, why)
	}
	// THE CLAIM IS TAKEN UNDER THE SAME ACQUISITION that decided to take it.
	// It is what a real prompt arriving during the ping turn is held behind
	// (queue.go, the keep-alive hold), so a claim published after the mutex was
	// released would leave a window in which the ping is in flight and nothing
	// says so.
	turnID = newKeepAliveTurnID()
	d.keepAliveTurnID = turnID
	sessionID := d.sessionID
	m.mu.Unlock()

	m.logf("session-controller: keep-alive ping SUBMITTING ws=%q session=%s turn_id=%s — refreshing the vendor prompt cache before its TTL expires",
		workspace, sessionID, turnID)
	err = m.forwardPrompt(ctx, d, turnID, keepalive.PingText, "keep-alive:"+turnID, "",
		corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE, submitterKeepAlive)
	if err != nil {
		// THE CLAIM IS RELEASED ON FAILURE, or the session would hold a queue
		// entry behind a ping turn that never started and nothing would ever
		// release it.
		m.mu.Lock()
		if d.keepAliveTurnID == turnID {
			d.keepAliveTurnID = ""
		}
		m.mu.Unlock()
		m.logf("session-controller: keep-alive ping FAILED ws=%q session=%s turn_id=%s error=%v — the claim is released; the cache is left to expire unless a later tick still finds the window open",
			workspace, sessionID, turnID, err)
		return "", err
	}
	m.logf("session-controller: keep-alive ping SUBMITTED ws=%q session=%s turn_id=%s", workspace, sessionID, turnID)
	return turnID, nil
}

// KeepAliveTurnID reports the in-flight keep-alive turn for a workspace, if
// any. It exists for the queue's hold, which names the turn whose completion
// releases it.
func (m *Manager) KeepAliveTurnID(workspace string) (string, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	d, ok := m.byWS[workspace]
	if !ok || d.keepAliveTurnID == "" {
		return "", false
	}
	return d.keepAliveTurnID, true
}

// noteKeepAliveTurnEndedLocked clears the ping claim when the ending turn is
// the ping's own. Caller holds m.mu.
//
// IT MATCHES ON TURN ID rather than clearing unconditionally, because a turn
// end for some other turn — a user prompt that raced in, a late end for a turn
// the daemon already accounted — must not release a hold the ping still owns.
func (d *sessionController) noteKeepAliveTurnEndedLocked(turnID string) bool {
	if d.keepAliveTurnID == "" {
		return false
	}
	if turnID != "" && turnID != d.keepAliveTurnID {
		return false
	}
	d.keepAliveTurnID = ""
	return true
}
