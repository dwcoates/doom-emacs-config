package sessioncontroller

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/statedb"
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
	if m.keepAliveHoldTurnLocked(d) != "" {
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
	// THE ELAPSED IS MEASURED BEFORE THE CLAIM, and remembered with it. It is
	// what a cold ping's hibernation reports, and it cannot be taken afterwards:
	// the ping's own turn end stamps the durable last-turn-end to now, so a
	// figure derived later would read ~0 for a session that had in fact been
	// quiet for an hour (keepalivecold.go).
	//
	// Taken with NO mutex held, because the durable read may reach the legacy
	// stamper, which lives outside this package. The id is minted here for the
	// same reason the measurement is — the two are one fact about one ping — and
	// an id minted for a ping that then turns out to be ineligible is simply
	// discarded, having named nothing.
	turnID = newKeepAliveTurnID()
	measurement := m.measureKeepAlivePing(workspace, turnID)

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
	//
	// THE MEASUREMENT IS PUBLISHED WITH THE CLAIM, in this one acquisition. It
	// is what the ping's own result fills and what the ping's own turn end
	// reads, and a measurement published a moment after the claim would leave an
	// instant in which a result could land against a ping that had nothing to
	// record it in.
	d.keepAliveTurnID = turnID
	d.keepAlivePing = &measurement
	sessionID := d.sessionID
	m.mu.Unlock()

	// THE WINDOW OPENS BEFORE THE PROMPT REACHES THE SHIM, which is the prompt
	// receipt's ordering inverted and for the mirror-image reason. A receipt is
	// written first so a prompt the user saw can never be unrecoverable; a
	// keep-alive window is written first so a ping the vendor ran can never be
	// UNATTRIBUTED — an unwindowed ping renders as though the user typed it,
	// and that is the one outcome the exclusion exists to prevent.
	//
	// A window for a ping whose submit then fails costs nothing: it excludes an
	// interval in which nothing was written.
	// The window's start is REMEMBERED, not re-read. It is the ledger's own
	// durable lower bound, and it is what an abandoned ping's window closes at:
	// a ping that never ran occupies an empty interval, and stamping that end
	// from a fresh clock read would invent an exclusion interval instead.
	var windowStartedAtMs int64
	if m.cfg.KeepAliveWindows != nil {
		windowStartedAtMs = m.now()
		if err := m.cfg.KeepAliveWindows.Open(KeepAliveWindowRecord{
			TurnID: turnID, Workspace: workspace, StartedAtMs: windowStartedAtMs,
		}); err != nil {
			// NO WINDOW EXISTS TO CLOSE — Open is the write that would have
			// created the row — so this abandonment releases the claim and the
			// holds only. Calling Close here would report a second failure for
			// a row that was never written.
			released := m.abandonKeepAlivePing(d, turnID)
			m.logf("session-controller: keep-alive ping ABANDONED ws=%q session=%s turn_id=%s held_released=%d error=%v — the window could not be recorded, so the ping is NOT submitted; running it unwindowed would render the ping as the user's own prompt",
				workspace, sessionID, turnID, released, err)
			return "", err
		}
	} else {
		m.logf("session-controller: keep-alive ping ws=%q session=%s turn_id=%s has NO WINDOW LEDGER — the ping's conversation items will not be excluded from rendering",
			workspace, sessionID, turnID)
	}
	m.logf("session-controller: keep-alive ping SUBMITTING ws=%q session=%s turn_id=%s — refreshing the vendor prompt cache before its TTL expires",
		workspace, sessionID, turnID)
	err = m.forwardPrompt(ctx, d, turnID, keepalive.PingText, "keep-alive:"+turnID, "",
		corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE, submitterKeepAlive)
	if err != nil {
		// THE CLAIM, THE HOLDS AND THE WINDOW ARE RELEASED TOGETHER. Each one
		// alone is a leak the others cannot repair: a surviving claim parks
		// every later prompt behind a turn that never started, a surviving hold
		// names a turn id nothing will ever end, and a surviving window has no
		// upper bound and withholds the whole conversation from here on. The
		// acquire that took all three is this one release.
		released := m.abandonKeepAlivePing(d, turnID)
		m.logf("session-controller: keep-alive ping FAILED ws=%q session=%s turn_id=%s held_released=%d error=%v — the claim, the holds and the window are all released; the cache is left to expire until the next tick",
			workspace, sessionID, turnID, released, err)
		m.closeKeepAliveWindow(d, turnID, windowStartedAtMs)
		return "", err
	}
	m.logf("session-controller: keep-alive ping SUBMITTED ws=%q session=%s turn_id=%s", workspace, sessionID, turnID)
	return turnID, nil
}

// abandonKeepAlivePing releases everything a claimed ping owns in the session's
// memory — the claim itself and every prompt held behind its turn id — under
// ONE acquisition of m.mu, and reports how many holds it released.
//
// THE CLAIM AND THE HOLDS ARE ONE RELEASE because they were one acquire. The
// hold's whole meaning is "waiting for turn X to end"; if turn X is abandoned,
// nothing will ever end it, and the sole ordinary release (the ping's turn-end
// boundary, queue.go) requires a real end that is never coming. Clearing the
// claim alone therefore does not free the prompts — it strands them, invisibly,
// until the user cancels them by hand.
//
// The released prompts are ordinary PENDING entries afterwards, so the queue is
// republished and the front one delivered exactly as a turn-end drain would:
// the ping never ran, so there is nothing to rewind before they go.
func (m *Manager) abandonKeepAlivePing(d *sessionController, turnID string) int {
	m.mu.Lock()
	if d.keepAliveTurnID == turnID {
		d.keepAliveTurnID = ""
		// The measurement goes with the claim it was published with. A ping that
		// never ran measured nothing, and a measurement left standing here would
		// be read at the NEXT ping's turn end as though it were that ping's own.
		d.keepAlivePing = nil
	}
	released := d.queue.releaseKeepAliveHold(turnID)
	if released == 0 {
		m.mu.Unlock()
		return 0
	}
	var next *queueEntry
	if !d.turn.active() {
		next = d.queue.popFrontDeliverable()
	}
	view, recs := m.publishQueueLocked(d)
	sessionID := d.sessionID
	m.mu.Unlock()

	m.publish(sessionID, view, recs)
	m.noteDrainActivity()
	if next != nil {
		go m.deliver(d, next)
	}
	return released
}

// restampKeepAliveWindowStart moves a ping's window start onto the clock that
// stamps the conversation items the window is compared against.
//
// WHY THE PRE-SUBMIT STAMP IS NOT ENOUGH. SubmitKeepAlivePing writes the window
// before the prompt reaches the shim, so the only instant available to it is
// m.now() — the DAEMON's clock. The items the exclusion later judges are stamped
// by the VENDOR (the shim's own clock, carried through to ConversationItem.ts_ms).
// Under any skew between the two the interval is not wrong by a little: a daemon
// running ahead produces started_at_ms greater than every item the ping writes,
// so the open window covers nothing at all while it is open, and the close —
// stamped from the boundary's vendor-clocked produced_at_ms — would land BELOW
// its own start. The bound has to come from the same authority as the values it
// is compared with, and the ping's own TurnStarted is where that authority first
// speaks about this turn.
//
// IT IS MATCHED ON THE TURN ID, never applied to whatever turn started: a real
// user turn beginning here would otherwise move the ping's lower bound onto
// itself and hand the user's own records to the exclusion.
//
// A FAILED RE-STAMP LEAVES THE PROVISIONAL BOUND STANDING and says so. The row
// is already there and already excludes from the daemon-clock instant onward,
// which is the pre-existing behavior; refusing the ping at this point is not
// available (it is already running) and dropping the window would un-attribute a
// ping that is mid-flight.
func (m *Manager) restampKeepAliveWindowStart(d *sessionController, turnID string, atMs int64) {
	if m.cfg.KeepAliveWindows == nil || turnID == "" || atMs <= 0 {
		return
	}
	m.mu.Lock()
	ping := d.keepAliveTurnID
	m.mu.Unlock()
	if ping == "" || ping != turnID {
		return
	}
	if err := m.cfg.KeepAliveWindows.Open(KeepAliveWindowRecord{
		TurnID: turnID, Workspace: d.workspace, StartedAtMs: atMs,
	}); err != nil {
		m.logf("session-controller: keep-alive window START RE-STAMP FAILED ws=%q session=%s turn_id=%s started_at_ms=%d error=%v — the window keeps the daemon-clock bound taken before the submit, so its lower edge is only as good as the two clocks agreeing",
			d.workspace, d.sessionID, turnID, atMs, err)
		return
	}
	m.logf("session-controller: keep-alive window START RE-STAMPED ws=%q session=%s turn_id=%s started_at_ms=%d — the bound now comes from the turn's own boundary, the same clock that stamps the items it is compared against",
		d.workspace, d.sessionID, turnID, atMs)
}

// closeKeepAliveWindow stamps a ping's end in the durable ledger and FAILS HARD
// when it cannot.
//
// AN UNCLOSED WINDOW IS NOT A BOOKKEEPING NUISANCE. It has no upper bound, so
// from its start onward every conversation item on the workspace is withheld
// from every rendering — the live push, the resync, the replay — for as long as
// the row stands. Continuing past the failure with a log line leaves the user
// watching a conversation that has silently stopped existing.
//
// The log line stays as the single canonical record of the violation; the card
// is the same fact reaching a human, on the established system-failure channel
// every other unrecoverable session fault uses.
func (m *Manager) closeKeepAliveWindow(d *sessionController, turnID string, endedAtMs int64) {
	if m.cfg.KeepAliveWindows == nil {
		return
	}
	err := m.cfg.KeepAliveWindows.Close(turnID, endedAtMs)
	if err == nil {
		return
	}
	// THE TWO REFUSALS ARE DIFFERENT FAULTS AND ARE NAMED DIFFERENTLY. An
	// unwritable row leaves the window OPEN and unbounded — every later item on
	// the workspace withheld, forever. An INVERTED interval is the ledger
	// refusing an end that precedes its own start: the row IS bounded (clamped
	// to its start, so it can never cover more than the instant the ping began),
	// and what is broken is the pair of clocks that produced the two instants.
	// Reporting the second as the first would send a reader hunting a blackout
	// that is not there.
	detail := fmt.Sprintf("turn_id=%s ended_at_ms=%d: %v", turnID, endedAtMs, err)
	if errors.Is(err, statedb.ErrKeepAliveWindowInverted) {
		m.logf("session-controller: keep-alive window CLOSE REFUSED ws=%q session=%s turn_id=%s ended_at_ms=%d error=%v — the end precedes the window's own start, so the interval is clamped to the start and covers nothing after it; the ping's own id-less records may render as the user's until the clocks behind these two instants agree",
			d.workspace, d.sessionID, turnID, endedAtMs, err)
		if d.consumer != nil {
			d.consumer.pushFailure(d.consumer.keepAliveWindowInvertedUUID(turnID),
				errclass.KeepAliveWindowInverted(detail))
		}
		return
	}
	m.logf("session-controller: keep-alive window CLOSE FAILED ws=%q session=%s turn_id=%s ended_at_ms=%d error=%v — the window stays open, and an open window excludes every later item on this workspace; this must be repaired before the conversation renders again",
		d.workspace, d.sessionID, turnID, endedAtMs, err)
	if d.consumer != nil {
		d.consumer.pushFailure(d.consumer.keepAliveWindowUnclosedUUID(turnID),
			errclass.KeepAliveWindowUnclosed(detail))
	}
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

// keepAliveHoldTurnLocked reports the keep-alive turn a real prompt arriving
// right now must park behind, or "" when none does. Caller holds m.mu.
//
// IT IS ONE CONTINUOUS OWNERSHIP EXPRESSED IN TWO FIELDS, and every eligibility
// read goes through here so no caller can consult half of it. The ping's own
// claim covers the turn; the workspace's rewind claim covers the aftermath the
// turn's end starts — the stop, the truncated copy, the registry flip and the
// respawn. A prompt admitted between them would start a turn the rewind is
// about to kill and cut out of the transcript, with nothing told to the user.
func (m *Manager) keepAliveHoldTurnLocked(d *sessionController) string {
	if d.keepAliveTurnID != "" {
		return d.keepAliveTurnID
	}
	return m.keepAliveRewinds[d.workspace]
}

// claimKeepAliveRewindLocked transfers the ending ping's claim to the rewind
// that is about to run. Caller holds m.mu, in the SAME acquisition that cleared
// the ping's own claim — the transfer is what makes the two fields one hold
// rather than two states with a gap between them.
func (m *Manager) claimKeepAliveRewindLocked(workspace, pingTurnID string) {
	if m.keepAliveRewinds == nil {
		m.keepAliveRewinds = map[string]string{}
	}
	m.keepAliveRewinds[workspace] = pingTurnID
}

// releaseKeepAliveRewindLocked ends the rewind's hold. Caller holds m.mu. It
// matches on the turn id for noteKeepAliveTurnEndedLocked's reason: a later
// ping's rewind must not be released by an earlier one's tail.
func (m *Manager) releaseKeepAliveRewindLocked(workspace, pingTurnID string) {
	if m.keepAliveRewinds[workspace] == pingTurnID {
		delete(m.keepAliveRewinds, workspace)
	}
}

// noteKeepAliveTurnEndedLocked clears the ping claim when the ending turn is
// the ping's own, and hands back the measurement that claim carried. Caller
// holds m.mu.
//
// IT MATCHES ON TURN ID rather than clearing unconditionally, because a turn
// end for some other turn — a user prompt that raced in, a late end for a turn
// the daemon already accounted — must not release a hold the ping still owns.
//
// THE MEASUREMENT LEAVES WITH THE CLAIM, in this one acquisition, and is
// RETURNED rather than left for the caller to read separately. That is what
// makes the cold-ping verdict ordered after the ping's turn end by construction:
// the only way to obtain the measurement is to be the boundary that ended the
// turn it belongs to (keepalivecold.go).
func (d *sessionController) noteKeepAliveTurnEndedLocked(turnID string) (*keepAlivePingMeasurement, bool) {
	if d.keepAliveTurnID == "" {
		return nil, false
	}
	if turnID != "" && turnID != d.keepAliveTurnID {
		return nil, false
	}
	d.keepAliveTurnID = ""
	measurement := d.keepAlivePing
	d.keepAlivePing = nil
	return measurement, true
}
