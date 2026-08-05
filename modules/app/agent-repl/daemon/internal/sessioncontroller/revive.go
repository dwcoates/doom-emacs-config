package sessioncontroller

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/registry"
)

// revive.go — BRINGING A HIBERNATED SESSION BACK, on the user's stated terms.
//
// Revival is LAZY AND GATED. The daemon nacks every prompt for a hibernated
// session (hibernation.go), so no model use can precede the choice: the webapp
// renders the gate from SessionView.hibernation and sends exactly one
// ReviveSessionCmd. That is the whole point of the feature — a session with six
// hours of accumulated context is expensive to resume, and the user should be
// told the price before paying it rather than after.
//
// THE TWO MODES DIFFER IN WHEN THE GATE COMES DOWN, not in how the session is
// brought up. Both take the ordinary create/resume path.
//
//   - DIRECT clears the durable hibernation first, then brings up. The gate is
//     gone the moment the record is written, and the session behaves exactly as
//     any other live session does.
//
//   - COMPACT_FIRST brings up while the record STILL SAYS HIBERNATED, drives a
//     compaction to completion, and only then clears. Keeping the record is
//     what keeps the gate standing, so "prompts are refused until compaction
//     lands" is the same mechanism that refused them before the revival began
//     rather than a second, parallel gate that could disagree with it.
//
// AND THAT IS WHY A FAILED COMPACTION LEAVES THE SESSION GATED. The clear is
// the LAST step and happens only on the completion signal; there is no path in
// which a compaction that errored, timed out, or never reported completion ends
// with an ungated session. The session limps into nothing — it stays asleep,
// loudly, and the user can choose again.

// compactFirstBound bounds how long a compact-first revival waits for the
// compaction to complete. It is a FAILURE bound, not a tuned delay: the wait
// ends the instant the compacting axis closes.
//
// Generous, because a compaction over a six-hour conversation is a model call
// across the whole history and is legitimately slow. Expiring it leaves the
// session gated, which is the safe direction.
const compactFirstBound = 10 * time.Minute

// compactCommandText is the prompt a compact-first revival submits. It is
// ordinary prompt text: sessioncommand.go recognizes it, promptdispatch.go
// forwards it verbatim, and the CLI runs the compaction. The daemon does not
// need a control frame for something the conversation surface already has.
const compactCommandText = "/compact"

// ReviveMode is the user's revival choice. It has no zero value that means
// anything: the wire oneof makes "no decision" unrepresentable, and so does
// this — Revive refuses a mode it was not given.
type ReviveMode int

const (
	// ReviveModeUnset is the refused zero.
	ReviveModeUnset ReviveMode = iota
	// ReviveModeDirect resumes the conversation as-is, full accumulated
	// context and all. The deliberate "I know it's big" path.
	ReviveModeDirect
	// ReviveModeCompactFirst compacts before accepting any prompt, paying the
	// full-context cost ONCE instead of on every subsequent turn.
	ReviveModeCompactFirst
)

func (m ReviveMode) String() string {
	switch m {
	case ReviveModeDirect:
		return "direct"
	case ReviveModeCompactFirst:
		return "compact_first"
	default:
		return "unset"
	}
}

// ErrRevivalInFlight reports a revival asked for while another is already
// running on the same workspace.
//
// IT IS HIBERNATION'S CLAIM, MIRRORED. Two ReviveSessionCmds can genuinely
// arrive at once — a double click on the gate, a webapp and an editor both
// holding the same stale SessionView — and without a claim both would drive
// their own `/compact` under the SAME request id, and the second would
// overwrite the first's completion waiter, leaving that revival waiting out its
// whole bound for a signal that had already been delivered to a channel nobody
// held.
var ErrRevivalInFlight = errors.New("session-controller: a revival is already in flight for this workspace")

// newReviveCompactRequestID mints the identity ONE revival compaction submits
// under. The session id is carried for readability; the random suffix is what
// makes the id unique across the repeated hibernate/revive cycles a single
// session goes through, and an entropy failure is surfaced rather than papered
// over with a weaker id (newSecureControllerGenerationID's discipline).
func newReviveCompactRequestID(sessionID string) (string, error) {
	var raw [8]byte
	if _, err := rand.Read(raw[:]); err != nil {
		return "", fmt.Errorf("session-controller: mint revive compaction request id for session %s: %w", sessionID, err)
	}
	return "revive-compact:" + sessionID + ":" + hex.EncodeToString(raw[:]), nil
}

// ReviveSession brings a hibernated workspace back under the user's chosen
// mode.
//
// THE ACK IS AT ACCEPTANCE, NOT AT COMPLETION. Returning means "the revival was
// accepted, the session is up, and the compaction has been submitted" — never
// "the compaction finished". A compact-first revival's compaction is a model
// call across the whole conversation and is bounded at compactFirstBound, so
// acking at completion left the user's ReviveSessionCmd unacked for up to ten
// minutes on the one path that has something to report.
//
// THE GATE'S RELEASE HAS ITS OWN CHANNEL and does not need this one: the
// hibernation record is what stands the gate up, and clearing it publishes a
// SessionView the webapp already renders from. The completion wait therefore
// runs on past this return, holding the revival claim so a second revival is
// still nacked until the compaction lands or its bound expires.
func (m *Manager) ReviveSession(ctx context.Context, workspace string, mode ReviveMode) error {
	if mode == ReviveModeUnset {
		return fmt.Errorf("session-controller: refusing to revive workspace %q with no revival mode; the choice between compacting and resuming as-is is the user's and the daemon does not have a default for it", workspace)
	}
	if m.cfg.Hibernations == nil {
		return fmt.Errorf("session-controller: cannot revive workspace %q: no hibernation registrar is wired", workspace)
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return fmt.Errorf("session-controller: workspace %q has no session to revive", workspace)
	}
	release, detail, err := m.claimRevival(workspace, sessionID)
	if err != nil {
		return err
	}
	// OWNERSHIP OF THE CLAIM MOVES AT HANDOFF. Every exit taken here releases
	// it; once the compaction completion wait is detached, that goroutine owns
	// the release instead, because the claim must outlive this return for as
	// long as the compaction is pending.
	detached := false
	defer func() {
		if !detached {
			// EVERY PRE-HANDOFF EXIT DISPOSES OF WHAT IT PARKED. A compact-first
			// revival brings the session up before it submits, so a prompt can
			// arrive and be parked in the window between the bring-up and a
			// failure — and this release is the last thing that knows a revival
			// was ever in flight. Dropping here is what keeps a refused submit
			// or a closing manager from leaving an entry no path can deliver.
			m.dropRevivalHolds(workspace, sessionID, "the revival did not reach its compaction")
			release()
		}
	}()
	if detail.Cause == "" {
		// NOT AN ERROR TO REPORT AS A FAILURE, but not silently successful
		// either: the user acted on a gate that is no longer standing, and
		// saying so is more useful than pretending to have revived something
		// that was never asleep.
		m.logf("session-controller: revive ws=%q session=%s mode=%s — the session is not hibernated; nothing to revive",
			workspace, sessionID, mode)
		return nil
	}
	m.logf("session-controller: revive BEGIN ws=%q session=%s mode=%s slept_since_ms=%d cause=%s",
		workspace, sessionID, mode, detail.SinceMs, detail.Cause)

	if mode == ReviveModeDirect {
		// THE CLEAR COMES FIRST on this path. There is nothing to gate: the
		// user asked to resume as-is, so the moment the record stops saying
		// hibernated the session is an ordinary one.
		if err := m.clearHibernation(workspace, sessionID); err != nil {
			return err
		}
		if _, err := m.ensure(ctx, workspace); err != nil {
			return fmt.Errorf("session-controller: reviving session %s (ws %q) directly: bringing it up: %w", sessionID, workspace, err)
		}
		m.logf("session-controller: revive COMPLETE ws=%q session=%s mode=direct", workspace, sessionID)
		return nil
	}

	// COMPACT-FIRST: bring up while the record STILL SAYS HIBERNATED, so the
	// gate that has been refusing prompts keeps refusing them for free.
	d, err := m.ensure(ctx, workspace)
	if err != nil {
		return fmt.Errorf("session-controller: reviving session %s (ws %q) for compaction: bringing it up: %w", sessionID, workspace, err)
	}
	compacted, disarm, err := m.armCompactionWait(d)
	if err != nil {
		return err
	}
	// The waiter travels with the claim: it is this revival's only route to the
	// completion signal, so it is retired here on every exit taken before the
	// handoff and by the completion goroutine afterwards.
	defer func() {
		if !detached {
			disarm()
		}
	}()
	m.logf("session-controller: revive ws=%q session=%s mode=compact_first — the session is up and STILL GATED; submitting %s before any prompt is accepted",
		workspace, sessionID, compactCommandText)

	// THE REVIVAL'S OWN COMPACTION IS THE ONE THING THE GATE LETS THROUGH. It
	// is submitted as submitterRevival, which guardHibernation admits precisely
	// so that the record can stay hibernated — and therefore keep gating the
	// user's prompts — while the compaction runs.
	//
	// ITS REQUEST ID IS UNIQUE PER ATTEMPT, and must be: a request id is now the
	// submitted turn's own identity on the wire (promptdispatch.go), and the
	// durable turn ledger refuses a second start under a name it already holds.
	// A session hibernated and compact-revived twice would otherwise submit its
	// second compaction under the first one's name — the same collision the
	// durable prompt receipt, keyed by request id, would already have taken.
	// The session id stays in the id so a log line still says which session's
	// revival it belongs to without a lookup.
	compactRequestID, err := newReviveCompactRequestID(sessionID)
	if err != nil {
		return err
	}
	if err := m.forwardPrompt(ctx, d, compactRequestID, compactCommandText,
		"revive-compact:"+sessionID, "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, submitterRevival); err != nil {
		m.logf("session-controller: revive COMPACTION SUBMIT FAILED ws=%q session=%s error=%v — the session STAYS GATED; it was not left half-revived and accepting prompts",
			workspace, sessionID, err)
		return fmt.Errorf("session-controller: reviving session %s (ws %q): submitting the compaction: %w", sessionID, workspace, err)
	}

	// THE HANDOFF. The compaction is submitted, which is everything the ack
	// reports; the wait for it to land runs on without the command context.
	m.mu.Lock()
	if m.closed {
		m.mu.Unlock()
		m.logf("session-controller: revive COMPACTION WAIT REFUSED ws=%q session=%s — the manager is closing, so nothing will observe the compaction; the session STAYS GATED and can be revived again",
			workspace, sessionID)
		return fmt.Errorf("session-controller: reviving session %s (ws %q): the manager is closing; the session remains hibernated and can be revived again", sessionID, workspace)
	}
	// Registered with the same WaitGroup Close joins, so the wait cannot
	// outlive the manager and race whatever tears down after it.
	m.exits.Add(1)
	detached = true
	m.mu.Unlock()
	go m.awaitReviveCompaction(workspace, sessionID, compacted, disarm, release)

	m.logf("session-controller: revive ACCEPTED ws=%q session=%s mode=compact_first — %s is submitted and the session STAYS GATED until it lands",
		workspace, sessionID, compactCommandText)
	return nil
}

// awaitReviveCompaction is the compact-first revival's completion half, run
// detached from the command context that accepted the revival.
//
// IT OWNS THE CLAIM AND THE WAITER for as long as the compaction is pending.
// Releasing them at the ack instead would let a second ReviveSessionCmd arm a
// second waiter over this one's, which is precisely the strand
// armCompactionWait refuses.
//
// ITS BOUND IS THE DAEMON'S, NOT A CALLER'S. compactFirstBound and the root
// context are what end the wait, so a webapp that disconnected the instant it
// got its ack does not abandon a compaction the session is still gated on.
//
// EVERY FAILING EXIT LEAVES THE RECORD UNTOUCHED. The clear is reached only on
// the completion signal, which is what makes "a failed compaction leaves the
// session gated" structural rather than a promise each error path has to keep.
func (m *Manager) awaitReviveCompaction(workspace, sessionID string, compacted <-chan struct{}, disarm, release func()) {
	defer m.exits.Done()
	defer release()
	defer disarm()
	bound := m.reviveCompactBound()
	select {
	case <-compacted:
		m.logf("session-controller: revive compaction LANDED ws=%q session=%s — releasing the gate", workspace, sessionID)
	case <-m.rootCtx.Done():
		m.logf("session-controller: revive compaction ABANDONED ws=%q session=%s error=%v — the session STAYS GATED",
			workspace, sessionID, m.rootCtx.Err())
		m.dropRevivalHolds(workspace, sessionID, "the daemon shut down before the compaction landed")
		return
	case <-time.After(bound):
		m.logf("session-controller: revive compaction TIMED OUT ws=%q session=%s bound=%s — the session STAYS GATED rather than limping into accepting prompts on a conversation that was never compacted",
			workspace, sessionID, bound)
		m.dropRevivalHolds(workspace, sessionID, "the compaction did not land within its bound")
		return
	}

	// THE CLEAR IS THE LAST STEP, reached only on the completion signal. Its
	// own failure is logged by clearHibernation and leaves the gate standing:
	// there is no caller left to return it to, and a gate that could not be
	// retired is the safe direction.
	if err := m.clearHibernation(workspace, sessionID); err != nil {
		m.logf("session-controller: revive GATE RELEASE FAILED ws=%q session=%s error=%v — the compaction landed but the record still claims a sleep; the session STAYS GATED and can be revived again",
			workspace, sessionID, err)
		m.dropRevivalHolds(workspace, sessionID, "the gate could not be released after the compaction landed")
		return
	}
	// THE PARKED PROMPTS ARE RELEASED AFTER THE CLEAR AND NOWHERE ELSE. This is
	// the second half of the delayed-never-dropped contract: the gate refused
	// nothing during the compaction, it DELAYED — and this is the instant the
	// delay is over (queue.go, revivalHoldSessionID).
	m.releaseRevivalHolds(workspace, sessionID)
	m.logf("session-controller: revive COMPLETE ws=%q session=%s mode=compact_first", workspace, sessionID)
}

// revivalHoldSessionLocked reports the session whose in-flight compact-first
// revival must park a prompt arriving for d right now, or "" when none does.
// Caller holds m.mu.
//
// IT IS THE TWO FACTS TOGETHER, and neither alone. The revival claim alone
// would park prompts through a DIRECT revival, which clears the gate first and
// has nothing to delay them for; the hibernation record alone is the ordinary
// gate, whose refusal for an UNDECIDED session is the whole feature and stays
// exactly as it was. Their conjunction names precisely one window — the user has
// chosen compact-first and its compaction has not landed — which is the only
// window in which a prompt is owed a delay instead of a refusal.
func (m *Manager) revivalHoldSessionLocked(d *sessionController) string {
	if !m.reviving[d.workspace] {
		return ""
	}
	if _, asleep := m.hibernatedLocked(d.sessionID); !asleep {
		return ""
	}
	return d.sessionID
}

// revivalParkAdmits reports whether a prompt from who must be admitted to
// workspace's queue as a PARKED entry rather than refused by the revival gate.
//
// IT IS ASKED AHEAD OF THE GATE in submitPromptAs, and it answers the same
// question revivalHoldSessionLocked does — from the workspace rather than from a
// live controller, because the gate is asked before ensure() and there may not
// be one yet. The two producers the gate keeps refusing are named here:
//
//   - submitterRevival is the revival's OWN `/compact`, which the gate admits
//     outright and which must never be parked behind itself;
//   - submitterKeepAlive is a ping, and a hibernated session is outside the
//     keep-alive loop by construction — a ping reaching here is that
//     construction having failed, which is said out loud rather than queued.
func (m *Manager) revivalParkAdmits(workspace string, who submitter) bool {
	if who == submitterRevival || who == submitterKeepAlive {
		return false
	}
	if m.cfg.Hibernations == nil {
		return false
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return false
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if !m.reviving[workspace] {
		return false
	}
	_, asleep := m.hibernatedLocked(sessionID)
	return asleep
}

// releaseRevivalHolds is the compaction's aftermath: the gate is down, so the
// prompts it parked become ordinary queued prompts and the front one is
// delivered.
//
// CALLED ONLY AFTER clearHibernation HAS SUCCEEDED. The release is what makes a
// parked prompt deliverable, and delivering one while the record still claimed a
// sleep would meet forwardPrompt's own gate and mark a perfectly good prompt
// ERROR. Ordering the clear first is what makes that unreachable rather than
// merely unlikely.
func (m *Manager) releaseRevivalHolds(workspace, sessionID string) {
	m.mu.Lock()
	d, live := m.byWS[workspace]
	if !live {
		m.mu.Unlock()
		m.logf("session-controller: revival holds NOT RELEASED ws=%q session=%s — the compaction landed but the workspace has no live session controller; any parked prompt went with its queue",
			workspace, sessionID)
		return
	}
	released := d.queue.releaseRevivalHold(sessionID)
	if released == 0 {
		m.mu.Unlock()
		return
	}
	next := d.queue.popFrontDeliverable()
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.logf("session-controller: revival holds RELEASED ws=%q session=%s released=%d — the compaction landed and the gate is down, so the prompts typed during it are delivered in the order they were typed",
		workspace, sessionID, released)
	m.publish(d.sessionID, view, recs)
	m.noteDrainActivity()
	if next != nil {
		go m.deliver(d, next)
	}
}

// dropRevivalHolds is the parked entry's BOUND, taken on every revival exit
// that leaves the gate standing: the compaction's bound expiring, the daemon
// shutting down mid-compaction, a clear that failed, and a revival that never
// reached its handoff at all.
//
// THE ENTRIES ARE DROPPED, NOT KEPT. The session is still asleep and the claim
// is going away, so nothing will ever release these holds: every delivery path
// refuses a held entry, so keeping them would leave chips rendering "waiting"
// against a queue no boundary can ever drain — a leak that outlives the very
// revival that caused it. Each one is named in the log with the text that was
// lost, because a prompt the user typed and the daemon discarded is not an
// event to record as a count.
//
// The user is not left guessing: the session is still gated, so the next prompt
// meets the ordinary refusal and the revival gate is still there to choose from.
func (m *Manager) dropRevivalHolds(workspace, sessionID, reason string) {
	m.mu.Lock()
	d, live := m.byWS[workspace]
	if !live {
		m.mu.Unlock()
		return
	}
	dropped := d.queue.dropRevivalHeld(sessionID)
	if len(dropped) == 0 {
		m.mu.Unlock()
		return
	}
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	for _, e := range dropped {
		m.logf("session-controller: revival-parked prompt DROPPED entry=%s ws=%q session=%s request_id=%s reason=%s text=%q — the compact-first revival ended without opening the gate, so the session is still asleep and nothing can ever deliver this prompt; it is discarded rather than left queued forever",
			e.id, workspace, sessionID, e.requestID, reason, e.text)
	}
	m.logf("session-controller: revival holds DROPPED ws=%q session=%s dropped=%d reason=%s — the gate still stands and the user can revive again",
		workspace, sessionID, len(dropped), reason)
	m.publish(d.sessionID, view, recs)
	m.noteDrainActivity()
}

// refuseRevivalForce is the loud nack for a force aimed at a revival-parked
// prompt. It carries ErrHibernated because that is exactly what is true: the
// session is still asleep, and this prompt runs when the compaction opens the
// gate and not before.
func (m *Manager) refuseRevivalForce(workspace, entryID, sessionID string) error {
	m.logf("session-controller: force REFUSED for a revival-parked queue entry=%s ws=%q session=%s — a compact-first revival's compaction is still pending; forcing the prompt would answer it against the whole uncompacted conversation the user chose this mode to avoid paying for. It is delivered on its own when the compaction lands, and is still cancellable",
		entryID, workspace, sessionID)
	return fmt.Errorf("session-controller: cannot force queued prompt %q on workspace %q: session %s is being revived and its compaction has not landed; the prompt is delivered automatically once it does, and can be cancelled: %w",
		entryID, workspace, sessionID, ErrHibernated)
}

// reviveCompactBound is how long the detached completion wait allows the
// compaction. Production always uses compactFirstBound; only a test assigns an
// override, so the timeout branch can be driven without a ten-minute wait.
func (m *Manager) reviveCompactBound() time.Duration {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.reviveCompactBoundOverride > 0 {
		return m.reviveCompactBoundOverride
	}
	return compactFirstBound
}

// claimRevival takes the exclusive per-workspace revival claim and reports the
// session's durable hibernation detail, read under the SAME acquisition.
//
// IT IS claimHibernation'S SHAPE, and for the identical reason. Revival was a
// check-then-act: it read HibernationOf, released nothing (it held no lock at
// all), and only then brought the session up and submitted `/compact`. Two
// concurrent revivals therefore both saw a hibernated session, both submitted a
// compaction under the same request id, and the second's arm silently replaced
// the first's completion waiter.
//
// The claim is taken under the manager mutex, the same one every prompt
// submission takes, so the read of the durable state and the decision to act on
// it are one act with respect to any other producer.
func (m *Manager) claimRevival(workspace, sessionID string) (release func(), detail registry.HibernationDetail, err error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.reviving == nil {
		m.reviving = map[string]bool{}
	}
	if m.reviving[workspace] {
		m.logf("session-controller: revive REFUSED ws=%q session=%s — a revival is already in flight for this workspace; nothing was brought up and no compaction was submitted",
			workspace, sessionID)
		return nil, registry.HibernationDetail{}, fmt.Errorf("%w: workspace %q", ErrRevivalInFlight, workspace)
	}
	detail, asleep := m.cfg.Hibernations.HibernationOf(sessionID)
	if !asleep {
		detail = registry.HibernationDetail{}
	}
	m.reviving[workspace] = true
	return func() {
		m.mu.Lock()
		delete(m.reviving, workspace)
		m.mu.Unlock()
	}, detail, nil
}

// armCompactionWait installs the one-shot completion signal a compact-first
// revival waits on, and returns the channel it closes plus the disarm that
// retires it.
//
// Armed BEFORE the compaction is submitted, never after: a compaction that
// completed between the submit and the arm would close an axis nobody was
// listening to, and the revival would wait out its whole bound for an event
// that had already happened.
//
// AN EXISTING WAITER IS A LOUD REFUSAL, NOT AN OVERWRITE. The revival claim
// makes a second arm unreachable, and this is the detection that says so if it
// ever becomes reachable again: overwriting would strand the first revival on a
// channel nothing will ever close, which is exactly the silent hang the bound
// above cannot distinguish from a slow compaction.
func (m *Manager) armCompactionWait(d *sessionController) (<-chan struct{}, func(), error) {
	done := make(chan struct{})
	var once bool
	armed := func() {
		m.mu.Lock()
		defer m.mu.Unlock()
		if once {
			return
		}
		once = true
		close(done)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if d.consumer.onContextCompacted != nil {
		m.logf("session-controller: revive ARM REFUSED ws=%q session=%s — a compaction completion waiter is already installed; overwriting it would strand the revival that owns it on a signal nothing will deliver",
			d.workspace, d.sessionID)
		return nil, nil, fmt.Errorf("session-controller: refusing to arm a second compaction waiter for workspace %q session %s: one is already installed", d.workspace, d.sessionID)
	}
	// The token identifies this arm. Funcs are not comparable in Go, and the
	// disarm must be able to tell its own waiter from a later one rather than
	// clearing whatever it finds.
	token := new(struct{})
	d.consumer.onContextCompacted, d.consumer.contextCompactedToken = armed, token
	disarm := func() {
		m.mu.Lock()
		defer m.mu.Unlock()
		// Cleared only while it is still OURS. A waiter belonging to somebody
		// else is not this revival's to retire.
		if d.consumer.contextCompactedToken == token {
			d.consumer.onContextCompacted, d.consumer.contextCompactedToken = nil, nil
		}
	}
	return done, disarm, nil
}

// HibernateWorkspace is the user-forced hibernation behind
// HibernateWorkspaceCmd. It is the SAME transition the sweeper's two automatic
// causes take, differing only in the cause it records.
//
// A LIVE TURN OR A HELD MERGE LEASE IS A LOUD NACK, not a wait and not a
// discard: the user interrupts first. The daemon never throws away in-flight
// work to satisfy a hibernate, so the refusal is the honest answer rather than
// an inconvenience.
func (m *Manager) HibernateWorkspace(workspace string) error {
	if m.cfg.SSM.MergeLeaseHeld(workspace) {
		err := fmt.Errorf("session-controller: workspace %q is being merged: merge.Coordinator holds the exclusivity lease on its session, so it cannot be hibernated until the merge reaches a terminal phase", workspace)
		m.logf("session-controller: forced hibernation REFUSED ws=%q — the merge lease is held; nothing was stopped",
			workspace)
		return err
	}
	return m.HibernateWithCause(workspace, registry.HibernationDetail{Cause: registry.HibernationCauseForced})
}
