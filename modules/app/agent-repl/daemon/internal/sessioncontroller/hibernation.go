package sessioncontroller

import (
	"errors"
	"fmt"

	"claude-repld/internal/errclass"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/registry"
)

// hibernation.go — THE ONE TRANSITION, and the gate it makes unavoidable.
//
// Three causes put a session to sleep: the idle cutoff, a cache that went cold
// before a ping could fire, and the user's own HibernateWorkspaceCmd. All three
// call HibernateWithCause and nothing else. That is not tidiness — it is what
// makes "hibernated but still being pinged" unrepresentable rather than merely
// unlikely:
//
//   - the transition STOPS the shim through the existing settled-gated teardown
//     (hibernate), which removes the workspace from Manager.byWS, and the ping
//     path requires a live controller;
//   - it then PERSISTS the flag and its typed account in one write, and the ping
//     path also refuses a session whose record says hibernated;
//   - it takes an exclusive per-workspace claim first, so two causes racing
//     produce exactly one transition and exactly one durable account.
//
// Any one of those would end keep-alive eligibility. Having the same function
// do all three is what stops a future caller from arranging half of it.
//
// ORDERING: the record is written AFTER a successful stop, never before. A
// daemon that dies in the window leaves a stopped shim and a record that does
// not claim hibernation, so the next prompt simply brings the session back up —
// the benign direction. The reverse ordering would leave a record claiming a
// sleep over a shim that is still running and still eligible to be pinged,
// which is the exact combination this file exists to prevent.

// ErrAlreadyHibernated reports a hibernation transition asked for on a session
// that is already asleep. It is the SINGLE-TRANSITION guarantee's refusal: the
// first cause to arrive owns the durable account, and a second one cannot
// overwrite it with its own story.
var ErrAlreadyHibernated = errors.New("session-controller: the session is already hibernated")

// ErrHibernationInFlight reports a hibernation transition asked for while
// another is mid-flight on the same workspace.
var ErrHibernationInFlight = errors.New("session-controller: a hibernation transition is already in flight for this workspace")

// ErrHibernated reports an operation refused because the session is
// hibernated and the user has not made a revival choice. Every prompt path
// funnels into the gate that returns it (promptdispatch.go).
//
// It ALIASES the errclass sentinel rather than being a second sentinel beside
// it. The refusal is expected and ordinary — it is the answer for every
// hibernated workspace — so it must reach a client as a NAMED failure the
// revival gate can be rendered from, and two sentinels would mean two chances
// for one of them to miss the classifier.
var ErrHibernated = errclass.ErrSessionHibernated

// HibernationRegistrar is the durable half of hibernation. It is separate from
// SessionRegistrar for the same reason ModelCatalogRegistrar is: a sleep is not
// transcript identity, and the two are written by different edges.
type HibernationRegistrar interface {
	// HibernationChanged persists the flag and its typed account in ONE write
	// and re-pushes the SessionView. A zero detail clears hibernation.
	//
	// It RETURNS an error, unlike most registrar hooks, because the caller
	// cannot proceed without it: a stop whose record did not land is a session
	// the next daemon will revive implicitly, which is the silent un-sleeping
	// the durable flag exists to prevent.
	HibernationChanged(sessionID string, detail registry.HibernationDetail) error
	// HibernationOf reads the persisted account. It is the REHYDRATION read: a
	// freshly booted daemon has no live controller to ask, and the record is
	// the only thing that knows the session was deliberately put to sleep.
	HibernationOf(sessionID string) (registry.HibernationDetail, bool)
	// TurnEndObserved persists when the session's most recent turn ended — the
	// keep-alive policy's one input.
	TurnEndObserved(sessionID string, atMs int64)
}

// HibernateWithCause is THE hibernation transition. account.Cause names why,
// and must be one of the registry's hibernation cause tokens.
//
// The workspace must be SETTLED: the underlying teardown refuses a live turn or
// an unseen vendor block with ErrNotSettled, which is what makes a teal tab over
// a working agent unreachable rather than merely rare.
func (m *Manager) HibernateWithCause(workspace string, account registry.HibernationDetail) error {
	if account.Cause == "" || !registry.ValidHibernationCause(account.Cause) {
		return fmt.Errorf("session-controller: refusing a hibernation transition for workspace %q with cause %q; a sleep the gate cannot explain is not one to take",
			workspace, account.Cause)
	}
	if m.cfg.Hibernations == nil {
		return fmt.Errorf("session-controller: refusing to hibernate workspace %q cause=%q: no hibernation registrar is wired, so the sleep could not be made durable and the next daemon would revive the session implicitly",
			workspace, account.Cause)
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return fmt.Errorf("session-controller: workspace %q has no session to hibernate", workspace)
	}
	release, err := m.claimHibernation(workspace, sessionID, account.Cause)
	if err != nil {
		return err
	}
	defer release()

	if account.SinceMs == 0 {
		account.SinceMs = m.now()
	}
	m.logf("session-controller: hibernation transition BEGIN ws=%q session=%s cause=%s since_ms=%d cutoff_ms=%d elapsed_ms=%d ttl_ms=%d",
		workspace, sessionID, account.Cause, account.SinceMs, account.CutoffMs, account.ElapsedMs, account.TTLMs)

	// THE STOP FIRST. A workspace with no live controller is already stopped,
	// and that is not a failure for this transition: the record still has to be
	// marked, or a never-brought-up hibernated workspace would accept prompts
	// through the ordinary bring-up path. Every OTHER stop failure — above all
	// ErrNotSettled — refuses the whole transition.
	if err := m.hibernate(workspace, sessionID, hibernationStopCause(account.Cause)); err != nil {
		if !errors.Is(err, ErrNoLiveSessionController) && !isNoLiveSessionForHibernate(err) {
			m.logf("session-controller: hibernation transition REFUSED ws=%q session=%s cause=%s error=%v — nothing was stopped and nothing was persisted, so the session is exactly as it was found",
				workspace, sessionID, account.Cause, err)
			return err
		}
		m.logf("session-controller: hibernation transition found no live shim ws=%q session=%s cause=%s — nothing to stop, so the transition continues to the durable mark",
			workspace, sessionID, account.Cause)
	}

	if err := m.cfg.Hibernations.HibernationChanged(sessionID, account); err != nil {
		// THE SHIM IS ALREADY DOWN and cannot be un-stopped. The honest report
		// is the loud one: the session is stopped, the record does not say so,
		// and the next prompt will bring it back up rather than meeting the
		// gate. That is the benign direction, and it is stated rather than
		// swallowed.
		m.logf("session-controller: hibernation DURABLE MARK FAILED ws=%q session=%s cause=%s error=%v — the shim is stopped but the record does not record the sleep, so the next prompt will revive the session implicitly instead of meeting the revival gate",
			workspace, sessionID, account.Cause, err)
		return err
	}
	m.logf("session-controller: hibernation transition COMPLETE ws=%q session=%s cause=%s since_ms=%d — the session is stopped, durably marked, and by construction outside the keep-alive loop",
		workspace, sessionID, account.Cause, account.SinceMs)
	return nil
}

// hibernationStopCause maps a hibernation cause onto the shim-stop vocabulary,
// so the process being killed is told the same story the record keeps.
func hibernationStopCause(cause string) StopCause {
	switch cause {
	case registry.HibernationCauseForced:
		return StopCauseHibernateForced()
	case registry.HibernationCauseCacheExpired:
		return StopCauseHibernateCacheExpired()
	default:
		return StopCauseHibernateIdleSweep()
	}
}

// isNoLiveSessionForHibernate reports the teardown's "nothing to stop" error,
// which hibernate() states as a plain error rather than a sentinel.
func isNoLiveSessionForHibernate(err error) bool {
	return err != nil && errors.Is(err, errNoLiveSessionToHibernate)
}

// errNoLiveSessionToHibernate is hibernate()'s typed "nothing to stop". It
// exists so the transition can tell that benign finding from a refusal it must
// propagate, without matching on message text.
var errNoLiveSessionToHibernate = errors.New("session-controller: no live session for the workspace to hibernate")

// claimHibernation takes the exclusive per-workspace hibernation claim and
// refuses a session that is already asleep.
//
// THIS IS THE SINGLE-TRANSITION INVARIANT. Two causes can genuinely arrive at
// once — the sweeper evaluating the idle cutoff while the user clicks hibernate
// — and without a claim both would run the teardown and both would write an
// account, leaving the record telling whichever story landed second. The claim
// is taken under the manager mutex, the same mutex every prompt submission
// takes, so it also serializes against a prompt racing the sleep.
func (m *Manager) claimHibernation(workspace, sessionID, cause string) (release func(), err error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.hibernating == nil {
		m.hibernating = map[string]bool{}
	}
	if m.hibernating[workspace] {
		return nil, fmt.Errorf("%w: workspace %q, cause %s", ErrHibernationInFlight, workspace, cause)
	}
	if detail, ok := m.cfg.Hibernations.HibernationOf(sessionID); ok && detail.Cause != "" {
		return nil, fmt.Errorf("%w: workspace %q session %s is asleep since %d for %s; the %s transition changes nothing",
			ErrAlreadyHibernated, workspace, sessionID, detail.SinceMs, detail.Cause, cause)
	}
	m.hibernating[workspace] = true
	return func() {
		m.mu.Lock()
		delete(m.hibernating, workspace)
		m.mu.Unlock()
	}, nil
}

// hibernatedLocked reports whether the session backing d is durably
// hibernated. Caller holds m.mu.
//
// It reads the RECORD rather than any live flag, because the question outlives
// this daemon: a rehydrated session that was asleep when the last daemon died
// must meet the gate on its first prompt, and no in-memory state survives to
// tell it so.
func (m *Manager) hibernatedLocked(sessionID string) (registry.HibernationDetail, bool) {
	if m.cfg.Hibernations == nil {
		return registry.HibernationDetail{}, false
	}
	detail, ok := m.cfg.Hibernations.HibernationOf(sessionID)
	if !ok || detail.Cause == "" {
		return registry.HibernationDetail{}, false
	}
	return detail, true
}

// clearHibernation retires a session's sleep. It is the revival path's ONE
// write, and it is deliberately the same call the transition uses with an empty
// detail, so "asleep" and "awake" are two values of one durable fact rather
// than two fields that could disagree.
func (m *Manager) clearHibernation(workspace, sessionID string) error {
	if m.cfg.Hibernations == nil {
		return nil
	}
	if err := m.cfg.Hibernations.HibernationChanged(sessionID, registry.HibernationDetail{}); err != nil {
		m.logf("session-controller: hibernation CLEAR FAILED ws=%q session=%s error=%v — the session is being brought up while its record still claims a sleep, so the gate would refuse the very prompts the revival was asked for",
			workspace, sessionID, err)
		return err
	}
	m.logf("session-controller: hibernation cleared ws=%q session=%s — the revival gate no longer stands", workspace, sessionID)
	return nil
}

// guardHibernation refuses any prompt aimed at a hibernated session.
//
// IT IS PLACED WHERE THE MERGE LEASE'S GATE IS, and for the same reason. Before
// this existed, ZERO of the thirteen prompt-submission paths asked about
// hibernation: every one funnels into forwardPrompt, and ensure() would quietly
// bring the shim back up on the way, so a hibernated session accepted prompts
// exactly like an awake one and the revival choice was never put to the user.
// Two placements close that:
//
//   - submitPromptAs asks BEFORE ensure(), so a refused prompt neither spawns a
//     shim nor lands on the queue;
//   - forwardPrompt asks again at the funnel EVERY path reaches, which is the
//     one a new caller cannot forget.
//
// THE REFUSAL IS TOTAL, including the keep-alive ping. A hibernated session is
// outside the keep-alive loop by construction, and a ping that reached here
// would be that construction having failed — so it is refused and said out loud
// rather than quietly allowed through as machinery.
//
// Non-user producers get the same loud error rather than an implicit revival:
// merge conflict resolution, a displaced turn's resume and a workspace-create's
// initial prompt all belong to somebody who is not looking at the revival gate,
// and silently waking a session on their behalf would spend the user's context
// budget on a decision they were never offered.
func (m *Manager) guardHibernation(workspace, requestID, origin string, who submitter) error {
	if m.cfg.Hibernations == nil {
		return nil
	}
	// THE ONE ADMISSION. A compact-first revival brings the session up while
	// the record still says hibernated — deliberately, so this gate keeps
	// refusing the user's prompts — and drives its `/compact` through here. It
	// is admitted by SUBMITTER rather than by a flag or a text match, so a
	// prompt that merely looks like a compaction cannot borrow the exemption.
	if who == submitterRevival {
		return nil
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return nil
	}
	detail, asleep := m.cfg.Hibernations.HibernationOf(sessionID)
	if !asleep || detail.Cause == "" {
		return nil
	}
	m.logf("session-controller: prompt REFUSED by the revival gate ws=%q session=%s request_id=%s origin=%q cause=%s since_ms=%d — the session is hibernated and no shim was spawned, no queue entry was made, and nothing was submitted; the user must choose a revival mode first",
		workspace, sessionID, requestID, origin, detail.Cause, detail.SinceMs)
	return fmt.Errorf("%w: workspace %q session %s has been asleep since %d (%s)",
		ErrHibernated, workspace, sessionID, detail.SinceMs, detail.Cause)
}

// keepAliveConfig is the resolved policy this manager runs. A Manager built
// without one (a focused harness) gets the shipped defaults rather than a zero
// Config, whose zero TTL would make every session read as cache-expired.
func (m *Manager) keepAliveConfig() keepalive.Config {
	if m.cfg.KeepAlive.CacheTTL <= 0 {
		return keepalive.DefaultConfig()
	}
	return m.cfg.KeepAlive
}
