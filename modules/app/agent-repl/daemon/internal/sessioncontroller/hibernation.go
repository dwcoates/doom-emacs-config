package sessioncontroller

import (
	"errors"
	"fmt"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/registry"
)

// hibernation.go — THE ONE TRANSITION, and the gate it makes unavoidable.
//
// Three causes put a session to sleep: the idle cutoff, a cache that went cold
// before a ping could fire, and the user's own HibernateWorkspaceCmd. The
// cache-expired cause has two routes into it — the sweeper's time-since
// prediction and a keep-alive ping that came back cold, which measured the same
// thing instead of predicting it (keepalivecold.go) — and every route calls
// hibernateWithCause and nothing else. That is not tidiness — it is what
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

// ErrHibernationNoLongerIdle reports an AUTOMATIC hibernation whose elapsed no
// longer clears the threshold it was decided against, re-read at claim time.
//
// It is the sweeper's stale-snapshot refusal. A sweep evaluates a registry
// snapshot and can only act on it some time later; a session that started and
// finished a turn in that gap is genuinely active, and hibernating it would
// write a durable sleep — with the snapshot's account attached — over work that
// had just happened.
var ErrHibernationNoLongerIdle = errors.New("session-controller: the session is no longer idle enough for the automatic hibernation that was decided for it")

// ErrHibernationMergeLeaseHeld reports a hibernation transition refused because
// merge.Coordinator owns the workspace's shim.
//
// IT IS CHECKED IN THE TRANSITION, not only in the forced path that used to be
// the sole place it was asked. A merge holds the lease precisely because it is
// driving the session, and an automatic cause stopping that shim would kill
// conflict resolution mid-flight and leave a durable sleep the merge's own
// prompts would then be nacked by. The forced path keeps its earlier, friendlier
// nack; this is the one no route goes around.
var ErrHibernationMergeLeaseHeld = errors.New("session-controller: the workspace's session is held by a merge exclusivity lease and cannot be hibernated")

// ErrHibernationMergeQueued reports an AUTOMATIC hibernation refused because the
// workspace is waiting in the merge queue.
//
// A QUEUED WORKSPACE IS NOT A QUIET ONE. Both automatic causes read idleness off
// the clock and cannot see the difference between "nobody wants this workspace"
// and "this workspace is waiting its turn for the merge lock", and the second is
// PENDING WORK — the queue exists precisely because somebody asked for it. The
// cost of getting it wrong is not cosmetic: the workspace was hibernated while
// queued, and the conflict resolution that eventually ran against it re-ingested
// the whole conversation at the uncached rate.
//
// THE FORCED CAUSE IS NOT REFUSED HERE. A user hibernating a queued workspace has
// looked at it and decided; the merge lease's gate above outranks even that,
// because there the shim is genuinely being driven, but nothing is being driven
// here and the user's own decision is not a stale clock reading.
var ErrHibernationMergeQueued = errors.New("session-controller: the workspace is waiting in the merge queue and cannot be hibernated by an automatic cause")

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
	// LastTurnEndOf reads that instant back. It is what the hibernation claim
	// re-validates an AUTOMATIC cause's elapsed against, under the manager
	// mutex, so a session that started and finished work between the sweep's
	// snapshot and the claim cannot be put to sleep on the stale reading.
	LastTurnEndOf(sessionID string) (int64, bool)
}

// LegacyTurnEndStamper resolves — and, for a record that has none, stamps —
// the durable last-turn-end instant the keep-alive policy measures from.
//
// It is ONE method because there is one rule, and it lives outside this package
// because the instant a legacy record gets comes from the workspace's dated
// state history, which this package does not read. The idle sweeper and the
// acceptance/bring-up staleness check call the same implementation.
type LegacyTurnEndStamper interface {
	// StampLegacyTurnEnd reports the session's durable last-turn-end,
	// stamping a missing one from durable state first. A false return is a
	// session with no dated fact to measure — never a guess, and never now().
	StampLegacyTurnEnd(sessionID, workspace string) (int64, bool)
}

// hibernationEvidence says what KIND of claim an account makes, and therefore
// whether the claim may re-measure it against the clock.
//
// It is not a knob. It names the difference between the two ways this daemon
// can come to believe a prompt cache is gone, and only one of them is
// re-checkable.
type hibernationEvidence int

const (
	// evidencePredicted is a TIME-SINCE decision: the account says the session
	// has been quiet for longer than a threshold, which is a prediction about a
	// cache nobody observed. A prediction is re-measured at claim time, because
	// the reading it was taken from can have moved between the sweep and the
	// transition (revalidateElapsedLocked).
	evidencePredicted hibernationEvidence = iota
	// evidenceObserved is proof already in hand: a cache keep-alive ping paid a
	// full context re-ingest, which is the cache's absence MEASURED rather than
	// inferred from idle time (keepalivecold.go).
	//
	// Re-measuring it would refuse it every single time and for a reason that
	// has nothing to do with the question — the ping's own turn end has just
	// stamped the durable last-turn-end to now, so the fresh elapsed is ~0 —
	// which is the prediction overruling the observation that exists precisely
	// because the prediction was wrong.
	evidenceObserved
)

// HibernateWithCause is THE hibernation transition for a PREDICTED account: the
// idle sweeper's two time-since causes and the user's own forced sleep.
// account.Cause names why, and must be one of the registry's hibernation cause
// tokens.
//
// The workspace must be SETTLED: the underlying teardown refuses a live turn or
// an unseen vendor block with ErrNotSettled, which is what makes a teal tab over
// a working agent unreachable rather than merely rare.
func (m *Manager) HibernateWithCause(workspace string, account registry.HibernationDetail) error {
	return m.hibernateWithCause(workspace, account, evidencePredicted)
}

// hibernateWithCause is the transition itself. Every cause reaches it, and
// evidence decides only whether the claim re-measures the account's threshold.
func (m *Manager) hibernateWithCause(workspace string, account registry.HibernationDetail, evidence hibernationEvidence) error {
	if account.Cause == "" || !registry.ValidHibernationCause(account.Cause) {
		return fmt.Errorf("session-controller: refusing a hibernation transition for workspace %q with cause %q; a sleep the gate cannot explain is not one to take",
			workspace, account.Cause)
	}
	if m.cfg.Hibernations == nil {
		return fmt.Errorf("session-controller: refusing to hibernate workspace %q cause=%q: no hibernation registrar is wired, so the sleep could not be made durable and the next daemon would revive the session implicitly",
			workspace, account.Cause)
	}
	// THE MERGE LEASE OUTRANKS EVERY CAUSE. Asked here rather than in each
	// caller, so the automatic causes cannot reach the teardown by a route the
	// forced path's own check does not cover.
	if m.cfg.SSM.MergeLeaseHeld(workspace) {
		m.logf("session-controller: hibernation transition REFUSED ws=%q cause=%s — merge.Coordinator holds the exclusivity lease on this workspace's session; nothing was stopped and nothing was persisted",
			workspace, account.Cause)
		return fmt.Errorf("%w: workspace %q, cause %s", ErrHibernationMergeLeaseHeld, workspace, account.Cause)
	}
	// THE MERGE QUEUE OUTRANKS THE TWO AUTOMATIC CAUSES, and it is asked HERE
	// for the lease gate's reason: every route into a sleep funnels through this
	// one transition, so a gate placed here is one a new caller cannot forget.
	// The cache-cold arm (keepalivecold.go), the idle sweep, the keep-alive
	// policy's own hibernate arms and the acceptance-time stale-record check all
	// reach it, and each of them decides on nothing but a clock.
	if err := m.refuseAutomaticHibernationWhileQueued(workspace, account); err != nil {
		return err
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return fmt.Errorf("session-controller: workspace %q has no session to hibernate", workspace)
	}
	release, account, err := m.claimHibernation(workspace, sessionID, account, evidence)
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

// refuseAutomaticHibernationWhileQueued refuses the two clock-driven causes for
// a workspace resting on RENDER_STATE_MERGE_QUEUED.
//
// ONLY THE AUTOMATIC CAUSES ARE GATED. The idle cutoff and the expired cache are
// both readings of how long nothing has happened, and neither can tell "nobody
// wants this workspace" from "this workspace is queued behind another one's
// merge". A forced sleep is the user's own decision about a workspace they are
// looking at, and it is not a stale clock reading, so it passes.
//
// A READ FAILURE REFUSES THE TRANSITION rather than being logged past. This gate
// is the only thing standing between a queued workspace and a teardown, and a
// hibernation taken because the daemon could not find out whether it was queued
// is the exact outcome the gate exists to prevent. A workspace with NO resolved
// state is not queued — a queue membership is a state row, and there is none —
// so that answers no rather than refusing every hibernation on a stateless
// workspace.
func (m *Manager) refuseAutomaticHibernationWhileQueued(workspace string, account registry.HibernationDetail) error {
	switch account.Cause {
	case registry.HibernationCauseIdleCutoff, registry.HibernationCauseCacheExpired:
	default:
		return nil
	}
	state, found, err := m.cfg.SSM.Current(workspace)
	if err != nil {
		m.logf("session-controller: hibernation transition REFUSED ws=%q cause=%s error=%v — the workspace's resolved state could not be read, so whether it is waiting in the merge queue is unknown; nothing was stopped and nothing was persisted",
			workspace, account.Cause, err)
		return fmt.Errorf("session-controller: reading the resolved state of workspace %q before an automatic %s hibernation: %w",
			workspace, account.Cause, err)
	}
	if !found || state.GetState() != frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED {
		return nil
	}
	m.logf("session-controller: hibernation transition REFUSED ws=%q cause=%s state=%s — the workspace is WAITING IN THE MERGE QUEUE, which is pending work rather than quiet; hibernating it here is what made a later conflict resolution re-ingest the whole conversation at the uncached rate. Nothing was stopped and nothing was persisted",
		workspace, account.Cause, state.GetState())
	return fmt.Errorf("%w: workspace %q, cause %s", ErrHibernationMergeQueued, workspace, account.Cause)
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
// THE ELAPSED IS RE-READ HERE TOO, for the two AUTOMATIC causes. The sweeper
// decides on a registry snapshot and the claim is taken later; re-reading the
// durable last-turn-end inside the claim is what makes "hibernated a session
// that was working" unrepresentable rather than merely improbable, and the
// account the transition persists is then computed from the FRESH figure rather
// than from the snapshot's.
//
// account is the caller's decision; the returned detail is the one to persist.
func (m *Manager) claimHibernation(workspace, sessionID string, account registry.HibernationDetail, evidence hibernationEvidence) (release func(), fresh registry.HibernationDetail, err error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.hibernating == nil {
		m.hibernating = map[string]bool{}
	}
	if m.hibernating[workspace] {
		return nil, account, fmt.Errorf("%w: workspace %q, cause %s", ErrHibernationInFlight, workspace, account.Cause)
	}
	if detail, ok := m.cfg.Hibernations.HibernationOf(sessionID); ok && detail.Cause != "" {
		return nil, account, fmt.Errorf("%w: workspace %q session %s is asleep since %d for %s; the %s transition changes nothing",
			ErrAlreadyHibernated, workspace, sessionID, detail.SinceMs, detail.Cause, account.Cause)
	}
	fresh, err = m.revalidateElapsedLocked(workspace, sessionID, account, evidence)
	if err != nil {
		return nil, account, err
	}
	m.hibernating[workspace] = true
	return func() {
		m.mu.Lock()
		delete(m.hibernating, workspace)
		m.mu.Unlock()
	}, fresh, nil
}

// revalidateElapsedLocked re-measures an automatic cause's idleness against the
// durable last-turn-end and returns the account to persist. Caller holds m.mu.
//
// A FORCED hibernation is not re-validated and must not be: the user asked for
// it, and idleness was never its premise.
//
// The threshold re-checked is the one the DECISION carried — CutoffMs for the
// idle cutoff, TTLMs for an expired cache — rather than the manager's own
// configuration, so the claim re-validates exactly what the caller acted on
// instead of substituting a second opinion for it. An account carrying no
// threshold, or a session with no durable instant to measure from, is left
// alone: this gate refuses a stale reading, and it has no business inventing a
// refusal where there is nothing to read.
//
// AN OBSERVED ACCOUNT IS NOT RE-VALIDATED AT ALL, for the same reason a forced
// one is not: idleness was never its premise. A cold keep-alive ping MEASURED
// the cache's absence, and the ping's own turn end has just moved the durable
// instant this gate would measure against to now — so re-measuring would refuse
// every cold-ping hibernation there will ever be, on the strength of the very
// prediction the measurement exists to overrule.
func (m *Manager) revalidateElapsedLocked(workspace, sessionID string, account registry.HibernationDetail, evidence hibernationEvidence) (registry.HibernationDetail, error) {
	if evidence == evidenceObserved {
		return account, nil
	}
	threshold := automaticHibernationThreshold(account)
	if threshold <= 0 {
		return account, nil
	}
	lastEndMs, ok := m.cfg.Hibernations.LastTurnEndOf(sessionID)
	if !ok || lastEndMs <= 0 {
		return account, nil
	}
	elapsedMs := m.now() - lastEndMs
	if elapsedMs < threshold {
		m.logf("session-controller: hibernation transition REFUSED ws=%q session=%s cause=%s snapshot_elapsed_ms=%d fresh_elapsed_ms=%d threshold_ms=%d — the session's most recent turn ended after the sweep decided to hibernate it, so the decision was taken on a stale reading; nothing was stopped and nothing was persisted",
			workspace, sessionID, account.Cause, account.ElapsedMs, elapsedMs, threshold)
		return account, fmt.Errorf("%w: workspace %q session %s has been quiet for %dms against a %dms threshold (the sweep measured %dms)",
			ErrHibernationNoLongerIdle, workspace, sessionID, elapsedMs, threshold, account.ElapsedMs)
	}
	account.ElapsedMs = elapsedMs
	return account, nil
}

// automaticHibernationThreshold reports the elapsed a cause's account was
// decided against, or 0 for a cause that is not a time-since decision at all.
func automaticHibernationThreshold(account registry.HibernationDetail) int64 {
	switch account.Cause {
	case registry.HibernationCauseIdleCutoff:
		return account.CutoffMs
	case registry.HibernationCauseCacheExpired:
		return account.TTLMs
	default:
		return 0
	}
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
	// THE REVIVAL IS THE POLICY'S NEW MEASURING POINT, and stamping it is what
	// stops the revival from being undone by the very thresholds it answered.
	// The instant the record kept is the turn end that PRECEDED the sleep —
	// hours or days old, since that age is why the session slept — so a session
	// woken and left on that instant reads as stale to every evaluator the
	// moment it is awake: the idle sweeper's next tick would hibernate it
	// straight back, and the acceptance and bring-up checks would refuse the
	// revival's own bring-up outright. The user re-engaging is the freshest
	// dated fact about this session, and it is written through the same
	// registrar hook every other turn boundary uses rather than a second field.
	if nowMs := m.now(); nowMs > 0 {
		m.cfg.Hibernations.TurnEndObserved(sessionID, nowMs)
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
		// THE STALE-RECORD CLOSURE. A record that never hibernated can still
		// be long past the policy's thresholds — a session closed awake days
		// ago, restored at boot — and until this line the gate read only the
		// flag, so such a session answered prompts as though it were warm for
		// however long it took the next sweep to notice. The decision is taken
		// HERE, at acceptance, so the answer does not depend on the schedule.
		taken, took := m.hibernateIfStale(workspace, sessionID)
		if !took {
			return nil
		}
		detail = taken
	}
	m.logf("session-controller: prompt REFUSED by the revival gate ws=%q session=%s request_id=%s origin=%q cause=%s since_ms=%d — the session is hibernated and no shim was spawned, no queue entry was made, and nothing was submitted; the user must choose a revival mode first",
		workspace, sessionID, requestID, origin, detail.Cause, detail.SinceMs)
	return fmt.Errorf("%w: workspace %q session %s has been asleep since %d (%s)",
		ErrHibernated, workspace, sessionID, detail.SinceMs, detail.Cause)
}

// hibernateIfStale takes the hibernation a record has ALREADY EARNED, at the
// moment the session is asked for, and reports the account it persisted.
//
// WHY THE DECISION LIVES AT ACCEPTANCE AND BRING-UP RATHER THAN ON A SCHEDULE.
// The idle sweeper's first tick is a whole interval after boot, and it is the
// only thing that used to evaluate the policy. A session that was closed AWAKE
// days ago therefore came back ungated: its record says hibernated=false, so
// the gate passed it, and the bring-up spawned a shim for it — and a prompt in
// that window was forwarded as a cold, full-context turn on the user's budget.
// The window was a scheduling artifact, so the fix is to stop asking on a
// schedule: every route into a session now evaluates the same policy against
// the same durable instant before it can be used. The sweeper still runs, and
// still hibernates the sessions nobody asks for; it is no longer the only
// evaluator, and nothing about its arithmetic is duplicated here — this reads
// the same keepalive.Config and routes through the same one transition.
//
// ONLY THE HIBERNATE ARMS ARE ACTED ON. ActionPing belongs to the sweeper: a
// prompt arriving is the warming, and submitting a ping in front of it would
// spend a turn to keep alive a cache the very next line is about to use. The
// cutoff-versus-expiry precedence is keepalive.Evaluate's, untouched.
//
// A not-stale session, an already-asleep one, and a session with no durable
// instant to measure from are all no-ops. The caller holds NO manager mutex:
// HibernateWithCause takes the per-workspace claim itself and re-reads the
// elapsed under it, so this function's reading being a moment old is already
// covered by the transition's own fresh check.
func (m *Manager) hibernateIfStale(workspace, sessionID string) (registry.HibernationDetail, bool) {
	if m.cfg.Hibernations == nil {
		return registry.HibernationDetail{}, false
	}
	if detail, asleep := m.cfg.Hibernations.HibernationOf(sessionID); asleep && detail.Cause != "" {
		return registry.HibernationDetail{}, false
	}
	lastEndMs, ok := m.durableLastTurnEnd(sessionID, workspace)
	if !ok || lastEndMs <= 0 {
		return registry.HibernationDetail{}, false
	}
	cfg := m.keepAliveConfig()
	nowMs := m.now()
	decision := cfg.Evaluate(nowMs, lastEndMs)
	if decision.Action != keepalive.ActionHibernate {
		return registry.HibernationDetail{}, false
	}
	detail := registry.HibernationDetail{
		Cause:     decision.Cause,
		SinceMs:   nowMs,
		ElapsedMs: decision.ElapsedMs,
	}
	switch decision.Cause {
	case keepalive.CauseIdleCutoff:
		detail.CutoffMs = int64(cfg.IdleCutoff / time.Millisecond)
	case keepalive.CauseCacheExpired:
		detail.TTLMs = int64(cfg.CacheTTL / time.Millisecond)
	}
	m.logf("session-controller: STALE RECORD hibernating on demand ws=%q session=%s cause=%s elapsed_ms=%d last_turn_end_ms=%d — the session was found past the keep-alive policy's threshold by the route that asked for it rather than by a sweep, so it meets the revival gate now instead of one sweep interval from now",
		workspace, sessionID, decision.Cause, decision.ElapsedMs, lastEndMs)
	if err := m.HibernateWithCause(workspace, detail); err != nil {
		// EVERY refusal here is the transition's own, and each one means the
		// session stays awake: a live turn (ErrNotSettled), a merge holding the
		// lease, another cause winning the claim, or the claim's fresh read
		// finding work that happened since. None is swallowed, and none is
		// converted into a refusal of the caller — the record still says awake,
		// so the caller proceeds exactly as it did before this check existed.
		m.logf("session-controller: stale-record hibernation NOT TAKEN ws=%q session=%s cause=%s error=%v — the session stays awake and the caller proceeds; the sweeper will re-evaluate it on its own schedule",
			workspace, sessionID, decision.Cause, err)
		return registry.HibernationDetail{}, false
	}
	return detail, true
}

// durableLastTurnEnd reads the instant every keep-alive decision measures from,
// stamping a legacy record that has none through the SAME rule the sweeper
// applies (server.stampLegacyTurnEnd, reached through cfg.LegacyTurnEnds).
//
// THE STAMPING POLICY IS NOT RE-EXPRESSED HERE. A second copy would be a second
// answer to "when was this session last active", and the two would drift into
// one route hibernating a session the other considers undated. A Manager wired
// without the seam simply leaves such a session to the sweeper, and says so.
func (m *Manager) durableLastTurnEnd(sessionID, workspace string) (int64, bool) {
	if lastEndMs, ok := m.cfg.Hibernations.LastTurnEndOf(sessionID); ok && lastEndMs > 0 {
		return lastEndMs, true
	}
	if m.cfg.LegacyTurnEnds == nil {
		m.logf("session-controller: session %s (ws %q) has no durable last_turn_end_ms and no legacy stamper is wired — the staleness check at acceptance and bring-up cannot measure it, and it stays with the idle sweeper alone",
			sessionID, workspace)
		return 0, false
	}
	return m.cfg.LegacyTurnEnds.StampLegacyTurnEnd(sessionID, workspace)
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
