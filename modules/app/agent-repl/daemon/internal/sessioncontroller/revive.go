package sessioncontroller

import (
	"context"
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

// ReviveSession brings a hibernated workspace back under the user's chosen
// mode. Synchronous, because the ack is the user's only report of whether their
// session came back.
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
	detail, asleep := m.cfg.Hibernations.HibernationOf(sessionID)
	if !asleep || detail.Cause == "" {
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
	compacted := m.armCompactionWait(d)
	m.logf("session-controller: revive ws=%q session=%s mode=compact_first — the session is up and STILL GATED; submitting %s before any prompt is accepted",
		workspace, sessionID, compactCommandText)

	// THE REVIVAL'S OWN COMPACTION IS THE ONE THING THE GATE LETS THROUGH. It
	// is submitted as submitterRevival, which guardHibernation admits precisely
	// so that the record can stay hibernated — and therefore keep gating the
	// user's prompts — while the compaction runs.
	if err := m.forwardPrompt(ctx, d, "revive-compact:"+sessionID, compactCommandText,
		"revive-compact:"+sessionID, "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, submitterRevival); err != nil {
		m.logf("session-controller: revive COMPACTION SUBMIT FAILED ws=%q session=%s error=%v — the session STAYS GATED; it was not left half-revived and accepting prompts",
			workspace, sessionID, err)
		return fmt.Errorf("session-controller: reviving session %s (ws %q): submitting the compaction: %w", sessionID, workspace, err)
	}

	select {
	case <-compacted:
		m.logf("session-controller: revive compaction LANDED ws=%q session=%s — releasing the gate", workspace, sessionID)
	case <-ctx.Done():
		m.logf("session-controller: revive compaction ABANDONED ws=%q session=%s error=%v — the session STAYS GATED",
			workspace, sessionID, ctx.Err())
		return fmt.Errorf("session-controller: reviving session %s (ws %q): waiting for the compaction: %w", sessionID, workspace, ctx.Err())
	case <-time.After(compactFirstBound):
		m.logf("session-controller: revive compaction TIMED OUT ws=%q session=%s bound=%s — the session STAYS GATED rather than limping into accepting prompts on a conversation that was never compacted",
			workspace, sessionID, compactFirstBound)
		return fmt.Errorf("session-controller: reviving session %s (ws %q): the compaction did not complete within %s; the session remains hibernated and can be revived again",
			sessionID, workspace, compactFirstBound)
	}

	// THE CLEAR IS THE LAST STEP, reached only on the completion signal. Every
	// other exit above returns with the record untouched, which is what makes
	// "a failed compaction leaves the session gated" structural rather than a
	// promise each error path has to keep.
	if err := m.clearHibernation(workspace, sessionID); err != nil {
		return err
	}
	m.logf("session-controller: revive COMPLETE ws=%q session=%s mode=compact_first", workspace, sessionID)
	return nil
}

// armCompactionWait installs the one-shot completion signal a compact-first
// revival waits on, and returns the channel it closes.
//
// Armed BEFORE the compaction is submitted, never after: a compaction that
// completed between the submit and the arm would close an axis nobody was
// listening to, and the revival would wait out its whole bound for an event
// that had already happened.
func (m *Manager) armCompactionWait(d *sessionController) <-chan struct{} {
	done := make(chan struct{})
	var once bool
	m.mu.Lock()
	d.consumer.onContextCompacted = func() {
		m.mu.Lock()
		defer m.mu.Unlock()
		if once {
			return
		}
		once = true
		close(done)
	}
	m.mu.Unlock()
	return done
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
