package sessioncontroller

import (
	"context"
	"errors"

	"claude-repld/internal/session"
)

// keepaliveresidue.go — THE REWIND AS A DEBT, AND THE ONE PLACE IT IS SETTLED.
//
// A keep-alive ping is a real vendor turn. It writes a user record and an
// assistant record into the CLI's transcript, and everything the daemon does to
// keep it out of the CONVERSATION — the origin, the exclusion windows, the
// withheld receipt — is about what the USER sees. None of it touches what the
// MODEL sees: the next turn submitted on that transcript is answered with the
// ping's own text in context, and the agent occasionally says so.
//
// The transcript rewind (rewind.go) is the only thing that removes them, and it
// used to run in exactly ONE situation: a real prompt had been HELD behind the
// ping and the ping's turn end released it. A ping that finished with nothing
// waiting left its turns standing, and every later path submitted straight on
// top of them —
//
//   - a user prompt typed a minute after the ping finished,
//   - a revival of a hibernated session whose transcript tail is pings,
//   - a hard restart resuming that same conversation,
//   - a merge coordinator taking the shim for a resolution turn.
//
// So the ping's end now RECORDS A DEBT (Manager.keepAliveResidue) and every one
// of those paths settles it first. The debt is discharged by the SAME rewind the
// held-prompt path runs — there is one rewind implementation, one refusal
// semantics, and one set of degradation rules — and this file is only the ledger
// and the entry point.
//
// THE REFUSAL SEMANTICS ARE UNCHANGED AND DELIBERATE. No vendor transcript, an
// unreadable one, an interleaved sidechain, a live turn: every one of them
// DEGRADES to "proceed WITHOUT rewinding", loudly. The user's prompt is worth
// more than a clean transcript, and nothing here ever invents a transcript it
// could not read.

// noteKeepAliveResidueLocked records that pingTurnID's turns are standing in the
// workspace's transcript. Caller holds m.mu.
//
// It is taken at the ping's own turn end, in the same acquisition that clears
// the ping's claim, so there is no instant in which the ping is over and nothing
// says its turns are still there.
func (m *Manager) noteKeepAliveResidueLocked(workspace, pingTurnID string) {
	if pingTurnID == "" {
		return
	}
	if m.keepAliveResidue == nil {
		m.keepAliveResidue = map[string][]string{}
	}
	for _, id := range m.keepAliveResidue[workspace] {
		if id == pingTurnID {
			return
		}
	}
	m.keepAliveResidue[workspace] = append(m.keepAliveResidue[workspace], pingTurnID)
}

// clearKeepAliveResidueLocked discharges the workspace's whole debt. Caller
// holds m.mu.
//
// IT CLEARS EVERYTHING, NOT ONLY THE TURNS THE CALLER NAMED. The cut is decided
// from the transcript's TRAILING RUN of droppable turns (session.PlanRewind), so
// a rewind that succeeded removed every ping standing at the tail whether or not
// this ledger had a row for it — and a `no rewind needed` verdict is the file
// itself stating that no ping is standing there at all. Keeping rows after
// either outcome would re-run the rewind forever against a clean transcript.
func (m *Manager) clearKeepAliveResidueLocked(workspace string) {
	delete(m.keepAliveResidue, workspace)
}

// keepAliveResidueLocked reports the turns owed a rewind. Caller holds m.mu.
func (m *Manager) keepAliveResidueLocked(workspace string) []string {
	return m.keepAliveResidue[workspace]
}

// KeepAliveResidue reports the ping turns still standing in a workspace's
// transcript. It exists for the tests that assert the debt is recorded and
// discharged, which is the only externally checkable statement of the
// invariant this file maintains.
func (m *Manager) KeepAliveResidue(workspace string) []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	return append([]string(nil), m.keepAliveResidue[workspace]...)
}

// settleKeepAliveResidue rewinds any completed keep-alive turns out of the
// workspace's transcript before the caller submits or resumes, and reports
// whether the vendor context is now free of them.
//
// EVERY USER-FACING SUBMISSION AND EVERY SESSION RESUME CALLS THIS, which is
// what turns "the rewind happens when a prompt was held" into the invariant the
// feature actually needs: the model's context begins at the last real message
// before the most recent ping run.
//
// IT MUST NOT RUN ON THE SHIM READ-LOOP GOROUTINE. The rewind stops and respawns
// the shim, and the bring-up waits on a handshake only that loop can deliver.
//
// A ping IN FLIGHT, or a rewind already running, is left alone: that path owns
// the aftermath and holds the queue behind it (keepalivesubmit.go), and a second
// rewind racing it would stop the shim the first one is bringing up.
//
// The return value is advisory. A false is DEGRADED, never fatal: the caller
// proceeds, the ping stays visible, and the log line says so.
func (m *Manager) settleKeepAliveResidue(ctx context.Context, workspace, reason string) bool {
	m.mu.Lock()
	owed := m.keepAliveResidueLocked(workspace)
	if len(owed) == 0 {
		m.mu.Unlock()
		return true
	}
	d, live := m.byWS[workspace]
	if !live {
		m.mu.Unlock()
		m.logf("session-controller: keep-alive residue NOT SETTLED ws=%q reason=%s owed=%d — the workspace has no live session controller, so there is no transcript identity to rewind against; the debt stands and the next path that finds a live session settles it",
			workspace, reason, len(owed))
		return false
	}
	if held := m.keepAliveHoldTurnLocked(d); held != "" {
		m.mu.Unlock()
		m.logf("session-controller: keep-alive residue DEFERRED ws=%q reason=%s owed=%d keep_alive_turn=%s — a ping or its own rewind already owns this workspace's aftermath, and a second rewind would stop the shim the first one is bringing up",
			workspace, reason, len(owed), held)
		return false
	}
	// THE REWIND'S CLAIM IS TAKEN HERE, under the same acquisition that read the
	// debt, and for the claim's original reason: from this instant until the
	// respawned session is up, a prompt admitted would start a turn this rewind
	// is about to SIGTERM and truncate out of the transcript.
	pingTurnID := owed[len(owed)-1]
	m.claimKeepAliveRewindLocked(workspace, pingTurnID)
	sessionID := d.sessionID
	owed = append([]string(nil), owed...)
	// THE QUEUE IS TAKEN BEFORE THE STOP, exactly as the held-prompt aftermath
	// takes it: the dying client's exit tail drains it unconditionally and
	// persists nil over the durable record of what is still owed.
	held := m.takeQueueForRewindLocked(d)
	m.mu.Unlock()

	m.logf("session-controller: keep-alive residue SETTLING ws=%q session=%s reason=%s owed=%d turns=%v — completed keep-alive turns are standing at the transcript tail, and they are rewound out BEFORE this path puts anything on top of them",
		workspace, sessionID, reason, len(owed), owed)

	rewound := false
	if _, err := m.rewindKeepAliveTurns(ctx, workspace, sessionID, owed); err != nil {
		if errors.Is(err, session.ErrNoRewindNeeded) {
			// THE TRANSCRIPT ITSELF SAYS THE DEBT IS PAID. Its tail holds no
			// droppable turn, so nothing is standing between this path and the
			// last real message — which is precisely the invariant, reached
			// without a cut.
			rewound = true
			m.logf("session-controller: keep-alive residue ALREADY CLEAR ws=%q session=%s reason=%s — the transcript tail holds no keep-alive turns, so the debt is discharged without a cut",
				workspace, sessionID, reason)
		} else {
			m.warnf("session-controller: keep-alive residue rewind DEGRADED ws=%q session=%s reason=%s owed=%d error=%v — this path proceeds WITHOUT rewinding, so the keep-alive turns stay in the model's context; correctness over cleanliness, and the debt is kept so a later path can retry it",
				workspace, sessionID, reason, len(owed), err)
		}
	} else {
		rewound = true
	}

	m.mu.Lock()
	m.releaseKeepAliveRewindLocked(workspace, pingTurnID)
	if rewound {
		m.clearKeepAliveResidueLocked(workspace)
	}
	// THE HELD ENTRIES REJOIN A REAL QUEUE before anything else touches them,
	// and they rejoin the LIVE controller when the rewind replaced the one this
	// call started against. Delivering onto the retired controller would submit
	// through a client whose connection is gone.
	dst := d
	if live, ok := m.byWS[workspace]; ok {
		dst = live
	}
	m.returnQueueFromRewindLocked(dst, held)
	view, recs := m.publishQueueLocked(dst)
	dstSessionID := dst.sessionID
	m.mu.Unlock()

	m.publish(dstSessionID, view, recs)
	m.noteDrainActivity()
	m.logf("session-controller: keep-alive residue SETTLED ws=%q session=%s reason=%s rewound=%v requeued=%d",
		workspace, dstSessionID, reason, rewound, len(held))
	return rewound
}
