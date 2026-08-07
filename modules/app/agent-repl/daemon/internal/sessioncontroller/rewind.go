package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/google/uuid"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/session"
)

// rewind.go — THE DAEMON-ORCHESTRATED CONVERSATION REWIND.
//
// WHY THE DAEMON AND NOT THE SHIM. The shim cannot do this. It cannot rebuild
// its own SDK query, and it does not know where its transcript lives — it is
// handed a session id and a resume target, never a path. The daemon knows the
// config root, the workspace, and therefore the exact file; and the daemon is
// the only party that can stop a process, rewrite a registry record, and start
// a replacement as one sequence.
//
// THE SEQUENCE, and why it is in this order:
//
//  1. HOLD the real prompt (already done by the queue's keep-alive hold).
//  2. STOP the shim. The CLI holds the transcript open, and a copy taken from
//     underneath a live writer could catch a half-written line.
//  3. COPY-AND-TRUNCATE to a new uuid. NON-DESTRUCTIVE: the original is
//     untouched, so everything up to this point is free to fail.
//  4. FLIP the registry to the new uuid in ONE Update, which also resets the
//     store cursors AND records the lineage — AdoptVendorSessionID's precedent,
//     and for its reason: the rotation retires the old seq space, and a split
//     write would let the hydrate-from-checkpoint step undo the reset before
//     the new uuid landed.
//  5. SPAWN, resuming the new uuid, carrying the lineage argv off the record
//     and clearing it in the write that records the spawn.
//  6. DELIVER the held prompt.
//
// CRASH SAFETY FALLS OUT OF STEP 4 BEING THE ONLY DESTRUCTIVE ONE, AND BEING
// ONE WRITE. A daemon that dies BEFORE the flip leaves an orphaned copy nobody
// reads and a record still naming the original, so the next real prompt simply
// re-triggers the whole thing. A daemon that dies AFTER it leaves a record that
// names the truncated conversation AND carries the lineage that describes it,
// so the next ensure() of that session spawns with the rewind argv and the
// SessionRewound is emitted late rather than never. The rewind is idempotent by
// being re-runnable, and its one durable step is recoverable by being one
// write.
//
// AND IT IS ALWAYS OPTIONAL. Every failure short of the flip degrades to
// "submit WITHOUT rewinding": the user's prompt is worth more than a clean
// transcript, and a visible keep-alive turn is a cosmetic problem where a
// dropped prompt is not.

// RewindLineage is the frozen argv contract between this daemon and the shim.
// The shim accepts these three as --rewound-from, --rewind-retained-leaf and
// --rewind-dropped-turns, and emits the durable SessionRewound from them.
type RewindLineage struct {
	// PreviousVendorSessionID is the transcript that was truncated — the seq
	// space this rewind retires.
	PreviousVendorSessionID string
	// RetainedLeafUUID is the last record kept: the final record of the last
	// real turn.
	RetainedLeafUUID string
	// DroppedTurnIDs is the comma-separated turn_id list, in submission order.
	// NEVER EMPTY: the shim rejects an empty list, and a rewind that dropped
	// nothing is not a rewind.
	DroppedTurnIDs string
}

// VendorSessionAdopter performs the ATOMIC registry flip: adopt the new vendor
// uuid, reset the store cursors, and record the lineage the next spawn must
// announce — all in ONE Update.
//
// THE LINEAGE TRAVELS WITH THE FLIP because the flip is what it accounts for.
// An in-memory arm beside a durable flip meant a daemon that died in between
// left a record naming a truncated conversation with nothing left to say it had
// been truncated: no SessionRewound was ever emitted, and no recovery existed.
// One write makes both facts land together or neither land at all.
type VendorSessionAdopter interface {
	AdoptRewoundVendorSessionID(sessionID, claudeSessionID string, lineage RewindLineage) (rotated bool, previous string, adopted bool)
}

// ErrRewindSkipped reports a rewind that did not happen and did not need to.
var ErrRewindSkipped = errors.New("session-controller: no transcript rewind was needed")

// keepAliveDropText is the droppable-text set the cut is decided against. The
// ping's text is the ONE thing the transcript carries that identifies a
// keep-alive turn: transcript records carry no prompt origin and no turn id, so
// the literal is the only join available from the file alone.
func keepAliveDropText() map[string]bool {
	return map[string]bool{strings.TrimSpace(keepalive.PingText): true}
}

// rewindKeepAliveTurns performs the whole sequence for one workspace and
// reports the new vendor session id.
//
// It must NOT run on the shim read-loop goroutine: it stops and respawns the
// shim, and the bring-up waits on a handshake only that loop can deliver.
func (m *Manager) rewindKeepAliveTurns(ctx context.Context, workspace, sessionID string, droppedTurnIDs []string) (newVendorSessionID string, err error) {
	if m.cfg.VendorSessions == nil {
		return "", fmt.Errorf("session-controller: refusing to rewind ws=%q session=%s: no vendor-session adopter is wired, so the registry flip and the lineage the shim must announce could not be made one write",
			workspace, sessionID)
	}
	if len(droppedTurnIDs) == 0 {
		return "", fmt.Errorf("session-controller: refusing to rewind ws=%q session=%s with no dropped turn ids; the shim rejects an empty list and a rewind that dropped nothing is not a rewind",
			workspace, sessionID)
	}
	configDir, vendorSessionID, ok := m.rewindIdentity(sessionID)
	if !ok {
		return "", fmt.Errorf("session-controller: cannot rewind ws=%q session=%s: the session names no vendor conversation to truncate", workspace, sessionID)
	}
	transcriptPath := session.TranscriptPath(session.ClaudeConfigDir(configDir), workspace, vendorSessionID)

	// THE PLAN IS DECIDED BEFORE THE SHIM IS STOPPED. A cut that turns out to
	// be unsafe must cost nothing, and stopping first would mean paying a
	// bring-up to discover the transcript could not be rewound.
	records, err := session.LoadTranscript(transcriptPath)
	if err != nil {
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: reading %s: %w", workspace, sessionID, transcriptPath, err)
	}
	plan, err := session.PlanRewind(records, keepAliveDropText())
	if err != nil {
		return "", err
	}
	m.logf("session-controller: rewind PLANNED ws=%q session=%s vendor_session=%s transcript=%s keep_through=%d retained_leaf=%s dropping=%d turn(s)",
		workspace, sessionID, vendorSessionID, transcriptPath, plan.KeepThrough, plan.RetainedLeafUUID, len(plan.DroppedTexts))

	// THE STOP IS GUARDED BY THE SAME SETTLED LEASE HIBERNATION USES, and for
	// the same reason with one addition. The keep-alive hold is what should
	// make a live turn impossible here — no prompt is admitted from the ping's
	// submit until this rewind's tail releases the workspace's rewind claim —
	// so a turn found running at this point is evidence that the hold has a
	// hole in it, not an ordinary condition. It therefore REFUSES rather than
	// stops: the plan above was decided on a snapshot taken before that turn
	// existed, so proceeding would SIGTERM an acked user turn and truncate it
	// out of the transcript with nothing told to the user.
	//
	// The refusal travels the ErrNotSettled path back to releaseKeepAliveHolds,
	// which is the established DEGRADED channel: the held prompt is submitted
	// WITHOUT the rewind, so the ping stays visible and nothing is lost.
	//
	// The lease is released as soon as the shim is down rather than deferred to
	// the end of the sequence: the respawn below takes a controller
	// registration, which the SSM refuses while a hibernation lease is held,
	// and there is no shim left to start a turn in the interval anyway.
	releaseSettled, err := m.acquireSettledHibernationLease(workspace)
	if err != nil {
		m.logf("session-controller: rewind REFUSED ws=%q session=%s vendor_session=%s error=%v — a turn is live where the keep-alive hold promised none could be, and stopping the shim now would kill an acked user turn and cut it out of the transcript",
			workspace, sessionID, vendorSessionID, err)
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: the workspace is not settled: %w", workspace, sessionID, err)
	}

	// STOP THE SHIM. The CLI holds the transcript open; copying from under a
	// live writer risks a half-written trailing line.
	stopErr := m.stopSessionController(workspace, sessionID, StopCauseHibernateIdleSweep())
	releaseSettled()
	if stopErr != nil {
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: stopping the shim: %w", workspace, sessionID, stopErr)
	}

	newVendorSessionID = uuid.NewString()
	destDir := session.ProjectDir(session.ClaudeConfigDir(configDir), workspace)
	dest, err := session.WriteRewound(records, plan, newVendorSessionID, destDir)
	if err != nil {
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: writing the truncated copy: %w", workspace, sessionID, err)
	}
	m.logf("session-controller: rewind COPY WRITTEN ws=%q session=%s from=%s to=%s path=%s — the original is untouched, so a crash before the registry flip simply re-triggers this rewind",
		workspace, sessionID, vendorSessionID, newVendorSessionID, dest)

	// THE FLIP: ONE Update that adopts the new uuid, resets the store cursors,
	// AND records the lineage the next spawn must announce. Splitting the
	// cursors out would let the registry's hydrate-from-checkpoint step undo the
	// reset before the new uuid was recorded (AdoptVendorSessionID); splitting
	// the lineage out would let a crash leave a truncated conversation with
	// nothing left to say it had been truncated.
	//
	// A REFUSED FLIP THEREFORE ARMS NOTHING. The original conversation is still
	// the record's, and there is no lineage left behind to ride some later,
	// unrelated spawn.
	lineage := RewindLineage{
		PreviousVendorSessionID: vendorSessionID,
		RetainedLeafUUID:        plan.RetainedLeafUUID,
		DroppedTurnIDs:          strings.Join(droppedTurnIDs, ","),
	}
	rotated, previous, adopted := m.cfg.VendorSessions.AdoptRewoundVendorSessionID(sessionID, newVendorSessionID, lineage)
	if !adopted {
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: the registry refused to adopt the rewound vendor session %s (previous %s); the original conversation is intact and still the record's, and no lineage was armed",
			workspace, sessionID, newVendorSessionID, previous)
	}
	m.logf("session-controller: rewind REGISTRY FLIPPED ws=%q session=%s %s -> %s rotated=%v — the old seq space is retired and the store cursors reset in the same write",
		workspace, sessionID, previous, newVendorSessionID, rotated)

	if _, err := m.ensure(ctx, workspace); err != nil {
		return newVendorSessionID, fmt.Errorf("session-controller: rewind ws=%q session=%s: bringing the rewound conversation back up: %w", workspace, sessionID, err)
	}
	m.logf("session-controller: rewind COMPLETE ws=%q session=%s vendor_session=%s dropped_turns=%s",
		workspace, sessionID, newVendorSessionID, lineage.DroppedTurnIDs)
	return newVendorSessionID, nil
}

// releaseKeepAliveHolds is the ping's whole aftermath: rewind the keep-alive
// turns out of the transcript, release the holds, and deliver the prompt that
// was waiting. Runs on its own goroutine (it bounces the shim).
//
// THE REWIND IS ATTEMPTED FIRST AND IS ALLOWED TO FAIL. Every refusal — an
// interleaved sidechain, an unreadable transcript, a store that will not take
// the flip — degrades to submitting WITHOUT the rewind, loudly. The user's
// prompt is worth more than a clean transcript: a visible ping in the history
// is cosmetic, and a prompt withheld because the daemon could not tidy up is
// not. The degradation is reported rather than absorbed, so a rewind that has
// silently stopped working is findable.
func (m *Manager) releaseKeepAliveHolds(d *sessionController, pingTurnID string, heldIDs []string) {
	workspace, sessionID := d.workspace, d.sessionID
	m.logf("session-controller: keep-alive aftermath BEGIN ws=%q session=%s turn_id=%s held=%d — rewinding the ping out of the transcript before the held prompt is submitted",
		workspace, sessionID, pingTurnID, len(heldIDs))

	// THE QUEUE IS TAKEN FIRST, before anything can stop the shim this queue
	// belongs to. From here to the re-park below, these entries have exactly
	// one owner, so the exit tail the rewind's stop provokes has nothing to
	// drop and nothing to persist nil over.
	owned := m.takeQueueForRewind(d)

	rewound := false
	if _, err := m.rewindKeepAliveTurns(m.rootCtx, workspace, sessionID, []string{pingTurnID}); err != nil {
		if errors.Is(err, session.ErrNoRewindNeeded) {
			m.logf("session-controller: keep-alive rewind SKIPPED ws=%q session=%s turn_id=%s — the transcript tail holds no keep-alive turns to drop",
				workspace, sessionID, pingTurnID)
		} else {
			m.warnf("session-controller: keep-alive rewind DEGRADED ws=%q session=%s turn_id=%s error=%v — the held prompt is submitted WITHOUT rewinding, so the keep-alive turn stays visible in the conversation; correctness over cleanliness",
				workspace, sessionID, pingTurnID, err)
		}
	} else {
		rewound = true
	}

	// THE LIVE CONTROLLER IS RE-RESOLVED. A successful rewind replaced the one
	// this function was handed, and delivering onto the retired controller
	// would submit through a client whose connection is gone.
	m.mu.Lock()
	// THE REWIND'S CLAIM IS RELEASED HERE and nowhere else. It was taken at the
	// ping's turn end (queue.go) as the continuation of the ping's own hold, so
	// this — the instant the respawned session is up and the held prompts are
	// about to be delivered — is the first moment a fresh prompt may start a
	// turn without the rewind cutting it away. It is released on EVERY exit,
	// including the abandoned one: a claim that outlived its rewind would hold
	// every later prompt on the workspace forever.
	m.releaseKeepAliveRewindLocked(workspace, pingTurnID)
	live, ok := m.byWS[workspace]
	if !ok {
		// NOTHING IS DISCARDED HERE EITHER. The workspace has no controller to
		// deliver onto, so the entries go back to the retired one — which keeps
		// them renderable and, crucially, persists them, so the record still
		// names prompts the user typed and the daemon has not delivered.
		m.returnQueueFromRewindLocked(d, owned)
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.logf("session-controller: keep-alive aftermath ABANDONED ws=%q session=%s turn_id=%s rewound=%v held=%d — the workspace has no live session controller after the bounce; the held prompts stay queued and are delivered by the next boundary",
			workspace, sessionID, pingTurnID, rewound, len(owned))
		m.publish(sessionID, view, recs)
		return
	}
	// RE-PARK, THEN RELEASE, THEN DELIVER. The entries rejoin a real queue
	// first, so the hold release and the PENDING re-stamp are applied where the
	// view and the durable records are read from — one queue holding one truth
	// about each prompt, rather than a slice and a queue disagreeing.
	migrated := live != d
	m.returnQueueFromRewindLocked(live, owned)
	if migrated {
		// THE RETIRED CONTROLLER KEEPS ITS MIGRATING MARK FOREVER, and that is
		// deliberate rather than an oversight. Its exit tail may still be
		// pending, and the rewind keeps the SAME session id — so a tail that
		// published its own empty view would persist nil over the durable
		// record of the prompts the replacement is now holding.
		m.logf("session-controller: keep-alive aftermath MIGRATED %d held prompt(s) ws=%q from the retired controller's queue to the rewound one",
			len(owned), workspace)
	}
	released := live.queue.releaseKeepAliveHold(pingTurnID)
	next := live.queue.popFrontDeliverable()
	view, recs := m.publishQueueLocked(live)
	m.mu.Unlock()

	m.logf("session-controller: keep-alive holds RELEASED ws=%q session=%s turn_id=%s released=%d rewound=%v",
		workspace, sessionID, pingTurnID, released, rewound)
	m.publish(live.sessionID, view, recs)
	m.noteDrainActivity()
	if next != nil {
		go m.deliver(live, next)
	}
}

// takeQueueForRewind moves a controller's whole queue into the rewind
// orchestrator's ownership and marks the controller migrating.
//
// THIS IS THE ARBITRATION. Three actors reach for the same queue during a
// rewind bounce: the dying client.Run's exit tail, which drains it
// unconditionally and persists nil; the orchestrator, which carries the held
// prompts across to the replacement; and any prompt still arriving at the old
// controller. Whoever ran first won, and in the common schedule the exit tail
// won and the user's prompts were gone with their durable record.
//
// Ownership moves ONCE, under the manager mutex, BEFORE the stop that starts
// the tail. The tail then structurally finds an empty queue it has been told is
// not its own — there is no ordering left to lose, because there is no longer
// anything for the loser to take.
func (m *Manager) takeQueueForRewind(d *sessionController) []*queueEntry {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.takeQueueForRewindLocked(d)
}

// takeQueueForRewindLocked is takeQueueForRewind with the mutex already held.
func (m *Manager) takeQueueForRewindLocked(d *sessionController) []*queueEntry {
	d.queueMigrating = true
	return d.queue.drainAll()
}

// returnQueueFromRewind re-parks owned entries at the head of dst's queue and
// ends the migration. Caller holds m.mu.
//
// dst is the RETIRED controller when the rewind refused or the bounce failed,
// and the rewound one when it succeeded. Either way the entries land in a queue
// before any of them is released, classified or delivered: the release →
// classify → deliver ordering the held prompts were promised is applied to the
// queue they are in, not to a slice in flight between two of them.
func (m *Manager) returnQueueFromRewindLocked(dst *sessionController, owned []*queueEntry) {
	dst.queue.pushFrontAll(owned)
	dst.queueMigrating = false
}

// drainQueueForExitLocked empties a dying controller's queue for its exit tail,
// or reports nothing when the queue has been taken by a rewind. Caller holds
// m.mu.
func (m *Manager) drainQueueForExitLocked(d *sessionController) []*queueEntry {
	if d.queueMigrating {
		m.logf("session-controller: exit tail DECLINED the queue ws=%q session=%s — a transcript rewind owns these prompts and will re-park them onto the replacement; draining here would drop them and persist nil over the record of what is still owed",
			d.workspace, d.sessionID)
		return nil
	}
	return d.queue.drainAll()
}

// rewindIdentity resolves the config root and vendor uuid the rewind operates
// on. Both come from the durable record, which is the only place that knows
// which account root this session's transcripts live under.
func (m *Manager) rewindIdentity(sessionID string) (configDir, vendorSessionID string, ok bool) {
	if m.cfg.SessionConfigDir != nil {
		configDir = m.cfg.SessionConfigDir(sessionID)
	}
	if m.cfg.VendorSessionOf == nil {
		return "", "", false
	}
	vendorSessionID, ok = m.cfg.VendorSessionOf(sessionID)
	if !ok || vendorSessionID == "" {
		return "", "", false
	}
	return configDir, vendorSessionID, true
}
