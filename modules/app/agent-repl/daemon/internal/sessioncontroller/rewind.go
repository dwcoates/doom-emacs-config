package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/google/uuid"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

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
//     store cursors — AdoptVendorSessionID's precedent, and for its reason: the
//     rotation retires the old seq space, and a split write would let the
//     hydrate-from-checkpoint step undo the reset before the new uuid landed.
//  5. SPAWN, resuming the new uuid, carrying the lineage argv.
//  6. DELIVER the held prompt.
//
// CRASH SAFETY FALLS OUT OF STEP 4 BEING THE ONLY DESTRUCTIVE ONE. A daemon
// that dies before the flip leaves an orphaned copy nobody reads and a record
// still naming the original, so the next real prompt simply re-triggers the
// whole thing. The rewind is idempotent by being re-runnable, not by being
// transactional.
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

// RewindLineageArmer arms the one-shot lineage argv for a session's next spawn.
// Satisfied by *server.ShimSpawner. Nil disables the rewind entirely, which is
// loud at the one site that would have used it rather than silently skipped.
type RewindLineageArmer interface {
	ArmRewindLineage(sessionID string, lineage RewindLineage) error
}

// VendorSessionAdopter performs the ATOMIC registry flip: adopt the new vendor
// uuid and reset the store cursors in one Update. It is
// SessionRegistrar.AdoptVendorSessionID, named separately here because the
// rewind needs exactly that one method and nothing else on the registrar.
type VendorSessionAdopter interface {
	AdoptVendorSessionID(sessionID, claudeSessionID string) (rotated bool, previous string, adopted bool)
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
	if m.cfg.RewindLineages == nil {
		return "", fmt.Errorf("session-controller: refusing to rewind ws=%q session=%s: no rewind lineage armer is wired, so the shim could not be told what was dropped and the rewind would be unrecorded",
			workspace, sessionID)
	}
	if m.cfg.VendorSessions == nil {
		return "", fmt.Errorf("session-controller: refusing to rewind ws=%q session=%s: no vendor-session adopter is wired, so the registry flip could not be made atomic",
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

	// STOP THE SHIM. The CLI holds the transcript open; copying from under a
	// live writer risks a half-written trailing line.
	if err := m.stopSessionController(workspace, sessionID, StopCauseHibernateIdleSweep()); err != nil {
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: stopping the shim: %w", workspace, sessionID, err)
	}

	newVendorSessionID = uuid.NewString()
	destDir := session.ProjectDir(session.ClaudeConfigDir(configDir), workspace)
	dest, err := session.WriteRewound(records, plan, newVendorSessionID, destDir)
	if err != nil {
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: writing the truncated copy: %w", workspace, sessionID, err)
	}
	m.logf("session-controller: rewind COPY WRITTEN ws=%q session=%s from=%s to=%s path=%s — the original is untouched, so a crash before the registry flip simply re-triggers this rewind",
		workspace, sessionID, vendorSessionID, newVendorSessionID, dest)

	// THE ARM COMES BEFORE THE FLIP. Arming is in-memory and cannot fail
	// durably, and doing it first means the spawn that follows the flip cannot
	// find itself without a lineage to announce.
	lineage := RewindLineage{
		PreviousVendorSessionID: vendorSessionID,
		RetainedLeafUUID:        plan.RetainedLeafUUID,
		DroppedTurnIDs:          strings.Join(droppedTurnIDs, ","),
	}
	if err := m.cfg.RewindLineages.ArmRewindLineage(sessionID, lineage); err != nil {
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: arming the lineage argv: %w", workspace, sessionID, err)
	}

	// THE FLIP: ONE Update that adopts the new uuid AND resets the store
	// cursors. Splitting them would let the registry's hydrate-from-checkpoint
	// step undo the reset before the new uuid was recorded (AdoptVendorSessionID).
	rotated, previous, adopted := m.cfg.VendorSessions.AdoptVendorSessionID(sessionID, newVendorSessionID)
	if !adopted {
		return "", fmt.Errorf("session-controller: rewind ws=%q session=%s: the registry refused to adopt the rewound vendor session %s (previous %s); the original conversation is intact and still the record's",
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

	rewound := false
	if _, err := m.rewindKeepAliveTurns(m.rootCtx, workspace, sessionID, []string{pingTurnID}); err != nil {
		if errors.Is(err, session.ErrNoRewindNeeded) {
			m.logf("session-controller: keep-alive rewind SKIPPED ws=%q session=%s turn_id=%s — the transcript tail holds no keep-alive turns to drop",
				workspace, sessionID, pingTurnID)
		} else {
			m.logf("session-controller: keep-alive rewind DEGRADED ws=%q session=%s turn_id=%s error=%v — the held prompt is submitted WITHOUT rewinding, so the keep-alive turn stays visible in the conversation; correctness over cleanliness",
				workspace, sessionID, pingTurnID, err)
		}
	} else {
		rewound = true
	}

	// THE LIVE CONTROLLER IS RE-RESOLVED. A successful rewind replaced the one
	// this function was handed, and delivering onto the retired controller
	// would submit through a client whose connection is gone.
	m.mu.Lock()
	live, ok := m.byWS[workspace]
	if !ok {
		m.mu.Unlock()
		m.logf("session-controller: keep-alive aftermath ABANDONED ws=%q session=%s turn_id=%s rewound=%v — the workspace has no live session controller after the bounce; the held prompts stay queued and are delivered by the next boundary",
			workspace, sessionID, pingTurnID, rewound)
		return
	}
	released := live.queue.releaseKeepAliveHold(pingTurnID)
	if released == 0 && live != d {
		// The rewind bounced the controller, so the queue that held the prompts
		// is the RETIRED one. Carry them across rather than losing them: they
		// are prompts the user typed and the daemon promised to deliver.
		moved := d.queue.drainAll()
		for _, e := range moved {
			e.keepAliveHoldTurnID = ""
			e.classification = frontendv1.QueueClassification_QUEUE_CLASSIFICATION_PENDING
			live.queue.add(e)
		}
		released = len(moved)
		m.logf("session-controller: keep-alive aftermath MIGRATED %d held prompt(s) ws=%q from the retired controller's queue to the rewound one",
			released, workspace)
	}
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
