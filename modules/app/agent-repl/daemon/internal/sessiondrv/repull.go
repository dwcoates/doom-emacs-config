package sessiondrv

import (
	"context"
	"errors"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// HistorySource re-pulls a slice of a session's history straight from the
// event store, bypassing the shim entirely. Satisfied by *storesub.Client; an
// interface so the driver's re-pull policy is unit-testable without a store.
//
// vendorSessionID is the VENDOR session uuid: the store keys its seq space,
// dedup index, and fan-out by it, so any other id subscribes to a channel
// nothing publishes to.
type HistorySource interface {
	// Replay delivers events with seq in (fromSeq, stopAtSeq), calling onEvent
	// in store order, and reports how many it delivered. A bound tripped before
	// stopAtSeq is a partial answer and comes back as an error.
	Replay(ctx context.Context, vendorSessionID string, fromSeq, stopAtSeq uint64, onEvent func(*corev1.Event)) (int, error)
}

// VendorSessionIDFunc resolves a daemon session id to the vendor session uuid
// the store files its events under. Bound at stitch to the registry record's
// claude_session_id, which is DURABLE — the re-pull's whole purpose is serving
// a frontend after a daemon restart, and an in-memory mapping learned from a
// SessionStarted would be empty exactly then.
type VendorSessionIDFunc func(sessionID string) string

// repullTimeout bounds one below-floor re-pull end to end. It is a FAILURE
// bound, not a pacing knob: a store mid-replay writes back to back, so this
// only decides how long a wedged store may hold a re-pull open.
const repullTimeout = 60 * time.Second

// ErrRepullInFlight reports that the workspace already has a re-pull running
// that does NOT cover the newly requested range.
var ErrRepullInFlight = errors.New("sessiondrv: a history re-pull is already in flight for this workspace")

// repullState is one workspace's in-flight re-pull, guarded by m.mu.
type repullState struct {
	fromSeq uint64
	stopAt  uint64
}

// ---------------------------------------------------------------------------
// Why a below-floor re-pull exists at all, and why it stays a side channel
// ---------------------------------------------------------------------------
//
// The daemon deliberately subscribes each shim from its HIGH-WATER mark. That
// is a settled tradeoff with a scar to show for it: a resumed conversation that
// re-subscribed from 0 replayed thousands of frames down the same connection
// the control Acks ride, and the first prompt after a restart blew its 10s ack
// timeout while the daemon re-read the whole history (see
// RegistrySeqStore.LastSeq). Nothing here reopens that.
//
// What this reopens is narrower: a FRONTEND — not the daemon — may ask for
// history the daemon no longer holds. The retained ring is 4,096 events and is
// empty outright after a restart, so `resync(fromSeq)` below the ring's oldest
// retained seq would otherwise answer with silence, which is precisely the
// blank-feed bug. The re-pull answers it under four standing constraints:
//
//  1. FRONTEND-INITIATED. Nothing in the daemon starts one. It exists only as
//     the tail of a ResyncCmd a frontend sent.
//  2. BOUNDED. It stops at the ring floor (the first seq the live window
//     already covers), a hard event cap, an idle window, and a deadline —
//     see internal/storesub.
//  3. SIDE CHANNEL. Its own throwaway store connection. The standing shim
//     subscription's position is never touched, and no SeqStore write happens.
//  4. CONVERSATION ONLY. Re-pulled events reach frontend conversation
//     translation and NOTHING else — not the SSM, not the task catalog, not the
//     progress resolver, not the retained ring. Those planes consumed these
//     events once already; feeding them again is what makes historical tasks
//     masquerade as live activity and drives live_task_count into impossible
//     values. This constraint is enforced in one place (repullConversation) and
//     is the reason the re-pull does not reuse consumer.Consume.
//
// A daemon-standing version of any of this would be the replay storm the
// high-water subscribe exists to prevent. This is not that, and must not become
// it.

// startRepull runs a below-floor history re-pull for d, delivering re-pulled
// events to CONVERSATION TRANSLATION ONLY (constraint 4 above).
//
// Concurrency: at most one re-pull per workspace. A second request whose range
// is already COVERED by the in-flight one is coalesced onto it — the pull's
// output is broadcast to every subscriber of the workspace, so the second
// caller is genuinely served by the first pull rather than being told "yes"
// while nothing happens. A request reaching FURTHER BACK than the in-flight one
// is not covered, so it is refused loudly (ErrRepullInFlight) instead of being
// silently under-served.
//
// Runs synchronously: it is the tail of a ResyncCmd, and the CommandAck should
// report what actually happened rather than acknowledging an intent.
func (m *Manager) startRepull(d *driven, fromSeq, stopAt uint64) error {
	if m.cfg.History == nil {
		return fmt.Errorf("sessiondrv: resync for workspace %q asked for seq %d, below the retained window (floor %d), but no history source is wired",
			d.workspace, fromSeq, stopAt)
	}
	vendorID := ""
	if m.cfg.VendorSessionID != nil {
		vendorID = m.cfg.VendorSessionID(d.sessionID)
	}
	if vendorID == "" {
		return fmt.Errorf("sessiondrv: cannot re-pull history for session %s (ws %q): no vendor session id known, and the store keys events by it",
			d.sessionID, d.workspace)
	}

	m.mu.Lock()
	if cur := d.repull; cur != nil {
		covered := cur.fromSeq <= fromSeq
		m.mu.Unlock()
		if covered {
			m.logf("sessiondrv: coalescing re-pull ws=%q from_seq=%d onto the in-flight one (from_seq=%d stop_at=%d)",
				d.workspace, fromSeq, cur.fromSeq, cur.stopAt)
			return nil
		}
		return fmt.Errorf("%w: ws=%q in-flight from_seq=%d does not cover the requested from_seq=%d",
			ErrRepullInFlight, d.workspace, cur.fromSeq, fromSeq)
	}
	d.repull = &repullState{fromSeq: fromSeq, stopAt: stopAt}
	m.mu.Unlock()
	defer func() {
		m.mu.Lock()
		d.repull = nil
		m.mu.Unlock()
	}()

	ctx, cancel := context.WithTimeout(m.rootCtx, repullTimeout)
	defer cancel()
	m.logf("sessiondrv: re-pulling history ws=%q session=%s vendor=%s from_seq=%d stop_at=%d (frontend-initiated, conversation only)",
		d.workspace, d.sessionID, vendorID, fromSeq, stopAt)
	n, err := m.cfg.History.Replay(ctx, vendorID, fromSeq, stopAt, d.consumer.repullConversation)
	if err != nil {
		return fmt.Errorf("sessiondrv: history re-pull for ws %q (from_seq=%d stop_at=%d) delivered %d event(s) then failed: %w",
			d.workspace, fromSeq, stopAt, n, err)
	}
	m.logf("sessiondrv: re-pull complete ws=%q delivered=%d event(s) from_seq=%d stop_at=%d", d.workspace, n, fromSeq, stopAt)
	return nil
}

// repullConversation is the ONLY sink a re-pulled historical event may reach.
//
// It deliberately does NOT call retain, ssm.Apply, applyProgress,
// observeBackfill, or PushTaskCatalog. Every one of those planes already
// consumed this event when it was live; re-applying it would double-count
// tasks, re-open closed turns, and re-drive the footer from history. The event
// is translated to a ConversationDelta and pushed, exactly as consumer.resync
// does for the retained ring, and that is all.
//
// The ring is not extended either: the ring is the LIVE window, and back-filling
// it with re-pulled history would make the floor drift under the next request.
func (c *consumer) repullConversation(ev *corev1.Event) {
	if ev.GetVendor() == nil {
		return // only vendor payloads carry conversation content
	}
	c.pushConversation(ev)
}
