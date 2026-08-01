package sessioncontroller

import (
	"context"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/storehistory"
)

// This file serves a frontend resync for a workspace that has NO live session
// controller.
//
// THE GAP IT CLOSES. Manager.Resync's live path replays the retained ring and
// closes the remainder through the shim (repull.go). Both halves need a session
// controller, so a workspace without one used to have its conversation replay
// SKIPPED — quietly, one layer up in the frontend command handler. The observed
// symptom is a reloaded webview with a correct footer and an EMPTY feed: the
// daemon had bounced, nothing had brought the session back up, and the whole
// conversation was still sitting in the store.
//
// WHY IT DOES NOT BRING A SESSION UP. A frontend mounting is not a reason to
// spawn a vendor process. Bring-up is what the user's first prompt and an
// explicit workspace open are for; a read must cost a read.
//
// WHY IT IS NOT A FALLBACK. See storehistory's package header: an unwired
// workspace is not a shim outage but the resting state of every workspace after
// a daemon bounce, and for it the store is not a SECOND route to the history —
// it is the ONLY one. Nothing is being masked, because there is no live route
// whose failure could be hidden.
//
// WHAT IT REPRODUCES EXACTLY. The events are pushed through the SAME
// consumer.pushConversation the live ring replay and the shim re-pull use, so
// the translation, the curation passes, the ConversationDelta.through_seq
// stamping, and the provenance verdict are one implementation rather than two
// that must be kept in agreement. Provenance in particular is read from the
// merge lease's DURABLE ledger by each item's own timestamp
// (StateApplier.ConversationSourceAt), so a merge-window item replays as
// CONVERSATION_SOURCE_MERGE long after the lease was released.

// DurableHistorySource replays a session's PERSISTED conversation events with
// no shim in the loop. Satisfied by *storehistory.Reader.
//
// fromSeq is EXCLUSIVE (Subscribe.from_seq); toSeq is an EXCLUSIVE upper bound
// with 0 meaning "until the history drains"; maxEvents caps one replay, and a
// tripped cap comes back as a TRUNCATED result rather than a quiet short answer.
type DurableHistorySource interface {
	ReplayHistory(ctx context.Context, workspace, sessionID string, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (storehistory.Result, error)
}

// resyncFromDurableHistory serves a resync for a workspace with no live session
// controller, straight from the durable store.
//
// SEQ AND FLOOR SEMANTICS ARE THE LIVE PATH'S, UNCHANGED. The replay starts at
// max(fromSeq, newestClearOrCompactSeq) INCLUSIVE — the same replayFloor rule,
// applied here against the DURABLE last_seen_seq alone, because an unwired
// workspace has no retained ring to raise that ceiling. The inclusive floor is
// then converted to the store's exclusive from_seq by exclusiveLowerBound, so a
// replay floored at a clear or a compaction still replays that event itself.
// There is no upper bound: with no ring there is no live window for the replay
// to stop short of, so the whole conversation above the floor is served.
//
// EVERY FAILURE IS LOUD. A missing session record, an unwired durable source,
// an unreadable store, and a truncated replay all return an error, so the
// resync's CommandAck reports the failure. Silence here would be
// indistinguishable from an empty conversation, which is the bug.
func (m *Manager) resyncFromDurableHistory(workspace string, fromSeq uint64) error {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", workspace, "from_seq", fromSeq, "source", "shim-store")
	logf("session-controller: resync for an UNWIRED workspace — serving the conversation replay from DURABLE history (no session controller is live, and none is brought up to answer a read)")
	if m.cfg.DurableHistory == nil {
		logf("session-controller: durable resync REFUSED — no DurableHistorySource is wired, so the workspace's stored conversation cannot be read at all")
		return fmt.Errorf("session-controller: resync for unwired ws %q cannot be served: no durable history source is wired", workspace)
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		logf("session-controller: durable resync REFUSED — the workspace resolves to no session record, so there is no conversation to locate in the store")
		return fmt.Errorf("session-controller: resync for unwired ws %q cannot be served: %w", workspace, errclass.ErrNoLiveSessionController)
	}
	lastSeen := m.cfg.SeqStore.LastSeq(sessionID)
	replayFrom := m.replayFloorAt(workspace, sessionID, lastSeen, fromSeq)
	cons := m.durableConsumer(workspace, sessionID)
	logf("session-controller: durable resync replaying ws=%q session=%s replay_from=%d (inclusive) last_seen_seq=%d max_events=%d",
		workspace, sessionID, replayFrom, lastSeen, repullMaxEvents)
	res, err := m.cfg.DurableHistory.ReplayHistory(m.rootCtx, workspace, sessionID,
		exclusiveLowerBound(replayFrom), 0, repullMaxEvents,
		func(ev *corev1.Event) { cons.pushConversation(ev, false) })
	if err != nil {
		logf("session-controller: durable resync FAILED ws=%q session=%s replay_from=%d delivered=%d: %v",
			workspace, sessionID, replayFrom, res.Delivered, err)
		return fmt.Errorf("session-controller: durable resync for ws %q (replay_from=%d) failed: %w", workspace, replayFrom, err)
	}
	if res.Truncated {
		logf("session-controller: durable resync TRUNCATED ws=%q session=%s replay_from=%d delivered=%d first_seq=%d last_seq=%d reason=%q",
			workspace, sessionID, replayFrom, res.Delivered, res.FirstSeq, res.LastSeq, res.Reason)
		return fmt.Errorf("%w: ws=%q replay_from=%d delivered %d event(s) from durable history: %s",
			ErrRepullTruncated, workspace, replayFrom, res.Delivered, res.Reason)
	}
	logf("session-controller: durable resync COMPLETE ws=%q session=%s replay_from=%d events_served=%d first_seq=%d last_seq=%d",
		workspace, sessionID, replayFrom, res.Delivered, res.FirstSeq, res.LastSeq)
	return nil
}

// durableConsumer builds the throwaway translation consumer one durable replay
// runs through.
//
// It is deliberately NOT a session controller and holds no shim, no ring, and
// no lifecycle hooks: replayed history must reach conversation translation and
// nothing else, exactly as repullConversation guarantees for the shim-served
// re-pull. The curation state (the skill correlator in particular) starts empty
// per replay, which is what a replay read in store order from the floor wants.
func (m *Manager) durableConsumer(workspace, sessionID string) *consumer {
	return newConsumer(workspace, sessionID, m.cfg.Push, m.cfg.SSM, nil, m.cfg.ClearCompactStore, m.logf, nil, nil, nil, nil, nil)
}
