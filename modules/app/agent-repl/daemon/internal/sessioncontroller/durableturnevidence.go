package sessioncontroller

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/dlog"
)

// durableturnevidence.go — WHAT THE STORE KNOWS ABOUT A TURN THE DAEMON MISSED.
//
// # The invariant
//
// THE STORE'S RECORD OUTRANKS ANY PROCESS'S MEMORY. A returning shim's hello is
// one process's account of what it is running RIGHT NOW; a `TurnEnded` sitting
// in the store is the recorded fact that a turn finished. When the two are put
// against a standing claim they answer different questions, and only the second
// one can distinguish a turn that was CUT when its process went away from a turn
// that COMPLETED while the daemon was not there to hear it.
//
// # What went wrong without it
//
// The handshake reconciliation cut every claim the hello no longer named as
// `interrupted_by_restart` (ssm.TurnCloseRestartInterrupted). A turn that ran to
// completion during a daemon bounce is, from the hello's point of view,
// indistinguishable from one that died — the shim is not running either of them
// any more — so a finished turn was reported to the user as interrupted, its
// claim was closed by a synthesized end, and its own durable `TurnEnded`
// arriving moments later on the subscription replay landed on a claim that was
// already dead.
//
// # The resolution, and why it is a REFUSAL rather than a second settlement
//
// This probe does not settle anything. It reads the store and reports which of
// the standing claims already have a terminal event above the daemon's own
// last-delivered seq; those claims are SPARED by the handshake reconciliation
// and left open. The subscription that DaemonHello is about to open then
// replays that very `TurnEnded` through the ordinary turn lifecycle — the same
// path a live end takes — so the turn settles COMPLETED with its accounting, its
// result item, and its progress boundary intact. Nothing is synthesized, nothing
// is delivered twice, and the one implementation of a turn ending stays one.
//
// # Cost
//
// The probe is a store replay and is therefore paid only where it changes an
// answer: exclusively on the handshake that would OTHERWISE cut a claim (the
// hello names no turn at all while the ledger still holds one). That is already
// the exceptional bring-up, never the ordinary one, so no reattach that had no
// phantom claim pays anything for this.
//
// # Failure is loud and CHANGES NOTHING
//
// An unreadable store leaves the probe with no evidence, and no evidence is not
// evidence of completion. The reconciliation then proceeds exactly as it did
// before — the claim is cut as interrupted — and the log names the read that
// failed, so a turn reported interrupted despite having finished is explainable
// rather than mysterious.

// durableTurnEndProbe reports which of candidates the durable store already
// holds a `TurnEnded` for. It is bound onto the consumer by the Manager and is
// nil on a consumer built without a durable history source.
type durableTurnEndProbe func(candidates []string) ([]string, error)

// durableTurnEnds reads the session's unconsumed durable history and reports the
// candidate turn ids whose `TurnEnded` is already recorded there.
//
// fromSeq is the daemon's own last-delivered seq, so the scan covers exactly the
// events the reattaching subscription is about to replay: an end at or below
// that mark was already delivered and accepted, and a claim standing against one
// of those is a different defect than this one.
func (m *Manager) durableTurnEnds(workspace, sessionID string, candidates []string) ([]string, error) {
	if len(candidates) == 0 {
		return nil, nil
	}
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", workspace, "session", sessionID, "source", "shim-store")
	if m.cfg.DurableHistory == nil {
		return nil, fmt.Errorf("session-controller: durable turn-end probe for ws %q cannot run: no durable history source is wired", workspace)
	}
	wanted := make(map[string]struct{}, len(candidates))
	for _, id := range candidates {
		// A LEGACY (identity-less) CLAIM IS NEVER PROVABLE. Its end carries no
		// turn id to match, so no stored event can be attributed to it and the
		// existing interrupted verdict is the only honest one.
		if id != "" {
			wanted[id] = struct{}{}
		}
	}
	if len(wanted) == 0 {
		logf("session-controller: durable turn-end probe SKIPPED candidates=%s — every standing claim is identity-less, and an end with no turn id cannot be attributed to one",
			formatTurnIDs(candidates))
		return nil, nil
	}
	lastSeen := m.cfg.SeqStore.LastSeq(sessionID)
	logf("session-controller: durable turn-end probe reading the store ws=%q session=%s from_seq=%d candidates=%s — deciding whether these claims name turns that FINISHED during the gap or turns that were CUT by it",
		workspace, sessionID, lastSeen, formatTurnIDs(candidates))
	found := map[string]struct{}{}
	res, err := m.cfg.DurableHistory.ReplayHistory(m.rootCtx, workspace, sessionID, lastSeen, 0, repullMaxEvents,
		func(ev *corev1.Event) {
			ended := ev.GetTurnEnded()
			if ended == nil {
				return
			}
			if _, want := wanted[ended.GetTurnId()]; want {
				found[ended.GetTurnId()] = struct{}{}
			}
		})
	if err != nil {
		return nil, fmt.Errorf("session-controller: durable turn-end probe for ws %q (from_seq=%d, %d event(s) read) failed: %w",
			workspace, lastSeen, res.Delivered, err)
	}
	// A TRUNCATED SCAN IS NOT A NEGATIVE ANSWER. The cap stops the read before
	// the history drains, so an end beyond it is unseen rather than absent, and
	// reading "unseen" as "this turn was cut" is exactly the misreport this
	// probe exists to end.
	if res.Truncated {
		return nil, fmt.Errorf("%w: ws=%q durable turn-end probe read %d event(s) from_seq=%d without draining: %s",
			ErrRepullTruncated, workspace, res.Delivered, lastSeen, res.Reason)
	}
	// Reported in the CALLER's order so the log reads against the claim list.
	settled := make([]string, 0, len(found))
	for _, id := range candidates {
		if _, ok := found[id]; ok {
			settled = append(settled, id)
		}
	}
	logf("session-controller: durable turn-end probe COMPLETE ws=%q session=%s from_seq=%d events_read=%d candidates=%s durably_ended=%s — the ends found here are already recorded, so their claims are settled by their own replayed boundary rather than cut",
		workspace, sessionID, lastSeen, res.Delivered, formatTurnIDs(candidates), formatTurnIDs(settled))
	return settled, nil
}

// bindDurableTurnEndProbe wires the store probe onto a live consumer. A Manager
// with no durable history source binds nothing, and the handshake then keeps its
// pre-existing behavior with the reason logged at the judgment site.
func (m *Manager) bindDurableTurnEndProbe(cons *consumer, workspace, sessionID string) {
	if m.cfg.DurableHistory == nil {
		return
	}
	cons.durableTurnEnds = func(candidates []string) ([]string, error) {
		return m.durableTurnEnds(workspace, sessionID, candidates)
	}
}
