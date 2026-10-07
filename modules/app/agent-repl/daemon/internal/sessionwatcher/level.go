package sessionwatcher

import (
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"
	"claude-repld/internal/dlog"
)

// THE LIVE-WORK LEVEL (owner ruling, 2026-10-07; conversation.v1
// SessionLiveWork).
//
// WHICH DETACHED WORK IS RUNNING IS A LEVEL OWNED BY THE VENDOR PROCESS THAT
// RUNS IT, never an edge read out of a record. A shim stamped
// SESSION_CONTRACT_LIVE_WORK_LEVEL pushes the whole set on every membership
// change, and an empty one whenever its vendor process ends or restarts; the
// ledger then holds EXACTLY what the latest level names:
//
//   - an announcement DESCRIBES an item (its kind, its agent) and never admits
//     it, so a history replay can never make finished work live again;
//   - a level naming an item admits it -- with its description when one has
//     arrived, or PENDING by its handle until one does;
//   - a level no longer naming an item concludes it (concludedLeftLevel);
//   - a terminal still concludes an item, and a concluded handle is never
//     re-admitted by a later level (retiredWork);
//   - a SHELL is not in the level: its end has one writer, the sidecar's
//     terminal once the spool is read to its end, so a shell is admitted at its
//     announcement and concluded by that terminal, in level mode as before it.
//
// The defect this replaced (2026-10-07, workspace `doom`): a deploy's handover
// re-attached a watcher whose catch-up page replayed every subagent the
// conversation ever announced. Each announcement admitted its item after the
// re-announcement's reconciliation had run, so nothing judged it, and the
// thirteen whose transcripts recorded their endings in a form no converter
// read stood live in the footer for good.

// levelModeLocked reports whether the shim speaks the live-work level, so the
// ledger follows it rather than announcements.
func (w *watcher) levelModeLocked() bool {
	return w.contract >= conversationv1.SessionContract_SESSION_CONTRACT_LIVE_WORK_LEVEL
}

// takeContractLocked records the session contract a SessionStarted states. A
// shim's contract is fixed for its process, so a re-announcement stating a
// different one is a producer breach, recorded at ERROR; the newer statement
// is the one the shim now speaks.
func (w *watcher) takeContractLocked(started *conversationv1.SessionStarted) {
	stated := started.GetContract()
	if w.started && stated != w.contract {
		w.log.Error("daemon.sessionwatcher.session_contract_changed", "a re-announcement stated a different session contract than the shim's first; the newer one is taken", dlog.Context{
			"was": w.contract.String(), "now": stated.String(),
		})
	}
	if stated != w.contract || !w.started {
		w.log.Info("daemon.sessionwatcher.session_contract", "took the session contract the shim speaks", dlog.Context{
			"contract": stated.String(), "level_mode": stated >= conversationv1.SessionContract_SESSION_CONTRACT_LIVE_WORK_LEVEL,
		})
	}
	w.contract = stated
}

// levelNamesLocked reports whether the latest level names handle.
func (w *watcher) levelNamesLocked(handle string) bool {
	_, ok := w.level[handle]
	return ok
}

// applyLevelLocked takes one live-work level -- a SessionUpdate.live_work push
// or a SessionStarted's live membership -- and holds the ledger to it,
// reporting whether the live set changed. SOURCE names where it came from, for
// the record.
func (w *watcher) applyLevelLocked(handles []*conversationv1.DetachedWorkId, source string) bool {
	next := make(map[string]struct{}, len(handles))
	for _, handle := range handles {
		if handle.GetValue() == "" {
			w.log.Error("daemon.sessionwatcher.level_unaddressable", "a live-work level named an item with no handle; it is not counted", dlog.Context{
				"source": source,
			})
			continue
		}
		next[handle.GetValue()] = struct{}{}
	}
	w.level = next
	changed := false
	var admitted, pended, left []string
	for value := range next {
		if _, retired := w.retiredWork[value]; retired {
			w.log.Debug("daemon.sessionwatcher.level_names_retired", "the level names work this watcher already saw settle; it stays out of the live set", dlog.Context{
				"work_id": value, "source": source,
			})
			continue
		}
		if _, live := w.live[value]; live {
			continue
		}
		if _, waiting := w.pending[value]; waiting {
			continue
		}
		if work, ok := w.described[value]; ok {
			if w.admitDescribedLocked(work) {
				admitted = append(admitted, value)
				changed = true
			}
			continue
		}
		w.pending[value] = &conversationv1.DetachedWorkId{Value: value}
		pended = append(pended, value)
		changed = true
	}
	for value := range w.pending {
		if _, still := next[value]; still {
			continue
		}
		delete(w.pending, value)
		left = append(left, value)
		changed = true
	}
	for value, item := range w.live {
		if _, still := next[value]; still {
			continue
		}
		if item.kind == kindBash {
			continue
		}
		if w.concludeLocked(value, concludedLeftLevel) {
			left = append(left, value)
			changed = true
		}
	}
	sort.Strings(admitted)
	sort.Strings(pended)
	sort.Strings(left)
	w.log.Info("daemon.sessionwatcher.level_applied", "the ledger was held to the shim's live-work level", dlog.Context{
		"source": source, "named": len(next), "admitted": admitted, "pending": pended, "left": left,
		"live_after": len(w.live), "pending_after": len(w.pending),
	})
	return changed
}

// describeLocked records what an announcement says an item is, and reports
// whether the ledger may admit it now: in level mode only while the latest
// level names it. An item the level holds pending is admitted the moment its
// description arrives.
func (w *watcher) describeLocked(work *conversationv1.AgentDetachedWork) bool {
	handle := work.GetWork().GetValue()
	if handle == "" {
		return !w.levelModeLocked()
	}
	w.described[handle] = work
	if !w.levelModeLocked() || isShellWork(work) {
		return true
	}
	if !w.levelNamesLocked(handle) {
		w.log.Debug("daemon.sessionwatcher.described_not_live", "an announcement described work the live-work level does not name; it is drawn and not admitted", dlog.Context{
			"work_id": handle,
		})
		return false
	}
	delete(w.pending, handle)
	return true
}

// publishProcessEndedLocked publishes the set that follows the vendor process
// ending, marked so a view can tell lost work from work that left the level.
func (w *watcher) publishProcessEndedLocked() {
	w.processEnded = true
	w.publishLiveWorkLocked()
	w.processEnded = false
}

// startedLevel is a SessionStarted's live membership as a level: the handles
// of the items it names.
func startedLevel(started *conversationv1.SessionStarted) []*conversationv1.DetachedWorkId {
	handles := make([]*conversationv1.DetachedWorkId, 0, len(started.GetLiveWork()))
	for _, item := range started.GetLiveWork() {
		if isShellWork(item) {
			continue
		}
		handles = append(handles, item.GetWork())
	}
	return handles
}

// isShellWork reports whether an announcement names a detached shell, whose
// liveness is the record's, not the level's.
func isShellWork(work *conversationv1.AgentDetachedWork) bool {
	_, shell := work.GetKind().GetKind().(*conversationv1.DetachedWorkKind_Bash)
	return shell
}

// describeOnlyLocked records an item's description without admitting it, for
// a level that will be applied right after.
func (w *watcher) describeOnlyLocked(work *conversationv1.AgentDetachedWork) {
	if handle := work.GetWork().GetValue(); handle != "" {
		w.described[handle] = work
	}
}
