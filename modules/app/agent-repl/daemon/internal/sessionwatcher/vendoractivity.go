package sessionwatcher

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// THE VENDOR'S ACTIVITY LEVEL (owner ruling, 2026-10-30; conversation.v1
// SessionVendorActivity).
//
// THE SHIM IS THE SOLE AUTHORITY ON STAND-DOWN, and it counts the vendor's own
// state: busy from the moment a background task ends until the turn that hands
// its result back is over, and while a wakeup or cron job is pending. The
// turn in flight and the live-work level say neither, so a freeness read from
// them alone asked the shim to stand down inside that span, and a shim that
// agreed lost the hand-back (2026-10-29, emudhra-cert-compromise-response).
//
// This watcher's reading is ADVICE for when to ask: a busy vendor is work in
// flight, so freeness waits for its idle, and the move to idle is a freeness
// edge (OnFree) that re-runs a replacement the shim refused. The shim's
// refusal stays the answer.
//
// A shim whose contract predates SESSION_CONTRACT_VENDOR_ACTIVITY_LEVEL states
// no level, and its vendor is read as idle, as before the level existed.

// vendorBusyOf reports whether an activity statement says the vendor is busy.
// ANY STATE BUT IDLE IS BUSY, a statement naming no arm included: a state this
// revision does not know is never read as permission to stand down.
func vendorBusyOf(activity *conversationv1.SessionVendorActivity) bool {
	return activity.GetIdle() == nil
}

// takeVendorActivityLocked holds the watcher to one statement of the vendor's
// activity -- a SessionUpdate.vendor_activity push, or a SessionStarted's --
// and reports whether the reading changed. SOURCE names where it came from.
func (w *watcher) takeVendorActivityLocked(activity *conversationv1.SessionVendorActivity, source string) bool {
	busy := vendorBusyOf(activity)
	if busy == w.vendorBusy {
		return false
	}
	w.log.Debug("daemon.sessionwatcher.state_transition", "the vendor's activity level changed", dlog.Context{
		"state": "vendor_busy", "before": w.vendorBusy, "after": busy, "source": source,
		"named_arm": activity.GetState() != nil,
	})
	w.vendorBusy = busy
	return true
}
