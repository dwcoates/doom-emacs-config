package sessionwatcher

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// The vendor's activity level (vendoractivity.go): a busy vendor is work in
// flight, and its move to idle is a freeness edge. These pin each rule, and
// the 2026-10-29 regression: a replacement judged free while the vendor was
// about to run the turn handing a finished subagent's result back.

// activityBusy and activityIdle are the two statements this revision names.
func activityBusy() *conversationv1.SessionVendorActivity {
	return &conversationv1.SessionVendorActivity{State: &conversationv1.SessionVendorActivity_Busy{Busy: &conversationv1.SessionVendorActivityBusy{}}}
}

func activityIdle() *conversationv1.SessionVendorActivity {
	return &conversationv1.SessionVendorActivity{State: &conversationv1.SessionVendorActivity_Idle{Idle: &conversationv1.SessionVendorActivityIdle{}}}
}

// vendorActivityUpdate is one SessionUpdate.vendor_activity push.
func vendorActivityUpdate(activity *conversationv1.SessionVendorActivity) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_VendorActivity{VendorActivity: activity}}
}

// activityStarted is sessionStarted from a shim that states the vendor's activity.
func activityStarted(activity *conversationv1.SessionVendorActivity) *conversationv1.SessionStarted {
	started := levelStarted("")
	started.Contract = conversationv1.SessionContract_SESSION_CONTRACT_VENDOR_ACTIVITY_LEVEL
	started.VendorActivity = activity
	return started
}

// pushActivity sends one activity statement and waits until the watcher has
// routed it, by routing a session update after it.
func (h *harness) pushActivity(t *testing.T, activity *conversationv1.SessionVendorActivity) {
	t.Helper()
	h.sendSessionUpdate(t, vendorActivityUpdate(activity))
	h.sendSessionUpdate(t, compactingUpdate())
	h.rec.until(t, "footer.OnSessionUpdate")
}

// drainFrees empties the lifecycle sink's freeness edges told so far.
func (h *harness) drainFrees() {
	h.w.dispatching.Wait()
	for {
		select {
		case <-h.rec.frees:
		default:
			return
		}
	}
}

// freeTold reports whether the lifecycle sink was told OnFree since the last drain.
func (h *harness) freeTold() bool {
	h.w.dispatching.Wait()
	select {
	case <-h.rec.frees:
		return true
	default:
		return false
	}
}

func TestVendorBusyOf(t *testing.T) {
	tests := []struct {
		name     string
		activity *conversationv1.SessionVendorActivity
		want     bool
	}{
		{name: "idle is not busy", activity: activityIdle(), want: false},
		{name: "busy is busy", activity: activityBusy(), want: true},
		{name: "a statement naming no arm is busy", activity: &conversationv1.SessionVendorActivity{}, want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := vendorBusyOf(tc.activity)

			// Assert.
			if got != tc.want {
				t.Fatalf("vendorBusyOf = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestABusyVendorIsNotFree(t *testing.T) {
	// Arrange: nothing the watcher has seen runs.
	h := newHarness(t, Session{Started: activityStarted(activityIdle())})
	h.quiet()

	// Act.
	h.pushActivity(t, activityBusy())

	// Assert.
	if h.w.Free() {
		t.Fatal("Free() = true while the vendor states itself busy")
	}
}

func TestTheVendorFallingIdleIsAFreenessEdge(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: activityStarted(activityIdle())})
	h.quiet()
	h.pushActivity(t, activityBusy())
	h.drainFrees()

	// Act.
	h.pushActivity(t, activityIdle())

	// Assert.
	if !h.w.Free() || !h.freeTold() {
		t.Fatalf("Free() = %v with OnFree told = false, want free and told once the vendor states idle", h.w.Free())
	}
}

func TestATurnEndingOverABusyVendorRaisesNoFreenessEdge(t *testing.T) {
	// Arrange: the emudhra shape -- the turn ends while the vendor already
	// owes the hand-back turn.
	started := activityStarted(activityIdle())
	started.TurnInFlight = &conversationv1.TurnId{Value: "turn-1"}
	h := newHarness(t, Session{Started: started})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	h.pushActivity(t, activityBusy())
	h.drainFrees()

	// Act.
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	if h.freeTold() {
		t.Fatal("OnFree was told while the vendor states itself busy")
	}
}

func TestTheOpeningsBusyVendorStartsTheWatcherNotFree(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t, Session{Started: activityStarted(activityBusy())})
	h.quiet()

	// Assert.
	if h.w.Free() {
		t.Fatal("Free() = true for an opening that states the vendor busy")
	}
}

func TestAShimStatingNoActivityReadsItsVendorIdle(t *testing.T) {
	// Arrange, Act: a shim whose contract predates the level.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()

	// Assert.
	if !h.w.Free() {
		t.Fatal("Free() = false for a shim that states no vendor activity and runs nothing")
	}
}

func TestTheVendorsActivityIsRecordedAsAStateTransition(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: activityStarted(activityIdle())})
	h.quiet()

	// Act.
	h.pushActivity(t, activityBusy())

	// Assert.
	for _, record := range dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Level: "debug", Message: "the vendor's activity level changed"}) {
		if record.Context["after"] == true && record.Context["source"] == "session_update" {
			return
		}
	}
	t.Fatal("no state-transition record of the vendor falling busy")
}
