package sessionwatcher

import (
	"testing"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// concludedWork marks an announcement as made at the work's conclusion.
func concludedWork(work *conversationv1.AgentDetachedWork) *conversationv1.AgentDetachedWork {
	work.Concluded = &conversationv1.DetachedWorkConcluded{}
	return work
}

// hasShellWatch reports whether the watcher holds a watch for a shell.
func (h *harness) hasShellWatch(work string) bool {
	h.t.Helper()
	h.w.mu.Lock()
	defer h.w.mu.Unlock()
	_, ok := h.w.shells[work]
	return ok
}

// TestWorkFirstAnnouncedAtItsConclusionIsNeverLive covers each kind the shim
// can first announce at its end: none enters the live set, and the workspace
// stays free.
func TestWorkFirstAnnouncedAtItsConclusionIsNeverLive(t *testing.T) {
	tests := []struct {
		name string
		work *conversationv1.AgentDetachedWork
	}{
		{name: "a shell", work: detachedWork("w-1", "act-1", bashKind())},
		{name: "a subagent", work: detachedWork("w-1", "act-1", subagentKind("sub-1"))},
		{name: "a monitor", work: detachedWork("w-1", "act-1", monitorKind())},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.quiet()

			// Act.
			h.route(h.main, entryFrame(frameDetached("main-1", concludedWork(tt.work))))
			h.client.settleOpens()

			// Assert.
			if live := h.w.LiveWork(); !live.Empty() {
				t.Fatalf("live work = %+v, want none", live)
			}
			if !h.w.Free() {
				t.Fatal("the workspace is not free after work announced concluded")
			}
		})
	}
}

// TestAConcludedFirstShellsRecordIsReadToItsEnd pins the record read: the
// shell's run is still watched so the feed draws its outcome.
func TestAConcludedFirstShellsRecordIsReadToItsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", concludedWork(detachedWork("w-1", "act-1", bashKind())))))

	// Assert.
	if open := h.client.nextBashOpen(t); open.work.GetValue() != "w-1" {
		t.Fatalf("record read opened for %q, want w-1", open.work.GetValue())
	}
}

// TestAConcludedFirstShellsTerminalReapsItsReadWatch pins the read's end: the
// terminal drops the watch and publishes no live-work change.
func TestAConcludedFirstShellsTerminalReapsItsReadWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", concludedWork(detachedWork("w-1", "act-1", bashKind())))))
	h.client.nextBashOpen(t)
	entry := h.shellWatchFor("w-1")

	// Act.
	events := h.routeNow(func(w *watcher) {
		w.routeBashLocked(entry, &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}},
		})
	})

	// Assert.
	if h.hasShellWatch("w-1") {
		t.Fatal("the concluded shell's read watch outlived its terminal")
	}
	for _, e := range events {
		if e.name() == "footer.OnLiveWorkChanged" {
			t.Fatalf("a never-live shell's terminal republished the live set: %+v", e)
		}
	}
}

// TestALaterRunningAnnouncementNeverRevivesConcludedWork pins the retirement:
// a restated running announcement of work recorded concluded is not admitted.
func TestALaterRunningAnnouncementNeverRevivesConcludedWork(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", concludedWork(detachedWork("w-1", "act-1", bashKind())))))

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "act-1", bashKind()))))
	h.client.settleOpens()

	// Assert.
	if live := h.w.LiveWork(); !live.Empty() {
		t.Fatalf("live work = %+v, want none", live)
	}
}

// TestAConcludedRestatementOfALiveShellLeavesItLive pins the other side: work
// already running ends at its own terminal, never at the restatement.
func TestAConcludedRestatementOfALiveShellLeavesItLive(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "act-1", bashKind()))))
	h.client.nextBashOpen(t)

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", concludedWork(detachedWork("w-1", "act-1", bashKind())))))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{workID("w-1")}})
}

// TestTheIncidentsConcludedFirstRefusedShellLeavesTheWorkspaceFree reproduces
// 2026-10-08 18:15:27 deterministically: the shim announces a shell it first
// saw conclude, the store refuses its watch (no rows yet), and the workspace is
// free at once -- with the record read re-armed on the clock, not live.
func TestTheIncidentsConcludedFirstRefusedShellLeavesTheWorkspaceFree(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setBashErr(refusedOpenError("WatchBash", connect.CodeNotFound, "the store holds no rows for shell run w-1"))

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", concludedWork(detachedWork("w-1", "act-1", bashKind())))))
	h.client.awaitRefusedOpen(t, "WatchBash")

	// Assert.
	if !h.w.Free() {
		t.Fatalf("the workspace is not free: live = %+v", h.w.LiveWork())
	}
	if delay := h.clock.awaitArmed(t); delay != reopenFloor {
		t.Fatalf("the record read's re-open waits %v, want %v", delay, reopenFloor)
	}
}
