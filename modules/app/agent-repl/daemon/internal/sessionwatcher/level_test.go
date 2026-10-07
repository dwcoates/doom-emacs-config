package sessionwatcher

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// The live-work level (level.go): a shim stamped SESSION_CONTRACT_LIVE_WORK_LEVEL
// says which work runs, and the ledger holds exactly what its latest level
// names. These pin each rule, and the 2026-10-07 regression: an announcement
// replayed out of the record made finished work live again.

// levelStarted is sessionStarted from a shim that speaks the live-work level.
func levelStarted(turn string, live ...*conversationv1.AgentDetachedWork) *conversationv1.SessionStarted {
	started := sessionStarted(turn, live...)
	started.Contract = conversationv1.SessionContract_SESSION_CONTRACT_LIVE_WORK_LEVEL
	return started
}

// levelUpdate is one SessionUpdate.live_work push naming handles.
func levelUpdate(handles ...string) *conversationv1.SessionUpdate {
	level := &conversationv1.SessionLiveWork{}
	for _, handle := range handles {
		level.LiveWork = append(level.LiveWork, workID(handle))
	}
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_LiveWork{LiveWork: level}}
}

// pushLevel sends one level and waits until the watcher has routed it, by
// routing a session update after it.
func (h *harness) pushLevel(t *testing.T, handles ...string) {
	t.Helper()
	h.sendSessionUpdate(t, levelUpdate(handles...))
	h.sendSessionUpdate(t, compactingUpdate())
	h.rec.until(t, "footer.OnSessionUpdate")
}

func TestAnAnnouncementAdmitsNothingUnderTheLevel(t *testing.T) {
	// Arrange: the doom regression's shape -- a finished subagent's
	// announcement arriving on a catch-up page, with no level naming it.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	h.client.noAgentOpen(t)
}

func TestALevelAdmitsADescribedItem(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))

	// Act.
	h.pushLevel(t, "w-1")

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}})
}

func TestALevelHoldsAnUndescribedItemPending(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()

	// Act.
	h.pushLevel(t, "w-1")

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Pending: []*conversationv1.DetachedWorkId{workID("w-1")}})
}

func TestAPendingItemIsAdmittedWhenItsAnnouncementArrives(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.pushLevel(t, "w-1")

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}})
}

func TestALevelNoLongerNamingAnItemRetiresItAsLeftLevel(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))
	h.pushLevel(t, "w-1")

	// Act.
	h.pushLevel(t)

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	if got := h.retiredRecord(t, "w-1")["conclusion"]; got != string(concludedLeftLevel) {
		t.Fatalf("conclusion = %v, want %q", got, concludedLeftLevel)
	}
}

func TestALevelNeverRetiresAShell(t *testing.T) {
	// Arrange: a shell's liveness is the record's -- its announcement and the
	// sidecar's terminal -- so the level, which never names shells, leaves it.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))

	// Act.
	h.pushLevel(t)

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{workID("w-1")}})
}

func TestARetiredHandleIsNotReadmittedByALaterLevel(t *testing.T) {
	// Arrange: the terminal lands before the level drops the item.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("")))))
	h.pushLevel(t, "w-1")
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("w-1", false)))))

	// Act.
	h.pushLevel(t, "w-1")

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
}

func TestAPendingItemRetiresOnItsTerminal(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.pushLevel(t, "w-1")

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("w-1", false)))))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	if got := h.retiredRecord(t, "w-1")["kind"]; got != "pending" {
		t.Fatalf("retired kind = %v, want pending", got)
	}
}

func TestTheOpeningsLevelAdmitsItsItemsWhole(t *testing.T) {
	// Arrange / Act: the opening states one live subagent.
	h := newHarness(t, Session{Started: levelStarted("", createdWork("w-1", subagentWork("sub-1")))})
	h.quiet()

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}})
}

func TestALevelFromAShimThatPredatesTheStampIsRefusedLoudly(t *testing.T) {
	// Arrange: the opening states no contract.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.pushLevel(t, "w-1")

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	if !h.hasRecord("error", "daemon.sessionwatcher.level_unexpected") {
		t.Fatalf("records = %+v, want the unexpected level at ERROR", h.log.Records())
	}
}

func TestTheLevelIsRecordedWhenApplied(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()

	// Act.
	h.pushLevel(t, "w-1")

	// Assert.
	var got map[string]any
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == "daemon.sessionwatcher.level_applied" && r.Context["source"] == "session_update" {
			got = r.Context
		}
	}
	if got == nil || got["named"] != 1 {
		t.Fatalf("level record = %+v, want a session_update level naming 1; records = %+v", got, h.log.Records())
	}
}

func TestAQueryDeathMarksItsPublicationProcessEnded(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))
	h.pushLevel(t, "w-1")

	// Act.
	h.sendSessionUpdate(t, queryDiedUpdate())

	// Assert: the feed is handed an empty set marked process-ended (the wait
	// fails the test if no such publication ever comes).
	h.rec.untilEvent(t, "feed.OnLiveWorkChanged(process_ended)", func(e event) bool {
		return e.sink == "feed" && e.method == "OnLiveWorkChanged" && e.live != nil && e.live.ProcessEnded && e.live.Empty()
	})
}
