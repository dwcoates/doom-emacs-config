package sessionwatcher

import (
	"testing"

	"agent-repld/internal/dlog"
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
	h.client.NoAgentOpen(t)
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

func TestTheFirstLevelThatOmitsARetiredHandleReleasesIt(t *testing.T) {
	// Arrange: the terminal lands first, and a stale level still names the item.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("")))))
	h.pushLevel(t, "w-1")
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("w-1", false)))))
	h.pushLevel(t, "w-1")

	// Act: the level drops it, then names it again for a new run.
	h.pushLevel(t)
	h.pushLevel(t, "w-1")

	// Assert: the described item is live again, counted by its handle (its
	// announcement named no agent).
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("w-1")}})
}

func TestALevelThatConcludesAnItemAlsoReleasesItsHandle(t *testing.T) {
	// Arrange: the item leaves the level with no terminal.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))
	h.pushLevel(t, "w-1")
	h.pushLevel(t)

	// Act: the vendor runs it again.
	h.pushLevel(t, "w-1")

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}})
}

func TestARunReadmittedAfterItsReleaseOpensItsAgentWatchAgain(t *testing.T) {
	// Arrange: the subagent ran once, and its watch was opened then.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))
	h.pushLevel(t, "w-1")
	h.client.NextAgentOpenFor(t, "sub-1")
	h.pushLevel(t)

	// Act.
	h.pushLevel(t, "w-1")

	// Assert: the second run's watch is opened.
	h.client.NextAgentOpenFor(t, "sub-1")
}

func TestAReleaseIsRecordedOnTheLevelThatMadeIt(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("")))))
	h.pushLevel(t, "w-1")
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("w-1", false)))))

	// Act.
	h.pushLevel(t)

	// Assert.
	released, _ := h.lastLevelRecord(t)["released"].([]string)
	if len(released) != 1 || released[0] != "w-1" {
		t.Fatalf("released = %v, want [w-1]", h.lastLevelRecord(t)["released"])
	}
}

func TestAStaleLevelNamingARetiredHandleIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: levelStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("")))))
	h.pushLevel(t, "w-1")
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("w-1", false)))))

	// Act.
	h.pushLevel(t, "w-1")

	// Assert.
	if !h.hasRecord("info", "daemon.sessionwatcher.level_names_retired") {
		t.Fatalf("records = %+v, want the stale level's refusal at INFO", h.log.Records())
	}
}

func TestAShellsRetirementOutlivesEveryLevel(t *testing.T) {
	// Arrange: the shell ends, and a level (which never names shells) follows.
	h := newHarness(t, Session{Started: levelStarted("", createdWork("w-1", bashWork()))})
	h.client.NextBashOpen(t)
	h.quiet()
	entry := h.shellWatchFor("w-1")
	h.routeNow(func(w *watcher) {
		w.routeBashLocked(entry, &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}},
		})
	})
	h.pushLevel(t)

	// Act: the end-of-run upsert re-serves the announcement.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
}

// lastLevelRecord answers the context of the latest level_applied record.
func (h *harness) lastLevelRecord(t *testing.T) dlog.Context {
	t.Helper()
	var last dlog.Context
	for _, r := range h.log.Records() {
		if r.Operation == "daemon.sessionwatcher.level_applied" {
			last = r.Context
		}
	}
	if last == nil {
		t.Fatalf("no level_applied record in %+v", h.log.Records())
	}
	return last
}
