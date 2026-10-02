package sessionwatcher

import (
	"io"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"connectrpc.com/connect"

	"claude-repld/internal/shimclient"
)

// The live-work ledger's invariant: an item leaves the live set exactly when
// the shim concludes it, whatever its watch is doing, and a conclusion tears
// that watch down. See livework.go.

// awaitFree waits for the lifecycle sink's freeness edge.
func (h *harness) awaitFree(t *testing.T) {
	t.Helper()
	select {
	case <-h.rec.frees:
	case <-time.After(waitDeadline):
		t.Fatal("OnFree was never told")
	}
}

// noFree asserts no freeness edge has been told.
func (h *harness) noFree(t *testing.T) {
	t.Helper()
	h.w.dispatching.Wait()
	select {
	case ws := <-h.rec.frees:
		t.Fatalf("OnFree(%q) was told while detached work is still live", ws)
	default:
	}
}

// reannounce sends the shim's re-announcement on the current session stream and
// waits until the watcher has routed it, by routing a session update after it.
func (h *harness) reannounce(t *testing.T, started *conversationv1.SessionStarted) {
	t.Helper()
	h.sendSessionStarted(t, started)
	h.sendSessionUpdate(t, compactingUpdate())
	h.rec.until(t, "footer.OnSessionUpdate")
}

// retiredRecord answers the retirement record for one handle.
func (h *harness) retiredRecord(t *testing.T, work string) map[string]any {
	t.Helper()
	for _, r := range h.log.Records() {
		if r.Operation == "daemon.sessionwatcher.live_work_retired" && r.Context["work_id"] == work {
			return r.Context
		}
	}
	t.Fatalf("no retirement was recorded for %q; records = %+v", work, h.log.Records())
	return nil
}

// TestAShellConcludedWhileItsWatchOpenHangsLeavesTheLiveSet is the
// 2026-09-27 wedge: a hand-backgrounded shell's WatchBash never got its first
// frame, and the shell stayed live after the shim had concluded it. The
// conclusion takes it out of the set and makes the workspace free.
func TestAShellConcludedWhileItsWatchOpenHangsLeavesTheLiveSet(t *testing.T) {
	// Arrange: a shell whose watch the shim never answers, and a re-open of
	// the fleet whose own shell open hangs the same way.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	first := h.client.gate(&h.client.bashGate, true)
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	first.awaitBlocked(t)
	reopened := h.client.gate(&h.client.bashGate, true)
	h.relink(t)
	reopened.awaitBlocked(t)

	// Act: the shim's re-announcement no longer names the shell.
	h.reannounce(t, sessionStarted(""))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	h.awaitFree(t)
}

// TestAConcludedShellsHungOpenIsCancelled pins the teardown half: the open the
// conclusion found in flight is cancelled at once, not left for the watcher's
// close, and its completion is discarded.
func TestAConcludedShellsHungOpenIsCancelled(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	h.client.nextBashOpen(t)
	reopened := h.client.gate(&h.client.bashGate, true)
	h.relink(t)
	reopened.awaitBlocked(t)

	// Act.
	h.reannounce(t, sessionStarted(""))

	// Assert: the hung open returned (no goroutine is left holding it), and
	// the retirement names the open it cancelled.
	withinDeadline(t, "the concluded shell's hung open", h.client.settleOpens)
	if got := h.retiredRecord(t, "w-1")["watch"]; got != "opening" {
		t.Fatalf("retired watch state = %v, want opening", got)
	}
	if !h.hasRecord("info", "daemon.sessionwatcher.open_discarded") {
		t.Fatalf("records = %+v, want the cancelled open discarded", h.log.Records())
	}
}

// TestAConcludedShellsOpenStreamIsClosed pins the other teardown: a watch that
// was open when its item concluded is closed, so its reader ends.
func TestAConcludedShellsOpenStreamIsClosed(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	h.client.nextBashOpen(t)
	h.relink(t)
	open := h.client.nextBashOpen(t)
	h.client.settleOpens()

	// Act.
	h.reannounce(t, sessionStarted(""))

	// Assert.
	select {
	case <-open.stream.closed:
	case <-time.After(waitDeadline):
		t.Fatal("the concluded shell's open stream was never closed")
	}
}

// TestAShellConcludedBeforeAnyWatchOpenedLeavesTheLiveSet covers the shell the
// shim would not open a watch for at all: it was live from its announcement,
// and it leaves at its conclusion all the same.
func TestAShellConcludedBeforeAnyWatchOpenedLeavesTheLiveSet(t *testing.T) {
	// Arrange: every WatchBash is refused, so the shell never has a watch.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setBashErr(refusedOpenError("WatchBash", connect.CodeNotFound, "no rows for the handle yet"))
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	h.client.awaitRefusedOpen(t, "WatchBash")
	h.relink(t)
	h.client.awaitRefusedOpen(t, "WatchBash")
	h.client.settleOpens()

	// Act.
	h.reannounce(t, sessionStarted(""))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	if got := h.retiredRecord(t, "w-1")["watch"]; got != "none" {
		t.Fatalf("retired watch state = %v, want none", got)
	}
}

// TestAnUnaddressableSubagentIsCounted covers the announcement that names no
// agent to watch: it is recorded as the defect it is, and still counted.
func TestAnUnaddressableSubagentIsCounted(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "spawn-1", subagentKind("")))))

	// Assert.
	h.client.noAgentOpen(t)
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("w-1")}})
	if !h.hasRecord("error", "daemon.sessionwatcher.detached_subagent_unaddressable") {
		t.Fatalf("records = %+v, want the unaddressable announcement at ERROR", h.log.Records())
	}
}

// TestAnUnaddressableSubagentIsRetiredByItsUnitsTerminal is the other half: the
// spawn unit's own terminal concludes it, though no watch was ever opened.
func TestAnUnaddressableSubagentIsRetiredByItsUnitsTerminal(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "spawn-1", subagentKind("")))))

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("spawn-1", false)))))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	h.awaitFree(t)
}

// TestACreatedUnaddressableSubagentIsRetiredByItsUnitsTerminal covers a
// `created`-origin announcement, which records no unit join: the unit's
// terminal retires the handle by the contract's equality alone.
func TestACreatedUnaddressableSubagentIsRetiredByItsUnitsTerminal(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", withKind(createdWork("w-1", subagentWork("")), subagentKind("")))))

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("w-1", false)))))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
}

// TestAWatchStreamEndingDoesNotRetireALiveItem pins that a stream's end is not
// a conclusion: the item stays live whichever kind of watch ended.
func TestAWatchStreamEndingDoesNotRetireALiveItem(t *testing.T) {
	tests := []struct {
		name string
		// announce makes one item live and answers its open stream.
		announce func(t *testing.T, h *harness) interface{ fail(error) }
		// ended waits until the watcher has handled the stream's end: a
		// shell's watch is re-opened, and an agent's severs the link.
		ended func(t *testing.T, h *harness)
		want  LiveWorkSet
	}{
		{
			name: "a detached shell's WatchBash",
			announce: func(t *testing.T, h *harness) interface{ fail(error) } {
				h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
				return h.client.nextBashOpen(t).stream
			},
			ended: func(t *testing.T, h *harness) { h.client.nextBashOpen(t) },
			want:  LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{workID("w-1")}},
		},
		{
			name: "a detached subagent's WatchAgent",
			announce: func(t *testing.T, h *harness) interface{ fail(error) } {
				h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))
				return h.client.nextAgentOpenFor(t, "sub-1").stream
			},
			ended: func(t *testing.T, h *harness) { h.rec.until(t, "sidebar.OnLink") },
			want:  LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.quiet()
			stream := tt.announce(t, h)
			h.client.settleOpens()

			// Act.
			stream.fail(io.ErrUnexpectedEOF)
			tt.ended(t, h)

			// Assert.
			assertLiveWork(t, h.w.LiveWork(), tt.want)
		})
	}
}

// TestAReconciliationRetiresAStaleItemLoudly pins the reconciliation's record:
// an item the daemon held that the shim no longer names is an invariant
// violation, recorded at ERROR before the item is retired.
func TestAReconciliationRetiresAStaleItemLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))
	h.relink(t)

	// Act.
	h.reannounce(t, sessionStarted(""))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	if got := h.recordContext(t, "error", "daemon.sessionwatcher.live_work_stale")["work_id"]; got != "w-1" {
		t.Fatalf("stale record names %v, want w-1", got)
	}
}

// TestAReconciliationKeepsAnItemTheShimStillNames covers the ordinary case: a
// re-announcement naming the item leaves it live and records nothing.
func TestAReconciliationKeepsAnItemTheShimStillNames(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	h.relink(t)

	// Act.
	h.reannounce(t, sessionStarted("", createdWork("w-1", bashWork())))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{workID("w-1")}})
	if h.hasRecord("error", "daemon.sessionwatcher.live_work_stale") {
		t.Fatal("an item the shim still names was reported stale")
	}
}

// TestAReconciliationDoesNotJudgeAnItemAdmittedAfterTheWatchOpened pins the
// ordering guard: the re-announcement was read when its watch was opened, and
// an item announced after that may simply postdate it.
func TestAReconciliationDoesNotJudgeAnItemAdmittedAfterTheWatchOpened(t *testing.T) {
	// Arrange: the session watch in force was opened at start, before the
	// shell existed.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))

	// Act.
	h.reannounce(t, sessionStarted(""))

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{workID("w-1")}})
}

// TestFreenessIsToldWhenTheLastItemConcludes pins the edge: concluding one of
// two items leaves the workspace busy, and the last conclusion frees it.
func TestFreenessIsToldWhenTheLastItemConcludes(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "spawn-1", subagentKind("")))))
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-2", "spawn-2", subagentKind("")))))
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("spawn-1", false)))))
	h.noFree(t)

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("spawn-2", false)))))

	// Assert.
	h.awaitFree(t)
}

// TestAQueryDeathConcludesAnItemWithNoWatch covers the session-wide
// conclusion: an item that never had a watch leaves with the query.
func TestAQueryDeathConcludesAnItemWithNoWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "spawn-1", subagentKind("")))))

	// Act.
	h.sendSessionUpdate(t, queryDiedUpdate())
	h.sendSessionUpdate(t, compactingUpdate())
	h.rec.until(t, "footer.OnSessionUpdate")

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
	if got := h.retiredRecord(t, "w-1")["conclusion"]; got != string(concludedQueryDied) {
		t.Fatalf("conclusion = %v, want %s", got, concludedQueryDied)
	}
}

// ---- a departure settles the departed shim's work ----

// departedLive is the live work every departure test starts with: a monitor
// and a shell the shim announced as already live.
func departedLive() *conversationv1.SessionStarted {
	return sessionStarted("", createdWork("act-1", monitorWork()), createdWork("w-1", bashWork()))
}

// lastLiveWorkTo answers the last live-work set SINK was handed among events.
func lastLiveWorkTo(events []event, sink string) (LiveWorkSet, bool) {
	var last *LiveWorkSet
	for _, e := range events {
		if e.sink == sink && e.method == "OnLiveWorkChanged" && e.live != nil {
			last = e.live
		}
	}
	if last == nil {
		return LiveWorkSet{}, false
	}
	return *last, true
}

// departWith drives one departure edge and answers every view call it made.
func departWith(t *testing.T, h *harness, edge string) []event {
	t.Helper()
	switch edge {
	case "link_dead":
		h.client.links <- shimclient.LinkDead
		h.awaitDeparture(t)
	case "link_dead_in_stand_down":
		h.client.StandDown()
		h.client.links <- shimclient.LinkDead
		h.awaitDeparture(t)
	case "close_ending":
		h.w.SessionEnding("a test ends the session")
		closeWatcher(t, h)
	case "close_reaped":
		h.client.setReaped(shimclient.ExitInfo{PID: 4242, Code: -1, Signal: "killed"})
		closeWatcher(t, h)
	default:
		t.Fatalf("unknown departure edge %q", edge)
	}
	return h.rec.drain()
}

// TestADepartureEmptiesTheLiveSet: whichever edge establishes that the shim is
// gone, nothing it ran is live any more.
func TestADepartureEmptiesTheLiveSet(t *testing.T) {
	for _, edge := range []string{"link_dead", "link_dead_in_stand_down", "close_ending", "close_reaped"} {
		t.Run(edge, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: departedLive()})
			h.quiet()

			// Act.
			departWith(t, h, edge)

			// Assert.
			assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{})
		})
	}
}

// TestADepartureTellsEveryViewTheWorkEnded is the owner's report (2026-10-02):
// work a restart killed stayed listed in the webapp's expanded footer. The
// footer, the roster and the feed are each handed the emptied set.
func TestADepartureTellsEveryViewTheWorkEnded(t *testing.T) {
	for _, sink := range []string{"footer", "sidebar", "feed"} {
		t.Run(sink, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: departedLive()})
			h.quiet()

			// Act.
			events := departWith(t, h, "link_dead")

			// Assert.
			got, told := lastLiveWorkTo(events, sink)
			if !told {
				t.Fatalf("%s was never handed a live-work set at the departure; saw %v", sink, names(events))
			}
			assertLiveWork(t, got, LiveWorkSet{})
		})
	}
}

// TestADepartureRecordsEachItemRetiredAsDeparted names the conclusion, so the
// log says why each item left the set.
func TestADepartureRecordsEachItemRetiredAsDeparted(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: departedLive()})
	h.quiet()

	// Act.
	departWith(t, h, "close_ending")

	// Assert.
	if got := h.retiredRecord(t, "w-1")["conclusion"]; got != string(concludedDeparted) {
		t.Fatalf("the shell's conclusion = %v, want %q", got, concludedDeparted)
	}
}

// TestADepartureWithNothingLivePublishesNothing: an idle shim's departure has
// no work to settle, so no view is told a set it already holds.
func TestADepartureWithNothingLivePublishesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	events := departWith(t, h, "close_ending")

	// Assert.
	if _, told := lastLiveWorkTo(events, "footer"); told {
		t.Fatalf("an idle departure republished the live set; saw %v", names(events))
	}
}

// TestClosingTheWatchOfARunningShimKeepsItsLiveWork: the daemon's own exit and
// a handover stop WATCHING a shim that keeps running, so its work is not ended
// and no view is told it was.
func TestClosingTheWatchOfARunningShimKeepsItsLiveWork(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: departedLive()})
	h.quiet()

	// Act.
	closeWatcher(t, h)

	// Assert.
	if _, told := lastLiveWorkTo(h.rec.drain(), "footer"); told {
		t.Fatalf("closing the watch of a running shim republished its live set")
	}
}

// TestADisplacedWatchersDepartureRepublishesNothing: a newer watcher of the
// same workspace already published its own set, and the old one's departure
// must not overwrite it with the old ledger's emptied one.
func TestADisplacedWatchersDepartureRepublishesNothing(t *testing.T) {
	// Arrange: the old watcher holds live work; a newer one starts on the
	// same workspace before the old one is closed.
	old := newHarness(t, Session{Started: departedLive()})
	old.quiet()
	newHarness(t, Session{Started: sessionStarted("")})

	// Act.
	events := departWith(t, old, "close_reaped")

	// Assert.
	if _, told := lastLiveWorkTo(events, "footer"); told {
		t.Fatalf("a displaced watcher's departure republished over the newer watcher's set; saw %v", names(events))
	}
}

// TestADisplacedWatchersDepartureStillEmptiesItsOwnLedger: what the views are
// told is the newer watcher's business, but the old ledger's work is ended.
func TestADisplacedWatchersDepartureStillEmptiesItsOwnLedger(t *testing.T) {
	// Arrange.
	old := newHarness(t, Session{Started: departedLive()})
	old.quiet()
	newHarness(t, Session{Started: sessionStarted("")})

	// Act.
	departWith(t, old, "close_reaped")

	// Assert.
	assertLiveWork(t, old.w.LiveWork(), LiveWorkSet{})
}
