package sessionwatcher

import (
	"errors"
	"reflect"
	"sync"
	"testing"
	"time"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

func TestWatcherStateTransitionsRecordTheirBeforeAndAfter(t *testing.T) {
	tests := []struct {
		name      string
		operation string
		state     string
		before    any
		after     any
		legacyKey string
		legacy    any
		act       func(*harness)
	}{
		{
			name: "an output route leaves the root feed", operation: "daemon.sessionwatcher.set_output_address",
			state: "output_feed_root", before: true, after: false, legacyKey: "root", legacy: false,
			act: func(h *harness) { h.w.SetOutputAddress(&OutputAddress{Feed: feedFor("sub-1")}) },
		},
		{
			name: "the main agent is named", operation: "daemon.sessionwatcher.main_agent",
			state: "main_agent", before: "", after: "main-9", legacyKey: "agent_id", legacy: "main-9",
			act: func(h *harness) { h.w.SetMainAgent(agentID("main-9")) },
		},
		{
			name: "a turn enters flight", operation: "daemon.sessionwatcher.turn_opening",
			state: "turn_in_flight", before: "", after: "turn-9", legacyKey: "turn_id", legacy: "turn-9",
			act: func(h *harness) { h.w.OnTurnOpening("ws-1", "turn-9") },
		},
		{
			name: "a refused turn leaves flight", operation: "daemon.sessionwatcher.turn_open_failed",
			state: "turn_in_flight", before: "turn-9", after: "", legacyKey: "turn_id", legacy: "turn-9",
			act: func(h *harness) {
				h.w.OnTurnOpening("ws-1", "turn-9")
				h.w.OnTurnOpenFailed("ws-1", "turn-9")
			},
		},
		{
			name: "the session-ending latch rises", operation: "daemon.sessionwatcher.state_transition",
			state: "session_ended", before: false, after: true,
			act: func(h *harness) { h.w.SessionEnding("test shutdown") },
		},
		{
			name: "the connected link starts redialing", operation: "daemon.sessionwatcher.state_transition",
			state: "link", before: int(shimclient.LinkConnected), after: int(shimclient.LinkRedialing),
			act: func(h *harness) {
				h.w.mu.Lock()
				h.w.setLinkLocked(shimclient.LinkRedialing)
				h.w.mu.Unlock()
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			beforeRecords := len(h.log.Records())

			// Act.
			tt.act(h)

			// Assert.
			for _, record := range h.log.Records()[beforeRecords:] {
				if record.Level == "debug" && record.Operation == tt.operation && record.Context["state"] == tt.state &&
					reflect.DeepEqual(record.Context["before"], tt.before) && reflect.DeepEqual(record.Context["after"], tt.after) &&
					(tt.legacyKey == "" || reflect.DeepEqual(record.Context[tt.legacyKey], tt.legacy)) {
					return
				}
			}
			t.Fatalf("records = %+v, want %s state %s before=%v after=%v %s=%v", h.log.Records()[beforeRecords:], tt.operation, tt.state, tt.before, tt.after, tt.legacyKey, tt.legacy)
		})
	}
}

// TestStartOpensTheSessionAndMainWatches covers the opening: WatchSession goes
// up immediately and the main agent's watch is addressed by an UNSET target,
// which the shim resolves to the session's prompt thread.
func TestStartOpensTheSessionAndMainWatches(t *testing.T) {
	// Arrange / Act.
	h := newHarness(t, Session{Started: sessionStarted("")})

	// Assert.
	if h.session == nil {
		t.Fatal("WatchSession was not opened")
	}
	if h.mainReq.Target != nil {
		t.Fatalf("the main watch was addressed to %q, want an unset target", h.mainReq.GetTarget().GetValue())
	}
	if h.mainReq.GetPageSize() == 0 {
		t.Fatal("the main watch was opened with no page budget")
	}
	if !h.hasRecord("info", "daemon.sessionwatcher.start") {
		t.Fatal("the watch-fleet bring-up has no info lifecycle record")
	}
}

// TestStartCatchesUpFromThePersistedPointer covers the caller's persisted
// mark: passing it is what makes the opening page a catch-up instead of a
// repaint of history the daemon already holds.
func TestStartCatchesUpFromThePersistedPointer(t *testing.T) {
	// Arrange / Act.
	h := newHarness(t, Session{
		Started:          sessionStarted(""),
		MainKnownThrough: &conversationv1.HistoryPointer{Value: "ptr-main"},
	})

	// Assert.
	if h.mainReq.GetKnownThrough().GetValue() != "ptr-main" {
		t.Fatalf("known_through = %q, want ptr-main", h.mainReq.GetKnownThrough().GetValue())
	}
}

// TestStartOpensOneWatchPerLiveWorkKind covers the opening LEVEL: a daemon
// that restarted was not there for the announcements, so it is told the
// membership once and opens the matching stream for each item by kind.
func TestStartOpensOneWatchPerLiveWorkKind(t *testing.T) {
	tests := []struct {
		name       string
		item       *conversationv1.AgentDetachedWork
		wantAgent  string
		wantShell  bool
		wantLive   LiveWorkSet
		wantKicked bool
	}{
		{
			name:      "a detached subagent is watched on its created agent id",
			item:      createdWork("w-1", subagentWork("sub-1")),
			wantAgent: "sub-1",
			wantLive:  LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}},
		},
		{
			name:      "a detached shell is watched by its handle",
			item:      createdWork("w-1", bashWork()),
			wantShell: true,
			wantLive:  LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{workID("w-1")}},
		},
		{
			name:     "a monitor is live with no stream at all",
			item:     createdWork("w-1", monitorWork()),
			wantLive: LiveWorkSet{Monitors: []*conversationv1.DetachedWorkId{workID("w-1")}},
		},
		{
			name:       "a workflow is kicked, never watched and never live",
			item:       createdWork("w-1", workflowWork()),
			wantLive:   LiveWorkSet{},
			wantKicked: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act.
			h := newHarness(t, Session{Started: sessionStarted("", tt.item)})

			// Assert.
			if tt.wantAgent != "" {
				open := h.client.nextAgentOpen(t)
				if open.req.GetTarget().GetValue() != tt.wantAgent {
					t.Fatalf("watched %q, want %q", open.req.GetTarget().GetValue(), tt.wantAgent)
				}
			} else {
				h.client.noAgentOpen(t)
			}
			if tt.wantShell {
				h.client.nextBashOpen(t)
			} else {
				h.client.noBashOpen(t)
			}
			assertLiveWork(t, h.w.LiveWork(), tt.wantLive)
			if tt.wantKicked && !h.hasRecord("info", "daemon.sessionwatcher.workflow_kicked") {
				t.Fatal("a workflow was not recorded as kicked")
			}
		})
	}
}

// TestDetachedAnnouncementOpensExactlyOneWatch covers the eager leg: the
// announcement opens the watch, and a REPEATED announcement (a re-announcement
// after a restart states the original instant again) must not open a second.
func TestDetachedAnnouncementOpensExactlyOneWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	announcement := createdWork("w-1", subagentWork("sub-1"))

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", announcement)))
	h.client.nextAgentOpen(t)
	h.route(h.main, entryFrame(frameDetached("main-1", announcement)))

	// Assert.
	h.client.noAgentOpen(t)
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}})
}

// TestDetachedOriginResolvesItsKindFromTheUnit covers the `detached` origin,
// which names only the in-turn unit the work used to be: the kind comes from
// what that unit's own activity already taught the watcher.
func TestDetachedOriginResolvesItsKindFromTheUnit(t *testing.T) {
	tests := []struct {
		name      string
		activity  *conversationv1.AgentActivity
		wantAgent string
		wantShell bool
	}{
		{
			name:      "a spawn unit resolves to its created agent's watch",
			activity:  subagentActivity("act-1", "sub-1"),
			wantAgent: "sub-1",
		},
		{
			name:      "a shell unit resolves to a bash watch",
			activity:  bashActivity("act-1"),
			wantShell: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the unit streams first, which is what teaches the kind.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.quiet()
			h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(tt.activity))))

			// Act.
			h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "act-1"))))

			// Assert.
			if tt.wantAgent != "" {
				open := h.client.nextAgentOpen(t)
				if open.req.GetTarget().GetValue() != tt.wantAgent {
					t.Fatalf("watched %q, want %q", open.req.GetTarget().GetValue(), tt.wantAgent)
				}
			}
			if tt.wantShell {
				open := h.client.nextBashOpen(t)
				if open.work.GetValue() != "w-1" {
					t.Fatalf("watched shell %q, want w-1", open.work.GetValue())
				}
			}
		})
	}
}

// TestDetachedOriginForAnUnknownUnitIsAnError covers the gap honestly: an
// announcement naming a unit this daemon never saw cannot be resolved to a
// kind, and guessing one would open the wrong stream.
func TestDetachedOriginForAnUnknownUnitIsAnError(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "never-seen"))))

	// Assert.
	h.client.noAgentOpen(t)
	h.client.noBashOpen(t)
	if !h.hasRecord("error", "daemon.sessionwatcher.detached_kind_unknown") {
		t.Fatal("an unresolvable detached announcement was not recorded as an error")
	}
}

// TestAgentTerminalReapsItsWatch covers the reap: the open set IS the live
// set, so a subagent's terminal has to close its watch and leave the set.
func TestAgentTerminalReapsItsWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", subagentWork("sub-1")))})
	open := h.client.nextAgentOpen(t)
	h.quiet()

	// Act.
	h.routeReaping(open.stream, entryFrame(frameSuccess("sub-1", completed())))

	// Assert.
	if !h.w.LiveWork().Empty() {
		t.Fatalf("the reaped subagent is still live: %+v", h.w.LiveWork())
	}
}

// TestBashTerminalReapsItsWatch covers the same for a detached shell, whose
// terminal is the bash unit's own success arm.
func TestBashTerminalReapsItsWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", bashWork()))})
	h.client.nextBashOpen(t)
	h.quiet()
	entry := h.shellWatchFor("w-1")

	// Act.
	h.routeNow(func(w *watcher) {
		w.routeBashLocked(entry, &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}},
		})
	})

	// Assert.
	if !h.w.LiveWork().Empty() {
		t.Fatalf("the reaped shell is still live: %+v", h.w.LiveWork())
	}
}

// TestMonitorIsRetiredByItsOwnTerminal covers the one live item with no
// stream: a monitor can only be retired by its activity's terminal arm.
func TestMonitorIsRetiredByItsOwnTerminal(t *testing.T) {
	// Arrange: the monitor detaches from a unit, which is what ties the
	// handle to the activity that will end it.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(monitorActivity("act-1", false)))))
	h.route(h.main, entryFrame(frameDetached("main-1", detachedWork("w-1", "act-1"))))
	if h.w.LiveWork().Empty() {
		t.Fatal("the announced monitor was not live")
	}

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(monitorActivity("act-1", true)))))

	// Assert.
	if !h.w.LiveWork().Empty() {
		t.Fatalf("the ended monitor is still live: %+v", h.w.LiveWork())
	}
}

// TestACreatedMonitorIsRetiredByItsEndedFrame covers the re-adopted monitor:
// it was never announced on this watch, so nothing ties an activity to its
// handle — but DetachedWorkId.value IS the unit's AgentActivityId.value, so
// the monitor's own ended frame retires it.
func TestACreatedMonitorIsRetiredByItsEndedFrame(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("act-1", monitorWork()))})
	h.quiet()
	if h.w.LiveWork().Empty() {
		t.Fatal("the re-adopted monitor was not live")
	}

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(monitorActivity("act-1", true)))))

	// Assert.
	if !h.w.LiveWork().Empty() {
		t.Fatalf("the ended monitor is still live: %+v", h.w.LiveWork())
	}
}

// TestACreatedMonitorIsRetiredByItsFailureFrame covers the other terminal arm:
// a watch that could not be armed is over, so it leaves the live set too.
func TestACreatedMonitorIsRetiredByItsFailureFrame(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("act-1", monitorWork()))})
	h.quiet()
	if h.w.LiveWork().Empty() {
		t.Fatal("the re-adopted monitor was not live")
	}

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(monitorFailedActivity("act-1")))))

	// Assert.
	if !h.w.LiveWork().Empty() {
		t.Fatalf("the failed monitor is still live: %+v", h.w.LiveWork())
	}
}

// TestTheRosterHearsTheEmptiedLiveWorkSet covers the roster's half of the
// live-work seam: the set the watcher republishes when the last detached item
// is reaped reaches the sidebar, which is what retires `idle_async`.
func TestTheRosterHearsTheEmptiedLiveWorkSet(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("act-1", monitorWork()))})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(monitorFailedActivity("act-1")))))

	// Assert.
	e := requireEvent(t, got, "sidebar.OnLiveWorkChanged")
	if e.live == nil || !e.live.Empty() {
		t.Fatalf("the roster was told %+v, want an empty live-work set", e.live)
	}
}

// TestTheFooterHearsTheEmptiedLiveWorkSet covers the FOOTER's half of the same
// seam. The strip's `background` arm and the roster's `idle_async` arm are two
// renderings of one fact, so the set that retires one must reach the other: a
// footer told nothing decided liveness from its own frame ledger, and reported
// a background task for work that had ended.
func TestTheFooterHearsTheEmptiedLiveWorkSet(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("act-1", monitorWork()))})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(monitorFailedActivity("act-1")))))

	// Assert.
	e := requireEvent(t, got, "footer.OnLiveWorkChanged")
	if e.live == nil || !e.live.Empty() {
		t.Fatalf("the footer was told %+v, want an empty live-work set", e.live)
	}
}

// TestTheFooterHearsAnAdoptedItemWithNoAnnouncer pins the premise the footer's
// launch focus rests on: an item the session says is ALREADY live reaches the
// footer with no announcing agent, and before the set that lists it, so the
// footer can tell an adoption from a launch.
func TestTheFooterHearsAnAdoptedItemWithNoAnnouncer(t *testing.T) {
	// Arrange / Act: the watcher starts on a session with one live shell.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", bashWork()))})
	h.client.nextBashOpen(t)

	// Assert.
	got := h.rec.drain()
	announced, ok := find(got, "footer.OnDetachedWork")
	if !ok {
		t.Fatalf("the footer never heard the adopted item; saw %v", names(got))
	}
	if announced.agent != "" {
		t.Fatalf("the adopted item reached the footer announced by %q, want no announcer", announced.agent)
	}
	for _, e := range got {
		if e.name() == "footer.OnDetachedWork" {
			break
		}
		if e.name() == "footer.OnLiveWorkChanged" && e.live != nil && len(e.live.Shells) > 0 {
			t.Fatalf("the footer was handed a set listing the adopted item before its announcement; saw %v", names(got))
		}
	}
}

// TestBothViewSinksHearOneLiveWorkSet is the fan-out itself: the roster and the
// footer are handed the SAME value on the same edge, which is what makes a
// disagreement between the two surfaces unrepresentable.
func TestBothViewSinksHearOneLiveWorkSet(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act: one detached item is announced.
	got := h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", monitorWork()))))

	// Assert.
	roster := requireEvent(t, got, "sidebar.OnLiveWorkChanged")
	footer := requireEvent(t, got, "footer.OnLiveWorkChanged")
	if roster.live == nil || footer.live == nil {
		t.Fatalf("a view sink was handed no set: roster %+v, footer %+v", roster.live, footer.live)
	}
	assertLiveWork(t, *roster.live, *footer.live)
}

// TestTheFeedHearsTheLiveWorkSetTheFooterHears covers the feed's half of the
// fan-out: a detached shell's bubble settles when its run leaves the set, so
// the feed must be handed the same value the footer is.
func TestTheFeedHearsTheLiveWorkSetTheFooterHears(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act: one detached item is announced.
	got := h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", monitorWork()))))

	// Assert.
	footer := requireEvent(t, got, "footer.OnLiveWorkChanged")
	feed := requireEvent(t, got, "feed.OnLiveWorkChanged")
	if footer.live == nil || feed.live == nil {
		t.Fatalf("a sink was handed no set: footer %+v, feed %+v", footer.live, feed.live)
	}
	assertLiveWork(t, *footer.live, *feed.live)
}

// TestATerminalForUnannouncedWorkChangesNothing covers the levels rule: the
// live set is the announcements and the terminals of what was announced, and
// an unpaired terminal edge must not invent or retire membership.
func TestATerminalForUnannouncedWorkChangesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", subagentWork("sub-1")))})
	h.client.nextAgentOpen(t)
	h.quiet()

	// Act: a terminal for a subagent that was never announced.
	got := h.route(h.main, entryFrame(frameSuccess("sub-unknown", completed())))

	// Assert.
	if _, changed := find(got, "lifecycle.OnLiveWorkChanged"); changed {
		t.Fatal("an unpaired terminal edge changed the live set")
	}
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Agents: []*conversationv1.AgentId{agentID("sub-1")}})
}

// TestFree covers freeness, which is the whole reason the watcher tracks both
// halves: no turn in flight AND an empty live set.
func TestFree(t *testing.T) {
	tests := []struct {
		name    string
		session Session
		want    bool
	}{
		{
			name:    "an idle session with nothing detached is free",
			session: Session{Started: sessionStarted("")},
			want:    true,
		},
		{
			name:    "a turn in flight is not free",
			session: Session{Started: sessionStarted("turn-1")},
			want:    false,
		},
		{
			name:    "a live subagent is not free",
			session: Session{Started: sessionStarted("", createdWork("w-1", subagentWork("sub-1")))},
			want:    false,
		},
		{
			name:    "a live monitor is not free even though it has no stream",
			session: Session{Started: sessionStarted("", createdWork("w-1", monitorWork()))},
			want:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act.
			h := newHarness(t, tt.session)

			// Assert.
			if got := h.w.Free(); got != tt.want {
				t.Fatalf("Free() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestStartPublishesTheOpeningFacts covers what a started session owes its
// views before a single frame arrives: its identity, its roster row and the
// link it is serving on.
func TestStartPublishesTheOpeningFacts(t *testing.T) {
	// Arrange / Act.
	h := newHarness(t, Session{Started: sessionStarted("")})
	got := h.quietAll()

	// Assert.
	assertNames(t, got, []string{
		"topbar.OnSessionStarted", "sidebar.OnSessionStarted",
		"footer.OnLink", "topbar.OnLink", "sidebar.OnLink", "lifecycle.OnLinkChanged",
		"lifecycle.OnLiveWorkChanged", "sidebar.OnLiveWorkChanged", "footer.OnLiveWorkChanged", "feed.OnLiveWorkChanged",
	})
	if !h.w.Connected() {
		t.Fatal("a started session is not connected")
	}
}

// TestATurnDrivenToItsTerminalOpensTheMainWatchExactlyOnce covers the count
// itself: the main agent's book is watched by ONE standing WatchAgent for the
// life of the session, and a turn running and ending on it is not an occasion
// to open another.
//
// A SECOND TAIL ON ONE BOOK IS A STALL, not merely waste. The shim registers
// every open WatchAgent for its teardown and concludes each one through the
// book's head; a tail nobody drains cannot reach that head, so the shim's
// `KillSession` spends its whole conclusion budget on it and the daemon's stop
// waits inside that call.
func TestATurnDrivenToItsTerminalOpensTheMainWatchExactlyOnce(t *testing.T) {
	// Arrange: the session's one main watch, and the turn it is about to run.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act: the turn's prompt, one activity, and the terminal that ends it.
	h.route(h.main, entryPrompt("turn-1", "main-1"))
	h.route(h.main, entryFrameAt(frameUpdate("main-1", activityUpdate(readActivity("act-1"))), "ptr-42"))
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	h.client.noAgentOpen(t)
}

// TestSeveredLinkReopensFromTheTrackedPointer covers the transport failure:
// only the consumer knows a stream should still be open, so a WatchSession that
// ends while the session lives severs the link, and the return re-opens the
// fleet catching up from the newest pointer each watch was served.
func TestSeveredLinkReopensFromTheTrackedPointer(t *testing.T) {
	// Arrange: a frame first, so the watcher holds a pointer to catch up from.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.main.send(t, entryFrameAt(frameUpdate("main-1", activityUpdate(readActivity("act-1"))), "ptr-42"))
	h.rec.until(t, "footer.OnActivity")

	// Act: the session stream dies, then the client reports the link back.
	h.session.fail(errors.New("connection reset"))
	h.rec.until(t, "sidebar.OnLink")
	if h.w.Connected() {
		t.Fatal("the link is connected while a standing stream is down")
	}
	h.client.links <- shimclient.LinkConnected

	// Assert.
	h.client.nextSessionOpen(t)
	reopened := h.client.nextAgentOpen(t)
	if reopened.req.GetKnownThrough().GetValue() != "ptr-42" {
		t.Fatalf("re-opened with known_through %q, want ptr-42", reopened.req.GetKnownThrough().GetValue())
	}
	if !h.hasRecord("error", "daemon.sessionwatcher.watch_session") {
		t.Fatal("the transport failure was not recorded as an error")
	}
	if !h.hasRecord("warn", "daemon.sessionwatcher.reopen") {
		t.Fatal("the re-open was not warned about")
	}
}

// TestABringUpLinkReplayWithEveryStreamStandingDoesNotReopen covers the
// connectivity feed's HISTORY: the client publishes dialing and then connected
// during bring-up, and the watcher is created afterwards holding a connected
// link. Replaying those transitions is not a link that broke, so nothing is
// re-opened -- a re-open here tears down the streams bring-up just
// established, and the session is then left with no standing watch at all.
func TestABringUpLinkReplayWithEveryStreamStandingDoesNotReopen(t *testing.T) {
	// Arrange: a started session whose every stream is standing.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act: the bring-up transitions arrive after the watcher already exists.
	// Neither publishes a link: the replayed dial is refused as stale and the
	// `connected` that follows is the state already held, so the replay is
	// observed through the watcher's own record of refusing it.
	h.client.links <- shimclient.LinkDialing
	h.awaitRecord(t, "debug", "daemon.sessionwatcher.link_replay")
	h.client.links <- shimclient.LinkConnected
	h.quiet()

	// Assert.
	h.client.noAgentOpen(t)
	if h.hasRecord("warn", "daemon.sessionwatcher.reopen") {
		t.Fatal("the fleet was re-opened on a link replay with every stream standing")
	}
}

// TestABringUpLinkReplayNeverWalksTheLinkBackToDialing pins the other half of
// the replay: the watcher is born on the connected link, so the bring-up's
// own `dialing` arriving late must not be published as a transition. It was,
// and the roster painted `init` over a workspace whose turn was already
// accepted (a headless run's cold start measured `submitting` -> `init` ->
// `submitting` within 1ms of the first StartTurn).
func TestABringUpLinkReplayNeverWalksTheLinkBackToDialing(t *testing.T) {
	// Arrange: a started session, already published as connected.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act: the bring-up's own first dial arrives after the fact.
	h.client.links <- shimclient.LinkDialing
	h.awaitRecord(t, "debug", "daemon.sessionwatcher.link_replay")
	events := h.quietAll()

	// Assert: no sink was told the link went anywhere, and the watcher still
	// answers connected.
	for _, e := range events {
		if e.method == "OnLink" {
			t.Fatalf("%s published link %d on a stale first-dial replay; the link must stay as it was", e.name(), e.link)
		}
	}
	if !h.w.Connected() {
		t.Fatalf("Link() = %d after a stale first-dial replay, want connected", h.w.Link())
	}
}

// TestReopenRestoresEveryDetachedWatch covers the fleet, not just the main
// watch: a live subagent whose stream died has to be followed again or its
// bubble stops growing with no terminal ever arriving.
func TestReopenRestoresEveryDetachedWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", subagentWork("sub-1")))})
	h.client.nextAgentOpen(t)
	h.quiet()

	// Act.
	h.session.fail(errors.New("connection reset"))
	h.rec.until(t, "sidebar.OnLink")
	h.client.links <- shimclient.LinkConnected

	// Assert: the session, the main watch and the subagent all come back.
	h.client.nextSessionOpen(t)
	first := h.client.nextAgentOpen(t)
	second := h.client.nextAgentOpen(t)
	targets := map[string]bool{
		first.req.GetTarget().GetValue():  true,
		second.req.GetTarget().GetValue(): true,
	}
	if !targets[""] || !targets["sub-1"] {
		t.Fatalf("re-opened %v, want the main watch and sub-1", targets)
	}
}

// TestStreamEndAfterTheSessionDiedIsNotAFailure covers the one legal end: once
// query_died has been seen the session is over, and reporting its streams'
// ends as transport failures would sever a link that has nothing to reconnect.
func TestStreamEndAfterTheSessionDiedIsNotAFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(queryDiedUpdate()) })

	// Act.
	h.main.Close()

	// Assert: the watcher stays connected and logs the end as ordinary.
	h.awaitRecord(t, "debug", "daemon.sessionwatcher.stream_closed")
	if !h.w.Connected() {
		t.Fatal("a stream ending with its dead session severed the link")
	}
}

// TestStreamEndAfterTheDaemonEndedTheSessionIsNotAFailure covers the OTHER
// legal end: the daemon itself is standing the session down, so the streams
// the shim closes on its way out are that stand-down, never a severing to
// redial.
func TestStreamEndAfterTheDaemonEndedTheSessionIsNotAFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.w.SessionEnding("the daemon is ending the session")

	// Act.
	h.main.Close()

	// Assert.
	h.awaitRecord(t, "debug", "daemon.sessionwatcher.stream_closed")
	if !h.w.Connected() {
		t.Fatal("a stream ending with the daemon's own stand-down severed the link")
	}
}

// TestCloseClosesEveryStream covers the teardown: it ends every watch and
// KILLS NOTHING, because attaching created nothing.
func TestCloseClosesEveryStream(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", bashWork()))})
	bash := h.client.nextBashOpen(t)
	h.quiet()

	// Act.
	if err := h.w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if !h.session.isClosed() {
		t.Fatal("the session stream was left open")
	}
	if !h.main.isClosed() {
		t.Fatal("the main agent stream was left open")
	}
	if !bash.stream.isClosed() {
		t.Fatal("the detached shell's stream was left open")
	}
	if !h.hasRecord("info", "daemon.sessionwatcher.close") {
		t.Fatal("the watch-fleet shutdown has no info lifecycle record")
	}
}

// TestCloseIsIdempotent covers the second call: a workspace teardown and a
// drain can both reach it, and the second must not panic on closed streams.
func TestCloseIsIdempotent(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})

	// Act.
	first := h.w.Close()
	second := h.w.Close()

	// Assert.
	if first != nil || second != nil {
		t.Fatalf("Close returned %v then %v, want nil twice", first, second)
	}
}

// TestSetMainAgentNamesTheTurnsAgent covers the authoritative source: the
// prompt queue hands over StartTurnSuccess.prompt.agent, and that is what
// makes a terminal attributable to the turn.
func TestSetMainAgentNamesTheTurnsAgent(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.quiet()

	// Act.
	h.w.SetMainAgent(agentID("main-1"))
	got := h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	if _, ok := find(got, "lifecycle.OnTurnEnded"); !ok {
		t.Fatalf("the turn did not end: %v", names(got))
	}
}

// TestMainAgentIsRecoveredFromTheOpeningPage covers an adoption with a turn
// already in flight: no StartTurn has happened in this process, and the
// opening page's prompt is the only thing that names the recipient.
func TestMainAgentIsRecoveredFromTheOpeningPage(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.quiet()

	// Act.
	h.route(h.main, pageFrame(promptEntry("ptr-1", "turn-1", "main-1")))
	got := h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	if _, ok := find(got, "lifecycle.OnTurnEnded"); !ok {
		t.Fatalf("the turn did not end: %v", names(got))
	}
}

// TestOnTurnOpenedTracksTheTurnAndItsPage covers the queue's hand-over: it
// names the main agent, records the turn, states the turn-open edge to BOTH
// surfaces that have no other source for it (the footer's clock and the feed's
// running turn), and feeds StartTurnSuccess's page through the opening-page
// path without mirroring the prompt a second time.
func TestOnTurnOpenedTracksTheTurnAndItsPage(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.w.OnTurnOpened("ws-1", &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-9"},
		Agent: agentID("main-1"),
	}, &conversationv1.HistoryPage{})
	got := h.drainNow()

	// Assert.
	assertNames(t, got, []string{"footer.OnTurnOpened", "feed.OnTurnOpened", "feed.OnHistoryPage", "footer.OnHistoryPage"})
	turn := h.w.TurnInFlight()
	if turn == nil || *turn != ids.TurnID("turn-9") {
		t.Fatalf("turn in flight = %v, want turn-9", turn)
	}
}

// TestOnTurnOpeningRecordsTheTurnBeforeTheShimTakesIt covers the pre-record:
// the caller has not yet dispatched StartTurn, and the turn must already stand
// in flight so a terminal arriving first is attributable.
func TestOnTurnOpeningRecordsTheTurnBeforeTheShimTakesIt(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.w.OnTurnOpening("ws-1", ids.TurnID("turn-9"))

	// Assert.
	turn := h.w.TurnInFlight()
	if turn == nil || *turn != ids.TurnID("turn-9") {
		t.Fatalf("turn in flight = %v, want turn-9", turn)
	}
}

// TestATerminalAheadOfTheAcceptanceStillEndsTheTurn covers the race the
// pre-record exists for: the shim put the turn's terminal on the agent stream
// before StartTurn's response was processed, and the turn must still end.
func TestATerminalAheadOfTheAcceptanceStillEndsTheTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.w.SetMainAgent(agentID("main-1"))
	h.w.OnTurnOpening("ws-1", ids.TurnID("turn-9"))

	// Act.
	got := h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	if _, ok := find(got, "lifecycle.OnTurnEnded"); !ok {
		t.Fatalf("the turn did not end: %v", names(got))
	}
}

// TestAnAcceptanceDoesNotReopenATurnThatAlreadyEnded covers the other half of
// that race: the late acceptance must not stand the dead turn back up, because
// no edge is left to take it down again.
func TestAnAcceptanceDoesNotReopenATurnThatAlreadyEnded(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.w.SetMainAgent(agentID("main-1"))
	h.w.OnTurnOpening("ws-1", ids.TurnID("turn-9"))
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Act.
	h.w.OnTurnOpened("ws-1", &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-9"},
		Agent: agentID("main-1"),
	}, &conversationv1.HistoryPage{})

	// Assert.
	if turn := h.w.TurnInFlight(); turn != nil {
		t.Fatalf("turn in flight = %v, want none: the turn had already ended", *turn)
	}
}

// TestOnTurnOpenFailedRetiresTheRecordedTurn covers the refusal: the shim never
// took the turn, so it must not stand in flight and block the next submission.
func TestOnTurnOpenFailedRetiresTheRecordedTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.w.OnTurnOpening("ws-1", ids.TurnID("turn-9"))

	// Act.
	h.w.OnTurnOpenFailed("ws-1", ids.TurnID("turn-9"))

	// Assert.
	if turn := h.w.TurnInFlight(); turn != nil {
		t.Fatalf("turn in flight = %v, want none after the shim refused it", *turn)
	}
}

// TestOnTurnOpenFailedLeavesALaterTurnAlone covers the stale refusal: it clears
// only the turn it names, never whatever is running now.
func TestOnTurnOpenFailedLeavesALaterTurnAlone(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.w.OnTurnOpening("ws-1", ids.TurnID("turn-10"))

	// Act.
	h.w.OnTurnOpenFailed("ws-1", ids.TurnID("turn-9"))

	// Assert.
	turn := h.w.TurnInFlight()
	if turn == nil || *turn != ids.TurnID("turn-10") {
		t.Fatalf("turn in flight = %v, want turn-10 left standing", turn)
	}
}

// TestOnTurnOpeningRefusesAnotherWorkspacesTurn covers the same invariant
// OnTurnOpened holds: a watcher is one workspace's.
func TestOnTurnOpeningRefusesAnotherWorkspacesTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.w.OnTurnOpening("ws-other", ids.TurnID("turn-9"))

	// Assert.
	if h.w.TurnInFlight() != nil {
		t.Fatal("another workspace's turn was adopted")
	}
	if !h.hasRecord("error", "daemon.sessionwatcher.turn_opening_foreign") {
		t.Fatal("a foreign opening turn was not recorded as an error")
	}
}

// TestOnTurnOpeningRefusesAnUnidentifiedTurn covers the empty id: nothing can
// be attributed to it, so it is recorded rather than stored.
func TestOnTurnOpeningRefusesAnUnidentifiedTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.w.OnTurnOpening("ws-1", ids.TurnID(""))

	// Assert.
	if !h.hasRecord("error", "daemon.sessionwatcher.turn_opening_unidentified") {
		t.Fatal("an unidentified opening turn was not recorded as an error")
	}
}

// TestOnTurnOpenedRaisesTheFootersTurnOpenEdge covers the edge nothing on the
// shim's streams states: the accepted turn reaches the footer, which is what
// raises `thinking submitting` before the first frame of the turn arrives.
func TestOnTurnOpenedRaisesTheFootersTurnOpenEdge(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.w.OnTurnOpened("ws-1", &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-9"},
		Agent: agentID("main-1"),
	}, nil)
	got := h.drainNow()

	// Assert.
	ev, ok := find(got, "footer.OnTurnOpened")
	if !ok {
		t.Fatalf("the footer never took the turn-open edge: %v", names(got))
	}
	if ev.detail != "turn-9" {
		t.Fatalf("footer.OnTurnOpened turn = %q, want turn-9", ev.detail)
	}
	if !h.hasRecord("info", "daemon.sessionwatcher.turn_opened") {
		t.Fatal("the turn start has no info lifecycle record")
	}
}

// TestOnTurnOpenedRefusesAnotherWorkspacesTurn covers the invariant: a
// watcher is one workspace's, and a turn handed to the wrong one has no useful
// treatment beyond being recorded loudly.
func TestOnTurnOpenedRefusesAnotherWorkspacesTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.w.OnTurnOpened("ws-other", &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-9"},
		Agent: agentID("main-1"),
	}, &conversationv1.HistoryPage{})

	// Assert.
	if h.w.TurnInFlight() != nil {
		t.Fatal("another workspace's turn was adopted")
	}
	if !h.hasRecord("error", "daemon.sessionwatcher.turn_opened_foreign") {
		t.Fatal("a foreign turn was not recorded as an error")
	}
}

// TestSetOutputAddress covers the lease holder's redirection and its
// restoration, since every feed row the session produces is stamped with it.
func TestSetOutputAddress(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	elsewhere := OutputAddress{Feed: feedFor("sub-1")}

	// Act / Assert: installed.
	h.w.SetOutputAddress(&elsewhere)
	if h.addressNow().Feed.Root {
		t.Fatal("the installed address was ignored")
	}

	// Act / Assert: nil restores the root feed.
	h.w.SetOutputAddress(nil)
	if !h.addressNow().Feed.Root {
		t.Fatal("nil did not restore the root feed")
	}
}

// TestSinkCallsAreSerializedPerWorkspace covers the ordering guarantee the
// resolvers depend on: many streams, one mutex, so each agent's frames reach
// the sinks in that stream's order however they interleave.
func TestSinkCallsAreSerializedPerWorkspace(t *testing.T) {
	// Arrange: the main watch plus a detached subagent's own watch.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", subagentWork("sub-1")))})
	sub := h.client.nextAgentOpen(t)
	h.quiet()

	const frames = 25
	var wg sync.WaitGroup
	wg.Add(2)
	send := func(stream *fakeStream[*shimResponse], agent string) {
		defer wg.Done()
		for i := 0; i < frames; i++ {
			stream.send(t, entryFrame(frameUpdate(agent, activityUpdate(readActivity(agent+"-"+itoa(i))))))
		}
	}

	// Act.
	go send(h.main, "main-1")
	go send(sub.stream, "sub-1")
	wg.Wait()

	// Assert: per agent, the feed saw its activities in the order they were
	// sent, with nothing interleaved inside one frame's routing.
	got := h.collect(t, 2*frames*3)
	assertPerAgentOrder(t, got, "main-1", frames)
	assertPerAgentOrder(t, got, "sub-1", frames)
}

// TestAReopenWhoseStreamCloseBlocksStillAnswersTurnInFlight covers the one
// thing a stream Close may never do: hold the watcher's lock. The real
// transport's Close drains the response body and returns only when the server
// ends the stream, which a standing watch never does on its own, so closing
// under the lock wedges every reader of the watcher.
func TestAReopenWhoseStreamCloseBlocksStillAnswersTurnInFlight(t *testing.T) {
	// Arrange: a session whose stream will not finish closing.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	release := make(chan struct{})
	h.session.blockClose = release
	t.Cleanup(func() { close(release) })

	// Act: sever the link and bring it back, which re-opens the fleet.
	h.session.fail(errors.New("connection reset"))
	h.rec.until(t, "sidebar.OnLink")
	h.client.links <- shimclient.LinkConnected
	h.client.nextSessionOpen(t)

	answered := make(chan struct{})
	go func() {
		h.w.TurnInFlight()
		close(answered)
	}()

	// Assert: the freeness read is answered while the close is still standing.
	select {
	case <-answered:
	case <-time.After(waitDeadline):
		t.Fatal("TurnInFlight never answered while a stream close was in flight")
	}
}

// TestADeadLinkIsNeverWalkedBackToRedialing covers the terminal-death
// invariant: the shim process being gone is stronger evidence than any stream
// break, and the breaks that follow the death are its consequence.
func TestADeadLinkIsNeverWalkedBackToRedialing(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.links <- shimclient.LinkDead
	h.rec.until(t, "sidebar.OnLink")

	// Act: a standing stream breaks after the death, as every one of them does.
	h.session.fail(errors.New("connection reset"))

	// Assert.
	h.awaitRecord(t, "info", "daemon.sessionwatcher.link")
	if got := h.w.Link(); got != shimclient.LinkDead {
		t.Fatalf("the link after a post-death stream break = %v, want LinkDead", got)
	}
}

// TestASeveredStreamRaisesASeveredLinkFault pins the evidence half of a broken
// standing stream: the views get the link state, and the lifecycle sink gets
// the fault the session's health answer is built from.
func TestASeveredStreamRaisesASeveredLinkFault(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.session.fail(errors.New("connection reset"))

	// Assert.
	got := h.awaitLinkFault(t)
	if got.Kind != LinkFaultSevered {
		t.Fatalf("link fault kind = %q, want %q", got.Kind, LinkFaultSevered)
	}
	if got.ExitCode != nil {
		t.Fatalf("link fault exit code = %d, want none: the shim is still running", *got.ExitCode)
	}
}

// TestADeadLinkRaisesAShimDiedFaultCarryingTheExitCode pins that the reap's
// own decoding travels with the fault, which is what the two booleans of the
// liveness probe could never carry.
func TestADeadLinkRaisesAShimDiedFaultCarryingTheExitCode(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setReaped(shimclient.ExitInfo{PID: 4242, Code: 3})

	// Act.
	h.client.links <- shimclient.LinkDead

	// Assert.
	got := h.awaitLinkFault(t)
	if got.Kind != LinkFaultDead {
		t.Fatalf("link fault kind = %q, want %q", got.Kind, LinkFaultDead)
	}
	if got.ExitCode == nil || *got.ExitCode != 3 {
		t.Fatalf("link fault exit code = %v, want 3", got.ExitCode)
	}
}

// TestADeadLinkWithNoDecodedExitRaisesNoExitCode pins presence over sentinels:
// an exit nothing decoded is ABSENT, never a zero that reads as a clean exit.
func TestADeadLinkWithNoDecodedExitRaisesNoExitCode(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.client.links <- shimclient.LinkDead

	// Assert.
	got := h.awaitLinkFault(t)
	if got.ExitCode != nil {
		t.Fatalf("link fault exit code = %d, want none", *got.ExitCode)
	}
}

// TestTheLinkComingBackRaisesNoFault pins that only a LOST link is evidence: a
// link that connects is the ordinary path.
//
// The loss is spelled `redialing`, which is how a client that has already
// connected reports a link it is re-establishing. It was `dialing` here, and
// that only ever passed because the watcher applied a first-time dial it had
// already outrun -- the replay `setLinkLocked` now refuses.
func TestTheLinkComingBackRaisesNoFault(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.client.links <- shimclient.LinkRedialing
	h.client.links <- shimclient.LinkConnected

	// Assert: the connected edge reaches the views, and no fault rides with it.
	h.rec.until(t, "lifecycle.OnLinkChanged")
	select {
	case e := <-h.rec.ch:
		if e.name() == "lifecycle.OnLinkFault" {
			t.Fatalf("a link coming back raised %+v, want no fault", e.linkFault)
		}
	default:
	}
}

// ---------------------------------------------------------------------------
// The landing-7 re-announcement: WatchSessionResponse.session_started
// ---------------------------------------------------------------------------

// TestAPureAttachTakesTheSessionFactsFromTheReannouncement is the adoption
// case: a watcher opened with NO facts (crash boot, handover) learns the
// session's identity and its turn in flight from the shim's own re-announcement
// rather than from a durable record.
func TestAPureAttachTakesTheSessionFactsFromTheReannouncement(t *testing.T) {
	// Arrange: a pure attach — Session carries nothing.
	h := newHarnessAttachingPurely(t)

	// Act.
	h.sendSessionStarted(t, sessionStarted("turn-7"))
	h.sendSessionUpdate(t, compactingUpdate())

	// Assert: the facts reached the surfaces, and the open turn is held.
	seen := h.rec.until(t, "footer.OnSessionUpdate")
	if !hasEvent(seen, "topbar.OnSessionStarted") {
		t.Fatalf("the re-announcement routed %v, want topbar.OnSessionStarted", names(seen))
	}
	turn := h.w.TurnInFlight()
	if turn == nil || *turn != ids.TurnID("turn-7") {
		t.Fatalf("TurnInFlight() = %v, want the re-announced turn-7", turn)
	}
}

// TestSessionFactsNeverReopenAnEndedTurn covers the session facts taking the
// same guarded edge the queue's turn opening does: a turn whose end this
// watcher already handed to every observer is not stood back up in flight by
// facts naming it, because nothing would ever end it a second time.
func TestSessionFactsNeverReopenAnEndedTurn(t *testing.T) {
	// Arrange: a pure attach that has already seen turn-7 end.
	h := newHarnessAttachingPurely(t)
	h.w.mu.Lock()
	h.w.rememberClosedTurnLocked("turn-7", wsm.CloseCompleted)
	h.w.mu.Unlock()

	// Act.
	h.sendSessionStarted(t, sessionStarted("turn-7"))
	h.sendSessionUpdate(t, compactingUpdate())
	h.rec.until(t, "footer.OnSessionUpdate")

	// Assert.
	if turn := h.w.TurnInFlight(); turn != nil {
		t.Fatalf("TurnInFlight() = %v, want none: turn-7 already ended", *turn)
	}
}

// TestAPureAttachOpensNoAgentWatchBeforeTheReannouncement pins the refusal
// this design removes: the shim resolves an unset WatchAgent target through
// the session's identity, so a survivor with no session answers `not_found: no
// session has been started on this shim`. That is not a race and cannot be
// waited out, so the daemon must not ask before the session announces itself.
func TestAPureAttachOpensNoAgentWatchBeforeTheReannouncement(t *testing.T) {
	// Arrange, Act: a pure attach opens with no facts at all.
	h := newHarnessAttachingPurely(t)

	// Assert.
	select {
	case open := <-h.client.agentOpens:
		t.Fatalf("a pure attach opened WatchAgent(%v) before any session announced itself", open.req.GetTarget())
	default:
	}
}

// TestAPureAttachOpensTheAgentWatchOnTheReannouncement is the other half: the
// deferral is not an omission, and the facts are the occasion.
func TestAPureAttachOpensTheAgentWatchOnTheReannouncement(t *testing.T) {
	// Arrange.
	h := newHarnessAttachingPurely(t)

	// Act.
	h.sendSessionStarted(t, sessionStarted(""))

	// Assert.
	open := h.client.nextAgentOpen(t)
	if open.req.GetTarget() != nil {
		t.Fatalf("WatchAgent target = %v, want the unset main target", open.req.GetTarget())
	}
}

// TestAWatcherOpenedWithFactsStillOpensTheAgentWatchAtOnce pins that the
// deferral is scoped to a PURE ATTACH: a fresh bring-up already holds the
// session's identity, so nothing about its main watch waits.
func TestAWatcherOpenedWithFactsStillOpensTheAgentWatchAtOnce(t *testing.T) {
	// Arrange, Act.
	h := startHarness(t, Session{Started: sessionStarted("")}, nil)
	h.session = h.client.nextSessionOpen(t)

	// Assert.
	open := h.client.nextAgentOpen(t)
	if open.req.GetTarget() != nil {
		t.Fatalf("WatchAgent target = %v, want the unset main target", open.req.GetTarget())
	}
}

// TestAReannouncementOnAWatchThatAlreadyHoldsTheFactsIsIgnored is the
// idempotence half: the re-announcement rides EVERY new watch, including each
// re-open after a link break, so a watcher that already holds the facts must
// not republish them.
func TestAReannouncementOnAWatchThatAlreadyHoldsTheFactsIsIgnored(t *testing.T) {
	// Arrange: a watcher that opened WITH the facts.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.sendSessionStarted(t, sessionStarted(""))
	h.sendSessionUpdate(t, compactingUpdate())

	// Assert: nothing was republished, and the ordinary case is not warned.
	seen := h.rec.until(t, "footer.OnSessionUpdate")
	if hasEvent(seen, "topbar.OnSessionStarted") {
		t.Fatalf("a repeat re-announcement routed %v, want no re-publication", names(seen))
	}
	if h.hasRecord("warn", "daemon.sessionwatcher.watch_session") {
		t.Fatal("a repeat re-announcement was warned; it is the ordinary case")
	}
}

// TestASessionFrameWithNoArmIsRecordedAsAnError is the validation half: the
// frame oneof is never guessed at, and an unset one is surfaced.
func TestASessionFrameWithNoArmIsRecordedAsAnError(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.session.send(t, &shimv1.WatchSessionResponse{})
	h.sendSessionUpdate(t, compactingUpdate())

	// Assert.
	h.rec.until(t, "footer.OnSessionUpdate")
	if !h.hasRecord("error", "daemon.sessionwatcher.watch_session") {
		t.Fatal("an armless session frame was not recorded as an error")
	}
}

// hasEvent reports whether a run of recorded sink calls contains one by name.
func hasEvent(seen []event, name string) bool {
	for _, e := range seen {
		if e.name() == name {
			return true
		}
	}
	return false
}

// TestShellStreamEndingEarlyReopensTheWatch covers a watch the shim drops
// before the shell settles: the shell's terminal frame is what reaps the watch,
// so a stream ending while the entry is still registered means the daemon lost
// sight of live work and must open the watch again.
func TestShellStreamEndingEarlyReopensTheWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", bashWork()))})
	first := h.client.nextBashOpen(t)
	h.quiet()

	// Act.
	first.stream.fail(errors.New("stream ended"))

	// Assert.
	if second := h.client.nextBashOpen(t); second.work.GetValue() != "w-1" {
		t.Fatalf("re-opened watch = %q, want w-1", second.work.GetValue())
	}
}

// TestShellStreamEndingAfterTheTerminalIsNotReopened covers the reaped case: a
// settled shell's watch was already forgotten, so its stream ending is the
// teardown and nothing is opened again.
func TestShellStreamEndingAfterTheTerminalIsNotReopened(t *testing.T) {
	// Arrange: a shell that has settled.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", bashWork()))})
	open := h.client.nextBashOpen(t)
	entry := h.shellWatchFor("w-1")
	h.quiet()
	h.routeNow(func(w *watcher) {
		w.routeBashLocked(entry, &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}},
		})
	})

	// Act.
	open.stream.fail(errors.New("stream ended"))
	h.quiet()

	// Assert.
	h.client.noBashOpen(t)
}

// ---------------------------------------------------------------------------
// A REFUSED watch open is not a severed link
// ---------------------------------------------------------------------------

// TestRefusedOpenClassification pins which failures on a watch OPEN are the
// shim's semantic refusal and which are the transport failing.
func TestRefusedOpenClassification(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{
			name: "not found on the open is a refusal",
			err:  refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"),
			want: true,
		},
		{
			name: "failed precondition on the open is a refusal",
			err:  refusedOpenError("WatchBash", connect.CodeFailedPrecondition, "no rows yet"),
			want: true,
		},
		{
			name: "unavailable on the open is the transport failing",
			err:  refusedOpenError("WatchAgent", connect.CodeUnavailable, "connection refused"),
			want: false,
		},
		{
			name: "a bare transport error is not a refusal",
			err:  errors.New("connection reset"),
			want: false,
		},
		{
			name: "a connect refusal that is NOT an open error is not a refusal",
			err:  connect.NewError(connect.CodeNotFound, errors.New("no such agent")),
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := refusedOpen(tc.err)

			// Assert.
			if got != tc.want {
				t.Fatalf("refusedOpen(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}

// TestARefusedMainWatchOpenAtBringUpNeverSeversTheLink is the defect: the
// store has not registered the main agent's book at a fresh bring-up, and the
// refusal that answers is a semantic one — not a transport that broke.
func TestARefusedMainWatchOpenAtBringUpNeverSeversTheLink(t *testing.T) {
	// Arrange & Act: bring up a watcher whose main watch the shim refuses.
	h := newHarnessRefusingAgents(t, Session{Started: sessionStarted("")},
		refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"))

	// Assert: the link is intact, and the refusal was logged rather than raised.
	if got := h.w.Link(); got != shimclient.LinkConnected {
		t.Fatalf("the link after a refused main open = %v, want LinkConnected", got)
	}
	if !h.hasRecord("info", "daemon.sessionwatcher.watch_agent") {
		t.Fatalf("a refused main open logged %v, want an info record", h.log.Records())
	}
}

// TestARefusedMainWatchOpenRaisesNoLinkFault is the evidence half: nothing
// severed, so nothing is recorded as severed.
func TestARefusedMainWatchOpenRaisesNoLinkFault(t *testing.T) {
	// Arrange & Act.
	h := newHarnessRefusingAgents(t, Session{Started: sessionStarted("")},
		refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"))

	// Assert: the start's own events are drained; no fault is among them.
	for _, e := range h.rec.drain() {
		if e.name() == "lifecycle.OnLinkFault" || e.name() == "lifecycle.OnWatchOpenRefused" {
			t.Fatalf("a refused main open raised %s, want no fault", e.name())
		}
	}
}

// TestARefusedMainWatchIsReopenedWhenTheBookAppears pins the retry: nothing
// re-announces the main agent, so a frame on the session's own stream is the
// occasion to open its watch again.
func TestARefusedMainWatchIsReopenedWhenTheBookAppears(t *testing.T) {
	// Arrange: a bring-up whose main open was refused, then a shim that holds
	// the book.
	h := newHarnessRefusingAgents(t, Session{Started: sessionStarted("")},
		refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"))
	h.client.setAgentErr(nil)

	// Act.
	h.sendSessionUpdate(t, compactingUpdate())

	// Assert.
	if open := h.client.nextAgentOpen(t); open.req.GetTarget() != nil {
		t.Fatalf("the re-opened watch targeted %q, want the main agent's unset target", open.req.GetTarget().GetValue())
	}
}

// TestARefusedMainWatchStopsBeingRetriedAndRaisesItsOwnFault pins the bound: a
// shim that refuses forever is reported, and still never severs the link.
func TestARefusedMainWatchStopsBeingRetriedAndRaisesItsOwnFault(t *testing.T) {
	// Arrange: a shim that refuses every open.
	h := newHarnessRefusingAgents(t, Session{Started: sessionStarted("")},
		refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"))

	// Act: drive the retries past the bound.
	for i := 0; i < openRefusalLimit; i++ {
		h.sendSessionUpdate(t, compactingUpdate())
	}

	// Assert.
	got := h.awaitRefusal(t)
	if got.Operation != "watch_agent" {
		t.Fatalf("refusal operation = %q, want watch_agent", got.Operation)
	}
	if link := h.w.Link(); link != shimclient.LinkConnected {
		t.Fatalf("the link after exhausted retries = %v, want LinkConnected", link)
	}
}

// TestARefusedOpenOnAnUnannouncedHandleRaisesItsOwnFault pins the other half
// of the classification: nothing announced this agent, so the refusal is a
// disagreement about what exists and is reported at once.
func TestARefusedOpenOnAnUnannouncedHandleRaisesItsOwnFault(t *testing.T) {
	// Arrange: a healthy watcher whose shim refuses agent opens.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setAgentErr(refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"))

	// Act: open a watch for a handle no announcement ever registered.
	h.w.mu.Lock()
	h.w.openAgentStreamLocked(&agentWatch{id: agentID("ghost-1")})
	h.w.mu.Unlock()

	// Assert.
	got := h.awaitRefusal(t)
	if got.Handle != "ghost-1" {
		t.Fatalf("refusal handle = %q, want ghost-1", got.Handle)
	}
	if link := h.w.Link(); link != shimclient.LinkConnected {
		t.Fatalf("the link after an unannounced refusal = %v, want LinkConnected", link)
	}
}

// TestARefusedOpenOnAnUnannouncedHandleIsWarned pins the level: an unexpected
// refusal is a warning, not the ordinary bring-up race's info line.
func TestARefusedOpenOnAnUnannouncedHandleIsWarned(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setAgentErr(refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"))

	// Act.
	h.w.mu.Lock()
	h.w.openAgentStreamLocked(&agentWatch{id: agentID("ghost-1")})
	h.w.mu.Unlock()
	h.awaitRefusal(t)

	// Assert.
	if !h.hasRecord("warn", "daemon.sessionwatcher.watch_open_refused") {
		t.Fatalf("an unannounced refusal logged %v, want a warn record", h.log.Records())
	}
}

// TestAnAgentStreamEndingAfterOpeningStillSevers pins that the fix narrowed
// nothing else: a stream that OPENED and then ended while the session is live
// is a transport failure exactly as before.
func TestAnAgentStreamEndingAfterOpeningStillSevers(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.main.fail(errors.New("connection reset"))

	// Assert.
	got := h.awaitLinkFault(t)
	if got.Kind != LinkFaultSevered {
		t.Fatalf("link fault kind = %q, want %q", got.Kind, LinkFaultSevered)
	}
}

// TestAnUnavailableWatchOpenStillSevers pins the same for the OPEN: a
// transport that will not carry the stream is a severed link, refusals or no
// refusals.
func TestAnUnavailableWatchOpenStillSevers(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setAgentErr(refusedOpenError("WatchAgent", connect.CodeUnavailable, "connection refused"))

	// Act.
	h.w.mu.Lock()
	h.w.openAgentStreamLocked(&agentWatch{id: agentID("sub-1")})
	h.w.mu.Unlock()

	// Assert.
	got := h.awaitLinkFault(t)
	if got.Kind != LinkFaultSevered {
		t.Fatalf("link fault kind = %q, want %q", got.Kind, LinkFaultSevered)
	}
}

// ---- a teardown this daemon ordered is not a fault ----

// TestStreamEndAfterAnAskedStandDownIsNotAFailure covers the teardown route
// that announces NOTHING. `Fleet.KillSession` tells the watcher through
// SessionEnding, but the rollout's stand-down and the verbs' kill reach the
// shim's `KillSession` rpc directly, and the shim ends its process on it. The
// watcher reads the shim's own latch instead of waiting to be told, so the
// streams the shim closes on its way out are the answer to an act this daemon
// performed rather than a severing to redial.
//
// MEASURED: a stale-build relaunch bounce recorded two ERRORs, a link_severed
// WARN and its health fault against exactly this, in the realtest sweep of
// 2026-09-12T15:23:03.
func TestStreamEndAfterAnAskedStandDownIsNotAFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.StandDown()

	// Act.
	h.main.Close()

	// Assert.
	h.awaitRecord(t, "debug", "daemon.sessionwatcher.stream_closed")
	if !h.w.Connected() {
		t.Fatal("a stream ending inside an asked-for stand-down severed the link")
	}
}

// TestASessionStreamEndAfterAnAskedStandDownIsNotAFailure is the same edge on
// the SESSION stream, which is the other standing stream a stand-down ends and
// the one the sweep recorded first.
func TestASessionStreamEndAfterAnAskedStandDownIsNotAFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.StandDown()

	// Act.
	h.session.Close()

	// Assert.
	h.awaitRecord(t, "debug", "daemon.sessionwatcher.stream_closed")
	if h.hasRecord("error", "daemon.sessionwatcher.watch_session") {
		t.Fatal("the session stream's end inside an asked-for stand-down was recorded as a severing")
	}
}

// TestAStreamEndWithNoStandDownStillSevers is the other half, and it is the
// point of the whole distinction: nothing asked this shim to stand down, so a
// standing stream ending is exactly as loud as it has always been.
func TestAStreamEndWithNoStandDownStillSevers(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.session.fail(errors.New("connection reset"))

	// Assert.
	got := h.awaitLinkFault(t)
	if got.Kind != LinkFaultSevered {
		t.Fatalf("link fault kind = %q, want %q", got.Kind, LinkFaultSevered)
	}
	if !h.hasRecord("error", "daemon.sessionwatcher.watch_session") {
		t.Fatal("an unasked stream end was not recorded as a severing")
	}
}

// TestADeadLinkInsideAnAskedStandDownRaisesNoFault covers the fault half. The
// shim's process going is the END of the teardown this daemon ordered, and a
// `link_dead` fault raised against it would stand on the health surface for a
// workspace whose session the user deliberately ended.
func TestADeadLinkInsideAnAskedStandDownRaisesNoFault(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.StandDown()

	// Act.
	h.client.links <- shimclient.LinkDead

	// Assert.
	h.awaitRecord(t, "debug", "daemon.sessionwatcher.link_fault")
	for _, e := range h.rec.drain() {
		if e.name() == "lifecycle.OnLinkFault" {
			t.Fatal("a link fault was raised inside a stand-down this daemon asked for")
		}
	}
}

// TestADeadLinkWithNoStandDownStillRaisesItsFault is that fault's other half:
// a shim that went away unasked is still the loudest thing the watcher says.
func TestADeadLinkWithNoStandDownStillRaisesItsFault(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.client.links <- shimclient.LinkDead

	// Assert.
	got := h.awaitLinkFault(t)
	if got.Kind != LinkFaultDead {
		t.Fatalf("link fault kind = %q, want %q", got.Kind, LinkFaultDead)
	}
}

// ---- a severing states what the daemon knows about the peer ----

// TestASeveringNamesALivingShim covers the case the log could not previously
// tell from any other: the shim is UP and ended one stream of several.
//
// MEASURED, realtest run 2026-09-13T16:03:25. Adopted shim pid 3031 ended
// workspace 2b81f45a724642ef's agent stream with EOF; its session stream
// stayed open, the process was alive enough for the teardown eleven seconds
// later to kill it, and the shim's own log recorded nothing. The daemon's
// record said only "agent stream ended while the session was live", which is
// equally true of a shim that had simply died -- and the two are remediated in
// different systems.
func TestASeveringNamesALivingShim(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act: the main agent stream ends with the session live and nothing asked.
	h.main.Close()

	// Assert.
	h.awaitRecord(t, "error", "daemon.sessionwatcher.watch_agent")
	got := h.recordContext(t, "error", "daemon.sessionwatcher.watch_agent")
	if got["shim_reaped"] != false {
		t.Fatalf("shim_reaped = %v, want false: the shim is still running", got["shim_reaped"])
	}
	if got["stream"] != "agent" {
		t.Fatalf("stream = %v, want the agent stream named", got["stream"])
	}
	if got["shim_pid"] != 4242 {
		t.Fatalf("shim_pid = %v, want the peer's pid on the record", got["shim_pid"])
	}
}

// TestASeveringNamesADeadShim is the other half of the same distinction: when
// the peer HAS been reaped, the record says so and carries the exit, because
// that is a session to bring back rather than a watch the shim dropped.
func TestASeveringNamesADeadShim(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setReaped(shimclient.ExitInfo{PID: 4242, Code: 9})

	// Act.
	h.main.Close()

	// Assert.
	h.awaitRecord(t, "error", "daemon.sessionwatcher.watch_agent")
	got := h.recordContext(t, "error", "daemon.sessionwatcher.watch_agent")
	if got["shim_reaped"] != true {
		t.Fatalf("shim_reaped = %v, want true: the shim was reaped", got["shim_reaped"])
	}
	if got["shim_exit_code"] != 9 {
		t.Fatalf("shim_exit_code = %v, want 9", got["shim_exit_code"])
	}
}
