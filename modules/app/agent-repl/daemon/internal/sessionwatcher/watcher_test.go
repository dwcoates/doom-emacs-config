package sessionwatcher

import (
	"errors"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
)

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
		"lifecycle.OnLiveWorkChanged", "sidebar.OnLiveWorkChanged",
	})
	if !h.w.Connected() {
		t.Fatal("a started session is not connected")
	}
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
	h.client.links <- shimclient.LinkDialing
	h.rec.until(t, "sidebar.OnLink")
	h.client.links <- shimclient.LinkConnected
	h.rec.until(t, "sidebar.OnLink")

	// Assert.
	h.client.noAgentOpen(t)
	if h.hasRecord("warn", "daemon.sessionwatcher.reopen") {
		t.Fatal("the fleet was re-opened on a link replay with every stream standing")
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
// names the main agent, records the turn, and feeds StartTurnSuccess's page
// through the opening-page path without mirroring the prompt a second time.
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
	assertNames(t, got, []string{"footer.OnTurnOpened", "feed.OnHistoryPage"})
	turn := h.w.TurnInFlight()
	if turn == nil || *turn != ids.TurnID("turn-9") {
		t.Fatalf("turn in flight = %v, want turn-9", turn)
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
	got := h.collect(t, 2*frames*2)
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
