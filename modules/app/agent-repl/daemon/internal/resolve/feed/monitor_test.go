package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/sessionwatcher"
)

// watchedCommand is the start a command monitor is armed with.
func watchedCommand(command string) *conversationv1.AgentMonitorStart {
	return &conversationv1.AgentMonitorStart{
		Description: "build log",
		Source:      &conversationv1.AgentMonitorStart_Command{Command: &conversationv1.AgentMonitorCommand{Command: command}},
		StartedAtMs: 1_000,
	}
}

// monitorFrame wraps one monitor result as the unit's activity.
func monitorFrame(unit string, monitor *conversationv1.AgentMonitor) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item:       &conversationv1.AgentActivity_Monitor{Monitor: monitor},
	}
}

// monitorArmed is a monitor's start frame.
func monitorArmed(unit string, start *conversationv1.AgentMonitorStart) *conversationv1.AgentActivity {
	return monitorFrame(unit, &conversationv1.AgentMonitor{Result: &conversationv1.AgentMonitor_Start{Start: start}})
}

// monitorEnded is a monitor's ended frame, restating CALL (nil for none).
func monitorEnded(unit string, call *conversationv1.AgentMonitorStart) *conversationv1.AgentActivity {
	return monitorFrame(unit, &conversationv1.AgentMonitor{Result: &conversationv1.AgentMonitor_Ended{
		Ended: &conversationv1.AgentMonitorEnded{Call: call},
	}})
}

// monitorCard is the root feed's one tool-call card.
func (h *harness) monitorCard() *frontendv1.FeedSimpleToolCall {
	h.t.Helper()
	rows := h.rows(rootFeed())
	if len(rows) != 1 {
		h.t.Fatalf("root rows = %d, want the monitor's one card", len(rows))
	}
	card := rows[0].GetActivity().GetSimpleToolCall()
	if card == nil {
		h.t.Fatalf("root row = %v, want a tool-call card", rows[0])
	}
	return card
}

// liveMonitors publishes the session watcher's live set holding UNITS as monitors.
func (h *harness) liveMonitors(units ...string) {
	h.t.Helper()
	live := sessionwatcher.LiveWorkSet{}
	for _, unit := range units {
		live.Monitors = append(live.Monitors, &conversationv1.DetachedWorkId{Value: unit})
	}
	h.resolver.OnLiveWorkChanged(testWorkspace, live)
}

func TestAMonitorCallDrawsTheToolCallCardNamedMonitor(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	if got := h.monitorCard().GetName().GetText(); got != "Monitor" {
		t.Fatalf("name = %q, want Monitor", got)
	}
}

func TestAMonitorCardsInputIsTheWatchedCommandAsAShellLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	input := h.monitorCard().GetInput()
	if input.GetText() != "tail -f build.log" || input.GetCommand() == nil {
		t.Fatalf("input = %v, want the command drawn as a shell line", input)
	}
}

func TestAWebsocketMonitorCardsInputIsTheURLAsPlainText(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	start := &conversationv1.AgentMonitorStart{
		Source: &conversationv1.AgentMonitorStart_Websocket{Websocket: &conversationv1.AgentMonitorWebsocket{Url: "wss://host/events"}},
	}

	// Act.
	h.send(monitorArmed("toolu_monitor", start))

	// Assert.
	input := h.monitorCard().GetInput()
	if input.GetText() != "wss://host/events" || input.GetForm() != nil {
		t.Fatalf("input = %v, want the URL as plain text", input)
	}
}

func TestAnArmedMonitorsCardIsRunning(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	if h.monitorCard().GetRunning() == nil {
		t.Fatalf("card = %v, want the running arm", h.monitorCard())
	}
}

func TestAMonitorsCardIsAnnouncedToTheFooterByTheMonitorsID(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	want := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "toolu_monitor"},
	}).GetValue()
	if len(h.placed) != 1 || h.placed[0] != (placedEntry{unit: "toolu_monitor", row: want}) {
		t.Fatalf("placed = %+v, want the card announced once at %q", h.placed, want)
	}
}

func TestASubagentsMonitorCardIsAnnouncedOnTheSubagentsFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	h.send(spawnCall("toolu_spawn", sub))

	// Act.
	h.sendAs(sub, monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	want := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: agentFeed(sub),
		Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "toolu_monitor"},
	}).GetValue()
	var got string
	for _, placed := range h.placed {
		if placed.unit == "toolu_monitor" {
			got = placed.row
		}
	}
	if got != want {
		t.Fatalf("toolu_monitor announced at %q, want %q (placed = %+v)", got, want, h.placed)
	}
}

func TestARedrawnMonitorCardIsNotAnnouncedAgain(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Act: the other plane restates the same start.
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	if len(h.placed) != 1 {
		t.Fatalf("placed = %+v, want one announcement for an unmoved card", h.placed)
	}
}

func TestAMonitorsDetachmentKeepsItsCardRatherThanDrawingAShellHead(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Act.
	h.announceDetachment(mainAgent(), mainAgent(), "toolu_monitor", "toolu_monitor")

	// Assert.
	if got := h.rowKinds(rootFeed()); len(got) != 1 || got[0] != "tool_card" {
		t.Fatalf("root rows = %v, want the monitor's card alone", got)
	}
}

func TestADetachmentHeldBeforeAMonitorsCardDrawsFindsTheCard(t *testing.T) {
	// Arrange: the announcement beats the call's own frame.
	h := newHarness(t)
	h.detachWork("toolu_monitor", "toolu_monitor")

	// Act.
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	if !h.hasRecord("debug", "daemon.feed.detached_monitor_card_is_entry") {
		t.Fatalf("records = %+v, want the held detachment to find the card", h.records())
	}
}

func TestAHeldMonitorDetachmentIsNotReportedUnplaceableAtTheTurnsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.detachWork("toolu_monitor", "toolu_monitor")
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert.
	if h.hasRecord("error", "daemon.feed.detached_unplaceable") {
		t.Fatalf("records = %+v, want NO detached_unplaceable once the card claimed it", h.records())
	}
}

func TestAnEndedMonitorsCardIsSettledSucceeded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Act.
	h.send(monitorEnded("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	if h.monitorCard().GetReturned().GetSucceeded() == nil {
		t.Fatalf("card = %v, want returned succeeded", h.monitorCard())
	}
}

func TestAReplayedEndedMonitorDrawsItsCardFromTheRestatedCall(t *testing.T) {
	// Arrange: a replay serves the settled row alone.
	h := newHarness(t)

	// Act.
	h.send(monitorEnded("toolu_monitor", watchedCommand("tail -f build.log")))

	// Assert.
	if got := h.monitorCard().GetInput().GetText(); got != "tail -f build.log" {
		t.Fatalf("input = %q, want the restated command", got)
	}
}

func TestAnEndedMonitorThatRestatesNothingAndHeldNoStartDrawsNoRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(monitorEnded("toolu_monitor", nil))

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none for a settle that restated nothing", len(rows))
	}
}

func TestAnEndedMonitorThatRestatesNothingAndHeldNoStartIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(monitorEnded("toolu_monitor", nil))

	// Assert.
	if !h.hasRecord("error", "daemon.feed.activity_undrawable") {
		t.Fatalf("records = %+v, want the producer's violation recorded at ERROR", h.records())
	}
}

func TestAnEndedMonitorThatRestatesNothingIsDrawnFromTheHeldStart(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Act.
	h.send(monitorEnded("toolu_monitor", nil))

	// Assert.
	if got := h.monitorCard().GetInput().GetText(); got != "tail -f build.log" {
		t.Fatalf("input = %q, want the held start's command", got)
	}
}

func TestAFailedMonitorsCardIsSettledFailed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(monitorFrame("toolu_monitor", &conversationv1.AgentMonitor{Result: &conversationv1.AgentMonitor_Failure{
		Failure: &conversationv1.AgentMonitorFailure{
			Failure: &conversationv1.AgentToolFailure{},
			Call:    watchedCommand("tail -f build.log"),
		},
	}}))

	// Assert.
	if h.monitorCard().GetReturned().GetFailed() == nil {
		t.Fatalf("card = %v, want returned failed", h.monitorCard())
	}
}

func TestAMonitorThatLeftTheLiveSetHasItsRunningCardSettled(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))
	h.liveMonitors("toolu_monitor")

	// Act.
	h.liveMonitors()

	// Assert.
	if h.monitorCard().GetReturned().GetSucceeded() == nil {
		t.Fatalf("card = %v, want settled once the watch left the live set", h.monitorCard())
	}
}

func TestAMonitorNeverListedLiveKeepsItsCardRunning(t *testing.T) {
	// Arrange: the live set has not listed the watch yet.
	h := newHarness(t)
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))

	// Act.
	h.liveMonitors()

	// Assert.
	if h.monitorCard().GetRunning() == nil {
		t.Fatalf("card = %v, want still running: it may simply not be listed yet", h.monitorCard())
	}
}

func TestAMonitorStillListedLiveKeepsItsCardRunning(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))
	h.liveMonitors("toolu_monitor")

	// Act.
	h.liveMonitors("toolu_monitor")

	// Assert.
	if h.monitorCard().GetRunning() == nil {
		t.Fatalf("card = %v, want still running", h.monitorCard())
	}
}
