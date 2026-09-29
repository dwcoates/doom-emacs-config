package footer

import (
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// itemFrame builds one activity frame generically: the item arm named ARM
// carrying its own oneof arm PHASE, empty otherwise. The empty payloads are
// all the working step and the quiet stretch read.
func itemFrame(t *testing.T, unit string, arm protoreflect.Name, phase protoreflect.Name) *conversationv1.AgentActivity {
	t.Helper()
	act := &conversationv1.AgentActivity{ActivityId: &conversationv1.AgentActivityId{Value: unit}}
	msg := act.ProtoReflect()
	field := msg.Descriptor().Fields().ByName(arm)
	if field == nil {
		t.Fatalf("AgentActivity has no item arm %q", arm)
	}
	item := msg.NewField(field).Message()
	phaseField := item.Descriptor().Fields().ByName(phase)
	if phaseField == nil {
		t.Fatalf("%s has no arm %q", item.Descriptor().FullName(), phase)
	}
	item.Set(phaseField, item.NewField(phaseField))
	msg.Set(field, protoreflect.ValueOfMessage(item))
	return act
}

// rowOf is the FeedId the fake feed draws UNIT's row at.
func rowOf(unit string) *frontendv1.FeedId { return &frontendv1.FeedId{Value: "row-" + unit} }

// surface is a feed item's first frame as the watcher routes it: the feed
// draws the item's row (OnItemDrawn) before the footer takes the frame.
func surface(t *testing.T, h *harness, agent *conversationv1.AgentId, unit string, arm protoreflect.Name) {
	t.Helper()
	h.r.OnItemDrawn(testWS, unit, rowOf(unit), true)
	h.r.OnActivity(testWS, agent, itemFrame(t, unit, arm, "start"))
}

// inTurn arranges a delivered turn whose main agent is named.
func inTurn(h *harness) {
	connected(h)
	h.r.OnMainAgent(testWS, mainAgent)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnTurnOpened(testWS, testTurnID)
}

// working reads the published working arm.
func working(t *testing.T, h *harness) *frontendv1.FooterStatusWorking {
	t.Helper()
	arm := h.view(t).GetStrip().GetStatus().GetWorking()
	if arm == nil {
		t.Fatalf("status = %q, want working", h.status(t))
	}
	return arm
}

// quietLine reads the published working arm's quiet-stretch line, "" when none.
func quietLine(t *testing.T, h *harness) string {
	t.Helper()
	return working(t, h).GetActivity().GetQuietStretch().GetText()
}

// stepName names the working arm's step the way the strip spells it.
func stepName(arm *frontendv1.FooterStatusWorking) string {
	m := arm.ProtoReflect()
	field := m.WhichOneof(m.Descriptor().Oneofs().ByName("substatus"))
	if field == nil {
		return ""
	}
	return string(field.Name())
}

func TestTheWorkingStepNamesTheRunningCall(t *testing.T) {
	tests := []struct {
		name string
		arm  protoreflect.Name
		want string
	}{
		{"a read", "read", "reading"},
		{"a write", "write", "writing"},
		{"an edit", "edit", "writing"},
		{"a grep", "grep", "searching"},
		{"a glob", "glob", "searching"},
		{"a web search", "web_search", "searching"},
		{"a web fetch", "web_fetch", "fetching"},
		{"a sync subagent", "subagent", "delegating"},
		{"a shell command", "bash", "executing"},
		{"an MCP tool", "mcp_tool_call", "executing"},
		{"a tool the contract does not model", "unmodeled", "executing"},
		{"a streaming response", "response", "thinking"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			inTurn(h)

			// Act
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", tt.arm, "start"))

			// Assert
			if got := stepName(working(t, h)); got != tt.want {
				t.Fatalf("step = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheWorkingStepIsThinkingOnceTheCallLands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "start"))

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "success"))

	// Assert
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: no call runs, so an inference call does", got)
	}
}

func TestTheLatestRunningCallNamesTheStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "start"))

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-2", "grep", "start"))

	// Assert
	if got := stepName(working(t, h)); got != "searching" {
		t.Fatalf("step = %q, want searching: the latest running call names it", got)
	}
}

func TestAnEarlierCallNamesTheStepWhenTheLatestLands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "start"))
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-2", "grep", "start"))

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-2", "grep", "success"))

	// Assert
	if got := stepName(working(t, h)); got != "reading" {
		t.Fatalf("step = %q, want reading: the read still runs", got)
	}
}

func TestASubagentsOwnCallNamesNoStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	h.r.OnActivity(testWS, detachedAgent, itemFrame(t, "u-sub", "read", "start"))

	// Assert
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: only the main agent's calls name the step", got)
	}
}

func TestAnUnnamedMainAgentNamesNoStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnTurnOpened(testWS, testTurnID)

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "start"))

	// Assert
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: attribution to the main agent is never guessed", got)
	}
}

func TestADeliveredPromptStandsTheQuietLine(t *testing.T) {
	tests := []struct {
		name string
		act  SessionAct
		want string
	}{
		{"a prompt", ActPrompt, "✅ Prompt delivered — awaiting response..."},
		{"a /clear", ActClear, "✅ /clear delivered — clearing context..."},
		{"a /compact", ActCompact, "✅ /compact delivered — compacting context..."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.OnMainAgent(testWS, mainAgent)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: tt.act})

			// Act
			h.r.OnTurnOpened(testWS, testTurnID)

			// Assert
			if got := deliveredLine(tt.act); got != tt.want {
				t.Fatalf("delivered line = %q, want %q", got, tt.want)
			}
			if tt.act == ActPrompt {
				if got := quietLine(t, h); got != tt.want {
					t.Fatalf("line = %q, want %q", got, tt.want)
				}
			}
		})
	}
}

func TestALandedItemStandsItsQuietLine(t *testing.T) {
	tests := []struct {
		name  string
		arm   protoreflect.Name
		phase protoreflect.Name
		want  string
	}{
		{"a finished shell command", "bash", "success", "✅ Bash finished — handling result..."},
		{"a failed read", "read", "failure", "❌ Read failed — handling failure..."},
		{"a finished response", "response", "success", "✅ Response finished — continuing..."},
		{"a cancelled hook", "hook", "cancelled", "❌ Hook cancelled — continuing..."},
		{"a blocking hook", "hook", "blocking_error", "❌ Hook failed — handling failure..."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			inTurn(h)
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", tt.arm, "start"))

			// Act
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", tt.arm, tt.phase))

			// Assert
			if got := quietLine(t, h); got != tt.want {
				t.Fatalf("line = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheNextSurfacingClearsTheQuietLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "start"))
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "success"))

	// Act: the response's FIRST frame, long before it lands.
	surface(t, h, mainAgent, "u-2", "response")

	// Assert
	if got := quietLine(t, h); got != "" {
		t.Fatalf("line = %q, want none: the next item surfaced", got)
	}
}

func TestTheFirstSurfacingClearsTheDeliveredLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	surface(t, h, mainAgent, "u-1", "thinking")

	// Assert
	if got := quietLine(t, h); got != "" {
		t.Fatalf("line = %q, want none once the turn's first item surfaced", got)
	}
}

func TestALandingBesideARunningItemStandsNoLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	surface(t, h, mainAgent, "u-1", "read")
	surface(t, h, mainAgent, "u-2", "grep")

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "success"))

	// Assert
	if got := quietLine(t, h); got != "" {
		t.Fatalf("line = %q, want none: the grep still runs, so the feed is moving", got)
	}
}

func TestAnUndrawnItemSurfacingLeavesTheLineStanding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "start"))
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "success"))

	// Act: a tool the feed draws no row for.
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-2", "unmodeled", "start"))

	// Assert
	if got := quietLine(t, h); got != "✅ Bash finished — handling result..." {
		t.Fatalf("line = %q, want the bash line: nothing surfaced in the feed", got)
	}
}

func TestALandingReplacesAStandingNotification(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, notificationFrame("look at this"))
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "start"))

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "success"))

	// Assert
	activity := working(t, h).GetActivity()
	if activity.GetNotification() != nil || activity.GetQuietStretch() == nil {
		t.Fatalf("activity = %v, want the quiet-stretch line in the notification's place", activity)
	}
}

func TestARunningHookOutranksTheQuietLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Assert
	if got := working(t, h).GetActivity().GetHook().GetName(); got != "pre-commit" {
		t.Fatalf("activity = %v, want the running hook's line", working(t, h).GetActivity())
	}
}

func TestTheTurnsEndRetiresTheQuietLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	endTurn(h, testTurnID)
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, nil, nil))

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetBackground().GetActivity().GetQuietStretch(); got != nil {
		t.Fatalf("background line = %v, want none: the turn's line ended with it", got)
	}
}

func TestADetachedCallLeavesTheStepAndStandsTheLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "subagent", "start"))

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, movedSubagent("u-1"))

	// Assert
	arm := working(t, h)
	if got := stepName(arm); got != "thinking" {
		t.Fatalf("step = %q, want thinking: detached work is never a step", got)
	}
	if got := arm.GetActivity().GetQuietStretch().GetText(); got != "✅ Moved to background — continuing..." {
		t.Fatalf("line = %q, want the moved-to-background line", got)
	}
}

func TestADetachedUnitsLaterFramesAreIgnored(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "start"))
	h.r.OnDetachedWork(testWS, mainAgent, &conversationv1.AgentDetachedWork{
		Work: workID("u-1"),
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: "u-1"},
		}},
	})

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "tail"))

	// Assert
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: the shell left the turn", got)
	}
}

func TestAMonitorSurfacesAndHandsOffAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "monitor", "start"))

	// Assert
	arm := working(t, h)
	if got := arm.GetActivity().GetQuietStretch().GetText(); got != "✅ Monitor started — continuing..." {
		t.Fatalf("line = %q, want the monitor-started line", got)
	}
	if got := stepName(arm); got != "thinking" {
		t.Fatalf("step = %q, want thinking: a monitor is always detached", got)
	}
}

func TestABackgroundLandingStandsItsLine(t *testing.T) {
	tests := []struct {
		name string
		act  func(t *testing.T, h *harness)
		want string
	}{
		{"a detached subagent finishing", func(t *testing.T, h *harness) {
			h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(false))
		}, "✅ Subagent finished"},
		{"a detached subagent failing", func(t *testing.T, h *harness) {
			h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(true))
		}, "❌ Subagent failed"},
		{"a detached shell failing", func(t *testing.T, h *harness) {
			h.r.OnBash(testWS, workID("w-1"), &conversationv1.AgentBash{
				Result: &conversationv1.AgentBash_Failure{Failure: &conversationv1.AgentBashFailure{}}})
		}, "❌ Bash failed"},
		{"a subagent's own read landing", func(t *testing.T, h *harness) {
			h.r.OnActivity(testWS, detachedAgent, itemFrame(t, "u-9", "read", "success"))
		}, "✅ Read finished"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2", "w-2"}, nil, nil))

			// Act
			tt.act(t, h)

			// Assert
			got := h.view(t).GetStrip().GetStatus().GetBackground().GetActivity().GetQuietStretch().GetText()
			if got != tt.want {
				t.Fatalf("background line = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestABackgroundSurfacingClearsTheLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, nil, nil))
	h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(false))

	// Act
	surface(t, h, detachedAgent, "u-9", "read")

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetBackground().GetActivity().GetQuietStretch(); got != nil {
		t.Fatalf("background line = %v, want none: the next item surfaced", got)
	}
}

func TestADetachedLandingDuringATurnStandsNoLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, nil, nil))
	inTurn(h)
	surface(t, h, mainAgent, "u-1", "response")

	// Act
	h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(false))

	// Assert
	if got := quietLine(t, h); got != "" {
		t.Fatalf("line = %q, want none: background items are not surfaced while a turn runs", got)
	}
}

func TestTheBackgroundLineEndsWithTheLastDetachedWork(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, nil, nil))
	h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(false))

	// Act
	h.r.OnLiveWorkChanged(testWS, LiveWorkSet{})
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-3"}, nil, nil))

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetBackground().GetActivity().GetQuietStretch(); got != nil {
		t.Fatalf("background line = %v, want none: it stood for work that has ended", got)
	}
}

func TestAQuietStretchWithNoLineIsRecordedAtError(t *testing.T) {
	// Arrange: a delivered turn whose line was lost, which the tracker never
	// produces on its own.
	h := newHarness(t)
	inTurn(h)
	h.r.mu.Lock()
	s := h.r.states[testWS]
	s.motion.line = nil

	// Act
	h.r.checkQuietStretch(testWS, s, "under_test")
	h.r.mu.Unlock()

	// Assert
	records := recordsOf(h.log.Records(), "daemon.footer.quiet_stretch_without_line")
	if len(records) != 1 || records[0].Level != dlog.LevelError {
		t.Fatalf("records = %+v, want one ERROR", records)
	}
	if records[0].Context["cause"] != "under_test" || records[0].Context["invariant_violation"] == nil {
		t.Fatalf("context = %v, want the cause and the invariant", records[0].Context)
	}
}

func TestAnOrdinaryTurnRecordsNoQuietStretchError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "start"))
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "success"))
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-2", "response", "start"))
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-2", "response", "success"))

	// Assert
	if got := countOf(h.log.Records(), dlog.LevelError, "daemon.footer.quiet_stretch_without_line"); got != 0 {
		t.Fatalf("%d quiet-stretch errors, want none", got)
	}
}

func TestAnUnknownItemPhaseIsRecordedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	saved := itemPhases["tail"]
	delete(itemPhases, "tail")
	t.Cleanup(func() { itemPhases["tail"] = saved })

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "tail"))

	// Assert
	records := recordsOf(h.log.Records(), "daemon.footer.quiet_stretch_unknown_phase")
	if len(records) != 1 || records[0].Level != dlog.LevelError || records[0].Context["phase_arm"] != "tail" {
		t.Fatalf("records = %+v, want one ERROR naming the arm", records)
	}
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: an unread frame opens nothing", got)
	}
}

func TestEveryActivityArmHasAFeedKind(t *testing.T) {
	items := (&conversationv1.AgentActivity{}).ProtoReflect().Descriptor().Oneofs().ByName("item").Fields()
	for i := 0; i < items.Len(); i++ {
		if _, ok := feedKinds[items.Get(i).Name()]; !ok {
			t.Errorf("AgentActivity arm %q has no footer.feedKinds entry", items.Get(i).Name())
		}
	}
	if len(feedKinds) != items.Len() {
		t.Errorf("feedKinds holds %d arms, the contract declares %d", len(feedKinds), items.Len())
	}
}

func TestEveryItemPhaseArmIsRead(t *testing.T) {
	items := (&conversationv1.AgentActivity{}).ProtoReflect().Descriptor().Oneofs().ByName("item").Fields()
	for i := 0; i < items.Len(); i++ {
		item := items.Get(i).Message()
		if item.Oneofs().Len() != 1 {
			t.Errorf("%s carries %d oneofs, want exactly one", item.FullName(), item.Oneofs().Len())
			continue
		}
		arms := item.Oneofs().Get(0).Fields()
		for j := 0; j < arms.Len(); j++ {
			if _, ok := itemPhases[arms.Get(j).Name()]; !ok {
				t.Errorf("%s arm %q has no footer.itemPhases entry", item.FullName(), arms.Get(j).Name())
			}
		}
	}
}

func TestLandedHeadWordsEveryOutcome(t *testing.T) {
	tests := []struct {
		name  string
		phase itemPhase
		want  string
	}{
		{"finished", phaseFinished, "✅ Bash finished"},
		{"failed", phaseFailed, "❌ Bash failed"},
		{"cancelled", phaseCancelled, "❌ Bash cancelled"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := landedHead("Bash", tt.phase); got != tt.want {
				t.Fatalf("landedHead = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestEveryLandedLineStartsWithTheSharedHead pins that both landed lines are
// built on landedHead, so a hand-worded line cannot drift from it.
func TestEveryLandedLineStartsWithTheSharedHead(t *testing.T) {
	kind := feedKinds["read"]
	for _, phase := range []itemPhase{phaseFinished, phaseFailed, phaseCancelled} {
		head := landedHead(kind.label, phase)
		if got := turnLandedLine(kind, phase); !strings.HasPrefix(got, head+" — ") {
			t.Errorf("turnLandedLine(%v) = %q, want it to start with %q", phase, got, head)
		}
		if got := backgroundLandedLine(kind.label, phase); got != head {
			t.Errorf("backgroundLandedLine(%v) = %q, want %q", phase, got, head)
		}
	}
}

// ---- the ended line is held until the client paints its successor ----

// endingOf reads the published working arm's quiet-stretch ending.
func endingOf(t *testing.T, h *harness) *frontendv1.FooterStatusQuietStretchEnding {
	t.Helper()
	return working(t, h).GetQuietStretchEnding()
}

func TestTheDrawingThatEndsAQuietStretchStatesTheEndedLineWithItsRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	surface(t, h, mainAgent, "u-1", "bash")
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "success"))

	// Act
	surface(t, h, mainAgent, "u-2", "response")

	// Assert
	got := endingOf(t, h)
	if got.GetText() != "✅ Bash finished — handling result..." || got.GetUntilPainted().GetValue() != "row-u-2" {
		t.Fatalf("ending = %v, want the bash line held until row-u-2 is painted", got)
	}
	if line := quietLine(t, h); line != "" {
		t.Fatalf("line = %q, want none in the activity: the stretch ended", line)
	}
}

func TestASurfacingTheFeedHasNotDrawnLeavesTheLineStanding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act: a spawn the feed holds for the frame naming its agent.
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "subagent", "start"))

	// Assert
	if got := quietLine(t, h); got != "✅ Prompt delivered — awaiting response..." {
		t.Fatalf("line = %q, want the delivered line: nothing is drawn yet", got)
	}
	if got := endingOf(t, h); got != nil {
		t.Fatalf("ending = %v, want none before the draw", got)
	}
}

func TestADrawAfterItsSurfacingEndsTheStretch(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "subagent", "start"))

	// Act
	h.r.OnItemDrawn(testWS, "u-1", rowOf("u-1"), true)

	// Assert
	got := endingOf(t, h)
	if got.GetText() != "✅ Prompt delivered — awaiting response..." || got.GetUntilPainted().GetValue() != "row-u-1" {
		t.Fatalf("ending = %v, want the delivered line held until row-u-1 is painted", got)
	}
	if line := quietLine(t, h); line != "" {
		t.Fatalf("line = %q, want none in the activity once the item is drawn", line)
	}
}

func TestADrawOfAnItemNotYetSurfacedEndsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act: the feed draws before the footer takes the frame.
	h.r.OnItemDrawn(testWS, "u-1", rowOf("u-1"), true)

	// Assert
	if got := quietLine(t, h); got != "✅ Prompt delivered — awaiting response..." {
		t.Fatalf("line = %q, want the delivered line until the item surfaces", got)
	}
}

func TestTheNextQuietLineRetiresTheEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	surface(t, h, mainAgent, "u-1", "bash")

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "success"))

	// Assert
	if got := endingOf(t, h); got != nil {
		t.Fatalf("ending = %v, want none: a new line stands", got)
	}
	if got := quietLine(t, h); got != "✅ Bash finished — handling result..." {
		t.Fatalf("line = %q, want the bash line", got)
	}
}

func TestAnActivityThatOutranksTheQuietLineHoldsNoEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	surface(t, h, mainAgent, "u-1", "response")

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Assert
	if got := endingOf(t, h); got != nil {
		t.Fatalf("ending = %v, want none: the running hook outranks the line and is drawn at once", got)
	}
}

func TestTheTurnsEndRetiresTheEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	surface(t, h, mainAgent, "u-1", "response")

	// Act
	endTurn(h, testTurnID)
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, nil, nil))

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetBackground().GetQuietStretchEnding(); got != nil {
		t.Fatalf("background ending = %v, want none: the turn's ending ended with it", got)
	}
}

func TestABackgroundDrawingStatesTheEndedLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, nil, nil))
	h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(false))

	// Act
	surface(t, h, detachedAgent, "u-9", "read")

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetBackground().GetQuietStretchEnding()
	if got.GetText() != "✅ Subagent finished" || got.GetUntilPainted().GetValue() != "row-u-9" {
		t.Fatalf("background ending = %v, want the subagent line held until row-u-9 is painted", got)
	}
}

func TestALandedItemsRowIsForgotten(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	surface(t, h, mainAgent, "u-1", "bash")

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "success"))

	// Assert
	h.r.mu.Lock()
	_, kept := h.r.stateLocked(testWS).motion.drawn["u-1"]
	h.r.mu.Unlock()
	if kept {
		t.Fatal("the landed item's row is still held; drawn rows must not outlive their items")
	}
}

func TestADrawnRowWithNoIdentityIsAnError(t *testing.T) {
	tests := []struct {
		name string
		unit string
		row  *frontendv1.FeedId
	}{
		{name: "no unit", unit: "", row: rowOf("u-1")},
		{name: "no FeedId", unit: "u-1", row: &frontendv1.FeedId{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			inTurn(h)
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "subagent", "start"))

			// Act
			h.r.OnItemDrawn(testWS, tc.unit, tc.row, true)

			// Assert
			if got := countOf(h.log.Records(), dlog.LevelError, "daemon.footer.item_drawn_unaddressed"); got != 1 {
				t.Fatalf("errors = %d, want 1 item_drawn_unaddressed record", got)
			}
			if got := quietLine(t, h); got == "" {
				t.Fatal("an unaddressed draw ended the stretch")
			}
		})
	}
}

func TestOnlyActivitiesAboveTheQuietLineOutrankIt(t *testing.T) {
	working := []struct {
		name string
		act  *frontendv1.FooterStatusWorkingActivity
		want bool
	}{
		{"none", nil, false},
		{"a fault", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_Fault{}}, true},
		{"an update", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_Update{}}, true},
		{"a notification", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_Notification{}}, true},
		{"a compaction", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_Compaction{}}, true},
		{"a hook", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_Hook{}}, true},
		{"a retry", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_Retrying{}}, true},
		{"a new quiet line", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_QuietStretch{}}, true},
		{"injected context", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_ContextInjected{}}, false},
		{"a rate limit", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_RateLimited{}}, false},
		{"a context budget", &frontendv1.FooterStatusWorkingActivity{Kind: &frontendv1.FooterStatusWorkingActivity_ContextBudget{}}, false},
	}
	for _, tc := range working {
		t.Run("working: "+tc.name, func(t *testing.T) {
			if got := workingOutranksQuietLine(tc.act); got != tc.want {
				t.Fatalf("workingOutranksQuietLine = %v, want %v", got, tc.want)
			}
		})
	}
	background := []struct {
		name string
		act  *frontendv1.FooterStatusBackgroundActivity
		want bool
	}{
		{"none", nil, false},
		{"a fault", &frontendv1.FooterStatusBackgroundActivity{Kind: &frontendv1.FooterStatusBackgroundActivity_Fault{}}, true},
		{"an update", &frontendv1.FooterStatusBackgroundActivity{Kind: &frontendv1.FooterStatusBackgroundActivity_Update{}}, true},
		{"a notification", &frontendv1.FooterStatusBackgroundActivity{Kind: &frontendv1.FooterStatusBackgroundActivity_Notification{}}, true},
		{"a new quiet line", &frontendv1.FooterStatusBackgroundActivity{Kind: &frontendv1.FooterStatusBackgroundActivity_QuietStretch{}}, true},
		{"a rate limit", &frontendv1.FooterStatusBackgroundActivity{Kind: &frontendv1.FooterStatusBackgroundActivity_RateLimited{}}, false},
		{"a context budget", &frontendv1.FooterStatusBackgroundActivity{Kind: &frontendv1.FooterStatusBackgroundActivity_ContextBudget{}}, false},
	}
	for _, tc := range background {
		t.Run("background: "+tc.name, func(t *testing.T) {
			if got := backgroundOutranksQuietLine(tc.act); got != tc.want {
				t.Fatalf("backgroundOutranksQuietLine = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestTheEndingCarriesWhenTheEndedLineBeganStanding(t *testing.T) {
	// Arrange: the line stands at instant+2s, and is ended 5s later.
	h := newHarness(t)
	inTurn(h)
	surface(t, h, mainAgent, "u-1", "bash")
	h.clock.Advance(2 * time.Second)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "success"))
	h.clock.Advance(5 * time.Second)

	// Act
	surface(t, h, mainAgent, "u-2", "response")

	// Assert
	if got, want := endingOf(t, h).GetAt().GetAtMs(), instant.Add(2*time.Second).UnixMilli(); got != want {
		t.Fatalf("ending at = %d, want %d: the line's own standing instant", got, want)
	}
}

func TestASubFeedDrawingEndsTheStretchWithNoEnding(t *testing.T) {
	// Arrange: background, with a subagent's line standing.
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, nil, nil))
	h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(false))

	// Act: the next item is drawn on the subagent's own sub-feed.
	h.r.OnItemDrawn(testWS, "u-9", rowOf("u-9"), false)
	h.r.OnActivity(testWS, detachedAgent, itemFrame(t, "u-9", "read", "start"))

	// Assert
	background := h.view(t).GetStrip().GetStatus().GetBackground()
	if background.GetActivity().GetQuietStretch() != nil || background.GetQuietStretchEnding() != nil {
		t.Fatalf("background = %v, want the line ended with no hold: the client cannot promise to paint a sub-feed row", background)
	}
}
