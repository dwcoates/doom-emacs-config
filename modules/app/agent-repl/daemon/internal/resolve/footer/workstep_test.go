package footer

import (
	"testing"
	"time"

	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/shimclient"
)

// itemFrame builds one activity frame generically: the item arm named ARM
// carrying its own oneof arm PHASE, empty otherwise. The empty payloads are
// all the working step reads.
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
	// Arrange: the link is up, and the main agent is never named.
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)
	h.r.OnLink(testWS, shimclient.LinkConnected)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnTurnOpened(testWS, testTurnID)

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "start"))

	// Assert
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: attribution to the main agent is never guessed", got)
	}
}

func TestADetachedUnitsLaterFramesAreIgnored(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "start"))
	h.r.OnDetachedWork(testWS, mainAgent, &conversationv1.AgentDetachedWork{
		Owner: mainAgent,
		Work:  workID("u-1"),
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
	records := recordsOf(h.log.Records(), "daemon.footer.work_step_unknown_phase")
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

func TestARunningTurnFoundAtAttachIsWorkingFromItsRowsStart(t *testing.T) {
	tests := []struct {
		name      string
		startedAt *time.Time
		wantClock time.Time
	}{
		{name: "with its row's start", startedAt: func() *time.Time { at := instant.Add(-3 * time.Minute); return &at }(), wantClock: instant.Add(-3 * time.Minute)},
		{name: "with no row the clock counts from the attach", startedAt: nil, wantClock: instant},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a daemon that took a busy workspace over.
			h := newHarness(t)
			connected(h)
			h.r.OnMainAgent(testWS, mainAgent)

			// Act.
			h.r.OnTurnRunningAtAttach(testWS, testTurnID, tc.startedAt)

			// Assert.
			if got := stepName(working(t, h)); got == "submitting" || got == "" {
				t.Fatalf("step = %q, want a running step: the turn is past its submission", got)
			}
			clock := h.view(t).GetStrip().GetClock()
			if clock.TurnStartedAtMs == nil || *clock.TurnStartedAtMs != tc.wantClock.UnixMilli() {
				t.Fatalf("clock = %v, want the turn's start %v", clock, tc.wantClock)
			}
		})
	}
}

func TestARunningTurnFoundAtAttachLeavesAStandingTurn(t *testing.T) {
	// Arrange: this daemon's own turn stands.
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "bash", "start"))

	// Act.
	h.r.OnTurnRunningAtAttach(testWS, testTurnID, nil)

	// Assert.
	if got := stepName(working(t, h)); got != "executing" {
		t.Fatalf("step = %q, want the standing turn's executing step untouched", got)
	}
}

func TestADetachedCallLeavesTheStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "subagent", "start"))

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, movedSubagent("u-1"))

	// Assert
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: detached work is never a step", got)
	}
}

func TestAMonitorNamesNoStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "monitor", "start"))

	// Assert
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: a monitor is detached at its first frame", got)
	}
}

func TestAnUnknownItemArmIsRecordedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	saved := feedKinds["read"]
	delete(feedKinds, "read")
	t.Cleanup(func() { feedKinds["read"] = saved })

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "start"))

	// Assert
	records := recordsOf(h.log.Records(), "daemon.footer.work_step_unknown_item")
	if len(records) != 1 || records[0].Level != dlog.LevelError || records[0].Context["arm"] != "read" {
		t.Fatalf("records = %+v, want one ERROR naming the arm", records)
	}
	if got := stepName(working(t, h)); got != "thinking" {
		t.Fatalf("step = %q, want thinking: an unread frame opens nothing", got)
	}
}
