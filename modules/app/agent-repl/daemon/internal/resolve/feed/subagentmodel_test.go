package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// modelFrame is one of an agent's API responses naming MODEL, riding a
// thinking unit as the vendor's observed shape does.
func modelFrame(unit, model string) *conversationv1.AgentActivity {
	frame := thinkingFrame(unit, nil)
	frame.Model = &conversationv1.AgentModel{Name: model}
	return frame
}

// subagentAnswers sends one of the subagent's own API responses naming MODEL.
func (h *harness) subagentAnswers(created *conversationv1.AgentId, unit, model string) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, created, modelFrame(unit, model), nil, nil)
}

func TestTheSubagentCardDrawsTheModelItsResponseNamed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act.
	h.subagentAnswers(created, "sub-1", "claude-sonnet-5")

	// Assert.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetModel().GetName(); got != "claude-sonnet-5" {
		t.Fatalf("model = %q, want claude-sonnet-5", got)
	}
}

func TestTheSubagentCardDrawsNoModelBeforeAnyResponseNamesOne(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act.
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Assert: nothing is guessed from the parent or the request.
	if model := bubbleOf(h.bubbleRow("spawn-1", created)).GetModel(); model != nil {
		t.Fatalf("model = %q, want unset", model.GetName())
	}
}

func TestTheSubagentCardDrawsItsLatestModel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	h.subagentAnswers(created, "sub-1", "claude-sonnet-5")

	// Act: the run's model changed mid-way.
	h.subagentAnswers(created, "sub-2", "claude-opus-5")

	// Assert.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetModel().GetName(); got != "claude-opus-5" {
		t.Fatalf("model = %q, want the latest, claude-opus-5", got)
	}
}

func TestAResponseNamingNoModelKeepsTheCardsModel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	h.subagentAnswers(created, "sub-1", "claude-sonnet-5")

	// Act: a response whose producer reported no model.
	h.subagentSpends(created, "sub-2", misses(10, 0))

	// Assert.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetModel().GetName(); got != "claude-sonnet-5" {
		t.Fatalf("model = %q, want claude-sonnet-5 kept", got)
	}
}

func TestTheMainAgentsModelNeverReachesACard(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act: the main agent answers.
	h.send(modelFrame("main-1", "claude-opus-5"))

	// Assert.
	if model := bubbleOf(h.bubbleRow("spawn-1", created)).GetModel(); model != nil {
		t.Fatalf("model = %q, want unset: the main agent's model is not the subagent's", model.GetName())
	}
}

func TestADetachedSubagentCardDrawsTheModelItsResponseNamed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	h.detachSubagentWork("w-1", "spawn-1", created.GetValue())

	// Act.
	h.subagentAnswers(created, "sub-1", "claude-sonnet-5")

	// Assert.
	row := h.bubbleRow("spawn-1", created)
	if got := row.GetDetachedSubagent().GetSubagent().GetModel().GetName(); got != "claude-sonnet-5" {
		t.Fatalf("row = %+v, want the detached head carrying claude-sonnet-5", row)
	}
}

func TestRecordModel(t *testing.T) {
	tests := []struct {
		name     string
		agent    string
		filed    string
		model    *conversationv1.AgentModel
		wantMove bool
		want     string
	}{
		{name: "a first model", agent: "a", model: &conversationv1.AgentModel{Name: "m1"}, wantMove: true, want: "m1"},
		{name: "a changed model", agent: "a", filed: "m1", model: &conversationv1.AgentModel{Name: "m2"}, wantMove: true, want: "m2"},
		{name: "the same model", agent: "a", filed: "m1", model: &conversationv1.AgentModel{Name: "m1"}, wantMove: false, want: "m1"},
		{name: "no model", agent: "a", filed: "m1", model: nil, wantMove: false, want: "m1"},
		{name: "an empty name", agent: "a", filed: "m1", model: &conversationv1.AgentModel{}, wantMove: false, want: "m1"},
		{name: "no agent", agent: "", model: &conversationv1.AgentModel{Name: "m1"}, wantMove: false, want: ""},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			s := newHarness(t).resolver.state(testWorkspace)
			if test.filed != "" {
				s.agentModel[test.agent] = test.filed
			}

			// Act.
			moved := s.recordModel(&conversationv1.AgentId{Value: test.agent}, test.model)

			// Assert.
			if moved != test.wantMove || s.agentModel[test.agent] != test.want {
				t.Fatalf("moved %v filed %q, want %v %q", moved, s.agentModel[test.agent], test.wantMove, test.want)
			}
		})
	}
}
