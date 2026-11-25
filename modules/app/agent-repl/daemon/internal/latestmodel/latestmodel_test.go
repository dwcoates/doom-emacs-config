package latestmodel

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestRecord(t *testing.T) {
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
			ledger := Ledger{}
			if test.filed != "" {
				ledger[test.agent] = test.filed
			}

			// Act.
			moved := ledger.Record(&conversationv1.AgentId{Value: test.agent}, test.model)

			// Assert.
			if moved != test.wantMove || ledger.Of(test.agent) != test.want {
				t.Fatalf("moved %v filed %q, want %v %q", moved, ledger.Of(test.agent), test.wantMove, test.want)
			}
		})
	}
}

func TestOfAnAgentNoResponseNamedIsEmpty(t *testing.T) {
	// Arrange.
	ledger := Ledger{"a": "m1"}

	// Act.
	got := ledger.Of("b")

	// Assert.
	if got != "" {
		t.Fatalf("Of(b) = %q, want empty", got)
	}
}
