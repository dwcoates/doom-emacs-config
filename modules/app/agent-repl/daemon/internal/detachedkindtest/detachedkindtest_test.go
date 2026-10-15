package detachedkindtest

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/protoarm"
)

func TestEachBuilderSetsItsOwnKindArm(t *testing.T) {
	tests := []struct {
		name string
		kind *conversationv1.DetachedWorkKind
		want string
	}{
		{name: "a subagent", kind: Subagent(&conversationv1.AgentId{Value: "agent-1"}), want: "subagent"},
		{name: "a shell", kind: Bash(), want: "bash"},
		{name: "a monitor", kind: Monitor(), want: "monitor"},
		{name: "a workflow", kind: Workflow(), want: "workflow"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := protoarm.Must(tt.kind.ProtoReflect(), "kind", "test")

			// Assert
			if string(got) != tt.want {
				t.Fatalf("kind arm = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestSubagentCarriesTheAgentItRunsAs(t *testing.T) {
	// Act
	got := Subagent(&conversationv1.AgentId{Value: "agent-1"})

	// Assert
	if id := got.GetSubagent().GetAgentId().GetValue(); id != "agent-1" {
		t.Fatalf("agent = %q, want agent-1", id)
	}
}
