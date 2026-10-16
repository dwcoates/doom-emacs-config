//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

func TestTheEffortSelectorStartsAtTheConfigRootsLevelAndMovesOnTheShimsConfirmation(t *testing.T) {
	t.Parallel()
	// Arrange: the default root persists medium; the fake catalog's opus
	// accepts low, medium and high.
	f := newOpened(t, harness.Opts{DefaultSettings: `{"effortLevel":"medium"}`})
	topbar := f.d.WatchTopbar(f.ws)
	awaitTopbar(t, f, topbar, "the selector at the root's medium", func(v *frontendv1.TopbarView) bool {
		return v.GetEffortSelector().GetSupported().GetCurrent().GetLevel() == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM
	})

	// Act.
	resp, err := f.d.Client().SetEffort(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetEffortRequest{
		Workspace: f.ws,
		Effort:    conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH,
	}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("SetEffort(high) = (%v, %v), want a success", resp, err)
	}
	if got := f.shim.ExpectSetSessionEffort().GetEffort(); got != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH {
		t.Fatalf("SetSessionEffort asked %v, want high", got)
	}
	awaitTopbar(t, f, topbar, "the selector at the confirmed high", func(v *frontendv1.TopbarView) bool {
		return v.GetEffortSelector().GetSupported().GetCurrent().GetLevel() == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH
	})
}
