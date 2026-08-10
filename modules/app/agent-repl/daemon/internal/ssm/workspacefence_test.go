package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestCompositeWorkspaceStateFence pins the ANSWER every other fenced push is
// measured against.
//
// The failure it exists for: a daemon bounce leaves a detached workspace
// hibernated with no controller generation yet. The WorkspaceState published in
// that window used to carry Fence(session, "") — a token no fenced view could
// ever equal (every view is published from a live generation) and one the
// resync eligibility ladder rejects as identity_mismatch. A client that adopted
// it had its status chrome frozen and its resyncs refused until some later
// WorkspaceState happened to reach it, which for a workspace nobody focuses
// never happened.
func TestCompositeWorkspaceStateFence(t *testing.T) {
	tests := []struct {
		name       string
		session    string
		generation ControllerGenerationID
		want       string
	}{
		{
			name:       "live generation composes both identities",
			session:    "s_9de8689040244f34",
			generation: ControllerGenerationID("cg_25e7e7dd"),
			want:       Fence("s_9de8689040244f34", "cg_25e7e7dd"),
		},
		{
			name:       "absent generation yields an absent fence",
			session:    "s_9de8689040244f34",
			generation: ControllerGenerationID(""),
			want:       "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			projection := resolved{found: true, state: frontendv1.RenderState_RENDER_STATE_IDLE}
			composite := CompositeState{
				AgentReplSessionID:     tc.session,
				ControllerGenerationID: tc.generation,
				Connectivity:           SessionConnectivityHibernated,
			}

			// Act.
			msg, err := compositeWorkspaceState("/ws", projection, composite)

			// Assert.
			if err != nil {
				t.Fatalf("compositeWorkspaceState: %v", err)
			}
			if got := msg.GetFence(); got != tc.want {
				t.Fatalf("fence = %q, want %q", got, tc.want)
			}
		})
	}
}
