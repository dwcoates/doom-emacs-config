package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/workspace"
)

// standingDownSelect arranges a harness whose select verb answers the
// stand-down refusal, as a daemon that is leaving does.
func standingDownSelect(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.Verbs.selectErr = &workspace.Refusal{Rpc: "SelectWorkspace", Arm: workspace.ArmStandingDown, Reason: "this daemon is standing down"}
	return h
}

func TestSelectWorkspaceOnADaemonStandingDownAnswersTheStandingDownArm(t *testing.T) {
	// Arrange.
	h := standingDownSelect(t)

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace answered a transport error rather than a typed arm: %v", err)
	}
	if resp.Msg.GetError().GetStandingDown() == nil {
		t.Fatalf("result = %v, want standing_down", resp.Msg.GetResult())
	}
}

func TestSelectWorkspaceOnADaemonStandingDownWarnsNothing(t *testing.T) {
	// Arrange.
	h := standingDownSelect(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log

	// Act.
	if _, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()})); err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}

	// Assert: an expected answer, never an unlanded arm.
	for _, rec := range log.Records() {
		if rec.Level == "warn" || rec.Level == "error" {
			t.Fatalf("recorded %+v, want no WARN or ERROR for a landed arm", rec)
		}
	}
}
