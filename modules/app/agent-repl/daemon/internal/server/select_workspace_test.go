package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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

func TestSelectingAHiddenMergeWorkspaceRevealsItFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	record := h.DB.workspaces[testWorkspaceID]
	record.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}
	h.DB.workspaces[testWorkspaceID] = record

	// Act.
	if _, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()})); err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}

	// Assert.
	if len(h.Merge.reveals) != 1 || h.Merge.reveals[0] != testWorkspaceID {
		t.Fatalf("reveals = %v, want the merge workspace revealed", h.Merge.reveals)
	}
}

func TestSelectingAnOrdinaryWorkspaceRevealsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()})); err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}

	// Assert.
	if len(h.Merge.reveals) != 0 {
		t.Fatalf("reveals = %v, want none", h.Merge.reveals)
	}
}

func TestSelectingARevealedMergeWorkspaceRevealsNothingMore(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	record := h.DB.workspaces[testWorkspaceID]
	record.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}
	record.MergeRevealed = true
	h.DB.workspaces[testWorkspaceID] = record

	// Act.
	if _, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()})); err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}

	// Assert.
	if len(h.Merge.reveals) != 0 {
		t.Fatalf("reveals = %v, want none", h.Merge.reveals)
	}
}

func TestSelectWorkspaceAnswersTheRosterRevisionItsSelectionIsIn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Deps.Sidebar.(*fakeSidebar).revision = 42

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetRosterRevision(); got != 42 {
		t.Fatalf("roster_revision = %d, want the sidebar's 42", got)
	}
}

func TestSelectWorkspaceLetsGoOfEveryHeldBanner(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()})); err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}

	// Assert.
	if got := h.Banners.forgotten(); len(got) != 1 || got[0] != "a workspace was selected" {
		t.Fatalf("forgets = %q, want the selection to let every banner go", got)
	}
}

func TestARefusedSelectWorkspaceLetsGoOfNoBanner(t *testing.T) {
	// Arrange.
	h := standingDownSelect(t)

	// Act.
	if _, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()})); err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}

	// Assert.
	if got := h.Banners.forgotten(); len(got) != 0 {
		t.Fatalf("forgets = %q, want none for a refused selection", got)
	}
}
