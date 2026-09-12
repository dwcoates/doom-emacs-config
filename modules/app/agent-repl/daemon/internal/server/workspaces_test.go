package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/merge"
	"claude-repld/internal/workspace"
)

// TestUnknownWorkspaceIsRefusedByArm pins that a ref naming an id the registry
// does not hold answers the unknown_workspace arm.
func TestUnknownWorkspaceIsRefusedByArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// TestWorkspaceRefMismatchCarriesTheRegistryDir pins the ruling: the daemon keys
// on `id` and REFUSES a ref whose dir disagrees, telling the caller what the
// registry actually holds.
func TestWorkspaceRefMismatchCarriesTheRegistryDir(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if got := resp.Msg.GetError().GetWorkspaceRefMismatch().GetRegistryDir(); got != testWorkspaceDir {
		t.Fatalf("registry_dir = %q, want %q", got, testWorkspaceDir)
	}
}

// TestTransferringAwayCarriesTheSuccessorAddress pins the OLD daemon's refusal
// after the transfer notice: the caller is told where to go.
func TestTransferringAwayCarriesTheSuccessorAddress(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Ownership.standing = workspace.StandingTransferringAway

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if got := resp.Msg.GetError().GetTransferringAway().GetAddress(); got != "127.0.0.1:9999" {
		t.Fatalf("address = %q, want the successor's", got)
	}
}

// TestNotYetAdoptedIsRefusedOnAJoiningDaemon pins the JOINING daemon's refusal
// before adoption.
func TestNotYetAdoptedIsRefusedOnAJoiningDaemon(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Ownership.standing = workspace.StandingNotYetAdopted

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetNotYetAdopted() == nil {
		t.Fatalf("result = %v, want not_yet_adopted", resp.Msg.GetResult())
	}
}

// TestOwnedWorkspaceIsServed pins that a served workspace reaches the verb.
func TestOwnedWorkspaceIsServed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestCloseWorkspaceMapsTheBlockedArm pins that the verbs' close refusal reaches
// the caller as CloseWorkspaceError.blocked.
func TestCloseWorkspaceMapsTheBlockedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.closeErr = &workspace.Refusal{Arm: "blocked", Reason: "a turn is in flight"}

	// Act.
	resp, err := h.Client.CloseWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("CloseWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetBlocked() == nil {
		t.Fatalf("result = %v, want blocked", resp.Msg.GetResult())
	}
}

// TestMergeWorkspaceMapsAPreStateRefusal pins that the orchestrator's arm name
// lands on MergeWorkspaceError.
func TestMergeWorkspaceMapsAPreStateRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.enqueueErr = &merge.RefusalError{Arm: merge.ArmAlreadyQueued, Reason: "already waiting"}

	// Act.
	resp, err := h.Client.MergeWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetAlreadyQueued() == nil {
		t.Fatalf("result = %v, want already_queued", resp.Msg.GetResult())
	}
}

// TestCreateWorkspaceRefusesAnUnknownRepository pins that a repository ref
// matching nothing registered is refused rather than materialized somewhere.
func TestCreateWorkspaceRefusesAnUnknownRepository(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
			Repository: &workspacev1.RepositoryRef{Id: "repo-nope"},
			Form: &agentreplv1.CreateWorkspaceRequest_Standard{
				Standard: &agentreplv1.CreateWorkspaceStandard{},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("result = %v, want unknown_repository", resp.Msg.GetResult())
	}
}

// ---------------------------------------------------------------------------
// ForgetWorkspace. The verb, its three refusals and its command-file route all
// existed before the endpoint did; these pin that the wire now carries them.
// ---------------------------------------------------------------------------

// TestForgetWorkspaceAnswersSuccessWhenTheVerbForgets pins the ordinary answer:
// the record is gone and the caller is told so.
func TestForgetWorkspaceAnswersSuccessWhenTheVerbForgets(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestForgetWorkspaceMapsTheNotClosedArm pins the refusal that keeps the close
// verb the one owner of the quiet requirement.
func TestForgetWorkspaceMapsTheNotClosedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.forgetErr = &workspace.Refusal{
		Rpc: "ForgetWorkspace", Arm: workspace.ArmNotClosed,
		Reason: `workspace "ws-1" is open; close it before forgetting it`,
	}

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetNotClosed() == nil {
		t.Fatalf("result = %v, want not_closed", resp.Msg.GetResult())
	}
}

// TestForgetWorkspaceBlockedCarriesTheSameFiveFieldsACloseDoes pins that the
// re-run quiet check states its evidence, not only a sentence: a hold outlives
// a close when the close raced it, and the caller must be able to see it.
func TestForgetWorkspaceBlockedCarriesTheSameFiveFieldsACloseDoes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.forgetErr = &workspace.Refusal{
		Rpc: "ForgetWorkspace", Arm: "blocked", Reason: "2 held prompts are undelivered",
		Fields: map[string]any{
			"turn_in_flight": true,
			"live_work":      uint32(3),
			"held_prompts":   uint32(2),
			"merge_queued":   true,
			"summary":        "2 held prompts are undelivered",
		},
	}

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	blocked := resp.Msg.GetError().GetBlocked()
	if !blocked.GetTurnInFlight() || blocked.GetLiveWork() != 3 || blocked.GetHeldPrompts() != 2 ||
		!blocked.GetMergeQueued() || blocked.GetSummary() != "2 held prompts are undelivered" {
		t.Fatalf("blocked = %v, want all five fields as the composer stated them", blocked)
	}
}

// TestForgetWorkspaceHasChildrenNamesTheChildren pins the REPEATED arm field:
// the schema's parent_id is ON DELETE SET NULL, so the caller is owed the ids
// it must deal with first rather than a count in a sentence.
func TestForgetWorkspaceHasChildrenNamesTheChildren(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.forgetErr = &workspace.Refusal{
		Rpc: "ForgetWorkspace", Arm: workspace.ArmHasChildren,
		Reason: `2 workspaces were spawned from "ws-1"; forget them first`,
		Fields: map[string]any{"children": []string{"ws-2", "ws-3"}},
	}

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	got := resp.Msg.GetError().GetHasChildren().GetChildren()
	if len(got) != 2 || got[0] != "ws-2" || got[1] != "ws-3" {
		t.Fatalf("children = %v, want the two spawned ids", got)
	}
}

// TestForgetWorkspaceRefusesAnUnknownWorkspace pins that the shared per-verb
// resolution refuses before the verb is reached, as it does for every other
// per-workspace rpc.
func TestForgetWorkspaceRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}
