package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/rollout"
	"claude-repld/internal/workspace"
)

// TestAdoptWebAnswersNoTransferAnnouncedOnAnOrdinaryBoot pins the project-lead
// ruling: the web side never redials, so EVERY non-handover page boot calls
// AdoptWebWorkspace and gets this answer. It is an answer, not a fault.
func TestAdoptWebAnswersNoTransferAnnouncedOnAnOrdinaryBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptWebErr = rollout.ErrNoTransferAnnounced

	// Act.
	resp, err := h.Client.AdoptWebWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptWebWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetNoTransferAnnounced() == nil {
		t.Fatalf("result = %v, want no_transfer_announced", resp.Msg.GetResult())
	}
}

// TestAdoptWebAnswersNotYetAdoptedWhileAdoptionRuns pins the retry answer the
// page backs off on.
func TestAdoptWebAnswersNotYetAdoptedWhileAdoptionRuns(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptWebErr = rollout.ErrNotYetAdopted

	// Act.
	resp, err := h.Client.AdoptWebWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptWebWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetNotYetAdopted() == nil {
		t.Fatalf("result = %v, want not_yet_adopted", resp.Msg.GetResult())
	}
}

// TestAdoptHostAnswersParticipantNotExpected pins the third rollout refusal: a
// caller whose stream was not open at announcement.
func TestAdoptHostAnswersParticipantNotExpected(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptHostErr = rollout.ErrParticipantNotExpected

	// Act.
	resp, err := h.Client.AdoptHostWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptHostWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetParticipantNotExpected() == nil {
		t.Fatalf("result = %v, want participant_not_expected", resp.Msg.GetResult())
	}
}

// TestAdoptionIsReachableOnAJoiningDaemon pins that the adoption calls do NOT
// refuse on `not_yet_adopted` standing: that is exactly the state they exist to
// leave, and refusing would make the rendezvous unreachable.
func TestAdoptionIsReachableOnAJoiningDaemon(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Ownership.standing = workspace.StandingNotYetAdopted

	// Act.
	resp, err := h.Client.AdoptHostWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptHostWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success on a joining daemon", resp.Msg.GetResult())
	}
}

// TestAdoptionRefusesAnUnknownWorkspace pins that the rendezvous still keys on a
// registered workspace.
func TestAdoptionRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.AdoptHostWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptHostWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}
