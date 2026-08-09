package frontend

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func hibernationDetail() *frontendv1.HibernationDetail {
	return &frontendv1.HibernationDetail{
		SinceMs: 1700,
		Cause: &frontendv1.HibernationDetail_Forced{
			Forced: &frontendv1.HibernationForced{},
		},
	}
}

func TestWorkspaceGateViewRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange + Act.
	view, err := WorkspaceGateView("", "s|g", false, nil)
	// Assert.
	if err == nil {
		t.Fatalf("a gate with no workspace resolved: %v", view)
	}
}

func TestWorkspaceGateViewRefusesAnEmptyFence(t *testing.T) {
	// Arrange + Act.
	view, err := WorkspaceGateView("/w", "", false, nil)
	// Assert.
	if err == nil {
		t.Fatalf("an unfenced gate resolved: %v", view)
	}
}

func TestAnAwakeWorkspaceResolvesTheOpenArm(t *testing.T) {
	// Arrange + Act.
	view, err := WorkspaceGateView("/w", "s|g", false, nil)
	// Assert.
	if err != nil {
		t.Fatalf("WorkspaceGateView: %v", err)
	}
	if view.GetOpen() == nil {
		t.Fatalf("an awake workspace did not resolve the open arm")
	}
}

func TestAHibernatedWorkspaceResolvesTheHibernatedArmWithItsAccount(t *testing.T) {
	// Arrange — a closed gate always arrives with the account the card renders.
	// Act.
	view, err := WorkspaceGateView("/w", "s|g", true, hibernationDetail())
	// Assert.
	if err != nil {
		t.Fatalf("WorkspaceGateView: %v", err)
	}
	if view.GetHibernated().GetDetail().GetForced() == nil {
		t.Fatalf("the hibernated arm carries no cause the revival card can state")
	}
}

func TestAHibernatedWorkspaceWithNoAccountIsRefused(t *testing.T) {
	// Arrange — closing a composer with nothing to explain it is the state the
	// arms exist to make unrepresentable.
	// Act.
	view, err := WorkspaceGateView("/w", "s|g", true, nil)
	// Assert.
	if err == nil {
		t.Fatalf("a hibernated gate resolved with no hibernation detail: %v", view)
	}
}

func TestWorkspaceGateViewCarriesTheFenceItWasGiven(t *testing.T) {
	// Arrange + Act.
	view, err := WorkspaceGateView("/w", "s_3|g_3", false, nil)
	// Assert.
	if err != nil {
		t.Fatalf("WorkspaceGateView: %v", err)
	}
	if view.GetFence() != "s_3|g_3" {
		t.Fatalf("fence = %q, want the fence supplied verbatim", view.GetFence())
	}
}
