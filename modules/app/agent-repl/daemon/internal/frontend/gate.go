package frontend

// gate.go resolves the WORKSPACE GATE VIEW: whether prompts may be sent to a
// workspace right now, and — when they may not — the account the revival card
// needs to say what put the session to sleep.
//
// IT ANSWERS THE QUESTION A GATE ACTUALLY HAS. A rendering frontend used to
// read hibernation off the session catalog, which answers "what is true of
// session X" and so forced the reader to first work out which X was current.
// This answers "what is true of this workspace now", fenced like every other
// per-workspace push.
//
// THE ARMS ARE THE POINT. A closed gate always arrives WITH its detail,
// because the gate and the reason for it are one fact: a hibernated arm with
// nothing inside would leave the revival card asking the user to choose a
// wake-up mode without being able to say what the session is asleep from.
//
// THE FENCE IS NOT MINTED HERE; see topbar.go.

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// WorkspaceGateView resolves one workspace's revival gate completely.
//
// hibernated is the DURABLE record's own hibernation flag and detail is that
// record's typed account of it, so both come from the one authority rather
// than one being a live guess beside the other's durable fact.
//
// A hibernated workspace with no detail is REFUSED. The registry refuses to
// write a hibernation without a cause, so reaching here without one means the
// caller paired the two facts wrongly; publishing the gate anyway would close
// a composer against a workspace with no account of why.
func WorkspaceGateView(workspace, fence string, hibernated bool, detail *frontendv1.HibernationDetail) (*frontendv1.WorkspaceGateView, error) {
	if workspace == "" {
		return nil, fmt.Errorf("frontend: workspace gate view requires a workspace")
	}
	if fence == "" {
		return nil, fmt.Errorf("frontend: workspace gate view for workspace %q requires the workspace's fence; an unfenced push cannot be told from a stale one", workspace)
	}
	view := &frontendv1.WorkspaceGateView{Workspace: workspace, Fence: fence}
	if !hibernated {
		view.Gate = &frontendv1.WorkspaceGateView_Open{Open: &frontendv1.WorkspaceGateOpen{}}
		return view, nil
	}
	if detail == nil {
		return nil, fmt.Errorf("frontend: workspace gate view for workspace %q is hibernated with no hibernation detail; the gate is withheld rather than closing a composer with nothing to explain it", workspace)
	}
	view.Gate = &frontendv1.WorkspaceGateView_Hibernated{
		Hibernated: &frontendv1.WorkspaceGateHibernated{Detail: detail},
	}
	return view, nil
}
