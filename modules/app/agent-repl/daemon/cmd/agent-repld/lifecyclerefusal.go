package main

import (
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// THE LIFECYCLE VERBS' REFUSALS: open, close, kill, select, restart, park,
// interrupt, compact, status, the cold gate's answer and notify. Each
// describer says every arm of its verb's error in one plain sentence about
// WS, the workspace as the caller named it.

// describeOpenRefusal says in one plain sentence why WS was not opened. Every
// OpenWorkspaceError arm has its own sentence.
func describeOpenRefusal(ws string, e *agentreplv1.OpenWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.OpenWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.OpenWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.OpenWorkspaceError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "open it again"), nil
	case *agentreplv1.OpenWorkspaceError_NotYetAdopted:
		return sayNotYetAdopted(ws, "open it again"), nil
	case *agentreplv1.OpenWorkspaceError_SessionDeleted:
		return fmt.Sprintf("%s's session has been deleted and cannot be reopened", ws), nil
	case *agentreplv1.OpenWorkspaceError_TranscriptMissing:
		t := r.TranscriptMissing
		return fmt.Sprintf("%s's vendor transcript %s is nowhere on disk (searched %s)", ws, t.GetVendorSessionId(), strings.Join(t.GetSearchedPaths(), ", ")), nil
	case *agentreplv1.OpenWorkspaceError_SpawnFailed:
		return fmt.Sprintf("spawning %s's shim failed (%s)", ws, r.SpawnFailed.GetDetail()), nil
	case *agentreplv1.OpenWorkspaceError_VendorStartFailed:
		return fmt.Sprintf("the vendor failed to start %s's session (%s)", ws, r.VendorStartFailed.GetDetail()), nil
	case *agentreplv1.OpenWorkspaceError_LockHolderUnavailable:
		return fmt.Sprintf("%s's shim came up, but its lock helper %s failed, so it would not start a session it could not prove it alone owns", ws, r.LockHolderUnavailable.GetFailure().GetBinary()), nil
	case *agentreplv1.OpenWorkspaceError_WorktreeUnrestorable:
		u := r.WorktreeUnrestorable
		return fmt.Sprintf("%s's directory %s is gone and its branch %s no longer exists, so there is nothing to restore its worktree from (%s)", ws, u.GetDir(), u.GetBranch(), u.GetDetail()), nil
	case *agentreplv1.OpenWorkspaceError_NotMerged:
		return fmt.Sprintf("%s's merge has not landed, or it was reopened since, so there is no merge to revert; nothing was opened", ws), nil
	case *agentreplv1.OpenWorkspaceError_BriefMissing:
		return fmt.Sprintf("the merge-revert brief could not be composed (%s); nothing was opened", r.BriefMissing.GetDetail()), nil
	case *agentreplv1.OpenWorkspaceError_ParentNotOpen:
		return sayParentNotOpen(r.ParentNotOpen, "open "+ws+" again")
	}
	return "", undescribed("opening "+ws, e)
}

// describeCloseRefusal says in one plain sentence why WS was not closed.
// Every CloseWorkspaceError arm has its own sentence.
func describeCloseRefusal(ws string, e *agentreplv1.CloseWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.CloseWorkspaceError_Blocked:
		return fmt.Sprintf("%s has work in flight (%s); the footer shows what blocks the close", ws, r.Blocked.GetSummary()), nil
	case *agentreplv1.CloseWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.CloseWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.CloseWorkspaceError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "close it again"), nil
	case *agentreplv1.CloseWorkspaceError_NotYetAdopted:
		return sayNotYetAdopted(ws, "close it again"), nil
	case *agentreplv1.CloseWorkspaceError_HasOpenChildren:
		return sayOpenChildren(ws, r.HasOpenChildren, "close it again"), nil
	}
	return "", undescribed("closing "+ws, e)
}

// describeKillRefusal says in one plain sentence why WS was not killed. Every
// KillWorkspaceError arm has its own sentence.
func describeKillRefusal(ws string, e *agentreplv1.KillWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.KillWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.KillWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.KillWorkspaceError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "kill it again"), nil
	case *agentreplv1.KillWorkspaceError_NotYetAdopted:
		return sayNotYetAdopted(ws, "kill it again"), nil
	case *agentreplv1.KillWorkspaceError_HasOpenChildren:
		return sayOpenChildren(ws, r.HasOpenChildren, "kill it again"), nil
	}
	return "", undescribed("killing "+ws, e)
}

// describeSelectRefusal says in one plain sentence why WS was not selected.
// Every SelectWorkspaceError arm has its own sentence.
func describeSelectRefusal(ws string, e *agentreplv1.SelectWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SelectWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SelectWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SelectWorkspaceError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "select it again"), nil
	case *agentreplv1.SelectWorkspaceError_NotYetAdopted:
		return sayNotYetAdopted(ws, "select it again"), nil
	case *agentreplv1.SelectWorkspaceError_StandingDown:
		return fmt.Sprintf("the daemon is standing down, so it recorded %s as selected but starts no session; the daemon that serves next revives it", ws), nil
	}
	return "", undescribed("selecting "+ws, e)
}

// describeRestartRefusal says in one plain sentence why WS was not restarted.
// Every RestartWorkspaceError arm has its own sentence.
func describeRestartRefusal(ws string, e *agentreplv1.RestartWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.RestartWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.RestartWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.RestartWorkspaceError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "restart it again"), nil
	case *agentreplv1.RestartWorkspaceError_NotYetAdopted:
		return sayNotYetAdopted(ws, "restart it again"), nil
	case *agentreplv1.RestartWorkspaceError_RelaunchFailed:
		return fmt.Sprintf("relaunching %s failed, so it was not restarted (%s)", ws, r.RelaunchFailed.GetDetail()), nil
	}
	return "", undescribed("restarting "+ws, e)
}

// describeParkRefusal says in one plain sentence why WS was not parked. Every
// ParkWorkspaceError arm has its own sentence.
func describeParkRefusal(ws string, e *agentreplv1.ParkWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.ParkWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.ParkWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.ParkWorkspaceError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "park it again"), nil
	case *agentreplv1.ParkWorkspaceError_NotYetAdopted:
		return sayNotYetAdopted(ws, "park it again"), nil
	}
	return "", undescribed("parking "+ws, e)
}

// describeInterruptRefusal says in one plain sentence why WS was not
// interrupted. Every InterruptError arm has its own sentence.
func describeInterruptRefusal(ws string, e *agentreplv1.InterruptError) (string, error) {
	switch r := e.GetKind().(type) {
	case *agentreplv1.InterruptError_ConfirmRequired:
		return fmt.Sprintf("%s has %d live detached agents; interrupt it again with -confirm-agents to stop them with the turn, or stop them first with -all-agents", ws, r.ConfirmRequired.GetLiveAgentCount()), nil
	case *agentreplv1.InterruptError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.InterruptError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.InterruptError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "interrupt it again"), nil
	case *agentreplv1.InterruptError_NotYetAdopted:
		return sayNotYetAdopted(ws, "interrupt it again"), nil
	case *agentreplv1.InterruptError_NotDetachedWork:
		return fmt.Sprintf("the interrupt named no detached work of %s", ws), nil
	case *agentreplv1.InterruptError_NoSession:
		return fmt.Sprintf("%s has no session to interrupt", ws), nil
	case *agentreplv1.InterruptError_ShimRefused:
		return fmt.Sprintf("%s's session refused the interrupt (%s)", ws, r.ShimRefused.GetDetail()), nil
	case *agentreplv1.InterruptError_NotMergeTab:
		return fmt.Sprintf("the interrupt named no agentic tab of a merge %s's feed draws", ws), nil
	}
	return "", undescribed("interrupting "+ws, e)
}

// describeCompactRefusal says in one plain sentence why WS was not compacted.
// Every CompactWorkspaceError arm has its own sentence.
func describeCompactRefusal(ws string, e *agentreplv1.CompactWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.CompactWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.CompactWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.CompactWorkspaceError_Closed:
		return fmt.Sprintf("%s is closed, so no session runs to compact", ws), nil
	case *agentreplv1.CompactWorkspaceError_NoSession:
		return fmt.Sprintf("%s has no session to compact", ws), nil
	case *agentreplv1.CompactWorkspaceError_ColdGate:
		return fmt.Sprintf("%s is parked at its cold gate, where compacting is the gate's compact or compact-and-resume answer, which the user must ask for or approve; nothing was submitted", ws), nil
	}
	return "", undescribed("compacting "+ws, e)
}

// describeStatusRefusal says in one plain sentence why WS's status was not
// read. Every GetWorkspaceStatusError arm has its own sentence.
func describeStatusRefusal(ws string, e *agentreplv1.GetWorkspaceStatusError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.GetWorkspaceStatusError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.GetWorkspaceStatusError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	}
	return "", undescribed("the status of "+ws, e)
}

// describePassColdGateRefusal says in one plain sentence why WS's cold gate
// was not passed. Every PassColdGateError arm has its own sentence.
func describePassColdGateRefusal(ws string, e *agentreplv1.PassColdGateError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.PassColdGateError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.PassColdGateError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.PassColdGateError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "pass the gate again"), nil
	case *agentreplv1.PassColdGateError_NotYetAdopted:
		return sayNotYetAdopted(ws, "pass the gate again"), nil
	case *agentreplv1.PassColdGateError_NoColdGate:
		return fmt.Sprintf("no cold gate stands on %s", ws), nil
	case *agentreplv1.PassColdGateError_UnservedRemediation:
		return fmt.Sprintf("the cold gate on %s offered no Haiku compaction of all its context, the one way an agent passes a gate", ws), nil
	}
	return "", undescribed("the cold gate of "+ws, e)
}

// describeNotifyRefusal says in one plain sentence why no banner was posted
// for WS. Every NotifyWorkspaceError arm has its own sentence.
func describeNotifyRefusal(ws string, e *agentreplv1.NotifyWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.NotifyWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.NotifyWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.NotifyWorkspaceError_WorkspaceClosed:
		return fmt.Sprintf("%s is closed, so a click on the banner would have no tab to select", ws), nil
	case *agentreplv1.NotifyWorkspaceError_NoBannerProgram:
		return fmt.Sprintf("no desktop banner program could be found on this host (%s)", r.NoBannerProgram.GetCause()), nil
	case *agentreplv1.NotifyWorkspaceError_StandingDown:
		return "the daemon is standing down and starts no banner", nil
	}
	return "", undescribed("notifying for "+ws, e)
}
