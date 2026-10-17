package workspace

import (
	"context"
	"errors"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

const opStatus = "daemon.workspace.status"

// Status answers the workspace's status as an agent reads it
// (agentrepl.v1.WorkspaceStatus): what it is, where its branch lands, and,
// when it is open, what its session is doing, projected from the footer's
// status, which is THE workspace status every surface projects.
func (v *verbs) Status(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error) {
	record, err := v.deps.DB.Workspace(ctx, ws)
	if errors.Is(err, wsm.ErrNotFound) {
		return nil, refuse(v.deps.Log.Global(), "GetWorkspaceStatus", ArmUnknownWorkspace, fmt.Sprintf("no workspace %q is registered", ws), true)
	}
	if err != nil {
		v.deps.Log.Global().Error(opStatus, "could not read the workspace whose status was asked", dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
		return nil, fmt.Errorf("status: read %q: %w", ws, err)
	}
	target, err := v.mergeTargetOf(ctx, record)
	if err != nil {
		return nil, err
	}
	out := &agentreplv1.WorkspaceStatus{
		Workspace:   &workspacev1.WorkspaceRef{Id: string(record.ID), Dir: record.Dir},
		Name:        record.Name,
		Branch:      record.Branch,
		MergeTarget: target,
		Hidden:      record.Hidden || record.MergeHidden(),
	}
	if record.Closed {
		out.Standing = &agentreplv1.WorkspaceStatusStanding{Standing: &agentreplv1.WorkspaceStatusStanding_Closed{Closed: &agentreplv1.WorkspaceStatusClosed{}}}
		return out, nil
	}
	activity, err := v.activityOf(ws)
	if err != nil {
		return nil, err
	}
	out.Standing = &agentreplv1.WorkspaceStatusStanding{Standing: &agentreplv1.WorkspaceStatusStanding_Open{Open: &agentreplv1.WorkspaceStatusOpen{Activity: activity}}}
	return out, nil
}

// mergeTargetOf answers where RECORD's branch lands: its owner when it has
// one, and for a merge workspace the workspace whose worktree its creation
// recorded as the landing, the primary branch otherwise.
func (v *verbs) mergeTargetOf(ctx context.Context, record wsm.Workspace) (*agentreplv1.WorkspaceStatusMergeTarget, error) {
	owner := func(id ids.WorkspaceID, lands bool) *agentreplv1.WorkspaceStatusMergeTarget {
		return &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Owner{Owner: &agentreplv1.WorkspaceStatusMergeTargetOwner{
			Owner: &workspacev1.WorkspaceRef{Id: string(id)}, OwnerLands: lands}}}
	}
	primary := &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Primary{Primary: &agentreplv1.WorkspaceStatusMergeTargetPrimary{}}}
	fields := dlog.Context{"workspace": string(record.ID)}
	if record.MergeOf == nil {
		if record.Parent != nil {
			return owner(*record.Parent, record.OwnerLands), nil
		}
		return primary, nil
	}
	// A MERGE WORKSPACE'S PARENT IS ITS REQUESTER, which the roster nests it
	// under, not where it lands: the landing is its creation's layout.
	job, found, err := v.deps.DB.CreationJob(ctx, record.ID)
	if err != nil {
		v.deps.Log.Global().Error(opStatus, "could not read a merge workspace's creation job to say where it lands", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("status: creation job of %q: %w", record.ID, err)
	}
	if !found {
		err := fmt.Errorf("status: merge workspace %q has no creation job", record.ID)
		v.deps.Log.Global().Error(opStatus, "a merge workspace has no creation job to say where it lands", dlog.WithCause(fields, err))
		return nil, err
	}
	into, err := v.deps.DB.WorkspaceByDir(ctx, job.Layout.TargetDir)
	if errors.Is(err, wsm.ErrNotFound) {
		return primary, nil
	}
	if err != nil {
		v.deps.Log.Global().Error(opStatus, "could not read the workspace a merge workspace lands in", dlog.WithCause(dlog.Merge(fields, dlog.Context{"target_dir": job.Layout.TargetDir}), err))
		return nil, fmt.Errorf("status: workspace at %q: %w", job.Layout.TargetDir, err)
	}
	return owner(into.ID, false), nil
}

// activityOf projects an open workspace's footer status onto what an agent
// decides on. The standing cold gate card is the authority on the gate, since
// it carries the gate's address.
func (v *verbs) activityOf(ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatusActivity, error) {
	if gate, standing := v.deps.Cards.ColdGate(ws); standing {
		return &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_ColdGate{ColdGate: &agentreplv1.WorkspaceStatusActivityColdGate{
			Gate: coldGateRowID(ws, gate.VendorSessionID)}}}, nil
	}
	status := v.deps.Footer.Status(ws)
	activity, ok := ActivityOf(status)
	if !ok {
		err := fmt.Errorf("status: the footer of %q states no status arm this projection knows (%T)", ws, status.GetStatus())
		v.deps.Log.Global().Error(opStatus, "a workspace's footer status has no activity", dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
		return nil, err
	}
	return activity, nil
}

// ActivityOf projects a footer status onto a workspace's activity, false for
// a status with no arm (or one the projection does not declare). A cold gate
// whose card is already spent is its answer being carried out, which brings
// the session back up.
func ActivityOf(status *frontendv1.FooterStatus) (*agentreplv1.WorkspaceStatusActivity, bool) {
	idle := &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_Idle{Idle: &agentreplv1.WorkspaceStatusActivityIdle{}}}
	running := &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_TurnRunning{TurnRunning: &agentreplv1.WorkspaceStatusActivityTurnRunning{}}}
	starting := &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_Starting{Starting: &agentreplv1.WorkspaceStatusActivityStarting{}}}
	needs := func(ask *agentreplv1.WorkspaceStatusActivityNeedsInput) *agentreplv1.WorkspaceStatusActivity {
		return &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_NeedsInput{NeedsInput: ask}}
	}
	fault := &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_Fault{Fault: &agentreplv1.WorkspaceStatusActivityFault{
		Description: footer.StatusWords(status)}}}
	switch arm := status.GetStatus().(type) {
	case *frontendv1.FooterStatus_Idle, *frontendv1.FooterStatus_Interrupted, *frontendv1.FooterStatus_Merged,
		*frontendv1.FooterStatus_TurnFailed, *frontendv1.FooterStatus_Background, *frontendv1.FooterStatus_MergeFailed,
		*frontendv1.FooterStatus_Closing:
		return idle, true
	case *frontendv1.FooterStatus_Working:
		return running, true
	case *frontendv1.FooterStatus_Waiting:
		switch arm.Waiting.GetSubstatus().(type) {
		case *frontendv1.FooterStatusWaiting_Wakeup:
			return idle, true
		case *frontendv1.FooterStatusWaiting_ColdGate:
			return starting, true
		}
		return running, true
	case *frontendv1.FooterStatus_Permission:
		return needs(&agentreplv1.WorkspaceStatusActivityNeedsInput{Ask: &agentreplv1.WorkspaceStatusActivityNeedsInput_Permission{
			Permission: &agentreplv1.WorkspaceStatusActivityNeedsInputPermission{}}}), true
	case *frontendv1.FooterStatus_Question:
		return needs(&agentreplv1.WorkspaceStatusActivityNeedsInput{Ask: &agentreplv1.WorkspaceStatusActivityNeedsInput_Question{
			Question: &agentreplv1.WorkspaceStatusActivityNeedsInputQuestion{}}}), true
	case *frontendv1.FooterStatus_Merging:
		return &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_Merging{Merging: &agentreplv1.WorkspaceStatusActivityMerging{}}}, true
	case *frontendv1.FooterStatus_Starting, *frontendv1.FooterStatus_Loading:
		return starting, true
	case *frontendv1.FooterStatus_VendorFault, *frontendv1.FooterStatus_AgentReplFault,
		*frontendv1.FooterStatus_NetworkFault, *frontendv1.FooterStatus_Degraded:
		return fault, true
	}
	return nil, false
}
