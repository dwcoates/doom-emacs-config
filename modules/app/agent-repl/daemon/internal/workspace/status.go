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
	links, err := v.deps.DB.WorkspaceLinks(ctx, ws)
	if err != nil {
		v.deps.Log.Global().Error(opStatus, "could not read the links of the workspace whose status was asked", dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
		return nil, fmt.Errorf("status: read the links of %q: %w", ws, err)
	}
	out := &agentreplv1.WorkspaceStatus{
		Workspace:   record.Ref(),
		Name:        record.Name,
		Branch:      record.Branch,
		MergeTarget: target,
		Hidden:      record.Hidden || record.MergeHidden(),
		OneShot:     record.OneShot,
		Repository:  &workspacev1.RepositoryRef{Id: string(record.Repo)},
		Links:       LinksView(links),
	}
	if record.Awaiting != nil {
		awaited, err := v.refOf(ctx, *record.Awaiting, "the workspace it awaits")
		if err != nil {
			return nil, err
		}
		out.Awaiting = awaited
	}
	if err := v.relationshipOf(ctx, record, out); err != nil {
		return nil, err
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

// mergeTargetOf answers where RECORD's branch lands: the workspace whose
// worktree its creation job targets (wsm.Workspace.LandsInto), the primary
// branch otherwise. A merge workspace reads the same way: its target is the
// one the queue decided at hand-off.
func (v *verbs) mergeTargetOf(ctx context.Context, record wsm.Workspace) (*agentreplv1.WorkspaceStatusMergeTarget, error) {
	// A MERGE WORKSPACE ALWAYS HAS A CREATION JOB: the queue's hand-off
	// recorded its target there. One without it cannot say where it lands, and
	// reading it as landing on the primary branch would hide that.
	if record.MergeOf != nil {
		fields := dlog.Context{"workspace": string(record.ID)}
		_, found, err := v.deps.DB.CreationJob(ctx, record.ID)
		if err != nil {
			v.deps.Log.Global().Error(opStatus, "could not read a merge workspace's creation job to say where it lands", dlog.WithCause(fields, err))
			return nil, fmt.Errorf("status: creation job of %q: %w", record.ID, err)
		}
		if !found {
			err := fmt.Errorf("status: merge workspace %q has no creation job", record.ID)
			v.deps.Log.Global().Error(opStatus, "a merge workspace has no creation job to say where it lands", dlog.WithCause(fields, err))
			return nil, err
		}
	}
	if record.LandsInto == nil {
		return &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Primary{Primary: &agentreplv1.WorkspaceStatusMergeTargetPrimary{}}}, nil
	}
	into, err := v.refOf(ctx, *record.LandsInto, "the workspace it lands into")
	if err != nil {
		return nil, err
	}
	return &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Workspace{Workspace: &agentreplv1.WorkspaceStatusMergeTargetWorkspace{
		Workspace: into}}}, nil
}

// relationshipOf fills OUT's relationship facts from RECORD: child or peer,
// its owner, who asked for it, and its open children.
func (v *verbs) relationshipOf(ctx context.Context, record wsm.Workspace, out *agentreplv1.WorkspaceStatus) error {
	out.Relationship = &agentreplv1.WorkspaceStatusRelationship{Kind: &agentreplv1.WorkspaceStatusRelationship_Peer{Peer: &agentreplv1.WorkspaceStatusRelationshipPeer{}}}
	if record.Parent != nil {
		parent, err := v.refOf(ctx, *record.Parent, "its owner")
		if err != nil {
			return err
		}
		out.Relationship = &agentreplv1.WorkspaceStatusRelationship{Kind: &agentreplv1.WorkspaceStatusRelationship_Child{Child: &agentreplv1.WorkspaceStatusRelationshipChild{Parent: parent}}}
		out.Owner = &agentreplv1.WorkspaceStatusOwner{Workspace: parent, OwnerLands: record.OwnerLands}
	}
	if record.RequestedBy != nil {
		creator, err := v.refOf(ctx, *record.RequestedBy, "the workspace that asked for it")
		if err != nil {
			return err
		}
		out.CreatedBy = creator
	}
	children, err := v.deps.DB.OpenChildren(ctx, record.ID)
	if err != nil {
		v.deps.Log.Global().Error(opStatus, "could not read the open children of the workspace whose status was asked", dlog.WithCause(dlog.Context{"workspace": string(record.ID)}, err))
		return fmt.Errorf("status: open children of %q: %w", record.ID, err)
	}
	for _, child := range children {
		out.Children = append(out.Children, child.Ref())
	}
	return nil
}

// refOf answers workspace ID as every wire names it, its directory and all
// (workspace.v1.WorkspaceRef.dir is for display): a ref of the id alone drew
// a subscriber's notice as landing "into the workspace at “" (2026-10-22).
// WHAT names the workspace in the record of a failed read.
func (v *verbs) refOf(ctx context.Context, id ids.WorkspaceID, what string) (*workspacev1.WorkspaceRef, error) {
	record, err := v.deps.DB.Workspace(ctx, id)
	if err != nil {
		v.deps.Log.Global().Error(opStatus, "could not read "+what+" to name it in a workspace's status", dlog.WithCause(dlog.Context{"workspace": string(id)}, err))
		return nil, fmt.Errorf("status: read %s %q: %w", what, id, err)
	}
	return record.Ref(), nil
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
	case *frontendv1.FooterStatus_Working, *frontendv1.FooterStatus_Updating:
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
