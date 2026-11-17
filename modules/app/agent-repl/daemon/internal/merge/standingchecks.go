package merge

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// THE STANDING DEPLOY MEDIATOR (owner rulings, 2026-11-17). A repository
// whose queue checks its deploys keeps ONE workspace across every deploy,
// never one per check: it checks each deploy, hidden; fixes what it
// dispatches itself, in sight in its repository's section; and settles an
// escalation with the user. Its conversation carries what the earlier checks
// found, fixed and settled, and is never cleared, only compacted
// (compactStanding). It stays open and warm
// (wsm.Workspace.StandingDeployCheck). The daemon makes it only for a queue
// that has none, and reopens it when the user closed it.

// mediatorName is the name a made mediator takes. A repository holds it
// once; a closed mediator keeps it, and is reopened. A queue whose fixer
// became its mediator keeps that workspace's name ("deploy-fixer").
const mediatorName = "deploy-mediator"

// standing is a queue's mediator, as a check finds it.
type standing struct {
	ws ids.WorkspaceID
	// briefed is whether it was prompted as the mediator before.
	briefed bool
}

// mediatorOf answers queue Q's standing mediator: the one recorded,
// reopened when it was closed, or one made and recorded when the queue has
// none (or the one recorded is gone from the registry).
func (o *orchestrator) mediatorOf(ctx context.Context, q checkedQueue, fields dlog.Context) (standing, error) {
	recorded, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q.key, wsm.StandingMediator)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the queue's deploy mediator", withField(fields, "error", err.Error()))
		return standing{}, err
	}
	if found {
		record, err := o.deps.DB.Workspace(ctx, recorded.Workspace)
		switch {
		case errors.Is(err, wsm.ErrNotFound):
			o.deps.Log.Global().Info(opMediation, "the deploy mediator is gone from the registry; a new one is made", withField(fields, "gone", string(recorded.Workspace)))
		case err != nil:
			o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator", withField(withField(fields, "workspace", string(recorded.Workspace)), "error", err.Error()))
			return standing{}, err
		case record.Closed:
			if err := o.deps.DeployChecks.Reopen(ctx, recorded.Workspace); err != nil {
				o.deps.Log.Global().Error(opMediation, "the deploy mediator could not be reopened", withField(withField(fields, "workspace", string(recorded.Workspace)), "error", err.Error()))
				return standing{}, fmt.Errorf("merge: reopen the deploy mediator %s: %w", recorded.Workspace, err)
			}
			o.deps.Log.Global().Info(opMediation, "reopened the deploy mediator", withField(fields, "workspace", string(recorded.Workspace)))
			return standing{ws: recorded.Workspace, briefed: recorded.Briefed}, nil
		default:
			return standing{ws: recorded.Workspace, briefed: recorded.Briefed}, nil
		}
	}
	made, err := o.makeMediator(ctx, q, fields)
	if err != nil {
		return standing{}, err
	}
	return standing{ws: made}, nil
}

// makeMediator makes queue Q's mediator and records it: as the queue's
// standing mediator, which its session's landing reads, and as landing
// directly (RecordDeployEscalation), so its finish is the daemon's to land
// whichever brief it works.
func (o *orchestrator) makeMediator(ctx context.Context, q checkedQueue, fields dlog.Context) (ids.WorkspaceID, error) {
	fields = withField(fields, "name", mediatorName)
	made, err := o.deps.DeployChecks.CreateDeployMediator(ctx, q.repository.Dir, mediatorName)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator could not be made", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: make the deploy mediator %s: %w", mediatorName, err)
	}
	fields = withField(fields, "workspace", string(made.ID))
	if err := o.deps.DB.RecordStandingDeployCheck(ctx, q.key, wsm.StandingMediator, made.ID); err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator was made but could not be recorded", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: record the deploy mediator %s: %w", made.ID, err)
	}
	if err := o.deps.DB.RecordDeployEscalation(ctx, made.ID, q.key, o.deps.Now()); err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator could not be recorded as landing directly", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: record the deploy mediator %s as landing directly: %w", made.ID, err)
	}
	o.deps.Log.Global().Info(opMediation, "made the queue's deploy mediator", fields)
	return made.ID, nil
}

// readyMediator answers queue Q's mediator ready to be briefed: its worktree
// at the main branch's tip (Refresh), at its effort, and shown when SHOW
// (an escalation's brief), since it is in sight only while it holds its queue
// at work (owner ruling, 2026-11-17). A check's brief leaves it hidden: a
// dispatch shows it (dispatch).
func (o *orchestrator) readyMediator(ctx context.Context, q checkedQueue, show bool, fields dlog.Context) (standing, error) {
	mediator, err := o.mediatorOf(ctx, q, fields)
	if err != nil {
		return standing{}, err
	}
	fields = withField(fields, "mediator", string(mediator.ws))
	if err := o.deps.DeployChecks.Refresh(ctx, mediator.ws); err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator's worktree could not be brought to the main branch's tip; it is not briefed", withField(fields, "error", err.Error()))
		return standing{}, fmt.Errorf("merge: refresh the deploy mediator %s: %w", mediator.ws, err)
	}
	// THE STANDING IS RECORDED BEFORE THE EFFORT IS SET: setting it starts a
	// made mediator's shim, whose landing reads the standing.
	if err := o.deps.DeployChecks.SetDeployMediatorEffort(ctx, mediator.ws); err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator's effort could not be set; it is not briefed", withField(fields, "error", err.Error()))
		return standing{}, fmt.Errorf("merge: set the deploy mediator %s's effort: %w", mediator.ws, err)
	}
	if show {
		if err := o.deps.DeployChecks.Resume(ctx, mediator.ws); err != nil {
			o.deps.Log.Global().Error(opMediation, "the deploy mediator could not be shown for its brief; it is not briefed", withField(fields, "error", err.Error()))
			return standing{}, fmt.Errorf("merge: show the deploy mediator %s: %w", mediator.ws, err)
		}
	}
	return mediator, nil
}

// settleFixerSight shows queue Q's standing fixer while check M has it at
// work holding the queue (a dispatch whose finish has not been accepted, or
// an escalation), and hides it otherwise (owner ruling, 2026-11-17). Every
// pass settles it, so a fixer a transition left in the wrong sight, or an
// earlier build left in sight, is put right.
func (o *orchestrator) settleFixerSight(ctx context.Context, q checkedQueue, m *wsm.Mediation) {
	fields := dlog.Context{"repo": string(q.key)}
	recorded, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q.key, wsm.StandingMediator)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the standing fixer to settle whether it is in sight", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	fixer := recorded.Workspace
	fields = withField(fields, "fixer", string(fixer))
	record, err := o.deps.DB.Workspace(ctx, fixer)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the standing fixer to settle whether it is in sight", withField(fields, "error", err.Error()))
		return
	}
	atWork := m != nil && ((m.Dispatched && m.Remediation == fixer) || (m.Escalated() && m.Escalation == fixer))
	if atWork && m.Dispatched {
		finish, err := o.finishUnderWay(ctx, q.key)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read whether the fixer's finish is under way to settle whether it is in sight", withField(fields, "error", err.Error()))
			return
		}
		atWork = finish == nil || finish.Workspace != fixer
	}
	switch {
	case atWork && record.Hidden:
		if err := o.deps.DeployChecks.Resume(ctx, fixer); err != nil {
			o.deps.Log.Global().Error(opMediation, "the standing fixer at work could not be shown", withField(fields, "error", err.Error()))
		}
	case !atWork && !record.Hidden:
		if err := o.deps.DeployChecks.Hide(ctx, fixer); err != nil {
			o.deps.Log.Global().Error(opMediation, "the standing fixer no check holds its queue for could not be hidden", withField(fields, "error", err.Error()))
		}
	}
}

// markBriefed records queue Q's mediator briefed, once its first brief is
// submitted. A failure is recorded and left: the next brief states its work
// in full again, which is no harm.
func (o *orchestrator) markBriefed(ctx context.Context, q checkedQueue, fields dlog.Context) {
	if err := o.deps.DB.MarkStandingBriefed(ctx, q.key, wsm.StandingMediator); err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator was briefed but could not be recorded so; its next brief states its work in full again", withField(fields, "error", err.Error()))
	}
}

// noteCheckWork records that check work came for queue Q (a dispatch or an
// escalation): its quiet run starts over, and its standing workspaces owe a
// compaction once the next quiet run completes.
func (o *orchestrator) noteCheckWork(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	if err := o.deps.DB.NoteDeployCheckWork(ctx, q); err != nil {
		o.deps.Log.Global().Error(opMediation, "could not record the check's work; the standing workspaces' compaction is not armed by it", withField(fields, "error", err.Error()))
	}
}

// noteQuietCheck records that a check of queue Q ended with no dispatch and
// no escalation, and compacts its standing workspaces when that completes
// an armed quiet run (owner ruling, 2026-11-17).
func (o *orchestrator) noteQuietCheck(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	compact, err := o.deps.DB.NoteQuietDeployCheck(ctx, q)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not count the quiet check; the standing workspaces are not compacted by it", withField(fields, "error", err.Error()))
		return
	}
	if compact {
		o.compactStanding(ctx, q, fields)
	}
}

// compactStanding compacts queue Q's mediator by Haiku NOW (owner ruling,
// 2026-11-17): a conversation is never cleared, only compacted, once a quiet
// run follows work, and never left for a later check to wait on. A quiet
// check's approving turn may still be ending: a turn under way is never cut,
// and its next start compacts it (workspace.CompactDeployCheck). The
// compaction runs off the decision's path, under the mediation's lifetime,
// since it takes a summarizer's run.
func (o *orchestrator) compactStanding(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	var targets []ids.WorkspaceID
	recorded, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q, wsm.StandingMediator)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator to compact; it is not compacted", withField(fields, "error", err.Error()))
		return
	}
	if found {
		targets = append(targets, recorded.Workspace)
	}
	compact := func(ctx context.Context) {
		for _, ws := range targets {
			wsFields := withField(fields, "workspace", string(ws))
			compacted, err := o.deps.DeployChecks.Compact(ctx, ws)
			if err != nil {
				o.deps.Log.Global().Error(opMediation, "the quiet run after the checks' work could not compact a standing workspace", withField(wsFields, "error", err.Error()))
				continue
			}
			o.deps.Log.Global().Info(opMediation, "the quiet run after the checks' work compacted the standing workspace", withField(wsFields, "compacted", compacted))
		}
	}
	life, leave, entered := o.enterMediation()
	if !entered {
		o.deps.Log.Global().Info(opMediation, mediationNotRun, fields)
		return
	}
	if !o.async {
		defer leave()
		compact(life)
		return
	}
	inflight.Go(func() {
		defer leave()
		compact(life)
	})
}

// adoptStanding makes the workspace check M has at work in each role queue Q
// has no standing workspace in yet the standing one, briefed (owner ruling,
// 2026-11-17): a queue whose checks made a workspace per check before the
// standing ones keeps the workspaces at work, with their conversations, and
// a held escalation goes on holding the queue in the workspace the user
// already works in.
func (o *orchestrator) adoptStanding(ctx context.Context, q checkedQueue, m *wsm.Mediation) {
	if m == nil {
		return
	}
	for _, at := range []struct {
		role wsm.DeployCheckRoleName
		ws   ids.WorkspaceID
	}{{wsm.StandingMediator, m.Escalation}, {wsm.StandingMediator, m.Remediation}} {
		if at.ws == "" {
			continue
		}
		fields := dlog.Context{"repo": string(q.key), "role": string(at.role), "workspace": string(at.ws)}
		_, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q.key, at.role)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read whether the queue has a standing workspace to adopt the check's into", withField(fields, "error", err.Error()))
			continue
		}
		if found {
			continue
		}
		if err := o.deps.DB.RecordStandingDeployCheck(ctx, q.key, at.role, at.ws); err != nil {
			o.deps.Log.Global().Error(opMediation, "could not adopt the check's workspace as the queue's standing one", withField(fields, "error", err.Error()))
			continue
		}
		if err := o.deps.DB.MarkStandingBriefed(ctx, q.key, at.role); err != nil {
			o.deps.Log.Global().Error(opMediation, "adopted the check's workspace as the standing one but could not record it briefed", withField(fields, "error", err.Error()))
			continue
		}
		// AN ADOPTED FIXER LANDS DIRECTLY WHICHEVER BRIEF IT WORKS, as a made
		// one does (makeStanding): a remediation adopted is recorded so.
		if at.role == wsm.StandingMediator {
			if _, escalation, err := o.deps.DB.DeployEscalationOf(ctx, at.ws); err != nil || !escalation {
				if err == nil {
					err = o.deps.DB.RecordDeployEscalation(ctx, at.ws, q.key, o.deps.Now())
				}
				if err != nil {
					o.deps.Log.Global().Error(opMediation, "adopted the standing fixer but could not record it as landing directly", withField(fields, "error", err.Error()))
					continue
				}
			}
		}
		o.deps.Log.Global().Info(opMediation, "adopted the check's workspace as the queue's standing one", fields)
	}
}

// retireStandingIntake closes queue Q's standing intake, retired with the
// role (owner ruling, 2026-11-17: the mediator checks every deploy), and
// forgets it, unless check M is still told to it: the check its brief
// started is decided first, and the next pass retires it. Its session ends
// with the close, at its running turn's end. A close that fails keeps the
// record, and the next pass tries again.
func (o *orchestrator) retireStandingIntake(ctx context.Context, q checkedQueue, m *wsm.Mediation) {
	fields := dlog.Context{"repo": string(q.key)}
	intake, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q.key, wsm.StandingIntake)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the queue's retired intake; it is not retired", withField(fields, "error", err.Error()))
		return
	}
	if !found || (m != nil && m.Intake == intake.Workspace) {
		return
	}
	fields = withField(fields, "intake", string(intake.Workspace))
	record, err := o.deps.DB.Workspace(ctx, intake.Workspace)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
	case err != nil:
		o.deps.Log.Global().Error(opMediation, "could not read the retired intake; it is not retired", withField(fields, "error", err.Error()))
		return
	case !record.Closed:
		if _, err := o.deps.DeployChecks.CloseSelf(ctx, intake.Workspace); err != nil {
			o.deps.Log.Global().Error(opMediation, "the retired intake could not be closed; it is retired at the next pass", withField(fields, "error", err.Error()))
			return
		}
	}
	if _, err := o.deps.DB.ForgetStandingDeployCheck(ctx, q.key, wsm.StandingIntake); err != nil {
		o.deps.Log.Global().Error(opMediation, "the retired intake was closed but its standing could not be forgotten; it is forgotten at the next pass", withField(fields, "error", err.Error()))
		return
	}
	o.deps.Log.Global().Info(opMediation, "retired the queue's intake: its deploy mediator checks every deploy", fields)
}

// retireOneOffEscalations closes every escalation workspace made for queue Q
// before its standing one, once the standing one exists (owner ruling,
// 2026-11-17), except the one check M is held by: the standing escalation
// owns what they owned from then on, and an escalation still holding the
// queue is the user's to release. Each is closed at its running turn's end.
func (o *orchestrator) retireOneOffEscalations(ctx context.Context, q checkedQueue, m *wsm.Mediation) {
	fields := dlog.Context{"repo": string(q.key)}
	standing, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q.key, wsm.StandingMediator)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the standing escalation; no earlier escalation is retired", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	escalations, err := o.deps.DB.DeployEscalationsOf(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not list the queue's escalations; no earlier escalation is retired", withField(fields, "error", err.Error()))
		return
	}
	for _, ws := range escalations {
		if ws == standing.Workspace || (m != nil && ws == m.Escalation) {
			continue
		}
		wsFields := withField(fields, "escalation", string(ws))
		record, err := o.deps.DB.Workspace(ctx, ws)
		if errors.Is(err, wsm.ErrNotFound) {
			continue
		}
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read an earlier escalation; it is not retired", withField(wsFields, "error", err.Error()))
			continue
		}
		if record.Closed {
			continue
		}
		if _, err := o.deps.DeployChecks.CloseSelf(ctx, ws); err != nil {
			o.deps.Log.Global().Error(opMediation, "an earlier escalation could not be retired", withField(wsFields, "error", err.Error()))
			continue
		}
		o.deps.Log.Global().Info(opMediation, "retired an escalation made before the queue's standing one", wsFields)
	}
}
