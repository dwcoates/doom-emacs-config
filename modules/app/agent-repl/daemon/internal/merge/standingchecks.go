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
// (compactMediator). It stays open and warm
// (wsm.Workspace.DeployMediator). The daemon makes it only for a queue
// that has none, and reopens it when the user closed it.

// mediatorName is the name a made mediator takes. A repository holds it
// once; a closed mediator keeps it, and is reopened.
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
	recorded, found, err := o.deps.DB.DeployMediatorOf(ctx, q.key)
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

// makeMediator makes queue Q's mediator and records it, which its
// session's landing reads and which makes its finish the daemon's to land
// directly, whichever brief it works.
func (o *orchestrator) makeMediator(ctx context.Context, q checkedQueue, fields dlog.Context) (ids.WorkspaceID, error) {
	fields = withField(fields, "name", mediatorName)
	made, err := o.deps.DeployChecks.CreateDeployMediator(ctx, q.repository.Dir, mediatorName)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator could not be made", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: make the deploy mediator %s: %w", mediatorName, err)
	}
	fields = withField(fields, "workspace", string(made.ID))
	if err := o.deps.DB.RecordDeployMediator(ctx, q.key, made.ID); err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator was made but could not be recorded", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: record the deploy mediator %s: %w", made.ID, err)
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
		o.mediationFailed(ctx, fields, "the deploy mediator's worktree could not be brought to the main branch's tip; it is not briefed", err)
		return standing{}, fmt.Errorf("merge: refresh the deploy mediator %s: %w", mediator.ws, err)
	}
	// THE MEDIATOR IS RECORDED BEFORE THE EFFORT IS SET: setting it starts a
	// made mediator's shim, whose landing reads the record.
	if err := o.deps.DeployChecks.SetDeployMediatorEffort(ctx, mediator.ws); err != nil {
		o.mediationFailed(ctx, fields, "the deploy mediator's effort could not be set; it is not briefed", err)
		return standing{}, fmt.Errorf("merge: set the deploy mediator %s's effort: %w", mediator.ws, err)
	}
	if show {
		if err := o.show(ctx, mediator, fields); err != nil {
			return standing{}, err
		}
	}
	return mediator, nil
}

// showMediator answers queue Q's mediator shown and nothing else readied:
// the mediator that escalated in its own turn is mid-turn, so its worktree
// holds its work and its effort is set (escalate).
func (o *orchestrator) showMediator(ctx context.Context, q checkedQueue, fields dlog.Context) (standing, error) {
	mediator, err := o.mediatorOf(ctx, q, fields)
	if err != nil {
		return standing{}, err
	}
	if err := o.show(ctx, mediator, withField(fields, "mediator", string(mediator.ws))); err != nil {
		return standing{}, err
	}
	return mediator, nil
}

// show puts MEDIATOR in sight for its escalation, recording a failure at
// ERROR.
func (o *orchestrator) show(ctx context.Context, mediator standing, fields dlog.Context) error {
	if err := o.deps.DeployChecks.Resume(ctx, mediator.ws); err != nil {
		o.mediationFailed(ctx, fields, "the deploy mediator could not be shown for its brief; it is not briefed", err)
		return fmt.Errorf("merge: show the deploy mediator %s: %w", mediator.ws, err)
	}
	return nil
}

// settleMediatorSight shows queue Q's mediator while check M has it at
// work holding the queue (a dispatch whose finish is not under way, or an
// escalation), and hides it otherwise (owner ruling, 2026-11-17). Every pass
// settles it, so a mediator a transition left in the wrong sight is put
// right.
func (o *orchestrator) settleMediatorSight(ctx context.Context, q checkedQueue, m *wsm.Mediation) {
	fields := dlog.Context{"repo": string(q.key)}
	recorded, found, err := o.deps.DB.DeployMediatorOf(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator to settle whether it is in sight", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	mediator := recorded.Workspace
	fields = withField(fields, "mediator", string(mediator))
	record, err := o.deps.DB.Workspace(ctx, mediator)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator to settle whether it is in sight", withField(fields, "error", err.Error()))
		return
	}
	atWork := m != nil && (m.Dispatched || m.Escalated())
	if atWork && m.Dispatched {
		finish, err := o.finishUnderWay(ctx, q.key)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read whether the mediator's finish is under way to settle whether it is in sight", withField(fields, "error", err.Error()))
			return
		}
		atWork = finish == nil
	}
	switch {
	case atWork && record.Hidden:
		if err := o.deps.DeployChecks.Resume(ctx, mediator); err != nil {
			o.deps.Log.Global().Error(opMediation, "the deploy mediator at work could not be shown", withField(fields, "error", err.Error()))
		}
	case !atWork && !record.Hidden:
		if err := o.deps.DeployChecks.Hide(ctx, mediator); err != nil {
			o.deps.Log.Global().Error(opMediation, "the deploy mediator no check holds its queue for could not be hidden", withField(fields, "error", err.Error()))
		}
	}
}

// markBriefed records queue Q's mediator briefed, once its first brief is
// submitted. A failure is recorded and left: the next brief states its work
// in full again, which is no harm.
func (o *orchestrator) markBriefed(ctx context.Context, q checkedQueue, fields dlog.Context) {
	if err := o.deps.DB.MarkMediatorBriefed(ctx, q.key); err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator was briefed but could not be recorded so; its next brief states its work in full again", withField(fields, "error", err.Error()))
	}
}

// noteCheckWork records that check work came for queue Q (a dispatch or an
// escalation): its quiet run starts over, and its mediator owes a
// compaction once the next quiet run completes.
func (o *orchestrator) noteCheckWork(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	if err := o.deps.DB.NoteDeployCheckWork(ctx, q); err != nil {
		o.deps.Log.Global().Error(opMediation, "could not record the check's work; the mediator's compaction is not armed by it", withField(fields, "error", err.Error()))
	}
}

// noteQuietCheck records that a check of queue Q ended with no dispatch and
// no escalation, and compacts its mediator when that completes an armed
// quiet run (owner ruling, 2026-11-17).
func (o *orchestrator) noteQuietCheck(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	compact, err := o.deps.DB.NoteQuietDeployCheck(ctx, q)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not count the quiet check; the mediator is not compacted by it", withField(fields, "error", err.Error()))
		return
	}
	if compact {
		o.compactMediator(ctx, q, fields)
	}
}

// compactMediator compacts queue Q's mediator by Haiku NOW (owner ruling,
// 2026-11-17): a conversation is never cleared, only compacted, once a quiet
// run follows work, and never left for a later check to wait on. A quiet
// check's approving turn may still be ending: a turn under way is never cut,
// and its next start compacts it (workspace.CompactDeployCheck). The
// compaction runs off the decision's path, under the mediation's lifetime,
// since it takes a summarizer's run.
func (o *orchestrator) compactMediator(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	recorded, found, err := o.deps.DB.DeployMediatorOf(ctx, q)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator to compact; it is not compacted", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	fields = withField(fields, "workspace", string(recorded.Workspace))
	compact := func(ctx context.Context) {
		compacted, err := o.deps.DeployChecks.Compact(ctx, recorded.Workspace)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "the quiet run after the checks' work could not compact the deploy mediator", withField(fields, "error", err.Error()))
			return
		}
		o.deps.Log.Global().Info(opMediation, "the quiet run after the checks' work compacted the deploy mediator", withField(fields, "compacted", compacted))
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

// hideMediator hides queue REPO's mediator, which no check holds the queue
// for any more. A queue with no mediator recorded has nothing to hide; a
// failure is recorded, and the next pass settles its sight
// (settleMediatorSight).
func (o *orchestrator) hideMediator(ctx context.Context, repo wsm.RepoKey, fields dlog.Context) {
	recorded, found, err := o.deps.DB.DeployMediatorOf(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator to hide it", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	if err := o.deps.DeployChecks.Hide(ctx, recorded.Workspace); err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator no check holds its queue for could not be hidden", withField(withField(fields, "mediator", string(recorded.Workspace)), "error", err.Error()))
	}
}
