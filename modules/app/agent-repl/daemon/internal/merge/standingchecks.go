package merge

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"
)

// THE STANDING MEDIATORS. The deploy mediator (owner rulings, 2026-11-17):
// a repository
// whose queue checks its deploys keeps ONE workspace across every deploy,
// never one per check: it checks each deploy, hidden; fixes what it
// dispatches itself, in sight in its repository's section; and settles an
// escalation with the user. Its conversation carries what the earlier checks
// found, fixed and settled, and is never cleared, only compacted: when a
// remediation's deploy starts, the compaction is owed to its session's next
// start and the session ended (compactMediatorAfterRemediation); after a
// quiet check, only past QuietCheckCompactionTokens, it is compacted at once
// (compactMediator). It stays open and warm
// (wsm.Workspace.Mediator). The daemon makes it only for a queue
// that has none, and reopens it when the user closed it.

// mediatorRole is what tells one standing mediator from the other: the one
// engine checks and briefs both (docs/investigations/
// 2026-11-23-production-mediator-plan.md), and every way they differ is a
// field here, never a branch on the role's name.
type mediatorRole struct {
	// role is the workspace's standing (wsm.Workspace.Mediator), and the
	// table its record lives in.
	role wsm.MediatorRole
	// name is the slug a made mediator takes; the workspace verbs make it
	// under the daemon's own branch namespace (workspace.StandingBranch), so
	// no branch a person or an agent cut can hold it. A repository holds it
	// once; a closed mediator keeps it, and is reopened. A mediator recorded
	// before the namespace keeps the name it was made under.
	name string
	// agent is the section of a repository's lifecycle config that says how
	// the role's mediator runs, nil when the role's checks are off.
	agent func(*repoconfigv1.LifecycleConfig) (*repoconfigv1.MediatorAgent, bool)
}

// deployRole is the deploy mediator's: it checks each landing and holds the
// queue until it decides.
var deployRole = mediatorRole{
	role: wsm.MediatorDeploy,
	name: "deploy-mediator",
	agent: func(cfg *repoconfigv1.LifecycleConfig) (*repoconfigv1.MediatorAgent, bool) {
		return cfg.GetDeployChecks().GetAgent(), cfg.GetDeployChecks() != nil
	},
}

// productionRole is the production mediator's: it reviews the span between
// landings and dispatches children, holding nothing.
var productionRole = mediatorRole{
	role: wsm.MediatorProduction,
	name: "production-mediator",
	agent: func(cfg *repoconfigv1.LifecycleConfig) (*repoconfigv1.MediatorAgent, bool) {
		return cfg.GetProductionChecks().GetAgent(), cfg.GetProductionChecks() != nil
	},
}

// standingRoles is every standing mediator's role.
var standingRoles = []mediatorRole{deployRole, productionRole}

// roleFields is FIELDS naming R's role.
func (r mediatorRole) roleFields(fields dlog.Context) dlog.Context {
	return withField(fields, "role", string(r.role))
}

// standing is a queue's mediator, as a check finds it.
type standing struct {
	ws ids.WorkspaceID
	// briefed is whether it was prompted as the mediator before.
	briefed bool
}

// THE MEDIATOR IS SERVED WHERE ITS ROUND RUNS. A round of a queue's check,
// and the pass's keeping its mediator shown and taking up its remediation
// finish, touch the recorded mediator only on the daemon that serves its
// workspace now: the workspace verbs refuse it anywhere else
// (not_yet_adopted on a successor still adopting it, transferring_away on the
// daemon that handed it on). The edge on which a daemon comes to serve the
// mediator runs the pass again (mediateAdopted, ServingWidened), so no wait,
// retry or timer stands between a handover and the round (2026-11-24 03:34Z:
// a successor serving the queues from its first adoption ran a round on a
// mediator it was still adopting, the refresh was refused not_yet_adopted,
// and the check was escalated to the user as one that could not run).

// mediatorStanding answers queue REPO's recorded mediator in role R and this
// daemon's serving standing of it; a queue with none recorded answers no
// mediator, served, since one made here (makeMediator) is owned from its
// creation. A failed read is answered.
func (o *orchestrator) mediatorStanding(ctx context.Context, r mediatorRole, repo wsm.RepoKey) (ids.WorkspaceID, ServingStanding, error) {
	recorded, found, err := o.deps.DB.StandingMediatorOf(ctx, r.role, repo)
	if err != nil || !found {
		return "", ServedHere, err
	}
	standing, err := o.deps.Standing.ServingStanding(recorded.Workspace)
	if err != nil {
		return recorded.Workspace, ServedHere, fmt.Errorf("merge: the serving standing of the %s mediator %s: %w", r.role, recorded.Workspace, err)
	}
	return recorded.Workspace, standing, nil
}

// mediateAdopted runs the mediation pass when WS, a workspace this daemon has
// just come to serve (AdoptWorkspace, called after its standing turned
// owned), is a queue's recorded deploy mediator or a remediation finish's
// workspace: the pass then runs the rounds and takes up the finish it passed
// over while WS was not served here (Mediate's gate). Any other workspace's
// adoption runs no pass. A failed read is recorded, and the round waits for
// the queue's next pass.
//
// THE PASS OUTLIVES THE ADOPTION'S CALLER: the rpc an adoption answers may
// end before the pass does, and the pass is bounded by the mediation's
// lifetime (enterMediation), which the daemon's exit cuts.
func (o *orchestrator) mediateAdopted(ctx context.Context, ws ids.WorkspaceID) {
	fields := dlog.Context{"workspace": string(ws)}
	var (
		repo       wsm.RepoKey
		isMediator bool
	)
	for _, r := range standingRoles {
		var err error
		repo, isMediator, err = o.deps.DB.StandingMediatorRepoOf(ctx, r.role, ws)
		if err != nil {
			o.mediationFailed(ctx, r.roleFields(fields), logsite.Msg("could not read whether the adopted workspace is a {role} mediator; a round its adoption owes waits for the queue's next pass"), err)
			return
		}
		if isMediator {
			fields = r.roleFields(fields)
			break
		}
	}
	if !isMediator {
		finish, err := o.deps.DB.RemediationFinishOf(ctx, ws)
		if err != nil {
			o.mediationFailed(ctx, fields, logsite.Msg("could not read whether the adopted workspace has a remediation finish; a finish its adoption owes waits for the queue's next pass"), err)
			return
		}
		if finish == nil {
			return
		}
		repo = finish.Repo
		// A remediation finish is the deploy mediator's work.
		fields = deployRole.roleFields(fields)
	}
	o.deps.Log.Global().Info(opMediation, "this daemon adopted a {role} mediator; the mediation pass runs what it passed over while the mediator was not served here", withField(fields, "repo", string(repo)))
	o.mediateAgain(context.WithoutCancel(ctx))
}

// ServingWidened implements Orchestrator: this daemon came to serve
// workspaces no adoption of its own told it of (rollout.MergeMover), so the
// mediation pass runs again for every round it passed over while their
// mediators were not served here (Mediate's gate).
func (o *orchestrator) ServingWidened(ctx context.Context) {
	o.deps.Log.Global().Info(opMediation, "this daemon serves workspaces it did not adopt one by one; the mediation pass runs what it passed over while their mediators were not served here", nil)
	o.mediateAgain(context.WithoutCancel(ctx))
}

// mediatorOf answers queue Q's standing mediator in role R: the one
// recorded, reopened when it was closed, or one made and recorded when the
// queue has none (or the one recorded is gone from the registry).
func (o *orchestrator) mediatorOf(ctx context.Context, r mediatorRole, q checkedQueue, fields dlog.Context) (standing, error) {
	fields = r.roleFields(fields)
	recorded, found, err := o.deps.DB.StandingMediatorOf(ctx, r.role, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the queue's {role} mediator", withField(fields, "error", err.Error()))
		return standing{}, err
	}
	if found {
		record, err := o.deps.DB.Workspace(ctx, recorded.Workspace)
		switch {
		case errors.Is(err, wsm.ErrNotFound):
			o.deps.Log.Global().Info(opMediation, "the {role} mediator is gone from the registry; a new one is made", withField(fields, "gone", string(recorded.Workspace)))
		case err != nil:
			o.deps.Log.Global().Error(opMediation, "could not read the {role} mediator", withField(withField(fields, "workspace", string(recorded.Workspace)), "error", err.Error()))
			return standing{}, err
		case record.Closed:
			if err := o.deps.DeployChecks.Reopen(ctx, recorded.Workspace); err != nil {
				o.deps.Log.Global().Error(opMediation, "the {role} mediator could not be reopened", withField(withField(fields, "workspace", string(recorded.Workspace)), "error", err.Error()))
				return standing{}, fmt.Errorf("merge: reopen the %s mediator %s: %w", r.role, recorded.Workspace, err)
			}
			o.deps.Log.Global().Info(opMediation, "reopened the {role} mediator", withField(fields, "workspace", string(recorded.Workspace)))
			return standing{ws: recorded.Workspace, briefed: recorded.Briefed}, nil
		default:
			return standing{ws: recorded.Workspace, briefed: recorded.Briefed}, nil
		}
	}
	made, err := o.makeMediator(ctx, r, q, fields)
	if err != nil {
		return standing{}, err
	}
	return standing{ws: made}, nil
}

// makeMediator makes queue Q's mediator in role R and records it, which its
// session's landing reads and which makes its finish the daemon's to land,
// whichever brief it works.
func (o *orchestrator) makeMediator(ctx context.Context, r mediatorRole, q checkedQueue, fields dlog.Context) (ids.WorkspaceID, error) {
	fields = withField(r.roleFields(fields), "name", r.name)
	agent, _ := r.agent(q.config)
	made, err := o.deps.DeployChecks.CreateMediator(ctx, q.repository.Dir, r.name, agent)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the {role} mediator could not be made", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: make the %s mediator %s: %w", r.role, r.name, err)
	}
	fields = withField(fields, "workspace", string(made.ID))
	if err := o.deps.DB.RecordStandingMediator(ctx, r.role, q.key, made.ID); err != nil {
		o.deps.Log.Global().Error(opMediation, "the {role} mediator was made but could not be recorded", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: record the %s mediator %s: %w", r.role, made.ID, err)
	}
	o.deps.Log.Global().Info(opMediation, "made the queue's {role} mediator", fields)
	return made.ID, nil
}

// readyMediator answers queue Q's mediator in role R ready to be briefed: its
// worktree at the main branch's tip (Refresh), on the model and effort its
// repository configures, and shown, since it stands in sight for good (owner
// ruling, 2026-11-19).
func (o *orchestrator) readyMediator(ctx context.Context, r mediatorRole, q checkedQueue, fields dlog.Context) (standing, error) {
	mediator, err := o.mediatorOf(ctx, r, q, fields)
	if err != nil {
		return standing{}, err
	}
	fields = withField(r.roleFields(fields), "mediator", string(mediator.ws))
	if err := o.deps.DeployChecks.Refresh(ctx, mediator.ws); err != nil {
		o.mediationFailed(ctx, fields, logsite.Msg("the {role} mediator's worktree could not be brought to the main branch's tip; it is not briefed"), err)
		return standing{}, fmt.Errorf("merge: refresh the %s mediator %s: %w", r.role, mediator.ws, err)
	}
	// THE MEDIATOR IS RECORDED BEFORE ITS MODEL AND EFFORT ARE PICKED, and so
	// before the brief's delivery brings its session up at the pick: the
	// session's landing reads the record.
	agent, _ := r.agent(q.config)
	if err := o.deps.DeployChecks.SetMediatorAgent(ctx, mediator.ws, agent); err != nil {
		o.mediationFailed(ctx, fields, logsite.Msg("the {role} mediator's model and effort could not be set; it is not briefed"), err)
		return standing{}, fmt.Errorf("merge: set the %s mediator %s's model and effort: %w", r.role, mediator.ws, err)
	}
	if err := o.show(ctx, r, mediator, fields); err != nil {
		return standing{}, err
	}
	return mediator, nil
}

// showMediator answers queue Q's mediator in role R shown and nothing else
// readied: the mediator that escalated in its own turn is mid-turn, so its
// worktree holds its work and its effort is set (escalate).
func (o *orchestrator) showMediator(ctx context.Context, r mediatorRole, q checkedQueue, fields dlog.Context) (standing, error) {
	mediator, err := o.mediatorOf(ctx, r, q, fields)
	if err != nil {
		return standing{}, err
	}
	if err := o.show(ctx, r, mediator, withField(r.roleFields(fields), "mediator", string(mediator.ws))); err != nil {
		return standing{}, err
	}
	return mediator, nil
}

// show puts MEDIATOR, of role R, in sight for its brief, recording a failure
// at ERROR.
func (o *orchestrator) show(ctx context.Context, r mediatorRole, mediator standing, fields dlog.Context) error {
	if err := o.deps.DeployChecks.Resume(ctx, mediator.ws); err != nil {
		o.mediationFailed(ctx, r.roleFields(fields), logsite.Msg("the {role} mediator could not be shown for its brief; it is not briefed"), err)
		return fmt.Errorf("merge: show the %s mediator %s: %w", r.role, mediator.ws, err)
	}
	return nil
}

// keepMediatorShown shows queue Q's mediator in role R whenever it is
// hidden: it stands in its repository's section for good (owner ruling,
// 2026-11-19, replacing 2026-11-17's sight only while at work), so a mediator
// an earlier build hid, or the user hid, is shown again by the next pass.
func (o *orchestrator) keepMediatorShown(ctx context.Context, r mediatorRole, q checkedQueue) {
	fields := r.roleFields(dlog.Context{"repo": string(q.key)})
	recorded, found, err := o.deps.DB.StandingMediatorOf(ctx, r.role, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the {role} mediator to keep it shown", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	mediator := recorded.Workspace
	fields = withField(fields, "mediator", string(mediator))
	record, err := o.deps.DB.Workspace(ctx, mediator)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the {role} mediator's workspace to keep it shown", withField(fields, "error", err.Error()))
		return
	}
	if !record.Hidden {
		return
	}
	if err := o.deps.DeployChecks.Resume(ctx, mediator); err != nil {
		// THE GATE PASSED (Mediate), BUT THE MEDIATOR MAY BE HANDED AWAY
		// SINCE: a refusal on its standing is the daemon that serves it next
		// to keep it shown.
		if o.refusedOnStanding(fields, "keeping the hidden {role} mediator shown", err) {
			return
		}
		o.deps.Log.Global().Error(opMediation, "the hidden {role} mediator could not be shown", withField(fields, "error", err.Error()))
		return
	}
	o.deps.Log.Global().Info(opMediation, "showed the hidden {role} mediator; it stands in its repository's section for good", fields)
}

// markBriefed records queue Q's mediator in role R briefed, once its first
// brief is submitted. A failure is recorded and left: the next brief states
// its work in full again, which is no harm.
func (o *orchestrator) markBriefed(ctx context.Context, r mediatorRole, q checkedQueue, fields dlog.Context) {
	if err := o.deps.DB.MarkStandingMediatorBriefed(ctx, r.role, q.key); err != nil {
		o.deps.Log.Global().Error(opMediation, "the {role} mediator was briefed but could not be recorded so; its next brief states its work in full again", withField(r.roleFields(fields), "error", err.Error()))
	}
}

// QuietCheckCompactionTokens is the context, in tokens, past which a check
// that approved a deploy with nothing to fix compacts the mediator (owner
// ruling, 2026-11-20). A check that fixed or escalated something compacts it
// whatever its context (compactMediatorAfterRemediation).
const QuietCheckCompactionTokens = 200_000

// noteQuietCheck compacts queue Q's mediator after a check that ended with no
// dispatch and no escalation only when its context passed
// QuietCheckCompactionTokens: an approval that changed nothing leaves the
// conversation warm for the next brief.
func (o *orchestrator) noteQuietCheck(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	recorded, found, err := o.deps.DB.DeployMediatorOf(ctx, q)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator to size its context; it is not compacted", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	tokens, err := o.deps.DeployChecks.ContextTokens(recorded.Workspace)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator's context; it is not compacted", withField(fields, "error", err.Error()))
		return
	}
	fields = withField(withField(fields, "context_tokens", tokens), "bound", QuietCheckCompactionTokens)
	if tokens <= QuietCheckCompactionTokens {
		o.deps.Log.Global().Info(opMediation, "the quiet check left the mediator's context within its bound; it is not compacted", fields)
		return
	}
	o.compactMediator(ctx, q, "the mediator's context passed its bound after a quiet check", fields)
}

// compactMediatorAfterRemediation owes queue Q's mediator's compaction to
// its session's next start as a remediation or an escalation's fix deploys
// (owner ruling, 2026-11-20): its work is concluded, so its conversation is
// carried over as a summary. Its session is ended and none started
// (DeployChecks.OweCompaction): the rollout beside it hands the mediator to
// a successor about 30s in, which cut every compacting start (the summarizer
// takes about 70s) and logged three false errors (2026-11-22 17:30:34). The
// next start compacts it: the successor's bring-up after a handover, or the
// next brief's when the deploy replaces no daemon. It runs inline, since it
// runs no summarizer, and its caller tells the rollout of the landing only
// after it, on CTX, the mediation's lifetime.
func (o *orchestrator) compactMediatorAfterRemediation(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	recorded, found, err := o.deps.DB.DeployMediatorOf(ctx, q)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator to compact; it is not compacted", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	fields = withField(withField(fields, "workspace", string(recorded.Workspace)), "why", "the mediator's remediation concluded and its deploy starts")
	started := o.deps.Now()
	owed, err := o.deps.DeployChecks.OweCompaction(ctx, recorded.Workspace)
	o.deps.Meter.ObserveMediatorCompaction(mediatorOwedCompactionOutcome(owed, err), o.deps.Now().Sub(started))
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator's compaction could not be owed to its next start", withField(fields, "error", err.Error()))
		return
	}
	o.deps.Log.Global().Info(opMediation, "owed the deploy mediator's compaction to its next start", withField(fields, "owed", owed))
}

// compactMediator compacts queue Q's mediator by Haiku NOW (owner ruling,
// 2026-11-17): a conversation is never cleared, only compacted, and never left for
// a later check to wait on. WHY says what called for it. It serves a quiet
// check past its bound, beside which no rollout runs to hand the mediator to
// a successor mid-start (compactMediatorAfterRemediation owes it instead). A quiet
// check's approving turn may still be ending: a turn under way is never cut,
// and its next start compacts it (workspace.CompactDeployCheck). The
// compaction runs off the decision's path, under the mediation's lifetime,
// since it takes a summarizer's run.
func (o *orchestrator) compactMediator(ctx context.Context, q wsm.RepoKey, why string, fields dlog.Context) {
	recorded, found, err := o.deps.DB.DeployMediatorOf(ctx, q)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the deploy mediator to compact; it is not compacted", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	fields = withField(withField(fields, "workspace", string(recorded.Workspace)), "why", why)
	compact := func(ctx context.Context) {
		started := o.deps.Now()
		compacted, err := o.deps.DeployChecks.Compact(ctx, recorded.Workspace)
		o.deps.Meter.ObserveMediatorCompaction(mediatorCompactionOutcome(compacted, err), o.deps.Now().Sub(started))
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "the deploy mediator could not be compacted", withField(fields, "error", err.Error()))
			return
		}
		o.deps.Log.Global().Info(opMediation, "compacted the deploy mediator", withField(fields, "compacted", compacted))
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
