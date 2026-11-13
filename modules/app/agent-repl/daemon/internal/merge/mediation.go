package merge

import (
	"context"
	"errors"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/sessioncommand"
	"agent-repld/internal/wsm"
)

// THE DEPLOY MEDIATOR (repoconfig.v1.LifecycleConfig.deploy_mediator; owner
// request, 2026-11-13; docs/investigations/2026-11-13-deploy-mediator-plan.md).
//
// EVERY DEPLOY OWES A MEDIATION, durably, at its start (OweMediations): the
// queue advances nothing while one is owed (queueHeld). Once the deploy has
// finished -- its own end, or its successor's takeover -- the serving daemon
// runs a ROUND (Mediate): the mediator's context is compacted, it is told the
// deploy's window in its brief, and its turn is awaited. Within that turn it
// approves the deploy, escalates it to the user (DecideMediation: either
// releases the queue), or remediates and deploys, which owes the next round.
// A turn that ends with the same mediation still owed decided nothing: the
// queue is let through, loudly, and the user is told.

// BriefDeployMediation is the mediator's brief.
const BriefDeployMediation = "deploy-mediation"

// opMediation is the operation every mediation record is filed under.
const opMediation = "daemon.merge.mediation"

// The refusals DecideMediation answers.
const (
	// ArmNotMediator refuses a decision from a workspace that is no
	// repository's deploy mediator.
	ArmNotMediator = "not_mediator"
	// ArmNoMediationOwed refuses a decision when the mediator's repository
	// owes no mediation.
	ArmNoMediationOwed = "no_mediation_owed"
)

// MediationDecision is what a mediator decided of a deploy.
type MediationDecision int

const (
	// MediationApprove passes the deploy: the queue advances.
	MediationApprove MediationDecision = iota + 1
	// MediationEscalate puts the deploy before the user, and the queue
	// advances.
	MediationEscalate
)

// String names the decision in records.
func (d MediationDecision) String() string {
	switch d {
	case MediationApprove:
		return "approve"
	case MediationEscalate:
		return "escalate"
	}
	return fmt.Sprintf("MediationDecision(%d)", int(d))
}

// mediatedQueue is a registered repository whose deploys a mediator checks.
type mediatedQueue struct {
	registeredQueue
	// mediator is the mediator workspace's name.
	mediator string
}

// mediatedQueues answers every registered repository that names a deploy
// mediator. A repository with no lifecycle config names none; one whose
// config cannot be read is recorded under OP and passed over.
func (o *orchestrator) mediatedQueues(ctx context.Context, op string) ([]mediatedQueue, error) {
	queues, err := o.registeredQueues(ctx, op)
	if err != nil {
		return nil, err
	}
	var out []mediatedQueue
	for _, q := range queues {
		cfg, err := o.deps.DeployConfig.Load(q.repository.Dir)
		var missing *lifecycleconfig.MissingError
		switch {
		case errors.As(err, &missing):
			continue
		case err != nil:
			o.deps.Log.Global().Error(op, "could not read a repository's lifecycle config to find its deploy mediator; it is passed over",
				dlog.Context{"repository": q.repository.Dir, "error": err.Error()})
			continue
		}
		if name := cfg.GetDeployMediator().GetName(); name != "" {
			out = append(out, mediatedQueue{registeredQueue: q, mediator: name})
		}
	}
	return out, nil
}

// OweMediations implements Orchestrator: the deploy that started AT owes a
// mediation in every repository with a mediator. A mediation that cannot be
// recorded is recorded at ERROR and answered: that deploy goes unchecked.
func (o *orchestrator) OweMediations(ctx context.Context, at time.Time) error {
	queues, err := o.mediatedQueues(ctx, opMediation)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not list the repositories a deploy owes mediations in; the deploy goes unchecked", dlog.Context{"error": err.Error()})
		return err
	}
	var errs []error
	for _, q := range queues {
		if err := o.deps.DB.OweMediation(ctx, q.key, at, ""); err != nil {
			o.deps.Log.Global().Error(opMediation, "could not owe a deploy's mediation; the deploy goes unchecked in this repository", dlog.Context{
				"repo": string(q.key), "mediator": q.mediator, "error": err.Error()})
			errs = append(errs, err)
			continue
		}
		o.deps.Log.Global().Info(opMediation, "the deploy owes a mediation; the queue advances nothing until its mediator decides", dlog.Context{
			"repo": string(q.key), "mediator": q.mediator, "deploy_started": at.UTC().Format(time.RFC3339Nano)})
		o.republishMediation(ctx, q.key)
	}
	return errors.Join(errs...)
}

// Mediate implements Orchestrator: runs a round for every mediation owed and
// not yet awaited here. A daemon that may not advance its queues runs none;
// its successor's takeover calls this again.
func (o *orchestrator) Mediate(ctx context.Context) {
	if !o.deps.Standing.ServesQueue() {
		o.deps.Log.Global().Info(opMediation, "this daemon does not serve the queues; their mediations are run by the daemon that does", nil)
		return
	}
	queues, err := o.mediatedQueues(ctx, opMediation)
	if cutUnder(ctx, err) {
		o.deps.Log.Global().Info(opMediation, "the mediations were not run: this daemon is stopping; the daemon that serves the queues next runs them", dlog.Context{"cause": err.Error()})
		return
	}
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not list the repositories whose mediations are owed", dlog.Context{"error": err.Error()})
		return
	}
	for _, q := range queues {
		m, err := o.deps.DB.MediationOf(ctx, q.key)
		if cutUnder(ctx, err) {
			o.deps.Log.Global().Info(opMediation, "the mediations were not run: this daemon is stopping; the daemon that serves the queues next runs them", dlog.Context{"repo": string(q.key), "cause": err.Error()})
			return
		}
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read a repository's owed mediation", dlog.Context{"repo": string(q.key), "error": err.Error()})
			continue
		}
		if m == nil || !o.claimMediation(q.key) {
			continue
		}
		if !o.async {
			o.mediationRound(context.WithoutCancel(ctx), q, *m)
			continue
		}
		owed := *m
		inflight.Go(func() { o.mediationRound(context.WithoutCancel(ctx), q, owed) })
	}
}

// claimMediation reports whether this caller runs REPO's round: none runs
// for it already.
func (o *orchestrator) claimMediation(repo wsm.RepoKey) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.mediating[repo] {
		return false
	}
	o.mediating[repo] = true
	return true
}

// mediationRound tells Q's mediator of mediation M, when it was not told yet,
// and awaits the turn it was told in; then settles a mediation that turn
// decided nothing about (mediationTurnEnded).
func (o *orchestrator) mediationRound(ctx context.Context, q mediatedQueue, m wsm.Mediation) {
	defer func() {
		o.mu.Lock()
		delete(o.mediating, q.key)
		o.mu.Unlock()
	}()
	fields := dlog.Context{"repo": string(q.key), "mediator": q.mediator, "deploy_started": m.OwedAt.UTC().Format(time.RFC3339Nano)}
	ws, err := o.mediatorOf(ctx, q)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the deploy mediator could not be found or made; the queue stays held for it", withField(fields, "error", err.Error()))
		return
	}
	fields["workspace"] = string(ws)
	brief, err := o.deps.Briefs(BriefDeployMediation, map[string]string{
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"repo_dir":       q.repository.Dir,
		"version":        mediationVersion(m),
	})
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the mediation brief could not be composed; the queue stays held", withField(fields, "error", err.Error()))
		return
	}
	if !m.Prompted() {
		turn, err := o.tellMediator(ctx, q, ws, m, brief, fields)
		if err != nil || turn == "" {
			return
		}
		_, err = o.deps.AwaitTurnEnd(ctx, ws, turn)
	} else {
		// THE TURN WAS RECORDED BEFORE ITS BRIEF WAS QUEUED, so a round cut
		// between the two (a refused submission, the daemon's exit) left a
		// turn that never reached the session: reattaching submits it then,
		// and awaits one that did.
		o.deps.Log.Global().Info(opMediation, "the mediator was told of this deploy before; its turn is reattached", withField(fields, "turn", string(m.Turn)))
		_, err = o.reattachTurn(ctx, ws, fields, m.Turn, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION)
	}
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the mediator's turn could not be awaited; the queue stays held until the next round", withField(fields, "error", err.Error()))
		return
	}
	o.mediationTurnEnded(ctx, q, ws, m.OwedAt, fields)
}

// tellMediator compacts the mediator's context and tells it of M in BRIEF,
// behind the compaction, answering the brief's turn: empty when the
// mediation was replaced or released meanwhile.
func (o *orchestrator) tellMediator(ctx context.Context, q mediatedQueue, ws ids.WorkspaceID, m wsm.Mediation, brief string, fields dlog.Context) (ids.TurnID, error) {
	// EVERY ROUND OPENS ON A COMPACTED CONTEXT (owner request, 2026-11-13).
	// A mediator that never ran a turn has no context to compact: the vendor
	// refuses it ("Not enough messages to compact."), so its first round
	// opens on the empty context it has.
	conversed, err := o.deps.DB.HasTurns(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read whether the mediator has a context to compact; the queue stays held", withField(fields, "error", err.Error()))
		return "", err
	}
	if conversed {
		compact := sessioncommand.Literal(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
		if _, err := o.submitMediatorPrompt(ctx, ws, wsm.NewTurnID(), compact, conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT); err != nil {
			o.deps.Log.Global().Error(opMediation, "the mediator's context could not be compacted; the queue stays held", withField(fields, "error", err.Error()))
			return "", err
		}
	}
	turn := wsm.NewTurnID()
	marked, err := o.deps.DB.MarkMediationPrompted(ctx, q.key, m.OwedAt, o.deps.Now(), turn)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not record that the mediator is told of the deploy; the queue stays held", withField(fields, "error", err.Error()))
		return "", err
	}
	if !marked {
		o.deps.Log.Global().Info(opMediation, "the mediation was replaced or released before its mediator was told; this round ends", fields)
		return "", nil
	}
	if _, err := o.submitMediatorPrompt(ctx, ws, turn, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		o.deps.Log.Global().Error(opMediation, "the mediation brief could not be submitted; the queue stays held", withField(fields, "error", err.Error()))
		return "", err
	}
	o.deps.Log.Global().Info(opMediation, "the mediator was told of the deploy", withField(withField(fields, "turn", string(turn)), "compacted", conversed))
	o.republishMediation(ctx, q.key)
	return turn, nil
}

// submitMediatorPrompt puts TEXT in WS's prompt queue as TURN, behind any turn
// it is running.
func (o *orchestrator) submitMediatorPrompt(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, text string, origin conversationv1.PromptOrigin) (promptqueue.Disposition, error) {
	disposition, err := o.deps.Queue.Submit(ctx, promptqueue.Submission{
		WS: ws, Turn: turn, Said: saidText(text), Origin: origin, Delivery: wsm.DeliveryDeferred,
	})
	if err == nil && disposition.RefusedArm != "" {
		err = fmt.Errorf("merge: the prompt queue refused the mediator's prompt: %s", disposition.RefusedArm)
	}
	return disposition, err
}

// mediationTurnEnded settles the mediation owed at OWED once the turn the
// mediator was told of it in ended: still owed is a deploy it decided nothing
// about, let through and put before the user; one replaced by a newer
// deploy is that deploy's round's; one released was decided.
func (o *orchestrator) mediationTurnEnded(ctx context.Context, q mediatedQueue, ws ids.WorkspaceID, owed time.Time, fields dlog.Context) {
	released, err := o.deps.DB.ReleaseMediation(ctx, q.key, owed)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not settle a mediation its turn ended on; the queue stays held", withField(fields, "error", err.Error()))
		return
	}
	if !released {
		o.deps.Log.Global().Info(opMediation, "the mediator's turn ended with its mediation decided or replaced by a newer deploy", fields)
		return
	}
	o.deps.Log.Global().Error(opMediation, "the mediator's turn ended deciding nothing about the deploy; the queue is let through and the user is told", fields)
	o.deps.Banners.Mediated(ws, "the deploy mediator ended its turn without deciding the deploy; the queue moved on")
	o.republishMediation(ctx, q.key)
	o.kick(q.key)
}

// mediatorOf answers Q's mediator workspace, making it when it does not
// exist (Deps.CreateMediator).
func (o *orchestrator) mediatorOf(ctx context.Context, q mediatedQueue) (ids.WorkspaceID, error) {
	ws, ok, err := o.existingMediator(ctx, q)
	if err != nil {
		return "", err
	}
	if ok {
		return ws, nil
	}
	made, err := o.deps.CreateMediator(ctx, q.repository.Dir, q.mediator)
	if err != nil {
		return "", fmt.Errorf("merge: make the mediator %s: %w", q.mediator, err)
	}
	o.deps.Log.Global().Info(opMediation, "made the repository's deploy mediator", dlog.Context{
		"repo": string(q.key), "mediator": q.mediator, "workspace": string(made.ID)})
	return made.ID, nil
}

// existingMediator answers Q's open mediator workspace, and false while none
// is made: the one workspace of Q's repository carrying the mediator's name.
func (o *orchestrator) existingMediator(ctx context.Context, q mediatedQueue) (ids.WorkspaceID, bool, error) {
	workspaces, err := o.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return "", false, fmt.Errorf("merge: list the workspaces to find the mediator %s: %w", q.mediator, err)
	}
	for _, ws := range workspaces {
		if !ws.Closed && ws.Name == q.mediator && ws.Repo == q.repository.ID {
			return ws.ID, true, nil
		}
	}
	return "", false, nil
}

// mediationVersion is how the brief names M's build.
func mediationVersion(m wsm.Mediation) string {
	if m.Version == "" {
		return "the build its deploy installed"
	}
	return m.Version
}

// DecideMediation implements Orchestrator: mediator WS's DECISION releases
// the mediation its repository owes, and the queue advances; an escalation
// also puts SUMMARY before the user.
func (o *orchestrator) DecideMediation(ctx context.Context, ws ids.WorkspaceID, decision MediationDecision, summary string) error {
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the workspace a mediation decision came from", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return fmt.Errorf("merge: read the deciding workspace %s: %w", ws, err)
	}
	queues, err := o.mediatedQueues(ctx, opMediation)
	if err != nil {
		return err
	}
	var q *mediatedQueue
	for i := range queues {
		if queues[i].mediator == record.Name && queues[i].repository.ID == record.Repo {
			q = &queues[i]
		}
	}
	fields := dlog.Context{"workspace": string(ws), "decision": decision.String(), "summary": summary}
	if q == nil {
		o.deps.Log.Global().Warn(opMediation, "refused a mediation decision from a workspace that mediates no repository's deploys", fields)
		return refuse(ArmNotMediator, ws, "workspace %s mediates no repository's deploys", record.Name)
	}
	fields["repo"] = string(q.key)
	m, err := o.deps.DB.MediationOf(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the mediation a decision releases", withField(fields, "error", err.Error()))
		return err
	}
	if m == nil {
		o.deps.Log.Global().Warn(opMediation, "refused a mediation decision while no deploy owes one", fields)
		return refuse(ArmNoMediationOwed, ws, "the repository owes no mediation")
	}
	if _, err := o.deps.DB.ReleaseMediation(ctx, q.key, m.OwedAt); err != nil {
		o.deps.Log.Global().Error(opMediation, "could not release the mediation the decision named", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Log.Global().Info(opMediation, "the mediator decided the deploy; the queue advances", fields)
	if decision == MediationEscalate {
		o.deps.Banners.Mediated(ws, summary)
	}
	o.republishMediation(ctx, q.key)
	o.kick(q.key)
	return nil
}

// rosterMediation is the deploy mediation REPO's queue waits on as the
// roster draws it: nil while it owes none, or while its mediator is not made
// yet (the round that makes it republishes the queue).
func (o *orchestrator) rosterMediation(ctx context.Context, repo wsm.RepoKey) (*sidebar.QueueMediation, error) {
	m, err := o.deps.DB.MediationOf(ctx, repo)
	if err != nil || m == nil {
		return nil, err
	}
	queues, err := o.mediatedQueues(ctx, opMediation)
	if err != nil {
		return nil, err
	}
	for _, q := range queues {
		if q.key != repo {
			continue
		}
		ws, ok, err := o.existingMediator(ctx, q)
		if err != nil || !ok {
			return nil, err
		}
		return &sidebar.QueueMediation{Mediator: ws, Told: m.Prompted()}, nil
	}
	return nil, nil
}

// republishMediation tells the roster and the queue's subscribers a change
// of REPO's mediation; a republication that fails is recorded.
func (o *orchestrator) republishMediation(ctx context.Context, repo wsm.RepoKey) {
	if err := o.republishQueue(ctx, repo); err != nil {
		o.deps.Log.Global().Error(opMediation, "could not republish the queue whose mediation changed", dlog.Context{"repo": string(repo), "error": err.Error()})
	}
}
