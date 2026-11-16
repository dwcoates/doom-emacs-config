package merge

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// THE DEPLOY CHECKS (repoconfig.v1.LifecycleConfig.deploy_checks; owner
// rulings, 2026-11-14; docs/investigations/2026-11-14-deploy-intake-plan.md).
//
// EVERY DEPLOY OWES A CHECK, durably, at its start (OweMediations): the
// queue's head lands nothing while one is owed (queueHeld). Deploys owed in a
// row form a CHAIN, which has one hidden INTAKE workspace, made when the
// chain's first deploy has rolled out and told of every deploy of the chain
// in turn (Mediate). Within that turn the intake approves the deploy or
// escalates it to the user (either releases the queue and ends the chain), or
// DISPATCHES the problems the deploy caused to a remediation workspace
// (dispatch.go), whose finish lands and deploys its fix directly
// (remediationfinish.go): that deploy owes the chain's next check. A turn
// that ends with the check still owed and nothing dispatched decided
// nothing: the queue is let through, loudly, and the user is told.

// BriefDeployIntake is the intake's brief of one deploy.
const BriefDeployIntake = "deploy-intake"

// opMediation is the operation every mediation record is filed under.
const opMediation = "daemon.merge.mediation"

// The refusals DecideMediation answers.
const (
	// ArmNotMediator refuses a decision from a workspace that is not the
	// intake of a chain whose check is owed.
	ArmNotMediator = "not_mediator"
	// ArmNoMediationOwed refuses a decision when the intake's deploy owes no
	// check: a fix was dispatched for it already.
	ArmNoMediationOwed = "no_mediation_owed"
	// ArmNoRemediationToRestore refuses a dispatch restoring the chain's last
	// remediation when the chain has none.
	ArmNoRemediationToRestore = "no_remediation_to_restore"
	// ArmRemediationBoundReached refuses a dispatch once the chain deployed
	// as many remediations in a row as its repository allows.
	ArmRemediationBoundReached = "remediation_bound_reached"
	// ArmProblemsUnowned refuses an approval while the check's window holds a
	// group no one owns: NEW, SEEN BEFORE, or RECURRED after its fix.
	ArmProblemsUnowned = "problems_unowned"
)

// MediationDecision is what an intake decided of a deploy.
type MediationDecision int

const (
	// MediationApprove passes the deploy: the queue advances.
	MediationApprove MediationDecision = iota + 1
	// MediationEscalate puts the deploy before the user, and the queue
	// advances.
	MediationEscalate
	// MediationDispatch hands the problems the deploy caused to a
	// remediation workspace; the queue stays held.
	MediationDispatch
)

// String names the decision in records.
func (d MediationDecision) String() string {
	switch d {
	case MediationApprove:
		return "approve"
	case MediationEscalate:
		return "escalate"
	case MediationDispatch:
		return "dispatch"
	}
	return fmt.Sprintf("MediationDecision(%d)", int(d))
}

// DeployProblem is one problem a deploy caused, as the intake states it
// (agentrepl.v1.DeployProblem).
type DeployProblem struct {
	Statement string
	Evidence  []string
}

// Decision is an intake's decision on the deploy its chain owes a check of
// (agentrepl.v1.UpdateMergeQueueMediation).
type Decision struct {
	Kind    MediationDecision
	Summary string
	// Problems are a dispatch's or an escalation's findings, with their
	// evidence; Restore is a dispatch's: whether the chain's last remediation
	// workspace takes them rather than a fresh one.
	Problems []DeployProblem
	Restore  bool
}

// The spans of a deploy check a Meter records: how long its deploy rolled out
// before its intake was told (the sidebar's "hot reloading"), how long the
// intake then assessed it ("assessing reload"), and both together.
const (
	MediationSpanRollout    = "rollout"
	MediationSpanAssessment = "assessment"
	MediationSpanTotal      = "total"
)

// The outcomes a check's spans end in. The rollout span ends told; the
// assessment span (when the intake was told) ends at the decision, and the
// total at the store write that ended the check.
const (
	// MediationTold ends the rollout span: the intake was told.
	MediationTold = "told"
	// MediationApproved: the intake found nothing this deploy caused.
	MediationApproved = "approved"
	// MediationEscalated: the intake put the deploy before the user.
	MediationEscalated = "escalated"
	// MediationDispatched: the intake dispatched the problems the deploy
	// caused to a remediation workspace.
	MediationDispatched = "dispatched"
	// MediationSuperseded: a newer deploy replaced the check before anything
	// was dispatched for it.
	MediationSuperseded = "superseded"
	// MediationUndecided: the intake's turn ended deciding nothing.
	MediationUndecided = "undecided"
	// MediationUnmediated: the repository checks its deploys no more.
	MediationUnmediated = "unmediated"
	// MediationUnchanged: the deploy changed nothing, so it was not checked.
	MediationUnchanged = "unchanged"
)

// The outcomes a remediation's span ends in (Meter.ObserveRemediation): from
// its dispatch to the check that ended it.
const (
	// RemediationLanded: the fix's deploy was approved.
	RemediationLanded = "landed"
	// RemediationEscalated: the chain was escalated, or ended undecided,
	// after the dispatch.
	RemediationEscalated = "escalated"
	// RemediationRedispatched: the fix's deploy was still broken, and the
	// intake dispatched again.
	RemediationRedispatched = "redispatched"
)

// observeMediationEnd records how check M ended, as OUTCOME, at AT: its
// assessment span when its intake was told and decided nothing else of it,
// and its total. An end that lets the queue advance (all but a replacement)
// ends M's chain too, and the remediation the chain dispatched last.
func (o *orchestrator) observeMediationEnd(m wsm.Mediation, outcome string, at time.Time) {
	// A DISPATCHED CHECK'S ASSESSMENT ENDED AT ITS DISPATCH, recorded then.
	if m.PromptedAt != nil && !m.Dispatched {
		o.deps.Meter.ObserveMediationSpan(MediationSpanAssessment, outcome, at.Sub(*m.PromptedAt))
	}
	o.deps.Meter.ObserveMediationSpan(MediationSpanTotal, outcome, at.Sub(m.OwedAt))
	if outcome == MediationDispatched || outcome == MediationSuperseded {
		return
	}
	o.deps.Meter.ObserveMediationChain(outcome, m.ChainRedeploys, at.Sub(m.ChainStartedAt))
	o.deps.Meter.SetRemediationsInARow(0)
	if m.LastDispatchedAt != nil {
		ended := RemediationEscalated
		if outcome == MediationApproved {
			ended = RemediationLanded
		}
		o.deps.Meter.ObserveRemediation(ended, at.Sub(*m.LastDispatchedAt))
	}
}

// replacedOutcome is how a check a newer deploy replaced ended: dispatched
// when the newer deploy is the fix dispatched for it, superseded when not,
// including a dispatch the newer deploy carries on (wsm.OweMediation).
func replacedOutcome(m wsm.ReplacedMediation) string {
	if m.Dispatched && !m.DispatchCarried {
		return MediationDispatched
	}
	return MediationSuperseded
}

// checkedQueue is a registered repository whose deploys are checked.
type checkedQueue struct {
	registeredQueue
	// bound is the most remediations a chain deploys in a row
	// (repoconfig.v1.DeployChecks.remediations_before_escalation).
	bound int
}

// checkedQueues answers every registered repository whose lifecycle config
// checks its deploys. A repository with no lifecycle config checks none; one
// whose config cannot be read is recorded under OP and passed over.
func (o *orchestrator) checkedQueues(ctx context.Context, op string) ([]checkedQueue, error) {
	queues, err := o.registeredQueues(ctx, op)
	if err != nil {
		return nil, err
	}
	var out []checkedQueue
	for _, q := range queues {
		cfg, err := o.deps.DeployConfig.Load(q.repository.Dir)
		var missing *lifecycleconfig.MissingError
		switch {
		case errors.As(err, &missing):
			continue
		case err != nil:
			o.deps.Log.Global().Error(op, "could not read a repository's lifecycle config to find whether its deploys are checked; it is passed over",
				dlog.Context{"repository": q.repository.Dir, "error": err.Error()})
			continue
		}
		if checks := cfg.GetDeployChecks(); checks != nil {
			out = append(out, checkedQueue{registeredQueue: q, bound: int(checks.GetRemediationsBeforeEscalation())})
		}
	}
	return out, nil
}

// checkedQueueOf answers the checked queue keyed REPO, false when its
// repository checks its deploys no more.
func (o *orchestrator) checkedQueueOf(ctx context.Context, repo wsm.RepoKey) (checkedQueue, bool, error) {
	queues, err := o.checkedQueues(ctx, opMediation)
	if err != nil {
		return checkedQueue{}, false, err
	}
	for _, q := range queues {
		if sameQueue(q.key, repo) {
			return q, true, nil
		}
	}
	return checkedQueue{}, false, nil
}

// sameQueue reports whether A and B key one queue: the store answers a key
// in its canonical spelling (dirpath.Canonical), which a queue's registered
// key may spell through a symlink. A key is always an absolute directory, so
// one that cannot be canonicalized is a broken invariant.
func sameQueue(a, b wsm.RepoKey) bool { return canonicalKey(a) == canonicalKey(b) }

// canonicalKey is KEY as the store spells it.
func canonicalKey(key wsm.RepoKey) string {
	out, err := dirpath.Canonical(string(key))
	if err != nil {
		panic(fmt.Sprintf("merge: the queue key %q is no absolute directory: %v", key, err))
	}
	return out
}

// OweMediations implements Orchestrator: the deploy that started AT owes a
// check in every repository that checks its deploys. A check that cannot be
// recorded is recorded at ERROR and answered: that deploy goes unchecked.
func (o *orchestrator) OweMediations(ctx context.Context, at time.Time) error {
	queues, err := o.checkedQueues(ctx, opMediation)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not list the repositories a deploy owes checks in; the deploy goes unchecked", dlog.Context{"error": err.Error()})
		return err
	}
	var errs []error
	for _, q := range queues {
		replaced, err := o.deps.DB.OweMediation(ctx, q.key, at, "")
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not owe a deploy's check; the deploy goes unchecked in this repository", dlog.Context{
				"repo": string(q.key), "error": err.Error()})
			errs = append(errs, err)
			continue
		}
		if replaced != nil {
			outcome := replacedOutcome(*replaced)
			o.observeMediationEnd(replaced.Mediation, outcome, at)
			o.deps.Log.Global().Info(opMediation, "a newer deploy replaced the check owed; its chain carries on", dlog.Context{
				"repo": string(q.key), "outcome": outcome, "intake": string(replaced.Intake), "remediation": string(replaced.Remediation),
				"dispatch_carried": replaced.DispatchCarried, "replaced_deploy_started": replaced.OwedAt.UTC().Format(time.RFC3339Nano)})
		}
		o.deps.Log.Global().Info(opMediation, "the deploy owes a check; the queue's head lands nothing until the chain's intake decides", dlog.Context{
			"repo": string(q.key), "deploy_started": at.UTC().Format(time.RFC3339Nano)})
		// A REMEDIATION DEPLOYING IS THIS DEPLOY'S, so its finish is done.
		if err := o.remediationDeployed(ctx, q.key, at); err != nil {
			o.deps.Log.Global().Error(opMediation, "could not record that the deploy is the remediation's; the next daemon's pass records it", dlog.Context{"repo": string(q.key), "error": err.Error()})
		}
		o.republishMediation(ctx, q.key)
	}
	return errors.Join(errs...)
}

// ReleaseUnchangedDeploy implements Orchestrator: the deploy that began AT
// changed nothing, every component up to date, so the check it owes is
// released unchecked and its queue advances (2026-11-14: a deploy that
// installed nothing held the queue for an intake's check). The logs it left
// are not skipped: the next check's window starts where the last one read.
// A check that carries an earlier check's chain (a fix dispatched or an
// intake at work) stays owed for that chain's intake. A failure is recorded
// at ERROR and answered: the check stays owed and its intake checks it.
func (o *orchestrator) ReleaseUnchangedDeploy(ctx context.Context, at time.Time) error {
	queues, err := o.checkedQueues(ctx, opMediation)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not list the repositories an unchanged deploy owes checks in; its checks stay owed", dlog.Context{"error": err.Error()})
		return err
	}
	var errs []error
	for _, q := range queues {
		fields := dlog.Context{"repo": string(q.key), "deploy_started": at.UTC().Format(time.RFC3339Nano)}
		m, err := o.deps.DB.MediationOf(ctx, q.key)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read the check an unchanged deploy owes; it stays owed", withField(fields, "error", err.Error()))
			errs = append(errs, err)
			continue
		}
		if m == nil || !m.OwedAt.Equal(at) {
			continue
		}
		if !m.ChainStartedAt.Equal(m.OwedAt) {
			o.deps.Log.Global().Info(opMediation, "the deploy changed nothing, but its check carries an earlier chain; the chain's intake checks it", withField(fields, "chain_started", m.ChainStartedAt.UTC().Format(time.RFC3339Nano)))
			continue
		}
		released, err := o.deps.DB.ReleaseMediation(ctx, q.key, at)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not release the check of a deploy that changed nothing; it stays owed", withField(fields, "error", err.Error()))
			errs = append(errs, err)
			continue
		}
		if !released {
			continue
		}
		o.endChain(ctx, q.key, *m, MediationUnchanged)
		o.deps.Log.Global().Info(opMediation, "the deploy changed nothing; its check is released unchecked and the queue advances", fields)
		o.republishMediation(ctx, q.key)
		o.kick(q.key)
	}
	return errors.Join(errs...)
}

// Mediate implements Orchestrator: runs a round for every check owed and not
// yet awaited here, and takes up every remediation finish a restart cut
// (remediationfinish.go). A daemon that may not advance its queues runs
// none; its successor's takeover calls this again.
func (o *orchestrator) Mediate(ctx context.Context) {
	if !o.deps.Standing.ServesQueue() {
		o.deps.Log.Global().Info(opMediation, "this daemon does not serve the queues; their checks are run by the daemon that does", nil)
		return
	}
	// THE PASS RUNS INSIDE THE MEDIATION'S LIFETIME (mediationlife.go), so
	// the daemon's exit cuts it and waits for it before the state client
	// closes; it also ends with the caller's context.
	life, leave, entered := o.enterMediation()
	if !entered {
		o.deps.Log.Global().Info(opMediation, mediationNotRun, nil)
		return
	}
	defer leave()
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	defer context.AfterFunc(life, cancel)()
	queues, err := o.checkedQueues(ctx, opMediation)
	if err != nil {
		o.mediationFailed(ctx, nil, "could not list the repositories whose checks are owed", err)
		return
	}
	o.releaseUnchecked(ctx, queues)
	o.resumeRemediationFinishes(ctx)
	for _, q := range queues {
		m, err := o.deps.DB.MediationOf(ctx, q.key)
		if err != nil {
			if o.mediationFailed(ctx, dlog.Context{"repo": string(q.key)}, "could not read a repository's owed check", err) {
				return
			}
			continue
		}
		if m == nil {
			continue
		}
		// A ROUND OUTLIVES THE PASS, NOT THE DAEMON: it runs under the
		// mediation's lifetime, which only the daemon's exit cuts.
		round, leaveRound, entered := o.enterMediation()
		if !entered {
			o.deps.Log.Global().Info(opMediation, mediationNotRun, dlog.Context{"repo": string(q.key)})
			return
		}
		if !o.claimMediation(q.key, m.OwedAt) {
			o.deps.Log.Global().Info(opMediation, "a round for this check runs already; this pass leaves it to that round", dlog.Context{
				"repo": string(q.key), "deploy_started": m.OwedAt.UTC().Format(time.RFC3339Nano)})
			leaveRound()
			continue
		}
		if !o.async {
			o.intakeRound(round, q, *m)
			leaveRound()
			continue
		}
		owed := *m
		inflight.Go(func() {
			defer leaveRound()
			o.intakeRound(round, q, owed)
		})
	}
}

// releaseUnchecked releases the check every registered repository in ALL
// owes whose lifecycle config checks its deploys no more (CHECKED): no intake
// can decide it, so it would hold the queue forever. A repository whose
// config cannot be read was passed over by checkedQueues, recorded, and is
// left owed: its checks may still be on.
func (o *orchestrator) releaseUnchecked(ctx context.Context, checked []checkedQueue) {
	all, err := o.registeredQueues(ctx, opMediation)
	if err != nil {
		o.mediationFailed(ctx, nil, "could not list the repositories to release the checks no intake decides", err)
		return
	}
	on := make(map[wsm.RepoKey]bool, len(checked))
	for _, q := range checked {
		on[q.key] = true
	}
	for _, q := range all {
		if on[q.key] {
			continue
		}
		if _, err := o.deps.DeployConfig.Load(q.repository.Dir); err != nil {
			var missing *lifecycleconfig.MissingError
			if !errors.As(err, &missing) {
				continue
			}
		}
		m, err := o.deps.DB.MediationOf(ctx, q.key)
		if err != nil {
			if o.mediationFailed(ctx, dlog.Context{"repo": string(q.key)}, "could not read whether a repository that checks no deploys owes a check", err) {
				return
			}
			continue
		}
		if m == nil {
			continue
		}
		released, err := o.deps.DB.ReleaseMediation(ctx, q.key, m.OwedAt)
		if err != nil {
			if o.mediationFailed(ctx, dlog.Context{"repo": string(q.key)}, "could not release a check no intake decides; the queue stays held", err) {
				return
			}
			continue
		}
		if released {
			o.endChain(ctx, q.key, *m, MediationUnmediated)
			o.deps.Log.Global().Info(opMediation, "released a check owed by a repository whose lifecycle config checks no deploys now; its queue advances", dlog.Context{
				"repo": string(q.key), "deploy_started": m.OwedAt.UTC().Format(time.RFC3339Nano)})
			o.republishMediation(ctx, q.key)
			o.kick(q.key)
		}
	}
}

// endChain records how check M of queue REPO, the last of its chain, ended
// as OUTCOME, forgets what the chain's remediations landed, and stops the
// chain's intake at once: the next deploy begins another chain with a fresh
// intake. The intake deciding inside its own turn is cut mid-turn, so it
// writes nothing past its decision (owner ruling, 2026-11-16).
func (o *orchestrator) endChain(ctx context.Context, repo wsm.RepoKey, m wsm.Mediation, outcome string) {
	o.observeMediationEnd(m, outcome, o.deps.Now())
	fields := dlog.Context{"repo": string(repo), "intake": string(m.Intake), "outcome": outcome, "chain_started": m.ChainStartedAt.UTC().Format(time.RFC3339Nano)}
	if _, err := o.deps.DB.EndRemediationFinishesOf(ctx, repo); err != nil {
		o.deps.Log.Global().Error(opMediation, "the chain ended but what its remediations landed could not be forgotten", withField(fields, "error", err.Error()))
	}
	if m.Intake == "" {
		return
	}
	if err := o.deps.DeployChecks.StopNow(ctx, m.Intake); err != nil {
		o.deps.Log.Global().Error(opMediation, "the chain ended but its intake could not be stopped", withField(fields, "error", err.Error()))
		return
	}
	o.deps.Log.Global().Info(opMediation, "the chain ended; its intake is stopped", fields)
}

// checkClaim is one check's round: the queue and the deploy it checks. A
// claim is the check's, never the queue's, so a round still awaiting the
// turn of a check a newer deploy replaced never keeps the newer check's
// round from running (2026-11-14: a replaced round's claim on the queue
// left a deploy's check untold for twenty minutes).
type checkClaim struct {
	repo wsm.RepoKey
	owed int64
}

// claimMediation reports whether this caller runs the round of the check
// REPO owes at OWED: none runs for it already.
func (o *orchestrator) claimMediation(repo wsm.RepoKey, owed time.Time) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	claim := checkClaim{repo: repo, owed: owed.UnixNano()}
	if o.mediating[claim] {
		return false
	}
	o.mediating[claim] = true
	return true
}

// intakeRound tells Q's chain's intake of check M, when it was not told yet,
// and awaits the turn it was told in; then settles a check that turn decided
// nothing about (intakeTurnEnded). A check a fix was dispatched for has no
// round: the remediation workspace is at work, and its deploy owes the next.
func (o *orchestrator) intakeRound(ctx context.Context, q checkedQueue, m wsm.Mediation) {
	defer func() {
		o.mu.Lock()
		delete(o.mediating, checkClaim{repo: q.key, owed: m.OwedAt.UnixNano()})
		o.mu.Unlock()
	}()
	fields := dlog.Context{"repo": string(q.key), "deploy_started": m.OwedAt.UTC().Format(time.RFC3339Nano),
		"chain_started": m.ChainStartedAt.UTC().Format(time.RFC3339Nano), "chain_redeploys": m.ChainRedeploys}
	// EVERY ROUND STATES THE CHAIN IT CHECKS, so a daemon that booted on a
	// remediation's deploy serves the count the chain carries.
	o.deps.Meter.SetRemediationsInARow(m.ChainRedeploys)
	if m.Dispatched {
		o.deps.Log.Global().Info(opMediation, "a fix is dispatched for this deploy; its remediation workspace is at work, and its deploy owes the next check", withField(fields, "remediation", string(m.Remediation)))
		return
	}
	ws, err := o.intakeOf(ctx, q, m, fields)
	if err != nil {
		o.mediationFailed(ctx, fields, "the chain's intake could not be found or made; the queue stays held for it", err)
		return
	}
	if ws == "" {
		return
	}
	fields["intake"] = string(ws)
	brief, window, err := o.intakeBrief(ctx, q, m)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the intake's brief could not be composed; the queue stays held", withField(fields, "error", err.Error()))
		return
	}
	if !m.Prompted() {
		// THE AWAIT'S ERROR IS THE ROUND'S err, never one scoped to this
		// branch: a shadowed err dropped a failed await, and the round then
		// settled the check as decided nothing, letting the queue through.
		var turn ids.TurnID
		turn, err = o.tellIntake(ctx, q, ws, m, brief, window, fields)
		if err != nil || turn == "" {
			return
		}
		_, err = o.deps.AwaitTurnEnd(ctx, ws, turn)
	} else {
		// THE TURN WAS RECORDED BEFORE ITS BRIEF WAS QUEUED, so a round cut
		// between the two (a refused submission, the daemon's exit) left a
		// turn that never reached the session: reattaching submits it then,
		// and awaits one that did.
		// A RESUMED INTAKE'S TURN IS ITS RESUMING BRIEF'S.
		told := brief
		if m.Resumed {
			if told, err = o.resumeBrief(m); err != nil {
				o.deps.Log.Global().Error(opMediation, "the undecided intake's resuming brief could not be composed; the queue stays held", withField(fields, "error", err.Error()))
				return
			}
		}
		o.deps.Log.Global().Info(opMediation, "the intake was told of this deploy before; its turn is reattached", withField(withField(fields, "turn", string(m.Turn)), "resumed", m.Resumed))
		_, err = o.reattachTurn(ctx, ws, fields, m.Turn, told, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION)
	}
	if err != nil {
		o.mediationFailed(ctx, fields, "the intake's turn could not be awaited; the queue stays held until the next round", err)
		return
	}
	o.intakeTurnEnded(ctx, q, ws, m.OwedAt, fields)
}

// intakeOf answers the intake of the chain check M belongs to, making it,
// hidden and unprompted, for a chain's first deploy and recording it on M
// before anything prompts it: its first prompt then brings its session up
// as the intake. Empty when M was replaced or released meanwhile.
func (o *orchestrator) intakeOf(ctx context.Context, q checkedQueue, m wsm.Mediation, fields dlog.Context) (ids.WorkspaceID, error) {
	if m.Intake != "" {
		return m.Intake, nil
	}
	name := "deploy-intake-" + m.ChainStartedAt.UTC().Format("20060102-150405")
	made, err := o.deps.DeployChecks.CreateDeployIntake(ctx, q.repository.Dir, name)
	if err != nil {
		return "", fmt.Errorf("merge: make the chain's intake %s: %w", name, err)
	}
	recorded, err := o.deps.DB.RecordMediationIntake(ctx, q.key, m.OwedAt, made.ID)
	if err != nil {
		return "", fmt.Errorf("merge: record the chain's intake %s: %w", name, err)
	}
	if !recorded {
		// THE CHECK ENDED WHILE ITS INTAKE WAS MADE: the made workspace
		// serves no chain, so it is closed; a replacing deploy's round makes
		// its own.
		o.deps.Log.Global().Info(opMediation, "the check was replaced or released while its intake was made; the made intake is closed", withField(fields, "intake", string(made.ID)))
		if _, err := o.deps.DeployChecks.CloseSelf(ctx, made.ID); err != nil {
			o.deps.Log.Global().Error(opMediation, "an intake made for a check that ended could not be closed", withField(withField(fields, "intake", string(made.ID)), "error", err.Error()))
		}
		return "", nil
	}
	// THE ROLE IS RECORDED BEFORE THE EFFORT IS SET: setting it starts the
	// sessionless intake's shim, whose landing reads the role.
	if err := o.deps.DeployChecks.SetDeployCheckEffort(ctx, made.ID, DeployCheckIntake); err != nil {
		return "", fmt.Errorf("merge: set the chain's intake %s's effort: %w", name, err)
	}
	o.deps.Log.Global().Info(opMediation, "made the chain's intake", withField(withField(fields, "intake", string(made.ID)), "name", name))
	return made.ID, nil
}

// tellIntake tells the chain's intake WS of M in BRIEF, answering the brief's
// turn: empty when the check was replaced or released meanwhile. The intake's
// context is never compacted: every deploy of its chain stays in it.
func (o *orchestrator) tellIntake(ctx context.Context, q checkedQueue, ws ids.WorkspaceID, m wsm.Mediation, brief string, window wsm.CheckWindow, fields dlog.Context) (ids.TurnID, error) {
	turn := wsm.NewTurnID()
	told := o.deps.Now()
	marked, err := o.deps.DB.MarkMediationPrompted(ctx, q.key, m.OwedAt, told, turn)
	if err != nil {
		o.mediationFailed(ctx, fields, "could not record that the intake is told of the deploy; the queue stays held", err)
		return "", err
	}
	if !marked {
		o.deps.Log.Global().Info(opMediation, "the check was replaced or released before its intake was told; this round ends", fields)
		return "", nil
	}
	// THE WINDOW IS RECORDED WITH THE TELLING: the next check reads from its
	// end, and the decision is judged against its groups. A crash between the
	// two writes leaves the next window starting earlier, reading some records
	// twice, never fewer.
	recorded, err := o.deps.DB.RecordMediationWindow(ctx, q.key, m.OwedAt, window)
	if err != nil {
		o.mediationFailed(ctx, fields, "could not record the window the intake is told; the queue stays held", err)
		return "", err
	}
	if !recorded {
		o.deps.Log.Global().Info(opMediation, "the check was replaced or released before its window was recorded; this round ends", fields)
		return "", nil
	}
	o.deps.Meter.ObserveMediationSpan(MediationSpanRollout, MediationTold, told.Sub(m.OwedAt))
	if _, err := o.submitCheckPrompt(ctx, ws, turn, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		o.mediationFailed(ctx, fields, "the intake's brief could not be submitted; the queue stays held", err)
		return "", err
	}
	o.deps.Log.Global().Info(opMediation, "the intake was told of the deploy", withField(fields, "turn", string(turn)))
	o.republishMediation(ctx, q.key)
	return turn, nil
}

// submitCheckPrompt puts TEXT in WS's prompt queue as TURN, behind any turn
// it is running.
func (o *orchestrator) submitCheckPrompt(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, text string, origin conversationv1.PromptOrigin) (promptqueue.Disposition, error) {
	disposition, err := o.deps.Queue.Submit(ctx, promptqueue.Submission{
		WS: ws, Turn: turn, Said: saidText(text), Origin: origin, Delivery: wsm.DeliveryDeferred,
	})
	if err == nil && disposition.RefusedArm != "" {
		err = fmt.Errorf("merge: the prompt queue refused the deploy check's prompt: %s", disposition.RefusedArm)
	}
	return disposition, err
}

// intakeTurnEnded settles the check owed at OWED once the turn the intake
// was told of it in ended: still owed with nothing dispatched is a deploy it
// decided nothing about, let through and put before the user; one replaced
// by a newer deploy is that deploy's round's; one released was decided; one
// dispatched waits on its remediation.
func (o *orchestrator) intakeTurnEnded(ctx context.Context, q checkedQueue, ws ids.WorkspaceID, owed time.Time, fields dlog.Context) {
	m, err := o.deps.DB.MediationOf(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the check its turn ended on; the queue stays held", withField(fields, "error", err.Error()))
		return
	}
	if m == nil || !m.OwedAt.Equal(owed) {
		o.deps.Log.Global().Info(opMediation, "the intake's turn ended with its check decided or replaced by a newer deploy", fields)
		return
	}
	if m.Dispatched {
		o.deps.Log.Global().Info(opMediation, "the intake's turn ended with a fix dispatched; the queue stays held for the remediation", withField(fields, "remediation", string(m.Remediation)))
		return
	}
	// AN INTAKE CANNOT END ITS TURN UNDECIDED (owner ruling, 2026-11-16):
	// its first undecided end resumes it once to decide the deploy, the
	// queue still held; only a second is let through, as the intake's fault.
	if !m.Resumed {
		o.resumeIntake(ctx, q, ws, *m, fields)
		return
	}
	// THE WINDOW IS READ BEFORE THE RELEASE, which clears it.
	window, err := o.toldWindow(ctx, q.key, fields)
	if err != nil {
		o.mediationFailed(ctx, fields, "could not read the window of a check its turn ended on; the queue stays held", err)
		return
	}
	released, err := o.deps.DB.ReleaseMediation(ctx, q.key, owed)
	if err != nil {
		o.mediationFailed(ctx, fields, "could not settle a check its turn ended on; the queue stays held", err)
		return
	}
	if !released {
		o.deps.Log.Global().Info(opMediation, "the intake's turn ended with its check decided or replaced by a newer deploy", fields)
		return
	}
	o.endChain(ctx, q.key, *m, MediationUndecided)
	o.deps.Log.Global().Error(opMediation, "the intake's turn ended deciding nothing again after it was resumed to decide; the intake failed, the queue is let through and the user is told", fields)
	o.escalateOwning(ctx, q, *m, "the deploy intake ended its turn without deciding the deploy, again after it was resumed to decide it; the queue moved on", nil, window, fields)
	o.republishMediation(ctx, q.key)
	o.kick(q.key)
}

// BriefDeployIntakeResumed resumes an intake whose turn ended undecided.
const BriefDeployIntakeResumed = "deploy-intake-resumed"

// resumeBrief is the prompt resuming check M's undecided intake.
func (o *orchestrator) resumeBrief(m wsm.Mediation) (string, error) {
	return o.deps.Briefs(BriefDeployIntakeResumed, map[string]string{
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"version":        mediationVersion(m),
	})
}

// resumeIntake resumes intake WS, whose turn on check M of queue Q ended
// deciding nothing, in a turn recorded on M before it is submitted, then
// settles what that turn decided (intakeTurnEnded). A step that fails holds
// the queue and is recorded: the next round reattaches the recorded turn.
func (o *orchestrator) resumeIntake(ctx context.Context, q checkedQueue, ws ids.WorkspaceID, m wsm.Mediation, fields dlog.Context) {
	brief, err := o.resumeBrief(m)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the undecided intake's resuming brief could not be composed; the queue stays held", withField(fields, "error", err.Error()))
		return
	}
	turn := wsm.NewTurnID()
	fields = withField(fields, "turn", string(turn))
	marked, err := o.deps.DB.MarkMediationResumed(ctx, q.key, m.OwedAt, turn)
	if err != nil {
		o.mediationFailed(ctx, fields, "could not record that the undecided intake is resumed; the queue stays held", err)
		return
	}
	if !marked {
		o.deps.Log.Global().Info(opMediation, "the check was decided, dispatched or replaced before its undecided intake was resumed; this round ends", fields)
		return
	}
	if _, err := o.submitCheckPrompt(ctx, ws, turn, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		o.mediationFailed(ctx, fields, "the undecided intake's resuming brief could not be submitted; the queue stays held", err)
		return
	}
	o.deps.Log.Global().Warn(opMediation, "the intake's turn ended deciding nothing about the deploy; it is resumed once to decide it, the queue still held", fields)
	if _, err := o.deps.AwaitTurnEnd(ctx, ws, turn); err != nil {
		o.mediationFailed(ctx, fields, "the resumed intake's turn could not be awaited; the queue stays held until the next round", err)
		return
	}
	o.intakeTurnEnded(ctx, q, ws, m.OwedAt, fields)
}

// mediationVersion is how the brief names M's build.
func mediationVersion(m wsm.Mediation) string {
	if m.Version == "" {
		return "the build its deploy installed"
	}
	return m.Version
}

// DecideMediation implements Orchestrator: the chain's intake WS decides the
// deploy its chain owes a check of. An approval or an escalation releases
// the check, ends the chain and lets the queue advance (an escalation also
// puts the summary before the user); a dispatch hands the problems to a
// remediation workspace and keeps the queue held (dispatch.go).
func (o *orchestrator) DecideMediation(ctx context.Context, ws ids.WorkspaceID, d Decision) error {
	fields := dlog.Context{"workspace": string(ws), "decision": d.Kind.String(), "summary": d.Summary}
	role, err := o.deps.DB.MediationRoleOf(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the role of the workspace a decision came from", withField(fields, "error", err.Error()))
		return err
	}
	if role == nil || !role.Intake {
		o.deps.Log.Global().Warn(opMediation, "refused a decision from a workspace that is the intake of no chain whose check is owed", fields)
		return refuse(ArmNotMediator, ws, "this workspace is the intake of no chain whose deploy owes a check")
	}
	fields["repo"] = string(role.Repo)
	q, ok, err := o.checkedQueueOf(ctx, role.Repo)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the repository whose check the decision is on", withField(fields, "error", err.Error()))
		return err
	}
	if !ok {
		o.deps.Log.Global().Warn(opMediation, "refused a decision on a repository that checks its deploys no more", fields)
		return refuse(ArmNotMediator, ws, "the repository checks its deploys no more")
	}
	m := role.Mediation
	if m.Dispatched {
		o.deps.Log.Global().Warn(opMediation, "refused a decision on a deploy a fix was dispatched for already", withField(fields, "remediation", string(m.Remediation)))
		return refuse(ArmNoMediationOwed, ws, "a fix is dispatched for this deploy already; its deploy owes the next check")
	}
	if d.Kind == MediationDispatch {
		if err := o.dispatch(ctx, q, m, ws, d, fields); err != nil {
			return err
		}
		// THE DISPATCH IS RECORDED BEFORE ANY ESCALATION IS TOLD: a window
		// that cannot be read for the confirmations leaves the dispatch
		// standing, and the record of the failure is toldWindow's.
		if window, err := o.toldWindow(ctx, q.key, fields); err == nil {
			o.tellEscalations(ctx, q, m, d.Summary, window, nil, false, fields)
		}
		return nil
	}
	window, err := o.toldWindow(ctx, q.key, fields)
	if err != nil {
		return err
	}
	// AN APPROVAL LEAVES NO PROBLEM WITHOUT AN OWNER (owner ruling,
	// 2026-11-16): a warning or error nobody owns, old or new, is dispatched
	// or escalated, never approved past, or it recurs with no one to end it.
	if unowned := unownedGroups(window); d.Kind == MediationApprove && len(unowned) > 0 {
		refusal := &RefusalError{Arm: ArmProblemsUnowned, Workspace: ws,
			Reason: fmt.Sprintf("the window holds %d groups no one owns; dispatch or escalate them", len(unowned)),
			Fields: map[string]any{"groups": unowned}}
		logRefusal(o.deps.Log.Global(), opMediation, "refused an approval while the check's window holds groups no one owns", withField(fields, "unowned", unowned), refusal)
		return refusal
	}
	released, err := o.deps.DB.ReleaseMediation(ctx, q.key, m.OwedAt)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not release the check the decision named", withField(fields, "error", err.Error()))
		return err
	}
	// A DECISION RACING A NEWER DEPLOY RELEASES NOTHING: the replacement
	// counted that check's end, and the newer one is still owed.
	if released {
		outcome := MediationApproved
		if d.Kind == MediationEscalate {
			outcome = MediationEscalated
		}
		o.endChain(ctx, q.key, m, outcome)
	}
	o.deps.Log.Global().Info(opMediation, "the intake decided the deploy; the queue advances", withField(fields, "released", released))
	switch {
	case d.Kind == MediationEscalate:
		o.escalateOwning(ctx, q, m, d.Summary, d.Problems, window, fields)
	case released:
		o.tellEscalations(ctx, q, m, d.Summary, window, nil, false, fields)
	}
	o.republishMediation(ctx, q.key)
	o.kick(q.key)
	return nil
}

// CheckStage is where an owed deploy check stands.
type CheckStage int

const (
	// CheckRollingOut: the deploy still rolls out; its intake is not told.
	CheckRollingOut CheckStage = iota + 1
	// CheckAssessing: the chain's intake is checking the deploy.
	CheckAssessing
	// CheckFixing: a remediation workspace fixes what the intake dispatched.
	CheckFixing
)

// OwedMediation is the deploy check a queue waits on.
type OwedMediation struct {
	// Repository is the registered repository whose queue it holds.
	Repository ids.RepoID
	// Stage is where the check stands.
	Stage CheckStage
	// Intake is the chain's intake, empty while a chain's first deploy rolls
	// out; Remediation the workspace fixing, set while Stage is CheckFixing.
	Intake      ids.WorkspaceID
	Remediation ids.WorkspaceID
	// InARow is how many remediations the chain deployed so far.
	InARow int
}

// owedMediation is the deploy check REPO's queue waits on, nil while it owes
// none or its repository checks its deploys no more.
func (o *orchestrator) owedMediation(ctx context.Context, repo wsm.RepoKey) (*OwedMediation, error) {
	m, err := o.deps.DB.MediationOf(ctx, repo)
	if err != nil || m == nil {
		return nil, err
	}
	q, ok, err := o.checkedQueueOf(ctx, repo)
	if err != nil || !ok {
		// OWED IN A REPOSITORY THAT CHECKS NO DEPLOYS NOW: nobody decides
		// it, and the next sweep (releaseUnchecked) releases it.
		return nil, err
	}
	return owedFrom(q.repository.ID, *m)
}

// owedFrom is check M of repository REPO as the queue's status reads it. A
// told check names its intake: one recorded nowhere is a broken store.
func owedFrom(repo ids.RepoID, m wsm.Mediation) (*OwedMediation, error) {
	out := &OwedMediation{Repository: repo, Intake: m.Intake, InARow: m.ChainRedeploys}
	switch {
	case m.Dispatched:
		out.Stage, out.Remediation = CheckFixing, m.Remediation
	case m.Prompted():
		out.Stage = CheckAssessing
	default:
		out.Stage = CheckRollingOut
	}
	if (out.Stage != CheckRollingOut && m.Intake == "") || (out.Stage == CheckFixing && m.Remediation == "") {
		return nil, fmt.Errorf("merge: the check of %s owed at %v is %d with no workspace at work (%+v)", repo, m.OwedAt, out.Stage, m)
	}
	return out, nil
}

// rosterMediation is the deploy check REPO's queue waits on as the roster
// draws it, beside the repository's name: nil while it owes none.
func (o *orchestrator) rosterMediation(ctx context.Context, repo wsm.RepoKey) (*sidebar.QueueMediation, error) {
	// A DIRECT FINISH UNDER WAY IS DRAWN FIRST, owed check or none: the
	// workspace that finished stands by hidden, and the label is where it is
	// shown (an escalation's finish runs after its chain ended).
	finishing, err := o.finishingIn(ctx, repo)
	if err != nil {
		return nil, err
	}
	if finishing != "" {
		q, ok, err := o.checkedQueueOf(ctx, repo)
		if err != nil || !ok {
			return nil, err
		}
		return &sidebar.QueueMediation{Repo: q.repository.ID, Stage: sidebar.MediationFinishing, Remediation: finishing}, nil
	}
	owed, err := o.owedMediation(ctx, repo)
	if err != nil || owed == nil {
		return nil, err
	}
	out := &sidebar.QueueMediation{Repo: owed.Repository, Intake: owed.Intake, Remediation: owed.Remediation}
	switch owed.Stage {
	case CheckRollingOut:
		out.Stage = sidebar.MediationWaiting
	case CheckAssessing:
		out.Stage = sidebar.MediationChecking
	case CheckFixing:
		out.Stage = sidebar.MediationFixing
	}
	return out, nil
}

// republishMediation tells the roster and the queue's subscribers a change
// of REPO's check; a republication that fails is recorded.
func (o *orchestrator) republishMediation(ctx context.Context, repo wsm.RepoKey) {
	if err := o.republishQueue(ctx, repo); err != nil {
		o.deps.Log.Global().Error(opMediation, "could not republish the queue whose check changed", dlog.Context{"repo": string(repo), "error": err.Error()})
	}
}

// toldWindow is the window the intake of REPO's owed check was told, empty
// for one a previous build told, which recorded none.
func (o *orchestrator) toldWindow(ctx context.Context, repo wsm.RepoKey, fields dlog.Context) (wsm.CheckWindow, error) {
	window, found, err := o.deps.DB.MediationWindowOf(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the window the intake was told", withField(fields, "error", err.Error()))
		return wsm.CheckWindow{}, err
	}
	if !found {
		o.deps.Log.Global().Info(opMediation, "the check was told by a previous build, which recorded no window; its decision is judged against none", fields)
	}
	return window, nil
}

// unownedGroups names every group of WINDOW no one owns, as
// "OPERATION: MESSAGE (CLASS)", in its order.
func unownedGroups(window wsm.CheckWindow) []string {
	var out []string
	for _, g := range window.Groups {
		if g.Class != wsm.WindowOwned {
			out = append(out, fmt.Sprintf("%s: %s (%s)", g.Operation, g.Message, g.Class))
		}
	}
	return out
}

// escalatedKeys is every group of WINDOW an escalation takes over: each no
// one owns.
func escalatedKeys(window wsm.CheckWindow) []wsm.ProblemKey {
	var out []wsm.ProblemKey
	for _, g := range window.Groups {
		if g.Class != wsm.WindowOwned {
			out = append(out, wsm.ProblemKey{Operation: g.Operation, Message: g.Message})
		}
	}
	return out
}

// problemKeys is the group each of PROBLEMS quotes a record of: a record's
// operation and message, read from its JSON line. Evidence that is no log
// record (a test reuse line) names no group.
func problemKeys(problems []DeployProblem) []wsm.ProblemKey {
	seen := map[wsm.ProblemKey]bool{}
	var out []wsm.ProblemKey
	for _, p := range problems {
		for _, line := range p.Evidence {
			var rec struct {
				Operation string `json:"operation"`
				Message   string `json:"message"`
			}
			if json.Unmarshal([]byte(strings.TrimSpace(line)), &rec) != nil || rec.Operation == "" {
				continue
			}
			k := wsm.ProblemKey{Operation: rec.Operation, Message: rec.Message}
			if !seen[k] {
				seen[k] = true
				out = append(out, k)
			}
		}
	}
	return out
}

// ownProblems makes OWNER the owner of every group PROBLEMS quote and every
// group in ALSO, in REPO, with the problems' statements as its findings, or
// UNSTATED when the problems state none. A write that fails is recorded at
// ERROR: the decision stands, and the groups come back unowned to the next
// check.
func (o *orchestrator) ownProblems(ctx context.Context, repo wsm.RepoKey, owner ids.WorkspaceID, problems []DeployProblem, also []wsm.ProblemKey, unstated string, fields dlog.Context) {
	keys := problemKeys(problems)
	seen := map[wsm.ProblemKey]bool{}
	for _, k := range keys {
		seen[k] = true
	}
	for _, k := range also {
		if !seen[k] {
			seen[k] = true
			keys = append(keys, k)
		}
	}
	if len(keys) == 0 {
		return
	}
	statements := make([]string, 0, len(problems))
	for _, p := range problems {
		statements = append(statements, p.Statement)
	}
	findings := strings.Join(statements, " / ")
	if findings == "" {
		findings = unstated
	}
	if err := o.deps.DB.OwnDeployProblems(ctx, repo, keys, owner, findings, o.deps.Now()); err != nil {
		o.deps.Log.Global().Error(opMediation, "the decided problems could not be recorded as owned; the next check finds them unowned", withField(withField(fields, "owner", string(owner)), "error", err.Error()))
		return
	}
	o.deps.Log.Global().Info(opMediation, "recorded the decided problems as owned", withField(withField(fields, "owner", string(owner)), "problems", len(keys)))
}

// escalateOwning puts check M before the user: each problem PROBLEMS or
// WINDOW hold that an open escalation owns goes back to that escalation
// (tellEscalations), and the rest to a new escalation workspace (escalate),
// which then owns them. An escalation that cannot be made records its own
// failure, and its groups stay unowned for the next check.
func (o *orchestrator) escalateOwning(ctx context.Context, q checkedQueue, m wsm.Mediation, summary string, problems []DeployProblem, window wsm.CheckWindow, fields dlog.Context) {
	routes, ok := o.tellEscalations(ctx, q, m, summary, window, problems, true, fields)
	if !ok {
		return
	}
	if len(routes.order) > 0 && len(routes.fresh) == 0 && len(routes.freshKeys) == 0 {
		o.deps.Log.Global().Info(opMediation, "every escalated problem went to the open escalation that owns it; no new escalation is made", withField(fields, "escalations", len(routes.order)))
		return
	}
	made, err := o.escalate(ctx, q, m, summary, routes.fresh, window, fields)
	if err != nil {
		return
	}
	o.ownProblems(ctx, q.key, made, routes.fresh, routes.freshKeys, "taken over unowned by the escalation", fields)
}

// finishingIn answers the workspace whose direct finish is under way in
// REPO's queue: gating, landing or deploying, its workspace standing by.
// Empty when none is.
func (o *orchestrator) finishingIn(ctx context.Context, repo wsm.RepoKey) (ids.WorkspaceID, error) {
	finishes, err := o.deps.DB.RemediationFinishes(ctx)
	if err != nil {
		return "", err
	}
	for _, f := range finishes {
		if !sameQueue(f.Repo, repo) {
			continue
		}
		switch f.Step {
		case wsm.RemediationGating, wsm.RemediationLanding, wsm.RemediationDeploying:
			return f.Workspace, nil
		}
	}
	return "", nil
}
