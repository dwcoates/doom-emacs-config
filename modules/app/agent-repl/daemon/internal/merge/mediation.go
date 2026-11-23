package merge

import (
	repoconfigv1 "agentrepl/proto/repoconfig/v1"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"agentrepl/logging/logsite"
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
// row form a CHAIN, which has one deploy mediator, told of every deploy of
// the chain in turn (Mediate). Within that turn the mediator approves the
// deploy, escalates it to the user, who holds the queue until the mediator
// approves it or the user releases it, or DISPATCHES the problems the deploy
// caused to itself (dispatch.go). A fix, dispatched or not, lands and deploys
// through the mediator's finish (remediationfinish.go), which holds the queue
// itself from its first finish until its deploy owes the chain's next check.
//
// THE QUEUE IS HELD FROM A DEPLOY'S START UNTIL THE MEDIATOR APPROVES A
// DEPLOY (owner ruling, 2026-11-22; deployHold). An approval while the
// mediator's finish stands releases the check but not the queue: the finish
// holds it through its deploy, whose own check the mediator then decides.
// Only agent-repl's release of a check whose window holds no warning and no
// error (releaseQuiet), a deploy that changed nothing, and the user's
// release of an escalation release a check without the mediator's word, and
// none of them releases a queue a finish holds.

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
	// ArmNoEscalationHeld refuses a release of an escalation's hold when no
	// escalation holds the queue.
	ArmNoEscalationHeld = "no_escalation_held"
	// ArmRemediationBoundReached is DEPRECATED and never answered
	// (2026-11-20): it refused a dispatch once the chain deployed as many
	// remediations in a row as its repository allowed. No dispatch is refused
	// for what its deploy holds now (dispatch.go). It stays named because the
	// protobuf still declares its arm.
	ArmRemediationBoundReached = "remediation_bound_reached"
	// ArmProblemsUnowned is DEPRECATED and never answered (2026-11-19): it
	// refused an approval while the check's window held a group no one owned.
	// The mediator alone decides an approval now. It stays named because the
	// protobuf still declares its arm.
	ArmProblemsUnowned = "problems_unowned"
	// ArmProblemsRecurring is DEPRECATED and never answered (2026-11-20): it
	// refused a dispatch while the check's window held a group that came back
	// after more than five of its fixes deployed. It stays named because the
	// protobuf still declares its arm.
	ArmProblemsRecurring = "problems_recurring"
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
	// evidence.
	Problems []DeployProblem
}

// The spans of a deploy check a Meter records: how long its deploy rolled out
// before its intake was told (the sidebar's "HOT-LOADING ZETTEL"), how long the
// intake then assessed it ("ASSESSING ZETTEL HEALTH"), and both together.
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
	// MediationQuiet: no system logged a warning or an error in the check's
	// window, so the mediator was not run.
	MediationQuiet = "quiet"
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
	// config is the repository's lifecycle config as the queue was listed.
	config *repoconfigv1.LifecycleConfig
}

// checkedQueues answers every registered repository whose lifecycle config
// checks its deploys. A repository with no lifecycle config checks none; one
// whose config cannot be read is recorded under OP and passed over.
func (o *orchestrator) checkedQueues(ctx context.Context, op string) ([]checkedQueue, error) {
	return o.queuesWhere(ctx, op, func(cfg *repoconfigv1.LifecycleConfig) bool { return cfg.GetDeployChecks() != nil })
}

// queuesWhere answers every registered repository whose lifecycle config
// CHECKS says yes to, with the config as it was read. A repository with no
// lifecycle config is passed over; one whose config cannot be read is
// recorded under OP and passed over.
func (o *orchestrator) queuesWhere(ctx context.Context, op string, checks func(*repoconfigv1.LifecycleConfig) bool) ([]checkedQueue, error) {
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
		if checks(cfg) {
			out = append(out, checkedQueue{registeredQueue: q, config: cfg})
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
	q, found := queueKeyed(queues, repo)
	return q, found, nil
}

// queueKeyed answers the queue of QUEUES keyed REPO, by sameQueue, and
// whether one is. Every lookup of a listed queue by its key goes through it.
func queueKeyed(queues []checkedQueue, repo wsm.RepoKey) (checkedQueue, bool) {
	for _, q := range queues {
		if sameQueue(q.key, repo) {
			return q, true
		}
	}
	return checkedQueue{}, false
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
				"repo": string(q.key), "outcome": outcome,
				"dispatch_carried": replaced.DispatchCarried, "replaced_deploy_started": replaced.OwedAt.UTC().Format(time.RFC3339Nano)})
		}
		o.deps.Log.Global().Info(opMediation, "the deploy owes a check; the queue's head lands nothing until the deploy mediator decides", dlog.Context{
			"repo": string(q.key), "deploy_started": at.UTC().Format(time.RFC3339Nano)})
		// A REMEDIATION DEPLOYING IS THIS DEPLOY'S, so its finish is done.
		if err := o.remediationDeployed(ctx, q.key, at); err != nil {
			o.deps.Log.Global().Error(opMediation, "could not record that the deploy is the remediation's; the next daemon's pass records it", dlog.Context{"repo": string(q.key), "error": err.Error()})
		}
		o.republishMediation(ctx, q.key)
	}
	// THE PRODUCTION SPAN THE LANDING CLOSED IS OWED BESIDE ITS DEPLOY CHECK:
	// the deploy check reads the landing from its start, the production
	// check the span that ended there (owner ruling, 2026-11-23).
	if err := o.oweProductionChecks(ctx, at); err != nil {
		errs = append(errs, err)
	}
	return errors.Join(errs...)
}

// NoteDeployEnding records ENDING on every check the deploy that began at AT
// owes, so the intake told of it states whether it finished or stopped short
// (owner ruling, 2026-11-17: a failed build or rollout is checked as any
// deploy is). A check a newer deploy replaced takes nothing. Each failure is
// recorded at ERROR and answered; the check is told without its ending.
func (o *orchestrator) NoteDeployEnding(ctx context.Context, at time.Time, ending wsm.DeployEnding) error {
	life, leave, entered := o.enterMediation()
	if !entered {
		o.deps.Log.Global().Info(opMediation, mediationNotRun, dlog.Context{"deploy_started": at.UTC().Format(time.RFC3339Nano)})
		return nil
	}
	defer leave()
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	defer context.AfterFunc(life, cancel)()
	queues, err := o.checkedQueues(ctx, opMediation)
	if err != nil {
		o.mediationFailed(ctx, nil, logsite.Msg("could not list the repositories a deploy's ending is recorded in; its checks are told without it"), err)
		return err
	}
	var errs []error
	for _, q := range queues {
		fields := dlog.Context{"repo": string(q.key), "deploy_started": at.UTC().Format(time.RFC3339Nano), "ending": string(ending)}
		recorded, err := o.deps.DB.RecordMediationEnding(ctx, q.key, at, ending)
		if err != nil {
			errs = append(errs, err)
			o.deps.Log.Global().Error(opMediation, "could not record how the deploy ended on its check; the intake is told without it", withField(fields, "error", err.Error()))
			continue
		}
		if recorded {
			o.deps.Log.Global().Info(opMediation, "recorded how the deploy ended on its check", fields)
		}
	}
	if err := o.noteProductionEnding(ctx, at, ending); err != nil {
		errs = append(errs, err)
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
	// THE RELEASE RUNS INSIDE THE MEDIATION'S LIFETIME, as a pass does: the
	// daemon's exit cuts it and waits for it before the state client
	// closes, and a cut is recorded as one, never as a failure.
	life, leave, entered := o.enterMediation()
	if !entered {
		o.deps.Log.Global().Info(opMediation, mediationNotRun, dlog.Context{"deploy_started": at.UTC().Format(time.RFC3339Nano)})
		return nil
	}
	defer leave()
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	defer context.AfterFunc(life, cancel)()
	queues, err := o.checkedQueues(ctx, opMediation)
	if err != nil {
		o.mediationFailed(ctx, nil, logsite.Msg("could not list the repositories an unchanged deploy owes checks in; its checks stay owed"), err)
		return err
	}
	var errs []error
	for _, q := range queues {
		fields := dlog.Context{"repo": string(q.key), "deploy_started": at.UTC().Format(time.RFC3339Nano)}
		m, err := o.deps.DB.MediationOf(ctx, q.key)
		if err != nil {
			errs = append(errs, err)
			if o.mediationFailed(ctx, fields, logsite.Msg("could not read the check an unchanged deploy owes; it stays owed"), err) {
				break
			}
			continue
		}
		if !owes(m, at) {
			continue
		}
		if !m.ChainStartedAt.Equal(m.OwedAt) {
			o.deps.Log.Global().Info(opMediation, "the deploy changed nothing, but its check carries an earlier chain; the chain's intake checks it", withField(fields, "chain_started", m.ChainStartedAt.UTC().Format(time.RFC3339Nano)))
			continue
		}
		released, err := o.releaseCheck(ctx, q.key, *m, MediationUnchanged)
		if err != nil {
			errs = append(errs, err)
			if o.mediationFailed(ctx, fields, logsite.Msg("could not release the check of a deploy that changed nothing; it stays owed"), err) {
				break
			}
			continue
		}
		if !released {
			continue
		}
		o.releasedCheck(ctx, q.key, checkRelease{
			unread:   logsite.Msg("the deploy changed nothing; its check is released unchecked; what holds the queue after it could not be read, and the queue is kicked to read it again"),
			held:     logsite.Msg("the deploy changed nothing; its check is released unchecked; the queue stays held for {holder}"),
			advances: logsite.Msg("the deploy changed nothing; its check is released unchecked and the queue advances"),
		}, fields)
	}
	return errors.Join(errs...)
}

// Mediate implements Orchestrator: runs a round for every check owed and not
// yet awaited here whose deploy mediator this daemon serves, and takes up
// every remediation finish a restart cut whose workspace it serves
// (remediationfinish.go). A daemon that may not advance its queues runs
// none; its successor's takeover calls this again. A round or finish passed
// over for its mediator is run by the edge on which this daemon comes to
// serve it (mediateAdopted, ServingWidened).
func (o *orchestrator) Mediate(ctx context.Context) {
	notRun := logsite.Msg(mediationNotRun)
	if !o.deps.Standing.ServesQueue() {
		o.deps.Log.Global().Info(opMediation, "this daemon does not serve the queues; their checks are run by the daemon that does", nil)
		return
	}
	// THE PASS RUNS INSIDE THE MEDIATION'S LIFETIME (mediationlife.go), so
	// the daemon's exit cuts it and waits for it before the state client
	// closes; it also ends with the caller's context.
	life, leave, entered := o.enterMediation()
	if !entered {
		o.deps.Log.Global().Log(dlog.LevelInfo, opMediation, notRun, nil)
		return
	}
	defer leave()
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	defer context.AfterFunc(life, cancel)()
	queues, err := o.checkedQueues(ctx, opMediation)
	if err != nil {
		o.mediationFailed(ctx, nil, logsite.Msg("could not list the repositories whose checks are owed"), err)
		return
	}
	o.releaseUnchecked(ctx, queues)
	o.resumeRemediationFinishes(ctx)
	// THE PRODUCTION CHECKS RUN BESIDE THE DEPLOY CHECKS and hold nothing, so
	// a deploy check that ends this pass early never stops them.
	defer o.productionPass(ctx)
	for _, q := range queues {
		m, err := o.deps.DB.MediationOf(ctx, q.key)
		if err != nil {
			if o.mediationFailed(ctx, dlog.Context{"repo": string(q.key)}, logsite.Msg("could not read a repository's owed check"), err) {
				return
			}
			continue
		}
		// THE ROUND RUNS ONLY WHERE ITS MEDIATOR IS SERVED (standingchecks.go),
		// asked before anything touches it, by the predicate the workspace
		// verbs refuse on. NO ROUND IS LOST TO IT: a daemon comes to serve a
		// workspace only by a transition to owned that is made before the
		// edge telling the merge queue of it (rollout's adopt, the
		// stragglers' adoption and joining mode's end at the takeover, the
		// take-back of a transfer), and that edge runs this pass again
		// (mediateAdopted, ServingWidened). So a gate that reads the mediator
		// not served reads it before that transition, and the pass its edge
		// runs after it finds the mediator served.
		fields := dlog.Context{"repo": string(q.key)}
		mediator, standing, err := o.mediatorStanding(ctx, deployRole, q.key)
		if err != nil {
			if o.mediationFailed(ctx, fields, logsite.Msg("could not read whether this daemon serves the queue's deploy mediator; its check's round is not run, and the next pass runs it"), err) {
				return
			}
			continue
		}
		served := standing == ServedHere
		if served {
			o.keepMediatorShown(ctx, deployRole, q)
		}
		o.surfaceFinish(ctx, q.key)
		if m == nil {
			if !served {
				o.deps.Log.Global().Debug(opMediation, "this daemon does not serve the queue's deploy mediator now; the daemon that serves it keeps it shown", withField(withField(fields, "mediator", string(mediator)), "standing", standing.String()))
			}
			continue
		}
		if !served {
			o.mediatorNotServed(withField(withField(fields, "mediator", string(mediator)), "deploy_started", m.OwedAt.UTC().Format(time.RFC3339Nano)), standing)
			continue
		}
		// A ROUND OUTLIVES THE PASS, NOT THE DAEMON: it runs under the
		// mediation's lifetime, which only the daemon's exit cuts.
		round, leaveRound, entered := o.enterMediation()
		if !entered {
			o.deps.Log.Global().Log(dlog.LevelInfo, opMediation, notRun, dlog.Context{"repo": string(q.key)})
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
		o.mediationFailed(ctx, nil, logsite.Msg("could not list the repositories to release the checks no intake decides"), err)
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
		if err := o.endUncheckedDeploys(ctx, q.key); err != nil {
			if o.mediationFailed(ctx, dlog.Context{"repo": string(q.key)}, logsite.Msg("could not end the remediation finishes deploying in a repository that checks no deploys; they hold its queue until the next pass"), err) {
				return
			}
			continue
		}
		m, err := o.deps.DB.MediationOf(ctx, q.key)
		if err != nil {
			if o.mediationFailed(ctx, dlog.Context{"repo": string(q.key)}, logsite.Msg("could not read whether a repository that checks no deploys owes a check"), err) {
				return
			}
			continue
		}
		if m == nil {
			continue
		}
		released, err := o.releaseCheck(ctx, q.key, *m, MediationUnmediated)
		if err != nil {
			if o.mediationFailed(ctx, dlog.Context{"repo": string(q.key)}, logsite.Msg("could not release a check no intake decides; the queue stays held"), err) {
				return
			}
			continue
		}
		if released {
			o.releasedCheck(ctx, q.key, checkRelease{
				unread:   logsite.Msg("released a check owed by a repository whose lifecycle config checks no deploys now; what holds the queue after it could not be read, and the queue is kicked to read it again"),
				held:     logsite.Msg("released a check owed by a repository whose lifecycle config checks no deploys now; the queue stays held for {holder}"),
				advances: logsite.Msg("released a check owed by a repository whose lifecycle config checks no deploys now; its queue advances"),
			}, dlog.Context{
				"repo": string(q.key), "deploy_started": m.OwedAt.UTC().Format(time.RFC3339Nano)})
		}
	}
}

// endUncheckedDeploys ends every remediation finish of REPO, a repository
// that checks its deploys no more, whose deploy is starting: that deploy owes
// no check in REPO (OweMediations), so nothing would take the hold on from
// the finish, which would hold the queue for nobody. A queue a finish was
// ended in is republished and kicked.
func (o *orchestrator) endUncheckedDeploys(ctx context.Context, repo wsm.RepoKey) error {
	finishes, err := o.queueFinishesAt(ctx, repo, wsm.RemediationDeploying)
	if err != nil {
		return err
	}
	ended := false
	var errs []error
	for _, f := range finishes {
		did, err := o.deps.DB.EndRemediationFinish(ctx, f.Workspace)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if did {
			ended = true
			o.deps.Log.Global().Info(opRemediationFinish, "the remediation's deploy owes no check in a repository that checks no deploys; its finish is ended and holds the queue no more", dlog.Context{"workspace": string(f.Workspace), "repo": string(repo)})
		}
	}
	if ended {
		o.republishMediation(ctx, repo)
		o.kick(repo)
	}
	return errors.Join(errs...)
}

// owes reports whether M, a repository's owed check or nil for none, is the
// check of the deploy that began at OWED: none owed, or a newer deploy's, is
// not.
func owes(m *wsm.Mediation, owed time.Time) bool {
	return m != nil && m.OwedAt.Equal(owed)
}

// releaseCheck releases check M of queue REPO, still the one owed, and ends
// its chain as OUTCOME when the release took: a released check is always its
// chain's last. It answers whether it released it; one a newer deploy
// replaced, or a decision released already, is not.
func (o *orchestrator) releaseCheck(ctx context.Context, repo wsm.RepoKey, m wsm.Mediation, outcome string) (bool, error) {
	released, err := o.deps.DB.ReleaseMediation(ctx, repo, m.OwedAt)
	if err != nil || !released {
		return false, err
	}
	o.endChain(ctx, repo, m, outcome)
	// A CHECK RELEASED WITH NOTHING TO FIX IS QUIET: approved, or a deploy
	// that changed nothing. It compacts the mediator only past
	// QuietCheckCompactionTokens (noteQuietCheck).
	if outcome == MediationApproved || outcome == MediationUnchanged || outcome == MediationQuiet {
		o.noteQuietCheck(ctx, repo, dlog.Context{"repo": string(repo), "outcome": outcome})
	}
	return true, nil
}

// endChain records how check M of queue REPO, the last of its chain, ended
// as OUTCOME and forgets what the chain's remediations landed: the next
// deploy begins another chain, which the same mediator takes in the session
// it already runs.
func (o *orchestrator) endChain(ctx context.Context, repo wsm.RepoKey, m wsm.Mediation, outcome string) {
	o.observeMediationEnd(m, outcome, o.deps.Now())
	fields := dlog.Context{"repo": string(repo), "outcome": outcome, "chain_started": m.ChainStartedAt.UTC().Format(time.RFC3339Nano)}
	if _, err := o.deps.DB.EndRemediationFinishesOf(ctx, repo); err != nil {
		o.deps.Log.Global().Error(opMediation, "the chain ended but what its remediations landed could not be forgotten", withField(fields, "error", err.Error()))
	}
	o.deps.Log.Global().Info(opMediation, "the chain ended; the mediator's session runs on for the next", fields)
}

// checkClaim is one check's round: the queue and the deploy it checks. A
// claim is the check's, never the queue's, so a round still awaiting the
// turn of a check a newer deploy replaced never keeps the newer check's
// round from running (2026-11-14: a replaced round's claim on the queue
// left a deploy's check untold for twenty minutes).
type checkClaim struct {
	role wsm.MediatorRole
	repo wsm.RepoKey
	owed int64
}

// claimMediation reports whether this caller runs the round of the check
// REPO owes at OWED: none runs for it already.
func (o *orchestrator) claimMediation(repo wsm.RepoKey, owed time.Time) bool {
	return o.claimRound(deployRole, repo, owed)
}

// claimRound reports whether this caller runs role R's round of the check of
// REPO identified by OWED, claiming it when no round runs; releaseRound gives
// the claim up.
func (o *orchestrator) claimRound(r mediatorRole, repo wsm.RepoKey, owed time.Time) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	claim := checkClaim{role: r.role, repo: repo, owed: owed.UnixNano()}
	if o.mediating[claim] {
		return false
	}
	o.mediating[claim] = true
	return true
}

// releaseRound gives up role R's claim on the round of REPO's check OWED.
func (o *orchestrator) releaseRound(r mediatorRole, repo wsm.RepoKey, owed time.Time) {
	o.mu.Lock()
	defer o.mu.Unlock()
	delete(o.mediating, checkClaim{role: r.role, repo: repo, owed: owed.UnixNano()})
}

// intakeRound tells Q's mediator of check M, when it was not told yet, and
// awaits the turn it was told in; then settles a check that turn decided
// nothing about (intakeTurnEnded). A check the mediator dispatched a fix
// for is watched instead (watchFixer): its fix's deploy owes the next.
func (o *orchestrator) intakeRound(ctx context.Context, q checkedQueue, m wsm.Mediation) {
	defer o.releaseRound(deployRole, q.key, m.OwedAt)
	fields := dlog.Context{"repo": string(q.key), "deploy_started": m.OwedAt.UTC().Format(time.RFC3339Nano),
		"chain_started": m.ChainStartedAt.UTC().Format(time.RFC3339Nano), "chain_redeploys": m.ChainRedeploys}
	// EVERY ROUND STATES THE CHAIN IT CHECKS, so a daemon that booted on a
	// remediation's deploy serves the count the chain carries.
	o.deps.Meter.SetRemediationsInARow(m.ChainRedeploys)
	if m.Dispatched {
		o.deps.Log.Global().Info(opMediation, "a fix is dispatched for this deploy; the mediator is at work on it, and its deploy owes the next check", fields)
		o.watchFixer(ctx, q, m, fields)
		return
	}
	if m.Escalated() {
		if !m.EscalationTold {
			o.remakeHeldEscalation(ctx, q, m, fields)
			return
		}
		o.deps.Log.Global().Info(opMediation, "this deploy's check is escalated; the queue waits for the user", fields)
		return
	}
	brief, window, err := o.intakeBrief(ctx, q, m)
	if err != nil {
		o.roundFailed(ctx, q, m, "the deploy check's brief could not be composed", err, fields)
		return
	}
	// A WINDOW WITH NO WARNING AND NO ERROR NEEDS NO MEDIATOR (owner ruling,
	// 2026-11-19): there is nothing for it to investigate, so the check is
	// released here and no session is started. A check the mediator was told
	// of already is its own to decide, whatever the window holds now.
	if !m.Prompted() && len(window.Groups) == 0 {
		o.releaseQuiet(ctx, q, m, window, fields)
		return
	}
	ws, err := o.mediatorFor(ctx, q, m, fields)
	if err != nil {
		o.roundFailed(ctx, q, m, "the deploy mediator could not be found, made or readied for the check", err, fields)
		return
	}
	fields["mediator"] = string(ws)
	var closed wsm.TurnClose
	if !m.Prompted() {
		// THE AWAIT'S ERROR IS THE ROUND'S err, never one scoped to this
		// branch: a shadowed err dropped a failed await, and the round then
		// settled the check as decided nothing, letting the queue through.
		var turn ids.TurnID
		turn, err = o.tellIntake(ctx, q, ws, &m, brief, window, fields)
		if err != nil || turn == "" {
			return
		}
		closed, err = o.deps.AwaitTurnEnd(ctx, ws, turn)
	} else {
		// THE TURN WAS RECORDED BEFORE ITS BRIEF WAS QUEUED, so a round cut
		// between the two (a refused submission, the daemon's exit) left a
		// turn that never reached the session: reattaching submits it then,
		// and awaits one that did.
		// A RESUMED INTAKE'S TURN IS ITS RESUMING BRIEF'S.
		told := brief
		if m.Resumed {
			if told, err = o.resumeBrief(m); err != nil {
				o.roundFailed(ctx, q, m, "the undecided mediator's resuming brief could not be composed", err, fields)
				return
			}
		}
		o.deps.Log.Global().Info(opMediation, "the intake was told of this deploy before; its turn is reattached", withField(withField(fields, "turn", string(m.Turn)), "resumed", m.Resumed))
		closed, err = o.reattachTurn(ctx, ws, fields, m.Turn, told, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION)
	}
	if err != nil {
		o.awaitFailed(ctx, q, m, fields, "the deploy mediator's turn on the check could not be awaited", err)
		return
	}
	o.intakeTurnEnded(ctx, q, ws, m, closed, fields)
	// A CHECK THE INTAKE DISPATCHED IS WATCHED BY THIS SAME ROUND: the claim
	// it holds kept every pass the dispatch ran from starting another.
	if now, err := o.deps.DB.MediationOf(ctx, q.key); err != nil {
		o.mediationFailed(ctx, fields, logsite.Msg("could not read the check after its intake's turn ended; the next pass waits on its fixer"), err)
	} else if owes(now, m.OwedAt) && now.Dispatched {
		o.watchFixer(ctx, q, *now, fields)
	}
}

// roundFailed settles a round of check M of queue Q that failed short of
// telling the mediator what it owes, WHAT failing with ERR: the check is
// escalated to the user, naming the cause, and the mediator is told the
// check could not run, WHAT and ERR verbatim, so it can diagnose and fix it
// (owner rulings, 2026-11-19 and 2026-11-21). It is THE ONE PATH every such
// failure takes, so after any round the check is told, escalated, or its
// round was cut.
//
// NOTHING RUNS SUCH A ROUND AGAIN on its own: Mediate runs only at a boot, at
// a deploy's end and from the fixer watch. A check left owed and only logged
// held its queue under the rollout's label until the next deploy (2026-11-19:
// the brief read a table its build's migration had not yet created, held
// forty minutes; 2026-11-21 12:56:28: the mediator's effort met a shim the
// rollout had stood down, held fifty-three minutes, the mediator told
// nothing and its approval refused).
//
// A ROUND THE DAEMON'S EXIT OR A HANDOVER CUT IS NO FAILURE: the daemon that
// serves the queues next runs it again (mediationCut), so it is not
// escalated. Nor is a workspace verb's refusal on the mediator's serving
// standing, which a handover can bring about after the pass's gate let the
// round run (mediatorNotServed).
//
// THE CHECK IS ESCALATED ONLY TO A RECORDED MEDIATOR: a check held past its
// rollout names its mediator (owedFrom), and the user's notice opens it
// (notifyHeld). A recorded one is enough, whether or not it can be
// reopened or shown now: the user is notified through it, and the next pass
// tells it. With none recorded one is made first; one that cannot be made is
// left owed, recorded at ERROR, and the next pass makes it again.
func (o *orchestrator) roundFailed(ctx context.Context, q checkedQueue, m wsm.Mediation, what string, err error, fields dlog.Context) {
	if o.mediationCut(ctx, fields, what, err) {
		return
	}
	if _, recorded, rerr := o.deps.DB.DeployMediatorOf(ctx, q.key); rerr == nil && recorded {
		o.escalateNotRun(ctx, q, m, what, err, fields)
		return
	}
	if _, merr := o.mediatorOf(ctx, deployRole, q, fields); merr != nil {
		o.deps.Log.Global().With(withField(withField(fields, "error", err.Error()), "mediator_error", merr.Error())).Error(opMediation,
			"{what}; no deploy mediator could be found or made to escalate the check to, and the queue stays held for it", dlog.Context{"what": what})
		return
	}
	o.escalateNotRun(ctx, q, m, what, err, fields)
}

// escalateNotRun records roundFailed's failure at ERROR and escalates check M
// of queue Q as one that could not run, WHAT failing with ERR.
func (o *orchestrator) escalateNotRun(ctx context.Context, q checkedQueue, m wsm.Mediation, what string, err error, fields dlog.Context) {
	o.deps.Log.Global().With(withField(fields, "error", err.Error())).Error(opMediation, "{what}; the check is escalated to the user", dlog.Context{"what": what})
	summary := fmt.Sprintf("the deploy check could not run: %s (%v); the queue waits for you", what, err)
	o.holdBrief(ctx, q, m, escalationBrief{Summary: summary, NotRun: &checkNotRun{What: what, Cause: err.Error(), Told: m.Prompted()}}, wsm.CheckWindow{}, false, fields)
}

// releaseQuiet releases check M of queue Q, whose WINDOW holds no warning and
// no error, without running the mediator, and records where the check read
// to so the next check's window starts there.
func (o *orchestrator) releaseQuiet(ctx context.Context, q checkedQueue, m wsm.Mediation, window wsm.CheckWindow, fields dlog.Context) {
	fields = withField(withField(fields, "window_started", window.Start.UTC().Format(time.RFC3339Nano)), "window_through", window.Through.UTC().Format(time.RFC3339Nano))
	if _, err := o.deps.DB.RecordMediationWindow(ctx, q.key, m.OwedAt, window); err != nil {
		o.roundFailed(ctx, q, m, "the quiet check's window could not be recorded as read", err, fields)
		return
	}
	released, err := o.releaseCheck(ctx, q.key, m, MediationQuiet)
	if err != nil {
		o.roundFailed(ctx, q, m, "the quiet check could not be released", err, fields)
		return
	}
	o.releasedCheck(ctx, q.key, checkRelease{
		unread:   logsite.Msg("no system logged a warning or an error in the check's window; the deploy is released and the mediator is not run; what holds the queue after it could not be read, and the queue is kicked to read it again"),
		held:     logsite.Msg("no system logged a warning or an error in the check's window; the deploy is released and the mediator is not run; the queue stays held for {holder}"),
		advances: logsite.Msg("no system logged a warning or an error in the check's window; the deploy is released and the mediator is not run"),
	}, withField(fields, "released", released))
}

// awaitFailed settles a failed await of the mediator's turn on check M. An
// await cut after the check was decided (released, dispatched or escalated)
// finds the check decided: that is the decision's end, recorded at INFO. Any
// other failure is the round's, WHAT failing (roundFailed): no round awaits
// the turn now, so nothing would settle a turn that ends deciding nothing.
func (o *orchestrator) awaitFailed(ctx context.Context, q checkedQueue, m wsm.Mediation, fields dlog.Context, what string, err error) {
	if now, readErr := o.deps.DB.MediationOf(ctx, q.key); readErr == nil && (!owes(now, m.OwedAt) || now.Dispatched || now.Escalated()) {
		o.deps.Log.Global().Info(opMediation, "the intake's turn could not be awaited after its check was decided; the check is decided", withField(fields, "cause", err.Error()))
		return
	}
	o.roundFailed(ctx, q, m, what, err, fields)
}

// mediatorFor answers the deploy mediator check M is told to: readied
// (readyMediator) before its first telling; as it stands once M was told, since its turn on M is reattached and a refresh would move its
// worktree under that turn.
func (o *orchestrator) mediatorFor(ctx context.Context, q checkedQueue, m wsm.Mediation, fields dlog.Context) (ids.WorkspaceID, error) {
	if m.Prompted() {
		mediator, err := o.mediatorOf(ctx, deployRole, q, fields)
		if err != nil {
			return "", fmt.Errorf("merge: the queue's deploy mediator: %w", err)
		}
		return mediator.ws, nil
	}
	mediator, err := o.readyMediator(ctx, deployRole, q, fields)
	if err != nil {
		return "", fmt.Errorf("merge: the queue's deploy mediator: %w", err)
	}
	if !mediator.briefed {
		o.markBriefed(ctx, deployRole, q, fields)
	}
	o.deps.Log.Global().Info(opMediation, "the deploy mediator takes the check", withField(fields, "mediator", string(mediator.ws)))
	return mediator.ws, nil
}

// tellIntake tells the chain's intake WS of M in BRIEF, answering the brief's
// turn: empty when the check was replaced or released meanwhile. M is updated
// to the check told once its brief is submitted, so a later failure of the
// round says the mediator was told (checkNotRun.Told). The intake's
// context is never compacted: every deploy of its chain stays in it.
func (o *orchestrator) tellIntake(ctx context.Context, q checkedQueue, ws ids.WorkspaceID, told *wsm.Mediation, brief string, window wsm.CheckWindow, fields dlog.Context) (ids.TurnID, error) {
	m := *told
	turn := wsm.NewTurnID()
	at := o.deps.Now()
	marked, err := o.deps.DB.MarkMediationPrompted(ctx, q.key, m.OwedAt, at, turn)
	if err != nil {
		o.roundFailed(ctx, q, m, "could not record that the deploy mediator is told of the deploy", err, fields)
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
		o.roundFailed(ctx, q, m, "could not record the window the deploy mediator is told", err, fields)
		return "", err
	}
	if !recorded {
		o.deps.Log.Global().Info(opMediation, "the check was replaced or released before its window was recorded; this round ends", fields)
		return "", nil
	}
	o.deps.Meter.ObserveMediationSpan(MediationSpanRollout, MediationTold, at.Sub(m.OwedAt))
	if _, err := o.submitCheckPrompt(ctx, ws, turn, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		o.roundFailed(ctx, q, m, "the deploy mediator's brief of the check could not be submitted", err, fields)
		return "", err
	}
	told.PromptedAt, told.Turn = &at, turn
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

// intakeTurnEnded settles check TOLD once the turn the intake was told of
// it in ended: still owed with nothing dispatched is a deploy it
// decided nothing about, let through and put before the user; one replaced
// by a newer deploy is that deploy's round's; one released was decided; one
// dispatched waits on its remediation.
func (o *orchestrator) intakeTurnEnded(ctx context.Context, q checkedQueue, ws ids.WorkspaceID, told wsm.Mediation, closed wsm.TurnClose, fields dlog.Context) {
	m, err := o.deps.DB.MediationOf(ctx, q.key)
	if err != nil {
		o.roundFailed(ctx, q, told, "could not read the check the deploy mediator's turn ended on", err, fields)
		return
	}
	if !owes(m, told.OwedAt) {
		// THE MEDIATOR'S SESSION IS NEVER ENDED BY THE DAEMON (owner ruling,
		// 2026-11-20): it stands in remote control for good, and ending its
		// session severed the user's connection to it after every decision.
		o.deps.Log.Global().Info(opMediation, "the intake's turn ended with its check decided or replaced by a newer deploy; its session runs on", fields)
		return
	}
	// A dispatch is the mediator's own to fix, and an escalation its own to
	// settle with the user.
	if m.Dispatched {
		o.deps.Log.Global().Info(opMediation, "the mediator's turn ended with a fix dispatched; the queue stays held for the fix", fields)
		return
	}
	if m.Escalated() {
		o.deps.Log.Global().Info(opMediation, "the mediator's turn ended with the deploy escalated; the queue stays held for the user", fields)
		return
	}
	// A TURN THE USER INTERRUPTED IS THE USER'S (owner ruling, 2026-11-18):
	// they took the session over, so the daemon sends no resuming brief and
	// escalates nothing into it; the check stays owed, the queue held, and
	// the mediator decides it in the turns the user drives.
	if closed == wsm.CloseKilled {
		o.deps.Log.Global().Info(opMediation, "the user interrupted the intake's turn; the check is left to the user's turns with the mediator, and no brief is sent", fields)
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
		o.roundFailed(ctx, q, *m, "could not read the window of the check the deploy mediator's turn ended on", err, fields)
		return
	}
	o.deps.Log.Global().Error(opMediation, "the intake's turn ended deciding nothing again after it was resumed to decide; the intake failed, and the check is escalated to the user", fields)
	o.holdEscalated(ctx, q, *m, "the deploy mediator ended its turn without deciding the deploy, again after it was resumed to decide it; the queue waits for you", nil, window, false, fields)
}

// errMediationStopped answers a decision made while this daemon stops: the
// check stays owed, and the daemon that serves the queues next tells the
// intake again.
var errMediationStopped = errors.New("merge: this daemon is stopping; the check stays owed for the daemon that serves the queues next")

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
		o.roundFailed(ctx, q, m, "the undecided mediator's resuming brief could not be composed", err, fields)
		return
	}
	turn := wsm.NewTurnID()
	fields = withField(fields, "turn", string(turn))
	marked, err := o.deps.DB.MarkMediationResumed(ctx, q.key, m.OwedAt, turn)
	if err != nil {
		o.roundFailed(ctx, q, m, "could not record that the undecided deploy mediator is resumed", err, fields)
		return
	}
	if !marked {
		o.deps.Log.Global().Info(opMediation, "the check was decided, dispatched or replaced before its undecided intake was resumed; this round ends", fields)
		return
	}
	if _, err := o.submitCheckPrompt(ctx, ws, turn, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		o.roundFailed(ctx, q, m, "the undecided deploy mediator's resuming brief could not be submitted", err, fields)
		return
	}
	o.deps.Log.Global().Warn(opMediation, "the intake's turn ended deciding nothing about the deploy; it is resumed once to decide it, the queue still held", fields)
	closed, err := o.deps.AwaitTurnEnd(ctx, ws, turn)
	if err != nil {
		o.awaitFailed(ctx, q, m, fields, "the resumed deploy mediator's turn could not be awaited", err)
		return
	}
	o.intakeTurnEnded(ctx, q, ws, m, closed, fields)
}

// mediationVersion is how the brief names M's build.
func mediationVersion(m wsm.Mediation) string {
	if m.Version == "" {
		return "the build its deploy installed"
	}
	return m.Version
}

// DecideMediation implements Orchestrator: the deploy mediator WS decides
// the deploy its queue owes a check of, once it was told of it. An approval
// releases the check, ends the chain and lets the queue advance unless the
// mediator's own remediation finish holds it (deployHold); an
// escalation holds the queue for the user; a dispatch keeps the queue held
// while the mediator fixes the problems itself (dispatch.go).
func (o *orchestrator) DecideMediation(ctx context.Context, ws ids.WorkspaceID, d Decision) error {
	fields := dlog.Context{"workspace": string(ws), "decision": d.Kind.String(), "summary": d.Summary}
	// A DECISION RUNS ON THE MEDIATION'S LIFETIME, NEVER ITS REQUEST'S: the
	// request comes from the intake's own session, whose end cancels the
	// request mid-decision (2026-11-16 17:55: an escalation's workspace was
	// made on the cancelled request, failed, and the user was never told).
	life, leave, entered := o.enterMediation()
	if !entered {
		o.deps.Log.Global().Info(opMediation, mediationNotRun, fields)
		return errMediationStopped
	}
	defer leave()
	ctx = life
	repo, isMediator, err := o.deps.DB.MediatorRepoOf(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read whether the workspace a decision came from is a deploy mediator", withField(fields, "error", err.Error()))
		return err
	}
	if !isMediator {
		o.deps.Log.Global().Warn(opMediation, "refused a decision from a workspace that is no repository's deploy mediator", fields)
		return refuse(ArmNotMediator, ws, "this workspace is no repository's deploy mediator")
	}
	fields["repo"] = string(repo)
	q, ok, err := o.checkedQueueOf(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the repository whose check the decision is on", withField(fields, "error", err.Error()))
		return err
	}
	if !ok {
		o.deps.Log.Global().Warn(opMediation, "refused a decision on a repository that checks its deploys no more", fields)
		return refuse(ArmNotMediator, ws, "the repository checks its deploys no more")
	}
	owed, err := o.deps.DB.MediationOf(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the check the decision is on", withField(fields, "error", err.Error()))
		return err
	}
	// AN ESCALATED CHECK WAS TOLD TO THE MEDIATOR BY ITS ESCALATION, whether
	// or not the check's own brief ever reached it: a check whose brief
	// could not be composed is escalated unprompted (roundFailed), and the
	// mediator's approval of it was refused here as a check it was never
	// told of (2026-11-20 17:25Z), holding the queue for the user.
	if owed == nil || (!owed.Prompted() && !owed.Escalated()) {
		o.deps.Log.Global().Warn(opMediation, "refused a decision while the mediator's queue owes no check it was told of", fields)
		return refuse(ArmNoMediationOwed, ws, "no deploy owes a check you were told of")
	}
	m := *owed
	if m.Dispatched {
		o.deps.Log.Global().Warn(opMediation, "refused a decision on a deploy a fix was dispatched for already", fields)
		return refuse(ArmNoMediationOwed, ws, "a fix is dispatched for this deploy already; its deploy owes the next check")
	}
	// AN ESCALATED DEPLOY THE MEDIATOR FINDS HEALTHY IS APPROVED BY THE
	// MEDIATOR (owner ruling, 2026-11-19): the checks are automatic, and a
	// deploy that broke nothing is never left waiting on the user, whoever
	// escalated it (the daemon's own escalation of an undecided turn held a
	// healthy deploy until the user released it by hand). Only an approval
	// ends an escalation here: a fix of one lands through the finish.
	if m.Escalated() && d.Kind != MediationApprove {
		o.deps.Log.Global().Warn(opMediation, "refused a decision on a deploy escalated already", fields)
		return refuse(ArmNoMediationOwed, ws, "this deploy is escalated already; approve it if it broke nothing, or fix it and finish")
	}
	if m.Escalated() {
		o.deps.Log.Global().Info(opMediation, "the mediator approves a deploy that was escalated; its escalation ends with the approval", fields)
	}
	window, err := o.toldWindow(ctx, q.key, fields)
	if err != nil {
		return err
	}
	// THE MEDIATOR ALONE DECIDES A DEPLOY (owner rulings, 2026-11-19 and
	// 2026-11-20): neither an approval nor a dispatch is judged against the
	// window's records.
	if d.Kind == MediationDispatch {
		return o.dispatch(ctx, q, m, ws, d, fields)
	}
	// A GROUP THE MEDIATOR JUDGED IGNORABLE IS NAMED, NEVER SILENTLY PASSED
	// (owner ruling, 2026-11-18): an approval's problems are the records it
	// ignores, such as a one-off user-driven error or a record of the
	// metaprompt's probably-transient table. They are recorded, not judged.
	if d.Kind == MediationApprove && len(d.Problems) > 0 {
		o.deps.Log.Global().Info(opMediation, "the mediator approves past the groups it judged ignorable, naming each",
			withField(fields, "ignored", problemStatements(d.Problems)))
	}
	// AN ESCALATION HOLDS THE QUEUE (owner ruling, 2026-11-16): the check
	// stays owed, escalated, until the mediator's fix deploys or the user
	// releases it.
	if d.Kind == MediationEscalate {
		o.deps.Log.Global().Info(opMediation, "the mediator escalated the deploy; the queue is held for the user", fields)
		o.holdEscalated(ctx, q, m, d.Summary, d.Problems, window, true, fields)
		return nil
	}
	// A CHECK THAT COULD NOT RUN IS READ THROUGH ITS APPROVAL: its window was
	// never recorded, so the next check's window would start where the last
	// recorded one ended and hold everything this one could not read. On
	// 2026-11-20 that was 363 MB of records, and every check after the first
	// that could not read them failed the same way. The mediator looked at
	// the deploy itself, so the window is recorded as read up to now.
	if !m.Prompted() {
		if err := o.recordUnreadWindow(ctx, q, m, fields); err != nil {
			return err
		}
	}
	// A DECISION RACING A NEWER DEPLOY RELEASES NOTHING: the replacement
	// counted that check's end, and the newer one is still owed.
	released, err := o.releaseCheck(ctx, q.key, m, MediationApproved)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not release the check the decision named", withField(fields, "error", err.Error()))
		return err
	}
	if released {
		o.endMediatorRun(q.key, escalatedPhase(m.Escalated(), MediatorPhaseAssessing), MediatorRunApproved)
	}
	o.releasedCheck(ctx, q.key, checkRelease{
		unread:   logsite.Msg("the mediator approved the deploy; what holds the queue after it could not be read, and the queue is kicked to read it again"),
		held:     logsite.Msg("the mediator approved the deploy; the queue stays held for {holder}"),
		advances: logsite.Msg("the mediator approved the deploy; the queue advances"),
	}, withField(fields, "released", released))
	return nil
}

// checkRelease words the records of one release of a check, each its caller's
// whole sentence: what holds the queue after it could not be read (UNREAD),
// the queue still held, whose {holder} names what holds it (HELD), and a
// queue nothing holds any more (ADVANCES).
type checkRelease struct {
	unread, held, advances logsite.Message
}

// releasedCheck records the release of REPO's check, as WHY words it, with
// what holds the queue after it, and moves the queue on: republished, and
// kicked, so a queue nothing holds now advances. A release while the mediator's remediation finish stands
// leaves the queue held for it (deployHold), and so does a newer deploy's
// check; the record then says so and names the holder (held_by), and the
// release a caller recorded (FIELDS' released) is named check_released, since
// the queue was not released.
func (o *orchestrator) releasedCheck(ctx context.Context, repo wsm.RepoKey, why checkRelease, fields dlog.Context) {
	hold, err := o.deployHoldOf(ctx, repo)
	switch {
	case err != nil:
		o.deps.Log.Global().Log(dlog.LevelError, opMediation, why.unread, withField(fields, "error", err.Error()))
	case hold.holdsQueue():
		held := withField(fields, "held_by", hold.heldBy())
		if released, ok := held["released"]; ok {
			delete(held, "released")
			held["check_released"] = released
		}
		held["holder"] = heldByWords(hold.heldBy())
		o.deps.Log.Global().Log(dlog.LevelInfo, opMediation, why.held, held)
	default:
		o.deps.Log.Global().Log(dlog.LevelInfo, opMediation, why.advances, fields)
	}
	o.republishMediation(ctx, repo)
	o.kick(repo)
}

// heldByWords is how a record names what holds a queue (deployHold.heldBy).
func heldByWords(by string) string {
	if by == heldByRemediationFinish {
		return "the deploy mediator's remediation finish under way, until the mediator approves its deploy"
	}
	return "the check a newer deploy owes"
}

// CheckStage is where an owed deploy check stands.
type CheckStage int

const (
	// CheckRollingOut: the deploy still rolls out; the mediator is not told.
	CheckRollingOut CheckStage = iota + 1
	// CheckAssessing: the mediator is checking the deploy.
	CheckAssessing
	// CheckFixing: the mediator fixes what it dispatched to itself.
	CheckFixing
	// CheckEscalated: the check waits on the user's escalation.
	CheckEscalated
)

// OwedMediation is the deploy check a queue waits on.
type OwedMediation struct {
	// Repository is the registered repository whose queue it holds.
	Repository ids.RepoID
	// Stage is where the check stands.
	Stage CheckStage
	// Mediator is the repository's deploy mediator, empty only while the
	// repository's first check rolls out, before the mediator is made.
	Mediator ids.WorkspaceID
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
	mediator, found, err := o.deps.DB.DeployMediatorOf(ctx, repo)
	if err != nil {
		return nil, err
	}
	var ws ids.WorkspaceID
	if found {
		ws = mediator.Workspace
	}
	return owedFrom(q.repository.ID, *m, ws)
}

// owedFrom is check M of repository REPO, whose deploy mediator is MEDIATOR,
// as the queue's status reads it. Only a check still rolling out may have
// no mediator: every later stage is reached through it, so one with none
// is a broken store.
func owedFrom(repo ids.RepoID, m wsm.Mediation, mediator ids.WorkspaceID) (*OwedMediation, error) {
	out := &OwedMediation{Repository: repo, Mediator: mediator, InARow: m.ChainRedeploys}
	switch {
	case m.Escalated():
		out.Stage = CheckEscalated
	case m.Dispatched:
		out.Stage = CheckFixing
	case m.Prompted():
		out.Stage = CheckAssessing
	default:
		out.Stage = CheckRollingOut
	}
	if out.Stage != CheckRollingOut && mediator == "" {
		return nil, fmt.Errorf("merge: the check of %s owed at %v is %d with no deploy mediator recorded (%+v)", repo, m.OwedAt, out.Stage, m)
	}
	return out, nil
}

// rosterMediation is the deploy check REPO's queue waits on as the roster
// draws it, beside the repository's name: nil while it owes none.
func (o *orchestrator) rosterMediation(ctx context.Context, repo wsm.RepoKey) (*sidebar.QueueMediation, error) {
	// A DIRECT FINISH UNDER WAY IS DRAWN FIRST, owed check or none (an
	// escalation's finish runs after its chain ended): its gate and landing
	// test the fix, its deploy rolls out. A finish sent back to fix is drawn
	// as its owed check draws it, and as FIXING with none owed.
	finishes, err := o.queueFinishesAt(ctx, repo, wsm.RemediationGating, wsm.RemediationLanding, wsm.RemediationDeploying, wsm.RemediationFixing)
	if err != nil {
		return nil, err
	}
	var fixing *wsm.RemediationFinish
	for i, f := range finishes {
		if f.Step != wsm.RemediationFixing {
			return o.rosterFinish(ctx, repo, f)
		}
		if fixing == nil {
			fixing = &finishes[i]
		}
	}
	owed, err := o.owedMediation(ctx, repo)
	if err != nil {
		return nil, err
	}
	if owed == nil {
		if fixing == nil {
			return nil, nil
		}
		return o.rosterFixingUnowed(ctx, repo, *fixing)
	}
	out := &sidebar.QueueMediation{Repo: owed.Repository, Mediator: owed.Mediator}
	switch owed.Stage {
	case CheckEscalated:
		out.Stage = sidebar.MediationEscalated
	case CheckRollingOut:
		out.Stage = sidebar.MediationWaiting
	case CheckAssessing:
		out.Stage = sidebar.MediationChecking
	case CheckFixing:
		out.Stage = sidebar.MediationFixing
	}
	return out, nil
}

// rosterFinish is finish F under way in REPO's queue as the roster draws it:
// its gate and landing test the fix; its deploy rolls out, selecting the
// mediator when a check is owed.
func (o *orchestrator) rosterFinish(ctx context.Context, repo wsm.RepoKey, f wsm.RemediationFinish) (*sidebar.QueueMediation, error) {
	q, ok, err := o.checkedQueueOf(ctx, repo)
	if err != nil || !ok {
		return nil, err
	}
	switch f.Step {
	case wsm.RemediationGating, wsm.RemediationLanding:
		return &sidebar.QueueMediation{Repo: q.repository.ID, Stage: sidebar.MediationTesting, Mediator: f.Workspace}, nil
	case wsm.RemediationDeploying:
		owed, err := o.owedMediation(ctx, repo)
		if err != nil {
			return nil, err
		}
		out := &sidebar.QueueMediation{Repo: q.repository.ID, Stage: sidebar.MediationWaiting}
		if owed != nil {
			out.Mediator = owed.Mediator
		}
		return out, nil
	}
	panic(fmt.Sprintf("merge: the finish of %s under way stands at %q", f.Workspace, f.Step))
}

// rosterFixingUnowed is the label of REPO's queue while the mediator's
// finish F, sent back to fix by its gate, holds it with no check owed: the
// finish holds the queue (deployHold), so its reason is drawn as a check's
// fixing is.
func (o *orchestrator) rosterFixingUnowed(ctx context.Context, repo wsm.RepoKey, f wsm.RemediationFinish) (*sidebar.QueueMediation, error) {
	q, ok, err := o.checkedQueueOf(ctx, repo)
	if err != nil || !ok {
		return nil, err
	}
	return &sidebar.QueueMediation{Repo: q.repository.ID, Stage: sidebar.MediationFixing, Mediator: f.Workspace}, nil
}

// republishMediation tells the roster and the queue's subscribers a change
// of REPO's check; a republication that fails is recorded.
func (o *orchestrator) republishMediation(ctx context.Context, repo wsm.RepoKey) {
	o.surfaceFinish(ctx, repo)
	if err := o.republishQueue(ctx, repo); err != nil {
		o.deps.Log.Global().Error(opMediation, "could not republish the queue whose check changed", dlog.Context{"repo": string(repo), "error": err.Error()})
	}
}

// recordUnreadWindow records the window of check M of queue Q, whose brief
// could not be composed, as read from where the last check read to through
// now, holding no groups: the mediator approved the deploy from its own
// reading of it. The next check's window then starts here.
func (o *orchestrator) recordUnreadWindow(ctx context.Context, q checkedQueue, m wsm.Mediation, fields dlog.Context) error {
	since, err := o.windowStart(ctx, q.key, m)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read where the window of a check that could not run starts; the approval is not recorded and the queue stays held", withField(fields, "error", err.Error()))
		return err
	}
	window := wsm.CheckWindow{Start: since, Through: o.deps.Now()}
	fields = withField(withField(fields, "window_started", window.Start.UTC().Format(time.RFC3339Nano)), "window_through", window.Through.UTC().Format(time.RFC3339Nano))
	if _, err := o.deps.DB.RecordMediationWindow(ctx, q.key, m.OwedAt, window); err != nil {
		o.deps.Log.Global().Error(opMediation, "the window of a check that could not run could not be recorded as read; the approval is not recorded and the queue stays held", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Log.Global().Info(opMediation, "the mediator approved a check that could not run; its window is recorded as read, and the next check's starts here", fields)
	return nil
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

// problemStatements is each of PROBLEMS's statement, for a record.
func problemStatements(problems []DeployProblem) []string {
	out := make([]string, 0, len(problems))
	for _, p := range problems {
		out = append(out, p.Statement)
	}
	return out
}

// escalatedKeys is every group of WINDOW an escalation takes on: each the
// mediator has not taken on, or that came back after its fix.
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

// takeOnProblems records that the mediator took on every group PROBLEMS
// quote and every group in ALSO, in REPO, with the problems' statements as
// its findings, or UNSTATED when the problems state none. A write that
// fails is recorded at ERROR: the decision stands, and the groups come back
// untaken to the next check.
func (o *orchestrator) takeOnProblems(ctx context.Context, repo wsm.RepoKey, problems []DeployProblem, also []wsm.ProblemKey, unstated string, fields dlog.Context) {
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
	if err := o.deps.DB.TakeOnDeployProblems(ctx, repo, keys, findings, o.deps.Now()); err != nil {
		o.deps.Log.Global().Error(opMediation, "the decided problems could not be recorded as taken on; the next check finds them untaken", withField(fields, "error", err.Error()))
		return
	}
	o.deps.Log.Global().Info(opMediation, "recorded the decided problems as taken on by the mediator", withField(fields, "problems", len(keys)))
}

// escalationBrief is what a check was escalated with, kept on the owed check
// (wsm.Mediation.EscalationBrief) so a later pass can tell the mediator and
// the user when the first could not.
type escalationBrief struct {
	Summary  string          `json:"summary"`
	Problems []DeployProblem `json:"problems"`
	// NotRun is set when the check was escalated because it could not run
	// (roundFailed), nil for an escalation of something found: the mediator
	// is then told what failed, never that the deploy was found broken.
	NotRun *checkNotRun `json:"not_run,omitempty"`
}

// checkNotRun is why a check could not run: WHAT failed, and its CAUSE.
// TOLD is whether the mediator was told of the check before the failure, so
// its brief says truly whether anything has looked at the deploy (a brief
// kept by an earlier build reads as untold).
type checkNotRun struct {
	What  string `json:"what"`
	Cause string `json:"cause"`
	Told  bool   `json:"told,omitempty"`
}

// holdEscalated escalates check M of queue Q with SUMMARY and PROBLEMS,
// WINDOW being the window the mediator was told (owner ruling,
// 2026-11-16): the brief is recorded on the check first, so the queue is
// held for the user from that write on, then the mediator and the user are
// told (tellEscalation). SAID is whether the mediator escalated in its own
// turn. The hold ends only when the mediator's own fix deploys
// (wsm.OweMediation) or the user releases it.
func (o *orchestrator) holdEscalated(ctx context.Context, q checkedQueue, m wsm.Mediation, summary string, problems []DeployProblem, window wsm.CheckWindow, said bool, fields dlog.Context) {
	o.holdBrief(ctx, q, m, escalationBrief{Summary: summary, Problems: problems}, window, said, fields)
}

// holdBrief is holdEscalated for a whole BRIEF, which may say the check
// could not run (escalationBrief.NotRun).
func (o *orchestrator) holdBrief(ctx context.Context, q checkedQueue, m wsm.Mediation, brief escalationBrief, window wsm.CheckWindow, said bool, fields dlog.Context) {
	encoded, err := json.Marshal(brief)
	if err != nil {
		panic(fmt.Sprintf("merge: an escalation's brief of strings did not encode: %v", err))
	}
	held, err := o.deps.DB.EscalateMediation(ctx, q.key, m.OwedAt, string(encoded))
	if err != nil {
		// THE USER IS TOLD OF A HOLD NOTHING WILL DECIDE: no escalation stands
		// for the mediator to approve or the user to release, so the notice
		// says the check is held as it is, and why.
		if !o.mediationFailed(ctx, fields, logsite.Msg("the check could not be recorded as escalated; nothing is escalated and the queue stays held"), err) {
			o.notifyHeld(ctx, q, fmt.Sprintf("the deploy check is held and could not be escalated (%v): %s", err, brief.Summary), fields)
		}
		return
	}
	if !held {
		o.deps.Log.Global().Info(opMediation, "the check was replaced, released or escalated already; it is not escalated again", fields)
		return
	}
	o.notifyHeld(ctx, q, brief.Summary, fields)
	o.republishMediation(ctx, q.key)
	o.tellEscalation(ctx, q, m, brief, window, said, fields)
}

// notifyHeld notifies the user, with SUMMARY, of the check of queue Q held
// with no mediator turn to decide it: as its escalation is recorded, once per
// escalation, since only the one write that escalates the check comes here
// (wsm.EscalateMediation takes a check not escalated yet), and BEFORE the
// mediator is told, so the user's notice never depends on the mediator being
// reachable, shown or briefable (2026-11-21: a telling that failed left the
// user told nothing); and when that write fails. The notice's click opens
// the queue's recorded mediator; a hold with none recorded is a broken
// invariant (roundFailed makes one first, and every other escalation comes
// from the mediator's own check), recorded at ERROR: a desktop notice names a
// workspace (desktopnotify.Notifier.show), so none is posted.
func (o *orchestrator) notifyHeld(ctx context.Context, q checkedQueue, summary string, fields dlog.Context) {
	recorded, found, err := o.deps.DB.DeployMediatorOf(ctx, q.key)
	if err != nil {
		o.mediationFailed(ctx, fields, logsite.Msg("could not read the deploy mediator to name the held check's notice; the user is not notified of it"), err)
		return
	}
	if !found {
		o.deps.Log.Global().Error(opMediation, "the check is held with no deploy mediator recorded to name its notice; the user is not notified of it", fields)
		return
	}
	o.deps.Banners.Mediated(recorded.Workspace, summary)
	o.deps.Log.Global().Info(opMediation, "the user is notified of the held check", withField(fields, "mediator", string(recorded.Workspace)))
}

// tellEscalation tells the mediator and the user of escalated check M of
// queue Q (escalate), records that the mediator took on what it escalated,
// and records the check told. A telling that fails leaves the check
// escalated and untold, holding the queue, and the next pass tells it again
// (intakeRound).
func (o *orchestrator) tellEscalation(ctx context.Context, q checkedQueue, m wsm.Mediation, brief escalationBrief, window wsm.CheckWindow, said bool, fields dlog.Context) {
	if _, err := o.escalate(ctx, q, m, brief, window, said, fields); err != nil {
		// ESCALATE RECORDED WHY, at ERROR or, for a cut, at INFO: this
		// records only that the check waits for the next pass.
		o.deps.Log.Global().Info(opMediation, "the escalated check was not told to the mediator; the queue stays held, and the next pass tells it again", fields)
		return
	}
	o.takeOnProblems(ctx, q.key, brief.Problems, escalatedKeys(window), "taken on with the escalation", fields)
	told, err := o.deps.DB.MarkEscalationTold(ctx, q.key, m.OwedAt)
	if err != nil {
		o.mediationFailed(ctx, fields, logsite.Msg("the escalation was told but could not be recorded so; the queue stays held, and the next pass tells it again"), err)
		return
	}
	if !told {
		o.deps.Log.Global().Info(opMediation, "the escalated check was released or replaced while it was told", fields)
		return
	}
	o.deps.Log.Global().Info(opMediation, "the check is escalated; the queue is held until the mediator's fix deploys or the user releases it", fields)
	o.republishMediation(ctx, q.key)
}

// remakeHeldEscalation tells the mediator and the user of escalated check M
// of queue Q, recorded untold, from the brief the check keeps.
func (o *orchestrator) remakeHeldEscalation(ctx context.Context, q checkedQueue, m wsm.Mediation, fields dlog.Context) {
	var brief escalationBrief
	if err := json.Unmarshal([]byte(m.EscalationBrief), &brief); err != nil {
		panic(fmt.Sprintf("merge: the escalated check of %s keeps a brief that does not decode: %v", q.key, err))
	}
	window, err := o.toldWindow(ctx, q.key, fields)
	if err != nil {
		return
	}
	o.deps.Log.Global().Info(opMediation, "the escalated check was not told; it is told again", fields)
	o.tellEscalation(ctx, q, m, brief, window, false, fields)
}

// finishUnderWay answers the direct finish under way in REPO's queue:
// gating, landing or deploying. Nil when none is.
func (o *orchestrator) finishUnderWay(ctx context.Context, repo wsm.RepoKey) (*wsm.RemediationFinish, error) {
	return o.queueFinishAt(ctx, repo, wsm.RemediationGating, wsm.RemediationLanding, wsm.RemediationDeploying)
}

// queueFinishAt answers the first finish of REPO's queue standing at one of
// STEPS (queueFinishesAt), nil when none does.
func (o *orchestrator) queueFinishAt(ctx context.Context, repo wsm.RepoKey, steps ...wsm.RemediationStep) (*wsm.RemediationFinish, error) {
	finishes, err := o.queueFinishesAt(ctx, repo, steps...)
	if err != nil || len(finishes) == 0 {
		return nil, err
	}
	return &finishes[0], nil
}

// queueFinishesAt answers every finish of REPO's queue standing at one of
// STEPS, in the store's order. A failed read is answered.
func (o *orchestrator) queueFinishesAt(ctx context.Context, repo wsm.RepoKey, steps ...wsm.RemediationStep) ([]wsm.RemediationFinish, error) {
	finishes, err := o.deps.DB.RemediationFinishes(ctx)
	if err != nil {
		return nil, err
	}
	var out []wsm.RemediationFinish
	for _, f := range finishes {
		if sameQueue(f.Repo, repo) && slices.Contains(steps, f.Step) {
			out = append(out, f)
		}
	}
	return out, nil
}
