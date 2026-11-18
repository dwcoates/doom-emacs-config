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
	// ArmNoEscalationHeld refuses a release of an escalation's hold when no
	// escalation holds the queue.
	ArmNoEscalationHeld = "no_escalation_held"
	// ArmRemediationBoundReached refuses a dispatch once the chain deployed
	// as many remediations in a row as its repository allows.
	ArmRemediationBoundReached = "remediation_bound_reached"
	// ArmProblemsUnowned refuses an approval while the check's window holds a
	// group no one owns: NEW, SEEN BEFORE, or RECURRED after its fix.
	ArmProblemsUnowned = "problems_unowned"
	// ArmProblemsRecurring refuses anything but an escalation while the
	// check's window holds a group that came back after more than
	// maxFixesBeforeEscalation of its fixes deployed.
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
		o.mediationFailed(ctx, nil, "could not list the repositories a deploy's ending is recorded in; its checks are told without it", err)
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
		o.mediationFailed(ctx, nil, "could not list the repositories an unchanged deploy owes checks in; its checks stay owed", err)
		return err
	}
	var errs []error
	for _, q := range queues {
		fields := dlog.Context{"repo": string(q.key), "deploy_started": at.UTC().Format(time.RFC3339Nano)}
		m, err := o.deps.DB.MediationOf(ctx, q.key)
		if err != nil {
			errs = append(errs, err)
			if o.mediationFailed(ctx, fields, "could not read the check an unchanged deploy owes; it stays owed", err) {
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
			if o.mediationFailed(ctx, fields, "could not release the check of a deploy that changed nothing; it stays owed", err) {
				break
			}
			continue
		}
		if !released {
			continue
		}
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
		o.settleMediatorSight(ctx, q, m)
		o.surfaceFinish(ctx, q.key)
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
		released, err := o.releaseCheck(ctx, q.key, *m, MediationUnmediated)
		if err != nil {
			if o.mediationFailed(ctx, dlog.Context{"repo": string(q.key)}, "could not release a check no intake decides; the queue stays held", err) {
				return
			}
			continue
		}
		if released {
			o.deps.Log.Global().Info(opMediation, "released a check owed by a repository whose lifecycle config checks no deploys now; its queue advances", dlog.Context{
				"repo": string(q.key), "deploy_started": m.OwedAt.UTC().Format(time.RFC3339Nano)})
			o.republishMediation(ctx, q.key)
			o.kick(q.key)
		}
	}
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
	// that changed nothing. Its count is what compacts the standing
	// workspaces after their work (noteQuietCheck).
	if outcome == MediationApproved || outcome == MediationUnchanged {
		o.noteQuietCheck(ctx, repo, dlog.Context{"repo": string(repo), "outcome": outcome})
	}
	return true, nil
}

// endChain records how check M of queue REPO, the last of its chain, ended
// as OUTCOME and forgets what the chain's remediations landed: the next
// deploy begins another chain, which the same mediator takes. The
// mediator's session ends when its turn does (checkTurnEnded), never
// mid-turn.
func (o *orchestrator) endChain(ctx context.Context, repo wsm.RepoKey, m wsm.Mediation, outcome string) {
	o.observeMediationEnd(m, outcome, o.deps.Now())
	fields := dlog.Context{"repo": string(repo), "outcome": outcome, "chain_started": m.ChainStartedAt.UTC().Format(time.RFC3339Nano)}
	if _, err := o.deps.DB.EndRemediationFinishesOf(ctx, repo); err != nil {
		o.deps.Log.Global().Error(opMediation, "the chain ended but what its remediations landed could not be forgotten", withField(fields, "error", err.Error()))
	}
	o.deps.Log.Global().Info(opMediation, "the chain ended; the mediator's session ends with its turn", fields)
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

// intakeRound tells Q's mediator of check M, when it was not told yet, and
// awaits the turn it was told in; then settles a check that turn decided
// nothing about (intakeTurnEnded). A check the mediator dispatched a fix
// for is watched instead (watchFixer): its fix's deploy owes the next.
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
	ws, err := o.mediatorFor(ctx, q, m, fields)
	if err != nil {
		o.mediationFailed(ctx, fields, "the deploy mediator could not be found or made; the queue stays held for it", err)
		return
	}
	fields["mediator"] = string(ws)
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
		o.awaitFailed(ctx, q, m.OwedAt, fields, "the intake's turn could not be awaited; the queue stays held until the next round", err)
		return
	}
	o.intakeTurnEnded(ctx, q, ws, m.OwedAt, fields)
	// A CHECK THE INTAKE DISPATCHED IS WATCHED BY THIS SAME ROUND: the claim
	// it holds kept every pass the dispatch ran from starting another.
	if now, err := o.deps.DB.MediationOf(ctx, q.key); err != nil {
		o.mediationFailed(ctx, fields, "could not read the check after its intake's turn ended; the next pass waits on its fixer", err)
	} else if owes(now, m.OwedAt) && now.Dispatched {
		o.watchFixer(ctx, q, *now, fields)
	}
}

// awaitFailed settles a failed await of the intake's turn on the check owed
// at OWED. THE DECISION IS RECORDED BEFORE IT STOPS THE INTAKE (the check
// released, dispatched or escalated, then the intake closed), so an await
// cut by that stop finds the check decided: that is the decision's end,
// recorded at INFO. Any
// other failure holds the queue and is the round's failure, MESSAGE.
func (o *orchestrator) awaitFailed(ctx context.Context, q checkedQueue, owed time.Time, fields dlog.Context, message string, err error) {
	if now, readErr := o.deps.DB.MediationOf(ctx, q.key); readErr == nil && (!owes(now, owed) || now.Dispatched || now.Escalated()) {
		o.deps.Log.Global().Info(opMediation, "the intake's turn ended with its session, which its own decision stopped; the check is decided", withField(fields, "cause", err.Error()))
		return
	}
	o.mediationFailed(ctx, fields, message, err)
}

// mediatorFor answers the deploy mediator check M is told to: readied
// (readyMediator) and left hidden before its first telling, since it is in
// sight only while it fixes or waits on the user; as it stands once M was
// told, since its turn on M is reattached and a refresh would move its
// worktree under that turn.
func (o *orchestrator) mediatorFor(ctx context.Context, q checkedQueue, m wsm.Mediation, fields dlog.Context) (ids.WorkspaceID, error) {
	if m.Prompted() {
		mediator, err := o.mediatorOf(ctx, q, fields)
		if err != nil {
			return "", fmt.Errorf("merge: the queue's deploy mediator: %w", err)
		}
		return mediator.ws, nil
	}
	mediator, err := o.readyMediator(ctx, q, false, fields)
	if err != nil {
		return "", fmt.Errorf("merge: the queue's deploy mediator: %w", err)
	}
	if !mediator.briefed {
		o.markBriefed(ctx, q, fields)
	}
	o.deps.Log.Global().Info(opMediation, "the deploy mediator takes the check", withField(fields, "mediator", string(mediator.ws)))
	return mediator.ws, nil
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
	if !owes(m, owed) {
		o.deps.Log.Global().Info(opMediation, "the intake's turn ended with its check decided or replaced by a newer deploy", fields)
		// A NEWER DEPLOY'S CHECK IS TOLD TO THIS SAME INTAKE, by the round
		// that owns it, so only a decided check ends the session here.
		if m == nil {
			o.stopIntake(ctx, q.key, ws, fields)
		}
		return
	}
	// THE MEDIATOR'S SESSION RUNS ON WHILE IT HOLDS ITS QUEUE AT WORK: a
	// dispatch is its own to fix (its finish stands it by), and an
	// escalation is its own to settle with the user.
	if m.Dispatched {
		o.deps.Log.Global().Info(opMediation, "the mediator's turn ended with a fix dispatched; the queue stays held for the fix", fields)
		return
	}
	if m.Escalated() {
		o.deps.Log.Global().Info(opMediation, "the mediator's turn ended with the deploy escalated; the queue stays held for the user", fields)
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
		o.awaitFailed(ctx, q, m.OwedAt, fields, "the resumed intake's turn could not be awaited; the queue stays held until the next round", err)
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

// DecideMediation implements Orchestrator: the deploy mediator WS decides
// the deploy its queue owes a check of, once it was told of it. An approval
// releases the check, ends the chain and lets the queue advance; an
// escalation holds the queue for the user; a dispatch keeps the queue held
// while the mediator fixes the problems itself (dispatch.go).
func (o *orchestrator) DecideMediation(ctx context.Context, ws ids.WorkspaceID, d Decision) error {
	fields := dlog.Context{"workspace": string(ws), "decision": d.Kind.String(), "summary": d.Summary}
	// A DECISION RUNS ON THE MEDIATION'S LIFETIME, NEVER ITS REQUEST'S: the
	// request comes from the intake's own session, and an approval or an
	// escalation ends the chain, which stops that session and cancels the
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
	if owed == nil || !owed.Prompted() {
		o.deps.Log.Global().Warn(opMediation, "refused a decision while the mediator's queue owes no check it was told of", fields)
		return refuse(ArmNoMediationOwed, ws, "no deploy owes a check you were told of")
	}
	m := *owed
	if m.Dispatched {
		o.deps.Log.Global().Warn(opMediation, "refused a decision on a deploy a fix was dispatched for already", fields)
		return refuse(ArmNoMediationOwed, ws, "a fix is dispatched for this deploy already; its deploy owes the next check")
	}
	if m.Escalated() {
		o.deps.Log.Global().Warn(opMediation, "refused a decision on a deploy escalated already", fields)
		return refuse(ArmNoMediationOwed, ws, "this deploy is escalated to the user already; the queue waits for them")
	}
	window, err := o.toldWindow(ctx, q.key, fields)
	if err != nil {
		return err
	}
	// A PROBLEM PAST ITS FIXES IS THE USER'S (owner ruling, 2026-11-16): while
	// the window holds one, only an escalation is accepted.
	if recurring := recurringGroups(window); d.Kind != MediationEscalate && len(recurring) > 0 {
		refusal := &RefusalError{Arm: ArmProblemsRecurring, Workspace: ws,
			Reason: fmt.Sprintf("the window holds %d groups that came back after more than %d of their fixes deployed; escalate", len(recurring), maxFixesBeforeEscalation),
			Fields: map[string]any{"groups": recurring}}
		logRefusal(o.deps.Log.Global(), opMediation, "refused a decision other than an escalation while the window holds problems past their fixes", withField(fields, "recurring", recurring), refusal)
		return refusal
	}
	if d.Kind == MediationDispatch {
		return o.dispatch(ctx, q, m, ws, d, fields)
	}
	// AN APPROVAL LEAVES NO PROBLEM UNTAKEN (owner ruling, 2026-11-16): a
	// warning or error the mediator never took on, old or new, is dispatched
	// or escalated, never approved past, or it recurs with no one to end it.
	if untaken := untakenGroups(window); d.Kind == MediationApprove && len(untaken) > 0 {
		refusal := &RefusalError{Arm: ArmProblemsUnowned, Workspace: ws,
			Reason: fmt.Sprintf("the window holds %d groups you have not taken on; dispatch or escalate them", len(untaken)),
			Fields: map[string]any{"groups": untaken}}
		logRefusal(o.deps.Log.Global(), opMediation, "refused an approval while the check's window holds groups the mediator never took on", withField(fields, "untaken", untaken), refusal)
		return refusal
	}
	// AN ESCALATION HOLDS THE QUEUE (owner ruling, 2026-11-16): the check
	// stays owed, escalated, until the mediator's fix deploys or the user
	// releases it.
	if d.Kind == MediationEscalate {
		o.deps.Log.Global().Info(opMediation, "the mediator escalated the deploy; the queue is held for the user", fields)
		o.holdEscalated(ctx, q, m, d.Summary, d.Problems, window, true, fields)
		return nil
	}
	// A DECISION RACING A NEWER DEPLOY RELEASES NOTHING: the replacement
	// counted that check's end, and the newer one is still owed.
	released, err := o.releaseCheck(ctx, q.key, m, MediationApproved)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not release the check the decision named", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Log.Global().Info(opMediation, "the mediator approved the deploy; the queue advances", withField(fields, "released", released))
	o.republishMediation(ctx, q.key)
	o.kick(q.key)
	return nil
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
	// as its owed check draws it.
	finish, err := o.finishUnderWay(ctx, repo)
	if err != nil {
		return nil, err
	}
	if finish != nil {
		return o.rosterFinish(ctx, repo, *finish)
	}
	owed, err := o.owedMediation(ctx, repo)
	if err != nil || owed == nil {
		return nil, err
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
// its gate and landing test the fix, with its workspace standing by hidden;
// its deploy rolls out, selecting the mediator when a check is owed.
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

// republishMediation tells the roster and the queue's subscribers a change
// of REPO's check; a republication that fails is recorded.
func (o *orchestrator) republishMediation(ctx context.Context, repo wsm.RepoKey) {
	o.surfaceFinish(ctx, repo)
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

// maxFixesBeforeEscalation is how many fixes of one problem may deploy
// before its recurrence is the user's (owner ruling, 2026-11-16): a problem
// that comes back after more than two is escalated, never dispatched again.
const maxFixesBeforeEscalation = 2

// pastItsFixes reports whether a group of CLASS whose problem's fixes
// deployed FIXES times came back past what a remediation may try.
func pastItsFixes(class wsm.WindowClass, fixes int) bool {
	return class == wsm.WindowRecurred && fixes > maxFixesBeforeEscalation
}

// recurringGroups names every group of WINDOW that came back past its fixes
// (pastItsFixes), as "OPERATION: MESSAGE (N fixes)", in its order.
func recurringGroups(window wsm.CheckWindow) []string {
	var out []string
	for _, g := range window.Groups {
		if pastItsFixes(g.Class, g.Fixes) {
			out = append(out, fmt.Sprintf("%s: %s (%d fixes)", g.Operation, g.Message, g.Fixes))
		}
	}
	return out
}

// untakenGroups names every group of WINDOW the mediator has not taken on,
// or that came back after its fix, as "OPERATION: MESSAGE (CLASS)", in its
// order.
func untakenGroups(window wsm.CheckWindow) []string {
	var out []string
	for _, g := range window.Groups {
		if g.Class != wsm.WindowOwned {
			out = append(out, fmt.Sprintf("%s: %s (%s)", g.Operation, g.Message, g.Class))
		}
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
}

// holdEscalated escalates check M of queue Q with SUMMARY and PROBLEMS,
// WINDOW being the window the mediator was told (owner ruling,
// 2026-11-16): the brief is recorded on the check first, so the queue is
// held for the user from that write on, then the mediator and the user are
// told (tellEscalation). SAID is whether the mediator escalated in its own
// turn. The hold ends only when the mediator's own fix deploys
// (wsm.OweMediation) or the user releases it.
func (o *orchestrator) holdEscalated(ctx context.Context, q checkedQueue, m wsm.Mediation, summary string, problems []DeployProblem, window wsm.CheckWindow, said bool, fields dlog.Context) {
	encoded, err := json.Marshal(escalationBrief{Summary: summary, Problems: problems})
	if err != nil {
		panic(fmt.Sprintf("merge: an escalation's brief of strings did not encode: %v", err))
	}
	held, err := o.deps.DB.EscalateMediation(ctx, q.key, m.OwedAt, string(encoded))
	if err != nil {
		o.mediationFailed(ctx, fields, "the check could not be recorded as escalated; nothing is escalated and the queue stays held", err)
		return
	}
	if !held {
		o.deps.Log.Global().Info(opMediation, "the check was replaced, released or escalated already; it is not escalated again", fields)
		return
	}
	o.noteCheckWork(ctx, q.key, fields)
	o.republishMediation(ctx, q.key)
	o.tellEscalation(ctx, q, m, summary, problems, window, said, fields)
}

// tellEscalation tells the mediator and the user of escalated check M of
// queue Q (escalate), records that the mediator took on what it escalated,
// and records the check told. A telling that fails leaves the check
// escalated and untold, holding the queue, and the next pass tells it again
// (intakeRound).
func (o *orchestrator) tellEscalation(ctx context.Context, q checkedQueue, m wsm.Mediation, summary string, problems []DeployProblem, window wsm.CheckWindow, said bool, fields dlog.Context) {
	if _, err := o.escalate(ctx, q, m, summary, problems, window, said, fields); err != nil {
		// ESCALATE RECORDED WHY, at ERROR or, for a cut, at INFO: this
		// records only that the check waits for the next pass.
		o.deps.Log.Global().Info(opMediation, "the escalated check was not told to the mediator; the queue stays held, and the next pass tells it again", fields)
		return
	}
	o.takeOnProblems(ctx, q.key, problems, escalatedKeys(window), "taken on with the escalation", fields)
	told, err := o.deps.DB.MarkEscalationTold(ctx, q.key, m.OwedAt)
	if err != nil {
		o.mediationFailed(ctx, fields, "the escalation was told but could not be recorded so; the queue stays held, and the next pass tells it again", err)
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
	o.tellEscalation(ctx, q, m, brief.Summary, brief.Problems, window, false, fields)
}

// stopIntake ends intake WS's session once the turn that decided a check of
// queue REPO ended: an intake decides once (owner ruling, 2026-11-16). THE
// STOP WAITS FOR THE TURN'S END, never cuts it (owner ruling, 2026-11-17): a
// decision command cut mid-run was saved in the intake's transcript as
// "User rejected tool use", and the intake read its own decisions as the
// user's rejections on its next brief and decided nothing.
func (o *orchestrator) stopIntake(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, fields dlog.Context) {
	if err := o.deps.DeployChecks.StopNow(ctx, ws); err != nil {
		o.deps.Log.Global().Error(opMediation, "the intake decided but its session could not be ended", withField(withField(fields, "intake", string(ws)), "error", err.Error()))
		return
	}
	o.deps.Log.Global().Info(opMediation, "the intake decided and its session is ended; it decides this check no more, and stands for the next", withField(withField(fields, "intake", string(ws)), "repo", string(repo)))
}

// finishUnderWay answers the direct finish under way in REPO's queue:
// gating, landing or deploying, its workspace standing by. Nil when none is.
func (o *orchestrator) finishUnderWay(ctx context.Context, repo wsm.RepoKey) (*wsm.RemediationFinish, error) {
	finishes, err := o.deps.DB.RemediationFinishes(ctx)
	if err != nil {
		return nil, err
	}
	for _, f := range finishes {
		if !sameQueue(f.Repo, repo) {
			continue
		}
		switch f.Step {
		case wsm.RemediationGating, wsm.RemediationLanding, wsm.RemediationDeploying:
			return &f, nil
		}
	}
	return nil, nil
}
