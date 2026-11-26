package merge

import (
	"context"
	"errors"
	"fmt"
	"time"

	"agentrepl/logging/logsite"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE PRODUCTION PAUSE (agentrepl.v1.UpdateProductionPause; the store's half
// is wsm's productionpause.go). Pausing records the pause and drops every
// undecided span in one write, retires a span brief still held in the
// mediator's prompt queue, then interrupts the mediator's running turn.
//
// NO BRIEF REACHES THE MEDIATOR AFTER A PAUSE, STRUCTURALLY. Every brief of a
// span is marked told and submitted under the repository's production lock
// (underProductionLock), and the pause takes the same lock to drop the spans
// and retire any held brief. A round that marks after the pause finds its
// span gone and submits nothing; one that submitted before it has its brief
// retired, or, already delivered, its turn interrupted.

const (
	opPauseProduction  = "daemon.merge.pause_production"
	opResumeProduction = "daemon.merge.resume_production"
)

// The refusal arms UpdateProductionPause answers beyond the queue's shared
// ArmAlreadyPaused, ArmNotPaused and ArmUnknownRepository.
const (
	// ArmProductionChecksOff refuses a pause or a resume of a repository
	// whose lifecycle config turns no production checks on
	// (UpdateProductionPauseError.checks_off).
	ArmProductionChecksOff = "checks_off"
	// ArmProductionPaused refuses a production mediator's decision or fold
	// while its repository's checks are paused
	// (UpdateProductionMediationError.paused).
	ArmProductionPaused = "paused"
)

// ProductionPaused is what a pause stopped.
type ProductionPaused struct {
	// Interrupted is the production mediator whose running turn the pause
	// interrupted, empty when it ran none or there is no mediator yet.
	Interrupted ids.WorkspaceID
	// Dropped is how many undecided spans the pause dropped.
	Dropped int
}

// underProductionLock runs PERFORM holding REPO's production lock, which
// serializes its span briefs with its pause: every step that tells the
// mediator a span, and the pause.
func (o *orchestrator) underProductionLock(repo wsm.RepoKey, perform func()) {
	defer o.productionLocks.Lock(repo)()
	perform()
}

// productionQueueOf answers the production-checked queue SCOPE names: an
// unknown repository, or one that checks no production, is refused.
func (o *orchestrator) productionQueueOf(ctx context.Context, op, what string, scope RepositoryScope) (checkedQueue, dlog.Context, error) {
	repository, key, err := o.scopedRepository(ctx, op, what, scope)
	if err != nil {
		return checkedQueue{}, nil, err
	}
	fields := dlog.Context{"repo": string(key), "repository": repository.Dir}
	queues, err := o.productionQueues(ctx, op)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not list the repositories that check their production", withField(fields, "error", err.Error()))
		return checkedQueue{}, nil, err
	}
	q, found := queueKeyed(queues, key)
	if !found {
		o.deps.Log.Global().Info(op, "refused {what} of a repository that checks no production", withField(withField(fields, "what", what), "arm", ArmProductionChecksOff))
		return checkedQueue{}, nil, refuse(ArmProductionChecksOff, "", "the repository %s turns no production checks on", repository.Dir)
	}
	return q, fields, nil
}

// PauseProduction implements Orchestrator.
func (o *orchestrator) PauseProduction(ctx context.Context, scope RepositoryScope) (ProductionPaused, error) {
	log := o.deps.Log.Global()
	q, fields, err := o.productionQueueOf(ctx, opPauseProduction, "a production pause", scope)
	if err != nil {
		return ProductionPaused{}, err
	}
	mediator, hasMediator, err := o.deps.DB.StandingMediatorOf(ctx, wsm.MediatorProduction, q.key)
	if err != nil {
		log.Error(opPauseProduction, "could not read the repository's production mediator; nothing is paused", withField(fields, "error", err.Error()))
		return ProductionPaused{}, err
	}
	var (
		dropped []wsm.ProductionCheck
		retired []error
	)
	o.underProductionLock(q.key, func() {
		dropped, err = o.deps.DB.PauseProduction(ctx, q.key, o.deps.Now())
		if err != nil || !hasMediator {
			return
		}
		retired = o.retireHeldBriefs(ctx, mediator.Workspace, dropped, fields)
	})
	if errors.Is(err, wsm.ErrProductionPaused) {
		log.Info(opPauseProduction, "refused a pause of production checks already paused", fields)
		return ProductionPaused{}, refuse(ArmAlreadyPaused, "", "the repository's production checks are already paused")
	}
	if err != nil {
		log.Error(opPauseProduction, "could not pause the repository's production checks; nothing is paused", withField(fields, "error", err.Error()))
		return ProductionPaused{}, err
	}
	for range dropped {
		o.deps.Meter.ObserveProductionCheck(ProductionCheckPaused, 0)
	}
	out := ProductionPaused{Dropped: len(dropped)}
	fields = withField(fields, "dropped_spans", len(dropped))
	if hasMediator {
		fields = withField(fields, "mediator", string(mediator.Workspace))
		running, err := o.deps.InterruptTurn(ctx, mediator.Workspace)
		if err != nil {
			o.mediationFailedUnder(ctx, opPauseProduction, fields, logsite.Msg("the production checks are paused, but the production mediator's running turn could not be interrupted"), err)
			return out, fmt.Errorf("merge: the production checks are paused, but the production mediator's turn could not be interrupted: %w", err)
		}
		if running {
			out.Interrupted = mediator.Workspace
		}
		fields = withField(fields, "interrupted", running)
	}
	if err := errors.Join(retired...); err != nil {
		return out, fmt.Errorf("merge: the production checks are paused, but a span brief held for the mediator could not be retired: %w", err)
	}
	log.Info(opPauseProduction, "paused the repository's production checks: its undecided spans are dropped and its mediator is told none", fields)
	return out, nil
}

// retireHeldBriefs retires every brief of DROPPED still held in mediator
// WS's prompt queue, so none starts a turn after the pause. A brief already
// delivered is left to the interrupt. It answers each failure, recorded at
// ERROR.
func (o *orchestrator) retireHeldBriefs(ctx context.Context, ws ids.WorkspaceID, dropped []wsm.ProductionCheck, fields dlog.Context) []error {
	var errs []error
	for _, c := range dropped {
		if c.Turn == "" {
			continue
		}
		span := withField(withField(fields, "turn", string(c.Turn)), "span_until", c.Until.UTC().Format(time.RFC3339Nano))
		retired, err := o.deps.Queue.Supersede(ctx, ws, c.Turn)
		if err != nil {
			o.mediationFailedUnder(ctx, opPauseProduction, span, logsite.Msg("a span brief held for the production mediator could not be retired at the pause; it may still start a turn, whose decisions are refused"), err)
			errs = append(errs, err)
			continue
		}
		o.deps.Log.Global().Info(opPauseProduction, "settled the brief of a span the pause dropped", withField(span, "retired_unsent", retired))
	}
	return errs
}

// ResumeProduction implements Orchestrator.
func (o *orchestrator) ResumeProduction(ctx context.Context, scope RepositoryScope) error {
	log := o.deps.Log.Global()
	q, fields, err := o.productionQueueOf(ctx, opResumeProduction, "a production resume", scope)
	if err != nil {
		return err
	}
	err = o.deps.DB.ResumeProduction(ctx, q.key, o.deps.Now())
	if errors.Is(err, wsm.ErrProductionNotPaused) {
		log.Info(opResumeProduction, "refused a resume of production checks that are not paused", fields)
		return refuse(ArmNotPaused, "", "the repository's production checks are not paused")
	}
	if err != nil {
		log.Error(opResumeProduction, "could not resume the repository's production checks; they stay paused", withField(fields, "error", err.Error()))
		return err
	}
	log.Info(opResumeProduction, "resumed the repository's production checks: they start again at the first landing after now", fields)
	return nil
}

// refuseWhilePaused refuses production mediator WS's ACTION while REPO's
// checks are paused, answering nil while they run.
func (o *orchestrator) refuseWhilePaused(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, action string, fields dlog.Context) error {
	pause, err := o.deps.DB.ProductionPauseOf(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(opProduction, "could not read whether the production checks are paused", withField(withField(fields, "action", action), "error", err.Error()))
		return err
	}
	if !pause.Paused {
		return nil
	}
	o.deps.Log.Global().Info(opProduction, "refused a production mediator's action while its repository's production checks are paused", withField(withField(fields, "action", action), "paused_since", pause.Since.UTC().Format(time.RFC3339Nano)))
	return refuse(ArmProductionPaused, ws, "the repository's production checks are paused: decide and fold nothing until they are resumed")
}

// ProductionStanding is one repository's production checks as the topbar's
// production-checks chip draws them.
type ProductionStanding struct {
	// On reports that the repository's lifecycle config turns production
	// checks on; nothing else is set when it is false.
	On bool
	// Paused reports that the checks are paused.
	Paused bool
	// Mediator is the repository's production mediator, empty while none is
	// made yet.
	Mediator ids.WorkspaceID
}

// ProductionStanding implements Orchestrator: a repository that checks no
// production answers the zero standing, and no refusal.
func (o *orchestrator) ProductionStanding(ctx context.Context, scope RepositoryScope) (ProductionStanding, error) {
	const op = "daemon.merge.production_standing"
	_, key, err := o.scopedRepository(ctx, op, "a production standing", scope)
	if err != nil {
		return ProductionStanding{}, err
	}
	fields := dlog.Context{"repo": string(key)}
	queues, err := o.productionQueues(ctx, op)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not list the repositories that check their production", withField(fields, "error", err.Error()))
		return ProductionStanding{}, err
	}
	if _, found := queueKeyed(queues, key); !found {
		return ProductionStanding{}, nil
	}
	pause, err := o.deps.DB.ProductionPauseOf(ctx, key)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read whether the repository's production checks are paused", withField(fields, "error", err.Error()))
		return ProductionStanding{}, err
	}
	mediator, _, err := o.deps.DB.StandingMediatorOf(ctx, wsm.MediatorProduction, key)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the repository's production mediator", withField(fields, "error", err.Error()))
		return ProductionStanding{}, err
	}
	return ProductionStanding{On: true, Paused: pause.Paused, Mediator: mediator.Workspace}, nil
}
