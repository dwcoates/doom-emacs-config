package deploy

import (
	"context"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// A FAILED DEPLOY IS A STANDING FAULT (owner request, 2026-09-27). The Deploy
// rpc's answer reaches only its caller, and a deploy a landing started has no
// caller at all, so a build, an install or a service restart that did not go
// through opens the daemon-scoped `deploy_failed` fault, which the footer
// draws on every strip through the one fault hook (health.ObserveFaults).
//
// ONE FAULT STANDS PER STEP. A step that fails again supersedes its own
// earlier fault; a step a later deploy gets through closes every fault of
// that step. A failure after the install began is ROLLED BACK (rollback.go),
// and its fault says so; a rollback that did not restore the previous build
// is a fault of its own, under the `rollback` step, which only a deploy that
// gets ALL the way through closes. A daemon that boots closes the ones an earlier daemon left
// standing, because no strip of the new process draws them: the footer
// learns of a fault only as it is opened.

// opFault is the operation every deploy-fault record is made under.
const opFault = "daemon.deploy.fault"

// Faults is the slice of the state client a deploy records its failures
// through: the observed one, so every open and close reaches the footer.
type Faults interface {
	OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error)
	OpenFaults(ctx context.Context, scope wsm.FaultScope) ([]wsm.Fault, error)
	CloseFault(ctx context.Context, id ids.FaultID, at time.Time) error
}

// failureOf answers the fault a deploy's error stands as, false for an error
// that is no build, install or service restart failure.
func failureOf(err error) (health.DeployFailure, bool) {
	var (
		build    *BuildFailed
		install  *InstallFailed
		service  *ServiceRestartFailed
		rollback *RollbackFailed
	)
	switch {
	case errors.As(err, &build):
		return health.DeployFailure{Step: health.DeployStepBuild, BuildStep: build.Step, Detail: build.Detail, Log: build.Log}, true
	case errors.As(err, &install):
		return health.DeployFailure{Step: health.DeployStepInstall, Component: install.Component.Arm(), Detail: install.Detail}, true
	case errors.As(err, &service):
		return health.DeployFailure{Step: health.DeployStepRestartServices, Component: service.Component.Arm(), Detail: service.Detail}, true
	case errors.As(err, &rollback):
		return health.DeployFailure{Step: health.DeployStepRollback, Component: rollback.Component.Arm(), Detail: rollback.Detail}, true
	default:
		return health.DeployFailure{}, false
	}
}

// rolledBack is what a failed deploy's rollback came to.
type rolledBack struct {
	// ran is true when the deploy had installed or restarted something, so a
	// rollback ran.
	ran bool
	// err is the rollback's own failure, nil when it restored the previous
	// build.
	err error
}

// state answers the rollback as a DeployFailure spells it.
func (r rolledBack) state() string {
	switch {
	case !r.ran:
		return health.RollbackNone
	case r.err != nil:
		return health.RollbackIncomplete
	default:
		return health.RollbackRestored
	}
}

// clause says what became of the install, in the fault record's own prose.
func (r rolledBack) clause() string {
	switch r.state() {
	case health.RollbackRestored:
		return "it was rolled back to the previous build"
	case health.RollbackIncomplete:
		return "its rollback did NOT restore the previous build"
	default:
		return "nothing was installed"
	}
}

// recordFailure opens the fault a failed deploy stands as, superseding the
// step's earlier one, and — when the rollback did not restore the previous
// build — the rollback's own fault beside it. An error that is no step's
// failure opens no step fault. A fault that cannot be recorded is ERROR: the
// failure is still the deploy's answer, and its own ERROR record stands.
func (d *Deployer) recordFailure(ctx context.Context, err error, rollback rolledBack) {
	ctx = context.WithoutCancel(ctx)
	if failure, ok := failureOf(err); ok {
		if failure.Step != health.DeployStepBuild {
			failure.Rollback = rollback.state()
		}
		d.openStepFault(ctx, failure, fmt.Sprintf("the deploy failed at its %s step; %s; the running build keeps serving: %v",
			failure.Step, rollback.clause(), err), err)
	} else {
		d.log.Debug(opFault, "the deploy's failure is no build, install or service restart; it opens no step fault", withCause(nil, err))
	}
	if rollback.err == nil {
		return
	}
	failure, ok := failureOf(rollback.err)
	if !ok {
		d.log.Error(opFault, "the rollback's failure is not a RollbackFailed; it opens no fault", withCause(nil, rollback.err))
		return
	}
	d.openStepFault(ctx, failure, fmt.Sprintf("the failed deploy's rollback did not restore the previous build: %v", rollback.err), rollback.err)
}

// openStepFault opens one step's fault, closing that step's earlier ones.
func (d *Deployer) openStepFault(ctx context.Context, failure health.DeployFailure, detail string, cause error) {
	fields := dlog.Context{"step": failure.Step, "rollback": failure.Rollback}
	d.closeStepFaults(ctx, failure.Step, "a later failure of the same step supersedes it")
	id, openErr := d.deps.Faults.OpenFault(ctx, wsm.Fault{
		Kind:     health.KindDeployFailed,
		Detail:   detail,
		Evidence: failure.Evidence(),
		OpenedAt: d.deps.Clock.Now(),
	})
	if openErr != nil {
		d.log.Error(opFault, "could not record the failed deploy as a fault", withCause(fields, openErr))
		return
	}
	d.log.Info(opFault, "recorded the failed deploy as a fault", merge(fields, dlog.Context{"fault": string(id), "cause": cause.Error()}))
}

// stepSucceeded closes every standing fault of a step this deploy got
// through.
func (d *Deployer) stepSucceeded(ctx context.Context, step string) {
	d.closeStepFaults(context.WithoutCancel(ctx), step, "a deploy got through the step")
}

// CloseEarlierFailures closes every `deploy_failed` fault standing when this
// daemon boots: an earlier daemon opened it, and no strip of this one draws
// it. A deploy that fails again reopens it. It is called once, by a daemon
// that owns its state (never a successor still joining, whose handle is
// read-only and whose incumbent closed each step's faults as its deploy got
// through them).
func (d *Deployer) CloseEarlierFailures(ctx context.Context) {
	d.closeStepFaults(ctx, "", "an earlier daemon left it standing, and no strip of this one draws it")
}

// closeStepFaults closes the standing daemon-scoped `deploy_failed` faults of
// one step, or of every step when step is empty. A read or a close that fails
// is ERROR and leaves the fault standing.
func (d *Deployer) closeStepFaults(ctx context.Context, step, why string) {
	fields := dlog.Context{"step": step, "why": why}
	open, err := d.deps.Faults.OpenFaults(ctx, wsm.FaultScope{Kind: health.KindDeployFailed})
	if err != nil {
		d.log.Error(opFault, "could not read the standing deploy faults to close them", withCause(fields, err))
		return
	}
	for _, f := range open {
		if f.Workspace != nil || (step != "" && health.DeployFailureOf(f).Step != step) {
			continue
		}
		faultFields := merge(fields, dlog.Context{"fault": string(f.ID), "fault_step": health.DeployFailureOf(f).Step})
		if err := d.deps.Faults.CloseFault(ctx, f.ID, d.deps.Clock.Now()); err != nil {
			d.log.Error(opFault, "could not close a standing deploy fault", withCause(faultFields, err))
			continue
		}
		d.log.Info(opFault, "closed a standing deploy fault", faultFields)
	}
}
