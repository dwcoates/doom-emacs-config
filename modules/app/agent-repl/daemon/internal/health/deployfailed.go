package health

import (
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/wsm"
)

// The steps a `deploy_failed` fault names, spelled as DaemonFaultDeployFailed's
// `step` oneof spells its arms.
const (
	// DeployStepBuild is the build into staging (its setup and the hashing of
	// what it staged included): nothing was installed.
	DeployStepBuild = "build"
	// DeployStepInstall is the install of a staged artifact: nothing was
	// restarted.
	DeployStepInstall = "install"
	// DeployStepRestartServices is the restart of a launchd service onto the
	// installed build.
	DeployStepRestartServices = "restart_services"
	// DeployStepRollback is the failed deploy's ROLLBACK, which did not
	// restore the previous build. It is a fault of its own, beside the fault
	// of the step that failed.
	DeployStepRollback = "rollback"
)

// What became of a failed step's install: a failure after the install began
// rolls back (owner ruling, 2026-09-28).
const (
	// RollbackNone is a failure that installed nothing, so nothing was rolled
	// back: a build, or an install that could not keep the previous build.
	RollbackNone = ""
	// RollbackRestored is a failure whose rollback restored the previous
	// build.
	RollbackRestored = "restored"
	// RollbackIncomplete is a failure whose rollback did not restore the
	// previous build; the rollback's own fault names why.
	RollbackIncomplete = "incomplete"
)

// RollbackClause says what became of a failed step's install, one of the
// Rollback constants, in the prose the fault record and the topbar's overlay
// share.
func RollbackClause(rollback string) string {
	switch rollback {
	case RollbackRestored:
		return "it was rolled back to the previous build"
	case RollbackIncomplete:
		return "its rollback did NOT restore the previous build"
	default:
		return "nothing was installed"
	}
}

// DeployFailure is what a `deploy_failed` fault records: the step, and the
// same facts the Deploy rpc's refusal for that step carries. It is the ONE
// place the fault's evidence is written and read, so the deploy that opens the
// fault and the reporter and the footer that render it cannot disagree about
// its keys.
type DeployFailure struct {
	// Step is one of the DeployStep constants.
	Step string
	// BuildStep is the build step that failed (`proto`, a build-frontend
	// target, `setup`, or the artifact whose hash could not be taken); set
	// for DeployStepBuild only.
	BuildStep string
	// Component is the component whose artifact could not be installed or
	// whose service did not come back; set for the other two steps.
	Component agentreplv1.DeployComponent
	// Detail is the failure's own account: the tail of the build's output,
	// the install's error, the restart's error.
	Detail string
	// Log is where the build's whole output is archived; build only.
	Log string
	// Rollback is what became of the install, one of the Rollback
	// constants; set for DeployStepInstall and DeployStepRestartServices.
	Rollback string
}

// The evidence keys a DeployFailure is recorded under.
const (
	keyStep      = "step"
	keyBuildStep = "build_step"
	keyComponent = "component"
	keyDetail    = "detail"
	keyLog       = "log"
	keyRollback  = "rollback"
)

// Evidence is the failure as a fault's evidence.
func (d DeployFailure) Evidence() map[string]string {
	out := map[string]string{keyStep: d.Step, keyDetail: d.Detail}
	switch d.Step {
	case DeployStepBuild:
		out[keyBuildStep] = d.BuildStep
		out[keyLog] = d.Log
	case DeployStepRollback:
		out[keyComponent] = d.Component.String()
	default:
		out[keyComponent] = d.Component.String()
		out[keyRollback] = d.Rollback
	}
	return out
}

// DeployFailureOf reads a recorded `deploy_failed` fault's evidence back.
func DeployFailureOf(f wsm.Fault) DeployFailure {
	return DeployFailure{
		Step:      f.Evidence[keyStep],
		BuildStep: f.Evidence[keyBuildStep],
		Component: agentreplv1.DeployComponent(agentreplv1.DeployComponent_value[f.Evidence[keyComponent]]),
		Detail:    f.Evidence[keyDetail],
		Log:       f.Evidence[keyLog],
		Rollback:  f.Evidence[keyRollback],
	}
}

// arm renders the failure as its typed arm, nil for a step the oneof does not
// spell: such a record answers with its detail line alone, as any fault whose
// kind has no arm does.
func (d DeployFailure) arm() *agentreplv1.DaemonFaultDeployFailed {
	switch d.Step {
	case DeployStepBuild:
		return &agentreplv1.DaemonFaultDeployFailed{Step: &agentreplv1.DaemonFaultDeployFailed_Build{
			Build: &agentreplv1.DeployBuildFailed{Step: d.BuildStep, Detail: d.Detail, Log: d.Log},
		}}
	case DeployStepInstall:
		return &agentreplv1.DaemonFaultDeployFailed{Step: &agentreplv1.DaemonFaultDeployFailed_Install{
			Install: &agentreplv1.DeployInstallFailed{Component: d.Component, Detail: d.Detail},
		}}
	case DeployStepRestartServices:
		return &agentreplv1.DaemonFaultDeployFailed{Step: &agentreplv1.DaemonFaultDeployFailed_RestartServices{
			RestartServices: &agentreplv1.DeployServiceRestartFailed{Component: d.Component, Detail: d.Detail},
		}}
	case DeployStepRollback:
		return &agentreplv1.DaemonFaultDeployFailed{Step: &agentreplv1.DaemonFaultDeployFailed_Rollback{
			Rollback: &agentreplv1.DeployRollbackFailed{Component: d.Component, Detail: d.Detail},
		}}
	default:
		return nil
	}
}

// DeployFailedDetail composes the ONE line the footer draws for a failed
// deploy: the step in words, what it failed on, what became of the install,
// and the last line of the failure's own account (the build's detail is the
// tail of its output, and the strip has one line to draw). "build webapp:
// error TS2322", "install store, rolled back: permission denied", "restart
// services sidecar, rollback failed: exit 5", "rollback daemon: EROFS".
func DeployFailedDetail(f wsm.Fault) string {
	d := DeployFailureOf(f)
	if d.Step == "" {
		return f.Detail
	}
	subject := d.BuildStep
	if d.Step != DeployStepBuild {
		subject = strings.ToLower(strings.TrimPrefix(d.Component.String(), "DEPLOY_COMPONENT_"))
	}
	if subject == d.Step {
		// A builder that is one step (the operator's override) names the
		// step the build itself; it is said once.
		subject = ""
	}
	head := strings.TrimSpace(strings.ReplaceAll(d.Step, "_", " ") + " " + subject)
	switch d.Rollback {
	case RollbackRestored:
		head += ", rolled back"
	case RollbackIncomplete:
		head += ", rollback failed"
	}
	tail := lastLine(d.Detail)
	if tail == "" {
		return head
	}
	return head + ": " + tail
}
