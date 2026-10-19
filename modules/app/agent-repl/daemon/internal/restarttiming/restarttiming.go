// Package restarttiming is the vocabulary of how long agent-repl's RESTARTS
// take, for the daemon's always-on metrics (docs/PLAN-runtime-metrics.md,
// "Restarts").
//
// A restart is anything that takes what serves the user down and brings it
// back: a deploy, a daemon's boot, a store or sidecar restart, a workspace's
// session start during a boot or a takeover, a shim replacement, and a
// workspace's transfer to a successor daemon. Each one is timed by the code
// that sees it START and END, in the one process that runs it, and stated
// here as one Observation.
//
// AN OBSERVATION CAN ONLY BE BUILT RIGHT. Its fields are unexported and each
// kind of restart has its own constructor taking that kind's own phase type,
// so a phase of one restart can never be recorded under another, and every
// label value comes from a closed set declared here or, for a shim
// replacement's reason, from rollout.RelaunchReason.
//
// It is a leaf of its own so the parties that time restarts (deploy, rollout,
// bringup, the daemon's boot) and the party that publishes the series
// (runtimemetrics) share one spelling without importing each other.
package restarttiming

import (
	"fmt"
	"time"

	"agent-repld/internal/ids"
)

// Restart names the kind of restart, the series' `restart` label.
type Restart string

// The restarts.
const (
	// Deploy is a deploy: build, install, the services, the decisions, and,
	// when the daemon moves, the handover to its successor.
	Deploy Restart = "deploy"
	// DaemonBoot is one daemon process from its start until its sessions are
	// up.
	DaemonBoot Restart = "daemon_boot"
	// ServiceRestart is the store or the sidecar restarted until it serves.
	ServiceRestart Restart = "service_restart"
	// WorkspaceStart is ONE workspace's session started by a boot's or a
	// takeover's bring-up, until the session is live.
	WorkspaceStart Restart = "workspace_start"
	// ShimRestart is ONE workspace's shim replaced by a fresh one.
	ShimRestart Restart = "shim_restart"
	// Transfer is ONE workspace moved from this daemon to its successor.
	Transfer Restart = "handover_transfer"
)

// Outcome is how the timed restart or phase ended, the `outcome` label.
type Outcome string

// The outcomes.
const (
	OK     Outcome = "ok"
	Failed Outcome = "failed"
)

// OutcomeOf answers OK for a nil error and Failed for any other.
func OutcomeOf(err error) Outcome {
	if err != nil {
		return Failed
	}
	return OK
}

// DeployPhase is one phase of a deploy.
type DeployPhase string

// The deploy phases.
const (
	// DeployBuild is the build into staging.
	DeployBuild DeployPhase = "build"
	// DeployInstall is the staged build installed over the running one.
	DeployInstall DeployPhase = "install"
	// DeployServices is the out-of-date store and sidecar restarted.
	DeployServices DeployPhase = "restart_services"
	// DeployDecide is every remaining component decided: Emacs and the
	// webviews told to reload, stale shims bounced or registered, and a
	// moving daemon's handover accepted.
	DeployDecide DeployPhase = "decide"
	// DeployHandover is a moving daemon's handover, from its intent manifest
	// to the successor having taken every workspace over. The successor
	// records it.
	DeployHandover DeployPhase = "handover"
	// DeployTotal is the whole deploy, from its start until `updated`: said by
	// the daemon that stays, or by the successor of one that moved.
	DeployTotal DeployPhase = "total"
)

// BootPhase is one phase of a daemon's boot, each measured from the
// process's own start.
type BootPhase string

// The boot phases.
const (
	// BootClaim is the boot claim held: the process may serve its state root.
	BootClaim BootPhase = "claim"
	// BootReconciled is the boot reconciliation finished.
	BootReconciled BootPhase = "reconciled"
	// BootServing is the daemon serving: its address advertised, so every
	// client dials it.
	BootServing BootPhase = "serving"
	// BootSessionsUp is the bring-up finished: every pending open workspace's
	// session started (or failed).
	BootSessionsUp BootPhase = "sessions_up"
)

// BootMode is how the daemon booted, the boot's `cause` label.
type BootMode string

// The boot modes.
const (
	// BootCold is an ordinary boot, owning its state root from the start.
	BootCold BootMode = "cold"
	// BootSuccessor is a successor spawned by a handover, joining.
	BootSuccessor BootMode = "successor"
)

// Service is a launchd service a restart timed, the `phase` of a
// ServiceRestart.
type Service string

// The services.
const (
	Store   Service = "store"
	Sidecar Service = "sidecar"
)

// StartVia is the bring-up that started a workspace's session, its `cause`.
type StartVia string

// The bring-ups.
const (
	// StartBoot is a daemon's own boot.
	StartBoot StartVia = "boot"
	// StartTakeover is a successor taking workspaces over.
	StartTakeover StartVia = "takeover"
)

// StartPhase is what a workspace's start was, its `phase`.
type StartPhase string

// The start phases.
const (
	// StartFresh is a session started anew or resumed.
	StartFresh StartPhase = "start"
	// StartWake is a hibernated session woken.
	StartWake StartPhase = "wake"
)

// ShimPhase is one step of a shim replacement.
type ShimPhase string

// The shim replacement steps, in the order they run.
const (
	// ShimWait is the time from the replacement being asked until it runs:
	// the workspace's own work ending, and a restart slot coming free.
	ShimWait ShimPhase = "wait"
	// ShimPrelaunch is the fresh shim spawned, inert.
	ShimPrelaunch ShimPhase = "prelaunch"
	// ShimStandDown is the old shim stood down and confirmed gone.
	ShimStandDown ShimPhase = "stand_down"
	// ShimResume is the fresh shim installed and its session resumed.
	ShimResume ShimPhase = "resume"
	// ShimRun is the whole replacement, from running until it finished: the
	// time the workspace is drained and takes no prompt.
	ShimRun ShimPhase = "run"
)

// TransferPhase is one part of a workspace's transfer.
type TransferPhase string

// The transfer phases.
const (
	// TransferWait is the time from the transfer being asked until it runs.
	TransferWait TransferPhase = "wait"
	// TransferRun is the transfer itself, the time the workspace is drained
	// on this daemon.
	TransferRun TransferPhase = "run"
)

// Observation is one timed restart or phase. Build one with the constructor
// of its restart; the zero value is not an observation.
type Observation struct {
	restart   Restart
	phase     string
	cause     string
	outcome   Outcome
	workspace ids.WorkspaceID
	seconds   float64
}

// Restart answers the observation's restart.
func (o Observation) Restart() Restart { return o.restart }

// Phase answers the observation's phase.
func (o Observation) Phase() string { return o.phase }

// Cause answers why or how the restart ran: a boot's mode, a bring-up's
// kind, a shim replacement's reason. Empty for a restart that has none.
func (o Observation) Cause() string { return o.cause }

// Outcome answers how it ended.
func (o Observation) Outcome() Outcome { return o.outcome }

// Workspace answers the workspace a per-workspace restart timed, empty for
// one that is not.
func (o Observation) Workspace() ids.WorkspaceID { return o.workspace }

// Seconds answers the duration.
func (o Observation) Seconds() float64 { return o.seconds }

func observation(r Restart, phase, cause string, outcome Outcome, ws ids.WorkspaceID, d time.Duration) Observation {
	// A NEGATIVE DURATION IS A DEFECT IN THE CALLER: every duration here is
	// measured on one process's monotonic clock, or between two instants of
	// one host's wall clock taken in order.
	if d < 0 {
		panic(fmt.Sprintf("restarttiming: %s %s took %v, which is not a duration", r, phase, d))
	}
	return Observation{restart: r, phase: phase, cause: cause, outcome: outcome, workspace: ws, seconds: d.Seconds()}
}

// OfDeploy is one deploy phase.
func OfDeploy(p DeployPhase, outcome Outcome, d time.Duration) Observation {
	return observation(Deploy, string(p), "", outcome, "", d)
}

// OfBoot is one boot phase, measured from the process's start.
func OfBoot(p BootPhase, mode BootMode, d time.Duration) Observation {
	return observation(DaemonBoot, string(p), string(mode), OK, "", d)
}

// OfService is one service restart.
func OfService(s Service, outcome Outcome, d time.Duration) Observation {
	return observation(ServiceRestart, string(s), "", outcome, "", d)
}

// OfWorkspaceStart is one workspace's session start during a bring-up.
func OfWorkspaceStart(ws ids.WorkspaceID, p StartPhase, via StartVia, outcome Outcome, d time.Duration) Observation {
	return observation(WorkspaceStart, string(p), string(via), outcome, ws, d)
}

// OfShimRestart is one step of one workspace's shim replacement. reason is
// the replacement's rollout.RelaunchReason, a closed set.
func OfShimRestart(ws ids.WorkspaceID, p ShimPhase, reason string, outcome Outcome, d time.Duration) Observation {
	return observation(ShimRestart, string(p), reason, outcome, ws, d)
}

// OfTransfer is one part of one workspace's transfer.
func OfTransfer(ws ids.WorkspaceID, p TransferPhase, outcome Outcome, d time.Duration) Observation {
	return observation(Transfer, string(p), "", outcome, ws, d)
}

// Recorder records restart observations. runtimemetrics.Surface is the
// production one; its off surface records nothing. Observe never blocks on
// anything but its own short lock.
type Recorder interface {
	ObserveRestart(o Observation)
}
