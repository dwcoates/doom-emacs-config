// Package rollout is the self-reload trigger consumer: daemon handover, the
// adopt rendezvous, the shim relaunch engine, the build-staleness bounce, the
// asset origin's intent manifest and the reload_webapp push.
//
// The handover WAITS FOREVER for its participants, warning every ten minutes
// rather than giving up. The relaunch engine is the ONE engine for both a
// self-merge shim change and a build-staleness bounce, and it deliberately
// does NOT use Hibernate: it kills gracefully and reaps. See ARCHITECTURE.md
// "rollout".
package rollout

import (
	"context"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
)

// RelaunchReason names why the relaunch engine was invoked. The engine is one
// path; the reason is what the log and the hold record say.
type RelaunchReason string

// The relaunch reasons.
const (
	// ReasonShimChanged is a self-merge that landed a shim change.
	ReasonShimChanged RelaunchReason = "shim_changed"
	// ReasonBuildStale is the build-staleness bounce.
	ReasonBuildStale RelaunchReason = "build_stale"
	// ReasonRestartVerb is an operator's RestartWorkspace.
	ReasonRestartVerb RelaunchReason = "restart_verb"
)

// Controller is the rollout surface.
type Controller interface {
	// Trigger classifies landed commits by subsystem prefix, invokes
	// bin/deploy-all.sh --no-bounce ONCE, and then takes the per-subsystem
	// action. Merge calls it only after lease release and terminal
	// publication.
	Trigger(ctx context.Context, landed []gitclient.Commit) error
	// Handover spawns the successor with -joining, announces the stand-down on
	// WatchDaemon, transfers each workspace at freeness (quiesce, intent
	// manifest, the `transferred` push), times the adoption window, and waits
	// forever with ten-minute holdout warnings.
	Handover(ctx context.Context) error
	// AdoptHost is Emacs's half of the rendezvous, called on the NEW daemon.
	// It completes when every expected participant recorded at announcement
	// has called; a headless daemon expects zero participants and completes at
	// once.
	AdoptHost(ctx context.Context, ws ids.WorkspaceID) error
	// AdoptWeb is the webview's half of the same rendezvous.
	AdoptWeb(ctx context.Context, ws ids.WorkspaceID) error
	// RelaunchShim bounces one workspace's shim: prelaunch inert, wait for
	// freeness, take the restart-pending hold, stand the old one down with a
	// graceful KillSession{force:false} (NOT Hibernate), pass the reap gate,
	// StartSession(resume), then drain the holds.
	RelaunchShim(ctx context.Context, ws ids.WorkspaceID, reason RelaunchReason) error
	// ReloadWebapp pushes reload_webapp to one workspace's webview.
	ReloadWebapp(ctx context.Context, ws ids.WorkspaceID) error
	// ExpectedParticipants reports who the announcement recorded as owing an
	// adoption call for a workspace, which is what makes the rendezvous
	// terminate.
	ExpectedParticipants(ws ids.WorkspaceID) int
}

// Deps are the controller's collaborators. ARCHITECTURE.md names the
// responsibilities rather than the wiring; the minimum the contract implies is
// the deploy script, the daemon's own argv for the successor spawn, and the
// hooks for freeness, stand-down and bring-up.
type Deps struct {
	// DeployScript is bin/deploy-all.sh, invoked once per trigger with
	// --no-bounce.
	DeployScript string
	// SelfExe is the daemon binary a successor is spawned from.
	SelfExe string
	// IntentManifest is the stand-down manifest path.
	IntentManifest string
	// Git supplies the changed paths a trigger classifies by.
	Git gitclient.Git
	// Log is the controller's logger.
	Log dlog.Surfaces
}

// New builds the controller.
func New(deps Deps) (Controller, error) {
	return nil, notimpl.Err
}
