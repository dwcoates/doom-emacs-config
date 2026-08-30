// Package rollout is the self-reload trigger consumer: daemon handover, the
// adopt rendezvous, the shim relaunch engine, the build-staleness bounce, the
// asset origin's intent manifest and the reload_webapp push.
//
// The handover WAITS FOREVER for its participants, warning every ten minutes
// rather than giving up. The relaunch engine is the ONE engine for both a
// self-merge shim change and a build-staleness bounce, and it deliberately
// does NOT use Hibernate: it kills gracefully and reaps. See ARCHITECTURE.md
// "rollout".
//
// NO DAEMON-TO-DAEMON CHANNEL. The outgoing and incoming daemons coordinate
// through three things only: the Emacs relay (the announcement and the adopt
// verbs), WSM facts (serving ownership), and the shim-held kernel locks. The
// INTENT MANIFEST is a WSM-adjacent file in the shared state root, not a
// channel: the outgoing daemon writes it and then never reads it again.
//
// AGENT_REPL_SELF_REPO_DIR does NOT disable the trigger. Test safety comes
// from AGENT_REPL_DEPLOY_SCRIPT naming a fake deploy script, so a landed range
// reaching the trigger and the trigger reaching the deploy chain is assertable
// end to end.
package rollout

import (
	"context"
	"errors"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
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
	// Handover spawns the successor with --joining, announces the stand-down on
	// WatchDaemon, transfers each workspace at freeness (quiesce, intent
	// manifest, the `transferred` push), times the adoption window, and waits
	// forever with ten-minute holdout warnings.
	Handover(ctx context.Context) error
	// Join is the JOINING daemon's half: read the intent manifest, reconcile it
	// against the kernel locks, arm the rendezvous, and adopt every headless
	// workspace at once. A daemon that is not joining finds no manifest and
	// says so at DEBUG.
	Join(ctx context.Context) error
	// AdoptHost is Emacs's half of the rendezvous, called on the NEW daemon.
	// It completes when every expected participant recorded at announcement
	// has called; a headless daemon expects zero participants and completes at
	// once.
	AdoptHost(ctx context.Context, ws ids.WorkspaceID) error
	// AdoptWeb is the webview's half of the same rendezvous. THE WEB SIDE NEVER
	// REDIALS: the reloaded page calls this once at boot, so
	// ErrNoTransferAnnounced is the ORDINARY answer on every non-handover page
	// boot and is recorded at INFO, never as a warning or a fault.
	AdoptWeb(ctx context.Context, ws ids.WorkspaceID) error
	// RelaunchShim bounces one workspace's shim: prelaunch inert, wait for
	// freeness, take the restart-pending hold, stand the old one down with a
	// graceful KillSession{force:false} (NOT Hibernate), pass the reap gate,
	// StartSession(resume), then drain the holds.
	RelaunchShim(ctx context.Context, ws ids.WorkspaceID, reason RelaunchReason) error
	// CheckStaleness compares a session's reported shim build against the
	// deploy stamp and schedules the build-staleness bounce at freeness when
	// they disagree. A shim on the deployed build is left alone.
	CheckStaleness(ctx context.Context, ws ids.WorkspaceID, reportedSHA string) error
	// ReloadWebapp pushes reload_webapp to one workspace's webview.
	ReloadWebapp(ctx context.Context, ws ids.WorkspaceID) error
	// ExpectedParticipants reports who the announcement recorded as owing an
	// adoption call for a workspace, which is what makes the rendezvous
	// terminate.
	ExpectedParticipants(ws ids.WorkspaceID) int
	// Reconcile reads the intent manifest against the kernel locks actually
	// held and answers one disposition PER SESSION. PRESERVED, ROLLED, DIED
	// and UNKNOWN are never collapsed and never counted.
	Reconcile(ctx context.Context) ([]Disposition, error)
}

// Deps are the controller's collaborators.
type Deps struct {
	// DeployScript is bin/deploy-all.sh, invoked once per trigger with
	// --no-bounce. AGENT_REPL_DEPLOY_SCRIPT overrides it; New applies that
	// override itself, so no wiring can forget to.
	DeployScript string
	// Deploy runs the deploy script. Injected so a trigger is exercised against
	// a scripted script rather than the real chain: THE ROLLOUT INVOKES THE ONE
	// DEPLOY CHAIN AND NEVER A SECOND BUILD PATH, and a test proves that by
	// asserting what this was asked to run.
	Deploy ScriptRunner
	// SelfExe is the daemon binary a successor is spawned from.
	SelfExe string
	// SelfRepoDir is the daemon's OWN checkout: where the landed range's
	// changed paths are read, and the root the subsystem prefixes are relative
	// to.
	SelfRepoDir string
	// SelfAddress is this daemon's own `127.0.0.1:<port>`, handed to the
	// successor as --joining and never inferred by it.
	SelfAddress string
	// Instance identifies this daemon process for serving ownership.
	Instance ids.InstanceID
	// StateDir is the state root; the deploy run's output is archived beneath
	// it and the successor reports its address in it.
	StateDir string
	// IntentManifest is the stand-down manifest path.
	IntentManifest string
	// Git supplies the changed paths a trigger classifies by.
	Git gitclient.Git
	// DB holds the lease, serving ownership, the session facts the manifest is
	// built from, and the fault records the accounting writes.
	DB wsm.DB
	// Spawner starts the successor and waits for it to report its address.
	Spawner SuccessorSpawner
	// Announcer publishes the WatchDaemon shutdown announcement.
	Announcer Announcer
	// Pusher relays the per-workspace handover and webapp pushes.
	Pusher WorkspacePusher
	// Participants is the expected-participant snapshot: who holds a
	// workspace's two per-workspace streams at announcement.
	Participants ParticipantSource
	// Quiesce holds ALL intake for one workspace: from the transfer notice on,
	// this daemon does no work for it.
	Quiesce QuiesceFunc
	// DrainIntake releases the held intake IN ORDER once the successor owns the
	// workspace.
	DrainIntake DrainIntakeFunc
	// Freeness answers, and waits for, a workspace's freeness — the
	// sessionwatcher's answer.
	Freeness Freeness
	// Shims is how the controller reaches the shim fleet.
	Shims ShimFleet
	// LockProbe probes a workspace's shim-held kernel lock. It is what makes a
	// headless transfer and the bounce accounting possible without any channel.
	LockProbe LockProbeFunc
	// PublishViews republishes a workspace's whole views once it is owned.
	PublishViews PublishViewsFunc
	// WriteDaemonAddr writes daemon.addr. A joining daemon calls it ONLY once
	// every workspace is adopted, and otherwise never writes it.
	WriteDaemonAddr WriteDaemonAddrFunc
	// DeployStamp reads the deployed build's sha — daemon/bin/.built-sha, which
	// the deploy chain writes from the checkout's own revision.
	DeployStamp DeployStampFunc
	// SessionBuildSHA reports the build a workspace's live session says it is
	// running (conversation.v1 SessionRuntime.shim_build_sha, routed by the
	// sessionwatcher). It is a hook rather than a WSM column because it is a
	// fact of the RUNNING process, not a durable fact of the session: a shim
	// that dies takes its build with it.
	SessionBuildSHA SessionBuildFunc
	// ColdGate raises the ordinary cold gate when a resume answers `cold`.
	ColdGate ColdGateFunc
	// Exit performs the daemon's orderly exit after the last transfer.
	Exit ExitFunc
	// ExpectedOutage is the bounded outage the announcement states, so clients
	// size their quiet window instead of reading the link death as unexplained.
	ExpectedOutage time.Duration
	// AdoptionWindow is how long the OUTGOING daemon gives an adoption before
	// recording the workspace's own fault. Expiry is remediated as it comes up:
	// there is deliberately no abort and no retry machinery.
	AdoptionWindow time.Duration
	// HoldoutWarnEvery is the never-free warning cadence. A never-free
	// workspace is waited on FOREVER; this is only how often it is named.
	HoldoutWarnEvery time.Duration
	// StandDownWindow is how long a gracefully killed shim has before the
	// force-kill. Its expiry is a LOUD log, not an invariant.
	StandDownWindow time.Duration
	// Clock is the controller's view of time.
	Clock Clock
	// Log is the controller's logger.
	Log dlog.Surfaces
}

// ScriptRunner runs one command in a directory and reports its combined output
// with the process's exit code. It is the merge orchestrator's spelling, so the
// two script gates in the daemon are driven the same way.
type ScriptRunner interface {
	// Run executes argv in dir and returns the combined stdout and stderr with
	// the process's exit code.
	Run(ctx context.Context, dir string, argv []string) (output string, exitCode int, err error)
}

// SuccessorSpawner starts the blue-green successor and waits for it to report
// its address.
type SuccessorSpawner interface {
	// Spawn starts `<self exe> --joining <incumbent address>` with THIS
	// process's environment and returns the successor's own address once it has
	// reported it. The report travels through <state>/joining.addr, which the
	// successor writes atomically the moment its listener is bound.
	Spawn(ctx context.Context, incumbentAddress string) (successorAddress string, err error)
}

// Announcer publishes this daemon's WatchDaemon shutdown announcement.
type Announcer interface {
	// ShutdownAnnounced publishes the stand-down, address and all.
	ShutdownAnnounced(push *agentreplv1.DaemonShutdownAnnounced)
}

// WorkspacePusher relays the per-workspace pushes of a rollout. Both are
// PUSHES, never terminal frames: the client cancels its own streams after
// acting.
type WorkspacePusher interface {
	// PushTransferred pushes `transferred` on the workspace's HOST stream and
	// `transferred{address}` on its WEB stream. The address rides the web arm
	// because a webview has no daemon-level stream to have learned it from.
	PushTransferred(ws ids.WorkspaceID, successorAddress string)
	// PushReloadWebapp pushes the EMPTY reload_webapp arm on the workspace's
	// host stream: no address rides it, because the daemon is not changing.
	PushReloadWebapp(ws ids.WorkspaceID)
}

// ParticipantSource answers who holds a workspace's two per-workspace streams.
// The snapshot is taken AT ANNOUNCEMENT and is what makes the rendezvous
// terminate.
type ParticipantSource interface {
	// Participants reports which of the workspace's two streams have a holder
	// right now.
	Participants(ws ids.WorkspaceID) Participants
}

// Participants is one workspace's stream occupancy. The HOST side is Emacs,
// which is singular. The WEB side is one slot however many webviews are open:
// the ruling is that the reloaded page's first AdoptWebWorkspace, from any
// connection, satisfies it.
type Participants struct {
	// Host reports whether a WatchHostWorkspace stream is held.
	Host bool
	// Web reports whether a WatchWebWorkspace stream is held.
	Web bool
}

// Count is how many adoption calls the pair owes.
func (p Participants) Count() int {
	n := 0
	if p.Host {
		n++
	}
	if p.Web {
		n++
	}
	return n
}

// QuiesceFunc holds ALL intake for one workspace: queue, views, anything. From
// the transfer notice on the outgoing daemon does no work for it.
type QuiesceFunc func(ctx context.Context, ws ids.WorkspaceID) error

// DrainIntakeFunc releases a quiesced workspace's held intake IN ORDER, on the
// daemon that now owns it.
type DrainIntakeFunc func(ctx context.Context, ws ids.WorkspaceID) error

// PublishViewsFunc republishes one workspace's whole views after adoption, so
// the re-attaching clients land on fresh state rather than on nothing.
type PublishViewsFunc func(ctx context.Context, ws ids.WorkspaceID) error

// WriteDaemonAddrFunc writes daemon.addr. It exists as a hook because a
// JOINING daemon must not write it until it owns every workspace.
type WriteDaemonAddrFunc func(ctx context.Context) error

// DeployStampFunc reads the deployed build's sha from daemon/bin/.built-sha.
type DeployStampFunc func() (string, error)

// SessionBuildFunc reports the build a workspace's live session is running. The
// bool is false when no live session reports one, which leaves the shim alone.
type SessionBuildFunc func(ws ids.WorkspaceID) (sha string, known bool)

// ColdGateFunc raises the ordinary cold gate for a resume that answered `cold`.
type ColdGateFunc func(ctx context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) error

// ExitFunc performs the daemon's orderly exit.
type ExitFunc func(ctx context.Context) error

// LockProbeFunc probes one workspace's shim-held kernel lock. StateUnknown is
// NEVER read as free.
type LockProbeFunc func(workspaceDir string) (sessionlock.State, error)

// Freeness answers a workspace's freeness and lets a caller WAIT for it without
// polling.
type Freeness interface {
	// Free reports freeness right now: no turn in flight and no live detached
	// work.
	Free(ws ids.WorkspaceID) bool
	// AwaitFree blocks until the workspace is free, or until ctx ends.
	AwaitFree(ctx context.Context, ws ids.WorkspaceID) error
}

// ShimFleet is how the controller reaches the shim processes. It is a hook
// rather than the supervisor itself because the socket paths, the account root
// and the log sink belong to the component that spawns shims for every other
// reason too.
type ShimFleet interface {
	// Client answers the workspace's current shim client, false when none is
	// up.
	Client(ws ids.WorkspaceID) (shimclient.Client, bool)
	// Prelaunch brings up a NEW shim for the workspace on a fresh socket,
	// INERT BY CONSTRUCTION: connected but with no session started, so it
	// coexists with the old one indefinitely.
	Prelaunch(ctx context.Context, ws ids.WorkspaceID) (shimclient.Client, error)
	// Install makes c the workspace's shim client, retiring whatever was there.
	Install(ctx context.Context, ws ids.WorkspaceID, c shimclient.Client) error
	// Adopt dials the workspace's ALREADY RUNNING shim without spawning — the
	// successor's half of a transfer, and a crash boot's surviving process.
	Adopt(ctx context.Context, ws ids.WorkspaceID) (shimclient.Client, error)
	// Resume runs StartSession(resume) on c. A cold context is an ANSWER, not
	// an error: it comes back on Resumed.Cold for the ordinary cold gate.
	Resume(ctx context.Context, ws ids.WorkspaceID, c shimclient.Client) (Resumed, error)
}

// Resumed is what a resume answered.
type Resumed struct {
	// Cold is set when the resume answered `cold` and nothing was resumed.
	Cold *conversationv1.SessionCold
}

// Clock is the controller's view of time, injected so every window in this
// package is driven by the test rather than the wall clock.
type Clock interface {
	// Now is the current instant.
	Now() time.Time
	// After yields one value after d has passed.
	After(d time.Duration) <-chan time.Time
}

// SystemClock is the production Clock.
type SystemClock struct{}

// Now is the wall clock's instant.
func (SystemClock) Now() time.Time { return time.Now() }

// After is time.After.
func (SystemClock) After(d time.Duration) <-chan time.Time { return time.After(d) }

// The rendezvous refusals. Each is one arm of AdoptHostWorkspaceError and
// AdoptWebWorkspaceError; the server maps them onto the typed arm.
var (
	// ErrNoTransferAnnounced is the ORDINARY answer on every non-handover boot:
	// no transfer was announced for this workspace.
	ErrNoTransferAnnounced = errors.New("rollout: no transfer was announced for this workspace")
	// ErrParticipantNotExpected is a caller whose stream was not open at
	// announcement.
	ErrParticipantNotExpected = errors.New("rollout: this participant's stream was not open at announcement")
	// ErrNotYetAdopted is answered while adoption is still in progress.
	ErrNotYetAdopted = errors.New("rollout: this workspace is not adopted yet")
)

// New builds the controller.
func New(deps Deps) (Controller, error) {
	if deps.Log == nil {
		return nil, errors.New("rollout: log surfaces are required")
	}
	if deps.DB == nil {
		return nil, errors.New("rollout: a state client is required")
	}
	if deps.Clock == nil {
		deps.Clock = SystemClock{}
	}
	if deps.ExpectedOutage <= 0 {
		deps.ExpectedOutage = DefaultExpectedOutage
	}
	if deps.AdoptionWindow <= 0 {
		deps.AdoptionWindow = DefaultAdoptionWindow
	}
	if deps.HoldoutWarnEvery <= 0 {
		deps.HoldoutWarnEvery = DefaultHoldoutWarnEvery
	}
	if deps.StandDownWindow <= 0 {
		deps.StandDownWindow = DefaultStandDownWindow
	}
	deps.DeployScript = ResolveDeployScript(deps.DeployScript)
	c := &controller{deps: deps, log: deps.Log.Global(), rendezvous: make(map[ids.WorkspaceID]*entry)}
	c.log.Debug(opNew, "the rollout controller is up", dlog.Context{
		"deploy_script":      deps.DeployScript,
		"adoption_window":    deps.AdoptionWindow.String(),
		"holdout_warn_every": deps.HoldoutWarnEvery.String(),
		"stand_down_window":  deps.StandDownWindow.String(),
	})
	return c, nil
}
