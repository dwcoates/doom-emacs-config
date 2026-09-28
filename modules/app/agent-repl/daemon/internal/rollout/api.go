// Package rollout is the deploy's ACTING half: the daemon handover, the adopt
// rendezvous, the shim bounce engine, the build-staleness bounce, the intent
// manifest and the reload_webapp push. The deploy (internal/deploy) builds and
// decides WHAT is out of date; this package replaces it, each piece WHEN it
// may.
//
// EVERY SHIM BOUNCE GOES THROUGH THE PROMPT QUEUE'S BOUNCE REGISTRY, and so
// does every handover transfer. The queue owns dispatch, so it is the one
// place "the workspace is free" and "replace what serves it" can be decided
// as one step: a free workspace is bounced at once, a busy one is registered
// and taken on its own freeness edge, and nothing waits per workspace here.
// The bounce engine itself deliberately does NOT use Hibernate: it kills
// gracefully and reaps. See ARCHITECTURE.md "rollout".
//
// NO DAEMON-TO-DAEMON CHANNEL. The outgoing and incoming daemons coordinate
// through three things only: the Emacs relay (the announcement and the adopt
// verbs), WSM facts (serving ownership), and the shim-held kernel locks. The
// INTENT MANIFEST is a WSM-adjacent file in the shared state root, not a
// channel: the outgoing daemon writes it and then never reads it again.
package rollout

import (
	"context"
	"errors"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/clock"
	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// RelaunchReason names why the bounce engine was invoked. The engine is one
// path; the reason is what the log and the hold record say.
type RelaunchReason string

// The bounce reasons.
const (
	// ReasonBuildStale is the build-staleness bounce: the shim reports a build
	// that is not the installed bundle's.
	ReasonBuildStale RelaunchReason = "build_stale"
	// ReasonRestartVerb is an operator's RestartWorkspace.
	ReasonRestartVerb RelaunchReason = "restart_verb"
	// ReasonShimLogCeiling is dlog forcing a process roll after shim.log
	// reached its hard ceiling.
	ReasonShimLogCeiling RelaunchReason = "shim_log_hard_ceiling"
	// ReasonHandoverTransfer is a workspace handed to a successor daemon. It is
	// a bounce of what SERVES the workspace (the daemon), not of its shim, and
	// it keeps the workspace drained on this daemon afterwards.
	ReasonHandoverTransfer RelaunchReason = "handover_transfer"
)

// Controller is the rollout surface.
type Controller interface {
	// HandOver begins a blue-green handover: spawn the successor with
	// --joining, announce the stand-down on WatchDaemon, write the intent
	// manifest, and ask the bounce registry to TRANSFER every served workspace
	// — each at its own freeness, independently, or all at once when forced.
	// It answers the handover's ACCEPTANCE; the transfers and the exit run on
	// the daemon's lifetime. It refuses with *ErrAlreadyRollingOut while a
	// handover is in flight, and with ErrJoining on a joining successor.
	HandOver(ctx context.Context, force bool) (HandoverAcceptance, error)
	// Restart rolls a fresh build out STOP-THEN-START, for a build whose
	// state layout differs from this one's (a joining successor cannot carry
	// an older layout forward on its read-only handle). Every served
	// workspace's serving stands down through the bounce registry at its own
	// freeness (all at once when forced), its shim detached and left running;
	// then a replacement is spawned on the fresh binary, waiting on the boot
	// claim, and this daemon exits. A restart that cannot finish takes every
	// workspace back and keeps serving. It shares HandOver's one slot and
	// refusals.
	Restart(ctx context.Context, force bool) (HandoverAcceptance, error)
	// BounceShim asks the bounce registry to replace one workspace's shim with
	// a fresh one on the installed bundle: at once when nothing is in flight or
	// force is set, else when the workspace's work ends. done, when set, is
	// told how the bounce ended.
	BounceShim(ctx context.Context, ws ids.WorkspaceID, reason RelaunchReason, force bool, done func(error)) (bounce.Decision, error)
	// ShimReported takes one live shim's build report — every shim reports it
	// on the opening diagnostics of every watch — and bounces the shim through
	// the registry when the build is not the installed bundle's. It never
	// blocks its caller, which is a stream router holding its own lock.
	ShimReported(ws ids.WorkspaceID, build string)
	// CheckStaleness re-judges a workspace's last reported shim build against
	// the installed bundle, bouncing it through the registry when they differ.
	// It is what a deploy and a mount call; neither waits on the bounce.
	CheckStaleness(ctx context.Context, ws ids.WorkspaceID, force bool) (StaleCheck, error)
	// Joining reports that this daemon is a successor still joining: it serves
	// nothing yet, so it has nothing to deploy onto.
	Joining() bool
	// RollingOut reports whether a handover is in flight, and the workspaces it
	// has not transferred yet.
	RollingOut() ([]ids.WorkspaceID, bool)
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
	// ReloadWebapp pushes reload_webapp to one workspace's webview.
	ReloadWebapp(ctx context.Context, ws ids.WorkspaceID) error
	// ExpectedParticipants reports who the announcement recorded as owing an
	// adoption call for a workspace, which is what makes the rendezvous
	// terminate.
	ExpectedParticipants(ws ids.WorkspaceID) int
	// Standing answers a workspace's SERVING STANDING on this daemon, which is
	// what every per-workspace rpc refuses on before it delegates. The two
	// non-owned standings are the two the rollout itself creates: a workspace
	// transferred away, and one a joining daemon has not adopted yet.
	Standing(ws ids.WorkspaceID) Standing
	// SuccessorAddress answers the address a transferred workspace moved to,
	// empty when no handover is in flight.
	SuccessorAddress() string
	// Reconcile reads the intent manifest against the kernel locks actually
	// held and answers one disposition PER SESSION. PRESERVED, ROLLED, DIED
	// and UNKNOWN are never collapsed and never counted.
	//
	// survivors.Adopted is the SESSIONS whose surviving shim this boot adopted
	// — never an inert survivor. A shim takes the workspace lock at StartSession and not
	// at process start (agent-shim/claude/shim/src/engine/session.ts, "it lands
	// HERE rather than at process start because an inert shim owns no
	// conversation"), so a live shim whose lock reads FREE carries NO SESSION:
	// there is no process whose survival could be judged, exactly as for a
	// manifest entry that reads IntentNoSession. The boot passes only the
	// lock-held survivors, and passing an inert one would raise a fault over a
	// workspace that never had a session to lose.
	//
	// AN ADOPTED SESSION NEVER RECORDS bounce_unknown: its adoption accounts
	// for it (PRESERVED), with a manifest or without one.
	//
	// With NO MANIFEST — a crash or a force-kill, where the outgoing daemon
	// never stood down — each session in survivors.Unadopted (the lock says it
	// may survive, and this boot did not adopt it) is one whose bounce nobody
	// accounted for, and BOUNCE ACCOUNTABILITY says which sessions were left
	// unaccounted is surfaced per workspace rather than passed over, so each
	// gets an OPEN bounce_unknown fault.
	Reconcile(ctx context.Context, survivors Survivors) ([]Disposition, error)
}

// HandoverAcceptance is a handover that was ACCEPTED and is under way. It is
// not a completion: every transfer waits on its workspace's freeness unless
// the handover was forced.
type HandoverAcceptance struct {
	// Workspaces is how many workspaces the handover transfers.
	Workspaces int
	// Busy is how many of them were NOT free at acceptance.
	Busy int
	// Forced reports that the transfers do not wait.
	Forced bool
}

// StaleCheck is what one staleness judgement found.
type StaleCheck struct {
	// Stale reports that the shim's reported build is not the installed
	// bundle's (or that it reported none).
	Stale bool
	// Reported and Installed are the two builds compared.
	Reported, Installed string
	// Bounce is the registry's decision when Stale; zero otherwise.
	Bounce bounce.Decision
	// Skipped names why a stale shim was not bounced again: its bounce for this
	// very build already ran and the relaunched shim still reports it
	// (SkippedAlreadyBounced), or that bounce is still registered or running
	// (SkippedBounceInFlight).
	Skipped string
}

// Deps are the controller's collaborators.
type Deps struct {
	// SelfExe is the daemon binary a successor is spawned from.
	SelfExe string
	// SelfAddress is this daemon's own `127.0.0.1:<port>`, handed to the
	// successor as --joining and never inferred by it.
	SelfAddress string
	// Instance identifies this daemon process for serving ownership.
	Instance ids.InstanceID
	// StateDir is the state root; the successor reports its address in it.
	StateDir string
	// IntentManifest is the stand-down manifest path.
	IntentManifest string
	// DB holds the lease, serving ownership, the session facts the manifest is
	// built from, and the fault records the accounting writes.
	DB wsm.DB
	// Spawner starts the successor and waits for it to report its address.
	Spawner SuccessorSpawner
	// Announcer publishes the WatchDaemon shutdown announcement.
	Announcer Announcer
	// Pusher relays the per-workspace handover and webapp pushes.
	Pusher WorkspacePusher
	// PublishHost recomposes and republishes one workspace's HOST view. The
	// restart-pending hold is the composer's `restarting` arm, and the server
	// cannot see a lease taken or released. Nil means no host surface.
	PublishHost func(ids.WorkspaceID)
	// Participants is the expected-participant snapshot: who holds a
	// workspace's two per-workspace streams at announcement.
	Participants ParticipantSource
	// Quiesce holds ALL intake for one workspace: from the transfer notice on,
	// this daemon does no work for it.
	Quiesce QuiesceFunc
	// DrainIntake releases the held intake IN ORDER once the successor owns the
	// workspace.
	DrainIntake DrainIntakeFunc
	// LeaseChanged tells the prompt queue a workspace's lease set changed, so
	// it re-evaluates every standing hold against it. RELEASING THE RESTART
	// HOLD IS WHAT UN-STAMPS THE INTAKE, and the release alone does not reach
	// the queue: without this hook a bounce leaves the intake held forever.
	LeaseChanged LeaseChangedFunc
	// Bounces is the prompt queue's per-workspace BOUNCE REGISTRY. Every shim
	// bounce and every handover transfer is asked of it: it decides when, under
	// the same lock it dispatches under.
	Bounces BounceRegistry
	// Freeness answers a workspace's freeness right now — for the handover's
	// acceptance count only. Nothing here WAITS on freeness: the registry does.
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
	// ShimBuild answers the INSTALLED shim bundle's build: the content hash a
	// fresh spawn would report. It is the staleness authority every reported
	// build is judged against.
	ShimBuild ShimBuildFunc
	// ColdGate raises the ordinary cold gate when a resume answers `cold`.
	ColdGate ColdGateFunc
	// Progress is the footer's update line. A SUCCESSOR says `updated` on it
	// once it has taken over from a deploy's handover (the old daemon's
	// streams ended at the transfer), and an incumbent whose handover or
	// restart cannot finish takes the deploy's line down.
	Progress deployprogress.Sink
	// Exit performs the daemon's orderly exit after the last transfer.
	Exit ExitFunc
	// ExpectedOutage is the bounded outage the announcement states, so clients
	// size their quiet window instead of reading the link death as unexplained.
	ExpectedOutage time.Duration
	// AdoptionWindow is how long the OUTGOING daemon gives an adoption before
	// recording the workspace's own fault. Expiry is remediated as it comes up:
	// there is deliberately no abort and no retry machinery.
	AdoptionWindow time.Duration
	// HoldoutWarnEvery is the cadence a handover waiting on busy workspaces
	// names them at. A never-free workspace is waited on FOREVER; this is only
	// how often it is named.
	HoldoutWarnEvery time.Duration
	// StandDownWindow is how long a gracefully killed shim has before the
	// force-kill. Its expiry is a LOUD log, not an invariant.
	StandDownWindow time.Duration
	// ReadyBound bounds the wait for a spawned successor to prove it is
	// serving (Successor.Ready). Zero means DefaultReadyBound.
	ReadyBound time.Duration
	// FactsBound bounds a successor's wait, on a MID-WORK adoption, for the
	// adopted shim to re-announce its session facts. A shim that does not is
	// refused, and the incumbent transfers the workspace at freeness. Zero
	// means DefaultFactsBound.
	FactsBound time.Duration
	// Clock is the controller's view of time.
	Clock Clock
	// Lifetime is the daemon's serving lifetime. Work the controller runs past
	// the rpc that started it ends with it. Nil leaves such work bounded by the
	// process alone.
	Lifetime context.Context
	// Log is the controller's logger.
	Log dlog.Surfaces
}

// BounceRegistry is the prompt queue's bounce registry, as the rollout asks it.
type BounceRegistry interface {
	// RequestBounce asks for one workspace to be bounced; see
	// promptqueue.Queue.RequestBounce.
	RequestBounce(ctx context.Context, ws ids.WorkspaceID, req bounce.Request) (bounce.Decision, error)
	// EndKeptDrain ends the drain a handover transfer left standing, once
	// this daemon has taken the workspace back; see
	// promptqueue.Queue.EndKeptDrain.
	EndKeptDrain(ws ids.WorkspaceID)
	// SealMove takes, for a running transfer, what the queue holds for the
	// workspace only in memory and the replacements the move carries; see
	// promptqueue.Queue.SealMove.
	SealMove(ctx context.Context, ws ids.WorkspaceID) (bounce.Handoff, []bounce.Request, error)
	// UnsealMove puts a seal's memory back for a move that did not land.
	UnsealMove(ctx context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error
	// AdoptHandoff installs the carried queue memory on the adopting daemon.
	AdoptHandoff(ctx context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error
	// RejudgeHeld re-judges the held prompts whose verdicts the seal
	// superseded, against the adopted shim's running turn.
	RejudgeHeld(ctx context.Context, ws ids.WorkspaceID) error
}

// ShimBuildFunc answers the installed shim bundle's content hash.
type ShimBuildFunc func() (string, error)

// SuccessorSpawner starts the blue-green successor and waits for it to report
// its address.
type SuccessorSpawner interface {
	// Spawn starts `<self exe> --joining <incumbent address>` with THIS
	// process's environment and returns the successor once it has reported its
	// address. The report travels through <state>/joining.addr, which the
	// successor writes atomically the moment its listener is bound.
	//
	// WHOEVER HOLDS A NON-NIL SUCCESSOR OWNS ITS LIFETIME. Spawn answers one
	// whenever it started a process -- alongside an error too, when the process
	// started and never reported -- so no path out of a spawn leaves a joining
	// daemon running with nothing holding it. A nil Successor means no process
	// was started.
	Spawn(ctx context.Context, incumbentAddress string) (Successor, error)
	// SpawnReplacement starts `<self exe> --replacing` -- an ORDINARY daemon,
	// not a joining one -- with this process's environment and answers its
	// pid once it is started. It waits on the boot claim this process holds,
	// so it opens the state only after this process has exited. The process
	// outlives this one by design and is never stopped by it.
	SpawnReplacement(ctx context.Context) (int, error)
}

// Successor is one spawned successor daemon: the address it reported, and the
// handle that stops it.
//
// A HANDOVER THAT FAILS AFTER THE SPAWN STOPS ITS SUCCESSOR. Left running, the
// successor waits in joining for a manifest that never comes, the next deploy
// spawns a second one beside it, and the two poll the same manifest path and
// race for every workspace. One that SUCCEEDS is never stopped: the successor
// outlives this process by design.
type Successor interface {
	// Address is the successor's own `127.0.0.1:<port>`, as it reported it;
	// empty when it never reported one.
	Address() string
	// PID is the successor's process id, for the records that name it.
	PID() int
	// Ready answers nil ONLY once the successor has proven it is serving: a
	// real DaemonHealth round trip on its reported address. A reported
	// address is not that proof -- it is written the instant the listener is
	// bound, before the successor has opened its state or reached its server.
	// It answers *SuccessorExitedError when the process ends first, and an
	// error wrapping ctx's when the bound runs out first.
	Ready(ctx context.Context) error
	// Stop ends the successor and returns nil ONLY once the process is
	// confirmed gone (reaped). An error means it may still be running, and the
	// caller must go on treating it as alive.
	Stop(ctx context.Context) error
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
//
// It answers the lease it TOOK, empty when another holder's lease already held
// the intake: a transfer that fails, or whose adoption window expires, releases
// exactly that lease and never another holder's.
type QuiesceFunc func(ctx context.Context, ws ids.WorkspaceID) (ids.LeaseID, error)

// LeaseChangedFunc re-evaluates one workspace's standing holds against its new
// lease set. It is promptqueue.Queue.OnLeaseChanged.
type LeaseChangedFunc func(ws ids.WorkspaceID)

// DrainIntakeFunc releases a quiesced workspace's held intake IN ORDER, on the
// daemon that now owns it.
type DrainIntakeFunc func(ctx context.Context, ws ids.WorkspaceID) error

// PublishViewsFunc republishes one workspace's whole views after adoption, so
// the re-attaching clients land on fresh state rather than on nothing.
type PublishViewsFunc func(ctx context.Context, ws ids.WorkspaceID) error

// WriteDaemonAddrFunc writes daemon.addr. It exists as a hook because a
// JOINING daemon must not write it until it owns every workspace.
type WriteDaemonAddrFunc func(ctx context.Context) error

// ColdGateFunc raises the ordinary cold gate for a resume that answered `cold`.
type ColdGateFunc func(ctx context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) error

// ExitFunc performs the daemon's orderly exit.
type ExitFunc func(ctx context.Context) error

// LockProbeFunc probes one workspace's shim-held kernel lock. StateUnknown is
// NEVER read as free.
type LockProbeFunc func(workspaceDir string) (sessionlock.State, error)

// Freeness answers a workspace's freeness right now.
type Freeness interface {
	// Free reports freeness right now: no turn in flight and no live detached
	// work.
	Free(ws ids.WorkspaceID) bool
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
	// StandDown ends the workspace's session and stops its shim, THROUGH THE
	// FLEET rather than through the client. The fleet is what tells the
	// session watcher first, and a watcher that has not been told reads this
	// daemon's own act as a transport fault: it records a severing at ERROR,
	// marks the link degraded, and reopens watches at a shim the next line is
	// about to stop.
	StandDown(ctx context.Context, ws ids.WorkspaceID) error
	// HandOver closes the workspace's watches and detaches its shim, leaving
	// the process running for the successor; false when there is no session.
	HandOver(ws ids.WorkspaceID) (bool, error)
	// Resume runs StartSession(resume) on c. A cold context is an ANSWER, not
	// an error: it comes back on Resumed.Cold for the ordinary cold gate.
	Resume(ctx context.Context, ws ids.WorkspaceID, c shimclient.Client) (Resumed, error)
	// AwaitFacts blocks until the adopted watcher has taken up the session
	// facts the shim re-announced, or ctx ends: a mid-work adoption lets no
	// held prompt go before it knows the turn in flight.
	AwaitFacts(ctx context.Context, ws ids.WorkspaceID) error
	// ColdGateStanding answers the facts of a cold gate standing on the
	// workspace, false when none stands: the carry takes it across.
	ColdGateStanding(ws ids.WorkspaceID) (*conversationv1.SessionCold, bool)
	// AdoptParked dials a running shim parked at its cold gate, holds it with
	// no watcher, and raises the carried gate on this daemon.
	AdoptParked(ctx context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) (shimclient.Client, error)
}

// Resumed is what a resume answered.
type Resumed struct {
	// Cold is set when the resume answered `cold` and nothing was resumed.
	Cold *conversationv1.SessionCold
}

// Clock is the controller's view of time; see internal/clock.
type Clock = clock.Clock

// SystemClock is the production Clock.
type SystemClock = clock.System

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
	// ErrReclaimed settles a rendezvous whose workspace the incumbent took
	// back: its adoption window expired, or its transfer failed.
	ErrReclaimed = errors.New("rollout: the incumbent took this workspace back; it is not being handed over")
)

// New builds the controller.
func New(deps Deps) (Controller, error) {
	if deps.Log == nil {
		return nil, errors.New("rollout: log surfaces are required")
	}
	if deps.DB == nil {
		return nil, errors.New("rollout: a state client is required")
	}
	if deps.LeaseChanged == nil {
		return nil, errors.New("rollout: a lease-changed hook is required; a bounce that releases the restart hold without it leaves the intake held")
	}
	if deps.Bounces == nil {
		return nil, errors.New("rollout: the prompt queue's bounce registry is required; every bounce and every transfer is decided there")
	}
	if deps.ShimBuild == nil {
		return nil, errors.New("rollout: the installed shim build is required; a reported build is judged against it")
	}
	if deps.Progress == nil {
		return nil, errors.New("rollout: the deploy progress sink is required; a successor ends the deploy's story on it")
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
	if deps.ReadyBound <= 0 {
		deps.ReadyBound = DefaultReadyBound
	}
	if deps.FactsBound <= 0 {
		deps.FactsBound = DefaultFactsBound
	}
	c := &controller{
		deps:          deps,
		log:           deps.Log.Global(),
		rendezvous:    make(map[ids.WorkspaceID]*entry),
		bouncedStamp:  make(map[ids.WorkspaceID]string),
		staleInFlight: make(map[ids.WorkspaceID]bool),
		reported:      make(map[ids.WorkspaceID]string),
	}
	c.log.Debug(opNew, "the rollout controller is up", dlog.Context{
		"adoption_window":    deps.AdoptionWindow.String(),
		"holdout_warn_every": deps.HoldoutWarnEvery.String(),
		"stand_down_window":  deps.StandDownWindow.String(),
	})
	return c, nil
}
