package workspace

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"strconv"
	"sync"
	"time"

	"golang.org/x/text/language"
	"golang.org/x/text/message"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/startingshim"
	"claude-repld/internal/startup"
	"claude-repld/internal/wsm"
)

// ProbeFunc probes ONE workspace's kernel lock, deriving the lock path from the
// run directory and the worktree. It takes the two inputs rather than a path so
// the whole derive-and-probe step is one injection point, which is what lets a
// test drive the spawn-versus-adopt decision without a real shim holding a real
// flock.
type ProbeFunc func(runDir, workspaceDir string) (sessionlock.State, error)

// SocketProbeFunc probes ONE workspace's shim socket path for a listener. It
// is a SECOND kernel fact beside the lock: the lock says a conversation is
// owned, and only the socket says the owner is reachable.
type SocketProbeFunc func(socketPath string) (shimsocket.State, error)

// probeShimSocket builds the production socket probe over log.
func probeShimSocket(log dlog.Logger) SocketProbeFunc {
	log = log.With(dlog.Context{"component": "daemon.workspace.probe_shim_socket"})
	return func(socketPath string) (shimsocket.State, error) {
		return shimsocket.ProbeWithLog(log, socketPath)
	}
}

// probeWorkspaceLock builds the production probe over log. Every probe result
// — held, free, and could-not-tell — lands a record, because a lock probe is a
// diagnosis-critical event and a silent one defeats the boot report. Any error
// other than "held" is StateUnknown WITH the error, because an unreadable lock
// is never reported as free.
func probeWorkspaceLock(log dlog.Logger) ProbeFunc {
	log = log.With(dlog.Context{"component": "daemon.workspace.probe_workspace_lock"})
	return func(runDir, workspaceDir string) (sessionlock.State, error) {
		path, err := sessionlock.WorkspaceLockPath(runDir, workspaceDir)
		if err != nil {
			log.Error("daemon.workspace.probe_workspace_lock", "could not derive the workspace lock path",
				dlog.Context{"run_dir": runDir, "workspace_dir": workspaceDir, "error": err.Error()})
			return sessionlock.StateUnknown, fmt.Errorf("derive the workspace lock path: %w", err)
		}
		return sessionlock.ProbeWithLog(log, path)
	}
}

// WatcherStarter opens one workspace's watch fleet against its shim client.
type WatcherStarter func(ctx context.Context, ws ids.WorkspaceID, client shimclient.Client, session sessionwatcher.Session, sinks sessionwatcher.Sinks, log dlog.Logger) (sessionwatcher.Watcher, error)

// ShimBundle is the installed shim bundle a spawn runs: buildid.ShimBundle.
type ShimBundle interface {
	// Hold answers the bundle's build and holds it installed until release,
	// which the caller runs once the spawned shim has answered.
	Hold() (build string, release func(), err error)
}

// FleetDeps are what the session fleet needs to bring a session up.
type FleetDeps struct {
	// DB holds the session record the fresh-versus-resume decision is made
	// from.
	DB wsm.DB
	// Instance identifies this daemon process for serving ownership. Every
	// client the fleet begins holding claims the workspace for it; see
	// claimServing. Required: a fleet that cannot claim serves sessions its
	// own handover would never hand over.
	Instance ids.InstanceID
	// Accounts routes a workspace to its config root and locates a
	// conversation's transcript, which is what the resume guard checks.
	Accounts account.Resolver
	// Supervisor spawns and adopts shim processes.
	Supervisor shimclient.Supervisor
	// Sinks are the five resolvers plus the lifecycle sink the watcher routes
	// into.
	Sinks sessionwatcher.Sinks
	// BringUps is told when a start of a workspace's session begins (true)
	// and ends (false), whatever it came to: the roster holds the row's
	// availability at `pending` between the two until a link connects
	// (sidebar.Resolver.SetBringingUp).
	BringUps func(ws ids.WorkspaceID, underWay bool)
	// VendorStarts is told where a workspace's vendor-start run stands
	// whenever that changes (sidebar.Resolver.SetVendorStart): the roster
	// draws a run being retried as the bring-up and a stopped one as
	// start_failed.
	VendorStarts func(ws ids.WorkspaceID, state sidebar.VendorStart)
	// Feed carries the cold gate's row.
	Feed feed.Resolver
	// Footer carries the parked-session status a standing cold gate produces.
	Footer footer.Resolver
	// Topbar carries the cold-gate state of the STRIP. A cold-gated workspace
	// never starts a session, so the topbar's session facts never arrive and
	// its readiness gate never passes: without this the strip is blank for as
	// long as the gate stands. It is set from the same call sites as the feed
	// row and the footer status, so one gate cannot be three answers.
	Topbar topbar.Resolver
	// SocketPath answers a workspace's shim socket path under the state root.
	SocketPath func(ws ids.WorkspaceID) string
	// StoreSocket is passed to every shim explicitly.
	StoreSocket string
	// NodeBin and MainJS are the shim's command line.
	NodeBin, MainJS string
	// ShimBundle is the installed shim bundle every spawn runs. A spawn HOLDS
	// it from the moment it hashes the bundle — the build it stamps the shim
	// with, and the build the shim reports back — until the shim has answered,
	// so an install can never swap the bytes between the hash and node's read.
	ShimBundle ShimBundle
	// Fake forces the shim's offline scripted SDK.
	Fake bool
	// ForbidVendor sets AGENT_REPL_FORBID_VENDOR_CALLS on every spawn.
	ForbidVendor bool
	// LockDir is the absolute directory the shim-held kernel locks live in,
	// which the daemon only ever PROBES. It is resolved ONCE, at boot, by
	// sessionlock.ResolveRunDir, so the fleet, the boot sequence and the
	// reaper cannot disagree about it, and none of them guesses a default.
	LockDir string
	// Probe probes the workspace lock; nil means sessionlock.Probe.
	Probe ProbeFunc
	// SocketProbe probes a workspace's shim socket for a listener; nil means
	// shimsocket.Probe. It is injected for the same reason Probe is.
	SocketProbe SocketProbeFunc
	// StartWatcher opens one workspace's watch fleet; nil means
	// sessionwatcher.Start. It is a function for the same reason Probe is: the
	// fleet's decisions are exercised without a shim process behind them.
	StartWatcher WatcherStarter
	// Log is the fleet's logger.
	Log dlog.Surfaces
	// Now supplies the instants the fleet stamps; nil means time.Now.
	Now func() time.Time
	// AdoptBound bounds ONE adoption of an already-running shim. Zero means
	// shimclient.DefaultAdoptBound, which is where the sizing is stated; it is
	// a field for the same reason Probe is, so the give-up is exercised
	// without waiting the real bound out.
	AdoptBound time.Duration
	// StartBound bounds ONE StartSession call. Zero means
	// DefaultStartSessionBound, which is where the sizing is stated; it is a
	// field for the same reason AdoptBound is.
	StartBound time.Duration
	// Clock drives the wait for a PREDECESSOR'S STARTING SHIM to announce
	// itself on its socket. Nil means startingshim.SystemClock; it is a field
	// so a test of that wait drives time rather than sleeping through it.
	Clock startingshim.Clock
	// ShimAlive reports whether a recorded spawned pid names a live process.
	// Nil means startingshim.Alive, which is kill(pid, 0).
	ShimAlive func(pid int) bool
	// PublishHost recomposes and republishes one workspace's HOST view. The
	// fleet owns the edges that move it and the server cannot see them: a
	// session coming up, a session going away, a shim replaced. Nil means no
	// host surface is wired yet (the boot sequence runs before the server),
	// which is why every call goes through publishHost.
	PublishHost func(ids.WorkspaceID)
	// RetryAfter is the vendor-start run's wait between attempts: it answers
	// a channel that fires once d has passed. Nil means time.After; it is a
	// field so the backoff is driven by a test rather than slept through.
	RetryAfter func(d time.Duration) <-chan time.Time
	// VendorRetryWindow is how long a run of retryable vendor-start failures
	// is retried, from its first failure. Zero means DefaultVendorRetryWindow
	// (owner ruling); it is a field so the exhaustion is exercised without
	// waiting ten minutes.
	VendorRetryWindow time.Duration
	// Steps is told every step of every bring-up this fleet runs, whoever
	// asked for it: the editor's startup (internal/startup) prints them and
	// gates each tab's go-ahead on them.
	Steps startup.StepSink
	// SessionsUp is told that a session has come up on a workspace, however
	// it came up -- a start, a retried vendor start, a relaunch's resume, a
	// cold-gate re-open, an adoption -- so the prompts held until it
	// reconnected are delivered (promptqueue.Queue.ReleaseReconnectHolds).
	SessionsUp func(ws ids.WorkspaceID)
}

// live is one workspace's live session: the client, its watcher, and the facts
// the verbs read back.
type live struct {
	client  shimclient.Client
	watcher sessionwatcher.Watcher
	// freshStart reports a shim brought up for a FRESH conversation with no
	// session started on it yet. The book it persisted names the conversation
	// the fresh one replaces, so it is no history source until its session
	// starts (history.go): a reader is never served the conversation the user
	// just left.
	freshStart bool
	// configDir is the account root the shim was spawned under, empty for a
	// shim this daemon did not spawn. A start that reuses a held shim with no
	// session (a failed start's) reuses it only under the root the start
	// routes to: a root moved since is a different account.
	configDir string
	// hostSessionID is the session's host-facing identity, remembered here so
	// the host view's live half is answered from what this daemon IS
	// operating rather than from a durable row that may outlive the session.
	hostSessionID string
	// sessionStarted reports whether a SESSION exists on this client's shim.
	//
	// A CLIENT IS NOT A SESSION. The fleet installs a client on two paths that
	// leave the shim with no session at all: a bring-up the shim answered COLD
	// keeps its client so the gate's answer can re-open through it, and the
	// relaunch engine installs its prelaunched shim before Resume runs. Both
	// leave an entry in `sessions` that every session-directed verb would then
	// address, and the shim answers each one `no_session`.
	//
	// That is what the idle sweep did for ten hours: `Serving` read the entry,
	// the sweep sent Hibernate every five minutes, the shim refused
	// `no_session` every five minutes, and the WARN repeated forever for a
	// workspace whose session had never started (2026-09-13 log sweep, 142
	// records for one workspace). It is also what made a forced teardown of a
	// cold-gated workspace warn that "the session kill did not answer".
	//
	// It is set where a session is KNOWN to exist on the shim -- a session
	// this daemon started, and a shim it adopted, which has already started
	// its one -- and nowhere else.
	sessionStarted bool
	// sessionAbsent reports that this client's shim is KNOWN to hold no
	// session: the relaunch's prelaunched shim, from the moment its Resume
	// begins until a StartSession on it succeeds. It is not !sessionStarted:
	// an adopted shim is installed before its started session is noted, and
	// in that window its session is UNKNOWN, never absent (SessionAbsent).
	sessionAbsent bool
}

// Fleet brings sessions up and down. It is the SPAWN-ON-MOUNT semantics in one
// place, and it is what Deps.Sessions, Deps.Shim and Deps.Freeness are wired
// from, so the four answers cannot disagree about one workspace.
type Fleet struct {
	deps        FleetDeps
	probe       ProbeFunc
	socketProbe SocketProbeFunc
	watch       WatcherStarter
	now         func() time.Time
	// adoptBound bounds ONE adoption; see FleetDeps.AdoptBound.
	adoptBound time.Duration
	// startBound bounds ONE StartSession; see DefaultStartSessionBound.
	startBound time.Duration
	// after is the vendor-start run's wait; see FleetDeps.RetryAfter.
	after func(time.Duration) <-chan time.Time
	// vendorWindow is the vendor-start run's window; see
	// FleetDeps.VendorRetryWindow.
	vendorWindow time.Duration
	// starting waits for a predecessor's still-starting shim to announce
	// itself, so a bring-up never spawns a second shim onto one session
	// socket. See startingshim.
	starting  startingshim.Waiter
	mu        sync.RWMutex
	sessions  map[ids.WorkspaceID]*live
	coldGates map[ids.WorkspaceID]coldGate
	// handedOver names the workspaces a handover has taken from this daemon
	// (HandOver), until a reclaim gives one back (Reclaimed). A start that
	// spawns after its workspace was handed over must not serve it: see hold.
	handedOver map[ids.WorkspaceID]bool
	// sdkVersion is the Agent SDK version the shim last reported on a session
	// start (SessionRuntime.sdk_version); empty until one has. Under mu.
	sdkVersion string
	// lastCold is the shim's own cold facts for a parked workspace, kept whole
	// so the relaunch engine's cold arm carries what the shim stated rather
	// than a reconstruction of it.
	lastCold map[ids.WorkspaceID]*conversationv1.SessionCold
	// generation counts the shims this daemon has spawned per workspace, which
	// is what makes a prelaunched shim's socket path distinct from the running
	// one's.
	generation map[ids.WorkspaceID]int
	// startGates serialize the STARTS of one workspace. See Start: the
	// liveness check cannot do it, because a session is remembered only once
	// its shim is up. The map is guarded by mu; each gate is held ACROSS a
	// whole start, which is why it is not mu itself.
	startGates map[ids.WorkspaceID]*sync.Mutex
	// watched is the LAST watcher this process started per workspace, kept
	// after it closes: its pointers are what its successor resumes from, so a
	// restart, a revival or a relaunch never replays history. See opening.go.
	watched map[ids.WorkspaceID]sessionwatcher.Watcher
	// selected marks a workspace pointed at a different transcript since its
	// last watcher: the next watcher replays the selected transcript's first
	// page. See opening.go.
	selected map[ids.WorkspaceID]bool
	// reapedAt is, per workspace, when the last shim THIS DAEMON HELD for it
	// was concluded gone (shimclient.ExitInfo.At). Nothing that shim wrote can
	// be later, so a transcript last written at or before it has no writer
	// this daemon cannot account for. See noteReap and newestAdoptable.
	reapedAt map[ids.WorkspaceID]time.Time
	// vendorRuns is each workspace's vendor-start run: its anchor, its
	// attempt count, its standing faults and the bring-up asking. Guarded by
	// mu. See vendorstart.go.
	vendorRuns map[ids.WorkspaceID]*vendorRun

	// detached counts the session starts running OFF a caller's goroutine, and
	// detachedCtx is the context every one of them runs under. See
	// StartDetached and DrainStarts: the pair is this fleet's own lifetime for
	// work no request is waiting on, so the exit can end those starts and join
	// them rather than close the state client under one.
	detached    sync.WaitGroup
	detachedCtx context.Context
	endDetached context.CancelFunc
}

// StartDetached brings a workspace's session up WITHOUT the caller waiting for
// it, reporting the outcome to `done` on the start's own goroutine.
//
// A SESSION START IS NOT PART OF AN ANNOUNCEMENT'S ANSWER. RegisterWorkspace
// records a row; reviving the conversation that row names is work the row
// occasions, not work the answer contains. Running it inline made the answer
// wait for the start, and Start takes the workspace's start gate -- so a
// register that arrived while the boot's own bring-up held that gate waited
// for the WHOLE of the boot's start.
//
// MEASURED, realtest run 2026-09-13T16:20:34: three daemon generations in a
// row (pids 58458, 68787, 80526) had the boot bring-up's StartSession for
// `2b81f45a724642ef` hang -- the shim never answered -- and Emacs's
// re-announcement of that same workspace parked behind it on the gate. Emacs
// timed RegisterWorkspace out at 10s all three times and reported
// `link-up-register-failed` and then `call-on-closed-connection
// SelectWorkspace`, for a row the daemon had already written: the register
// answer was ready and the start was what was late.
//
// THE CONTEXT IS THE FLEET'S, NOT THE CALLER'S. The caller's dies with its
// answer, which would cancel the very start being detached from it. The
// fleet's own is ended by DrainStarts at the exit, so an in-flight start is
// ended and joined rather than left writing into a closing state client.
func (f *Fleet) StartDetached(ws ids.WorkspaceID, done func(error)) {
	f.runDetached(func(ctx context.Context) error { return f.Start(ctx, ws) },
		func(_ context.Context, err error) {
			if done != nil {
				done(err)
			}
		})
}

// ResumeColdDetached re-opens a parked session with the answered remediation
// OFF the caller's goroutine, reporting the outcome to `done` with the
// fleet's own context when it settles. An answered cold gate is ACKNOWLEDGED
// the moment the answer is taken (owner ruling, 2026-09-29: the gate
// disappears as soon as the daemon has the choice), and the remediation it
// starts -- a compaction can run for a minute -- is occasioned by that answer
// rather than contained in it.
func (f *Fleet) ResumeColdDetached(ws ids.WorkspaceID, resume ColdResume, done func(context.Context, error)) {
	f.runDetached(func(ctx context.Context) error { return f.ResumeCold(ctx, ws, resume) }, done)
}

// runDetached is THE ONE WAY session work runs off a caller's goroutine: under
// the fleet's own context and counted by `detached`, so DrainStarts ends and
// joins every piece of it at the exit rather than leaving any writing into a
// closing state client. `done`, when given, receives the outcome and the
// context the work ran under.
// Detach runs work no request waits on under the fleet's own lifetime, joined
// by DrainStarts at the daemon's exit exactly as StartDetached's starts are.
// The editor's startup runs its bring-up here.
func (f *Fleet) Detach(run func(context.Context)) {
	f.runDetached(func(ctx context.Context) error { run(ctx); return nil }, nil)
}

func (f *Fleet) runDetached(run func(context.Context) error, done func(context.Context, error)) {
	f.detached.Add(1)
	go func() {
		defer f.detached.Done()
		err := run(f.detachedCtx)
		if done != nil {
			done(f.detachedCtx, err)
		}
	}()
}

// DrainStarts ends every detached start and waits, bounded, for it to leave.
// It answers false when one is still running at the bound, which is the
// caller's cue to say so loudly: the state client closes next, and a start
// still writing through it is a refused write on a path that owes no error.
func (f *Fleet) DrainStarts(bound time.Duration) bool {
	f.endDetached()
	left := make(chan struct{})
	go func() {
		f.detached.Wait()
		close(left)
	}()
	select {
	case <-left:
		return true
	case <-time.After(bound):
		return false
	}
}

// startGate answers the gate that serializes one workspace's starts, minting
// it on first use. It is never removed: a gate is one mutex per workspace this
// daemon has ever started, and dropping one while a waiter held it would hand
// the next caller a gate nobody is behind.
func (f *Fleet) startGate(ws ids.WorkspaceID) *sync.Mutex {
	f.mu.Lock()
	defer f.mu.Unlock()
	gate, ok := f.startGates[ws]
	if !ok {
		gate = &sync.Mutex{}
		f.startGates[ws] = gate
	}
	return gate
}

// NewFleet builds the session fleet.
func NewFleet(deps FleetDeps) (*Fleet, error) {
	switch {
	case deps.DB == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a state client")
	case deps.Instance == "":
		return nil, fmt.Errorf("workspace: the session fleet needs this daemon's instance id; every session it holds is claimed for it")
	case deps.Accounts == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs an account resolver")
	case deps.Supervisor == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a shim supervisor")
	case deps.SocketPath == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a socket path resolver")
	case deps.ShimBundle == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs the installed shim bundle")
	case deps.Log == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs log surfaces")
	case !filepath.IsAbs(deps.LockDir):
		return nil, fmt.Errorf("workspace: the session fleet needs the absolute kernel-lock directory, got %q", deps.LockDir)
	case deps.BringUps == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a bring-up marker; the roster holds a starting workspace unopened by it")
	case deps.SessionsUp == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a session-up hook; prompts held until a session reconnects are delivered by it")
	case deps.VendorStarts == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a vendor-start marker; the roster draws a retried or failed vendor start by it")
	case deps.Steps == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a bring-up step sink; the editor's startup is told each step by it")
	}
	probe := deps.Probe
	if probe == nil {
		probe = probeWorkspaceLock(deps.Log.Global())
	}
	socketProbe := deps.SocketProbe
	if socketProbe == nil {
		socketProbe = probeShimSocket(deps.Log.Global())
	}
	watch := deps.StartWatcher
	if watch == nil {
		watch = sessionwatcher.Start
	}
	now := deps.Now
	if now == nil {
		now = time.Now
	}
	adoptBound := deps.AdoptBound
	if adoptBound <= 0 {
		adoptBound = shimclient.DefaultAdoptBound
	}
	startBound := deps.StartBound
	if startBound <= 0 {
		startBound = DefaultStartSessionBound
	}
	after := deps.RetryAfter
	if after == nil {
		after = time.After
	}
	vendorWindow := deps.VendorRetryWindow
	if vendorWindow <= 0 {
		vendorWindow = DefaultVendorRetryWindow
	}
	detachedCtx, endDetached := context.WithCancel(context.Background())
	return &Fleet{
		after:        after,
		vendorWindow: vendorWindow,
		vendorRuns:   map[ids.WorkspaceID]*vendorRun{},
		detachedCtx:  detachedCtx,
		endDetached:  endDetached,
		deps:         deps,
		probe:        probe,
		socketProbe:  socketProbe,
		watch:        watch,
		now:          now,
		adoptBound:   adoptBound,
		startBound:   startBound,

		starting: startingshim.Waiter{
			Alive: deps.ShimAlive,
			Probe: socketProbe,
			Clock: deps.Clock,
		},

		sessions:   map[ids.WorkspaceID]*live{},
		coldGates:  map[ids.WorkspaceID]coldGate{},
		handedOver: map[ids.WorkspaceID]bool{},
		lastCold:   map[ids.WorkspaceID]*conversationv1.SessionCold{},
		reapedAt:   map[ids.WorkspaceID]time.Time{},
		generation: map[ids.WorkspaceID]int{},
		startGates: map[ids.WorkspaceID]*sync.Mutex{},
		watched:    map[ids.WorkspaceID]sessionwatcher.Watcher{},
		selected:   map[ids.WorkspaceID]bool{},
	}, nil
}

// publishHost republishes the workspace's host view when a surface is wired.
// Before the server exists there is nothing to publish onto, and that is not a
// failure: the boot sequence deliberately runs first.
func (f *Fleet) publishHost(ws ids.WorkspaceID) {
	if f.deps.PublishHost == nil {
		return
	}
	f.deps.PublishHost(ws)
}

// Live reports whether the workspace has nothing for a start to do: a session
// runs on its shim, or its shim is parked at a standing cold gate, which only
// the gate's answer re-opens. A shim held with NO session and no gate (a
// failed start's) is not live: the next start reuses it.
//
// A SHIM IS NOT A SESSION. The fleet records every shim from the moment it is
// spawned or adopted (Held), and whether a session runs on it is a separate
// fact (sessionStarted).
func (f *Fleet) Live(ws ids.WorkspaceID) bool {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, ok := f.sessions[ws]
	if !ok {
		return false
	}
	_, gated := f.coldGates[ws]
	return session.sessionStarted || gated
}

// Held reports whether the fleet holds a shim for the workspace, a session on
// it or not. A teardown stands every held shim down; a start reuses one with
// no session.
func (f *Fleet) Held(ws ids.WorkspaceID) bool {
	_, ok := f.Client(ws)
	return ok
}

// idleHeld answers the held shim a start may reuse: one this fleet holds with
// no session on it and no cold gate parking it, whose process is still up.
func (f *Fleet) idleHeld(ws ids.WorkspaceID) (*live, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, ok := f.sessions[ws]
	if !ok || session.sessionStarted {
		return nil, false
	}
	if _, gated := f.coldGates[ws]; gated {
		return nil, false
	}
	if _, reaped := session.client.Reaped(); reaped {
		return nil, false
	}
	held := *session
	return &held, true
}

// Running answers what is in flight, which is the freeness the close verb and
// the interrupt verb are judged from. It is the FreenessFunc the verbs take.
func (f *Fleet) Running(ws ids.WorkspaceID) (Running, bool) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok {
		return Running{}, false
	}
	// A SESSION PARKED BEHIND A COLD GATE HAS NO WATCHER: the client is up and
	// the gate's answer re-opens through it, but no session was ever started,
	// so nothing is in flight and nothing is live. That is an ANSWER — the
	// close verb reads it as quiet, which is exactly right for a standing gate.
	if session.watcher == nil {
		return Running{}, true
	}
	return Running{Turn: session.watcher.TurnInFlight(), LiveWork: session.watcher.LiveWork()}, true
}

// Health answers the liveness probe health.Deps.Live takes: whether a session
// exists and whether its link is serving.
func (f *Fleet) Health(ws ids.WorkspaceID) (bool, bool) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok {
		return false, false
	}
	// A gate-parked session has no watcher and so no link truth: the session
	// exists and is not serving.
	if session.watcher == nil {
		return true, false
	}
	return true, session.watcher.Connected()
}

// coldGate is one workspace's gate as the fleet holds it: the menu it served,
// and whether an answer has already TAKEN it and its remediation is running.
//
// AN ANSWERED GATE IS NOT YET GONE. Its row leaves the feed the moment the
// answer is taken (owner ruling, 2026-09-29), but until the remediation it
// chose settles the session is still parked: a prompt is still refused by the
// gate's own name (ColdGateDetail), and a handover still carries it
// (ColdGateStanding). Only the menu stops being offered, so a second answer
// finds nothing to spend.
type coldGate struct {
	served    ServedColdGate
	answering bool
	// remediated is set once the shim ACCEPTED the answer's remediated resume:
	// the conversation is no longer cold, so a prompt is no longer refused by
	// the gate's name, though the gate itself retires only when the bring-up
	// settles (EndColdGate) and a failed bring-up raises it again.
	remediated bool
	// configDir is the account root the parked session spends from, kept so
	// a gate raised again serves the same menu.
	configDir string
}

// ColdGate answers the menu a STANDING cold gate served, which is what the
// cold-gate answer is echoed against. A gate an answer has taken offers no
// menu.
func (f *Fleet) ColdGate(ws ids.WorkspaceID) (ServedColdGate, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	held, ok := f.coldGates[ws]
	if !ok || held.answering {
		return ServedColdGate{}, false
	}
	return held.served, true
}

// TakeColdGate spends the standing gate for the conversation VENDORSESSIONID,
// answering whether this caller took it. Taking is ONE step under the fleet's
// lock -- the check and the removal cannot be split -- so of two answers racing
// for one gate exactly one spends it and the other finds nothing standing. A
// gate standing for a different conversation is not taken.
func (f *Fleet) TakeColdGate(ws ids.WorkspaceID, vendorSessionID string) bool {
	f.mu.Lock()
	held, stood := f.coldGates[ws]
	taken := stood && !held.answering && held.served.VendorSessionID == vendorSessionID
	if taken {
		held.answering = true
		f.coldGates[ws] = held
	}
	f.mu.Unlock()
	if taken {
		f.logTransition(ws, "cold_gate_standing", true, false, dlog.Context{"reason": "answered"})
		f.publishHost(ws)
	}
	return taken
}

// EndColdGate retires a TAKEN gate for VENDORSESSIONID once its remediation
// brought the session back: nothing is parked any more, so a prompt is no
// longer refused by the gate's name. A gate that was raised again or replaced
// since is left alone.
func (f *Fleet) EndColdGate(ws ids.WorkspaceID, vendorSessionID string) {
	f.mu.Lock()
	held, ok := f.coldGates[ws]
	ended := ok && held.answering && held.served.VendorSessionID == vendorSessionID
	if ended {
		delete(f.coldGates, ws)
	}
	f.mu.Unlock()
	if ended {
		f.logTransition(ws, "cold_gate_answering", true, false, dlog.Context{"reason": "remediated"})
	}
}

// remediateColdGate marks the TAKEN gate for VENDORSESSIONID remediated: the
// shim accepted the answer's resume, so the conversation is warm again and the
// prompt queue stops refusing by the gate's name (ColdGateDetail).
//
// IT RUNS BEFORE THE SESSION IS BROUGHT UP, NOT AFTER IT SETTLES. The bring-up
// publishes the host view live -- watcher attached, composer open -- and the
// gate used to retire only once the whole bring-up had returned
// (coldGateSettled -> EndColdGate). A prompt sent the moment the session
// showed live was refused `cold_gate: the conversation is cold at ...` for a
// conversation that had just been paid for (e2e TestColdGate/Pay and /Compact,
// 2026-10-03: host view live at .760, submit refused at .769, gate ended at
// .772). From here on the workspace is an ordinary session coming up.
func (f *Fleet) remediateColdGate(ws ids.WorkspaceID, vendorSessionID string) {
	f.mu.Lock()
	held, ok := f.coldGates[ws]
	marked := ok && held.answering && !held.remediated && held.served.VendorSessionID == vendorSessionID
	if marked {
		held.remediated = true
		f.coldGates[ws] = held
	}
	f.mu.Unlock()
	if marked {
		f.logTransition(ws, "cold_gate_refusing_prompts", true, false, dlog.Context{"reason": "remediation accepted"})
	}
}

// ReraiseColdGate stands the gate for VENDORSESSIONID again from the cold facts
// the shim stated when it was first raised, answering false when those facts
// are gone (the session was stopped or reaped since). It is how an answered
// gate whose remediation FAILED comes back: the session is still parked, so
// the choice is the user's again.
func (f *Fleet) ReraiseColdGate(ws ids.WorkspaceID, vendorSessionID string) bool {
	f.mu.RLock()
	cold := f.lastCold[ws]
	held, stood := f.coldGates[ws]
	f.mu.RUnlock()
	if cold == nil || !stood {
		return false
	}
	f.raiseColdGate(ws, vendorSessionID, cold, held.configDir)
	return true
}

// source is the decided way a session comes up: fresh, or a resume of one named
// conversation.
type source struct {
	// Fresh is legal ONLY with proof the workspace never had a conversation.
	Fresh bool
	// VendorSessionID is the conversation a resume names.
	VendorSessionID string
	// ColdRemediation is the answered cold gate's choice, carried on the
	// resume so the shim pays, clears or compacts instead of refusing the read
	// again. It is nil on every path but Fleet.ResumeCold: a bring-up names no
	// remediation, which is exactly what makes the shim state the cost.
	ColdRemediation *conversationv1.SessionColdRemediation
	// Rebind marks the resume that follows a BindWorkspaceSession: the
	// workspace was just pointed at a DIFFERENT conversation, so the shim
	// adopts that conversation's identity as the workspace's book instead of
	// keeping the persisted one. It is true on the Fleet.StartRebound path and
	// nowhere else — every other bring-up is the workspace continuing the
	// conversation it is already on, and marking those would let a rotated
	// resume handle orphan the records filed before the rotation.
	Rebind bool
}

// classifySource makes the FRESH-CONVERSATION decision, and it is the ONE
// classifier both bring-up paths use: Fleet.Start and Fleet.Resume.
//
// StartSession(fresh) is legal ONLY with proof the workspace has no
// conversation to resume — abandonment is irreversible, every alternative is
// recoverable — and the transcript IS that proof, so the decision is
// transcript-aware rather than a test of the recorded id alone:
//
//   - no session record, or an empty vendor id: FRESH;
//   - a DELETED session: refused outright rather than resurrected;
//   - a vendor id whose transcript is found: RESUME;
//   - a vendor id whose transcript is MISSING: FRESH, at a loudness that
//     depends on whether anything was lost (below).
//
// TWO POPULATIONS REACH THE MISSING-TRANSCRIPT BRANCH, and they are told apart
// by whether the workspace has ever recorded a turn (Fleet.neverEngaged).
// A pre-minted id that NEVER TOOK A TURN writes no transcript, so nothing was
// ever there to lose: this is the ORDINARY state of a session bounced before
// its first turn — a stale-build relaunch mints an id at spawn and stands the
// shim down milliseconds later — and it is recorded at INFO with no fault,
// because there is no abandoned conversation to report. A conversation that
// DID take turns and whose transcript then VANISHED off disk is the other
// population: coming up fresh silently abandons real history, so it stays
// exactly as loud as it has always been, a WARN plus the
// once-per-conversation `conversation_abandoned` fault, which is the only
// record the user gets.
//
// Either way the session comes up FRESH rather than resuming: a resume names
// a conversation the shim rightly refuses as `unknown_session`, which left the
// workspace with no client at all.
func (f *Fleet) classifySource(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, dir string, session wsm.Session, exists bool) (source, error) {
	if !exists {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!exists"})
		// NO SESSION RECORD IS THE ONE BRANCH THAT MAY ADOPT. Owner ruling
		// 2026-09-15: when the workspace has never recorded a session of its
		// own, an existing on-disk transcript — a conversation begun in the
		// interactive vendor CLI, say — should be CONTINUED rather than left
		// behind for an empty new session. This is confined to the no-record
		// branch on purpose: every branch below has a recorded id whose fate is
		// already settled (resume it, refuse a deleted one, or come up fresh
		// for a lost transcript), and re-opening that decision here would change
		// behavior the owner asked to leave exactly as it is.
		return f.adoptOrFresh(ctx, log, ws, dir), nil
	}
	if session.Terminal != nil && session.Terminal.Kind == "deleted" {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "session.Terminal != nil && session.Terminal.Kind == \"deleted\""})
		return source{}, fmt.Errorf("the session was deleted: %s", session.Terminal.Detail)
	}
	if session.VendorSessionID == "" {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "session.VendorSessionID == \"\""})
		return source{Fresh: true}, nil
	}
	if _, err := f.deps.Accounts.FindTranscript(ctx, dir, session.VendorSessionID); err != nil {
		if f.neverEngaged(ctx, log, ws) {
			log.Info(opBringUp, "the recorded conversation never took a turn and wrote no transcript; the session comes up FRESH", dlog.Context{
				"vendor_session_id": session.VendorSessionID, "cause": err.Error(),
			})
			return source{Fresh: true}, nil
		}
		// THE DIRECTORY'S LAST SESSION IS RESTORED. The recorded id can name
		// a conversation whose transcript is gone while the conversation the
		// workspace really ran sits in its own directory (2026-09-30: a stale
		// id after an unrecorded rotation lost the master workspace's whole
		// conversation). Owner ruling: restore the directory's newest
		// transcript; come up fresh only when there is none to restore.
		if candidate, ok := f.newestAdoptable(ctx, log, dir, f.writerGoneAt(ws)); ok {
			log.Info(opBringUp, "the recorded conversation has no transcript on disk; restoring the directory's newest transcript", dlog.Context{
				"recorded_vendor_session_id": session.VendorSessionID,
				"adopted_vendor_session_id":  candidate.VendorSessionID,
				"cause":                      err.Error(),
			})
			f.recordAdopted(ctx, log, ws, candidate.VendorSessionID)
			return source{VendorSessionID: candidate.VendorSessionID}, nil
		}
		log.Warn(opBringUp, "the recorded conversation has no transcript on disk; the session comes up FRESH", dlog.Context{
			"vendor_session_id": session.VendorSessionID, "cause": err.Error(),
		})
		f.noteConversationAbandoned(ctx, log, ws, session.VendorSessionID, err)
		return source{Fresh: true}, nil
	}
	log.Debug(opBringUp, "the classifier found the transcript; the session resumes", dlog.Context{
		"vendor_session_id": session.VendorSessionID,
	})
	return source{VendorSessionID: session.VendorSessionID}, nil
}

// TranscriptAdoptionIdleWindow is how quiet a transcript must have been on disk
// before a no-record bring-up will adopt it.
//
// THE GUARD EXISTS BECAUSE TWO WRITERS ON ONE TRANSCRIPT CORRUPT IT. A vendor
// process still holding the file open — an interactive `claude` in the same
// folder, mid-conversation — writes lines the shim would then interleave its
// own with, which is exactly the hazard the shim warns of in engine/session.ts.
// A recent modification is the only signal the daemon has that another writer
// may be live, so a transcript touched inside this window is treated as in-use
// and the session comes up FRESH instead; a stale one is safe to continue.
// ~45s is comfortably longer than the gap between an idle interactive session's
// own writes while staying short enough that a genuinely finished conversation
// is adoptable almost immediately after the user walks away.
const TranscriptAdoptionIdleWindow = 45 * time.Second

// adoptOrFresh decides how a workspace with NO session record of its own comes
// up: it ADOPTS the newest idle transcript already on disk when there is one,
// and otherwise starts FRESH.
func (f *Fleet) adoptOrFresh(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, dir string) source {
	// NO RECORD IS NO WRITER THIS DAEMON CAN ACCOUNT FOR, so the idle guard
	// stands whatever this daemon reaped.
	candidate, ok := f.newestAdoptable(ctx, log, dir, time.Time{})
	if !ok {
		return source{Fresh: true}
	}
	log.Info(opBringUp, "no session record; adopting the newest idle on-disk transcript", dlog.Context{
		"workspace_dir":     dir,
		"vendor_session_id": candidate.VendorSessionID,
		"last_record_at":    candidate.LastRecordAt.Format(time.RFC3339Nano),
	})
	return source{VendorSessionID: candidate.VendorSessionID}
}

// newestAdoptable is THE ONE DECISION whether the directory's newest on-disk
// transcript may be adopted: it answers the candidate and true when it may.
//
// THE IDLE GUARD IS WAIVED FOR A TRANSCRIPT WHOSE WRITER IS ACCOUNTED FOR.
// writerGoneAt is when the last shim this daemon held for the workspace was
// concluded gone, zero when there is none. A transcript last written at or
// before it was written by nothing still running that this daemon cannot
// account for, so the two-writer hazard the guard exists for does not arise
// -- and right after a relaunch or a shim's death the transcript was always
// just written, so the guard would otherwise refuse the very conversation
// being restored. A write after it is another writer's, and the guard stands.
//
// EVERY REFUSAL IS LOGGED HERE, with its reason, so a caller that then comes
// up fresh is never silent about a transcript it declined. A probe or parse
// error never crashes the bring-up and never mis-routes it.
func (f *Fleet) newestAdoptable(ctx context.Context, log dlog.Logger, dir string, writerGoneAt time.Time) (account.AdoptableTranscript, bool) {
	candidate, err := f.deps.Accounts.NewestTranscript(ctx, dir)
	if err != nil {
		if errors.Is(err, account.ErrNoTranscripts) {
			log.Debug(opBringUp, "no on-disk transcript to adopt; the session comes up FRESH", dlog.Context{
				"workspace_dir": dir,
			})
			return account.AdoptableTranscript{}, false
		}
		log.Warn(opBringUp, "could not probe for a transcript to adopt; the session comes up FRESH", dlog.Context{
			"workspace_dir": dir, "cause": err.Error(),
		})
		return account.AdoptableTranscript{}, false
	}

	if !writerGoneAt.IsZero() && !candidate.ModTime.After(writerGoneAt) {
		log.Info(opBringUp, "the newest transcript was last written before this daemon's own shim for it was gone; the idle guard is waived", dlog.Context{
			"workspace_dir":     dir,
			"vendor_session_id": candidate.VendorSessionID,
			"modified_at":       candidate.ModTime.Format(time.RFC3339Nano),
			"writer_gone_at":    writerGoneAt.Format(time.RFC3339Nano),
		})
		return candidate, true
	}
	if idle := f.now().Sub(candidate.ModTime); idle < TranscriptAdoptionIdleWindow {
		// TOO FRESH TO ADOPT: another writer may still hold it. See
		// TranscriptAdoptionIdleWindow — two writers on one transcript corrupt
		// it, so a recently touched one is left alone and the session comes up
		// fresh.
		log.Info(opBringUp, "the newest transcript was modified too recently to adopt safely; the session comes up FRESH", dlog.Context{
			"workspace_dir":     dir,
			"vendor_session_id": candidate.VendorSessionID,
			"idle_ms":           idle.Milliseconds(),
			"idle_window_ms":    TranscriptAdoptionIdleWindow.Milliseconds(),
		})
		return account.AdoptableTranscript{}, false
	}
	return candidate, true
}

// noteReap records that a shim this daemon held for the workspace is gone, at
// the instant its death was concluded. A client not yet reaped records
// nothing: its writes may not be over.
func (f *Fleet) noteReap(ws ids.WorkspaceID, c shimclient.Client) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.noteReapLocked(ws, c)
}

// noteReapLocked is noteReap with the fleet's lock already held.
func (f *Fleet) noteReapLocked(ws ids.WorkspaceID, c shimclient.Client) {
	info, reaped := c.Reaped()
	if !reaped {
		return
	}
	if info.At.After(f.reapedAt[ws]) {
		f.reapedAt[ws] = info.At
	}
}

// writerGoneAt answers when the last shim this daemon held for the workspace
// was concluded gone, zero when this daemon reaped none.
func (f *Fleet) writerGoneAt(ws ids.WorkspaceID) time.Time {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return f.reapedAt[ws]
}

// recordAdopted records an adopted conversation as the session's resume
// handle, so the next bring-up resumes it directly. A record that fails is an
// ERROR and the session still resumes the adopted conversation: the record
// only saves the next bring-up the adoption.
func (f *Fleet) recordAdopted(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, vendorSessionID string) {
	replaced, err := f.deps.DB.SetVendorSessionID(ctx, ws, vendorSessionID)
	if err != nil {
		log.Error(opBringUp, "could not record the adopted conversation as the session's resume handle; the session still resumes it", dlog.Context{
			"adopted_vendor_session_id": vendorSessionID, "cause": err.Error(),
		})
		return
	}
	log.Debug(opBringUp, "recorded the adopted conversation as the session's resume handle", dlog.Context{
		"adopted_vendor_session_id": vendorSessionID, "replaced_vendor_session_id": replaced,
	})
}

// neverEngaged is the PROOF that a missing transcript lost nothing: the
// workspace has never recorded a turn, so no conversation of its has ever
// spoken and no vendor ever had reason to write a transcript file.
//
// IT ANSWERS TRUE ONLY ON PROOF. The turns table is keyed by workspace, not by
// vendor conversation (wsm.Turn, internal/wsm/types.go), so a workspace that
// ROTATED conversations reads as engaged even for a freshly minted id that
// never took a turn. That is deliberate: the question the caller is really
// asking is "may I be quiet about this", and everything short of proof —
// turns present, or a read that failed — answers false and leaves the caller
// loud. A read failure is recorded at ERROR in its own right, because a state
// client that cannot answer an existence query is a fault of its own; it is
// never allowed to quiet the abandonment.
func (f *Fleet) neverEngaged(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) bool {
	engaged, err := f.deps.DB.HasTurns(ctx, ws)
	if err != nil {
		log.Error(opBringUp, "could not tell whether the workspace ever took a turn; the abandoned conversation stays loud", dlog.Context{
			"cause": err.Error(),
		})
		return false
	}
	return !engaged
}

// noteConversationAbandoned records the abandoned conversation ONCE, keyed on
// the vendor id it names: the workspace keeps a live session, so the fault is
// the only record that the recorded conversation was left behind, and a
// re-classification of the SAME id must not stack a second copy of it.
func (f *Fleet) noteConversationAbandoned(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, vendorSessionID string, cause error) {
	workspace := ws
	standing, err := f.deps.DB.OpenFaults(ctx, wsm.FaultScope{Workspace: &workspace, Kind: health.KindConversationAbandoned})
	if err != nil {
		log.Error(opBringUp, "could not read the standing abandoned-conversation faults", dlog.Context{"cause": err.Error()})
		return
	}
	for _, fault := range standing {
		if fault.Evidence["vendor_session_id"] == vendorSessionID {
			log.Debug(opBringUp, "the abandoned conversation already carries its fault", dlog.Context{
				"vendor_session_id": vendorSessionID,
			})
			return
		}
	}
	if _, err := f.deps.DB.OpenFault(ctx, wsm.Fault{
		Workspace: &workspace,
		Kind:      health.KindConversationAbandoned,
		Detail:    "the recorded conversation had no transcript; the session came up fresh",
		Evidence:  map[string]string{"vendor_session_id": vendorSessionID, "cause": cause.Error()},
		OpenedAt:  f.now(),
	}); err != nil {
		log.Error(opBringUp, "could not record the abandoned conversation", dlog.Context{"cause": err.Error()})
	}
}

// MarkUnserved states, on every session view, that this daemon serves no
// session for ws: its link is dead and never connected. It is the boot's
// answer for a workspace whose kernel lock or socket could not tell whether a
// survivor owns it — the daemon neither adopts nor spawns there, and without
// a link state its roster row would stay `pending` forever.
func (f *Fleet) MarkUnserved(ws ids.WorkspaceID) {
	f.deps.Log.Global().Info(opBringUp, "this daemon serves no session for an undetermined workspace; its views say so", dlog.Context{
		dlog.KeyWorkspaceID: string(ws),
	})
	f.deps.Sinks.Footer.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Topbar.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Sidebar.OnLink(ws, shimclient.LinkDead)
}

// Start brings a workspace's session up: the mount IS the revival.
//
// The order is what the rulings fix:
//
//  1. decide fresh-versus-resume from the durable record;
//  2. run the RESUME GUARD — a resume whose vendor transcript is missing is
//     refused BEFORE any process spawns, because a vanished file yields no death
//     evidence and the redial ladder would loop forever on an unchangeable fact;
//  3. PROBE the workspace's kernel lock — held means a surviving shim already
//     owns this conversation, so the daemon ADOPTS it rather than spawning a
//     second one, and "could not tell" is never read as free;
//  4. start the session, answering a cold refusal with the gate rather than
//     paying for it;
//  5. record the session facts and start the watcher.
func (f *Fleet) Start(ctx context.Context, ws ids.WorkspaceID) error {
	return f.start(ctx, ws, false)
}

// StartRebound is Start for the ONE start that follows a BindWorkspaceSession:
// the workspace has just been pointed at a DIFFERENT conversation, and the
// resume says so on the wire (`shim.v1.StartSessionResume.rebind`).
//
// WHY THE SHIM HAS TO BE TOLD, rather than inferring it from the record it is
// handed: the shim keeps a persisted main AgentId per workspace — the book the
// daemon reads history under — and a plain resume deliberately keeps whatever
// was persisted, so that a rotated resume handle cannot orphan the records
// filed before the rotation. That rule is right for every OTHER start and
// wrong for exactly this one, and nothing in a resume request distinguishes
// them but this.
//
// EVERY OTHER BRING-UP GOES THROUGH Start AND STAYS A PLAIN RESUME: a
// revival, a restart, a rollout relaunch, a boot bring-up and a cold-gate
// re-open are all the workspace continuing the conversation it is already on.
func (f *Fleet) StartRebound(ctx context.Context, ws ids.WorkspaceID) error {
	return f.start(ctx, ws, true)
}

// start is the one bring-up body. `rebind` is true only for StartRebound.
func (f *Fleet) start(ctx context.Context, ws ids.WorkspaceID, rebind bool) error {
	// ONE START PER WORKSPACE AT A TIME, and it is a LOCK rather than the
	// liveness check below because that check is a check-then-act: the session
	// is remembered only after the shim is up, so two starts that overlap both
	// read "not live" and both spawn. The second then reaches the workspace's
	// socket, finds the FIRST one's shim listening behind a lock its
	// StartSession has not taken yet, and attaches to it as an inert survivor
	// — a warning about a race rather than a session.
	//
	// It stopped being hypothetical when the boot's bring-up moved out of the
	// reconciliation and beside the accept loop (internal/boot: BringUp): a
	// relaunch now has Emacs announcing the workspaces it holds while the boot
	// is still starting their sessions, which is two starts of one workspace
	// on two goroutines. The waiter re-reads liveness under the gate and
	// answers the session the winner brought up.
	gate := f.startGate(ws)
	gate.Lock()
	defer gate.Unlock()
	// A DEAD SHIM'S ROW IS RETIRED BEFORE LIVENESS IS JUDGED. The row outlives
	// the process, so a bring-up that read map presence alone answered "already
	// live" for a workspace whose shim was killed out from under the daemon —
	// and a prompt's revival then had nothing to revive.
	f.retireReaped(ws)
	if f.Live(ws) {
		return nil
	}
	// THE BRING-UP IS UNDER WAY FROM HERE UNTIL THIS START RETURNS, however
	// it ends. A success has connected the link and a failure has stated it
	// dead before the lowering runs, so the row never reads `available` in
	// between for want of either.
	f.deps.BringUps(ws, true)
	defer f.deps.BringUps(ws, false)
	err := f.startUp(ctx, ws, rebind)
	f.noteStartEnded(ws, err)
	return err
}

// noteStartEnded tells the startup a bring-up's service-level failure. A
// session that came up, a vendor that would not start, a restart that ended
// the vendor run and a stand-down this daemon ordered have each said so (or
// are no failure of agent-repl's); every other error is the bring-up failing.
func (f *Fleet) noteStartEnded(ws ids.WorkspaceID, err error) {
	if err == nil || errors.Is(err, ErrShimTaken) {
		return
	}
	var label *startLabel
	if errors.As(err, &label) && (label.vendor || label.network) {
		return
	}
	f.deps.Steps(ws, startup.Step{Kind: startup.StepFailed, Text: err.Error()})
}

// startUp is start's body, under the start gate, with the bring-up marker
// raised.
func (f *Fleet) startUp(ctx context.Context, ws ids.WorkspaceID, rebind bool) error {
	// A SELECTED TRANSCRIPT IS A DIFFERENT CONVERSATION: the watcher this
	// start opens replays its first page, and the previous watcher's pointers,
	// which name another book, are forgotten. See opening.go.
	if rebind {
		f.noteTranscriptSelected(ws)
	}
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return fmt.Errorf("start session for %q: %w", ws, err)
	}
	log, err := f.deps.Log.Workspace(record.Dir)
	if err != nil {
		return fmt.Errorf("start session for %q: resolve log sink: %w", ws, err)
	}
	log = log.With(dlog.Context{"workspace": string(ws)})

	session, exists, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opBringUp, "could not read the session record", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("start session for %q: read the session record: %w", ws, err)
	}
	if exists && session.Hibernated() {
		f.deps.Steps(ws, startup.Step{Kind: startup.StepWaking})
	} else {
		f.deps.Steps(ws, startup.Step{Kind: startup.StepStartingSession})
	}

	src, err := f.classifySource(ctx, log, ws, record.Dir, session, exists)
	if err != nil {
		return refuse(log, "OpenWorkspace", ArmSessionDeleted, err.Error(), false)
	}
	// THE REBIND RIDES THE SOURCE, decided by the CALLER rather than the
	// classifier: the classifier reads the durable record, which says which
	// conversation the workspace is on and not whether the user has just
	// changed it. A fresh start carries no resume to mark, so the marker is
	// simply never composed for one.
	src.Rebind = rebind
	// A FRESH START IS A NEW CONVERSATION: its book is new, and the previous
	// watcher's pointers name lines of another one. See forgetPointers.
	if src.Fresh {
		f.forgetPointers(ws)
		// AND ITS BOOK IS EMPTY AS A FACT: no reader's open asks the store for
		// a book that only its first prompt's row will register.
		f.deps.Feed.NoteFreshBook(ws)
	}
	log.Debug(opBringUp, "decided how the session comes up", dlog.Context{
		"fresh": src.Fresh, "vendor_session_id": src.VendorSessionID, "rebind": src.Rebind,
	})

	// THE ACCOUNT ROUTING IS DECIDED AT EVERY START (daemon.md 10a), never
	// inherited from the record: $MULTI_REPO_ROOT can move between boots, and
	// a session resumed under the root it was FILED in rather than the one it
	// now ROUTES to would run the whole conversation against the wrong
	// account. When the two disagree, the vendor transcript is carried into
	// the newly routed root BEFORE the resume is sent — a resume against a
	// root that does not hold the transcript is a resume of nothing.
	configDir := accountRootFor(f.deps.Accounts, record.Dir, session)
	if session.ConfigDir != "" && session.ConfigDir != configDir {
		if err := f.portAcrossAccounts(ctx, log, record.Dir, session, configDir, src); err != nil {
			return err
		}
	}
	udsPath := f.deps.SocketPath(ws)

	// THE HOST SESSION IDENTITY IS DECIDED BEFORE THE SPAWN, because the shim
	// is stamped with it (AGENT_REPL_SESSION_ID, for log correlation) and a
	// stamp cannot be applied after the process is running. A RESUME keeps the
	// identity it was given -- it is the same session -- and a FRESH start
	// mints a new one, because a fresh conversation on one workspace is a new
	// session and Emacs correlates fault windows against exactly this.
	hostSessionID := session.HostSessionID
	if hostSessionID == "" || src.Fresh {
		hostSessionID = wsm.NewHostSessionID()
		log.Debug(opBringUp, "minted the session's host identity", dlog.Context{
			"host_session_id": hostSessionID, "fresh": src.Fresh,
		})
	}
	// THE DAEMON STAMPS THE IDENTITY IT HANDS THE SHIM. The shim is spawned
	// with AGENT_REPL_SESSION_ID=<hostSessionID>, so every daemon record about
	// this bring-up carries the same agent_repl_session_id its shim's records
	// do and the two sides of one session join on that field. The stamp is
	// applied to a logger resolved fresh for THIS start, so a rotated identity
	// restamps and nothing carries a previous session's id forward.
	log = stampSession(log, hostSessionID)

	// A HELD SHIM WITH NO SESSION IS REUSED, never spawned over and never
	// adopted a second time: a failed start left it held exactly so a retry,
	// a revival or the next prompt starts the session on it.
	client, path, err := f.reuseOrBringUp(ctx, log, ws, record.Dir, udsPath, configDir, hostSessionID, src)
	if err != nil {
		return err
	}
	if path == pathHeld {
		hostSessionID = f.heldHostSessionID(ws, hostSessionID)
	}
	adopted := path == pathAdopted
	// THE HEALTHY ATTACH IS A RECOVERY EDGE. Bring-up gates on the shim's
	// first healthy diagnostics, so reaching here IS the repair of every
	// standing fault whose lifetime ends at a healthy attach: a lost link, a
	// start that failed, an undetermined bounce. A mid-stream redial is NOT
	// this moment: the link coming back on a stream the daemon never
	// re-attached leaves the evidence standing.
	f.closeOnEdge(ctx, log, ws, health.EdgeHealthyAttach)
	// AGENT-REPL'S OWN SERVICES SERVE THE WORKSPACE from here, whatever the
	// vendor goes on to do: this is the moment the editor's tab may open.
	f.deps.Steps(ws, startup.Step{Kind: startup.StepServing})

	// AN ADOPTED SHIM IS ATTACHED TO, NEVER STARTED. The lock probe selected
	// the adopt path precisely because a shim is still alive on this
	// conversation, and a live shim has ALREADY started its one session:
	// shim.v1 answers a second StartSession with `already_started`, so sending
	// one turns a perfectly good mount into a failed rpc. The session facts
	// come from the shim's own re-announcement of SessionStarted on the watch
	// this install opens (landing 7) — the same attach-only path the
	// handover's successor and the boot adoption take.
	if adopted {
		// AN ADOPTED SHIM HAS ALREADY STARTED ITS ONE SESSION -- that is the
		// premise of the whole branch -- so the entry says so, and every
		// session-directed verb may address it.
		if err := f.hold(ctx, log, ws, &live{client: client, hostSessionID: hostSessionID, sessionStarted: true}); err != nil {
			return err
		}
		if err := f.Install(ctx, ws, client); err != nil {
			log.Error(opBringUp, "the adopted shim could not be installed", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("start session for %q: install the adopted shim: %w", ws, err)
		}
		// AFTER the install, which rewrote the entry the remember above wrote.
		f.noteSessionStarted(ws)
		log.Info(opBringUp, "attached to a surviving shim without starting a session", dlog.Context{
			"adopted": true, "shim_pid": client.PID(),
		})
		f.deps.SessionsUp(ws)
		f.deps.Steps(ws, startup.Step{Kind: startup.StepUp})
		return nil
	}

	// THE VENDOR-START RUN IS CANCELLABLE BY A RESTART until this start has
	// finished with it.
	runCtx, finishRun := f.beginVendorStart(ctx, ws)
	if !src.Fresh {
		f.deps.Steps(ws, startup.Step{Kind: startup.StepResuming})
	}
	started, err := f.startSession(runCtx, log, ws, client, src, session, configDir)
	if errors.Is(err, errSessionAlreadyRunning) {
		finishRun()
		return f.takeRunningSession(ctx, log, ws, client)
	}
	if err != nil {
		defer finishRun()
		// A FAILED START KEEPS ITS SHIM HELD, with no session on it, exactly
		// as a cold gate keeps its own: the shim still serves the workspace's
		// book (the feed reads history through it), and a retry, a revival or
		// the next prompt starts the session on it (reuseOrBringUp), while a
		// restart replaces it. Nothing is stopped and nothing is adopted
		// twice: the held entry is what every later bring-up finds first. A
		// shim taken from this start meanwhile (a handover's transfer, a
		// kill) is not this start's to keep.
		if !f.holds(ws, client) {
			return fmt.Errorf("start session for %q: %w: %w", ws, ErrShimTaken, err)
		}
		log.Info(opBringUp, "the failed start's shim stays held with no session; the next start reuses it", dlog.Context{
			"shim_pid": client.PID(), "cause": err.Error(),
		})
		return err
	}
	finishRun()
	if started == nil {
		// The session is parked behind a standing cold gate. The client stays
		// up: the gate's answer re-opens through it.
		//
		// THE LINK IS RESTATED HERE because no watcher opens on this path and
		// nothing else would. Bring-up gated on the shim's first healthy
		// diagnostics, so the link IS serving — and without saying so the
		// surfaces keep drawing the DEAD link of whatever shim died before this
		// one, which outranks the gate in the footer's status tree and hides
		// the very thing the user has to answer.
		f.deps.Sinks.Footer.OnLink(ws, shimclient.LinkConnected)
		f.deps.Sinks.Topbar.OnLink(ws, shimclient.LinkConnected)
		f.deps.Sinks.Sidebar.OnLink(ws, shimclient.LinkConnected)
		// THE PARKED SESSION KEEPS ITS HOST IDENTITY. The gate's answer re-opens
		// through this very entry, and a resume that had to mint a second
		// identity would report a NEW session for a conversation that never
		// ended.
		// AND NO SESSION EXISTS ON THE SHIM. `sessionStarted` stays false, so
		// the idle sweep skips the workspace instead of directing a shim that
		// can only refuse `no_session`, and a teardown stops the process
		// without asking it to end a session it never began.
		if err := f.restate(ws, &live{client: client, hostSessionID: hostSessionID, configDir: configDir}); err != nil {
			return err
		}
		f.publishHost(ws)
		log.Info(opBringUp, "the session is parked at its cold gate", dlog.Context{
			"shim_pid": client.PID(), "host_session_id": hostSessionID,
		})
		f.deps.Steps(ws, startup.Step{Kind: startup.StepColdGate})
		return nil
	}

	if err := f.sessionUp(ctx, log, ws, client, started, session, configDir, hostSessionID); err != nil {
		return err
	}
	f.deps.Steps(ws, startup.Step{Kind: startup.StepUp})
	return nil
}

// sessionUp is EVERYTHING a started session still needs, and it is the ONE
// place that does it: the watcher that opens the session's watches, the facts
// the session record keeps, and the host view's statement that a session now
// exists. Both paths that can produce a SessionStarted run through it — the
// cold start (Fleet.Start) and the remediated re-open an answered cold gate
// performs (Fleet.ResumeCold) — because a second copy is how the re-open came
// to install no watcher and never report live, which refused the very next
// prompt with `no_session`.
func (f *Fleet) sessionUp(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
	client shimclient.Client,
	started *conversationv1.SessionStarted,
	previous wsm.Session,
	configDir, hostSessionID string,
) error {
	// The watcher is handed the opening LEVEL: the turn in flight and every
	// live detached item are what it opens its watches for, and nothing else
	// states them.
	//
	// Its context is DETACHED from the caller's: the watch fleet outlives the
	// verb that brought the session up (an OpenWorkspace rpc, a create, the
	// relaunch engine), and every stream it opens -- now and on every redial --
	// is opened against this context. Bound to the request instead, the whole
	// fleet is canceled the instant the rpc answers, and the session is then
	// left with no standing WatchSession and no standing WatchAgent at all.
	// THE SESSION IS REMEMBERED BEFORE ITS WATCHER OPENS. Starting the watcher
	// publishes its opening facts synchronously -- the link among them -- and
	// the host view is recomposed from that edge; a fleet that did not yet
	// know the session would answer "no live facts" for a workspace whose
	// session record already exists, and the host view would be withheld with
	// an invariant violation for a session that is coming up perfectly well.
	if err := f.restate(ws, &live{client: client, hostSessionID: hostSessionID, configDir: configDir, sessionStarted: true}); err != nil {
		return err
	}
	// THE SESSION FACTS ARE DURABLE BEFORE THE WATCH OPENS. The shim
	// re-announces its session on every new watch, and the watcher records
	// the vendor id it states as the resume handle (SetVendorSessionID) --
	// which writes the session row recordFacts creates. Opened first, the
	// watch could land that write before the row existed: "wsm: not found",
	// an ERROR for a session coming up perfectly well (integration
	// TestABlockedHookDrawsACard under load, 2026-10-03). A failure to record
	// is still returned, after the watch opens, so the session it started
	// stays watched and usable.
	factsErr := f.recordFacts(ctx, log, ws, previous, started, configDir, hostSessionID, client.PID())
	watcher, err := f.startWatcher(ctx, log, ws, client, sessionwatcher.Session{Started: started})
	if err != nil {
		log.Error(opBringUp, "could not start the session watcher", dlog.Context{"cause": err.Error()})
		return errors.Join(fmt.Errorf("start session for %q: start the watcher: %w", ws, err), factsErr)
	}
	if err := f.restate(ws, &live{client: client, watcher: watcher, hostSessionID: hostSessionID, configDir: configDir, sessionStarted: true}); err != nil {
		// THE WATCHER OPENED FOR A SHIM THIS START NO LONGER HOLDS: it watches
		// for nobody, so it is closed rather than left streaming.
		f.closeDisplaced(ws, watcher, "the shim was taken from the start that opened this watcher")
		return err
	}
	if factsErr != nil {
		return factsErr
	}
	// A NEW SHIM STARTS AT THE ROOT'S LEVEL, while the reader picked another
	// for the rest of the session: put it back before the session is called
	// up. An ADOPTED shim never comes through here; it kept its level.
	f.reapplyEffort(ctx, log, ws, client)
	log.Info(opBringUp, "the session is up", dlog.Context{
		"adopted": false, "vendor_session_id": started.GetVendorSessionId(), "shim_pid": client.PID(),
	})
	// A SESSION NOW EXISTS where none did: the host view's whole session arm
	// changed, and nothing the server can see says so.
	f.publishHost(ws)
	f.deps.SessionsUp(ws)
	return nil
}

// ResumeCold re-opens a session parked behind a standing cold gate, carrying
// the remediation the user chose, and completes the SAME bring-up the cold
// start performs. It is what an answered cold gate spends: daemon.md's "The
// daemon reopens naming a remediation — pay | clear | compact — chosen by the
// user (AnswerColdGate)".
//
// The shim it re-opens through is the one the park left serving: a parked
// session's client stays up precisely so the answer has somewhere to go, and a
// workspace with no live client has no gate to answer.
//
// THE FACTS ARE RE-DERIVED, never carried over from the refused attempt: the
// account routing is decided at every start (daemon.md 10a) and the record is
// what the resume must be filed against, so both are read here exactly as
// Fleet.Start reads them.
func (f *Fleet) ResumeCold(ctx context.Context, ws ids.WorkspaceID, resume ColdResume) error {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok {
		return refuse(f.deps.Log.Global(), "AnswerColdGate", ArmNoSession,
			fmt.Sprintf("workspace %q has no live session to re-open", ws), false)
	}
	// A REAPED CLIENT IS NO SESSION, read the same way Fleet.Shim reads it: the
	// map entry outlives the process, and re-opening over a dead connection
	// answers a raw transport error where the contract spells no_session.
	if _, reaped := session.client.Reaped(); reaped {
		return refuse(f.deps.Log.Global(), "AnswerColdGate", ArmNoSession,
			fmt.Sprintf("workspace %q has no live session to re-open", ws), false)
	}

	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return fmt.Errorf("re-open session for %q: %w", ws, err)
	}
	log, err := f.deps.Log.Workspace(record.Dir)
	if err != nil {
		return fmt.Errorf("re-open session for %q: resolve log sink: %w", ws, err)
	}
	log = log.With(dlog.Context{"workspace": string(ws)})

	previous, _, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opColdGate, "could not read the session record", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("re-open session for %q: read the session record: %w", ws, err)
	}

	// THE PARK'S HOST IDENTITY IS KEPT. The remediated re-open resumes the same
	// conversation the refused attempt named, so it is the same session and
	// Emacs correlates its fault windows against exactly this id.
	hostSessionID := session.hostSessionID
	if hostSessionID == "" {
		hostSessionID = previous.HostSessionID
	}
	if hostSessionID == "" {
		hostSessionID = wsm.NewHostSessionID()
	}
	log = stampSession(log, hostSessionID)

	// THE PHASES ARE WATCHED BEFORE THE SHIM IS DIALED, and that ordering is
	// the whole point: the compaction runs INSIDE StartSession, so a watch
	// opened after the call would subscribe to a fan-out that had already sent
	// every frame it was going to send. The stream is closed the moment
	// StartSession answers, whichever way it answered.
	configDir := accountRootFor(f.deps.Accounts, record.Dir, previous)
	stopPhases := f.relayCompactionPhases(ctx, log, ws, session.client, resume.OnPhase)
	runCtx, finishRun := f.beginVendorStart(ctx, ws)
	started, err := f.startSession(runCtx, log, ws, session.client, source{
		VendorSessionID: resume.VendorSessionID,
		ColdRemediation: resume.Remediation,
	}, previous, configDir)
	finishRun()
	stopPhases()
	if err != nil {
		return err
	}
	if started == nil {
		// THE SHIM REFUSED THE REMEDIATED RESUME AS COLD AGAIN. startSession has
		// already raised a fresh gate from that refusal, so the session is parked
		// once more rather than up — and saying so is what keeps the caller from
		// reporting a re-opened session that does not exist.
		return refuse(log, "AnswerColdGate", ArmNoSession,
			fmt.Sprintf("the shim refused the remediated resume of %q as cold again", ws), false)
	}
	// THE PROMPT REFUSAL LIFTS BEFORE THE SESSION SHOWS LIVE. See
	// remediateColdGate.
	f.remediateColdGate(ws, resume.VendorSessionID)
	return f.sessionUp(ctx, log, ws, session.client, started, previous, configDir, hostSessionID)
}

// relayCompactionPhases opens a WatchSession on the parked shim for the
// duration of one remediated re-open and hands every compaction phase it
// carries to `onPhase`, recording each one.
//
// GROUNDED (owner's report, 2026-09-14). The gate's `compact` remediation
// compacted a 101.6k-token conversation with the daemon writing exactly one
// record — at the very end — and the footer saying nothing at all for the
// whole of it. Every phase is now one INFO record and one footer line.
//
// IT IS ADDITIVE AND IT NEVER FAILS THE RE-OPEN. A watch that will not open,
// or a stream that ends early, costs the re-open its narration and nothing
// else: the session bring-up is the act, and refusing to bring a session up
// because its commentary was unavailable would be a worse failure than the
// silence this exists to end. The returned func closes the watch and JOINS the
// reader, so nothing is still writing to the footer after the verb returns.
func (f *Fleet) relayCompactionPhases(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
	client shimclient.Client,
	onPhase func(*conversationv1.SessionCompactionProgress),
) func() {
	if onPhase == nil {
		return func() {}
	}
	watchCtx, cancel := context.WithCancel(context.WithoutCancel(ctx))
	stream, err := client.WatchSession(watchCtx)
	if err != nil {
		cancel()
		log.Warn(opColdGate, "the remediated re-open runs without its compaction phases: the watch would not open",
			dlog.Context{"cause": err.Error()})
		return func() {}
	}
	done := make(chan struct{})
	go func() {
		defer close(done)
		for {
			frame, err := stream.Recv()
			if err != nil {
				// The ordinary end is this relay's own cancel when the re-open
				// answers, which is not news.
				if watchCtx.Err() == nil {
					log.Info(opColdGate, "the compaction phase stream ended before the re-open did",
						dlog.Context{"cause": err.Error()})
				}
				return
			}
			progress := frame.GetUpdate().GetCompactionProgress()
			if progress == nil {
				continue
			}
			log.Info(opColdGate, "the cold gate's compaction reported a phase", dlog.Context{
				"workspace":     string(ws),
				"phase":         progress.GetPhase().String(),
				"tokens_before": progress.GetTokensBefore(),
				"tokens_after":  progress.GetTokensAfter(),
				"error":         progress.GetError(),
			})
			onPhase(progress)
		}
	}()
	return func() {
		// BOTH, IN THIS ORDER. The cancel ends the request the stream was
		// opened under; the Close ends the stream from this side, which is
		// what unblocks a Recv that is already parked on a frame that will
		// never come. Either alone has left the reader goroutine parked.
		cancel()
		stream.Close()
		<-done
	}
}

// bringUpPath is WHICH of the three ways a client came up, and it is a
// distinct answer from "was it adopted" because the caller has to act on the
// third one. A shim THIS bring-up spawned is this daemon's to stop when the
// start that follows fails: an error returned before the client is remembered
// leaves nothing else holding it, and the shim keeps serving the workspace
// socket for the next bring-up to find.
type bringUpPath int

const (
	// pathNone is no client at all: the bring-up refused or failed.
	pathNone bringUpPath = iota
	// pathSpawned is a shim THIS bring-up started.
	pathSpawned
	// pathAdopted is a surviving shim that holds the workspace lock, and
	// therefore already has its one session.
	pathAdopted
	// pathInert is a surviving shim that is LISTENING while holding no lock:
	// it has no session yet, so it is attached to and then started.
	pathInert
	// pathHeld is a shim this fleet already holds with no session on it (a
	// failed start's): it is started, never probed, spawned over or adopted.
	pathHeld
)

// errSessionAlreadyRunning is a StartSession the shim answered
// `already_started`: the session runs, and the start takes it.
var errSessionAlreadyRunning = errors.New("workspace: the shim already runs its session")

// takeRunningSession takes the session a held shim already runs, which a
// start learned of only from the shim's `already_started` answer: the entry
// records it running, and a shim not yet watched has its watches opened,
// attach-only, exactly as an adoption's are. It is a session up, not a failed
// start.
func (f *Fleet) takeRunningSession(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client) error {
	if !f.noteSessionRunningOn(ws, client) {
		return fmt.Errorf("start session for %q: %w", ws, ErrShimTaken)
	}
	f.mu.RLock()
	watched := f.sessions[ws] != nil && f.sessions[ws].watcher != nil
	f.mu.RUnlock()
	if !watched {
		openAtAttach, err := f.openTurns(ctx, ws)
		if err != nil {
			return fmt.Errorf("start session for %q: %w", ws, err)
		}
		if err := f.watchInstalled(ctx, ws, client, openAtAttach); err != nil {
			return err
		}
	}
	log.Info(opBringUp, "the held shim already runs its session; the start takes it rather than starting one", dlog.Context{
		"shim_pid": client.PID(), "watched_already": watched,
	})
	f.publishHost(ws)
	f.deps.SessionsUp(ws)
	f.deps.Steps(ws, startup.Step{Kind: startup.StepUp})
	return nil
}

// noteSessionRunningOn records that CLIENT's shim runs a session, when the
// workspace's entry still holds CLIENT, and answers whether it does.
func (f *Fleet) noteSessionRunningOn(ws ids.WorkspaceID, client shimclient.Client) bool {
	f.mu.Lock()
	session, ok := f.sessions[ws]
	if !ok || session.client != client {
		f.mu.Unlock()
		return false
	}
	was := session.sessionStarted
	session.sessionStarted, session.sessionAbsent, session.freshStart = true, false, false
	f.mu.Unlock()
	if !was {
		f.logTransition(ws, "session_started", false, true, dlog.Context{"shim_pid": client.PID()})
	}
	return true
}

// reuseOrBringUp answers the shim a start starts its session on: the one the
// fleet holds with no session (pathHeld), else a shim bringUpClient spawns or
// attaches to. A spawned or attached inert shim is HELD from this moment, with
// no session, before StartSession is asked: every shim the fleet brings up is
// recorded the moment it exists, so a handover sees it and a failed start
// leaves it held rather than untracked. An ADOPTED shim's entry is the
// caller's to write: it already runs its session.
func (f *Fleet) reuseOrBringUp(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, dir, udsPath, configDir, hostSessionID string, src source) (shimclient.Client, bringUpPath, error) {
	if held, ok := f.idleHeld(ws); ok {
		if held.configDir == "" || held.configDir == configDir {
			log.Info(opBringUp, "a shim is held with no session; starting the session on it", dlog.Context{
				"shim_pid": held.client.PID(), "config_dir": configDir, "fresh": src.Fresh,
			})
			// WHETHER IT IS A HISTORY SOURCE follows THIS start's source: a
			// resume's shim reads the book it resumes, a fresh one's does not.
			held.freshStart, held.sessionAbsent = src.Fresh, true
			if err := f.restate(ws, held); err != nil {
				return nil, pathNone, err
			}
			if !src.Fresh {
				f.deps.Feed.SourceUp(ws)
			}
			return held.client, pathHeld, nil
		}
		// THE ACCOUNT ROUTING MOVED since the held shim was spawned: it runs
		// the other account, so it is replaced rather than reused.
		log.Info(opBringUp, "the held shim with no session runs another account's root; replacing it", dlog.Context{
			"shim_pid": held.client.PID(), "held_config_dir": held.configDir, "config_dir": configDir,
		})
		if err := f.Stop(ctx, ws, true); err != nil {
			log.Error(opBringUp, "the held shim of another account's root could not be stopped", dlog.Context{"cause": err.Error()})
			return nil, pathNone, fmt.Errorf("start session for %q: stop the held shim of another root: %w", ws, err)
		}
	}
	client, path, err := f.bringUpClient(ctx, log, ws, dir, udsPath, configDir, hostSessionID, src)
	if err != nil {
		return nil, pathNone, err
	}
	if path == pathSpawned || path == pathInert {
		heldConfigDir := ""
		if path == pathSpawned {
			heldConfigDir = configDir
		}
		if err := f.hold(ctx, log, ws, &live{client: client, hostSessionID: hostSessionID, configDir: heldConfigDir, sessionAbsent: true, freshStart: src.Fresh}); err != nil {
			return nil, pathNone, err
		}
	}
	return client, path, nil
}

// heldHostSessionID answers the identity of the held shim a start reuses: it
// was stamped with it at its spawn, and the session it now starts is that
// shim's. FALLBACK is the start's own, for an entry that names none.
func (f *Fleet) heldHostSessionID(ws ids.WorkspaceID, fallback string) string {
	f.mu.RLock()
	defer f.mu.RUnlock()
	if session, ok := f.sessions[ws]; ok && session.hostSessionID != "" {
		return session.hostSessionID
	}
	return fallback
}

// bringUpClient probes the workspace lock and either ADOPTS the surviving shim
// that holds it or SPAWNS a new one. A probe that could not tell is never read
// as free: spawning a second shim onto one conversation is the failure the lock
// exists to prevent.
func (f *Fleet) bringUpClient(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, dir, udsPath, configDir, hostSessionID string, src source) (shimclient.Client, bringUpPath, error) {
	// A DEPARTING DAEMON BRINGS NOTHING UP, and it says so BEFORE it probes or
	// spawns. The supervisor already refuses the spawn -- that is the latch's
	// backstop and it stays -- but reaching the refusal that way costs a
	// wasted fork attempt and, worse, three loud records for a state the
	// daemon decided on purpose: measured at realtest 2026-09-13T18:32:16 as
	// `daemon.shimclient.spawn` WARN, `daemon.workspace.bring_up` ERROR "the
	// shim did not come up" and `daemon.workspace.register` ERROR, plus a
	// `shim_start_failed` FAULT and a footer failure line, all for an
	// announcement that arrived while the daemon was leaving.
	//
	// IT IS A REFUSAL, NOT A FAILURE, so it is recorded at INFO through the
	// package's typed-refusal path and opens no workspace fault: nothing is
	// wrong with this workspace, and the next daemon revives it. The arm is
	// `spawn_failed`, which is the arm the caller already read for this state
	// and the only landed one that fits; the error additionally wraps
	// shimclient.ErrStandingDown so the register can tell the departure from a
	// shim that genuinely would not come up.
	if f.deps.Supervisor.StandingDown() {
		log.Info(opBringUp, "no shim is brought up: this daemon is standing down", dlog.Context{
			"workspace": string(ws), "socket": udsPath,
		})
		return nil, pathNone, fmt.Errorf("%w: %w", shimclient.ErrStandingDown,
			refuse(log, "OpenWorkspace", ArmSpawnFailed, shimclient.ErrStandingDown.Error(), false))
	}
	lockPath := f.deps.LockDir
	state, err := f.probe(lockPath, dir)
	// THE SOCKET IS THE SECOND KERNEL FACT. The lock says whether this
	// conversation is OWNED; only the socket says whether its owner is
	// REACHABLE, and a bring-up that spawned on the lock alone put a second
	// shim onto a path the survivor still held — the newcomer could not bind
	// and died, this daemon dialed the path and reached the SURVIVOR, and the
	// answer was StartSession{already_started} over a turn already running.
	//
	// THE PATH IS THE SHIM'S CURRENT GENERATION, not the layout's base name,
	// for the reason boot's adopt states: a relaunch moves the workspace's
	// shim onto `<base>.nN.sock` and the counter that minted N lives in the
	// fleet's MEMORY, so a bring-up after a daemon restart dialed a path the
	// survivor has not held since the relaunch while its lock still read HELD.
	// Boot resolved this with shimsocket.NewestLive and the fleet did not,
	// which is one probe disagreeing with the other about the same shim.
	socketPath, socket, socketErr := shimsocket.NewestLive(f.socketProbe, udsPath)
	// THE BRANCH CHOICE IS RECORDED WITH BOTH KERNEL FACTS ON IT. Which path a
	// bring-up took, and why, was reconstructible only from DEBUG records that
	// never reach a deployed log — so an adoption that dialed a socket nobody
	// was listening on read, on disk, as an unexplained ten-second failure.
	log.Info(opBringUp, "probed the two kernel facts the bring-up branches on", dlog.Context{
		"lock_dir": lockPath, "lock_state": state.String(),
		"socket": socketPath, "socket_state": socket.String(),
		"socket_base": udsPath,
	})
	// A LISTENER IS NOT A SESSION. The shim takes its two conversation locks
	// INSIDE StartSession (shim.md), so an INERT shim — spawned, serving, no
	// session — is listening while holding NEITHER lock. That is exactly the
	// shim left behind by a StartSession that REFUSED (vendor_start_failed
	// rolls the locks back and the process keeps serving), and the rollout's
	// prelaunched shim. Such a survivor is ATTACHED TO rather than spawned
	// over — a second shim could not bind the path anyway — but it is then
	// STARTED, because nothing on this conversation has a session yet. Only a
	// HELD lock says the conversation is already owned and must not be
	// started a second time.
	//
	// IT IS NOT AN ANOMALY, AND IT IS NOT A WARNING. The shim takes its locks
	// INSIDE StartSession, so free-and-listening is what an inert shim looks
	// like BY CONTRACT -- and recording it as a disagreement between two
	// kernel facts states something untrue. The BOOT's own copy of this exact
	// branch already says so at INFO ("a shim is listening with no session of
	// its own; adopting the inert survivor", internal/boot/sequence.go), and
	// `TestAnInertSurvivorIsNotWarnedAbout` pins the level there; one
	// condition recorded at two levels by two callers is the drift, not the
	// state.
	//
	// IT IS NOT THE PHANTOM-SHIM CLASS EITHER, and the lock is what tells
	// them apart. A phantom is a shim that OUTLIVED a lock it had taken; this
	// is a shim that never took one, because its StartSession refused and
	// rolled them back. The realtest's own occurrence (2026-09-13T18:18:41)
	// followed a `vendor_start_failed` on the very shim being adopted, one
	// bring-up earlier -- the designed recovery, working.
	inert := false
	if state == sessionlock.StateFree && socket == shimsocket.StateLive {
		log.Info(opBringUp, "the workspace lock reads free but a shim is listening; attaching to the inert survivor and starting its session",
			dlog.Context{"lock": lockPath, "socket": socketPath, "lock_state": state.String()})
		inert = true
	}
	// A SOCKET THAT COULD NOT BE PROBED IS NEVER SPAWNED ONTO, for the same
	// reason an unreadable lock is never read as free.
	if state == sessionlock.StateFree && socket == shimsocket.StateUndetermined {
		log.Error(opBringUp, "the shim socket probe could not tell", dlog.Context{
			"socket": socketPath, "cause": errText(socketErr),
		})
		return nil, pathNone, fmt.Errorf("start session for %q: the shim socket at %q could not be probed: %w", ws, socketPath, socketErr)
	}
	// A PREDECESSOR'S SHIM MAY BE STILL STARTING. A free lock and an absent
	// socket are also what a shim spawned moments ago looks like: it takes its
	// conversation locks inside StartSession and its socket is bound by Node
	// tens of milliseconds after the fork. The registry's recorded spawn pid
	// is the only witness, and it is consulted BEFORE the spawn branch below
	// concludes there is nothing here. See startingshim.
	if !inert && state == sessionlock.StateFree {
		announced, path, err := f.awaitStartingSurvivor(ctx, log, ws, udsPath)
		if err != nil {
			f.noteStartFailed(ctx, log, ws, err)
			return nil, pathNone, err
		}
		if announced {
			socketPath, socket, inert = path, shimsocket.StateLive, true
		}
	}
	if inert {
		if err := f.refuseAdoptingOurOwnSpawn(ctx, log, ws, socketPath, "inert_survivor"); err != nil {
			return nil, pathNone, err
		}
		client, err := f.adoptBounded(ctx, log, ws, dir, socketPath, "inert_survivor")
		if err != nil {
			log.Error(opBringUp, "could not attach to the inert survivor", dlog.Context{"cause": err.Error()})
			f.noteStartFailed(ctx, log, ws, err)
			return nil, pathNone, fmt.Errorf("start session for %q: adopt: %w", ws, err)
		}
		return client, pathInert, nil
	}
	// A HELD LOCK WITH NOTHING LISTENING IS, FIRST, AN OWNER GOING AWAY. A
	// shim that has just died leaves its socket behind before its lock holder
	// (shim-lock, a child that releases on its stdin's EOF) has finished
	// exiting: for that window the lock reads held over a stale socket. A
	// revival that refused in it -- two ERRORs, "the lock's owner is
	// unreachable" -- refused a workspace about to be free (e2e
	// TestASessionlessWorkspaceHandedOverDrawsItsFeedWithNoPrompt under load,
	// 2026-10-03). So the bring-up waits, bounded, for the lock to go; one
	// that never goes is the unreachable owner below.
	if state == sessionlock.StateHeld && (socket == shimsocket.StateAbsent || socket == shimsocket.StateStale) {
		state, err = f.awaitLockReleased(ctx, log, lockPath, dir)
		if state == sessionlock.StateFree {
			socketPath, socket, socketErr = shimsocket.NewestLive(f.socketProbe, udsPath)
			if socket == shimsocket.StateLive || socket == shimsocket.StateUndetermined {
				// A LISTENER APPEARED while the lock went: the kernel facts
				// moved under the wait, so the bring-up is decided again
				// from the start rather than spawned onto a live path.
				log.Info(opBringUp, "a shim began listening while the workspace lock was released; deciding the bring-up again", dlog.Context{
					"socket": socketPath, "socket_state": socket.String(), "cause": errText(socketErr),
				})
				return f.bringUpClient(ctx, log, ws, dir, udsPath, configDir, hostSessionID, src)
			}
		}
	}
	switch state {
	case sessionlock.StateHeld:
		// A HELD LOCK WITH NO LISTENER IS NOT AN ADOPTABLE SHIM. The lock is
		// keyed by the workspace DIRECTORY and the socket by the workspace ID,
		// so a shim left running for a registry row that has since been
		// forgotten keeps holding the directory's lock while the id's socket
		// never existed — and the daemon then spent the whole adoption bound
		// dialing a path nothing was ever bound to, once per prompt. Every
		// generation on disk has already been probed above, so "none is live"
		// is the whole kernel truth: the owner is UNREACHABLE, which is a
		// different sentence from "the shim refused", and dialing it cannot
		// make it true. Spawning is not the alternative — the lock is what
		// forbids a second shim on one conversation, and the survivor's own
		// StartSession would answer `conversation_owned` — so this refuses,
		// loudly and at once, naming both facts.
		if socket == shimsocket.StateAbsent || socket == shimsocket.StateStale {
			unreachable := fmt.Errorf(
				"start session for %q: the workspace lock at %q reads held but no shim is listening at %q (socket %s): the lock's owner is unreachable",
				ws, lockPath, socketPath, socket.String())
			log.Error(opBringUp, "the workspace lock is held but no shim is listening; there is nothing to adopt", dlog.Context{
				"lock_dir": lockPath, "lock_state": state.String(),
				"socket": socketPath, "socket_state": socket.String(),
			})
			f.noteStartFailed(ctx, log, ws, unreachable)
			return nil, pathNone, unreachable
		}
		log.Info(opBringUp, "a surviving shim holds the workspace lock; adopting it", dlog.Context{
			"lock_dir": lockPath, "socket": socketPath, "socket_state": socket.String(),
		})
		if err := f.refuseAdoptingOurOwnSpawn(ctx, log, ws, socketPath, "lock_held"); err != nil {
			return nil, pathNone, err
		}
		client, err := f.adoptBounded(ctx, log, ws, dir, socketPath, "lock_held")
		if err != nil {
			log.Error(opBringUp, "could not adopt the surviving shim", dlog.Context{"cause": err.Error()})
			// A FAILED ADOPTION IS A WORKSPACE FAULT, exactly as a failed spawn
			// is. It was not: an adoption that never landed opened no fault and
			// stated no dead link, so the footer showed nothing at all while
			// the queue dropped the prompt that was waiting on it.
			f.noteStartFailed(ctx, log, ws, err)
			return nil, pathNone, fmt.Errorf("start session for %q: adopt: %w", ws, err)
		}
		return client, pathAdopted, nil
	case sessionlock.StateFree:
		// THE CLASSIFIER ALREADY SETTLED FRESH-VERSUS-RESUME, transcript and
		// all, before this probe ran: a resume reaching here names a
		// transcript that was found, and a recorded conversation with none
		// comes up FRESH with its own fault rather than being refused. There
		// is nothing left for a spawn-side guard to test.
		log.Info(opBringUp, "the workspace lock is free; spawning a shim", dlog.Context{
			"lock_dir": lockPath, "socket": udsPath, "socket_state": socket.String(),
		})
		// A DEAD SHIM'S SOCKET FILE OUTLIVES IT: an AF_UNIX path is not
		// reclaimed on process death the way a flock is, so the spawn's bind
		// would fail for a reason that no longer exists. Clearing re-probes
		// before it unlinks, so a listener that appeared in the meantime is
		// refused rather than stranded, and a clear that fails refuses the
		// spawn LOUDLY instead of handing the shim a path it cannot bind.
		if clearErr := shimsocket.ClearStale(log, udsPath); clearErr != nil {
			log.Error(opBringUp, "the shim socket path could not be cleared for a spawn", dlog.Context{
				"socket": udsPath, "cause": clearErr.Error(),
			})
			f.noteStartFailed(ctx, log, ws, clearErr)
			return nil, pathNone, refuse(log, "OpenWorkspace", ArmSpawnFailed, clearErr.Error(), false)
		}
		sink, err := f.deps.Log.ShimSink(dir)
		if err != nil {
			log.Error(opBringUp, "could not borrow the shim log sink", dlog.Context{"cause": err.Error()})
			return nil, pathNone, fmt.Errorf("start session for %q: shim log sink: %w", ws, err)
		}
		// THE BUNDLE IS HELD FROM THE HASH TO THE SHIM'S ANSWER: the build the
		// spawn states is the bytes node runs.
		build, release, err := f.deps.ShimBundle.Hold()
		if err != nil {
			log.Error(opBringUp, "the installed shim bundle's build is unresolvable; no shim is spawned", dlog.Context{"cause": err.Error()})
			f.noteStartFailed(ctx, log, ws, err)
			return nil, pathNone, refuse(log, "OpenWorkspace", ArmSpawnFailed, err.Error(), false)
		}
		client, err := f.deps.Supervisor.Spawn(ctx, shimclient.Spec{
			WorkspaceID:  ws,
			WorkspaceDir: dir,
			UDSPath:      udsPath,
			StoreSocket:  f.deps.StoreSocket,
			ConfigDir:    configDir,
			SessionID:    hostSessionID,
			ShimBuildSHA: build,
			NodeBin:      f.deps.NodeBin,
			MainJS:       f.deps.MainJS,
			Fake:         f.deps.Fake,
			LogSink:      sink.File(),
			ForbidVendor: f.deps.ForbidVendor,
			// THE PID IS DURABLE FROM THE FORK. Spawn blocks until the shim
			// has bound and answered; a daemon killed inside that window
			// leaves a shim its successor cannot tell from no shim at all.
			Spawned: func(pid int) { f.recordSpawnedShimPID(ctx, log, ws, pid) },
		})
		release()
		if errors.Is(err, shimclient.ErrStandingDown) {
			// THE SUPERVISOR BEGAN STANDING DOWN between the check above and
			// this spawn: the supervisor's own refusal is the authoritative
			// answer, and it is the same refusal, recorded the same way.
			log.Info(opBringUp, "no shim is brought up: this daemon began standing down as the spawn was asked", dlog.Context{
				"workspace": string(ws), "socket": udsPath,
			})
			return nil, pathNone, fmt.Errorf("%w: %w", shimclient.ErrStandingDown,
				refuse(log, "OpenWorkspace", ArmSpawnFailed, shimclient.ErrStandingDown.Error(), false))
		}
		if err != nil {
			log.Error(opBringUp, "the shim did not come up", dlog.Context{"cause": err.Error()})
			// A BRING-UP DEATH IS A WORKSPACE FAULT, not only a failed rpc.
			// The rpc answers whoever asked; the fault and the dead link are
			// what every OTHER surface reads, and without them a workspace
			// whose shim will not start looks merely idle.
			f.noteStartFailed(ctx, log, ws, err)
			return nil, pathNone, refuse(log, "OpenWorkspace", ArmSpawnFailed, err.Error(), false)
		}
		return client, pathSpawned, nil
	default:
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "default"})
		log.Error(opBringUp, "the workspace lock probe could not tell", dlog.Context{
			"lock": lockPath, "cause": errText(err),
		})
		return nil, pathNone, fmt.Errorf("start session for %q: the workspace lock at %q could not be probed: %w", ws, lockPath, err)
	}
}

// lockReleasePoll is how often a bring-up re-probes a workspace lock whose
// owner is going away; lockReleaseBound bounds the whole wait. A dying shim's
// lock holder exits within milliseconds of its stdin's EOF on a quiet host;
// the bound is a generous multiple of that for a loaded one.
const (
	lockReleasePoll  = 20 * time.Millisecond
	lockReleaseBound = 3 * time.Second
)

// awaitLockReleased waits, bounded, for the workspace lock to read free,
// answering the last state read and the probe's error. Polled, because a
// kernel lock cannot announce its release.
func (f *Fleet) awaitLockReleased(ctx context.Context, log dlog.Logger, lockDir, dir string) (sessionlock.State, error) {
	clk := f.deps.Clock
	if clk == nil {
		clk = startingshim.SystemClock{}
	}
	log.Info(opBringUp, "the workspace lock is held with no shim listening; waiting for its owner to finish going away", dlog.Context{
		"lock_dir": lockDir, "bound_ms": lockReleaseBound.Milliseconds(),
	})
	deadline := clk.Now().Add(lockReleaseBound)
	for {
		state, err := f.probe(lockDir, dir)
		if state != sessionlock.StateHeld {
			log.Info(opBringUp, "the workspace lock's owner went away", dlog.Context{"lock_state": state.String(), "cause": errText(err)})
			return state, err
		}
		if !clk.Now().Before(deadline) {
			return state, err
		}
		select {
		case <-clk.After(lockReleasePoll):
		case <-ctx.Done():
			return state, err
		}
	}
}

// noteStartFailed records a bring-up death as the workspace's own fault and
// states the DEAD link on every surface. The link matters as much as the fault:
// the footer's disconnected step reads a dead link that never connected as
// `start_failed`, which is the sentence the user needs.
func (f *Fleet) noteStartFailed(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, cause error) {
	workspace := ws
	fault := wsm.Fault{
		Workspace: &workspace,
		Kind:      health.KindShimStartFailed,
		Detail:    "the workspace's shim would not come up",
		Evidence:  map[string]string{"stderr_tail": cause.Error()},
		OpenedAt:  f.now(),
	}
	var death *shimclient.BringUpDeathError
	if errors.As(cause, &death) {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "errors.As(cause, &death)"})
		fault.Evidence["exit_code"] = strconv.Itoa(death.Exit.Code)
		fault.Evidence["stderr_tail"] = death.Exit.Stderr
	}
	if _, err := f.deps.DB.OpenFault(ctx, fault); err != nil {
		log.Error(opBringUp, "could not record the failed bring-up", dlog.Context{"cause": err.Error()})
	}
	// THE FOOTER ALONE CARRIES A BRING-UP FAILURE (the owner's ruling of
	// 2026-09-12: no feed row). The dead link below states the `start_failed`
	// step; this states the line that explains it, out of the fault's own
	// evidence so the two cannot disagree.
	f.deps.Footer.SetStartFailed(ws, &footer.StartFailed{Detail: health.StartFailedDetail(fault)})
	f.deps.Sinks.Footer.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Topbar.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Sidebar.OnLink(ws, shimclient.LinkDead)
	f.publishHost(ws)
}

// noteSessionRefused records a StartSession the shim would not serve as the
// workspace's OWN fault, so it reaches a surface a user reads.
//
// GROUNDED, 2026-09-13: a shim refused the start of one workspace on every
// boot, and `daemon.boot.bring_up` logged it, counted it and went on. Nothing
// else happened — no fault, no line anywhere — so the workspace looked merely
// idle while it was in fact unserveable, and the same silence met the start a
// cold-gate answer re-opens with.
//
// NOT `shim_start_failed`, AND THE LINK IS NOT DEAD. That kind and the dead
// link both name the shim PROCESS, which here is up and answering: what it
// refused is the SESSION. `resume_failed` is the kind for that, its typed arm
// carries the shim's own account as the cause, and nothing about the link is
// restated — saying it died would send every surface hunting a process that is
// right there.
func (f *Fleet) noteSessionRefused(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, cause error) {
	if ctx.Err() != nil {
		// A CANCELLED CONTEXT IS A DAEMON STANDING DOWN, not a fault to file:
		// the write would fail, and the error it logged would be about the
		// shutdown rather than about this workspace.
		return
	}
	workspace := ws
	fault := wsm.Fault{
		Workspace: &workspace,
		Kind:      health.KindResumeFailed,
		Detail:    "the shim refused to start this workspace's session",
		Evidence:  map[string]string{"cause": cause.Error()},
		OpenedAt:  f.now(),
	}
	if _, err := f.deps.DB.OpenFault(ctx, fault); err != nil {
		log.Error(opBringUp, "could not record the refused session start", dlog.Context{"cause": err.Error()})
		return
	}
	f.publishHost(ws)
}

// closeOnEdge closes every standing fault of one workspace whose declared
// lifetime (health/lifetime.go) ends at edge. The kinds are the table's, never
// a list here: a healthy attach and a started session once closed hand-listed
// kinds, and the kinds nobody listed stood until the next daemon restart.
func (f *Fleet) closeOnEdge(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, edge health.Edge) {
	workspace := ws
	health.CloseOnEdge(ctx, f.deps.DB, log, edge, health.EdgeScope{Workspace: &workspace}, f.now())
}

// portAcrossAccounts carries a session's vendor transcript from the root it
// was filed under into the one this boot routes the workspace to.
//
// A FRESH start ports nothing: there is no conversation to carry, and the new
// root is simply where this one is filed.
func (f *Fleet) portAcrossAccounts(
	ctx context.Context,
	log dlog.Logger,
	dir string,
	session wsm.Session,
	routed string,
	src source,
) error {
	log.Info(opBringUp, "the workspace's account routing changed since the session was recorded", dlog.Context{
		"recorded_config_dir": session.ConfigDir, "routed_config_dir": routed, "fresh": src.Fresh,
	})
	if src.Fresh {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "src.Fresh"})
		return nil
	}
	transcript, err := f.deps.Accounts.FindTranscript(ctx, dir, src.VendorSessionID)
	if err != nil {
		// The resume guard below refuses a missing transcript with its own
		// arm; nothing is invented here.
		log.Warn(opBringUp, "no transcript to port across the account switch", dlog.Context{
			"vendor_session_id": src.VendorSessionID, "cause": err.Error(),
		})
		return nil
	}
	if transcript.ConfigDir == routed {
		log.Debug(opBringUp, "the transcript already lives under the routed root", dlog.Context{
			"config_dir": routed,
		})
		return nil
	}
	if err := f.deps.Accounts.MoveTranscript(ctx, transcript.Path, routed, dir); err != nil {
		log.Error(opBringUp, "could not port the transcript across the account switch", dlog.Context{
			"from": transcript.ConfigDir, "to": routed, "cause": err.Error(),
		})
		return fmt.Errorf("port the transcript of %q into %q: %w", dir, routed, err)
	}
	log.Info(opBringUp, "ported the transcript across the account switch", dlog.Context{
		"from": transcript.ConfigDir, "to": routed, "vendor_session_id": src.VendorSessionID,
	})
	return nil
}

// DefaultLaunchModel is the model a fresh session runs under when the user
// named none (owner ruling 2026-09-14: "the default model should be opus ...
// what's selected if the user does nothing").
//
// `opus` is a FAMILY ALIAS, not a pinned version: the SDK's availableModels
// documents that "opus" allows any opus version, and the served catalog carries
// an opus row of its own — so naming the alias here launches a real, nameable
// model that modelSelector can match to its option, rather than the empty
// override that surfaces as the `<synthetic>` marker.
//
// IT IS THE MODEL FAMILY THIS REPO ALREADY NAMES: the fake catalog's own
// default model is spelled "opus" (fakeshim.DefaultModel), so the daemon states
// the same word its own catalog does rather than inventing a pinned id.
const DefaultLaunchModel = "opus"

// freshModel answers the model a fresh session names.
//
// A CHOICE, OR THE DEFAULT — NEVER THE VENDOR'S OWN. StartSessionFresh.model is
// optional on the wire, but an unset one let the SDK pick whatever it liked and
// the CLI then reported that pick as the `<synthetic>` marker, which names no
// selectable model — so the button had no selection and "the actual default"
// was nothing at all. This reverses landing 7's "substitute nothing": an unset
// choice becomes DefaultLaunchModel, so a session the user never modeled runs
// under opus and the selector draws opus as the selection.
// SessionStarted.effective_model is what took effect, and recordFacts persists
// that.
func freshModel(recorded string) *conversationv1.AgentModel {
	name := recorded
	if name == "" {
		name = DefaultLaunchModel
	}
	return &conversationv1.AgentModel{Name: name}
}

// DefaultStartSessionBound bounds ONE StartSession call.
//
// A START THAT NEVER ANSWERS IS A WORKSPACE THAT NEVER COMES BACK. StartSession
// was the one step of the bring-up with no bound at all: the shim client's
// dial ladder is bounded, an adoption is bounded by AdoptBound, and then the
// rpc that actually starts the session was allowed to take forever. It also
// holds the workspace's START GATE for its whole duration, so every other
// route to starting that workspace waits behind it.
//
// MEASURED, realtest run 2026-09-13T16:20:34. Workspace 2b81f45a724642ef's
// shim accepted the start, logged `shim.convert.hooks: a hook blocked the
// gated action` on `SessionStart:resume`, and never answered. Three daemon
// generations each sat in that call until the NEXT deploy's SIGTERM ended the
// process -- 35s, 3m30s and 8m30s -- with the workspace sessionless the whole
// time and nothing in the daemon's log saying why.
//
// IT IS A PRODUCT WINDOW, NOT A BOUND ON THE DAEMON'S OWN WORK, and it is
// sized the way boot.DefaultAdoptBound is. What it has to cover is the shim's
// vendor resume plus whatever SessionStart hooks the user has configured,
// which are arbitrary commands this daemon cannot measure. Every HEALTHY start
// in that same run finished far inside it: the whole bring-up -- spawn, ready,
// StartSession and the watcher -- ran 116ms, 245ms and 254ms, and the
// StartSession rpc is a fraction of each. 60s is therefore ~240x the slowest
// healthy start observed, which is deliberate: this bound exists to end a
// start that has HUNG, never to hurry a slow one.
//
// AGENT_REPL_START_SESSION_BOUND overrides it; a malformed or non-positive
// value is REFUSED, never ignored.
const DefaultStartSessionBound = 60 * time.Second

// askToStartSession sends ONE StartSession and answers its outcome. A nil
// SessionStarted with a nil error means the session is parked behind a
// standing cold gate, which is an answer and not a failure. Every refusal is
// LABELED (startLabel) with the shim's verdict on whether asking again can
// help; startSession owns what follows from it.
func (f *Fleet) askToStartSession(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, src source, session wsm.Session, configDir string) (*conversationv1.SessionStarted, error) {
	req := &shimv1.StartSessionRequest{}
	if src.Fresh {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "src.Fresh"})
		req.Source = &shimv1.StartSessionRequest_Fresh{Fresh: &shimv1.StartSessionFresh{
			Model:          freshModel(session.Model),
			PermissionMode: permissionMode(session.PermissionMode),
		}}
	} else {
		resume := &shimv1.StartSessionResume{
			VendorSessionId: src.VendorSessionID,
			ColdRemediation: src.ColdRemediation,
		}
		// A ROLLED-BACK TURN STAYS ROLLED BACK ACROSS A RESTART: the vendor's
		// transcript still ends on the dropped branch until the next prompt
		// appends past the cut, so the shim is told every rolled-back turn and
		// resumes before them (StartSessionResume.rolled_back_turns).
		rolledBack, err := f.deps.DB.RolledBackTurns(ctx, ws)
		if err != nil {
			log.Error("daemon.workspace.start_session", "the rolled-back turns could not be read; the session was not resumed",
				dlog.Context{"cause": err.Error()})
			return nil, fmt.Errorf("read the rolled-back turns of %q: %w", ws, err)
		}
		for _, turn := range rolledBack {
			resume.RolledBackTurns = append(resume.RolledBackTurns, &conversationv1.TurnId{Value: string(turn)})
		}
		if src.Rebind {
			// PRESENCE IS THE FACT. The marker tells the shim to adopt this
			// conversation's own identity as the workspace's book; without it
			// the shim keeps the persisted one and the feed replays the
			// conversation the user just replaced.
			resume.Rebind = &shimv1.StartSessionRebind{}
		}
		req.Source = &shimv1.StartSessionRequest_Resume{Resume: resume}
	}

	// THE CALL IS BOUNDED. See DefaultStartSessionBound: a shim that accepts
	// the start and never answers held this call, and the workspace's start
	// gate with it, until the process died.
	startCtx, endStart := context.WithTimeout(ctx, f.startBound)
	defer endStart()
	response, err := client.StartSession(startCtx, req)
	if err != nil {
		// AN EXPIRED BOUND IS NAMED, never left as a bare deadline. "context
		// deadline exceeded" says nothing about which step spent it, and this
		// one is the difference between a shim that is not there and a shim
		// that took the request and went quiet -- which are remediated
		// differently and which the log has to tell apart.
		if errors.Is(err, context.DeadlineExceeded) && ctx.Err() == nil {
			log.Error(opBringUp, "the shim accepted the start and did not answer inside its bound", dlog.Context{
				"bound_ms": f.startBound.Milliseconds(), "shim_pid": client.PID(), "cause": err.Error(),
			})
			return nil, fmt.Errorf("start session for %q: the shim did not answer StartSession within %s: %w",
				ws, f.startBound, err)
		}
		// A START THAT DIED IN A TEARDOWN THIS DAEMON ORDERED IS NOT A FAILED
		// START. The shim client latches the KillSession or Kill the daemon
		// asked for, and a StartSession still in flight to that shim then
		// comes back `unavailable: unexpected EOF` -- not because the session
		// would not come up, but because the daemon killed the shim it was
		// asking. The error is still returned and the bring-up still stops;
		// only the record says which of the two happened.
		if errors.Is(err, shimclient.ErrStandDownOrdered) {
			log.Info(opBringUp, "the StartSession call ended in a stand-down this daemon ordered",
				dlog.Context{"cause": err.Error()})
			return nil, fmt.Errorf("start session for %q: %w", ws, err)
		}
		log.Error(opBringUp, "the StartSession call failed", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("start session for %q: %w", ws, err)
	}
	if failure := response.GetFailure(); failure != nil {
		if cold := failure.GetCold(); cold != nil {
			f.raiseColdGate(ws, src.VendorSessionID, cold, configDir)
			// AN ANSWER, NOT A FAILURE -- as this function's own doc says. The
			// gate is a designed product state: the shim states the cost of
			// resuming a large context, `raiseColdGate` publishes it to the
			// footer and the feed, and the USER chooses pay, clear or compact.
			// Nothing is broken and nothing is to be fixed, so it is INFO,
			// carrying the cost facts that no other record does.
			log.Info(opBringUp, "the session is parked behind a cold gate", dlog.Context{
				"context_tokens":  cold.GetContextTokens(),
				"requested_model": cold.GetRequestedModel().GetName(),
			})
			return nil, nil
		}
		if failure.GetConversationOwned() != nil {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "failure.GetConversationOwned() != nil"})
			// ANOTHER SHIM HOLDS THIS CONVERSATION. It took the workspace
			// kernel lock first, which is exactly what that lock is for: two
			// vendor processes on one conversation is the state it prevents.
			// The refusal is the shim's own verdict, relayed. NOT RETRYABLE:
			// the other shim's ownership is a real conflict.
			return nil, labeled(refuse(log, "OpenWorkspace", ArmConversationOwned,
				fmt.Sprintf("another shim holds workspace %q's conversation: %s", ws, failure.GetDetail()), false),
				false, false, failure.GetDetail())
		}
		if unavailable := failure.GetLockHolderUnavailable(); unavailable != nil {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "failure.GetLockHolderUnavailable() != nil"})
			// THE SHIM'S OWN LOCK HELPER FAILED. No claim was completed, so
			// nobody is known to own the conversation: saying "another shim
			// holds it" here would send the reader hunting for a process that
			// does not exist. The shim's LockHolderFailure rides the typed arm
			// whole, because how that binary failed is the remediation.
			holder := unavailable.GetFailure()
			how, stated := describeLockHolderFailure(holder)
			if !stated {
				log.Error(opBringUp, "the shim's lock_holder_unavailable refusal states no how; relayed as given",
					dlog.Context{"binary": holder.GetBinary(), "detail": failure.GetDetail()})
			}
			// RETRYABLE (shim.v1): a holder that failed to spawn or answer may
			// succeed on the next StartSession on the same shim.
			return nil, labeled(refuseWith(log, "OpenWorkspace", ArmLockHolderUnavailable,
				fmt.Sprintf("the shim's lock helper %s %s for workspace %q; nobody owns the conversation",
					holder.GetBinary(), how, ws), false,
				map[string]any{"failure": holder}),
				true, false, fmt.Sprintf("the lock helper %s %s", holder.GetBinary(), how))
		}
		if failure.GetUnknownSession() != nil {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "failure.GetUnknownSession() != nil"})
			// THE RESUME NAMED A CONVERSATION THE SHIM HAS NO TRANSCRIPT FOR.
			// The classifier is what keeps a never-turned session off this
			// path; reaching it anyway is a real vanished transcript, and it
			// gets its NAMED arm rather than a generic sentence, because
			// "no transcript exists" is remediated differently from every
			// other StartSession refusal.
			// NOT RETRYABLE: the transcript will not reappear by asking again.
			return nil, labeled(refuse(log, "OpenWorkspace", ArmUnknownSession,
				fmt.Sprintf("the shim has no transcript for conversation %q: %s", src.VendorSessionID, failure.GetDetail()), false),
				false, false, failure.GetDetail())
		}
		if vendor := failure.GetVendorStartFailed(); vendor != nil {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "failure.GetVendorStartFailed() != nil"})
			// THE VENDOR FAILED TO START INSIDE A HEALTHY SHIM. The shim
			// process is up and serving — only its StartSession answer is a
			// refusal — so neither `spawn_failed` nor `shim_start_failed`,
			// which both name the SHIM PROCESS, describes it. LANDING 9 gave
			// it its own arm: OpenWorkspaceError.vendor_start_failed carries
			// the shim's OWN account in `detail`, so the verdict is relayed
			// typed rather than through the unlanded-arm convention.
			refusal := refuseWith(log, "OpenWorkspace", ArmVendorStartFailed,
				fmt.Sprintf("the vendor failed to start for workspace %q: %s", ws, failure.GetDetail()), false,
				map[string]any{"detail": failure.GetDetail()})
			// THE SHIM LABELS WHETHER ASKING AGAIN CAN HELP, and the daemon
			// never re-derives it from `detail`. A frame with neither arm is
			// malformed: it is treated as a rejection, loudly.
			switch retry := vendor.GetRetry().(type) {
			case *shimv1.StartSessionVendorStartFailed_Retryable:
				// WHOSE FAILURE IT WAS is the shim's to say too: an
				// unreachable network is not the vendor's fault, and the
				// daemon draws it as a network fault while it retries.
				switch retry.Retryable.GetCause().(type) {
				case *shimv1.StartSessionVendorStartRetryable_Network:
					return nil, labeledNetwork(refusal, failure.GetDetail())
				case *shimv1.StartSessionVendorStartRetryable_Vendor:
				default:
					log.Error(opBringUp, "the shim's retryable vendor start names no cause; it is read as the vendor's", dlog.Context{
						"detail":              failure.GetDetail(),
						"invariant_violation": "StartSessionVendorStartRetryable.cause is always set",
					})
				}
				return nil, labeled(refusal, true, true, failure.GetDetail())
			case *shimv1.StartSessionVendorStartFailed_Rejected:
				return nil, labeled(refusal, false, true, failure.GetDetail())
			default:
				log.Error(opBringUp, "the shim's vendor_start_failed states neither retryable nor rejected; treated as a rejection", dlog.Context{
					"detail":              failure.GetDetail(),
					"invariant_violation": "StartSessionVendorStartFailed.retry is always set",
				})
				return nil, labeled(refusal, false, true, failure.GetDetail())
			}
		}
		if failure.GetAlreadyStarted() != nil {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "failure.GetAlreadyStarted() != nil"})
			// THE SHIM ALREADY SERVES A SESSION. One shim serves exactly one,
			// so this is a StartSession the daemon should never have sent: the
			// bring-up dialed a shim that is already live. It is a NAMED state
			// with its own remediation (attach, do not start), and an untyped
			// `internal` on a contract path hides it from every client.
			return nil, refuse(log, "OpenWorkspace", ArmAlreadyStarted,
				fmt.Sprintf("the shim serving workspace %q already started its session: %s", ws, failure.GetDetail()), false)
		}
		// THE CAUSE ONEOF IS UNSET — illegal on the wire. It is surfaced under
		// its own arm rather than guessed at or collapsed into an untyped
		// error, exactly as SetSessionModelFailure's unset cause is.
		log.Error(opBringUp, "StartSession refused", dlog.Context{"detail": failure.GetDetail()})
		return nil, refuse(log, "OpenWorkspace", ArmStartSessionUnspecified,
			fmt.Sprintf("the shim refused to start workspace %q's session with no cause set: %s", ws, failure.GetDetail()), false)
	}
	return response.GetSuccess().GetSession(), nil
}

// raiseColdGate publishes the gate's row and the footer's parked status, and
// remembers the MENU it served so the answer can be echoed against it.
//
// The compact menu is the models the session could be compacted onto: the model
// the cold start requested, plus the model the record holds when it differs.
// Every scope but the unspecified one is offered, because an unspecified scope
// is not a choice.
func (f *Fleet) raiseColdGate(ws ids.WorkspaceID, vendorSessionID string, cold *conversationv1.SessionCold, configDir string) {
	// EVERY ACCOUNT IS OFFERED COMPACTION (owner ruling 2026-10-06, reversing
	// the 2026-09-30 work-account exclusion): the work account's gate offers
	// compact too.
	compact, menu := coldCompactMenu(cold)

	cost := coldGateCost(cold, f.deps.Topbar.ContextWindow(ws))
	detail := cost.Text()

	f.mu.Lock()
	held, stood := f.coldGates[ws]
	stood = stood && !held.answering
	f.coldGates[ws] = coldGate{served: ServedColdGate{
		VendorSessionID: vendorSessionID, Compact: compact, Detail: detail}, configDir: configDir}
	f.lastCold[ws] = cold
	f.mu.Unlock()
	f.logTransition(ws, "cold_gate_standing", stood, true,
		dlog.Context{"vendor_session_id": vendorSessionID, "compaction_offered": compact != nil})

	f.deps.Feed.UpsertSynthesized(ws, feedid.Feed{Root: true}, &frontendv1.FeedRow{
		Id: coldGateRowID(ws, vendorSessionID),
		Row: &frontendv1.FeedRow_ColdGate{ColdGate: &frontendv1.FeedColdGate{
			State: &frontendv1.FeedColdGate_Standing{Standing: &frontendv1.FeedColdGateStanding{
				ContextTokens: &frontendv1.FeedColdGateContextTokens{
					Tokens: int64(cold.GetContextTokens()), WindowFill: cost.WindowFill},
				LastRequest: &frontendv1.FeedColdGateLastRequest{AtMs: cold.GetLastRequestAtMs()},
				Model:       &frontendv1.FeedColdGateModel{Model: cold.GetRequestedModel()},
				Compact:     menu,
			}},
		}},
	})
	f.deps.Footer.SetColdGate(ws, footer.ColdGate{Standing: true, Cost: cost})
	f.deps.Topbar.SetColdGate(ws, topbar.ColdGate{
		Standing:      true,
		ContextTokens: int64(cold.GetContextTokens()),
	})
	f.publishHost(ws)
}

// coldCompactMenu is the compact menu a gate serves: the model the cold start
// requested, and every scope but the unspecified one, which is not a choice.
// The served half is what the answer is echoed against; the drawn half is the
// gate card's submenu.
func coldCompactMenu(cold *conversationv1.SessionCold) (*ServedColdGateCompact, *frontendv1.FeedColdGateCompactMenu) {
	served := &ServedColdGateCompact{
		Models: []*conversationv1.AgentModel{cold.GetRequestedModel()},
		Scopes: []conversationv1.SessionCompactScope{
			conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL,
			conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_PROMPTS,
			conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_RESPONSES,
		},
	}
	options := make([]*frontendv1.FeedColdGateModelOption, 0, len(served.Models))
	for _, m := range served.Models {
		options = append(options, &frontendv1.FeedColdGateModelOption{Model: m})
	}
	return served, &frontendv1.FeedColdGateCompactMenu{Models: options, Scopes: served.Scopes}
}

// englishPrinter groups digits ("409,051") in counts a user reads.
var englishPrinter = message.NewPrinter(language.English)

// coldGateCost is the ONE sentence a standing gate is accounted for by, in the
// parts the footer draws it in. The footer's cold-gate line, the served gate
// the verbs read, and the `cold_gate` arm a prompt to a parked workspace is
// refused with all take it from here (its Text): three surfaces wording one
// gate three ways is how a user comes to think they are looking at three
// problems.
//
// The figure's window fill is the count over WINDOW, the window the context
// chip measures against (topbar.Resolver.ContextWindow: the vendor's stated
// window, else the assumed 1,000,000), so the gate and the chip agree on how
// full one count is. The gate's fill and the feed figure's are this one value.
func coldGateCost(cold *conversationv1.SessionCold, window int64) footer.ColdGateCost {
	tokens := cold.GetContextTokens()
	return footer.ColdGateCost{
		Lead:       "the conversation is cold at ",
		Figure:     englishPrinter.Sprintf("%d", tokens),
		Tail:       " context tokens",
		WindowFill: topbar.WindowFill(int64(tokens), window),
	}
}

// ColdGateShown answers whether a cold gate stands on the workspace UNANSWERED:
// the gate the user sees and must answer. It is the host view's gate, and the
// feed's standing gate row holds exactly as long (the row retires the moment an
// answer takes the gate, and returns when a failed re-open raises it again), so
// Emacs's hidden input and the webapp's docked banner move together.
func (f *Fleet) ColdGateShown(ws ids.WorkspaceID) bool {
	f.mu.RLock()
	defer f.mu.RUnlock()
	held, ok := f.coldGates[ws]
	return ok && !held.answering
}

// ColdGateDetail answers the standing gate's account for a workspace, false
// when no gate stands. It is the promptqueue's ColdGateFunc: a prompt to a
// parked session is refused by the gate's OWN name, carrying the gate's own
// sentence.
func (f *Fleet) ColdGateDetail(ws ids.WorkspaceID) (string, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	held, ok := f.coldGates[ws]
	if !ok || held.remediated {
		return "", false
	}
	return held.served.Detail, true
}

// recordFacts persists the session facts that outlive one shim process: the
// vendor identity, the config dir it was spawned under, and the model and mode
// in force. The shim pid and the shim's build are LOGGED rather than persisted,
// because both belong to the process rather than to the session.
func (f *Fleet) recordFacts(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, previous wsm.Session, started *conversationv1.SessionStarted, configDir, hostSessionID string, pid int) error {
	now := f.now()
	next := wsm.Session{
		Workspace:       ws,
		HostSessionID:   hostSessionID,
		VendorSessionID: started.GetVendorSessionId(),
		ConfigDir:       configDir,
		// THE CHOICE SURVIVES THE START THAT HONORED IT. recordFacts composes
		// the row fresh, so a selected root left out here would be erased by
		// the very bring-up it asked for and the next one would re-route.
		SelectedConfigDir: previous.SelectedConfigDir,
		Model:             started.GetEffectiveModel().GetName(),
		PermissionMode:    permissionModeName(started.GetPermissionMode()),
		StartedAt:         previous.StartedAt,
		LastEngagementAt:  now,
	}
	f.noteSDKVersion(log, started.GetRuntime().GetSdkVersion())
	if next.StartedAt.IsZero() {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "next.StartedAt.IsZero()"})
		next.StartedAt = now
	}
	if err := f.deps.DB.PutSession(ctx, next); err != nil {
		log.Error(opBringUp, "could not record the session facts", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("start session for %q: record the session facts: %w", ws, err)
	}
	log.Debug(opBringUp, "recorded the session facts", dlog.Context{
		"host_session_id":   next.HostSessionID,
		"vendor_session_id": next.VendorSessionID,
		"config_dir":        next.ConfigDir,
		"model":             next.Model,
		"permission_mode":   next.PermissionMode,
		"shim_pid":          pid,
		"shim_build_sha":    started.GetRuntime().GetShimBuildSha(),
	})
	return nil
}

// noteSDKVersion keeps the Agent SDK version a session start reported as the
// one agent-repl runs. The shim states it on every start, so an empty one is
// a malformed report: recorded at ERROR and not kept, never a guessed version.
func (f *Fleet) noteSDKVersion(log dlog.Logger, version string) {
	if version == "" {
		log.Error(opBringUp, "the session start reported no Agent SDK version", dlog.Context{
			"invariant_violation": "SessionRuntime.sdk_version is stated on every session start",
		})
		return
	}
	f.mu.Lock()
	before := f.sdkVersion
	f.sdkVersion = version
	f.mu.Unlock()
	if before != version {
		log.Info(opBringUp, "the shim reported the Agent SDK version agent-repl runs", dlog.Context{
			"sdk_version": version, "previous": before,
		})
	}
}

// SDKVersion answers the Agent SDK version the shim last reported on a session
// start, false when no session has started since this daemon did.
func (f *Fleet) SDKVersion() (string, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return f.sdkVersion, f.sdkVersion != ""
}

// Stop ends a workspace's session. Forced stops kill the process; a graceful
// stop leaves the shim to end its own session first. Stopping a workspace with
// no live session is SUCCESS: the caller asked for a state that already holds.
func (f *Fleet) Stop(ctx context.Context, ws ids.WorkspaceID, force bool) error {
	f.mu.Lock()
	session, ok := f.sessions[ws]
	_, gated := f.coldGates[ws]
	delete(f.sessions, ws)
	delete(f.coldGates, ws)
	delete(f.lastCold, ws)
	f.mu.Unlock()
	if !ok {
		// A gate can stand with no session in the map; its retirement still
		// moves the host view.
		if gated {
			f.publishHost(ws)
		}
		return nil
	}
	f.logTransition(ws, "session_live", true, false, dlog.Context{"force": force})
	// THE SESSION IS GONE from this daemon's point of view the moment it
	// leaves the map: the host view's session arm changes here, whatever the
	// teardown below then does.
	defer f.publishHost(ws)
	// THE STAND-DOWN IS ARMED BEFORE THE WATCHER CLOSES. A stop is a teardown
	// this daemon orders, and the watcher's close reads the latch to tell the
	// bounce registry HOW the shim departed: an ordered departure unregisters
	// a registered shim bounce, while an unasked one relaunches the shim. Left
	// to the kill below, the latch arrived after the close, so a stop that no
	// KillSession preceded (a transcript bind's swap) read as a close on a
	// running shim, the registry heard no departure at all, and a bounce
	// registered behind the stopped session's work waited for the next
	// session's edges instead of being decided.
	session.client.StandDown()
	if session.watcher != nil {
		if err := session.watcher.Close(); err != nil {
			return fmt.Errorf("stop session for %q: close the watcher: %w", ws, err)
		}
	}
	// THE CALLER'S BOUND REACHES THE KILL. It did not: this handed Kill no
	// context at all, so the drain's stand-down sat through the whole SIGTERM
	// grace and the escalation after it whatever its own budget said. The
	// bound is real now, and shimclient.GracefulKillBound is what a caller
	// must leave for a graceful stop to fit inside it.
	killErr := session.client.Kill(ctx, shimclient.KillAttribution{
		Actor:  "workspace.stop",
		Reason: "the workspace's session was stopped",
		Force:  force,
	})
	// THE VIEWS ARE TOLD HERE, not left to the connectivity feed, AND ON EVERY
	// PATH OUT. The watcher was closed above, so the client's own LinkDead
	// publish has nobody left to route it: whether the views ever saw the death
	// would otherwise depend on the exit landing before the close, which is a
	// race the stop itself can settle.
	//
	// A FAILED KILL IS STILL A DEAD SESSION AS FAR AS THE VIEWS GO. The session
	// left this fleet's map at the top of this function and the kill has sent
	// everything it is going to send; a return that skipped this would leave the
	// footer, topbar and sidebar showing a live link for a session nothing is
	// serving. The error below is what says the reap was never witnessed.
	f.deps.Sinks.Footer.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Topbar.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Sidebar.OnLink(ws, shimclient.LinkDead)
	if killErr != nil {
		return fmt.Errorf("stop session for %q: kill the shim: %w", ws, killErr)
	}
	f.noteReap(ws, session.client)
	f.deps.Log.Global().With(dlog.Context{"workspace": string(ws)}).Info(opBringUp,
		"stopped the workspace session", dlog.Context{"force": force, "shim_pid": session.client.PID()})
	return nil
}

// Shim answers the narrow shim surface the verbs drive. It is the ShimFunc
// Deps.Shim takes.
func (f *Fleet) Shim(ws ids.WorkspaceID) (Shim, bool) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok {
		return nil, false
	}
	// A REAPED CLIENT IS NO SESSION. The map entry outlives the process — a
	// shim killed out from under the daemon leaves its row behind until
	// something tears it down — and reading liveness from map presence alone
	// would send the verb over a dead connection, which answers a raw transport
	// error where the contract spells no_session.
	if _, reaped := session.client.Reaped(); reaped {
		return nil, false
	}
	return &shimAdapter{client: session.client}, true
}

// Workspaces answers every workspace this fleet holds a session for, sorted.
// It reads the fleet's own map, never the state client, so it still answers
// when the store under the state root is gone -- the state-root-loss
// stand-down walks it to stop every shim nothing will be left to adopt.
func (f *Fleet) Workspaces() []ids.WorkspaceID {
	f.mu.RLock()
	out := make([]ids.WorkspaceID, 0, len(f.sessions))
	for ws := range f.sessions {
		out = append(out, ws)
	}
	f.mu.RUnlock()
	slices.Sort(out)
	return out
}

// ShimPIDs answers the pid of every live shim this fleet holds, sorted: the
// processes agent-repl's vendor traffic is measured under (internal/
// vendortraffic). A reaped client is no live process, and an adopted one whose
// pid its socket could not answer (pid 0, recorded at WARN by the adoption)
// names nothing to measure, so neither is answered.
func (f *Fleet) ShimPIDs() []int {
	f.mu.RLock()
	out := make([]int, 0, len(f.sessions))
	for _, session := range f.sessions {
		if _, reaped := session.client.Reaped(); reaped {
			continue
		}
		if pid := session.client.PID(); pid > 0 {
			out = append(out, pid)
		}
	}
	f.mu.RUnlock()
	slices.Sort(out)
	return out
}

// CloseWatchers closes every live session's watcher and JOINS whatever sink
// work each of them still had in flight. It KILLS NOTHING -- a watcher's close
// ends watching, never the session -- and it is the daemon's teardown step
// before the state client is closed: a watcher's sinks read that client, and a
// turn end being handled while the store closes under it is a refused read on
// a path that owes no error at all.
func (f *Fleet) CloseWatchers() {
	f.mu.Lock()
	watchers := make([]sessionwatcher.Watcher, 0, len(f.sessions))
	for _, session := range f.sessions {
		if session.watcher != nil {
			watchers = append(watchers, session.watcher)
		}
	}
	f.mu.Unlock()
	for _, w := range watchers {
		if err := w.Close(); err != nil {
			f.deps.Log.Global().Error("daemon.workspace.close_watchers",
				"a session watcher could not be closed", dlog.Context{"error": err.Error()})
		}
	}
}

// stampSession binds a workspace logger to the session identity the daemon
// exported to that session's shim (AGENT_REPL_SESSION_ID), so daemon and shim
// records of one session correlate through agent_repl_session_id. An empty
// identity leaves the logger unstamped rather than writing an empty field.
func stampSession(log dlog.Logger, hostSessionID string) dlog.Logger {
	if hostSessionID == "" {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "hostSessionID == \"\""})
		return log
	}
	return log.With(dlog.Context{dlog.KeyAgentReplSessionID: hostSessionID})
}

// retireReaped drops the workspace's session row when the process behind it is
// GONE, and closes the watches that were open on it. It is the teardown a shim
// killed out from under the daemon never gets: nothing signals the fleet that
// the row is stale, so the next bring-up is what tears it down.
//
// It is a NO-OP on a live session, which is what lets every bring-up call it
// unconditionally. The dead link's faults are untouched — they are the
// operator's record of the death and the next healthy attach retracts them.
func (f *Fleet) retireReaped(ws ids.WorkspaceID) {
	f.mu.Lock()
	session, ok := f.sessions[ws]
	if !ok {
		f.mu.Unlock()
		return
	}
	if _, reaped := session.client.Reaped(); !reaped {
		f.mu.Unlock()
		return
	}
	delete(f.sessions, ws)
	delete(f.coldGates, ws)
	delete(f.lastCold, ws)
	f.noteReapLocked(ws, session.client)
	f.mu.Unlock()
	f.logTransition(ws, "session_live", true, false,
		dlog.Context{"reason": "shim_reaped", "shim_pid": session.client.PID()})

	// The watcher is closed OUTSIDE the lock: closing joins whatever sink work
	// it still had in flight, and those sinks read the fleet.
	if session.watcher != nil {
		if err := session.watcher.Close(); err != nil {
			f.deps.Log.Global().Error(opBringUp, "a dead session's watcher could not be closed",
				dlog.Context{"workspace": string(ws), "cause": err.Error()})
		}
	}
	f.deps.Log.Global().Info(opBringUp, "retired the session of a shim that is gone", dlog.Context{
		"workspace": string(ws), "shim_pid": session.client.PID(),
	})
	f.publishHost(ws)
}

// remember records a workspace's live session.
//
// A DISPLACED WATCHER IS CLOSED HERE, not left to the caller. The map entry is
// the only handle a watcher has: overwriting it with a new session dropped the
// old one on the floor with its streams still standing, and those streams then
// ended on their own -- against a fleet nothing had told, which recorded the
// end as a severing. Closing it in the one place a session is displaced is what
// makes that unrepresentable rather than a rule each caller must remember.
//
// The close runs OFF the lock: it cancels the watcher's context, drains its
// streams and joins its in-flight sink dispatch, none of which may hold the
// fleet's lock.
// hold is how the fleet BEGINS holding a live client for a workspace, and it
// is what makes THE LIVE-SHIM INVARIANT structural rather than a step each
// arrival has to remember to take: the workspace's terminal session record is
// retired in the same breath as the client is installed, so no surface that
// composes off the record can go on calling a serving workspace's session dead
// (see retireTerminalRecord for what that cost).
//
// A RESTATEMENT OF THE SAME CLIENT IS NOT A NEW ARRIVAL. sessionUp states its
// entry twice — once before the watcher opens, because the opening facts are
// published synchronously against it, and once after, to carry the watcher —
// and the record was already retired when that client arrived, so the second
// statement writes nothing.
func (f *Fleet) hold(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, session *live) error {
	f.mu.RLock()
	previous, held := f.sessions[ws]
	handedOver := f.handedOver[ws]
	f.mu.RUnlock()
	restated := held && previous != nil && previous.client == session.client
	// A SHIM THAT ARRIVES AFTER ITS WORKSPACE WAS HANDED OVER SERVES NOTHING.
	// The transfer found nothing to detach -- the start had not spawned yet --
	// and released the serving row; holding the client and claiming the row
	// now would leave the successor waiting for a release that never comes
	// (e2e TestEmacsHandoverTransfersAtFreeness, 2026-10-03). The shim is the
	// successor's: detached, never stopped, it keeps its lock and the
	// successor adopts it as it adopts any handed shim.
	if !restated && handedOver {
		session.client.Detach()
		log.Info(opBringUp, "a shim arrived after its workspace was handed to a successor; it is left running for the successor to adopt", dlog.Context{
			"shim_pid": session.client.PID(),
		})
		return fmt.Errorf("start session for %q: %w", ws, ErrShimTaken)
	}
	f.remember(ws, session)
	if restated {
		return nil
	}
	// A NEW ARRIVAL IS SERVED BY THIS DAEMON, and the durable row says so
	// before the bring-up can report the session up. See claimServing.
	if err := f.claimServing(ctx, log, ws); err != nil {
		return err
	}
	if err := retireTerminalRecord(ctx, log, f.deps.DB, opBringUp, ws); err != nil {
		return err
	}
	// A HELD CLIENT WITH NO SESSION STARTED IS A HISTORY SOURCE all the same
	// (a cold gate, a parked adoption): a reader that opened before it has the
	// newest page pushed (history.go). A STARTED session's readers are kicked
	// by its watch opening, as they always were: a fresh book is not written
	// until its first turn, and reading it before then asks the store for a
	// book that does not exist yet.
	if !session.sessionStarted && !session.freshStart {
		f.deps.Feed.SourceUp(ws)
	}
	return nil
}

func (f *Fleet) remember(ws ids.WorkspaceID, session *live) {
	f.mu.Lock()
	previous, stood := f.sessions[ws]
	f.sessions[ws] = session
	f.mu.Unlock()
	if stood && previous != nil && previous.watcher != nil && previous.watcher != session.watcher {
		f.closeDisplaced(ws, previous.watcher, "the workspace's session was replaced")
	}
	f.logTransition(ws, "session_live", stood, true,
		dlog.Context{"shim_pid": session.client.PID(), "watcher_attached": session.watcher != nil})
}

// restate rewrites the entry of a client THIS START already holds: the cold
// gate's park, the session coming up on it. It is never an arrival, so it
// claims nothing; and a client the entry no longer holds was taken from the
// start meanwhile (a handover's transfer detached it, a kill stopped it), so
// the start serves nothing and says so with ErrShimTaken.
func (f *Fleet) restate(ws ids.WorkspaceID, session *live) error {
	f.mu.Lock()
	previous, ok := f.sessions[ws]
	if !ok || previous.client != session.client {
		f.mu.Unlock()
		return fmt.Errorf("start session for %q: %w", ws, ErrShimTaken)
	}
	f.sessions[ws] = session
	f.mu.Unlock()
	if previous.watcher != nil && previous.watcher != session.watcher {
		f.closeDisplaced(ws, previous.watcher, "the workspace's session was replaced")
	}
	f.logTransition(ws, "session_live", true, true,
		dlog.Context{"shim_pid": session.client.PID(), "watcher_attached": session.watcher != nil, "session_started": session.sessionStarted})
	return nil
}

// holds reports whether the workspace's entry still holds CLIENT.
func (f *Fleet) holds(ws ids.WorkspaceID, client shimclient.Client) bool {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, ok := f.sessions[ws]
	return ok && session.client == client
}

// noteSessionStarted marks the workspace's installed client as one whose shim
// holds a started session. It is the ONE mutation of that fact after the entry
// exists, and it exists because Install rewrites the entry: an adoption that
// remembered the fact first would have it erased by the very install that
// attaches to the started session.
//
// A workspace with no entry is not created here: nothing was installed, so
// there is nothing to say a session started on.
func (f *Fleet) noteSessionStarted(ws ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if session, ok := f.sessions[ws]; ok {
		session.sessionStarted = true
		session.sessionAbsent = false
	}
}

// markSessionAbsent records that the workspace's installed client, when it is
// C, is KNOWN to hold no session. A workspace holding another client is left
// alone: the fact is about one shim.
func (f *Fleet) markSessionAbsent(ws ids.WorkspaceID, c shimclient.Client) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if session, ok := f.sessions[ws]; ok && session.client == c {
		session.sessionAbsent = true
	}
}

// SessionAbsent reports whether the workspace's installed shim is KNOWN to
// hold no session. It is the prompt queue's Deps.SessionAbsent: such a shim
// runs no turn and holds no live work, so it never holds a bounce, while an
// adopted shim whose facts have not arrived (neither started nor absent)
// still does.
func (f *Fleet) SessionAbsent(ws ids.WorkspaceID) bool {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, ok := f.sessions[ws]
	return ok && session.sessionAbsent
}

// sessionStarted reports whether the workspace's installed client's shim holds
// a started session. A workspace with no entry has no shim at all, so it has no
// session either.
func (f *Fleet) sessionStarted(ws ids.WorkspaceID) bool {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, ok := f.sessions[ws]
	return ok && session.sessionStarted
}

// closeDisplaced closes a watcher no handle points at any more. A close is a
// DELIBERATE teardown -- it bumps the fleet's generation before it cancels, so
// every stream end it causes reads as the tear-down it is -- and a failure to
// close is surfaced rather than swallowed: the streams and the sink dispatch it
// owns outlive it.
func (f *Fleet) closeDisplaced(ws ids.WorkspaceID, watcher sessionwatcher.Watcher, reason string) {
	log := f.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	if err := watcher.Close(); err != nil {
		log.Error(opBringUp, "could not close the watcher a new session displaced", dlog.Context{
			"reason": reason, "cause": err.Error(),
		})
		return
	}
	log.Debug(opBringUp, "closed the watcher a new session displaced", dlog.Context{"reason": reason})
}

// logTransition records one workspace fleet state edge with enough context to
// reconstruct the in-memory lifecycle from the trace.
func (f *Fleet) logTransition(ws ids.WorkspaceID, state string, before, after any, extra dlog.Context) {
	fields := dlog.Context{"state": state, "before": before, "after": after}
	for key, value := range extra {
		fields[key] = value
	}
	f.deps.Log.Global().With(dlog.Context{"workspace": string(ws)}).Debug(
		"daemon.workspace.state_transition", "workspace fleet state changed", fields)
}

// errText renders an error for a log context without a nil check at every site.
func errText(err error) string {
	if err == nil {
		return ""
	}
	return err.Error()
}

// permissionMode renders a recorded mode name as the vendor's mode oneof.
//
// AN UNRECOGNIZED, EMPTY, OR `default` NAME YIELDS `auto` (owner ruling
// 2026-09-14: "the default permission mode should be auto for the SDK/shim").
// `auto` is a GATED mode — a classifier decides each ask rather than the user,
// which is why it is absent from UngatedPermissionModes — so this fallback
// still never resolves an unknown name to a mode that drops the gate.
//
// THIS IS ALSO WHERE A STORED `default` IS UPGRADED. A session row written
// before the ruling carries "default"; the next start asks the shim for `auto`
// here, the shim reports `auto` back, and recordFacts rewrites the row to
// "auto" from that report. Nothing rewrites the row without a start, because
// nothing else knows the session came up.
func permissionMode(name string) *conversationv1.AgentPermissionMode {
	switch name {
	case "acceptEdits", "accept_edits":
		return &conversationv1.AgentPermissionMode{Mode: &conversationv1.AgentPermissionMode_AcceptEdits{AcceptEdits: &conversationv1.AgentPermissionModeAcceptEdits{}}}
	case "bypassPermissions", "bypass":
		return &conversationv1.AgentPermissionMode{Mode: &conversationv1.AgentPermissionMode_Bypass{Bypass: &conversationv1.AgentPermissionModeBypass{}}}
	case "plan":
		return &conversationv1.AgentPermissionMode{Mode: &conversationv1.AgentPermissionMode_Plan{Plan: &conversationv1.AgentPermissionModePlan{}}}
	case "dontAsk", "dont_ask":
		return &conversationv1.AgentPermissionMode{Mode: &conversationv1.AgentPermissionMode_DontAsk{DontAsk: &conversationv1.AgentPermissionModeDontAsk{}}}
	default:
		return &conversationv1.AgentPermissionMode{Mode: &conversationv1.AgentPermissionMode_Auto{Auto: &conversationv1.AgentPermissionModeAuto{}}}
	}
}

// permissionModeName is the recorded spelling of a mode the shim REPORTED.
//
// It is permissionMode's inverse on every arm the shim can pick, and the one
// place `default` is still written down: the vendor may report it for a
// session started before the auto ruling, and that fact is recorded as it was
// reported rather than relabeled. permissionMode then upgrades it at the next
// start.
func permissionModeName(mode *conversationv1.AgentPermissionMode) string {
	switch mode.GetMode().(type) {
	case *conversationv1.AgentPermissionMode_AcceptEdits:
		return "acceptEdits"
	case *conversationv1.AgentPermissionMode_Bypass:
		return "bypassPermissions"
	case *conversationv1.AgentPermissionMode_Plan:
		return "plan"
	case *conversationv1.AgentPermissionMode_DontAsk:
		return "dontAsk"
	case *conversationv1.AgentPermissionMode_Auto:
		return "auto"
	default:
		return "default"
	}
}

// shimAdapter narrows a shim client down to the verbs' Shim surface, which is
// what keeps every verb testable against a fake instead of a whole process.
type shimAdapter struct{ client shimclient.Client }

func (a *shimAdapter) KillTurn(ctx context.Context, turn ids.TurnID, force bool, commandedBy *conversationv1.AgentInterruptedByUser) error {
	return killTurn(ctx, a.client, turn, force, commandedBy)
}

func (a *shimAdapter) StopAgent(ctx context.Context, agent *conversationv1.AgentId) error {
	return a.updateAgent(ctx, agent, &conversationv1.AgentInput{
		Input: &conversationv1.AgentInput_Stop{Stop: &conversationv1.AgentStop{}},
	})
}

func (a *shimAdapter) Answer(ctx context.Context, agent *conversationv1.AgentId, answer *conversationv1.AgentAnswer) error {
	return a.updateAgent(ctx, agent, &conversationv1.AgentInput{
		Input: &conversationv1.AgentInput_Answer{Answer: answer},
	})
}

func (a *shimAdapter) updateAgent(ctx context.Context, agent *conversationv1.AgentId, input *conversationv1.AgentInput) error {
	response, err := a.client.UpdateAgent(ctx, &shimv1.UpdateAgentRequest{Target: agent, Input: input})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "UpdateAgent", Arm: updateAgentArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

func (a *shimAdapter) StopBash(ctx context.Context, work *conversationv1.DetachedWorkId) error {
	response, err := a.client.StopBash(ctx, &shimv1.StopBashRequest{Work: work})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "StopBash", Arm: stopBashArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// ReadTranscripts relays the shim's whole answer, arms and all. Unlike every
// other method here it does NOT collapse a refusal into a ShimRefusal: the
// failure's arms carry their own evidence — the path searched and the read's
// account — and a collapse to a bare arm name and sentence would lose it.
func (a *shimAdapter) ReadTranscripts(ctx context.Context) (*shimv1.ReadTranscriptsResponse, error) {
	return a.client.ReadTranscripts(ctx, &shimv1.ReadTranscriptsRequest{})
}

// StandDown arms the client's stand-down latch. See Shim.StandDown.
func (a *shimAdapter) StandDown() bool { return a.client.StandDown() }

func (a *shimAdapter) KillSession(ctx context.Context, force bool) error {
	response, err := a.client.KillSession(ctx, &shimv1.KillSessionRequest{Force: force})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "KillSession", Arm: killSessionArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// The compile-time assertions that the two concrete types answer the seams the
// rest of the daemon is wired against.
var (
	_ Sessions = (*Fleet)(nil)
	_ Verbs    = (*verbs)(nil)
)

// adoptBounded dials an already-running shim under the ONE adoption bound
// (`shimclient.DefaultAdoptBound`), and records the attempt on both sides of
// the call.
//
// THE BOUND IS THE WHOLE POINT. `shimclient.bringUp` is a dial ladder with no
// attempt limit, and an adopted client concludes death only when the socket is
// gone AND the workspace lock reads FREE — so a lock that reads HELD for a
// shim whose socket path is gone is redialed forever. The boot sequence has
// bounded its own adoptions since that shape cost a daemon ten hours of
// serving nothing; the fleet's were still unbounded, and the same shape then
// stranded realtest 7's prompt: the queue held it under `session_starting`,
// the background revival called Adopt, and nothing was ever heard from it
// again, so no turn was ever recorded and the tray's own loud drop — which
// exists for exactly this — could not fire.
//
// THE ATTEMPT IS VISIBLE EITHER WAY. Before the fix, the whole of an adoption
// that never finished was one INFO record saying it had begun; a reader could
// not tell "it never happened" from "it is still dialing a socket that is not
// there". Both edges are recorded here, and an adoption that spent its whole
// bound says so as its own cause.
func (f *Fleet) adoptBounded(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
	dir, udsPath, why string,
) (shimclient.Client, error) {
	fields := dlog.Context{"socket": udsPath, "reason": why, "bound_ms": f.adoptBound.Milliseconds()}
	log.Info(opBringUp, "adopting an already-running shim under the adoption bound", fields)

	bounded, cancel := context.WithTimeout(ctx, f.adoptBound)
	defer cancel()
	started := f.now()
	client, err := f.deps.Supervisor.Adopt(bounded, ws, dir, udsPath)
	elapsed := f.now().Sub(started)
	fields["elapsed_ms"] = elapsed.Milliseconds()
	if err != nil {
		// THE OVERRUN IS NAMED, not folded into the dial error. "the shim did
		// not answer within the bound" and "the shim refused" send a reader to
		// two different places, and only the caller's context can tell them
		// apart.
		if errors.Is(bounded.Err(), context.DeadlineExceeded) && ctx.Err() == nil {
			log.Error(opBringUp, "the adoption spent its whole bound without an answer", fields)
			return nil, fmt.Errorf(
				"adopt %q: the shim at %q did not answer within %s", ws, udsPath, f.adoptBound)
		}
		return nil, err
	}
	// THE PID IS NAMED HERE, and this is the one record that can name it: the
	// probe two callers up sees a live socket and nothing else, so an adoption
	// is the first moment the daemon learns WHICH process it attached to. A
	// reader correlating an inert survivor's adoption against the spawn that
	// left it -- which is how the 2026-09-13T18:18:41 pair was read at all --
	// needs both pids, and the spawn record already carries its own.
	fields["shim_pid"] = client.PID()
	log.Info(opBringUp, "adopted the running shim", fields)
	return client, nil
}
