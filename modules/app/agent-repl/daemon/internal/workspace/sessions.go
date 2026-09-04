package workspace

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"sync"
	"time"

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
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/wsm"
)

// DefaultLockDir is where the shim-held kernel locks live. The daemon only ever
// PROBES them.
const DefaultLockDir = "~/.cache/agent-repl/run"

// LockDirEnv overrides DefaultLockDir for tests; the fake shim honors it too, so
// a test's probe and a test's shim agree about which lock is which.
const LockDirEnv = "AGENT_REPL_LOCK_DIR"

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

// FleetDeps are what the session fleet needs to bring a session up.
type FleetDeps struct {
	// DB holds the session record the fresh-versus-resume decision is made
	// from.
	DB wsm.DB
	// Accounts routes a workspace to its config root and locates a
	// conversation's transcript, which is what the resume guard checks.
	Accounts account.Resolver
	// Supervisor spawns and adopts shim processes.
	Supervisor shimclient.Supervisor
	// Sinks are the five resolvers plus the lifecycle sink the watcher routes
	// into.
	Sinks sessionwatcher.Sinks
	// Feed carries the cold gate's row.
	Feed feed.Resolver
	// Footer carries the parked-session status a standing cold gate produces.
	Footer footer.Resolver
	// SocketPath answers a workspace's shim socket path under the state root.
	SocketPath func(ws ids.WorkspaceID) string
	// StoreSocket is passed to every shim explicitly.
	StoreSocket string
	// NodeBin and MainJS are the shim's command line.
	NodeBin, MainJS string
	// ShimBuildSHA is the build every spawn is stamped with.
	ShimBuildSHA string
	// Fake forces the shim's offline scripted SDK.
	Fake bool
	// ForbidVendor sets AGENT_REPL_FORBID_VENDOR_CALLS on every spawn.
	ForbidVendor bool
	// LockDir overrides DefaultLockDir; empty reads LockDirEnv, then the
	// default.
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
	// PublishHost recomposes and republishes one workspace's HOST view. The
	// fleet owns the edges that move it and the server cannot see them: a
	// session coming up, a session going away, a shim replaced. Nil means no
	// host surface is wired yet (the boot sequence runs before the server),
	// which is why every call goes through publishHost.
	PublishHost func(ids.WorkspaceID)
}

// live is one workspace's live session: the client, its watcher, and the facts
// the verbs read back.
type live struct {
	client  shimclient.Client
	watcher sessionwatcher.Watcher
	// hostSessionID is the session's host-facing identity, remembered here so
	// the host view's live half is answered from what this daemon IS
	// operating rather than from a durable row that may outlive the session.
	hostSessionID string
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

	mu        sync.RWMutex
	sessions  map[ids.WorkspaceID]*live
	coldGates map[ids.WorkspaceID]ServedColdGate
	// lastCold is the shim's own cold facts for a parked workspace, kept whole
	// so the relaunch engine's cold arm carries what the shim stated rather
	// than a reconstruction of it.
	lastCold map[ids.WorkspaceID]*conversationv1.SessionCold
	// generation counts the shims this daemon has spawned per workspace, which
	// is what makes a prelaunched shim's socket path distinct from the running
	// one's.
	generation map[ids.WorkspaceID]int
	// buildSHA is the shim build each live session reported at start. It is a
	// fact of the RUNNING process, which is why it is remembered here and not
	// persisted.
	buildSHA map[ids.WorkspaceID]string
}

// NewFleet builds the session fleet.
func NewFleet(deps FleetDeps) (*Fleet, error) {
	switch {
	case deps.DB == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a state client")
	case deps.Accounts == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs an account resolver")
	case deps.Supervisor == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a shim supervisor")
	case deps.SocketPath == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs a socket path resolver")
	case deps.Log == nil:
		return nil, fmt.Errorf("workspace: the session fleet needs log surfaces")
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
	return &Fleet{
		deps:        deps,
		probe:       probe,
		socketProbe: socketProbe,
		watch:       watch,
		now:         now,
		sessions:    map[ids.WorkspaceID]*live{},
		coldGates:   map[ids.WorkspaceID]ServedColdGate{},
		lastCold:    map[ids.WorkspaceID]*conversationv1.SessionCold{},
		generation:  map[ids.WorkspaceID]int{},
		buildSHA:    map[ids.WorkspaceID]string{},
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

// lockDir answers where the kernel locks live: the explicit setting, then the
// environment override the fake shim honors, then the default.
func (f *Fleet) lockDir() string {
	if f.deps.LockDir != "" {
		return f.deps.LockDir
	}
	if fromEnv := os.Getenv(LockDirEnv); fromEnv != "" {
		return fromEnv
	}
	if home, err := os.UserHomeDir(); err == nil {
		return filepath.Join(home, ".cache", "agent-repl", "run")
	}
	return DefaultLockDir
}

// Live reports whether the workspace currently has a live session.
func (f *Fleet) Live(ws ids.WorkspaceID) bool {
	f.mu.RLock()
	defer f.mu.RUnlock()
	_, ok := f.sessions[ws]
	return ok
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

// ColdGate answers the menu a standing cold gate served, which is what the
// cold-gate answer is echoed against.
func (f *Fleet) ColdGate(ws ids.WorkspaceID) (ServedColdGate, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	gate, ok := f.coldGates[ws]
	return gate, ok
}

// ClearColdGate retires an answered gate. It is the resolve path's own step:
// Stop clears the gate along with the session, but a gate that was ANSWERED
// leaves no session behind to clear it.
func (f *Fleet) ClearColdGate(ws ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	delete(f.coldGates, ws)
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
//   - a vendor id whose transcript is MISSING: FRESH, with a fault opened once
//     naming the abandoned conversation. A pre-minted id that never took a
//     turn writes no transcript, so this is the ORDINARY state of a session
//     bounced before its first turn — and resuming it names a conversation the
//     shim rightly refuses as `unknown_session`, which left the workspace with
//     no client at all. The fault is loud because a genuinely VANISHED
//     transcript reaches the same branch; what it must not do is cost the
//     workspace its live session.
func (f *Fleet) classifySource(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, dir string, session wsm.Session, exists bool) (source, error) {
	if !exists {
		return source{Fresh: true}, nil
	}
	if session.Terminal != nil && session.Terminal.Kind == "deleted" {
		return source{}, fmt.Errorf("the session was deleted: %s", session.Terminal.Detail)
	}
	if session.VendorSessionID == "" {
		return source{Fresh: true}, nil
	}
	if _, err := f.deps.Accounts.FindTranscript(ctx, dir, session.VendorSessionID); err != nil {
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
	if f.Live(ws) {
		return nil
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

	src, err := f.classifySource(ctx, log, ws, record.Dir, session, exists)
	if err != nil {
		return refuse(log, "OpenWorkspace", ArmSessionDeleted, err.Error(), false)
	}
	log.Debug(opBringUp, "decided how the session comes up", dlog.Context{
		"fresh": src.Fresh, "vendor_session_id": src.VendorSessionID,
	})

	// THE ACCOUNT ROUTING IS DECIDED AT EVERY START (daemon.md 10a), never
	// inherited from the record: $MULTI_REPO_ROOT can move between boots, and
	// a session resumed under the root it was FILED in rather than the one it
	// now ROUTES to would run the whole conversation against the wrong
	// account. When the two disagree, the vendor transcript is carried into
	// the newly routed root BEFORE the resume is sent — a resume against a
	// root that does not hold the transcript is a resume of nothing.
	configDir := f.deps.Accounts.ConfigDirFor(record.Dir)
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

	client, adopted, err := f.bringUpClient(ctx, log, ws, record.Dir, udsPath, configDir, hostSessionID, src)
	if err != nil {
		return err
	}
	// THE HEALTHY ATTACH CLOSES THE LOST-LINK FAULTS. Bring-up gates on the
	// shim's first healthy diagnostics, so reaching here IS the repair of
	// whatever shim_died or link_severed the previous attachment recorded. A
	// mid-stream redial is NOT this moment: the link coming back on a stream
	// the daemon never re-attached leaves the evidence standing.
	f.closeLinkFaults(ctx, log, ws)

	// AN ADOPTED SHIM IS ATTACHED TO, NEVER STARTED. The lock probe selected
	// the adopt path precisely because a shim is still alive on this
	// conversation, and a live shim has ALREADY started its one session:
	// shim.v1 answers a second StartSession with `already_started`, so sending
	// one turns a perfectly good mount into a failed rpc. The session facts
	// come from the shim's own re-announcement of SessionStarted on the watch
	// this install opens (landing 7) — the same attach-only path the
	// handover's successor and the boot adoption take.
	if adopted {
		f.remember(ws, &live{client: client, hostSessionID: hostSessionID})
		if err := f.Install(ctx, ws, client); err != nil {
			log.Error(opBringUp, "the adopted shim could not be installed", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("start session for %q: install the adopted shim: %w", ws, err)
		}
		log.Info(opBringUp, "attached to a surviving shim without starting a session", dlog.Context{
			"adopted": true, "shim_pid": client.PID(),
		})
		return nil
	}

	started, err := f.startSession(ctx, log, ws, client, src, session)
	if err != nil {
		return err
	}
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
		f.remember(ws, &live{client: client, hostSessionID: hostSessionID})
		f.publishHost(ws)
		return nil
	}

	return f.sessionUp(ctx, log, ws, client, started, session, configDir, hostSessionID)
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
	f.remember(ws, &live{client: client, hostSessionID: hostSessionID})
	watcher, err := f.watch(context.WithoutCancel(ctx), ws, client, sessionwatcher.Session{Started: started}, f.deps.Sinks, log)
	if err != nil {
		log.Error(opBringUp, "could not start the session watcher", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("start session for %q: start the watcher: %w", ws, err)
	}
	f.remember(ws, &live{client: client, watcher: watcher, hostSessionID: hostSessionID})

	if err := f.recordFacts(ctx, log, ws, previous, started, configDir, hostSessionID, client.PID()); err != nil {
		return err
	}
	log.Info(opBringUp, "the session is up", dlog.Context{
		"adopted": false, "vendor_session_id": started.GetVendorSessionId(), "shim_pid": client.PID(),
	})
	// A SESSION NOW EXISTS where none did: the host view's whole session arm
	// changed, and nothing the server can see says so.
	f.publishHost(ws)
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

	started, err := f.startSession(ctx, log, ws, session.client, source{
		VendorSessionID: resume.VendorSessionID,
		ColdRemediation: resume.Remediation,
	}, previous)
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
	configDir := f.deps.Accounts.ConfigDirFor(record.Dir)
	return f.sessionUp(ctx, log, ws, session.client, started, previous, configDir, hostSessionID)
}

// bringUpClient probes the workspace lock and either ADOPTS the surviving shim
// that holds it or SPAWNS a new one. A probe that could not tell is never read
// as free: spawning a second shim onto one conversation is the failure the lock
// exists to prevent.
func (f *Fleet) bringUpClient(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, dir, udsPath, configDir, hostSessionID string, src source) (shimclient.Client, bool, error) {
	lockPath := f.lockDir()
	state, err := f.probe(lockPath, dir)
	// THE SOCKET IS THE SECOND KERNEL FACT. The lock says whether this
	// conversation is OWNED; only the socket says whether its owner is
	// REACHABLE, and a bring-up that spawned on the lock alone put a second
	// shim onto a path the survivor still held — the newcomer could not bind
	// and died, this daemon dialed the path and reached the SURVIVOR, and the
	// answer was StartSession{already_started} over a turn already running.
	socket, socketErr := f.socketProbe(udsPath)
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
	inert := false
	if state == sessionlock.StateFree && socket == shimsocket.StateLive {
		log.Warn(opBringUp, "the workspace lock reads free but a shim is listening; attaching to the inert survivor and starting its session",
			dlog.Context{"lock": lockPath, "socket": udsPath, "lock_state": state.String()})
		inert = true
	}
	// A SOCKET THAT COULD NOT BE PROBED IS NEVER SPAWNED ONTO, for the same
	// reason an unreadable lock is never read as free.
	if state == sessionlock.StateFree && socket == shimsocket.StateUndetermined {
		log.Error(opBringUp, "the shim socket probe could not tell", dlog.Context{
			"socket": udsPath, "cause": errText(socketErr),
		})
		return nil, false, fmt.Errorf("start session for %q: the shim socket at %q could not be probed: %w", ws, udsPath, socketErr)
	}
	if inert {
		client, err := f.deps.Supervisor.Adopt(ctx, ws, dir, udsPath)
		if err != nil {
			log.Error(opBringUp, "could not attach to the inert survivor", dlog.Context{"cause": err.Error()})
			return nil, false, fmt.Errorf("start session for %q: adopt: %w", ws, err)
		}
		return client, false, nil
	}
	switch state {
	case sessionlock.StateHeld:
		log.Debug(opBringUp, "a surviving shim holds the workspace lock; adopting it", dlog.Context{"lock": lockPath})
		client, err := f.deps.Supervisor.Adopt(ctx, ws, dir, udsPath)
		if err != nil {
			log.Error(opBringUp, "could not adopt the surviving shim", dlog.Context{"cause": err.Error()})
			return nil, false, fmt.Errorf("start session for %q: adopt: %w", ws, err)
		}
		return client, true, nil
	case sessionlock.StateFree:
		// THE CLASSIFIER ALREADY SETTLED FRESH-VERSUS-RESUME, transcript and
		// all, before this probe ran: a resume reaching here names a
		// transcript that was found, and a recorded conversation with none
		// comes up FRESH with its own fault rather than being refused. There
		// is nothing left for a spawn-side guard to test.
		log.Debug(opBringUp, "the workspace lock is free; spawning a shim", dlog.Context{"lock": lockPath})
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
			return nil, false, refuse(log, "OpenWorkspace", ArmSpawnFailed, clearErr.Error(), false)
		}
		sink, err := f.deps.Log.ShimSink(dir)
		if err != nil {
			log.Error(opBringUp, "could not borrow the shim log sink", dlog.Context{"cause": err.Error()})
			return nil, false, fmt.Errorf("start session for %q: shim log sink: %w", ws, err)
		}
		client, err := f.deps.Supervisor.Spawn(ctx, shimclient.Spec{
			WorkspaceID:  ws,
			WorkspaceDir: dir,
			UDSPath:      udsPath,
			StoreSocket:  f.deps.StoreSocket,
			ConfigDir:    configDir,
			SessionID:    hostSessionID,
			ShimBuildSHA: f.deps.ShimBuildSHA,
			NodeBin:      f.deps.NodeBin,
			MainJS:       f.deps.MainJS,
			Fake:         f.deps.Fake,
			LogSink:      sink.File(),
			ForbidVendor: f.deps.ForbidVendor,
		})
		if err != nil {
			log.Error(opBringUp, "the shim did not come up", dlog.Context{"cause": err.Error()})
			// A BRING-UP DEATH IS A WORKSPACE FAULT, not only a failed rpc.
			// The rpc answers whoever asked; the fault and the dead link are
			// what every OTHER surface reads, and without them a workspace
			// whose shim will not start looks merely idle.
			f.noteStartFailed(ctx, log, ws, err)
			return nil, false, refuse(log, "OpenWorkspace", ArmSpawnFailed, err.Error(), false)
		}
		return client, false, nil
	default:
		log.Error(opBringUp, "the workspace lock probe could not tell", dlog.Context{
			"lock": lockPath, "cause": errText(err),
		})
		return nil, false, fmt.Errorf("start session for %q: the workspace lock at %q could not be probed: %w", ws, lockPath, err)
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
		fault.Evidence["exit_code"] = strconv.Itoa(death.Exit.Code)
		fault.Evidence["stderr_tail"] = death.Exit.Stderr
	}
	if _, err := f.deps.DB.OpenFault(ctx, fault); err != nil {
		log.Error(opBringUp, "could not record the failed bring-up", dlog.Context{"cause": err.Error()})
	}
	f.deps.Sinks.Footer.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Topbar.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Sidebar.OnLink(ws, shimclient.LinkDead)
	f.publishHost(ws)
}

// linkFaultKinds are the fault kinds a lost daemon-to-shim link records, and
// the ones a healthy attach retracts.
var linkFaultKinds = []string{health.KindShimDied, health.KindLinkSevered}

// closeLinkFaults retracts the lost-link faults of one workspace.
func (f *Fleet) closeLinkFaults(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	workspace := ws
	for _, kind := range linkFaultKinds {
		open, err := f.deps.DB.OpenFaults(ctx, wsm.FaultScope{Workspace: &workspace, Kind: kind})
		if err != nil {
			log.Error(opBringUp, "could not read the standing link faults", dlog.Context{
				"kind": kind, "cause": err.Error(),
			})
			continue
		}
		for _, fault := range open {
			if err := f.deps.DB.CloseFault(ctx, fault.ID, f.now()); err != nil {
				log.Error(opBringUp, "could not close a standing link fault", dlog.Context{
					"kind": kind, "fault": string(fault.ID), "cause": err.Error(),
				})
				continue
			}
			log.Info(opBringUp, "a healthy attach retracted a lost-link fault", dlog.Context{
				"kind": kind, "fault": string(fault.ID),
			})
		}
	}
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

// freshModel answers the model a fresh session names, or nil when the user
// chose none.
//
// LANDING 7: StartSessionFresh.model is OPTIONAL and an unset one means the
// SDK's own default, so the daemon no longer substitutes a model of its own —
// it states the user's choice or says nothing. SessionStarted.effective_model
// is what took effect, and recordFacts persists that.
func freshModel(recorded string) *conversationv1.AgentModel {
	if recorded == "" {
		return nil
	}
	return &conversationv1.AgentModel{Name: recorded}
}

// startSession runs StartSession and answers a COLD refusal with the gate. A
// nil SessionStarted with a nil error means the session is parked behind a
// standing gate, which is an answer and not a failure.
func (f *Fleet) startSession(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, src source, session wsm.Session) (*conversationv1.SessionStarted, error) {
	req := &shimv1.StartSessionRequest{}
	if src.Fresh {
		req.Source = &shimv1.StartSessionRequest_Fresh{Fresh: &shimv1.StartSessionFresh{
			Model:          freshModel(session.Model),
			PermissionMode: permissionMode(session.PermissionMode),
		}}
	} else {
		req.Source = &shimv1.StartSessionRequest_Resume{Resume: &shimv1.StartSessionResume{
			VendorSessionId: src.VendorSessionID,
			ColdRemediation: src.ColdRemediation,
		}}
	}

	response, err := client.StartSession(ctx, req)
	if err != nil {
		log.Error(opBringUp, "the StartSession call failed", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("start session for %q: %w", ws, err)
	}
	if failure := response.GetFailure(); failure != nil {
		if cold := failure.GetCold(); cold != nil {
			f.raiseColdGate(ws, src.VendorSessionID, cold)
			log.Warn(opBringUp, "the session is parked behind a cold gate", dlog.Context{
				"context_tokens":  cold.GetContextTokens(),
				"requested_model": cold.GetRequestedModel().GetName(),
			})
			return nil, nil
		}
		if failure.GetConversationOwned() != nil {
			// ANOTHER SHIM HOLDS THIS CONVERSATION. It took the workspace
			// kernel lock first, which is exactly what that lock is for: two
			// vendor processes on one conversation is the state it prevents.
			// The refusal is the shim's own verdict, relayed.
			return nil, refuse(log, "OpenWorkspace", ArmConversationOwned,
				fmt.Sprintf("another shim holds workspace %q's conversation: %s", ws, failure.GetDetail()), false)
		}
		if failure.GetUnknownSession() != nil {
			// THE RESUME NAMED A CONVERSATION THE SHIM HAS NO TRANSCRIPT FOR.
			// The classifier is what keeps a never-turned session off this
			// path; reaching it anyway is a real vanished transcript, and it
			// gets its NAMED arm rather than a generic sentence, because
			// "no transcript exists" is remediated differently from every
			// other StartSession refusal.
			return nil, refuse(log, "OpenWorkspace", ArmUnknownSession,
				fmt.Sprintf("the shim has no transcript for conversation %q: %s", src.VendorSessionID, failure.GetDetail()), false)
		}
		if failure.GetVendorStartFailed() != nil {
			// THE VENDOR FAILED TO START INSIDE A HEALTHY SHIM. The shim
			// process is up and serving — only its StartSession answer is a
			// refusal — so neither `spawn_failed` nor `shim_start_failed`,
			// which both name the SHIM PROCESS, describes it. LANDING 9 gave
			// it its own arm: OpenWorkspaceError.vendor_start_failed carries
			// the shim's OWN account in `detail`, so the verdict is relayed
			// typed rather than through the unlanded-arm convention.
			return nil, refuseWith(log, "OpenWorkspace", ArmVendorStartFailed,
				fmt.Sprintf("the vendor failed to start for workspace %q: %s", ws, failure.GetDetail()), false,
				map[string]any{"detail": failure.GetDetail()})
		}
		if failure.GetAlreadyStarted() != nil {
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
func (f *Fleet) raiseColdGate(ws ids.WorkspaceID, vendorSessionID string, cold *conversationv1.SessionCold) {
	models := []*conversationv1.AgentModel{cold.GetRequestedModel()}
	scopes := []conversationv1.SessionCompactScope{
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_PROMPTS,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_RESPONSES,
	}
	options := make([]*frontendv1.FeedColdGateModelOption, 0, len(models))
	for _, m := range models {
		options = append(options, &frontendv1.FeedColdGateModelOption{Model: m})
	}

	f.mu.Lock()
	f.coldGates[ws] = ServedColdGate{VendorSessionID: vendorSessionID, Models: models, Scopes: scopes}
	f.lastCold[ws] = cold
	f.mu.Unlock()

	ref := feedid.Ref{
		WS:   ws,
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindColdGate, ID: vendorSessionID},
	}
	f.deps.Feed.UpsertSynthesized(ws, feedid.Feed{Root: true}, &frontendv1.FeedRow{
		Id: feedid.Encode(ref),
		Row: &frontendv1.FeedRow_ColdGate{ColdGate: &frontendv1.FeedColdGate{
			State: &frontendv1.FeedColdGate_Standing{Standing: &frontendv1.FeedColdGateStanding{
				ContextTokens: &frontendv1.FeedColdGateContextTokens{Tokens: int64(cold.GetContextTokens())},
				LastRequest:   &frontendv1.FeedColdGateLastRequest{AtMs: cold.GetLastRequestAtMs()},
				Model:         &frontendv1.FeedColdGateModel{Model: cold.GetRequestedModel()},
				Compact:       &frontendv1.FeedColdGateCompactMenu{Models: options, Scopes: scopes},
			}},
		}},
	})
	f.deps.Footer.SetColdGate(ws, footer.ColdGate{
		Standing: true,
		Detail:   fmt.Sprintf("the conversation is cold at %d context tokens", cold.GetContextTokens()),
	})
}

// recordFacts persists the session facts that outlive one shim process: the
// vendor identity, the config dir it was spawned under, and the model and mode
// in force. The shim pid and the shim's build are LOGGED rather than persisted,
// because both belong to the process rather than to the session.
func (f *Fleet) recordFacts(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, previous wsm.Session, started *conversationv1.SessionStarted, configDir, hostSessionID string, pid int) error {
	now := f.now()
	next := wsm.Session{
		Workspace:        ws,
		HostSessionID:    hostSessionID,
		VendorSessionID:  started.GetVendorSessionId(),
		ConfigDir:        configDir,
		Model:            started.GetEffectiveModel().GetName(),
		PermissionMode:   permissionModeName(started.GetPermissionMode()),
		StartedAt:        previous.StartedAt,
		LastEngagementAt: now,
	}
	if next.StartedAt.IsZero() {
		next.StartedAt = now
	}
	if sha := started.GetRuntime().GetShimBuildSha(); sha != "" {
		f.mu.Lock()
		f.buildSHA[ws] = sha
		f.mu.Unlock()
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

// Stop ends a workspace's session. Forced stops kill the process; a graceful
// stop leaves the shim to end its own session first. Stopping a workspace with
// no live session is SUCCESS: the caller asked for a state that already holds.
func (f *Fleet) Stop(ctx context.Context, ws ids.WorkspaceID, force bool) error {
	f.mu.Lock()
	session, ok := f.sessions[ws]
	delete(f.sessions, ws)
	delete(f.coldGates, ws)
	delete(f.lastCold, ws)
	delete(f.buildSHA, ws)
	f.mu.Unlock()
	if !ok {
		return nil
	}
	// THE SESSION IS GONE from this daemon's point of view the moment it
	// leaves the map: the host view's session arm changes here, whatever the
	// teardown below then does.
	defer f.publishHost(ws)
	if session.watcher != nil {
		if err := session.watcher.Close(); err != nil {
			return fmt.Errorf("stop session for %q: close the watcher: %w", ws, err)
		}
	}
	if err := session.client.Kill(shimclient.KillAttribution{
		Actor:  "workspace.stop",
		Reason: "the workspace's session was stopped",
		Force:  force,
	}); err != nil {
		return fmt.Errorf("stop session for %q: kill the shim: %w", ws, err)
	}
	// THE VIEWS ARE TOLD HERE, not left to the connectivity feed. The watcher
	// was closed above, so the client's own LinkDead publish has nobody left
	// to route it: whether the views ever saw the death would otherwise depend
	// on the exit landing before the close, which is a race the stop itself
	// can settle. Kill has already passed the reap gate, so the process is
	// gone by the time this runs.
	f.deps.Sinks.Footer.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Topbar.OnLink(ws, shimclient.LinkDead)
	f.deps.Sinks.Sidebar.OnLink(ws, shimclient.LinkDead)
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
		return log
	}
	return log.With(dlog.Context{dlog.KeyAgentReplSessionID: hostSessionID})
}

// remember records a workspace's live session.
func (f *Fleet) remember(ws ids.WorkspaceID, session *live) {
	f.mu.Lock()
	f.sessions[ws] = session
	f.mu.Unlock()
}

// errText renders an error for a log context without a nil check at every site.
func errText(err error) string {
	if err == nil {
		return ""
	}
	return err.Error()
}

// permissionMode renders a recorded mode name as the vendor's mode oneof. An
// unrecognized or empty name yields the DEFAULT mode, which is the gated one:
// an unknown name never resolves to a mode that disables the gate.
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
	case "auto":
		return &conversationv1.AgentPermissionMode{Mode: &conversationv1.AgentPermissionMode_Auto{Auto: &conversationv1.AgentPermissionModeAuto{}}}
	default:
		return &conversationv1.AgentPermissionMode{Mode: &conversationv1.AgentPermissionMode_Default{Default: &conversationv1.AgentPermissionModeDefault{}}}
	}
}

// permissionModeName is permissionMode's inverse: the recorded spelling of a
// mode the shim reported.
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

func (a *shimAdapter) KillTurn(ctx context.Context, turn ids.TurnID, force bool) error {
	response, err := a.client.KillTurn(ctx, &shimv1.KillTurnRequest{
		Turn:  &conversationv1.TurnId{Value: string(turn)},
		Force: force,
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "KillTurn", Arm: killTurnArm(failure), Detail: failure.GetDetail()}
	}
	return nil
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
