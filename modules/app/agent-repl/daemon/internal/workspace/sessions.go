package workspace

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
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

// probeWorkspaceLock is the production probe: derive the path, take and release
// the lock. Any error other than "held" is StateUnknown WITH the error, because
// an unreadable lock is never reported as free.
func probeWorkspaceLock(runDir, workspaceDir string) (sessionlock.State, error) {
	path, err := sessionlock.WorkspaceLockPath(runDir, workspaceDir)
	if err != nil {
		return sessionlock.StateUnknown, fmt.Errorf("derive the workspace lock path: %w", err)
	}
	return sessionlock.Probe(path)
}

// WatcherStarter opens one workspace's watch fleet against its shim client.
type WatcherStarter func(ctx context.Context, ws ids.WorkspaceID, client shimclient.Client, sinks sessionwatcher.Sinks, log dlog.Logger) (sessionwatcher.Watcher, error)

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
	// StartWatcher opens one workspace's watch fleet; nil means
	// sessionwatcher.Start. It is a function for the same reason Probe is: the
	// fleet's decisions are exercised without a shim process behind them.
	StartWatcher WatcherStarter
	// Log is the fleet's logger.
	Log dlog.Surfaces
	// Now supplies the instants the fleet stamps; nil means time.Now.
	Now func() time.Time
}

// live is one workspace's live session: the client, its watcher, and the facts
// the verbs read back.
type live struct {
	client  shimclient.Client
	watcher sessionwatcher.Watcher
}

// Fleet brings sessions up and down. It is the SPAWN-ON-MOUNT semantics in one
// place, and it is what Deps.Sessions, Deps.Shim and Deps.Freeness are wired
// from, so the four answers cannot disagree about one workspace.
type Fleet struct {
	deps  FleetDeps
	probe ProbeFunc
	watch WatcherStarter
	now   func() time.Time

	mu        sync.RWMutex
	sessions  map[ids.WorkspaceID]*live
	coldGates map[ids.WorkspaceID]ServedColdGate
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
		probe = probeWorkspaceLock
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
		deps:      deps,
		probe:     probe,
		watch:     watch,
		now:       now,
		sessions:  map[ids.WorkspaceID]*live{},
		coldGates: map[ids.WorkspaceID]ServedColdGate{},
	}, nil
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

// source is the decided way a session comes up: fresh, or a resume of one named
// conversation.
type source struct {
	// Fresh is legal ONLY with proof the workspace never had a conversation.
	Fresh bool
	// VendorSessionID is the conversation a resume names.
	VendorSessionID string
}

// decideSource makes the FRESH-CONVERSATION decision. StartSession(fresh) is
// legal ONLY with proof the workspace never had a conversation at all —
// abandonment is irreversible, every alternative is recoverable — so anything
// else resumes, and a DELETED session refuses outright rather than resurrecting.
func decideSource(session wsm.Session, exists bool) (source, error) {
	if !exists {
		return source{Fresh: true}, nil
	}
	if session.Terminal != nil && session.Terminal.Kind == "deleted" {
		return source{}, fmt.Errorf("the session was deleted: %s", session.Terminal.Detail)
	}
	if session.VendorSessionID == "" {
		return source{Fresh: true}, nil
	}
	return source{VendorSessionID: session.VendorSessionID}, nil
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

	src, err := decideSource(session, exists)
	if err != nil {
		return refuse(log, "OpenWorkspace", ArmSessionDeleted, err.Error(), false)
	}
	log.Debug(opBringUp, "decided how the session comes up", dlog.Context{
		"fresh": src.Fresh, "vendor_session_id": src.VendorSessionID,
	})

	if !src.Fresh {
		if _, err := f.deps.Accounts.FindTranscript(ctx, record.Dir, src.VendorSessionID); err != nil {
			return refuse(log, "OpenWorkspace", ArmTranscriptMissing,
				fmt.Sprintf("the transcript for conversation %q is missing: %v", src.VendorSessionID, err), false)
		}
		log.Debug(opBringUp, "the resume guard found the transcript", dlog.Context{
			"vendor_session_id": src.VendorSessionID,
		})
	}

	configDir := session.ConfigDir
	if configDir == "" {
		configDir = f.deps.Accounts.ConfigDirFor(record.Dir)
	}
	udsPath := f.deps.SocketPath(ws)

	client, adopted, err := f.bringUpClient(ctx, log, ws, record.Dir, udsPath, configDir)
	if err != nil {
		return err
	}

	started, err := f.startSession(ctx, log, ws, client, src, session)
	if err != nil {
		return err
	}
	if started == nil {
		// The session is parked behind a standing cold gate. The client stays
		// up: the gate's answer re-opens through it.
		f.remember(ws, &live{client: client})
		return nil
	}

	watcher, err := f.watch(ctx, ws, client, f.deps.Sinks, log)
	if err != nil {
		log.Error(opBringUp, "could not start the session watcher", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("start session for %q: start the watcher: %w", ws, err)
	}
	f.remember(ws, &live{client: client, watcher: watcher})

	if err := f.recordFacts(ctx, log, ws, session, started, configDir, client.PID()); err != nil {
		return err
	}
	log.Info(opBringUp, "the session is up", dlog.Context{
		"adopted": adopted, "vendor_session_id": started.GetVendorSessionId(), "shim_pid": client.PID(),
	})
	return nil
}

// bringUpClient probes the workspace lock and either ADOPTS the surviving shim
// that holds it or SPAWNS a new one. A probe that could not tell is never read
// as free: spawning a second shim onto one conversation is the failure the lock
// exists to prevent.
func (f *Fleet) bringUpClient(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, dir, udsPath, configDir string) (shimclient.Client, bool, error) {
	lockPath := f.lockDir()
	state, err := f.probe(lockPath, dir)
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
		log.Debug(opBringUp, "the workspace lock is free; spawning a shim", dlog.Context{"lock": lockPath})
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
			ShimBuildSHA: f.deps.ShimBuildSHA,
			NodeBin:      f.deps.NodeBin,
			MainJS:       f.deps.MainJS,
			Fake:         f.deps.Fake,
			LogSink:      os.NewFile(sink.File(), dir+"/.claude/emacs/shim.log"),
			ForbidVendor: f.deps.ForbidVendor,
		})
		if err != nil {
			log.Error(opBringUp, "the shim did not come up", dlog.Context{"cause": err.Error()})
			return nil, false, fmt.Errorf("start session for %q: spawn: %w", ws, err)
		}
		return client, false, nil
	default:
		log.Error(opBringUp, "the workspace lock probe could not tell", dlog.Context{
			"lock": lockPath, "cause": errText(err),
		})
		return nil, false, fmt.Errorf("start session for %q: the workspace lock at %q could not be probed: %w", ws, lockPath, err)
	}
}

// startSession runs StartSession and answers a COLD refusal with the gate. A
// nil SessionStarted with a nil error means the session is parked behind a
// standing gate, which is an answer and not a failure.
func (f *Fleet) startSession(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, src source, session wsm.Session) (*conversationv1.SessionStarted, error) {
	req := &shimv1.StartSessionRequest{}
	if src.Fresh {
		req.Source = &shimv1.StartSessionRequest_Fresh{Fresh: &shimv1.StartSessionFresh{
			Model:          &conversationv1.AgentModel{Name: session.Model},
			PermissionMode: permissionMode(session.PermissionMode),
		}}
	} else {
		req.Source = &shimv1.StartSessionRequest_Resume{Resume: &shimv1.StartSessionResume{
			VendorSessionId: src.VendorSessionID,
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
		log.Error(opBringUp, "StartSession refused", dlog.Context{"detail": failure.GetDetail()})
		return nil, fmt.Errorf("start session for %q: %s", ws, failure.GetDetail())
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
func (f *Fleet) recordFacts(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, previous wsm.Session, started *conversationv1.SessionStarted, configDir string, pid int) error {
	now := f.now()
	next := wsm.Session{
		Workspace:        ws,
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
	if err := f.deps.DB.PutSession(ctx, next); err != nil {
		log.Error(opBringUp, "could not record the session facts", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("start session for %q: record the session facts: %w", ws, err)
	}
	log.Debug(opBringUp, "recorded the session facts", dlog.Context{
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
	f.mu.Unlock()
	if !ok {
		return nil
	}
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
	return &shimAdapter{client: session.client}, true
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
		return fmt.Errorf("kill turn %q: %s", turn, failure.GetDetail())
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
		return fmt.Errorf("update agent %q: %s", agent.GetValue(), failure.GetDetail())
	}
	return nil
}

func (a *shimAdapter) StopBash(ctx context.Context, work *conversationv1.DetachedWorkId) error {
	response, err := a.client.StopBash(ctx, &shimv1.StopBashRequest{Work: work})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return fmt.Errorf("stop bash %q: %s", work.GetValue(), failure.GetDetail())
	}
	return nil
}

func (a *shimAdapter) KillSession(ctx context.Context, force bool) error {
	response, err := a.client.KillSession(ctx, &shimv1.KillSessionRequest{Force: force})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return fmt.Errorf("kill session: %s", failure.GetDetail())
	}
	return nil
}

func (a *shimAdapter) StartSession(ctx context.Context, resume ColdResume) error {
	response, err := a.client.StartSession(ctx, &shimv1.StartSessionRequest{
		Source: &shimv1.StartSessionRequest_Resume{Resume: &shimv1.StartSessionResume{
			VendorSessionId: resume.VendorSessionID,
			ColdRemediation: resume.Remediation,
		}},
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return fmt.Errorf("re-open the session: %s", failure.GetDetail())
	}
	return nil
}

// The compile-time assertions that the two concrete types answer the seams the
// rest of the daemon is wired against.
var (
	_ Sessions = (*Fleet)(nil)
	_ Verbs    = (*verbs)(nil)
)
