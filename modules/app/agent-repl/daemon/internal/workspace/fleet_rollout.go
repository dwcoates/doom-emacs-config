package workspace

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// The fleet's ROLLOUT and LEASE-HOLDER surfaces.
//
// The relaunch engine, the drain controller and the merge orchestrator all
// need the same four things about one workspace's session — which shim serves
// it, whether it is free, who occupies it, and how to stand a fresh one up —
// and the fleet is the one place that knows. Answering them anywhere else
// would be a second opinion about a single workspace.

// opFleetRollout is this surface's operation name.
const opFleetRollout = "daemon.workspace.fleet_rollout"

// Client answers the workspace's current shim client, false when none is up.
// It is rollout.ShimFleet's first method.
func (f *Fleet) Client(ws ids.WorkspaceID) (shimclient.Client, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, ok := f.sessions[ws]
	if !ok {
		return nil, false
	}
	return session.client, true
}

// Free answers freeness right now: no turn in flight and no live detached
// work. A workspace with NO live session is free — there is nothing in flight
// to wait on — which is what lets a lease holder proceed on a hibernated or
// never-started workspace instead of blocking forever.
func (f *Fleet) Free(ws ids.WorkspaceID) bool {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return true
	}
	return session.watcher.Free()
}

// AwaitFree blocks until the workspace is free, or until ctx ends. It waits on
// the watcher's stream edges; there is no poll and no timer anywhere beneath
// it.
func (f *Fleet) AwaitFree(ctx context.Context, ws ids.WorkspaceID) error {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return nil
	}
	return session.watcher.AwaitFree(ctx)
}

// AwaitTurnEnd blocks until one submitted turn ends and reports how. A
// workspace with no live session has no turn to wait on, which is a refusal
// rather than an immediate success: the caller asked about a turn it believes
// is running.
func (f *Fleet) AwaitTurnEnd(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (wsm.TurnClose, error) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return 0, fmt.Errorf("workspace: %q has no live session to await turn %q on", ws, turn)
	}
	return session.watcher.AwaitTurnEnd(ctx, turn)
}

// Occupy takes the shim client's in-memory occupancy guard, which is what the
// WSM lease row describes. It reports false when the workspace has no live
// session: there is no process to occupy, and a merge on a session-less
// workspace is legal.
func (f *Fleet) Occupy(ws ids.WorkspaceID, holder string) (func(), bool, error) {
	client, ok := f.Client(ws)
	if !ok {
		return nil, false, nil
	}
	release, err := client.Occupy(holder)
	if err != nil {
		return nil, false, fmt.Errorf("workspace: occupy %q for %q: %w", ws, holder, err)
	}
	return release, true, nil
}

// CaptureDisplaced durably marks the turn a lease holder displaced, so it is
// resubmitted EXACTLY ONCE at lease release even across a daemon bounce. It
// reports false when nothing was in flight, which is the ordinary case.
func (f *Fleet) CaptureDisplaced(ctx context.Context, ws ids.WorkspaceID) (ids.TurnID, bool, error) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return "", false, nil
	}
	inFlight := session.watcher.TurnInFlight()
	if inFlight == nil {
		return "", false, nil
	}
	open, err := f.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		return "", false, fmt.Errorf("workspace: capture the displaced turn %q: %w", *inFlight, err)
	}
	record, found := wsm.Turn{}, false
	for _, t := range open {
		if t.ID == *inFlight {
			record, found = t, true
			break
		}
	}
	if !found {
		// The turn is in flight on the stream but has no durable record to
		// mark. That is a fact about the record, not a reason to displace
		// silently: the caller is told nothing was captured.
		f.deps.Log.Global().Warn(opFleetRollout, "the in-flight turn has no open durable record to displace", dlog.Context{
			"workspace": string(ws), "turn": string(*inFlight),
		})
		return "", false, nil
	}
	record.Displaced = true
	if err := f.deps.DB.PutTurn(ctx, record); err != nil {
		return "", false, fmt.Errorf("workspace: record the displaced turn %q: %w", *inFlight, err)
	}
	f.deps.Log.Global().Debug(opFleetRollout, "captured the displaced turn", dlog.Context{
		"workspace": string(ws), "turn": string(*inFlight),
	})
	return *inFlight, true, nil
}

// Prelaunch brings up a NEW shim for the workspace on a FRESH socket, INERT BY
// CONSTRUCTION: the process is spawned and dialed, but no session is started
// on it, so it coexists with the running one indefinitely. Nothing about the
// workspace's current session is touched here — that is what makes a failed
// prelaunch cost nothing.
func (f *Fleet) Prelaunch(ctx context.Context, ws ids.WorkspaceID) (shimclient.Client, error) {
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("workspace: prelaunch %q: %w", ws, err)
	}
	log, err := f.deps.Log.Workspace(record.Dir)
	if err != nil {
		return nil, fmt.Errorf("workspace: prelaunch %q: resolve log sink: %w", ws, err)
	}
	session, _, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("workspace: prelaunch %q: read the session record: %w", ws, err)
	}
	configDir := session.ConfigDir
	if configDir == "" {
		configDir = f.deps.Accounts.ConfigDirFor(record.Dir)
	}
	sink, err := f.deps.Log.ShimSink(record.Dir)
	if err != nil {
		return nil, fmt.Errorf("workspace: prelaunch %q: shim log sink: %w", ws, err)
	}
	uds := f.freshSocketPath(ws)
	client, err := f.deps.Supervisor.Spawn(ctx, shimclient.Spec{
		WorkspaceID:  ws,
		WorkspaceDir: record.Dir,
		UDSPath:      uds,
		StoreSocket:  f.deps.StoreSocket,
		ConfigDir:    configDir,
		// The relaunched shim carries the SAME host session identity: a
		// relaunch rotates the process, never the session.
		SessionID:    session.HostSessionID,
		ShimBuildSHA: f.deps.ShimBuildSHA,
		NodeBin:      f.deps.NodeBin,
		MainJS:       f.deps.MainJS,
		Fake:         f.deps.Fake,
		LogSink:      sink.File(),
		ForbidVendor: f.deps.ForbidVendor,
	})
	if err != nil {
		log.Error(opFleetRollout, "the inert prelaunch did not come up", dlog.Context{
			"workspace": string(ws), "uds": uds, "cause": err.Error(),
		})
		return nil, fmt.Errorf("workspace: prelaunch %q: %w", ws, err)
	}
	log.Debug(opFleetRollout, "prelaunched an inert shim beside the running one", dlog.Context{
		"workspace": string(ws), "uds": uds, "pid": client.PID(),
	})
	return client, nil
}

// HostSessionFacts are the DAEMON's own facts about one workspace's live
// session: exactly what the host stream's HostSessionExisting arm needs and
// nothing a webview draws. The server composes the arm from them.
type HostSessionFacts struct {
	// SessionID is the daemon-minted host session identity.
	SessionID string
	// Generation is the controller generation operating the session. It
	// rotates on a relaunch without the session id changing, which is what
	// scopes a fault window.
	Generation string
	// ShimAttached reports whether the session's shim link is connected right
	// now. It is what distinguishes "live but momentarily unwired" from "up".
	ShimAttached bool
	// VendorSessionID and ConfigDir are the vendor conversation's identifiers.
	// VendorSessionID is empty while no vendor conversation exists yet.
	VendorSessionID string
	ConfigDir       string
	// BackfillKnown reports whether anything in this daemon can state the
	// transcript's backfill. IT IS ALWAYS FALSE: backfill is the FILE PLANE's
	// delivery into the store, the daemon never imports store.v1 and holds no
	// store client, so there is no honest source for it here. The field exists
	// so the seam states the absence rather than inventing an arm.
	BackfillKnown bool
}

// HostSessionFacts answers one workspace's host-facing session facts. The bool
// reports that the workspace HAS a session; a workspace with none is the host
// stream's `none` arm, which is an answer and not a failure.
func (f *Fleet) HostSessionFacts(ctx context.Context, ws ids.WorkspaceID) (HostSessionFacts, bool, error) {
	session, ok, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		return HostSessionFacts{}, false, fmt.Errorf("workspace: host session facts for %q: %w", ws, err)
	}
	if !ok || session.HostSessionID == "" {
		return HostSessionFacts{}, false, nil
	}
	f.mu.RLock()
	generation := f.generation[ws]
	current := f.sessions[ws]
	f.mu.RUnlock()
	attached := current != nil && current.watcher != nil && current.watcher.Connected()
	return HostSessionFacts{
		SessionID:       session.HostSessionID,
		Generation:      strconv.Itoa(generation),
		ShimAttached:    attached,
		VendorSessionID: session.VendorSessionID,
		ConfigDir:       session.ConfigDir,
	}, true, nil
}

// freshSocketPath mints a socket path no running shim of this workspace holds.
// The generation rides in the name rather than in a directory, so the state
// root's socket-path budget — checked once at boot — still bounds it.
func (f *Fleet) freshSocketPath(ws ids.WorkspaceID) string {
	f.mu.Lock()
	f.generation[ws]++
	gen := f.generation[ws]
	f.mu.Unlock()
	base := f.deps.SocketPath(ws)
	return strings.TrimSuffix(base, ".sock") + ".n" + strconv.Itoa(gen) + ".sock"
}

// Install makes c the workspace's shim client, retiring whatever was there.
// The OLD PROCESS IS NOT KILLED HERE: the relaunch engine stood it down and
// passed the reap gate before calling, and the handover deliberately leaves it
// running. Only this daemon's watches on it are closed.
func (f *Fleet) Install(ctx context.Context, ws ids.WorkspaceID, c shimclient.Client) error {
	if c == nil {
		return fmt.Errorf("workspace: install a shim for %q: no client", ws)
	}
	f.mu.Lock()
	previous := f.sessions[ws]
	f.sessions[ws] = &live{client: c}
	delete(f.coldGates, ws)
	f.mu.Unlock()

	if previous != nil && previous.watcher != nil {
		if err := previous.watcher.Close(); err != nil {
			return fmt.Errorf("workspace: install a shim for %q: close the retired watches: %w", ws, err)
		}
	}
	f.deps.Log.Global().Debug(opFleetRollout, "installed a new shim client", dlog.Context{
		"workspace": string(ws), "pid": c.PID(), "retired": previous != nil,
	})
	return nil
}

// Adopt dials the workspace's ALREADY RUNNING shim without spawning — the
// successor's half of a transfer. It goes through the fleet's ordinary bring-up
// so the resume guard, the lock probe and the watcher opening are the same on
// both halves of a handover; the probe finds the transferred shim's lock still
// held, which is what selects the adopt path rather than a spawn.
func (f *Fleet) Adopt(ctx context.Context, ws ids.WorkspaceID) (shimclient.Client, error) {
	if err := f.Start(ctx, ws); err != nil {
		return nil, fmt.Errorf("workspace: adopt %q: %w", ws, err)
	}
	client, ok := f.Client(ws)
	if !ok {
		return nil, fmt.Errorf("workspace: adopt %q: the bring-up left no shim client", ws)
	}
	return client, nil
}

// Resume runs StartSession(resume) on c and, on success, opens the workspace's
// watches against it. It is GREEDY BY DESIGN: the resume is attempted at once
// rather than gated on a freshness guess, because a fast swap stays warm and
// the cold arm is the answer when it does not.
func (f *Fleet) Resume(ctx context.Context, ws ids.WorkspaceID, c shimclient.Client) (rollout.Resumed, error) {
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return rollout.Resumed{}, fmt.Errorf("workspace: resume %q: %w", ws, err)
	}
	log, err := f.deps.Log.Workspace(record.Dir)
	if err != nil {
		return rollout.Resumed{}, fmt.Errorf("workspace: resume %q: resolve log sink: %w", ws, err)
	}
	log = log.With(dlog.Context{"workspace": string(ws)})

	session, exists, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		return rollout.Resumed{}, fmt.Errorf("workspace: resume %q: read the session record: %w", ws, err)
	}
	src, err := decideSource(session, exists)
	if err != nil {
		return rollout.Resumed{}, fmt.Errorf("workspace: resume %q: %w", ws, err)
	}
	if src.Fresh {
		return rollout.Resumed{}, fmt.Errorf("workspace: resume %q: the workspace has no conversation to resume", ws)
	}

	started, err := f.startSession(ctx, log, ws, c, src, session)
	if err != nil {
		return rollout.Resumed{}, err
	}
	if started == nil {
		// The shim answered cold. The client stays installed: the gate's
		// answer re-opens the session through it, and the cold facts the shim
		// stated are handed back whole rather than reconstructed.
		f.mu.RLock()
		cold := f.lastCold[ws]
		f.mu.RUnlock()
		return rollout.Resumed{Cold: cold}, nil
	}

	watcher, err := f.watch(ctx, ws, c, sessionwatcher.Session{Started: started}, f.deps.Sinks, log)
	if err != nil {
		log.Error(opFleetRollout, "could not re-open the session's watches after a resume", dlog.Context{
			"cause": err.Error(),
		})
		return rollout.Resumed{}, fmt.Errorf("workspace: resume %q: start the watcher: %w", ws, err)
	}
	f.remember(ws, &live{client: c, watcher: watcher})
	// A resume keeps the session's host identity: the process rotated, the
	// session did not.
	if err := f.recordFacts(ctx, log, ws, session, started, session.ConfigDir, session.HostSessionID, c.PID()); err != nil {
		return rollout.Resumed{}, err
	}
	log.Info(opFleetRollout, "resumed the conversation on the new shim", dlog.Context{
		"vendor_session_id": started.GetVendorSessionId(), "shim_pid": c.PID(),
	})
	return rollout.Resumed{}, nil
}

// Hibernate stands a workspace's session down for the idle sweep. It is
// drain.Stand's first method; the fleet owns the client, so the controller
// never dials one.
func (f *Fleet) Hibernate(ctx context.Context, ws ids.WorkspaceID) (*shimv1.HibernateResponse, error) {
	client, ok := f.Client(ws)
	if !ok {
		return nil, fmt.Errorf("workspace: hibernate %q: the workspace has no live session", ws)
	}
	return client.Hibernate(ctx, &shimv1.HibernateRequest{})
}

// KillSession ends a workspace's session. It is drain.Stand's second method
// and Deps.Sessions' Stop in one behavior, so the two cannot disagree about
// what stopping a session means.
func (f *Fleet) KillSession(ctx context.Context, ws ids.WorkspaceID, force bool) error {
	return f.Stop(ctx, ws, force)
}

// The fleet IS the rollout's shim fleet, the drain's stand and the verbs'
// session surface. The assertions are here so a signature drift is a compile
// error at the definition rather than a nil interface at the composition root.
var (
	_ rollout.ShimFleet = (*Fleet)(nil)
	_ rollout.Freeness  = (*fleetFreeness)(nil)
	_ drain.Freeness    = (*fleetFreeness)(nil)
	_ drain.Stand       = (*Fleet)(nil)
	_ Sessions          = (*Fleet)(nil)
)

// fleetFreeness adapts the fleet to the per-workspace freeness answer the
// rollout and the drain wait on. It is a distinct type because both of their
// Freeness interfaces take the workspace explicitly while the fleet's own
// AwaitFree is already that shape — the adapter exists only to name the
// contract, so a change to either side fails here.
type fleetFreeness struct{ fleet *Fleet }

// Freeness answers the rollout's and the drain's shared freeness contract.
func (f *Fleet) Freeness() *fleetFreeness { return &fleetFreeness{fleet: f} }

func (a *fleetFreeness) Free(ws ids.WorkspaceID) bool { return a.fleet.Free(ws) }

func (a *fleetFreeness) AwaitFree(ctx context.Context, ws ids.WorkspaceID) error {
	return a.fleet.AwaitFree(ctx, ws)
}

// RouteGuidance delivers one submission straight to the workspace's session as
// a turn of its own, BYPASSING the prompt queue's lease policy.
//
// It is the merge orchestrator's guidance route. The queue cannot serve it: a
// parked merge lease is precisely what sends a submission to the orchestrator,
// so routing the orchestrator's own delivery back through the queue would loop
// on the lease that produced it.
//
// The turn is minted here and handed to the watcher, so the orchestrator
// resumes on that turn's REAL end rather than on a timer.
func (f *Fleet) RouteGuidance(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) (ids.TurnID, error) {
	if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return "", fmt.Errorf("workspace: route guidance on %q: an unspecified prompt origin is never delivered", ws)
	}
	sender, ok := f.Sender(ws)
	if !ok {
		return "", fmt.Errorf("workspace: route guidance on %q: the workspace has no live session", ws)
	}
	turn := wsm.NewTurnID()
	success, err := sender.StartTurn(ctx, turn, said, origin)
	if err != nil {
		return "", fmt.Errorf("workspace: route guidance on %q: %w", ws, err)
	}
	// The watcher is handed the accepted turn the same way the queue hands one
	// over: it names the main agent and feeds the opening page through the
	// history path, which is what makes the turn's end attributable.
	if watcher, live := f.Watcher(ws); live {
		watcher.SetMainAgent(success.GetPrompt().GetAgent())
		watcher.OnTurnOpened(ws, success.GetPrompt(), success.GetPage())
	}
	f.deps.Log.Global().Debug(opFleetRollout, "routed guidance into the session as its own turn", dlog.Context{
		"workspace": string(ws), "turn": string(turn), "origin": origin.String(),
	})
	return turn, nil
}

// RaiseColdGate draws the ordinary cold gate for a workspace whose resume
// answered `cold`. It is rollout.ColdGateFunc: the relaunch engine learns the
// cold facts and the fleet, which owns the gate's menu, is what serves them.
func (f *Fleet) RaiseColdGate(_ context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) error {
	if cold == nil {
		return fmt.Errorf("workspace: raise the cold gate on %q: no cold facts", ws)
	}
	session, _, err := f.deps.DB.Session(context.Background(), ws)
	if err != nil {
		return fmt.Errorf("workspace: raise the cold gate on %q: read the session record: %w", ws, err)
	}
	f.raiseColdGate(ws, session.VendorSessionID, cold)
	return nil
}

// SessionBuildSHA reports the shim build a workspace's LIVE session says it is
// running. It is a fact of the running process rather than of the session — a
// shim that dies takes its build with it — so it lives in the fleet's memory
// and not in a durable column.
func (f *Fleet) SessionBuildSHA(ws ids.WorkspaceID) (string, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	sha, ok := f.buildSHA[ws]
	return sha, ok
}

// ProbeLock probes ONE workspace's shim-held kernel lock from its worktree. It
// is rollout.LockProbeFunc and boot's probe in one behavior, so the two cannot
// disagree about which lock a workspace's is or about what "could not tell"
// means.
func (f *Fleet) ProbeLock(workspaceDir string) (sessionlock.State, error) {
	return f.probe(f.lockDir(), workspaceDir)
}
