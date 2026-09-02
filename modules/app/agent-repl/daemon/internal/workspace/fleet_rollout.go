package workspace

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
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
	if err := f.recordFacts(ctx, log, ws, session, started, session.ConfigDir, c.PID()); err != nil {
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
