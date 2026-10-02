package workspace

import (
	"context"
	"errors"
	"fmt"
	"strconv"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
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
//
// A REAPED CLIENT IS NO SESSION, read exactly as Fleet.Shim reads it. The map
// entry outlives the process — a shim killed out from under the daemon leaves
// its row behind until something tears it down — and answering that row as a
// live client is what sent a submitted prompt down the DELIVERY path instead
// of the revival one: StartTurn dialed a socket nothing was listening on, the
// call answered `unavailable`, and the workspace stayed dead with the prompt
// held as an outage. The sidebar's dead arm and this answer must not disagree
// about which shim serves a workspace.
func (f *Fleet) Client(ws ids.WorkspaceID) (shimclient.Client, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, ok := f.sessions[ws]
	if !ok {
		return nil, false
	}
	if _, reaped := session.client.Reaped(); reaped {
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

// Displaced is the turn a lease holder took the session away from: its id and
// the text it carried, so the resubmission at lease release does not depend on
// the turn's record still being open.
type Displaced struct {
	Turn ids.TurnID
	Text string
}

// CaptureDisplaced durably marks the turn a lease holder displaced, so it is
// resubmitted EXACTLY ONCE at lease release even across a daemon bounce, and
// ENDS that turn: the holder is about to drive the session itself, and a user
// turn left running underneath it would be racing the holder for the same
// conversation. It reports false when nothing was in flight, which is the
// ordinary case.
//
// THE TURN IS ENDED, NEVER ITS DETACHED WORK. The kill is UNFORCED: the user
// asked for a merge, not for their background agents, shells and monitors to
// stop, and detached work ends only by its own per-task stop or a forced kill
// the user explicitly asked for. A holder that needs the session quiet waits
// for it to fall free (Fleet.AwaitFree) rather than killing what runs there.
func (f *Fleet) CaptureDisplaced(ctx context.Context, ws ids.WorkspaceID) (Displaced, bool, error) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return Displaced{}, false, nil
	}
	inFlight := session.watcher.TurnInFlight()
	if inFlight == nil {
		return Displaced{}, false, nil
	}
	open, err := f.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		return Displaced{}, false, fmt.Errorf("workspace: capture the displaced turn %q: %w", *inFlight, err)
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
		return Displaced{}, false, nil
	}
	record.Displaced = true
	if err := f.deps.DB.PutTurn(ctx, record); err != nil {
		return Displaced{}, false, fmt.Errorf("workspace: record the displaced turn %q: %w", *inFlight, err)
	}
	// THE MARK GOES DOWN BEFORE THE KILL. A kill that landed with no durable
	// mark would end the user's turn and leave nothing to put back.
	if err := (&shimAdapter{client: session.client}).KillTurn(ctx, *inFlight, false, nil); err != nil {
		// The kill failing does NOT unmark the turn: it is still the turn the
		// holder displaced, and putting it back at release is right either way.
		f.deps.Log.Global().Warn(opFleetRollout, "the displaced turn could not be ended", dlog.Context{
			"workspace": string(ws), "turn": string(*inFlight), "cause": err.Error(),
		})
	}
	f.deps.Log.Global().Debug(opFleetRollout, "captured the displaced turn", dlog.Context{
		"workspace": string(ws), "turn": string(*inFlight),
	})
	return Displaced{Turn: *inFlight, Text: record.Text}, true, nil
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
	configDir := spawnRootFor(f.deps.Accounts, record.Dir, session)
	sink, err := f.deps.Log.ShimSink(record.Dir)
	if err != nil {
		return nil, fmt.Errorf("workspace: prelaunch %q: shim log sink: %w", ws, err)
	}
	uds := f.freshSocketPath(ws)
	// THE BUNDLE IS HELD FROM THE HASH TO THE SHIM'S ANSWER, as every spawn
	// holds it: the build the prelaunch states is the bytes node runs.
	build, release, err := f.deps.ShimBundle.Hold()
	if err != nil {
		log.Error(opFleetRollout, "the installed shim bundle's build is unresolvable; no shim is prelaunched", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return nil, fmt.Errorf("workspace: prelaunch %q: %w", ws, err)
	}
	defer release()
	client, err := f.deps.Supervisor.Spawn(ctx, shimclient.Spec{
		WorkspaceID:  ws,
		WorkspaceDir: record.Dir,
		UDSPath:      uds,
		StoreSocket:  f.deps.StoreSocket,
		ConfigDir:    configDir,
		// The relaunched shim carries the SAME host session identity: a
		// relaunch rotates the process, never the session.
		SessionID:    session.HostSessionID,
		ShimBuildSHA: build,
		NodeBin:      f.deps.NodeBin,
		MainJS:       f.deps.MainJS,
		Fake:         f.deps.Fake,
		LogSink:      sink.File(),
		ForbidVendor: f.deps.ForbidVendor,
		// THE PRELAUNCH IS A SPAWN LIKE ANY OTHER, and it spends the same
		// Node startup unrecorded. See Spec.Spawned.
		Spawned: func(pid int) { f.recordSpawnedShimPID(ctx, log, ws, pid) },
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
	log.Info(opFleetRollout, "the replacement shim is prelaunched and inert", dlog.Context{
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
	// BackfillKnown reports whether anything in this daemon can state the
	// transcript's backfill. IT IS ALWAYS FALSE: backfill is the FILE PLANE's
	// delivery into the store, the daemon never imports store.v1 and holds no
	// store client, so there is no honest source for it here. The field exists
	// so the seam states the absence rather than inventing an arm.
	BackfillKnown bool
}

// HostSessionFacts answers one workspace's host-facing session facts. The bool
// reports whether THIS DAEMON OPERATES a session for the workspace; anything
// else is the host view's `none` arm, which is an answer and not a failure.
//
// It reads only what the fleet holds in memory: the durable row can outlive
// the session it describes (a workspace this daemon has handed away, or has
// not brought up), and the live half of the host view must never be composed
// from a session nobody is operating.
func (f *Fleet) HostSessionFacts(ws ids.WorkspaceID) (HostSessionFacts, bool) {
	f.mu.RLock()
	current := f.sessions[ws]
	generation := f.generation[ws]
	f.mu.RUnlock()
	if current == nil || current.hostSessionID == "" {
		return HostSessionFacts{}, false
	}
	// The FIRST shim of a session is generation 1: freshSocketPath bumps the
	// counter only for a RELAUNCH's prelaunch, so an untouched session sits at
	// zero, and a generation of "0" would read as no generation at all.
	return HostSessionFacts{
		SessionID:    current.hostSessionID,
		Generation:   strconv.Itoa(generation + 1),
		ShimAttached: current.watcher != nil && current.watcher.Connected(),
	}, true
}

// freshSocketPath mints a socket path no running shim of this workspace holds.
// The generation rides in the name rather than in a directory, so the state
// root's socket-path budget — checked once at boot — still bounds it. It is
// minted past every generation on disk as well as this fleet's own counter
// (shimsocket.NextGeneration): an ADOPTED shim may already hold one this
// daemon never minted.
func (f *Fleet) freshSocketPath(ws ids.WorkspaceID) string {
	base := f.deps.SocketPath(ws)
	f.mu.Lock()
	before := f.generation[ws]
	path, gen := shimsocket.NextGeneration(base, before)
	f.generation[ws] = gen
	f.mu.Unlock()
	f.logTransition(ws, "shim_generation", before, gen, nil)
	return path
}

// ErrInstallDeadShim is Install's refusal of a client whose process is gone.
var ErrInstallDeadShim = errors.New("the shim to install has already exited")

// Install makes c the workspace's shim client, retiring whatever was there.
// The OLD PROCESS IS NOT KILLED HERE: the relaunch engine stood it down and
// passed the reap gate before calling, and the handover deliberately leaves it
// running. Only this daemon's watches on it are closed.
func (f *Fleet) Install(ctx context.Context, ws ids.WorkspaceID, c shimclient.Client) error {
	if c == nil {
		return fmt.Errorf("workspace: install a shim for %q: no client", ws)
	}
	// A DEAD CLIENT IS NEVER INSTALLED. A relaunch installed a replacement
	// that had died thirty seconds earlier, and its watches then dialed a
	// socket nobody served, leaving the workspace linked to nothing (deploy
	// 2026-09-24T18:27:44). Refused here, before the claim, the fleet is left
	// exactly as it was.
	if info, reaped := c.Reaped(); reaped {
		return fmt.Errorf("workspace: install a shim for %q: %w (pid %d, code %d, signal %q)", ws, ErrInstallDeadShim, info.PID, info.Code, info.Signal)
	}
	// AN INSTALLED CLIENT IS SERVED BY THIS DAEMON: the boot's adoption of a
	// survivor, the handover's adoption, the takeover's orphan recovery and a
	// relaunch's rotation all arrive here, and none may leave the serving row
	// naming a daemon that is gone. See claimServing.
	//
	// THE CLAIM COMES BEFORE THE MAP WRITE. Every caller of Install still owns
	// the client it passed -- the adoption detaches it, the relaunch retires
	// its prelaunch, the boot fails -- so a refused claim must leave the fleet
	// exactly as it was. Claimed after the write, a refusal left the fleet
	// holding a client with no watches and no started session that its caller
	// believed it had given up (live deploy 2026-09-24 15:07, three orphans on
	// a successor whose handle was still read-only).
	if err := f.claimServing(ctx, f.deps.Log.Global().With(dlog.Context{"workspace": string(ws)}), ws); err != nil {
		return err
	}
	// THE TURNS OPEN AT ATTACH ARE READ BEFORE THE CLIENT CAN BE SERVED. An
	// installed shim ran while no daemon of this process watched it, so a
	// turn row left open may have ended unobserved; the watcher compares this
	// set with the shim's own turn_in_flight once its facts arrive. Read
	// before the map write, no turn this daemon delivers to the new client can
	// be in it, and a read that fails leaves the fleet as it was.
	openAtAttach, err := f.openTurns(ctx, ws)
	if err != nil {
		return fmt.Errorf("workspace: install a shim for %q: %w", ws, err)
	}
	f.mu.Lock()
	previous := f.sessions[ws]
	if previous != nil {
		// A RELAUNCH INSTALLS ONLY ONCE ITS REAP GATE HAS PASSED, so the
		// client it replaces is gone, and is recorded so.
		f.noteReapLocked(ws, previous.client)
	}
	// An INSTALL rotates the process, never the session: the adopted client
	// serves the identity the retired one did.
	carried := ""
	if previous != nil {
		carried = previous.hostSessionID
	}
	// A NEWLY INSTALLED CLIENT HAS NO SESSION YET. `Install` rotates the
	// process, and the two things that follow it differ: the relaunch engine
	// installs a PRELAUNCHED shim and calls Resume next, while an adoption
	// attaches to a shim that has already started its one session and says so
	// itself. So the flag is not carried from the retired entry the way the
	// host identity is -- it is false here, and the adopting callers set it.
	f.sessions[ws] = &live{client: c, hostSessionID: carried}
	_, gateStood := f.coldGates[ws]
	delete(f.coldGates, ws)
	f.mu.Unlock()
	f.logTransition(ws, "session_live", previous != nil, true,
		dlog.Context{"shim_pid": c.PID(), "retired": previous != nil})
	if gateStood {
		f.logTransition(ws, "cold_gate_standing", true, false,
			dlog.Context{"reason": "replacement_shim_installed"})
	}

	if previous != nil && previous.watcher != nil {
		if err := previous.watcher.Close(); err != nil {
			return fmt.Errorf("workspace: install a shim for %q: close the retired watches: %w", ws, err)
		}
	}

	// AN INSTALLED SHIM IS WATCHED. The adoption's whole point is that the
	// conversation keeps running under a daemon that can SEE it: a client with
	// no watches leaves the daemon blind to the session it just adopted — no
	// turn terminals, no live work, no connectivity truth.
	//
	// THE ATTACH IS PURE (landing 7): the watcher opens with NO session facts
	// and takes them from the shim's own re-announcement of SessionStarted,
	// which rides every new WatchSession right after the opening diagnostics.
	// The durable record is not consulted for the opening level any more —
	// the shim is the authority on its own session.
	// THE LIVE-SHIM INVARIANT on the ROTATION path. An installed client is the
	// fleet holding a live shim for this workspace, and the two things that
	// follow an install can both leave the record untouched: an adoption
	// records no facts at all, and the relaunch's Resume parks at a cold gate
	// without recording any either. Retiring here is what keeps a workspace
	// whose shim is serving from reading killed to every surface that composes
	// off the record. See retireTerminalRecord.
	if err := f.retireTerminal(ctx, ws); err != nil {
		return err
	}

	if err := f.watchInstalled(ctx, ws, c, openAtAttach); err != nil {
		return err
	}

	f.deps.Log.Global().Debug(opFleetRollout, "installed a new shim client", dlog.Context{
		"workspace": string(ws), "pid": c.PID(), "retired": previous != nil,
	})
	f.deps.Log.Global().With(dlog.Context{"workspace": string(ws)}).Info(opFleetRollout,
		"installed the replacement shim client", dlog.Context{
			"pid": c.PID(), "retired": previous != nil,
		})
	// The process behind the session changed; the host view carries its pid's
	// attachment and its generation.
	f.publishHost(ws)
	return nil
}

// openTurnIDs names the workspace's turn rows that have no terminal.
func (f *Fleet) openTurns(ctx context.Context, ws ids.WorkspaceID) ([]sessionwatcher.OpenTurn, error) {
	open, err := f.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("read the turns open at attach: %w", err)
	}
	turns := make([]sessionwatcher.OpenTurn, 0, len(open))
	for _, t := range open {
		turns = append(turns, sessionwatcher.OpenTurn{ID: t.ID, StartedAt: t.StartedAt})
	}
	return turns, nil
}

// retireTerminal retires the workspace's terminal session record for a caller
// that holds no resolved logger of its own; the workspace's log sink is
// resolved from its record, and a record that cannot be read fails the call
// rather than retiring nothing quietly.
func (f *Fleet) retireTerminal(ctx context.Context, ws ids.WorkspaceID) error {
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return fmt.Errorf("workspace: retire the terminal session record for %q: %w", ws, err)
	}
	log := f.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(ws)})
	return retireTerminalRecord(ctx, log, f.deps.DB, opFleetRollout, ws)
}

// Adopt dials the workspace's ALREADY RUNNING shim without spawning — the
// successor's half of a transfer. It goes through the fleet's ordinary bring-up
// so the resume guard, the lock probe and the watcher opening are the same on
// both halves of a handover; the probe finds the transferred shim's lock still
// held, which is what selects the adopt path rather than a spawn.
func (f *Fleet) Adopt(ctx context.Context, ws ids.WorkspaceID) (shimclient.Client, error) {
	if client, ok := f.Client(ws); ok {
		return client, nil
	}
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("workspace: adopt %q: %w", ws, err)
	}
	log, logErr := f.deps.Log.Workspace(record.Dir)
	if logErr != nil {
		return nil, fmt.Errorf("workspace: adopt %q: resolve the workspace log sink: %w", ws, logErr)
	}
	log = log.With(dlog.Context{"workspace": string(ws)})
	// THE SOCKET IS THE SHIM'S CURRENT GENERATION, not the layout's base
	// name, exactly as boot's adopt and bringUpClient resolve it: a relaunch
	// moves the shim onto `<base>.nN.sock` and the counter that minted N lived
	// in the predecessor's memory, so dialing the base reaches nothing and the
	// adoption spends its whole bound.
	// A generation that is not live leaves the base path, which is what was
	// dialed before, and the adoption bound still covers a shim that is gone.
	socketPath, socket, socketErr := shimsocket.NewestLive(f.socketProbe, f.deps.SocketPath(ws))
	log.Debug(opFleetRollout, "resolved the running shim's socket generation", dlog.Context{
		"socket": socketPath, "socket_state": socket.String(), "cause": errText(socketErr),
	})
	client, err := f.adoptBounded(ctx, log, ws, record.Dir, socketPath, "handover")
	if err != nil {
		return nil, fmt.Errorf("workspace: adopt %q: dial the transferred shim: %w", ws, err)
	}
	// ATTACH ONLY. The transferred shim's session is ALREADY STARTED — the
	// whole point of a handover is that the conversation never stopped — and
	// StartSession on it would either be refused or, worse, start a second one.
	// The session facts come from the shim's re-announcement on the watch this
	// install opens (landing 7), never from the durable record.
	if err := f.Install(ctx, ws, client); err != nil {
		// A REFUSED INSTALL LEAVES NO HALF-ADOPTION. The fleet does not hold
		// this client (Install refuses before its map write when the claim
		// fails), so the link this call dialed is let go -- DETACHED, never
		// killed: the shim is someone's live conversation, and this daemon
		// ordering its teardown to tidy a failed adoption is the forced kill
		// nobody asked for.
		// A failure AFTER the map write (the terminal retirement, the
		// watches) leaves the client held, and a held client is the fleet's
		// to stop or hand over; only an unheld one is let go here.
		f.mu.RLock()
		entry := f.sessions[ws]
		f.mu.RUnlock()
		if entry == nil || entry.client != client {
			client.Detach()
			log.Info(opFleetRollout, "let go of the dialed shim after a refused install; the process keeps running", dlog.Context{
				"shim_pid": client.PID(), "socket": socketPath,
			})
		}
		return nil, fmt.Errorf("workspace: adopt %q: %w", ws, err)
	}
	// THE TRANSFERRED SHIM'S SESSION IS ALREADY STARTED, as the comment above
	// says, so the entry Install just wrote says so too.
	f.noteSessionStarted(ws)
	return client, nil
}

// NoteAdoptedSession states that the shim installed for a workspace by a boot
// adoption holds its started session (a survivor that is not inert). Install
// leaves the fact false, because a relaunch installs a shim with none. The
// boot releases the reconnect holds itself, once the holds are restored.
func (f *Fleet) NoteAdoptedSession(ws ids.WorkspaceID) {
	f.noteSessionStarted(ws)
}

// watchInstalled opens an adopted shim's watches. The durable record is read
// for ONE decision only — whether there is a conversation here at all — because
// a workspace with no session record has nothing to watch. Every FACT about the
// session comes from the shim's re-announcement on the watch itself.
func (f *Fleet) watchInstalled(ctx context.Context, ws ids.WorkspaceID, c shimclient.Client, openAtAttach []sessionwatcher.OpenTurn) error {
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return fmt.Errorf("workspace: install a shim for %q: %w", ws, err)
	}
	log, err := f.deps.Log.Workspace(record.Dir)
	if err != nil {
		return fmt.Errorf("workspace: install a shim for %q: resolve log sink: %w", ws, err)
	}
	log = log.With(dlog.Context{"workspace": string(ws)})

	session, exists, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		return fmt.Errorf("workspace: install a shim for %q: read the session record: %w", ws, err)
	}
	if !exists || session.VendorSessionID == "" {
		log.Debug(opFleetRollout, "the installed shim has no recorded conversation to watch", nil)
		return nil
	}

	watcher, err := f.startWatcher(ctx, log, ws, c, sessionwatcher.Session{OpenAtAttach: openAtAttach})
	if err != nil {
		log.Error(opFleetRollout, "could not open the adopted session's watches", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("workspace: install a shim for %q: start the watcher: %w", ws, err)
	}
	f.mu.Lock()
	attached := false
	var displaced sessionwatcher.Watcher
	if current, ok := f.sessions[ws]; ok && current.client == c {
		attached = current.watcher != nil
		displaced = current.watcher
		current.watcher = watcher
	}
	f.mu.Unlock()
	// THE WATCHER THIS ONE REPLACES IS CLOSED, for the reason `remember`
	// states: the map entry is a watcher's only handle, and one overwritten
	// in place keeps its streams standing until they end on their own -- an
	// end nothing has been told about, which is recorded as a severing.
	if displaced != nil && displaced != watcher {
		f.closeDisplaced(ws, displaced, "the installed shim's watches replaced the previous fleet")
	}
	f.logTransition(ws, "watcher_attached", attached, true,
		dlog.Context{"shim_pid": c.PID()})
	log.Info(opFleetRollout, "opened the adopted session's watches", dlog.Context{
		"vendor_session_id": session.VendorSessionID, "shim_pid": c.PID(),
	})
	return nil
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
	// THE SAME TRANSCRIPT-AWARE CLASSIFIER THE COLD BRING-UP USES. A bounce
	// of a session that pre-minted a vendor id but never took a turn has no
	// transcript to resume, and naming it anyway earned `unknown_session` from
	// the shim with no client installed and every later prompt answered
	// `no_session`. It comes up FRESH instead, on the prelaunched shim, with
	// the abandoned id recorded as a fault.
	src, err := f.classifySource(ctx, log, ws, record.Dir, session, exists)
	if err != nil {
		return rollout.Resumed{}, fmt.Errorf("workspace: resume %q: %w", ws, err)
	}

	// THE BOUNCE IS ALSO HOW AN ACCOUNT SWITCH TAKES EFFECT (SelectAccount,
	// owner ruling 2026-09-13). The prelaunched shim above was spawned under
	// exactly this root, so the transcript has to be carried into it before
	// the resume is sent: a resume against a root that does not hold the
	// transcript is a resume of nothing.
	configDir := spawnRootFor(f.deps.Accounts, record.Dir, session)
	if session.ConfigDir != "" && session.ConfigDir != configDir {
		if err := f.portAcrossAccounts(ctx, log, record.Dir, session, configDir, src); err != nil {
			return rollout.Resumed{}, err
		}
	}

	// THE PRELAUNCHED SHIM HOLDS NO SESSION until this StartSession succeeds,
	// and that is KNOWN here rather than merely unread: a failed or retried
	// vendor start must not leave a later unforced bounce waiting on a
	// freeness its watcher can never state (Fleet.SessionAbsent).
	f.markSessionAbsent(ws, c)
	runCtx, finishRun := f.beginVendorStart(ctx, ws)
	started, err := f.startSession(runCtx, log, ws, c, src, session, configDir)
	finishRun()
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

	// THE WATCHER OUTLIVES THE CALL THAT OPENED IT. `ctx` here is the
	// relaunch's own, and it is cancelled the moment the relaunch returns:
	// handed straight to the watcher, it tore the freshly-opened fleet down
	// within a millisecond of opening it, and the watcher read the daemon's own
	// cancel back as `canceled: context canceled` on both standing streams --
	// two ERRORs, a `link_severed` WARN and its health fault, plus two h2c
	// CANCEL warnings on the shim, for a session that had just come up
	// (realtest sweep 2026-09-12T15:23:03.338). Both other sites that open a
	// fleet already detach the context for exactly this reason; this one did
	// not.
	watcher, err := f.startWatcher(ctx, log, ws, c, sessionwatcher.Session{Started: started})
	if err != nil {
		log.Error(opFleetRollout, "could not re-open the session's watches after a resume", dlog.Context{
			"cause": err.Error(),
		})
		return rollout.Resumed{}, fmt.Errorf("workspace: resume %q: start the watcher: %w", ws, err)
	}
	if err := f.hold(ctx, log, ws, &live{client: c, watcher: watcher, hostSessionID: session.HostSessionID, sessionStarted: true}); err != nil {
		return rollout.Resumed{}, err
	}
	// A resume keeps the session's host identity: the process rotated, the
	// session did not.
	if err := f.recordFacts(ctx, log, ws, session, started, configDir, session.HostSessionID, c.PID()); err != nil {
		return rollout.Resumed{}, err
	}
	f.publishHost(ws)
	log.Info(opFleetRollout, "the session is up on the new shim", dlog.Context{
		"fresh": src.Fresh, "vendor_session_id": started.GetVendorSessionId(), "shim_pid": c.PID(),
	})
	f.deps.SessionsUp(ws)
	return rollout.Resumed{}, nil
}

// Hibernate stands a workspace's session down for the idle sweep. It is
// drain.Stand's first method; the fleet owns the client, so the controller
// never dials one.
func (f *Fleet) Hibernate(ctx context.Context, ws ids.WorkspaceID) (*shimv1.HibernateResponse, error) {
	client, ok := f.Client(ws)
	if !ok {
		return nil, fmt.Errorf("workspace: hibernate %q: %w", ws, drain.ErrNoLiveSession)
	}
	response, err := client.Hibernate(ctx, &shimv1.HibernateRequest{})
	if err != nil {
		return nil, err
	}
	f.deps.Log.Global().With(dlog.Context{"workspace": string(ws)}).Info(opFleetRollout,
		"hibernated the workspace session", dlog.Context{"shim_pid": client.PID()})
	return response, nil
}

// Serving reports whether this daemon holds a shim it can address for the
// workspace AND that shim holds a started session. It is drain.Stand's
// selection predicate, and it answers off Client so it cannot disagree with the
// directive it gates: a reaped client is a row awaiting teardown, not a
// session, and a workspace whose bring-up has not installed a client yet is not
// one either.
//
// THE SECOND HALF IS THE ONE THE FIRST DOES NOT COVER. A cold-gated bring-up
// and a relaunch's freshly installed shim both leave an ADDRESSABLE client with
// no session behind it, and the idle sweep then sent a Hibernate directive
// whose only possible answer was `no_session` -- every five minutes, forever,
// for a workspace whose session had never begun. The predicate's own contract
// already said a workspace "whose session is not up" is skipped; this is what
// makes that true.
func (f *Fleet) Serving(ws ids.WorkspaceID) bool {
	if _, ok := f.Client(ws); !ok {
		return false
	}
	return f.sessionStarted(ws)
}

// KillSession ends a workspace's session: it ASKS THE SHIM to end the session
// first and only then stops the process.
//
// The order is the whole point of the verb's name. The drain's stand-down and
// the kill verb both need the shim to write its own terminals before its
// process goes, and a signal alone gives it no chance to. A shim that will not
// answer is not a reason to leave the process running, so the stop below is
// unconditional and the refusal is evidence.
func (f *Fleet) KillSession(ctx context.Context, ws ids.WorkspaceID, force bool) error {
	if shim, live := f.Shim(ws); live && f.sessionStarted(ws) {
		// THE WATCHER IS TOLD BEFORE THE VERB GOES. The shim ends its standing
		// streams as the session ends, and a watcher that has not been told
		// reads this daemon's own act as a transport fault: it records a
		// severing at ERROR, marks the link degraded, and reopens watches at a
		// shim the next line is about to stop. It stays OPEN, though — the
		// shim writes its terminals as the session ends, and Stop below is
		// what closes it.
		if watcher, ok := f.sessionWatcher(ws); ok {
			watcher.SessionEnding("the daemon is ending the session")
		}
		// THE LATCH IS ARMED BEFORE THE ASK, so it is armed before the
		// ESCALATION the ask's failure leads to. `Fleet.Stop` below is
		// unconditional, and a shim that never answered the rpc never latched
		// anything through it -- the realtest's preflight stood the daemon
		// down while both shims were hung, and the forced stop that followed
		// was recorded as `daemon.shimclient.exit` ERROR "shim died" plus four
		// `daemon.shimclient.redial` WARNs for a teardown this daemon ordered.
		ordered := shim.StandDown()
		if err := shim.KillSession(ctx, force); err != nil {
			f.logKillDidNotAnswer(ctx, ws, force, ordered, err)
		}
	} else if live {
		// A SHIM WITH NO SESSION IS STOPPED, NOT DIRECTED. Asking it to end a
		// session it never started is answered `no_session` -- which the
		// branch above then reported as a kill that "did not answer", against
		// a shim that answered perfectly well. The process stop below is what
		// this verb owed such a workspace all along.
		f.logNoSessionToKill(ctx, ws, force)
	}
	return f.Stop(ctx, ws, force)
}

// logKillDidNotAnswer records a session kill the shim did not answer, ahead of
// the process stop that follows it regardless.
//
// IT IS INFO WHEN THIS DAEMON ORDERED THE STAND-DOWN, because then the
// escalation is the mechanism working: the verb's whole contract is that a
// shim which will not answer is still stopped, and the stop is this daemon's
// own act, recorded on its own. It stays WARN for a kill outside a stand-down
// this daemon ordered -- a DETACHED client arms nothing, because that process
// belongs to the successor daemon and this one is ending nothing of its.
func (f *Fleet) logKillDidNotAnswer(ctx context.Context, ws ids.WorkspaceID, force, ordered bool, cause error) {
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return
	}
	log, err := f.deps.Log.Workspace(record.Dir)
	if err != nil {
		return
	}
	evidence := dlog.Context{"workspace": string(ws), "force": force, "cause": cause.Error()}
	if ordered {
		log.Info(opBringUp, "the session kill did not answer; stopping the process anyway", evidence)
		return
	}
	log.Warn(opBringUp, "the session kill did not answer; stopping the process anyway", evidence)
}

// logNoSessionToKill records, at DEBUG, that a stand-down skipped the session
// directive because the shim holds no session. It is DEBUG because it is an
// ordinary shape -- a cold-gated workspace and a relaunch's prelaunched shim
// both reach it -- and the stop it precedes is recorded on its own.
func (f *Fleet) logNoSessionToKill(ctx context.Context, ws ids.WorkspaceID, force bool) {
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		f.deps.Log.Global().Debug(opBringUp, "no session was started on this shim; stopping the process",
			dlog.Context{"workspace": string(ws), "force": force})
		return
	}
	f.deps.Log.WorkspaceOrCentral(record.Dir).Debug(opBringUp,
		"no session was started on this shim; stopping the process",
		dlog.Context{"workspace": string(ws), "force": force})
}

// HandOver ends this daemon's supervision of a workspace's shim for a handover:
// its watches are closed, then the client is detached, leaving the process
// running for the successor. It answers false when the workspace has no
// session here.
//
// THE WATCHES CLOSE WITH THE DETACH. A detach alone left the watch fleet
// standing on a shim that was now the successor's, so when the successor
// bounced that shim, every stream ended against a watcher nobody had told and
// the exiting incumbent recorded ERROR severings, a `link_severed` WARN and a
// health fault for a shim it no longer owned (live handover 2026-09-24T18:06).
// A closed watcher reads any later stream end as a torn-down stream, at DEBUG.
//
// A FAILED CLOSE IS RETURNED AND NOTHING IS DETACHED, so the transfer fails
// loudly rather than handing over a shim this daemon is still watching.
func (f *Fleet) HandOver(ws ids.WorkspaceID) (bool, error) {
	f.mu.Lock()
	session, ok := f.sessions[ws]
	var watcher sessionwatcher.Watcher
	if ok {
		watcher = session.watcher
		session.watcher = nil
	}
	f.mu.Unlock()
	if !ok {
		return false, nil
	}
	// The close runs OFF the lock: it joins the watcher's in-flight sink
	// dispatch, and those sinks read the fleet.
	if watcher != nil {
		if err := watcher.Close(); err != nil {
			return true, fmt.Errorf("workspace: hand over %q: close the watches: %w", ws, err)
		}
	}
	session.client.Detach()
	// A DETACHED CLIENT IS NO LONGER THIS FLEET'S, so it leaves the session
	// map with the detach. Left in it, Client answered a link that was
	// already closed, and Adopt returned that dead link instead of dialing:
	// an incumbent taking a handed-over workspace BACK (its adoption window
	// expired) re-attached nothing and served it through a closed client.
	f.mu.Lock()
	if f.sessions[ws] == session {
		delete(f.sessions, ws)
		delete(f.coldGates, ws)
		delete(f.lastCold, ws)
	}
	f.mu.Unlock()
	f.logTransition(ws, "session_live", true, false,
		dlog.Context{"reason": "handed_over", "shim_pid": session.client.PID()})
	f.deps.Log.Global().Info(opFleetRollout, "handed the workspace's shim over; its watches are closed and the process keeps running", dlog.Context{
		"workspace": string(ws), "shim_pid": session.client.PID(), "watched": watcher != nil,
	})
	return true, nil
}

// AwaitFacts blocks until the workspace's installed watcher has taken up the
// session facts, or ctx ends. It is what a MID-WORK adoption waits on before
// it lets a held prompt go: until the adopted shim's re-announcement lands,
// the watcher answers no turn in flight for a turn the shim is running, and a
// prompt delivered in that window would start a second turn beside it. A
// workspace with no watcher has no recorded conversation to announce, and
// answers at once.
func (f *Fleet) AwaitFacts(ctx context.Context, ws ids.WorkspaceID) error {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return nil
	}
	if err := session.watcher.AwaitSessionFacts(ctx); err != nil {
		return fmt.Errorf("workspace: await the adopted session's facts on %q: %w", ws, err)
	}
	return nil
}

// ColdGateStanding answers the cold facts of the gate standing on a
// workspace, false when none stands. A handover carries them: the gate is
// daemon memory, and the successor raises it again over the parked shim it
// adopts (AdoptParked).
func (f *Fleet) ColdGateStanding(ws ids.WorkspaceID) (*conversationv1.SessionCold, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	if _, ok := f.coldGates[ws]; !ok {
		return nil, false
	}
	cold := f.lastCold[ws]
	return cold, cold != nil
}

// AdoptParked adopts a running shim whose session is PARKED AT ITS COLD GATE:
// a handover's successor taking over a workspace whose gate the incumbent
// served. The shim started no session -- it refused the resume `cold` before
// it took any lock -- so there is nothing to watch: the client is held exactly
// as the park holds it, with no watcher, and the gate is raised again from the
// carried facts, so the user's answer re-opens through this daemon.
func (f *Fleet) AdoptParked(ctx context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) (shimclient.Client, error) {
	if cold == nil {
		return nil, fmt.Errorf("workspace: adopt the parked shim of %q: no cold facts", ws)
	}
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("workspace: adopt the parked shim of %q: %w", ws, err)
	}
	log, err := f.deps.Log.Workspace(record.Dir)
	if err != nil {
		return nil, fmt.Errorf("workspace: adopt the parked shim of %q: resolve the workspace log sink: %w", ws, err)
	}
	log = log.With(dlog.Context{"workspace": string(ws)})
	session, _, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opFleetRollout, "could not read the parked session's record", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("workspace: adopt the parked shim of %q: read the session record: %w", ws, err)
	}
	socketPath, socket, socketErr := shimsocket.NewestLive(f.socketProbe, f.deps.SocketPath(ws))
	log.Debug(opFleetRollout, "resolved the parked shim's socket generation", dlog.Context{
		"socket": socketPath, "socket_state": socket.String(), "cause": errText(socketErr),
	})
	client, err := f.adoptBounded(ctx, log, ws, record.Dir, socketPath, "handover_parked")
	if err != nil {
		return nil, fmt.Errorf("workspace: adopt the parked shim of %q: %w", ws, err)
	}
	// THE CLAIM COMES BEFORE THE MAP WRITE, as Install's does: a refused claim
	// leaves no half-adoption, and the link this call dialed is let go --
	// DETACHED, never killed: the parked shim is the user's conversation
	// waiting on their answer.
	if err := f.claimServing(ctx, log, ws); err != nil {
		client.Detach()
		log.Info(opFleetRollout, "let go of the dialed parked shim after a refused claim; the process keeps running", dlog.Context{
			"shim_pid": client.PID(), "socket": socketPath,
		})
		return nil, fmt.Errorf("workspace: adopt the parked shim of %q: %w", ws, err)
	}
	if err := f.hold(ctx, log, ws, &live{client: client, hostSessionID: session.HostSessionID}); err != nil {
		return nil, fmt.Errorf("workspace: adopt the parked shim of %q: %w", ws, err)
	}
	f.standCarriedColdGate(ws, session.VendorSessionID, cold, spawnRootFor(f.deps.Accounts, record.Dir, session))
	log.Info(opFleetRollout, "adopted the running shim parked at its cold gate; the gate stands on this daemon", dlog.Context{
		"shim_pid": client.PID(), "vendor_session_id": session.VendorSessionID,
	})
	return client, nil
}

// RaiseCarriedColdGate raises a carried cold gate over the parked shim a
// FRESH BOOT already adopted: a restart's replacement dials every surviving
// shim in its boot, parked ones included, and the gate -- daemon memory --
// comes across in the carry the outgoing daemon wrote. A workspace this
// daemon holds no shim for has nothing to raise the gate over.
func (f *Fleet) RaiseCarriedColdGate(ctx context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) error {
	if cold == nil {
		return fmt.Errorf("workspace: raise the carried cold gate of %q: no cold facts", ws)
	}
	if _, ok := f.Client(ws); !ok {
		return fmt.Errorf("workspace: raise the carried cold gate of %q: no shim is held for the workspace", ws)
	}
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return fmt.Errorf("workspace: raise the carried cold gate of %q: %w", ws, err)
	}
	session, _, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		return fmt.Errorf("workspace: raise the carried cold gate of %q: read the session record: %w", ws, err)
	}
	f.standCarriedColdGate(ws, session.VendorSessionID, cold, spawnRootFor(f.deps.Accounts, record.Dir, session))
	f.deps.Log.Global().With(dlog.Context{"workspace": string(ws)}).Info(opFleetRollout,
		"raised the carried cold gate over the adopted parked shim; the gate stands on this daemon", dlog.Context{
			"vendor_session_id": session.VendorSessionID,
		})
	return nil
}

// standCarriedColdGate stands a carried cold gate on this daemon over a held
// parked shim: whichever path adopted the shim, the gate is raised the one
// way.
//
// THE LINK IS RESTATED, as the park restates it: no session runs behind a
// parked shim, and without it the surfaces draw whatever link they last knew
// over the gate the user has to answer.
func (f *Fleet) standCarriedColdGate(ws ids.WorkspaceID, vendorSessionID string, cold *conversationv1.SessionCold, configDir string) {
	f.deps.Sinks.Footer.OnLink(ws, shimclient.LinkConnected)
	f.deps.Sinks.Topbar.OnLink(ws, shimclient.LinkConnected)
	f.deps.Sinks.Sidebar.OnLink(ws, shimclient.LinkConnected)
	f.raiseColdGate(ws, vendorSessionID, cold, configDir)
	f.publishHost(ws)
}

// StandDown is the rollout's stand-down: end the session, then stop the
// process, forced. It is KillSession under the name the rollout's contract
// gives it, so a handover that must stop a workspace it cannot transfer takes
// the same path -- watcher told first, terminals written, then the process --
// as every other stand-down in the daemon.
func (f *Fleet) StandDown(ctx context.Context, ws ids.WorkspaceID) error {
	return f.KillSession(ctx, ws, true)
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

// RaiseColdGate draws the ordinary cold gate for a workspace whose resume
// answered `cold`. It is rollout.ColdGateFunc: the relaunch engine learns the
// cold facts and the fleet, which owns the gate's menu, is what serves them.
func (f *Fleet) RaiseColdGate(_ context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) error {
	if cold == nil {
		return fmt.Errorf("workspace: raise the cold gate on %q: no cold facts", ws)
	}
	record, err := f.deps.DB.Workspace(context.Background(), ws)
	if err != nil {
		return fmt.Errorf("workspace: raise the cold gate on %q: %w", ws, err)
	}
	session, _, err := f.deps.DB.Session(context.Background(), ws)
	if err != nil {
		return fmt.Errorf("workspace: raise the cold gate on %q: read the session record: %w", ws, err)
	}
	// The relaunched shim was spawned under spawnRootFor (Prelaunch), so the
	// gate serves that account's menu.
	f.raiseColdGate(ws, session.VendorSessionID, cold, spawnRootFor(f.deps.Accounts, record.Dir, session))
	return nil
}

// ProbeLock probes ONE workspace's shim-held kernel lock from its worktree. It
// is rollout.LockProbeFunc and boot's probe in one behavior, so the two cannot
// disagree about which lock a workspace's is or about what "could not tell"
// means.
func (f *Fleet) ProbeLock(workspaceDir string) (sessionlock.State, error) {
	return f.probe(f.deps.LockDir, workspaceDir)
}
