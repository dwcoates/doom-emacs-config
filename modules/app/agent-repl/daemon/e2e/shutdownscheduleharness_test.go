// The SCHEDULED-SHUTDOWN DRAIN LEASE harness: a daemon stack that can be
// BOUNCED over one durable state directory while still spawning REAL shims,
// and whose executed shutdown is OBSERVABLE.
//
// WHY THIS IS NOT newUDSHarness. Two things the drain lease needs are absent
// there, and neither can be added from a test file without editing the shared
// harness:
//
//  1. AgentShimConfig.RequestShutdown is UNSET in newUDSHarness, so the daemon
//     it builds has no way to execute a shutdown at all — which is precisely
//     the terminal step of a drain. Here the hook is bound, records the
//     stop_shims it was handed, and drives the real server.ShutdownAll, so
//     "the drain executed, honoring the flag fixed at schedule time" is a fact
//     the test reads rather than infers.
//
//  2. newUDSHarness constructs its state store, shim listener and session
//     controller inside itself over per-call temp paths and lives exactly as
//     long as the test does, so a daemon BOUNCE — the durability half of the
//     lease contract — has no seam. This file follows mergedurability's
//     precedent instead: a `shutdownWorld` owns the long-lived things a bounce
//     must not disturb (the node runtime, the built shim bundle, the shim
//     socket, the running shim-store, the state directory), and `boot` stands
//     a whole daemon up over them as many times as a test needs.
//
// Everything else is deliberately the SAME wiring newUDSHarness builds — real
// TS shim in `--fake` offline mode over UDS, real shim-store, real SSM,
// registry, session controller, merge lease and frontend server — because the
// lease is enforced daemon-side in the prompt queue and a stubbed queue would
// exercise nothing.
//
// Reuses e2e_test.go's building blocks READ-ONLY (nodePath, buildShim,
// buildShimStore, startShimStore, isolatedShimSocket, stubLifecycle,
// newEmptyWorkspaceCreation, e2eHarness/createSession/dial, readFrame,
// writeCmd, frameTimeout) plus interrupt_e2e_test.go's awaitAll and
// clearcompact_e2e_test.go's liveSession.
package e2e

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"

	"claude-repld/internal/dlog"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shim"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/geometry"
	"claude-repld/internal/workspace/merge"
)

// --- the world a bounce does not disturb ------------------------------------

// shutdownWorld owns everything that must OUTLIVE any one daemon: the node
// runtime, the shim bundle built from source, the daemon⇄shim socket, the
// running shim-store, and the directory the durable state lives in.
//
// A daemon bounce is `world.boot(t)` twice. Both boots resolve the same shim
// socket through the production resolver and open the same state file, which
// is what makes the second one a bounce rather than a different daemon.
type shutdownWorld struct {
	node      string
	script    string
	shimSock  string
	storeSock string
	// stateDir holds the state database AND the merge queue, so a second boot
	// finds what the first wrote.
	stateDir string
}

func newShutdownWorld(t *testing.T) *shutdownWorld {
	t.Helper()
	node := nodePath(t)
	script := buildShim(t, node, "")
	storeBin := buildShimStore(t)
	// Not t.TempDir(): the test-name-derived path exceeds the 104-byte
	// sun_path limit, so bind(2) fails on macOS (e2e_test.go, same reason).
	sockDir, err := os.MkdirTemp("/tmp", "agent-repl-drain-")
	if err != nil {
		t.Fatalf("make short socket dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(sockDir) })
	// The session locks resolve under $HOME; the shim socket is claimed
	// separately and explicitly (isolatedShimSocket) so it is a stated claim
	// rather than a side effect of moving HOME.
	t.Setenv("HOME", sockDir)
	shimSock := isolatedShimSocket(t, sockDir)
	storeSock := filepath.Join(sockDir, "store.sock")
	startShimStore(t, storeBin, storeSock)
	return &shutdownWorld{node: node, script: script, shimSock: shimSock, storeSock: storeSock, stateDir: sockDir}
}

// --- one boot of the daemon --------------------------------------------------

// trackedShim is one spawned shim process and the channel that closes when it
// has exited. It is how `stop_shims` is observed as a FACT about a process
// rather than as a flag the daemon logged.
type trackedShim struct {
	workspace string
	proc      *shim.Proc
	exited    chan struct{}
}

// shutdownBoot is ONE daemon, stood up over a shutdownWorld.
type shutdownBoot struct {
	ts    *httptest.Server
	world *shutdownWorld

	// executed carries the stop_shims of every shutdown this daemon EXECUTED,
	// published only after server.ShutdownAll has returned — so a test that has
	// received from it knows the teardown already happened and may assert on
	// shim liveness without racing it.
	executed chan bool

	shimsMu sync.Mutex
	shims   []*trackedShim

	stop     func()
	stopOnce sync.Once
}

// boot stands a complete daemon up over the world's durable state. Calling it
// twice (with a bounce in between) is the durability scenario.
func (w *shutdownWorld) boot(t *testing.T) *shutdownBoot {
	t.Helper()
	b := &shutdownBoot{world: w, executed: make(chan bool, 4)}

	stateStore, err := statedb.Open(filepath.Join(w.stateDir, "state.db"))
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	reg := registry.OpenWith(registry.Options{DB: stateStore, Logf: t.Logf})
	ssmMgr, err := ssm.Open(ssm.Options{
		DB:       stateStore,
		Resolver: server.NewRegistryResolver(reg),
		Logf:     t.Logf,
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}

	forwarder := &server.PushForwarder{Logf: t.Logf}
	shimListener := shimlisten.New(t.Logf)
	if err := shimListener.Listen(w.shimSock); err != nil {
		t.Fatalf("listen for shims: %v", err)
	}
	if err := sessionlock.EnsureDir(); err != nil {
		t.Fatalf("make session lock dir: %v", err)
	}
	targets := dlog.NewTargetManager()
	fileDiagnostics, err := server.NewTargetFileDiagnosticPersister(targets, os.Stderr, false)
	if err != nil {
		t.Fatalf("build file diagnostic persister: %v", err)
	}

	udsSpawn := func(sessionID string, opts server.CreateOpts) (server.ShimStopFunc, error) {
		workspace, err := dlog.WorkspaceFromDirectory(opts.CWD)
		if err != nil {
			return nil, err
		}
		shimTarget, err := targets.OpenWorkspaceRuntime(workspace, dlog.RuntimeShim)
		if err != nil {
			return nil, err
		}
		canonicalOpts := opts
		canonicalOpts.CWD = workspace.Directory
		argv := server.ShimUDSArgv(w.node, w.script, sessionID, true /*forceFake*/, canonicalOpts, w.shimSock)
		argv = append(argv, "--log-fd", "3", "--store-socket", w.storeSock)
		proc, spawnErr := shim.Spawn(shim.Options{
			Argv:       argv,
			Dir:        workspace.Directory,
			ExtraFiles: []*os.File{shimTarget},
			Logger:     testShimLogger{t: t},
		})
		if spawnErr != nil {
			return nil, spawnErr
		}
		tracked := &trackedShim{workspace: workspace.Directory, proc: proc, exited: make(chan struct{})}
		b.shimsMu.Lock()
		b.shims = append(b.shims, tracked)
		b.shimsMu.Unlock()
		// ONE reaper per process: it drains the event stream, reaps the child
		// exactly once (cmd.Wait is not re-entrant), and publishes the exit as
		// a closed channel every waiter can observe. The harness's cleanup
		// waits on that channel rather than calling Wait a second time.
		go func() {
			for range proc.Events() { //nolint:revive
			}
			_ = proc.Wait()
			close(tracked.exited)
		}()
		t.Cleanup(func() {
			_ = proc.Terminate(shim.Stop{Initiator: "e2e_harness_cleanup", Reason: "test teardown"})
			<-tracked.exited
		})
		return func(by server.ShimStop) error { return proc.Terminate(by) }, nil
	}

	seqStore := server.NewRegistrySeqStore(reg, t.Logf)
	modelCatalogs := server.NewSessionModelCatalogs()
	registrar := &server.RegistryRegistrar{Reg: reg, Logf: t.Logf, ModelCatalogs: modelCatalogs}
	progressMgr := progress.New(progress.Options{Logf: t.Logf})
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           server.NewShimSpawner(reg, shimListener.Connected, shimListener.Evict, udsSpawn, t.Logf),
		Locator:           &server.SessionLocator{Reg: reg},
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		PermissionModes:   server.NewRegistryModeStore(reg),
		Registrar:         registrar,
		ModelCatalogs:     registrar,
		DaemonVersion:     "0.1.0-e2e-drain",
		ProtocolVersion:   "1",
		ShimBuildSHA:      func() string { return "" },
		Logf:              t.Logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}

	binding := &server.SessionCommandBinding{Logf: t.Logf}
	// Beside the state store, not in a per-boot temp dir: a bounce must find
	// what the previous boot published.
	mergeQueue, err := merge.NewFileQueue(filepath.Join(w.stateDir, "merge-queue"), t.Logf)
	if err != nil {
		t.Fatalf("open merge queue: %v", err)
	}
	mergeLease, err := ssm.NewMergeLease(ssm.MergeLeaseConfig{
		Manager: ssmMgr, Queue: mergeQueue, Interrupter: controller,
	})
	if err != nil {
		t.Fatalf("build merge lease: %v", err)
	}
	geometryStore, err := geometry.Open(stateStore, t.Logf)
	if err != nil {
		t.Fatalf("open geometry: %v", err)
	}
	// The drain lease's durable record, opened over the SAME state store the
	// world owns — so a bounce reads back the schedule its predecessor wrote
	// rather than starting idle.
	shutdownSchedules, err := statedb.NewShutdownSchedules(stateStore)
	if err != nil {
		t.Fatalf("open shutdown schedule store: %v", err)
	}

	// srv is assigned below and read only from inside the hook, which cannot
	// fire before a frontend connection exists — i.e. long after the assignment.
	var srv *server.Server
	requestShutdown := func(stopShims bool) {
		srv.ShutdownAll(stopShims)
		b.executed <- stopShims
	}

	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		Resumes:           &server.ConversationResolver{Reg: reg, Logf: t.Logf},
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Prompts:           controller,
		Turns:             controller,
		Health:            controller,
		Lifecycle:         &server.WorkspaceOpener{Reg: reg, Ensurer: controller, Logf: t.Logf},
		SessionDeaths:     server.RegistrySessionDeaths{Reg: reg},
		Resyncer:          controller,
		Catalogs:          controller,
		SessionCommands:   binding,
		WorkspaceCreation: newEmptyWorkspaceCreation(),
		RequestShutdown:   requestShutdown,
		// The scheduled-shutdown drain lease: its durable record and the fleet
		// it derives its holds from (the controller satisfies DrainHoldSource),
		// exactly as main.go wires them. Without these the daemon nacks every
		// ScheduleShutdownCmd as unsupported.
		ShutdownSchedules: shutdownSchedules,
		DrainHolds:        controller,
		MergeLease:        mergeLease,
		MergeQueue:        mergeQueue,
		MergeGeometry:     geometryStore,
		Logf:              t.Logf,
		LogVerbosef:       t.Logf,
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	forwarder.SetTarget(agentShim.Server)
	// A schedule this boot's PREDECESSOR took and did not finish draining is
	// re-taken here, before any client connects — the durability half of the
	// lease contract, done exactly once at boot, as main.go does it.
	if agentShim.ShutdownScheduler != nil {
		if rerr := agentShim.ShutdownScheduler.Restore(); rerr != nil {
			t.Fatalf("restore the scheduled-shutdown drain lease: %v", rerr)
		}
	}

	srv = server.New(server.Config{
		DaemonVersion: "0.1.0-e2e-drain",
		Registry:      reg,
		ModelCatalogs: modelCatalogs,
		Controller:    controller,
		SSM:           ssmMgr,
		Frontend:      agentShim.Server,
		Logf:          t.Logf,
	})
	binding.SetTarget(srv)
	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	b.ts = httptest.NewServer(mux)

	// Teardown in reverse construction order, so nothing writes to a closed
	// database on its way down.
	b.stop = func() {
		b.ts.Close()
		_ = agentShim.Close()
		controller.Close()
		_ = shimListener.Close()
		_ = targets.Close()
		_ = ssmMgr.Close()
		_ = stateStore.Close()
	}
	t.Cleanup(b.bounce)
	return b
}

// bounce shuts this daemon down. Idempotent, so a test may bounce explicitly
// and still leave the cleanup registered.
func (b *shutdownBoot) bounce() { b.stopOnce.Do(b.stop) }

// harness adapts this boot to the shape e2e_test.go's session helpers take.
// createSession, dial and liveSession read nothing but the httptest server, so
// this is an adapter rather than a second harness.
func (b *shutdownBoot) harness() *e2eHarness { return &e2eHarness{ts: b.ts} }

// dialFrontend opens the UNFILTERED /frontend socket — the surface the
// daemon-global schedule commands and the ShutdownScheduleView broadcast ride.
func (b *shutdownBoot) dialFrontend(t *testing.T) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(b.ts.URL, "http") + "/frontend"
	conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("dial /frontend: %v", err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn
}

// shimFor returns the tracked shim process this boot spawned for workspace.
func (b *shutdownBoot) shimFor(t *testing.T, workspace string) *trackedShim {
	t.Helper()
	b.shimsMu.Lock()
	defer b.shimsMu.Unlock()
	for _, s := range b.shims {
		if s.workspace == workspace {
			return s
		}
	}
	t.Fatalf("this daemon spawned no shim for workspace %s", workspace)
	return nil
}

// --- frame accessors ---------------------------------------------------------

// scheduleView returns the ShutdownScheduleView a frame carries (FrontendFrame
// arm 19), or nil when the frame is something else.
func scheduleView(frame *frontendv1.FrontendFrame) *frontendv1.ShutdownScheduleView {
	s, ok := frame.GetFrame().(*frontendv1.FrontendFrame_ShutdownSchedule)
	if !ok {
		return nil
	}
	return s.ShutdownSchedule
}

// queueViewFor returns the QueueView a frame carries for workspace, or nil.
func queueViewFor(frame *frontendv1.FrontendFrame, workspace string) *frontendv1.QueueView {
	q, ok := frame.GetFrame().(*frontendv1.FrontendFrame_Queue)
	if !ok || q.Queue.GetWorkspace() != workspace {
		return nil
	}
	return q.Queue
}

// holdFor returns the drain's hold on workspace, or nil when the drain is not
// waiting on that workspace.
func holdFor(draining *frontendv1.ShutdownScheduleDraining, workspace string) *frontendv1.ShutdownHold {
	for _, hold := range draining.GetHolds() {
		if hold.GetWorkspace() == workspace {
			return hold
		}
	}
	return nil
}

// describeHolds renders a drain's holds for a failure message, so a test that
// wanted a particular hold says what the daemon was actually waiting on.
func describeHolds(draining *frontendv1.ShutdownScheduleDraining) string {
	if len(draining.GetHolds()) == 0 {
		return "(no holds at all)"
	}
	parts := make([]string, 0, len(draining.GetHolds()))
	for _, hold := range draining.GetHolds() {
		parts = append(parts, fmt.Sprintf("{ws=%s session=%s turn=%v tasks=%v}",
			hold.GetWorkspace(), hold.GetSessionId(), hold.GetTurn(), hold.GetTasks()))
	}
	return strings.Join(parts, " ")
}

// --- commands ----------------------------------------------------------------

// sendSchedule writes a ScheduleShutdownCmd (FrontendCommand arm 26).
func sendSchedule(t *testing.T, conn *websocket.Conn, requestID string, stopShims bool, cause string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"scheduleShutdown":{"stopShims":%v,"cause":%q}}`,
		requestID, stopShims, cause))
}

// sendCancelSchedule writes a CancelScheduledShutdownCmd (arm 27).
func sendCancelSchedule(t *testing.T, conn *websocket.Conn, requestID, scheduleID string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"cancelScheduledShutdown":{"scheduleId":%q}}`,
		requestID, scheduleID))
}

// --- waiting -----------------------------------------------------------------

// awaitIdleSchedule reads conn until a ShutdownScheduleView carrying the IDLE
// arm arrives — the representable "no lease" the contract requires a cancel or
// a completed drain to broadcast.
func awaitIdleSchedule(t *testing.T, conn *websocket.Conn, what string) {
	t.Helper()
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		what: func(frame *frontendv1.FrontendFrame) bool {
			view := scheduleView(frame)
			return view != nil && view.GetIdle() != nil
		},
	})
}

// awaitHeldEntry reads conn until workspace's QueueView carries an entry whose
// shutdown_hold is set, and returns that entry.
func awaitHeldEntry(t *testing.T, conn *websocket.Conn, workspace, what string) *frontendv1.QueueEntry {
	t.Helper()
	var held *frontendv1.QueueEntry
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		what: func(frame *frontendv1.FrontendFrame) bool {
			for _, entry := range queueViewFor(frame, workspace).GetEntries() {
				if entry.GetShutdownHold() != nil {
					held = entry
					return true
				}
			}
			return false
		},
	})
	return held
}

// connectSnapshot dials a FRESH /frontend connection and returns its connect
// StateSnapshot — the resync a client joining mid-drain receives.
func connectSnapshot(t *testing.T, b *shutdownBoot) *frontendv1.StateSnapshot {
	t.Helper()
	conn := b.dialFrontend(t)
	frame := readFrame(t, conn)
	snap := frame.GetSnapshot()
	if snap == nil {
		t.Fatalf("first /frontend frame = %T, want a StateSnapshot", frame.GetFrame())
	}
	return snap
}

// awaitExecutedShutdown reports the stop_shims of the shutdown this daemon
// executed, failing loudly if none is executed before the deadline.
func awaitExecutedShutdown(t *testing.T, b *shutdownBoot) bool {
	t.Helper()
	select {
	case stopShims := <-b.executed:
		return stopShims
	case <-time.After(frameTimeout):
		t.Fatal("the daemon never executed the scheduled shutdown: the drain's last hold cleared and nothing bounced")
		return false
	}
}
