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
	"context"
	"database/sql"
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
	"claude-repld/internal/keepalive"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shim"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/storehistory"
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
	// shimLog is the world's tap on the stderr of every shim its daemons spawn.
	// A bounce test that must destroy a daemon AT a particular point in the
	// shim's own processing has no other vantage: the daemon-side ack for the
	// command it is racing does not exist yet by construction.
	shimLog *shimLogTap
}

// shimLogTap forwards shim stderr to the test log AND lets a test wait for a
// line the shim emits.
//
// WHY A TEST NEEDS THIS. A frontend command is written and dispatched
// ASYNCHRONOUSLY: `writeCmd` returns as soon as the websocket write completes,
// long before the daemon has read the frame, let alone forwarded it to the
// shim. A test that writes a command and immediately destroys the daemon is
// therefore not arranging "the daemon died with the request in flight" — it is
// racing the daemon's read loop, and when it loses, the command is nacked
// ("session-controller: manager closed", "no live session for workspace") and
// the scenario under test never happened at all. The shim's own record of
// having RECEIVED the work is the only evidence available before the bounce,
// because the daemon-side ack for that same command is precisely what the test
// is arranging to lose.
//
// It is a rendezvous with a line the shim writes synchronously on the path
// being arranged, not a delay: nothing here waits out a duration, and a line
// that never comes fails at the suite's frame budget with a named reason.
type shimLogTap struct {
	mu      sync.Mutex
	t       *testing.T
	done    bool
	seen    []string
	waiters map[chan struct{}]string
}

func newShimLogTap(t *testing.T) *shimLogTap {
	return &shimLogTap{t: t, waiters: make(map[chan struct{}]string)}
}

// line records one shim stderr line and releases every waiter it satisfies.
func (tap *shimLogTap) line(format string, args ...any) {
	text := fmt.Sprintf(format, args...)
	tap.mu.Lock()
	defer tap.mu.Unlock()
	if tap.done {
		return
	}
	tap.t.Logf("%s", text)
	tap.seen = append(tap.seen, text)
	for ch, want := range tap.waiters {
		if strings.Contains(text, want) {
			close(ch)
			delete(tap.waiters, ch)
		}
	}
}

// finish stops the tap, for the same reason e2eSpawnLog.finish exists: the
// stderr scanner outlives the test function and t.Logf then panics.
func (tap *shimLogTap) finish() {
	tap.mu.Lock()
	defer tap.mu.Unlock()
	tap.done = true
	for ch := range tap.waiters {
		close(ch)
		delete(tap.waiters, ch)
	}
}

// await blocks until some shim has emitted a line containing want, bounded by
// the suite's frame budget. A line already seen satisfies it immediately, so
// the caller cannot lose the very event it is waiting for by arriving late.
func (tap *shimLogTap) await(t *testing.T, want, what string) {
	t.Helper()
	ch := make(chan struct{})
	tap.mu.Lock()
	for _, text := range tap.seen {
		if strings.Contains(text, want) {
			tap.mu.Unlock()
			return
		}
	}
	tap.waiters[ch] = want
	tap.mu.Unlock()
	select {
	case <-ch:
	case <-time.After(frameTimeout):
		tap.mu.Lock()
		delete(tap.waiters, ch)
		tap.mu.Unlock()
		t.Fatalf("no shim ever logged a line containing %q before the deadline: %s", want, what)
	}
}

// awaitShimLog is the world-scoped form of the tap's wait.
func (w *shutdownWorld) awaitShimLog(t *testing.T, want, what string) {
	t.Helper()
	w.shimLog.await(t, want, what)
}

// tapShimLogger feeds one shim's stderr into the world's tap.
type tapShimLogger struct{ tap *shimLogTap }

func (l tapShimLogger) Log(format string, args ...any) { l.tap.line(format, args...) }

func (l tapShimLogger) LogVerbose(format string, args ...any) { l.tap.line(format, args...) }

func (l tapShimLogger) LogLifecycle(format string, args ...any) { l.tap.line(format, args...) }

func newShutdownWorld(t *testing.T, options ...worldOption) *shutdownWorld {
	t.Helper()
	var tuning worldTuning
	for _, opt := range options {
		opt(&tuning)
	}
	// Registered before anything else the world owns, so its LIFO cleanup runs
	// LAST: the shim stderr scanners keep producing lines throughout every
	// boot's teardown, and the tap must still be accepting them then.
	tap := newShimLogTap(t)
	t.Cleanup(tap.finish)
	node := nodePath(t)
	// The BUNDLE'S OWN build identity, baked in by build.mjs exactly as
	// bin/build-frontend.sh does it. Empty is the ordinary case (no identity,
	// so no staleness question); a world that bakes one is arranging a shim
	// that can OUTLIVE A DEPLOY, which is what the bounce-resilience suite's
	// stale-refresh tests are about.
	script := buildShim(t, node, tuning.shimBuildSHA)
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
	return &shutdownWorld{node: node, script: script, shimSock: shimSock, storeSock: storeSock, stateDir: sockDir, shimLog: tap}
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

	// t is the test this daemon was booted by, and the only sink its records
	// go to. It is held so `logf` can be handed to the daemon's every logging
	// seam and still report through the test's own output.
	t *testing.T

	// logMu guards watchers, which are the test-side rendezvous points on this
	// daemon's OWN records (watchLog), and retired, which is set once the test
	// this daemon belongs to is finished with it (see logf).
	logMu    sync.Mutex
	watchers []*logWatch
	retired  bool

	// stateDB is the durable state database THIS boot opened over the world's
	// state directory — the same file every other boot opens. It is carried so
	// harness() can hand it to the shared e2eHarness helpers that read the
	// daemon's durable ledgers (durableTurnAccountings and friends): a fact the
	// daemon persisted is read back through its production statedb owner rather
	// than inferred from a frame.
	stateDB *sql.DB

	// executed carries the stop_shims of every shutdown this daemon EXECUTED,
	// published only after server.ShutdownAll has returned — so a test that has
	// received from it knows the teardown already happened and may assert on
	// shim liveness without racing it.
	executed chan bool

	shimsMu sync.Mutex
	shims   []*trackedShim

	// recheck fires the boot sweeper's one re-check pass. It is INJECTED for
	// the same reason bootsweep.go injects it: the pass must be an event the
	// test drives, never a timer the test waits out.
	recheck chan time.Time

	// shimConnected reports whether a session's shim has a PARKED CONNECTION on
	// this boot's listener — a shim that outlived the previous daemon and has
	// redialled this one, but has not been claimed by anything yet. It is the
	// boot sweeper's own evidence (server.BootSweeper.Connected), so a test can
	// fire the re-check on the observed fact the pass exists to act on rather
	// than on where it happens to sit in the test.
	shimConnected func(sessionID string) (bool, error)

	// hostVerdicts records the boot-sweep verdicts routed to the HOST half. The
	// frontend half is asserted through the socket like every other pushed
	// fact; this is the only place the host-action half is observable in a
	// harness that runs no workspace-create assembly.
	hostVerdicts *recordingBootSweepHost

	// sweepIdle fires this boot's IDLE SWEEPER (server.Config.IdleSweepTicks),
	// and clock is the wall clock that sweep measures elapsed idleness with.
	// Both are nil/zero unless the boot was given withBootIdleSweeper: a test
	// that never asks for one cannot accidentally hibernate its own session.
	sweepIdle chan<- time.Time
	clock     *testClock

	stop     func()
	stopOnce sync.Once
}

// bootTuning is the small set of knobs one boot of the daemon may bend.
type bootTuning struct {
	idleSweeper bool
	idleTimeout time.Duration
	// shimBuildSHA is what THIS boot believes the current bundle to be
	// (sessioncontroller.Config.ShimBuildSHA). Empty is the ordinary case and
	// makes every staleness comparison unresolvable, which is never a
	// mismatch. A boot that sets it differently from the world's baked
	// identity is a daemon that came up after a deploy the surviving shim
	// missed.
	shimBuildSHA string
}

type bootOption func(*bootTuning)

// withBootIdleSweeper gives this boot a test-driven idle sweeper on a
// test-movable clock, with `cutoff` as the sweeper's elapsed-idle gate.
//
// It exists for the durability half of a TIME-SINCE policy: whether a
// hibernated session stays out of the keep-alive loop, and whether a ping in
// flight when the daemon died is submitted twice, are questions only a daemon
// that comes back up can answer — and neither can be asked without driving the
// sweep and moving the clock on the SUCCESSOR.
func withBootIdleSweeper(cutoff time.Duration) bootOption {
	return func(o *bootTuning) { o.idleSweeper, o.idleTimeout = true, cutoff }
}

// sweep fires this boot's boot-sweeper re-check pass. The channel is
// buffered(1), so the send is safe even when the sweeper is not waiting on it.
func (b *shutdownBoot) sweepRecheck() { b.recheck <- time.Now() }

// recordingBootSweepHost stands in for the workspace-create manager's retained
// host-action envelope. It is a recorder rather than a no-op so a test can
// assert the host half was reached at all, and so a routing failure surfaces as
// a missing record instead of as nothing.
type recordingBootSweepHost struct {
	mu    sync.Mutex
	notes []bootSweepHostNote
}

type bootSweepHostNote struct{ workspace, sessionID, verdict, reason string }

func (r *recordingBootSweepHost) SurfaceBootSweepVerdict(_ context.Context, workspace, sessionID, verdict, reason string) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.notes = append(r.notes, bootSweepHostNote{workspace, sessionID, verdict, reason})
	return nil
}

// bootSweepHostNotes returns a copy of what the host half received.
func (r *recordingBootSweepHost) bootSweepHostNotes() []bootSweepHostNote {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]bootSweepHostNote(nil), r.notes...)
}

// sweepRecheckWhenParked fires the re-check pass only once the session's shim
// is OBSERVED parked on this boot's listener.
//
// The gate is the point. The re-check exists to claim shims that redialled
// after the first pass ran, so firing it before the redial lands is firing it at
// nothing — and a test that "worked" that way was relying on the redial
// happening to beat the send, which is a race, not a sequence. Waiting on the
// listener's own answer is the same evidence the sweeper acts on.
func (b *shutdownBoot) sweepRecheckWhenParked(t *testing.T, sessionID string) {
	t.Helper()
	deadline := time.Now().Add(20 * time.Second)
	for time.Now().Before(deadline) {
		parked, err := b.shimConnected(sessionID)
		if err != nil {
			t.Fatalf("parked-connection probe for session %s: %v", sessionID, err)
		}
		if parked {
			b.sweepRecheck()
			return
		}
		time.Sleep(5 * time.Millisecond)
	}
	t.Fatalf("session %s never parked a connection on the successor's listener, so the boot sweep's re-check would have had nothing to claim", sessionID)
}

// boot stands a complete daemon up over the world's durable state. Calling it
// twice (with a bounce in between) is the durability scenario.
func (w *shutdownWorld) boot(t *testing.T, options ...bootOption) *shutdownBoot {
	t.Helper()
	var tuning bootTuning
	for _, opt := range options {
		opt(&tuning)
	}
	b := &shutdownBoot{t: t, world: w, executed: make(chan bool, 4), recheck: make(chan time.Time, 1)}
	// REGISTERED FIRST SO IT RUNS LAST (cleanups are LIFO): every other cleanup
	// this boot adds — the teardown above all — still reports through the test,
	// and only records emitted after all of them are treated as late.
	t.Cleanup(b.retireLogging)

	stateStore, err := statedb.Open(filepath.Join(w.stateDir, "state.db"))
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	b.stateDB = stateStore
	reg := registry.OpenWith(registry.Options{DB: stateStore, Logf: b.logf})
	ssmMgr, err := ssm.Open(ssm.Options{
		DB:       stateStore,
		Resolver: server.NewRegistryResolver(reg),
		Logf:     b.logf,
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}

	forwarder := &server.PushForwarder{Logf: b.logf}
	shimListener := shimlisten.New(b.logf)
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

	// THE SAME SPAWN PROCEDURE THE DAEMON RUNS (server.NewUDSSpawner), including
	// its one-reaper-per-process rule: the exit it publishes through Reaped is
	// what this harness's cleanup waits on, rather than a second cmd.Wait.
	spawnLog := &e2eSpawnLog{t: t}
	t.Cleanup(spawnLog.finish)
	udsSpawn, err := server.NewUDSSpawner(server.UDSSpawnConfig{
		Targets:    targets,
		Node:       w.node,
		Script:     w.script,
		ShimSocket: w.shimSock,
		ForceFake:  true,
		ExtraArgv:  []string{"--store-socket", w.storeSock},
		Logger:     func(dlog.Workspace, string) shim.Logger { return tapShimLogger{tap: w.shimLog} },
		Event:      spawnLog.event,
		Spawned: func(s server.SpawnedShim) {
			tracked := &trackedShim{workspace: s.Workspace.Directory, proc: s.Proc, exited: make(chan struct{})}
			b.shimsMu.Lock()
			b.shims = append(b.shims, tracked)
			b.shimsMu.Unlock()
			t.Cleanup(func() {
				_ = s.Proc.Terminate(shim.Stop{Initiator: "e2e_harness_cleanup", Reason: "test teardown"})
				<-tracked.exited
			})
		},
		Reaped: func(s server.SpawnedShim, _ error) {
			b.shimsMu.Lock()
			defer b.shimsMu.Unlock()
			for _, tracked := range b.shims {
				if tracked.proc == s.Proc {
					close(tracked.exited)
					return
				}
			}
		},
	})
	if err != nil {
		t.Fatalf("build the UDS spawner: %v", err)
	}

	seqStore := server.NewRegistrySeqStore(reg, b.logf)
	modelCatalogs := server.NewSessionModelCatalogs()
	registrar := &server.RegistryRegistrar{Reg: reg, Logf: b.logf, ModelCatalogs: modelCatalogs}
	progressMgr := progress.New(progress.Options{Logf: b.logf})
	// The drain lease's durable record, opened over the SAME state store the
	// world owns — so a bounce reads back the schedule its predecessor wrote
	// rather than starting idle.
	//
	// It is opened BEFORE the controller because the controller needs it too:
	// the parking ledger behind a drain-held prompt is what makes the THIRD
	// exit — delivery after the bounce — durable, and main.go wires it in the
	// same place for the same reason.
	shutdownSchedules, err := statedb.NewShutdownSchedules(stateStore)
	if err != nil {
		t.Fatalf("open shutdown schedule store: %v", err)
	}
	keepAliveWindows, err := statedb.NewKeepAliveWindows(stateStore)
	if err != nil {
		t.Fatalf("open keep-alive window store: %v", err)
	}
	// THE DURABLE ACCOUNTING LEDGER main.go wires, not a per-boot in-memory
	// stand-in. A turn's accounting is the evidence a drain test reads back
	// AFTER the daemon that produced it is gone, so the ledger has to be the one
	// that lives in the world's state file rather than one that dies with the
	// process being bounced.
	turnAccountings, err := statedb.NewTurnAccountings(stateStore)
	if err != nil {
		t.Fatalf("open turn accounting ledger: %v", err)
	}
	kaCfg, err := keepalive.FromEnv()
	if err != nil {
		t.Fatalf("keep-alive config: %v", err)
	}
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           fakeShimSpawner(reg, shimListener, udsSpawn, b.logf),
		Locator:           &server.SessionLocator{Reg: reg},
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		TurnAccountings:   turnAccountings,
		// THE DURABLE ROUTE TO THE CONVERSATION, wired exactly as main.go wires
		// it and keyed by the vendor uuid the store files the seq space under.
		//
		// A bounce world is the one place where a daemon routinely faces a
		// workspace whose history exists ONLY in the store: the shim outlived
		// the process that was reading it, and the successor's frontends resync
		// against a workspace nothing has re-wired yet. Without this the
		// successor refuses every such resync ("no durable history source is
		// wired") and the handshake reconciliation has no way to tell a turn
		// that FINISHED during the gap from one the restart cut — both of which
		// are production capabilities, not test conveniences, so leaving it
		// unset made this world a strictly weaker daemon than the shipped one.
		DurableHistory: &storehistory.Reader{
			Socket: w.storeSock,
			Vendor: func(sessionID string) (string, bool) {
				rec, ok := reg.Get(sessionID)
				if !ok {
					return "", false
				}
				return rec.ClaudeSessionID, rec.ClaudeSessionID != ""
			},
			// The suite's short drain window: the production default waits
			// five seconds of quiet, which is the whole frame budget every
			// wait in this suite is bounded by.
			Idle: durableReplayIdle,
			Logf: b.logf,
		},
		Hibernations:     registrar,
		VendorSessions:   registrar,
		KeepAliveWindows: server.KeepAliveWindowStore{Windows: keepAliveWindows},
		KeepAlive:        kaCfg,
		VendorSessionOf: func(sessionID string) (string, bool) {
			rec, ok := reg.Get(sessionID)
			if !ok || rec.ClaudeSessionID == "" {
				return "", false
			}
			return rec.ClaudeSessionID, true
		},
		ShutdownHolds:   shutdownSchedules,
		PermissionModes: server.NewRegistryModeStore(reg),
		Registrar:       registrar,
		ModelCatalogs:   registrar,
		DaemonVersion:   "0.1.0-e2e-drain",
		ProtocolVersion: "1",
		ShimBuildSHA:    func() string { return tuning.shimBuildSHA },
		Logf:            b.logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}

	binding := &server.SessionCommandBinding{Logf: b.logf}
	// Beside the state store, not in a per-boot temp dir: a bounce must find
	// what the previous boot published.
	mergeQueue, err := merge.NewFileQueue(filepath.Join(w.stateDir, "merge-queue"), b.logf)
	if err != nil {
		t.Fatalf("open merge queue: %v", err)
	}
	mergeLease, err := ssm.NewMergeLease(ssm.MergeLeaseConfig{
		Manager: ssmMgr, Queue: mergeQueue, Interrupter: controller,
	})
	if err != nil {
		t.Fatalf("build merge lease: %v", err)
	}
	geometryStore, err := geometry.Open(stateStore, b.logf)
	if err != nil {
		t.Fatalf("open geometry: %v", err)
	}
	// THE SHUTDOWN REQUEST IS A CHANNEL, exactly as main.go's is, and for the
	// reason main.go's is: the hook is called from wherever a drain completes —
	// including from INSIDE Restore, before `srv` has been assigned — so a
	// closure that dereferences srv races its own assignment. A buffered(1)
	// channel plus a sync.Once sender carries the one stop_shims decision, and
	// the consumer that acts on it does not exist until srv does.
	// The request carries the CAUSE with the decision, as main.go's does: the
	// drain execution and an ordinary shutdown command are different events
	// reaching the same teardown.
	type harnessShutdownRequest struct {
		stopShims bool
		cause     sessioncontroller.StopCause
	}
	shutdownReq := make(chan harnessShutdownRequest, 1)
	var shutdownOnce sync.Once
	requestShutdown := func(stopShims bool, cause sessioncontroller.StopCause) {
		shutdownOnce.Do(func() {
			shutdownReq <- harnessShutdownRequest{stopShims: stopShims, cause: cause}
			close(shutdownReq)
		})
	}

	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		// The boot orphan sweep scans THIS directory and no other: a test that
		// let it resolve the process temp dir deleted the live daemon's rebase
		// worktrees mid-merge.
		RebaseRoot:    t.TempDir(),
		Resumes:       &server.ConversationResolver{Reg: reg, Logf: b.logf},
		SSM:           ssmMgr,
		Progress:      progressMgr,
		Prompts:       controller,
		Turns:         controller,
		Health:        controller,
		Hibernations:  controller,
		Restarts:      controller,
		Lifecycle:     &server.WorkspaceOpener{Reg: reg, Ensurer: controller, Failures: controller, Logf: b.logf},
		SessionDeaths: server.RegistrySessionDeaths{Reg: reg},
		Resyncer:      controller,
		Catalogs:      controller,
		// The registry-backed SessionView source main.go wires, so a connect
		// snapshot taken mid-drain lists the registry's sessions rather than
		// nothing at all.
		Sessions: server.RegistrySessions{Reg: reg, Controller: controller, ModelCatalogs: modelCatalogs, Logf: b.logf},
		// The queue backend, and therefore the force and cancel EXITS a parked
		// prompt has. Without it the daemon refuses to construct at all: a lease
		// that can park prompts it cannot release is not shippable.
		Queues:            controller,
		SessionCommands:   binding,
		WorkspaceCreation: newEmptyWorkspaceCreation(),
		RequestShutdown:   requestShutdown,
		// The scheduled-shutdown drain lease: its durable record, the fleet it
		// derives its holds from (the controller satisfies DrainHoldSource), and
		// the boot evidence a RESTORED lease seeds its unresolved set from —
		// exactly as main.go wires them. Without these the daemon nacks every
		// ScheduleShutdownCmd as unsupported.
		ShutdownSchedules: shutdownSchedules,
		DrainHolds:        controller,
		DrainEvidence: server.RegistryDrainEvidence{
			Reg:       reg,
			Connected: shimListener.Connected,
			Held:      sessionlock.Held,
		},
		MergeLease:    mergeLease,
		MergeQueue:    mergeQueue,
		MergeGeometry: geometryStore,
		Logf:          b.logf,
		LogVerbosef:   b.logf,
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
	// The prompts that lease PARKED are materialized immediately after it and
	// before any client connects, exactly as main.go does it: their sessions
	// have not wired yet, and until this runs the connect snapshot carries no
	// QueueView for them at all.
	if _, merr := controller.MaterializeShutdownHolds(); merr != nil {
		t.Fatalf("materialize the drain-lease parked-prompt ledger: %v", merr)
	}
	// The listener is kept so a test can observe WHEN a surviving shim has
	// redialled this boot, instead of inferring it from test phase order.
	b.shimConnected = shimListener.Connected

	var sweepTicks chan time.Time
	var nowFn func() time.Time
	if tuning.idleSweeper {
		sweepTicks = make(chan time.Time)
		b.sweepIdle = sweepTicks
		b.clock = newTestClock()
		nowFn = b.clock.now
	}
	srv := server.New(server.Config{
		DaemonVersion:  "0.1.0-e2e-drain",
		Registry:       reg,
		ModelCatalogs:  modelCatalogs,
		Controller:     controller,
		AgentShim:      agentShim,
		IdleSweepTicks: sweepTicks,
		IdleTimeout:    tuning.idleTimeout,
		KeepAlive:      kaCfg,
		Now:            nowFn,
		Logf:           b.logf,
	})
	binding.SetTarget(srv)
	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	b.ts = httptest.NewServer(mux)

	// THE SHUTDOWN CONSUMER, started only now that srv exists. A drain that
	// completed inside Restore has already parked its decision on the buffered
	// channel; this picks it up and runs the real teardown, and records
	// stop_shims only AFTER ShutdownAll returns, so a test that has read from
	// `executed` may assert on shim liveness without racing the teardown.
	teardown := make(chan struct{})
	go func() {
		select {
		case req, ok := <-shutdownReq:
			if !ok {
				return
			}
			srv.ShutdownAll(req.stopShims, req.cause)
			b.executed <- req.stopShims
		case <-teardown:
		}
	}()

	// BOOT RECONCILIATION, as main.go runs it: the shims that outlived the
	// previous daemon are redialling this one's listener, and without this pass
	// nothing ever claims them. The re-check is an INJECTED channel, so a test
	// fires the second pass as an event instead of waiting out a timer.
	sweepCtx, cancelSweep := context.WithCancel(context.Background())
	// AND ITS VERDICTS GO SOMEWHERE, as main.go routes them: the pushed
	// workspace state through the SSM's controller-less entry point, and the
	// host through the retained-action envelope. The host half is a recording
	// stub rather than a whole workspace-create assembly — this harness has no
	// creation subsystem, and what the e2e reads is the frontend surface.
	b.hostVerdicts = &recordingBootSweepHost{}
	go (&server.BootSweeper{
		Reg:       reg,
		Connected: shimListener.Connected,
		Held:      sessionlock.Held,
		Ensurer:   controller,
		Logf:      b.logf,
		Recheck:   b.recheck,
		Unwired: (&server.BootSweepVerdictRouter{
			State: ssmMgr,
			Host:  b.hostVerdicts,
			Logf:  b.logf,
		}).Route,
	}).Run(sweepCtx)

	// Teardown in reverse construction order, so nothing writes to a closed
	// database on its way down.
	b.stop = func() {
		cancelSweep()
		close(teardown)
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

// --- rendezvous on this daemon's own records ---------------------------------

// logWatch is one armed rendezvous: the record fragment it is waiting for, and
// the channel closed by the emitter that writes it.
type logWatch struct {
	substring string
	hit       chan struct{}
}

// logf is the sink every logging seam of this daemon is wired to. It reports
// through the test AND releases any watcher whose fragment the record carries.
//
// A RETIRED BOOT STILL REPORTS, IT JUST STOPS REPORTING THROUGH THE TEST. A
// daemon's goroutines can outlive the test that booted it — the boot sweeper's
// per-session reconcile is already running when its context is cancelled — and
// testing.T PANICS on a Logf after its test has completed, which turns a
// late record into a dead package rather than a line. Past retirement the line
// goes to stderr, named as late, so nothing is swallowed.
func (b *shutdownBoot) logf(format string, args ...any) {
	line := fmt.Sprintf(format, args...)
	b.logMu.Lock()
	defer b.logMu.Unlock()
	if b.retired {
		fmt.Fprintf(os.Stderr, "[boot retired after %s completed] %s\n", b.t.Name(), line)
	} else {
		b.t.Logf("%s", line)
	}
	kept := b.watchers[:0]
	for _, w := range b.watchers {
		if strings.Contains(line, w.substring) {
			close(w.hit)
			continue
		}
		kept = append(kept, w)
	}
	b.watchers = kept
}

// retireLogging closes this boot's records to the test's own sink. It is the
// LAST cleanup this boot runs, so everything the teardown itself emits is still
// reported normally.
func (b *shutdownBoot) retireLogging() {
	b.logMu.Lock()
	defer b.logMu.Unlock()
	b.retired = true
}

// watchLog arms a rendezvous on a record this daemon has NOT emitted yet, and
// must be called BEFORE the command that provokes it.
//
// WHY A DAEMON RECORD IS THE RIGHT RENDEZVOUS HERE. Some of what a bounce test
// must sequence against happens BETWEEN two processes and is reported by
// neither frame stream: the permission response leaving the daemon for the shim
// is the case this exists for. It is dispatched from a goroutine parked inside
// HandlePermission and is DROPPED when the connection's context is already
// cancelled, so a test that answers and bounces in the same breath silently
// arranges nothing at all. The daemon's own "sent permission response" record is
// the only statement that the answer left the process, and waiting on it is
// waiting on the fact rather than on a scheduling hope.
func (b *shutdownBoot) watchLog(substring string) <-chan struct{} {
	w := &logWatch{substring: substring, hit: make(chan struct{})}
	b.logMu.Lock()
	defer b.logMu.Unlock()
	b.watchers = append(b.watchers, w)
	return w.hit
}

// awaitLogged waits for an armed watchLog rendezvous, bounded by the suite's
// frame budget so a record that never comes is a named failure rather than a
// hang.
func awaitLogged(t *testing.T, hit <-chan struct{}, what string) {
	t.Helper()
	select {
	case <-hit:
	case <-time.After(frameTimeout):
		t.Fatalf("the daemon never emitted the record this step waits on: %s", what)
	}
}

// harness adapts this boot to the shape e2e_test.go's session helpers take.
// createSession, dial and liveSession read nothing but the httptest server, and
// the durable-ledger readers (durableTurnAccountings) read nothing but the state
// database, so this is an adapter rather than a second harness.
func (b *shutdownBoot) harness() *e2eHarness { return &e2eHarness{ts: b.ts, stateDB: b.stateDB} }

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
//
// THE LOOKUP GOES THROUGH THE SAME CANONICALIZER THE SPAWN DID. The spawn path
// records dlog.WorkspaceFromDirectory(...).Directory, and a test passes the raw
// t.TempDir() path it created the session with — which on macOS is /var/... while
// the canonical form is /private/var/.... Comparing those two derivations is a
// silent no-match dressed up as "this daemon spawned no shim", so the argument is
// put through the one canonicalizer rather than compared against it.
func (b *shutdownBoot) shimFor(t *testing.T, workspace string) *trackedShim {
	t.Helper()
	canonical, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		t.Fatalf("canonicalize workspace %s: %v", workspace, err)
	}
	b.shimsMu.Lock()
	defer b.shimsMu.Unlock()
	for _, s := range b.shims {
		if s.workspace == canonical.Directory {
			return s
		}
	}
	t.Fatalf("this daemon spawned no shim for workspace %s (canonical %s)", workspace, canonical.Directory)
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
				if entry.GetShutdown() != nil {
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
