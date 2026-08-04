// WHO ORDERED THIS SHIM STOPPED: the stop record's ATTRIBUTION, end to end.
//
// THE FAILURE THIS EXISTS FOR. shim.Stop makes attribution impossible to omit —
// every deliberate stop names an initiator and a reason — but the daemon's one
// funnel to it (server.ShimSpawner.StopShim) hands every caller the SAME
// constant. A merge teardown, an idle sweep, an explicit session delete and a
// scheduled drain's execution therefore all produce the identical stop record,
// so the one question the record exists to answer — why did this shim stop —
// can only be answered by reconstructing it from whatever surrounding lines
// happen to be nearby. That is the reconstruction the attribution was introduced
// to end.
//
// WHAT IS ASSERTED, AND WHAT IS DELIBERATELY NOT. The test drives two stops with
// genuinely different commanding causes and asserts their stop records DIFFER.
// It does not pin the specific initiator or reason strings: the vocabulary's
// members are the daemon's to name, and a test that pinned them would break on
// every rewording while still failing to state the property that matters —
// two different causes must not be one record.
//
// HOW THE RECORD IS OBSERVED. The stop record travels to the shim through the
// stop closure the spawn returned, which is where this file captures it. That is
// the same value the daemon's own stop line is rendered from, one hop earlier and
// structured rather than grepped, so an assertion on it is an assertion on the
// record itself rather than on a log format.
//
// WHY A SECOND BOOT FUNCTION. shutdownscheduleharness_test.go's boot builds its
// stop closure inline and keeps nothing, which is right for what it covers
// (whether the process died) and useless for what this covers (what it was told
// when it was asked to). The world — node runtime, shim bundle, sockets, state
// directory — is REUSED unchanged; only the daemon assembly is repeated, with a
// recording stop closure and a capturing logger.
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

// --- a daemon whose stop records are kept -------------------------------------

// stopCauseBoot is one daemon that REMEMBERS what every shim stop was attributed
// to. It embeds the ordinary boot so the frame helpers, the session helpers and
// the executed-shutdown wait all apply unchanged.
type stopCauseBoot struct {
	*shutdownBoot

	mu    sync.Mutex
	stops map[string][]server.ShimStop // canonical workspace dir -> stops, in order
	logs  []string
}

// record captures one stop attribution against the workspace whose shim it was
// aimed at.
func (b *stopCauseBoot) record(workspace string, by server.ShimStop) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.stops[workspace] = append(b.stops[workspace], by)
}

// logLine keeps the daemon's own log so a failure can quote what the daemon said
// about the stops it made, instead of reporting a mismatch with no context.
func (b *stopCauseBoot) logLine(format string, args ...any) {
	line := fmt.Sprintf(format, args...)
	b.mu.Lock()
	b.logs = append(b.logs, line)
	b.mu.Unlock()
}

// stopsFor returns the stop records aimed at workspace's shim.
//
// THE LOOKUP GOES THROUGH THE SAME CANONICALIZER THE SPAWN DID, for the reason
// shimFor states: a raw t.TempDir() path and the canonical form differ on macOS,
// and comparing the two derivations is a silent no-match dressed up as "nothing
// stopped this shim".
func (b *stopCauseBoot) stopsFor(t *testing.T, workspace string) []server.ShimStop {
	t.Helper()
	canonical, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		t.Fatalf("canonicalize workspace %s: %v", workspace, err)
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	out := make([]server.ShimStop, len(b.stops[canonical.Directory]))
	copy(out, b.stops[canonical.Directory])
	return out
}

// daemonLog renders everything the daemon said, for a failure message that would
// otherwise be two identical structs and no account of how they got that way.
func (b *stopCauseBoot) daemonLog(needle string) string {
	b.mu.Lock()
	defer b.mu.Unlock()
	var kept []string
	for _, line := range b.logs {
		if strings.Contains(line, needle) {
			kept = append(kept, line)
		}
	}
	if len(kept) == 0 {
		return fmt.Sprintf("(no daemon log line contains %q)", needle)
	}
	return "\n\t" + strings.Join(kept, "\n\t")
}

// describeStop renders one stop record for a failure message.
func describeStop(by server.ShimStop) string {
	return fmt.Sprintf("{initiator=%q reason=%q}", by.Initiator, by.Reason)
}

// bootRecordingStops stands a daemon up over the world exactly as boot does, with
// two differences and no others: the stop closure records what it is handed
// before passing it on, and the logger keeps its lines as well as reporting them.
func (w *shutdownWorld) bootRecordingStops(t *testing.T) *stopCauseBoot {
	t.Helper()
	sc := &stopCauseBoot{stops: map[string][]server.ShimStop{}}
	b := &shutdownBoot{world: w, executed: make(chan bool, 4), recheck: make(chan time.Time, 1)}
	sc.shutdownBoot = b
	logf := func(format string, args ...any) {
		sc.logLine(format, args...)
		t.Logf(format, args...)
	}

	stateStore, err := statedb.Open(filepath.Join(w.stateDir, "state.db"))
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	reg := registry.OpenWith(registry.Options{DB: stateStore, Logf: logf})
	ssmMgr, err := ssm.Open(ssm.Options{
		DB:       stateStore,
		Resolver: server.NewRegistryResolver(reg),
		Logf:     logf,
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}

	forwarder := &server.PushForwarder{Logf: logf}
	shimListener := shimlisten.New(logf)
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

	udsSpawn := func(sessionID string, opts server.CreateOpts) (server.ShimHandle, error) {
		workspace, err := dlog.WorkspaceFromDirectory(opts.CWD)
		if err != nil {
			return server.ShimHandle{}, err
		}
		shimTarget, err := targets.OpenWorkspaceRuntime(workspace, dlog.RuntimeShim)
		if err != nil {
			return server.ShimHandle{}, err
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
			return server.ShimHandle{}, spawnErr
		}
		tracked := &trackedShim{workspace: workspace.Directory, proc: proc, exited: make(chan struct{})}
		b.shimsMu.Lock()
		b.shims = append(b.shims, tracked)
		b.shimsMu.Unlock()
		// ONE reaper per process, exactly as boot's: cmd.Wait is not re-entrant,
		// so the exit is published as a closed channel every waiter reads.
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
		// THE CAPTURE, and the only behavioral difference from boot's closure:
		// the attribution is recorded against the workspace before it is passed
		// on, so the assertion reads the record the daemon composed rather than a
		// rendering of it.
		return server.ShimHandle{
			Reaped: tracked.exited,
			Stop: func(by server.ShimStop) error {
				sc.record(workspace.Directory, by)
				return proc.Terminate(by)
			},
		}, nil
	}

	seqStore := server.NewRegistrySeqStore(reg, logf)
	modelCatalogs := server.NewSessionModelCatalogs()
	registrar := &server.RegistryRegistrar{Reg: reg, Logf: logf, ModelCatalogs: modelCatalogs}
	progressMgr := progress.New(progress.Options{Logf: logf})
	shutdownSchedules, err := statedb.NewShutdownSchedules(stateStore)
	if err != nil {
		t.Fatalf("open shutdown schedule store: %v", err)
	}
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           server.NewShimSpawner(reg, shimListener.Connected, shimListener.Evict, udsSpawn, logf),
		Locator:           &server.SessionLocator{Reg: reg},
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		ShutdownHolds:     shutdownSchedules,
		PermissionModes:   server.NewRegistryModeStore(reg),
		Registrar:         registrar,
		ModelCatalogs:     registrar,
		DaemonVersion:     "0.1.0-e2e-stop-cause",
		ProtocolVersion:   "1",
		ShimBuildSHA:      func() string { return "" },
		Logf:              logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}

	binding := &server.SessionCommandBinding{Logf: logf}
	mergeQueue, err := merge.NewFileQueue(filepath.Join(w.stateDir, "merge-queue"), logf)
	if err != nil {
		t.Fatalf("open merge queue: %v", err)
	}
	mergeLease, err := ssm.NewMergeLease(ssm.MergeLeaseConfig{
		Manager: ssmMgr, Queue: mergeQueue, Interrupter: controller,
	})
	if err != nil {
		t.Fatalf("build merge lease: %v", err)
	}
	geometryStore, err := geometry.Open(stateStore, logf)
	if err != nil {
		t.Fatalf("open geometry: %v", err)
	}
	// The shutdown request is a channel for the reason boot's is: the hook is
	// called from wherever a drain completes, including from inside Restore,
	// before srv exists.
	type shutdownRequest struct {
		stopShims bool
		cause     sessioncontroller.StopCause
	}
	shutdownReq := make(chan shutdownRequest, 1)
	var shutdownOnce sync.Once
	requestShutdown := func(stopShims bool, cause sessioncontroller.StopCause) {
		shutdownOnce.Do(func() { shutdownReq <- shutdownRequest{stopShims: stopShims, cause: cause}; close(shutdownReq) })
	}

	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		Resumes:           &server.ConversationResolver{Reg: reg, Logf: logf},
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Prompts:           controller,
		Turns:             controller,
		Health:            controller,
		Lifecycle:         &server.WorkspaceOpener{Reg: reg, Ensurer: controller, Logf: logf},
		SessionDeaths:     server.RegistrySessionDeaths{Reg: reg},
		Resyncer:          controller,
		Catalogs:          controller,
		Sessions:          server.RegistrySessions{Reg: reg, Controller: controller, ModelCatalogs: modelCatalogs, Logf: logf},
		Queues:            controller,
		SessionCommands:   binding,
		WorkspaceCreation: newEmptyWorkspaceCreation(),
		RequestShutdown:   requestShutdown,
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
		Logf:          logf,
		LogVerbosef:   logf,
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	forwarder.SetTarget(agentShim.Server)
	if agentShim.ShutdownScheduler != nil {
		if rerr := agentShim.ShutdownScheduler.Restore(); rerr != nil {
			t.Fatalf("restore the scheduled-shutdown drain lease: %v", rerr)
		}
	}
	if _, merr := controller.MaterializeShutdownHolds(); merr != nil {
		t.Fatalf("materialize the drain-lease parked-prompt ledger: %v", merr)
	}
	b.shimConnected = shimListener.Connected

	srv := server.New(server.Config{
		DaemonVersion: "0.1.0-e2e-stop-cause",
		Registry:      reg,
		ModelCatalogs: modelCatalogs,
		Controller:    controller,
		SSM:           ssmMgr,
		Frontend:      agentShim.Server,
		Logf:          logf,
	})
	binding.SetTarget(srv)
	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	b.ts = httptest.NewServer(mux)

	// The shutdown consumer, started only now that srv exists, publishing
	// stop_shims only AFTER ShutdownAll returns — so a test that has read from
	// `executed` may assert on the stops it made without racing them.
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

	b.stop = func() {
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
	return sc
}

// --- the test -----------------------------------------------------------------

// TestE2ETwoDifferentlyCausedStopsCarryDistinctAttribution covers the closed
// stop-cause vocabulary at the only place it matters: the stop record itself.
//
// THE TWO CAUSES ARE GENUINELY DIFFERENT COMMANDS, not two spellings of one.
// A session DELETE is a user discarding one session and nothing else; a drain
// EXECUTION with stop_shims=true is the daemon replacing its whole shim bundle.
// They differ in scope, in who asked, and in what a reader should do about the
// record afterwards — so a log in which they are indistinguishable cannot answer
// "was this shim stopped because someone deleted the session, or because the
// deploy took the fleet down".
func TestE2ETwoDifferentlyCausedStopsCarryDistinctAttribution(t *testing.T) {
	// Arrange — two live workspaces, and a drain scheduled on shim-stopping
	// terms while A's turn holds it open.
	//
	// The tempdirs precede the world so cleanups (LIFO) tear the daemon and its
	// shims down before the directories are removed.
	cwdA, cwdB := t.TempDir(), t.TempDir()
	world := newShutdownWorld(t)
	boot := world.bootRecordingStops(t)
	h := boot.harness()
	_, connA, _, _ := liveSession(t, h, cwdA)
	sessionB, _, _, _ := liveSession(t, h, cwdB)
	holdTurnOpen(t, connA, cwdA, "r-hold-a", "sleep e2e-stop-cause")
	frontend := boot.dialFrontend(t)
	scheduleAndAwaitDraining(t, frontend, "r-sched", true /*stopShims*/, "deploy that replaced the shim bundle")

	// Act 1 — an explicit session delete stops B's shim. It is awaited to
	// completion before the drain is allowed to run, so the two stops are
	// sequenced by their acks rather than by hoping they do not overlap.
	writeCmd(t, frontend, fmt.Sprintf(`{"requestId":"r-delete","deleteSession":{"sessionId":%q}}`, sessionB))
	var deleteAck *frontendv1.CommandAck
	awaitAll(t, frontend, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the session delete": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-delete"); a != nil {
				deleteAck = a
				return true
			}
			return false
		},
	})
	if !deleteAck.GetOk() {
		t.Fatalf("deleteSession for %s nacked: %s — this test needs the delete's own stop to have happened", sessionB, deleteAck.GetError())
	}

	// Act 2 — clearing A's hold lets the drain execute, which stops A's shim on
	// the terms the schedule fixed.
	writeCmd(t, connA, `{"requestId":"r-interrupt","interrupt":{}}`)
	if got := awaitExecutedShutdown(t, boot.shutdownBoot); !got {
		t.Fatalf("the executed shutdown used stop_shims=%v, want true: without it the drain stops no shim and there is no drain-caused stop record to compare", got)
	}

	// Assert
	deleteStops := boot.stopsFor(t, cwdB)
	drainStops := boot.stopsFor(t, cwdA)
	// Fatal: the comparison below is meaningless without both records, and
	// "no stop was recorded at all" is a different failure worth saying plainly.
	if len(deleteStops) == 0 {
		t.Fatalf("the explicit delete of session %s (ws %s) produced no stop record at all; daemon log:%s",
			sessionB, cwdB, boot.daemonLog("StopShim"))
	}
	if len(drainStops) == 0 {
		t.Fatalf("the executed drain produced no stop record for ws %s although it ran with stop_shims=true; daemon log:%s",
			cwdA, boot.daemonLog("SHIM STOP"))
	}
	deleteStop, drainStop := deleteStops[0], drainStops[0]

	// Reported rather than fatal: whether each record is attributed at all and
	// whether the two are distinguishable are independent facts, and a run that
	// fails all three should say so once.
	if deleteStop.Initiator == "" || deleteStop.Reason == "" {
		t.Errorf("the delete's stop record is %s: a stop with a blank initiator or reason is a shim death the log cannot tell from a crash", describeStop(deleteStop))
	}
	if drainStop.Initiator == "" || drainStop.Reason == "" {
		t.Errorf("the drain's stop record is %s: a stop with a blank initiator or reason is a shim death the log cannot tell from a crash", describeStop(drainStop))
	}
	if deleteStop.Initiator == drainStop.Initiator && deleteStop.Reason == drainStop.Reason {
		t.Errorf("an explicit session delete and a scheduled drain's execution produced the SAME stop record %s: the commanding cause is exactly what the attribution exists to carry, so two different commands must not resolve to one initiator and one reason; daemon log:%s",
			describeStop(deleteStop), boot.daemonLog("SHIM STOP"))
	}
}
