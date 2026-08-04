// QUEUE DURABILITY ACROSS A DAEMON BOUNCE, end to end: a merge enqueued behind
// a parked head survives the daemon exiting, is reconstructed by
// merge.Coordinator.Drain on the next boot, and drains without anyone
// re-submitting it.
//
// WHY THIS FILE BOOTS ITS OWN DAEMON. The bounce IS the test, and newUDSHarness
// builds a daemon that lives exactly as long as the test does. So this file
// stands the same stack up twice over ONE durable state store — the second boot
// opening the very database the first wrote — which is the production shape:
// the state store outlives the daemon, and a daemon bounce is a new process
// opening it again.
//
// WHY THE BOUNCE MATTERS AT ALL. The daemon merges its own repository. A
// self-merge therefore bounces the daemon mid-queue as a matter of course, so a
// queue held only in memory would lose every entry behind the head precisely
// when the system is being used as designed (merge/contract.go Drain: "what
// lets a merge queue survive the daemon bounce that a self-merge of the daemon
// itself causes").
//
// WHY THERE ARE NO SESSIONS HERE. A queue entry and a cherry-pick need no shim;
// the LEASE does, and the lease is covered in mergelease_e2e_test.go. Leaving
// sessions out keeps the second boot about the one thing under test — whether
// the durable record was there to be read.
//
// Reuses mergequeue_e2e_test.go's fixtures / mergeWatch and e2e_test.go's
// stubs READ-ONLY.
package e2e

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/gorilla/websocket"

	"claude-repld/internal/dlog"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/geometry"
	"claude-repld/internal/workspace/merge"
)

// --- a bounceable daemon -----------------------------------------------------

// mergeBoot is ONE boot of the daemon stack over a caller-owned state store
// path. Two of them in sequence over the same path is a bounce.
type mergeBoot struct {
	ts *httptest.Server
	// geometry is this boot's handle on THE daemon's workspace -> merge-geometry
	// map. It lives in the same durable state store the bounce reopens, so a
	// record the first boot's test wrote is still the second boot's answer —
	// which is what makes the resume after the bounce resolvable at all.
	geometry  *geometry.Store
	stop      func()
	stopOnce  sync.Once
	stateFile string
}

// bootMergeDaemon stands the daemon up against stateFile, which may or may not
// already exist: an existing one is the SECOND boot, and reading it is the
// whole point.
//
// The spawner is deliberately hostile. Nothing here creates a session, so a
// spawn attempt would mean the stack took a path this test does not model, and
// discovering that as a loud error beats discovering it as a mystery timeout.
func bootMergeDaemon(t *testing.T, stateFile string) *mergeBoot {
	t.Helper()
	stateStore, err := statedb.Open(stateFile)
	if err != nil {
		t.Fatalf("open state store %s: %v", stateFile, err)
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
	// BOTH boots resolve the SAME isolated socket, through the production
	// function, because bounceStateDir set the override for the whole test: a
	// bounce that landed on a different socket would not be a bounce.
	shimSock, err := shimlisten.DefaultSocketPath()
	if err != nil {
		t.Fatalf("resolve the isolated shim socket: %v", err)
	}
	shimListener := shimlisten.New(t.Logf)
	if err := shimListener.Listen(shimSock); err != nil {
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
	noSpawn := func(string, server.CreateOpts) (server.ShimHandle, error) {
		return server.ShimHandle{}, errNoSpawnInBounceHarness
	}
	seqStore := server.NewRegistrySeqStore(reg, t.Logf)
	modelCatalogs := server.NewSessionModelCatalogs()
	registrar := &server.RegistryRegistrar{Reg: reg, Logf: t.Logf, ModelCatalogs: modelCatalogs}
	progressMgr := progress.New(progress.Options{Logf: t.Logf})
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           server.NewShimSpawner(reg, shimListener.Connected, shimListener.Evict, noSpawn, t.Logf),
		Locator:           &server.SessionLocator{Reg: reg},
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		TurnAccountings:   newTestTurnAccountingStore(),
		PermissionModes:   server.NewRegistryModeStore(reg),
		Registrar:         registrar,
		ModelCatalogs:     registrar,
		DaemonVersion:     "0.1.0-e2e-bounce",
		ProtocolVersion:   "1",
		ShimBuildSHA:      func() string { return "" },
		Logf:              t.Logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}

	binding := &server.SessionCommandBinding{Logf: t.Logf}
	// The queue lives BESIDE the state store, not in a per-boot temp dir: the
	// bounce this harness models must find the same durable entries the first
	// boot published, and a fresh directory per boot would make the queue's
	// survival unobservable.
	mergeQueue, err := merge.NewFileQueue(filepath.Join(filepath.Dir(stateFile), "merge-queue"), t.Logf)
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
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		Resumes:           &server.ConversationResolver{Reg: reg, Logf: t.Logf},
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Prompts:           controller,
		Turns:             controller,
		Health:            controller,
		Lifecycle:         stubLifecycle{},
		SessionDeaths:     server.RegistrySessionDeaths{Reg: reg},
		Resyncer:          controller,
		Catalogs:          controller,
		SessionCommands:   binding,
		WorkspaceCreation: newEmptyWorkspaceCreation(),
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

	srv := server.New(server.Config{
		DaemonVersion: "0.1.0-e2e-bounce",
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
	ts := httptest.NewServer(mux)

	boot := &mergeBoot{ts: ts, geometry: geometryStore, stateFile: stateFile}
	// Teardown in reverse construction order: stop accepting, then release the
	// listeners, then close the state store LAST so nothing writes to a closed
	// database on its way down.
	boot.stop = func() {
		ts.Close()
		_ = agentShim.Close()
		controller.Close()
		_ = shimListener.Close()
		_ = targets.Close()
		_ = ssmMgr.Close()
		_ = stateStore.Close()
	}
	t.Cleanup(boot.bounce)
	return boot
}

// bounce shuts this boot down. It is idempotent so the test can bounce
// explicitly and still leave the cleanup registered.
func (b *mergeBoot) bounce() { b.stopOnce.Do(b.stop) }

func (b *mergeBoot) dialFrontend(t *testing.T) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(b.ts.URL, "http") + "/frontend"
	conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("dial /frontend: %v", err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	return conn
}

type bounceHarnessError string

func (e bounceHarnessError) Error() string { return string(e) }

const errNoSpawnInBounceHarness = bounceHarnessError(
	"e2e: the merge-bounce harness never spawns a shim; a spawn here means the stack took an unmodeled path")

// bounceStateDir prepares the short-path HOME every boot resolves its sockets
// and locks under, and returns the state-store path the two boots SHARE.
func bounceStateDir(t *testing.T) string {
	t.Helper()
	// Not t.TempDir(): the test-name-derived path exceeds the 104-byte
	// sun_path limit, so bind(2) fails on macOS (e2e_test.go, same reason).
	sockDir, err := os.MkdirTemp("/tmp", "agent-repl-merge-bounce-")
	if err != nil {
		t.Fatalf("make short socket dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(sockDir) })
	t.Setenv("HOME", sockDir)
	isolatedShimSocket(t, sockDir)
	return filepath.Join(sockDir, "state.db")
}

// --- the test ----------------------------------------------------------------

// TestE2EAQueuedMergeSurvivesADaemonBounce is the durability assertion: the
// entry behind the parked head is submitted to the FIRST daemon and drains
// under the SECOND, with no frontend ever re-submitting it.
//
// The proof is negative and therefore strong: the second boot receives exactly
// one merge command — the resolve-and-continue handoff for the PARKED
// workspace — so the queued workspace reaching merged there is only possible if
// its entry was read back off the durable record.
func TestE2EAQueuedMergeSurvivesADaemonBounce(t *testing.T) {
	// Arrange — one repository, a parked head, and a clean sibling queued
	// behind it, all against the first boot.
	stateFile := bounceStateDir(t)
	repo := newMergeRepo(t)
	parkedDir := repo.conflictingWorktree("feature-parked")
	queuedDir := repo.cleanWorktree("feature-queued")

	first := bootMergeDaemon(t, stateFile)
	firstConn := first.dialFrontend(t)
	w1 := newMergeWatch(t, firstConn)
	parkedCmd := parkTheQueueHead(t, first.geometry, w1, repo, parkedDir, "feature-parked", "r-merge-parked")
	sendMerge(t, firstConn, "r-merge-queued", mergeCmdFor(t, first.geometry, repo, queuedDir, "feature-queued"))
	w1.awaitOKAck("r-merge-queued")
	w1.awaitPhase(queuedDir, phaseMergeQueued)
	assertAdmittedQueuePosition(t, w1, queuedDir, 2, 2)

	// Act — the daemon goes away with the queue outstanding, comes back, and is
	// handed nothing but the resolution of the head it left parked.
	_ = firstConn.Close()
	first.bounce()

	resolveConflict(t, repo, "resolved by the e2e harness\n")
	second := bootMergeDaemon(t, stateFile)
	secondConn := second.dialFrontend(t)
	defer secondConn.Close()
	w2 := newMergeWatch(t, secondConn)
	sendMergeResume(t, secondConn, "r-resume-parked", parkedCmd)
	w2.awaitOKAck("r-resume-parked")
	w2.awaitPhase(parkedDir, phaseMerged)

	// Assert — the entry nobody re-submitted drained, and its work is on disk.
	w2.awaitPhase(queuedDir, phaseMerged)
	if !strings.Contains(readMergeFile(t, repo.target, "feature-queued.txt"), "hello from feature-queued") {
		t.Error("the queued workspace reported merged but its file is not in the target: the drain reported work it did not do")
	}
	// REWRITTEN off the retired flat merge_queue_position, which this asserted
	// had returned to 0 once the entry drained. The field is gone, and the
	// structured statement of the same guarantee is stronger: a drained run has
	// LEFT the queue, so its newest status must have moved off the `enqueued`
	// arm entirely rather than merely reporting index 0 on it.
	state := w2.latest(queuedDir)
	if state.GetMergeStatus().GetEnqueued() != nil {
		t.Errorf("the drained workspace still presents an enqueued merge_status (position=%d depth=%d): a run that has merged has left the queue",
			state.GetMergeStatus().GetEnqueued().GetPosition(),
			state.GetMergeStatus().GetEnqueued().GetDepth())
	}
	if state.GetMergeStatus().GetMerged() == nil {
		t.Errorf("the drained workspace's newest merge_status is %T, want the terminal merged arm",
			state.GetMergeStatus().GetPhase())
	}
}

// resolveConflict stands in for the human: it writes the resolved content over
// the conflicted file so the resolve-and-continue handoff has something to
// continue. It deliberately does NOT stage the file — `git add -u` is
// merge.Driver.Resume's own first step, and doing it here would hide a
// regression in that step.
func resolveConflict(t *testing.T, repo *mergeRepo, content string) {
	t.Helper()
	writeMergeFile(t, repo.target, "shared.txt", content)
}
