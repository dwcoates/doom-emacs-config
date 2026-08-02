// THE NAME-ONLY MERGE, end to end: the post-cutover Emacs command carries a
// workspace and nothing else, and the DAEMON supplies the three coordinates
// from the map it owns.
//
// WHY THIS FILE EXISTS SEPARATELY FROM THE REST OF THE MERGE SUITE. Every other
// merge e2e file records its fixture's geometry through a helper
// (mergequeue_e2e_test.go mergeCmdFor) and never observes the resolution
// itself, so nothing there fails if the daemon stops consulting its map. This
// file asks the map question directly: a RECORDED workspace merges into the
// recorded target, and an UNRECORDED one is refused rather than guessed at.
//
// WHAT IS ACTUALLY PROVED HERE:
//
//   - a recorded workspace merges from a command that names only the workspace,
//     landing the commit in the recorded target worktree on disk;
//   - an UNRECORDED workspace is refused on the ack with an explanation, and
//     nothing is written into any repository.
//
// Reuses mergequeue_e2e_test.go's git fixtures / mergeWatch and e2e_test.go's
// stubs READ-ONLY.
package e2e

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
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

// geometryBoot is the daemon stack with its merge-geometry map wired, plus a
// handle on that map so the test can record what the daemon would have recorded
// at workspace-creation time.
type geometryBoot struct {
	ts       *httptest.Server
	geometry *geometry.Store
}

// bootGeometryDaemon stands the stack up over a fresh state store. It is
// deliberately session-free: a cherry-pick needs no shim, and the lease is
// covered in mergelease_e2e_test.go.
func bootGeometryDaemon(t *testing.T) *geometryBoot {
	t.Helper()
	stateDir := geometryStateDir(t)
	stateStore, err := statedb.Open(filepath.Join(stateDir, "state.db"))
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	reg := registry.OpenWith(registry.Options{DB: stateStore, Logf: t.Logf})
	ssmMgr, err := ssm.Open(ssm.Options{DB: stateStore, Resolver: server.NewRegistryResolver(reg), Logf: t.Logf})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	geometryStore, err := geometry.Open(stateStore, t.Logf)
	if err != nil {
		t.Fatalf("open geometry: %v", err)
	}

	forwarder := &server.PushForwarder{Logf: t.Logf}
	shimListener := shimlisten.New(t.Logf)
	if err := shimListener.Listen(isolatedShimSocket(t, os.Getenv("HOME"))); err != nil {
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
	noSpawn := func(string, server.CreateOpts) (func() error, error) {
		return nil, errNoSpawnInGeometryHarness
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
		PermissionModes:   server.NewRegistryModeStore(reg),
		Registrar:         registrar,
		ModelCatalogs:     registrar,
		DaemonVersion:     "0.1.0-e2e-geometry",
		ProtocolVersion:   "1",
		ShimBuildSHA:      func() string { return "" },
		Logf:              t.Logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}

	binding := &server.SessionCommandBinding{Logf: t.Logf}
	mergeQueue, err := merge.NewFileQueue(filepath.Join(stateDir, "merge-queue"), t.Logf)
	if err != nil {
		t.Fatalf("open merge queue: %v", err)
	}
	mergeLease, err := ssm.NewMergeLease(ssm.MergeLeaseConfig{Manager: ssmMgr, Queue: mergeQueue, Interrupter: controller})
	if err != nil {
		t.Fatalf("build merge lease: %v", err)
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
		DaemonVersion: "0.1.0-e2e-geometry",
		Registry:      reg,
		ModelCatalogs: modelCatalogs,
		Controller:    controller,
		SSM:           ssmMgr,
		Frontend:      agentShim.Server,
		Logf:          t.Logf,
	})
	binding.SetTarget(srv)
	mux := http.NewServeMux()
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	ts := httptest.NewServer(mux)
	t.Cleanup(func() {
		ts.Close()
		_ = agentShim.Close()
		controller.Close()
		_ = shimListener.Close()
		_ = targets.Close()
		_ = ssmMgr.Close()
		_ = stateStore.Close()
	})
	return &geometryBoot{ts: ts, geometry: geometryStore}
}

func (b *geometryBoot) dialFrontend(t *testing.T) *websocket.Conn {
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

type geometryHarnessError string

func (e geometryHarnessError) Error() string { return string(e) }

const errNoSpawnInGeometryHarness = geometryHarnessError(
	"e2e: the merge-geometry harness never spawns a shim; a spawn here means the stack took an unmodeled path")

// geometryStateDir is a short-path state directory. Not t.TempDir(): the
// test-name-derived path exceeds the 104-byte sun_path limit, so bind(2) fails
// on macOS (e2e_test.go, same reason).
func geometryStateDir(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "agent-repl-merge-geometry-")
	if err != nil {
		t.Fatalf("make state dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}

// sendBareMerge is the POST-CUTOVER Emacs command: a workspace and a display
// name, and NO geometry at all.
func sendBareMerge(t *testing.T, conn *websocket.Conn, requestID, workspace, name string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":%q,"workspace":%q,"mergeWorkspace":{"workspaceName":%q}}`,
		requestID, workspace, name))
}

func TestBareMergeCommandResolvesTheDaemonsRecordedGeometry(t *testing.T) {
	// Arrange — a real sibling worktree with one commit, and the geometry the
	// daemon would have recorded when it created that worktree.
	repo := newMergeRepo(t)
	worktree := repo.cleanWorktree("recorded-one")
	boot := bootGeometryDaemon(t)
	if err := boot.geometry.Record(context.Background(), geometry.Record{
		Workspace:    worktree,
		SourceBranch: "recorded-one",
		SourceDir:    worktree,
		TargetDir:    repo.target,
		Origin:       geometry.OriginCreated,
	}); err != nil {
		t.Fatalf("record geometry: %v", err)
	}
	conn := boot.dialFrontend(t)
	defer conn.Close()
	watch := newMergeWatch(t, conn)

	// Act — the command names the workspace and nothing else.
	sendBareMerge(t, conn, "bare-1", worktree, "recorded-one")

	// Assert — the ack is an admission, and the pick lands in the RECORDED
	// target worktree on disk.
	watch.awaitOKAck("bare-1")
	watch.awaitPhase(worktree, phaseMerged)
	if got := readMergeFile(t, repo.target, "recorded-one.txt"); got != "hello from recorded-one\n" {
		t.Fatalf("target file = %q, want the cherry-picked content", got)
	}
}

func TestBareMergeOfAnUnrecordedWorkspaceIsRefusedOnTheAck(t *testing.T) {
	// Arrange — a real worktree the daemon has no record for.
	repo := newMergeRepo(t)
	worktree := repo.cleanWorktree("unrecorded-one")
	boot := bootGeometryDaemon(t)
	conn := boot.dialFrontend(t)
	defer conn.Close()
	watch := newMergeWatch(t, conn)

	// Act.
	sendBareMerge(t, conn, "bare-2", worktree, "unrecorded-one")

	// Assert — the refusal explains itself, and nothing reached the repository.
	ack := watch.awaitAck("bare-2")
	if ack.GetOk() {
		t.Fatal("the merge was admitted for a workspace with no recorded geometry")
	}
	if !strings.Contains(ack.GetError(), "no recorded merge geometry") {
		t.Fatalf("nack = %q, want an unrecorded-geometry explanation", ack.GetError())
	}
	if _, err := os.Stat(filepath.Join(repo.target, "unrecorded-one.txt")); !os.IsNotExist(err) {
		t.Fatalf("the target worktree was written to: stat err = %v", err)
	}
}
