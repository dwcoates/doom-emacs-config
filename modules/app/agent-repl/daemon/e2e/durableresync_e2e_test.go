// A frontend that connects AFTER A DAEMON BOUNCE, with NO session brought back
// up, still receives the workspace's prior conversation.
//
// THE DEFECT THIS PINS. A resync for a workspace with no live session
// controller used to be skipped quietly: the frontend got its StateSnapshot, so
// the footer was right, and got no conversation deltas at all, so the feed was
// empty. The whole conversation was in the shim-store the entire time, and
// every daemon read path required a live shim to reach it.
//
// WHY THIS FILE STANDS UP ITS OWN DAEMON HALF rather than reusing
// newUDSHarness. The premise here is the ABSENCE of a session controller, and
// newUDSHarness's flows all begin by creating one. e2e_test.go's helpers are
// reused READ-ONLY (buildShimStore, startShimStore, dialStoreProducer,
// readFrame, writeCmd, frameTimeout, assistantText); nothing in it is modified.
//
// WHAT IS REAL HERE. The shim-store is the real launchd binary over a real
// SQLite database, written through its real producer protocol. The registry,
// the SSM, the session-controller manager, the storehistory reader, the
// frontend server, and the WebSocket transport are all the production types.
// The one thing deliberately absent is a shim — because that is the state under
// test — and the spawner is rigged to FAIL the test if anything tries to start
// one, so "served without a bring-up" is asserted rather than assumed.
package e2e

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/storehistory"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/types/known/anypb"
)

// bouncedHarness is a daemon that has just restarted: durable state exists,
// and NOTHING is live.
type bouncedHarness struct {
	ts *httptest.Server
	// workspace is the cwd the session record is keyed by.
	workspace string
	// vendorSessionID is the uuid the store files the conversation under.
	vendorSessionID string
	// spawns counts shim spawn attempts. It must stay at zero: a read must
	// cost a read.
	spawns *atomic.Int64
	// sessionID is the daemon-minted id the workspace's record is filed under.
	sessionID string
	// seq is the durable last_seen_seq mark the bounced daemon left behind.
	// shimclient writes it BEFORE forwarding an event, so it is the ceiling on
	// any honest client mark and the evidence a higher one counts in a retired
	// vendor seq space.
	seq *server.RegistrySeqStore
}

// newBouncedHarness builds the post-bounce daemon over a real shim-store.
func newBouncedHarness(t *testing.T) *bouncedHarness {
	t.Helper()
	storeBin := buildShimStore(t)
	// The store socket cannot live under t.TempDir(): the test-name-derived
	// path exceeds the 104-byte sun_path limit, so bind(2) fails on macOS.
	sockDir, err := os.MkdirTemp("/tmp", "agent-repl-bounce-")
	if err != nil {
		t.Fatalf("make short socket dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(sockDir) })
	storeSock := filepath.Join(sockDir, "store.sock")
	// dialStoreProducer resolves the store socket as $HOME/store.sock, the
	// same convention newUDSHarness establishes.
	t.Setenv("HOME", sockDir)
	shimSock := isolatedShimSocket(t, sockDir)
	startShimStore(t, storeBin, storeSock)

	stateStore, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	t.Cleanup(func() { _ = stateStore.Close() })
	reg := registry.OpenWith(registry.Options{DB: stateStore, Logf: t.Logf})
	ssmMgr, err := ssm.Open(ssm.Options{DB: stateStore, Resolver: server.NewRegistryResolver(reg), Logf: t.Logf})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = ssmMgr.Close() })

	// THE DURABLE STATE A BOUNCE LEAVES BEHIND: a registry record binding the
	// workspace to its session and that session to its vendor conversation
	// uuid, and no live anything.
	h := &bouncedHarness{
		workspace:       filepath.Join(sockDir, "ws"),
		vendorSessionID: "vendor-uuid-bounced",
		spawns:          &atomic.Int64{},
		sessionID:       "s_bounced",
	}
	if err := os.MkdirAll(h.workspace, 0o700); err != nil {
		t.Fatalf("make workspace dir: %v", err)
	}
	if err := reg.Put(registry.Record{
		SessionID:       h.sessionID,
		CWD:             h.workspace,
		ClaudeSessionID: h.vendorSessionID,
		CreatedAt:       time.Now().UTC().Format(time.RFC3339),
	}); err != nil {
		t.Fatalf("seed registry record: %v", err)
	}
	const retiredGeneration = "generation-before-bounce"
	if err := ssmMgr.ApplySessionConnectivity(h.workspace, h.sessionID, retiredGeneration, ssm.SessionConnectivityConnecting, "e2e_bring_up"); err != nil {
		t.Fatalf("seed connecting workspace state: %v", err)
	}
	if err := ssmMgr.ApplySessionConnectivity(h.workspace, h.sessionID, retiredGeneration, ssm.SessionConnectivityOperational, "e2e_shim_ready"); err != nil {
		t.Fatalf("seed operational workspace state: %v", err)
	}
	if err := ssmMgr.ApplySessionConnectivity(h.workspace, h.sessionID, retiredGeneration, ssm.SessionConnectivityHibernated, "e2e_daemon_bounce"); err != nil {
		t.Fatalf("seed hibernated workspace state: %v", err)
	}

	forwarder := &server.PushForwarder{Logf: t.Logf}
	shimListener := shimlisten.New(t.Logf)
	if err := shimListener.Listen(shimSock); err != nil {
		t.Fatalf("listen for shims: %v", err)
	}
	t.Cleanup(func() { _ = shimListener.Close() })
	targets := dlog.NewTargetManager()
	t.Cleanup(func() { _ = targets.Close() })
	fileDiagnostics, err := server.NewTargetFileDiagnosticPersister(targets, os.Stderr, false)
	if err != nil {
		t.Fatalf("build file diagnostic persister: %v", err)
	}
	progressMgr := progress.New(progress.Options{Logf: t.Logf})
	t.Cleanup(func() { _ = progressMgr.Close() })
	seqStore := server.NewRegistrySeqStore(reg, t.Logf)
	h.seq = seqStore
	// A read must never start a session. Any spawn attempt is a failure of the
	// property under test, so it is refused loudly rather than served.
	refuseSpawn := func(sessionID string, _ server.CreateOpts) (server.ShimHandle, error) {
		h.spawns.Add(1)
		return server.ShimHandle{}, fmt.Errorf("e2e: a durable resync tried to spawn a shim for %s", sessionID)
	}
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           server.NewShimSpawner(reg, shimListener.Connected, shimListener.Evict, refuseSpawn, t.Logf),
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		Locator:           &server.SessionLocator{Reg: reg},
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		TurnAccountings:   newTestTurnAccountingStore(),
		// THE WHOLE POINT: the durable route to the conversation, keyed by the
		// vendor uuid the store files it under.
		DurableHistory: &storehistory.Reader{
			Socket: storeSock,
			Vendor: func(sessionID string) (string, bool) {
				rec, ok := reg.Get(sessionID)
				if !ok {
					return "", false
				}
				return rec.ClaudeSessionID, rec.ClaudeSessionID != ""
			},
			Idle: durableReplayIdle,
			Logf: t.Logf,
		},
		DaemonVersion:   "0.1.0-e2e",
		ProtocolVersion: "1",
		Logf:            t.Logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}
	t.Cleanup(controller.Close)

	binding := &server.SessionCommandBinding{Logf: t.Logf}
	mergeQueue := newTestMergeQueue(t)
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
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
		Resumes:           &server.ConversationResolver{Reg: reg, Logf: t.Logf},
		MergeLease:        stubMergeLease{},
		MergeQueue:        mergeQueue,
		Logf:              t.Logf,
		LogVerbosef:       t.Logf,
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	forwarder.SetTarget(agentShim.Server)
	t.Cleanup(func() { _ = agentShim.Close() })

	srv := server.New(server.Config{
		DaemonVersion: "0.1.0-e2e",
		Registry:      reg,
		ModelCatalogs: server.NewSessionModelCatalogs(),
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
	t.Cleanup(ts.Close)
	h.ts = ts
	return h
}

// dialFrontend opens the unfiltered /frontend socket, the surface a mounting
// webapp connects on.
func (h *bouncedHarness) dialFrontend(t *testing.T) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/frontend"
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

// storedAssistantEvent is what the stream plane persisted for one assistant
// reply, in the store's own envelope shape.
func storedAssistantEvent(t *testing.T, vendorSessionID, uuid, text string) *corev1.Event {
	t.Helper()
	payload, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
			Uuid: uuid,
			Message: &datav1.ApiAssistantMessage{
				Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}}},
			},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		DedupKey:     "assistant:" + uuid,
		Payload:      &corev1.Event_Vendor{Vendor: payload},
	}
}

// resyncFrom sends one resync for the harness's workspace and returns the
// assistant text of every ConversationDelta that arrives before the request's
// CommandAck, which terminates the read: the frontend server processes one
// command at a time and pushes the whole replay before acking.
func (h *bouncedHarness) resyncFrom(t *testing.T, conn *websocket.Conn, state *frontendv1.WorkspaceState, requestID string, fromSeq uint64) []string {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"workspace":%q,"resync":{"fromSeq":"%d","fence":%q}}`,
		requestID, h.workspace, fromSeq, state.GetFence()))
	var texts []string
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_ConversationDelta:
			for _, item := range f.ConversationDelta.GetItems() {
				if text := assistantText(item); text != "" {
					texts = append(texts, text)
				}
			}
		case *frontendv1.FrontendFrame_CommandAck:
			if f.CommandAck.GetRequestId() != requestID {
				continue
			}
			if !f.CommandAck.GetOk() {
				t.Fatalf("resync nacked: %s", f.CommandAck.GetError())
			}
			return texts
		}
	}
	t.Fatalf("no CommandAck for resync %s arrived before the deadline", requestID)
	return nil
}

func workspaceStateInSnapshot(t *testing.T, frame *frontendv1.FrontendFrame, workspace string) *frontendv1.WorkspaceState {
	t.Helper()
	snapshot := frame.GetSnapshot()
	if snapshot == nil {
		t.Fatalf("first /frontend frame = %T, want a StateSnapshot", frame.GetFrame())
	}
	for _, state := range snapshot.GetWorkspaces() {
		if state.GetWorkspace() == workspace {
			return state
		}
	}
	t.Fatalf("snapshot has no authoritative state for workspace %q", workspace)
	return nil
}

// --- tests ------------------------------------------------------------------

func TestAFrontendConnectingAfterADaemonBounceReceivesThePriorConversation(t *testing.T) {
	// Arrange — the conversation is in the store; nothing is live.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-1", "the first reply"))
	producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-2", "the second reply"))
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act — exactly what a reloaded webview sends.
	texts := h.resyncFrom(t, conn, state, "e2e-bounced-resync-1", 0)

	// Assert.
	joined := strings.Join(texts, "|")
	if !strings.Contains(joined, "the first reply") || !strings.Contains(joined, "the second reply") {
		t.Fatalf("replayed assistant texts = %v, want both stored replies", texts)
	}
}

func TestServingADurableResyncNeverSpawnsAShim(t *testing.T) {
	// Arrange — a frontend mounting is not a reason to start a vendor process.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-1", "the only reply"))
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act.
	h.resyncFrom(t, conn, state, "e2e-bounced-resync-2", 0)

	// Assert.
	if got := h.spawns.Load(); got != 0 {
		t.Fatalf("shim spawn attempts = %d, want 0 — a read must cost a read", got)
	}
}

func TestADurableResyncRespectsTheClientsFromSeq(t *testing.T) {
	// Arrange — a frontend that already holds the first reply asks only for
	// what came after it.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	first := producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-1", "the first reply"))
	second := producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-2", "the second reply"))
	// The bounced daemon had consumed the whole conversation before it died,
	// which is what makes the client's mark an in-space one rather than
	// evidence of a retired vendor seq space.
	h.seq.SetLastSeq(h.sessionID, second.GetLastSeq())
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act — from_seq is the client's INCLUSIVE mark, one past the first reply.
	texts := h.resyncFrom(t, conn, state, "e2e-bounced-resync-3", first.GetLastSeq()+1)

	// Assert.
	joined := strings.Join(texts, "|")
	if strings.Contains(joined, "the first reply") {
		t.Fatalf("replayed assistant texts = %v, want the first reply withheld below the client's mark", texts)
	}
	if !strings.Contains(joined, "the second reply") {
		t.Fatalf("replayed assistant texts = %v, want the second reply", texts)
	}
}
