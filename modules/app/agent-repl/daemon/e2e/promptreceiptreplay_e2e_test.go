// What a successor daemon serves after the daemon that accepted a prompt died
// before the turn ever became durable.
//
// WHAT THIS FILE USED TO PIN, AND WHAT REPLACED IT. It asserted that the
// daemon's own prompt RECEIPT survived the crash in the state store and was
// replayed to a reconnecting frontend, so the user still saw evidence the
// prompt had been sent. FROZEN-slash-command-durability.md Part 2 removes
// prompt receipts entirely: a prompt renders when its durable line round-trips
// through the SDK and never before, because the standing-in bubble was a second
// identity for one prompt and reconciling the two produced the duplicate
// bubbles. There is therefore no receipt left to replay, and this file now pins
// the behaviour that replaced it — the successor serves exactly what is
// DURABLE, and invents nothing for the prompt that never got there.
//
// THE ACCEPTED CONSEQUENCE, STATED SO IT IS NOT MISTAKEN FOR AN OVERSIGHT. A
// prompt accepted by a shim that then died without writing anything leaves no
// trace in the conversation at all. That is the deliberate trade of Part 2: in
// the real system the CLI writes the durable user line as soon as it takes the
// prompt, so the window is the crash window between accept and that write, and
// the alternative is the two-identity problem for every prompt ever submitted.
//
// WHY IT STANDS UP TWO DAEMON HALVES. The claim is about a fact crossing a
// process boundary: half A accepts the prompt and dies, half B — a genuinely
// separate Manager, frontend server, and session-state manager over the SAME
// state store — is asked for the conversation. A single-process test could
// observe in-memory state instead of what actually survived.
//
// WHY THE SHIM IS A GO FAKE HERE. What the test needs is a shim that ACCEPTS a
// prompt and then dies without ever writing the turn anywhere — the one window
// in which no durable line for a submitted prompt can ever exist. The real
// offline shim answers every prompt promptly and files the turn in the store,
// so it produces the opposite of the condition under test. The fake speaks the
// real length-prefixed protocol over the real listener, so the daemon's accept
// path and its state edges are exercised for real; only the vendor behind it
// is scripted.
//
// Shares e2e_test.go's package and reuses its helpers READ-ONLY (buildShimStore,
// startShimStore, dialStoreProducer, readFrame, writeCmd, frameTimeout,
// stubLifecycle, stubMergeLease, newEmptyWorkspaceCreation, newTestMergeQueue)
// plus durableresync_e2e_test.go's storedAssistantEvent; nothing in either is
// modified.
package e2e

import (
	"context"
	"database/sql"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/wire"

	"claude-repld/internal/dlog"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/storehistory"

	"github.com/gorilla/websocket"
)

// receiptWorld is the durable ground both daemon halves stand on: one state
// store path, one shim-store, one registry record.
type receiptWorld struct {
	statePath string
	storeSock string
	sockDir   string
	shimSock  string
	// workspace is the cwd the session record is keyed by.
	workspace string
	// sessionID is the daemon-minted id the workspace's record is filed under.
	sessionID string
	// vendorSessionID is the uuid the store files the conversation under.
	vendorSessionID string
}

// newReceiptWorld starts the real shim-store and seeds the registry record a
// live session would already have.
func newReceiptWorld(t *testing.T) *receiptWorld {
	t.Helper()
	storeBin := buildShimStore(t)
	// The store socket cannot live under t.TempDir(): the test-name-derived
	// path exceeds the 104-byte sun_path limit, so bind(2) fails on macOS.
	sockDir, err := os.MkdirTemp("/tmp", "agent-repl-receipt-")
	if err != nil {
		t.Fatalf("make short socket dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(sockDir) })
	// dialStoreProducer resolves the store socket as $HOME/store.sock, and the
	// session locks resolve under $HOME too.
	t.Setenv("HOME", sockDir)
	shimSock := isolatedShimSocket(t, sockDir)
	if err := sessionlock.EnsureDir(); err != nil {
		t.Fatalf("make session lock dir: %v", err)
	}
	storeSock := filepath.Join(sockDir, "store.sock")
	startShimStore(t, storeBin, storeSock)

	w := &receiptWorld{
		statePath:       filepath.Join(t.TempDir(), "state.db"),
		storeSock:       storeSock,
		sockDir:         sockDir,
		shimSock:        shimSock,
		workspace:       filepath.Join(sockDir, "ws"),
		sessionID:       "s_receipt",
		vendorSessionID: "vendor-uuid-receipt",
	}
	if err := os.MkdirAll(w.workspace, 0o700); err != nil {
		t.Fatalf("make workspace dir: %v", err)
	}
	db, reg := w.openState(t)
	// NO vendor uuid yet: bring-up VALIDATES a resume pointer against the disk
	// and drops one whose transcript is not there, and the fake shim writes no
	// transcript. The binding a real session would have by now is restored in
	// restart(), where it is durable state the bounced half reads rather than
	// something the fake had to produce.
	if err := reg.Put(registry.Record{
		SessionID: w.sessionID,
		CWD:       w.workspace,
		CreatedAt: time.Now().UTC().Format(time.RFC3339),
	}); err != nil {
		t.Fatalf("seed registry record: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("close seeding state store: %v", err)
	}
	return w
}

// openState opens the shared state store and its registry.
func (w *receiptWorld) openState(t *testing.T) (*sql.DB, *registry.Registry) {
	t.Helper()
	db, err := statedb.Open(w.statePath)
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	return db, registry.OpenWith(registry.Options{DB: db, Logf: t.Logf})
}

// --- the fake shim ----------------------------------------------------------

// acceptOnceShim is a shim that completes the real handshake, ACKS exactly one
// prompt, and then dies without producing a single event.
//
// It makes the no-durable-line window deterministic: the prompt
// genuinely reached a shim (so the daemon's accept is honest), and the
// conversation genuinely never received it (so nothing durable can ever carry
// it).
type acceptOnceShim struct {
	sessionID       string
	vendorSessionID string
	// accepted closes once the shim has acked a prompt, so the test waits on
	// the event rather than on a duration.
	accepted chan string
}

func newAcceptOnceShim(sessionID, vendorSessionID string) *acceptOnceShim {
	return &acceptOnceShim{
		sessionID:       sessionID,
		vendorSessionID: vendorSessionID,
		accepted:        make(chan string, 1),
	}
}

// run dials the daemon's shim socket and speaks the protocol until it has
// acked one prompt.
func (s *acceptOnceShim) run(t *testing.T, shimSock string) error {
	conn, err := net.Dial("unix", shimSock)
	if err != nil {
		return fmt.Errorf("fake shim: dial %s: %w", shimSock, err)
	}
	go func() {
		defer conn.Close()
		if err := wire.WriteAny(conn, &corev1.ShimHello{
			SessionId:       s.sessionID,
			Vendor:          "claude",
			ShimVersion:     "fake-accept-once",
			ProtocolVersion: "1",
			VendorSessionId: s.vendorSessionID,
			QueryInstanceId: "query-accept-once-" + s.sessionID,
		}); err != nil {
			t.Errorf("fake shim: write ShimHello: %v", err)
			return
		}
		for {
			msg, err := wire.ReadAny(conn)
			if err != nil {
				return // the daemon went away, or this shim already died
			}
			switch m := msg.(type) {
			case *corev1.DaemonHello:
				// The session's own announcement that it started, BEFORE the
				// gate closes: an operational session must already carry a
				// status, and a workspace that has never heard a start is
				// still INIT, which no prompt may be accepted over. It rides
				// as an EPHEMERAL event (seq 0) under the DAEMON's session id,
				// exactly as every off-store event does, so nothing about it
				// reaches the store — this shim's whole point is producing no
				// durable record of the turn it is about to accept.
				if err := wire.WriteAny(conn, &corev1.Event{
					SessionId:    s.sessionID,
					Plane:        corev1.Plane_PLANE_STREAM,
					Class:        corev1.EventClass_EVENT_CLASS_EPHEMERAL,
					ProducedAtMs: time.Now().UnixMilli(),
					Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{
						VendorSessionId: s.vendorSessionID,
					}},
				}); err != nil {
					t.Errorf("fake shim: write SessionStarted: %v", err)
					return
				}
				if err := wire.WriteAny(conn, &corev1.ShimReady{
					SessionId:       s.sessionID,
					FromSeq:         m.GetFromSeq(),
					VendorSessionId: s.vendorSessionID,
				}); err != nil {
					t.Errorf("fake shim: write ShimReady: %v", err)
					return
				}
			case *corev1.SubmitPrompt:
				// ACCEPT, then DIE: the ack is written and the connection is
				// closed without a single event ever being produced.
				if err := wire.WriteAny(conn, &corev1.Ack{RequestId: m.GetRequestId()}); err != nil {
					t.Errorf("fake shim: write Ack: %v", err)
					return
				}
				s.accepted <- m.GetText()
				return
			}
		}
	}()
	return nil
}

// awaitAccepted takes the prompt text the fake shim acked.
func (s *acceptOnceShim) awaitAccepted(t *testing.T) string {
	t.Helper()
	select {
	case text := <-s.accepted:
		return text
	case <-time.After(frameTimeout):
		t.Fatal("the fake shim never acked a prompt")
		return ""
	}
}

// --- half A: the daemon that accepts the prompt and dies --------------------

// submitThenDie brings up a session over the real listener, submits one prompt
// through the production submit path, and then tears the whole daemon half
// down — leaving only what is on disk.
func (w *receiptWorld) submitThenDie(t *testing.T, requestID, text string) {
	t.Helper()
	db, reg := w.openState(t)
	ssmMgr, err := ssm.Open(ssm.Options{DB: db, Resolver: server.NewRegistryResolver(reg), Logf: t.Logf})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	promptReceipts, err := statedb.NewPromptReceipts(db)
	if err != nil {
		t.Fatalf("open prompt receipts: %v", err)
	}
	shimListener := shimlisten.New(t.Logf)
	if err := shimListener.Listen(w.shimSock); err != nil {
		t.Fatalf("listen for shims: %v", err)
	}
	targets := dlog.NewTargetManager()
	fileDiagnostics, err := server.NewTargetFileDiagnosticPersister(targets, os.Stderr, false)
	if err != nil {
		t.Fatalf("build file diagnostic persister: %v", err)
	}
	progressMgr := progress.New(progress.Options{Logf: t.Logf})
	seqStore := server.NewRegistrySeqStore(reg, t.Logf)
	shim := newAcceptOnceShim(w.sessionID, w.vendorSessionID)
	spawn := func(sessionID string, _ server.CreateOpts) (server.ShimHandle, error) {
		if err := shim.run(t, w.shimSock); err != nil {
			return server.ShimHandle{}, err
		}
		return server.ShimHandle{Stop: func(server.ShimStop) error { return nil }}, nil
	}
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              &server.PushForwarder{Logf: t.Logf},
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           server.NewShimSpawner(reg, shimListener.Connected, shimListener.Evict, spawn, t.Logf),
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		Locator:           &server.SessionLocator{Reg: reg},
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		TurnAccountings:   newTestTurnAccountingStore(),
		// The prompt-receipt store is still wired because it is now the
		// interrupted-turn resumption ledger; nothing in it stands in for a
		// prompt, which is what this file asserts.
		PromptReceipts:  promptReceipts,
		DaemonVersion:   "0.1.0-e2e",
		ProtocolVersion: "1",
		Logf:            t.Logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}

	// THE RENDEZVOUS, not a wait. A prompt may only be accepted over a session
	// the state log already calls operational, and the connectivity edge the
	// shim's ShimReady triggers is applied on the session controller's own
	// goroutine. Subscribing to the state log and taking the edge is the event
	// that says the session is driveable; polling or sleeping for it would be
	// guessing at the same fact.
	states, unsubscribe := ssmMgr.Subscribe()
	defer unsubscribe()
	ctx, cancel := context.WithTimeout(context.Background(), frameTimeout)
	defer cancel()
	if err := controller.Ensure(w.workspace); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	awaitOperational(t, states, w.workspace)
	if err := controller.SubmitPrompt(ctx, w.workspace, requestID, text, "default", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if got := shim.awaitAccepted(t); got != text {
		t.Fatalf("the shim acked %q, want the submitted prompt", got)
	}

	// THE DEATH. Everything this half owned goes, in the order a process exit
	// would take it, leaving the state store on disk as the only survivor.
	controller.Close()
	progressMgr.Close()
	_ = shimListener.Close()
	_ = targets.Close()
	_ = ssmMgr.Close()
	if err := db.Close(); err != nil {
		t.Fatalf("close state store: %v", err)
	}
}

// awaitOperational takes the state-log edge that reports the workspace's
// session as wired and driveable.
func awaitOperational(t *testing.T, states <-chan *frontendv1.WorkspaceState, workspace string) {
	t.Helper()
	deadline := time.After(frameTimeout)
	for {
		select {
		case st, ok := <-states:
			if !ok {
				t.Fatal("the state log closed before the session became operational")
			}
			if isOperationalState(st, workspace) {
				return
			}
		case <-deadline:
			t.Fatalf("the session for %s never became operational", workspace)
		}
	}
}

// --- half B: the daemon that comes back and is asked for the conversation ---

// bouncedFrontend is the restarted daemon half: a real frontend server over a
// real store-backed durable replay, with NOTHING live.
type bouncedFrontend struct {
	ts *httptest.Server
}

// restart builds the daemon half that comes back after the crash.
func (w *receiptWorld) restart(t *testing.T) *bouncedFrontend {
	t.Helper()
	db, reg := w.openState(t)
	t.Cleanup(func() { _ = db.Close() })
	// The conversation's vendor uuid — the key the store's seq space is under,
	// and the binding every real session's record carries by the time it has
	// produced a turn.
	rec, ok := reg.Get(w.sessionID)
	if !ok {
		t.Fatalf("the session record for %s did not survive the bounce", w.sessionID)
	}
	rec.ClaudeSessionID = w.vendorSessionID
	if err := reg.Put(rec); err != nil {
		t.Fatalf("bind the vendor conversation uuid: %v", err)
	}
	ssmMgr, err := ssm.Open(ssm.Options{DB: db, Resolver: server.NewRegistryResolver(reg), Logf: t.Logf})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = ssmMgr.Close() })
	promptReceipts, err := statedb.NewPromptReceipts(db)
	if err != nil {
		t.Fatalf("open prompt receipts: %v", err)
	}
	forwarder := &server.PushForwarder{Logf: t.Logf}
	shimListener := shimlisten.New(t.Logf)
	if err := shimListener.Listen(w.shimSock); err != nil {
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
	// A read must never start a session: the replay has to come from what is
	// on disk, not from bringing the vendor back to ask it.
	refuseSpawn := func(sessionID string, _ server.CreateOpts) (server.ShimHandle, error) {
		return server.ShimHandle{}, fmt.Errorf("e2e: the bounced half tried to spawn a shim for %s", sessionID)
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
		PromptReceipts:    promptReceipts,
		DurableHistory: &storehistory.Reader{
			Socket: w.storeSock,
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
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		// The boot orphan sweep scans THIS directory and no other: a test that
		// let it resolve the process temp dir deleted the live daemon's rebase
		// worktrees mid-merge.
		RebaseRoot:        t.TempDir(),
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
		MergeQueue:        newTestMergeQueue(t),
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
		AgentShim:     agentShim,
		Logf:          t.Logf,
	})
	binding.SetTarget(srv)
	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	ts := httptest.NewServer(mux)
	t.Cleanup(ts.Close)
	return &bouncedFrontend{ts: ts}
}

// dial opens the unfiltered /frontend socket and consumes the connect snapshot.
func (f *bouncedFrontend) dial(t *testing.T, workspace string) (*websocket.Conn, *frontendv1.WorkspaceState) {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(f.ts.URL, "http") + "/frontend"
	conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("dial /frontend: %v", err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn, workspaceStateInSnapshot(t, readFrame(t, conn), workspace)
}

// resyncItems sends one resync and returns every conversation item that
// arrives before its CommandAck, which terminates the read.
func (f *bouncedFrontend) resyncItems(t *testing.T, conn *websocket.Conn, state *frontendv1.WorkspaceState, workspace, requestID string) []*frontendv1.Message {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"workspace":%q,"resync":{"fromSeq":"0","fence":%q}}`,
		requestID, workspace, state.GetFence()))
	var items []*frontendv1.Message
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch fr := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_ConversationDelta:
			items = append(items, fr.ConversationDelta.GetMessages()...)
		case *frontendv1.FrontendFrame_CommandAck:
			if fr.CommandAck.GetRequestId() != requestID {
				continue
			}
			if !fr.CommandAck.GetOk() {
				t.Fatalf("resync nacked: %s", fr.CommandAck.GetError())
			}
			return items
		}
	}
	t.Fatalf("no CommandAck for resync %s arrived before the deadline", requestID)
	return nil
}

// promptDetachedWork returns the user-prompt items among a replay's items.
func promptDetachedWork(items []*frontendv1.Message) []*frontendv1.Message {
	var out []*frontendv1.Message
	for _, it := range items {
		if um := it.GetUserMessage(); um != nil && um.GetContentString() != "" {
			out = append(out, it)
		}
	}
	return out
}

// --- tests ------------------------------------------------------------------

// TestAPromptWithNoDurableLineIsNotSynthesizedByTheSuccessorDaemon is the
// Part 2 regression guard across a process boundary: the successor may not
// invent a bubble for a prompt no record exists for.
//
// It is the inversion of the receipt claim this file used to make, and it is
// the stronger guard of the two against the duplicate-bubble defect: a
// synthetic bubble here would be a second identity for a prompt whose real
// line may still arrive later from the vendor's transcript.
func TestAPromptWithNoDurableLineIsNotSynthesizedByTheSuccessorDaemon(t *testing.T) {
	// Arrange — the prompt reached a shim, the shim died without producing
	// anything, and the daemon that accepted it is gone.
	w := newReceiptWorld(t)
	w.submitThenDie(t, "e2e-receipt-1", "the prompt nobody kept")
	f := w.restart(t)
	conn, state := f.dial(t, w.workspace)

	// Act — exactly what a reloaded webview sends.
	items := f.resyncItems(t, conn, state, w.workspace, "e2e-receipt-resync-1")

	// Assert.
	for _, it := range promptDetachedWork(items) {
		t.Errorf("the successor served a prompt bubble uuid=%q request_id=%q content=%q for a turn no record exists for — a prompt renders when its durable line round-trips and never before",
			it.GetUuid(), it.GetRequestId(), it.GetUserMessage().GetContentString())
	}
}

// TestTheStoredConversationReplaysIntactAfterTheAcceptingDaemonDies keeps the
// half of the old "beside the stored conversation" claim that survives: what is
// DURABLE is served in full by the successor, and the lost prompt neither
// suppresses it nor appends itself to it.
func TestTheStoredConversationReplaysIntactAfterTheAcceptingDaemonDies(t *testing.T) {
	// Arrange — a workspace with real stored history whose next prompt was
	// accepted by a daemon that then died.
	w := newReceiptWorld(t)
	producer := dialStoreProducer(t)
	producer.write(storedAssistantEvent(t, w.vendorSessionID, "u-1", "an earlier reply"))
	w.submitThenDie(t, "e2e-receipt-2", "the prompt nobody kept")
	f := w.restart(t)
	conn, state := f.dial(t, w.workspace)

	// Act.
	items := f.resyncItems(t, conn, state, w.workspace, "e2e-receipt-resync-2")

	// Assert — the stored reply is there, and nothing was appended for the
	// prompt that never became durable.
	if len(items) == 0 {
		t.Fatal("the replay carried nothing at all, want the stored reply")
	}
	last := items[len(items)-1]
	if last.GetRequestId() == "e2e-receipt-2" {
		t.Errorf("the replay ended with an item claiming the lost submit's request id %q — nothing durable carries it", last.GetRequestId())
	}
	for _, it := range promptDetachedWork(items) {
		t.Errorf("the replay carried a prompt bubble uuid=%q content=%q beside the stored history, want only what the store holds",
			it.GetUuid(), it.GetUserMessage().GetContentString())
	}
}
