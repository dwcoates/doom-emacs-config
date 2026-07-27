// Clear and compact, end to end over the REAL processes: a file-plane
// ContextCleared / ContextCompacted injected into the real shim-store, fanned
// out to the real TS shim's store subscription, forwarded to the daemon, and
// rendered onto the frontend WebSocket as ConversationItem arms 32/33.
//
// WHY THE EVENTS ARE INJECTED RATHER THAN PROVOKED. The shim-claude-sidecar is
// the SOLE producer of both payloads (core.proto §"the file plane is the only
// producer"; agent-shim/claude/shim-sidecar/internal/handler/clearcompact.go
// file header), and it produces them by TAILING A REAL VENDOR TRANSCRIPT. This
// harness runs the shim `--fake` offline, so no transcript exists and no
// sidecar runs. The tests therefore write to the store EXACTLY what the sidecar
// writes — every field below is traced to sidecar/store source in the comment
// on the constructor that sets it — and exercise everything downstream of the
// producer for real: store ingest, store dedup, store fan-out, the shim's
// merged-stream forward, the daemon's curation, the replay floor, and the
// frontend frames.
//
// These tests share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, createSession, dial, readFrame, writeCmd, frameTimeout).
//
// NOT COVERED — daemon-restart persistence of the replay floor. e2eHarness
// exposes only its httptest.Server: the registry file, the SSM database, the
// shim listener socket, and the driver are all constructed inside
// newUDSHarness over per-call t.TempDir() paths with no seam to rebuild the
// daemon half over the same persisted state. Restarting the daemon in-process
// would mean forking the entire harness wiring into this file (and re-binding
// the shim listener under a live shim), which is a second harness rather than a
// test. Left out deliberately rather than faked.
package e2e

import (
	"fmt"
	"net"
	"os"
	"path/filepath"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/proto"

	"agentrepl/wire"
)

// sidecarProducer is the StoreWrite producer identity the sidecar writes under
// (handler.Producer, shim-sidecar/internal/handler/handler.go §5.2).
const sidecarProducer = "shim-claude-sidecar"

// --- injection: the sidecar's write surface --------------------------------

// storeSocket resolves the harness's store socket.
//
// newUDSHarness builds its store socket as <sockDir>/store.sock and points HOME
// at that same sockDir (e2e_test.go, "The daemon's shim socket and the session
// locks resolve under $HOME"), so $HOME is the harness's own short-path dir for
// the duration of the test. Read-only derivation — nothing here creates or
// moves the socket.
func storeSocket(t *testing.T) string {
	t.Helper()
	home, ok := os.LookupEnv("HOME")
	if !ok || home == "" {
		t.Fatal("HOME is unset: newUDSHarness must have pointed it at its socket dir")
	}
	sock := filepath.Join(home, "store.sock")
	if _, err := os.Stat(sock); err != nil {
		t.Fatalf("store socket %s not present: %v", sock, err)
	}
	return sock
}

// storeProducer is one producer connection to the store, speaking the same
// transport the sidecar's storeclient does: agentrepl/wire WriteAny/ReadAny,
// StoreWrite out, StoreWriteAck back (shim-store/internal/server/server.go
// "Connection roles"; shim-sidecar/internal/storeclient/client.go Write).
type storeProducer struct {
	t    *testing.T
	conn net.Conn
}

func dialStoreProducer(t *testing.T) *storeProducer {
	t.Helper()
	conn, err := net.Dial("unix", storeSocket(t))
	if err != nil {
		t.Fatalf("dial store: %v", err)
	}
	t.Cleanup(func() { _ = conn.Close() })
	return &storeProducer{t: t, conn: conn}
}

// write sends one single-event StoreWrite batch and returns the store's ack. A
// rejected batch is a hard failure: the injection is the test's premise.
//
// cursor_advance is deliberately unset. The sidecar advances a cursor when it
// has read a file; there is no file here, and EventBatch.cursor_advance is an
// optional field the store only upserts when non-nil (db.Ingest).
func (p *storeProducer) write(ev *corev1.Event) *corev1.StoreWriteAck {
	p.t.Helper()
	if err := p.conn.SetDeadline(time.Now().Add(frameTimeout)); err != nil {
		p.t.Fatalf("store conn deadline: %v", err)
	}
	if err := wire.WriteAny(p.conn, &corev1.StoreWrite{
		Producer: sidecarProducer,
		Batch:    &corev1.EventBatch{Events: []*corev1.Event{ev}},
	}); err != nil {
		p.t.Fatalf("send StoreWrite: %v", err)
	}
	msg, err := wire.ReadAny(p.conn)
	if err != nil {
		p.t.Fatalf("read StoreWriteAck: %v", err)
	}
	ack, ok := msg.(*corev1.StoreWriteAck)
	if !ok {
		p.t.Fatalf("store replied %T, want *corev1.StoreWriteAck", msg)
	}
	if ack.GetError() != "" {
		p.t.Fatalf("store rejected the batch: %s", ack.GetError())
	}
	return ack
}

// sidecarClearEvent is byte-for-byte what handler.clearedEvent builds:
//
//	session_id     the VENDOR session id (shim-store/AGENTS.md: every
//	               session_id in the store is the vendor uuid)
//	plane          PLANE_FILE            (handler.base, called with Plane_PLANE_FILE)
//	class          EVENT_CLASS_PERSISTENT (handler.base)
//	produced_at_ms producer wall clock   (handler.base, nowMillis)
//	dedup_key      "clear:<line uuid>"   (handler.clearDedupKey)
//	payload        ContextCleared{}      (empty message: it has no fields)
func sidecarClearEvent(vendorSessionID, lineUUID string) *corev1.Event {
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		DedupKey:     clearDedupKey(lineUUID),
		Payload:      &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}},
	}
}

// sidecarCompactEvent is what handler.compactedEvent builds for a
// compact_boundary line coalesced with the summary line that follows it:
// the same envelope as a clear, keyed "compact:<boundary uuid>"
// (handler.compactDedupKey), carrying trigger / pre_tokens / post_tokens /
// duration_ms off the boundary's compact_metadata, and the summary text off
// the following line. There is no outcome field: the boundary line's existence
// IS the completion record, so the file plane — the sole producer — has nothing
// but success to report.
func sidecarCompactEvent(vendorSessionID, boundaryUUID, summary string) *corev1.Event {
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		DedupKey:     compactDedupKey(boundaryUUID),
		Payload: &corev1.Event_ContextCompacted{ContextCompacted: &corev1.ContextCompacted{
			Trigger:    corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_MANUAL,
			PreTokens:  120_000,
			PostTokens: 18_000,
			DurationMs: 4_200,
			Summary:    summary,
		}},
	}
}

// clearDedupKey / compactDedupKey mirror the sidecar's key derivation
// (clearcompact.go §6.4). They are also the frontend item uuid: the daemon
// curates a clear or a compaction under its dedup key (translate.go
// clearOrCompactUUID).
func clearDedupKey(lineUUID string) string       { return "clear:" + lineUUID }
func compactDedupKey(boundaryUUID string) string { return "compact:" + boundaryUUID }

// --- frontend observation ---------------------------------------------------

// waitAttachedSession waits for the shim to attach to the daemon and reports
// the VENDOR session id to write events under.
//
// The attach wait mirrors the harness's own (e2e_test.go): a submit or an
// injection before the shim is up has nowhere to land, and the shim's store
// subscription is opened off the daemon's Subscribe, which rides the attach.
//
// The vendor id is taken from SessionView.claude_session_id when the daemon has
// captured it. In `--fake` mode with no resume the shim's fake init reports the
// SHIM session id as its session uuid (agent-shim/claude/shim/src/fake-query.ts:
// "the daemon's captured claude_session_id equals the ephemeral daemon session
// id"), and the shim's --session-id IS the daemon id (server.ShimUDSArgv), so
// the fallback names the same string rather than a different one.
func waitAttachedSession(t *testing.T, conn *websocket.Conn, sessionID, cwd string) string {
	t.Helper()
	vendorID := ""
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		attached := false
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_WorkspaceState:
			attached = f.WorkspaceState.GetSessionId() == sessionID || f.WorkspaceState.GetWorkspace() == cwd
		case *frontendv1.FrontendFrame_SessionInit:
			attached = f.SessionInit.GetSessionId() == sessionID
		case *frontendv1.FrontendFrame_SessionView:
			sv := f.SessionView
			if sv.GetSessionId() == sessionID {
				if id := sv.GetClaudeSessionId(); id != "" {
					vendorID = id
				}
				attached = sv.GetShimAttached()
			}
		}
		if attached {
			if vendorID == "" {
				t.Logf("no claude_session_id captured yet; writing store events under the daemon session id %s (fake mode: the vendor uuid IS the shim session id)", sessionID)
				return sessionID
			}
			return vendorID
		}
	}
	t.Fatal("shim never attached (no session-scoped push arrived)")
	return ""
}

// deltaItems returns the conversation items a frame carries for workspace, or
// nil when the frame is not this workspace's ConversationDelta.
func deltaItems(frame *frontendv1.FrontendFrame, workspace string) []*frontendv1.ConversationItem {
	cd, ok := frame.GetFrame().(*frontendv1.FrontendFrame_ConversationDelta)
	if !ok || cd.ConversationDelta.GetWorkspace() != workspace {
		return nil
	}
	return cd.ConversationDelta.GetItems()
}

// awaitItem reads frames until a conversation item for workspace satisfies
// match, returning that item and EVERY workspace item seen before it (in
// arrival order). It fails loudly at the deadline.
func awaitItem(t *testing.T, conn *websocket.Conn, workspace string, what string, match func(*frontendv1.ConversationItem) bool) (*frontendv1.ConversationItem, []*frontendv1.ConversationItem) {
	t.Helper()
	var before []*frontendv1.ConversationItem
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		for _, item := range deltaItems(readFrame(t, conn), workspace) {
			if match(item) {
				return item, before
			}
			before = append(before, item)
		}
	}
	t.Fatalf("no %s arrived for workspace %s before the deadline", what, workspace)
	return nil, nil
}

// collectItems reads n further conversation items for workspace.
func collectItems(t *testing.T, conn *websocket.Conn, workspace string, n int) []*frontendv1.ConversationItem {
	t.Helper()
	var out []*frontendv1.ConversationItem
	deadline := time.Now().Add(frameTimeout)
	for len(out) < n && time.Now().Before(deadline) {
		out = append(out, deltaItems(readFrame(t, conn), workspace)...)
	}
	if len(out) < n {
		t.Fatalf("collected %d conversation items for workspace %s, want %d, before the deadline", len(out), workspace, n)
	}
	return out
}

// replayItems asks conn for a full replay (ResyncCmd from_seq 0) and returns
// every conversation item the daemon replays for workspace.
//
// The CommandAck for the resync is the terminator, and it is a sound one: the
// frontend server enqueues the ack only AFTER dispatching the command
// (frontend/server.go readLoop), and a ring-covered resync pushes its whole
// replay synchronously inside that dispatch (sessiondrv.Manager.Resync returns
// without a store re-pull when the ring covers the request).
func replayItems(t *testing.T, conn *websocket.Conn, workspace, requestID string) []*frontendv1.ConversationItem {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"resync":{"fromSeq":0}}`, requestID))
	var out []*frontendv1.ConversationItem
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if ack, ok := frame.GetFrame().(*frontendv1.FrontendFrame_CommandAck); ok && ack.CommandAck.GetRequestId() == requestID {
			if !ack.CommandAck.GetOk() {
				t.Fatalf("resync nacked: %s", ack.CommandAck.GetError())
			}
			return out
		}
		out = append(out, deltaItems(frame, workspace)...)
	}
	t.Fatalf("no CommandAck for resync %s arrived before the deadline", requestID)
	return nil
}

// isClear / isCompact identify the two first-class arms (frontend.proto 32/33).
func isClear(item *frontendv1.ConversationItem) bool   { return item.GetContextCleared() != nil }
func isCompact(item *frontendv1.ConversationItem) bool { return item.GetContextCompacted() != nil }

// seqItems drops the items a replay serves REGARDLESS of seq — the retained
// permission items (arm 30) and failure cards (arm 31), which are daemon-
// composed, carry no store seq, and are re-pushed on every resync by contract
// (sessiondrv/sinks.go consumer.resync). They are the only documented
// asymmetry between a live stream and a replay, so an equivalence check names
// and excludes exactly them.
func seqItems(items []*frontendv1.ConversationItem) []*frontendv1.ConversationItem {
	var out []*frontendv1.ConversationItem
	for _, item := range items {
		if item.GetPermission() != nil || item.GetSystemFailure() != nil {
			continue
		}
		out = append(out, item)
	}
	return out
}

// liveSession brings a session up, dials its scoped stream, waits for the shim
// to attach, and returns the daemon session id, the stream conn, the vendor
// session id events are stored under, and a store producer connection.
func liveSession(t *testing.T, h *e2eHarness, cwd string) (string, *websocket.Conn, string, *storeProducer) {
	t.Helper()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}
	vendorID := waitAttachedSession(t, conn, id, cwd)
	return id, conn, vendorID, dialStoreProducer(t)
}

// --- tests -----------------------------------------------------------------

// TestE2EInjectedClearReachesFrontendAsArm32 covers the LIVE path for a clear:
// a sidecar-shaped ContextCleared written to the real store arrives at a
// connected frontend as a ConversationItem carrying arm 32, keyed by the event's
// dedup key.
func TestE2EInjectedClearReachesFrontendAsArm32(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const lineUUID = "e2e-clear-line-1"

	// Act
	store.write(sidecarClearEvent(vendorID, lineUUID))

	// Assert
	item, _ := awaitItem(t, conn, cwd, "ContextCleared item", isClear)
	if got, want := item.GetUuid(), clearDedupKey(lineUUID); got != want {
		t.Errorf("clear item uuid = %q, want the event's dedup key %q", got, want)
	}
}

// TestE2EInjectedCompactCarriesSummaryAsArm33 covers the LIVE path for a
// compaction: the summary the file plane alone can supply survives the store,
// the shim, the daemon's curation, and the frontend wire intact on arm 33.
func TestE2EInjectedCompactCarriesSummaryAsArm33(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const summary = "The user asked for an e2e test; the agent wrote one."

	// Act
	store.write(sidecarCompactEvent(vendorID, "e2e-compact-boundary-1", summary))

	// Assert
	item, _ := awaitItem(t, conn, cwd, "ContextCompacted item", isCompact)
	if got := item.GetContextCompacted().GetSummary(); got != summary {
		t.Errorf("compaction summary = %q, want %q", got, summary)
	}
}

// TestE2EReplayFloorsAtTheClear covers the REPLAY FLOOR over the real
// processes: after conversation traffic and then a clear, a fresh frontend
// connection's replay starts AT the clear (inclusive) and carries nothing from
// above it.
func TestE2EReplayFloorsAtTheClear(t *testing.T) {
	// Arrange — real traffic first, so there is history for the floor to hide.
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, live, vendorID, store := liveSession(t, h, cwd)
	writeCmd(t, live, `{"requestId":"r-before","submitPrompt":{"text":"before the clear"}}`)
	const lineUUID = "e2e-clear-line-floor"
	clearUUID := clearDedupKey(lineUUID)
	// The turn's items must sit in the store BELOW the clear for the floor to
	// have anything to hide, so observe them live before injecting it.
	beforeClear := collectItems(t, live, cwd, 2)

	// Act — inject the clear, then wait until the live stream has shown it, so
	// the daemon has certainly recorded the floor before the replay is asked for.
	store.write(sidecarClearEvent(vendorID, lineUUID))
	awaitItem(t, live, cwd, "ContextCleared item", isClear)
	fresh := h.dial(t, id)

	// Assert
	replayed := seqItems(replayItems(t, fresh, cwd, "r-replay"))
	if len(replayed) == 0 {
		t.Fatal("the replay carried no conversation items at all")
	}
	if got := replayed[0]; !isClear(got) || got.GetUuid() != clearUUID {
		t.Fatalf("replay starts at item uuid=%q (%T), want the clear %q", got.GetUuid(), got.GetItem(), clearUUID)
	}
	preClear := map[string]bool{}
	for _, item := range beforeClear {
		preClear[item.GetUuid()] = true
	}
	for _, item := range replayed {
		if preClear[item.GetUuid()] {
			t.Errorf("replay carried item uuid=%q from ABOVE the clear", item.GetUuid())
		}
	}
}

// TestE2ELiveAndReplayAgreeFromTheFloor covers LIVE-VS-REPLAY EQUIVALENCE: the
// item sequence a live subscriber saw from the floor onward is the prefix of
// what a fresh post-hoc subscriber replays, item for item.
//
// Permission items and failure cards are excluded (seqItems): they are the only
// items the contract replays regardless of seq. Nothing else is excluded — both
// paths run the SAME curator over the SAME retained event
// (sessiondrv consumer.pushConversation, whose `live` flag only gates a log
// line), so the items themselves must compare equal field for field.
func TestE2ELiveAndReplayAgreeFromTheFloor(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, live, vendorID, store := liveSession(t, h, cwd)
	writeCmd(t, live, `{"requestId":"r-before","submitPrompt":{"text":"before the clear"}}`)

	// Act — clear (the floor), then more real traffic above it, observed live.
	store.write(sidecarClearEvent(vendorID, "e2e-clear-line-equiv"))
	clearItem, _ := awaitItem(t, live, cwd, "ContextCleared item", isClear)
	writeCmd(t, live, `{"requestId":"r-after","submitPrompt":{"text":"after the clear"}}`)
	fromFloor := append([]*frontendv1.ConversationItem{clearItem}, collectItems(t, live, cwd, 2)...)
	fromFloor = seqItems(fromFloor)

	// Assert
	replayed := seqItems(replayItems(t, h.dial(t, id), cwd, "r-replay"))
	if len(replayed) < len(fromFloor) {
		t.Fatalf("replay carried %d items, want at least the %d the live subscriber saw from the floor", len(replayed), len(fromFloor))
	}
	for i, want := range fromFloor {
		if got := replayed[i]; !proto.Equal(got, want) {
			t.Errorf("replayed item %d = %v, want the live item %v", i, got, want)
		}
	}
}

// TestE2EDuplicateClearYieldsOneFrame covers DEDUP: the same event written
// twice under the same dedup key is one row, one seq, and one frame.
//
// The duplicate is chased with a DISTINCT sentinel compaction: the store
// assigns seq in arrival order and every hop below it preserves that order, so
// a duplicate that had been accepted would necessarily arrive before the
// sentinel. Seeing the sentinel is therefore proof the duplicate is not merely
// late.
func TestE2EDuplicateClearYieldsOneFrame(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, vendorID, store := liveSession(t, h, cwd)
	const lineUUID = "e2e-clear-line-dup"

	// Act
	first := store.write(sidecarClearEvent(vendorID, lineUUID))
	second := store.write(sidecarClearEvent(vendorID, lineUUID))
	store.write(sidecarCompactEvent(vendorID, "e2e-compact-sentinel", "sentinel"))

	// Assert — the store deduped it, and exactly one clear frame reached the
	// frontend by the time the sentinel did.
	if first.GetAccepted() != 1 || first.GetDeduped() != 0 {
		t.Errorf("first write acked accepted=%d deduped=%d, want 1/0", first.GetAccepted(), first.GetDeduped())
	}
	if second.GetAccepted() != 0 || second.GetDeduped() != 1 {
		t.Errorf("duplicate write acked accepted=%d deduped=%d, want 0/1", second.GetAccepted(), second.GetDeduped())
	}
	_, beforeSentinel := awaitItem(t, live, cwd, "sentinel ContextCompacted item", isCompact)
	clears := 0
	for _, item := range beforeSentinel {
		if isClear(item) && item.GetUuid() == clearDedupKey(lineUUID) {
			clears++
		}
	}
	if clears != 1 {
		t.Errorf("frontend saw %d clear items for dedup key %q, want exactly 1", clears, clearDedupKey(lineUUID))
	}
}
