// The two CONTEXT-CUT PHASE WORDS, end to end over the REAL processes: the
// workspace resolving to RENDER_STATE_COMPACTING and RENDER_STATE_CLEARING and
// back out of them, observed as WorkspaceState frames on a frontend WebSocket.
//
// WHY THE COMPACTION'S OPENING EDGE IS INJECTED. It is the vendor's own
// `status="compacting"` stream message, which in a live session the CLI emits
// and the shim's converter forwards. The fake engine has no compaction and so
// emits no such status (fake-query.ts has no compaction path at all), which
// means provoking one would mean teaching the fake to compact — i.e. faking
// the precondition rather than exercising it. The test therefore writes the
// event the converter would have written into the real store and exercises
// everything downstream for real: store ingest, store fan-out, the shim's
// merged-stream forward, the daemon's consumer, the SSM's axis resolution, and
// the pushed frontend frame.
//
// THE CLEAR'S OPENING EDGE IS NOT INJECTED. It is the daemon's own — a `/clear`
// dispatched through the real frontend command surface — so it is driven the
// way a user drives it, over the WebSocket.
//
// Both CLOSING edges are the first-class file-plane events, injected exactly as
// clearcompact_e2e_test.go injects them (see its header for why the sidecar
// cannot run here).
//
// This file shares e2e_test.go's package and reuses its helpers READ-ONLY
// (newUDSHarness, dial, readFrame, writeCmd, frameTimeout), plus
// clearcompact_e2e_test.go's liveSession / dialStoreProducer /
// sidecarClearEvent / sidecarCompactEvent and interrupt_e2e_test.go's
// awaitAll / workspaceStateFor.
//
// NOT COVERED — the clearing watchdog's expiry. Its bound is a Manager option
// the e2e harness constructs the SSM without a seam for, and the expiry is a
// pure SSM edge with no frontend contribution of its own; it is covered
// deterministically in internal/ssm/contextcut_test.go against a captured
// timer, which is a stronger test than waiting out a real deadline here.
package e2e

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/types/known/anypb"
)

// vendorStatusEvent is what the shim's converter forwards for the CLI's
// compaction ticker: a stream-plane ClaudeStreamMessage carrying the
// StatusMessage arm (data/v1 stream.proto arm 7). An empty status is the
// vendor's null — the window closing — not an absent field.
func vendorStatusEvent(t *testing.T, vendorSessionID, dedupKey, status string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Status{Status: &datav1.StatusMessage{Status: status}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_STREAM,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		DedupKey:     dedupKey,
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

// awaitRenderState reads frames until the workspace resolves to want.
func awaitRenderState(t *testing.T, conn *websocket.Conn, workspace string, want frontendv1.RenderState) {
	t.Helper()
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState resolving to " + want.String(): func(frame *frontendv1.FrontendFrame) bool {
			return workspaceStateFor(frame, workspace).GetState() == want
		},
	})
}

// awaitRenderStateLeaves reads frames until the workspace resolves to anything
// OTHER than unwanted. It is the closing assertion: what the workspace lands on
// afterwards is the other axes' business, and pinning it here would assert the
// session-status lifecycle rather than the cut.
func awaitRenderStateLeaves(t *testing.T, conn *websocket.Conn, workspace string, unwanted frontendv1.RenderState) {
	t.Helper()
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState off " + unwanted.String(): func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, workspace)
			return state != nil && state.GetState() != unwanted
		},
	})
}

func TestE2ECompactingPhaseWordOpensOnTheVendorStatusAndClosesOnTheEvent(t *testing.T) {
	// Arrange — a live session.
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)

	// Act — the vendor opens its compaction window.
	store.write(vendorStatusEvent(t, vendorID, "e2e-status-compacting", "compacting"))

	// Assert — the phase word, not merely the color.
	awaitRenderState(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_COMPACTING)

	// Act — the file plane reports the compaction that actually happened.
	store.write(sidecarCompactEvent(vendorID, "e2e-compact-phaseword", "what the discarded history said"))

	// Assert — the window closed on its own completion, with no timeout and no
	// turn boundary needed.
	awaitRenderStateLeaves(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_COMPACTING)
}

func TestE2EClearingPhaseWordOpensOnDispatchAndClosesOnTheEvent(t *testing.T) {
	// Arrange.
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)

	// Act — the user dispatches the clear over the real command surface. The
	// daemon is the ONLY place that knows a clear has begun: nothing in the
	// event stream announces one until it has already finished.
	writeCmd(t, conn, `{"requestId":"r-clear","submitPrompt":{"text":"/clear","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert.
	awaitRenderState(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_CLEARING)

	// Act — the clear completes and the file plane says so.
	store.write(sidecarClearEvent(vendorID, "e2e-clear-phaseword"))

	// Assert — released by its own event rather than by the watchdog.
	awaitRenderStateLeaves(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_CLEARING)
}
