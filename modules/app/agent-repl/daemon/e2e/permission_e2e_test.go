// The PERMISSION RENDER STATE over the REAL processes: the real daemon, the
// real TS shim running `--fake` offline, the real store, and a real frontend
// connection on the scoped /stream socket.
//
// WHAT THIS IS PROVING. RENDER_STATE_PERMISSION was reachable on every render
// plane and produced by nothing: `permission` appeared only inside the SSM's
// own resolution vocabulary, so a workspace parked on a canUseTool question
// kept the `thinking` row of the turn that asked it and painted RED — busy —
// at the one moment the agent was doing nothing but waiting on the user. These
// tests drive a real question through the whole stack and assert the color the
// workspace resolves to while it stands, and the one it returns to when it is
// answered.
//
// HOW A REAL QUESTION IS PROVOKED. The fake engine's `!tool <command>` turn
// AWAITS canUseTool (fake-query.ts runToolTurn), which travels to the daemon as
// a PermissionRequest and parks the daemon's rendezvous until something
// answers. Nothing here injects a permission through an internal seam: the
// question is the shim's, asked over its own control path, and the answer is a
// real PermissionAnswerCmd off the frontend socket.
//
// WHY THE ASSERTION IS ON cause_kind AND NOT ONLY THE STATE. The permission
// handler ALSO pushes a bare WorkspaceState carrying the PERMISSION enum
// directly (sessioncontroller/sessioncontroller.go HandlePermission), predating the SSM producer
// and independent of it. A test that matched on the enum alone would pass with
// the producer deleted. The SSM's own push is the one carrying a resolved
// cause_kind — `permission:pending=N` — so that is what these match, and the
// direct push cannot satisfy it.
//
// These tests share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, dial, readFrame, writeCmd), plus clearcompact_e2e_test.go's
// liveSession and interrupt_e2e_test.go's awaitAll / isPendingPermission.
package e2e

import (
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// ssmResolved reports whether a frame is the SSM's OWN resolution of the
// workspace to state — a WorkspaceState carrying a cause kind, which the
// permission handler's direct push never sets.
func ssmResolved(frame *frontendv1.FrontendFrame, workspace string, state frontendv1.RenderState, causePrefix string) bool {
	st := workspaceStateFor(frame, workspace)
	return st.GetState() == state && strings.HasPrefix(st.GetCauseKind(), causePrefix)
}

// askQuestion submits a `!tool` prompt and returns once the shim's canUseTool
// has reached the frontend as a PENDING permission item, handing back that
// item's uuid — which IS the permission request id the answer is keyed by.
func askQuestion(t *testing.T, conn *websocket.Conn, workspace, requestID, command string) string {
	t.Helper()
	var permID string
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"submitPrompt":{"text":"!tool %s"}}`, requestID, command))
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a PENDING permission item (the parked question)": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, workspace) {
				if isPendingPermission(item) {
					permID = item.GetUuid()
					return true
				}
			}
			return false
		},
	})
	return permID
}

// TestE2EPendingPermissionResolvesThePermissionState covers THE OPENING EDGE: a
// real parked canUseTool resolves the workspace to RENDER_STATE_PERMISSION,
// through the SSM, on the daemon's own resolution rather than a hand-pushed
// enum.
func TestE2EPendingPermissionResolvesThePermissionState(t *testing.T) {
	// Arrange
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)

	// Act — a turn that stops to ask.
	askQuestion(t, conn, cwd, "r-ask", "ls e2e-permission")

	// Assert — green, and the agent is waiting on the user.
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState the SSM resolved to PERMISSION": func(frame *frontendv1.FrontendFrame) bool {
			return ssmResolved(frame, cwd, frontendv1.RenderState_RENDER_STATE_PERMISSION, "permission")
		},
	})
}

// TestE2EAnsweredPermissionReturnsToThinking covers THE CLOSING EDGE: answering
// the question releases the row and hands the axis back to the turn that asked,
// which is still in flight — so the workspace goes green→red→settled rather
// than sitting green over a running turn.
func TestE2EAnsweredPermissionReturnsToThinking(t *testing.T) {
	// Arrange — a real question, parked, with the workspace resolved green.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	permID := askQuestion(t, conn, cwd, "r-ask", "ls e2e-permission")
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState the SSM resolved to PERMISSION": func(frame *frontendv1.FrontendFrame) bool {
			return ssmResolved(frame, cwd, frontendv1.RenderState_RENDER_STATE_PERMISSION, "permission")
		},
	})

	// Act — the human allows it.
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":"r-answer","workspace":%q,"permissionAnswer":{"permissionRequestId":%q,"allow":true}}`, cwd, permID))

	// Assert — the turn resumes under its own row, and runs to completion.
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState back in THINKING with the turn active": func(frame *frontendv1.FrontendFrame) bool {
			st := workspaceStateFor(frame, cwd)
			return st.GetState() == frontendv1.RenderState_RENDER_STATE_THINKING && st.GetTurnActive()
		},
		"a WorkspaceState settling the answered turn": func(frame *frontendv1.FrontendFrame) bool {
			return ssmResolved(frame, cwd, frontendv1.RenderState_RENDER_STATE_DONE, "turn_ended")
		},
	})
}
