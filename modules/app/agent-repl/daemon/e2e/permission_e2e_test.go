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

	corev1 "agentrepl/proto/agentshim/core/v1"
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

// askQuestion submits a `!tool` prompt and returns once BOTH authoritative
// consequences have reached the frontend: the PENDING permission item and the
// SSM-resolved PERMISSION state. Their producers are independent and either
// may arrive first, so one await must retain both observations rather than
// consuming whichever came first while waiting only for the other.
func askQuestion(t *testing.T, conn *websocket.Conn, workspace, requestID, command string) string {
	t.Helper()
	var permID string
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"submitPrompt":{"text":"!tool %s","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, requestID, command))
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
		"a WorkspaceState the SSM resolved to PERMISSION": func(frame *frontendv1.FrontendFrame) bool {
			return ssmResolved(frame, workspace, frontendv1.RenderState_RENDER_STATE_PERMISSION, "permission")
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

	// Act / Assert — a turn that stops to ask, with both its item and
	// authoritative state observed regardless of arrival order.
	askQuestion(t, conn, cwd, "r-ask", "ls e2e-permission")
}

// isDeniedPermission identifies a permission item the daemon resolved as a
// DECLINE (sessioncontroller HandlePermission pushes RESOLUTION_DENIED and then
// answers the shim with nothing — the stop is the delivery).
func isDeniedPermission(item *frontendv1.ConversationItem) bool {
	perm := item.GetPermission()
	return perm != nil && perm.GetResolution() == corev1.PermissionItem_RESOLUTION_DENIED
}

// declineObservation is what a declined permission produced on the frontend.
type declineObservation struct {
	denied *corev1.PermissionItem
	window *frontendv1.InterruptWindow
	state  *frontendv1.WorkspaceState
}

// awaitDecline waits for all three consequences a decline owes: the DENIED
// resolution on the question's own uuid, the footer's interrupt window, and the
// workspace resolving to INTERRUPTED. All three rather than whichever lands
// first, because the claim under test is that a decline means what an interrupt
// means, and any one of them alone is consistent with it meaning less.
func awaitDecline(t *testing.T, conn *websocket.Conn, workspace, permID string) declineObservation {
	t.Helper()
	var obs declineObservation
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the DENIED resolution of the declined question": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, workspace) {
				if item.GetUuid() == permID && isDeniedPermission(item) {
					obs.denied = item.GetPermission()
					return true
				}
			}
			return false
		},
		"a ProgressView carrying an OPEN interrupt window": func(frame *frontendv1.FrontendFrame) bool {
			view := progressFor(frame, workspace)
			if !view.GetInterrupt().GetActive() {
				return false
			}
			obs.window = view.GetInterrupt()
			return true
		},
		"a WorkspaceState resolving the declined turn to INTERRUPTED": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, workspace)
			if state.GetState() != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
				return false
			}
			obs.state = state
			return true
		},
	})
	return obs
}

// TestE2EDeclinedPermissionStopsTheTurn covers THE DECLINE: answering no is the
// user taking the session back, so the turn that asked ENDS — with the footer's
// interrupt window open and the workspace resolved to INTERRUPTED, exactly as a
// typed stop leaves it — rather than the agent reading the denial as a tool
// result and carrying on.
func TestE2EDeclinedPermissionStopsTheTurn(t *testing.T) {
	// Arrange — a real parked question.
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	permID := askQuestion(t, conn, cwd, "r-ask", "ls e2e-decline")

	// Act — the human declines it.
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":"r-decline","workspace":%q,"permissionAnswer":{"permissionRequestId":%q,"allow":false,"denyMessage":"not that one"}}`,
		cwd, permID))

	// Assert.
	obs := awaitDecline(t, conn, cwd, permID)
	if got := obs.window.GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		t.Errorf("interrupt window outcome = %s, want INTERRUPTED: a decline stops the turn that asked", got)
	}
	if msg := obs.denied.GetDenyMessage(); msg != "not that one" {
		t.Errorf("recorded deny message = %q, want the reason the user gave", msg)
	}
}

// TestE2EPromptOverAParkedPermissionDeclinesIt covers THE IMPLIED DECLINE: a
// prompt typed while a question is parked IS the answer, so it declines and
// stops before it is submitted — the same three consequences, provoked by a
// submitPrompt rather than by a permissionAnswer.
func TestE2EPromptOverAParkedPermissionDeclinesIt(t *testing.T) {
	// Arrange — a real parked question.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	permID := askQuestion(t, conn, cwd, "r-ask", "ls e2e-superseded")

	// Act — the user types instead of answering.
	writeCmd(t, conn, `{"requestId":"r-next","submitPrompt":{"text":"never mind, do this","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — the question was declined and the turn stopped; the prompt then
	// runs as the paused queue's one deliverable (covered by the interrupted-
	// queue test in interrupt_e2e_test.go, whose path this joins).
	obs := awaitDecline(t, conn, cwd, permID)
	if got := obs.window.GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		t.Errorf("interrupt window outcome = %s, want INTERRUPTED: the prompt declined the parked question and stopped its turn", got)
	}
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
