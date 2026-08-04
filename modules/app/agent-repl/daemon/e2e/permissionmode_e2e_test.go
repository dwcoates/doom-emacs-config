// THE SESSION'S PERMISSION POSTURE, end to end over the REAL processes.
//
// The defect: a session created with NO permission_mode (the
// workspace-generation inbox path) spawned a shim with no --permission-mode
// flag, whose argv parser then defaulted to "default" — prompting mode — so the
// generated first prompt ran waiting for an answer nobody was there to give.
// Emacs-created sessions carried "auto", which is why the hole showed only on
// inbox-created ones.
//
// WHY THIS IS AN E2E RATHER THAN A UNIT TEST. The claim spans three processes
// and two channels: a record with no mode, the daemon's resolution of it, the
// bring-up gate that carries it, and the shim's application of it to the SDK
// query. Every unit test on that path can pass while the assembled system still
// runs the first turn in the wrong mode — which is precisely what happened.
//
// THE RECEIPT is the fake query's own echo: its reply is
// `echo: <prompt> [mode=<permissionMode>]` (fake-query.ts runTextTurn), so the
// mode the SDK query is actually in comes back in the answer text. Nothing
// about it is a proxy.
//
// Shares e2e_test.go's package and reuses its helpers READ-ONLY
// (newUDSHarness, createSessionWithMode, dial, readFrame, writeCmd,
// frameTimeout) plus clearcompact_e2e_test.go's waitAttachedSession/awaitItem
// and interrupt_e2e_test.go's assistantText/echoOf.
package e2e

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// echoedMode runs one prompt through a live session and returns the mode the
// fake query reported in its reply.
func echoedMode(t *testing.T, conn *websocket.Conn, cwd, prompt string) string {
	t.Helper()
	writeCmd(t, conn, `{"requestId":"r-mode","submitPrompt":{"text":"`+prompt+`","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	marker := echoOf(prompt)
	item, _ := awaitItem(t, conn, cwd, "the fake's echoed reply", func(item *frontendv1.ConversationItem) bool {
		return strings.Contains(assistantText(item), marker)
	})
	text := assistantText(item)
	mode := text[strings.Index(text, marker)+len(marker):]
	if end := strings.Index(mode, "]"); end >= 0 {
		mode = mode[:end]
	}
	return mode
}

// liveSessionInMode brings up a session created with permissionMode (empty
// omits the field) and returns its scoped stream plus the workspace.
func liveSessionInMode(t *testing.T, h *e2eHarness, permissionMode string) (*websocket.Conn, string) {
	t.Helper()
	cwd := t.TempDir()
	id := h.createSessionWithMode(t, cwd, permissionMode)
	conn := h.dial(t, id)
	first := readFrame(t, conn)
	if first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}
	waitAttachedSession(t, first.GetSnapshot(), conn, id, cwd)
	return conn, cwd
}

// TestE2EASessionCreatedWithNoModeRunsItsFirstTurnInAuto is the fix itself: an
// EMPTY permission_mode on the create resolves to "auto" all the way into the
// SDK query, so the first turn is never a prompting one nobody can answer.
func TestE2EASessionCreatedWithNoModeRunsItsFirstTurnInAuto(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	conn, cwd := liveSessionInMode(t, h, "")

	// Act
	mode := echoedMode(t, conn, cwd, "what mode am I in")

	// Assert
	if mode != "auto" {
		t.Fatalf("the SDK query reported mode %q for a session created with NO permission_mode, want auto", mode)
	}
}

// TestE2EAnExplicitModeOnCreateReachesTheSDKQuery is the other half: the fix
// must not flatten every session to the default. An Emacs-style create naming
// a mode still gets exactly that mode.
func TestE2EAnExplicitModeOnCreateReachesTheSDKQuery(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	conn, cwd := liveSessionInMode(t, h, "plan")

	// Act
	mode := echoedMode(t, conn, cwd, "what mode am I in")

	// Assert
	if mode != "plan" {
		t.Fatalf("the SDK query reported mode %q for a session created as plan, want plan", mode)
	}
}
