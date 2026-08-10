// A PERMISSION QUESTION ACROSS A DAEMON BOUNCE.
//
// A canUseTool question is a RENDEZVOUS BETWEEN TWO PROCESSES, and both halves
// of it die with the daemon:
//
//   - THE UNDELIVERED HALF. The shim raises the question by calling
//     `sendPermissionRequest` (uds/server.ts), which DROPS the request outright
//     when no daemon is attached: one error line, and the request is gone.
//     Nothing re-asks it, so the SDK callback the fake is awaiting never
//     resolves and the turn waits forever.
//   - THE DELIVERED-UNANSWERED HALF. The question reached the daemon and became
//     a pending rendezvous in the daemon's own memory (sessioncontroller's
//     permission map). The daemon dies; the rendezvous dies with it. The shim's
//     `control.pending` entry survives — pending waits are deliberately NOT
//     cancelled on disconnect (uds-session.ts onDaemonDisconnected: "a
//     reattaching daemon can still answer them") — but nothing on either side
//     re-arms it, so the answer the shim is still waiting for can never be
//     given.
//
// EITHER WAY THE USER'S TURN HANGS, permanently, with a workspace that looks
// busy. What must be true instead: on reattach the question is RE-ASKED (or
// re-armed) and is ANSWERABLE, and answering it lets the turn proceed.
//
// NOTHING HERE WAITS FOREVER. That is deliberate and load-bearing: the present
// behaviour of both halves IS an indefinite wait, so every assertion below is
// bounded by the suite's frame budget and fails naming the guarantee that is
// missing rather than hanging the package.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EAPermissionRaisedWhileTheDaemonIsDownIsRedeliveredOnReattach covers
// THE UNDELIVERED HALF.
//
// THE ARRANGEMENT. The daemon is taken away at the earliest moment it can
// itself observe the turn — the pushed WorkspaceState carrying turn_active,
// which is the daemon's receipt that the prompt was delivered and the turn
// claimed — and BEFORE any permission item has been seen. The shim's
// `canUseTool` call for that turn therefore lands with no daemon attached and
// is dropped by sendPermissionRequest.
//
// WHY THE TEST IS HONEST EITHER WAY. If a run is unlucky and the request was
// already delivered before the teardown completed, the arrangement degenerates
// into the delivered-unanswered half — whose guarantee is the SAME assertion
// below, and which is equally absent today. The test therefore cannot pass by
// missing its window; it can only pass when a question raised across the gap
// is genuinely re-asked and answerable.
func TestE2EAPermissionRaisedWhileTheDaemonIsDownIsRedeliveredOnReattach(t *testing.T) {
	// Arrange — a live session and a tool turn whose question has not yet
	// reached any frontend.
	// Tempdirs before the world: cleanups run LIFO, so this tears the daemons
	// and their shims down before the directories are removed.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	submitPrompt(t, conn, "r-tool", "!tool sleep e2e-permission-in-gap")
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the daemon's observation of the turn (turn_active), taken BEFORE any permission item": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, cwd)
			return state != nil && state.GetTurnActive()
		},
	})

	// Act — the daemon goes away while the shim is asking.
	first.bounce()
	second := world.boot(t)
	_, afterConn, _ := reattached(t, second, cwd)

	// Assert — the question is asked again, is answerable, and the turn runs on.
	permID := awaitReattachedPendingPermission(t, afterConn, cwd,
		"the parked canUseTool question, RE-ASKED to the daemon that replaced the one it was raised against — a request the shim dropped for want of a listener is not a request the user never made, and the turn behind it cannot proceed until it is put again")
	answerPermission(t, afterConn, cwd, "r-answer", permID, true)
	awaitSettledTurn(t, afterConn, cwd, nil,
		"a WorkspaceState settling the turn whose question was answered after the bounce")
}

// TestE2EAPermissionDeliveredBeforeTheBounceIsAnswerableAfterIt covers THE
// DELIVERED-UNANSWERED HALF, and its arrangement is fully deterministic: the
// question is OBSERVED at a frontend (so delivery is a fact, not a hope) and
// only then is the daemon destroyed, taking the in-memory rendezvous with it.
//
// The shim is still parked on that exact request. The successor must re-arm the
// rendezvous — the pending question is the shim's, and it outlived the process
// that was holding the other end of it.
func TestE2EAPermissionDeliveredBeforeTheBounceIsAnswerableAfterIt(t *testing.T) {
	// Arrange — a question that provably reached a frontend.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	askQuestion(t, conn, cwd, "r-ask", "sleep e2e-permission-rendezvous")

	// Act — destroy the rendezvous by destroying the daemon that held it.
	first.bounce()
	second := world.boot(t)
	_, afterConn, _ := reattached(t, second, cwd)

	// Assert — the surviving question is presented again and can be answered.
	//
	// The id is re-read rather than carried across the bounce on purpose: the
	// contract is that the SUCCESSOR presents an answerable question, and a test
	// that reused the dead daemon's id would be asserting an implementation
	// detail of how the re-arming is keyed.
	permID := awaitPendingPermission(t, afterConn, cwd,
		"the still-parked canUseTool question, RE-ARMED by the successor daemon — the shim never cancelled its pending wait (a reattaching daemon may still answer it), so the answer must be reachable again rather than lost with the process that was holding the other end")
	answerPermission(t, afterConn, cwd, "r-answer", permID, true)
	awaitSettledTurn(t, afterConn, cwd, nil,
		"a WorkspaceState settling the turn whose surviving question was answered after the bounce")
}
