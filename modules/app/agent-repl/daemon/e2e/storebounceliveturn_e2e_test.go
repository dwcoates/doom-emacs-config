// A STORE BOUNCE UNDER A LIVE TURN.
//
// THE GAP. `bin/deploy-all.sh` kickstarts the shim-store
// (`launchctl kickstart -k gui/501/com.agentrepl.shim-store`), which takes the
// store process away under every live shim. The store is a SEPARATE process
// from the session: its restart is not a fact about anybody's conversation.
// But the shim awaits a store receipt for each persistent SDK event INSIDE the
// pump's `for await` (uds-session.ts routeSdkMessage), so any rejected write
// left the loop body and was caught by the pump's own catch as an ITERATOR
// FAILURE — reported to the daemon as `unexpected_query_termination` and
// exiting the shim nonzero. The user saw "the agent SDK query ended
// unexpectedly / store-client: write on a down connection" mid-turn, every
// time the backend was bounced.
//
// WHAT MUST BE TRUE INSTEAD. A full backend bounce is seamless. Writes made
// during the outage are held (and fsynced to the workspace's spill journal),
// flushed onto the restored link, and the turn finishes the ordinary way. The
// query never ends, and the user is never shown a failure card for it.
//
// HOW THE ARRANGEMENT IS MADE A FACT RATHER THAN A RACE. The turn is held open
// on a real parked canUseTool — `askQuestion` returns only once the PENDING
// permission item AND the SSM's PERMISSION resolution have both reached the
// frontend, so the turn is provably in flight when the store is taken away.
// The store bounce is a real process death and a real replacement bound to the
// same socket over the same events database (shimStoreProc.bounce), not a
// simulated disconnect. Nothing here sleeps.
//
// Shares e2e_test.go's package and reuses its helpers READ-ONLY (newUDSHarness,
// liveSession, askQuestion, answerPermission, awaitSettledTurn,
// awaitStoreTurnEndedFor, degradedCardIn).
package e2e

import (
	"fmt"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// rejectShimSDKFailureCard fails the moment the frontend is shown a failure
// card filed under the shim's SDK component for this workspace.
//
// It is the negative half of the contract, enforced over the same read loop
// rather than by waiting and then looking: "the turn settled DONE eventually"
// is satisfied by a session that reported its query dead first and recovered
// afterwards, and a query killed by a store restart is exactly the defect.
func rejectShimSDKFailureCard(workspace, why string) func(*frontendv1.FrontendFrame) string {
	return func(frame *frontendv1.FrontendFrame) string {
		item := degradedCardIn(frame, workspace, "claude-shim-sdk")
		if item == nil {
			return ""
		}
		return fmt.Sprintf("workspace %s was shown a claude-shim-sdk failure card (%s): %s",
			workspace, protoText(item.GetFailureCard()), why)
	}
}

// TestE2EAStoreBounceUnderALiveTurnDoesNotEndTheQuery is the regression itself:
// the store dies and is replaced while a turn is in flight, and the turn
// finishes.
func TestE2EAStoreBounceUnderALiveTurnDoesNotEndTheQuery(t *testing.T) {
	// Arrange — a live turn, parked on a real permission question. The
	// workspace tempdir is created BEFORE the harness on purpose: cleanups run
	// LIFO, so this tears the shims down before the directory is removed.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, vendorID, _ := liveSession(t, h, cwd)
	permID := askQuestion(t, conn, cwd, "r-ask", "sleep e2e-store-bounce-live-turn")

	// Act — the deploy's store bounce lands with the turn in flight, and the
	// answer then releases the turn to run its tail over the restored link.
	h.store.bounce()
	answerPermission(t, conn, cwd, "r-answer", permID, true)

	// Assert — the turn settles the ordinary way, with no failure card for the
	// query the store restart used to kill.
	awaitSettledTurn(t, conn, cwd,
		rejectShimSDKFailureCard(cwd, "a shim-store restart is not a fact about this conversation: writes made during the outage are held and flushed onto the restored link, so the SDK query must outlive the bounce"),
		"a WorkspaceState settling the bounced turn as DONE under turn_ended")

	// ...and the turn's end is DURABLE, read back from the replacement store
	// over the same events database, so the evidence written across the outage
	// genuinely landed rather than merely being reported as fine.
	if !awaitStoreTurnEndedFor(t, vendorID, "") {
		t.Fatalf("the store holds no durable TurnEnded for conversation %s after the bounce: the turn's evidence did not survive the store restart", vendorID)
	}
}
