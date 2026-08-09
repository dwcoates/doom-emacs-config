// A TURN THAT FINISHED WHILE THE DAEMON WAS GONE.
//
// THE GAP. A shim outlives its daemon and keeps running. If the turn it was
// running FINISHES during the gap, the store holds the TurnEnded durably and
// the shim comes back reporting no turn in flight — while the durable claim
// ledger, written by the daemon that died, still says the workspace owns a
// turn. reconcileTurnHandshake (sessioncontroller/turnlifecycle.go) resolves
// that contradiction by believing the SHIM'S HELLO and CUTTING the claim as
// `interrupted_by_restart`, so a turn the user's agent completed successfully
// is reported to them as one the restart killed. The store's own durable
// record — which says the turn ENDED, cleanly, with a result — is never
// consulted.
//
// WHAT MUST BE TRUE INSTEAD. The reconciliation judges the returning shim's
// claim against the STORE'S DURABLE RECORD. A claim the store has an end for
// settles as COMPLETED; only a claim with no durable end is a phantom the
// restart cut.
//
// HOW THE GAP IS ARRANGED. The turn is held open on a real parked canUseTool
// (the only way a fake-mode turn is catchable mid-flight — see
// interrupt_e2e_test.go's header) and the question is ANSWERED. The bounce is
// then fired on the daemon's OWN record that the response has left it for the
// shim (`sent permission response`, watchLog), and on nothing after that. The
// answer releases the shim's turn, which runs its tail — tool result, closing
// message, result, TurnEnded — as several store round-trips while the daemon it
// was talking to is being torn down, so the store keeps the end and the dying
// daemon never delivers it.
//
// WHY THE RELEASE NEEDS ITS OWN RENDEZVOUS. The daemon dispatches a permission
// response from a goroutine parked inside HandlePermission, and that goroutine
// DROPS the response when the connection's context is already cancelled
// ("connection gone before permission response", shimclient/events.go). A test
// that answered and bounced in the same breath lost that race every time: the
// shim stayed parked on a question nobody answered, and the turn these tests are
// about never ran at all.
//
// THE ARRANGEMENT IS CHECKED, NOT ASSUMED. Before asserting anything about the
// reattach, each test reads the STORE and requires a durable TurnEnded to
// exist: that is the premise ("the shim finished") stated as a fact about the
// shared record rather than as a hope about scheduling. The read is a bounded
// WAIT, because the tail is still being written while the daemon comes down, and
// a single sample would report a shim that finishes a millisecond later as one
// that never finished. A run in which the shim did not finish fails there,
// naming the arrangement, instead of passing vacuously.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EATurnCompletedDuringTheGapSettlesCompletedNotInterrupted covers THE
// COMPLETED-DURING-THE-GAP EDGE: the returning shim's "no turn in flight" is
// reconciled against the store's durable end, so the turn settles DONE and the
// workspace is never painted INTERRUPTED.
func TestE2EATurnCompletedDuringTheGapSettlesCompletedNotInterrupted(t *testing.T) {
	// Arrange — a live turn, parked on a real permission question.
	// Tempdirs before the world: cleanups run LIFO, so this tears the daemons
	// and their shims down before the directories are removed.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, vendorID, _ := liveSession(t, first.harness(), cwd)
	permID := askQuestion(t, conn, cwd, "r-ask", "sleep e2e-completed-in-gap")

	// Act — release the turn and take the daemon away while its tail runs.
	//
	// THE BOUNCE IS SEQUENCED ON THE ANSWER LEAVING THE DAEMON, and on nothing
	// after it. A bounce fired straight after the write raced the daemon's own
	// dispatch of the answer and won every time — the response was dropped
	// ("connection gone before permission response"), the shim stayed parked,
	// and the turn this test is about never ran. Waiting for the daemon's own
	// record that the response was sent makes the RELEASE a fact while leaving
	// the whole tail — tool result, closing message, result, TurnEnded — to race
	// the teardown, which is the gap.
	sent := first.watchLog("sent permission response")
	answerPermission(t, conn, cwd, "r-answer", permID, true)
	awaitLogged(t, sent, "the permission response leaving the daemon for the shim, which is what releases the parked turn")
	first.bounce()
	second := world.boot(t)

	// The premise, read from the shared durable record: the shim genuinely
	// finished the turn with no daemon there to see it.
	if !awaitStoreTurnEndedFor(t, vendorID, "") {
		t.Fatalf("the store holds no durable TurnEnded for conversation %s, so the shim did not finish its turn during the gap and this run never reproduced the completed-during-the-gap arrangement", vendorID)
	}

	// Assert — the reattach settles the turn as completed and never cuts it.
	_, afterConn, _ := reattached(t, second, cwd)
	awaitSettledTurn(t, afterConn, cwd,
		rejectRestartInterrupted(cwd, "the shim FINISHED this turn while the daemon was gone and the store holds its durable end; a handshake that judges the returning shim against that record settles it COMPLETED, and only a claim with no durable end may be cut as interrupted_by_restart"),
		"a WorkspaceState settling the gap-completed turn as DONE under turn_ended")
}

// TestE2EATurnEndedDuringTheGapReachesTheFrontendAfterReattach covers THE
// DELIVERY EDGE: the turn's own terminal RESULT ITEM — the last conversation
// item a turn produces — reaches a frontend that only ever connected to the
// SUCCESSOR daemon, off the store replay the reattach opens.
//
// It is a different fact from the settlement above: a workspace can resolve
// DONE off the reconciliation while the conversation the user reads is missing
// the turn's answer entirely.
func TestE2EATurnEndedDuringTheGapReachesTheFrontendAfterReattach(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, vendorID, _ := liveSession(t, first.harness(), cwd)
	permID := askQuestion(t, conn, cwd, "r-ask", "sleep e2e-terminal-in-gap")

	// Act — same arrangement as the settlement test above, sequenced on the same
	// record and for the same reason: an answer that never left the daemon
	// leaves the shim parked, and a turn that never ran has no terminal item to
	// deliver.
	sent := first.watchLog("sent permission response")
	answerPermission(t, conn, cwd, "r-answer", permID, true)
	awaitLogged(t, sent, "the permission response leaving the daemon for the shim, which is what releases the parked turn")
	first.bounce()
	second := world.boot(t)
	if !awaitStoreTurnEndedFor(t, vendorID, "") {
		t.Fatalf("the store holds no durable TurnEnded for conversation %s, so this run never reproduced the ended-during-the-gap arrangement", vendorID)
	}

	// Assert — the frontend attached to the successor is told how the turn ended.
	_, afterConn, _ := reattached(t, second, cwd)
	awaitAll(t, afterConn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the gap-ended turn's terminal result item, replayed to a frontend that never saw the turn run": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if isResult(item) {
					return true
				}
			}
			return false
		},
	})
}
