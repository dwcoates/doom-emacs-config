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
// interrupt_e2e_test.go's header), the question is ANSWERED, and the daemon is
// bounced in the same breath. The answer releases the shim's turn, which then
// runs its tail — tool result, closing message, result, TurnEnded — as several
// store round-trips, while the daemon it was talking to is being torn down.
//
// THE ARRANGEMENT IS CHECKED, NOT ASSUMED. Before asserting anything about the
// reattach, each test reads the STORE and requires a durable TurnEnded to
// exist: that is the premise ("the shim finished") stated as a fact about the
// shared record rather than as a hope about scheduling. A run in which the
// shim did not finish fails there, naming the arrangement, instead of passing
// vacuously.
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
	answerPermission(t, conn, cwd, "r-answer", permID, true)
	first.bounce()
	second := world.boot(t)

	// The premise, read from the shared durable record: the shim genuinely
	// finished the turn with no daemon there to see it.
	if !storeTurnEndedFor(t, vendorID, "") {
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

	// Act
	answerPermission(t, conn, cwd, "r-answer", permID, true)
	first.bounce()
	second := world.boot(t)
	if !storeTurnEndedFor(t, vendorID, "") {
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
