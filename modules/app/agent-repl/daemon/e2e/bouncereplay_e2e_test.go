// REPLAY INTEGRITY ACROSS A CONNECTION LOSS.
//
// A replay is a request with two failure modes the shim's own code names in as
// many words (uds/server.ts):
//
//   - THE HOLE. `sendReplayEvent` drops an event when no daemon is attached:
//     "the replay the daemon receives is short by it". The daemon receives a
//     history it BELIEVES COMPLETE, so the gap is silent on the receiving end
//     and the log line is the only place it exists. A user scrolls back and
//     part of their conversation is simply not there.
//   - THE UNTERMINATED REQUEST. `sendReplayDone` drops the completion: "the
//     daemon's replay request never terminates and stays indistinguishable from
//     one still in flight". The contract is exactly one ReplayDone per
//     ReplayRequest, whatever the outcome, and a lost one leaves the daemon
//     waiting on a request nobody will ever close.
//
// WHAT MUST BE TRUE. After the link is re-established, a replay TERMINATES with
// a COMPLETE history. Both halves are separate facts and get separate tests: a
// replay can terminate and be short, and it can be complete and never close.
//
// BOTH TESTS RUN THEIR REPLAY ACROSS THE RE-ESTABLISHMENT BOUNDARY — the
// request is issued against a successor daemon whose shim link has only just
// come back — because that is the window in which the shim's two drop paths
// are reachable at all.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EAReplayAcrossAReattachCarriesTheWholeHistory covers THE HOLE.
//
// THE BASELINE IS THE PRE-BOUNCE DAEMON'S OWN REPLAY, not a count. Two real
// turns are run and then read back through the same resync path a frontend
// uses, so the history the successor must reproduce is one this test already
// saw served whole. `seqItems` drops the documented asymmetries (retained
// permission items, failure cards, prompt receipts — daemon-composed, carrying
// no store seq and re-pushed on every resync by contract), so the comparison is
// over exactly the events the store owns.
func TestE2EAReplayAcrossAReattachCarriesTheWholeHistory(t *testing.T) {
	// Arrange — real conversation traffic, read back before the bounce.
	// Tempdirs before the world: cleanups run LIFO, so this tears the daemons
	// and their shims down before the directories are removed.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	sessionID, conn, _, _ := liveSession(t, first.harness(), cwd)
	const (
		promptOne = "history-before-the-bounce-one"
		promptTwo = "history-before-the-bounce-two"
	)
	submitPrompt(t, conn, "r-h1", promptOne)
	awaitEcho(t, conn, cwd, promptOne, "the first pre-bounce turn's reply")
	submitPrompt(t, conn, "r-h2", promptTwo)
	awaitEcho(t, conn, cwd, promptTwo, "the second pre-bounce turn's reply")
	beforeConn, beforeState := dialForReplay(t, first.harness(), sessionID, cwd)
	before := seqItems(replayItems(t, beforeConn, beforeState, cwd, "r-replay-before-bounce"))
	if len(before) == 0 {
		t.Fatalf("the pre-bounce replay for %s served no seq-bearing items at all, so there is no history for the post-bounce replay to be short of", cwd)
	}

	// Act — bounce, reattach, and ask for the whole history again.
	first.bounce()
	second := world.boot(t)
	_, afterConn, afterState := reattached(t, second, cwd)
	after := seqItems(replayItems(t, afterConn, afterState, cwd, "r-replay-across-reattach"))

	// Assert — nothing the pre-bounce history carried is missing from it.
	if missing := missingKeys(itemKeys(before), itemKeys(after)); len(missing) > 0 {
		t.Fatalf("the replay served after the reattach is SHORT by %d item(s) (%v) out of the %d served before the bounce: an event dropped for want of an attached daemon leaves a history the daemon believes complete, so the hole is silent everywhere except a log nobody reads",
			len(missing), missing, len(before))
	}
}

// TestE2EAReplayIssuedAtReattachStillTerminates covers THE UNTERMINATED
// REQUEST: the resync's own CommandAck — the one-per-request completion — must
// arrive even when the request is issued while the link is still being
// re-established.
//
// The request is deliberately issued BEFORE the boot sweep's re-check claims
// the surviving shim, which is the narrow window in which a ReplayDone has
// nowhere to go. A dropped completion is observed here as an ack that never
// comes, bounded by the frame budget rather than waited on forever.
func TestE2EAReplayIssuedAtReattachStillTerminates(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	const prompt = "history the resync must be able to close over"
	submitPrompt(t, conn, "r-h1", prompt)
	awaitEcho(t, conn, cwd, prompt, "the pre-bounce turn's reply")

	// Act — the successor is up, the shim is redialling, and a frontend asks
	// for its history right now.
	first.bounce()
	second := world.boot(t)
	snap := connectSnapshot(t, second)
	sessionID := snapshotSessionID(t, snap, cwd)
	afterConn := second.harness().dial(t, sessionID)
	frame := readFrame(t, afterConn)
	if frame.GetSnapshot() == nil {
		t.Fatalf("first scoped frame after the bounce = %T, want a StateSnapshot", frame.GetFrame())
	}
	state := workspaceStateInSnapshot(t, frame, cwd)
	const requestID = "r-resync-at-reattach"
	sendResync(t, afterConn, requestID, state.GetFence())
	// The re-check is fired after the request so the request genuinely spans the
	// re-establishment, and on the OBSERVED redial rather than on test phase
	// order.
	second.sweepRecheckWhenParked(t, sessionID)

	// Assert — the request closes.
	awaitAll(t, afterConn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the resync's own CommandAck — exactly one completion per replay request, whatever the outcome; a lost one leaves the daemon holding a request that is indistinguishable from one still in flight": func(f *frontendv1.FrontendFrame) bool {
			ack := ackFor(f, requestID)
			if ack == nil {
				return false
			}
			if !ack.GetOk() {
				t.Fatalf("the resync issued across the reattach was NACKED (%q): a replay that spans a re-establishment must be SERVED from the durable record, not refused", ack.GetError())
			}
			return true
		},
	})
}
