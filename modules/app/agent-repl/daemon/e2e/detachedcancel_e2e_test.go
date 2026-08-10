// THE DETACHED-AGENT CANCEL, end to end over the REAL processes: a frontend
// `{"cancelDetachedAgents":{}}` command on the scoped /stream socket, through
// the real daemon, to the real TS shim running `--fake` offline, and the frames
// it produces coming back — the CommandAck's typed outcome and the async
// bubble delta that settles the stopped agent.
//
// WHY THIS COMMAND EXISTS. The Emacs interrupt confirmation ("Cancel the
// running subagents?") fires only when the main turn is over and detached work
// is still running. The interrupt it used to send is aimed at that turn, so the
// shim answered it ALREADY_COMPLETE and the agents the user had just agreed to
// stop kept running: the one state that raised the question was the one state
// its answer could not act on.
//
// HOW A DETACHED AGENT IS PRODUCED OFFLINE. The fake engine's `!agent <desc>`
// branch (fake-query.ts runAgentTurn) emits a Task call, a real
// `system:task_started`, and the launch's tool_result — then ends its turn with
// the agent STILL RUNNING. That is genuine task lifecycle, not a fabricated
// precondition: `!bg` cannot serve here, because its detached path emits a
// `<task-notification>` TEXT block that converts to the notification arm and
// never to task lifecycle, so nothing is ever live after it.
//
// The stop is genuine too. The shim calls the SDK's native `stopTask`, and the
// fake answers it exactly as the CLI does — a `system:task_notification` with
// status `stopped` — so nothing here injects an outcome or an ack.
//
// These tests share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, dial, readFrame, writeCmd, frameTimeout), clearcompact's
// liveSession, asyncspecharness's asyncDeltaIn / asyncTraffic, and
// interrupt_e2e_test.go's ackFor.
package e2e

import (
	"fmt"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// launchDetachedAgent submits `!agent <description>` and returns once the
// agent's bubble has OPENED, along with that bubble's id.
//
// The opened bubble is the receipt, and it is the right one: it proves the
// task_started reached the daemon and was classified as detached work, which is
// exactly the precondition the cancel acts on. Waiting on anything earlier
// would race the classification the assertions then depend on.
func launchDetachedAgent(t *testing.T, conn *websocket.Conn, workspace, requestID, description string) string {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":%q,"submitPrompt":{"text":"!agent %s","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`,
		requestID, description))
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		delta := asyncDeltaIn(frame, workspace)
		if delta == nil {
			continue
		}
		for _, bubble := range delta.GetOpened() {
			if bubble.GetAgent() != nil {
				return bubble.GetId()
			}
		}
	}
	t.Fatalf("no detached-agent bubble opened for workspace %s before the deadline", workspace)
	return ""
}

// awaitCancelAck sends the cancel and reads until its ack arrives, collecting
// every async push seen along the way.
//
// AN ACK IS NOT A BARRIER: the daemon's pushes and its acks travel one stream
// from different producers, so the settlement push can arrive before or after
// the ack. Reading both out of one loop makes the arrival order between them
// irrelevant rather than merely unlikely to matter.
func awaitCancelAck(t *testing.T, conn *websocket.Conn, workspace, requestID string) (*frontendv1.CommandAck, asyncTraffic) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"cancelDetachedAgents":{}}`, requestID))
	var seen asyncTraffic
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if delta := asyncDeltaIn(frame, workspace); delta != nil {
			seen.deltas = append(seen.deltas, delta)
		}
		if ack := ackFor(frame, requestID); ack != nil {
			return ack, seen
		}
	}
	t.Fatalf("no CommandAck for %s arrived before the deadline (saw %d async pushes)", requestID, len(seen.deltas))
	return nil, asyncTraffic{}
}

// awaitBubbleSettled returns the settled liveness for bubbleID, reading further
// frames only when the pushes already seen do not carry it.
//
// THE SEEN PUSHES COME FIRST because an ack is not a barrier: the settlement
// push and the ack travel one stream from different producers, so the push may
// have arrived either side of the ack. Handing the earlier read's frames in
// makes that order irrelevant instead of merely unlikely to matter, and keeps
// the wait a wait for a FRAME rather than for a duration.
func awaitBubbleSettled(t *testing.T, conn *websocket.Conn, workspace, bubbleID string, seen asyncTraffic) *frontendv1.AsyncSettled {
	t.Helper()
	if settled := lastSettledLiveness(seen, bubbleID); settled != nil {
		return settled
	}
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		delta := asyncDeltaIn(frame, workspace)
		if delta == nil {
			continue
		}
		seen.deltas = append(seen.deltas, delta)
		if settled := lastSettledLiveness(seen, bubbleID); settled != nil {
			return settled
		}
	}
	t.Fatalf("bubble %q never settled across the cancel (saw %d updates for it)",
		bubbleID, len(seen.updatesFor(bubbleID)))
	return nil
}

// --- cancel WITH detached agents running ------------------------------------

func TestE2ECancelStopsARunningDetachedAgent(t *testing.T) {
	// Arrange
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed. The other order races a still-
	// exiting shim against RemoveAll, which fails the test from cleanup.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	launchDetachedAgent(t, conn, cwd, "r-launch", "hunt bugs")

	// Act
	ack, _ := awaitCancelAck(t, conn, cwd, "c-stop")

	// Assert
	if !ack.GetOk() {
		t.Fatalf("cancel ack ok=false (error=%q), want ok: a detached agent was running", ack.GetError())
	}
	if got := ack.GetDetachedCancel().GetCancelled().GetCount(); got != 1 {
		t.Errorf("cancelled count = %d, want 1: the shim stopped exactly the one agent it had live", got)
	}
}

func TestE2ECancelSettlesTheStoppedAgentsBubble(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	bubbleID := launchDetachedAgent(t, conn, cwd, "r-launch", "hunt bugs")

	// Act
	_, seen := awaitCancelAck(t, conn, cwd, "c-stop")

	// Assert: no orphaned running bubble. A feed and a footer still showing
	// live work the daemon has already stopped say the opposite of what the
	// cancel just did.
	settled := awaitBubbleSettled(t, conn, cwd, bubbleID, seen)
	// KILLED, not done and not error: the work did not fail, it was not
	// allowed to conclude.
	if settled.GetKilled() == nil {
		t.Errorf("settled arm = %T, want killed", settled.GetOutcome())
	}
}

// --- cancel with NOTHING running -------------------------------------------

func TestE2ECancelWithNothingDetachedIsRefusedWithTheTypedArm(t *testing.T) {
	// Arrange — a live session that has launched no detached work at all.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)

	// Act
	ack, seen := awaitCancelAck(t, conn, cwd, "c-empty")

	// Assert: a LOUD refusal. Acking a stop that reached nothing as ok is how a
	// stop control comes to look like it works when it reaches nothing.
	if ack.GetOk() {
		t.Fatalf("cancel ack = %+v, want a refusal: nothing was running to stop", ack)
	}
	// And the account travels with it, so the client can say "nothing was
	// running" rather than render a bare transport failure.
	if ack.GetDetachedCancel().GetNothingRunning() == nil {
		t.Errorf("refusal lost its nothing_running arm: %+v", ack.GetDetachedCancel())
	}
	// Nothing was settled, because nothing was stopped.
	for _, delta := range seen.deltas {
		if len(delta.GetUpdates()) > 0 {
			t.Errorf("a cancel that stopped nothing pushed %d bubble update(s)", len(delta.GetUpdates()))
		}
	}
}
