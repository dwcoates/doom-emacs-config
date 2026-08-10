// The INTERRUPT (I1), end to end over the REAL processes: a frontend
// `{"interrupt":{}}` command on the scoped /stream socket, delivered through
// the real daemon to the real TS shim running `--fake` offline, and the frames
// it produces coming back — the CommandAck, the footer's interrupt window
// (ProgressView.interrupt), and the workspace's resolved render state.
//
// HOW A TURN IS HELD OPEN LONG ENOUGH TO INTERRUPT IT. The fake engine runs an
// ordinary text turn to completion synchronously (fake-query.ts runTextTurn:
// stream events, one assistant message, one result, with nothing to await), so
// there is no window in which such a turn is "in flight". A `!tool <command>`
// turn is different: it AWAITS canUseTool (fake-query.ts runToolTurn), which
// travels to the daemon as a PermissionRequest and back as a PENDING permission
// ConversationItem (sessioncontroller/sessioncontroller.go pushPermission), and the fake stays
// parked there until something answers. That pending item is therefore both the
// hold and the proof of the hold, and it is what every test here waits for
// before stopping the turn. The interrupt itself releases it: the shim cancels
// every blocked permission wait (uds-session.ts interrupt → control.cancelAll,
// resolving them as denials), the fake sees `interrupted` and emits
// `error_during_execution`, and the turn ends. That error flavor is the SDK's
// only word for an abort; the shim, which acked the stop, renames the terminal
// it closes to RESULT_SUBTYPE_ABORTED (see
// TestE2EAStoppedTurnsResultIsNamedAborted).
//
// WHY THE OUTCOMES ARE THE FAKE'S HONESTLY. The verdict is decided by the shim,
// synchronously, off its own turn counter (uds-session.ts interrupt: `const
// wasLive = this.turnsInFlight > 0` → INTERRUPTED else ALREADY_COMPLETE), and
// that counter is moved by real submits and real results in fake mode exactly
// as in live mode. Nothing here injects an outcome.
//
// These tests share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, createSession, dial, readFrame, writeCmd, frameTimeout) plus
// clearcompact_e2e_test.go's liveSession / deltaItems / replayItems.
//
// NOT COVERED — THE CONFIRM CHALLENGE (frontend.proto InterruptConfirmRequired).
// The gate fires only when NO turn is in flight AND the workspace has live
// subagent tasks (server/frontendcmd.go interruptChallenge), and the live-task
// count is the SSM's, folded from lifecycle TaskStarted/TaskEnded events
// (ssm/resolve.go live_task_count). The ONLY producer of a stream-plane
// TaskStarted is the converter's `task_started` SDK message family
// (shim/src/proto/convert.ts buildTaskStarted), and the file-plane producer is
// the sidecar reading a real transcript (shim-sidecar/internal/handler/
// transcript.go) — which does not run here. The fake engine never emits a
// `task_started` message: its only detached-work path is `!bg`, which emits a
// tool_progress plus a <task-notification> text block (fake-query.ts
// runBackgroundTurn), and a task-notification converts to the
// `taskNotification` arm (convert.ts case "task-notification"), never to task
// lifecycle. So a live task count cannot be produced in fake mode without
// writing task events through an internal seam — i.e. without faking the
// precondition the gate exists to detect. Left out deliberately rather than
// faked; it stays covered by server/frontendcmd's own tests.
package e2e

import (
	"claude-repld/internal/errclass"
	"fmt"
	"sort"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// --- frame accessors --------------------------------------------------------

// progressFor returns the ProgressView a frame carries for workspace, or nil
// when the frame is not this workspace's ProgressView (frontend.proto
// FrontendFrame arm 13; scoped by frontend/scope.go scopeFrame on
// ProgressView.workspace).
func progressFor(frame *frontendv1.FrontendFrame, workspace string) *frontendv1.ProgressView {
	if p, ok := frame.GetFrame().(*frontendv1.FrontendFrame_Progress); ok {
		if p.Progress.GetWorkspace() == workspace {
			return p.Progress
		}
		return nil
	}
	// Snapshot carriage: pushes are edge-triggered and an earlier await loop
	// on the same conn may have consumed the edge; the periodic snapshot
	// redelivery keeps every progress await ordering-robust.
	if snap, ok := frame.GetFrame().(*frontendv1.FrontendFrame_Snapshot); ok {
		for _, pv := range snap.Snapshot.GetProgress() {
			if pv.GetWorkspace() == workspace {
				return pv
			}
		}
	}
	return nil
}

// workspaceStateFor returns the WorkspaceState a frame carries for workspace,
// or nil when the frame is not this workspace's WorkspaceState (arm 2).
func workspaceStateFor(frame *frontendv1.FrontendFrame, workspace string) *frontendv1.WorkspaceState {
	ws, ok := frame.GetFrame().(*frontendv1.FrontendFrame_WorkspaceState)
	if !ok || ws.WorkspaceState.GetWorkspace() != workspace {
		return nil
	}
	return ws.WorkspaceState
}

// ackFor returns the CommandAck a frame carries for requestID, or nil. Acks are
// connection-global (scope.go: "CommandAck / unknown: connection-global").
func ackFor(frame *frontendv1.FrontendFrame, requestID string) *frontendv1.CommandAck {
	ack, ok := frame.GetFrame().(*frontendv1.FrontendFrame_CommandAck)
	if !ok || ack.CommandAck.GetRequestId() != requestID {
		return nil
	}
	return ack.CommandAck
}

// isPendingPermission identifies the item a blocked canUseTool round-trip
// produces: arm 30 with RESOLUTION_PENDING (sessioncontroller/sessioncontroller.go:1092 pushes
// permissionItem(req, PermissionItem_RESOLUTION_PENDING, "")).
func isPendingPermission(item *frontendv1.ConversationItem) bool {
	perm := item.GetPermission()
	return perm != nil && perm.GetResolution() == corev1.PermissionItem_RESOLUTION_PENDING
}

// assistantText concatenates the TEXT blocks of an assistant_message item (arm
// 10 → data.v1.ApiAssistantMessage.content → ContentBlock.text.text). It is how
// a fake turn is identified: the fake's reply is `echo: <prompt> [mode=<mode>]`
// (fake-query.ts runTextTurn), so the prompt text rides its own answer.
func assistantText(item *frontendv1.ConversationItem) string {
	msg := item.GetAgent().GetResponse().GetBody()
	if msg == nil {
		return ""
	}
	out := ""
	for _, block := range msg.GetContent() {
		out += block.GetText().GetText()
	}
	return out
}

// echoOf is the marker the fake's reply to prompt carries (fake-query.ts:
// `echo: ${text} [mode=${permissionMode}]`). The trailing " [mode=" is kept so
// one prompt's marker can never be a prefix-match of another's.
func echoOf(prompt string) string { return fmt.Sprintf("echo: %s [mode=", prompt) }

// --- frame waiting ----------------------------------------------------------

// awaitAll reads frames until EVERY condition in want has been satisfied at
// least once, in any order, and fails at the deadline naming the ones that
// never were.
//
// Order-independence is the point. The daemon pushes the interrupt window from
// inside the command dispatch (sessioncontroller noteUserInterrupt → progress
// NoteInterrupt, which pushes at once), enqueues the CommandAck after that
// dispatch returns (frontend/server.go readLoop), and resolves the workspace
// state only when the stopped turn's TurnEnded comes back through the store —
// three different producers, so pinning an arrival order in the test would be
// asserting an implementation detail rather than the contract.
//
// reject, when non-nil, is shown every frame first: a non-empty string from it
// fails the test immediately. It is how a NEGATIVE contract ("never repainted
// INTERRUPTED") is enforced over the same read loop rather than by sleeping.
func awaitAll(t *testing.T, conn *websocket.Conn, reject func(*frontendv1.FrontendFrame) string, want map[string]func(*frontendv1.FrontendFrame) bool) {
	t.Helper()
	awaitAllWithin(t, conn, frameTimeout, reject, want)
}

// awaitAllWithin is awaitAll under an explicit budget. It exists for the awaits
// whose bound is something other than the suite's frame budget, and every
// caller must say in its own comment what that bound is.
func awaitAllWithin(t *testing.T, conn *websocket.Conn, within time.Duration, reject func(*frontendv1.FrontendFrame) string, want map[string]func(*frontendv1.FrontendFrame) bool) {
	t.Helper()
	awaitAllSeeded(t, conn, within, nil, reject, want)
}

// awaitAllSeeded is awaitAllWithin over frames a PRIOR await already read off
// the same socket, before it reads any more.
//
// It exists because AN ACK IS NOT A BARRIER. The daemon's pushes and its
// CommandAcks travel one stream from different producers, so a push the daemon
// made before the ack can arrive after it — or the reverse. An await that read
// past a frame and dropped it would then wait out its whole budget for
// something already delivered, which is a test that fails on machine load
// rather than on the contract. Handing the read-past frames to the next await
// makes the arrival order between them irrelevant instead of merely unlikely to
// matter.
func awaitAllSeeded(t *testing.T, conn *websocket.Conn, within time.Duration, seed []*frontendv1.FrontendFrame, reject func(*frontendv1.FrontendFrame) string, want map[string]func(*frontendv1.FrontendFrame) bool) {
	t.Helper()
	pending := make(map[string]func(*frontendv1.FrontendFrame) bool, len(want))
	for name, match := range want {
		pending[name] = match
	}
	for _, frame := range seed {
		if reject != nil {
			if why := reject(frame); why != "" {
				t.Fatalf("forbidden frame (read before this await): %s", why)
			}
		}
		for name, match := range pending {
			if match(frame) {
				delete(pending, name)
			}
		}
	}
	deadline := time.Now().Add(within)
	for len(pending) > 0 && time.Now().Before(deadline) {
		frame, err := tryReadFrameWithin(t, conn, within)
		if err != nil {
			t.Fatalf("read: %v — still waiting for: %v", err, pendingNames(pending))
		}
		if reject != nil {
			if why := reject(frame); why != "" {
				t.Fatalf("forbidden frame: %s", why)
			}
		}
		for name, match := range pending {
			if match(frame) {
				delete(pending, name)
			}
		}
	}
	if len(pending) > 0 {
		t.Fatalf("these never arrived before the deadline: %v", pendingNames(pending))
	}
}

// pendingNames is the sorted list of awaits still outstanding, which is what
// every timeout in this file must report: WHICH frame never came is the fact,
// and the error alone never carries it.
func pendingNames(pending map[string]func(*frontendv1.FrontendFrame) bool) []string {
	names := make([]string, 0, len(pending))
	for name := range pending {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}

// --- the stop sequence ------------------------------------------------------

// holdTurnOpenAskingNothing submits a `!hold` prompt and returns once the turn
// is provably IN FLIGHT: the fake parks it until the session is interrupted,
// with no question asked (fake-query.ts runHoldTurn).
//
// IT IS THE HOLD FOR CALLERS THAT THEN SUBMIT PROMPTS. holdTurnOpen's `!tool`
// parks a canUseTool question, and a prompt typed over a parked question is
// itself an answer: it declines the question and stops the turn
// (sessioncontroller/permdecline.go). A caller queueing prompts behind a live
// turn would therefore stop the very turn it was queueing behind.
//
// The receipt is the SSM's turn_active, which is what holdTurnOpen falls back
// on too — see its own comment for what that does and does not prove.
func holdTurnOpenAskingNothing(t *testing.T, conn *websocket.Conn, workspace, requestID string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"submitPrompt":{"text":"!hold","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, requestID))
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the daemon's observation of the held turn (turn_active)": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, workspace)
			return state != nil && state.GetTurnActive()
		},
	})
}

// holdTurnOpen submits a `!tool` prompt and returns once the turn is provably
// IN FLIGHT and parked: the fake is awaiting canUseTool, which reached the
// frontend as a PENDING permission item. See this file's header for why an
// ordinary text turn cannot be caught mid-flight.
func holdTurnOpen(t *testing.T, conn *websocket.Conn, workspace, requestID, command string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"submitPrompt":{"text":"!tool %s","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, requestID, command))
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a PENDING permission item (the held turn)": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, workspace) {
				if isPendingPermission(item) {
					return true
				}
			}
			return false
		},
		// The permission item travels the DIRECT shim control path and can
		// beat the store-plane TurnStarted; a prompt submitted before the
		// daemon OBSERVES the turn is delivered rather than queued. The
		// receipt that a turn is claimed is a pushed WorkspaceState with the
		// SSM's turn_active set.
		//
		// WHAT THAT RECEIPT DOES AND DOES NOT PROVE. turn_active resolves off
		// the accepted edge as well as off the durable TurnStarted (ssm's
		// resolve: prompt_accepted, prompt_delivered and turn_started all set
		// it), so this wait proves a turn is CLAIMED, not that the daemon has
		// consumed the turn's start. What makes that enough for a caller that
		// then asks the daemon to name the turn is on the daemon's side of the
		// line, not this file's: the controller's turn record binds its name
		// from the durable claim ledger BEFORE the SSM apply that publishes
		// this very state, and a hold still in the accepted phase resolves its
		// name from that same ledger (sessioncontroller, turnrecord.go and
		// nameAcceptedHold). Neither is a property this wait can assert, and
		// both are covered by unit tests there.
		"the daemon's observation of the turn (turn_active)": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, workspace)
			return state != nil && state.GetTurnActive()
		},
	})
}

// stopObservation is what a user-commanded stop of a live turn produced.
type stopObservation struct {
	ack    *frontendv1.CommandAck
	view   *frontendv1.ProgressView
	window *frontendv1.InterruptWindow
	state  *frontendv1.WorkspaceState
}

// stopLiveTurn holds a turn open, sends the interrupt while it is in flight,
// and returns the ack, the ProgressView carrying the interrupt window, and the
// WorkspaceState the stopped turn's end resolved to.
//
// It waits for all three rather than for whichever arrives first, so a caller
// using it as SETUP is guaranteed the stop has fully settled — the window is
// open and the turn has ended — before it acts.
func stopLiveTurn(t *testing.T, conn *websocket.Conn, workspace string) stopObservation {
	t.Helper()
	holdTurnOpen(t, conn, workspace, "r-hold", "sleep e2e-interrupt")

	const requestID = "r-interrupt"
	var obs stopObservation
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"interrupt":{}}`, requestID))
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the interrupt": func(frame *frontendv1.FrontendFrame) bool {
			ack := ackFor(frame, requestID)
			if ack == nil {
				return false
			}
			obs.ack = ack
			return true
		},
		"a ProgressView carrying an OPEN interrupt window": func(frame *frontendv1.FrontendFrame) bool {
			view := progressFor(frame, workspace)
			if !view.GetInterrupt().GetActive() {
				return false
			}
			obs.view, obs.window = view, view.GetInterrupt()
			return true
		},
		"a WorkspaceState resolving the stopped turn": func(frame *frontendv1.FrontendFrame) bool {
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

// --- tests ------------------------------------------------------------------

// TestE2EInterruptOfALiveTurnResolvesInterrupted covers THE LIVE STOP: an
// interrupt sent while a turn is genuinely in flight is acked ok, opens the
// footer's interrupt window carrying INTERRUPTED, and the turn it ended
// resolves the workspace to RENDER_STATE_INTERRUPTED.
func TestE2EInterruptOfALiveTurnResolvesInterrupted(t *testing.T) {
	// Arrange
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed. The other order races a still-
	// exiting shim against RemoveAll, which fails the test from cleanup.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)

	// Act
	obs := stopLiveTurn(t, conn, cwd)

	// Assert
	if !obs.ack.GetOk() {
		t.Errorf("interrupt ack ok=false (error=%q, challenge=%v), want ok: interrupting a LIVE turn never asks for confirmation",
			obs.ack.GetError(), obs.ack.GetInterruptConfirmRequired())
	}
	if got := obs.window.GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		t.Errorf("interrupt window outcome = %s, want INTERRUPTED: the shim's turn counter was non-zero when the stop landed", got)
	}
	if obs.state.GetState() != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
		t.Errorf("workspace state = %s, want RENDER_STATE_INTERRUPTED", obs.state.GetState())
	}
}

// TestE2EAStoppedTurnsResultIsNamedAborted covers THE FEED'S OWN VERDICT on a
// stop, which is a different record from the footer's interrupt window: the
// terminal ResultMessage the conversation carries.
//
// The fake aborts and emits `error_during_execution` (the SDK's word for the
// error an abort shakes out — see the file header), and passing that through
// put a red `error_during_execution` badge in the response output for a stop
// the user asked for. The shim acked the interrupt, so the shim names the
// terminal: RESULT_SUBTYPE_ABORTED, which the webapp draws as the yellow
// `interrupted` chip.
//
// It reads the record back through a RESYNC rather than off the live delta,
// because the stop's setup already consumed frames — and a replay is the
// harder claim anyway: the naming is DURABLE, not a live-push decoration.
func TestE2EAStoppedTurnsResultIsNamedAborted(t *testing.T) {
	// Arrange
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)

	// Act
	obs := stopLiveTurn(t, conn, cwd)
	items := replayItems(t, conn, obs.state, cwd, "r-replay-aborted")

	// Assert
	var subtypes []string
	for _, item := range items {
		if r := item.GetAgent().GetTurnResult(); r != nil {
			subtypes = append(subtypes, r.GetSubtype().String())
		}
	}
	if len(subtypes) == 0 {
		t.Fatalf("the replayed conversation carries no terminal result at all (items=%d)", len(items))
	}
	want := datav1.ResultSubtype_RESULT_SUBTYPE_ABORTED.String()
	if subtypes[len(subtypes)-1] != want {
		t.Errorf("stopped turn's terminal subtype = %s, want %s: a stop the user asked for is not an execution failure", subtypes[len(subtypes)-1], want)
	}
}

// TestE2ENextPromptClosesTheInterruptWindow covers SUPERSESSION: the window
// clears when the NEXT TURN STARTS and never on a timer (progress.go
// openTurnLocked, the one place that clears it), and the workspace leaves
// INTERRUPTED for THINKING under that same turn start.
func TestE2ENextPromptClosesTheInterruptWindow(t *testing.T) {
	// Arrange — a settled interrupted turn.
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed. The other order races a still-
	// exiting shim against RemoveAll, which fails the test from cleanup.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	stopLiveTurn(t, conn, cwd)

	// Act — the prompt the user typed after stopping the agent. The queue is
	// PAUSED by the stop, but with no turn running this one goes straight
	// through as the lone runner (sessioncontroller queueSubmitLocked).
	writeCmd(t, conn, `{"requestId":"r-next","submitPrompt":{"text":"after the stop","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a ProgressView for the NEW turn with the interrupt window CLOSED": func(frame *frontendv1.FrontendFrame) bool {
			view := progressFor(frame, cwd)
			// turn_started_at_ms != 0 anchors this to the new turn's own view
			// (openTurnLocked stamps it), so a stale pre-stop view cannot
			// masquerade as the cleared one.
			return view.GetTurnStartedAtMs() != 0 && view.GetInterrupt() == nil
		},
		"a WorkspaceState back in THINKING": func(frame *frontendv1.FrontendFrame) bool {
			return workspaceStateFor(frame, cwd).GetState() == frontendv1.RenderState_RENDER_STATE_THINKING
		},
	})
}

// TestE2ELateInterruptReportsAlreadyComplete covers THE LATE STOP: with no turn
// in flight the interrupt is still DELIVERED — the daemon's view of "no turn"
// is an observation, not a ruling (server/frontendcmd.go Interrupt) — and the
// shim answers ALREADY_COMPLETE off its own counter. That outcome moves NO
// workspace phase: the window is the only surface that reports it.
func TestE2ELateInterruptReportsAlreadyComplete(t *testing.T) {
	// Arrange — one ordinary turn, run to completion, so there is a settled
	// workspace state for the stop to leave alone.
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed. The other order races a still-
	// exiting shim against RemoveAll, which fails the test from cleanup.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	writeCmd(t, conn, `{"requestId":"r-first","submitPrompt":{"text":"a turn that finishes","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	settled := frontendv1.RenderState_RENDER_STATE_UNSPECIFIED
	sawTurnClockOpen := false
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the first turn's clock closing (its end)": func(frame *frontendv1.FrontendFrame) bool {
			if state := workspaceStateFor(frame, cwd); state != nil {
				settled = state.GetState()
			}
			view := progressFor(frame, cwd)
			if view == nil {
				return false
			}
			if view.GetTurnStartedAtMs() != 0 {
				sawTurnClockOpen = true
				return false
			}
			// An initial snapshot also carries a zero clock. Requiring the opening
			// edge first proves this zero is closeTurnLocked's terminal edge.
			return sawTurnClockOpen && view.GetInterrupt() == nil
		},
	})

	// Act
	const requestID = "r-late"
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"interrupt":{}}`, requestID))

	// Assert — delivered (an ok ack, never the confirm challenge), reported as
	// ALREADY_COMPLETE in the window, and the workspace NEVER repainted
	// INTERRUPTED while that verdict travels.
	rejectInterrupted := func(frame *frontendv1.FrontendFrame) string {
		if workspaceStateFor(frame, cwd).GetState() == frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
			return fmt.Sprintf("workspace %s was repainted RENDER_STATE_INTERRUPTED by a stop that interrupted nothing (settled state was %s)", cwd, settled)
		}
		return ""
	}
	awaitAll(t, conn, rejectInterrupted, map[string]func(*frontendv1.FrontendFrame) bool{
		"an ok CommandAck for the late interrupt": func(frame *frontendv1.FrontendFrame) bool {
			ack := ackFor(frame, requestID)
			if ack == nil {
				return false
			}
			if !ack.GetOk() {
				t.Fatalf("late interrupt acked ok=false (error=%q, challenge=%v): with no turn and no live tasks it must be DELIVERED, not refused",
					ack.GetError(), ack.GetInterruptConfirmRequired())
			}
			return true
		},
		"a ProgressView whose interrupt window carries ALREADY_COMPLETE": func(frame *frontendv1.FrontendFrame) bool {
			view := progressFor(frame, cwd)
			if !view.GetInterrupt().GetActive() {
				return false
			}
			if got := view.GetInterrupt().GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE {
				t.Fatalf("interrupt window outcome = %s, want ALREADY_COMPLETE: no turn was in flight when the stop landed", got)
			}
			// The gate's precondition, read off the same resolver the gate
			// reads (progress.Manager.LiveTasks): no live subagent work, which
			// is why the ack above was a delivery rather than a challenge.
			if got := view.GetLiveTaskCount(); got != 0 {
				t.Fatalf("live_task_count = %d, want 0: this test's premise is a workspace with no live subagent tasks", got)
			}
			return true
		},
	})
}

// TestE2EFreshConnectCarriesTheInterruptedResolution covers THE RESOLVE PATH: a
// frontend that connects AFTER an interrupted turn settled learns both facts
// from its connect StateSnapshot — the open interrupt window in the workspace's
// ProgressView and RENDER_STATE_INTERRUPTED in its WorkspaceState — while the
// conversation replay carries nothing about the interrupt at all. Footer state
// is not feed content: the ConversationItem oneof has no interrupt arm
// (frontend.proto ConversationItem), and the stop is not a failure card either.
func TestE2EFreshConnectCarriesTheInterruptedResolution(t *testing.T) {
	// Arrange
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed. The other order races a still-
	// exiting shim against RemoveAll, which fails the test from cleanup.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, live, _, _ := liveSession(t, h, cwd)
	stopLiveTurn(t, live, cwd)

	// Act
	fresh := h.dial(t, id)
	snapshotFrame := readFrame(t, fresh)
	snap := snapshotFrame.GetSnapshot()
	if snap == nil {
		t.Fatal("first frame on a fresh connection was not a StateSnapshot")
	}

	// Assert — the snapshot resolves both halves of the stop.
	var window *frontendv1.InterruptWindow
	for _, view := range snap.GetProgress() {
		if view.GetWorkspace() == cwd {
			window = view.GetInterrupt()
		}
	}
	if !window.GetActive() {
		t.Errorf("snapshot ProgressView for %s carries interrupt=%v, want an OPEN window", cwd, window)
	}
	if got := window.GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		t.Errorf("snapshot interrupt window outcome = %s, want INTERRUPTED", got)
	}
	found := false
	for _, state := range snap.GetWorkspaces() {
		if state.GetWorkspace() != cwd {
			continue
		}
		found = true
		if state.GetState() != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
			t.Errorf("snapshot WorkspaceState for %s = %s, want RENDER_STATE_INTERRUPTED", cwd, state.GetState())
		}
	}
	if !found {
		t.Errorf("snapshot carried no WorkspaceState for %s at all", cwd)
	}
	// ... and the feed carries nothing about it. "interrupt" is a documented
	// SystemFailureItem.error_type (frontend.proto SystemFailureItem), so a
	// stop leaking into the conversation would show up as one of those cards.
	state := workspaceStateInSnapshot(t, snapshotFrame, cwd)
	for _, item := range replayItems(t, fresh, state, cwd, "r-replay") {
		if got := errclass.TypeName(item.GetFailureCard()); got == "interrupt" {
			t.Errorf("replay carried a system_failure card of error_type %q (uuid=%q): a user-commanded stop is footer state, not feed content",
				got, item.GetUuid())
		}
	}
}

// TestE2EInterruptedQueueRunsTheNextPromptAlone covers THE PAUSE over the real
// processes: a stop PAUSES the queue while RETAINING every held prompt, the
// prompt submitted afterwards jumps them and runs ALONE, and its clean end
// resumes the drain in the original order (sessioncontroller queue.go addHeadJump /
// onTurnBoundary).
//
// Order is observed off the fake's own replies, which echo the prompt text
// back (`echo: <prompt> [mode=...]`), because a fake-mode turn produces no user
// bubble at all: the user echo a real session's bubble comes from is a
// transcript UserLine or a stream UserMessage (uds-session.ts userPromptText),
// and the fake engine emits neither for a submitted prompt.
func TestE2EInterruptedQueueRunsTheNextPromptAlone(t *testing.T) {
	// Arrange — a held turn with two prompts queued behind it.
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed. The other order races a still-
	// exiting shim against RemoveAll, which fails the test from cleanup.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	// The hold that asks NOTHING: prompts are queued behind this turn, and a
	// prompt submitted over a parked question would decline it and stop the very
	// turn they are meant to queue behind.
	holdTurnOpenAskingNothing(t, conn, cwd, "r-hold")
	writeCmd(t, conn, `{"requestId":"r-q1","submitPrompt":{"text":"queued-one","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	writeCmd(t, conn, `{"requestId":"r-q2","submitPrompt":{"text":"queued-two","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	// Both prompts must be HELD before the stop lands, or the stop can outrun
	// a submit and the late prompt would be head-jump promoted instead of
	// retained — a different (legitimate) scenario than the one this test
	// pins. The pushed QueueView is the daemon's own receipt that both are in.
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueView holding both prompts": func(frame *frontendv1.FrontendFrame) bool {
			q := frame.GetQueue()
			return q.GetWorkspace() == cwd && len(q.GetEntries()) == 2
		},
	})

	// Act — stop the running turn, then type something new.
	writeCmd(t, conn, `{"requestId":"r-interrupt","interrupt":{}}`)
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a ProgressView carrying an OPEN interrupt window": func(frame *frontendv1.FrontendFrame) bool {
			return progressFor(frame, cwd).GetInterrupt().GetActive()
		},
	})
	writeCmd(t, conn, `{"requestId":"r-jump","submitPrompt":{"text":"after-the-stop","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — the jumper ran first and the retained pair drained behind it in
	// the order they were submitted.
	want := []string{"after-the-stop", "queued-one", "queued-two"}
	var order []string
	seen := map[string]bool{}
	deadline := time.Now().Add(frameTimeout)
	for len(order) < len(want) && time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		for _, item := range deltaItems(frame, cwd) {
			text := assistantText(item)
			for _, prompt := range want {
				if !seen[prompt] && text != "" && strings.Contains(text, echoOf(prompt)) {
					seen[prompt] = true
					order = append(order, prompt)
				}
			}
		}
	}
	if len(order) != len(want) {
		t.Fatalf("saw replies to %v before the deadline, want all of %v", order, want)
	}
	for i := range want {
		if order[i] != want[i] {
			t.Fatalf("turns ran in the order %v, want %v: the prompt typed after the stop runs ALONE and the retained prompts drain behind it in their original order", order, want)
		}
	}
}

// TestE2EInterruptDoesNotCrossWorkspaces covers SCOPING: a second session's
// frontend connection never sees the first workspace's interrupt. The scoped
// /stream filters ProgressView on its own workspace (frontend/scope.go
// scopeFrame), so a stop in one workspace is invisible in another.
func TestE2EInterruptDoesNotCrossWorkspaces(t *testing.T) {
	// Arrange — two live sessions on two workspaces.
	// Tempdirs before the harness: see the cleanup-order note in the first test.
	cwdA, cwdB := t.TempDir(), t.TempDir()
	h := newUDSHarness(t)
	_, connA, _, _ := liveSession(t, h, cwdA)
	_, connB, _, _ := liveSession(t, h, cwdB)

	// Act — stop A's live turn (observed on A's own connection), then give B a
	// turn of its own whose reply is the terminator for B's read below.
	stopLiveTurn(t, connA, cwdA)
	writeCmd(t, connB, `{"requestId":"r-b","submitPrompt":{"text":"b-sentinel","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — nothing about A's stop reached B.
	reject := func(frame *frontendv1.FrontendFrame) string {
		if progressFor(frame, cwdA) != nil || workspaceStateFor(frame, cwdA) != nil {
			return fmt.Sprintf("workspace %s frame arrived on workspace %s's scoped stream", cwdA, cwdB)
		}
		if view := progressFor(frame, cwdB); view.GetInterrupt().GetActive() {
			return fmt.Sprintf("workspace %s got an OPEN interrupt window (%v) from a stop commanded in %s", cwdB, view.GetInterrupt(), cwdA)
		}
		return ""
	}
	awaitAll(t, connB, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"workspace B's own turn replying": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwdB) {
				if strings.Contains(assistantText(item), echoOf("b-sentinel")) {
					return true
				}
			}
			return false
		},
	})
}
