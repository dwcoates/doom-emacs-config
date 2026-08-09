// asyncspecharness_test.go — harness affordances for the figma→idl executable
// specification: the transcript records that detach work, the store events that
// carry them, and the frame observers the specification's assertions read.
//
// WHY THE RECORDS ARE INJECTED RATHER THAN PROVOKED. Identical reasoning to
// clearcompact_e2e_test.go and skillbody_e2e_test.go, whose helpers this file
// reuses READ-ONLY (liveSession, storeProducer.write, awaitItem,
// workspaceStateInSnapshot): the shim-claude-sidecar is the sole producer of
// file-plane records and it produces them by tailing a real vendor transcript,
// which the `--fake` harness has none of. So these constructors write the store
// exactly the event shape the sidecar writes for each transcript line, and
// everything downstream — store ingest, store fan-out, the shim's merged-stream
// forward, the daemon's classification and folding, the frontend frames — runs
// for real.
//
// THE EVIDENCE PATH IS THE TRANSCRIPT'S OWN. A detached agent's records are
// SIDECHAIN records: LineEnvelope.is_sidechain is set, and
// LineEnvelope.source_tool_use_id names the tool call that dispatched them
// (agentshim/data/v1/transcript.proto). That linkage — not a client-side
// heuristic over free-text prose — is what a daemon-side classifier has to work
// from, and it is what these fixtures supply.
package e2e

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/types/known/anypb"
)

// --- injection: transcript records that detach work -------------------------

// vendorLineEvent wraps one transcript line the way handler.vendorEvent does:
// file plane, PERSISTENT, no dedup key (the store derives its own uuid: key).
// Same envelope sidecarLineEvent builds; named separately only so this file's
// fixtures read as one set.
func vendorLineEvent(t *testing.T, vendorSessionID string, line *datav1.TranscriptLine) *corev1.Event {
	t.Helper()
	a, err := anypb.New(line)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

// asyncToolCallLine is the assistant record making a tool call.
func asyncToolCallLine(uuid, toolUseID, toolName string) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{Uuid: uuid},
		Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: toolUseID, Name: toolName}}},
		}},
	}}}
}

// asyncToolResultLine is the user record reporting a tool call's result,
// carrying the TYPED outcome. The typed outcome is where a detachment becomes
// knowable (tool-call.proto, AgentToolOutcome.spawned_bubble_id).
func asyncToolResultLine(uuid, toolUseID, resultText string, outcome *datav1.ToolUseResult) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
		Envelope: &datav1.LineEnvelope{Uuid: uuid},
		Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
			ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{
					ToolUseId: toolUseID,
					Content:   &datav1.ToolResultBlock_ContentString{ContentString: resultText},
				}}},
			}},
		}},
		ToolUseResult:    outcome,
		HasToolUseResult: outcome != nil,
	}}}
}

// agentAsyncLaunchOutcome is the typed outcome a backgrounded Task launch
// writes: is_async, async_launched, and the agent's id and description
// (data/v1/tools.proto AgentAsyncLaunch).
func agentAsyncLaunchOutcome(agentID, description string) *datav1.ToolUseResult {
	return &datav1.ToolUseResult{Result: &datav1.ToolUseResult_AgentAsyncLaunch{
		AgentAsyncLaunch: &datav1.AgentAsyncLaunch{
			IsAsync:     true,
			Status:      datav1.RawTaskStatus_RAW_TASK_STATUS_ASYNC_LAUNCHED,
			AgentId:     agentID,
			Description: description,
		},
	}}
}

// bashBackgroundOutcome is the typed outcome a backgrounded Bash writes: a
// BashResult whose background_task_id is set, which the proto states is the
// signal of a background launch (data/v1/tools.proto BashResult §12).
func bashBackgroundOutcome(taskID string) *datav1.ToolUseResult {
	return &datav1.ToolUseResult{Result: &datav1.ToolUseResult_Bash{
		Bash: &datav1.BashResult{BackgroundTaskId: taskID},
	}}
}

// bashTaskOutcome is a TaskOutput retrieval over a backgrounded shell: the
// spool so far, and — once it_set — the exit status that settles it.
func bashTaskOutcome(taskID, command, output string, status datav1.RawTaskStatus, exitCode int32, exitSet bool) *datav1.ToolUseResult {
	return &datav1.ToolUseResult{Result: &datav1.ToolUseResult_TaskOutput{
		TaskOutput: &datav1.TaskOutputResult{
			RetrievalStatus: datav1.RetrievalStatus_RETRIEVAL_STATUS_SUCCESS,
			Task: &datav1.TaskOutputResult_LocalBash{LocalBash: &datav1.LocalBashTask{
				TaskId:      taskID,
				TaskType:    "local_bash",
				Status:      status,
				Description: command,
				Output:      output,
				ExitCode:    exitCode,
				ExitCodeSet: exitSet,
			}},
		},
	}}
}

// sidechainResponseLine is ONE utterance of a detached agent: an assistant
// record flagged is_sidechain and pointed at the dispatching call through
// source_tool_use_id. This is the record the acceptance criterion is about —
// the one that used to land in the top-level feed.
func sidechainResponseLine(uuid, parentUUID, sourceToolUseID, agentID, text string) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{
			Uuid:            uuid,
			ParentUuid:      parentUUID,
			IsSidechain:     true,
			AgentId:         agentID,
			SourceToolUseId: sourceToolUseID,
		},
		Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
		}},
	}}}
}

// degradedStateEvent is what the shim writes for a degradation window
// (agent-shim/claude/shim/src/uds/uds-session.ts): STREAM plane, PERSISTENT,
// carrying DegradedState. recovered=true is the window's CLOSING report, and
// the daemon re-sends the same card under the same uuid with the resolved arm.
func degradedStateEvent(vendorSessionID, component, reason string, droppedCount uint64, recovered bool) *corev1.Event {
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_STREAM,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{
			Component:    component,
			Reason:       reason,
			DroppedCount: droppedCount,
			Recovered:    recovered,
		}},
	}
}

// --- observation: the frames the specification reads ------------------------

// asyncDeltaIn returns the AsyncBubbleDelta a frame carries for workspace, or
// nil when the frame is not this workspace's async push.
func asyncDeltaIn(frame *frontendv1.FrontendFrame, workspace string) *frontendv1.AsyncBubbleDelta {
	d, ok := frame.GetFrame().(*frontendv1.FrontendFrame_AsyncBubbleDelta)
	if !ok || d.AsyncBubbleDelta.GetWorkspace() != workspace {
		return nil
	}
	return d.AsyncBubbleDelta
}

// asyncTraffic is everything one drain saw: the top-level conversation items
// and the async pushes, in arrival order within each.
//
// BOTH HALVES IN ONE READ because the acceptance criterion is a statement about
// both at once — a detached agent's utterance belongs in the async half and
// must be absent from the feed half. Reading them separately would let a record
// satisfy one observer and be missed by the other.
type asyncTraffic struct {
	items  []*frontendv1.ConversationItem
	deltas []*frontendv1.AsyncBubbleDelta
}

// bubbles returns every bubble opened across the drained pushes.
func (a asyncTraffic) bubbles() []*frontendv1.AsyncBubble {
	var out []*frontendv1.AsyncBubble
	for _, delta := range a.deltas {
		out = append(out, delta.GetOpened()...)
	}
	return out
}

// updatesFor returns every update addressed to bubbleID across the drained
// pushes, in arrival order.
func (a asyncTraffic) updatesFor(bubbleID string) []*frontendv1.AsyncBubbleUpdate {
	var out []*frontendv1.AsyncBubbleUpdate
	for _, delta := range a.deltas {
		for _, update := range delta.GetUpdates() {
			if update.GetBubbleId() == bubbleID {
				out = append(out, update)
			}
		}
	}
	return out
}

// agentEmissions returns every emission the agent-arm updates for bubbleID
// carried, in arrival order.
func (a asyncTraffic) agentEmissions(bubbleID string) []*frontendv1.AgentEmission {
	var out []*frontendv1.AgentEmission
	for _, update := range a.updatesFor(bubbleID) {
		out = append(out, update.GetAgent().GetEmissions()...)
	}
	return out
}

// drainUntilItem reads frames until a top-level conversation item for workspace
// satisfies match, returning everything seen up to AND INCLUDING that item.
//
// The matched item is the drain's BARRIER and it is a sound one: the store
// preserves per-session write order and the daemon curates in that order, so
// once the barrier's item has arrived every record written before it has been
// through the whole pipeline. No sleeping for a guessed duration.
func drainUntilItem(t *testing.T, conn *websocket.Conn, workspace, what string, match func(*frontendv1.ConversationItem) bool) asyncTraffic {
	t.Helper()
	var seen asyncTraffic
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if delta := asyncDeltaIn(frame, workspace); delta != nil {
			seen.deltas = append(seen.deltas, delta)
		}
		for _, item := range deltaItems(frame, workspace) {
			seen.items = append(seen.items, item)
			if match(item) {
				return seen
			}
		}
	}
	t.Fatalf("no %s arrived for workspace %s before the deadline (saw %d conversation items and %d async pushes)",
		what, workspace, len(seen.items), len(seen.deltas))
	return asyncTraffic{}
}

// awaitFrame reads frames until one satisfies match, and fails loudly at the
// deadline. The generic observer behind every fenced-view assertion.
func awaitFrame(t *testing.T, conn *websocket.Conn, what string, match func(*frontendv1.FrontendFrame) bool) *frontendv1.FrontendFrame {
	t.Helper()
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if match(frame) {
			return frame
		}
	}
	t.Fatalf("no %s frame arrived before the deadline", what)
	return nil
}

// spawningCall finds the tool_call emission for toolUseID among top-level
// items, which is where the daemon publishes its classification verdict
// (tool-call.proto AgentToolCall.spawned_bubble_id).
func spawningCall(items []*frontendv1.ConversationItem, toolUseID string) *frontendv1.AgentToolCall {
	for _, item := range items {
		call := item.GetAgent().GetToolCall()
		if call.GetCall().GetId() == toolUseID {
			return call
		}
	}
	return nil
}

// openedBubble finds the bubble with id among a drain's opened bubbles.
func openedBubble(bubbles []*frontendv1.AsyncBubble, id string) *frontendv1.AsyncBubble {
	for _, b := range bubbles {
		if b.GetId() == id {
			return b
		}
	}
	return nil
}

// asyncBubbleItems returns the top-level ConversationItems carrying arm 38 —
// the bubble's ANCHOR in the feed, distinct from the bubble's own updates.
func asyncBubbleItems(items []*frontendv1.ConversationItem) []*frontendv1.AsyncBubble {
	var out []*frontendv1.AsyncBubble
	for _, item := range items {
		if b := item.GetAsyncBubble(); b != nil {
			out = append(out, b)
		}
	}
	return out
}

// submitPrompt sends one user prompt over a session-scoped stream socket.
func submitPrompt(t *testing.T, conn *websocket.Conn, requestID, text string) {
	t.Helper()
	writeCmd(t, conn, `{"requestId":"`+requestID+`","submitPrompt":{"text":"`+text+`","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
}
