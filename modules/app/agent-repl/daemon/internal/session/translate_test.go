package session

import (
	"encoding/json"
	"fmt"
	"testing"

	"claude-repld/internal/protocol"
)

// --- helpers ----------------------------------------------------------------

func evt(t *testing.T, raw string) *protocol.L1Event {
	t.Helper()
	e, err := protocol.DecodeL1Event([]byte(raw))
	if err != nil {
		t.Fatalf("bad test event %s: %v", raw, err)
	}
	if e == nil {
		t.Fatalf("test event ignored: %s", raw)
	}
	return e
}

func streamEvt(t *testing.T, inner string) *protocol.L1Event {
	t.Helper()
	return evt(t, fmt.Sprintf(`{"type":"stream-event","session_id":"s1","uuid":"u","event":%s}`, inner))
}

func frameTypes(frames []protocol.L2Frame) []string {
	types := make([]string, len(frames))
	for i, f := range frames {
		types[i] = f.Env().Type
	}
	return types
}

func wantTypes(t *testing.T, frames []protocol.L2Frame, want ...string) {
	t.Helper()
	got := frameTypes(frames)
	if len(got) != len(want) {
		t.Fatalf("frame types = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("frame types = %v, want %v", got, want)
		}
	}
}

// runText streams one full text block (start, delta, stop) through tr.
func runTextBlock(t *testing.T, tr *Translator, index int, text string) []protocol.L2Frame {
	t.Helper()
	var frames []protocol.L2Frame
	frames = append(frames, tr.OnEvent(streamEvt(t, fmt.Sprintf(
		`{"type":"content_block_start","index":%d,"content_block":{"type":"text","text":""}}`, index)))...)
	frames = append(frames, tr.OnEvent(streamEvt(t, fmt.Sprintf(
		`{"type":"content_block_delta","index":%d,"delta":{"type":"text_delta","text":%q}}`, index, text)))...)
	frames = append(frames, tr.OnEvent(streamEvt(t, fmt.Sprintf(
		`{"type":"content_block_stop","index":%d}`, index)))...)
	return frames
}

func startMessage(t *testing.T, tr *Translator, id string) []protocol.L2Frame {
	t.Helper()
	return tr.OnEvent(streamEvt(t, fmt.Sprintf(
		`{"type":"message_start","message":{"id":%q,"role":"assistant","model":"m","usage":{"input_tokens":1,"output_tokens":2}}}`, id)))
}

// --- streaming text ----------------------------------------------------------

func TestTranslatorTextBlockLifecycle(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	// Act
	frames := runTextBlock(t, tr, 0, "hello")
	// Assert
	wantTypes(t, frames, "text-start", "text-delta", "text-end")
	start := frames[0].(*protocol.TextStartFrame)
	if start.MessageID != "msg1" || start.BlockID == "" {
		t.Errorf("text-start = %+v", start)
	}
	end := frames[2].(*protocol.TextEndFrame)
	if end.FinalText != "hello" || end.BlockID != start.BlockID {
		t.Errorf("text-end = %+v", end)
	}
}

func TestTranslatorMessageStartEmitsUsage(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := startMessage(t, tr, "msg1")
	// Assert
	wantTypes(t, frames, "usage")
	usage := frames[0].(*protocol.UsageFrame)
	if usage.MessageID != "msg1" || usage.Usage.OutputTokens != 2 {
		t.Errorf("usage = %+v", usage)
	}
}

func TestTranslatorMessageDeltaEmitsUsage(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	// Act
	frames := tr.OnEvent(streamEvt(t,
		`{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"input_tokens":5,"output_tokens":9}}`))
	// Assert
	wantTypes(t, frames, "usage")
	if frames[0].(*protocol.UsageFrame).Usage.OutputTokens != 9 {
		t.Errorf("usage = %+v", frames[0])
	}
}

func TestTranslatorSubagentMessageStartEmitsNoUsage(t *testing.T) {
	// Arrange — a subagent's request carries the SUBAGENT's context, not the
	// session's, so surfacing it would clobber the topbar token count.
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"stream-event","session_id":"s1","uuid":"u","parent_tool_use_id":"task1","event":{"type":"message_start","message":{"id":"sub1","role":"assistant","model":"m","usage":{"input_tokens":1,"output_tokens":2}}}}`))
	// Assert
	if len(frames) != 0 {
		t.Errorf("subagent message_start emitted %d frames, want none: %+v", len(frames), frames)
	}
}

func TestTranslatorSubagentMessageDeltaEmitsNoUsage(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"stream-event","session_id":"s1","uuid":"u","parent_tool_use_id":"task1","event":{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"input_tokens":5,"output_tokens":9}}}`))
	// Assert
	if len(frames) != 0 {
		t.Errorf("subagent message_delta emitted %d frames, want none: %+v", len(frames), frames)
	}
}

func TestTranslatorTextDeltasAccumulateAcrossChunks(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	tr.OnEvent(streamEvt(t, `{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}`))
	tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"foo"}}`))
	tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"bar"}}`))
	// Act
	frames := tr.OnEvent(streamEvt(t, `{"type":"content_block_stop","index":0}`))
	// Assert
	if frames[0].(*protocol.TextEndFrame).FinalText != "foobar" {
		t.Errorf("final = %q, want foobar", frames[0].(*protocol.TextEndFrame).FinalText)
	}
}

// --- thinking ---------------------------------------------------------------

func TestTranslatorThinkingBlockLifecycleWithSignature(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	// Act
	var frames []protocol.L2Frame
	frames = append(frames, tr.OnEvent(streamEvt(t, `{"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":""}}`))...)
	frames = append(frames, tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"hmm"}}`))...)
	frames = append(frames, tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"sig1"}}`))...)
	frames = append(frames, tr.OnEvent(streamEvt(t, `{"type":"content_block_stop","index":0}`))...)
	// Assert
	wantTypes(t, frames, "thinking-start", "thinking-delta", "thinking-end")
	end := frames[2].(*protocol.ThinkingEndFrame)
	if end.FinalText != "hmm" || end.Signature != "sig1" {
		t.Errorf("thinking-end = %+v", end)
	}
}

// --- tool use ----------------------------------------------------------------

func TestTranslatorToolUseStreamingLifecycle(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	// Act
	var frames []protocol.L2Frame
	frames = append(frames, tr.OnEvent(streamEvt(t, `{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"t1","name":"Bash","input":{}}}`))...)
	frames = append(frames, tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"command\":"}}`))...)
	frames = append(frames, tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"\"ls\"}"}}`))...)
	frames = append(frames, tr.OnEvent(streamEvt(t, `{"type":"content_block_stop","index":0}`))...)
	// Assert
	wantTypes(t, frames, "tool-use-start", "tool-use-input-delta", "tool-use-input-delta", "tool-use-input-end")
	start := frames[0].(*protocol.ToolUseStartFrame)
	if start.ToolUseID != "t1" || start.ToolName != "Bash" || start.MessageID != "msg1" {
		t.Errorf("tool-use-start = %+v", start)
	}
	end := frames[3].(*protocol.ToolUseInputEndFrame)
	if string(end.Input) != `{"command":"ls"}` {
		t.Errorf("input = %s", end.Input)
	}
}

func TestTranslatorToolInputFallsBackToStartInput(t *testing.T) {
	// Arrange — no input_json_delta frames stream before the stop.
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	tr.OnEvent(streamEvt(t, `{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"t1","name":"Read","input":{"file_path":"/x"}}}`))
	// Act
	frames := tr.OnEvent(streamEvt(t, `{"type":"content_block_stop","index":0}`))
	// Assert
	end := frames[0].(*protocol.ToolUseInputEndFrame)
	if string(end.Input) != `{"file_path":"/x"}` {
		t.Errorf("input = %s", end.Input)
	}
}

func TestTranslatorTruncatedToolInputFallsBackToEmptyObject(t *testing.T) {
	// Arrange — partial JSON never completes.
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	tr.OnEvent(streamEvt(t, `{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"t1","name":"Bash","input":{}}}`))
	tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"comm"}}`))
	// Act
	frames := tr.OnEvent(streamEvt(t, `{"type":"content_block_stop","index":0}`))
	// Assert
	if string(frames[0].(*protocol.ToolUseInputEndFrame).Input) != "{}" {
		t.Errorf("input = %s", frames[0].(*protocol.ToolUseInputEndFrame).Input)
	}
}

func TestTranslatorParentToolUseIDPropagatesToToolUseStart(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"stream-event","session_id":"s1","uuid":"u","parent_tool_use_id":"task1","event":{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"t2","name":"Grep","input":{}}}}`))
	// Assert
	if frames[0].(*protocol.ToolUseStartFrame).ParentToolUseID != "task1" {
		t.Errorf("parent = %+v", frames[0])
	}
}

// --- tool results --------------------------------------------------------------

func TestTranslatorToolResultCarriesBashRenderHint(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	tr.OnEvent(streamEvt(t, `{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"t1","name":"Bash","input":{}}}`))
	tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"command\":\"ls\"}"}}`))
	tr.OnEvent(streamEvt(t, `{"type":"content_block_stop","index":0}`))
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"tool-result","session_id":"s1","uuid":"u","tool_use_id":"t1","is_error":false,"content":"file-a\nfile-b"}`))
	// Assert
	wantTypes(t, frames, "tool-use-result")
	result := frames[0].(*protocol.ToolUseResultFrame)
	if result.Render == nil || result.Render.Kind != "bash" || result.Render.Stdout != "file-a\nfile-b" {
		t.Errorf("render = %+v", result.Render)
	}
}

func TestTranslatorToolResultWithoutKnownToolHasNoRender(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"tool-result","session_id":"s1","uuid":"u","tool_use_id":"t-unknown","is_error":false,"content":"x"}`))
	// Assert
	if frames[0].(*protocol.ToolUseResultFrame).Render != nil {
		t.Errorf("render = %+v, want nil", frames[0].(*protocol.ToolUseResultFrame).Render)
	}
}

// --- assistant-message synthesis -----------------------------------------------

func TestTranslatorAssistantMessageSynthesizesUnstreamedBlocks(t *testing.T) {
	// Arrange — no stream events preceded this message id. The mirror is
	// pinned to the fixture's model so this test sees only block synthesis.
	tr := NewTranslator()
	tr.Model = "m"
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"assistant-message","session_id":"s1","uuid":"u","message":{"id":"msgX","role":"assistant","model":"m","stop_reason":"end_turn","content":[{"type":"text","text":"full"},{"type":"tool_use","id":"t9","name":"Read","input":{"file_path":"/f"}}],"usage":{"input_tokens":1,"output_tokens":1}}}`))
	// Assert
	wantTypes(t, frames,
		"text-start", "text-delta", "text-end",
		"tool-use-start", "tool-use-input-end")
}

func TestTranslatorAssistantMessageDedupesStreamedMessage(t *testing.T) {
	// Arrange — the same message id already streamed.
	tr := NewTranslator()
	tr.Model = "m"
	startMessage(t, tr, "msgY")
	runTextBlock(t, tr, 0, "already streamed")
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"assistant-message","session_id":"s1","uuid":"u","message":{"id":"msgY","role":"assistant","model":"m","stop_reason":"end_turn","content":[{"type":"text","text":"already streamed"}],"usage":{"input_tokens":1,"output_tokens":1}}}`))
	// Assert
	if len(frames) != 0 {
		t.Errorf("frames = %v, want none", frameTypes(frames))
	}
}

// --- model mirror ---------------------------------------------------------------

// assistantMsg builds an assistant-message event line reporting MODEL,
// optionally as a subagent (non-empty PARENT).
func assistantMsg(id, model, parent string) string {
	p := ""
	if parent != "" {
		p = fmt.Sprintf(`"parent_tool_use_id":%q,`, parent)
	}
	return fmt.Sprintf(`{"type":"assistant-message","session_id":"s1","uuid":"u",%s"message":{"id":%q,"role":"assistant","model":%q,"stop_reason":"end_turn","content":[{"type":"text","text":"hi"}],"usage":{"input_tokens":1,"output_tokens":1}}}`, p, id, model)
}

func TestTranslatorAssistantMessageAnnouncesAnAgentModelSwitch(t *testing.T) {
	// Arrange — the mirror believes the session is on opus.
	tr := NewTranslator()
	tr.Model = "opus"
	// Act — the agent answers on haiku instead (a /model, a fallback).
	frames := tr.OnEvent(evt(t, assistantMsg("msg1", "haiku", "")))
	// Assert
	if got := frames[0].(*protocol.ModelChangedFrame); got.Model != "haiku" || got.Origin != "agent" {
		t.Errorf("model-changed = %+v, want model haiku origin agent", got)
	}
	if tr.Model != "haiku" {
		t.Errorf("mirror = %q, want haiku", tr.Model)
	}
}

func TestTranslatorAssistantMessageAnnouncesASwitchEvenWhenTheMessageStreamed(t *testing.T) {
	// Arrange — the streamed-dedup path is the COMMON case, so a mirror
	// blind to it would be blind to nearly every real switch.
	tr := NewTranslator()
	tr.Model = "opus"
	startMessage(t, tr, "msgS")
	runTextBlock(t, tr, 0, "streamed")
	// Act
	frames := tr.OnEvent(evt(t, assistantMsg("msgS", "haiku", "")))
	// Assert — the blocks are still deduped, but the switch is announced.
	wantTypes(t, frames, "model-changed")
}

func TestTranslatorAssistantMessageIgnoresASubagentModel(t *testing.T) {
	// Arrange — a Haiku subagent under an Opus session.
	tr := NewTranslator()
	tr.Model = "opus"
	// Act
	tr.OnEvent(evt(t, assistantMsg("msg2", "haiku", "toolu_1")))
	// Assert — the topbar must not flip to the subagent's model.
	if tr.Model != "opus" {
		t.Errorf("mirror = %q, want opus (a sidechain model leaked)", tr.Model)
	}
}

func TestTranslatorAssistantMessageEmitsNoFrameForASubagentModel(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	tr.Model = "opus"
	// Act
	frames := tr.OnEvent(evt(t, assistantMsg("msg2", "haiku", "toolu_1")))
	// Assert — block synthesis only, no model-changed.
	wantTypes(t, frames, "text-start", "text-delta", "text-end")
}

func TestTranslatorAssistantMessageIsSilentWhenTheModelIsUnchanged(t *testing.T) {
	// Arrange — the steady state: every turn reports the same model.
	tr := NewTranslator()
	tr.Model = "opus"
	// Act
	frames := tr.OnEvent(evt(t, assistantMsg("msg3", "opus", "")))
	// Assert — an unchanged model puts nothing on the wire.
	wantTypes(t, frames, "text-start", "text-delta", "text-end")
}

func TestTranslatorSetModelIgnoresAnEmptyModel(t *testing.T) {
	// Arrange — an assistant message with no model must not blank the mirror.
	tr := NewTranslator()
	tr.Model = "opus"
	// Act
	frame := tr.SetModel("", "agent")
	// Assert
	if frame != nil || tr.Model != "opus" {
		t.Errorf("frame = %v, mirror = %q, want nil / opus", frame, tr.Model)
	}
}

func TestTranslatorSetModelCmdAnnouncesTheSwitchOnlyOnAck(t *testing.T) {
	// Arrange — a switch the SDK has not confirmed is not a switch.
	tr := NewTranslator()
	tr.Model = "opus"
	// Act
	tr.OnSetModelCmd(&protocol.L1Command{Type: "set-model", RequestID: "r1", Model: "haiku"})
	// Assert — nothing moved yet.
	if tr.Model != "opus" {
		t.Errorf("mirror = %q, want opus (announced before the ack)", tr.Model)
	}
}

func TestTranslatorAckOfSetModelEmitsModelChanged(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	tr.Model = "opus"
	tr.OnSetModelCmd(&protocol.L1Command{Type: "set-model", RequestID: "r1", Model: "haiku"})
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"ack","session_id":"s1","request_id":"r1"}`))
	// Assert
	if got := frames[0].(*protocol.ModelChangedFrame); got.Model != "haiku" || got.Origin != "user" {
		t.Errorf("model-changed = %+v, want model haiku origin user", got)
	}
}

func TestTranslatorAckOfSetPermissionModeStillEmitsModeChanged(t *testing.T) {
	// Arrange — the two pending maps share the ack path; neither may
	// shadow the other.
	tr := NewTranslator()
	tr.OnSetPermissionModeCmd(&protocol.L1Command{Type: "set-permission-mode", RequestID: "r1", Mode: "plan"})
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"ack","session_id":"s1","request_id":"r1"}`))
	// Assert
	wantTypes(t, frames, "permission-mode-changed")
}

func TestTranslatorModelsEventCachesAndForwardsTheMenu(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"models","session_id":"s1","models":[{"value":"opus","displayName":"Opus","description":"d"}]}`))
	// Assert — forwarded to attached clients AND cached for later hellos.
	wantTypes(t, frames, "models")
	if len(tr.Models) != 1 || tr.Models[0].Value != "opus" {
		t.Errorf("cached models = %+v, want one opus entry", tr.Models)
	}
}

func TestTranslatorCommandsEventCachesTheMenu(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"commands","session_id":"s1","commands":[{"name":"compact","description":"d","argumentHint":"<how>"}]}`))
	// Assert — cached for the HTTP readers; no L2 frame, since the Emacs
	// input panel reads the menu over HTTP rather than off the socket.
	wantTypes(t, frames)
	if len(tr.Commands) != 1 || tr.Commands[0].Name != "compact" {
		t.Errorf("cached commands = %+v, want one compact entry", tr.Commands)
	}
}

func TestTranslatorCommandsEventKeepsTheArgumentHint(t *testing.T) {
	// Arrange — the hint is what the completion annotation renders.
	tr := NewTranslator()
	// Act
	tr.OnEvent(evt(t, `{"type":"commands","session_id":"s1","commands":[{"name":"compact","description":"d","argumentHint":"<how>"}]}`))
	// Assert
	if tr.Commands[0].ArgumentHint != "<how>" {
		t.Errorf("argument hint = %q, want %q", tr.Commands[0].ArgumentHint, "<how>")
	}
}

func TestTranslatorCommandsEventDedupesByName(t *testing.T) {
	// Arrange — a skill installed at BOTH user and project scope is resolved
	// once per scope, so the SDK genuinely reports it twice.
	tr := NewTranslator()
	// Act
	tr.OnEvent(evt(t, `{"type":"commands","session_id":"s1","commands":[{"name":"debug-logs","description":"user","argumentHint":""},{"name":"debug-logs","description":"project","argumentHint":""}]}`))
	// Assert — one row, and it is the first-resolved one.
	if len(tr.Commands) != 1 {
		t.Fatalf("cached commands = %+v, want the duplicate collapsed to one", tr.Commands)
	}
	if tr.Commands[0].Description != "user" {
		t.Errorf("kept description = %q, want the first-resolved %q", tr.Commands[0].Description, "user")
	}
}

func TestTranslatorCommandsEventReplacesRatherThanMergesTheMenu(t *testing.T) {
	// Arrange — a refresh republishes the whole list.
	tr := NewTranslator()
	tr.OnEvent(evt(t, `{"type":"commands","session_id":"s1","commands":[{"name":"gone","description":"d","argumentHint":""}]}`))
	// Act — the skill was deleted, so the re-probe no longer reports it.
	tr.OnEvent(evt(t, `{"type":"commands","session_id":"s1","commands":[{"name":"kept","description":"d","argumentHint":""}]}`))
	// Assert — merging would keep offering a command that no longer exists.
	if len(tr.Commands) != 1 || tr.Commands[0].Name != "kept" {
		t.Errorf("cached commands = %+v, want only the re-probed entry", tr.Commands)
	}
}

// --- result and the block-closure invariant --------------------------------------

func TestTranslatorResultMapsFields(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"result","session_id":"s1","uuid":"u","subtype":"success","duration_ms":10,"duration_api_ms":8,"num_turns":1,"total_cost_usd":0.02,"usage":{"input_tokens":1,"output_tokens":2},"result":"done","is_error":false}`))
	// Assert
	wantTypes(t, frames, "result")
	result := frames[0].(*protocol.ResultFrame)
	if result.Subtype != "success" || result.ResultText != "done" || result.DurationMS != 10 {
		t.Errorf("result = %+v", result)
	}
}

func TestTranslatorResultClosesDanglingBlocks(t *testing.T) {
	// Arrange — a text block is left open (interrupted turn).
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	tr.OnEvent(streamEvt(t, `{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}`))
	tr.OnEvent(streamEvt(t, `{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"partial"}}`))
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"result","session_id":"s1","uuid":"u","subtype":"aborted","duration_ms":1,"duration_api_ms":1,"num_turns":1,"total_cost_usd":0,"usage":{"input_tokens":0,"output_tokens":0},"is_error":true}`))
	// Assert — §2.4: the close frame precedes the result frame.
	wantTypes(t, frames, "text-end", "result")
	if frames[0].(*protocol.TextEndFrame).FinalText != "partial" {
		t.Errorf("final = %+v", frames[0])
	}
}

func TestTranslatorResultCancelsPendingPermissions(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	tr.OnEvent(evt(t, `{"type":"permission-request","session_id":"s1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{"command":"ls"}}`))
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"result","session_id":"s1","uuid":"u","subtype":"aborted","duration_ms":1,"duration_api_ms":1,"num_turns":1,"total_cost_usd":0,"usage":{"input_tokens":0,"output_tokens":0},"is_error":true}`))
	// Assert
	wantTypes(t, frames, "permission-resolved", "result")
	resolved := frames[0].(*protocol.PermissionResolvedFrame)
	if resolved.Decision != "cancel" || resolved.RequestID != "p1" {
		t.Errorf("resolved = %+v", resolved)
	}
}

// --- permissions ------------------------------------------------------------------

func TestTranslatorPermissionRequestBuildsBashPreview(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"permission-request","session_id":"s1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{"command":"rm -rf /tmp/x"}}`))
	// Assert
	wantTypes(t, frames, "permission-request")
	preview := frames[0].(*protocol.PermissionRequestFrame).Preview
	if preview == nil || preview.Kind != "bash" || preview.Command != "rm -rf /tmp/x" {
		t.Errorf("preview = %+v", preview)
	}
}

func TestTranslatorPermissionDecisionResolvesPendingRequest(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	tr.OnEvent(evt(t, `{"type":"permission-request","session_id":"s1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{"command":"ls"}}`))
	cmd, err := protocol.DecodeCommand([]byte(`{"type":"permission-decision","request_id":"p1","decision":{"behavior":"deny","message":"no"}}`))
	if err != nil {
		t.Fatal(err)
	}
	// Act
	frame, pending := tr.OnPermissionDecisionCmd(cmd)
	// Assert
	if !pending {
		t.Fatal("pending = false, want true")
	}
	resolved := frame.(*protocol.PermissionResolvedFrame)
	if resolved.Decision != "deny" || resolved.Message != "no" {
		t.Errorf("resolved = %+v", resolved)
	}
}

func TestTranslatorStalePermissionDecisionIsRejected(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	cmd, err := protocol.DecodeCommand([]byte(`{"type":"permission-decision","request_id":"ghost","decision":{"behavior":"allow"}}`))
	if err != nil {
		t.Fatal(err)
	}
	// Act
	_, pending := tr.OnPermissionDecisionCmd(cmd)
	// Assert
	if pending {
		t.Error("pending = true, want false for unknown request_id")
	}
}

func TestTranslatorInterruptCancelsPendingPermissions(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	tr.OnEvent(evt(t, `{"type":"permission-request","session_id":"s1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{}}`))
	// Act
	frames := tr.OnInterruptCmd()
	// Assert
	wantTypes(t, frames, "permission-resolved")
	if frames[0].(*protocol.PermissionResolvedFrame).Decision != "cancel" {
		t.Errorf("decision = %+v", frames[0])
	}
}

// --- permission mode ---------------------------------------------------------------

func TestTranslatorAckAfterSetPermissionModeEmitsModeChanged(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	cmd, err := protocol.DecodeCommand([]byte(`{"type":"set-permission-mode","request_id":"m1","mode":"acceptEdits"}`))
	if err != nil {
		t.Fatal(err)
	}
	tr.OnSetPermissionModeCmd(cmd)
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"ack","session_id":"s1","request_id":"m1"}`))
	// Assert
	wantTypes(t, frames, "permission-mode-changed")
	changed := frames[0].(*protocol.PermissionModeChangedFrame)
	if changed.Mode != protocol.PermissionModeAcceptEdits || changed.Origin != "user" {
		t.Errorf("changed = %+v", changed)
	}
	if tr.PermissionMode != protocol.PermissionModeAcceptEdits {
		t.Errorf("PermissionMode = %v", tr.PermissionMode)
	}
}

func TestTranslatorAckWithoutPendingModeEmitsNothing(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"ack","session_id":"s1","request_id":"shutdown-1"}`))
	// Assert
	if len(frames) != 0 {
		t.Errorf("frames = %v, want none", frameTypes(frames))
	}
}

// --- user-turn --------------------------------------------------------------------

func TestTranslatorUserMessageCmdNormalizesStringContent(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	cmd, err := protocol.DecodeCommand([]byte(`{"type":"user-message","request_id":"r1","content":"hi there"}`))
	if err != nil {
		t.Fatal(err)
	}
	// Act
	frame := tr.OnUserMessageCmd(cmd).(*protocol.UserTurnFrame)
	// Assert
	var blocks []map[string]string
	if err := json.Unmarshal(frame.Content, &blocks); err != nil {
		t.Fatalf("content: %v", err)
	}
	if len(blocks) != 1 || blocks[0]["type"] != "text" || blocks[0]["text"] != "hi there" {
		t.Errorf("blocks = %v", blocks)
	}
}

func TestTranslatorUserMessageCmdKeepsBlockContent(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	cmd, err := protocol.DecodeCommand([]byte(`{"type":"user-message","request_id":"r1","content":[{"type":"text","text":"block"}]}`))
	if err != nil {
		t.Fatal(err)
	}
	// Act
	frame := tr.OnUserMessageCmd(cmd).(*protocol.UserTurnFrame)
	// Assert
	if string(frame.Content) != `[{"type":"text","text":"block"}]` {
		t.Errorf("content = %s", frame.Content)
	}
}

// --- system -----------------------------------------------------------------------

func TestTranslatorSystemInitUpdatesSessionInfo(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"init","data":{"model":"opus-x","cwd":"/work","permissionMode":"plan"}}`))
	// Assert — init's model is ANNOUNCED, not merely recorded, so a client
	// that attached before it does not sit on the hello's empty model.
	wantTypes(t, frames, "model-changed", "system")
	if tr.Model != "opus-x" || tr.CWD != "/work" || tr.PermissionMode != protocol.PermissionModePlan {
		t.Errorf("info = %q %q %q", tr.Model, tr.CWD, tr.PermissionMode)
	}
}

func TestTranslatorSystemInitIsSilentWhenTheModelMatchesTheRequest(t *testing.T) {
	// Arrange — the session was created asking for the model init reports,
	// so init confirms the mirror rather than moving it.
	tr := NewTranslator()
	tr.Model = "opus-x"
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"init","data":{"model":"opus-x","cwd":"/work"}}`))
	// Assert
	wantTypes(t, frames, "system")
}

func TestTranslatorCompactBoundaryMapsToDedicatedFrame(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"compact_boundary","data":{"compact_metadata":{"trigger":"auto","pre_tokens":9000}}}`))
	// Assert
	wantTypes(t, frames, "compact-boundary")
	cb := frames[0].(*protocol.CompactBoundaryFrame)
	if cb.Trigger != "auto" || cb.PreTokens != 9000 {
		t.Errorf("compact = %+v", cb)
	}
}

func TestTranslatorCompactBoundaryReportsPostTokens(t *testing.T) {
	// Arrange — the current SDK reports the post-compaction size in
	// compact_metadata; the boundary frame must carry it so the topbar can
	// re-anchor its token count instead of showing "unknown".
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"compact_boundary","data":{"compact_metadata":{"trigger":"manual","pre_tokens":200000,"post_tokens":18000}}}`))
	// Assert
	wantTypes(t, frames, "compact-boundary")
	cb := frames[0].(*protocol.CompactBoundaryFrame)
	if cb.PostTokens != 18000 {
		t.Errorf("post tokens = %d, want 18000", cb.PostTokens)
	}
}

func TestTranslatorCompactBoundaryPostTokensZeroWhenOmitted(t *testing.T) {
	// Arrange — post_tokens is optional; an SDK that omits it leaves the size
	// unknown (0), never fabricated.
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"compact_boundary","data":{"compact_metadata":{"trigger":"auto","pre_tokens":9000}}}`))
	// Assert
	cb := frames[0].(*protocol.CompactBoundaryFrame)
	if cb.PostTokens != 0 {
		t.Errorf("post tokens = %d, want 0", cb.PostTokens)
	}
}

func TestTranslatorCompactingStatusMapsToCompactStatusFrame(t *testing.T) {
	// Arrange — the SDK announces compaction start with status:"compacting".
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"status","data":{"type":"system","subtype":"status","status":"compacting"}}`))
	// Assert
	wantTypes(t, frames, "compact-status")
	cs := frames[0].(*protocol.CompactStatusFrame)
	if !cs.Active {
		t.Errorf("compact-status Active = %v, want true", cs.Active)
	}
}

func TestTranslatorNonCompactingStatusDropped(t *testing.T) {
	// Arrange — every other status value (requesting, ...) has no Layer-2
	// meaning and must not emit a frame.
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"status","data":{"type":"system","subtype":"status","status":"requesting"}}`))
	// Assert
	wantTypes(t, frames)
}

func TestTranslatorToolUseProgressMapsToDedicatedFrame(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act — §2.9: never forwarded as a system frame.
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"tool_use_progress","data":{"tool_use_id":"t1","tool_name":"Bash","elapsed_time_seconds":3}}`))
	// Assert
	wantTypes(t, frames, "tool-use-progress")
	progress := frames[0].(*protocol.ToolUseProgressFrame)
	if progress.ToolUseID != "t1" || progress.Text == "" {
		t.Errorf("progress = %+v", progress)
	}
}

func TestTranslatorToolUseProgressCarriesStructuredFields(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"tool_use_progress","data":{"tool_use_id":"t1","tool_name":"Bash","elapsed_time_seconds":3.5}}`))
	// Assert — the raw clock and name travel beside the synthesized text.
	progress := frames[0].(*protocol.ToolUseProgressFrame)
	if progress.ToolName != "Bash" || progress.ElapsedSeconds != 3.5 {
		t.Errorf("progress = %+v", progress)
	}
}

func TestTranslatorToolUseProgressAttributesSubagentParent(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act — a subagent's heartbeat names its spawning tool call.
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"tool_use_progress","data":{"tool_use_id":"t1","tool_name":"Grep","parent_tool_use_id":"agent1","elapsed_time_seconds":2}}`))
	// Assert
	progress := frames[0].(*protocol.ToolUseProgressFrame)
	if progress.ParentToolUseID != "agent1" {
		t.Errorf("parent = %q", progress.ParentToolUseID)
	}
}

func TestTranslatorToolUseProgressNullParentStaysEmpty(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act — the SDK sends parent_tool_use_id: null for main-chain tools.
	frames := tr.OnEvent(evt(t, `{"type":"system","session_id":"s1","uuid":"u","subtype":"tool_use_progress","data":{"tool_use_id":"t1","tool_name":"Bash","parent_tool_use_id":null,"elapsed_time_seconds":1}}`))
	// Assert — omitempty keeps the field off the wire entirely.
	progress := frames[0].(*protocol.ToolUseProgressFrame)
	if progress.ParentToolUseID != "" {
		t.Errorf("parent = %q", progress.ParentToolUseID)
	}
}

// --- errors and closure --------------------------------------------------------------

func TestTranslatorShimErrorMapsCode(t *testing.T) {
	tests := []struct {
		name     string
		l1Code   string
		reqID    string
		wantCode string
		wantRec  bool
	}{
		{"sdk_throw becomes sdk_error", "sdk_throw", "", "sdk_error", false},
		{"transport passes through", "transport", "", "transport", false},
		{"bad_command becomes internal and recoverable", "bad_command", "r1", "internal", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			tr := NewTranslator()
			line := fmt.Sprintf(`{"type":"error","session_id":"s1","code":%q,"message":"m"`, tt.l1Code)
			if tt.reqID != "" {
				line += fmt.Sprintf(`,"request_id":%q`, tt.reqID)
			}
			line += "}"
			// Act
			frames := tr.OnEvent(evt(t, line))
			// Assert
			frame := frames[0].(*protocol.ErrorFrame)
			if frame.Code != tt.wantCode || frame.Recoverable != tt.wantRec {
				t.Errorf("frame = %+v, want code %s recoverable %v", frame, tt.wantCode, tt.wantRec)
			}
		})
	}
}

func TestTranslatorFatalClosedEmitsShimDied(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"closed","session_id":"s1","exit_code":1,"reason":"fatal_error"}`))
	// Assert
	wantTypes(t, frames, "error")
	frame := frames[0].(*protocol.ErrorFrame)
	if frame.Code != "shim_died" || frame.Recoverable {
		t.Errorf("frame = %+v", frame)
	}
}

func TestTranslatorCleanClosedEmitsNothing(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"closed","session_id":"s1","exit_code":0,"reason":"shutdown"}`))
	// Assert
	if len(frames) != 0 {
		t.Errorf("frames = %v, want none", frameTypes(frames))
	}
}

func TestTranslatorClosedClosesDanglingBlocksAndCancelsPermissions(t *testing.T) {
	// Arrange — open block plus pending permission at shim death.
	tr := NewTranslator()
	startMessage(t, tr, "msg1")
	tr.OnEvent(streamEvt(t, `{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}`))
	tr.OnEvent(evt(t, `{"type":"permission-request","session_id":"s1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{}}`))
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"closed","session_id":"s1","exit_code":1,"reason":"fatal_error"}`))
	// Assert
	wantTypes(t, frames, "text-end", "permission-resolved", "error")
}

func TestTranslatorReadyRecordsPermissionMode(t *testing.T) {
	// Arrange
	tr := NewTranslator()
	// Act
	frames := tr.OnEvent(evt(t, `{"type":"ready","session_id":"s1","shim_version":"1","sdk_version":"2","permission_mode":"bypassPermissions"}`))
	// Assert
	if len(frames) != 0 {
		t.Errorf("frames = %v, want none", frameTypes(frames))
	}
	if tr.PermissionMode != protocol.PermissionModeBypassPermissions {
		t.Errorf("PermissionMode = %v", tr.PermissionMode)
	}
}
