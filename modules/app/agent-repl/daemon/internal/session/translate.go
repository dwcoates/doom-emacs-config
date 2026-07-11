// Package session hosts the per-session daemon machinery: the Layer-1 →
// Layer-2 translator, and the session hub that stamps, retains and
// broadcasts frames to WebSocket clients.
package session

import (
	"encoding/json"
	"fmt"
	"sort"
	"strings"

	"claude-repld/internal/protocol"
)

// Translator converts Layer-1 shim events into unstamped Layer-2 frames
// (§2 of shared/protocol.md). It owns all streaming-block state and the
// §2.4 block-closure invariant: every *-start frame it emits is closed
// before the turn's result frame, even on interrupt/error paths.
//
// Translator is not goroutine-safe; the session hub serializes access.
type Translator struct {
	blockCounter int
	// open streaming blocks keyed by "<parent_tool_use_id>/<index>".
	open map[string]*openBlock
	// current streaming message id keyed by parent_tool_use_id ("" = top).
	currentMessage map[string]string
	// message ids whose blocks streamed (skip assistant-message synthesis).
	streamed map[string]bool
	// tool_use_id → metadata, for render hints on tool-use-result.
	tools map[string]*toolMeta
	// pending permission request_id → metadata.
	pendingPerms map[string]*permMeta
	// pending set-permission-mode request_id → requested mode.
	pendingModes map[string]protocol.PermissionMode

	// Session-info mirror for hello frames. Model and CWD are seeded
	// with the CreateOpts-requested values and overwritten by the
	// authoritative system:init payload once the SDK reports in.
	Model          string
	CWD            string
	PermissionMode protocol.PermissionMode
	// ClaudeSessionID is the CLI-assigned session uuid captured from
	// system:init. Empty until init arrives. This is the DURABLE id
	// (usable as CreateOpts.Resume across daemon restarts), unlike the
	// ephemeral daemon s_<hex> id.
	ClaudeSessionID string
}

type openBlock struct {
	kind      string // text | thinking | tool_use
	id        string // daemon-assigned block_id (text/thinking)
	messageID string
	toolUseID string
	text      strings.Builder
	signature string
	inputJSON strings.Builder
	// startInput is the content_block_start input object, used when no
	// input_json_delta frames follow.
	startInput json.RawMessage
}

type toolMeta struct {
	name  string
	input json.RawMessage
}

type permMeta struct {
	toolUseID string
	toolName  string
}

// NewTranslator returns an empty translator.
func NewTranslator() *Translator {
	return &Translator{
		open:           map[string]*openBlock{},
		currentMessage: map[string]string{},
		streamed:       map[string]bool{},
		tools:          map[string]*toolMeta{},
		pendingPerms:   map[string]*permMeta{},
		pendingModes:   map[string]protocol.PermissionMode{},
		PermissionMode: protocol.PermissionModeDefault,
	}
}

func (t *Translator) nextBlockID() string {
	t.blockCounter++
	return fmt.Sprintf("b%d", t.blockCounter)
}

// OnEvent translates one Layer-1 event into zero or more Layer-2 frames.
func (t *Translator) OnEvent(evt *protocol.L1Event) []protocol.L2Frame {
	switch evt.Type {
	case "ready":
		if protocol.ValidPermissionMode(evt.PermissionMode) {
			t.PermissionMode = protocol.PermissionMode(evt.PermissionMode)
		}
		return nil
	case "ack":
		return t.onAck(evt)
	case "stream-event":
		return t.onStreamEvent(evt)
	case "assistant-message":
		return t.onAssistantMessage(evt)
	case "tool-result":
		return t.onToolResult(evt)
	case "result":
		return t.onResult(evt)
	case "permission-request":
		return t.onPermissionRequest(evt)
	case "system":
		return t.onSystem(evt)
	case "error":
		return t.onError(evt)
	case "closed":
		return t.onClosed(evt)
	}
	return nil
}

// --- client-command hooks (invoked by the session hub) ---------------------

// OnUserMessageCmd yields the user-turn broadcast frame for an accepted
// user-message command, with string-shorthand content normalized.
func (t *Translator) OnUserMessageCmd(cmd *protocol.L1Command) protocol.L2Frame {
	content := cmd.Content
	var s string
	if err := json.Unmarshal(cmd.Content, &s); err == nil {
		norm, _ := json.Marshal([]map[string]string{{"type": "text", "text": s}})
		content = norm
	}
	return &protocol.UserTurnFrame{
		Envelope:  protocol.Envelope{Type: "user-turn"},
		RequestID: cmd.RequestID,
		Content:   content,
	}
}

// OnPermissionDecisionCmd resolves a pending permission prompt. The
// boolean reports whether the request_id was actually pending; stale or
// duplicate decisions return false and must not be forwarded to the shim.
func (t *Translator) OnPermissionDecisionCmd(cmd *protocol.L1Command) (protocol.L2Frame, bool) {
	if _, ok := t.pendingPerms[cmd.RequestID]; !ok {
		return nil, false
	}
	delete(t.pendingPerms, cmd.RequestID)
	frame := &protocol.PermissionResolvedFrame{
		Envelope:  protocol.Envelope{Type: "permission-resolved"},
		RequestID: cmd.RequestID,
		Decision:  cmd.Decision.Behavior,
	}
	if cmd.Decision.Behavior == "deny" {
		frame.Message = cmd.Decision.Message
	}
	if len(cmd.Decision.UpdatedInput) > 0 {
		frame.UpdatedInput = cmd.Decision.UpdatedInput
	}
	return frame, true
}

// OnInterruptCmd invalidates pending permission prompts (§2.7 "cancel").
func (t *Translator) OnInterruptCmd() []protocol.L2Frame {
	return t.cancelPendingPermissions("interrupted")
}

// OnShimDeath invalidates pending permission prompts when the shim's
// stdout closes WITHOUT a `closed` event (SIGKILL/crash). §2.7 names
// shim death as a cancel trigger; the graceful path is covered by
// onClosed, so this is the hard-death counterpart.
func (t *Translator) OnShimDeath() []protocol.L2Frame {
	return t.cancelPendingPermissions("shim died")
}

// OnSetPermissionModeCmd records the pending mode change; the
// permission-mode-changed frame is emitted once the shim acks (§1.2).
func (t *Translator) OnSetPermissionModeCmd(cmd *protocol.L1Command) {
	t.pendingModes[cmd.RequestID] = protocol.PermissionMode(cmd.Mode)
}

// --- Layer-1 event handlers -------------------------------------------------

func (t *Translator) onAck(evt *protocol.L1Event) []protocol.L2Frame {
	mode, ok := t.pendingModes[evt.RequestID]
	if !ok {
		return nil
	}
	delete(t.pendingModes, evt.RequestID)
	t.PermissionMode = mode
	return []protocol.L2Frame{&protocol.PermissionModeChangedFrame{
		Envelope: protocol.Envelope{Type: "permission-mode-changed"},
		Mode:     mode,
		Origin:   "user",
	}}
}

// rawStreamEvent is the subset of RawMessageStreamEvent the translator
// inspects.
type rawStreamEvent struct {
	Type    string `json:"type"`
	Index   int    `json:"index"`
	Message *struct {
		ID    string          `json:"id"`
		Usage *protocol.Usage `json:"usage"`
	} `json:"message"`
	ContentBlock *struct {
		Type  string          `json:"type"`
		ID    string          `json:"id"`
		Name  string          `json:"name"`
		Input json.RawMessage `json:"input"`
	} `json:"content_block"`
	Delta *struct {
		Type        string `json:"type"`
		Text        string `json:"text"`
		Thinking    string `json:"thinking"`
		PartialJSON string `json:"partial_json"`
		Signature   string `json:"signature"`
	} `json:"delta"`
	Usage *protocol.Usage `json:"usage"`
}

func (t *Translator) onStreamEvent(evt *protocol.L1Event) []protocol.L2Frame {
	var se rawStreamEvent
	if err := json.Unmarshal(evt.Event, &se); err != nil {
		return []protocol.L2Frame{&protocol.ErrorFrame{
			Envelope:    protocol.Envelope{Type: "error"},
			Code:        "internal",
			Message:     fmt.Sprintf("undecodable stream-event payload: %v", err),
			Recoverable: true,
		}}
	}
	parent := evt.ParentToolUseID
	key := func(index int) string { return fmt.Sprintf("%s/%d", parent, index) }

	switch se.Type {
	case "message_start":
		if se.Message == nil {
			return nil
		}
		t.currentMessage[parent] = se.Message.ID
		t.streamed[se.Message.ID] = true
		if se.Message.Usage != nil {
			return []protocol.L2Frame{&protocol.UsageFrame{
				Envelope:  protocol.Envelope{Type: "usage"},
				MessageID: se.Message.ID,
				Usage:     *se.Message.Usage,
			}}
		}
		return nil

	case "content_block_start":
		if se.ContentBlock == nil {
			return nil
		}
		msgID := t.currentMessage[parent]
		blk := &openBlock{kind: se.ContentBlock.Type, messageID: msgID}
		switch se.ContentBlock.Type {
		case "text":
			blk.id = t.nextBlockID()
			t.open[key(se.Index)] = blk
			return []protocol.L2Frame{&protocol.TextStartFrame{
				Envelope:  protocol.Envelope{Type: "text-start"},
				BlockID:   blk.id,
				MessageID: msgID,
			}}
		case "thinking":
			blk.id = t.nextBlockID()
			t.open[key(se.Index)] = blk
			return []protocol.L2Frame{&protocol.ThinkingStartFrame{
				Envelope:  protocol.Envelope{Type: "thinking-start"},
				BlockID:   blk.id,
				MessageID: msgID,
			}}
		case "tool_use":
			blk.toolUseID = se.ContentBlock.ID
			blk.startInput = se.ContentBlock.Input
			t.open[key(se.Index)] = blk
			t.tools[blk.toolUseID] = &toolMeta{name: se.ContentBlock.Name}
			frame := &protocol.ToolUseStartFrame{
				Envelope:        protocol.Envelope{Type: "tool-use-start"},
				ToolUseID:       blk.toolUseID,
				ToolName:        se.ContentBlock.Name,
				MessageID:       msgID,
				ParentToolUseID: parent,
			}
			return []protocol.L2Frame{frame}
		}
		return nil

	case "content_block_delta":
		blk, ok := t.open[key(se.Index)]
		if !ok || se.Delta == nil {
			return nil
		}
		switch se.Delta.Type {
		case "text_delta":
			blk.text.WriteString(se.Delta.Text)
			return []protocol.L2Frame{&protocol.TextDeltaFrame{
				Envelope: protocol.Envelope{Type: "text-delta"},
				BlockID:  blk.id,
				Text:     se.Delta.Text,
			}}
		case "thinking_delta":
			blk.text.WriteString(se.Delta.Thinking)
			return []protocol.L2Frame{&protocol.ThinkingDeltaFrame{
				Envelope: protocol.Envelope{Type: "thinking-delta"},
				BlockID:  blk.id,
				Text:     se.Delta.Thinking,
			}}
		case "input_json_delta":
			blk.inputJSON.WriteString(se.Delta.PartialJSON)
			return []protocol.L2Frame{&protocol.ToolUseInputDeltaFrame{
				Envelope:    protocol.Envelope{Type: "tool-use-input-delta"},
				ToolUseID:   blk.toolUseID,
				PartialJSON: se.Delta.PartialJSON,
			}}
		case "signature_delta":
			blk.signature += se.Delta.Signature
			return nil
		}
		return nil

	case "content_block_stop":
		blk, ok := t.open[key(se.Index)]
		if !ok {
			return nil
		}
		delete(t.open, key(se.Index))
		return []protocol.L2Frame{t.closeBlock(blk)}

	case "message_delta":
		if se.Usage == nil {
			return nil
		}
		return []protocol.L2Frame{&protocol.UsageFrame{
			Envelope:  protocol.Envelope{Type: "usage"},
			MessageID: t.currentMessage[parent],
			Usage:     *se.Usage,
		}}

	case "message_stop":
		// Blocks normally close via content_block_stop; sweep defensively
		// so a truncated stream cannot leak an open block past its message.
		return t.closeDanglingBlocks()
	}
	return nil
}

// closeBlock emits the closing frame for one open block.
func (t *Translator) closeBlock(blk *openBlock) protocol.L2Frame {
	switch blk.kind {
	case "text":
		return &protocol.TextEndFrame{
			Envelope:  protocol.Envelope{Type: "text-end"},
			BlockID:   blk.id,
			FinalText: blk.text.String(),
		}
	case "thinking":
		return &protocol.ThinkingEndFrame{
			Envelope:  protocol.Envelope{Type: "thinking-end"},
			BlockID:   blk.id,
			FinalText: blk.text.String(),
			Signature: blk.signature,
		}
	default: // tool_use
		input := parseToolInput(blk.inputJSON.String(), blk.startInput)
		if meta, ok := t.tools[blk.toolUseID]; ok {
			meta.input = input
		}
		return &protocol.ToolUseInputEndFrame{
			Envelope:  protocol.Envelope{Type: "tool-use-input-end"},
			ToolUseID: blk.toolUseID,
			Input:     input,
		}
	}
}

// parseToolInput finalizes a tool block's input: accumulated
// input_json_delta buffer first, content_block_start input second, `{}`
// as the guaranteed-parseable fallback for truncated streams.
func parseToolInput(buffered string, startInput json.RawMessage) json.RawMessage {
	if buffered != "" && json.Valid([]byte(buffered)) {
		return json.RawMessage(buffered)
	}
	if buffered == "" && len(startInput) > 0 && string(startInput) != "null" {
		return startInput
	}
	return json.RawMessage("{}")
}

func (t *Translator) closeDanglingBlocks() []protocol.L2Frame {
	if len(t.open) == 0 {
		return nil
	}
	keys := make([]string, 0, len(t.open))
	for k := range t.open {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	frames := make([]protocol.L2Frame, 0, len(keys))
	for _, k := range keys {
		frames = append(frames, t.closeBlock(t.open[k]))
		delete(t.open, k)
	}
	return frames
}

// assistantMessageBody is the assistant-message event's message payload.
type assistantMessageBody struct {
	ID      string `json:"id"`
	Content []struct {
		Type      string          `json:"type"`
		Text      string          `json:"text"`
		Thinking  string          `json:"thinking"`
		Signature string          `json:"signature"`
		ID        string          `json:"id"`
		Name      string          `json:"name"`
		Input     json.RawMessage `json:"input"`
	} `json:"content"`
}

// onAssistantMessage synthesizes block frames for messages that did not
// stream (includePartialMessages off, or replayed history). Messages
// whose blocks already streamed are deduplicated by message id.
func (t *Translator) onAssistantMessage(evt *protocol.L1Event) []protocol.L2Frame {
	var msg assistantMessageBody
	if err := json.Unmarshal(evt.Message, &msg); err != nil {
		return []protocol.L2Frame{&protocol.ErrorFrame{
			Envelope:    protocol.Envelope{Type: "error"},
			Code:        "internal",
			Message:     fmt.Sprintf("undecodable assistant-message payload: %v", err),
			Recoverable: true,
		}}
	}
	if t.streamed[msg.ID] {
		return nil
	}
	var frames []protocol.L2Frame
	for _, block := range msg.Content {
		switch block.Type {
		case "text":
			id := t.nextBlockID()
			frames = append(frames,
				&protocol.TextStartFrame{Envelope: protocol.Envelope{Type: "text-start"}, BlockID: id, MessageID: msg.ID},
				&protocol.TextDeltaFrame{Envelope: protocol.Envelope{Type: "text-delta"}, BlockID: id, Text: block.Text},
				&protocol.TextEndFrame{Envelope: protocol.Envelope{Type: "text-end"}, BlockID: id, FinalText: block.Text},
			)
		case "thinking":
			id := t.nextBlockID()
			frames = append(frames,
				&protocol.ThinkingStartFrame{Envelope: protocol.Envelope{Type: "thinking-start"}, BlockID: id, MessageID: msg.ID},
				&protocol.ThinkingDeltaFrame{Envelope: protocol.Envelope{Type: "thinking-delta"}, BlockID: id, Text: block.Thinking},
				&protocol.ThinkingEndFrame{Envelope: protocol.Envelope{Type: "thinking-end"}, BlockID: id, FinalText: block.Thinking, Signature: block.Signature},
			)
		case "tool_use":
			input := block.Input
			if len(input) == 0 {
				input = json.RawMessage("{}")
			}
			t.tools[block.ID] = &toolMeta{name: block.Name, input: input}
			frames = append(frames,
				&protocol.ToolUseStartFrame{
					Envelope:        protocol.Envelope{Type: "tool-use-start"},
					ToolUseID:       block.ID,
					ToolName:        block.Name,
					MessageID:       msg.ID,
					ParentToolUseID: evt.ParentToolUseID,
				},
				&protocol.ToolUseInputEndFrame{
					Envelope:  protocol.Envelope{Type: "tool-use-input-end"},
					ToolUseID: block.ID,
					Input:     input,
				},
			)
		}
	}
	return frames
}

func (t *Translator) onToolResult(evt *protocol.L1Event) []protocol.L2Frame {
	frame := &protocol.ToolUseResultFrame{
		Envelope:  protocol.Envelope{Type: "tool-use-result"},
		ToolUseID: evt.ToolUseID,
		IsError:   evt.IsError,
		Content:   evt.Content,
	}
	if meta, ok := t.tools[evt.ToolUseID]; ok {
		frame.Render = renderHint(meta.name, meta.input, evt.Content)
	}
	return []protocol.L2Frame{frame}
}

func (t *Translator) onResult(evt *protocol.L1Event) []protocol.L2Frame {
	// §2.4 block-closure invariant: every open block closes before the
	// result frame, including interrupted or failed turns.
	frames := t.closeDanglingBlocks()
	frames = append(frames, t.cancelPendingPermissions("turn ended")...)
	usage := protocol.Usage{}
	if evt.Usage != nil {
		usage = *evt.Usage
	}
	result := &protocol.ResultFrame{
		Envelope:      protocol.Envelope{Type: "result"},
		Subtype:       evt.Subtype,
		DurationMS:    evt.DurationMS,
		DurationAPIMS: evt.DurationAPIMS,
		NumTurns:      evt.NumTurns,
		TotalCostUSD:  evt.TotalCostUSD,
		Usage:         usage,
		IsError:       evt.IsError,
	}
	if evt.Result != nil {
		result.ResultText = *evt.Result
	}
	return append(frames, result)
}

func (t *Translator) onPermissionRequest(evt *protocol.L1Event) []protocol.L2Frame {
	t.pendingPerms[evt.RequestID] = &permMeta{toolUseID: evt.ToolUseID, toolName: evt.ToolName}
	return []protocol.L2Frame{&protocol.PermissionRequestFrame{
		Envelope:  protocol.Envelope{Type: "permission-request"},
		RequestID: evt.RequestID,
		ToolUseID: evt.ToolUseID,
		ToolName:  evt.ToolName,
		Input:     evt.Input,
		Preview:   permissionPreview(evt.ToolName, evt.Input),
	}}
}

func (t *Translator) cancelPendingPermissions(why string) []protocol.L2Frame {
	if len(t.pendingPerms) == 0 {
		return nil
	}
	ids := make([]string, 0, len(t.pendingPerms))
	for id := range t.pendingPerms {
		ids = append(ids, id)
	}
	sort.Strings(ids)
	frames := make([]protocol.L2Frame, 0, len(ids))
	for _, id := range ids {
		delete(t.pendingPerms, id)
		frames = append(frames, &protocol.PermissionResolvedFrame{
			Envelope:  protocol.Envelope{Type: "permission-resolved"},
			RequestID: id,
			Decision:  "cancel",
			Message:   why,
		})
	}
	return frames
}

func (t *Translator) onSystem(evt *protocol.L1Event) []protocol.L2Frame {
	switch evt.Subtype {
	case "init":
		var init struct {
			Model           string `json:"model"`
			CWD             string `json:"cwd"`
			PermissionMode  string `json:"permissionMode"`
			ClaudeSessionID string `json:"session_id"`
		}
		if err := json.Unmarshal(evt.Data, &init); err == nil {
			if init.Model != "" {
				t.Model = init.Model
			}
			if init.CWD != "" {
				t.CWD = init.CWD
			}
			if protocol.ValidPermissionMode(init.PermissionMode) {
				t.PermissionMode = protocol.PermissionMode(init.PermissionMode)
			}
			if init.ClaudeSessionID != "" {
				t.ClaudeSessionID = init.ClaudeSessionID
			}
		}
		return []protocol.L2Frame{&protocol.SystemFrame{
			Envelope: protocol.Envelope{Type: "system"},
			Subtype:  "init",
			Data:     evt.Data,
		}}
	case "slash_command":
		return []protocol.L2Frame{&protocol.SystemFrame{
			Envelope: protocol.Envelope{Type: "system"},
			Subtype:  "slash_command",
			Data:     evt.Data,
		}}
	case "compact_boundary":
		var data struct {
			CompactMetadata struct {
				Trigger   string `json:"trigger"`
				PreTokens int64  `json:"pre_tokens"`
			} `json:"compact_metadata"`
		}
		_ = json.Unmarshal(evt.Data, &data)
		return []protocol.L2Frame{&protocol.CompactBoundaryFrame{
			Envelope:  protocol.Envelope{Type: "compact-boundary"},
			Trigger:   data.CompactMetadata.Trigger,
			PreTokens: data.CompactMetadata.PreTokens,
			// The SDK does not report post-compaction size; 0 = unknown.
			PostTokens: 0,
		}}
	case "tool_use_progress":
		// §2.9: tool_use_progress system events map to the dedicated
		// tool-use-progress frame, never to a system frame.
		var data struct {
			ToolUseID          string  `json:"tool_use_id"`
			ToolName           string  `json:"tool_name"`
			ElapsedTimeSeconds float64 `json:"elapsed_time_seconds"`
		}
		_ = json.Unmarshal(evt.Data, &data)
		return []protocol.L2Frame{&protocol.ToolUseProgressFrame{
			Envelope:  protocol.Envelope{Type: "tool-use-progress"},
			ToolUseID: data.ToolUseID,
			Text:      fmt.Sprintf("%s running (%.0fs)", data.ToolName, data.ElapsedTimeSeconds),
		}}
	}
	return nil
}

func (t *Translator) onError(evt *protocol.L1Event) []protocol.L2Frame {
	code := "internal"
	switch evt.Code {
	case "sdk_throw":
		code = "sdk_error"
	case "transport":
		code = "transport"
	}
	return []protocol.L2Frame{&protocol.ErrorFrame{
		Envelope: protocol.Envelope{Type: "error"},
		Code:     code,
		Message:  evt.MessageText(),
		// Command-scoped shim errors leave the session usable; only
		// out-of-band errors (no request_id) precede a shim death.
		Recoverable: evt.RequestID != "",
	}}
}

func (t *Translator) onClosed(evt *protocol.L1Event) []protocol.L2Frame {
	frames := t.closeDanglingBlocks()
	frames = append(frames, t.cancelPendingPermissions("session closed")...)
	if evt.Reason == "fatal_error" {
		frames = append(frames, &protocol.ErrorFrame{
			Envelope:    protocol.Envelope{Type: "error"},
			Code:        "shim_died",
			Message:     fmt.Sprintf("shim closed with exit code %d", evt.ExitCode),
			Recoverable: false,
		})
	}
	return frames
}
