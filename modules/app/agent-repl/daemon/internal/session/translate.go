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

	// turnActive mirrors whether a user turn is in flight: set on the
	// accepted user-message command, cleared by the turn's result frame
	// (every turn ends in one, §2.4). Introspection-only (GET /sessions
	// turn_active) — no frame carries it.
	turnActive bool
	// pending set-permission-mode request_id → requested mode.
	pendingModes map[string]protocol.PermissionMode
	// pending set-model request_id → requested model.
	pendingModels map[string]string

	// Session-info mirror for hello frames. Model and CWD are seeded
	// with the CreateOpts-requested values and overwritten by the
	// authoritative system:init payload once the SDK reports in.
	//
	// Model is a MIRROR of a value the CLI owns, never a value the daemon
	// decides. It therefore follows observed truth from every direction:
	// system:init, an acked set-model, the model reported on each
	// main-chain assistant message, and the periodic transcript reconcile.
	Model          string
	CWD            string
	PermissionMode protocol.PermissionMode
	// Models is the selectable-model menu from the shim's `models` event.
	Models []protocol.ModelInfo
	// Commands is the invocable slash-command menu from the shim's
	// `commands` event, deduplicated by name. Read back over HTTP by the
	// Emacs input panel, which completes against it.
	Commands []protocol.SlashCommand
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
		pendingModels:  map[string]string{},
		PermissionMode: protocol.PermissionModeDefault,
	}
}

// SetModel adopts MODEL as the session's model and yields the frame
// announcing the move, or nil when MODEL is empty or already the mirror's
// value. The single funnel for every origin, so a no-op switch can never
// put a frame on the wire and each origin cannot drift from the others.
func (t *Translator) SetModel(model, origin string) protocol.L2Frame {
	if model == "" || model == t.Model {
		return nil
	}
	t.Model = model
	return &protocol.ModelChangedFrame{
		Envelope: protocol.Envelope{Type: "model-changed"},
		Model:    model,
		Origin:   origin,
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
	case "models":
		return t.onModels(evt)
	case "commands":
		return t.onCommands(evt)
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
	t.turnActive = true
	return &protocol.UserTurnFrame{
		Envelope:  protocol.Envelope{Type: "user-turn"},
		RequestID: cmd.RequestID,
		Content:   normalizeContent(cmd.Content),
	}
}

// normalizeContent expands string-shorthand content into the wire
// ContentBlock[] shape; already-structured content passes through. Shared
// by the user-turn frame and the queue's snapshot/frames so a queued item
// and the turn it becomes carry byte-identical content.
func normalizeContent(content json.RawMessage) json.RawMessage {
	var s string
	if err := json.Unmarshal(content, &s); err == nil {
		// A []map[string]string cannot fail to marshal, matching the
		// original OnUserMessageCmd behavior exactly.
		norm, _ := json.Marshal([]map[string]string{{"type": "text", "text": s}})
		return norm
	}
	return content
}

// commandText renders content (string shorthand or ContentBlock[]) as a
// plain-text approximation for the classifier prompt and the running-task
// tracker. Best-effort and total: it never fails, returning the raw JSON
// when the shape is unrecognized.
func commandText(content json.RawMessage) string {
	var s string
	if err := json.Unmarshal(content, &s); err == nil {
		return s
	}
	var blocks []struct {
		Type string `json:"type"`
		Text string `json:"text"`
	}
	if err := json.Unmarshal(content, &blocks); err == nil {
		var b strings.Builder
		for _, blk := range blocks {
			if blk.Type == "text" && blk.Text != "" {
				if b.Len() > 0 {
					b.WriteString("\n")
				}
				b.WriteString(blk.Text)
			}
		}
		if b.Len() > 0 {
			return b.String()
		}
	}
	return string(content)
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

// TurnActive reports whether a user turn is currently in flight.
func (t *Translator) TurnActive() bool { return t.turnActive }

// PendingPermissionIDs returns the request ids of unresolved permission
// prompts, sorted for deterministic introspection output.
func (t *Translator) PendingPermissionIDs() []string {
	ids := make([]string, 0, len(t.pendingPerms))
	for id := range t.pendingPerms {
		ids = append(ids, id)
	}
	sort.Strings(ids)
	return ids
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

// OnSetModelCmd records the pending model change; the model-changed
// frame is emitted only once the shim acks (§1.2). Gating on the ack is
// what keeps the topbar from announcing a switch the SDK went on to
// reject.
func (t *Translator) OnSetModelCmd(cmd *protocol.L1Command) {
	t.pendingModels[cmd.RequestID] = cmd.Model
}

// --- Layer-1 event handlers -------------------------------------------------

// onAck resolves whichever pending command the ack belongs to. A
// request_id lives in at most one pending map, so the two lookups cannot
// both hit.
func (t *Translator) onAck(evt *protocol.L1Event) []protocol.L2Frame {
	if mode, ok := t.pendingModes[evt.RequestID]; ok {
		delete(t.pendingModes, evt.RequestID)
		t.PermissionMode = mode
		return []protocol.L2Frame{&protocol.PermissionModeChangedFrame{
			Envelope: protocol.Envelope{Type: "permission-mode-changed"},
			Mode:     mode,
			Origin:   "user",
		}}
	}
	if model, ok := t.pendingModels[evt.RequestID]; ok {
		delete(t.pendingModels, evt.RequestID)
		if frame := t.SetModel(model, "user"); frame != nil {
			return []protocol.L2Frame{frame}
		}
		return nil
	}
	return nil
}

// onModels caches the selectable-model menu (so every later hello carries
// it) and forwards it, so a client already attached populates its picker
// without reconnecting.
func (t *Translator) onModels(evt *protocol.L1Event) []protocol.L2Frame {
	t.Models = evt.Models
	return []protocol.L2Frame{&protocol.ModelsFrame{
		Envelope: protocol.Envelope{Type: "models"},
		Models:   evt.Models,
	}}
}

// onCommands caches the slash-command menu, which the Emacs input panel
// reads back over HTTP to complete against. A `refresh-commands` republishes
// the list, so this REPLACES the cache rather than merging into it: a skill
// deleted since the last probe must disappear from the menu, and merging
// would keep offering it forever.
func (t *Translator) onCommands(evt *protocol.L1Event) []protocol.L2Frame {
	t.Commands = dedupeCommands(evt.Commands)
	return nil
}

// dedupeCommands collapses commands that share a name, keeping the first.
//
// The SDK really does report duplicates: a skill installed both at user
// scope and at project scope is resolved once per scope and reported once
// per resolution. Offering the same name twice would be a completion menu
// with two identical rows, so the list is deduplicated once here rather
// than by every reader.
func dedupeCommands(cmds []protocol.SlashCommand) []protocol.SlashCommand {
	if len(cmds) == 0 {
		return nil
	}
	seen := make(map[string]bool, len(cmds))
	out := make([]protocol.SlashCommand, 0, len(cmds))
	for _, c := range cmds {
		if seen[c.Name] {
			continue
		}
		seen[c.Name] = true
		out = append(out, c)
	}
	return out
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
		// Main-chain only. A subagent's request carries the SUBAGENT's
		// context, so emitting its usage would flip the topbar token count
		// to a sidechain figure for the length of every subagent — the same
		// reason onAssistantMessage filters the model mirror on parent.
		if parent == "" && se.Message.Usage != nil {
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
		// Main-chain only, mirroring message_start: a subagent's usage is
		// not the session's context and must not overwrite the topbar count.
		if parent != "" || se.Usage == nil {
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
	ID string `json:"id"`
	// Model is the model that actually produced this message — the
	// authoritative answer to "what model is this session on", and the
	// one the CLI can move without telling anybody.
	Model   string `json:"model"`
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
//
// It is also where the model mirror learns that the AGENT moved the
// model out from under it: every assistant message names the model that
// produced it, which is the only truth the daemon gets for free.
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
	// Deliberately BEFORE the streamed-dedup below: a message whose blocks
	// already streamed still carries the authoritative model, and skipping
	// it would blind the mirror to the common case (every streamed turn).
	//
	// Main-chain only. A subagent's message names the SUBAGENT's model (a
	// Haiku Explore under an Opus session), so trusting it would flip the
	// topbar to Haiku for the length of every subagent — the same reason
	// the Emacs mode-line filters isSidechain.
	var frames []protocol.L2Frame
	if evt.ParentToolUseID == "" {
		if frame := t.SetModel(msg.Model, "agent"); frame != nil {
			frames = append(frames, frame)
		}
	}
	if t.streamed[msg.ID] {
		return frames
	}
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
	t.turnActive = false
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
		var frames []protocol.L2Frame
		if err := json.Unmarshal(evt.Data, &init); err == nil {
			// Announced, not just recorded: a client that attached before
			// init would otherwise sit on the hello's empty model until it
			// reconnected or the first turn landed.
			if frame := t.SetModel(init.Model, "init"); frame != nil {
				frames = append(frames, frame)
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
		return append(frames, &protocol.SystemFrame{
			Envelope: protocol.Envelope{Type: "system"},
			Subtype:  "init",
			Data:     evt.Data,
		})
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
