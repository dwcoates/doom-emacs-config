// Package frontend is the daemon's frontend surface. It serves
// agentshim.frontend.v1 frames as protojson over a UDS listener (Emacs) and a
// WebSocket endpoint (webapp), translates internal agentshim.core.v1 /
// agentshim.data.v1 events into the resolved frontend vocabulary, and
// dispatches inbound FrontendCommands with CommandAcks.
//
// This file (translate.go) holds the PURE, IO-free translation layer: internal
// events and SSM state in, frontend.v1 protos out. server.go owns transports
// and fan-out; commands.go owns inbound dispatch.
//
// ---------------------------------------------------------------------------
// ConversationDelta item vocabulary (the contract the webapp adapter consumes)
// ---------------------------------------------------------------------------
//
// ConversationDelta.items is a repeated google.protobuf.Struct. Each Struct is
// ONE rendered item. Frontends RENDER these; they never re-interpret raw facts.
// Every item carries a "type" discriminator. The closed set of types and their
// fields is:
//
//	text          a rendered assistant/user text block
//	  type="text", role, uuid, block_index, text
//	thinking      an extended-thinking block
//	  type="thinking", role, uuid, block_index, thinking, signature
//	tool_use      a tool invocation card (name + input)
//	  type="tool_use", role, uuid, block_index, id, name, input(object)
//	tool_result   a tool result card, keyed to its tool_use by tool_use_id
//	  type="tool_result", role, uuid, block_index, tool_use_id,
//	  is_error(bool, omitted when the source did not set it), and EXACTLY ONE
//	  of: content(string) | content_blocks(array of nested item objects)
//	image         an image block (webapp may or may not render it; typed so it
//	              is never a silent drop)
//	  type="image", role, uuid, block_index, media_type
//	task          a detached-task lifecycle chip (agent/shell/workflow)
//	  type="task", task_id, kind, status, description, output_path,
//	  inference(present only for LOST), summary(present when the source had one)
//
// Reconciliation contract: text/thinking/tool_use/tool_result/image items carry
// (uuid, block_index). A TypingDelta preview with the same (uuid, block_index)
// is REPLACED when the ConversationDelta item for that block arrives. task chips
// are keyed by task_id, not uuid, and are not previewed.
//
// Fields whose source value is absent are omitted from the Struct (protojson
// then omits them too) rather than emitted as zero values.
package frontend

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// Conversation item type discriminators.
const (
	itemTypeText       = "text"
	itemTypeThinking   = "thinking"
	itemTypeToolUse    = "tool_use"
	itemTypeToolResult = "tool_result"
	itemTypeImage      = "image"
	itemTypeTask       = "task"

	roleAssistant = "assistant"
	roleUser      = "user"
)

// ---------------------------------------------------------------------------
// Frame wrappers — every FrontendFrame oneof arm, so callers never touch the
// generated oneof wrapper types directly.
// ---------------------------------------------------------------------------

// SnapshotFrame wraps a StateSnapshot.
func SnapshotFrame(s *frontendv1.StateSnapshot) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Snapshot{Snapshot: s}}
}

// WorkspaceStateFrame wraps a WorkspaceState.
func WorkspaceStateFrame(w *frontendv1.WorkspaceState) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_WorkspaceState{WorkspaceState: w}}
}

// SessionViewFrame wraps a SessionView.
func SessionViewFrame(v *frontendv1.SessionView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_SessionView{SessionView: v}}
}

// ConversationDeltaFrame wraps a ConversationDelta.
func ConversationDeltaFrame(c *frontendv1.ConversationDelta) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_ConversationDelta{ConversationDelta: c}}
}

// TypingDeltaFrame wraps a TypingDelta.
func TypingDeltaFrame(t *frontendv1.TypingDelta) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_TypingDelta{TypingDelta: t}}
}

// TaskCatalogFrame wraps a TaskCatalog.
func TaskCatalogFrame(c *frontendv1.TaskCatalog) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_TaskCatalog{TaskCatalog: c}}
}

// CommandAckFrame wraps a CommandAck.
func CommandAckFrame(a *frontendv1.CommandAck) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_CommandAck{CommandAck: a}}
}

// DegradedNoticeFrame wraps a DegradedNotice.
func DegradedNoticeFrame(n *frontendv1.DegradedNotice) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_DegradedNotice{DegradedNotice: n}}
}

// ---------------------------------------------------------------------------
// ContentDelta -> TypingDelta (ephemeral live typing)
// ---------------------------------------------------------------------------

// TypingDeltaFromContentDelta maps a core.ContentDelta to the ephemeral
// TypingDelta relay. It returns nil for delta arms that carry no visible typing
// preview (signature deltas are cryptographic, not display text) — a designed
// classification, not a swallowed value: the block is still delivered in full
// via the ConversationDelta round-trip.
func TypingDeltaFromContentDelta(workspace, sessionID string, cd *corev1.ContentDelta) *frontendv1.TypingDelta {
	if cd == nil {
		return nil
	}
	var kind, delta string
	switch d := cd.GetDelta().(type) {
	case *corev1.ContentDelta_Text:
		kind, delta = "text", d.Text
	case *corev1.ContentDelta_Thinking:
		kind, delta = "thinking", d.Thinking
	case *corev1.ContentDelta_InputJson:
		kind, delta = "input_json", d.InputJson
	case *corev1.ContentDelta_Signature:
		return nil // signature: no visible typing preview
	default:
		return nil
	}
	return &frontendv1.TypingDelta{
		Workspace:  workspace,
		SessionId:  sessionID,
		Uuid:       cd.GetUuid(),
		BlockIndex: cd.GetBlockIndex(),
		Kind:       kind,
		Delta:      delta,
	}
}

// ---------------------------------------------------------------------------
// DegradedState -> DegradedNotice (passthrough)
// ---------------------------------------------------------------------------

// DegradedNoticeFromState maps a core.DegradedState to the frontend
// DegradedNotice. It is a faithful passthrough: honest sad-path reporting, never
// a fallback.
func DegradedNoticeFromState(ds *corev1.DegradedState, atMs int64) *frontendv1.DegradedNotice {
	if ds == nil {
		return nil
	}
	return &frontendv1.DegradedNotice{
		Component: ds.GetComponent(),
		Reason:    ds.GetReason(),
		Recovered: ds.GetRecovered(),
		AtMs:      atMs,
	}
}

// ---------------------------------------------------------------------------
// Events -> ConversationDelta items
// ---------------------------------------------------------------------------

// ConversationDeltaFromEvent translates one core.Event into a ConversationDelta
// carrying the rendered items for that event, or nil when the event yields no
// conversation content (e.g. a turn boundary or a session-lifecycle event, which
// feed WorkspaceState/SessionView instead).
//
// It hard-errors (no silent fallback) when a vendor payload cannot be
// unmarshaled or carries a type URL unknown to the compiled schema set — those
// are genuine anomalies, distinct from a known-but-non-conversational payload.
func ConversationDeltaFromEvent(workspace string, ev *corev1.Event) (*frontendv1.ConversationDelta, error) {
	if ev == nil {
		return nil, nil
	}
	var items []*structpb.Struct
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_TaskStarted:
		if it := taskChipStarted(p.TaskStarted); it != nil {
			items = append(items, it)
		}
	case *corev1.Event_TaskEnded:
		if it := taskChipEnded(p.TaskEnded); it != nil {
			items = append(items, it)
		}
	case *corev1.Event_Vendor:
		vitems, err := conversationItemsFromVendor(p.Vendor)
		if err != nil {
			return nil, err
		}
		items = vitems
	default:
		return nil, nil // not a conversation-bearing payload
	}
	if len(items) == 0 {
		return nil, nil
	}
	return &frontendv1.ConversationDelta{
		Workspace:  workspace,
		SessionId:  ev.GetSessionId(),
		Items:      items,
		ThroughSeq: ev.GetSeq(),
	}, nil
}

// conversationItemsFromVendor unwraps the vendor Any (a data.v1 message) into
// conversation items. It unwraps the ClaudeStreamMessage envelope when present.
func conversationItemsFromVendor(a *anypb.Any) ([]*structpb.Struct, error) {
	if a == nil {
		return nil, nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil, fmt.Errorf("frontend: unmarshal vendor Any (type=%q): %w", a.GetTypeUrl(), err)
	}
	switch m := msg.(type) {
	case *datav1.ClaudeStreamMessage:
		switch inner := m.GetMsg().(type) {
		case *datav1.ClaudeStreamMessage_Assistant:
			return assistantItems(inner.Assistant), nil
		case *datav1.ClaudeStreamMessage_User:
			return userItems(inner.User), nil
		default:
			return nil, nil // known envelope, non-conversational arm
		}
	case *datav1.AssistantMessage:
		return assistantItems(m), nil
	case *datav1.UserMessage:
		return userItems(m), nil
	default:
		return nil, nil // known data.v1 message, not rendered as conversation
	}
}

func assistantItems(a *datav1.AssistantMessage) []*structpb.Struct {
	if a == nil {
		return nil
	}
	uuid := a.GetUuid()
	var out []*structpb.Struct
	for i, block := range a.GetMessage().GetContent() {
		if it := blockItem(roleAssistant, uuid, i, block); it != nil {
			out = append(out, it)
		}
	}
	return out
}

func userItems(u *datav1.UserMessage) []*structpb.Struct {
	if u == nil {
		return nil
	}
	uuid := u.GetUuid()
	switch c := u.GetMessage().GetContent().(type) {
	case *datav1.ApiUserMessage_ContentString:
		if c.ContentString == "" {
			return nil
		}
		return []*structpb.Struct{textItem(roleUser, uuid, 0, c.ContentString)}
	case *datav1.ApiUserMessage_ContentBlocks:
		var out []*structpb.Struct
		for i, block := range c.ContentBlocks.GetBlocks() {
			if it := blockItem(roleUser, uuid, i, block); it != nil {
				out = append(out, it)
			}
		}
		return out
	default:
		return nil
	}
}

// blockItem renders a single content block into a conversation item Struct, or
// nil for block kinds with no conversation representation.
func blockItem(role, uuid string, idx int, block *datav1.ContentBlock) *structpb.Struct {
	switch b := block.GetBlock().(type) {
	case *datav1.ContentBlock_Text:
		return textItem(role, uuid, idx, b.Text.GetText())
	case *datav1.ContentBlock_Thinking:
		return thinkingItem(role, uuid, idx, b.Thinking)
	case *datav1.ContentBlock_ToolUse:
		return toolUseItem(role, uuid, idx, b.ToolUse)
	case *datav1.ContentBlock_ToolResult:
		return toolResultItem(role, uuid, idx, b.ToolResult)
	case *datav1.ContentBlock_Image:
		return imageItem(role, uuid, idx, b.Image)
	default:
		return nil
	}
}

func baseItem(itemType, role, uuid string, idx int) map[string]any {
	return map[string]any{
		"type":        itemType,
		"role":        role,
		"uuid":        uuid,
		"block_index": float64(idx),
	}
}

func textItem(role, uuid string, idx int, text string) *structpb.Struct {
	m := baseItem(itemTypeText, role, uuid, idx)
	m["text"] = text
	return mustStruct(m)
}

func thinkingItem(role, uuid string, idx int, t *datav1.ThinkingBlock) *structpb.Struct {
	m := baseItem(itemTypeThinking, role, uuid, idx)
	m["thinking"] = t.GetThinking()
	if sig := t.GetSignature(); sig != "" {
		m["signature"] = sig
	}
	return mustStruct(m)
}

func toolUseItem(role, uuid string, idx int, tu *datav1.ToolUseBlock) *structpb.Struct {
	m := baseItem(itemTypeToolUse, role, uuid, idx)
	m["id"] = tu.GetId()
	m["name"] = tu.GetName()
	if in := tu.GetInput(); in != nil {
		m["input"] = in.AsMap()
	} else {
		m["input"] = map[string]any{}
	}
	return mustStruct(m)
}

func toolResultItem(role, uuid string, idx int, tr *datav1.ToolResultBlock) *structpb.Struct {
	m := baseItem(itemTypeToolResult, role, uuid, idx)
	m["tool_use_id"] = tr.GetToolUseId()
	if tr.GetIsErrorSet() {
		m["is_error"] = tr.GetIsError()
	}
	switch c := tr.GetContent().(type) {
	case *datav1.ToolResultBlock_ContentString:
		m["content"] = c.ContentString
	case *datav1.ToolResultBlock_ContentBlocks:
		var blocks []any
		for j, bl := range c.ContentBlocks.GetBlocks() {
			if it := blockItem(role, uuid, j, bl); it != nil {
				blocks = append(blocks, it.AsMap())
			}
		}
		m["content_blocks"] = blocks
	}
	return mustStruct(m)
}

func imageItem(role, uuid string, idx int, img *datav1.ImageBlock) *structpb.Struct {
	m := baseItem(itemTypeImage, role, uuid, idx)
	if src := img.GetSource(); src != nil {
		m["media_type"] = src.GetMediaType()
	}
	return mustStruct(m)
}

func taskChipStarted(ts *corev1.TaskStarted) *structpb.Struct {
	if ts == nil {
		return nil
	}
	m := map[string]any{
		"type":    itemTypeTask,
		"task_id": ts.GetTaskId(),
		"kind":    taskKindString(ts.GetKind()),
		"status":  "running",
	}
	if d := ts.GetDescription(); d != "" {
		m["description"] = d
	}
	if p := ts.GetOutputPath(); p != "" {
		m["output_path"] = p
	}
	return mustStruct(m)
}

func taskChipEnded(te *corev1.TaskEnded) *structpb.Struct {
	if te == nil {
		return nil
	}
	m := map[string]any{
		"type":    itemTypeTask,
		"task_id": te.GetTaskId(),
		"kind":    taskKindString(te.GetKind()),
		"status":  terminalStatusString(te.GetStatus()),
	}
	if p := te.GetOutputPath(); p != "" {
		m["output_path"] = p
	}
	if s := te.GetSummary(); s != "" {
		m["summary"] = s
	}
	if inf := te.GetInference(); inf != "" {
		m["inference"] = inf
	}
	return mustStruct(m)
}

// ---------------------------------------------------------------------------
// TaskCatalog — folded from task-lifecycle events
// ---------------------------------------------------------------------------

// BuildTaskCatalog folds an ordered slice of core.Events (TaskStarted /
// TaskProgress / TaskEnded) into a TaskCatalog, preserving task start order.
// TaskProgress does not change status; TaskEnded stamps the terminal status and
// end time. Non-task events are ignored.
func BuildTaskCatalog(workspace, sessionID string, events []*corev1.Event) *frontendv1.TaskCatalog {
	index := map[string]*frontendv1.TaskEntry{}
	var order []string
	get := func(id string) *frontendv1.TaskEntry {
		if e, ok := index[id]; ok {
			return e
		}
		e := &frontendv1.TaskEntry{TaskId: id}
		index[id] = e
		order = append(order, id)
		return e
	}
	for _, ev := range events {
		switch p := ev.GetPayload().(type) {
		case *corev1.Event_TaskStarted:
			ts := p.TaskStarted
			e := get(ts.GetTaskId())
			e.Kind = taskKindString(ts.GetKind())
			e.Description = ts.GetDescription()
			e.OutputPath = ts.GetOutputPath()
			e.Status = "running"
			e.StartedAtMs = ev.GetProducedAtMs()
		case *corev1.Event_TaskEnded:
			te := p.TaskEnded
			e := get(te.GetTaskId())
			if e.Kind == "" {
				e.Kind = taskKindString(te.GetKind())
			}
			e.Status = terminalStatusString(te.GetStatus())
			e.EndedAtMs = ev.GetProducedAtMs()
			if op := te.GetOutputPath(); op != "" {
				e.OutputPath = op
			}
		}
	}
	catalog := &frontendv1.TaskCatalog{Workspace: workspace, SessionId: sessionID}
	for _, id := range order {
		catalog.Tasks = append(catalog.Tasks, index[id])
	}
	return catalog
}

// ---------------------------------------------------------------------------
// SessionView — folded from session metadata events
// ---------------------------------------------------------------------------

// BuildSessionView folds session-metadata events into a SessionView:
// SessionStarted supplies the model, and the latest vendor ResultMessage
// supplies token totals and cost. Title/slug/context-window/permission-mode are
// sourced from the SSM and daemon-local metadata at the stitch phase (not
// carried by these core events) and are left to the caller to populate.
func BuildSessionView(workspace, sessionID string, events []*corev1.Event) *frontendv1.SessionView {
	view := &frontendv1.SessionView{Workspace: workspace, SessionId: sessionID}
	for _, ev := range events {
		switch p := ev.GetPayload().(type) {
		case *corev1.Event_SessionStarted:
			if m := p.SessionStarted.GetModel(); m != "" {
				view.Model = m
			}
		case *corev1.Event_Vendor:
			applyResultUsage(view, p.Vendor)
		}
	}
	return view
}

func applyResultUsage(view *frontendv1.SessionView, a *anypb.Any) {
	if a == nil {
		return
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return // corrupt vendor payloads are surfaced by ConversationDeltaFromEvent, not here
	}
	var result *datav1.ResultMessage
	switch m := msg.(type) {
	case *datav1.ClaudeStreamMessage:
		if r, ok := m.GetMsg().(*datav1.ClaudeStreamMessage_Result); ok {
			result = r.Result
		}
	case *datav1.ResultMessage:
		result = m
	}
	if result == nil {
		return
	}
	view.TotalCostUsd = result.GetTotalCostUsd()
	if u := result.GetUsage(); u != nil {
		view.TotalTokens = u.GetInputTokens() + u.GetOutputTokens() +
			u.GetCacheReadInputTokens() + u.GetCacheCreationInputTokens()
	}
}

// ---------------------------------------------------------------------------
// Enum-to-string mappings (the frontend vocabulary uses lowercase strings)
// ---------------------------------------------------------------------------

func taskKindString(k corev1.TaskKind) string {
	switch k {
	case corev1.TaskKind_TASK_KIND_AGENT:
		return "agent"
	case corev1.TaskKind_TASK_KIND_SHELL:
		return "shell"
	case corev1.TaskKind_TASK_KIND_WORKFLOW:
		return "workflow"
	default:
		return "unspecified"
	}
}

func terminalStatusString(s corev1.TerminalStatus) string {
	switch s {
	case corev1.TerminalStatus_TERMINAL_STATUS_DONE:
		return "done"
	case corev1.TerminalStatus_TERMINAL_STATUS_ERROR:
		return "error"
	case corev1.TerminalStatus_TERMINAL_STATUS_KILLED:
		return "killed"
	case corev1.TerminalStatus_TERMINAL_STATUS_STOPPED:
		return "stopped"
	case corev1.TerminalStatus_TERMINAL_STATUS_LOST:
		return "lost"
	default:
		return "unspecified"
	}
}

// mustStruct builds a structpb.Struct from a JSON-compatible map. The inputs are
// all constructed in this file from typed protos, so a failure is a programmer
// error, not runtime data — hence the panic (loud, never a silent zero value).
func mustStruct(m map[string]any) *structpb.Struct {
	s, err := structpb.NewStruct(m)
	if err != nil {
		panic(fmt.Sprintf("frontend: build conversation item struct: %v", err))
	}
	return s
}
