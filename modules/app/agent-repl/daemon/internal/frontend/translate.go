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
// ONE rendered item the webapp's state-adapter (webapp/src/state-adapter.ts,
// `buildConversationItem`/`buildToolItem`) materializes into a store
// ConversationItem. That receiving contract is ALREADY LOCKED by merged webapp
// tests; this file is the emitting half that matches it. Every item carries a
// "kind" discriminator (NOT the old "type"/"role" vocabulary), and every field
// name is camelCase — protojson serializes the surrounding frame with default
// lowerCamelCase names, and the Struct interiors are authored camelCase here.
//
// The closed set of kinds and their fields (required unless noted optional):
//
//	user-turn        a user prompt bubble
//	  kind, requestId, content(array of ContentBlock-shaped structs), ts,
//	  origin(optional)
//	text             a complete assistant/user text block
//	  kind, blockId, messageId, text, done(true — store-round-tripped), ts,
//	  parentToolUseId(optional), error(optional)
//	thinking         a complete extended-thinking block
//	  kind, blockId, messageId, text, done(true), signature(optional),
//	  parentToolUseId(optional)
//	tool             a tool call card (its tool_use, and — on the paired
//	                 tool_result — its result)
//	  kind, toolUseId, toolName, messageId, ts, inputJson, inputDone,
//	  input(optional object), result(optional {isError, content}),
//	  resultTs(optional), parentToolUseId(optional), …
//	result           an end-of-turn marker
//	  kind, subtype, durationMs, sincePrevFinalMs, numTurns, totalCostUsd,
//	  usage(object, snake_case interior), isError, resultText(optional),
//	  context(null ok — omitted here)
//	compact-boundary a context-compaction boundary
//	  kind, trigger("auto"|"manual"), preTokens, postTokens
//	error            a surfaced API error
//	  kind, code, message, recoverable
//	retry            an API retry-in-progress notice
//	  kind, attempt, reason, fatal
//	system           a system note (kind, subtype) — not emitted by this file
//	permission       a permission prompt — sourced from the control plane, not
//	                 this file
//
// Reconciliation contract: text/thinking items carry blockId = `${uuid}:${blockIndex}`
// and messageId = uuid, matching the TypingDelta preview key (state-adapter's
// typingEffect derives the same `${uuid}:${blockIndex}`). tool items key on
// toolUseId. A TypingDelta preview is REPLACED when the ConversationDelta item
// for that block arrives.
//
// Detached-task lifecycle is NOT a conversation item: it flows via TaskCatalog
// (BuildTaskCatalog), which the webapp roster renders. The webapp has no `task`
// conversation kind, so TaskStarted/TaskEnded route nothing here.
//
// Fields whose source value is absent are omitted from the Struct (protojson
// then omits them too) rather than emitted as zero values.
package frontend

import (
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// Conversation item kind discriminators (the webapp's ConversationItem union).
const (
	itemKindUserTurn        = "user-turn"
	itemKindText            = "text"
	itemKindThinking        = "thinking"
	itemKindTool            = "tool"
	itemKindResult          = "result"
	itemKindCompactBoundary = "compact-boundary"
	itemKindError           = "error"
	itemKindRetry           = "retry"
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
// conversation content (a turn/task-lifecycle event, or a known-but-non-
// conversational vendor payload — those feed WorkspaceState/SessionView/
// TaskCatalog instead).
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
	case *corev1.Event_Vendor:
		vitems, err := conversationItemsFromVendor(p.Vendor, ev.GetProducedAtMs(), ev.GetRequestId())
		if err != nil {
			return nil, err
		}
		items = vitems
	default:
		// Not a conversation-bearing payload. Task-lifecycle events
		// (TaskStarted/TaskEnded) deliberately route nothing here — they flow
		// via TaskCatalog (the webapp has no `task` conversation kind).
		return nil, nil
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
// conversation items. It handles both observation planes: the stream plane
// (ClaudeStreamMessage and its bare inner messages) and the file plane
// (TranscriptLine). producedAtMs stamps the item `ts` on the stream plane;
// transcript lines prefer their own on-disk envelope timestamp. requestID is
// the Event's control-request correlation, carried onto user-turn items.
func conversationItemsFromVendor(a *anypb.Any, producedAtMs int64, requestID string) ([]*structpb.Struct, error) {
	if a == nil {
		return nil, nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil, fmt.Errorf("frontend: unmarshal vendor Any (type=%q): %w", a.GetTypeUrl(), err)
	}
	streamTs := tsFromMs(producedAtMs)
	switch m := msg.(type) {
	case *datav1.ClaudeStreamMessage:
		switch inner := m.GetMsg().(type) {
		case *datav1.ClaudeStreamMessage_Assistant:
			return assistantItems(inner.Assistant, streamTs), nil
		case *datav1.ClaudeStreamMessage_User:
			return userItems(inner.User, streamTs, requestID), nil
		case *datav1.ClaudeStreamMessage_Result:
			return resultItems(inner.Result), nil
		case *datav1.ClaudeStreamMessage_CompactBoundary:
			return compactBoundaryStreamItems(inner.CompactBoundary), nil
		default:
			return nil, nil // known envelope, non-conversational arm
		}
	case *datav1.AssistantMessage:
		return assistantItems(m, streamTs), nil
	case *datav1.UserMessage:
		return userItems(m, streamTs, requestID), nil
	case *datav1.ResultMessage:
		return resultItems(m), nil
	case *datav1.CompactBoundary:
		return compactBoundaryStreamItems(m), nil
	case *datav1.TranscriptLine:
		return transcriptLineItems(m, streamTs, requestID), nil
	default:
		return nil, nil // known data.v1 message, not rendered as conversation
	}
}

// --- transcript (file) plane -----------------------------------------------

// transcriptLineItems renders the conversation-bearing on-disk line types.
func transcriptLineItems(tl *datav1.TranscriptLine, streamTs, requestID string) []*structpb.Struct {
	switch line := tl.GetLine().(type) {
	case *datav1.TranscriptLine_Assistant:
		al := line.Assistant
		env := al.GetEnvelope()
		return assistantContentItems(env.GetUuid(), transcriptTs(env, streamTs), al.GetMessage().GetContent())
	case *datav1.TranscriptLine_User:
		ul := line.User
		env := ul.GetEnvelope()
		return userContentItems(env.GetUuid(), transcriptTs(env, streamTs), requestID, ul.GetMessage())
	case *datav1.TranscriptLine_System:
		return systemLineItems(line.System)
	default:
		return nil // non-conversational metadata line
	}
}

// systemLineItems renders the system-line subtypes the webapp has a
// conversation kind for: compaction boundaries, and API errors/retries.
func systemLineItems(sl *datav1.SystemLine) []*structpb.Struct {
	switch sub := sl.GetSubtype().(type) {
	case *datav1.SystemLine_CompactBoundary:
		if it := compactBoundaryLineItem(sub.CompactBoundary); it != nil {
			return []*structpb.Struct{it}
		}
	case *datav1.SystemLine_ApiError:
		if it := apiErrorItem(sub.ApiError); it != nil {
			return []*structpb.Struct{it}
		}
	}
	return nil
}

// --- assistant / user content ----------------------------------------------

func assistantItems(a *datav1.AssistantMessage, ts string) []*structpb.Struct {
	if a == nil {
		return nil
	}
	return assistantContentItems(a.GetUuid(), ts, a.GetMessage().GetContent())
}

// assistantContentItems renders an assistant message's content blocks. An
// assistant message carries text, thinking, and tool_use blocks; other block
// kinds do not occur on the assistant side and yield no item.
func assistantContentItems(uuid, ts string, content []*datav1.ContentBlock) []*structpb.Struct {
	var out []*structpb.Struct
	for i, block := range content {
		switch b := block.GetBlock().(type) {
		case *datav1.ContentBlock_Text:
			out = append(out, textItem(uuid, i, b.Text.GetText(), ts))
		case *datav1.ContentBlock_Thinking:
			out = append(out, thinkingItem(uuid, i, b.Thinking))
		case *datav1.ContentBlock_ToolUse:
			out = append(out, toolUseItem(uuid, b.ToolUse, ts))
		}
	}
	return out
}

func userItems(u *datav1.UserMessage, ts, requestID string) []*structpb.Struct {
	if u == nil {
		return nil
	}
	return userContentItems(u.GetUuid(), ts, requestID, u.GetMessage())
}

// userContentItems splits a user message into its conversation items: a
// tool_result block becomes a `tool` item (paired to its tool_use by
// toolUseId), and the remaining content (a prompt string, text, or other
// blocks) becomes a single `user-turn` bubble. A pure tool-result feedback
// message thus yields only tool items and no user-turn.
func userContentItems(uuid, ts, requestID string, msg *datav1.ApiUserMessage) []*structpb.Struct {
	switch c := msg.GetContent().(type) {
	case *datav1.ApiUserMessage_ContentString:
		if c.ContentString == "" {
			return nil
		}
		content := []any{map[string]any{"type": "text", "text": c.ContentString}}
		return []*structpb.Struct{userTurnItem(requestID, content, ts)}
	case *datav1.ApiUserMessage_ContentBlocks:
		var tools []*structpb.Struct
		var content []any
		for _, block := range c.ContentBlocks.GetBlocks() {
			if tr, ok := block.GetBlock().(*datav1.ContentBlock_ToolResult); ok {
				tools = append(tools, toolResultItem(uuid, tr.ToolResult, ts))
				continue
			}
			if cb := userContentBlock(block); cb != nil {
				content = append(content, cb)
			}
		}
		var out []*structpb.Struct
		if len(content) > 0 {
			out = append(out, userTurnItem(requestID, content, ts))
		}
		out = append(out, tools...)
		return out
	default:
		return nil
	}
}

// userContentBlock shapes one non-tool_result user block into a ContentBlock
// struct for a user-turn's `content` array. Text blocks map to the API
// {type:"text", text} shape; any other block is preserved by its type name so
// it is never silently dropped from the prompt bubble.
func userContentBlock(block *datav1.ContentBlock) map[string]any {
	switch b := block.GetBlock().(type) {
	case *datav1.ContentBlock_Text:
		return map[string]any{"type": "text", "text": b.Text.GetText()}
	case *datav1.ContentBlock_Image:
		m := map[string]any{"type": "image"}
		if src := b.Image.GetSource(); src != nil {
			m["media_type"] = src.GetMediaType()
		}
		return m
	case *datav1.ContentBlock_ToolUse:
		return map[string]any{"type": "tool_use", "id": b.ToolUse.GetId(), "name": b.ToolUse.GetName()}
	default:
		return nil
	}
}

// --- item builders ----------------------------------------------------------

func userTurnItem(requestID string, content []any, ts string) *structpb.Struct {
	return mustStruct(map[string]any{
		"kind":      itemKindUserTurn,
		"requestId": requestID,
		"content":   content,
		"ts":        ts,
	})
}

func textItem(uuid string, idx int, text, ts string) *structpb.Struct {
	return mustStruct(map[string]any{
		"kind":      itemKindText,
		"blockId":   blockID(uuid, idx),
		"messageId": uuid,
		"text":      text,
		// A store-round-tripped block is complete: it is delivered whole, never
		// mid-stream, so it always renders done (its TypingDelta preview, keyed
		// by the same blockId, is replaced by this item).
		"done": true,
		"ts":   ts,
	})
}

func thinkingItem(uuid string, idx int, t *datav1.ThinkingBlock) *structpb.Struct {
	m := map[string]any{
		"kind":      itemKindThinking,
		"blockId":   blockID(uuid, idx),
		"messageId": uuid,
		"text":      t.GetThinking(),
		"done":      true,
	}
	if sig := t.GetSignature(); sig != "" {
		m["signature"] = sig
	}
	return mustStruct(m)
}

func toolUseItem(uuid string, tu *datav1.ToolUseBlock, ts string) *structpb.Struct {
	m := map[string]any{
		"kind":      itemKindTool,
		"toolUseId": tu.GetId(),
		"toolName":  tu.GetName(),
		"messageId": uuid,
		"ts":        ts,
		// The input arrived whole with the round-tripped message.
		"inputDone": true,
	}
	if in := tu.GetInput(); in != nil {
		m["input"] = in.AsMap()
	} else {
		m["input"] = map[string]any{}
	}
	return mustStruct(m)
}

// toolResultItem renders a tool_result block as a `tool` item carrying the
// call's result, keyed to its tool_use by toolUseId (the webapp store's join
// key). toolName is empty: a bare tool_result does not carry the tool's name,
// and the store already holds it on the tool_use item this reconciles onto.
func toolResultItem(uuid string, tr *datav1.ToolResultBlock, ts string) *structpb.Struct {
	result := map[string]any{"isError": tr.GetIsError()}
	switch c := tr.GetContent().(type) {
	case *datav1.ToolResultBlock_ContentString:
		result["content"] = c.ContentString
	case *datav1.ToolResultBlock_ContentBlocks:
		var blocks []any
		for _, bl := range c.ContentBlocks.GetBlocks() {
			blocks = append(blocks, toolResultContentBlock(bl))
		}
		result["content"] = blocks
	default:
		// No content arm set: an empty result body, not an error to swallow.
		result["content"] = ""
	}
	return mustStruct(map[string]any{
		"kind":      itemKindTool,
		"toolUseId": tr.GetToolUseId(),
		"toolName":  "",
		"messageId": uuid,
		"ts":        ts,
		"inputDone": true,
		"resultTs":  ts,
		"result":    result,
	})
}

// toolResultContentBlock shapes one nested tool-result block into the webapp's
// {type:"text", text} result-content entry. A non-text nested block is labeled
// by its kind rather than dropped (the webapp result.content type is text-only,
// so a faithful non-text rendering has no home; the label keeps it visible).
func toolResultContentBlock(block *datav1.ContentBlock) map[string]any {
	switch b := block.GetBlock().(type) {
	case *datav1.ContentBlock_Text:
		return map[string]any{"type": "text", "text": b.Text.GetText()}
	case *datav1.ContentBlock_Image:
		return map[string]any{"type": "text", "text": "[image block]"}
	default:
		return map[string]any{"type": "text", "text": "[non-text block]"}
	}
}

func resultItems(r *datav1.ResultMessage) []*structpb.Struct {
	if r == nil {
		return nil
	}
	m := map[string]any{
		"kind":       itemKindResult,
		"subtype":    resultSubtypeString(r.GetSubtype()),
		"durationMs": float64(r.GetDurationMs()),
		// sincePrevFinalMs measures from the session's PREVIOUS turn boundary,
		// which is cross-event session state this stateless per-event translator
		// does not hold. Emit 0; the store re-derives the real figure from its
		// own stamps (see store.ts `result` reducer) on adoption.
		"sincePrevFinalMs": float64(0),
		"numTurns":         float64(r.GetNumTurns()),
		"totalCostUsd":     r.GetTotalCostUsd(),
		"usage":            usageStruct(r.GetUsage()),
		"isError":          r.GetIsError(),
		// context is likewise session state (last-reported size + delta): omit
		// it so the adapter reads null, exactly as it does for a turn whose size
		// is unknown.
	}
	if txt := r.GetResult(); txt != "" {
		m["resultText"] = txt
	}
	return []*structpb.Struct{mustStruct(m)}
}

// usageStruct shapes a data.v1 Usage into the webapp's snake_case Usage object
// (the API-shaped token block the store reads verbatim). The cache fields are
// omitted when zero, matching the absent-fields-omitted convention.
func usageStruct(u *datav1.Usage) map[string]any {
	m := map[string]any{
		"input_tokens":  float64(u.GetInputTokens()),
		"output_tokens": float64(u.GetOutputTokens()),
	}
	if v := u.GetCacheReadInputTokens(); v != 0 {
		m["cache_read_input_tokens"] = float64(v)
	}
	if v := u.GetCacheCreationInputTokens(); v != 0 {
		m["cache_creation_input_tokens"] = float64(v)
	}
	return m
}

// compactBoundaryStreamItems renders the STREAM CompactBoundary. The stream
// boundary carries the pre-compaction size but NOT the post-compaction one
// (only the on-disk CompactBoundaryLine's metadata records post_tokens), so
// postTokens is 0 here — honestly unknown, never a fabricated estimate.
func compactBoundaryStreamItems(cb *datav1.CompactBoundary) []*structpb.Struct {
	if cb == nil {
		return nil
	}
	return []*structpb.Struct{mustStruct(map[string]any{
		"kind":       itemKindCompactBoundary,
		"trigger":    compactTriggerString(cb.GetTrigger()),
		"preTokens":  float64(cb.GetPreTokens()),
		"postTokens": float64(0),
	})}
}

// compactBoundaryLineItem renders the on-disk CompactBoundaryLine, whose
// metadata records both the pre- and post-compaction sizes.
func compactBoundaryLineItem(cbl *datav1.CompactBoundaryLine) *structpb.Struct {
	md := cbl.GetCompactMetadata()
	return mustStruct(map[string]any{
		"kind":       itemKindCompactBoundary,
		"trigger":    normalizeTrigger(md.GetTrigger()),
		"preTokens":  float64(md.GetPreTokens()),
		"postTokens": float64(md.GetPostTokens()),
	})
}

// apiErrorItem renders an on-disk ApiErrorLine as either a `retry` item (an
// in-progress backoff: the SDK is retrying) or an `error` item (a terminal API
// failure with no further retry). The split keys on the line's retry state.
func apiErrorItem(ae *datav1.ApiErrorLine) *structpb.Struct {
	detail := ae.GetError()
	reason := detail.GetMessage()
	if reason == "" {
		reason = detail.GetFormatted()
	}
	attempt := ae.GetRetryAttempt()
	if attempt > 0 || ae.GetRetryInMs() > 0 {
		fatal := detail.GetIsNetworkDown()
		if max := ae.GetMaxRetries(); max > 0 && attempt >= max {
			fatal = true
		}
		return mustStruct(map[string]any{
			"kind":    itemKindRetry,
			"attempt": float64(attempt),
			"reason":  reason,
			"fatal":   fatal,
		})
	}
	return mustStruct(map[string]any{
		"kind":        itemKindError,
		"code":        "api_error",
		"message":     reason,
		"recoverable": false,
	})
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
// Enum/value-to-string mappings (the frontend vocabulary uses lowercase strings)
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

// resultSubtypeString maps the data.v1 ResultSubtype to the webapp's subtype
// string. The two budget/structured-output subtypes exceed the webapp's
// declared ResultSubtype union (which the adapter adopts without validating);
// emitting their faithful string is more honest than collapsing them onto a
// wrong neighbor.
func resultSubtypeString(s datav1.ResultSubtype) string {
	switch s {
	case datav1.ResultSubtype_RESULT_SUBTYPE_SUCCESS:
		return "success"
	case datav1.ResultSubtype_RESULT_SUBTYPE_ERROR_DURING_EXECUTION:
		return "error_during_execution"
	case datav1.ResultSubtype_RESULT_SUBTYPE_ERROR_MAX_TURNS:
		return "error_max_turns"
	case datav1.ResultSubtype_RESULT_SUBTYPE_ERROR_MAX_BUDGET_USD:
		return "error_max_budget_usd"
	case datav1.ResultSubtype_RESULT_SUBTYPE_ERROR_MAX_STRUCTURED_OUTPUT_RETRIES:
		return "error_max_structured_output_retries"
	default:
		return ""
	}
}

func compactTriggerString(t datav1.CompactTrigger) string {
	switch t {
	case datav1.CompactTrigger_COMPACT_TRIGGER_AUTO:
		return "auto"
	case datav1.CompactTrigger_COMPACT_TRIGGER_MANUAL:
		return "manual"
	default:
		return ""
	}
}

// normalizeTrigger passes through the on-disk trigger string ("auto"|"manual"),
// mapping anything else to empty rather than guessing.
func normalizeTrigger(t string) string {
	switch t {
	case "auto", "manual":
		return t
	default:
		return ""
	}
}

// ---------------------------------------------------------------------------
// small helpers
// ---------------------------------------------------------------------------

// blockID is the stable block key `${uuid}:${blockIndex}` that both this item
// and its TypingDelta preview share, so the store reconciles them.
func blockID(uuid string, idx int) string {
	return fmt.Sprintf("%s:%d", uuid, idx)
}

// tsFromMs formats a producer wall-clock (unix millis) as an RFC3339 stamp, or
// "" when unset (0) — an absent stamp, not a fabricated one.
func tsFromMs(ms int64) string {
	if ms == 0 {
		return ""
	}
	return time.UnixMilli(ms).UTC().Format(time.RFC3339)
}

// transcriptTs prefers a transcript line's own on-disk envelope timestamp
// (already ISO-8601), falling back to the Event's producer stamp.
func transcriptTs(env *datav1.LineEnvelope, fallback string) string {
	if ts := env.GetTimestamp(); ts != "" {
		return ts
	}
	return fallback
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
