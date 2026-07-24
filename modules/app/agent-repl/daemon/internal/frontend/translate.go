// Package frontend is the daemon's frontend surface. It serves
// agentshim.frontend.v1 frames as protojson over a UDS listener (Emacs) and a
// WebSocket endpoint (webapp), CURATES internal agentshim.core.v1 /
// agentshim.data.v1 events into the resolved frontend vocabulary, and
// dispatches inbound FrontendCommands with CommandAcks.
//
// This file (translate.go) holds the PURE, IO-free translation layer: internal
// events and SSM state in, frontend.v1 protos out. server.go owns transports
// and fan-out; commands.go owns inbound dispatch.
//
// ---------------------------------------------------------------------------
// ConversationDelta item vocabulary (the S9 recomposition)
// ---------------------------------------------------------------------------
//
// ConversationDelta.items is a repeated frontendv1.ConversationItem: a THIN
// envelope (uuid / ts_ms / request_id) around the typed agent payload, carried
// through into the matching oneof arm. translate.go is a CURATOR, not a
// re-encoder: it selects WHICH store events carry visible conversation content
// and passes the typed data.v1 payload through UNCHANGED — it never re-types a
// payload into a webapp-specific struct vocabulary. Frontends render the typed
// payload; they never re-interpret facts the daemon already resolved.
//
// Payload → ConversationItem arm (the closed curated set):
//
//	AssistantMessage / AssistantLine   assistant_message (ApiAssistantMessage)
//	UserMessage / UserLine             user_message      (ApiUserMessage)
//	ResultMessage                      result            (ResultMessage)
//	CompactBoundary (stream)           compact_boundary  (CompactBoundary)
//	SystemLine.CompactBoundary (disk)  compact_boundary_line (CompactBoundaryLine)
//	SystemLine.ApiError (disk)         api_error         (ApiErrorLine)
//
// The tool_use / tool_result blocks ride INSIDE their carrying message: a
// tool call's tool_use block travels in the assistant_message item and its
// tool_result block travels in the user_message item — two items the webapp
// merges by tool_use_id (the old two-item behavior, preserved). A payload with
// no visual value (an empty message, a known-but-non-conversational stream arm,
// a metadata transcript line) is simply not pushed rather than emitted empty.
//
// Reconciliation contract: a message item's uuid is the claude message uuid the
// TypingDelta previews key on (state-adapter derives `${uuid}:${blockIndex}`);
// a permission item's uuid is the permission request_id (sourced from the
// control plane in sinks.go / driver.go, not this file). A TypingDelta preview
// is REPLACED when the ConversationDelta item for that message arrives.
//
// Detached-task lifecycle is NOT a conversation item: it flows via TaskCatalog
// (BuildTaskCatalog), which the webapp roster renders. The webapp has no `task`
// conversation kind, so TaskStarted/TaskEnded route nothing here.
package frontend

import (
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
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

// SessionInitViewFrame wraps a SessionInitView (S9): the session's retained
// SystemInit (slash commands, tools, skills, model list).
func SessionInitViewFrame(v *frontendv1.SessionInitView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_SessionInit{SessionInit: v}}
}

// HeartbeatViewFrame wraps a HeartbeatView (E4): the ephemeral long-tool
// liveness relay.
func HeartbeatViewFrame(h *frontendv1.HeartbeatView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Heartbeat{Heartbeat: h}}
}

// ---------------------------------------------------------------------------
// ContentDelta -> TypingDelta (ephemeral live typing)
// ---------------------------------------------------------------------------

// TypingDeltaFromContentDelta relays a core.ContentDelta as the ephemeral
// TypingDelta preview, embedding the delta UNCHANGED (S9): the frontend keys on
// delta.uuid/delta.block_index and reconciles against the ConversationDelta
// round-trip. It is a faithful passthrough — the curation of what streams lives
// upstream (the shim only emits deltas worth previewing), so this layer never
// re-types the delta into per-arm strings.
func TypingDeltaFromContentDelta(workspace, sessionID string, cd *corev1.ContentDelta) *frontendv1.TypingDelta {
	if cd == nil {
		return nil
	}
	return &frontendv1.TypingDelta{
		Workspace: workspace,
		SessionId: sessionID,
		Delta:     cd,
	}
}

// ---------------------------------------------------------------------------
// HeartbeatProgress -> HeartbeatView (ephemeral long-tool liveness)
// ---------------------------------------------------------------------------

// HeartbeatViewFromProgress relays a core.HeartbeatProgress as the ephemeral
// HeartbeatView (E4), embedding the progress UNCHANGED for the same reason
// TypingDeltaFromContentDelta embeds its delta unchanged: this layer relays,
// it never re-types. The frontend keys on progress.tool_use_id to find the
// running tool and ticks its elapsed display from progress.elapsed_seconds.
//
// Returns nil for a nil progress so the caller pushes nothing rather than an
// empty frame.
func HeartbeatViewFromProgress(workspace, sessionID string, hp *corev1.HeartbeatProgress) *frontendv1.HeartbeatView {
	if hp == nil {
		return nil
	}
	return &frontendv1.HeartbeatView{
		Workspace: workspace,
		SessionId: sessionID,
		Progress:  hp,
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
// Events -> ConversationDelta items (the curator)
// ---------------------------------------------------------------------------

// ConversationDeltaFromEvent curates one core.Event into a ConversationDelta
// carrying the typed ConversationItems for that event, or nil when the event
// yields no visible conversation content (a turn/task-lifecycle event, or a
// known-but-non-conversational vendor payload — those feed WorkspaceState/
// SessionView/TaskCatalog instead).
//
// It hard-errors (no silent fallback) when a vendor payload cannot be
// unmarshaled or carries a type URL unknown to the compiled schema set — those
// are genuine anomalies, distinct from a known-but-non-conversational payload.
func ConversationDeltaFromEvent(workspace string, ev *corev1.Event) (*frontendv1.ConversationDelta, error) {
	if ev == nil {
		return nil, nil
	}
	var items []*frontendv1.ConversationItem
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
// curated conversation items. It handles both observation planes: the stream
// plane (ClaudeStreamMessage and its bare inner messages) and the file plane
// (TranscriptLine). producedAtMs stamps ts_ms on the stream plane; transcript
// lines prefer their own on-disk envelope timestamp. requestID is the Event's
// control-request correlation, carried onto every item's envelope.
func conversationItemsFromVendor(a *anypb.Any, producedAtMs int64, requestID string) ([]*frontendv1.ConversationItem, error) {
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
			return assistantItems(inner.Assistant, producedAtMs, requestID), nil
		case *datav1.ClaudeStreamMessage_User:
			return userItems(inner.User, producedAtMs, requestID), nil
		case *datav1.ClaudeStreamMessage_Result:
			return resultItems(inner.Result, producedAtMs, requestID), nil
		case *datav1.ClaudeStreamMessage_CompactBoundary:
			return compactBoundaryItems(inner.CompactBoundary, producedAtMs, requestID), nil
		default:
			return nil, nil // known envelope, non-conversational arm
		}
	case *datav1.AssistantMessage:
		return assistantItems(m, producedAtMs, requestID), nil
	case *datav1.UserMessage:
		return userItems(m, producedAtMs, requestID), nil
	case *datav1.ResultMessage:
		return resultItems(m, producedAtMs, requestID), nil
	case *datav1.CompactBoundary:
		return compactBoundaryItems(m, producedAtMs, requestID), nil
	case *datav1.TranscriptLine:
		return transcriptLineItems(m, producedAtMs, requestID), nil
	default:
		return nil, nil // known data.v1 message, not rendered as conversation
	}
}

// --- transcript (file) plane -----------------------------------------------

// transcriptLineItems curates the conversation-bearing on-disk line types.
func transcriptLineItems(tl *datav1.TranscriptLine, producedAtMs int64, requestID string) []*frontendv1.ConversationItem {
	switch line := tl.GetLine().(type) {
	case *datav1.TranscriptLine_Assistant:
		al := line.Assistant
		env := al.GetEnvelope()
		return assistantMessageItem(env.GetUuid(), transcriptTsMs(env, producedAtMs), requestID, al.GetMessage())
	case *datav1.TranscriptLine_User:
		ul := line.User
		env := ul.GetEnvelope()
		return userMessageItem(env.GetUuid(), transcriptTsMs(env, producedAtMs), requestID, ul.GetMessage())
	case *datav1.TranscriptLine_System:
		return systemLineItems(line.System, producedAtMs, requestID)
	default:
		return nil // non-conversational metadata line
	}
}

// systemLineItems curates the system-line subtypes the webapp renders:
// compaction boundaries and API errors/retries. Their typed payloads pass
// through — the webapp splits a mid-backoff api_error into a retry chip from
// the ApiErrorLine's own retry fields, so no daemon re-typing is needed.
func systemLineItems(sl *datav1.SystemLine, tsMs int64, requestID string) []*frontendv1.ConversationItem {
	uuid := sl.GetEnvelope().GetUuid()
	switch sub := sl.GetSubtype().(type) {
	case *datav1.SystemLine_CompactBoundary:
		if sub.CompactBoundary == nil {
			return nil
		}
		return []*frontendv1.ConversationItem{{
			Uuid: uuid, TsMs: tsMs, RequestId: requestID,
			Item: &frontendv1.ConversationItem_CompactBoundaryLine{CompactBoundaryLine: sub.CompactBoundary},
		}}
	case *datav1.SystemLine_ApiError:
		if sub.ApiError == nil {
			return nil
		}
		return []*frontendv1.ConversationItem{{
			Uuid: uuid, TsMs: tsMs, RequestId: requestID,
			Item: &frontendv1.ConversationItem_ApiError{ApiError: sub.ApiError},
		}}
	default:
		return nil
	}
}

// --- assistant / user / result / compaction items --------------------------

func assistantItems(a *datav1.AssistantMessage, tsMs int64, requestID string) []*frontendv1.ConversationItem {
	if a == nil {
		return nil
	}
	return assistantMessageItem(a.GetUuid(), tsMs, requestID, a.GetMessage())
}

// assistantMessageItem passes an ApiAssistantMessage (text / thinking / tool_use
// blocks) through into the assistant_message arm. An assistant message with no
// content blocks has no visual value and is dropped rather than pushed empty.
func assistantMessageItem(uuid string, tsMs int64, requestID string, msg *datav1.ApiAssistantMessage) []*frontendv1.ConversationItem {
	if msg == nil || len(msg.GetContent()) == 0 {
		return nil
	}
	return []*frontendv1.ConversationItem{{
		Uuid: uuid, TsMs: tsMs, RequestId: requestID,
		Item: &frontendv1.ConversationItem_AssistantMessage{AssistantMessage: msg},
	}}
}

func userItems(u *datav1.UserMessage, tsMs int64, requestID string) []*frontendv1.ConversationItem {
	if u == nil {
		return nil
	}
	return userMessageItem(u.GetUuid(), tsMs, requestID, u.GetMessage())
}

// userMessageItem passes an ApiUserMessage (a prompt string, text, or
// tool_result blocks) through into the user_message arm. A user message with no
// content (empty string, no blocks) has no visual value and is dropped; a pure
// tool_result feedback message still carries its blocks, so it is pushed and the
// webapp renders only the tool result (no user bubble).
func userMessageItem(uuid string, tsMs int64, requestID string, msg *datav1.ApiUserMessage) []*frontendv1.ConversationItem {
	if !hasUserContent(msg) {
		return nil
	}
	return []*frontendv1.ConversationItem{{
		Uuid: uuid, TsMs: tsMs, RequestId: requestID,
		Item: &frontendv1.ConversationItem_UserMessage{UserMessage: msg},
	}}
}

// hasUserContent reports whether a user message carries any renderable content:
// a non-empty content string or at least one content block.
func hasUserContent(msg *datav1.ApiUserMessage) bool {
	switch c := msg.GetContent().(type) {
	case *datav1.ApiUserMessage_ContentString:
		return c.ContentString != ""
	case *datav1.ApiUserMessage_ContentBlocks:
		return len(c.ContentBlocks.GetBlocks()) > 0
	default:
		return false
	}
}

// resultItems passes an end-of-turn ResultMessage through into the result arm.
func resultItems(r *datav1.ResultMessage, tsMs int64, requestID string) []*frontendv1.ConversationItem {
	if r == nil {
		return nil
	}
	return []*frontendv1.ConversationItem{{
		TsMs: tsMs, RequestId: requestID,
		Item: &frontendv1.ConversationItem_Result{Result: r},
	}}
}

// compactBoundaryItems passes a STREAM CompactBoundary through into the
// compact_boundary arm (the on-disk twin uses the compact_boundary_line arm).
func compactBoundaryItems(cb *datav1.CompactBoundary, tsMs int64, requestID string) []*frontendv1.ConversationItem {
	if cb == nil {
		return nil
	}
	return []*frontendv1.ConversationItem{{
		TsMs: tsMs, RequestId: requestID,
		Item: &frontendv1.ConversationItem_CompactBoundary{CompactBoundary: cb},
	}}
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
// Enum/value-to-string mappings (the frontend TaskCatalog vocabulary uses
// lowercase strings; conversation payloads carry their typed enums through)
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

// ---------------------------------------------------------------------------
// small helpers
// ---------------------------------------------------------------------------

// transcriptTsMs prefers a transcript line's own on-disk envelope timestamp
// (ISO-8601), parsed to unix millis, falling back to the Event's producer stamp
// when the envelope carries no timestamp or an unparseable one — the honest
// producer stamp, never a fabricated one.
func transcriptTsMs(env *datav1.LineEnvelope, fallbackMs int64) int64 {
	if ts := env.GetTimestamp(); ts != "" {
		if t, err := time.Parse(time.RFC3339, ts); err == nil {
			return t.UnixMilli()
		}
	}
	return fallbackMs
}
