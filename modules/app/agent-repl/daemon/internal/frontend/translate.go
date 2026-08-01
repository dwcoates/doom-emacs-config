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
//	SystemLine.ApiError (disk), terminal only  system_failure (SystemFailureItem)
//	Event.ContextCleared               context_cleared   (core.v1 ContextCleared)
//	Event.ContextCompacted             context_compacted (core.v1 ContextCompacted)
//
// A CLEAR AND A COMPACTION ARE NOT PASSTHROUGH. Each reaches this daemon as
// several partial records — for a compaction, a start status, a boundary
// carrying token counts, and a summary line the vendor types as an ordinary
// user message — arriving on both planes and, in the transcript, timestamped
// out of order relative to each other. They therefore curate to core.v1
// ContextCleared / ContextCompacted, which the shim-sidecar COALESCES from
// those records, rather than passing any single record through. The retired
// compact_boundary / compact_boundary_line arms were the passthrough shape,
// and they made every frontend correlate the halves for itself.
//
// Those two are the ONLY non-vendor Event payloads that curate to an item, and
// the only ones a frontend must never have to FIND for itself: the daemon
// floors every replay at the newest clear or compaction
// (sessioncontroller.Manager.Resync), so one that arrives is always live, and the
// string-matching a frontend used to do on prompt text has no successor here
// by design.
//
// A mid-backoff SystemLine.ApiError curates to NOTHING: the retrying window
// (internal/progress) is what covers it, and the legacy api_error passthrough
// this used to also emit alongside the card was retired in step 11 once both
// frontends read the card only.
//
// The tool_use / tool_result blocks ride INSIDE their carrying message: a
// tool call's tool_use block travels in the assistant_message item and its
// tool_result block travels in the user_message item — two items the webapp
// merges by tool_use_id (the old two-item behavior, preserved). A payload with
// no visual value (an empty message, a known-but-non-conversational stream arm,
// a metadata transcript line) is simply not pushed rather than emitted empty.
//
// Reconciliation contract: a message item's uuid is the RECORD ENVELOPE's uuid
// — unique per emitted record, and what consumers dedup a replayed item on. It
// is deliberately NOT the claude message id, which the item's own payload
// carries: the SDK emits one assistant record per content block, all repeating
// that message id, so keying items on it collapses a multi-block message onto
// one item. A permission item's uuid is the permission request_id (sourced from
// the control plane in sinks.go / sessioncontroller.go, not this file).
//
// A TypingDelta preview is REPLACED when the ConversationDelta item for that
// message arrives, but the two carry no common id — the preview keys the
// message id + TRUE API block index, the record keys its envelope — so the
// frontend MATCHES them rather than looking one up by the other. This daemon
// carries both ids unchanged and derives neither; see stream_contract_test.go.
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

	"claude-repld/internal/errclass"

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

// QueueViewFrame wraps a QueueView (E4): the session's held-prompt queue.
func QueueViewFrame(q *frontendv1.QueueView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Queue{Queue: q}}
}

// ProgressViewFrame wraps a ProgressView (F1): the consolidated progress
// footer's whole input, resolved by internal/progress.
func ProgressViewFrame(p *frontendv1.ProgressView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Progress{Progress: p}}
}

// WorkspaceAvailableFrame wraps the durable, host-only workspace lifecycle
// notification.  Server routes it only to ClientKindHost connections.
func WorkspaceAvailableFrame(v *frontendv1.WorkspaceAvailable) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_WorkspaceAvailable{WorkspaceAvailable: v}}
}

// HostActionFrame wraps one durable UI-only action from the daemon inbox.
// Server routes it only to ClientKindHost connections.
func HostActionFrame(v *frontendv1.HostAction) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_HostAction{HostAction: v}}
}

// DaemonHealthFrame wraps a daemon-global correlated health assertion.
func DaemonHealthFrame(v *frontendv1.DaemonHealthView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_DaemonHealth{DaemonHealth: v}}
}

// SessionHealthFrame wraps a session-specific correlated health assertion.
func SessionHealthFrame(v *frontendv1.SessionHealthView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_SessionHealth{SessionHealth: v}}
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
// DegradedState -> SystemFailureItem
// ---------------------------------------------------------------------------

// SystemFailureItemFromDegradedState classifies a shim-reported DegradedState
// as a conversation card (F4), replacing the DegradedNotice banner (RETIRED,
// step 11).
//
// The window's two edges become ONE card: the opening report leaves
// resolved_at_ms zero and the recovery stamps it, under the same uuid the
// caller keys them by, so the feed reconciles in place and shows a settled
// card instead of a permanent alarm about something that ended.
//
// dropped_count finally survives. The banner discarded it, which meant the
// single most useful fact about a store outage — how much conversation was
// lost — reached no surface at all.
func SystemFailureItemFromDegradedState(ds *corev1.DegradedState, atMs int64) *frontendv1.SystemFailureItem {
	if ds == nil {
		return nil
	}
	item := errclass.Degraded(ds.GetComponent(), ds.GetReason(), int64(ds.GetDroppedCount()))
	if ds.GetRecovered() {
		item.ResolvedAtMs = atMs
	}
	return item
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
//
// The second return is the RECORD ENVELOPES of the items that came from the
// file plane, keyed by ConversationItem.uuid — see RecordEnvelope.
//
// PROVENANCE IS STAMPED CONVERSATION_SOURCE_USER HERE, EXPLICITLY, on every
// item this file builds. An ordinary turn is what a translated store event
// describes, and it is what every item predating the merge lease describes too,
// so USER is the CURATOR'S OWN VERDICT rather than a proto3 zero left
// unpopulated — UNSPECIFIED is a malformed frame the receiver must reject.
//
// The one party that can say otherwise is the one holding the merge lease's
// ledger, and it revises the verdict through StampConversationSource on the way
// out (sessioncontroller/sinks.go). This layer has no access to the lease and
// deliberately does not acquire one: a curator that re-derived provenance from
// live state would produce a different answer on a resync than it did on the
// original push, which is the exact failure the persisted ledger prevents.
func ConversationDeltaFromEvent(workspace string, ev *corev1.Event) (*frontendv1.ConversationDelta, map[string]RecordEnvelope, error) {
	if ev == nil {
		return nil, nil, nil
	}
	var items []*frontendv1.ConversationItem
	var envs map[string]RecordEnvelope
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_Vendor:
		vitems, venvs, err := conversationItemsFromVendor(p.Vendor, ev.GetProducedAtMs(), ev.GetRequestId())
		if err != nil {
			return nil, nil, err
		}
		items, envs = vitems, venvs
	case *corev1.Event_ContextCleared:
		items = contextClearedItems(p.ContextCleared, ev)
	case *corev1.Event_ContextCompacted:
		items = contextCompactedItems(p.ContextCompacted, ev)
	default:
		// Not a conversation-bearing payload. Task-lifecycle events
		// (TaskStarted/TaskEnded) deliberately route nothing here — they flow
		// via TaskCatalog (the webapp has no `task` conversation kind).
		return nil, nil, nil
	}
	if len(items) == 0 {
		return nil, nil, nil
	}
	return &frontendv1.ConversationDelta{
		Workspace:  workspace,
		SessionId:  ev.GetSessionId(),
		Items:      items,
		ThroughSeq: ev.GetSeq(),
	}, envs, nil
}

// RecordEnvelope is the file-plane transcript envelope of one curated item:
// the record-level facts the DAEMON's own curators need and the frontend wire
// deliberately does not carry.
//
// WHY IT IS RETURNED BESIDE THE DELTA RATHER THAN CARRIED ON IT. Every field
// here is bookkeeping the harness addressed to itself, and a frontend that
// received it could only be tempted to re-derive a curation the daemon has
// already made (the standing rule: frontends render, never interpret). Nor can
// a curator recover these from the delta afterwards — ConversationItem models
// the MESSAGE, so the moment translation runs the envelope is gone, which is
// exactly why the slash-command curator (sessioncontroller/machinery.go) has to
// string-match record bodies instead of reading a flag.
//
// Only file-plane records have one. The stream plane's messages carry no
// transcript envelope at all, so a stream item simply has no entry.
type RecordEnvelope struct {
	// ParentUUID is the uuid of the record this one replies to, the linkage
	// the harness threads its synthetic records onto. Empty at a chain root.
	ParentUUID string
	// IsMeta marks a record the harness wrote FOR THE MODEL rather than one a
	// person typed: a launched skill's body, a re-invocation notice, a
	// "continue from where you left off" nudge. It is a "user" record in every
	// other respect, so without this flag it renders as a prompt bubble the
	// user never wrote.
	IsMeta bool
}

// conversationItemsFromVendor unwraps the vendor Any (a data.v1 message) into
// curated conversation items. It handles both observation planes: the stream
// plane (ClaudeStreamMessage and its bare inner messages) and the file plane
// (TranscriptLine). producedAtMs stamps ts_ms on the stream plane; transcript
// lines prefer their own on-disk envelope timestamp. requestID is the Event's
// control-request correlation, carried onto every item's envelope.
//
// The second return carries the file plane's RecordEnvelopes; the stream plane
// has no transcript envelope at all and returns nil.
func conversationItemsFromVendor(a *anypb.Any, producedAtMs int64, requestID string) ([]*frontendv1.ConversationItem, map[string]RecordEnvelope, error) {
	if a == nil {
		return nil, nil, nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil, nil, fmt.Errorf("frontend: unmarshal vendor Any (type=%q): %w", a.GetTypeUrl(), err)
	}
	switch m := msg.(type) {
	case *datav1.ClaudeStreamMessage:
		switch inner := m.GetMsg().(type) {
		case *datav1.ClaudeStreamMessage_Assistant:
			return assistantItems(inner.Assistant, producedAtMs, requestID), nil, nil
		case *datav1.ClaudeStreamMessage_User:
			return userItems(inner.User, producedAtMs, requestID), nil, nil
		case *datav1.ClaudeStreamMessage_Result:
			return resultItems(inner.Result, producedAtMs, requestID), nil, nil
		default:
			return nil, nil, nil // known envelope, non-conversational arm
		}
	case *datav1.AssistantMessage:
		return assistantItems(m, producedAtMs, requestID), nil, nil
	case *datav1.UserMessage:
		return userItems(m, producedAtMs, requestID), nil, nil
	case *datav1.ResultMessage:
		return resultItems(m, producedAtMs, requestID), nil, nil
	case *datav1.TranscriptLine:
		items, envs := transcriptLineItems(m, producedAtMs, requestID)
		return items, envs, nil
	default:
		return nil, nil, nil // known data.v1 message, not rendered as conversation
	}
}

// --- clear and compact -----------------------------------------------------

// contextClearedItems curates a first-class ContextCleared into its arm,
// carrying the core.v1 message VERBATIM.
//
// A clear has no fields, so everything a frontend needs is the item's envelope:
// WHICH conversation position it lands at (the uuid, for reconciliation) and
// WHEN (ts_ms, for the bubble). The message itself is still passed rather than
// re-modeled — a frontend-shaped copy of an empty message would be a second
// shape to keep in step with the first for no gain.
func contextClearedItems(cc *corev1.ContextCleared, ev *corev1.Event) []*frontendv1.ConversationItem {
	if cc == nil {
		return nil
	}
	return []*frontendv1.ConversationItem{{
		Uuid: clearOrCompactUUID(ev, "clear"), TsMs: ev.GetProducedAtMs(), RequestId: ev.GetRequestId(),
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item:   &frontendv1.ConversationItem_ContextCleared{ContextCleared: cc},
	}}
}

// contextCompactedItems curates a first-class ContextCompacted into its arm,
// carrying the core.v1 message VERBATIM.
//
// The message reaching here is ALREADY the coalesced account: the shim-sidecar
// is the sole producer and merges the vendor's start status, boundary counts
// and separately-written summary line into one fact before it is ever an event.
// So there is nothing left for this layer to correlate, and re-typing the
// result into a webapp-shaped struct would only be a chance to drift from the
// fact the daemon already resolved. That is the whole reason the retired
// compact_boundary / compact_boundary_line arms are gone.
func contextCompactedItems(cc *corev1.ContextCompacted, ev *corev1.Event) []*frontendv1.ConversationItem {
	if cc == nil {
		return nil
	}
	return []*frontendv1.ConversationItem{{
		Uuid: clearOrCompactUUID(ev, "compact"), TsMs: ev.GetProducedAtMs(), RequestId: ev.GetRequestId(),
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item:   &frontendv1.ConversationItem_ContextCompacted{ContextCompacted: cc},
	}}
}

// clearOrCompactUUID is a clear's or a compaction's reconciliation identity.
//
// It is the event's DEDUP KEY (`clear:<uuid>` / `compact:<uuid>`) — the very
// key the store merges twins on, so every replay yields one item a frontend
// replaces in place rather than accumulating. No other field on the envelope is
// stable across replays of the same event.
//
// An event that carries no dedup key was never deduped, and its identity is
// then the position the store assigned it: seq is authoritative, gapless and
// per-session, so the derived id is just as stable across replays. kind keeps
// the derived form ("clear" / "compact") honest about which of the two it names,
// matching the producer's own prefixes so the two id spaces never overlap.
//
// Deriving it rather than dropping the item is what keeps the event RENDERABLE:
// an item with no uuid would leave a frontend discarding its history at a floor
// it can show no reason for.
func clearOrCompactUUID(ev *corev1.Event, kind string) string {
	if key := ev.GetDedupKey(); key != "" {
		return key
	}
	return fmt.Sprintf("%s:%s:%d", kind, ev.GetSessionId(), ev.GetSeq())
}

// --- transcript (file) plane -----------------------------------------------

// transcriptLineItems curates the conversation-bearing on-disk line types, and
// returns the RecordEnvelope of each item it produced alongside them.
func transcriptLineItems(tl *datav1.TranscriptLine, producedAtMs int64, requestID string) ([]*frontendv1.ConversationItem, map[string]RecordEnvelope) {
	switch line := tl.GetLine().(type) {
	case *datav1.TranscriptLine_Assistant:
		al := line.Assistant
		env := al.GetEnvelope()
		items := assistantMessageItem(env.GetUuid(), transcriptTsMs(env, producedAtMs), requestID, al.GetMessage())
		return items, recordEnvelopes(items, env)
	case *datav1.TranscriptLine_User:
		ul := line.User
		env := ul.GetEnvelope()
		items := userMessageItem(env.GetUuid(), transcriptTsMs(env, producedAtMs), requestID, ul.GetMessage())
		return items, recordEnvelopes(items, env)
	case *datav1.TranscriptLine_System:
		return systemLineItems(line.System, producedAtMs, requestID), nil
	default:
		return nil, nil // non-conversational metadata line
	}
}

// recordEnvelopes keys one on-disk line's envelope by the uuid of each item it
// curated to, which is the only handle the curators downstream have on it.
func recordEnvelopes(items []*frontendv1.ConversationItem, env *datav1.LineEnvelope) map[string]RecordEnvelope {
	if len(items) == 0 {
		return nil
	}
	re := RecordEnvelope{ParentUUID: env.GetParentUuid(), IsMeta: env.GetIsMeta()}
	out := make(map[string]RecordEnvelope, len(items))
	for _, it := range items {
		out[it.GetUuid()] = re
	}
	return out
}

// systemLineItems curates the system-line subtypes the webapp renders:
// compaction boundaries and terminal API failures. A mid-backoff API error
// curates to nothing here — internal/progress's retrying window is what
// covers it — so no daemon re-typing of the retry shape is needed.
func systemLineItems(sl *datav1.SystemLine, tsMs int64, requestID string) []*frontendv1.ConversationItem {
	uuid := sl.GetEnvelope().GetUuid()
	switch sub := sl.GetSubtype().(type) {
	case *datav1.SystemLine_ApiError:
		if sub.ApiError == nil {
			return nil
		}
		// Only a TERMINAL failure becomes a conversation item, as the
		// classified card (F4). Reporting a mid-backoff retry as one is how a
		// working session came to look broken between attempts.
		//
		// The webapp used to classify this same line by its OWN rule — and a
		// third rule for "fatal" on top — while the daemon classified it by a
		// different one, so the two processes disagreed about what the same
		// bytes meant. Both hold the fact; only the daemon holds the cause, so
		// the daemon decides and the frontend renders.
		//
		// Its uuid is derived from this line's rather than reusing it, so the
		// card stays stable and addressable across a resync. The raw api_error
		// passthrough this arm used to also emit alongside the card is retired
		// (step 11): both frontends now read the card only.
		failure := ApiFailureFromLine(sub.ApiError, uuid)
		if failure == nil {
			return nil
		}
		return []*frontendv1.ConversationItem{{
			Uuid: failure.GetItemUuid(), TsMs: tsMs, RequestId: requestID,
			Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
			Item:   &frontendv1.ConversationItem_SystemFailure{SystemFailure: failure},
		}}
	default:
		return nil
	}
}

// FailureUUID is the card uuid derived from the conversation item a failure
// came from. Deriving it — rather than minting a fresh one — is what keeps the
// card stable across a resync and distinct from the legacy item it accompanies.
func FailureUUID(itemUUID string) string {
	return "failure:" + itemUUID
}

// ApiFailureFromLine classifies a TERMINAL ApiErrorLine as a failure card, and
// returns nil for one that is still mid-backoff.
//
// The retrying/terminal split is the daemon's single rule (see
// internal/progress, which drives the retrying window off the same test): a
// line the SDK will try again is not a failure to report, because the turn is
// still in flight. Reporting it as one is how a working session came to look
// broken between attempts.
func ApiFailureFromLine(ae *datav1.ApiErrorLine, itemUUID string) *frontendv1.SystemFailureItem {
	if ae == nil || errclass.Retrying(ae) {
		return nil
	}
	return errclass.APIError(ae, FailureUUID(itemUUID))
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
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item:   &frontendv1.ConversationItem_AssistantMessage{AssistantMessage: msg},
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
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item:   &frontendv1.ConversationItem_UserMessage{UserMessage: msg},
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
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item:   &frontendv1.ConversationItem_Result{Result: r},
	}}
}

// ---------------------------------------------------------------------------
// TaskCatalog — folded from task-lifecycle events
// ---------------------------------------------------------------------------

// BuildTaskCatalog folds an ordered slice of core.Events (TaskStarted /
// TaskProgress / TaskEnded) into a TaskCatalog, preserving task start order.
// TaskProgress does not change status; TaskEnded stamps the terminal status and
// end time. Non-task events are ignored.
//
// A vendor `data.BackgroundTasksChanged` is AUTHORITATIVE reconciliation, not
// another increment: the SDK sends the complete live set on every change, so at
// that point in the stream the live set IS that list. Folding it (see
// applyBackgroundTasks) sweeps ghosts immediately instead of leaving them
// running until a LOST staleness sweep gets to them — which is what let
// replayed HISTORICAL task events masquerade as live activity in the footer's
// roster.
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
		case *corev1.Event_Vendor:
			if btc := BackgroundTasksFromVendor(p.Vendor); btc != nil {
				applyBackgroundTasks(btc, ev.GetProducedAtMs(), index, get)
			}
		}
	}
	catalog := &frontendv1.TaskCatalog{Workspace: workspace, SessionId: sessionID}
	for _, id := range order {
		catalog.Tasks = append(catalog.Tasks, index[id])
	}
	return catalog
}

// BackgroundTasksFromVendor decodes a vendor event's Any into its
// BackgroundTasksChanged arm, or nil when the Any is anything else. Exported so
// the session controller can recognize the event without re-deriving the unwrap.
func BackgroundTasksFromVendor(a *anypb.Any) *datav1.BackgroundTasksChanged {
	if a == nil {
		return nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil
	}
	csm, ok := msg.(*datav1.ClaudeStreamMessage)
	if !ok {
		return nil
	}
	return csm.GetBackgroundTasksChanged()
}

// applyBackgroundTasks reconciles the folded catalog against an authoritative
// live set, in both directions:
//
//   - A running entry ABSENT from the list is swept to `lost` at this event's
//     timestamp. `lost`, not `done`: the session says it is no longer running
//     and never said how it finished, and claiming success would be a
//     fabrication. It is the same terminal status the staleness sweep would
//     eventually assign, arrived at immediately.
//   - An id in the list with no entry yet is opened as `running`, carrying the
//     ref's type and description. Its start time is this event's timestamp —
//     the honest "first observed here", never a fabricated earlier one.
//   - An id in the list whose entry is already TERMINAL is re-opened: the list
//     is the live set as of this point in the stream, and a later TaskEnded
//     folds after it and closes it again.
func applyBackgroundTasks(
	btc *datav1.BackgroundTasksChanged,
	atMs int64,
	index map[string]*frontendv1.TaskEntry,
	get func(string) *frontendv1.TaskEntry,
) {
	live := make(map[string]struct{}, len(btc.GetTasks()))
	for _, ref := range btc.GetTasks() {
		if ref.GetTaskId() != "" {
			live[ref.GetTaskId()] = struct{}{}
		}
	}
	for id, e := range index {
		if _, ok := live[id]; ok {
			continue
		}
		if e.GetStatus() == "running" {
			e.Status = terminalStatusString(corev1.TerminalStatus_TERMINAL_STATUS_LOST)
			e.EndedAtMs = atMs
		}
	}
	for _, ref := range btc.GetTasks() {
		if ref.GetTaskId() == "" {
			continue
		}
		e := get(ref.GetTaskId())
		if e.GetStartedAtMs() == 0 {
			e.StartedAtMs = atMs
		}
		if e.GetKind() == "" {
			e.Kind = ref.GetTaskType()
		}
		if e.GetDescription() == "" {
			e.Description = ref.GetDescription()
		}
		e.Status = "running"
		e.EndedAtMs = 0
	}
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
