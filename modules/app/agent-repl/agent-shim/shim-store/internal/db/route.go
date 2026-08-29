package db

import (
	"database/sql"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// The `kind` column's closed vocabulary: THE ROUTED ARM, and nothing finer.
//
// It is the store's whole account of what a row is. Everything below
// kindPageLine is a NEVER-SERVED row — it carries no book, so no page query can
// reach it — and the arm is WHY it cannot be served, which is the only thing
// an investigator needs from the column.
const (
	kindPageLine       = "page_line"
	kindKeepalive      = "keepalive"
	kindVendorSpecific = "vendor_specific"
	kindUnknown        = "unknown"
	kindUnparsed       = "unparsed"
	kindBash           = "bash"
	kindWorkflow       = "workflow"
	kindSessionUpdate  = "session_update"
	kindDetachedWork   = "detached_work"
)

// Plane column values. The observing plane is a producer-side fact the store is
// entitled to, kept for attribution in the store's own logs and never served.
const (
	planeStream = 1
	planeFile   = 2
)

// detached_work.kind values.
const (
	detachedKindSubagent = "subagent"
	detachedKindBash     = "bash"
	detachedKindWorkflow = "workflow"
	detachedKindMonitor  = "monitor"
	// detachedKindDetached is the kind of an announcement whose origin arm is
	// `detached`. THE MESSAGE DOES NOT SAY WHAT KIND OF WORK LEFT: only the
	// `created` arm carries a DetachableWork, because a `detached` announcement
	// continues a unit the consumer already has. The store refuses to guess a
	// kind it was not told, records the announcement under this marker, and
	// never DOWNGRADES a row that a more specific write already classified
	// (see upsertDetachedWork).
	detachedKindDetached = "detached"
)

// detached_work.cause values.
const (
	causeRequested = "requested"
	causeByUser    = "by_user"
	causeTimedOut  = "timed_out"
	causeCreated   = "created"
)

// routed is one validated StoreEntry, resolved to the columns of its `entry`
// row. The proto message rides along because the LIFECYCLE effects (the agent,
// detached_work rows) dispatch on the same arms a second time — deliberately,
// so classification stays a pure, separately testable function.
type routed struct {
	entry     *storev1.StoreEntry
	writeID   string
	upsertKey string
	plane     int64
	kind      string
	// book is StorePageLine.page_agent_id.value for a page line and NULL for
	// every never-served row. It IS the never-served index.
	book sql.NullString
	// runID is StoreAgentBash.run.value for a bash row and NULL for everything
	// else. It is the bash equivalent of `book`: the indexed column WatchBashRun
	// replays a run's own rows by, so following one detached shell costs the
	// same single indexed lookup a page does.
	runID sql.NullString
	// topLevel is StoreAgentUpdate.top_level, the nearest non-sync ancestor.
	topLevel sql.NullString
	// frame is the whole serialized StoreEntry, exactly as written.
	frame []byte
	// pageLine is the line a page or a watcher serves. Non-nil exactly when
	// kind == kindPageLine.
	pageLine *storev1.StorePageLine
	// bashRow is the row a WatchBashRun watcher serves. Non-nil exactly when
	// kind == kindBash.
	bashRow *storev1.StoreAgentBash
}

// classify validates one StoreEntry whole and resolves its `entry` row.
//
// IT OPENS THE FRAME ONLY THIS FAR. Every field it reads is one the store
// itself indexes, filters or joins on; the conversation vocabulary underneath —
// what a tool call said, what the model answered — is never inspected, because
// interpreting it is the shim's job and unpacking it would put conversation.v1
// in the DDL.
func classify(entry *storev1.StoreEntry, index int) (routed, error) {
	if entry == nil {
		return routed{}, invalidf("entries[%d] is nil", index)
	}
	r := routed{entry: entry, writeID: entry.GetWriteId(), upsertKey: entry.GetUpsertKey()}

	plane, err := validatePlane(entry.GetPlane(), index)
	if err != nil {
		return routed{}, err
	}
	r.plane = plane
	if r.writeID == "" {
		return routed{}, invalidf("entries[%d].write_id is empty", index)
	}
	if r.upsertKey == "" {
		return routed{}, invalidf("entries[%d].upsert_key is empty (write_id=%q)", index, r.writeID)
	}

	frame, err := proto.Marshal(entry)
	if err != nil {
		return routed{}, invalidf("entries[%d] (write_id=%q) cannot be re-serialized: %v", index, r.writeID, err)
	}
	r.frame = frame

	switch arm := entry.GetEntry().(type) {
	case *storev1.StoreEntry_SessionUpdate:
		if err := validateSessionUpdate(arm.SessionUpdate, index); err != nil {
			return routed{}, err
		}
		// A session-scoped fact belongs to no book and names no ancestor: it is
		// the session's own, not any agent's work.
		r.kind = kindSessionUpdate
		return r, nil
	case *storev1.StoreEntry_AgentUpdate:
		return classifyAgentUpdate(r, arm.AgentUpdate, index)
	default:
		return routed{}, invalidf("entries[%d] (write_id=%q) sets no `entry` arm", index, r.writeID)
	}
}

// validatePlane is the base function for store.v1.Plane: an entry that does not
// say which producer observed it is unattributable and refused.
func validatePlane(plane *storev1.Plane, index int) (int64, error) {
	switch plane.GetPlane().(type) {
	case *storev1.Plane_Stream:
		return planeStream, nil
	case *storev1.Plane_File:
		return planeFile, nil
	default:
		return 0, invalidf("entries[%d].plane sets no arm — the entry does not name the producer that observed it", index)
	}
}

// validateSessionUpdate is the base function for conversation.v1.SessionUpdate
// at the store's depth: the arm must be set, because an unset oneof is an
// entry that states no fact at all.
func validateSessionUpdate(update *conversationv1.SessionUpdate, index int) error {
	if update == nil {
		return invalidf("entries[%d].session_update is nil", index)
	}
	if update.GetUpdate() == nil {
		return invalidf("entries[%d].session_update sets no `update` arm", index)
	}
	return nil
}

// classifyAgentUpdate is the base function for store.v1.StoreAgentUpdate.
func classifyAgentUpdate(r routed, update *storev1.StoreAgentUpdate, index int) (routed, error) {
	if update == nil {
		return routed{}, invalidf("entries[%d].agent_update is nil", index)
	}
	if update.TopLevel != nil {
		if update.GetTopLevel().GetValue() == "" {
			return routed{}, invalidf("entries[%d].agent_update.top_level is present with an empty value — absence is expressed by absence, never by an empty identifier", index)
		}
		r.topLevel = sql.NullString{String: update.GetTopLevel().GetValue(), Valid: true}
	}

	switch arm := update.GetAgentInfo().(type) {
	case *storev1.StoreAgentUpdate_ServeableFrame:
		return classifyServeableFrame(r, arm.ServeableFrame, index)
	case *storev1.StoreAgentUpdate_UnservedItem:
		kind, err := classifyUnservedItem(arm.UnservedItem, index)
		if err != nil {
			return routed{}, err
		}
		r.kind = kind
		return r, nil
	case *storev1.StoreAgentUpdate_Bash:
		if err := validateStoreAgentBash(arm.Bash, index); err != nil {
			return routed{}, err
		}
		// A BASH FRAME IS ITS OWN ROW, not merely a lifecycle-table update.
		// The sidecar writes a detached run's spool in deltas, and every one
		// of them must survive and be replayable in order — so the run's
		// history lives in the entry spine under `run_id`, the way a book's
		// history lives there under `book_agent_id`. It is still NOT a page
		// line: a detached run has no book, and its reader is WatchBashRun.
		r.kind = kindBash
		r.runID = sql.NullString{String: arm.Bash.GetRun().GetValue(), Valid: true}
		r.bashRow = arm.Bash
		return r, nil
	case *storev1.StoreAgentUpdate_Workflow:
		if err := validateStoreAgentWorkflow(arm.Workflow, index); err != nil {
			return routed{}, err
		}
		r.kind = kindWorkflow
		return r, nil
	default:
		return routed{}, invalidf("entries[%d].agent_update sets no `agent_info` arm — the producer did not decide the entry's pageability", index)
	}
}

// classifyServeableFrame is the base function for store.v1.StorePageLine. It is
// the ONLY arm that can produce a book, and the book comes from the envelope's
// page_agent_id rather than from the frame, because the producer decides
// pageability and names the book it decided on.
func classifyServeableFrame(r routed, line *storev1.StorePageLine, index int) (routed, error) {
	if line == nil {
		return routed{}, invalidf("entries[%d].agent_update.serveable_frame is nil", index)
	}
	book := line.GetPageAgentId().GetValue()
	if book == "" {
		return routed{}, invalidf("entries[%d].agent_update.serveable_frame.page_agent_id is unset or empty — a page line must name its book", index)
	}
	item := line.GetAgentItem()
	if item == nil {
		return routed{}, invalidf("entries[%d].agent_update.serveable_frame.agent_item is nil", index)
	}

	switch arm := item.GetItem().(type) {
	case *storev1.StoreAgentItem_AgentPrompt:
		if err := validateAgentPrompt(arm.AgentPrompt, index); err != nil {
			return routed{}, err
		}
		r.kind = kindPageLine
		r.book = sql.NullString{String: book, Valid: true}
		r.pageLine = line
		return r, nil
	case *storev1.StoreAgentItem_AgentFrame:
		return classifyAgentFrame(r, line, arm.AgentFrame, index)
	default:
		return routed{}, invalidf("entries[%d].agent_update.serveable_frame.agent_item sets no `item` arm", index)
	}
}

// validateAgentPrompt is the base function for conversation.v1.AgentPrompt at
// the store's depth: the recipient is what the store routes on and a prompt
// always has exactly one.
func validateAgentPrompt(prompt *conversationv1.AgentPrompt, index int) error {
	if prompt == nil {
		return invalidf("entries[%d] carries a nil agent_prompt", index)
	}
	if prompt.GetAgent().GetValue() == "" {
		return invalidf("entries[%d].agent_prompt.agent is unset or empty — a prompt always has exactly one recipient", index)
	}
	return nil
}

// classifyAgentFrame is the base function for conversation.v1.AgentFrame.
//
// THREE OF ITS FOUR ARMS ARE PAGE LINES AND ONE IS NOT. `detached_work` is an
// announcement that work left this stream; the spawning CALL is already the
// page line for it, so landing a second one would draw the same work twice.
func classifyAgentFrame(r routed, line *storev1.StorePageLine, frame *conversationv1.AgentFrame, index int) (routed, error) {
	if frame == nil {
		return routed{}, invalidf("entries[%d] carries a nil agent_frame", index)
	}
	if frame.GetAgentId().GetValue() == "" {
		return routed{}, invalidf("entries[%d].agent_frame.agent_id is unset or empty — a frame's attribution is its whole placement", index)
	}
	book := line.GetPageAgentId().GetValue()

	switch arm := frame.GetResult().(type) {
	case *conversationv1.AgentFrame_Update:
		if err := validateAgentUpdate(arm.Update, index); err != nil {
			return routed{}, err
		}
	case *conversationv1.AgentFrame_Success:
		if arm.Success.GetOutcome() == nil {
			return routed{}, invalidf("entries[%d].agent_frame.success sets no `outcome` arm", index)
		}
	case *conversationv1.AgentFrame_Failure:
		if arm.Failure.GetFailure() == nil {
			return routed{}, invalidf("entries[%d].agent_frame.failure sets no `failure` arm", index)
		}
	case *conversationv1.AgentFrame_DetachedWork:
		kind, err := validateDetachedWork(arm.DetachedWork, index)
		if err != nil {
			return routed{}, err
		}
		// A workflow announcement lands as workflow residue rather than as a
		// detached-work row's kind alone, so the not-implemented warning is
		// raised from exactly one place.
		if kind == detachedKindWorkflow {
			r.kind = kindWorkflow
		} else {
			r.kind = kindDetachedWork
		}
		return r, nil
	default:
		return routed{}, invalidf("entries[%d].agent_frame sets no `result` arm", index)
	}

	r.kind = kindPageLine
	r.book = sql.NullString{String: book, Valid: true}
	r.pageLine = line
	return r, nil
}

// validateAgentUpdate is the base function for conversation.v1.AgentUpdate at
// the store's depth. Every arm is conversation content and every arm is a page
// line — context_cut and api_error included; the store distinguishes none of
// them and only insists that one is set.
func validateAgentUpdate(update *conversationv1.AgentUpdate, index int) error {
	if update == nil {
		return invalidf("entries[%d].agent_frame.update is nil", index)
	}
	switch arm := update.GetUpdate().(type) {
	case *conversationv1.AgentUpdate_Activity:
		return validateAgentActivity(arm.Activity, index)
	case nil:
		return invalidf("entries[%d].agent_frame.update sets no `update` arm", index)
	default:
		return nil
	}
}

// validateAgentActivity is the base function for conversation.v1.AgentActivity
// at the store's depth: the unit identity, because it is the join key the
// detached-work close runs on, and the item arm, because a unit with no kind
// states nothing.
func validateAgentActivity(activity *conversationv1.AgentActivity, index int) error {
	if activity == nil {
		return invalidf("entries[%d].agent_frame.update.activity is nil", index)
	}
	if activity.GetActivityId().GetValue() == "" {
		return invalidf("entries[%d].agent_frame.update.activity.activity_id is unset or empty — a unit that names itself nothing can never be upserted", index)
	}
	if activity.GetItem() == nil {
		return invalidf("entries[%d].agent_frame.update.activity sets no `item` arm", index)
	}
	return nil
}

// classifyUnservedItem is the base function for store.v1.StoreUnservedItem.
// THE ARM IS WHY the row can never be served, so it is exactly the `kind`.
func classifyUnservedItem(item *storev1.StoreUnservedItem, index int) (string, error) {
	if item == nil {
		return "", invalidf("entries[%d].agent_update.unserved_item is nil", index)
	}
	switch arm := item.GetUnservedItem().(type) {
	case *storev1.StoreUnservedItem_Keepalive:
		if arm.Keepalive.GetItem() == nil {
			return "", invalidf("entries[%d].agent_update.unserved_item.keepalive sets no `item` arm", index)
		}
		return kindKeepalive, nil
	case *storev1.StoreUnservedItem_VendorSpecific:
		return kindVendorSpecific, nil
	case *storev1.StoreUnservedItem_Unknown:
		return kindUnknown, nil
	case *storev1.StoreUnservedItem_Unparsed:
		return kindUnparsed, nil
	default:
		return "", invalidf("entries[%d].agent_update.unserved_item sets no arm — the residue must say WHY it cannot be served", index)
	}
}

// validateStoreAgentBash is the base function for store.v1.StoreAgentBash. The
// run identity is the detached_work row's key, so an empty one has nowhere to
// land.
func validateStoreAgentBash(bash *storev1.StoreAgentBash, index int) error {
	if bash == nil {
		return invalidf("entries[%d].agent_update.bash is nil", index)
	}
	if bash.GetRun().GetValue() == "" {
		return invalidf("entries[%d].agent_update.bash.run is unset or empty — the run identity is the join the frame exists for", index)
	}
	if bash.GetFrame().GetResult() == nil {
		return invalidf("entries[%d].agent_update.bash.frame sets no `result` arm", index)
	}
	return nil
}

// validateStoreAgentWorkflow is the base function for
// store.v1.StoreAgentWorkflow.
func validateStoreAgentWorkflow(workflow *storev1.StoreAgentWorkflow, index int) error {
	if workflow == nil {
		return invalidf("entries[%d].agent_update.workflow is nil", index)
	}
	if workflow.GetRun().GetValue() == "" {
		return invalidf("entries[%d].agent_update.workflow.run is unset or empty", index)
	}
	if workflow.GetFrame().GetResult() == nil {
		return invalidf("entries[%d].agent_update.workflow.frame sets no `result` arm", index)
	}
	return nil
}

// validateDetachedWork is the base function for
// conversation.v1.AgentDetachedWork, and it returns the detached_work `kind`
// the announcement resolves to.
func validateDetachedWork(work *conversationv1.AgentDetachedWork, index int) (string, error) {
	if work == nil {
		return "", invalidf("entries[%d].agent_frame.detached_work is nil", index)
	}
	if work.GetWork().GetValue() == "" {
		return "", invalidf("entries[%d].agent_frame.detached_work.work is unset or empty — the handle is what the work is addressed by", index)
	}
	if work.Output != nil && work.GetOutput().GetReadability() == nil {
		return "", invalidf("entries[%d].agent_frame.detached_work.output sets no `readability` arm — whether this reader may open the spool is the answer, not an assumption", index)
	}
	switch arm := work.GetOrigin().(type) {
	case *conversationv1.AgentDetachedWork_Detached:
		if arm.Detached.GetDetachedFromId().GetValue() == "" {
			return "", invalidf("entries[%d].agent_frame.detached_work.detached.detached_from_id is unset or empty", index)
		}
		if arm.Detached.GetCause() == nil {
			return "", invalidf("entries[%d].agent_frame.detached_work.detached sets no `cause` arm", index)
		}
		return detachedKindDetached, nil
	case *conversationv1.AgentDetachedWork_Created:
		return detachableWorkKind(arm.Created.GetWorkCreated(), index)
	default:
		return "", invalidf("entries[%d].agent_frame.detached_work sets no `origin` arm", index)
	}
}

// detachableWorkKind is the base function for conversation.v1.DetachableWork:
// the closed set of things that can exist detached from a turn.
func detachableWorkKind(work *conversationv1.DetachableWork, index int) (string, error) {
	switch work.GetWork().(type) {
	case *conversationv1.DetachableWork_Subagent:
		return detachedKindSubagent, nil
	case *conversationv1.DetachableWork_Bash:
		return detachedKindBash, nil
	case *conversationv1.DetachableWork_Workflow:
		return detachedKindWorkflow, nil
	case *conversationv1.DetachableWork_Monitor:
		return detachedKindMonitor, nil
	default:
		return "", invalidf("entries[%d].agent_frame.detached_work.created.work_created sets no `work` arm — a kind absent from DetachableWork cannot claim to be detached", index)
	}
}

// terminalArmNames are the oneof arm names that CONCLUDE a unit, in every
// unit vocabulary conversation.v1 declares.
var terminalArmNames = map[string]bool{
	"success":        true,
	"failure":        true,
	"ended":          true,
	"blocking_error": true,
}

// activityIsTerminal reports whether an activity's item has reached a terminal
// arm, in whatever vocabulary that item kind uses.
//
// READ REFLECTIVELY RATHER THAN BY A THIRTY-ARM TYPE SWITCH. Every unit kind
// spells its conclusion with the same arm NAMES, and a switch would have to be
// edited every time conversation.v1 grows a unit — silently reporting each new
// kind as never-terminal until someone noticed, which would leave detached-work
// rows open forever. Reading the arm name keeps a new unit correct on arrival.
func activityIsTerminal(activity *conversationv1.AgentActivity) bool {
	item := activity.ProtoReflect()
	itemOneof := item.Descriptor().Oneofs().ByName("item")
	if itemOneof == nil {
		return false
	}
	which := item.WhichOneof(itemOneof)
	if which == nil || which.Kind() != protoreflect.MessageKind {
		return false
	}
	unit := item.Get(which).Message()
	oneofs := unit.Descriptor().Oneofs()
	for i := 0; i < oneofs.Len(); i++ {
		set := unit.WhichOneof(oneofs.Get(i))
		if set != nil && terminalArmNames[string(set.Name())] {
			return true
		}
	}
	return false
}
