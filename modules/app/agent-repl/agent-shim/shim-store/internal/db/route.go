package db

import (
	"database/sql"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/encoding/protowire"
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
	kindPageLine = "page_line"
	// kindKeepaliveRetired is NO LONGER WRITTEN: the arm is refused
	// (SiteKeepaliveRetired). It names the rows stored before the rule, which
	// are inert and which a real record may supersede (applyIdentityPolicy).
	kindKeepaliveRetired = "keepalive"
	kindVendorSpecific   = "vendor_specific"
	kindUnknown          = "unknown"
	kindUnparsed         = "unparsed"
	kindBash             = "bash"
	kindWorkflow         = "workflow"
	kindSessionUpdate    = "session_update"
	kindDetachedWork     = "detached_work"
	// kindRetired is a PAGE LINE the file plane's re-derivation RETIRED
	// (store.v1 StoreRetirement): the record behind it no longer converts to
	// it. It is never WRITTEN as an entry's kind — only retire.go turns a row
	// into one. The row keeps its book, position and last frame, so its
	// pointer stays valid and a standing watch replays the retirement by
	// write_seq, while every page (which reads kindPageLine alone) stops
	// serving it. A real page line under the same key takes it back
	// (applyIdentityPolicy).
	kindRetired = "retired"
	// kindHookDropped is a HOOK ROW THAT DRAWS NOTHING — a hook's start, a
	// success or a cancellation — dropped by SweepHookLines (owner ruling
	// 2026-10-06, "we should stop storing hook records, they are just
	// bloat"). The row keeps only its identity: its key, book, position, write
	// ledger and the envelope's turn and place stamps (hookStampsFrame), never
	// the record. No page and no replay reads it; its position stays a valid
	// pointer (pointerInBookSQL), because a reader's mark may be exactly that
	// line. A page line under the same key in the same book takes it back
	// (applyIdentityPolicy): a pre-rule shim's failed outcome landing after its
	// start was swept.
	kindHookDropped = "hook_dropped"
)

// hookDrawsNothing reports whether an agent frame carries a hook firing that
// draws nothing in the feed: its start, a success, or a cancellation. A failed
// or blocked firing draws a card and is not one of them. It is the one piece of
// conversation vocabulary the hook sweep reads.
func hookDrawsNothing(frame *conversationv1.AgentFrame) bool {
	switch frame.GetUpdate().GetActivity().GetHook().GetResult().(type) {
	case *conversationv1.AgentHook_Start, *conversationv1.AgentHook_Succeeded, *conversationv1.AgentHook_Cancelled:
		return true
	default:
		return false
	}
}

// hookStampsFrame is what a dropped hook row keeps: the envelope's identity
// and stamps, and no agent_update. The turn and place stay because
// carryStoredStamps reads them back on any later write of the same key (a
// failed hook's outcome superseding its swept start), exactly as for any row.
func hookStampsFrame(entry *storev1.StoreEntry) ([]byte, error) {
	return proto.Marshal(&storev1.StoreEntry{
		Plane:     entry.GetPlane(),
		WriteId:   entry.GetWriteId(),
		UpsertKey: entry.GetUpsertKey(),
		Turn:      entry.Turn,
		Place:     entry.Place,
	})
}

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

// entryField spells one entry's field path exactly as the failure arm reports
// it, so a producer reading `entries[1].upsert_key` off the wire is reading the
// store's own name for what it sent wrong.
func entryField(index int, path string) string {
	if path == "" {
		return fmt.Sprintf("entries[%d]", index)
	}
	return fmt.Sprintf("entries[%d].%s", index, path)
}

// serveableFramePath is the envelope prefix every conversation.v1 fact inside a
// page line sits under.
//
// THE FIELD IS A FULL ENVELOPE PATH, ALWAYS. A refusal naming `agent_frame.
// agent_id` asks the producer to find a field that appears nowhere in the
// message it sent: the frame is reached through
// agent_update.serveable_frame.agent_item, and a path that starts halfway down
// is one the caller has to guess the top of. Every frame-depth refusal composes
// its path from this prefix so the whole vocabulary is walkable from the
// StoreEntry root.
const serveableFramePath = "agent_update.serveable_frame.agent_item."

// frameField spells one entry's field path for a fault INSIDE the page line's
// frame, rooted at the entry exactly as entryField's own paths are.
func frameField(index int, path string) string {
	return entryField(index, serveableFramePath+path)
}

// routed is one validated StoreEntry, resolved to the columns of its `entry`
// row. The proto message rides along because the LIFECYCLE effects (the agent,
// detached_work rows) dispatch on the same arms a second time — deliberately,
// so classification stays a pure, separately testable function.
type routed struct {
	entry *storev1.StoreEntry
	// index is this entry's place in the producer's batch, kept so a refusal
	// raised after classification can still name `entries[i]`.
	index     int
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
	// ownerUnknown marks a page line its producer wrote `owner_unknown`
	// (store.v1 StorePageLineOwnerUnknown): `book` is NULL until applyEntry
	// places the line in the book that already holds its upsert_key
	// (placeUnownedLine), and nothing downstream of the placement ever sees it
	// set.
	ownerUnknown bool
	// workflowNotImplemented marks an entry that carries workflow material this
	// wave serves nothing for. It is a FLAG rather than a `kind`, because a
	// workflow-kind DETACHED ANNOUNCEMENT is a page line like every other
	// announcement — the warning is about what the store does not yet SERVE,
	// not about where the row lands.
	workflowNotImplemented bool
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
		return routed{}, invalidFieldf(entryField(index, ""), "entries[%d] is nil", index)
	}
	r := routed{entry: entry, index: index, writeID: entry.GetWriteId(), upsertKey: entry.GetUpsertKey()}

	plane, err := validatePlane(entry.GetPlane(), index)
	if err != nil {
		return routed{}, err
	}
	r.plane = plane
	if r.writeID == "" {
		return routed{}, invalidFieldf(entryField(index, "write_id"), "entries[%d].write_id is empty", index)
	}
	if r.upsertKey == "" {
		return routed{}, invalidFieldf(entryField(index, "upsert_key"), "entries[%d].upsert_key is empty (write_id=%q)", index, r.writeID)
	}

	// A TURN IS NAMED OR ABSENT, never present and empty: an empty identifier
	// would read as a turn nobody opened, and the store keeps the first stamp a
	// row is written with (carryStoredStamps), so a blank one would stick.
	if entry.Turn != nil && entry.GetTurn().GetValue() == "" {
		return routed{}, invalidFieldf(entryField(index, "turn"), "entries[%d].turn is present with an empty value (write_id=%q) — absence is expressed by absence, never by an empty identifier", index, r.writeID)
	}
	if err := validateConversionVersion(entry, plane, index); err != nil {
		return routed{}, err
	}
	// A PLACE IS STATED OR ABSENT, never present and empty: the store keeps a
	// row's first stated place (carryStoredStamps), so a zero instant would
	// stick and order the row before the whole conversation.
	if entry.Place != nil && entry.GetPlace().GetAtMs() <= 0 {
		return routed{}, invalidSitef(SitePlaceNotPositive, entryField(index, "place.at_ms"),
			"entries[%d].place.at_ms is %d (write_id=%q) — a conversation place is a positive instant, and absence is expressed by leaving the place unset",
			index, entry.GetPlace().GetAtMs(), r.writeID)
	}

	frame, err := proto.Marshal(entry)
	if err != nil {
		return routed{}, invalidFieldf(entryField(index, ""), "entries[%d] (write_id=%q) cannot be re-serialized: %v", index, r.writeID, err)
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
		return routed{}, invalidFieldf(entryField(index, "entry"), "entries[%d] (write_id=%q) sets no `entry` arm", index, r.writeID)
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
		return 0, invalidFieldf(entryField(index, "plane"), "entries[%d].plane sets no arm — the entry does not name the producer that observed it", index)
	}
}

// validateConversionVersion holds StoreEntry.conversion_version to its plane:
// SET, and above zero, on every file-plane entry, and UNSET on every
// stream-plane one.
//
// THE VERSION IS WHAT A RETIREMENT IS DECIDED BY. A file-plane row that
// carried none would read as version 0 forever, so every later re-read would
// consider it produced by a superseded conversion; a stream-plane row carrying
// one would claim a file conversion produced it. Either is a producer defect
// the store refuses rather than stores.
func validateConversionVersion(entry *storev1.StoreEntry, plane int64, index int) error {
	field := entryField(index, "conversion_version")
	switch {
	case plane == planeFile && entry.ConversionVersion == nil:
		return invalidSitef(SiteConversionVersionPlane, field,
			"entries[%d] (write_id=%q) is a file-plane entry with no conversion_version — every row the file plane produces records the conversion that produced it", index, entry.GetWriteId())
	case plane == planeFile && entry.GetConversionVersion() == 0:
		return invalidSitef(SiteConversionVersionPlane, field,
			"entries[%d] (write_id=%q) is a file-plane entry with conversion_version 0, which names no conversion", index, entry.GetWriteId())
	case plane == planeStream && entry.ConversionVersion != nil:
		return invalidSitef(SiteConversionVersionPlane, field,
			"entries[%d] (write_id=%q) is a stream-plane entry carrying conversion_version %d — the stream plane converts nothing from a file", index, entry.GetWriteId(), entry.GetConversionVersion())
	default:
		return nil
	}
}

// validateSessionUpdate is the base function for conversation.v1.SessionUpdate
// at the store's depth: the arm must be set, because an unset oneof is an
// entry that states no fact at all.
func validateSessionUpdate(update *conversationv1.SessionUpdate, index int) error {
	if update == nil {
		return invalidFieldf(entryField(index, "session_update"), "entries[%d].session_update is nil", index)
	}
	if update.GetUpdate() == nil {
		return invalidFieldf(entryField(index, "session_update"), "entries[%d].session_update sets no `update` arm", index)
	}
	return nil
}

// classifyAgentUpdate is the base function for store.v1.StoreAgentUpdate.
func classifyAgentUpdate(r routed, update *storev1.StoreAgentUpdate, index int) (routed, error) {
	if update == nil {
		return routed{}, invalidFieldf(entryField(index, "agent_update"), "entries[%d].agent_update is nil", index)
	}
	if update.TopLevel != nil {
		if update.GetTopLevel().GetValue() == "" {
			return routed{}, invalidFieldf(entryField(index, "agent_update.top_level"), "entries[%d].agent_update.top_level is present with an empty value — absence is expressed by absence, never by an empty identifier", index)
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
		r.workflowNotImplemented = true
		return r, nil
	default:
		return routed{}, invalidFieldf(entryField(index, "agent_update"), "entries[%d].agent_update sets no `agent_info` arm — the producer did not decide the entry's pageability", index)
	}
}

// classifyServeableFrame is the base function for store.v1.StorePageLine. It is
// the ONLY arm that can produce a book, and the book comes from the envelope's
// page_agent_id rather than from the frame, because the producer decides
// pageability and names the book it decided on.
func classifyServeableFrame(r routed, line *storev1.StorePageLine, index int) (routed, error) {
	if line == nil {
		return routed{}, invalidFieldf(entryField(index, "agent_update.serveable_frame"), "entries[%d].agent_update.serveable_frame is nil", index)
	}
	switch line.GetBook().(type) {
	case *storev1.StorePageLine_PageAgentId:
	case *storev1.StorePageLine_OwnerUnknown:
		return classifyUnownedLine(r, line, index)
	default:
		return routed{}, invalidFieldf(entryField(index, "agent_update.serveable_frame.book"), "entries[%d].agent_update.serveable_frame sets no `book` arm — a page line names its book or says its owner is unknown", index)
	}
	book := line.GetPageAgentId().GetValue()
	if book == "" {
		return routed{}, invalidFieldf(entryField(index, "agent_update.serveable_frame.page_agent_id"), "entries[%d].agent_update.serveable_frame.page_agent_id is unset or empty — a page line must name its book", index)
	}
	item := line.GetAgentItem()
	if item == nil {
		return routed{}, invalidFieldf(entryField(index, "agent_update.serveable_frame.agent_item"), "entries[%d].agent_update.serveable_frame.agent_item is nil", index)
	}

	switch arm := item.GetItem().(type) {
	case *storev1.StoreAgentItem_AgentPrompt:
		if err := validateAgentPrompt(arm.AgentPrompt, index); err != nil {
			return routed{}, err
		}
		if err := requireBookMatchesFrame(book, arm.AgentPrompt.GetAgent().GetValue(), "agent_prompt.agent", index); err != nil {
			return routed{}, err
		}
		r.kind = kindPageLine
		r.book = sql.NullString{String: book, Valid: true}
		r.pageLine = line
		return r, nil
	case *storev1.StoreAgentItem_PeerMessage:
		if err := validatePeerMessage(arm.PeerMessage, index); err != nil {
			return routed{}, err
		}
		if err := requireBookMatchesFrame(book, arm.PeerMessage.GetAgent().GetValue(), "peer_message.agent", index); err != nil {
			return routed{}, err
		}
		r.kind = kindPageLine
		r.book = sql.NullString{String: book, Valid: true}
		r.pageLine = line
		return r, nil
	case *storev1.StoreAgentItem_AgentFrame:
		return classifyAgentFrame(r, line, arm.AgentFrame, index)
	default:
		return routed{}, invalidFieldf(entryField(index, "agent_update.serveable_frame.agent_item"), "entries[%d].agent_update.serveable_frame.agent_item sets no `item` arm", index)
	}
}

// requireBookMatchesFrame refuses a page line whose ENVELOPE names a different
// agent than the FRAME inside it.
//
// THE PRODUCER DECIDES PAGEABILITY, NOT ATTRIBUTION. `page_agent_id` says which
// book the producer chose to file this line in; the frame says whose fact it is.
// They are two statements about one line, and when they disagree the store has
// no way to pick a winner — accepting either would file an agent's own words
// under another agent's name, producing a book that reads as a conversation
// that never happened. The write is refused whole instead.
func requireBookMatchesFrame(book, frameAgent, what string, index int) error {
	if book == frameAgent {
		return nil
	}
	return invalidSitef(SitePageBookMismatch,
		entryField(index, "agent_update.serveable_frame.page_agent_id"),
		"entries[%d].agent_update.serveable_frame.page_agent_id is %q but %s is %q — the envelope and the frame disagree about whose line this is",
		index, book, what, frameAgent)
}

// validateAgentPrompt is the base function for conversation.v1.AgentPrompt at
// the store's depth: the recipient is what the store routes on and a prompt
// always has exactly one.
func validateAgentPrompt(prompt *conversationv1.AgentPrompt, index int) error {
	if prompt == nil {
		return invalidFieldf(entryField(index, "agent_update.serveable_frame.agent_item.agent_prompt"), "entries[%d] carries a nil agent_prompt", index)
	}
	if prompt.GetAgent().GetValue() == "" {
		return invalidFieldf(entryField(index, "agent_update.serveable_frame.agent_item.agent_prompt.agent"), "entries[%d].agent_prompt.agent is unset or empty — a prompt always has exactly one recipient", index)
	}
	return nil
}

// validatePeerMessage is the base function for conversation.v1.PeerMessage at
// the store's depth: the recipient is what the store routes on, exactly as an
// AgentPrompt's is, and a peer message always names one.
func validatePeerMessage(peer *conversationv1.PeerMessage, index int) error {
	if peer == nil {
		return invalidFieldf(entryField(index, "agent_update.serveable_frame.agent_item.peer_message"), "entries[%d] carries a nil peer_message", index)
	}
	if peer.GetAgent().GetValue() == "" {
		return invalidFieldf(entryField(index, "agent_update.serveable_frame.agent_item.peer_message.agent"), "entries[%d].peer_message.agent is unset or empty — a peer message always names one recipient", index)
	}
	return nil
}

// classifyAgentFrame is the base function for conversation.v1.AgentFrame.
//
// EVERY ARM IS A PAGE LINE, `detached_work` included: an announcement that work
// left this stream is the HANDOFF the book's reader has to see, and it is the
// one durable copy of what was announced. The arms differ in what else they
// touch — a terminal ends its agent, an announcement writes the join row — not
// in whether they are served.
func classifyAgentFrame(r routed, line *storev1.StorePageLine, frame *conversationv1.AgentFrame, index int) (routed, error) {
	if frame == nil {
		return routed{}, invalidFieldf(entryField(index, "agent_update.serveable_frame.agent_item.agent_frame"), "entries[%d] carries a nil agent_frame", index)
	}
	if frame.GetAgentId().GetValue() == "" {
		return routed{}, invalidFieldf(frameField(index, "agent_frame.agent_id"), "entries[%d].agent_frame.agent_id is unset or empty — a frame's attribution is its whole placement", index)
	}
	book := line.GetPageAgentId().GetValue()
	if err := requireBookMatchesFrame(book, frame.GetAgentId().GetValue(), "agent_frame.agent_id", index); err != nil {
		return routed{}, err
	}
	workflow, err := validateFrameResult(frame, index)
	if err != nil {
		return routed{}, err
	}
	r.workflowNotImplemented = r.workflowNotImplemented || workflow
	r.kind = kindPageLine
	r.book = sql.NullString{String: book, Valid: true}
	r.pageLine = line
	return r, nil
}

// classifyUnownedLine is the base function for a page line written
// `owner_unknown` (store.v1 StorePageLineOwnerUnknown).
//
// ONLY AN AGENT FRAME MAY BE UNOWNED, AND IT STATES NO ATTRIBUTION. A prompt and
// a peer message always name their recipient, so either one unowned is a
// producer defect; a frame that names an agent while its envelope says the
// owner is unknown states two contradicting things, and so does a top_level.
// The line is validated exactly as a booked frame is, and is left with no book:
// applyEntry places it (placeUnownedLine) before anything reads one.
func classifyUnownedLine(r routed, line *storev1.StorePageLine, index int) (routed, error) {
	frame := line.GetAgentItem().GetAgentFrame()
	if frame == nil {
		return routed{}, invalidFieldf(entryField(index, "agent_update.serveable_frame.owner_unknown"), "entries[%d].agent_update.serveable_frame is owner_unknown but carries no agent_frame — only an agent frame can be filed by its unit's existing row; a prompt or a peer message names its recipient", index)
	}
	if frame.GetAgentId().GetValue() != "" {
		return routed{}, invalidFieldf(frameField(index, "agent_frame.agent_id"), "entries[%d].agent_frame.agent_id is %q but the line is owner_unknown — an unowned frame states no attribution; the store stamps the book it is placed in", index, frame.GetAgentId().GetValue())
	}
	if r.topLevel.Valid {
		return routed{}, invalidFieldf(entryField(index, "agent_update.top_level"), "entries[%d].agent_update.top_level is %q but the line is owner_unknown — the placed row keeps the stored row's top_level", index, r.topLevel.String)
	}
	workflow, err := validateFrameResult(frame, index)
	if err != nil {
		return routed{}, err
	}
	r.workflowNotImplemented = r.workflowNotImplemented || workflow
	r.kind = kindPageLine
	r.ownerUnknown = true
	r.pageLine = line
	return r, nil
}

// validateFrameResult validates an agent frame's `result` arm, for a booked
// line and an unowned one alike, and reports whether it carries workflow
// material this wave serves nothing for.
func validateFrameResult(frame *conversationv1.AgentFrame, index int) (workflow bool, err error) {
	switch arm := frame.GetResult().(type) {
	case *conversationv1.AgentFrame_Update:
		if err := validateAgentUpdate(arm.Update, index); err != nil {
			return false, err
		}
	case *conversationv1.AgentFrame_Success:
		if arm.Success.GetOutcome() == nil {
			return false, invalidFieldf(frameField(index, "agent_frame.success"), "entries[%d].agent_frame.success sets no `outcome` arm", index)
		}
	case *conversationv1.AgentFrame_Failure:
		if arm.Failure.GetFailure() == nil {
			return false, invalidFieldf(frameField(index, "agent_frame.failure"), "entries[%d].agent_frame.failure sets no `failure` arm", index)
		}
	case *conversationv1.AgentFrame_DetachedWork:
		kind, err := validateDetachedWork(arm.DetachedWork, index)
		if err != nil {
			return false, err
		}
		// AN ANNOUNCEMENT IS A PAGE LINE. "Work left this stream" is something
		// the reader of the announcing agent's book must SEE — it is the
		// handoff, and drawing the spawning call without it leaves the feed
		// claiming work that is still in the turn. It is also the one durable
		// copy of what was announced (the spool, the cause, the timeout), which
		// is why the lifecycle table keeps only the join columns.
		workflow = kind == detachedKindWorkflow
	default:
		return false, invalidFieldf(frameField(index, "agent_frame"), "entries[%d].agent_frame sets no `result` arm", index)
	}
	return workflow, nil
}

// validateAgentUpdate is the base function for conversation.v1.AgentUpdate at
// the store's depth. Every arm is conversation content and every arm is a page
// line — context_cut and api_error included; the store distinguishes none of
// them and only insists that one is set.
func validateAgentUpdate(update *conversationv1.AgentUpdate, index int) error {
	if update == nil {
		return invalidFieldf(frameField(index, "agent_frame.update"), "entries[%d].agent_frame.update is nil", index)
	}
	switch arm := update.GetUpdate().(type) {
	case *conversationv1.AgentUpdate_Activity:
		return validateAgentActivity(arm.Activity, index)
	case nil:
		return invalidFieldf(frameField(index, "agent_frame.update"), "entries[%d].agent_frame.update sets no `update` arm", index)
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
		return invalidFieldf(frameField(index, "agent_frame.update.activity"), "entries[%d].agent_frame.update.activity is nil", index)
	}
	if activity.GetActivityId().GetValue() == "" {
		return invalidFieldf(frameField(index, "agent_frame.update.activity.activity_id"), "entries[%d].agent_frame.update.activity.activity_id is unset or empty — a unit that names itself nothing can never be upserted", index)
	}
	if activity.GetItem() == nil {
		return invalidFieldf(frameField(index, "agent_frame.update.activity"), "entries[%d].agent_frame.update.activity sets no `item` arm", index)
	}
	return nil
}

// classifyUnservedItem is the base function for store.v1.StoreUnservedItem.
// THE ARM IS WHY the row can never be served, so it is exactly the `kind`.
func classifyUnservedItem(item *storev1.StoreUnservedItem, index int) (string, error) {
	if item == nil {
		return "", invalidFieldf(entryField(index, "agent_update.unserved_item"), "entries[%d].agent_update.unserved_item is nil", index)
	}
	switch arm := item.GetUnservedItem().(type) {
	case *storev1.StoreUnservedItem_VendorSpecific:
		// THE VERBATIM RECORD IS THE ONLY THING RESIDUE IS FOR. These arms exist
		// so nothing unconvertible is dropped — a row saying only "there was
		// something here" IS the drop, dressed up as durability, and the
		// follow-up work (a converter, a model, a parser fix) is impossible
		// without the bytes.
		if arm.VendorSpecific.GetRaw() == nil {
			return "", invalidSitef(SiteResidueRawUnset,
				entryField(index, "agent_update.unserved_item.vendor_specific.raw"),
				"entries[%d].agent_update.unserved_item.vendor_specific.raw is unset — residue exists to carry the record entire, and without it the row records only that something was lost", index)
		}
		return kindVendorSpecific, nil
	case *storev1.StoreUnservedItem_Unknown:
		if arm.Unknown.GetRaw() == nil {
			return "", invalidSitef(SiteResidueRawUnset,
				entryField(index, "agent_update.unserved_item.unknown.raw"),
				"entries[%d].agent_update.unserved_item.unknown.raw is unset — a record we do not model is worth keeping only verbatim", index)
		}
		return kindUnknown, nil
	case *storev1.StoreUnservedItem_Unparsed:
		if arm.Unparsed.GetRaw() == "" {
			return "", invalidSitef(SiteResidueRawUnset,
				entryField(index, "agent_update.unserved_item.unparsed.raw"),
				"entries[%d].agent_update.unserved_item.unparsed.raw is empty — an unreadable record is investigable only through its bytes", index)
		}
		return kindUnparsed, nil
	default:
		if carriesRetiredKeepalive(item) {
			// NOTHING OF A KEEP-ALIVE IS STORED, ON EITHER PLANE (2026-09-23),
			// and the arm is now RESERVED in the proto, so a stale producer
			// still minting it reaches here with the arm as an unknown field.
			// Held rows named real work: the shim that predated the rule
			// tagged a backgrounded subagent's frames arriving during a
			// keep-alive turn, and the sidecar's page line for the same
			// upsert_key was then refused as an identity change, parking the
			// subagent's whole transcript. A producer still minting the arm is
			// a defect to surface, never a row to keep.
			return "", invalidSitef(SiteKeepaliveRetired,
				entryField(index, "agent_update.unserved_item.keepalive"),
				"entries[%d].agent_update.unserved_item.keepalive is retired — nothing of a keep-alive is stored, on either plane", index)
		}
		return "", invalidFieldf(entryField(index, "agent_update.unserved_item"), "entries[%d].agent_update.unserved_item sets no arm — the residue must say WHY it cannot be served", index)
	}
}

// retiredKeepaliveField is store.v1.StoreUnservedItem's RESERVED tag 1, the
// retired `keepalive` arm.
const retiredKeepaliveField protowire.Number = 1

// carriesRetiredKeepalive reports whether an unserved item holds the retired
// `keepalive` arm. Generated code no longer knows the tag, so a stale
// producer's arm decodes as an UNKNOWN FIELD, and this reads it there. Unknown
// bytes that do not parse as a field sequence are no keep-alive arm; the item
// is then refused for setting no arm, which is still a loud refusal.
func carriesRetiredKeepalive(item *storev1.StoreUnservedItem) bool {
	unknown := item.ProtoReflect().GetUnknown()
	for len(unknown) > 0 {
		number, kind, n := protowire.ConsumeTag(unknown)
		if n < 0 {
			return false
		}
		if number == retiredKeepaliveField && kind == protowire.BytesType {
			return true
		}
		unknown = unknown[n:]
		m := protowire.ConsumeFieldValue(number, kind, unknown)
		if m < 0 {
			return false
		}
		unknown = unknown[m:]
	}
	return false
}

// validateStoreAgentBash is the base function for store.v1.StoreAgentBash. The
// run identity is the detached_work row's key, so an empty one has nowhere to
// land.
func validateStoreAgentBash(bash *storev1.StoreAgentBash, index int) error {
	if bash == nil {
		return invalidFieldf(entryField(index, "agent_update.bash"), "entries[%d].agent_update.bash is nil", index)
	}
	if bash.GetRun().GetValue() == "" {
		return invalidFieldf(entryField(index, "agent_update.bash.run"), "entries[%d].agent_update.bash.run is unset or empty — the run identity is the join the frame exists for", index)
	}
	if bash.GetFrame().GetResult() == nil {
		return invalidFieldf(entryField(index, "agent_update.bash.frame"), "entries[%d].agent_update.bash.frame sets no `result` arm", index)
	}
	// ONLY WHAT IS RENDERED IS STORED (owner ruling 2026-09-23). The tail's
	// bound is the contract's one constant, the same the producer cuts by and
	// the renderer draws by, so a longer tail is a producer that no longer
	// agrees with either — refused rather than stored past the cap.
	if n := len(bash.GetFrame().GetTail().GetText()); n > bashTailCap {
		return invalidSitef(SiteBashTailOverCap, entryField(index, "agent_update.bash.frame.tail.text"),
			"entries[%d].agent_update.bash.frame.tail.text is %d bytes, past the %d-byte AGENT_BASH_TAIL_CAP_BYTES", index, n, bashTailCap)
	}
	return nil
}

// bashTailCap is the contract's bound on a detached run's stored tail.
const bashTailCap = int(conversationv1.AgentBashTailCap_AGENT_BASH_TAIL_CAP_BYTES)

// validateStoreAgentWorkflow is the base function for
// store.v1.StoreAgentWorkflow.
func validateStoreAgentWorkflow(workflow *storev1.StoreAgentWorkflow, index int) error {
	if workflow == nil {
		return invalidFieldf(entryField(index, "agent_update.workflow"), "entries[%d].agent_update.workflow is nil", index)
	}
	if workflow.GetRun().GetValue() == "" {
		return invalidFieldf(entryField(index, "agent_update.workflow.run"), "entries[%d].agent_update.workflow.run is unset or empty", index)
	}
	if workflow.GetFrame().GetResult() == nil {
		return invalidFieldf(entryField(index, "agent_update.workflow.frame"), "entries[%d].agent_update.workflow.frame sets no `result` arm", index)
	}
	return nil
}

// validateDetachedWork is the base function for
// conversation.v1.AgentDetachedWork, and it returns the detached_work `kind`
// the announcement resolves to.
func validateDetachedWork(work *conversationv1.AgentDetachedWork, index int) (string, error) {
	if work == nil {
		return "", invalidFieldf(frameField(index, "agent_frame.detached_work"), "entries[%d].agent_frame.detached_work is nil", index)
	}
	if work.GetWork().GetValue() == "" {
		return "", invalidFieldf(frameField(index, "agent_frame.detached_work.work"), "entries[%d].agent_frame.detached_work.work is unset or empty — the handle is what the work is addressed by", index)
	}
	if work.Output != nil && work.GetOutput().GetReadability() == nil {
		return "", invalidFieldf(frameField(index, "agent_frame.detached_work.output"), "entries[%d].agent_frame.detached_work.output sets no `readability` arm — whether this reader may open the spool is the answer, not an assumption", index)
	}
	switch arm := work.GetOrigin().(type) {
	case *conversationv1.AgentDetachedWork_Detached:
		if arm.Detached.GetDetachedFromId().GetValue() == "" {
			return "", invalidFieldf(frameField(index, "agent_frame.detached_work.detached.detached_from_id"), "entries[%d].agent_frame.detached_work.detached.detached_from_id is unset or empty", index)
		}
		if arm.Detached.GetCause() == nil {
			return "", invalidFieldf(frameField(index, "agent_frame.detached_work.detached"), "entries[%d].agent_frame.detached_work.detached sets no `cause` arm", index)
		}
		return detachedKindDetached, nil
	case *conversationv1.AgentDetachedWork_Created:
		return detachableWorkKind(arm.Created.GetWorkCreated(), index)
	default:
		return "", invalidFieldf(frameField(index, "agent_frame.detached_work"), "entries[%d].agent_frame.detached_work sets no `origin` arm", index)
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
		return "", invalidFieldf(frameField(index, "agent_frame.detached_work.created.work_created"), "entries[%d].agent_frame.detached_work.created.work_created sets no `work` arm — a kind absent from DetachableWork cannot claim to be detached", index)
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
