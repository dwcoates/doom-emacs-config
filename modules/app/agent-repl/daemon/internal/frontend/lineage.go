// lineage.go is the OUTBOUND AUDIT of MessageLineage: the one place that reads
// every Message the daemon is about to put on the wire and refuses to let a
// missing or self-contradictory lineage leave silently.
//
// WHY AN AUDIT AND NOT ONLY CONSTRUCTORS. Lineage is set at two constructors —
// FeedRowLineage for a message that sits directly in the feed, and
// OpenDetachedWork for a message that IS detached work — and that is where the
// invariant is MADE. This is where it is CHECKED, because the field is
// DENORMALIZED: top_level_message_id is derivable by walking parent_message_id
// to its end, and storing it anyway is the whole reason a page of ten messages
// costs one query. The cost of denormalizing is drift, and drift is silent: a
// message with an empty root does not fail to render, it fails to be FOUND by
// the page query that will read this field, and the user sees a short page they
// cannot explain.
//
// IT NEVER DROPS THE FRAME. A message with a broken lineage is still the user's
// content, and withholding it would turn a bookkeeping defect into missing
// conversation. The defect is recorded in full — naming the frame, the message
// and which half is wrong — and the frame goes out.
package frontend

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// auditFrameLineage records every lineage defect in one outbound frame.
//
// It reads the THREE Message-bearing arms and no others. Records that are not
// messages — turn and session boundaries, heartbeats, latency samples, claim
// bridges, query lifecycle, usage observations, rewinds, file-plane diagnostics
// — have no lineage field to check and are not looked at: they are
// agentshim.core.v1 Envelope payloads, and if any of them could carry a
// top_level_message_id it would become a phantom feed row. Their absence from
// this walk is the same fact as their absence from the schema.
func auditFrameLineage(frame *frontendv1.FrontendFrame, warn func(string, ...any)) {
	if frame == nil || warn == nil {
		return
	}
	switch f := frame.GetFrame().(type) {
	case *frontendv1.FrontendFrame_ConversationDelta:
		auditMessages("conversation_delta", f.ConversationDelta.GetMessages(), warn)
	case *frontendv1.FrontendFrame_DetachedWorkDelta:
		auditMessages("detached_work_delta.opened", f.DetachedWorkDelta.GetOpened(), warn)
	case *frontendv1.FrontendFrame_ConversationPage:
		auditMessages("conversation_page", f.ConversationPage.GetMessages(), warn)
	case *frontendv1.FrontendFrame_Snapshot:
		auditMessages("snapshot.detached_work", f.Snapshot.GetDetachedWork(), warn)
	}
}

// auditMessages checks one list of messages against the two lineage rules the
// contract states, and records each violation with the evidence that makes it
// actionable.
func auditMessages(where string, msgs []*frontendv1.Message, warn func(string, ...any)) {
	for _, m := range msgs {
		lineage := m.GetLineage()
		if lineage == nil {
			warn("frontend: MESSAGE LINEAGE MISSING frame=%s uuid=%s — the message carries no MessageLineage at all, so it has no top_level_message_id for a page query to select it by and it will be invisible to paging; the frame is delivered anyway because withholding it would turn a bookkeeping defect into missing conversation",
				where, m.GetUuid())
			continue
		}
		if lineage.GetTopLevelMessageId() == "" {
			warn("frontend: MESSAGE LINEAGE ROOTLESS frame=%s uuid=%s parent_message_id=%q — top_level_message_id is EMPTY, which the contract forbids on every message including a feed row (where it equals the message's own uuid); a page query selecting DISTINCT top_level_message_id cannot see this message at all",
				where, m.GetUuid(), lineage.GetParentMessageId())
			continue
		}
		// A FEED ROW NAMES ITSELF. Empty parent means the message sits directly
		// in the feed, and the contract states its root IS its own uuid. A root
		// that disagrees is the denormalization having drifted — the exact
		// failure this audit exists to make audible.
		if lineage.GetParentMessageId() == "" && lineage.GetTopLevelMessageId() != m.GetUuid() {
			warn("frontend: MESSAGE LINEAGE CONTRADICTS ITSELF frame=%s uuid=%s top_level_message_id=%s — the message names NO parent, so it sits directly in the feed and its top_level_message_id must be its own uuid; a feed row pointing at a different root is corruption, not a variant",
				where, m.GetUuid(), lineage.GetTopLevelMessageId())
			continue
		}
		// A CONTAINED MESSAGE IS NOT ITS OWN ROOT. Naming a parent and then
		// naming yourself as the feed row is the two halves disagreeing, which
		// would make the message a phantom feed row AND a child at once.
		if lineage.GetParentMessageId() != "" && lineage.GetTopLevelMessageId() == m.GetUuid() {
			warn("frontend: MESSAGE LINEAGE CONTRADICTS ITSELF frame=%s uuid=%s parent_message_id=%s — the message names a parent while claiming to be its own top-level feed row, so it would be counted as a page slot and drawn inside its parent at the same time",
				where, m.GetUuid(), lineage.GetParentMessageId())
		}
	}
}
