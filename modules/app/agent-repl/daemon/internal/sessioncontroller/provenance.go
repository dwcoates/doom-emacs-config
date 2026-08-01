package sessioncontroller

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// This file is the session controller's whole reading of CONVERSATION
// PROVENANCE: which of the two parties that can drive a shim — the user, or
// merge.Coordinator under its lease — produced an item.
//
// TWO SOURCES, ONE RULE. An item translated from the store is placed against
// the merge lease's DURABLE LEDGER by its own timestamp, because the same path
// runs again on every resync and must reach the same verdict then as it did
// live. An item this daemon COMPOSED (a prompt receipt, a permission card, a
// failure card) is stamped from the lease's live state, because it is being
// composed at this instant and its composer retains the stamped copy for
// replay. Both end at CONVERSATION_SOURCE_USER or CONVERSATION_SOURCE_MERGE;
// neither can produce UNSPECIFIED.

// stampConversationProvenance stamps every item of a translated delta and
// reports whether the delta may be pushed.
//
// A DELTA WHOSE PROVENANCE CANNOT BE RESOLVED IS NOT PUSHED. UNSPECIFIED is
// defined as a malformed frame the receiver must reject, so sending one would
// only move the failure to a place with less context to report it from. The
// refusal is loud, and it carries the workspace, the session, the store seq and
// the item count so the dropped content is identifiable in the log.
func (c *consumer) stampConversationProvenance(cd *frontendv1.ConversationDelta) bool {
	for _, item := range cd.GetItems() {
		if item == nil {
			c.logf("session-controller: conversation provenance REFUSED ws=%q session=%s seq=%d — the curated delta carries a nil item; %d item(s) are not pushed",
				c.workspace, c.sessionID, cd.GetThroughSeq(), len(cd.GetItems()))
			return false
		}
		source, err := c.ssm.ConversationSourceAt(c.workspace, item.GetTsMs())
		if err != nil {
			c.logf("session-controller: conversation provenance REFUSED ws=%q session=%s seq=%d uuid=%s ts_ms=%d: %v — %d item(s) are not pushed rather than sent with an unset source",
				c.workspace, c.sessionID, cd.GetThroughSeq(), item.GetUuid(), item.GetTsMs(), err, len(cd.GetItems()))
			return false
		}
		if err := frontend.StampItemConversationSource(item, source); err != nil {
			c.logf("session-controller: conversation provenance REFUSED ws=%q session=%s seq=%d uuid=%s: %v",
				c.workspace, c.sessionID, cd.GetThroughSeq(), item.GetUuid(), err)
			return false
		}
	}
	return true
}

// stampLocalItemProvenance stamps one daemon-composed item and reports whether
// it may be pushed.
func (c *consumer) stampLocalItemProvenance(item *frontendv1.ConversationItem) bool {
	source := frontendv1.ConversationSource_CONVERSATION_SOURCE_USER
	if c.ssm.MergeLeaseHeld(c.workspace) {
		source = frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE
	}
	if err := frontend.StampItemConversationSource(item, source); err != nil {
		c.logf("session-controller: local item provenance REFUSED ws=%q session=%s: %v — the item is not pushed rather than sent with an unset source",
			c.workspace, c.sessionID, err)
		return false
	}
	return true
}
