package frontend

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// This file owns ConversationItem PROVENANCE on the frontend wire: the single
// place a curated item's ConversationSource may be revised after the curator
// (translate.go) stamped its own verdict.
//
// The curator says CONVERSATION_SOURCE_USER, because an ordinary turn is what a
// translated store event describes and what every item predating the merge
// lease describes. The one party that can contradict it is whoever consults the
// merge lease's durable ledger, and it does so through here.

// StampConversationSource writes src onto every item of d.
//
// It refuses CONVERSATION_SOURCE_UNSPECIFIED outright. proto3 reserves zero for
// "the field was not populated", so a receiver seeing it is looking at a
// malformed frame and must reject it; stamping it deliberately would be
// manufacturing exactly that frame. A caller with no verdict has an error to
// surface, not a zero to write.
func StampConversationSource(d *frontendv1.ConversationDelta, src frontendv1.ConversationSource) error {
	if d == nil {
		return fmt.Errorf("frontend: cannot stamp conversation source %s onto a nil delta", src)
	}
	if src == frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED {
		return fmt.Errorf("frontend: refusing to stamp CONVERSATION_SOURCE_UNSPECIFIED on %d item(s) of workspace %q; UNSPECIFIED names a malformed frame, never a provenance",
			len(d.GetItems()), d.GetWorkspace())
	}
	for _, item := range d.GetItems() {
		if item == nil {
			return fmt.Errorf("frontend: conversation delta for workspace %q carries a nil item; provenance cannot be stamped on it",
				d.GetWorkspace())
		}
		item.Source = src
	}
	return nil
}

// StampItemConversationSource is StampConversationSource for one daemon-composed
// item that never travels inside a delta — a prompt receipt, a permission card,
// a system-failure card, a skill body.
func StampItemConversationSource(item *frontendv1.ConversationItem, src frontendv1.ConversationSource) error {
	if item == nil {
		return fmt.Errorf("frontend: cannot stamp conversation source %s onto a nil item", src)
	}
	if src == frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED {
		return fmt.Errorf("frontend: refusing to stamp CONVERSATION_SOURCE_UNSPECIFIED on item %q; UNSPECIFIED names a malformed frame, never a provenance",
			item.GetUuid())
	}
	item.Source = src
	return nil
}
