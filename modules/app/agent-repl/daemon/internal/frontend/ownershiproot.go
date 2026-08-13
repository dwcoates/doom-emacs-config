// ownershiproot.go enforces the one rule the denormalization can violate:
// `top_level_message_id` MUST EQUAL THE ROOT OF THE PARENT CHAIN, and a write
// that disagrees is CORRUPTION, not a variant (FROZEN-message-lineage, Part 2).
//
// WHY IT IS CHECKED AT THE WRITE SITE AND NOT ONLY ON THE WIRE. lineage.go
// audits outbound frames and deliberately never drops one, because a frame with
// broken lineage is still the user's content. That is the right answer for
// DELIVERY and the wrong one for a WRITE: a stored record whose owner disagrees
// with its parent chain is wrong forever, is invisible to the page query that
// selects by the owner, and no later reader can tell it apart from a message
// that never existed. So here the disagreement REFUSES, and the refusal travels
// the curation path's existing error channel.
//
// IT WALKS ONLY WITHIN THE BATCH, ON PURPOSE. The check needs the parent's own
// root, and a message whose parent is not in the batch was constructed by
// NewDurableChild, which INHERITED the parent's root and so cannot disagree with
// it. Reaching outside the batch to re-derive a root would be the unbounded
// parent traversal this whole field exists to remove.
package frontend

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// VerifyOwnershipRoots refuses any message in one batch whose
// top_level_message_id is not the root of its own parent chain.
//
// The three ways the denormalization can drift, each refused by name:
//
//  1. NO ROOT AT ALL. Every message carries one, including a feed row where it
//     is self-referential. An empty root is not "top level" — it is a record no
//     `SELECT DISTINCT top_level_message_id` will ever return.
//  2. A FEED ROW NAMING SOMEONE ELSE. No parent means the message sits directly
//     in the feed, so its root IS its own uuid.
//  3. A CHILD DISAGREEING WITH ITS PARENT. When the parent is present in the
//     batch its root is known exactly, and a child naming a different one would
//     be filed under a feed row its parent is not in — the record and its
//     container would land on different pages.
//
// A CYCLE IS REFUSED RATHER THAN WALKED. Containment is a tree; a parent chain
// that revisits a message is corruption, and following it is a hang.
func VerifyOwnershipRoots(msgs []*frontendv1.Message) error {
	byID := make(map[string]*frontendv1.Message, len(msgs))
	for _, m := range msgs {
		if id := m.GetUuid(); id != "" {
			byID[id] = m
		}
	}

	for _, m := range msgs {
		uuid := m.GetUuid()
		root := m.GetLineage().GetTopLevelMessageId()
		parent := m.GetLineage().GetParentMessageId()

		if root == "" {
			return fmt.Errorf("frontend: ownership refused uuid=%s parent_message_id=%q: top_level_message_id is EMPTY, and the contract forbids that on every message including a feed row, where it equals the message's own uuid; a page query selecting DISTINCT top_level_message_id cannot see this message at all", uuid, parent)
		}
		if parent == "" {
			if root != uuid {
				return fmt.Errorf("frontend: ownership refused uuid=%s top_level_message_id=%s: the message names NO parent, so it sits directly in the feed and its root MUST be its own uuid; a feed row pointing at a different root is corruption, not a variant", uuid, root)
			}
			continue
		}
		if root == uuid {
			return fmt.Errorf("frontend: ownership refused uuid=%s parent_message_id=%s: the message names a parent while claiming to be its own top-level feed row, so it would be counted as a page slot and drawn inside its parent at the same time", uuid, parent)
		}

		// Walk to the end of the chain WITHIN the batch. A parent that is not
		// here was resolved by NewDurableChild, which inherited its own root,
		// so the walk stopping early is not a defect.
		seen := map[string]bool{uuid: true}
		cursor := parent
		for {
			if seen[cursor] {
				return fmt.Errorf("frontend: ownership refused uuid=%s: the parent chain revisits %s, so containment is a cycle rather than a tree and no root exists to agree with", uuid, cursor)
			}
			seen[cursor] = true
			ancestor, ok := byID[cursor]
			if !ok {
				// Chain leaves the batch: the deepest ancestor present already
				// carries the inherited root, checked on its own iteration.
				break
			}
			ancestorRoot := ancestor.GetLineage().GetTopLevelMessageId()
			if ancestorRoot != root {
				return fmt.Errorf("frontend: ownership refused uuid=%s top_level_message_id=%s: its ancestor %s names root %s instead, so the record and the message containing it would be filed under different feed rows and land on different pages", uuid, root, cursor, ancestorRoot)
			}
			next := ancestor.GetLineage().GetParentMessageId()
			if next == "" {
				if ancestorRoot != cursor {
					return fmt.Errorf("frontend: ownership refused uuid=%s top_level_message_id=%s: the chain ends at %s, which names no parent and is therefore the root, but the root recorded here is a different message", uuid, root, cursor)
				}
				break
			}
			cursor = next
		}
	}
	return nil
}
