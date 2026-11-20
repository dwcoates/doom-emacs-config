// Package nameprefix decides the marks drawn immediately before a workspace's
// name (frontend.v1.WorkspaceNamePrefix): THE ONE DECISION every surface that
// draws a name wears, so the sidebar's row (frontend.v1.RosterRowName.prefix)
// and the topbar's name line (frontend.v1.TopbarWorkspaceName.prefix) can
// never disagree.
//
// The marks, in drawn order (the message's field order), each independent of
// the other:
//
//  1. the priority icon, from the workspace's priority (wsm.Workspace.Priority);
//  2. the emoji-ack glyph, the thumbs up a workspace the Slack emoji-ack
//     reaction system created wears (wsm.Workspace.EmojiAck).
package nameprefix

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

// EmojiAck is the glyph a workspace the Slack emoji-ack reaction system
// created wears before its name: the thumbs up the reaction itself is.
const EmojiAck = "👍"

// Of is the prefix REC's name wears, or nil when it wears no mark: the field
// is then UNSET, never set empty (workspace_name_prefix.proto).
func Of(rec wsm.Workspace) *frontendv1.WorkspaceNamePrefix {
	var prefix frontendv1.WorkspaceNamePrefix
	marked := false
	if rec.Priority != nil {
		prefix.Priority = Priority(*rec.Priority)
		marked = true
	}
	if rec.EmojiAck {
		prefix.EmojiAck = &frontendv1.WorkspaceNameEmojiAck{Text: EmojiAck}
		marked = true
	}
	if !marked {
		return nil
	}
	return &prefix
}

// Priority is the icon P draws as. A level outside wsm's four is an
// invariant violation (wsm refuses to record one), so it panics rather than
// drawing no icon.
func Priority(p wsm.Priority) *frontendv1.WorkspaceNamePriority {
	switch p {
	case wsm.PriorityP05:
		return &frontendv1.WorkspaceNamePriority{Level: &frontendv1.WorkspaceNamePriority_P05{P05: &frontendv1.WorkspaceNamePriorityP05{}}}
	case wsm.PriorityP1:
		return &frontendv1.WorkspaceNamePriority{Level: &frontendv1.WorkspaceNamePriority_P1{P1: &frontendv1.WorkspaceNamePriorityP1{}}}
	case wsm.PriorityP2:
		return &frontendv1.WorkspaceNamePriority{Level: &frontendv1.WorkspaceNamePriority_P2{P2: &frontendv1.WorkspaceNamePriorityP2{}}}
	case wsm.PriorityP3:
		return &frontendv1.WorkspaceNamePriority{Level: &frontendv1.WorkspaceNamePriority_P3{P3: &frontendv1.WorkspaceNamePriorityP3{}}}
	default:
		panic(fmt.Sprintf("nameprefix: wsm recorded priority %d, which is no level", p))
	}
}
