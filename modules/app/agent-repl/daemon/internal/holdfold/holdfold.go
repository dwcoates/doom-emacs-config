// Package holdfold is the ONE reading of what a held entry IS that the prompt
// queue and the hold tray both act on: whether an entry is a session act, and
// the text a held prompt says.
//
// It is a leaf beneath both. The queue decides delivery and the tray draws the
// entries, and each must read "is this a session act" exactly as the other
// does, or the tray would offer an action on an entry the queue then treats
// differently.
package holdfold

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/sessioncommand"
	"claude-repld/internal/wsm"
)

// SaidText renders a submission's text: the text blocks, joined by a newline.
// Images carry no text and contribute none. It is what the classifier judges,
// what the durable turn record keeps, and what a context cut is recognized in.
func SaidText(said *conversationv1.UserSaid) string {
	out := ""
	for _, block := range said.GetContent().GetBlocks() {
		if text, ok := block.GetBlock().(*conversationv1.UserContentBlock_Text); ok {
			if out != "" {
				out += "\n"
			}
			out += text.Text.GetText()
		}
	}
	return out
}

// ContextCut reports whether a submission addressed to TARGET saying SAID IS a
// context cut, and which, with its argument. A bubble-addressed prompt (a
// non-nil TARGET) goes to a subagent's own composer and is never a session act.
func ContextCut(target *feedid.Ref, said *conversationv1.UserSaid) (conversationv1.SessionCommand, string, bool) {
	if target != nil {
		return conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED, "", false
	}
	return sessioncommand.ContextCut(SaidText(said))
}

// SessionAct reports whether a hold is a session act rather than a prompt: a
// held model or permission-mode change, or a prompt whose text is a context
// cut (/compact, /clear and its aliases).
func SessionAct(h wsm.HeldPrompt) bool {
	if h.Act != nil {
		return true
	}
	_, _, cut := ContextCut(h.Target, h.Said)
	return cut
}
