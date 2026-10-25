// Package holdfold is the ONE reading of what a held entry IS that the prompt
// queue and the hold tray both act on: whether an entry is a session act, the
// text a held prompt says, which entry stands directly ahead of another, and
// whether a prompt may be folded into it (FoldHeldPrompt).
//
// It is a leaf beneath both. The queue decides delivery and folds, and the
// tray draws the entries and offers the "fold above" button, and each must
// read these exactly as the other does, or the tray would offer a fold the
// queue then refuses, or hide one it would take.
package holdfold

import (
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessioncommand"
	"agent-repld/internal/wsm"
)

// SaidText renders a submission's text: the text blocks, joined by a newline.
// Images carry no text and contribute none, and neither does a reply's quote
// block: it is not the person's words. It is what the classifier judges,
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

// Why a prompt may not be folded into the entry directly ahead of it. Each is
// a fact about the two entries as they stand, never a failure.
var (
	// ErrNotAPrompt: the entry being folded is a session act. An act is never
	// folded into anything.
	ErrNotAPrompt = errors.New("holdfold: the entry is a session act, not a prompt")
	// ErrAboveNotAPrompt: the entry ahead is a session act, which never takes
	// a prompt's words.
	ErrAboveNotAPrompt = errors.New("holdfold: the entry ahead is a session act, not a prompt")
	// ErrOtherAuthor: the two entries have different authors (SameAuthor), so
	// the merged prompt would put one author's words under the other's.
	ErrOtherAuthor = errors.New("holdfold: the entry ahead has another author")
	// ErrThreadQuestion: one of the two entries is a chat thread's question,
	// and one turn is one question, so neither takes the other's words.
	ErrThreadQuestion = errors.New("holdfold: a chat thread's question is never folded")
	// ErrEdited: one of the two entries is being edited, so its words are in
	// the editor and changing them underneath it is refused.
	ErrEdited = errors.New("holdfold: one of the two entries is being edited")
)

// SameAuthor reports whether two prompts have one author, by the workspace
// that sent each (wsm.HeldPrompt.SentBy, nil for a prompt no workspace's agent
// sent): no workspace sent either, or one workspace sent both. A prompt is
// never folded into another author's, since the merged prompt is drawn under
// one author.
func SameAuthor(a, b *wsm.WorkspaceID) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	return *a == *b
}

// Ahead answers the standing hold directly ahead of TURN in STANDING, which is
// in queue order (queued_at, then the turn id). False when TURN is first, or
// does not stand at all: nothing is ahead of it to fold into.
func Ahead(standing []wsm.HeldPrompt, turn ids.TurnID) (wsm.HeldPrompt, bool) {
	for i, h := range standing {
		if h.Turn != turn {
			continue
		}
		if i == 0 {
			return wsm.HeldPrompt{}, false
		}
		return standing[i-1], true
	}
	return wsm.HeldPrompt{}, false
}

// Foldable answers whether FOLDED may be folded into AHEAD, the entry directly
// ahead of it: nil when it may, or the reason it may not. EDITING is the turn
// the workspace's standing edit is on, empty when none stands.
//
// The reasons are checked in this order — the folded entry is an act, the
// entry ahead is an act, the two have different authors, either is a chat
// thread's question, either is being edited — so one pair always answers the same reason.
func Foldable(folded, ahead wsm.HeldPrompt, editing ids.TurnID) error {
	switch {
	case SessionAct(folded):
		return ErrNotAPrompt
	case SessionAct(ahead):
		return ErrAboveNotAPrompt
	case !SameAuthor(folded.SentBy, ahead.SentBy):
		return ErrOtherAuthor
	case folded.Ask != nil || ahead.Ask != nil:
		return ErrThreadQuestion
	case editing != "" && (editing == folded.Turn || editing == ahead.Turn):
		return ErrEdited
	}
	return nil
}
