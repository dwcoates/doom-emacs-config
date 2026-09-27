package feed

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/sessioncommand"
)

// A CONTEXT-CUT DIRECTIVE IS RECOGNISED FROM ITS OWN PROMPT TEXT, so the
// suppression of its prompt/response/terminal bubbles is ORDER-INDEPENDENT and
// survives a restart. The daemon opens a /clear or /compact as a turn whose
// prompt said is exactly the command literal (promptqueue/acts.go composes it),
// the store persists that said on the turn's UserPrompt entry, and that entry is
// ALWAYS the turn's first frame — live and on replay. So a prompt carrying a
// directive literal registers the turn as a directive the moment it is drawn,
// before the response or terminal that follow are, without depending on the
// ContextCut frame arriving first.
//
// This is safe because a user CANNOT send a directive literal as an ordinary
// prompt: recognition.go routes /clear and /compact as session acts, never as
// text that falls through to the vendor, so a main-agent prompt whose said is a
// directive is only ever the daemon's own directive turn. The daemon sends a
// /reset or /new as the canonical /clear.

// isContextCutDirective reports whether a main-agent prompt's said text is a
// /clear or /compact directive, read through sessioncommand.ContextCut — the
// ONE parse the daemon recognizes and routes a context cut by — so what the
// feed draws as a directive is exactly what the queue ran as one.
func isContextCutDirective(said *conversationv1.UserSaid) bool {
	_, _, cut := sessioncommand.ContextCut(promptText(said))
	return cut
}

// promptText joins a prompt's text blocks, which is the whole of what a command
// literal is read from — an image block carries no command.
func promptText(said *conversationv1.UserSaid) string {
	var b strings.Builder
	for _, block := range said.GetContent().GetBlocks() {
		if t, ok := block.GetBlock().(*conversationv1.UserContentBlock_Text); ok {
			if b.Len() > 0 {
				b.WriteByte('\n')
			}
			b.WriteString(t.Text.GetText())
		}
	}
	return b.String()
}
