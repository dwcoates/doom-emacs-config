package feed

import (
	"strings"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/descriptorpb"
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
// directive literal is only ever the daemon's own directive turn.

// contextCutLiterals is the set of command literals that open a context-cut
// directive turn — /clear and /compact — read once off the SessionCommand
// enum's own descriptor, so a corrected spelling in the proto is corrected here
// too and nothing is hand-written.
var (
	contextCutOnce     sync.Once
	contextCutLiterals map[string]bool
)

func directiveLiterals() map[string]bool {
	contextCutOnce.Do(func() {
		contextCutLiterals = map[string]bool{}
		cuts := map[conversationv1.SessionCommand]bool{
			conversationv1.SessionCommand_SESSION_COMMAND_CLEAR:   true,
			conversationv1.SessionCommand_SESSION_COMMAND_COMPACT: true,
		}
		values := conversationv1.SessionCommand(0).Descriptor().Values()
		for i := 0; i < values.Len(); i++ {
			value := values.Get(i)
			if !cuts[conversationv1.SessionCommand(value.Number())] {
				continue
			}
			options, ok := value.Options().(*descriptorpb.EnumValueOptions)
			if !ok {
				continue
			}
			ext := proto.GetExtension(options, conversationv1.E_SessionCommandSpec)
			carried, ok := ext.(*conversationv1.SessionCommandSpec)
			if !ok || carried == nil || carried.GetLiteral() == "" {
				continue
			}
			contextCutLiterals[carried.GetLiteral()] = true
		}
	})
	return contextCutLiterals
}

// isContextCutDirective reports whether a main-agent prompt's said text is a
// /clear or /compact directive. The literal stands alone (/clear) or leads the
// text (/compact <instructions>), which is exactly how the daemon composes it.
func isContextCutDirective(said *conversationv1.UserSaid) bool {
	text := strings.TrimSpace(promptText(said))
	if text == "" {
		return false
	}
	first := text
	if i := strings.IndexAny(text, " \n"); i >= 0 {
		first = text[:i]
	}
	return directiveLiterals()[first]
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
