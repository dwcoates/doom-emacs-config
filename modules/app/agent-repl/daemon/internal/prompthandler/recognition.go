package prompthandler

import (
	"strings"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/descriptorpb"

	"claude-repld/internal/promptqueue"
)

// PanelCommands are the four commands the daemon answers ITSELF with a panel.
// They are exactly frontend.v1's FeedCommandPanel arms: a panel the feed
// cannot draw is a panel the daemon must not claim to answer.
var PanelCommands = map[conversationv1.SessionCommand]bool{
	conversationv1.SessionCommand_SESSION_COMMAND_STATUS:  true,
	conversationv1.SessionCommand_SESSION_COMMAND_TODOS:   true,
	conversationv1.SessionCommand_SESSION_COMMAND_MCP:     true,
	conversationv1.SessionCommand_SESSION_COMMAND_CONTEXT: true,
}

// ActCommands are the commands the daemon carries down the queue's one
// delivery path, and the act kind each becomes.
var ActCommands = map[conversationv1.SessionCommand]string{
	conversationv1.SessionCommand_SESSION_COMMAND_CLEAR:   promptqueue.ActClear,
	conversationv1.SessionCommand_SESSION_COMMAND_COMPACT: promptqueue.ActCompact,
	conversationv1.SessionCommand_SESSION_COMMAND_MODEL:   promptqueue.ActSetModel,
}

// spec is one command's schema-carried facts, read back from the enum value's
// session_command_spec option. NOTHING here is hand-written: a corrected
// spelling in the proto is a corrected spelling in the recognizer.
type spec struct {
	command   conversationv1.SessionCommand
	literal   string
	takesArgs bool
}

var (
	specsOnce sync.Once
	specs     map[string]spec
)

// commandSpecs reads the recognition table off the SessionCommand enum's
// descriptor, once. The option is the ONE definition of every literal and of
// whether trailing text is an argument.
func commandSpecs() map[string]spec {
	specsOnce.Do(func() {
		specs = make(map[string]spec)
		values := conversationv1.SessionCommand(0).Descriptor().Values()
		for i := 0; i < values.Len(); i++ {
			value := values.Get(i)
			options, ok := value.Options().(*descriptorpb.EnumValueOptions)
			if !ok {
				continue
			}
			ext := proto.GetExtension(options, conversationv1.E_SessionCommandSpec)
			carried, ok := ext.(*conversationv1.SessionCommandSpec)
			if !ok || carried == nil || carried.GetLiteral() == "" {
				// SESSION_COMMAND_UNSPECIFIED carries no spec, deliberately: it
				// names no command, so there is nothing to match it against.
				continue
			}
			specs[carried.GetLiteral()] = spec{
				command:   conversationv1.SessionCommand(value.Number()),
				literal:   carried.GetLiteral(),
				takesArgs: carried.GetTakesArgs(),
			}
		}
	})
	return specs
}

// recognized is the whole of what recognition made of a submission.
type recognized struct {
	kind Recognition
	// spec is the matched command, zero when the text is not a command.
	spec spec
	// literal is the command as TYPED, which is what a refusal card draws and
	// what RequestCommandSupport is called with.
	literal string
	// arg is the text following a command that takes arguments.
	arg string
}

// recognize is the whole recognition table, in one function.
//
// FALSE IS THE SAFE SIDE on takes_args: a command that takes no argument is
// recognized only as an ENTIRE prompt, so "/status of the build" stays a prompt
// and keeps its user message. Suppressing a prompt a user genuinely meant is
// unrecoverable; forwarding a command is not.
func recognize(text string) recognized {
	trimmed := strings.TrimSpace(text)
	if !strings.HasPrefix(trimmed, "/") {
		return recognized{kind: RecognizedNone}
	}
	name, rest, _ := strings.Cut(trimmed, " ")
	rest = strings.TrimSpace(rest)

	matched, known := commandSpecs()[name]
	if !known {
		// A command the closed set does not name. It is RECOGNIZED as a
		// command — it opens with a slash and names no prompt — and refused
		// with the add-support offer rather than forwarded.
		return recognized{kind: RecognizedRefused, literal: name}
	}
	if rest != "" && !matched.takesArgs {
		return recognized{kind: RecognizedNone}
	}

	switch {
	case PanelCommands[matched.command]:
		return recognized{kind: RecognizedPanel, spec: matched, literal: name, arg: rest}
	case matched.command == conversationv1.SessionCommand_SESSION_COMMAND_MODEL && rest == "":
		// BARE /model is refused daemon-side: the vendor's own picker is
		// unreachable through us, and the topbar's picker is the only
		// argument-less path to a model change.
		return recognized{kind: RecognizedRefused, spec: matched, literal: name}
	case ActCommands[matched.command] != "":
		return recognized{kind: RecognizedAct, spec: matched, literal: name, arg: rest}
	default:
		// Every other command the CLI answers itself — /agents, /help, /cost,
		// /login and the rest — is recognized, never forwarded, and refused
		// with the add-support offer.
		return recognized{kind: RecognizedRefused, spec: matched, literal: name}
	}
}

// Recognize reports what the daemon makes of a submission's text without acting
// on it, answering the recognition and the literal command as typed.
func (h *handler) Recognize(text string) (Recognition, string) {
	got := recognize(text)
	return got.kind, got.literal
}
