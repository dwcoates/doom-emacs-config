package prompthandler

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/promptqueue"
	"claude-repld/internal/sessioncommand"
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

// recognized is the whole of what recognition made of a submission.
type recognized struct {
	kind Recognition
	// spec is the matched command, zero when the text is not a command.
	spec sessioncommand.Spec
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
	parsed := sessioncommand.Parse(text)
	if !parsed.Slash {
		return recognized{kind: RecognizedNone}
	}
	name, rest, matched := parsed.Name, parsed.Arg, parsed.Spec

	if !parsed.Known {
		// A command the closed set does not name FALLS THROUGH TO THE VENDOR
		// like any other text, per endpoint_submit_prompt.proto: the retired
		// /cost and /usage arms say so in as many words ("the commands fall
		// through to the vendor like any unrecognized command"), and
		// command_refused is for a command the daemon RECOGNIZES and neither
		// answers nor forwards. Refusing here would suppress every vendor and
		// user-authored slash command the enum has not been taught.
		return recognized{kind: RecognizedNone}
	}
	if rest != "" && !matched.TakesArgs {
		return recognized{kind: RecognizedNone}
	}

	switch {
	case PanelCommands[matched.Command]:
		return recognized{kind: RecognizedPanel, spec: matched, literal: name, arg: rest}
	case matched.Command == conversationv1.SessionCommand_SESSION_COMMAND_MODEL && rest == "":
		// BARE /model is refused daemon-side: the vendor's own picker is
		// unreachable through us, and the topbar's picker is the only
		// argument-less path to a model change.
		return recognized{kind: RecognizedRefused, spec: matched, literal: name}
	case ActCommands[matched.Command] != "":
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
