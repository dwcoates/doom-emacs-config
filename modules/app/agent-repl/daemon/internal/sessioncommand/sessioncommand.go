// Package sessioncommand is the ONE reading of a submission's text as a slash
// command. The prompt handler's recognition and the prompt queue's
// session-act predicate both parse through it, so the two can never disagree
// about what `/compact ...` is.
//
// Every literal, and whether trailing text is an argument, is read off the
// SessionCommand enum's session_command_spec option: nothing here is
// hand-written, so a corrected spelling in the proto is a corrected spelling
// in every reader.
package sessioncommand

import (
	"strings"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/descriptorpb"
)

// Spec is one command's schema-carried facts.
type Spec struct {
	Command   conversationv1.SessionCommand
	Literal   string
	TakesArgs bool
}

var (
	specsOnce sync.Once
	specs     map[string]Spec
)

// Specs reads the recognition table off the SessionCommand enum's descriptor,
// once, keyed by literal.
func Specs() map[string]Spec {
	specsOnce.Do(func() {
		specs = make(map[string]Spec)
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
			specs[carried.GetLiteral()] = Spec{
				Command:   conversationv1.SessionCommand(value.Number()),
				Literal:   carried.GetLiteral(),
				TakesArgs: carried.GetTakesArgs(),
			}
		}
	})
	return specs
}

// Parsed is what Parse made of a submission's text.
type Parsed struct {
	// Slash reports that the trimmed text begins with `/`.
	Slash bool
	// Name is the command as TYPED: the first whitespace-delimited run.
	Name string
	// Arg is the trimmed text after the name.
	Arg string
	// Spec is the matched command; Known reports whether the name matched one.
	Spec  Spec
	Known bool
}

// Parse splits text into a command name and its argument: the text is
// trimmed, a leading `/` marks a command, and the name runs to the first
// space.
func Parse(text string) Parsed {
	trimmed := strings.TrimSpace(text)
	if !strings.HasPrefix(trimmed, "/") {
		return Parsed{}
	}
	name, rest, _ := strings.Cut(trimmed, " ")
	rest = strings.TrimSpace(rest)
	spec, known := Specs()[name]
	return Parsed{Slash: true, Name: name, Arg: rest, Spec: spec, Known: known}
}

// ContextCut reports whether text IS a context cut — /clear or /compact — as
// the command table defines one: the literal alone, or, for a command that
// takes arguments, the literal followed by any text. It answers the command
// and its argument.
//
// This is the predicate that keeps a session act away from the routing
// classifier: whatever path a context cut's text arrives by, it is a session
// act, never a prompt to be judged.
func ContextCut(text string) (conversationv1.SessionCommand, string, bool) {
	got := Parse(text)
	if !got.Known {
		return conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED, "", false
	}
	switch got.Spec.Command {
	case conversationv1.SessionCommand_SESSION_COMMAND_CLEAR,
		conversationv1.SessionCommand_SESSION_COMMAND_COMPACT:
	default:
		return conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED, "", false
	}
	if got.Arg != "" && !got.Spec.TakesArgs {
		return conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED, "", false
	}
	return got.Spec.Command, got.Arg, true
}
