package convert

// sessioncommand.go — WHICH SLASH COMMANDS THE CLI ANSWERS ITSELF.
//
// THE TABLE IS THE SCHEMA'S, NEVER THIS FILE'S. conversation.v1's
// SessionCommand enum carries each command's literal, its aliases and whether
// text after it is an argument as the `session_command_spec` enum-value
// option, and this reads it back by reflection exactly as the daemon's own
// recognizer does (daemon/internal/sessioncommand). The two readers live in
// different Go modules and the daemon's is `internal`, so the parse is restated
// here; the FACTS are not, and a corrected spelling in the proto is a corrected
// spelling in both.
//
// A COMMAND THAT IS NOT IN THE TABLE IS A PROMPT. A custom command (a skill, a
// project command) EXPANDS into a prompt for the agent, so what the person
// typed really is the turn's opening; the schema says so and this file never
// widens the set.

import (
	"strings"
	"sync"
	"unicode"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/descriptorpb"
)

// sessionCommandSpec is one command's schema-carried facts.
type sessionCommandSpec struct {
	command   conversationv1.SessionCommand
	takesArgs bool
}

var (
	sessionCommandsOnce sync.Once
	sessionCommands     map[string]sessionCommandSpec
)

// sessionCommandTable reads the recognition table off the SessionCommand enum's
// descriptor, once, keyed by every spelling the command is typed as: its
// literal and each of its aliases.
func sessionCommandTable() map[string]sessionCommandSpec {
	sessionCommandsOnce.Do(func() {
		sessionCommands = map[string]sessionCommandSpec{}
		values := conversationv1.SessionCommand(0).Descriptor().Values()
		for i := 0; i < values.Len(); i++ {
			value := values.Get(i)
			options, ok := value.Options().(*descriptorpb.EnumValueOptions)
			if !ok {
				continue
			}
			carried, ok := proto.GetExtension(options, conversationv1.E_SessionCommandSpec).(*conversationv1.SessionCommandSpec)
			if !ok || carried == nil || carried.GetLiteral() == "" {
				// SESSION_COMMAND_UNSPECIFIED carries no spec, deliberately: it
				// names no command, so there is nothing to match it against.
				continue
			}
			spec := sessionCommandSpec{
				command:   conversationv1.SessionCommand(value.Number()),
				takesArgs: carried.GetTakesArgs(),
			}
			sessionCommands[carried.GetLiteral()] = spec
			for _, alias := range carried.GetAliases() {
				sessionCommands[alias] = spec
			}
		}
	})
	return sessionCommands
}

// isSessionCommand reports whether a command NAME (leading slash included) with
// its ARGUMENT text is one the CLI answers itself.
//
// An argument on a command whose spec takes none means the text is prose that
// merely starts with the command's name — "/status of the build" is a prompt —
// which is the schema's own safe side.
func isSessionCommand(name, arg string) bool {
	spec, known := sessionCommandTable()[name]
	if !known {
		return false
	}
	return spec.takesArgs || arg == ""
}

// typedSessionCommand reports whether text, AS A PERSON TYPED IT, is a session
// command: trimmed, a leading `/`, and a name running to the first whitespace of
// any kind, exactly as the CLI splits it.
func typedSessionCommand(text string) bool {
	trimmed := strings.TrimSpace(text)
	if !strings.HasPrefix(trimmed, "/") {
		return false
	}
	name, arg := trimmed, ""
	if i := strings.IndexFunc(trimmed, unicode.IsSpace); i >= 0 {
		name, arg = trimmed[:i], strings.TrimSpace(trimmed[i:])
	}
	return isSessionCommand(name, arg)
}
