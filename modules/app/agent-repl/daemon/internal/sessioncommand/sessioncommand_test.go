package sessioncommand

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestSpecsAreReadOffTheEnumOption(t *testing.T) {
	// Arrange / Act
	table := Specs()
	// Assert: the literal and the takes_args fact come from the schema, and
	// nothing here is hand-written.
	got, ok := table["/compact"]
	if !ok {
		t.Fatal("the table must carry /compact")
	}
	if got.Command != conversationv1.SessionCommand_SESSION_COMMAND_COMPACT || !got.TakesArgs {
		t.Fatalf("spec = %+v, want the compact command taking arguments", got)
	}
}

func TestSpecsCarryNoEntryForUnspecified(t *testing.T) {
	// Arrange / Act
	table := Specs()
	// Assert
	for literal, s := range table {
		if s.Command == conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
			t.Fatalf("literal %q maps to UNSPECIFIED, which names no command", literal)
		}
	}
}

func TestParse(t *testing.T) {
	tests := []struct {
		name  string
		text  string
		want  Parsed
		known conversationv1.SessionCommand
	}{
		{name: "text that is not a command", text: "compact the notes", want: Parsed{}},
		{name: "a bare known command", text: "/clear", want: Parsed{Slash: true, Name: "/clear", Known: true},
			known: conversationv1.SessionCommand_SESSION_COMMAND_CLEAR},
		{name: "a known command with an argument", text: "/compact keep the plan",
			want:  Parsed{Slash: true, Name: "/compact", Arg: "keep the plan", Known: true},
			known: conversationv1.SessionCommand_SESSION_COMMAND_COMPACT},
		{name: "surrounding whitespace is trimmed", text: "  /clear  ", want: Parsed{Slash: true, Name: "/clear", Known: true},
			known: conversationv1.SessionCommand_SESSION_COMMAND_CLEAR},
		{name: "an unknown command", text: "/deploy-everything now", want: Parsed{Slash: true, Name: "/deploy-everything", Arg: "now"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Parse(tt.text)
			// Assert
			if got.Slash != tt.want.Slash || got.Name != tt.want.Name || got.Arg != tt.want.Arg || got.Known != tt.want.Known {
				t.Fatalf("Parse(%q) = %+v, want %+v", tt.text, got, tt.want)
			}
			if got.Spec.Command != tt.known {
				t.Fatalf("Parse(%q) command = %s, want %s", tt.text, got.Spec.Command, tt.known)
			}
		})
	}
}

func TestContextCut(t *testing.T) {
	tests := []struct {
		name    string
		text    string
		command conversationv1.SessionCommand
		arg     string
		ok      bool
	}{
		{name: "a bare /compact", text: "/compact", command: conversationv1.SessionCommand_SESSION_COMMAND_COMPACT, ok: true},
		{name: "/compact with instructions", text: "/compact foo bar", command: conversationv1.SessionCommand_SESSION_COMMAND_COMPACT, arg: "foo bar", ok: true},
		{name: "a bare /clear", text: "/clear", command: conversationv1.SessionCommand_SESSION_COMMAND_CLEAR, ok: true},
		{name: "/clear with trailing text, which the schema says it does not take", text: "/clear the table"},
		{name: "a longer word that begins with the literal", text: "/compacting"},
		{name: "the literal not at the start", text: "please /compact"},
		{name: "another act command", text: "/model opus"},
		{name: "ordinary text", text: "compact the notes"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			command, arg, ok := ContextCut(tt.text)
			// Assert
			if ok != tt.ok || command != tt.command || arg != tt.arg {
				t.Fatalf("ContextCut(%q) = (%s, %q, %v), want (%s, %q, %v)", tt.text, command, arg, ok, tt.command, tt.arg, tt.ok)
			}
		})
	}
}
