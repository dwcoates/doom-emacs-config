package protocmd

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// The concrete values every other consumer will key off. They are asserted
// literally on purpose: the point of the schema move is that these strings
// exist in exactly one place, and a test that recomputed them from the same
// descriptor would agree with any spelling at all.
func TestSessionCommandSpecsConcreteValues(t *testing.T) {
	tests := []struct {
		name      string
		command   frontendv1.SessionCommand
		literal   string
		takesArgs bool
	}{
		{
			name:      "model takes an inline argument",
			command:   frontendv1.SessionCommand_SESSION_COMMAND_MODEL,
			literal:   "/model",
			takesArgs: true,
		},
		{
			// The exactness matters most here: mistaking "/clear the build
			// cache" for the command that DISCARDS THE CONVERSATION would
			// destroy the context the user was speaking into.
			name:      "clear takes none",
			command:   frontendv1.SessionCommand_SESSION_COMMAND_CLEAR,
			literal:   "/clear",
			takesArgs: false,
		},
		{
			name:      "compact steers its summary",
			command:   frontendv1.SessionCommand_SESSION_COMMAND_COMPACT,
			literal:   "/compact",
			takesArgs: true,
		},
		{
			name:      "hyphenated literals survive the descriptor round trip",
			command:   frontendv1.SessionCommand_SESSION_COMMAND_OUTPUT_STYLE,
			literal:   "/output-style",
			takesArgs: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			specs := SessionCommandSpecs()

			// Act.
			spec, ok := specs[tc.command]

			// Assert.
			if !ok {
				t.Fatalf("no spec for %v", tc.command)
			}
			if spec.Literal != tc.literal {
				t.Errorf("literal = %q, want %q", spec.Literal, tc.literal)
			}
			if spec.TakesArgs != tc.takesArgs {
				t.Errorf("takesArgs = %v, want %v", spec.TakesArgs, tc.takesArgs)
			}
		})
	}
}

// Every command the wire can name must be spellable. A value without a spec
// is one the recognizer can never match and the webapp can never label, which
// is a silently missing command rather than a loud failure.
func TestSessionCommandSpecsCoverEveryNamedCommand(t *testing.T) {
	// Arrange.
	values := frontendv1.SessionCommand(0).Descriptor().Values()

	// Act.
	specs := SessionCommandSpecs()

	// Assert.
	for i := 0; i < values.Len(); i++ {
		value := values.Get(i)
		command := frontendv1.SessionCommand(value.Number())
		if command == frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
			continue
		}
		spec, ok := specs[command]
		if !ok {
			t.Errorf("%s carries no spec", value.FullName())
			continue
		}
		if spec.Literal == "" {
			t.Errorf("%s carries an empty literal", value.FullName())
		}
	}
}

// UNSPECIFIED names no command, so it must not appear as one: a caller
// iterating the map would otherwise have to know to skip a sentinel, and one
// that forgot would offer an unnamed command to the user.
func TestSessionCommandSpecsOmitUnspecified(t *testing.T) {
	// Arrange.
	specs := SessionCommandSpecs()

	// Act.
	_, ok := specs[frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED]

	// Assert.
	if ok {
		t.Error("SESSION_COMMAND_UNSPECIFIED carries a spec; it names no command")
	}
}

// The map size is the enum's size minus the one unnamed value, which catches a
// value added to the schema without a spec even if nothing yet reads it.
func TestSessionCommandSpecsCountMatchesEnum(t *testing.T) {
	// Arrange.
	values := frontendv1.SessionCommand(0).Descriptor().Values()

	// Act.
	specs := SessionCommandSpecs()

	// Assert.
	if want := values.Len() - 1; len(specs) != want {
		t.Errorf("len(specs) = %d, want %d", len(specs), want)
	}
}

// The marker literal is the single value five other sites used to restate.
func TestSyntheticModelLiteral(t *testing.T) {
	// Arrange, Act.
	literal := SyntheticModelLiteral()

	// Assert.
	if literal != "<synthetic>" {
		t.Errorf("SyntheticModelLiteral() = %q, want %q", literal, "<synthetic>")
	}
}
