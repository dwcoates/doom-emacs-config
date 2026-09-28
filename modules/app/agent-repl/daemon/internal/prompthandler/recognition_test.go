package prompthandler

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/promptqueue"
	"claude-repld/internal/sessioncommand"
)

func TestRecognizeReportsTheFourPanels(t *testing.T) {
	for _, literal := range []string{"/status", "/todos", "/mcp", "/context"} {
		t.Run(literal, func(t *testing.T) {
			// Arrange / Act
			got := recognize(literal)
			// Assert
			if got.kind != RecognizedPanel {
				t.Fatalf("%s = %s, want a panel", literal, recognitionName(got.kind))
			}
		})
	}
}

func TestRecognizeRefusesAgentsAndHelp(t *testing.T) {
	for _, literal := range []string{"/agents", "/help"} {
		t.Run(literal, func(t *testing.T) {
			// Arrange / Act
			got := recognize(literal)
			// Assert
			if got.kind != RecognizedRefused {
				t.Fatalf("%s = %s, want a refusal", literal, recognitionName(got.kind))
			}
		})
	}
}

// TestRecognizeFallsAnUnnamedCommandThroughToTheVendor covers the closed set's
// edge: `command_refused` is for a command the daemon RECOGNIZES and neither
// answers nor forwards, and endpoint_submit_prompt.proto says an unrecognized
// command falls through to the vendor like any other text. Refusing here would
// suppress every vendor and user-authored slash command the enum lacks.
func TestRecognizeFallsAnUnnamedCommandThroughToTheVendor(t *testing.T) {
	// Arrange / Act
	got := recognize("/deploy-everything")
	// Assert
	if got.kind != RecognizedNone {
		t.Fatalf("recognition = %s, want it forwarded as an ordinary prompt", recognitionName(got.kind))
	}
}

// TestRecognizeForwardsTheCommandsTheEnumNamesOnlyForTheSidecar pins that
// /effort, /plugin and /low-priority -- named by SessionCommand so the sidecar
// classifies their transcript envelopes, and forwarded to the vendor as
// ordinary text before the enum named them -- are STILL forwarded, bare or
// with an argument, never refused.
func TestRecognizeForwardsTheCommandsTheEnumNamesOnlyForTheSidecar(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "/effort bare", text: "/effort"},
		{name: "/effort with an argument", text: "/effort high"},
		{name: "/plugin bare", text: "/plugin"},
		{name: "/plugin with an argument", text: "/plugin install foo"},
		{name: "/low-priority bare", text: "/low-priority"},
		{name: "/low-priority with trailing text", text: "/low-priority please"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got := recognize(tc.text)
			// Assert
			if got.kind != RecognizedNone {
				t.Fatalf("recognize(%q) = %s, want it forwarded as an ordinary prompt", tc.text, recognitionName(got.kind))
			}
		})
	}
}

// TestForwardedCommandsAreNamedByTheEnum pins that every forwarded command is
// one the SessionCommand table recognizes: an entry the enum does not name
// would be dead, since an unnamed command is forwarded anyway.
func TestForwardedCommandsAreNamedByTheEnum(t *testing.T) {
	for command := range ForwardedCommands {
		t.Run(command.String(), func(t *testing.T) {
			// Arrange
			found := false
			// Act
			for _, spec := range sessioncommand.Specs() {
				if spec.Command == command {
					found = true
				}
			}
			// Assert
			if !found {
				t.Fatalf("%s is forwarded but the SessionCommand table does not name it", command)
			}
		})
	}
}

func TestRecognizeRefusesBareModel(t *testing.T) {
	// Arrange / Act
	got := recognize("/model")
	// Assert: the vendor's own picker is unreachable through us.
	if got.kind != RecognizedRefused {
		t.Fatalf("recognition = %s, want a refusal", recognitionName(got.kind))
	}
}

func TestRecognizeMakesAnActOfModelWithAnArgument(t *testing.T) {
	// Arrange / Act
	got := recognize("/model opus")
	// Assert
	if got.kind != RecognizedAct {
		t.Fatalf("recognition = %s, want a session act", recognitionName(got.kind))
	}
	if got.arg != "opus" {
		t.Fatalf("arg = %q, want the model id", got.arg)
	}
}

func TestRecognizeMakesActsOfTheContextCuts(t *testing.T) {
	tests := []struct {
		text string
		kind string
	}{
		{"/clear", promptqueue.ActClear},
		{"/compact", promptqueue.ActCompact},
		{"/clear foo", promptqueue.ActClear},
		{"/reset", promptqueue.ActClear},
		{"/new", promptqueue.ActClear},
		{"/reset foo", promptqueue.ActClear},
	}
	for _, tc := range tests {
		t.Run(tc.text, func(t *testing.T) {
			// Arrange / Act
			got := recognize(tc.text)
			// Assert
			if got.kind != RecognizedAct {
				t.Fatalf("recognition = %s, want a session act", recognitionName(got.kind))
			}
			if ActCommands[got.spec.Command] != tc.kind {
				t.Fatalf("act kind = %q, want %q", ActCommands[got.spec.Command], tc.kind)
			}
		})
	}
}

func TestRecognizeLeavesOrdinaryProseAlone(t *testing.T) {
	// Arrange / Act
	got := recognize("please fix the failing test")
	// Assert
	if got.kind != RecognizedNone {
		t.Fatalf("recognition = %s, want an ordinary prompt", recognitionName(got.kind))
	}
}

func TestRecognizeKeepsAProseTailOnAnArgumentLessCommand(t *testing.T) {
	// Arrange / Act: FALSE IS THE SAFE SIDE — "/status of the build" is a
	// prompt the user meant, and a suppressed prompt is never recovered.
	got := recognize("/status of the build")
	// Assert
	if got.kind != RecognizedNone {
		t.Fatalf("recognition = %s, want an ordinary prompt", recognitionName(got.kind))
	}
}

func TestRecognizeAcceptsAnArgumentOnACommandThatTakesOne(t *testing.T) {
	// Arrange / Act
	got := recognize("/compact focus on the tests")
	// Assert
	if got.kind != RecognizedAct {
		t.Fatalf("recognition = %s, want a session act", recognitionName(got.kind))
	}
	if got.arg != "focus on the tests" {
		t.Fatalf("arg = %q, want the command's argument", got.arg)
	}
}

// TestRecognizeReadsAnArgumentAfterANewline pins the vendor's split: the CLI
// ends a command's name at the first whitespace of any kind, so a /compact
// whose instructions start on the next line is still a compaction.
func TestRecognizeReadsAnArgumentAfterANewline(t *testing.T) {
	// Arrange / Act
	got := recognize("/compact\nfocus on the tests")
	// Assert
	if got.kind != RecognizedAct || got.arg != "focus on the tests" {
		t.Fatalf("recognition = %s arg = %q, want a session act carrying its argument", recognitionName(got.kind), got.arg)
	}
}

func TestRecognizeIgnoresSurroundingWhitespace(t *testing.T) {
	// Arrange / Act
	got := recognize("  /status  ")
	// Assert
	if got.kind != RecognizedPanel {
		t.Fatalf("recognition = %s, want a panel", recognitionName(got.kind))
	}
}

func TestRecognizeIsExposedOnTheHandler(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	kind, literal := h.h.Recognize("/help")
	// Assert
	if kind != RecognizedRefused || literal != "/help" {
		t.Fatalf("Recognize = (%s, %q), want the refusal and the literal", recognitionName(kind), literal)
	}
}

func TestPanelCommandsAreExactlyTheFeedsPanelArms(t *testing.T) {
	// Arrange: the feed can draw four panels, so the daemon claims exactly
	// those four.
	want := map[conversationv1.SessionCommand]bool{
		conversationv1.SessionCommand_SESSION_COMMAND_STATUS:  true,
		conversationv1.SessionCommand_SESSION_COMMAND_TODOS:   true,
		conversationv1.SessionCommand_SESSION_COMMAND_MCP:     true,
		conversationv1.SessionCommand_SESSION_COMMAND_CONTEXT: true,
	}
	// Act / Assert
	if len(PanelCommands) != len(want) {
		t.Fatalf("panel commands = %v, want exactly the feed's four arms", PanelCommands)
	}
	for command := range want {
		if !PanelCommands[command] {
			t.Fatalf("%s must be a panel command", command)
		}
	}
}

func TestRecognizeLeavesANearMissOfAClearAliasAPrompt(t *testing.T) {
	for _, text := range []string{"/newer", "/resetting", "please /new"} {
		t.Run(text, func(t *testing.T) {
			// Arrange / Act
			got := recognize(text)
			// Assert
			if got.kind != RecognizedNone {
				t.Fatalf("recognition = %s, want an ordinary prompt", recognitionName(got.kind))
			}
		})
	}
}
