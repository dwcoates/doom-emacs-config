package sessioncontroller

import (
	"context"
	"strings"
	"testing"

	"claude-repld/internal/registry"
)

// THE SCOPED REVIVAL MODES: one submitted `/compact` per mode, differing only
// in the instructions that say what the summary must leave alone.
//
// WHAT THESE TESTS PROTECT is the one thing the user can neither see nor undo:
// a scoped revival that submitted the bare `/compact` would summarize away the
// prompts, the responses, and the tool results alike while the gate said only
// the responses were going.

// scopedModeCases pairs each compacting mode with the phrase its submitted
// instruction must carry.
var scopedModeCases = []struct {
	name string
	mode ReviveMode
	// wants are substrings the submitted prompt must contain.
	wants []string
}{
	{
		name:  "responses only",
		mode:  ReviveModeCompactResponses,
		wants: []string{"Summarize ONLY the assistant's own response messages", "Preserve every user prompt"},
	},
	{
		name:  "prompts only",
		mode:  ReviveModeCompactPrompts,
		wants: []string{"Summarize ONLY the user's prompt messages", "Preserve every assistant response"},
	},
	{
		name: "prompts and responses",
		mode: ReviveModeCompactPromptsAndResponses,
		wants: []string{
			"Summarize ONLY the user's prompt messages and the assistant's own response messages",
			"Preserve every tool call and every tool result verbatim",
		},
	},
}

// EVERY SCOPED MODE IS STILL A `/compact`, so the CLI runs the compaction it
// already knows how to run and sessioncommand.go still suppresses the bubble.
func TestScopedCompactCommandsAreStillTheCompactSessionCommand(t *testing.T) {
	for _, tc := range scopedModeCases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			text, err := tc.mode.compactCommand()

			// Assert
			if err != nil {
				t.Fatalf("compactCommand for %s = %v, want a command", tc.mode, err)
			}
			if !strings.HasPrefix(text, compactCommandText+" ") {
				t.Fatalf("compactCommand for %s = %q, want it steered from %q", tc.mode, text, compactCommandText)
			}
		})
	}
}

// AND IS RECOGNIZED AS ONE. `/compact <instructions>` earns no prompt bubble
// only because the session-command table admits an argument for it; a scoped
// compaction that fell out of that table would echo the daemon's instructions
// into the feed as though the user had typed them.
func TestScopedCompactCommandsAreRecognizedAsSessionCommands(t *testing.T) {
	for _, tc := range scopedModeCases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			text, err := tc.mode.compactCommand()
			if err != nil {
				t.Fatalf("compactCommand for %s: %v", tc.mode, err)
			}

			// Act
			got := lookupSessionCommand(text)

			// Assert
			if got.String() != "SESSION_COMMAND_COMPACT" {
				t.Fatalf("lookupSessionCommand(%q) = %s, want SESSION_COMMAND_COMPACT", text, got)
			}
		})
	}
}

// THE UNSCOPED MODE SUBMITS THE BARE COMMAND, with no instructions at all: a
// whole-conversation compaction has nothing to preserve, and steering it would
// only narrow what the user asked to be rid of.
func TestCompactAllSubmitsTheBareCompactCommand(t *testing.T) {
	// Arrange / Act
	text, err := ReviveModeCompactAll.compactCommand()

	// Assert
	if err != nil {
		t.Fatalf("compactCommand for compact_all = %v, want a command", err)
	}
	if text != compactCommandText {
		t.Fatalf("compactCommand for compact_all = %q, want exactly %q", text, compactCommandText)
	}
}

// A NON-COMPACTING MODE HAS NO COMPACTION COMMAND, and asking for one is a
// routing failure that fails hard rather than defaulting to the compaction that
// discards the most.
func TestCompactCommandRefusesANonCompactingMode(t *testing.T) {
	tests := []struct {
		name string
		mode ReviveMode
	}{
		{"direct", ReviveModeDirect},
		{"unset", ReviveModeUnset},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			text, err := tc.mode.compactCommand()

			// Assert
			if err == nil {
				t.Fatalf("compactCommand for %s = %q, want a refusal", tc.mode, text)
			}
			if text != "" {
				t.Fatalf("compactCommand for %s returned %q alongside its error", tc.mode, text)
			}
		})
	}
}

// `compacts` IS THE COMPACTION PATH'S ONLY MEMBERSHIP TEST, so a new scope is
// on it by construction rather than by being added to a list.
func TestCompactsCoversEveryModeButDirectAndUnset(t *testing.T) {
	tests := []struct {
		name string
		mode ReviveMode
		want bool
	}{
		{"unset", ReviveModeUnset, false},
		{"direct", ReviveModeDirect, false},
		{"all", ReviveModeCompactAll, true},
		{"responses", ReviveModeCompactResponses, true},
		{"prompts", ReviveModeCompactPrompts, true},
		{"prompts and responses", ReviveModeCompactPromptsAndResponses, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.mode.compacts(); got != tc.want {
				t.Fatalf("%s.compacts() = %v, want %v", tc.mode, got, tc.want)
			}
		})
	}
}

// EVERY MODE NAMES ITSELF DISTINCTLY IN THE LOG. The revival log line is the
// only durable record of which scope a session was compacted under, so two
// modes sharing a name would make an over-compacted conversation unattributable.
func TestReviveModeNamesAreDistinct(t *testing.T) {
	// Arrange
	modes := []ReviveMode{
		ReviveModeUnset,
		ReviveModeDirect,
		ReviveModeCompactAll,
		ReviveModeCompactResponses,
		ReviveModeCompactPrompts,
		ReviveModeCompactPromptsAndResponses,
	}

	// Act
	seen := map[string]ReviveMode{}

	// Assert
	for _, mode := range modes {
		name := mode.String()
		if name == "" {
			t.Fatalf("mode %d has no name", int(mode))
		}
		if prior, dup := seen[name]; dup {
			t.Fatalf("modes %d and %d both name themselves %q", int(prior), int(mode), name)
		}
		seen[name] = mode
	}
}

// THE SCOPE REACHES THE VENDOR. Everything above is about the text; this is the
// test that the revival actually submits it.
func TestReviveSessionSubmitsTheScopedCompactionText(t *testing.T) {
	for _, tc := range scopedModeCases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)

			// Act
			if err := m.ReviveSession(context.Background(), "ws", tc.mode); err != nil {
				t.Fatalf("ReviveSession %s: %v", tc.mode, err)
			}
			awaitCompactionWaiter(t, m, "ws")

			// Assert
			c := fakeClientFor(t, m, "ws")
			c.mu.Lock()
			defer c.mu.Unlock()
			if len(c.prompts) != 1 {
				t.Fatalf("submitted prompts = %v, want exactly one compaction", c.prompts)
			}
			for _, want := range tc.wants {
				if !strings.Contains(c.prompts[0], want) {
					t.Fatalf("submitted compaction = %q, want it to carry %q", c.prompts[0], want)
				}
			}
		})
	}
}

// A SCOPED REVIVAL IS GATED EXACTLY AS THE UNSCOPED ONE IS. The scope changes
// what the compaction summarizes and nothing about when prompts are admitted,
// so the record must still say hibernated while the compaction runs.
func TestReviveSessionScopedStaysGatedUntilCompactionLands(t *testing.T) {
	for _, tc := range scopedModeCases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
			if err := m.ReviveSession(context.Background(), "ws", tc.mode); err != nil {
				t.Fatalf("ReviveSession %s: %v", tc.mode, err)
			}
			signal := awaitCompactionWaiter(t, m, "ws")
			if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
				t.Fatalf("hibernation detail = %+v while the %s compaction runs, want the session STILL gated", detail, tc.mode)
			}

			// Act
			signal()

			// Assert
			awaitGateReleased(t, hib, "s1")
		})
	}
}
