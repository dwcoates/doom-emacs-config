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
			cut, err := tc.mode.cut()

			// Assert
			if err != nil {
				t.Fatalf("cut for %s = %v, want a command", tc.mode, err)
			}
			if !strings.HasPrefix(cut.text, compactCommandText+" ") {
				t.Fatalf("cut text for %s = %q, want it steered from %q", tc.mode, cut.text, compactCommandText)
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
			cut, err := tc.mode.cut()
			if err != nil {
				t.Fatalf("cut for %s: %v", tc.mode, err)
			}

			// Act
			got := lookupSessionCommand(cut.text)

			// Assert
			if got.String() != "SESSION_COMMAND_COMPACT" {
				t.Fatalf("lookupSessionCommand(%q) = %s, want SESSION_COMMAND_COMPACT", cut.text, got)
			}
		})
	}
}

// THE UNSCOPED MODE SUBMITS THE BARE COMMAND, with no instructions at all: a
// whole-conversation compaction has nothing to preserve, and steering it would
// only narrow what the user asked to be rid of.
func TestCompactAllSubmitsTheBareCompactCommand(t *testing.T) {
	// Arrange / Act
	cut, err := ReviveModeCompactAll.cut()

	// Assert
	if err != nil {
		t.Fatalf("cut for compact_all = %v, want a command", err)
	}
	if cut.text != compactCommandText {
		t.Fatalf("cut text for compact_all = %q, want exactly %q", cut.text, compactCommandText)
	}
}

// A NON-CUTTING MODE HAS NO CUT, and asking for one is a routing failure that
// fails hard rather than defaulting to the compaction that discards the most.
func TestCutRefusesANonCuttingMode(t *testing.T) {
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
			cut, err := tc.mode.cut()

			// Assert
			if err == nil {
				t.Fatalf("cut for %s = %q, want a refusal", tc.mode, cut.text)
			}
			if cut.text != "" {
				t.Fatalf("cut for %s returned %q alongside its error", tc.mode, cut.text)
			}
		})
	}
}

// `cuts` IS THE GATED PATH'S ONLY MEMBERSHIP TEST, so a new scope — or a new
// cut — is on it by construction rather than by being added to a list.
func TestCutsCoversEveryModeButDirectAndUnset(t *testing.T) {
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
		{"clear", ReviveModeClear, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.mode.cuts(); got != tc.want {
				t.Fatalf("%s.cuts() = %v, want %v", tc.mode, got, tc.want)
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
		ReviveModeClear,
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

// THE CLEAR CUT SUBMITS `/clear`, WITH NO ARGUMENT. sessioncommand.go
// recognizes the command only as the ENTIRE prompt — deliberately, because
// mistaking a sentence for it would discard the conversation — so a clear
// revival that steered its text the way the scoped compactions do would lose
// the bubble suppression and, worse, stop being the command at all.
func TestClearCutSubmitsTheBareClearSessionCommand(t *testing.T) {
	// Arrange / Act
	cut, err := ReviveModeClear.cut()

	// Assert
	if err != nil {
		t.Fatalf("cut for clear = %v, want a command", err)
	}
	if cut.text != clearCommandText {
		t.Fatalf("cut text for clear = %q, want exactly %q", cut.text, clearCommandText)
	}
	if got := lookupSessionCommand(cut.text); got.String() != "SESSION_COMMAND_CLEAR" {
		t.Fatalf("lookupSessionCommand(%q) = %s, want SESSION_COMMAND_CLEAR", cut.text, got)
	}
}

// ONLY THE COMPACTIONS TAKE THE COLD-READ CLAIM. `/clear` is not a model call,
// so a claim over it would hand the NEXT turn's input cost to a turn that read
// nothing — the exact misattribution the claim exists to prevent.
func TestOnlyCompactingCutsClaimTheColdReadAlarm(t *testing.T) {
	tests := []struct {
		name string
		mode ReviveMode
		want bool
	}{
		{"all", ReviveModeCompactAll, true},
		{"responses", ReviveModeCompactResponses, true},
		{"prompts", ReviveModeCompactPrompts, true},
		{"prompts and responses", ReviveModeCompactPromptsAndResponses, true},
		{"clear", ReviveModeClear, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cut, err := tc.mode.cut()
			if err != nil {
				t.Fatalf("cut for %s: %v", tc.mode, err)
			}
			if cut.claimsCompaction != tc.want {
				t.Fatalf("%s claimsCompaction = %v, want %v", tc.mode, cut.claimsCompaction, tc.want)
			}
		})
	}
}

// THE TWO CUTS HAVE SEPARATE WAITER SLOTS, so a compaction closing its axis can
// never release a revival that asked for a clear, or the reverse. Sharing one
// slot would make either cut a valid answer to the other's question.
func TestCompactionAndClearWaitOnSeparateSlots(t *testing.T) {
	// Arrange
	c := &consumer{}

	// Act
	compact, err := ReviveModeCompactAll.cut()
	if err != nil {
		t.Fatalf("cut for compact_all: %v", err)
	}
	clear, err := ReviveModeClear.cut()
	if err != nil {
		t.Fatalf("cut for clear: %v", err)
	}

	// Assert
	if compact.waiter(c) == clear.waiter(c) {
		t.Fatal("the compaction and the clear resolve to the SAME waiter slot; either cut would then release a revival that asked for the other")
	}
}
