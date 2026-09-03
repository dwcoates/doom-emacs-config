package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// THE STAMP IS AN API RESPONSE'S FIGURES. Usage rides exactly one unit per API
// response — the unit for its FIRST content block — so the prose bubble's
// stamp comes from a sibling unit far more often than from its own envelope.

// thinkingFrame builds the withheld-thinking unit the vendor's observed
// `[thinking, text]` response opens with. It draws no row of its own; it is
// the unit that CARRIES the response's usage.
func thinkingFrame(unit string, usage *conversationv1.TokenUsage) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Usage:      usage,
		Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
			Result: &conversationv1.AgentThinking_Success{
				Success: &conversationv1.AgentThinkingSuccess{},
			},
		}},
	}
}

// misses builds a usage whose two expensive buckets sum to the drawn figure.
func misses(written, unwritten uint64) *conversationv1.TokenUsage {
	return &conversationv1.TokenUsage{
		InputMisses: &conversationv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
	}
}

func TestTheStampComesFromTheUnitThatOpenedTheApiResponse(t *testing.T) {
	// Arrange: the response's FIRST content block is the thinking unit, and it
	// is the one carrying the usage.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking", misses(18_000, 240)), noAddress())

	// Act: the prose unit of that same API response carries no usage at all.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "an answer"},
		}, nil), noAddress())

	// Assert: the bubble stamps its API response's figures.
	if got := h.response().GetUsage().GetText(); got != "18.2k" {
		t.Fatalf("usage stamp = %q, want the API response's 18.2k", got)
	}
}

func TestUnitsSeenBeforeAnyUsageDrawNoStamp(t *testing.T) {
	// Arrange, Act: nothing in this API response ever states a figure.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking", nil), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "an answer"},
		}, nil), noAddress())

	// Assert: absence draws no stamp, never an invented zero.
	if usage := h.response().GetUsage(); usage != nil {
		t.Fatalf("usage = %+v, want unset", usage)
	}
}

func TestEveryProseUnitOfOneApiResponseStampsThatResponsesFigures(t *testing.T) {
	// Arrange: one API response yielding SEVERAL units — the thinking block
	// that carries the usage, then two prose blocks.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking", misses(4_000, 100)), noAddress())
	for _, unit := range []string{"unit-prose-1", "unit-prose-2"} {
		h.resolver.OnActivity(testWorkspace, mainAgent(),
			responseFrame(unit, &conversationv1.AgentResponseSuccess{
				Prose: &conversationv1.AgentResponseProse{Markdown: unit},
			}, nil), noAddress())
	}

	// Assert: each bubble states the RESPONSE's figure once; nothing sums the
	// same charge per unit.
	rows := h.rows(rootFeed())
	if len(rows) != 2 {
		t.Fatalf("rows = %d, want the two prose bubbles", len(rows))
	}
	for _, row := range rows {
		if got := row.GetActivity().GetResponse().GetUsage().GetText(); got != "4.1k" {
			t.Fatalf("usage stamp = %q, want the API response's 4.1k on every bubble", got)
		}
	}
}

func TestTheNextApiResponsesUsageDoesNotRestampTheEarlierBubble(t *testing.T) {
	// Arrange: a first API response, drawn and stamped.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking-1", misses(2_100, 0)), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "first"},
		}, nil), noAddress())

	// Act: a SECOND API response with a much larger bill.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking-2", misses(50_000, 0)), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "!"}, nil),
		noAddress())

	// Assert: the first bubble keeps its own response's figure.
	if got := h.response().GetUsage().GetText(); got != "2.1k" {
		t.Fatalf("usage stamp = %q, want the bubble's own API response 2.1k", got)
	}
}
