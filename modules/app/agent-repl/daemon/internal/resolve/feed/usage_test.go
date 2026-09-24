package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE STAMP IS THIS TURN'S OWN WORK, not the context window. Usage rides
// exactly one unit per API response — the unit for its FIRST content block — so
// the prose bubble's stamp comes from a sibling unit far more often than from
// its own envelope, and it SUMS across every API response of the turn while
// EXCLUDING the two cached-context buckets (cache_read, cache_creation).

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

// misses builds a usage whose two cache-miss buckets are set (written =
// cache_creation, unwritten = fresh input_tokens); no output, no cache hits.
func misses(written, unwritten uint64) *conversationv1.TokenUsage {
	return &conversationv1.TokenUsage{
		InputMisses: &conversationv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
	}
}

// turnUsage builds a full usage: fresh input (input_tokens), cache_creation
// (written), cache_read (read), and output. It lets a test set the excluded
// cached-context buckets independently of the counted turn tokens.
func turnUsage(freshInput, cacheCreation, cacheRead, output uint64) *conversationv1.TokenUsage {
	return &conversationv1.TokenUsage{
		InputMisses:  &conversationv1.TokenCacheMisses{Written: cacheCreation, Unwritten: freshInput},
		InputHits:    &conversationv1.TokenCacheHits{Read: cacheRead},
		OutputTokens: output,
	}
}

// proseBubbles returns the prose (non-thinking) response bubbles on the root
// feed, skipping the user-prompt row a delivered turn adds and the thinking
// units that carry usage but draw no bubble.
func (h *harness) proseBubbles() []*frontendv1.FeedResponse {
	h.t.Helper()
	var out []*frontendv1.FeedResponse
	for _, row := range h.responseRows() {
		if resp := row.GetActivity().GetResponse(); resp != nil && !resp.GetThinking() {
			out = append(out, resp)
		}
	}
	return out
}

// soleProse returns the single prose response bubble, failing when there is not
// exactly one.
func (h *harness) soleProse() *frontendv1.FeedResponse {
	h.t.Helper()
	rows := h.proseBubbles()
	if len(rows) != 1 {
		h.t.Fatalf("prose bubbles = %d, want exactly 1", len(rows))
	}
	return rows[0]
}

// proseForTurn returns the prose response bubble whose row is stamped with the
// given turn, failing when none is.
func (h *harness) proseForTurn(turn string) *frontendv1.FeedResponse {
	h.t.Helper()
	for _, row := range h.responseRows() {
		resp := row.GetActivity().GetResponse()
		if resp == nil || resp.GetThinking() {
			continue
		}
		if row.GetTurn().GetValue() == turn {
			return resp
		}
	}
	h.t.Fatalf("no prose bubble stamped with turn %q", turn)
	return nil
}

func TestSingleResponseTurnStampsFreshInputPlusOutput(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: one API response — fresh input 240 + output 500, no cached context.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking", turnUsage(240, 0, 0, 500)), nil, noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "an answer"},
		}, nil), nil, noAddress())

	// Assert: the bubble stamps input_tokens + output_tokens (740), not
	// cache_creation and not a per-response context-window sum.
	if got := h.soleProse().GetUsage().GetText(); got != "740" {
		t.Fatalf("usage stamp = %q, want the turn's 240+500 = 740", got)
	}
}

func TestMultiResponseTurnSumsTokensAcrossTheTurn(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: a tool-use loop yields TWO API responses in the one turn.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-think-1", turnUsage(200, 0, 0, 300)), nil, noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "first"},
		}, nil), nil, noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-think-2", turnUsage(100, 0, 0, 400)), nil, noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose-2", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "second"},
		}, nil), nil, noAddress())

	// Assert: every bubble in the turn shows the TURN TOTAL (200+300+100+400 =
	// 1000), summed across both API responses.
	bubbles := h.proseBubbles()
	if len(bubbles) != 2 {
		t.Fatalf("prose bubbles = %d, want the two prose bubbles", len(bubbles))
	}
	for _, bubble := range bubbles {
		if got := bubble.GetUsage().GetText(); got != "1k" {
			t.Fatalf("usage stamp = %q, want the turn total 1k on every bubble", got)
		}
	}
}

func TestCacheReadAndCreationAreExcludedFromTheTurnStamp(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: one API response over a HUGE cached context — 99k cache_read and
	// 18k cache_creation — but only 240 fresh input and 260 output.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking", turnUsage(240, 18_000, 99_000, 260)), nil, noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "an answer"},
		}, nil), nil, noAddress())

	// Assert: the bubble shows the SMALL turn figure (240+260 = 500), not the
	// ~117k context window that cache_read + cache_creation would sum to.
	if got := h.soleProse().GetUsage().GetText(); got != "500" {
		t.Fatalf("usage stamp = %q, want the turn's 500 with cached context excluded", got)
	}
}

func TestANewTurnResetsTheTurnTokenSum(t *testing.T) {
	// Arrange: a first turn with a large bill, ended.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "first")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-t1", turnUsage(4_000, 0, 0, 5_000)), nil, noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-p1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "first answer"},
		}, nil), nil, noAddress())
	h.terminal("turn-1", &conversationv1.AgentSuccess{}, nil)

	// Act: a SECOND turn with a small bill of its own.
	h.deliverPrompt("turn-2", "second")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-t2", turnUsage(100, 0, 0, 200)), nil, noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-p2", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "second answer"},
		}, nil), nil, noAddress())

	// Assert: turn 2's bubble stamps only turn 2 (100+200 = 300); it does not
	// carry turn 1's 9k.
	if got := h.proseForTurn("turn-2").GetUsage().GetText(); got != "300" {
		t.Fatalf("usage stamp = %q, want turn 2's own 300, reset from turn 1", got)
	}
	// And turn 1's own bubble keeps turn 1's total (4_000+5_000 = 9k), never
	// summed with turn 2.
	if got := h.proseForTurn("turn-1").GetUsage().GetText(); got != "9k" {
		t.Fatalf("turn 1 usage stamp = %q, want turn 1's own 9k", got)
	}
}

func TestUsageRidesTheThinkingUnitAndTheProseBubbleReadsTheTurnTotal(t *testing.T) {
	// Arrange: a turn is running; the response's FIRST content block is the
	// thinking unit, and it is the one carrying the usage.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking", turnUsage(240, 0, 0, 260)), nil, noAddress())

	// Act: the prose unit of that same API response carries no usage at all.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "an answer"},
		}, nil), nil, noAddress())

	// Assert: the prose bubble reads the sibling thinking unit's usage as the
	// turn total (240+260 = 500).
	if got := h.soleProse().GetUsage().GetText(); got != "500" {
		t.Fatalf("usage stamp = %q, want the sibling's turn total 500", got)
	}
}

func TestAturnWithNoStatedUsageDrawsNoStamp(t *testing.T) {
	// Arrange, Act: a turn runs but nothing in it ever states a figure.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking", nil), nil, noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-prose", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "an answer"},
		}, nil), nil, noAddress())

	// Assert: absence draws no stamp, never an invented zero.
	if usage := h.soleProse().GetUsage(); usage != nil {
		t.Fatalf("usage = %+v, want unset", usage)
	}
}

func TestEveryProseUnitOfOneApiResponseStampsTheTurnTotalOnce(t *testing.T) {
	// Arrange: a turn running; one API response yielding SEVERAL units — the
	// thinking block that carries the usage, then two prose blocks.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingFrame("unit-thinking", turnUsage(4_000, 0, 0, 100)), nil, noAddress())
	for _, unit := range []string{"unit-prose-1", "unit-prose-2"} {
		h.resolver.OnActivity(testWorkspace, mainAgent(),
			responseFrame(unit, &conversationv1.AgentResponseSuccess{
				Prose: &conversationv1.AgentResponseProse{Markdown: unit},
			}, nil), nil, noAddress())
	}

	// Assert: each bubble states the turn total once (4000+100 = 4.1k); a
	// second prose unit of the SAME response does not double the charge.
	bubbles := h.proseBubbles()
	if len(bubbles) != 2 {
		t.Fatalf("prose bubbles = %d, want the two prose bubbles", len(bubbles))
	}
	for _, bubble := range bubbles {
		if got := bubble.GetUsage().GetText(); got != "4.1k" {
			t.Fatalf("usage stamp = %q, want the turn total 4.1k on every bubble", got)
		}
	}
}
