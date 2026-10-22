package feed

import (
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// THE STAMP IS FRESH INPUT, ONE AGENT AT A TIME (usage.go). Usage rides exactly
// one unit per API response — the unit for its FIRST content block — so the
// prose bubble's stamp comes from a sibling unit far more often than from its
// own envelope. A bubble's stamp is its agent's fresh input since the agent's
// previous bubble landed, frozen when it lands; the final answer carries the
// turn's whole tally.

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

// main sends one activity under the main agent.
func (h *harness) main(act *conversationv1.AgentActivity) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(), act, nil, nil)
}

// settledProse is a settled prose frame carrying no usage of its own.
func settledProse(unit, markdown string) *conversationv1.AgentActivity {
	return responseFrame(unit, &conversationv1.AgentResponseSuccess{
		Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
	}, nil)
}

// stamps answers the usage stamp of every prose bubble on the root feed, in
// feed order.
func (h *harness) stamps() []string {
	h.t.Helper()
	var out []string
	for _, bubble := range h.proseBubbles() {
		out = append(out, bubble.GetUsage().GetText())
	}
	return out
}

// wantStamps fails unless the root feed's prose bubbles carry exactly WANT.
func (h *harness) wantStamps(want ...string) {
	h.t.Helper()
	got := h.stamps()
	if len(got) != len(want) {
		h.t.Fatalf("stamps = %q, want %q", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			h.t.Fatalf("stamps = %q, want %q", got, want)
		}
	}
}

func TestABubbleStampsItsAgentsFreshInput(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: one API response — 240 uncached, 18k cache writes, 900k cache
	// reads, 500 output — its usage riding the thinking unit.
	h.main(thinkingFrame("unit-thinking", turnUsage(240, 18_000, 900_000, 500)))
	h.main(settledProse("unit-prose", "an answer"))

	// Assert: fresh input is the cache writes plus the uncached input.
	h.wantStamps("18.2k")
}

func TestAnArrivingBubbleGrowsAsItsUsageIsRestated(t *testing.T) {
	// Arrange: a bubble opens stating its usage.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.main(responseFrame("unit-prose", &conversationv1.AgentResponseStart{}, misses(0, 1_000)))

	// Act: the same unit restates a larger usage.
	h.main(responseFrame("unit-prose", &conversationv1.AgentResponseUpdate{NewMarkdown: "hi"}, misses(0, 3_000)))

	// Assert: the restated figure replaces the first, never adds to it.
	h.wantStamps("3k")
}

func TestAnArrivingBubbleGrowsWhenUsageLandsOnASibling(t *testing.T) {
	// Arrange: a bubble opens with no usage yet.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.main(responseFrame("unit-prose", &conversationv1.AgentResponseStart{}, nil))

	// Act: the API response's usage lands on a sibling unit.
	h.main(thinkingFrame("unit-thinking", misses(2_000, 0)))

	// Assert: the still-arriving bubble is re-stamped from its agent's tally.
	h.wantStamps("2k")
}

func TestTheNextBubbleStampsOnlyTheFreshInputSinceThePreviousLanded(t *testing.T) {
	// Arrange: a first API response lands its bubble.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.main(thinkingFrame("unit-think-1", misses(1_000, 0)))
	h.main(settledProse("unit-prose-1", "looking"))

	// Act: a second API response of the same turn.
	h.main(thinkingFrame("unit-think-2", misses(400, 0)))
	h.main(settledProse("unit-prose-2", "found it"))

	// Assert: the bubbles partition the turn's 1.4k.
	h.wantStamps("1k", "400")
}

func TestALandedBubbleNeverMovesAgain(t *testing.T) {
	// Arrange: a bubble lands.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.main(thinkingFrame("unit-think-1", misses(1_000, 0)))
	h.main(settledProse("unit-prose-1", "looking"))

	// Act: a later API response's usage arrives.
	h.main(thinkingFrame("unit-think-2", misses(400, 0)))

	// Assert: the landed bubble keeps the figure it landed with.
	h.wantStamps("1k")
}

func TestASettleRestatedByTheOtherPlaneKeepsTheFrozenFigure(t *testing.T) {
	// Arrange: a bubble lands, then more usage arrives.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.main(thinkingFrame("unit-think-1", misses(1_000, 0)))
	h.main(settledProse("unit-prose-1", "looking"))
	h.main(thinkingFrame("unit-think-2", misses(400, 0)))

	// Act: the other store plane restates the same settle.
	h.main(settledProse("unit-prose-1", "looking"))

	// Assert: the first settle's figure stands.
	h.wantStamps("1k")
}

func TestTheFinalAnswerCarriesTheTurnsWholeFreshInput(t *testing.T) {
	// Arrange: two bubbles land in one turn.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.main(thinkingFrame("unit-think-1", misses(1_000, 0)))
	h.main(settledProse("unit-prose-1", "looking"))
	h.main(thinkingFrame("unit-think-2", misses(400, 0)))
	h.main(settledProse("unit-prose-2", "found it"))

	// Act: the turn concludes on the second bubble.
	turn := ids.TurnID("turn-1")
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &turn, completedWith("unit-prose-2"), nil, nil)

	// Assert: the green bubble carries the turn's 1.4k; the earlier one keeps
	// its delta.
	h.wantStamps("1k", "1.4k")
}

func TestANewTurnStartsTheMainTallyAgain(t *testing.T) {
	// Arrange: a first turn spends 1k.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "first")
	h.main(thinkingFrame("unit-think-1", misses(1_000, 0)))
	h.main(settledProse("unit-prose-1", "one"))

	// Act: a second turn spends 400.
	h.deliverPrompt("turn-2", "second")
	h.main(thinkingFrame("unit-think-2", misses(400, 0)))
	h.main(settledProse("unit-prose-2", "two"))

	// Assert: the second turn's bubble counts only its own turn.
	if got := h.proseForTurn("turn-2").GetUsage().GetText(); got != "400" {
		t.Fatalf("turn-2 stamp = %q, want only its own 400", got)
	}
}

func TestASubagentsUsageNeverReachesTheMainBubbles(t *testing.T) {
	// Arrange: a main bubble is arriving while a subagent runs.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	h.main(thinkingFrame("unit-think-1", misses(1_000, 0)))
	h.main(responseFrame("unit-prose-1", &conversationv1.AgentResponseStart{}, nil))

	// Act: the subagent spends 5k.
	h.resolver.OnActivity(testWorkspace, created, thinkingFrame("sub-think-1", misses(5_000, 0)), nil, nil)

	// Assert: the main bubble counts only the main agent's fresh input.
	h.wantStamps("1k")
}

func TestASubFeedsBubblesPartitionTheirSubagentsFreshInput(t *testing.T) {
	// Arrange: a subagent runs.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	sub := func(act *conversationv1.AgentActivity) {
		h.resolver.OnActivity(testWorkspace, created, act, nil, nil)
	}

	// Act: two of its API responses land two bubbles in its sub-feed.
	sub(thinkingFrame("sub-think-1", misses(300, 0)))
	sub(settledProse("sub-prose-1", "reading"))
	sub(thinkingFrame("sub-think-2", misses(200, 0)))
	sub(settledProse("sub-prose-2", "done"))

	// Assert: each bubble froze its own delta.
	var got []string
	for _, row := range h.rows(feedid.Feed{Agent: created}) {
		if resp := row.GetActivity().GetResponse(); resp != nil && !resp.GetThinking() {
			got = append(got, resp.GetUsage().GetText())
		}
	}
	if len(got) != 2 || got[0] != "300" || got[1] != "200" {
		t.Fatalf("sub-feed stamps = %q, want [300 200]", got)
	}
}

func TestARegressedFreshInputIsRecordedAndTheTallyTakesTheRestatement(t *testing.T) {
	// Arrange: a unit states 3k.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.main(responseFrame("unit-prose", &conversationv1.AgentResponseStart{}, misses(0, 3_000)))

	// Act: the same unit restates a smaller figure.
	h.main(responseFrame("unit-prose", &conversationv1.AgentResponseUpdate{NewMarkdown: "hi"}, misses(0, 1_000)))

	// Assert: the contradiction is recorded with its figures, and the tally
	// takes the restated one.
	var found *dlog.Record
	for _, rec := range h.records() {
		if rec.Level == dlog.LevelError && rec.Operation == "daemon.feed.fresh_input_regressed" {
			rec := rec
			found = &rec
		}
	}
	if found == nil {
		t.Fatalf("records = %+v, want ERROR daemon.feed.fresh_input_regressed", h.records())
	}
	if found.Context["unit"] != "unit-prose" || found.Context["previous"] != uint64(3_000) || found.Context["restated"] != uint64(1_000) {
		t.Fatalf("context = %+v, want the unit and both figures", found.Context)
	}
	h.wantStamps("1k")
}

func TestIsMainAgent(t *testing.T) {
	tests := []struct {
		name      string
		mainAgent string
		agent     string
		want      bool
	}{
		{name: "the named main agent", mainAgent: "m", agent: "m", want: true},
		{name: "another agent", mainAgent: "m", agent: "s"},
		{name: "an unnamed agent", mainAgent: "m", agent: "", want: true},
		{name: "any agent while none is named", mainAgent: "", agent: "s", want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s := &wsState{mainAgent: tc.mainAgent}

			// Act
			got := s.isMainAgent(&conversationv1.AgentId{Value: tc.agent})

			// Assert
			if got != tc.want {
				t.Fatalf("isMainAgent() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestARecomposedCardKeepsWhatItsResponsesStated(t *testing.T) {
	// Arrange: one card only re-stamped by its responses, another re-composed
	// by a later frame of its spawn, so both reach their facts through
	// applyResponseFacts by the two paths.
	h := newHarness(t)
	restamped := &conversationv1.AgentId{Value: "agent-restamped"}
	recomposed := &conversationv1.AgentId{Value: "agent-recomposed"}
	h.spawnSubagent("spawn-1", restamped, "Explore", "map the daemon")
	h.spawnSubagent("spawn-2", recomposed, "Explore", "map the daemon")
	for _, created := range []*conversationv1.AgentId{restamped, recomposed} {
		h.subagentSpends(created, "spend-"+created.GetValue(), misses(1_000, 0))
		h.subagentAnswers(created, "answer-"+created.GetValue(), "claude-sonnet-5")
	}

	// Act.
	h.progressBeat("spawn-2", 12_400)

	// Assert.
	a := bubbleOf(h.bubbleRow("spawn-1", restamped))
	b := bubbleOf(h.bubbleRow("spawn-2", recomposed))
	if b.GetTokens() == nil || b.GetModel() == nil ||
		!proto.Equal(a.GetTokens(), b.GetTokens()) || !proto.Equal(a.GetModel(), b.GetModel()) {
		t.Fatalf("re-stamped (%v, %v) != re-composed (%v, %v)", a.GetTokens(), a.GetModel(), b.GetTokens(), b.GetModel())
	}
}

func TestApplyResponseFacts(t *testing.T) {
	tests := []struct {
		name       string
		spend      bool
		model      string
		wantTokens *frontendv1.FeedSubagentTokens
		wantModel  *frontendv1.FeedSubagentModel
	}{
		{name: "nothing stated", wantTokens: nil, wantModel: nil},
		{name: "usage alone", spend: true, wantTokens: &frontendv1.FeedSubagentTokens{Text: "1k tok"}},
		{name: "model alone", model: "claude-opus-5", wantModel: &frontendv1.FeedSubagentModel{Name: "claude-opus-5"}},
		{
			name: "both", spend: true, model: "claude-opus-5",
			wantTokens: &frontendv1.FeedSubagentTokens{Text: "1k tok"},
			wantModel:  &frontendv1.FeedSubagentModel{Name: "claude-opus-5"},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			created := &conversationv1.AgentId{Value: "agent-explore"}
			h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
			if test.spend {
				h.subagentSpends(created, "sub-1", misses(1_000, 0))
			}
			if test.model != "" {
				h.subagentAnswers(created, "sub-2", test.model)
			}
			s := h.resolver.state(testWorkspace)
			bubble := &frontendv1.FeedSubagent{
				Tokens: &frontendv1.FeedSubagentTokens{Text: "stale"},
				Model:  &frontendv1.FeedSubagentModel{Name: "stale"},
			}

			// Act.
			s.applyResponseFacts(bubble, created)

			// Assert.
			if !proto.Equal(bubble.GetTokens(), test.wantTokens) || !proto.Equal(bubble.GetModel(), test.wantModel) {
				t.Fatalf("(%v, %v), want (%v, %v)", bubble.GetTokens(), bubble.GetModel(), test.wantTokens, test.wantModel)
			}
		})
	}
}
