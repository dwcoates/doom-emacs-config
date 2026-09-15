package feed

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
)

// THE PROSE FOLD: the daemon accumulates and re-pushes the whole row, so the
// client accumulates nothing and a missed push self-corrects on the next one.

// responseFrame builds one prose frame.
func responseFrame(unit string, result any, usage *conversationv1.TokenUsage) *conversationv1.AgentActivity {
	act := &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Usage:      usage,
	}
	response := &conversationv1.AgentResponse{}
	switch r := result.(type) {
	case *conversationv1.AgentResponseStart:
		response.Result = &conversationv1.AgentResponse_Start{Start: r}
	case *conversationv1.AgentResponseUpdate:
		response.Result = &conversationv1.AgentResponse_Update{Update: r}
	case *conversationv1.AgentResponseSuccess:
		response.Result = &conversationv1.AgentResponse_Success{Success: r}
	case *conversationv1.AgentResponseFailure:
		response.Result = &conversationv1.AgentResponse_Failure{Failure: r}
	}
	act.Item = &conversationv1.AgentActivity_Response{Response: response}
	return act
}

// response returns the only prose bubble on the root feed.
func (h *harness) response() *frontendv1.FeedResponse {
	h.t.Helper()
	return h.only(rootFeed()).GetActivity().GetResponse()
}

// responseRows answers every prose bubble on the root feed, for the tests whose
// subject is whether one was drawn at all.
func (h *harness) responseRows() []*frontendv1.FeedRow {
	h.t.Helper()
	var out []*frontendv1.FeedRow
	for _, row := range h.rows(rootFeed()) {
		if row.GetActivity().GetResponse() != nil {
			out = append(out, row)
		}
	}
	return out
}

// A /clear (or /compact) DRAWS NO RESPONSE BUBBLE. The vendor emits an empty
// "(no content)" response for the directive turn, which the webapp would draw as
// a cut-short card; the directive's only visible outcome is its separation bar.
func TestAClearTurnsResponseDrawsNoBubble(t *testing.T) {
	// Arrange: the /clear turn is registered and running.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))

	// Act: the directive's empty response arrives.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: ""},
		}, nil), noAddress())

	// Assert: no response bubble.
	if got := h.responseRows(); len(got) != 0 {
		t.Fatalf("response rows = %d, want none for a /clear directive", len(got))
	}
}

// THE SUPPRESSION SURVIVES THE OTHER PLANE'S LATE RE-DELIVERY. The response
// re-arrives from the file plane after the terminal cleared the in-flight turn;
// it must still draw nothing, keyed on the unit the directive turn produced.
func TestAClearTurnsResponseStaysSuppressedOnLateRedelivery(t *testing.T) {
	// Arrange: the directive's response was suppressed during the turn, then the
	// turn ended.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: ""},
		}, nil), noAddress())
	h.cutAt("entry-clear", clearedContextCut())
	h.terminal("turn-2", interruptedByUser(), nil)

	// Act: the file plane re-delivers the same response with no turn in flight.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: ""},
		}, nil), noAddress())

	// Assert: still no response bubble.
	if got := h.responseRows(); len(got) != 0 {
		t.Fatalf("response rows = %d, want none however many planes deliver the directive's response", len(got))
	}
}

// A GENUINE USER-STOP KEEPS ITS RESPONSE. A real turn the user stopped mid-prose
// is not a directive, so its partial answer still draws — only a /clear draws
// nothing.
func TestAUserStoppedTurnsPartialResponseStillDraws(t *testing.T) {
	// Arrange: an ordinary turn with partial prose.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "partial answer"}, nil), noAddress())

	// Act: the user stops it — no context cut.
	h.terminal("turn-1", interruptedByUser(), nil)

	// Assert: the partial response is still drawn.
	if got := h.responseRows(); len(got) != 1 {
		t.Fatalf("response rows = %d, want the user-stopped turn's partial answer kept", len(got))
	}
}

func TestResponseStartDrawsTheEmptyBubble(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), noAddress())

	// Assert: an update arm with no prose is what makes the agent visibly begin
	// answering before the first token lands.
	update := h.response().GetUpdate()
	if update == nil || update.GetProse().GetMarkdown() != "" {
		t.Fatalf("bubble = %+v, want an empty update", h.response())
	}
}

func TestResponseFragmentsAccumulateDaemonSide(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), noAddress())

	// Act: two fragments, each a delta.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "Hello, "}, nil), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "world"}, nil), noAddress())

	// Assert: the row carries the whole, not the last delta.
	if got := h.response().GetUpdate().GetProse().GetMarkdown(); got != "Hello, world" {
		t.Fatalf("prose = %q, want the accumulated whole", got)
	}
}

func TestTheTerminalRestatesTheWholeAndSelfCorrectsALostFragment(t *testing.T) {
	// Arrange: a fold that missed a fragment in transit.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "Hel"}, nil), noAddress())

	// Act: the settled frame carries the whole text regardless.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "Hello, world"},
		}, nil), noAddress())

	// Assert: the settled bubble is right even though the fold was wrong.
	if got := h.response().GetSuccess().GetProse().GetMarkdown(); got != "Hello, world" {
		t.Fatalf("settled prose = %q, want the restated whole", got)
	}
}

func TestAFragmentAfterTheTerminalCannotReopenTheBubble(t *testing.T) {
	// Arrange: a settled bubble.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "done"},
		}, nil), noAddress())

	// Act: a late fragment.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: " and more"}, nil), noAddress())

	// Assert: the settled whole stands, and the arrival is recorded.
	//
	// AT DEBUG, NOT WARN. The two store planes that feed one block's fold —
	// the shim's stream and the sidecar's transcript — share an upsert key and
	// are not ordered against one another, so a delta behind a settled whole
	// is a routine interleave rather than a producer fault, and the daemon
	// cannot tell the two cases apart.
	if got := h.response().GetSuccess().GetProse().GetMarkdown(); got != "done" {
		t.Fatalf("prose = %q, want the settled whole unchanged", got)
	}
	if !h.hasRecord("debug", "daemon.feed.response_fragment_after_settle") {
		t.Fatalf("records = %+v, want a DEBUG daemon.feed.response_fragment_after_settle", h.records())
	}
}

// The cross-plane interleave the debug level exists for, in the order it was
// measured on the wire: the sidecar's settled whole lands between the shim's
// block start and the shim's own trailing deltas.
func TestASettledWholeFromOnePlaneOutrunsTheOthersTrailingDeltas(t *testing.T) {
	// Arrange: the stream plane opens the block.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), noAddress())

	// Act: the file plane settles the whole, then the stream plane's deltas
	// for that same block arrive behind it.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "Here is what I found."},
		}, nil), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "Here is wha"}, nil), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "t I found."}, nil), noAddress())

	// Assert: the bubble reads as the settled whole, with nothing doubled.
	if got := h.response().GetSuccess().GetProse().GetMarkdown(); got != "Here is what I found." {
		t.Fatalf("prose = %q, want the settled whole with no delta folded in behind it", got)
	}
}

// The same interleave raises no warning: the sweep that fails a test on an
// unexpected warning must not fire on two planes racing.
func TestACrossPlaneInterleaveRaisesNoWarning(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), noAddress())

	// Act.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "Here is what I found."},
		}, nil), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "Here is wha"}, nil), noAddress())

	// Assert.
	if h.hasRecord("warn", "daemon.feed.response_fragment_after_settle") {
		t.Fatalf("records = %+v, want no WARN for a cross-plane interleave", h.records())
	}
}

func TestResponseFailureKeepsWhatLandedAndMarksItBroken(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseFailure{
			Prose:  &conversationv1.AgentResponseProse{Markdown: "half an ans"},
			Reason: &conversationv1.AgentResponseFailureReason{},
		}, nil), noAddress())

	// Assert: WHY it died is the turn's terminal row, not this bubble's.
	broken := h.response().GetError()
	if broken == nil || broken.GetProse().GetMarkdown() != "half an ans" {
		t.Fatalf("bubble = %+v, want the broken arm keeping what landed", h.response())
	}
}

func TestTheUsageStampIsFreshInputPlusOutputAndExcludesCache(t *testing.T) {
	// Arrange: a response over a huge cached context — the response bubble is
	// this turn's work, not the context window (see usage.go).
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{
		InputHits:    &conversationv1.TokenCacheHits{Read: 900_000},
		InputMisses:  &conversationv1.TokenCacheMisses{Written: 18_000, Unwritten: 240},
		OutputTokens: 5_000,
	}

	// Act.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, usage), noAddress())

	// Assert: fresh input (240) + output (5_000) = 5.2k; cache_read and
	// cache_creation are the context window and are excluded.
	if got := h.response().GetUsage().GetText(); got != "5.2k" {
		t.Fatalf("usage stamp = %q, want the turn's 240+5_000 = 5.2k", got)
	}
}

func TestTheUsageStampSurvivesLaterFramesThatCarryNone(t *testing.T) {
	// Arrange: usage is stated when the response opens.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{InputMisses: &conversationv1.TokenCacheMisses{Unwritten: 2_100}}
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, usage), noAddress())

	// Act: a fragment with no usage of its own.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "hi"}, nil), noAddress())

	// Assert: absence means "not the carrying frame", never "free".
	if got := h.response().GetUsage().GetText(); got != "2.1k" {
		t.Fatalf("usage stamp = %q, want it kept across pushes", got)
	}
}

func TestAResponseWithNoObservedUsageDrawsNoStamp(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), noAddress())

	// Assert: absence draws no stamp, never a zero.
	if h.response().GetUsage() != nil {
		t.Fatalf("usage = %+v, want unset", h.response().GetUsage())
	}
}

func TestTheUsageStampCarriesTheSettledInstant(t *testing.T) {
	// Arrange: usage stated when the block opens.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{InputMisses: &conversationv1.TokenCacheMisses{Unwritten: 2_100}}
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, usage), noAddress())

	// Act: the terminal frame, at the harness clock's instant.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "done"},
		}, nil), noAddress())

	// Assert: the settled bubble's stamp counts back from when it settled.
	if got := h.response().GetUsage().GetAtMs(); got != h.nowMs {
		t.Fatalf("usage at_ms = %d, want the settle instant %d", got, h.nowMs)
	}
}

func TestTheUsageStampCarriesNoInstantWhileArriving(t *testing.T) {
	// Arrange, Act: usage stated when the block opens, still arriving.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{InputMisses: &conversationv1.TokenCacheMisses{Unwritten: 2_100}}
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, usage), noAddress())

	// Assert: there is no settled instant yet, so the corner carries a zero
	// and the client reveals no timestamp.
	if got := h.response().GetUsage().GetAtMs(); got != 0 {
		t.Fatalf("usage at_ms = %d, want 0 while arriving", got)
	}
}

func TestTheSettledInstantIsStampedOnceAcrossReDeliveries(t *testing.T) {
	// Arrange: the block opens and settles at the first instant.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{InputMisses: &conversationv1.TokenCacheMisses{Unwritten: 2_100}}
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, usage), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "done"},
		}, nil), noAddress())
	first := h.response().GetUsage().GetAtMs()

	// Act: the other plane re-delivers the same settle, later on the clock.
	h.nowMs += 5_000
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "done"},
		}, nil), noAddress())

	// Assert: the first settle instant stands, never the replay time.
	if got := h.response().GetUsage().GetAtMs(); got != first {
		t.Fatalf("usage at_ms = %d, want the first settle instant %d", got, first)
	}
}

// REGRESSION LOCK (2026-09-14, "response duration always 0s"): a re-compose
// from a fresh fold (replay/reconnect) must stamp the corner from the terminal
// arm's carried settled_at, NEVER the compose-time clock — otherwise "N ago"
// resets to 0s on every replay. Here the harness clock is deliberately far from
// the carried instant, and the carried instant must win.
func TestASettledSuccessStampsTheCarriedInstantNotComposeTime(t *testing.T) {
	// Arrange: a fresh fold whose replay clock differs from the real settle.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{InputMisses: &conversationv1.TokenCacheMisses{Unwritten: 2_100}}
	const carried = 1_700_000_000_000
	h.nowMs = carried + 5_000

	// Act: the terminal arm carries its real settle instant.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose:     &conversationv1.AgentResponseProse{Markdown: "done"},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: carried},
		}, usage), noAddress())

	// Assert: the corner is the carried settle instant, not the replay clock.
	if got := h.response().GetUsage().GetAtMs(); got != carried {
		t.Fatalf("usage at_ms = %d, want the carried settle instant %d, not compose time %d", got, carried, h.nowMs)
	}
}

// The failure arm carries its own settled_at for the identical reason.
func TestASettledFailureStampsTheCarriedInstantNotComposeTime(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{InputMisses: &conversationv1.TokenCacheMisses{Unwritten: 2_100}}
	const carried = 1_700_000_000_000
	h.nowMs = carried + 5_000

	// Act: a failed terminal arm carrying its real settle instant.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseFailure{
			Prose:     &conversationv1.AgentResponseProse{Markdown: "half an ans"},
			Reason:    &conversationv1.AgentResponseFailureReason{},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: carried},
		}, usage), noAddress())

	// Assert.
	if got := h.response().GetUsage().GetAtMs(); got != carried {
		t.Fatalf("usage at_ms = %d, want the carried settle instant %d, not compose time %d", got, carried, h.nowMs)
	}
}

// A truly live settle whose source carried no instant falls back to the clock —
// there is nothing else to stamp from.
func TestASettledResponseWithNoCarriedInstantFallsBackToNow(t *testing.T) {
	// Arrange, Act: a terminal arm with no settled_at.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{InputMisses: &conversationv1.TokenCacheMisses{Unwritten: 2_100}}
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "done"},
		}, usage), noAddress())

	// Assert: the corner is the harness clock, the only instant available.
	if got := h.response().GetUsage().GetAtMs(); got != h.nowMs {
		t.Fatalf("usage at_ms = %d, want the fallback clock %d", got, h.nowMs)
	}
}

func TestASynthesizedNoticeIsDrawnAsANoticeAndNotAsTheAgentsAnswer(t *testing.T) {
	// Arrange: the vendor's own outage wording, which arrives in the same shape
	// as an answer.
	h := newHarness(t)

	// Act.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "you have hit your monthly spend limit"},
			Authorship: &conversationv1.AgentResponseSuccess_SynthesizedNotice{
				SynthesizedNotice: &conversationv1.AgentResponseSynthesizedNotice{
					Subject: &conversationv1.AgentResponseSynthesizedNotice_UsageLimit{
						UsageLimit: &conversationv1.AgentNoticeUsageLimit{},
					},
				},
			},
		}, nil), noAddress())

	// Assert: the authorship is stated in the bubble's notice field.
	heading := h.response().GetNotice().GetHeading()
	if want := "Notice — your allowance is exhausted."; !contains(heading, want) {
		t.Fatalf("heading = %q, want it to state %q", heading, want)
	}
	if !h.hasRecord("debug", "daemon.feed.response_synthesized_notice") {
		t.Fatalf("records = %+v, want the synthesized-notice branch recorded", h.records())
	}
}

func TestASynthesizedNoticesProseIsKeptVerbatimWithNoHeadingSplicedIn(t *testing.T) {
	// Arrange, Act: a heading spliced into the markdown would be
	// indistinguishable from words the vendor actually wrote.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "you have hit your monthly spend limit"},
			Authorship: &conversationv1.AgentResponseSuccess_SynthesizedNotice{
				SynthesizedNotice: &conversationv1.AgentResponseSynthesizedNotice{
					Subject: &conversationv1.AgentResponseSynthesizedNotice_UsageLimit{
						UsageLimit: &conversationv1.AgentNoticeUsageLimit{},
					},
				},
			},
		}, nil), noAddress())

	// Assert.
	if got := h.response().GetSuccess().GetProse().GetMarkdown(); got != "you have hit your monthly spend limit" {
		t.Fatalf("prose = %q, want the vendor's own words byte for byte", got)
	}
}

func TestAModelAuthoredResponseCarriesNoNotice(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "here is the answer"},
			Authorship: &conversationv1.AgentResponseSuccess_FromModel{
				FromModel: &conversationv1.AgentResponseFromModel{},
			},
		}, nil), noAddress())

	// Assert: unset notice is what says "these are the agent's own words".
	if h.response().GetNotice() != nil {
		t.Fatalf("notice = %+v, want unset for model-authored prose", h.response().GetNotice())
	}
}

func TestAModelAuthoredResponseCarriesNoNoticeHeading(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "here is the answer"},
			Authorship: &conversationv1.AgentResponseSuccess_FromModel{
				FromModel: &conversationv1.AgentResponseFromModel{},
			},
		}, nil), noAddress())

	// Assert.
	if got := h.response().GetSuccess().GetProse().GetMarkdown(); got != "here is the answer" {
		t.Fatalf("prose = %q, want the model's words untouched", got)
	}
}

// contains is strings.Contains, spelled here so the assertions read as claims.
func contains(haystack, needle string) bool {
	return len(needle) == 0 || indexOf(haystack, needle) >= 0
}

// indexOf finds a substring, or -1.
func indexOf(haystack, needle string) int {
	for i := 0; i+len(needle) <= len(haystack); i++ {
		if haystack[i:i+len(needle)] == needle {
			return i
		}
	}
	return -1
}

// THE SETTLED TREE IS NO LONGER WRAPPED BY THE DAEMON. A response under the
// metaprompt is one bare Unicode tree; the daemon serves it VERBATIM on the
// settled arm, exactly as it serves the streaming delta and the failure arm,
// because only the webapp can wrap the tree to the bubble's true live width
// and re-flow it on resize (webapp/src/metaprompt-tree.ts).

// settle delivers one settled response carrying markdown and answers what the
// bubble now holds.
func (h *harness) settle(markdown string) string {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
		}, nil), noAddress())
	return h.response().GetSuccess().GetProse().GetMarkdown()
}

func TestASettledTreeWiderThanTheOldLimitIsServedVerbatim(t *testing.T) {
	// Arrange: one branch whose body runs well past the daemon's former
	// 105-column limit, which the daemon used to wrap and now must not touch.
	h := newHarness(t)
	tree := "1. 🎯 Root\n├── 1.1. " + strings.TrimSpace(strings.Repeat("word ", 30))

	// Act.
	got := h.settle(tree)

	// Assert: served exactly as it arrived, one line per branch, no wrapping.
	if got != tree {
		t.Fatalf("settled tree was altered: %q", got)
	}
}

func TestASettledTreeInsideTheOldLimitIsAlsoServedVerbatim(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	tree := "1. 🎯 Root\n├── 1.1. Short.\n└── 1.2. Also short."
	got := h.settle(tree)

	// Assert.
	if got != tree {
		t.Fatalf("settled tree changed: %q", got)
	}
}

func TestASettledArmMatchesTheStreamingDeltaArmForTheSameProse(t *testing.T) {
	// Arrange: a start, one delta, then the settled whole of the same tree.
	h := newHarness(t)
	tree := "1. 🎯 Root\n├── 1.1. " + strings.TrimSpace(strings.Repeat("word ", 30))
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: tree}, nil), noAddress())
	delta := h.response().GetUpdate().GetProse().GetMarkdown()

	// Act: the settled success restates the whole.
	got := h.settle(tree)

	// Assert: both arms carry the identical verbatim prose.
	if got != delta {
		t.Fatalf("settled %q does not match streamed %q", got, delta)
	}
}
