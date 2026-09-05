package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
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

func TestTheUsageStampIsTheCacheMissSumAndNothingElse(t *testing.T) {
	// Arrange: a response whose cheap bucket dwarfs its expensive ones.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{
		InputHits:    &conversationv1.TokenCacheHits{Read: 900_000},
		InputMisses:  &conversationv1.TokenCacheMisses{Written: 18_000, Unwritten: 240},
		OutputTokens: 5_000,
	}

	// Act.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, usage), noAddress())

	// Assert: both miss buckets, together — the cheap read is not the cost.
	if got := h.response().GetUsage().GetText(); got != "18.2k" {
		t.Fatalf("usage stamp = %q, want the cache-miss sum 18.2k", got)
	}
}

func TestTheUsageStampSurvivesLaterFramesThatCarryNone(t *testing.T) {
	// Arrange: usage is stated when the response opens.
	h := newHarness(t)
	usage := &conversationv1.TokenUsage{InputMisses: &conversationv1.TokenCacheMisses{Written: 2_100}}
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
