package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
)

// revisionOf is the response_revision page line rewriting UNIT to MARKDOWN.
func revisionOf(unit, markdown string) *conversationv1.AgentResponseRevision {
	return &conversationv1.AgentResponseRevision{
		Response: &conversationv1.AgentActivityId{Value: unit},
		Prose:    &conversationv1.AgentResponseProse{Markdown: markdown},
	}
}

// revisionEntry is the revision as a replayed history entry.
func revisionEntry(unit, markdown string) *conversationv1.HistoryEntry {
	return frameEntry(mainAgent(), &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ResponseRevision{ResponseRevision: revisionOf(unit, markdown)},
	})
}

// shownMarkdown is the markdown a response bubble draws.
func shownMarkdown(resp *frontendv1.FeedResponse) string {
	return resp.GetSuccess().GetProse().GetProse().GetSource().GetMarkdown()
}

// answerRowOf is the one response row on the root feed.
func (h *harness) answerRowOf() *frontendv1.FeedRow {
	h.t.Helper()
	rows := h.responseRows()
	if len(rows) != 1 {
		h.t.Fatalf("response rows = %d, want exactly 1", len(rows))
	}
	return rows[0]
}

// answerRow is the row id of the one response on the root feed.
func (h *harness) answerRow() *frontendv1.FeedId {
	h.t.Helper()
	return h.answerRowOf().GetId()
}

// revised is the one response bubble on the root feed.
func (h *harness) revised() *frontendv1.FeedResponse {
	h.t.Helper()
	return h.answerRowOf().GetActivity().GetResponse()
}

func TestALiveRevisionRedrawsTheFinalAnswerWithTheRevisedProse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")

	// Act.
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", "1 🔎 the revised"))

	// Assert.
	if got := shownMarkdown(h.revised()); got != "1 🔎 the revised" {
		t.Fatalf("the revised bubble draws %q, want the revised prose", got)
	}
}

func TestALiveRevisionTypesItsChangeOut(t *testing.T) {
	// Arrange.
	h, _ := newPacedHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")

	// Act.
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", "1 🔎 the revised"))

	// Assert.
	if h.revised().GetSuccess().GetRevealPace() == nil {
		t.Fatal("a live revision carried no reveal pace, so its change would appear at once")
	}
}

func TestARevisionKeepsTheFinalAnswerGreen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")

	// Act.
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", "1 🔎 the revised"))

	// Assert.
	if !h.revised().GetFinalAnswer() {
		t.Fatal("the revised response lost its final-answer mark")
	}
}

func TestARevisionIsWhatAReplyQuotes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")
	row := h.answerRow()

	// Act.
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", "1 🔎 the revised"))

	// Assert.
	text, ok := h.resolver.BubbleText(testWorkspace, row)
	if !ok || text.Markdown != "1 🔎 the revised" {
		t.Fatalf("a reply would quote %q (%v), want the revised prose", text.Markdown, ok)
	}
}

func TestTheOriginalsReDeliveredSettleLeavesTheRevisionStanding(t *testing.T) {
	// Arrange: the response was revised.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", "1 🔎 the revised"))

	// Act: the file plane re-delivers the original settle.
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
		Prose: &conversationv1.AgentResponseProse{Markdown: "1 🔎 the original"},
	}, nil), nil, nil)

	// Assert.
	if got := shownMarkdown(h.revised()); got != "1 🔎 the revised" {
		t.Fatalf("the re-delivered settle drew %q over the revision", got)
	}
}

func TestARevisionThatArrivesBeforeItsResponseIsAppliedWhenTheResponseIsDrawn(t *testing.T) {
	// Arrange: the revision lands first.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", "1 🔎 the revised"))

	// Act.
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")

	// Assert.
	if got := shownMarkdown(h.revised()); got != "1 🔎 the revised" {
		t.Fatalf("the response drew %q, want the revision filed before it", got)
	}
}

func TestAReplayedRevisionIsDrawnWholeOverItsResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	page := historyPage(&conversationv1.HistoryFloor{},
		revisionEntry("unit-1", "1 🔎 the revised"),
		frameEntry(mainAgent(), completedWith("unit-1")),
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{
			Activity: responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
				Prose: &conversationv1.AgentResponseProse{Markdown: "1 🔎 the original"},
			}, nil),
		}}),
		promptEntry("turn-1", "explain"),
	)

	// Act.
	h.replay(page)

	// Assert.
	resp := h.revised()
	if got := shownMarkdown(resp); got != "1 🔎 the revised" {
		t.Fatalf("the replayed response draws %q, want the revision", got)
	}
	if resp.GetSuccess().GetRevealPace() != nil {
		t.Fatal("a replayed revision carried a reveal pace; a replay is drawn whole")
	}
}

func TestAPendingRevisionRidesTheResponsesRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")

	// Act.
	ok := h.resolver.SetResponseRevision(testWorkspace, h.answerRow(), &frontendv1.FeedResponseRevision{
		State: &frontendv1.FeedResponseRevision_Pending{Pending: &frontendv1.FeedResponseRevisionPending{}},
	})

	// Assert.
	if !ok || h.revised().GetRevision().GetPending() == nil {
		t.Fatalf("the row carries no pending revision (restated %v)", ok)
	}
}

func TestTheRevisionEndsThePendingState(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")
	h.resolver.SetResponseRevision(testWorkspace, h.answerRow(), &frontendv1.FeedResponseRevision{
		State: &frontendv1.FeedResponseRevision_Pending{Pending: &frontendv1.FeedResponseRevisionPending{}},
	})

	// Act.
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", "1 🔎 the revised"))

	// Assert.
	if h.revised().GetRevision() != nil {
		t.Fatalf("the revised row still carries %v", h.revised().GetRevision())
	}
}

func TestAFailedRevisionRidesTheResponsesRowWithItsCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")

	// Act.
	h.resolver.SetResponseRevision(testWorkspace, h.answerRow(), &frontendv1.FeedResponseRevision{
		State: &frontendv1.FeedResponseRevision_Failed{Failed: &frontendv1.FeedResponseRevisionFailed{Text: "Couldn't revise: overloaded"}},
	})

	// Assert.
	if got := h.revised().GetRevision().GetFailed().GetText(); got != "Couldn't revise: overloaded" {
		t.Fatalf("the failed revision reads %q", got)
	}
	if got := shownMarkdown(h.revised()); got != "1 🔎 the original" {
		t.Fatalf("a failed revision changed the prose to %q", got)
	}
}

func TestSettingARevisionOnARowThatIsNoResponseIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	ok := h.resolver.SetResponseRevision(testWorkspace, &frontendv1.FeedId{Value: "no-such-row"}, nil)

	// Assert.
	if ok {
		t.Fatal("a revision state was set on a row that is no response")
	}
}

func TestAFinalAnswerIsRevisableWithItsUnitAndWhatItSaysNow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", "1 🔎 the revised"))

	// Act.
	got, ok := h.resolver.RevisableFinalResponse(testWorkspace, h.answerRow())

	// Assert.
	if !ok || got != (RevisableResponse{Unit: "unit-1", Markdown: "1 🔎 the revised"}) {
		t.Fatalf("revisable = %+v (%v), want unit-1 and the revised prose", got, ok)
	}
}

func TestAResponseThatIsNoFinalAnswerIsNotRevisable(t *testing.T) {
	// Arrange: a settled response no terminal named the answer.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
		Prose: &conversationv1.AgentResponseProse{Markdown: "commentary"},
	}, nil), nil, nil)

	// Act.
	_, ok := h.resolver.RevisableFinalResponse(testWorkspace, h.answerRow())

	// Assert.
	if ok {
		t.Fatal("a response that is no final answer was revisable")
	}
}

func TestAMalformedRevisionIsRecordedAndNotApplied(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "explain")
	h.concludeAnswer("turn-1", "unit-1", "1 🔎 the original")

	// Act.
	h.resolver.OnResponseRevision(testWorkspace, mainAgent(), revisionOf("unit-1", ""))

	// Assert.
	record, ok := dlog.FindRecord(h.records(), dlog.RecordQuery{Level: "error", Operation: "daemon.feed.response_revision_malformed"})
	if !ok || record.Context["unit"] != "unit-1" {
		t.Fatalf("the malformed revision was not recorded with its unit (%v)", record.Context)
	}
	if got := shownMarkdown(h.revised()); got != "1 🔎 the original" {
		t.Fatalf("a malformed revision changed the prose to %q", got)
	}
}
