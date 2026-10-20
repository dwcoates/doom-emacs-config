package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// A VENDOR NOTICE AND THE FAULT IT REPORTS ARE ONE CARD (notice.go): a notice
// of a turn that ended on a fault folds into the ending's marker and its own
// row leaves the feed, in either arrival order.

// notice hands the resolver one settled vendor notice of TURN, as UNIT.
func (h *harness) notice(unit, text, turn string) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame(unit, &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: text},
			Authorship: &conversationv1.AgentResponseSuccess_SynthesizedNotice{
				SynthesizedNotice: &conversationv1.AgentResponseSynthesizedNotice{
					Subject: &conversationv1.AgentResponseSynthesizedNotice_Unclassified{Unclassified: &conversationv1.AgentNoticeUnclassified{}},
				},
			},
		}, nil), &conversationv1.TurnId{Value: turn}, nil)
}

// internalFailure is a vendor 500 carrying MESSAGE.
func internalFailure(message string) *conversationv1.AgentFailure {
	return apiFailure(&conversationv1.ApiRequestFailed{Message: message, Kind: &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}}})
}

// noticeTexts is a marker's folded notices, in order.
func noticeTexts(m *frontendv1.FeedOutcomeMarker) []string {
	var out []string
	for _, n := range m.GetNotices() {
		out = append(out, n.GetText())
	}
	return out
}

// noticeRowDrawn reports whether UNIT's notice row is on the root feed.
func (h *harness) noticeRowDrawn(unit string) bool {
	h.t.Helper()
	_, ok := h.findRow(rootFeed(), root(unit))
	return ok
}

func TestANoticeOnItsOwnIsHeadedByTheNeutralMarkerNamingItsSubject(t *testing.T) {
	cases := []struct {
		name    string
		subject *conversationv1.AgentResponseSynthesizedNotice
		detail  string
	}{
		{name: "an exhausted allowance", subject: &conversationv1.AgentResponseSynthesizedNotice{Subject: &conversationv1.AgentResponseSynthesizedNotice_UsageLimit{UsageLimit: &conversationv1.AgentNoticeUsageLimit{}}}, detail: "allowance exhausted"},
		{name: "a changed allowance window", subject: &conversationv1.AgentResponseSynthesizedNotice{Subject: &conversationv1.AgentResponseSynthesizedNotice_UsageTransition{UsageTransition: &conversationv1.AgentNoticeUsageTransition{}}}, detail: "allowance window changed"},
		{name: "an approaching limit", subject: &conversationv1.AgentResponseSynthesizedNotice{Subject: &conversationv1.AgentResponseSynthesizedNotice_UsageWarning{UsageWarning: &conversationv1.AgentNoticeUsageWarning{}}}, detail: "approaching an allowance limit"},
		{name: "an unclassified notice names no detail", subject: &conversationv1.AgentResponseSynthesizedNotice{Subject: &conversationv1.AgentResponseSynthesizedNotice_Unclassified{Unclassified: &conversationv1.AgentNoticeUnclassified{}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			m := noticeMarker(tc.subject)

			// Assert
			if markerFamily(m) != "neutral" || m.GetLabel().GetText() != "vendor notice" {
				t.Fatalf("marker = %v, want the neutral \"vendor notice\" head", m)
			}
			if (m.Detail != nil) != (tc.detail != "") || m.GetDetail().GetText() != tc.detail {
				t.Fatalf("detail = %v, want %q", m.Detail, tc.detail)
			}
		})
	}
}

func TestFoldNoticeAddsOnlyWordsTheMarkerDoesNotAlreadySay(t *testing.T) {
	cases := []struct {
		name  string
		text  string
		added bool
		why   string
	}{
		{name: "new words are added", text: "API Error: 500", added: true},
		{name: "blank words are not", text: "  \n", why: "empty"},
		{name: "the vendor message is not repeated", text: " Internal server error\n", why: "vendor_message"},
		{name: "a notice already folded is not repeated", text: "earlier notice", why: "already_folded"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m := &frontendv1.FeedOutcomeMarker{
				Family: &frontendv1.FeedOutcomeMarker_VendorFault{VendorFault: &frontendv1.FeedOutcomeMarkerVendorFault{
					Expansion: &frontendv1.FeedOutcomeVendorFaultExpansion{Message: &frontendv1.FeedOutcomeVendorMessage{Text: "Internal server error"}},
				}},
				Notices: []*frontendv1.FeedOutcomeNotice{{Text: "earlier notice"}},
			}

			// Act
			added, why := foldNotice(m, tc.text)

			// Assert
			if added != tc.added || why != tc.why {
				t.Fatalf("foldNotice = (%v, %q), want (%v, %q)", added, why, tc.added, tc.why)
			}
			if want := 1 + map[bool]int{true: 1}[tc.added]; len(m.GetNotices()) != want {
				t.Fatalf("notices = %v, want %d", noticeTexts(m), want)
			}
		})
	}
}

func TestOnlyAFaultEndingTakesNotices(t *testing.T) {
	ending := func(m *frontendv1.FeedOutcomeMarker) *frontendv1.FeedRow {
		return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_TurnEnded{TurnEnded: &frontendv1.FeedTurnEnded{
			Outcome: &frontendv1.FeedTurnEnded_Errored{Errored: &frontendv1.FeedTurnEndedErrored{Marker: m}},
		}}}
	}
	cases := []struct {
		name  string
		row   *frontendv1.FeedRow
		fault bool
	}{
		{name: "a vendor fault", row: ending(&frontendv1.FeedOutcomeMarker{Family: &frontendv1.FeedOutcomeMarker_VendorFault{VendorFault: &frontendv1.FeedOutcomeMarkerVendorFault{}}}), fault: true},
		{name: "an agent-repl fault", row: ending(&frontendv1.FeedOutcomeMarker{Family: &frontendv1.FeedOutcomeMarker_AgentReplFault{AgentReplFault: &frontendv1.FeedOutcomeMarkerAgentReplFault{}}}), fault: true},
		{name: "a neutral ending", row: ending(neutralMarker("Stop hook ended the run", ""))},
		{name: "a row that is no ending", row: &frontendv1.FeedRow{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert
			if got := faultEndingMarker(tc.row) != nil; got != tc.fault {
				t.Fatalf("faultEndingMarker set = %v, want %v", got, tc.fault)
			}
		})
	}
}

func TestANoticeDrawnBeforeItsTurnsFaultEndingFoldsIntoTheEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.notice("unit-1", "API Error: 500 {\"type\":\"error\"}", "turn-1")

	// Act
	h.terminal("turn-1", nil, internalFailure("Internal server error"))

	// Assert
	if h.noticeRowDrawn("unit-1") {
		t.Fatalf("the notice row is still drawn, want it retired into the ending's card")
	}
	if got := noticeTexts(h.markerOf("turn-1")); len(got) != 1 || got[0] != "API Error: 500 {\"type\":\"error\"}" {
		t.Fatalf("notices = %q, want the notice's words", got)
	}
	if !h.hasRecord("info", opNoticeFolded) {
		t.Fatalf("records = %+v, want the fold recorded", h.records())
	}
}

func TestANoticeArrivingAfterItsTurnsFaultEndingIsNeverDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, internalFailure("Internal server error"))

	// Act
	h.notice("unit-1", "API Error: 500", "turn-1")

	// Assert
	if h.noticeRowDrawn("unit-1") {
		t.Fatalf("the late notice row was drawn, want it folded into the ending's card")
	}
	if got := noticeTexts(h.markerOf("turn-1")); len(got) != 1 || got[0] != "API Error: 500" {
		t.Fatalf("notices = %q, want the late notice's words", got)
	}
	if !h.hasRecord("info", opNoticeFoldedAfterEnding) {
		t.Fatalf("records = %+v, want the late fold recorded", h.records())
	}
}

func TestANoticeSayingTheVendorMessageIsRetiredWithoutBeingRepeated(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.notice("unit-1", "The API is overloaded.", "turn-1")

	// Act
	h.terminal("turn-1", nil, apiFailure(&conversationv1.ApiRequestFailed{
		Message: "The API is overloaded.", Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}},
	}))

	// Assert
	if h.noticeRowDrawn("unit-1") {
		t.Fatalf("the notice row is still drawn, want it retired")
	}
	if got := noticeTexts(h.markerOf("turn-1")); len(got) != 0 {
		t.Fatalf("notices = %q, want none: the expansion's message already says it", got)
	}
}

func TestARedeliveredFaultEndingKeepsTheNoticesItAlreadyCarries(t *testing.T) {
	// Arrange: the notice folded in and its row retired.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.notice("unit-1", "API Error: 500", "turn-1")
	h.terminal("turn-1", nil, internalFailure("Internal server error"))

	// Act: the same terminal again, composed without notices.
	h.terminal("turn-1", nil, internalFailure("Internal server error"))

	// Assert
	if got := noticeTexts(h.markerOf("turn-1")); len(got) != 1 || got[0] != "API Error: 500" {
		t.Fatalf("notices = %q, want the notice kept across the re-delivery", got)
	}
}

func TestANoticeOfATurnThatConcludedStaysItsOwnCard(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.notice("unit-1", "You're approaching your usage limit", "turn-1")

	// Act
	h.terminal("turn-1", &conversationv1.AgentSuccess{}, nil)

	// Assert
	if !h.noticeRowDrawn("unit-1") {
		t.Fatalf("the notice row was retired, want it kept: the turn raised no fault")
	}
}

func TestANoticeOfAnotherTurnIsNotFolded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.notice("unit-1", "API Error: 500", "turn-1")
	h.terminal("turn-1", &conversationv1.AgentSuccess{}, nil)
	h.deliverPrompt("turn-2", "again")

	// Act
	h.terminal("turn-2", nil, internalFailure("Internal server error"))

	// Assert
	if !h.noticeRowDrawn("unit-1") {
		t.Fatalf("turn-1's notice was retired by turn-2's ending")
	}
	if got := noticeTexts(h.markerOf("turn-2")); len(got) != 0 {
		t.Fatalf("turn-2 notices = %q, want none", got)
	}
}
