package feed

import (
	"errors"
	"os"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/feedid"
)

// THE QUESTION DRAFT: one daemon-owned record per standing batch, edited by the
// card's clicks and the editor's typed answer, drawn on the open card, and
// claimed for delivery by the edit that answers the last question.

// openTwoQuestionAsk poses the mixed batch and leaves it standing.
func (h *harness) openTwoQuestionAsk() {
	h.t.Helper()
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch:     twoQuestionBatch(),
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
}

// edit applies one draft edit to ask-1 and fails the test on a refusal.
func (h *harness) edit(edit QuestionEdit) QuestionDraftOutcome {
	h.t.Helper()
	outcome, err := h.resolver.EditQuestionDraft(testWorkspace, "ask-1", edit)
	if err != nil {
		h.t.Fatalf("EditQuestionDraft(%+v): %v", edit, err)
	}
	return outcome
}

// openDraft reads the open card's draft state.
func (h *harness) openDraft() *frontendv1.FeedQuestionOpen {
	h.t.Helper()
	open := h.questionCard().GetOpen()
	if open == nil {
		h.t.Fatalf("state = %T, want open", h.questionCard().GetState())
	}
	return open
}

func pickAuth(label string) QuestionEdit {
	return QuestionEdit{Kind: EditPick, Question: "Which auth method?", Label: label}
}

func pickSuite(label string) QuestionEdit {
	return QuestionEdit{Kind: EditPick, Question: "Which suites?", Label: label}
}

func TestAnOpenCardCarriesAnEmptyDraftPerQuestionShowingTheFirst(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Assert.
	open := h.openDraft()
	if open.GetActive() != 0 {
		t.Fatalf("active = %d, want 0", open.GetActive())
	}
	if len(open.GetDrafts()) != 2 {
		t.Fatalf("drafts = %d, want one per question", len(open.GetDrafts()))
	}
	for i, draft := range open.GetDrafts() {
		if draft.GetAnswered() || len(draft.GetChosen()) != 0 || draft.OtherText != nil {
			t.Fatalf("draft %d = %+v, want empty and unanswered", i, draft)
		}
	}
}

func TestAPickOnASingleSelectAnswersItAndShowsTheNextUnansweredQuestion(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Act.
	outcome := h.edit(pickAuth("OAuth"))

	// Assert.
	open := h.openDraft()
	if got := open.GetDrafts()[0]; !got.GetAnswered() || len(got.GetChosen()) != 1 || got.GetChosen()[0] != "OAuth" {
		t.Fatalf("draft = %+v, want answered with OAuth", got)
	}
	if open.GetActive() != 1 {
		t.Fatalf("active = %d, want the next unanswered question", open.GetActive())
	}
	if outcome.Deliver != nil {
		t.Fatalf("deliver = %+v, want nothing while a question is unanswered", outcome.Deliver)
	}
}

func TestAPickOnTheChosenOptionClearsItWithoutAdvancing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))
	h.edit(QuestionEdit{Kind: EditShow, Question: "Which auth method?"})

	// Act.
	h.edit(pickAuth("OAuth"))

	// Assert.
	open := h.openDraft()
	if got := open.GetDrafts()[0]; got.GetAnswered() || len(got.GetChosen()) != 0 {
		t.Fatalf("draft = %+v, want cleared and unanswered", got)
	}
	if open.GetActive() != 0 {
		t.Fatalf("active = %d, want the cleared question still shown", open.GetActive())
	}
}

func TestAPickOnAMultiSelectTogglesItsTickInTheServedOrderWithoutAnswering(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Act: ticked out of order.
	h.edit(pickSuite("webapp"))
	h.edit(pickSuite("daemon"))

	// Assert.
	got := h.openDraft().GetDrafts()[1]
	if len(got.GetChosen()) != 2 || got.GetChosen()[0] != "daemon" || got.GetChosen()[1] != "webapp" {
		t.Fatalf("chosen = %v, want [daemon webapp] in the served order", got.GetChosen())
	}
	if got.GetAnswered() {
		t.Fatal("a tick answered the multi-select; it waits for its confirm")
	}
}

func TestASecondPickOnAMultiSelectUnticksIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickSuite("daemon"))

	// Act.
	h.edit(pickSuite("daemon"))

	// Assert.
	if got := h.openDraft().GetDrafts()[1].GetChosen(); len(got) != 0 {
		t.Fatalf("chosen = %v, want none", got)
	}
}

func TestAConfirmAnswersAMultiSelectWithItsTicks(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickSuite("daemon"))

	// Act.
	h.edit(QuestionEdit{Kind: EditConfirm, Question: "Which suites?"})

	// Assert.
	open := h.openDraft()
	if !open.GetDrafts()[1].GetAnswered() {
		t.Fatal("the confirmed multi-select is not answered")
	}
	if open.GetActive() != 0 {
		t.Fatalf("active = %d, want the unanswered first question", open.GetActive())
	}
}

func TestAConfirmWithNothingTickedAnswersTheQuestionWithNone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Act.
	h.edit(QuestionEdit{Kind: EditConfirm, Question: "Which suites?"})

	// Assert.
	got := h.openDraft().GetDrafts()[1]
	if !got.GetAnswered() || len(got.GetChosen()) != 0 || got.GetOtherText() != nil {
		t.Fatalf("draft = %+v, want answered with nothing chosen or typed", got)
	}
}

func TestATypedAnswerReplacesASingleSelectsPick(t *testing.T) {
	// Arrange: the first question shown, with a pick that was then shown again.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))
	h.edit(QuestionEdit{Kind: EditShow, Question: "Which auth method?"})

	// Act.
	h.edit(QuestionEdit{Kind: EditType, Text: "mutual TLS"})

	// Assert.
	got := h.openDraft().GetDrafts()[0]
	if len(got.GetChosen()) != 0 || got.GetOtherText().GetText() != "mutual TLS" || !got.GetAnswered() {
		t.Fatalf("draft = %+v, want only the typed answer, answered", got)
	}
}

func TestATypedAnswerKeepsAMultiSelectsTicksAndAnswersIt(t *testing.T) {
	// Arrange: the multi-select shown, with one tick.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickSuite("daemon"))

	// Act.
	h.edit(QuestionEdit{Kind: EditType, Text: "and the e2e suite"})

	// Assert.
	got := h.openDraft().GetDrafts()[1]
	if len(got.GetChosen()) != 1 || got.GetOtherText().GetText() != "and the e2e suite" || !got.GetAnswered() {
		t.Fatalf("draft = %+v, want the tick kept beside the typed note, answered", got)
	}
}

func TestAShowMovesTheActiveQuestion(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Act.
	h.edit(QuestionEdit{Kind: EditShow, Question: "Which suites?"})

	// Assert.
	if got := h.openDraft().GetActive(); got != 1 {
		t.Fatalf("active = %d, want 1", got)
	}
}

func TestAnEditNamingAnUnservedValueIsRefusedWithoutChangingTheDraft(t *testing.T) {
	tests := []struct {
		name string
		edit QuestionEdit
		want string
	}{
		{"an unserved question", QuestionEdit{Kind: EditShow, Question: "Which colour?"}, "Which colour?"},
		{"an unserved option", pickAuth("SAML"), "SAML"},
		{"a confirm on a single-select", QuestionEdit{Kind: EditConfirm, Question: "Which auth method?"}, "Which auth method?"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.openTwoQuestionAsk()
			before := h.openDraft()

			// Act.
			_, err := h.resolver.EditQuestionDraft(testWorkspace, "ask-1", tc.edit)

			// Assert.
			var unserved *UnservedQuestionValueError
			if !errors.As(err, &unserved) || unserved.Text != tc.want {
				t.Fatalf("err = %v, want an unserved value %q", err, tc.want)
			}
			after := h.openDraft()
			if after.GetActive() != before.GetActive() || after.GetDrafts()[0].GetAnswered() || len(after.GetDrafts()[0].GetChosen()) != 0 {
				t.Fatalf("draft changed to %+v on a refused edit", after)
			}
		})
	}
}

func TestTheEditAnsweringTheLastQuestionClaimsTheBatchAndHandsBackItsAnswers(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("API key"))
	h.edit(pickSuite("webapp"))

	// Act.
	outcome := h.edit(QuestionEdit{Kind: EditType, Text: "and daemon"})

	// Assert.
	answers := outcome.Deliver.GetAnswers()
	if len(answers) != 2 {
		t.Fatalf("answers = %+v, want one per question", outcome.Deliver)
	}
	if answers[0].GetQuestion().GetText() != "Which auth method?" || answers[0].GetChosen()[0].GetLabel().GetLabel() != "API key" {
		t.Fatalf("first answer = %+v", answers[0])
	}
	if answers[1].GetChosen()[0].GetLabel().GetLabel() != "webapp" || answers[1].GetFreeText().GetText() != "and daemon" {
		t.Fatalf("second answer = %+v", answers[1])
	}
	if outcome.Agent.GetValue() != mainAgent().GetValue() {
		t.Fatalf("agent = %q, want the asking agent", outcome.Agent.GetValue())
	}
}

func TestAnEmptyConfirmOnTheLastQuestionDeliversAnEmptySelection(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("API key"))

	// Act.
	outcome := h.edit(QuestionEdit{Kind: EditConfirm, Question: "Which suites?"})

	// Assert.
	answers := outcome.Deliver.GetAnswers()
	if len(answers) != 2 {
		t.Fatalf("answers = %+v, want the batch delivered with one answer per question", outcome.Deliver)
	}
	got := answers[1]
	if got.GetQuestion().GetText() != "Which suites?" || len(got.GetChosen()) != 0 || got.FreeText != nil {
		t.Fatalf("second answer = %+v, want the multi-select answered with nothing chosen or typed", got)
	}
}

func TestAClaimedBatchRefusesEveryFurtherEdit(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))
	h.edit(QuestionEdit{Kind: EditType, Text: "all of them"})

	// Act.
	_, err := h.resolver.EditQuestionDraft(testWorkspace, "ask-1", pickAuth("API key"))

	// Assert.
	if !errors.Is(err, ErrQuestionNotStanding) {
		t.Fatalf("err = %v, want ErrQuestionNotStanding", err)
	}
}

func TestAReleasedClaimLetsTheNextAnsweringEditDeliverAgain(t *testing.T) {
	// Arrange: claimed, and the delivery failed.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))
	h.edit(QuestionEdit{Kind: EditType, Text: "all of them"})
	h.resolver.ReleaseQuestionDelivery(testWorkspace, "ask-1")

	// Act: a click that leaves the batch answered.
	outcome := h.edit(pickAuth("API key"))

	// Assert.
	if outcome.Deliver == nil {
		t.Fatal("the released batch did not deliver again once answered")
	}
}

func TestReleasingTheClaimOfASettledAskLeavesTheSettledCard(t *testing.T) {
	// Arrange: claimed, then settled before the failed delivery came back.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))
	h.edit(QuestionEdit{Kind: EditType, Text: "all of them"})
	h.pose("ask-1", &conversationv1.AgentQuestionFailure{})

	// Act.
	h.resolver.ReleaseQuestionDelivery(testWorkspace, "ask-1")

	// Assert.
	if h.questionCard().GetExpired() == nil {
		t.Fatalf("state = %T, want the settled card kept", h.questionCard().GetState())
	}
	if !h.hasRecord("info", "daemon.feed.question_draft_release") {
		t.Fatalf("records = %+v, want the release recorded", h.records())
	}
}

func TestARestatedStartKeepsTheDraft(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))

	// Act: the stream re-opens with the start it announced.
	h.openTwoQuestionAsk()

	// Assert.
	if !h.openDraft().GetDrafts()[0].GetAnswered() {
		t.Fatal("a restated start wiped the draft")
	}
}

func TestAnEditToASettledAskIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.pose("ask-1", &conversationv1.AgentQuestionFailure{})

	// Act.
	_, err := h.resolver.EditQuestionDraft(testWorkspace, "ask-1", pickAuth("OAuth"))

	// Assert.
	if !errors.Is(err, ErrQuestionNotStanding) {
		t.Fatalf("err = %v, want ErrQuestionNotStanding", err)
	}
}

func TestAnEditToAnAskNeverDrawnIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.resolver.EditQuestionDraft(testWorkspace, "ask-unknown", pickAuth("OAuth"))

	// Assert.
	if !errors.Is(err, ErrQuestionNotStanding) {
		t.Fatalf("err = %v, want ErrQuestionNotStanding", err)
	}
}

func TestEveryAppliedEditRepublishesTheCardInPlace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Act.
	h.edit(pickAuth("OAuth"))

	// Assert: still one row, carrying the edit.
	if rows := h.rows(rootFeed()); len(rows) != 1 {
		t.Fatalf("rows = %d, want the one card re-published in place", len(rows))
	}
	if !h.openDraft().GetDrafts()[0].GetAnswered() {
		t.Fatal("the re-published card does not carry the edit")
	}
}

// ---- StandingAsk ----

func openPermission(h *harness, askID string) {
	h.t.Helper()
	h.ask(askID, "", &conversationv1.AgentPermissionStart{
		Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run ls"},
	})
}

func TestNoStandingAskIsFoundOnAQuietFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, found := h.resolver.StandingAsk(testWorkspace, nil)

	// Assert.
	if found {
		t.Fatal("a standing ask was found on a feed with none")
	}
}

func TestTheOldestStandingAskIsTheOneATypedAnswerGoesTo(t *testing.T) {
	tests := []struct {
		name string
		draw func(h *harness)
		want StandingAsk
	}{
		{"a permission before a question", func(h *harness) {
			openPermission(h, "perm-1")
			h.openTwoQuestionAsk()
		}, StandingAsk{Kind: AskPermission, ID: "perm-1"}},
		{"a question before a permission", func(h *harness) {
			h.openTwoQuestionAsk()
			openPermission(h, "perm-1")
		}, StandingAsk{Kind: AskQuestion, ID: "ask-1"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tc.draw(h)

			// Act.
			got, found := h.resolver.StandingAsk(testWorkspace, nil)

			// Assert.
			if !found || got != tc.want {
				t.Fatalf("standing = %+v (found %v), want %+v", got, found, tc.want)
			}
		})
	}
}

func TestASettledPermissionIsNotStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	openPermission(h, "perm-1")
	h.ask("perm-1", "", &conversationv1.AgentPermissionFailure{})

	// Act.
	_, found := h.resolver.StandingAsk(testWorkspace, nil)

	// Assert.
	if found {
		t.Fatal("a settled permission was found standing")
	}
}

func TestAClaimedBatchIsNotStandingForATypedAnswer(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))
	h.edit(QuestionEdit{Kind: EditType, Text: "all of them"})

	// Act.
	_, found := h.resolver.StandingAsk(testWorkspace, nil)

	// Assert.
	if found {
		t.Fatal("a batch claimed for delivery was found standing")
	}
}

func TestAStandingAskIsFoundOnlyOnTheFeedItStandsOn(t *testing.T) {
	// Arrange: a subagent's permission, standing on its bubble's sub-feed.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-sub"}
	bubble := h.spawnSubagent("unit-1", created, "general-purpose", "")
	h.resolver.OnPermission(testWorkspace, created, &conversationv1.AgentPermission{
		Id: &conversationv1.AgentPermissionId{Value: "perm-sub"},
		Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{
			Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run ls"},
		}},
	}, nil, nil)
	ref, err := testDecode(bubble.GetId())
	if err != nil {
		t.Fatalf("decode the bubble row: %v", err)
	}

	// Act.
	_, onRoot := h.resolver.StandingAsk(testWorkspace, nil)
	got, onBubble := h.resolver.StandingAsk(testWorkspace, &ref)

	// Assert.
	if onRoot {
		t.Fatal("the subagent's ask was found on the root feed")
	}
	if !onBubble || got.ID != "perm-sub" {
		t.Fatalf("standing = %+v (found %v), want perm-sub on the bubble's feed", got, onBubble)
	}
}

func TestATargetHeadingNoSubFeedAddressesNoAsk(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	ref := feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "no-such-bubble"}}

	// Act.
	_, found := h.resolver.StandingAsk(testWorkspace, &ref)

	// Assert.
	if found {
		t.Fatal("a target heading no sub-feed found an ask")
	}
}

// ---- QuestionSelection ----

func TestQuestionSelectionNamesTheQuestionAndKeepsTheChoicesInOrder(t *testing.T) {
	// Act.
	got := QuestionSelection("Which suites?", []string{"webapp", "daemon"}, nil)

	// Assert.
	if got.GetQuestion().GetText() != "Which suites?" {
		t.Fatalf("question = %q", got.GetQuestion().GetText())
	}
	if len(got.GetChosen()) != 2 || got.GetChosen()[0].GetLabel().GetLabel() != "webapp" || got.GetChosen()[1].GetLabel().GetLabel() != "daemon" {
		t.Fatalf("chosen = %+v, want [webapp daemon] in order", got.GetChosen())
	}
	if got.GetFreeText() != nil {
		t.Fatalf("free text = %+v, want unset when nothing was typed", got.GetFreeText())
	}
}

func TestQuestionSelectionCarriesATypedAnswerEvenWhenEmpty(t *testing.T) {
	// Arrange: an empty typed string is still something typed, unlike nil.
	empty := ""

	// Act.
	got := QuestionSelection("q?", nil, &empty)

	// Assert.
	if got.GetFreeText() == nil || got.GetFreeText().GetText() != "" || len(got.GetChosen()) != 0 {
		t.Fatalf("selection = %+v, want the empty typed answer and no choices", got)
	}
}

func TestADeliveredDraftIsBuiltThroughQuestionSelection(t *testing.T) {
	// Arrange: a hand-rolled selection that drifted from the builder fails here.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))

	// Act.
	outcome := h.edit(QuestionEdit{Kind: EditType, Text: "typed"})

	// Assert.
	typed := "typed"
	want := []*conversationv1.AgentQuestionSelection{
		QuestionSelection("Which auth method?", []string{"OAuth"}, nil),
		QuestionSelection("Which suites?", nil, &typed),
	}
	for i, got := range outcome.Deliver.GetAnswers() {
		if !proto.Equal(got, want[i]) {
			t.Fatalf("answer %d = %+v, want %+v", i, got, want[i])
		}
	}
}

func TestOptionNumbersReadsOnlyASendOfOptionNumbers(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []string
		ok   bool
	}{
		{"one number", "2", []string{"2"}, true},
		{"one number with whitespace around it", "  2 \n", []string{"2"}, true},
		{"numbers delimited by whitespace", "1 3\t2", []string{"1", "3", "2"}, true},
		{"numbers delimited by a comma and whitespace", "1, 2", []string{"1", "2"}, true},
		{"numbers delimited by a bare comma", "1,2", []string{"1", "2"}, true},
		{"a number with words after it", "2 please", nil, false},
		{"a numeral prefix as the card draws it", "2.", nil, false},
		{"a trailing comma", "1,", nil, false},
		{"an empty send", "   ", nil, false},
		{"plain text", "mutual TLS", nil, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got, ok := optionNumbers(tc.text)

			// Assert.
			if ok != tc.ok || !slices.Equal(got, tc.want) {
				t.Fatalf("optionNumbers(%q) = %v, %v, want %v, %v", tc.text, got, ok, tc.want, tc.ok)
			}
		})
	}
}

func TestASendOfOneOptionNumberPicksTheShownSingleSelectsOption(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Act.
	h.edit(QuestionEdit{Kind: EditType, Text: "2"})

	// Assert.
	open := h.openDraft()
	got := open.GetDrafts()[0]
	if !got.GetAnswered() || !slices.Equal(got.GetChosen(), []string{"API key"}) || got.OtherText != nil {
		t.Fatalf("draft = %+v, want API key picked, answered, nothing typed", got)
	}
	if open.GetActive() != 1 {
		t.Fatalf("active = %d, want the next unanswered question", open.GetActive())
	}
	records := h.recordsAt("info", "daemon.feed.question_draft")
	if len(records) != 1 || records[0].Context["by_number"] != true {
		t.Fatalf("records = %+v, want the edit recorded as picked by number", records)
	}
}

func TestASendOfAnOptionNumberReplacesASingleSelectsPickAndTypedText(t *testing.T) {
	tests := []struct {
		name  string
		given QuestionEdit
	}{
		{"an earlier pick", pickAuth("OAuth")},
		{"an earlier typed answer", QuestionEdit{Kind: EditType, Text: "mutual TLS"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the first question answered, then shown again.
			h := newHarness(t)
			h.openTwoQuestionAsk()
			h.edit(tc.given)
			h.edit(QuestionEdit{Kind: EditShow, Question: "Which auth method?"})

			// Act.
			h.edit(QuestionEdit{Kind: EditType, Text: "2"})

			// Assert.
			got := h.openDraft().GetDrafts()[0]
			if !slices.Equal(got.GetChosen(), []string{"API key"}) || got.OtherText != nil || !got.GetAnswered() {
				t.Fatalf("draft = %+v, want only API key, answered", got)
			}
		})
	}
}

func TestASendOfOptionNumbersSetsAMultiSelectsTicksInTheServedOrderAndAnswersIt(t *testing.T) {
	// Arrange: the multi-select shown, with a tick and a typed note the send replaces.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(QuestionEdit{Kind: EditShow, Question: "Which suites?"})
	h.edit(QuestionEdit{Kind: EditType, Text: "and the e2e suite"})
	h.edit(QuestionEdit{Kind: EditShow, Question: "Which suites?"})

	// Act.
	h.edit(QuestionEdit{Kind: EditType, Text: "2, 1 2"})

	// Assert.
	got := h.openDraft().GetDrafts()[1]
	if !slices.Equal(got.GetChosen(), []string{"daemon", "webapp"}) || got.OtherText != nil || !got.GetAnswered() {
		t.Fatalf("draft = %+v, want daemon and webapp ticked in served order, answered, nothing typed", got)
	}
}

func TestASendOfOptionNumbersAnsweringTheLastQuestionDeliversTheBatch(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(QuestionEdit{Kind: EditType, Text: "1"})

	// Act.
	outcome := h.edit(QuestionEdit{Kind: EditType, Text: "2"})

	// Assert.
	want := []*conversationv1.AgentQuestionSelection{
		QuestionSelection("Which auth method?", []string{"OAuth"}, nil),
		QuestionSelection("Which suites?", []string{"webapp"}, nil),
	}
	got := outcome.Deliver.GetAnswers()
	if len(got) != len(want) {
		t.Fatalf("answers = %+v, want %+v", got, want)
	}
	for i := range want {
		if !proto.Equal(got[i], want[i]) {
			t.Fatalf("answer %d = %+v, want %+v", i, got[i], want[i])
		}
	}
}

func TestASendOfOptionNumbersTheQuestionCannotTakeIsRefusedWithoutChangingTheDraft(t *testing.T) {
	tests := []struct {
		name    string
		text    string
		wantWhy string
	}{
		{"a number past the last option", "3", "the question offers no option numbered 3 of 2"},
		{"a zero", "0", "the question offers no option numbered 0 of 2"},
		{"a number too large to read", "99999999999999999999", "the question offers no option numbered 99999999999999999999 of 2"},
		{"one bad number among good ones", "1 3", "the question offers no option numbered 3 of 2"},
		{"two numbers on a single-select", "1, 2", "a single-select question takes one option number"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the single-select shown, already picked, then shown again.
			h := newHarness(t)
			h.openTwoQuestionAsk()
			h.edit(pickAuth("OAuth"))
			h.edit(QuestionEdit{Kind: EditShow, Question: "Which auth method?"})
			before := proto.Clone(h.openDraft())

			// Act.
			_, err := h.resolver.EditQuestionDraft(testWorkspace, "ask-1", QuestionEdit{Kind: EditType, Text: tc.text})

			// Assert.
			var unserved *UnservedQuestionValueError
			if !errors.As(err, &unserved) || unserved.Text != tc.text || unserved.Why != tc.wantWhy {
				t.Fatalf("err = %v, want an unserved value %q because %q", err, tc.text, tc.wantWhy)
			}
			if after := h.openDraft(); !proto.Equal(after, before) {
				t.Fatalf("draft changed from %+v to %+v on a refused send", before, after)
			}
			if len(h.recordsAt("debug", "daemon.feed.question_draft_refused")) != 1 {
				t.Fatal("the refused send left no record")
			}
		})
	}
}

func TestInServedOrderAnswersTheHeldLabelsInTheServedOrder(t *testing.T) {
	tests := []struct {
		name string
		held map[string]bool
		want []string
	}{
		{"nothing held", map[string]bool{}, nil},
		{"held out of order", map[string]bool{"c": true, "a": true}, []string{"a", "c"}},
		{"a label held false", map[string]bool{"a": false, "b": true}, []string{"b"}},
		{"a label never served", map[string]bool{"z": true, "b": true}, []string{"b"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := inServedOrder([]string{"a", "b", "c"}, tc.held)

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("inServedOrder = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestTheChosenLabelsAreOrderedThroughInServedOrder(t *testing.T) {
	// Arrange: a hand-rolled ordering that drifted from the helper fails here.
	source, err := os.ReadFile("questiondraft.go")
	if err != nil {
		t.Fatalf("read questiondraft.go: %v", err)
	}

	// Act.
	calls := strings.Count(string(source), "inServedOrder(")

	// Assert: the definition plus toggleLabel's and pickByNumber's calls.
	if calls != 3 {
		t.Fatalf("inServedOrder( appears %d times, want its definition and two call sites", calls)
	}
}
