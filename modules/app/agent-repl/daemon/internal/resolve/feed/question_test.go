package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE QUESTION CARD: a batch of one to four, radios or checkboxes PER QUESTION.
// The answers ride the settled row, because a cold repaint has nothing else to
// draw the choices from.

// pose sends one question frame.
func (h *harness) pose(askID string, result any) {
	h.t.Helper()
	q := &conversationv1.AgentQuestion{Id: &conversationv1.AgentQuestionId{Value: askID}}
	switch r := result.(type) {
	case *conversationv1.AgentQuestionStart:
		q.Result = &conversationv1.AgentQuestion_Start{Start: r}
	case *conversationv1.AgentQuestionSuccess:
		q.Result = &conversationv1.AgentQuestion_Success{Success: r}
	case *conversationv1.AgentQuestionFailure:
		q.Result = &conversationv1.AgentQuestion_Failure{Failure: r}
	}
	h.resolver.OnQuestion(testWorkspace, mainAgent(), q, nil, nil)
}

// questionCard finds the choice card on the root feed.
func (h *harness) questionCard() *frontendv1.FeedQuestion {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if card := row.GetQuestion(); card != nil {
			return card
		}
	}
	h.t.Fatal("no question card on the root feed")
	return nil
}

// twoQuestionBatch mixes a pick-one with a pick-any, which one batch may do.
func twoQuestionBatch() *conversationv1.AgentQuestionBatch {
	description := "the ordinary path"
	return &conversationv1.AgentQuestionBatch{
		Questions: []*conversationv1.AgentQuestionAsked{
			{
				Question: &conversationv1.AgentQuestionText{Text: "Which auth method?"},
				Header:   "Auth method",
				Choices: &conversationv1.AgentQuestionAsked_SingleSelect{
					SingleSelect: &conversationv1.AgentQuestionSingleSelect{
						Options: []*conversationv1.AgentQuestionOption{
							{
								Label:       &conversationv1.AgentQuestionOptionLabel{Label: "OAuth"},
								Description: description,
							},
							{Label: &conversationv1.AgentQuestionOptionLabel{Label: "API key"}},
						},
					},
				},
			},
			{
				Question: &conversationv1.AgentQuestionText{Text: "Which suites?"},
				Header:   "Suites",
				Choices: &conversationv1.AgentQuestionAsked_MultiSelect{
					MultiSelect: &conversationv1.AgentQuestionMultiSelect{
						Options: []*conversationv1.AgentQuestionOption{
							{Label: &conversationv1.AgentQuestionOptionLabel{Label: "daemon"}},
							{Label: &conversationv1.AgentQuestionOptionLabel{Label: "webapp"}},
						},
					},
				},
			},
		},
	}
}

func TestAnOpenAskDrawsEveryQuestionInTheBatchsOrder(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch:     twoQuestionBatch(),
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	card := h.questionCard()
	if len(card.GetQuestions()) != 2 {
		t.Fatalf("questions = %d, want 2", len(card.GetQuestions()))
	}
	if card.GetQuestions()[0].GetHeader().GetText() != "Auth method" {
		t.Fatalf("first header = %q", card.GetQuestions()[0].GetHeader().GetText())
	}
	if card.GetOpen() == nil {
		t.Fatalf("state = %T, want open", card.GetState())
	}
}

func TestTheSelectionModeIsPerQuestion(t *testing.T) {
	// Arrange, Act: one batch mixing radios and checkboxes.
	h := newHarness(t)
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch:     twoQuestionBatch(),
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: read per question, never lifted to the batch.
	questions := h.questionCard().GetQuestions()
	if questions[0].GetSingleSelect() == nil {
		t.Fatalf("first options = %T, want single_select", questions[0].GetOptions())
	}
	if questions[1].GetMultiSelect() == nil {
		t.Fatalf("second options = %T, want multi_select", questions[1].GetOptions())
	}
}

func TestTheQuestionTextIsDrawnAndEchoedVerbatim(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch:     twoQuestionBatch(),
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: the text is the identity the producer recognizes, not a label.
	if got := h.questionCard().GetQuestions()[0].GetText().GetText(); got != "Which auth method?" {
		t.Fatalf("question text = %q, want it verbatim", got)
	}
}

func TestAnOptionWithNoDescriptionDrawsNoLine(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch:     twoQuestionBatch(),
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: absence draws no line, never an empty one.
	options := h.questionCard().GetQuestions()[0].GetSingleSelect().GetOptions()
	if options[0].GetDescription().GetText() != "the ordinary path" {
		t.Fatalf("first description = %q", options[0].GetDescription().GetText())
	}
	if options[1].GetDescription() != nil {
		t.Fatalf("second description = %+v, want unset", options[1].GetDescription())
	}
}

func TestTheAnsweredCardCarriesTheChoicesInTheBatchsOrder(t *testing.T) {
	// Arrange: the producer's answers arrive UNORDERED, each naming its
	// question.
	h := newHarness(t)
	batch := twoQuestionBatch()
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch: batch, StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.pose("ask-1", &conversationv1.AgentQuestionSuccess{
		Batch: batch,
		Outcome: &conversationv1.AgentQuestionSuccess_Answered{
			Answered: &conversationv1.AgentQuestionAnswers{
				Answers: []*conversationv1.AgentQuestionSelection{
					{
						Question: &conversationv1.AgentQuestionText{Text: "Which suites?"},
						Chosen: []*conversationv1.AgentQuestionChoice{
							{Label: &conversationv1.AgentQuestionOptionLabel{Label: "daemon"}},
							{Label: &conversationv1.AgentQuestionOptionLabel{Label: "webapp"}},
						},
					},
					{
						Question: &conversationv1.AgentQuestionText{Text: "Which auth method?"},
						Chosen: []*conversationv1.AgentQuestionChoice{
							{Label: &conversationv1.AgentQuestionOptionLabel{Label: "OAuth"}},
						},
					},
				},
			},
		},
	})

	// Assert: joined by the question text, drawn in the batch's order.
	answers := h.questionCard().GetAnswered().GetAnswers()
	if len(answers) != 2 {
		t.Fatalf("answers = %d, want 2", len(answers))
	}
	if answers[0].GetHeader().GetText() != "Auth method" || len(answers[0].GetChosen()) != 1 {
		t.Fatalf("first answer = %+v, want the auth question's", answers[0])
	}
	if answers[1].GetHeader().GetText() != "Suites" || len(answers[1].GetChosen()) != 2 {
		t.Fatalf("second answer = %+v, want the suites question's", answers[1])
	}
}

func TestTheFreeTextEscapeIsCarriedWhenTheUserTypedOne(t *testing.T) {
	// Arrange: an ask ALWAYS offers the escape, whether or not the agent asked.
	h := newHarness(t)
	batch := twoQuestionBatch()
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch: batch, StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.pose("ask-1", &conversationv1.AgentQuestionSuccess{
		Batch: batch,
		Outcome: &conversationv1.AgentQuestionSuccess_Answered{
			Answered: &conversationv1.AgentQuestionAnswers{
				Answers: []*conversationv1.AgentQuestionSelection{{
					Question: &conversationv1.AgentQuestionText{Text: "Which auth method?"},
					FreeText: &conversationv1.AgentQuestionFreeText{Text: "mTLS, actually"},
				}},
			},
		},
	})

	// Assert.
	answers := h.questionCard().GetAnswered().GetAnswers()
	if got := answers[0].GetOtherText().GetText(); got != "mTLS, actually" {
		t.Fatalf("other_text = %q", got)
	}
	if len(answers[0].GetChosen()) != 0 {
		t.Fatalf("chosen = %v, want empty when the answer was free text alone", answers[0].GetChosen())
	}
}

func TestAnUnansweredAskExpiresRatherThanPendingForever(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	batch := twoQuestionBatch()
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch: batch, StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act: nobody answering IS an answer to a legitimate ask.
	h.pose("ask-1", &conversationv1.AgentQuestionSuccess{
		Batch: batch,
		Outcome: &conversationv1.AgentQuestionSuccess_Unanswered{
			Unanswered: &conversationv1.AgentQuestionUnanswered{},
		},
	})

	// Assert.
	if h.questionCard().GetExpired() == nil {
		t.Fatalf("state = %T, want expired", h.questionCard().GetState())
	}
}

func TestAnAskThatCouldNotBePutToTheUserExpires(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.pose("ask-1", &conversationv1.AgentQuestionFailure{})

	// Assert: nothing was chosen and nothing waits.
	if h.questionCard().GetExpired() == nil {
		t.Fatalf("state = %T, want expired", h.questionCard().GetState())
	}
}

func TestTheDecisionUpsertsTheSameQuestionRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	batch := twoQuestionBatch()
	h.pose("ask-1", &conversationv1.AgentQuestionStart{
		Batch: batch, StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.pose("ask-1", &conversationv1.AgentQuestionSuccess{
		Batch: batch,
		Outcome: &conversationv1.AgentQuestionSuccess_Unanswered{
			Unanswered: &conversationv1.AgentQuestionUnanswered{},
		},
	})

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 1 {
		t.Fatalf("rows = %d, want the one card upserted", len(rows))
	}
}

func TestAQuestionFrameWithNoAskIdentityIsRefusedLoudly(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnQuestion(testWorkspace, mainAgent(), &conversationv1.AgentQuestion{
		Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{}},
	}, nil, nil)

	// Assert.
	if !h.hasRecord("error", "daemon.feed.question_without_identity") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.question_without_identity", h.records())
	}
}
