package convert

// question_test.go — pins the settleQuestion / questionAnswers path, reached
// only when an AskUserQuestion call actually settles; the existing activity
// tests exercise the announcement (Start) but never the result.

import "testing"

func TestSettleQuestionLandsAnAnsweredOutcomeUnderTheSameKey(t *testing.T) {
	// Arrange: the ask's start frame and its settled result must upsert the
	// SAME question row, keyed by the tool_use_id.
	c := newTestConverter(t)
	input := `{"questions":[{"question":"Which?","header":"Pick","multiSelect":false,` +
		`"options":[{"label":"A"},{"label":"B"}]}]}`
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_ask", "AskUserQuestion", input))
	result := toolResultLine("u1", "toolu_ask", ts2, `[{"type":"text","text":"answered"}]`,
		`{"answers":{"Which?":"A"}}`)

	// Act
	entries := convertLines(t, c, call, result)

	// Assert
	terminal := lastEntryByKey(t, entries, QuestionKey("toolu_ask"))
	answered := frameOf(terminal).GetUpdate().GetQuestion().GetSuccess().GetAnswered()
	if answered == nil {
		t.Fatal("a settled ask with answers must resolve AgentQuestionSuccess_Answered")
	}
	if len(answered.GetAnswers()) != 1 {
		t.Fatalf("answers = %d, want 1", len(answered.GetAnswers()))
	}
	if got := answered.GetAnswers()[0].GetChosen()[0].GetLabel().GetLabel(); got != "A" {
		t.Fatalf("chosen label = %q, want A", got)
	}
}

func TestQuestionAnswersReadsAFreeTextSelectionFromTheListForm(t *testing.T) {
	// Arrange: the list-shaped answers form carries free text separately from
	// any chosen label, and must not fabricate a chosen option for it.
	result := map[string]any{
		"answers": []any{
			map[string]any{"question": "Anything else?", "free_text": "yes, ship it"},
		},
	}

	// Act
	got := questionAnswers(result)

	// Assert
	if len(got.GetAnswers()) != 1 {
		t.Fatalf("answers = %d, want 1", len(got.GetAnswers()))
	}
	selection := got.GetAnswers()[0]
	if selection.GetFreeText().GetText() != "yes, ship it" {
		t.Fatalf("FreeText = %q, want %q", selection.GetFreeText().GetText(), "yes, ship it")
	}
	if len(selection.GetChosen()) != 0 {
		t.Fatalf("Chosen = %+v, want none for a pure free-text answer", selection.GetChosen())
	}
}
