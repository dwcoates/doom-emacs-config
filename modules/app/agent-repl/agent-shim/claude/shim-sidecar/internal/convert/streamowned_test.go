package convert

// streamowned_test.go — the file plane authors no part of a stream-owned unit.
//
// An ask is the whole set today: the shim gates it and holds the user's answer
// as a repeated `chosen`, while the transcript states that answer only in the
// vendor's comma-joined form. Every test here pins one half of the drop.

import "testing"

const askInput = `{"questions":[{"question":"Which suites?","header":"Suites","multiSelect":true,` +
	`"options":[{"label":"Unit"},{"label":"Integration"}]}]}`

func TestAskUserQuestionCallIsNotConvertedByTheFilePlane(t *testing.T) {
	// Arrange. The ask's announcement is the SHIM's: a file-plane start frame
	// arriving after the shim's settle re-opened an answered ask.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_ask", "AskUserQuestion", askInput)))

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("an AskUserQuestion call produced %d file-plane entries, want 0 (the stream plane owns the ask)", len(entries))
	}
}

func TestAskUserQuestionResultIsNotConvertedByTheFilePlane(t *testing.T) {
	// Arrange. The transcript answers with ONE comma-joined string per question,
	// so a settle minted here would supersede the shim's structured one with a
	// single label where the user picked two.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_ask", "AskUserQuestion", askInput))
	result := toolResultLine("u1", "toolu_ask", ts2, `[{"type":"text","text":"Answered."}]`,
		`{"answers":{"Which suites?":"Unit, Integration"}}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("an AskUserQuestion result produced %d file-plane entries, want 0 (the stream plane owns the ask)", len(entries))
	}
}

func TestAskUserQuestionResultIsDroppedRatherThanFiledAsAnOrphan(t *testing.T) {
	// Arrange. A dropped CALL must still be remembered: a result whose call this
	// reader never observed lands as vendor_specific residue, and reporting a
	// deliberately-dropped unit that way would be a false modelling gap.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_ask", "AskUserQuestion", askInput))
	result := toolResultLine("u1", "toolu_ask", ts2, `[{"type":"text","text":"Answered."}]`,
		`{"answers":{"Which suites?":"Unit"}}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	for _, e := range entries {
		if e.GetAgentUpdate().GetUnservedItem() != nil {
			t.Fatalf("the ask's result landed as residue %v, want it dropped as a stream-owned unit", e.GetAgentUpdate().GetUnservedItem())
		}
	}
}
