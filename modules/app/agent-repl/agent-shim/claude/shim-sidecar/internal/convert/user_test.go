package convert

// user_test.go — R15: a file-plane prompt is never a page line.

import "testing"

func TestFilePlaneUserPromptIsNeverAPageLine(t *testing.T) {
	// Arrange. R15: AgentPrompt carries a TurnId and a PromptOrigin, both
	// DAEMON-MINTED. A file reader holds neither, and inventing them would put a
	// fabricated turn identity on the wire — so no history page can regrow a fake
	// prompt bubble from this producer.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, promptLine("u1", "what does this do?"))

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	if pageLine(entries[0]) != nil {
		t.Fatal("a file-plane prompt must never be a page line")
	}
	if got := vendorKindOf(entries[0]); got != "user_prompt" {
		t.Fatalf("kind = %q, want user_prompt", got)
	}
}

func TestHarnessInjectedUserRecordIsNotSomethingAPersonSaid(t *testing.T) {
	// Arrange. A system reminder or attachment carrier rides a user record
	// because the vendor files it there; drawing it as prose would put words in
	// a person's mouth.
	c := newTestConverter(t)
	line := `{"type":"user","uuid":"u1","isSidechain":false,"isMeta":true,"timestamp":"` + ts1 +
		`","message":{"role":"user","content":[{"type":"text","text":"<system-reminder>x</system-reminder>"}]}}`

	// Act.
	entries := convertLines(t, c, line)

	// Assert.
	if got := vendorKindOf(entries[0]); got != "user/meta" {
		t.Fatalf("kind = %q, want user/meta", got)
	}
}

func TestToolResultCarrierEmitsNoEmptyPrompt(t *testing.T) {
	// Arrange. The vendor files tool results under the USER's role. Emitting an
	// empty prompt beside them would spend a page slot on a message nobody wrote.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_r", "Read", `{"file_path":"/f"}`))
	result := toolResultLine("u1", "toolu_r", ts2, `[{"type":"text","text":"c"}]`,
		`{"type":"text","file":{"filePath":"/f","content":"c","numLines":1,"totalLines":1}}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	for _, e := range entries {
		if vendorKindOf(e) == "user_prompt" {
			t.Fatal("a pure tool-result carrier must not also emit a prompt")
		}
	}
}

func TestKeepaliveBitIsReadFromThePromptEvenThoughItIsWithheld(t *testing.T) {
	// Arrange. The prompt itself is never served, but it still OPENS the
	// keep-alive turn — the bit must be read before the record is withheld.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, promptLine("u1", KeepaliveMarker+"ping"))

	// Assert.
	if got := vendorKindOf(entries[0]); got != "user_prompt" {
		t.Fatalf("kind = %q, want the prompt still withheld", got)
	}
	if !c.keepalive {
		t.Fatal("the keep-alive bit must be set by the marker even though the prompt is withheld")
	}
}
