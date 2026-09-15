package convert

// user_test.go — R15 holds for agent-repl's own prompts (sdk-cli, withheld) and
// is crossed for adopted external prompts (any other entrypoint, emitted).

import "testing"

func TestAgentReplsOwnSdkCliPromptIsStillWithheld(t *testing.T) {
	// Arrange. R15 regression lock: a prompt agent-repl submitted through its
	// SDK carries entrypoint "sdk-cli". The daemon minted its TurnId and drew
	// its bubble live, so the file-plane copy must stay withheld as
	// vendor_specific — emitting it would draw the bubble twice.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, sdkPromptLine("u1", "what does this do?"))

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	if pageLine(entries[0]) != nil {
		t.Fatal("agent-repl's own sdk-cli prompt must never be a page line")
	}
	if got := vendorKindOf(entries[0]); got != "user_prompt" {
		t.Fatalf("kind = %q, want user_prompt", got)
	}
}

func TestAdoptedExternalCliPromptIsEmittedAsAPageLine(t *testing.T) {
	// Arrange. A prompt typed in interactive Claude Code carries entrypoint
	// "cli". It was never submitted through agent-repl, so the daemon never drew
	// it — it must be emitted as a real prompt page line, or an adopted
	// conversation shows answers with no prompts above them.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, externalPromptLine("u1", "cli", "where did we leave off?"))

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	prompt := pageLine(entries[0]).GetAgentItem().GetAgentPrompt()
	if prompt == nil {
		t.Fatal("an adopted external prompt must be a served prompt page line")
	}
	said := prompt.GetSaid().GetContent().GetBlocks()
	if len(said) != 1 || said[0].GetText().GetText() != "where did we leave off?" {
		t.Fatalf("said = %v, want the one text block the person typed", said)
	}
	if got := prompt.GetOrigin(); got != promptOriginUnspecified {
		t.Fatalf("origin = %v, want UNSPECIFIED (drawn as the plain \"You\" author)", got)
	}
}

func TestSidechainCommissionIsWithheldEvenWithCliEntrypoint(t *testing.T) {
	// Arrange. A subagent's opening user message carries entrypoint "cli" like an
	// interactive prompt, but it is an agent-addressed commission the daemon
	// draws at both ends. It must stay withheld — isSidechain is what tells it
	// from a top-level adopted prompt.
	c := newTestConverter(t)
	line := `{"type":"user","uuid":"u1","isSidechain":true,"entrypoint":"cli","timestamp":"` + ts1 +
		`","message":{"role":"user","content":[{"type":"text","text":"do the thing"}]}}`

	// Act.
	entries := convertLines(t, c, line)

	// Assert.
	if pageLine(entries[0]) != nil {
		t.Fatal("a sidechain commission must never be an emitted prompt page line")
	}
	if got := vendorKindOf(entries[0]); got != "user_prompt" {
		t.Fatalf("kind = %q, want user_prompt", got)
	}
}

func TestAdoptedExternalPromptIdentityIsDerivedFromTheRecordUUID(t *testing.T) {
	// Arrange. The prompt has no daemon-minted turn id, so replay must be
	// idempotent: the identity is derived from the record's own uuid, so a
	// re-ingest supersedes its own row rather than growing a second bubble.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, externalPromptLine("rec-uuid-42", "cli", "hi"))

	// Assert.
	prompt := pageLine(entries[0]).GetAgentItem().GetAgentPrompt()
	if got := prompt.GetId().GetValue(); got != "rec-uuid-42" {
		t.Fatalf("turn id = %q, want the record uuid rec-uuid-42", got)
	}
	if got := entries[0].GetUpsertKey(); got != PromptKey("rec-uuid-42") {
		t.Fatalf("upsert key = %q, want %q", got, PromptKey("rec-uuid-42"))
	}
}

func TestAdoptedExternalPromptRecipientMatchesItsBook(t *testing.T) {
	// Arrange. The store refuses a page line whose page_agent_id disagrees with
	// the prompt's recipient, so the emitted prompt's recipient must be the book
	// it is filed in — the session's main agent.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, externalPromptLine("u1", "cli", "hi"))

	// Assert.
	line := pageLine(entries[0])
	book := line.GetPageAgentId().GetValue()
	recipient := line.GetAgentItem().GetAgentPrompt().GetAgent().GetValue()
	if book == "" || book != recipient {
		t.Fatalf("book = %q, recipient = %q; the store requires them equal", book, recipient)
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
