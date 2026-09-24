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

func TestPeerMessageIsEmittedAsAPeerPageLine(t *testing.T) {
	// Arrange. A message another Claude session sent in carries origin.kind
	// "peer" and isMeta:true. It must be recognized before the isMeta withhold,
	// or an adopted conversation loses it entirely.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, peerMessageLine("u1", "Explore", "found it", "Another Claude session sent a message", false))

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	peer := peerLineOf(entries[0])
	if peer == nil {
		t.Fatal("a peer message must be a served peer page line, not withheld as meta")
	}
}

func TestPeerMessageIsNotWithheldAsMeta(t *testing.T) {
	// Arrange. Regression: the isMeta branch would have swallowed a peer record.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, peerMessageLine("u1", "Explore", "hi", "envelope", false))

	// Assert.
	if got := vendorKindOf(entries[0]); got == "user/meta" {
		t.Fatal("a peer message must not be withheld as a meta record")
	}
}

func TestPeerMessageSenderIsTheOriginFrom(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, peerMessageLine("u1", "Plan", "the body", "envelope", false))

	// Assert.
	if got := peerLineOf(entries[0]).GetSender(); got != "Plan" {
		t.Fatalf("sender = %q, want Plan", got)
	}
}

func TestPeerMessagePrefersOriginBody(t *testing.T) {
	// Arrange. The vendor states origin.body; it wins over the record text.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, peerMessageLine("u1", "Plan", "the real body", "the envelope text", false))

	// Assert.
	if got := peerLineOf(entries[0]).GetBody(); got != "the real body" {
		t.Fatalf("body = %q, want the vendor-stated origin.body", got)
	}
}

func TestPeerMessageFallsBackToRecordTextForBody(t *testing.T) {
	// Arrange. No origin.body, so the record's own text is the body.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, peerMessageLine("u1", "Plan", "", "the envelope text", false))

	// Assert.
	if got := peerLineOf(entries[0]).GetBody(); got != "the envelope text" {
		t.Fatalf("body = %q, want the record text fallback", got)
	}
}

func TestSubagentHandbackIsAPeerMessageToo(t *testing.T) {
	// Arrange. A hand-back sets origin.handback but is still origin.kind "peer",
	// so it renders as the same peer bubble.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, peerMessageLine("u1", "child", "done", "envelope", true))

	// Assert.
	if peerLineOf(entries[0]) == nil {
		t.Fatal("a subagent hand-back must be a served peer page line")
	}
}

func TestPeerMessageIdentityIsDerivedFromTheRecordUUID(t *testing.T) {
	// Arrange. The stream and file planes both key the same vendor record
	// peer:<uuid> and spell the uuid into PeerMessage.id, so their rows collapse.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, peerMessageLine("rec-uuid-77", "Explore", "hi", "envelope", false))

	// Assert.
	if got := peerLineOf(entries[0]).GetId(); got != "rec-uuid-77" {
		t.Fatalf("id = %q, want the record uuid rec-uuid-77", got)
	}
	if got := entries[0].GetUpsertKey(); got != PeerKey("rec-uuid-77") {
		t.Fatalf("upsert key = %q, want %q", got, PeerKey("rec-uuid-77"))
	}
}

func TestPeerMessageRecipientMatchesItsBook(t *testing.T) {
	// Arrange. The store refuses a page line whose page_agent_id disagrees with
	// the peer message's recipient, so they must be equal — the session's main
	// agent.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, peerMessageLine("u1", "Explore", "hi", "envelope", false))

	// Assert.
	line := pageLine(entries[0])
	book := line.GetPageAgentId().GetValue()
	recipient := peerLineOf(entries[0]).GetAgent().GetValue()
	if book == "" || book != recipient {
		t.Fatalf("book = %q, recipient = %q; the store requires them equal", book, recipient)
	}
}

func TestGenuineHumanPromptStillEmittedNotAsPeer(t *testing.T) {
	// Arrange. Regression: an adopted human prompt (no peer origin) still emits
	// as a prompt page line, unaffected by the peer branch.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, externalPromptLine("u1", "cli", "where were we?"))

	// Assert.
	if pageLine(entries[0]).GetAgentItem().GetAgentPrompt() == nil {
		t.Fatal("a genuine human prompt must still be an AgentPrompt page line")
	}
	if peerLineOf(entries[0]) != nil {
		t.Fatal("a genuine human prompt must not become a peer message")
	}
}
