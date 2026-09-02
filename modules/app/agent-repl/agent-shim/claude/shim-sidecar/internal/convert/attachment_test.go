package convert

// attachment_test.go — hooks, and the context the vendor SILENTLY pulled in.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func attachmentLineOf(uuid, body string) string {
	return `{"type":"attachment","uuid":"` + uuid + `","isSidechain":false,"timestamp":"` + ts1 +
		`","attachment":` + body + `}`
}

func TestHookOutcomeArmsTable(t *testing.T) {
	// Arrange. THE ARM IS THE OUTCOME. A succeeded hook draws nothing, a failing
	// one draws a card, and a BLOCKING one is a refusal the user must understand.
	cases := []struct {
		kind string
		arm  func(*conversationv1.AgentHook) bool
	}{
		{kind: "hook_success", arm: func(h *conversationv1.AgentHook) bool { return h.GetSucceeded() != nil }},
		{kind: "hook_blocking_error", arm: func(h *conversationv1.AgentHook) bool { return h.GetBlockingError() != nil }},
		{kind: "hook_non_blocking_error", arm: func(h *conversationv1.AgentHook) bool { return h.GetNonBlockingError() != nil }},
		{kind: "hook_cancelled", arm: func(h *conversationv1.AgentHook) bool { return h.GetCancelled() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.kind, func(t *testing.T) {
			c := newTestConverter(t)
			body := `{"type":"` + tc.kind + `","hookName":"PostToolUse:Edit","toolUseID":"toolu_gated",` +
				`"hookEvent":"PostToolUse","command":"/h.sh","exitCode":0,"durationMs":45,` +
				`"blockingError":{"blockingError":"refused","command":"/h.sh"}}`

			// Act.
			entries := convertLines(t, c, attachmentLineOf("h1", body))

			// Assert.
			if len(entries) != 1 {
				t.Fatalf("entries = %d, want 1", len(entries))
			}
			hook := activityOf(entries[0]).GetHook()
			if hook == nil {
				t.Fatalf("%s did not reach the hook arm", tc.kind)
			}
			if !tc.arm(hook) {
				t.Fatalf("%s did not reach its own outcome arm", tc.kind)
			}
		})
	}
}

func TestBlockingHookCarriesItsRefusalText(t *testing.T) {
	// Arrange. The refusal text is THE WHOLE POINT of that arm: it is what the
	// user reads to understand why the gated action did not happen.
	c := newTestConverter(t)
	body := `{"type":"hook_blocking_error","hookName":"PostToolUse:Edit","toolUseID":"toolu_gated",` +
		`"hookEvent":"PostToolUse","blockingError":{"blockingError":"the webapp suite failed","command":"/run.sh"}}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("h1", body))

	// Assert.
	blocking := activityOf(entries[0]).GetHook().GetBlockingError()
	if got := blocking.GetBlockingText(); got != "the webapp suite failed" {
		t.Fatalf("blocking_text = %q, want the hook's stated reason", got)
	}
	if got := blocking.GetCommand(); got != "/run.sh" {
		t.Fatalf("command = %q, want the command that ran", got)
	}
}

func TestTwoFiringsAroundOneCallDoNotCollapseOntoOneRow(t *testing.T) {
	// Arrange. A PreToolUse and a PostToolUse gate the SAME call. Keying them by
	// the gated call alone would make the second overwrite the first.
	c := newTestConverter(t)
	pre := attachmentLineOf("h1", `{"type":"hook_success","hookName":"PreToolUse:Read","toolUseID":"toolu_g","hookEvent":"PreToolUse","command":"/h.sh"}`)
	post := attachmentLineOf("h2", `{"type":"hook_success","hookName":"PostToolUse:Read","toolUseID":"toolu_g","hookEvent":"PostToolUse","command":"/h.sh"}`)

	// Act.
	entries := convertLines(t, c, pre, post)

	// Assert.
	if len(entries) != 2 {
		t.Fatalf("entries = %d, want 2", len(entries))
	}
	if entries[0].GetUpsertKey() == entries[1].GetUpsertKey() {
		t.Fatalf("two firings share the key %q; the second would overwrite the first", entries[0].GetUpsertKey())
	}
}

func TestHookThatPrintedNothingLeavesOutputUnset(t *testing.T) {
	// Arrange. UNSET when it printed nothing, which is different from empty
	// streams a consumer would draw as a card with blank output.
	c := newTestConverter(t)
	body := `{"type":"hook_success","hookName":"PreToolUse:Read","toolUseID":"toolu_g","hookEvent":"PreToolUse","command":"/h.sh","exitCode":0}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("h1", body))

	// Assert.
	if activityOf(entries[0]).GetHook().GetSucceeded().Output != nil {
		t.Fatal("a hook that printed nothing must leave output UNSET")
	}
}

func TestInjectedMemoryIsSurfacedRatherThanSilent(t *testing.T) {
	// Arrange. The vendor pulls a memory file in with NO tool call announcing it,
	// so without this the user cannot see what shaped the agent's behavior.
	c := newTestConverter(t)
	body := `{"type":"nested_memory","path":"/p/CLAUDE.md",` +
		`"content":{"path":"/p/CLAUDE.md","type":"Project","content":"@./AGENTS.md\n"}}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("m1", body))

	// Assert.
	memory := activityOf(entries[0]).GetContextInjected().GetMemory()
	if memory == nil {
		t.Fatal("an injected memory file must reach the context_injected arm")
	}
	if got := memory.GetPath(); got != "/p/CLAUDE.md" {
		t.Fatalf("path = %q, want the vendor's own path", got)
	}
	if got := memory.GetContent(); got != "@./AGENTS.md\n" {
		t.Fatalf("content = %q, want it injected verbatim", got)
	}
}

func TestDynamicSkillDeltaNamesSkillsWithoutInventingTheirBodies(t *testing.T) {
	// Arrange. A discovery delta names the DIRECTORY and the names, not each
	// file's body — so path and content must stay UNSET rather than be invented.
	c := newTestConverter(t)
	body := `{"type":"dynamic_skill","skillDir":"/s","skillNames":["graphify","profile"]}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("s1", body))

	// Assert.
	skills := activityOf(entries[0]).GetContextInjected().GetSkills().GetSkills()
	if len(skills) != 2 {
		t.Fatalf("skills = %d, want 2", len(skills))
	}
	for _, skill := range skills {
		if skill.Content != nil {
			t.Fatalf("skill %q must not carry an invented body", skill.GetName())
		}
		if skill.Path != nil {
			t.Fatalf("skill %q must not carry an invented path", skill.GetName())
		}
	}
}

func TestInvokedSkillsAttachmentCarriesTheBodyItActuallyHas(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	body := `{"type":"invoked_skills","skills":[{"name":"workspace","path":"userSettings:workspace","content":"# body"}]}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("s1", body))

	// Assert.
	skills := activityOf(entries[0]).GetContextInjected().GetSkills().GetSkills()
	if len(skills) != 1 {
		t.Fatalf("skills = %d, want 1", len(skills))
	}
	if got := skills[0].GetContent(); got != "# body" {
		t.Fatalf("content = %q, want the injected body", got)
	}
}

func TestAttachmentWithNoAttachmentObjectIsUnknown(t *testing.T) {
	// Arrange. It parsed, so it is not unparsed; we cannot say what it is.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, `{"type":"attachment","uuid":"a1","isSidechain":false,"timestamp":"`+ts1+`"}`)

	// Assert.
	unknown := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown == nil || unknown.GetDiscriminatorField() != "attachment" {
		t.Fatalf("want unknown keyed on the missing attachment object, got %v", unknown)
	}
}

func TestDiagnosticSeverityTable(t *testing.T) {
	// Arrange. The LSP's closed scalar set.
	cases := []struct {
		vendor string
		want   conversationv1.AgentDiagnosticSeverity
	}{
		{vendor: "Error", want: conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_ERROR},
		{vendor: "Warning", want: conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_WARNING},
		{vendor: "Information", want: conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_INFORMATION},
		{vendor: "Hint", want: conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_HINT},
		{vendor: "Nonsense", want: conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_UNSPECIFIED},
	}

	// Act + Assert.
	for _, tc := range cases {
		t.Run(tc.vendor, func(t *testing.T) {
			if got := diagnosticSeverity(tc.vendor); got != tc.want {
				t.Fatalf("severity(%q) = %v, want %v", tc.vendor, got, tc.want)
			}
		})
	}
}

func TestTheContextBudgetWarningLandsAsAPageLineOfTheAgentsBook(t *testing.T) {
	// Arrange. A FILE-PLANE FACT WITH NO STREAM PRODUCER: it exists only as an
	// attachment line in the agent's transcript, so the sidecar is its only
	// producer (landing 4 moved it off SessionUpdate, which nothing on the live
	// session stream ever wrote).
	c := newTestConverter(t)
	body := `{"type":"context_budget_warning","text":"Context low (23% remaining)"}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("cb1", body))

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1: keys=%v", len(entries), allKeys(entries))
	}
	line := entries[0].GetAgentUpdate().GetServeableFrame()
	if line == nil {
		t.Fatalf("the warning must be a PAGE LINE of the agent's book: %v", entries[0])
	}
	warning := line.GetAgentItem().GetAgentFrame().GetUpdate().GetContextBudgetWarning()
	if warning == nil {
		t.Fatalf("the warning did not land on AgentUpdate.context_budget_warning: %v", line.GetAgentItem())
	}
	if got := warning.GetText(); got != "Context low (23% remaining)" {
		t.Fatalf("text = %q, want the vendor's sentence verbatim", got)
	}
}

func TestTheContextBudgetWarningIsKeyedByItsOwnRecord(t *testing.T) {
	// Arrange. It is INSTANTANEOUS with no lifecycle: two warnings in one session
	// are two facts, and collapsing them onto one key would leave only the last.
	c := newTestConverter(t)
	body := `{"type":"context_budget_warning","text":"Context low"}`

	// Act.
	entries := convertLines(t, c,
		attachmentLineOf("cb1", body),
		attachmentLineOf("cb2", body),
	)

	// Assert.
	if len(entries) != 2 {
		t.Fatalf("entries = %d, want one per record", len(entries))
	}
	if entries[0].GetUpsertKey() == entries[1].GetUpsertKey() {
		t.Fatalf("two warnings share the key %q; the later would erase the earlier", entries[0].GetUpsertKey())
	}
	if got := entries[0].GetUpsertKey(); got != SessionKey("context_budget_warning", "cb1") {
		t.Fatalf("upsert_key = %q, want the record's own session key", got)
	}
}

func TestAContextBudgetWarningWithNoTextIsStoredRatherThanDrawnEmpty(t *testing.T) {
	// Arrange. The record's whole content IS the sentence, so one without it has
	// nothing to draw — and landing an empty warning would put a blank line in
	// the footer rather than reporting the gap.
	c := newTestConverter(t)
	body := `{"type":"context_budget_warning"}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("cb1", body))

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	if entries[0].GetAgentUpdate().GetServeableFrame() != nil {
		t.Fatal("a warning with no text must not reach a page")
	}
	if got := entries[0].GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetKind(); got != "attachment/context_budget_warning" {
		t.Fatalf("kind = %q, want the record stored whole", got)
	}
}
