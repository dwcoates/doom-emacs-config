package convert

// attachment_test.go — hooks, and the context the vendor SILENTLY pulled in.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

func attachmentLineOf(uuid, body string) string {
	return `{"type":"attachment","uuid":"` + uuid + `","isSidechain":false,"timestamp":"` + ts1 +
		`","attachment":` + body + `}`
}

func TestHookOutcomeKindsAreAllKeptWholeAsUnservedItems(t *testing.T) {
	// Arrange. THE STREAM PLANE OWNS THE SERVED HOOK ROW (ruling 2026-09-04):
	// the two planes are handed disjoint identity material, so a hook converted
	// on both drew two rows nothing could reconcile. Every FAILING outcome kind
	// is still kept whole and unserved. `hook_success` is not in this table: it
	// has its own subject in neverpersist_test.go. NONE of these is persisted
	// since the residue ruling (2026-09-13) — the reader withholds every residue
	// arm — but the classification asserted here is what the counts name.
	cases := []string{"hook_blocking_error", "hook_non_blocking_error", "hook_cancelled"}
	for _, kind := range cases {
		t.Run(kind, func(t *testing.T) {
			c := newTestConverter(t)
			body := `{"type":"` + kind + `","hookName":"PostToolUse:Edit","toolUseID":"toolu_gated",` +
				`"hookEvent":"PostToolUse","command":"/h.sh","exitCode":0,"durationMs":45,` +
				`"blockingError":{"blockingError":"refused","command":"/h.sh"}}`

			// Act.
			entries := convertLines(t, c, attachmentLineOf("h1", body))

			// Assert.
			if len(entries) != 1 {
				t.Fatalf("entries = %d, want 1", len(entries))
			}
			if got := vendorKindOf(entries[0]); got != "attachment/"+kind {
				t.Fatalf("kind = %q, want the record kept whole under %q", got, "attachment/"+kind)
			}
		})
	}
}

func TestAHookAttachmentNeverReachesAPage(t *testing.T) {
	// Arrange. The whole point of the ruling: the file plane writes NO served
	// hook row, so the stream's row stands alone and a reader sees one firing
	// once.
	c := newTestConverter(t)
	body := `{"type":"hook_blocking_error","hookName":"PostToolUse:Edit","toolUseID":"toolu_gated",` +
		`"hookEvent":"PostToolUse","blockingError":{"blockingError":"the webapp suite failed","command":"/run.sh"}}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("h1", body))

	// Assert.
	if entries[0].GetAgentUpdate().GetServeableFrame() != nil {
		t.Fatalf("a hook attachment reached a page: %v", entries[0].GetAgentUpdate().GetServeableFrame())
	}
}

func TestAHookAttachmentIsKeyedByItsOwnRecordUuid(t *testing.T) {
	// Arrange. The record's uuid is the key, which is also what the shim and
	// this reader would collapse onto if both ever stored the same line.
	c := newTestConverter(t)
	body := `{"type":"hook_cancelled","hookName":"PreToolUse:Read","toolUseID":"toolu_g","hookEvent":"PreToolUse","command":"/h.sh"}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("h1", body))

	// Assert.
	if got := entries[0].GetUpsertKey(); got != "residue:h1" {
		t.Fatalf("upsert_key = %q, want the attachment record's own uuid key", got)
	}
}

func TestBlockingHookCarriesItsRefusalTextIntoTheStoredRecord(t *testing.T) {
	// Arrange. The refusal text is what a reader needs to understand why the
	// gated action did not happen, so it must survive the withhold VERBATIM
	// rather than being summarized away with the row.
	c := newTestConverter(t)
	body := `{"type":"hook_blocking_error","hookName":"PostToolUse:Edit","toolUseID":"toolu_gated",` +
		`"hookEvent":"PostToolUse","blockingError":{"blockingError":"the webapp suite failed","command":"/run.sh"}}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("h1", body))

	// Assert.
	raw := entries[0].GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetRaw().AsMap()
	attachment, _ := raw["attachment"].(map[string]any)
	blocking, _ := attachment["blockingError"].(map[string]any)
	if got := blocking["blockingError"]; got != "the webapp suite failed" {
		t.Fatalf("blockingError = %v, want the hook's stated reason carried verbatim", got)
	}
	if got := blocking["command"]; got != "/run.sh" {
		t.Fatalf("command = %v, want the command that ran", got)
	}
}

func TestTwoFiringsAroundOneCallDoNotCollapseOntoOneRow(t *testing.T) {
	// Arrange. A PreToolUse and a PostToolUse gate the SAME call. Keying them by
	// the gated call alone would make the second overwrite the first.
	c := newTestConverter(t)
	pre := attachmentLineOf("h1", `{"type":"hook_cancelled","hookName":"PreToolUse:Read","toolUseID":"toolu_g","hookEvent":"PreToolUse","command":"/h.sh"}`)
	post := attachmentLineOf("h2", `{"type":"hook_cancelled","hookName":"PostToolUse:Read","toolUseID":"toolu_g","hookEvent":"PostToolUse","command":"/h.sh"}`)

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

func TestTwoFiringsOfOneHookNameUnderOneToolUseIdStayTwoItems(t *testing.T) {
	// Arrange. THE CASE THE OLD IDENTITY LOST. The vendor reuses one toolUseID
	// across every firing that gates the same call — testdata/captures/hook-blocked
	// carries FOUR PreToolUse:Bash firings under one of them — so keying on
	// `hook:<hookName>:<toolUseID>` left one row where there were four facts.
	c := newTestConverter(t)
	body := `{"type":"hook_cancelled","hookName":"PreToolUse:Bash","toolUseID":"toolu_same","hookEvent":"PreToolUse","command":"/h.sh"}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("h1", body), attachmentLineOf("h2", body))

	// Assert.
	if len(entries) != 2 {
		t.Fatalf("entries = %d, want one per firing", len(entries))
	}
	if entries[0].GetUpsertKey() == entries[1].GetUpsertKey() {
		t.Fatalf("both firings landed on %q; three of the capture's four would be erased", entries[0].GetUpsertKey())
	}
}

func TestAHookThatPrintedNothingInventsNoOutputFields(t *testing.T) {
	// Arrange. UNSET when it printed nothing: the record is carried exactly as
	// the vendor wrote it, so a consumer never reads empty streams the hook
	// never produced.
	c := newTestConverter(t)
	body := `{"type":"hook_cancelled","hookName":"PreToolUse:Read","toolUseID":"toolu_g","hookEvent":"PreToolUse","command":"/h.sh","exitCode":0}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("h1", body))

	// Assert.
	raw := entries[0].GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetRaw().AsMap()
	attachment, _ := raw["attachment"].(map[string]any)
	for _, field := range []string{"stdout", "stderr"} {
		if _, ok := attachment[field]; ok {
			t.Fatalf("the stored record carries an invented %q the hook never printed", field)
		}
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

// TestDiagnosticsWithNoObservedChangeAreRecordedAtDebug pins the reclassified
// orphan diagnostics record. With no remembered change unit the report follows
// no write or edit in this reader's window — a cursor-resumed reader legitimately
// sees the report after its causing change scrolled past the cursor. The findings
// are stored whole as residue, which is correct, so the trace is debug rather
// than a warn that floods a cold re-scan. An in-window change always sets
// lastChangeUnit, so a report whose change IS in-window never reaches this branch.
func TestDiagnosticsWithNoObservedChangeAreRecordedAtDebug(t *testing.T) {
	// Arrange: a diagnostics report with no preceding write or edit.
	c, sink := loggedConverter(t)
	body := `{"type":"diagnostics","isNew":true,"files":[{"uri":"/p/x.ts",` +
		`"diagnostics":[{"message":"boom","severity":"Error",` +
		`"range":{"start":{"line":1,"character":0},"end":{"line":1,"character":3}},` +
		`"source":"typescript","code":"2304"}]}]}`

	// Act.
	entries := convertLines(t, c, attachmentLineOf("diag-1", body))

	// Assert: the residue is kept whole...
	if len(entries) != 1 || vendorKindOf(entries[0]) != "attachment/diagnostics" {
		t.Fatalf("entries = %v, want exactly one attachment/diagnostics residue", allKeys(entries))
	}
	// ...and only the severity drops.
	if got := levelForMessage(t, sink, "follow no write or edit this reader observed"); got != "debug" {
		t.Fatalf("the orphaned diagnostics record was recorded at %q, want debug (benign on a re-scan)", got)
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

func TestTheContextBudgetWarningReadsTheSameFieldsAsTheShim(t *testing.T) {
	// The shim reads `content`, else `text`, and the mocked vendor writes
	// `content`; a reader that skipped either would leave the warning undrawn.
	tests := []struct {
		name string
		body string
		want string
	}{
		{name: "content", body: `{"type":"context_budget_warning","content":"Context low (9% remaining)"}`, want: "Context low (9% remaining)"},
		{name: "text", body: `{"type":"context_budget_warning","text":"Context low (9% remaining)"}`, want: "Context low (9% remaining)"},
		{name: "content wins over text", body: `{"type":"context_budget_warning","content":"from content","text":"from text"}`, want: "from content"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)

			// Act.
			entries := convertLines(t, c, attachmentLineOf("cb1", tc.body))

			// Assert.
			if len(entries) != 1 {
				t.Fatalf("entries = %d, want 1: keys=%v", len(entries), allKeys(entries))
			}
			warning := entries[0].GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetContextBudgetWarning()
			if got := warning.GetText(); got != tc.want {
				t.Fatalf("text = %q, want %q", got, tc.want)
			}
		})
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

// convertAttachmentInBook converts one attachment line as though it were read
// from a transcript whose book is `agent`, at a fixed file position — the two
// inputs the injected-context write identity and key both derive from. It lets a
// test place the SAME vendor record under two different books, which is exactly
// what a skill inherited into a subagent's sidechain does.
func convertAttachmentInBook(t *testing.T, c *Converter, line, agent string, offset int64) *storev1.StoreEntry {
	t.Helper()
	at := Attribution{
		VendorSessionID: "session-uuid",
		MainAgentID:     "session-uuid",
		AgentID:         agent,
		Path:            "/p/projects/proj/" + agent + ".jsonl",
		FileID:          "dev:" + agent,
		Offset:          offset,
	}
	entries := c.Line(decode(t, line), at, nil)
	if len(entries) != 1 {
		t.Fatalf("want exactly one entry, got %d", len(entries))
	}
	return entries[0]
}

// TestInjectedSkillsKeyIsScopedToItsBook is the realtest-1 defect (ledger row
// 46): the vendor copies one invoked_skills record — uuid and all — into every
// sidechain that inherits the skill, so the same uuid recurred under two books.
// Keyed by the uuid alone the identical key named two books, and a re-ingest of
// the second file asked the store to MOVE the row (upsert_changes_identity),
// losing the write. The key must be scoped to its book so the two are two rows.
func TestInjectedSkillsKeyIsScopedToItsBook(t *testing.T) {
	// Arrange: one invoked_skills record, one uuid, inherited into two subagent
	// books.
	body := `{"type":"invoked_skills","skills":[{"name":"workspace","path":"userSettings:workspace","content":"# body"}]}`
	line := attachmentLineOf("1403ebb6", body)

	// Act: the same record read from two different sidechain transcripts.
	inA := convertAttachmentInBook(t, newTestConverter(t), line, "toolu_A", 0)
	inB := convertAttachmentInBook(t, newTestConverter(t), line, "toolu_B", 0)

	// Assert: two distinct keys, each deterministically naming its own book, so
	// nothing ever tries to move a row from one book to another.
	if inA.GetUpsertKey() == inB.GetUpsertKey() {
		t.Fatalf("the same skill under two books shares the key %q; the store would refuse the second as an identity move", inA.GetUpsertKey())
	}
	if got := inA.GetUpsertKey(); got != ActivityKey(ContextInjectedUnitID("skills", "toolu_A", "1403ebb6")) {
		t.Fatalf("book A key = %q, want it scoped to book A", got)
	}
	if got := pageLine(inA).GetPageAgentId().GetValue(); got != "toolu_A" {
		t.Fatalf("book A page agent = %q, want toolu_A", got)
	}
	if got := pageLine(inB).GetPageAgentId().GetValue(); got != "toolu_B" {
		t.Fatalf("book B page agent = %q, want toolu_B", got)
	}
}

// TestInjectedSkillsKeyIsStableAcrossReconversion proves the invariant a
// re-ingest rests on: converting the SAME transcript line again mints the
// identical upsert key, book, AND write id, so the store absorbs it by write id
// rather than ever seeing an identity move.
func TestInjectedSkillsKeyIsStableAcrossReconversion(t *testing.T) {
	// Arrange
	body := `{"type":"invoked_skills","skills":[{"name":"workspace","content":"# body"}]}`
	line := attachmentLineOf("1403ebb6", body)

	// Act: two independent converters read the same file line at the same book
	// and offset, as a fresh sidecar re-reading the transcript does.
	first := convertAttachmentInBook(t, newTestConverter(t), line, "toolu_A", 4096)
	again := convertAttachmentInBook(t, newTestConverter(t), line, "toolu_A", 4096)

	// Assert
	if first.GetUpsertKey() != again.GetUpsertKey() {
		t.Fatalf("re-conversion key %q != %q; a re-ingest would not settle in place", again.GetUpsertKey(), first.GetUpsertKey())
	}
	if pageLine(first).GetPageAgentId().GetValue() != pageLine(again).GetPageAgentId().GetValue() {
		t.Fatalf("re-conversion moved the book %q -> %q", pageLine(first).GetPageAgentId().GetValue(), pageLine(again).GetPageAgentId().GetValue())
	}
	if first.GetWriteId() != again.GetWriteId() {
		t.Fatalf("re-conversion write id %q != %q; the store would not absorb the duplicate", again.GetWriteId(), first.GetWriteId())
	}
}

// TestInjectedMemoryKeyIsScopedToItsBook is the same defect for the memory
// injection, which the vendor inherits into sidechains exactly as it does a
// skill, so the fix is applied consistently to both.
func TestInjectedMemoryKeyIsScopedToItsBook(t *testing.T) {
	// Arrange
	body := `{"type":"nested_memory","path":"/p/CLAUDE.md",` +
		`"content":{"path":"/p/CLAUDE.md","type":"Project","content":"@./AGENTS.md\n"}}`
	line := attachmentLineOf("mem-1", body)

	// Act
	inA := convertAttachmentInBook(t, newTestConverter(t), line, "toolu_A", 0)
	inB := convertAttachmentInBook(t, newTestConverter(t), line, "toolu_B", 0)

	// Assert
	if inA.GetUpsertKey() == inB.GetUpsertKey() {
		t.Fatalf("the same memory file under two books shares the key %q", inA.GetUpsertKey())
	}
	if got := inA.GetUpsertKey(); got != ActivityKey(ContextInjectedUnitID("memory", "toolu_A", "mem-1")) {
		t.Fatalf("book A key = %q, want it scoped to book A", got)
	}
	if got := pageLine(inA).GetPageAgentId().GetValue(); got != "toolu_A" {
		t.Fatalf("book A page agent = %q, want toolu_A", got)
	}
}
