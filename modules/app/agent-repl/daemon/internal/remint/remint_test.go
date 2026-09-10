package remint_test

import (
	"encoding/json"
	"strings"
	"testing"

	"claude-repld/internal/remint"
)

// prefixMint is a readable stand-in for the production minter: a test about the
// MAPPING should be able to state what it expects, and a random id would make
// every assertion an indirection through the mapper itself.
func prefixMint(old string) string { return "new-" + old }

// newMapper builds a mapper over the readable minter.
func newMapper(t *testing.T) *remint.Mapper {
	t.Helper()
	return remint.New("parent-session", "child-session", prefixMint)
}

// remintLines re-mints a whole transcript, failing the test on an error.
func remintLines(t *testing.T, m *remint.Mapper, body string) string {
	t.Helper()
	got, err := m.Lines([]byte(body))
	if err != nil {
		t.Fatalf("Lines() = %v, want nil", err)
	}
	return string(got)
}

// field reads one top-level string field out of a re-minted record.
func field(t *testing.T, line, name string) string {
	t.Helper()
	var record map[string]any
	if err := json.Unmarshal([]byte(line), &record); err != nil {
		t.Fatalf("Unmarshal(%q) = %v", line, err)
	}
	value, ok := record[name].(string)
	if !ok {
		t.Fatalf("record %q states no string %s", line, name)
	}
	return value
}

func TestLinesKeepsACrossRecordParentUuidLink(t *testing.T) {
	// Arrange: the child record points at the first record's uuid.
	m := newMapper(t)
	body := `{"type":"user","uuid":"u1","sessionId":"parent-session"}` + "\n" +
		`{"type":"assistant","uuid":"u2","parentUuid":"u1","sessionId":"parent-session"}` + "\n"

	// Act.
	lines := strings.Split(strings.TrimRight(remintLines(t, m, body), "\n"), "\n")

	// Assert: the link still names the first record, under its NEW uuid.
	if got, want := field(t, lines[1], "parentUuid"), field(t, lines[0], "uuid"); got != want {
		t.Fatalf("parentUuid = %q, want the re-minted uuid %q of the record it links to", got, want)
	}
}

func TestLinesKeepsAToolUsePairedWithItsToolResult(t *testing.T) {
	// Arrange: the assistant's tool_use block and the user's tool_result that
	// settles it. The sidecar keys BOTH as activity:<tool_use_id>, so the pair
	// must survive the port or the settle lands on a row nothing opened.
	m := newMapper(t)
	body := `{"type":"assistant","uuid":"u1","message":{"id":"msg_1","content":[{"type":"tool_use","id":"toolu_1","name":"Read","input":{"file_path":"/x"}}]}}` + "\n" +
		`{"type":"user","uuid":"u2","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_1","content":"ok"}]}}` + "\n"

	// Act.
	lines := strings.Split(strings.TrimRight(remintLines(t, m, body), "\n"), "\n")

	// Assert.
	if !strings.Contains(lines[0], `"id":"new-toolu_1"`) {
		t.Fatalf("the tool_use block = %q, want its id re-minted", lines[0])
	}
	if !strings.Contains(lines[1], `"tool_use_id":"new-toolu_1"`) {
		t.Fatalf("the tool_result = %q, want it still paired with the re-minted call", lines[1])
	}
}

func TestLinesRewritesTheSessionIdToTheChilds(t *testing.T) {
	// Arrange: the per-record sessionId diverges from the file's own id in real
	// transcripts, and a fork's copy must still name exactly one session.
	m := newMapper(t)
	body := `{"type":"user","uuid":"u1","sessionId":"some-other-session"}` + "\n"

	// Act.
	got := remintLines(t, m, body)

	// Assert.
	if want := "child-session"; field(t, strings.TrimRight(got, "\n"), "sessionId") != want {
		t.Fatalf("sessionId = %q, want the child's own %q", got, want)
	}
}

func TestLinesRewritesTheAssistantMessageId(t *testing.T) {
	// Arrange: a text block's activity key is `<message.id>:<index>`, so a
	// message id carried over unchanged re-keys the parent's rows.
	m := newMapper(t)
	body := `{"type":"assistant","uuid":"u1","message":{"id":"msg_1","content":[{"type":"text","text":"hi"}]}}` + "\n"

	// Act.
	got := remintLines(t, m, body)

	// Assert.
	if !strings.Contains(got, `"id":"new-msg_1"`) {
		t.Fatalf("Lines() = %q, want the message id re-minted", got)
	}
}

func TestLinesLeavesAToolInputsOwnIdAlone(t *testing.T) {
	// Arrange: an `id` inside a tool INPUT names something in the user's world,
	// not a vendor identity; rewriting it would corrupt the call.
	m := newMapper(t)
	body := `{"type":"assistant","uuid":"u1","message":{"id":"msg_1","content":[{"type":"tool_use","id":"toolu_1","name":"Jobs","input":{"id":"job-42"}}]}}` + "\n"

	// Act.
	got := remintLines(t, m, body)

	// Assert.
	if !strings.Contains(got, `"id":"job-42"`) {
		t.Fatalf("Lines() = %q, want the tool input's own id left alone", got)
	}
}

func TestLinesPassesAnUnrewrittenLineThroughByteForByte(t *testing.T) {
	tests := []struct {
		name string
		body string
	}{
		{name: "a blank line", body: "\n"},
		{name: "a record with no identity in it", body: `{"type":"summary", "summary":  "x"}` + "\n"},
		{name: "well-formed JSON that is not an object", body: `"just a string"` + "\n"},
		{name: "no trailing newline", body: `{"type":"summary"}`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m := newMapper(t)

			// Act.
			got := remintLines(t, m, tc.body)

			// Assert.
			if got != tc.body {
				t.Fatalf("Lines(%q) = %q, want it byte for byte", tc.body, got)
			}
		})
	}
}

func TestLinesRefusesAMalformedRecord(t *testing.T) {
	tests := []struct {
		name string
		body string
	}{
		{name: "truncated object", body: `{"uuid":"u1"` + "\n"},
		{name: "not JSON at all", body: "body\n"},
		{name: "two values on one line", body: `{"uuid":"u1"} {"uuid":"u2"}` + "\n"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m := newMapper(t)

			// Act.
			_, err := m.Lines([]byte(tc.body))

			// Assert: surfaced, never skipped — a dropped record loses
			// conversation and a carried one files the parent's identity in the
			// child's book.
			if err == nil {
				t.Fatalf("Lines(%q) = nil error, want a refusal", tc.body)
			}
		})
	}
}

func TestLinesNamesTheOffendingLine(t *testing.T) {
	// Arrange.
	m := newMapper(t)
	body := `{"uuid":"u1"}` + "\n" + `{"uuid":` + "\n"

	// Act.
	_, err := m.Lines([]byte(body))

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "line 2") {
		t.Fatalf("Lines() = %v, want an error naming line 2", err)
	}
}

func TestDocumentRemintsASubagentMetaFile(t *testing.T) {
	// Arrange: `toolUseId` IS the subagent's AgentId under the cross-plane
	// minting rule, and it is the same id the parent's spawning call carries.
	m := newMapper(t)

	// Act.
	got, err := m.Document([]byte(`{"agentType":"Explore","toolUseId":"toolu_1","spawnDepth":1}`))

	// Assert.
	if err != nil {
		t.Fatalf("Document() = %v, want nil", err)
	}
	if !strings.Contains(string(got), `"toolUseId":"new-toolu_1"`) {
		t.Fatalf("Document() = %q, want the spawning call's id re-minted", got)
	}
}

func TestPathSegmentRemintsTheIdentityANameCarries(t *testing.T) {
	tests := []struct {
		name    string
		segment string
		want    string
	}{
		{name: "a subagent transcript", segment: "agent-abc.jsonl", want: "agent-new-abc.jsonl"},
		{name: "its meta companion", segment: "agent-abc.meta.json", want: "agent-new-abc.meta.json"},
		{name: "a workflow run directory", segment: "wf_9.9", want: "new-wf_9.9"},
		{name: "the subagents directory itself", segment: "subagents", want: "subagents"},
		{name: "a journal", segment: "journal.jsonl", want: "journal.jsonl"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m := newMapper(t)

			// Act.
			got := m.PathSegment(tc.segment)

			// Assert.
			if got != tc.want {
				t.Fatalf("PathSegment(%q) = %q, want %q", tc.segment, got, tc.want)
			}
		})
	}
}

func TestPathSegmentAgreesWithTheRecordFieldThatNamesIt(t *testing.T) {
	// Arrange: the `agent-<id>` file name and the `agentId` a sidechain record
	// states are ONE identity; they must move together.
	m := newMapper(t)
	line := remintLines(t, m, `{"uuid":"u1","agentId":"abc","isSidechain":true}`)

	// Act.
	got := m.PathSegment("agent-abc.jsonl")

	// Assert.
	if want := "agent-" + field(t, line, "agentId") + ".jsonl"; got != want {
		t.Fatalf("PathSegment() = %q, want %q", got, want)
	}
}

func TestIDIsMemoizedPerOldIdentity(t *testing.T) {
	// Arrange: the mapping is a PURE FUNCTION of the old id — the same old id
	// answers the same new one however often it is seen.
	m := remint.New("parent-session", "child-session", nil)

	// Act.
	first, second := m.ID("toolu_1"), m.ID("toolu_1")

	// Assert.
	if first != second {
		t.Fatalf("ID() = %q then %q, want one answer per old identity", first, second)
	}
}

func TestIDMapsTheParentsSessionIdToTheChilds(t *testing.T) {
	// Arrange.
	m := remint.New("parent-session", "child-session", nil)

	// Act.
	got := m.ID("parent-session")

	// Assert.
	if got != "child-session" {
		t.Fatalf("ID(parent session) = %q, want the child's own id", got)
	}
}

func TestIDNeverMintsForAnAbsentIdentity(t *testing.T) {
	// Arrange.
	m := remint.New("parent-session", "child-session", nil)

	// Act.
	got := m.ID("")

	// Assert.
	if got != "" {
		t.Fatalf("ID(\"\") = %q, want the empty identity to stay absent", got)
	}
}

func TestDefaultMintKeepsTheShapeOfTheOldIdentity(t *testing.T) {
	tests := []struct {
		name  string
		old   string
		check func(minted string) bool
	}{
		{
			name:  "a record uuid stays uuid-shaped",
			old:   "f58479e7-4980-4320-ae52-92d2b3b129b8",
			check: func(minted string) bool { return len(minted) == 36 && strings.Count(minted, "-") == 4 },
		},
		{
			name:  "a tool_use id keeps its vendor prefix",
			old:   "toolu_0126vtXAKTJNo3UgfhWRefyz",
			check: func(minted string) bool { return strings.HasPrefix(minted, "toolu_") },
		},
		{
			name:  "a workflow run keeps its wf_ prefix",
			old:   "wf_01HZ",
			check: func(minted string) bool { return strings.HasPrefix(minted, "wf_") },
		},
		{
			name:  "an opaque agent locator keeps its length",
			old:   "aef975b7bc3422d4b",
			check: func(minted string) bool { return len(minted) == len("aef975b7bc3422d4b") },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := remint.DefaultMint(tc.old)

			// Assert.
			if got == tc.old || !tc.check(got) {
				t.Fatalf("DefaultMint(%q) = %q, want a fresh id of the same shape", tc.old, got)
			}
		})
	}
}

func TestLinesCarriesNoIdentityOfTheParentAcross(t *testing.T) {
	// Arrange: the whole point — nothing the parent was identified by may
	// appear in the child's copy.
	m := remint.New("parent-session", "child-session", nil)
	body := `{"type":"assistant","uuid":"f58479e7-4980-4320-ae52-92d2b3b129b8",` +
		`"parentUuid":"cbb9b86c-c6ac-46ce-897c-8d6b8c7bad72","sessionId":"parent-session",` +
		`"promptId":"5733a072-6422-45a2-9080-69f351059404","agentId":"aef975b7bc3422d4b",` +
		`"message":{"id":"msg_011CdHgrCKHqet2g2nBdh5K4","content":[{"type":"tool_use","id":"toolu_01LCRu14ksjtQZNtwZwCdYNu"}]}}` + "\n"
	parentIdentities := []string{
		"f58479e7-4980-4320-ae52-92d2b3b129b8",
		"cbb9b86c-c6ac-46ce-897c-8d6b8c7bad72",
		"parent-session",
		"5733a072-6422-45a2-9080-69f351059404",
		"aef975b7bc3422d4b",
		"msg_011CdHgrCKHqet2g2nBdh5K4",
		"toolu_01LCRu14ksjtQZNtwZwCdYNu",
	}

	// Act.
	got := remintLines(t, m, body)

	// Assert.
	for _, identity := range parentIdentities {
		if strings.Contains(got, identity) {
			t.Fatalf("the ported record still carries the parent's %q: %s", identity, got)
		}
	}
}
