package handler

// golden_test.go — THE CONTRACT TEST FOR THE WHOLE MAPPING.
//
// It drives every real captured fixture and the real session transcript through
// the handlers and asserts the two things that cannot be checked kind by kind:
//
//  1. ZERO `unparsed` residue. An unparsed entry means a line the reader could
//     not decode at all, and every fixture is a line the vendor actually wrote —
//     so one appearing here is a decoding regression, not a shape gap.
//  2. ZERO `unknown` residue. `unknown` means "we parsed it and do not model it",
//     and after the context-cut and api-error carriers landed there is no
//     record left in the corpus that legitimately reaches it. THE ALLOW-LIST IS
//     EMPTY ON PURPOSE: a recognizable kind degrading into residue is a producer
//     defect, and this is the assertion that makes it fail loudly instead of
//     quietly shrinking what the feed can show.
//
// The `vendor_specific` kinds are asserted as an EXPLICIT SET rather than merely
// counted, because that arm is the deliberate-withholding channel: a mapping that
// regressed into withholding something it used to convert would otherwise pass.

import (
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// fixtureDirs are the corpus directories written by the vendor's AGENT BINARY to
// the FILE plane, which is the only plane this producer reads.
//
// `stream/` is deliberately excluded: those are SDK stream probes, the SHIM's
// input, and feeding them to a file-plane converter would be testing a
// conversion that no production path performs.
var fixtureDirs = []string{
	"attachments",
	"content-blocks",
	"tool-inputs",
	"tool-results",
	"transcript-lines",
}

func TestGoldenCorpusProducesNoUnparsedResidue(t *testing.T) {
	// Arrange.
	entries := driveWholeCorpus(t)

	// Act.
	var offenders []string
	for _, e := range entries {
		if u := e.GetAgentUpdate().GetUnservedItem().GetUnparsed(); u != nil {
			offenders = append(offenders, u.GetSource()+" @"+u.GetParseError())
		}
	}

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("golden corpus produced %d unparsed entries, want 0:\n%s", len(offenders), strings.Join(offenders, "\n"))
	}
}

func TestGoldenCorpusProducesNoUnknownResidue(t *testing.T) {
	// Arrange. The allowed set is EMPTY: every pending carrier has landed.
	allowed := map[string]bool{}
	entries := driveWholeCorpus(t)

	// Act.
	var offenders []string
	for _, e := range entries {
		u := e.GetAgentUpdate().GetUnservedItem().GetUnknown()
		if u == nil || allowed[u.GetDiscriminator()] {
			continue
		}
		offenders = append(offenders, u.GetDiscriminatorField()+"="+u.GetDiscriminator())
	}

	// Assert.
	if len(offenders) != 0 {
		sort.Strings(offenders)
		t.Fatalf("golden corpus produced %d unknown entries, want 0 (a recognizable kind reaching residue is a producer defect):\n%s",
			len(offenders), strings.Join(dedupe(offenders), "\n"))
	}
}

func TestGoldenCorpusVendorSpecificKindsAreTheDeclaredSet(t *testing.T) {
	// Arrange. Every kind this producer deliberately withholds. A NEW kind here
	// is a mapping regression until it is added deliberately.
	want := map[string]bool{
		// CLI bookkeeping and machinery the harness writes about itself.
		"queue-operation": true, "last-prompt": true, "ai-title": true,
		"pr-link": true, "frame-link": true, "mode": true, "permission-mode": true,
		"file-history-snapshot": true, "file-history-delta": true,
		"system/local_command": true, "system/informational": true,
		"system/turn_duration": true, "system/stop_hook_summary": true,
		"system/away_summary": true, "system/scheduled_task_fire": true,
		"system/agents_killed": true, "system/model_refusal_fallback": true,
		"system/model_refusal_no_fallback": true,
		// Context-cut exclusions and other attachment machinery.
		"attachment/agent_listing_delta": true, "attachment/auto_mode": true,
		"attachment/command_permissions": true, "attachment/compact_file_reference": true,
		"attachment/context_tip": true, "attachment/date_change": true,
		"attachment/deferred_tools_delta": true, "attachment/edited_text_file": true,
		"attachment/file": true, "attachment/plan_mode_exit": true,
		"attachment/queued_command": true, "attachment/read_truncation_notice": true,
		"attachment/structured_output": true, "attachment/task_reminder": true,
		"attachment/ultra_effort_enter": true, "attachment/ultra_effort_exit": true,
		"attachment/ultrathink_effort": true,
		// The diagnostics attachment when no adjacent change was observed.
		"attachment/diagnostics": true,
		// R15: a file-plane prompt can never be a page line.
		"user_prompt": true, "user/meta": true,
		// A settle whose call is behind the cursor.
		"orphan_tool_result": true,
		// A content block kind this schema does not model.
		"content_block/fallback": true,
		// WORKFLOW IS KICKED THIS WAVE: the files are still discovered, tailed
		// and stored whole, but no workflow frame is produced for anyone to
		// consume yet.
		"workflow_journal/started": true, "workflow_journal/result": true,
	}
	entries := driveWholeCorpus(t)

	// Act.
	got := map[string]bool{}
	for _, kind := range vendorKinds(entries) {
		got[kind] = true
	}

	// Assert.
	var unexpected []string
	for kind := range got {
		if !want[kind] {
			unexpected = append(unexpected, kind)
		}
	}
	if len(unexpected) != 0 {
		sort.Strings(unexpected)
		t.Fatalf("undeclared vendor_specific kinds (each is either a mapping regression or a deliberate addition to declare):\n%s",
			strings.Join(unexpected, "\n"))
	}
}

func TestGoldenCorpusEveryEntryCarriesEnvelopeDuties(t *testing.T) {
	// Arrange.
	entries := driveWholeCorpus(t)
	if len(entries) == 0 {
		t.Fatal("the corpus produced no entries at all")
	}

	// Act + Assert: the four envelope duties hold for every entry, whatever arm
	// it landed on. A missing one is a store-side validation refusal at runtime,
	// so it must fail here instead.
	for _, e := range entries {
		if e.GetPlane().GetFile() == nil {
			t.Fatalf("entry write_id=%s does not name the FILE plane", e.GetWriteId())
		}
		if e.GetWriteId() == "" {
			t.Fatalf("entry upsert_key=%s carries no write_id", e.GetUpsertKey())
		}
		if e.GetUpsertKey() == "" {
			t.Fatalf("entry write_id=%s carries no upsert_key", e.GetWriteId())
		}
		if e.GetAgentUpdate() == nil {
			t.Fatalf("entry write_id=%s carries no agent_update", e.GetWriteId())
		}
		if e.GetAgentUpdate().GetAgentInfo() == nil {
			t.Fatalf("entry write_id=%s sets no agent_info arm", e.GetWriteId())
		}
	}
}

func TestGoldenCorpusWriteIdsAreUniquePerEntry(t *testing.T) {
	// Arrange. Two entries minted from one record must differ in their
	// discriminator; a collision would make the store absorb one as a replay of
	// the other and silently lose it.
	entries := driveWholeCorpus(t)

	// Act.
	seen := map[string]string{}
	var collisions []string
	for _, e := range entries {
		if prior, ok := seen[e.GetWriteId()]; ok {
			collisions = append(collisions, prior+" vs "+e.GetUpsertKey())
			continue
		}
		seen[e.GetWriteId()] = e.GetUpsertKey()
	}

	// Assert.
	if len(collisions) != 0 {
		t.Fatalf("write_id collisions would be absorbed as replays and lose a record:\n%s", strings.Join(collisions, "\n"))
	}
}

func TestGoldenCorpusIsDeterministicAcrossTwoRuns(t *testing.T) {
	// Arrange. Instants come from the file and write ids from the position, so a
	// re-read must mint byte-identical envelopes — that is what makes a replay
	// after a restart a no-op at the store rather than a duplicate.
	first := driveWholeCorpus(t)
	second := driveWholeCorpus(t)

	// Act + Assert.
	if len(first) != len(second) {
		t.Fatalf("two runs produced %d and %d entries; conversion is not deterministic", len(first), len(second))
	}
	for i := range first {
		if first[i].GetWriteId() != second[i].GetWriteId() {
			t.Fatalf("entry %d: write_id %s then %s; a re-read must mint the same identity",
				i, first[i].GetWriteId(), second[i].GetWriteId())
		}
		if first[i].GetUpsertKey() != second[i].GetUpsertKey() {
			t.Fatalf("entry %d: upsert_key %s then %s", i, first[i].GetUpsertKey(), second[i].GetUpsertKey())
		}
	}
}

func TestGoldenRealTranscriptProducesOnlyServableAndDeclaredResidue(t *testing.T) {
	// Arrange. The real captured session, driven as one file in file order — the
	// only fixture that exercises the call→result joins end to end, because it is
	// the only one where a tool_use and its tool_result sit in the same file.
	entries := driveRealTranscript(t)
	if len(entries) == 0 {
		t.Fatal("the real transcript produced no entries")
	}

	// Act.
	var unparsed, unknown int
	for _, e := range entries {
		item := e.GetAgentUpdate().GetUnservedItem()
		if item.GetUnparsed() != nil {
			unparsed++
		}
		if item.GetUnknown() != nil {
			unknown++
		}
	}

	// Assert.
	if unparsed != 0 || unknown != 0 {
		t.Fatalf("real transcript: unparsed=%d unknown=%d, want 0 and 0", unparsed, unknown)
	}
}

func TestGoldenRealTranscriptJoinsBothBashResultsToTheirCalls(t *testing.T) {
	// Arrange. The captured session makes two Bash calls and both return in the
	// same file, so both must settle as their OWN units rather than degrading to
	// orphan residue.
	entries := driveRealTranscript(t)

	// Act.
	settled := 0
	orphans := 0
	for _, e := range entries {
		if activityOf(e).GetBash().GetSuccess() != nil {
			settled++
		}
		if v := e.GetAgentUpdate().GetUnservedItem().GetVendorSpecific(); v != nil && v.GetKind() == "orphan_tool_result" {
			orphans++
		}
	}

	// Assert.
	if settled != 2 {
		t.Fatalf("settled bash units = %d, want 2 (the join from result to call regressed)", settled)
	}
	if orphans != 0 {
		t.Fatalf("orphan tool results = %d, want 0 in a file carrying both calls and both results", orphans)
	}
}

// ---- drivers ----

// driveWholeCorpus runs every file-plane fixture through the handler that reads
// its kind, plus the real transcript.
//
// EACH FIXTURE FILE IS ITS OWN CONVERTER, because each is an excerpt from a
// different real session: sharing one converter across them would invent joins
// between unrelated sessions and make the suite pass for the wrong reason.
func driveWholeCorpus(t *testing.T) []*storev1.StoreEntry {
	t.Helper()
	var all []*storev1.StoreEntry
	root := corpusRoot(t)
	for _, dir := range fixtureDirs {
		for _, path := range fixtureFiles(t, filepath.Join(root, dir)) {
			all = append(all, driveFixture(t, dir, path)...)
		}
	}
	for _, path := range fixtureFiles(t, filepath.Join(root, "journals")) {
		all = append(all, driveJournal(t, path)...)
	}
	all = append(all, driveSidechain(t, filepath.Join(root, "sidechain", "agent-aef975b7bc3422d4b.jsonl"))...)
	all = append(all, driveSpools(t, filepath.Join(root, "spools"))...)
	all = append(all, driveRealTranscript(t)...)
	return all
}

// driveFixture reads one transcript-line fixture. A `tool-inputs` fixture is a
// BARE tool_use BLOCK rather than a whole line (the manifest documents it as
// "tool_use block from an assistant line"), so it is wrapped back into the
// assistant line it was extracted from — driving it as a top-level line would
// test a shape the vendor never writes.
func driveFixture(t *testing.T, dir, path string) []*storev1.StoreEntry {
	t.Helper()
	text := readFixture(t, path)
	if dir == "tool-inputs" {
		text = wrapBlocksAsAssistantLines(t, text)
	}
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext(path, "session-"+filepath.Base(path))
	return h.Handle(framesFrom(t, text), ctx)
}

// wrapBlocksAsAssistantLines rebuilds the assistant line a bare content block was
// extracted from, so the block reaches the converter the way production sees it.
func wrapBlocksAsAssistantLines(t *testing.T, text string) string {
	t.Helper()
	var out []string
	for i, line := range strings.Split(strings.TrimSpace(text), "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		out = append(out, `{"type":"assistant","uuid":"wrapped-`+itoaTest(i)+
			`","isSidechain":false,"timestamp":"2026-07-23T00:00:00.000Z","message":{"id":"msg_wrapped_`+
			itoaTest(i)+`","role":"assistant","content":[`+line+`]}}`)
	}
	return strings.Join(out, "\n")
}

func driveJournal(t *testing.T, path string) []*storev1.StoreEntry {
	t.Helper()
	h := NewWorkflowJournalHandler(testLogger(t))
	ctx := &Context{
		Path: path, SessionID: "journal-session", MainAgentID: "journal-session",
		Kind: tail.KindWorkflowJournal, RunID: "wf_golden", TaskID: "w1golden", FileID: "dev:2",
	}
	return h.Handle(framesFrom(t, readFixture(t, path)), ctx)
}

func driveSidechain(t *testing.T, path string) []*storev1.StoreEntry {
	t.Helper()
	h := NewAgentTranscriptHandler(testLogger(t))
	ctx := &Context{
		Path: path, SessionID: "owning-session", MainAgentID: "owning-session",
		AgentID: "aef975b7bc3422d4b", Kind: tail.KindAgentTranscript, FileID: "dev:3",
	}
	return h.Handle(framesFrom(t, readFixture(t, path)), ctx)
}

// driveSpools reads the shell spools as raw bytes, which is what a spool IS: one
// unstructured chunk, not JSONL.
func driveSpools(t *testing.T, dir string) []*storev1.StoreEntry {
	t.Helper()
	var all []*storev1.StoreEntry
	for _, path := range spoolFiles(t, dir) {
		if !strings.HasPrefix(filepath.Base(path), "bash") {
			// `agent.output` is a backgrounded subagent's TRANSCRIPT, read by the
			// sidechain converter, not by the shell one.
			continue
		}
		h := NewShellOutputHandler(testLogger(t))
		ctx := &Context{
			Path: path, SessionID: "spool-session", MainAgentID: "spool-session",
			AgentID: "spool-session", TaskID: "b1golden", Kind: tail.KindShellSpool, FileID: "dev:4",
		}
		raw := readFixture(t, path)
		all = append(all, h.Handle([]tail.Frame{{Raw: []byte(raw), Offset: 0}}, ctx)...)
	}
	return all
}

func driveRealTranscript(t *testing.T) []*storev1.StoreEntry {
	t.Helper()
	path := realTranscriptPath(t)
	session := strings.TrimSuffix(filepath.Base(path), ".jsonl")
	h := NewSessionTranscriptHandler(testLogger(t))
	return h.Handle(framesFrom(t, readFixture(t, path)), sessionContext(path, session))
}

// realTranscriptPath locates the one captured session under projects/.
func realTranscriptPath(t *testing.T) string {
	t.Helper()
	var found string
	root := projectsRoot(t)
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".jsonl") && found == "" {
			found = path
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk %s: %v", root, err)
	}
	if found == "" {
		t.Fatalf("no captured transcript under %s", root)
	}
	return found
}

// ---- fixture IO ----

func fixtureFiles(t *testing.T, dir string) []string {
	t.Helper()
	matches, err := filepath.Glob(filepath.Join(dir, "*.jsonl"))
	if err != nil {
		t.Fatalf("glob %s: %v", dir, err)
	}
	if len(matches) == 0 {
		t.Fatalf("no fixtures under %s; the corpus is the contract and an empty directory would make this suite vacuous", dir)
	}
	sort.Strings(matches)
	return matches
}

func spoolFiles(t *testing.T, dir string) []string {
	t.Helper()
	matches, err := filepath.Glob(filepath.Join(dir, "*.output"))
	if err != nil {
		t.Fatalf("glob %s: %v", dir, err)
	}
	if len(matches) == 0 {
		t.Fatalf("no spool fixtures under %s", dir)
	}
	sort.Strings(matches)
	return matches
}

func readFixture(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	return string(data)
}

func dedupe(values []string) []string {
	seen := map[string]bool{}
	var out []string
	for _, v := range values {
		if !seen[v] {
			seen[v] = true
			out = append(out, v)
		}
	}
	return out
}

func itoaTest(n int) string {
	if n == 0 {
		return "0"
	}
	var digits []byte
	for n > 0 {
		digits = append([]byte{byte('0' + n%10)}, digits...)
		n /= 10
	}
	return string(digits)
}
