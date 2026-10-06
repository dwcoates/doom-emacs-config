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
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// goldenSpoolRun is the spawning call the corpus's spool fixtures are driven
// under — the identity every bash row they produce must carry.
const goldenSpoolRun = "toolu_golden_run"

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
		// The context-window reminder the CLI attaches to every prompt, first
		// carried by the re-sent-prompt fixture (transcript-lines/
		// user-prompt-resend.jsonl).
		"attachment/total_tokens_reminder": true,
		// The diagnostics attachment when no adjacent change was observed.
		"attachment/diagnostics": true,
		// The hook outcomes. READ AND KEPT WHOLE, never served: the STREAM plane
		// owns the hook row (ruling 2026-09-04) because the vendor hands the two
		// planes disjoint identity material and no key can name one firing on
		// both — see convert/attachment.go's hookAttachment.
		//
		// THIS CENSUS IS OVER WHAT THE CONVERTER CLASSIFIES, NOT OVER WHAT IS
		// STORED. Since the residue ruling (2026-09-13, convert/neverpersist.go)
		// NONE of these reach the store: the reader withholds every residue arm
		// at its write path. The classification is still the contract — it is
		// what the counts and the debug records name — so a new kind appearing
		// here is still a mapping regression.
		"attachment/hook_success":            true,
		"attachment/hook_blocking_error":     true,
		"attachment/hook_non_blocking_error": true, "attachment/hook_cancelled": true,
		// R15: a file-plane prompt can never be a page line.
		"user_prompt": true, "user/meta": true,
		// User-role records no person typed (convert/bookkeeping.go): the CLI's
		// own slash-command bookkeeping, a local command's printed output, the
		// interrupt marker, and a task notification this stream cannot settle
		// a spawn from. Never a prompt.
		"user/slash_command": true, "user/local_command_output": true,
		"user/interrupt": true, "user/task_notification": true,
		// A settle whose call is behind the cursor.
		"orphan_tool_result": true,
		// A fork's copied context: an assistant record it QUOTES from a parent,
		// and a tool result for such a quoted call. Their producer already booked
		// the units, so this reader keeps only residue rather than re-booking a
		// row under this agent (convert assistant.go / settle.go, ledger row 51).
		"assistant/quoted_context":   true,
		"tool_result/quoted_context": true,
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

// The captured session's two Bash calls, and they are NOT alike: the first was
// LAUNCHED INTO THE BACKGROUND (its receipt carries `backgroundTaskId`
// "bbkqcvn8k" and an empty stdout) and the second ran to completion. Named here
// because the difference is the point of the test below.
const (
	capturedMovedBashCall     = "toolu_01HhE2ReMxc7nhxD3LsRs53L"
	capturedCompletedBashCall = "toolu_01BkZUVG3kLG2cH5A2zWBzSx"
)

func TestGoldenRealTranscriptJoinsBothBashResultsToTheirCalls(t *testing.T) {
	// Arrange. Both results return in the same file, so both must be JOINED to
	// their calls rather than degrading to orphan residue — and the join is what
	// this is about, not the arm each one reaches.
	entries := driveRealTranscript(t)

	// Act.
	settled := map[string]bool{}
	orphans := 0
	for _, e := range entries {
		if a := activityOf(e); a.GetBash().GetSuccess() != nil {
			settled[a.GetActivityId().GetValue()] = true
		}
		if v := e.GetAgentUpdate().GetUnservedItem().GetVendorSpecific(); v != nil && v.GetKind() == "orphan_tool_result" {
			orphans++
		}
	}

	// Assert.
	if !settled[capturedCompletedBashCall] {
		t.Fatalf("the completed command %q settled no unit (the join from result to call regressed)", capturedCompletedBashCall)
	}
	// A BACKGROUNDED COMMAND DID NOT END, IT MOVED. Its receipt is the same
	// shape a finished command's is — empty output plus a task id — so settling
	// on it drew a command that ran and printed nothing, over the top of the
	// live card the stream plane had already published for the same unit. The
	// detached-work frames naming this unit are what settle it.
	if settled[capturedMovedBashCall] {
		t.Fatalf("the backgrounded command %q settled a unit whose work had only MOVED", capturedMovedBashCall)
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
	all = append(all, driveAdjacentDiagnostics(t)...)
	return all
}

// driveAdjacentDiagnostics composes the ONE conversion the per-file drivers
// structurally cannot reach: the IDE diagnostics report joins its write/edit
// unit by ADJACENCY, so it needs the change and the report in ONE converter, in
// file order — which is exactly how the vendor writes them and how no single
// fixture file holds them.
//
// BOTH LINES ARE REAL CAPTURES, composed rather than invented: the edit's
// tool_use block and its result, then the diagnostics attachment that followed a
// change in another session.
func driveAdjacentDiagnostics(t *testing.T) []*storev1.StoreEntry {
	t.Helper()
	root := corpusRoot(t)
	call := wrapBlocksAsAssistantLines(t, readFixture(t, filepath.Join(root, "tool-inputs", "file_edit.jsonl")))
	// The two fixtures are excerpts from DIFFERENT sessions, so the result names
	// a call id the assistant line never issued. Pairing them is what composing
	// them means — the vendor's own file has one id for both — and it is the id
	// alone that is re-pointed; every other byte of both captures stands.
	result := repointToolResult(t, readFixture(t, filepath.Join(root, "tool-results", "edit.jsonl")), toolUseIDOfLine(t, call))
	lines := []string{
		call,
		result,
		readFixture(t, filepath.Join(root, "attachments", "diagnostics.jsonl")),
	}
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/projects/proj/adjacent-diagnostics.jsonl", "adjacent-diagnostics")
	return h.Handle(framesFrom(t, strings.Join(lines, "\n")), ctx)
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
		Kind: tail.KindWorkflowJournal, RunID: "wf_golden", TaskID: "w1golden", FileID: testFileID(path),
	}
	return h.Handle(framesFrom(t, readFixture(t, path)), ctx)
}

func driveSidechain(t *testing.T, path string) []*storev1.StoreEntry {
	t.Helper()
	h := NewAgentTranscriptHandler(testLogger(t))
	ctx := &Context{
		Path: path, SessionID: "owning-session", MainAgentID: "owning-session",
		// The book is the SPAWNING CALL, which the corpus's own meta file states
		// as toolUseId; `agent-aef975b7bc3422d4b` is that file's locator.
		AgentID: "toolu_019w534yMVsDAc3KqJYLGhP8", Kind: tail.KindAgentTranscript, FileID: testFileID(path),
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
			AgentID: "spool-session", TaskID: "b1golden", RunActivityID: goldenSpoolRun,
			Kind: tail.KindShellSpool, FileID: testFileID(path),
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

// TestGoldenCorpusProducesTheSoleProducerConversions asserts that every fact the
// SIDECAR IS THE ONLY PRODUCER OF actually comes out of the real corpus.
//
// THE PINNED SDK STREAM CARRIES NO ATTACHMENT RECORDS, so if these conversions
// regress nothing else in the system will produce them and the facts simply
// vanish from every book — with no error anywhere, because a withheld
// attachment is an ordinary outcome. That silence is what this subject exists to
// break.
func TestGoldenCorpusProducesTheSoleProducerConversions(t *testing.T) {
	// Arrange + Act.
	entries := driveWholeCorpus(t)

	// Assert.
	var memory, skills, diagnostics int
	for _, e := range entries {
		frame := e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame()
		activity := frame.GetUpdate().GetActivity()
		if injected := activity.GetContextInjected(); injected != nil {
			if injected.GetMemory() != nil {
				memory++
			}
			if injected.GetSkills() != nil {
				skills++
			}
		}
		if activity.GetEdit().GetDiagnostics() != nil || activity.GetWrite().GetDiagnostics() != nil {
			diagnostics++
		}
	}
	for _, want := range []struct {
		name string
		got  int
	}{
		{"AgentContextInjected.memory (nested_memory)", memory},
		{"AgentContextInjected.skills (dynamic_skill/invoked_skills/skill_listing)", skills},
		{"the write/edit diagnostics consequence arm", diagnostics},
	} {
		if want.got == 0 {
			t.Errorf("the corpus produced no %s; the sidecar is its ONLY producer, so a regression here loses the fact entirely", want.name)
		}
	}
}

// toolUseIDOfLine answers the id of the single tool_use block on an assistant
// line.
func toolUseIDOfLine(t *testing.T, line string) string {
	t.Helper()
	var record map[string]any
	if err := json.Unmarshal([]byte(strings.TrimSpace(line)), &record); err != nil {
		t.Fatalf("assistant line is not JSON: %v", err)
	}
	message, _ := record["message"].(map[string]any)
	blocks, _ := message["content"].([]any)
	for _, raw := range blocks {
		block, ok := raw.(map[string]any)
		if !ok {
			continue
		}
		if block["type"] == "tool_use" {
			if id, ok := block["id"].(string); ok && id != "" {
				return id
			}
		}
	}
	t.Fatalf("assistant line carries no tool_use block: %s", line)
	return ""
}

// repointToolResult re-points a tool_result line at another call id.
func repointToolResult(t *testing.T, line, id string) string {
	t.Helper()
	var record map[string]any
	if err := json.Unmarshal([]byte(strings.TrimSpace(line)), &record); err != nil {
		t.Fatalf("tool result line is not JSON: %v", err)
	}
	message, _ := record["message"].(map[string]any)
	blocks, _ := message["content"].([]any)
	var repointed int
	for _, raw := range blocks {
		block, ok := raw.(map[string]any)
		if !ok {
			continue
		}
		if block["type"] == "tool_result" {
			block["tool_use_id"] = id
			repointed++
		}
	}
	if repointed != 1 {
		t.Fatalf("re-pointed %d tool_result blocks, want exactly 1: %s", repointed, line)
	}
	out, err := json.Marshal(record)
	if err != nil {
		t.Fatalf("re-encoding the tool result: %v", err)
	}
	return string(out)
}

// TestGoldenCorpusAnnouncesNoDetachedWork asserts the sidecar NEVER produces the
// detached-work announcement.
//
// LANDING 4 MADE IT A PAGE LINE of the announcing agent's book, and THE STREAM
// PLANE ANNOUNCES IT: the shim is first to know a call detached, and the
// sidecar only ever sees the spool that appears afterwards. A file-plane
// announcement would be a SECOND announcement of one detachment, racing the
// stream's and keyed the same, so the two producers would fight over one row.
// The sidecar's whole share of detached work is the run's own rows.
func TestGoldenCorpusAnnouncesNoDetachedWork(t *testing.T) {
	// Arrange + Act.
	entries := driveWholeCorpus(t)

	// Assert.
	for _, e := range entries {
		frame := e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame()
		if frame.GetDetachedWork() != nil {
			t.Errorf("entry %q announces detached work; the stream plane owns that announcement", e.GetUpsertKey())
		}
	}
}

// TestEveryBashRunIsNamedByItsSpawningCall asserts landing 4's identity equality
// where the sidecar writes it: a detached run's handle IS the spawning call's
// AgentActivityId, so a consumer holding the call can address the run and a
// vendor task id never reaches the wire.
func TestEveryBashRunIsNamedByItsSpawningCall(t *testing.T) {
	// Arrange + Act.
	entries := driveWholeCorpus(t)

	// Assert.
	var checked int
	for _, e := range entries {
		row := e.GetAgentUpdate().GetBash()
		if row == nil {
			continue
		}
		checked++
		run := row.GetRun().GetValue()
		if run != goldenSpoolRun {
			t.Errorf("bash row %q names run %q, wanted the spawning call %q", e.GetUpsertKey(), run, goldenSpoolRun)
		}
		if !strings.HasPrefix(e.GetUpsertKey(), "bash:"+run+":") {
			t.Errorf("bash row keyed %q, which does not name run %q", e.GetUpsertKey(), run)
		}
	}
	if checked == 0 {
		t.Fatal("the corpus produced no bash rows, so the identity was never checked")
	}
}

// contextTipRecordUUID is the vendor's own uuid on the checked-in
// `attachments/context_tip.jsonl` capture — a record the mapping deliberately
// withholds, so it lands as residue and its KEY is observable.
const contextTipRecordUUID = "91f90641-c528-4a97-aad0-6936bdadea70"

// TestGoldenCorpusResidueIsKeyedByTheVendorsRecordUuid pins the residue key
// LITERALLY, against a real capture.
//
// THE KEY IS A CROSS-PLANE AGREEMENT, not an internal detail. The shim sees the
// same vendor record and may store it as residue too; only a key both planes
// mint identically collapses the two writes onto one row. Nothing asserted this
// spelling before, which is how a plane-local digest survived as the key for as
// long as it did — so it is pinned here as an exact string.
func TestGoldenCorpusResidueIsKeyedByTheVendorsRecordUuid(t *testing.T) {
	// Arrange + Act.
	entries := driveWholeCorpus(t)

	// Assert.
	var found bool
	for _, e := range entries {
		if e.GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetKind() != "attachment/context_tip" {
			continue
		}
		found = true
		if got := e.GetUpsertKey(); got != "residue:"+contextTipRecordUUID {
			t.Fatalf("residue key = %q, want %q", got, "residue:"+contextTipRecordUUID)
		}
	}
	if !found {
		t.Fatal("the context_tip capture produced no residue, so the key was never checked")
	}
}

// TestGoldenCorpusEveryUuidBearingResidueIsKeyedByThatUuid generalizes the pin
// across every residue entry the whole corpus produces: none of them may key on
// anything but the record's own uuid.
func TestGoldenCorpusEveryUuidBearingResidueIsKeyedByThatUuid(t *testing.T) {
	// Arrange + Act.
	entries := driveWholeCorpus(t)

	// Assert.
	var checked int
	for _, e := range entries {
		key := e.GetUpsertKey()
		if !strings.HasPrefix(key, "residue:") || strings.HasPrefix(key, "residue:file:") {
			continue
		}
		checked++
		uuid := strings.TrimPrefix(key, "residue:")
		if _, err := parseUUIDish(uuid); err != nil {
			t.Errorf("residue key %q does not name a vendor record uuid: %v", key, err)
		}
	}
	if checked == 0 {
		t.Fatal("the corpus produced no uuid-keyed residue, so nothing was checked")
	}
}

// TestAnUnparsedLineIsKeyedByItsFileCoordinates pins the OTHER space: a line the
// reader could not decode has no uuid — that is why it is unparsed — so there is
// nothing the other plane could agree on and it keys on where it lives.
func TestAnUnparsedLineIsKeyedByItsFileCoordinates(t *testing.T) {
	// Arrange.
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/projects/proj/session-uuid.jsonl", "session-uuid")
	frames := []tail.Frame{{Raw: []byte("{not json"), Offset: 4096, ParseErr: errUnparsable}}

	// Act.
	entries := h.Handle(frames, ctx)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	want := "residue:file:/p/projects/proj/session-uuid.jsonl:4096"
	if got := entries[0].GetUpsertKey(); got != want {
		t.Fatalf("unparsed key = %q, want %q", got, want)
	}
}

// parseUUIDish checks the shape of a vendor record uuid without importing a
// uuid package: 8-4-4-4-12 hex, which every captured record carries.
func parseUUIDish(s string) (string, error) {
	groups := strings.Split(s, "-")
	want := []int{8, 4, 4, 4, 12}
	if len(groups) != len(want) {
		return "", fmt.Errorf("have %d dash-separated groups, want %d", len(groups), len(want))
	}
	for i, g := range groups {
		if len(g) != want[i] {
			return "", fmt.Errorf("group %d is %d chars, want %d", i, len(g), want[i])
		}
		for _, r := range g {
			if !strings.ContainsRune("0123456789abcdefABCDEF", r) {
				return "", fmt.Errorf("group %d holds a non-hex byte %q", i, r)
			}
		}
	}
	return s, nil
}

var errUnparsable = errors.New("invalid character 'n' looking for beginning of object key string")
