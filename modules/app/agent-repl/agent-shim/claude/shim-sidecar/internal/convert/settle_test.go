package convert

// settle_test.go — the joins, the exempt set, and the TaskStop carve-out.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

const ts2 = "2026-07-22T19:58:40.000Z"

func TestToolResultUpsertsItsCallsOwnUnit(t *testing.T) {
	// Arrange. A RETURN IS NEVER A CHILD: the settled frame supersedes the
	// announcement's row entirely, under the same identity.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_r", "Read", `{"file_path":"/f.go"}`))
	result := toolResultLine("u1", "toolu_r", ts2, `[{"type":"text","text":"contents"}]`,
		`{"type":"text","file":{"filePath":"/f.go","content":"contents","numLines":1,"totalLines":1}}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert: both frames landed under ONE key, and the last is the settle.
	var underKey int
	for _, e := range entries {
		if e.GetUpsertKey() == ActivityKey("toolu_r") {
			underKey++
		}
	}
	if underKey != 2 {
		t.Fatalf("frames under the call's key = %d, want 2 (an announcement and its settle)", underKey)
	}
	settled := entries[len(entries)-1]
	if activityOf(settled).GetRead().GetSuccess() == nil {
		t.Fatal("the result must settle the read on its success arm")
	}
	if got := activityOf(settled).GetActivityId().GetValue(); got != "toolu_r" {
		t.Fatalf("activity_id = %q, want the vendor tool_use_id", got)
	}
}

func TestStartedAtAndSettledAtComeFromTheFileRecords(t *testing.T) {
	// Arrange. INSTANTS ARE NEVER RE-STAMPED AT EMIT TIME: a re-read after a
	// restart must yield the identical frame, and a drawn clock must not reset.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", "2026-07-22T19:58:36.000Z",
		toolCall("toolu_r", "Read", `{"file_path":"/f.go"}`))
	result := toolResultLine("u1", "toolu_r", "2026-07-22T19:58:40.000Z", `[{"type":"text","text":"c"}]`,
		`{"type":"text","file":{"filePath":"/f.go","content":"c","numLines":1,"totalLines":1}}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	announced := entries[0]
	if got := activityOf(announced).GetRead().GetStart().GetStartedAt().GetAtMs(); got != 1784750316000 {
		t.Fatalf("started_at = %d, want the CALL record's timestamp", got)
	}
	settled := entries[len(entries)-1]
	if got := activityOf(settled).GetRead().GetSuccess().GetSettledAt().GetAtMs(); got != 1784750320000 {
		t.Fatalf("settled_at = %d, want the RESULT record's timestamp", got)
	}
}

func TestOrphanToolResultIsResidueRatherThanAnInventedParent(t *testing.T) {
	// Arrange. The call was read before this reader's cursor. There is no unit to
	// settle and none is invented — and after a restart this is a genuinely lost
	// settle the integration loop must see.
	c := newTestConverter(t)
	result := toolResultLine("u1", "toolu_missing", ts2, `[{"type":"text","text":"c"}]`, `{"stdout":"x"}`)

	// Act.
	entries := convertLines(t, c, result)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	if got := vendorKindOf(entries[0]); got != "orphan_tool_result" {
		t.Fatalf("kind = %q, want orphan_tool_result", got)
	}
}

func TestExemptToolCallProducesNoEntryAtAll(t *testing.T) {
	// Arrange. A DROP IS NOT RESIDUE: filing an exempt built-in as unknown would
	// pollute the very query built to find real modelling gaps.
	tests := []string{"TaskOutput", "TaskGet", "TaskList", "ToolSearch", "NotebookEdit", "REPL",
		"ListMcpResources", "ReadMcpResource", "SendFeedback"}
	for _, tool := range tests {
		t.Run(tool, func(t *testing.T) {
			c := newTestConverter(t)

			// Act.
			entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_x", tool, `{}`)))

			// Assert.
			if len(entries) != 0 {
				t.Fatalf("entries = %d, want 0 for the exempt tool %s: keys=%v", len(entries), tool, allKeys(entries))
			}
		})
	}
}

func TestExemptToolResultProducesNoEntryAtAll(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_x", "TaskList", `{}`))
	result := toolResultLine("u1", "toolu_x", ts2, `[{"type":"text","text":"tasks"}]`, `{"tasks":[]}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("entries = %d, want 0: keys=%v", len(entries), allKeys(entries))
	}
}

func TestAShellTasksStopIsReportedRatherThanConvertedHere(t *testing.T) {
	// Arrange. THE ONE CARVE-OUT: the TaskStop CALL stays dropped and its RESULT
	// resolves the owning task as CANCELLED — but a cancelled shell run's
	// terminal owes the OUTPUT the run produced, and those bytes are in the
	// spool this converter never reads. So the fact travels to the reader, which
	// mints the terminal through the spool's own handler.
	c := newTestConverter(t)
	var stopped []string
	c.SetObserver(recordingObserver{stopped: &stopped})
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_stop", "TaskStop", `{"task_id":"b7"}`))
	result := toolResultLine("u1", "toolu_stop", ts2, `[{"type":"text","text":"stopped"}]`,
		`{"command":"stop","task_type":"bash","task_id":"b7","message":"stopped"}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	if len(stopped) != 1 || stopped[0] != "b7" {
		t.Fatalf("stops reported = %v, want exactly the stopped task", stopped)
	}
	for _, e := range entries {
		if e.GetAgentUpdate().GetBash() != nil {
			t.Fatalf("the transcript converter minted a bash frame %q; the spool's reader owns the run's terminal", e.GetUpsertKey())
		}
	}
}

func TestAShellTasksStopProducesNoEntryOfItsOwn(t *testing.T) {
	// Arrange. The stop is a REPORT, and the TaskStop call is exempt either way,
	// so the record itself converts to nothing at all — not a page line, not
	// residue.
	c := newTestConverter(t)
	var stopped []string
	c.SetObserver(recordingObserver{stopped: &stopped})
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_stop", "TaskStop", `{"task_id":"b7"}`))
	result := toolResultLine("u1", "toolu_stop", ts2, `[{"type":"text","text":"stopped"}]`,
		`{"command":"stop","task_type":"local_bash","task_id":"b7","message":"stopped"}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("entries = %d, want 0: keys=%v", len(entries), allKeys(entries))
	}
}

func TestTaskStopResultIsConsumedAsAnAgentSpawnsStoppedFailure(t *testing.T) {
	// Arrange. An agent task DOES settle here: the spawn unit is a line in this
	// stream's own book. It is keyed by the CALL that spawned it, never by the
	// vendor task id, so the launch has to be in the file.
	c := newTestConverter(t)
	launch := assistantWith("a0", "msg_0", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
	launched := toolResultLine("u0", "toolu_spawn", ts1, `[{"type":"text","text":"launched"}]`,
		`{"isAsync":true,"agentId":"a9","outputFile":"/tmp/a9.output"}`)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_stop", "TaskStop", `{"task_id":"a9"}`))
	result := toolResultLine("u1", "toolu_stop", ts2, `[{"type":"text","text":"stopped"}]`,
		`{"command":"stop","task_type":"agent","task_id":"a9","message":"stopped"}`)

	// Act.
	entries := convertLines(t, c, launch, launched, call, result)

	// Assert. The spawn unit's row is written twice on purpose — the launch
	// settles it, and the stop supersedes it WHOLE — so the last write is the
	// unit's state, exactly as the store would hold it.
	terminal := lastEntryByKey(t, entries, ActivityKey("toolu_spawn"))
	failure := activityOf(terminal).GetSubagent().GetFailure()
	if failure.GetStoppedByUser() == nil {
		t.Fatal("a stopped subagent must resolve stopped_by_user, which is not a fault")
	}
}

func TestAnAgentStopForATaskNoLaunchOpenedIsStoredRatherThanKeyedOnAGuess(t *testing.T) {
	// Arrange. Without the launch nothing says WHICH call the spawn unit is, and
	// keying it on the vendor task id would settle a row no reader can join to a
	// call — so the record is stored whole instead.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_stop", "TaskStop", `{"task_id":"a9"}`))
	result := toolResultLine("u1", "toolu_stop", ts2, `[{"type":"text","text":"stopped"}]`,
		`{"command":"stop","task_type":"agent","task_id":"a9","message":"stopped"}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want exactly 1 (the record stored whole): keys=%v", len(entries), allKeys(entries))
	}
	if got := entries[0].GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetKind(); got != "task_stop/unlaunched" {
		t.Fatalf("kind = %q, want task_stop/unlaunched", got)
	}
}

func TestSkillDocumentSettlesItsInvocationBySourceToolUseId(t *testing.T) {
	// Arrange. The join is the vendor's own sourceToolUseID — DIRECT AND
	// STRUCTURAL, never a skill-name map matched against whatever arrives next.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_skill", "Skill", `{"skill":"graphify"}`))
	ack := toolResultLine("u1", "toolu_skill", ts2, `[{"type":"text","text":"graphify"}]`,
		`{"commandName":"graphify","success":true}`)
	doc := `{"type":"user","uuid":"u2","isSidechain":false,"isMeta":true,"sourceToolUseID":"toolu_skill",` +
		`"timestamp":"` + ts2 + `","message":{"role":"user","content":[{"type":"text","text":"# the skill body"}]}}`

	// Act.
	entries := convertLines(t, c, call, ack, doc)

	// Assert.
	var settled int
	for _, e := range entries {
		if s := activityOf(e).GetSkillUse().GetSuccess(); s != nil {
			settled++
			if got := s.GetDocument().GetMarkdown(); got != "# the skill body" {
				t.Fatalf("document = %q, want the body verbatim", got)
			}
			if got := s.GetSkill().GetName(); got != "graphify" {
				t.Fatalf("skill = %q, want the invoked name", got)
			}
			if got := e.GetUpsertKey(); got != ActivityKey("toolu_skill") {
				t.Fatalf("upsert_key = %q, want the CALL's key so the document upserts the invocation", got)
			}
		}
	}
	if settled != 1 {
		t.Fatalf("settled skill units = %d, want 1", settled)
	}
}

func TestDiagnosticsJoinTheLastChangeUnitByAdjacency(t *testing.T) {
	// Arrange. The vendor's diagnostics record carries NO tool-call id, so the
	// join is by ADJACENCY — one remembered last-write/edit unit.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_edit", "Edit", `{"file_path":"/f.ts"}`))
	result := toolResultLine("u1", "toolu_edit", ts2, `[{"type":"text","text":"edited"}]`,
		`{"filePath":"/f.ts","structuredPatch":[],"userModified":false}`)
	diag := `{"type":"attachment","uuid":"u2","isSidechain":false,"timestamp":"` + ts2 +
		`","attachment":{"type":"diagnostics","isNew":true,"files":[{"uri":"/f.ts","diagnostics":[` +
		`{"message":"Cannot find name 'x'.","severity":"Error","source":"typescript","code":"2304",` +
		`"range":{"start":{"line":10,"character":1},"end":{"line":10,"character":5}}}]}]}}`

	// Act.
	entries := convertLines(t, c, call, result, diag)

	// Assert: the report rides the EDIT's own unit as a further frame of it.
	last := entries[len(entries)-1]
	if got := last.GetUpsertKey(); got != ActivityKey("toolu_edit") {
		t.Fatalf("upsert_key = %q, want the adjacent edit's key", got)
	}
	report := activityOf(last).GetEdit().GetDiagnostics()
	if report == nil {
		t.Fatal("the report must land on the EDIT's diagnostics arm")
	}
	if len(report.GetFiles()) != 1 || len(report.GetFiles()[0].GetDiagnostics()) != 1 {
		t.Fatalf("report shape = %v, want one file with one finding", report)
	}
	finding := report.GetFiles()[0].GetDiagnostics()[0]
	if got := finding.GetStartLine(); got != 10 {
		t.Fatalf("start_line = %d, want the vendor's zero-based line 10", got)
	}
}

func TestDiagnosticsWithNoAdjacentChangeAreNotAttachedByGuess(t *testing.T) {
	// Arrange. The change was read before this reader's cursor.
	c := newTestConverter(t)
	diag := `{"type":"attachment","uuid":"u2","isSidechain":false,"timestamp":"` + ts2 +
		`","attachment":{"type":"diagnostics","isNew":true,"files":[]}}`

	// Act.
	entries := convertLines(t, c, diag)

	// Assert.
	if got := vendorKindOf(entries[0]); got != "attachment/diagnostics" {
		t.Fatalf("kind = %q, want the findings kept whole rather than pinned on a guess", got)
	}
}

func TestNonZeroShellExitIsCompletedNotAFailure(t *testing.T) {
	// Arrange. The command RAN; the code is its own verdict on itself. The
	// failure arm is for a call that could not be performed.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_b", "Bash", `{"command":"false"}`))
	result := toolResultLine("u1", "toolu_b", ts2, `[{"type":"text","text":""}]`,
		`{"stdout":"","stderr":"boom","interrupted":false,"isImage":false,"exitCode":3}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	settled := entries[len(entries)-1]
	success := activityOf(settled).GetBash().GetSuccess()
	if success == nil {
		t.Fatal("a command that ran must settle on the success arm whatever its exit code")
	}
	if got := success.GetCompleted().GetTermination().GetExited().GetCode(); got != 3 {
		t.Fatalf("exit code = %d, want 3", got)
	}
	if got := success.GetCompleted().GetOutput().GetText().GetStderr(); got != "boom" {
		t.Fatalf("stderr = %q, want it kept SEPARATE from stdout", got)
	}
}

func TestForegroundCommandStatesNoTermination(t *testing.T) {
	// Arrange. No producer states a termination for a foreground command, and
	// claiming an exit the vendor did not report would be a fabrication.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_b", "Bash", `{"command":"ls"}`))
	result := toolResultLine("u1", "toolu_b", ts2, `[{"type":"text","text":"a"}]`,
		`{"stdout":"a","stderr":"","interrupted":false,"isImage":false}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	completed := activityOf(entries[len(entries)-1]).GetBash().GetSuccess().GetCompleted()
	if completed.Termination != nil {
		t.Fatal("a foreground command must state NO termination: the fact does not exist for that path")
	}
	if completed.GetOutput() == nil {
		t.Fatal("output must ALWAYS be set, so a reader can tell 'ran and was silent' from 'has not run'")
	}
}

func TestEmptySearchResultIsSuccessWithAnEmptyAnswer(t *testing.T) {
	// Arrange. The caller asked a question and got one.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_g", "Glob", `{"pattern":"**/none"}`))
	result := toolResultLine("u1", "toolu_g", ts2, `[{"type":"text","text":""}]`, `{}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	success := activityOf(entries[len(entries)-1]).GetGlob().GetSuccess()
	if success == nil {
		t.Fatal("matching nothing must be a SUCCESS, not a failure")
	}
	if len(success.GetPaths()) != 0 {
		t.Fatalf("paths = %v, want empty", success.GetPaths())
	}
	if success.GetAll() == nil {
		t.Fatal("an empty complete answer must still state its completeness")
	}
}

func TestFailedToolResultLandsOnTheFailureArmWithItsAccount(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_r", "Read", `{"file_path":"/missing"}`))
	result := `{"type":"user","uuid":"u1","isSidechain":false,"timestamp":"` + ts2 +
		`","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_r","is_error":true,` +
		`"content":[{"type":"text","text":"no such file"}]}]}}`

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	failure := activityOf(entries[len(entries)-1]).GetRead().GetFailure()
	if failure == nil {
		t.Fatal("an is_error result must settle on the failure arm")
	}
	blocks := failure.GetError().GetContent().GetBlocks()
	if len(blocks) != 1 || blocks[0].GetText().GetText() != "no such file" {
		t.Fatalf("failure content = %v, want the tool's own account verbatim", blocks)
	}
}

func TestUnknownToolBecomesUnmodeledNotResidue(t *testing.T) {
	// Arrange. AgentUnmodeled is for a tool whose schema genuinely cannot be
	// known — an MCP server's tool. It is NOT a lazy fallback, and a recognizable
	// built-in reaching it would be a producer defect.
	c := newTestConverter(t)
	blocks := toolCall("toolu_m", "mcp__claude_ai_Gmail__send_message", `{"to":"x"}`)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, blocks))

	// Assert.
	start := activityOf(entries[0]).GetUnmodeled().GetStart()
	if start == nil {
		t.Fatal("an unknown tool must be carried as AgentUnmodeled")
	}
	if got := start.GetToolName(); got != "mcp__claude_ai_Gmail__send_message" {
		t.Fatalf("tool_name = %q, want the qualified name unparsed", got)
	}
	if start.GetArguments() == nil {
		t.Fatal("the arguments must be carried structured, so a generic view can list fields")
	}
}

func TestUnknownToolsResultSettlesAsUnmodeledSuccess(t *testing.T) {
	// Arrange. The RESULT of an unknown tool goes through settleUnmodeled,
	// a separate path from the call's own AgentUnmodeled_Start announcement.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_m", "mcp__claude_ai_Gmail__send_message", `{"to":"x"}`))
	result := toolResultLine("u1", "toolu_m", ts2, `[{"type":"text","text":"sent"}]`, `{}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	terminal := lastEntryByKey(t, entries, ActivityKey("toolu_m"))
	success := activityOf(terminal).GetUnmodeled().GetSuccess()
	if success == nil {
		t.Fatal("an unknown tool's result must settle AgentUnmodeled_Success")
	}
	if got := success.GetToolName(); got != "mcp__claude_ai_Gmail__send_message" {
		t.Fatalf("tool_name = %q, want the qualified name unparsed", got)
	}
}

// recordingObserver captures the facts a conversion reports to the reader.
type recordingObserver struct {
	stopped *[]string
}

func (o recordingObserver) TaskSpawned(string, string, string, string, bool) {}

func (o recordingObserver) TaskStopped(taskID string) {
	*o.stopped = append(*o.stopped, taskID)
}

// lastEntryByKey answers the LAST entry written under a key, which is the state
// the store holds: a write supersedes its row whole, so an earlier write of the
// same key is history rather than a duplicate.
func lastEntryByKey(t *testing.T, entries []*storev1.StoreEntry, key string) *storev1.StoreEntry {
	t.Helper()
	var out *storev1.StoreEntry
	for _, e := range entries {
		if e.GetUpsertKey() == key {
			out = e
		}
	}
	if out == nil {
		t.Fatalf("no entry under upsert_key %q; keys present: %v", key, allKeys(entries))
	}
	return out
}

// TestTaskStopWithTheVendorsLocalAgentSpellingSettlesTheSpawn pins the spelling
// the vendor ACTUALLY writes.
//
// The captured stop record (testdata/corpus/tool-results/task_stop.jsonl) states
// `task_type: "local_agent"` for a stopped Agent task. Matching only "agent"
// routed every real agent stop into the SHELL branch, where it was reported to a
// spool that does not exist and the spawn unit was never settled — so a
// deliberately stopped subagent stayed open in every reader downstream.
func TestTaskStopWithTheVendorsLocalAgentSpellingSettlesTheSpawn(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	launch := assistantWith("a0", "msg_0", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
	launched := toolResultLine("u0", "toolu_spawn", ts1, `[{"type":"text","text":"launched"}]`,
		`{"isAsync":true,"agentId":"a9","outputFile":"/tmp/a9.output"}`)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_stop", "TaskStop", `{"task_id":"a9"}`))
	result := toolResultLine("u1", "toolu_stop", ts2, `[{"type":"text","text":"stopped"}]`,
		`{"command":"stop","task_type":"local_agent","task_id":"a9","message":"stopped"}`)

	// Act.
	entries := convertLines(t, c, launch, launched, call, result)

	// Assert.
	terminal := lastEntryByKey(t, entries, ActivityKey("toolu_spawn"))
	failure := activityOf(terminal).GetSubagent().GetFailure()
	if failure.GetStoppedByUser() == nil {
		t.Fatal("a stopped subagent must resolve stopped_by_user, which is not a fault")
	}
}

func TestImageBlockPrefersAUrlSourceOverAPath(t *testing.T) {
	// Arrange: a vendor image with a url must resolve to the url location,
	// never the path arm, which the two are mutually exclusive over.
	block := map[string]any{"source": map[string]any{
		"media_type": "image/png",
		"url":        "https://example.com/a.png",
		"path":       "/tmp/a.png",
	}}

	// Act
	got := imageBlock(block)

	// Assert
	if got.GetMediaType() != "image/png" {
		t.Fatalf("MediaType = %q, want image/png", got.GetMediaType())
	}
	url, ok := got.GetLocation().(*conversationv1.ImageBlock_Url)
	if !ok {
		t.Fatalf("Location = %T, want ImageBlock_Url", got.GetLocation())
	}
	if url.Url.GetUrl() != "https://example.com/a.png" {
		t.Fatalf("Url = %q, want https://example.com/a.png", url.Url.GetUrl())
	}
}
