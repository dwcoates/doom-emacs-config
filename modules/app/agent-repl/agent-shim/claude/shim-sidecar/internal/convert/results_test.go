package convert

// results_test.go — pins the toolUseResult success converters. Each is
// reached only through the settled-items dispatch table (settled_items.go),
// which the higher-level suites never drive down to these specific shapes,
// so each gets one direct unit test from its documented proto contract.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestWriteSuccessNamesACreationRatherThanARewrite(t *testing.T) {
	// Arrange: no originalFile means the vendor wrote a file that did not
	// exist before, which must never be drawn as a rewrite of nothing.
	call := openCall{input: map[string]any{"file_path": "/p/new.go"}}
	result := map[string]any{"filePath": "/p/new.go", "originalFile": ""}

	// Act
	got := writeSuccess(call, result, 1000)

	// Assert
	if _, ok := got.GetOutcome().(*conversationv1.AgentWriteSuccess_Created); !ok {
		t.Fatalf("Outcome = %T, want AgentWriteSuccess_Created", got.GetOutcome())
	}
}

func TestGrepSuccessAnswersACountWhenAskedForOne(t *testing.T) {
	// Arrange: output_mode "count" must answer AgentGrepCount, never the
	// files or content shapes the other two modes use.
	call := openCall{input: map[string]any{"output_mode": "count"}}
	block := map[string]any{"content": "file1.go:1\nfile2.go:1\n"}

	// Act
	got := grepSuccess(call, block, 1000)

	// Assert
	count, ok := got.GetMatches().(*conversationv1.AgentGrepSuccess_Count)
	if !ok {
		t.Fatalf("Matches = %T, want AgentGrepSuccess_Count", got.GetMatches())
	}
	if count.Count.GetMatches() != 2 {
		t.Fatalf("Matches = %d, want 2", count.Count.GetMatches())
	}
}

func TestSendMessageSuccessReportsAResumedRecipient(t *testing.T) {
	// Arrange: a resumedAgentId means the send restarted a dormant agent,
	// which is a materially different delivery than queuing to a live one.
	call := openCall{}
	result := map[string]any{"resumedAgentId": "agent-7"}

	// Act
	got := sendMessageSuccess(call, result, 1000)

	// Assert
	if _, ok := got.GetDelivery().(*conversationv1.AgentSendMessageSuccess_ResumedRecipient); !ok {
		t.Fatalf("Delivery = %T, want AgentSendMessageSuccess_ResumedRecipient", got.GetDelivery())
	}
}

func TestWebFetchSuccessCarriesTheServedStatusAlongsideTheContent(t *testing.T) {
	// Arrange: an HTTP error page is still a served answer, not a failure.
	call := openCall{input: map[string]any{"url": "https://example.com"}}
	result := map[string]any{"code": float64(404), "codeText": "Not Found", "result": "gone"}

	// Act
	got := webFetchSuccess(call, result)

	// Assert
	if got.GetStatus().GetCode() != 404 || got.GetStatus().GetText() != "Not Found" {
		t.Fatalf("Status = %+v, want code=404 text=Not Found", got.GetStatus())
	}
}

func TestWebSearchSuccessPreservesOrderAcrossLinksAndNotes(t *testing.T) {
	// Arrange: the engine's array is heterogeneous — a bare string is a note,
	// an object is a link — and order must survive the conversion.
	call := openCall{}
	result := map[string]any{
		"results": []any{
			"a narration note",
			map[string]any{"title": "A Link", "url": "https://x.example"},
		},
	}

	// Act
	got := webSearchSuccess(call, result)

	// Assert
	if len(got.GetResults()) != 2 {
		t.Fatalf("len(Results) = %d, want 2", len(got.GetResults()))
	}
	if _, ok := got.GetResults()[0].GetEntry().(*conversationv1.AgentWebSearchResult_Note); !ok {
		t.Fatalf("Results[0] = %T, want a note", got.GetResults()[0].GetEntry())
	}
	if _, ok := got.GetResults()[1].GetEntry().(*conversationv1.AgentWebSearchResult_Link); !ok {
		t.Fatalf("Results[1] = %T, want a link", got.GetResults()[1].GetEntry())
	}
}

func TestWakeupSuccessReportsStoppedWhenTheCallAskedToStop(t *testing.T) {
	// Arrange: the `stop` input arm answers cancellation, not scheduling.
	call := openCall{input: map[string]any{"stop": true}}
	result := map[string]any{"cancelledWakeups": float64(3)}

	// Act
	got := wakeupSuccess(call, result)

	// Assert
	stopped, ok := got.GetOutcome().(*conversationv1.AgentScheduleWakeupSuccess_Stopped)
	if !ok {
		t.Fatalf("Outcome = %T, want AgentScheduleWakeupSuccess_Stopped", got.GetOutcome())
	}
	if stopped.Stopped.GetCancelledWakeups() != 3 {
		t.Fatalf("CancelledWakeups = %d, want 3", stopped.Stopped.GetCancelledWakeups())
	}
}

func TestArtifactSuccessReportsAListingSeparatelyFromAPublish(t *testing.T) {
	// Arrange: action "list" is a different act than publishing, and must
	// never be drawn as a publish of nothing.
	call := openCall{input: map[string]any{"action": "list"}}

	// Act
	got := artifactSuccess(call, map[string]any{})

	// Assert
	if _, ok := got.GetOutcome().(*conversationv1.AgentArtifactSuccess_Listed); !ok {
		t.Fatalf("Outcome = %T, want AgentArtifactSuccess_Listed", got.GetOutcome())
	}
}

func TestPlanModeSuccessReadsAnExitedPlanWhenTheCallExits(t *testing.T) {
	// Arrange: ExitPlanMode settles the "exited" act; anything else settles
	// "entered". The two share no fields.
	call := openCall{name: "ExitPlanMode", input: map[string]any{"plan": "do the thing"}}
	result := map[string]any{"planWasEdited": true}

	// Act
	got := planModeSuccess(call, result, 1000)

	// Assert
	exited, ok := got.GetAct().(*conversationv1.AgentPlanModeSuccess_Exited)
	if !ok {
		t.Fatalf("Act = %T, want AgentPlanModeSuccess_Exited", got.GetAct())
	}
	if !exited.Exited.GetPlanWasEdited() {
		t.Fatal("PlanWasEdited = false, want true")
	}
}

func TestFindingsSuccessIsARealReportWhenItFoundNothing(t *testing.T) {
	// Arrange: an empty findings list is a real report — a review that found
	// nothing — not an absent one.
	call := openCall{input: map[string]any{}}

	// Act
	got := findingsSuccess(call, 1000)

	// Assert
	if len(got.GetFindings()) != 0 {
		t.Fatalf("len(Findings) = %d, want 0", len(got.GetFindings()))
	}
	if got.GetSettledAt() == nil {
		t.Fatal("SettledAt is nil, want set even for an empty report")
	}
}

func TestWorktreeSuccessReportsRemovedWhenTheExitDiscardedTheTree(t *testing.T) {
	// Arrange: ExitWorktree with removed=true must report the discard
	// counts, never the "kept" arm the other outcome uses.
	call := openCall{name: "ExitWorktree"}
	result := map[string]any{"removed": true, "discardedFiles": float64(2)}

	// Act
	got := worktreeSuccess(call, result, 1000)

	// Assert
	exited, ok := got.GetAct().(*conversationv1.AgentWorktreeSuccess_Exited)
	if !ok {
		t.Fatalf("Act = %T, want AgentWorktreeSuccess_Exited", got.GetAct())
	}
	removed, ok := exited.Exited.GetOutcome().(*conversationv1.AgentWorktreeExited_Removed)
	if !ok {
		t.Fatalf("Outcome = %T, want AgentWorktreeExited_Removed", exited.Exited.GetOutcome())
	}
	if removed.Removed.GetDiscardedFiles() != 2 {
		t.Fatalf("DiscardedFiles = %d, want 2", removed.Removed.GetDiscardedFiles())
	}
}

func TestCronSuccessListsJobsForCronList(t *testing.T) {
	// Arrange: CronList settles the "listed" act, which carries the jobs
	// array; CronDelete and creation settle different, disjoint acts.
	call := openCall{name: "CronList"}
	result := map[string]any{"jobs": []any{
		map[string]any{"job_id": "j1", "cron": "* * * * *"},
	}}

	// Act
	got := cronSuccess(call, result, 1000)

	// Assert
	listed, ok := got.GetAct().(*conversationv1.AgentCronSuccess_Listed)
	if !ok {
		t.Fatalf("Act = %T, want AgentCronSuccess_Listed", got.GetAct())
	}
	if len(listed.Listed.GetJobs()) != 1 || listed.Listed.GetJobs()[0].GetJobId() != "j1" {
		t.Fatalf("Jobs = %+v, want one job with id j1", listed.Listed.GetJobs())
	}
}

func TestPushSuccessReportsNotSentWithItsReason(t *testing.T) {
	// Arrange: a push that was never sent must not be drawn as delivered,
	// and must name why so a waiting user's absence is explained.
	result := map[string]any{"reason": "no_transport"}

	// Act
	got := pushSuccess(result, 1000)

	// Assert
	notSent, ok := got.GetOutcome().(*conversationv1.AgentPushNotificationSuccess_NotSent)
	if !ok {
		t.Fatalf("Outcome = %T, want AgentPushNotificationSuccess_NotSent", got.GetOutcome())
	}
	if _, ok := notSent.NotSent.GetReason().(*conversationv1.AgentPushNotificationNotSent_NoTransport); !ok {
		t.Fatalf("Reason = %T, want AgentPushNotificationNotSent_NoTransport", notSent.NotSent.GetReason())
	}
}

func TestReadSuccessAnswersAMiddleSliceAsARangeRatherThanAHead(t *testing.T) {
	// Arrange: the caller asked for lines 2-3 of a 4-line file. A head would
	// claim the tail was omitted; a middle slice omits nothing.
	call := openCall{input: map[string]any{"file_path": "/p/f.go", "offset": float64(2), "limit": float64(2)}}
	result := map[string]any{"type": "text", "file": map[string]any{
		"filePath":   "/p/f.go",
		"content":    "two\nthree",
		"startLine":  float64(2),
		"numLines":   float64(2),
		"totalLines": float64(4),
	}}

	// Act
	got := readSuccess(call, result, 1000)

	// Assert
	extent, ok := got.GetExtent().(*conversationv1.AgentReadSuccess_Range)
	if !ok {
		t.Fatalf("Extent = %T, want AgentReadSuccess_Range", got.GetExtent())
	}
	if extent.Range.GetFirstLine() != 2 || extent.Range.GetLineCount() != 2 || extent.Range.GetTotalLines() != 4 {
		t.Fatalf("Range = %+v, want first=2 count=2 total=4", extent.Range)
	}
}

func TestReadSuccessAnswersAnOffsetReadAsARangeEvenWhenTheVendorStatedNoStartLine(t *testing.T) {
	// Arrange: the vendor omitted startLine on an offset read. The ASK is
	// still a middle slice, so no head may be claimed from the short count.
	call := openCall{input: map[string]any{"file_path": "/p/f.go", "offset": float64(2)}}
	result := map[string]any{"type": "text", "file": map[string]any{
		"content":    "two\nthree",
		"numLines":   float64(2),
		"totalLines": float64(4),
	}}

	// Act
	got := readSuccess(call, result, 1000)

	// Assert
	if _, ok := got.GetExtent().(*conversationv1.AgentReadSuccess_Range); !ok {
		t.Fatalf("Extent = %T, want AgentReadSuccess_Range", got.GetExtent())
	}
}

func TestReadSuccessNamesTheTokenCapThatCutAWholeFileRead(t *testing.T) {
	// Arrange: the vendor auto-paginated a whole-file read, so the cut is a
	// token budget rather than a line budget.
	call := openCall{input: map[string]any{"file_path": "/p/f.go"}}
	result := map[string]any{"type": "text", "file": map[string]any{
		"content":             "one",
		"numLines":            float64(1),
		"totalLines":          float64(9),
		"truncatedByTokenCap": true,
	}}

	// Act
	got := readSuccess(call, result, 1000)

	// Assert
	head, ok := got.GetExtent().(*conversationv1.AgentReadSuccess_Head)
	if !ok {
		t.Fatalf("Extent = %T, want AgentReadSuccess_Head", got.GetExtent())
	}
	if _, ok := head.Head.GetCut().(*conversationv1.AgentReadHead_TokenCap); !ok {
		t.Fatalf("Cut = %T, want AgentReadHead_TokenCap", head.Head.GetCut())
	}
}

func TestReadSuccessNamesTheLineCapThatCutALimitedRead(t *testing.T) {
	// Arrange: the caller set a limit and the read began at line 1, so the
	// answer is a head cut on a line boundary.
	call := openCall{input: map[string]any{"file_path": "/p/f.go", "limit": float64(5)}}
	result := map[string]any{"type": "text", "file": map[string]any{
		"content":    "one",
		"startLine":  float64(1),
		"numLines":   float64(5),
		"totalLines": float64(61),
	}}

	// Act
	got := readSuccess(call, result, 1000)

	// Assert
	head, ok := got.GetExtent().(*conversationv1.AgentReadSuccess_Head)
	if !ok {
		t.Fatalf("Extent = %T, want AgentReadSuccess_Head", got.GetExtent())
	}
	if _, ok := head.Head.GetCut().(*conversationv1.AgentReadHead_LineCap); !ok {
		t.Fatalf("Cut = %T, want AgentReadHead_LineCap", head.Head.GetCut())
	}
}

func TestReadSuccessAnswersAWholeFileWithNoTruncationVocabulary(t *testing.T) {
	// Arrange: every line of the file came back, so no arm may claim more
	// exists.
	call := openCall{input: map[string]any{"file_path": "/p/f.go"}}
	result := map[string]any{"type": "text", "file": map[string]any{
		"content":    "one\ntwo",
		"startLine":  float64(1),
		"numLines":   float64(2),
		"totalLines": float64(2),
	}}

	// Act
	got := readSuccess(call, result, 1000)

	// Assert
	if _, ok := got.GetExtent().(*conversationv1.AgentReadSuccess_Whole); !ok {
		t.Fatalf("Extent = %T, want AgentReadSuccess_Whole", got.GetExtent())
	}
}

func TestWriteSuccessDiffsACreationTheVendorLeftUnpatched(t *testing.T) {
	// Arrange: the vendor states an EMPTY structuredPatch for a creation, so
	// the producer must diff the versions itself or the change is lost.
	call := openCall{input: map[string]any{"file_path": "/p/new.go"}}
	result := map[string]any{
		"type":            "create",
		"filePath":        "/p/new.go",
		"content":         "hello",
		"structuredPatch": []any{},
		"originalFile":    nil,
	}

	// Act
	got := writeSuccess(call, result, 1000)

	// Assert
	if len(got.GetPatch()) != 1 {
		t.Fatalf("Patch = %d hunks, want 1", len(got.GetPatch()))
	}
	if lines := got.GetPatch()[0].GetLines(); len(lines) != 1 || lines[0] != "+hello" {
		t.Fatalf("Lines = %q, want [+hello]", lines)
	}
}

func TestWriteSuccessPrefersTheVendorsStatedPatchOverADiff(t *testing.T) {
	// Arrange: an update carries the vendor's own structured patch, whose line
	// numbers describe the file as it actually was.
	call := openCall{input: map[string]any{"file_path": "/p/f.txt"}}
	result := map[string]any{
		"type":     "update",
		"filePath": "/p/f.txt",
		"content":  "replaced",
		"structuredPatch": []any{map[string]any{
			"oldStart": float64(1), "oldLines": float64(1),
			"newStart": float64(1), "newLines": float64(1),
			"lines": []any{"-original content", "+replaced"},
		}},
		"originalFile": "original content\n",
	}

	// Act
	got := writeSuccess(call, result, 1000)

	// Assert
	if len(got.GetPatch()) != 1 || len(got.GetPatch()[0].GetLines()) != 2 {
		t.Fatalf("Patch = %+v, want the vendor's single two-line hunk", got.GetPatch())
	}
}

func TestWriteSuccessNamesAnUpdateFromTheVendorsOwnType(t *testing.T) {
	// Arrange: type "update" is the vendor stating the file existed, which no
	// guess from originalFile may override.
	call := openCall{input: map[string]any{"file_path": "/p/f.txt"}}
	result := map[string]any{"type": "update", "filePath": "/p/f.txt", "content": "b", "originalFile": "a"}

	// Act
	got := writeSuccess(call, result, 1000)

	// Assert
	if _, ok := got.GetOutcome().(*conversationv1.AgentWriteSuccess_Updated); !ok {
		t.Fatalf("Outcome = %T, want AgentWriteSuccess_Updated", got.GetOutcome())
	}
}
