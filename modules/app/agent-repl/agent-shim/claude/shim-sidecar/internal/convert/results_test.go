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
	got := grepSuccess(call, nil, block, 1000)

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
	got := findingsSuccess(call, map[string]any{"findings": []any{}}, 1000)

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

func TestGrepSuccessSubtractsTheOmittedLinesFromTheStatedTotal(t *testing.T) {
	// Arrange: content mode returned 2 of 7 lines. The proto carries the
	// OMITTED figure, which is what a reader is shown.
	call := openCall{input: map[string]any{"output_mode": "content"}}
	result := map[string]any{"mode": "content", "content": "a\nb", "numLines": float64(2), "totalLines": float64(7)}

	// Act
	got := grepSuccess(call, result, nil, 1000)

	// Assert
	content, ok := got.GetMatches().(*conversationv1.AgentGrepSuccess_Content)
	if !ok {
		t.Fatalf("Matches = %T, want AgentGrepSuccess_Content", got.GetMatches())
	}
	partial, ok := content.Content.GetExtent().(*conversationv1.AgentGrepContent_Partial)
	if !ok {
		t.Fatalf("Extent = %T, want AgentGrepContent_Partial", content.Content.GetExtent())
	}
	if partial.Partial.GetLinesReturned() != 2 || partial.Partial.GetLinesOmitted() != 5 {
		t.Fatalf("Partial = %+v, want returned=2 omitted=5", partial.Partial)
	}
}

func TestGrepSuccessClaimsNoOmissionWhenTheTotalMatchesWhatCameBack(t *testing.T) {
	// Arrange: every matching line came back, so the all arm applies.
	call := openCall{input: map[string]any{"output_mode": "content"}}
	result := map[string]any{"mode": "content", "content": "a\nb", "numLines": float64(2), "totalLines": float64(2)}

	// Act
	got := grepSuccess(call, result, nil, 1000)

	// Assert
	content := got.GetMatches().(*conversationv1.AgentGrepSuccess_Content)
	if _, ok := content.Content.GetExtent().(*conversationv1.AgentGrepContent_All); !ok {
		t.Fatalf("Extent = %T, want AgentGrepContent_All", content.Content.GetExtent())
	}
}

func TestGrepSuccessReadsTheFilenamesTheVendorTyped(t *testing.T) {
	// Arrange: files mode states its own filenames array and totals, which are
	// the answer rather than the rendered text.
	call := openCall{input: map[string]any{"output_mode": "files_with_matches"}}
	result := map[string]any{
		"mode":       "files_with_matches",
		"filenames":  []any{"a.txt", "b.txt"},
		"numFiles":   float64(2),
		"totalFiles": float64(5),
	}

	// Act
	got := grepSuccess(call, result, nil, 1000)

	// Assert
	files := got.GetMatches().(*conversationv1.AgentGrepSuccess_Files)
	partial, ok := files.Files.GetExtent().(*conversationv1.AgentGrepFiles_Partial)
	if !ok {
		t.Fatalf("Extent = %T, want AgentGrepFiles_Partial", files.Files.GetExtent())
	}
	if partial.Partial.GetFilesOmitted() != 3 {
		t.Fatalf("FilesOmitted = %d, want 3", partial.Partial.GetFilesOmitted())
	}
	if len(files.Files.GetPaths()) != 2 {
		t.Fatalf("Paths = %q, want two entries", files.Files.GetPaths())
	}
}

func TestGrepSuccessReadsTheVendorsMatchCountRatherThanTheRenderedLines(t *testing.T) {
	// Arrange: count mode states numMatches, which is the total across files
	// and not the number of rendered lines.
	call := openCall{input: map[string]any{"output_mode": "count"}}
	result := map[string]any{"mode": "count", "numMatches": float64(9)}

	// Act
	got := grepSuccess(call, result, map[string]any{"content": "a.txt:1\nb.txt:2"}, 1000)

	// Assert
	count := got.GetMatches().(*conversationv1.AgentGrepSuccess_Count)
	if count.Count.GetMatches() != 9 {
		t.Fatalf("Matches = %d, want 9", count.Count.GetMatches())
	}
}

func TestGrepSuccessFallsBackToTheVendorsDefaultModeWhenNothingNamedOne(t *testing.T) {
	// Arrange: neither the result nor the call names a mode, so the vendor's
	// own default — files_with_matches — applies rather than a guess.
	call := openCall{}
	block := map[string]any{"content": "a.txt\nb.txt"}

	// Act
	got := grepSuccess(call, nil, block, 1000)

	// Assert
	files, ok := got.GetMatches().(*conversationv1.AgentGrepSuccess_Files)
	if !ok {
		t.Fatalf("Matches = %T, want AgentGrepSuccess_Files", got.GetMatches())
	}
	if len(files.Files.GetPaths()) != 2 {
		t.Fatalf("Paths = %q, want two entries", files.Files.GetPaths())
	}
}

func TestGlobSuccessStatesTheExactOmittedCountForACompleteCount(t *testing.T) {
	// Arrange: the list stopped short and the search counted completely, so
	// the omitted figure is exact.
	call := openCall{input: map[string]any{"pattern": "**/*.md"}}
	result := map[string]any{
		"filenames":       []any{"one.md", "two.md"},
		"numFiles":        float64(2),
		"truncated":       true,
		"totalMatches":    float64(7),
		"countIsComplete": true,
	}

	// Act
	got := globSuccess(call, result, nil, 1000)

	// Assert
	partial, ok := got.GetExtent().(*conversationv1.AgentGlobSuccess_Partial)
	if !ok {
		t.Fatalf("Extent = %T, want AgentGlobSuccess_Partial", got.GetExtent())
	}
	exact, ok := partial.Partial.GetOmitted().(*conversationv1.AgentGlobPartial_Exact)
	if !ok {
		t.Fatalf("Omitted = %T, want AgentGlobPartial_Exact", partial.Partial.GetOmitted())
	}
	if exact.Exact.GetFilesOmitted() != 5 {
		t.Fatalf("FilesOmitted = %d, want 5", exact.Exact.GetFilesOmitted())
	}
}

func TestGlobSuccessStatesAFloorWhenTheSearchCappedItsOwnCounting(t *testing.T) {
	// Arrange: countIsComplete false makes the figure a FLOOR — "at least 5
	// more" is a different claim than "5 more".
	call := openCall{input: map[string]any{"pattern": "**/*.md"}}
	result := map[string]any{
		"filenames":       []any{"one.md", "two.md"},
		"numFiles":        float64(2),
		"truncated":       true,
		"totalMatches":    float64(7),
		"countIsComplete": false,
	}

	// Act
	got := globSuccess(call, result, nil, 1000)

	// Assert
	partial := got.GetExtent().(*conversationv1.AgentGlobSuccess_Partial)
	atLeast, ok := partial.Partial.GetOmitted().(*conversationv1.AgentGlobPartial_AtLeast)
	if !ok {
		t.Fatalf("Omitted = %T, want AgentGlobPartial_AtLeast", partial.Partial.GetOmitted())
	}
	if atLeast.AtLeast.GetFilesOmittedAtLeast() != 5 {
		t.Fatalf("FilesOmittedAtLeast = %d, want 5", atLeast.AtLeast.GetFilesOmittedAtLeast())
	}
}

func TestGlobSuccessClaimsOnlyAZeroFloorWhenNoTotalWasStated(t *testing.T) {
	// Arrange: a truncated list with no total leaves only the honest floor,
	// which never overstates what was left out.
	call := openCall{input: map[string]any{"pattern": "**/*.md"}}
	result := map[string]any{"filenames": []any{"one.md"}, "numFiles": float64(1), "truncated": true}

	// Act
	got := globSuccess(call, result, nil, 1000)

	// Assert
	partial := got.GetExtent().(*conversationv1.AgentGlobSuccess_Partial)
	atLeast, ok := partial.Partial.GetOmitted().(*conversationv1.AgentGlobPartial_AtLeast)
	if !ok {
		t.Fatalf("Omitted = %T, want AgentGlobPartial_AtLeast", partial.Partial.GetOmitted())
	}
	if atLeast.AtLeast.GetFilesOmittedAtLeast() != 0 {
		t.Fatalf("FilesOmittedAtLeast = %d, want 0", atLeast.AtLeast.GetFilesOmittedAtLeast())
	}
}

func TestGlobSuccessStatesTheAllExtentForAnUntruncatedList(t *testing.T) {
	// Arrange: the whole match set came back, so no omission may be claimed.
	call := openCall{input: map[string]any{"pattern": "**/*.md"}}
	result := map[string]any{"filenames": []any{"one.md", "two.md"}, "numFiles": float64(2)}

	// Act
	got := globSuccess(call, result, nil, 1000)

	// Assert
	all, ok := got.GetExtent().(*conversationv1.AgentGlobSuccess_All)
	if !ok {
		t.Fatalf("Extent = %T, want AgentGlobSuccess_All", got.GetExtent())
	}
	if all.All.GetFilesReturned() != 2 {
		t.Fatalf("FilesReturned = %d, want 2", all.All.GetFilesReturned())
	}
}

func TestGlobSuccessReadsTheRenderedListWhenTheVendorTypedNothing(t *testing.T) {
	// Arrange: an untyped result leaves only the rendered lines, which state
	// no total and so can claim no omission.
	call := openCall{input: map[string]any{"pattern": "**/*.md"}}
	block := map[string]any{"content": "./one.md\n./deep/two.md"}

	// Act
	got := globSuccess(call, nil, block, 1000)

	// Assert
	if len(got.GetPaths()) != 2 {
		t.Fatalf("Paths = %q, want two entries", got.GetPaths())
	}
	if _, ok := got.GetExtent().(*conversationv1.AgentGlobSuccess_All); !ok {
		t.Fatalf("Extent = %T, want AgentGlobSuccess_All", got.GetExtent())
	}
}

func TestFindingsSuccessCarriesTheVerifyPassVerdict(t *testing.T) {
	// Arrange: a verify pass that confirmed a finding is the whole point of
	// the verdict arm — dropping it draws a confirmed defect as unverified.
	call := openCall{}
	result := map[string]any{"findings": []any{map[string]any{
		"file": "a.py", "summary": "boom", "verdict": "CONFIRMED",
	}}}

	// Act
	got := findingsSuccess(call, result, 1000)

	// Assert
	if len(got.GetFindings()) != 1 {
		t.Fatalf("Findings = %d, want 1", len(got.GetFindings()))
	}
	if _, ok := got.GetFindings()[0].GetVerdict().(*conversationv1.AgentFinding_Confirmed); !ok {
		t.Fatalf("Verdict = %T, want AgentFinding_Confirmed", got.GetFindings()[0].GetVerdict())
	}
}

func TestFindingsSuccessLeavesAnUnknownVerdictUnset(t *testing.T) {
	// Arrange: a word outside the vendor's closed set is not a verdict, and
	// picking an arm for it would state a conclusion nobody reached.
	call := openCall{}
	result := map[string]any{"findings": []any{map[string]any{"file": "a.py", "verdict": "MAYBE"}}}

	// Act
	got := findingsSuccess(call, result, 1000)

	// Assert
	if verdict := got.GetFindings()[0].GetVerdict(); verdict != nil {
		t.Fatalf("Verdict = %T, want unset", verdict)
	}
}

func TestFindingsSuccessCarriesAReReportsOutcome(t *testing.T) {
	// Arrange: outcome is set only on a re-report after fixes were applied.
	call := openCall{}
	result := map[string]any{"findings": []any{map[string]any{"file": "a.py", "outcome": "no_change_needed"}}}

	// Act
	got := findingsSuccess(call, result, 1000)

	// Assert
	if _, ok := got.GetFindings()[0].GetOutcome().(*conversationv1.AgentFinding_NoChangeNeeded); !ok {
		t.Fatalf("Outcome = %T, want AgentFinding_NoChangeNeeded", got.GetFindings()[0].GetOutcome())
	}
}

func TestFindingsSuccessLeavesTheOutcomeUnsetOnAFirstReport(t *testing.T) {
	// Arrange: a first report states no outcome, and inventing one would say
	// a defect was handled when nothing was.
	call := openCall{}
	result := map[string]any{"findings": []any{map[string]any{"file": "a.py", "summary": "boom"}}}

	// Act
	got := findingsSuccess(call, result, 1000)

	// Assert
	if outcome := got.GetFindings()[0].GetOutcome(); outcome != nil {
		t.Fatalf("Outcome = %T, want unset", outcome)
	}
}

func TestFindingsSuccessFallsBackToTheCallWhenTheToolEchoedNothing(t *testing.T) {
	// Arrange: a vendor that echoed no findings would otherwise lose the
	// report entirely, so the call's own input is the fallback.
	call := openCall{input: map[string]any{"findings": []any{map[string]any{
		"file": "a.py", "failure_scenario": "divide by zero",
	}}}}

	// Act
	got := findingsSuccess(call, nil, 1000)

	// Assert
	if len(got.GetFindings()) != 1 || got.GetFindings()[0].GetFailureScenario() != "divide by zero" {
		t.Fatalf("Findings = %+v, want the call's single finding", got.GetFindings())
	}
}

func TestWorktreeSuccessReadsTheVendorsWorktreePathOnEntering(t *testing.T) {
	// Arrange: the vendor spells the tree's path worktreePath. Reading "path"
	// leaves the divider that names the tree blank.
	call := openCall{name: "EnterWorktree"}
	result := map[string]any{"worktreePath": "/w/feature", "worktreeBranch": "feature", "message": "entered"}

	// Act
	got := worktreeSuccess(call, result, 1000)

	// Assert
	entered, ok := got.GetAct().(*conversationv1.AgentWorktreeSuccess_Entered)
	if !ok {
		t.Fatalf("Act = %T, want AgentWorktreeSuccess_Entered", got.GetAct())
	}
	if entered.Entered.GetPath() != "/w/feature" || entered.Entered.GetBranch() != "feature" {
		t.Fatalf("Entered = %+v, want path=/w/feature branch=feature", entered.Entered)
	}
}

func TestWorktreeSuccessReadsTheVendorsWorktreePathOnExiting(t *testing.T) {
	// Arrange: the exited arm names the same key, and a frame without it says
	// the session moved somewhere unnamed.
	call := openCall{name: "ExitWorktree"}
	result := map[string]any{
		"worktreePath": "/w/scratch",
		"originalCwd":  "/w",
		"action":       "keep",
	}

	// Act
	got := worktreeSuccess(call, result, 1000)

	// Assert
	exited := got.GetAct().(*conversationv1.AgentWorktreeSuccess_Exited)
	if exited.Exited.GetPath() != "/w/scratch" {
		t.Fatalf("Path = %q, want /w/scratch", exited.Exited.GetPath())
	}
}

func TestWorktreeSuccessReadsARemovalFromTheVendorsActionWord(t *testing.T) {
	// Arrange: the vendor states action "remove"; no boolean says so.
	call := openCall{name: "ExitWorktree"}
	result := map[string]any{
		"worktreePath":     "/w/scratch",
		"action":           "remove",
		"discardedFiles":   float64(2),
		"discardedCommits": float64(1),
	}

	// Act
	got := worktreeSuccess(call, result, 1000)

	// Assert
	exited := got.GetAct().(*conversationv1.AgentWorktreeSuccess_Exited)
	removed, ok := exited.Exited.GetOutcome().(*conversationv1.AgentWorktreeExited_Removed)
	if !ok {
		t.Fatalf("Outcome = %T, want AgentWorktreeExited_Removed", exited.Exited.GetOutcome())
	}
	if removed.Removed.GetDiscardedFiles() != 2 || removed.Removed.GetDiscardedCommits() != 1 {
		t.Fatalf("Removed = %+v, want files=2 commits=1", removed.Removed)
	}
}

func TestWorktreeSuccessLeavesTheDiscardedFiguresUnsetWhenNoneWereStated(t *testing.T) {
	// Arrange: a removal that stated no figures. "No figure" is not "none".
	call := openCall{name: "ExitWorktree"}
	result := map[string]any{"worktreePath": "/w/scratch", "action": "remove"}

	// Act
	got := worktreeSuccess(call, result, 1000)

	// Assert
	exited := got.GetAct().(*conversationv1.AgentWorktreeSuccess_Exited)
	removed := exited.Exited.GetOutcome().(*conversationv1.AgentWorktreeExited_Removed)
	if removed.Removed.DiscardedFiles != nil || removed.Removed.DiscardedCommits != nil {
		t.Fatalf("Removed = %+v, want both figures unset", removed.Removed)
	}
}

func TestWorktreeSuccessLeavesTheExitOutcomeUnsetForAnUnrecognizedAction(t *testing.T) {
	// Arrange: an action outside the vendor's set. Defaulting to kept would
	// claim a tree survived that may have been deleted.
	call := openCall{name: "ExitWorktree"}
	result := map[string]any{"worktreePath": "/w/scratch", "action": "archive"}

	// Act
	got := worktreeSuccess(call, result, 1000)

	// Assert
	exited := got.GetAct().(*conversationv1.AgentWorktreeSuccess_Exited)
	if outcome := exited.Exited.GetOutcome(); outcome != nil {
		t.Fatalf("Outcome = %T, want unset", outcome)
	}
}
