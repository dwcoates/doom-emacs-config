package convert

// results_test.go — pins the toolUseResult success converters. Each is
// reached only through the settled-items dispatch table (settled_items.go),
// which the higher-level suites never drive down to these specific shapes,
// so each gets one direct unit test from its documented proto contract.

import (
	"slices"
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

// A SETTLED SEND STANDS ALONE: it restates the address and the summary, since
// the start it upserts over is gone once it lands.
func TestSendMessageSuccessRestatesTheStartsFields(t *testing.T) {
	tests := []struct {
		name  string
		input map[string]any
		// restated reads the one restated field under test.
		restated func(*conversationv1.AgentSendMessageSuccess) string
		want     string
	}{
		{
			name:     "the address as the caller wrote it",
			input:    map[string]any{"to": "vetter", "message": "go"},
			restated: func(s *conversationv1.AgentSendMessageSuccess) string { return s.GetAddressedTo() },
			want:     "vetter",
		},
		{
			name:     "the caller's one-line summary",
			input:    map[string]any{"to": "vetter", "message": "go", "summary": "Scroll fix landed; merge master in"},
			restated: func(s *conversationv1.AgentSendMessageSuccess) string { return s.GetSummary().GetText() },
			want:     "Scroll fix landed; merge master in",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			call := openCall{input: tt.input}
			result := map[string]any{"resumedAgentId": "agent-7"}

			// Act
			got := sendMessageSuccess(call, result, 1000)

			// Assert
			if restated := tt.restated(got); restated != tt.want {
				t.Fatalf("restated = %q, want %q", restated, tt.want)
			}
		})
	}
}

func TestSendMessageSuccessLeavesTheSummaryUnsetWhenTheCallerGaveNone(t *testing.T) {
	// Arrange
	call := openCall{input: map[string]any{"to": "vetter", "message": "go"}}

	// Act
	got := sendMessageSuccess(call, map[string]any{"resumedAgentId": "agent-7"}, 1000)

	// Assert
	if got.GetSummary() != nil {
		t.Fatalf("Summary = %v, want unset: the caller supplied none", got.GetSummary())
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

// TestWebSearchSuccessPreservesOrderAcrossLinksAndNotes pins the heterogeneous
// array's ORDER and, with it, the shape of its object entries.
//
// The object is a HIT GROUP, `{tool_use_id, content: {title,url}[]}` — that is
// what testdata/corpus/tool-results/web_search.jsonl holds, and what
// convert/tools/web-search.ts's file doc states. This test previously arranged
// a bare `{title, url}` object as though the group itself were a link, which is
// a shape the vendor never sends; the conversion read `title`/`url` off the
// GROUP, found neither, and minted an empty dead link while losing every page.
func TestWebSearchSuccessPreservesOrderAcrossLinksAndNotes(t *testing.T) {
	// Arrange: a narration line, then a group of two pages.
	c := newTestConverter(t)
	call := openCall{}
	result := map[string]any{
		"results": []any{
			"a narration note",
			map[string]any{
				"tool_use_id": "srvtoolu_01",
				"content": []any{
					map[string]any{"title": "A Link", "url": "https://x.example"},
					map[string]any{"title": "Another", "url": "https://y.example"},
				},
			},
		},
	}

	// Act
	got := c.webSearchSuccess(call, result, Attribution{})

	// Assert
	if len(got.GetResults()) != 3 {
		t.Fatalf("len(Results) = %d, want 3: the note plus BOTH pages of the group", len(got.GetResults()))
	}
	if _, ok := got.GetResults()[0].GetEntry().(*conversationv1.AgentWebSearchResult_Note); !ok {
		t.Fatalf("Results[0] = %T, want a note", got.GetResults()[0].GetEntry())
	}
	first, ok := got.GetResults()[1].GetEntry().(*conversationv1.AgentWebSearchResult_Link)
	if !ok {
		t.Fatalf("Results[1] = %T, want a link", got.GetResults()[1].GetEntry())
	}
	if first.Link.GetTitle() != "A Link" || first.Link.GetUrl() != "https://x.example" {
		t.Fatalf("Results[1] = %+v, want the group's first page", first.Link)
	}
	second, ok := got.GetResults()[2].GetEntry().(*conversationv1.AgentWebSearchResult_Link)
	if !ok {
		t.Fatalf("Results[2] = %T, want a link", got.GetResults()[2].GetEntry())
	}
	if second.Link.GetUrl() != "https://y.example" {
		t.Fatalf("Results[2] = %+v, want the group's second page", second.Link)
	}
}

// TestWebSearchSuccessDropsAHitWithNoUrlRatherThanDrawingADeadLink pins the
// refusal: a link row built around an empty href is a dead row drawn as a live
// one, so the hit is dropped and said so instead.
func TestWebSearchSuccessDropsAHitWithNoUrlRatherThanDrawingADeadLink(t *testing.T) {
	// Arrange: one page has a url, one does not.
	c := newTestConverter(t)
	result := map[string]any{
		"results": []any{
			map[string]any{"content": []any{
				map[string]any{"title": "No url here"},
				map[string]any{"title": "Real", "url": "https://x.example"},
			}},
		},
	}

	// Act
	got := c.webSearchSuccess(openCall{}, result, Attribution{})

	// Assert
	if len(got.GetResults()) != 1 {
		t.Fatalf("len(Results) = %d, want 1: the url-less hit is not a page anyone can open", len(got.GetResults()))
	}
	link, ok := got.GetResults()[0].GetEntry().(*conversationv1.AgentWebSearchResult_Link)
	if !ok || link.Link.GetUrl() != "https://x.example" {
		t.Fatalf("Results[0] = %v, want the hit that named a url", got.GetResults()[0].GetEntry())
	}
}

// TestWebSearchSuccessDropsAnEntryThatIsNeitherNoteNorGroup pins the other
// refusal: an object with no `content` array states no pages, and inventing an
// empty link from it is what put an invisible row on the card.
func TestWebSearchSuccessDropsAnEntryThatIsNeitherNoteNorGroup(t *testing.T) {
	// Arrange: an object carrying no content array at all.
	c := newTestConverter(t)
	result := map[string]any{"results": []any{map[string]any{"tool_use_id": "srvtoolu_01"}}}

	// Act
	got := c.webSearchSuccess(openCall{}, result, Attribution{})

	// Assert
	if len(got.GetResults()) != 0 {
		t.Fatalf("Results = %v, want none: a group with no content names no page", got.GetResults())
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

func TestBashSuccessReadsTheExitCodeTheVendorInterpreted(t *testing.T) {
	// Arrange: returnCodeInterpretation is the only place a foreground call
	// carries the shell's status, and it is the command's own verdict.
	call := openCall{input: map[string]any{"command": "false"}}
	result := map[string]any{"stdout": "", "returnCodeInterpretation": "exited with code 7"}

	// Act
	got := bashSuccess(call, result, nil, bashExitCode(result, nil, false), 1000)

	// Assert
	completed, ok := got.GetOutcome().(*conversationv1.AgentBashSuccess_Completed)
	if !ok {
		t.Fatalf("Outcome = %T, want AgentBashSuccess_Completed", got.GetOutcome())
	}
	exited, ok := completed.Completed.GetTermination().GetHow().(*conversationv1.AgentBashTermination_Exited)
	if !ok {
		t.Fatalf("How = %T, want AgentBashTermination_Exited", completed.Completed.GetTermination().GetHow())
	}
	if exited.Exited.GetCode() != 7 {
		t.Fatalf("Code = %d, want 7", exited.Exited.GetCode())
	}
}

func TestBashSuccessLeavesTerminationUnsetWhenNoStatusWasStated(t *testing.T) {
	// Arrange: an ordinary foreground result states no status, and
	// synthesizing exited(0) would invent a fact nothing reported.
	call := openCall{input: map[string]any{"command": "true"}}
	result := map[string]any{"stdout": "ok"}

	// Act
	got := bashSuccess(call, result, nil, bashExitCode(result, nil, false), 1000)

	// Assert
	completed := got.GetOutcome().(*conversationv1.AgentBashSuccess_Completed)
	if completed.Completed.GetTermination() != nil {
		t.Fatalf("Termination = %+v, want unset", completed.Completed.GetTermination())
	}
}

// THE TRUNCATION IS THIS PLANE'S TO STATE TOO. The stream plane and this one
// write the same unit under one upsert key, so a file-plane row claiming `whole`
// erases the truncation summary the other plane already drew.

func TestBashSuccessStatesTheWholeExtentWhenTheVendorDeclaredNoTotal(t *testing.T) {
	// Arrange: an ordinary result. Nothing was kept anywhere, so everything the
	// command printed is right here.
	call := openCall{input: map[string]any{"command": "echo hi"}}
	result := map[string]any{"stdout": "hi\n"}

	// Act
	got := bashSuccess(call, result, nil, nil, 1000)

	// Assert
	completed := got.GetOutcome().(*conversationv1.AgentBashSuccess_Completed)
	text := completed.Completed.GetOutput().GetText()
	if _, ok := text.GetExtent().(*conversationv1.AgentBashOutputText_Whole); !ok {
		t.Fatalf("Extent = %T, want AgentBashOutputText_Whole", text.GetExtent())
	}
}

func TestBashSuccessSubtractsTheInlineBytesFromTheDeclaredTotal(t *testing.T) {
	// Arrange: 200000 bytes were produced and 6 came back inline, so 199994 are
	// the figure a reader is owed.
	call := openCall{input: map[string]any{"command": "yes | head -100000"}}
	result := map[string]any{"stdout": "y\ny\ny\n", "persistedOutputSize": float64(200_000)}

	// Act
	got := bashSuccess(call, result, nil, nil, 1000)

	// Assert
	partial := bashPartialOf(t, got)
	if partial.GetBytesOmitted() != 199_994 {
		t.Fatalf("BytesOmitted = %d, want 199994", partial.GetBytesOmitted())
	}
}

func TestBashSuccessClampsAnOmittedFigureTheTotalCannotSupport(t *testing.T) {
	// Arrange: a total that TRAILS the inline bytes. Subtracting it unclamped
	// would draw "fewer bytes not shown", which is not a thing.
	call := openCall{input: map[string]any{"command": "echo abcde"}}
	result := map[string]any{"stdout": "abcde", "persistedOutputSize": float64(2)}

	// Act
	got := bashSuccess(call, result, nil, nil, 1000)

	// Assert
	partial := bashPartialOf(t, got)
	if partial.GetBytesOmitted() != 0 {
		t.Fatalf("BytesOmitted = %d, want 0", partial.GetBytesOmitted())
	}
}

func TestBashSuccessNamesTheFileTheWholeOutputWasSpilledTo(t *testing.T) {
	// Arrange: the producer kept the whole output, so the omitted bytes are
	// still fetchable and the path is the only place that is stated.
	call := openCall{input: map[string]any{"command": "yes | head -100000"}}
	result := map[string]any{
		"stdout":              "y\n",
		"persistedOutputSize": float64(200_000),
		"persistedOutputPath": "/spool/tool-results/spill.txt",
	}

	// Act
	got := bashSuccess(call, result, nil, nil, 1000)

	// Assert
	spilled := bashPartialOf(t, got).GetSpilled()
	if spilled.GetPath() != "/spool/tool-results/spill.txt" {
		t.Fatalf("Path = %q, want the spill file", spilled.GetPath())
	}
	if spilled.GetSizeBytes() != 200_000 {
		t.Fatalf("SizeBytes = %d, want the declared total 200000", spilled.GetSizeBytes())
	}
}

func TestBashSuccessLeavesTheSpillUnsetWhenTheOmittedBytesAreGone(t *testing.T) {
	// Arrange: a truncation with no path is a DEAD END, and the proto spells
	// that as an unset field rather than as a path that is not there.
	call := openCall{input: map[string]any{"command": "yes | head -100000"}}
	result := map[string]any{"stdout": "y\n", "persistedOutputSize": float64(200_000)}

	// Act
	got := bashSuccess(call, result, nil, nil, 1000)

	// Assert
	if spilled := bashPartialOf(t, got).GetSpilled(); spilled != nil {
		t.Fatalf("Spilled = %+v, want unset", spilled)
	}
}

func TestBashSuccessReadsTheTotalUnderTheVendorsSnakeCaseSpelling(t *testing.T) {
	// Arrange: the disk carries both spellings of one name, and reading only the
	// camelCase one would draw a truncated output as whole.
	call := openCall{input: map[string]any{"command": "yes | head -100000"}}
	result := map[string]any{"stdout": "y\n", "persisted_output_size": float64(1_002)}

	// Act
	got := bashSuccess(call, result, nil, nil, 1000)

	// Assert
	if omitted := bashPartialOf(t, got).GetBytesOmitted(); omitted != 1_000 {
		t.Fatalf("BytesOmitted = %d, want 1000", omitted)
	}
}

// bashPartialOf answers a settled shell's PARTIAL text extent, failing loudly on
// any other shape so a test never reads its figure off the wrong arm.
func bashPartialOf(t *testing.T, success *conversationv1.AgentBashSuccess) *conversationv1.AgentBashOutputPartial {
	t.Helper()
	completed, ok := success.GetOutcome().(*conversationv1.AgentBashSuccess_Completed)
	if !ok {
		t.Fatalf("Outcome = %T, want AgentBashSuccess_Completed", success.GetOutcome())
	}
	text := completed.Completed.GetOutput().GetText()
	partial, ok := text.GetExtent().(*conversationv1.AgentBashOutputText_Partial)
	if !ok {
		t.Fatalf("Extent = %T, want AgentBashOutputText_Partial", text.GetExtent())
	}
	return partial.Partial
}

func TestWakeupSuccessReadsTheStopsOwnReceipt(t *testing.T) {
	// Arrange: `stopped` in the output is the receipt, whatever the input said.
	call := openCall{input: map[string]any{}}
	result := map[string]any{"stopped": true, "cancelledWakeups": float64(3)}

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

func TestWakeupSuccessReadsTheScheduledInstantAsEpochMillis(t *testing.T) {
	// Arrange: the vendor states scheduledFor as epoch millis, which is the
	// fact the countdown ticks from.
	call := openCall{input: map[string]any{}}
	result := map[string]any{"scheduledFor": float64(1735689600000)}

	// Act
	got := wakeupSuccess(call, result)

	// Assert
	scheduled := got.GetOutcome().(*conversationv1.AgentScheduleWakeupSuccess_Scheduled)
	if scheduled.Scheduled.GetWakeAtMs() != 1735689600000 {
		t.Fatalf("WakeAtMs = %d, want 1735689600000", scheduled.Scheduled.GetWakeAtMs())
	}
}

func TestPushSuccessReadsTheVendorsDisabledReason(t *testing.T) {
	// Arrange: the decline reason rides under disabledReason, which is the
	// key the vendor writes.
	result := map[string]any{"pushSent": false, "localSent": false, "disabledReason": "user_present"}

	// Act
	got := pushSuccess(result, 1000)

	// Assert
	notSent, ok := got.GetOutcome().(*conversationv1.AgentPushNotificationSuccess_NotSent)
	if !ok {
		t.Fatalf("Outcome = %T, want AgentPushNotificationSuccess_NotSent", got.GetOutcome())
	}
	if _, ok := notSent.NotSent.GetReason().(*conversationv1.AgentPushNotificationNotSent_UserPresent); !ok {
		t.Fatalf("Reason = %T, want AgentPushNotificationNotSent_UserPresent", notSent.NotSent.GetReason())
	}
}

func TestPushSuccessLeavesAnUnknownDeclineReasonUnstated(t *testing.T) {
	// Arrange: a word outside the vendor's set. The delivery fact is still
	// stated; blaming config_off would name a setting nothing named.
	result := map[string]any{"pushSent": false, "disabledReason": "moon_phase"}

	// Act
	got := pushSuccess(result, 1000)

	// Assert
	notSent := got.GetOutcome().(*conversationv1.AgentPushNotificationSuccess_NotSent)
	if reason := notSent.NotSent.GetReason(); reason != nil {
		t.Fatalf("Reason = %T, want unset", reason)
	}
}

func TestPushSuccessReadsTheIsoSendInstant(t *testing.T) {
	// Arrange: sentAt is an ISO string on the wire and an instant here.
	result := map[string]any{"pushSent": true, "sentAt": "2026-01-01T00:00:00Z"}

	// Act
	got := pushSuccess(result, 1000)

	// Assert
	sent := got.GetOutcome().(*conversationv1.AgentPushNotificationSuccess_Sent)
	if sent.Sent.SentAtMs == nil || *sent.Sent.SentAtMs != 1767225600000 {
		t.Fatalf("SentAtMs = %v, want 1767225600000", sent.Sent.SentAtMs)
	}
}

func TestPushSuccessLeavesTheSendInstantUnsetWhenTheVendorStatedNone(t *testing.T) {
	// Arrange: resumed sessions replay pre-sentAt outputs verbatim, so the
	// instant is genuinely absent rather than the settle clock's reading.
	result := map[string]any{"pushSent": true}

	// Act
	got := pushSuccess(result, 1000)

	// Assert
	sent := got.GetOutcome().(*conversationv1.AgentPushNotificationSuccess_Sent)
	if sent.Sent.SentAtMs != nil {
		t.Fatalf("SentAtMs = %v, want unset", *sent.Sent.SentAtMs)
	}
}

func TestWebFetchSuccessReadsTheArtifactRouteFromTheDescriptorsPresence(t *testing.T) {
	// Arrange: the vendor states the artifact route by supplying the
	// descriptor, never by a boolean.
	call := openCall{input: map[string]any{"url": "https://claude.ai/public/artifacts/x"}}
	result := map[string]any{"code": float64(200), "artifactRead": map[string]any{"id": "x"}}

	// Act
	got := webFetchSuccess(call, result)

	// Assert
	if !got.GetArtifactRead() {
		t.Fatal("ArtifactRead = false, want true")
	}
}

func TestBashExitCodeReadsTheEndingStatedInTheReturnedText(t *testing.T) {
	// Arrange: a nonzero exit carries no structured field at all — the vendor
	// states the ending only in the text it returned to the model.
	block := map[string]any{"content": "Exit code 7\npartway\nto stderr"}

	// Act
	got := bashExitCode(nil, block, true)

	// Assert
	if got == nil || *got != 7 {
		t.Fatalf("bashExitCode = %v, want 7", got)
	}
}

func TestBashExitCodeIgnoresNumbersThatNameNoExit(t *testing.T) {
	// Arrange: output full of numbers states no ending, and reading the first
	// one would invent a status the shell never reported.
	block := map[string]any{"content": []any{map[string]any{"type": "text", "text": "4 files, 12 lines"}}}

	// Act
	got := bashExitCode(nil, block, true)

	// Assert
	if got != nil {
		t.Fatalf("bashExitCode = %d, want nil for text that names no exit", *got)
	}
}

func TestBashExitCodeIgnoresTheTextOfAResultTheVendorDidNotMarkAnError(t *testing.T) {
	// Arrange: a command that succeeded while PRINTING the words is output, not
	// a verdict, so its text is never mined for a status.
	block := map[string]any{"content": "Exit code 3"}

	// Act
	got := bashExitCode(nil, block, false)

	// Assert
	if got != nil {
		t.Fatalf("bashExitCode = %d, want nil for a result the vendor did not mark an error", *got)
	}
}

func TestBashExitCodeIgnoresAStatusNamedMidLine(t *testing.T) {
	// Arrange: the vendor states the ending on a line of its own, so a mention
	// inside a line of output is prose the command printed.
	block := map[string]any{"content": "make: recipe returned exit code 2 for the stale target\nError: EACCES"}

	// Act
	got := bashExitCode(nil, block, true)

	// Assert
	if got != nil {
		t.Fatalf("bashExitCode = %d, want nil for a status named mid-line", *got)
	}
}

func TestReadSuccessLeavesTheExtentUnsetForAnImageRead(t *testing.T) {
	// Arrange: an image read. AgentReadSuccess retired the image extent this
	// wave, so no arm can state how much came back — and `whole` with empty
	// contents would claim an empty file that was never read.
	call := openCall{input: map[string]any{"file_path": "/p/shot.png"}}
	result := map[string]any{"type": "image", "file": map[string]any{
		"filePath": "/p/shot.png",
		"type":     "image/png",
	}}

	// Act
	got := readSuccess(call, result, 1000)

	// Assert
	if got.GetExtent() != nil {
		t.Fatalf("Extent = %T, want unset for a non-text read", got.GetExtent())
	}
	if got.GetPath().GetPath() != "/p/shot.png" {
		t.Fatalf("Path = %q, want the read's path stated even with no extent", got.GetPath().GetPath())
	}
	if got.GetSettledAt() == nil {
		t.Fatal("SettledAt = nil, want the settle instant: the read did finish")
	}
}

// THE REFUSAL PROSE IS LOAD-BEARING. It is the only thing the vendor says
// about a refused send — it declares no refusal code — and the frontend's
// `refused` delivery arm draws it as the reason (feed.proto, landing 14).
// The transcript is the delivery that carries it when no shim was watching.
func TestSendMessageFailureCarriesTheRefusalProseIntoItsContent(t *testing.T) {
	// Arrange
	const prose = "Agent a85a6434719755df1 was stopped by the user and won't be resumed."
	c := newTestConverter(t)
	call := openCall{input: map[string]any{"to": "a85a6434719755df1"}}
	block := map[string]any{"content": prose}

	// Act
	got := c.settledItem(kindSendMessage, call, nil, block, true, 1000, Attribution{})

	// Assert
	failure := got.GetSendMessage().GetFailure()
	if failure == nil {
		t.Fatalf("result = %T, want AgentSendMessage_Failure", got.GetSendMessage().GetResult())
	}
	blocks := failure.GetError().GetContent().GetBlocks()
	if len(blocks) != 1 || blocks[0].GetText().GetText() != prose {
		t.Fatalf("failure content = %v, want the refusal prose %q verbatim", blocks, prose)
	}
}

// A BACKGROUNDED COMMAND DID NOT END, IT MOVED. Both planes write this unit
// under one upsert key, so a file-plane terminal arriving second replaced the
// stream plane's LIVE card with a settled one carrying no output at all.

func TestBashSettledProducesNoTerminalForACommandThatMovedToTheBackground(t *testing.T) {
	// Arrange: the vendor's receipt for a launch -- empty output and a task id.
	c := newTestConverter(t)
	call := openCall{input: map[string]any{"command": "sleep 600"}}
	result := map[string]any{"stdout": "", "backgroundTaskId": "b6d426ca0", "timedOutAfterMs": float64(120_000)}

	// Act
	got := c.settledItem(kindBash, call, result, nil, false, 1000, Attribution{})

	// Assert: nothing at all. The detached-work frames naming this unit are
	// what say where the work went.
	if got != nil {
		t.Fatalf("settledItem = %v, want no frame for a command that moved rather than ended", got)
	}
}

func TestBashSettledReadsTheTaskIdUnderTheVendorsSnakeCaseSpelling(t *testing.T) {
	// Arrange: the disk carries both spellings of one name, and reading only
	// the camelCase one would settle a run that is still going.
	c := newTestConverter(t)
	call := openCall{input: map[string]any{"command": "sleep 600"}}
	result := map[string]any{"stdout": "", "background_task_id": "b6d426ca0"}

	// Act
	got := c.settledItem(kindBash, call, result, nil, false, 1000, Attribution{})

	// Assert
	if got != nil {
		t.Fatalf("settledItem = %v, want no frame for a command that moved rather than ended", got)
	}
}

func TestBashSettledStillTerminatesACommandThatNamedNoBackgroundTask(t *testing.T) {
	// Arrange: an ordinary foreground command. The silence above must not
	// swallow the terminal every other shell result owes.
	c := newTestConverter(t)
	call := openCall{input: map[string]any{"command": "echo hi"}}
	result := map[string]any{"stdout": "hi\n"}

	// Act
	got := c.settledItem(kindBash, call, result, nil, false, 1000, Attribution{})

	// Assert
	if got.GetBash().GetSuccess() == nil {
		t.Fatalf("result = %T, want AgentBash_Success", got.GetBash().GetResult())
	}
}

// imageResultBlock is a `tool_result` answering with one base64 image block.
func imageResultBlock(mediaType, data string) map[string]any {
	return map[string]any{"content": []any{
		map[string]any{"type": "image", "source": map[string]any{
			"type":       "base64",
			"media_type": mediaType,
			"data":       data,
		}},
	}}
}

func TestBashSuccessCarriesTheImageBytesFromTheResultBlock(t *testing.T) {
	// Arrange: the Output object says only THAT the output was an image; the
	// bytes and the media type live on the answering result block.
	call := openCall{input: map[string]any{"command": "screencapture -x -"}}
	result := map[string]any{"stdout": "iVBORw==", "isImage": true}

	// Act
	got := bashSuccess(call, result, imageResultBlock("image/png", "iVBORw=="), nil, 1000)

	// Assert
	completed := got.GetOutcome().(*conversationv1.AgentBashSuccess_Completed)
	image, ok := completed.Completed.GetOutput().GetForm().(*conversationv1.AgentBashOutput_Image)
	if !ok {
		t.Fatalf("Form = %T, want AgentBashOutput_Image", completed.Completed.GetOutput().GetForm())
	}
	if string(image.Image.GetData()) != "\x89PNG" {
		t.Fatalf("Data = %q, want the decoded bytes", image.Image.GetData())
	}
	if image.Image.GetMediaType() != "image/png" {
		t.Fatalf("MediaType = %q, want image/png", image.Image.GetMediaType())
	}
}

func TestBashResultImageRefusesAPayloadThatDoesNotDecode(t *testing.T) {
	// Arrange: a payload we cannot reconstruct is not half-carried.
	block := imageResultBlock("image/png", "not base64 at all!!")

	// Act
	data, mediaType := bashResultImage(block)

	// Assert: the media type still comes back so a refusal can name it.
	if data != nil {
		t.Fatalf("Data = %q, want nil for an undecodable payload", data)
	}
	if mediaType != "image/png" {
		t.Fatalf("MediaType = %q, want image/png", mediaType)
	}
}

func TestBashResultImageAnswersNothingForATextOnlyResult(t *testing.T) {
	// Arrange
	block := map[string]any{"content": []any{
		map[string]any{"type": "text", "text": "hello"},
	}}

	// Act
	data, mediaType := bashResultImage(block)

	// Assert
	if data != nil || mediaType != "" {
		t.Fatalf("data, mediaType = %q, %q, want both empty", data, mediaType)
	}
}

// TestWritePatchDiffsTheVersionsRatherThanPreferringTheStatedPatch pins the
// proto's own words on AgentWriteSuccess.patch -- "The producer diffs after
// the fact" -- against the deviation that once stood here.
//
// This plane preferred `structuredPatch` when the vendor stated one, while the
// stream plane always diffs. The same update then drew a different hunk
// depending on WHICH PRODUCER settled the unit first, which is a race a card's
// content must never turn on.
func TestWritePatchDiffsTheVersionsRatherThanPreferringTheStatedPatch(t *testing.T) {
	// Arrange: the vendor states a patch AND hands over both versions.
	result := map[string]any{
		"type":         "update",
		"filePath":     "/w/s/a.ts",
		"originalFile": "one\ntwo\n",
		"content":      "one\ntwo\nthree\n",
		"structuredPatch": []any{map[string]any{
			"oldStart": float64(2), "oldLines": float64(1),
			"newStart": float64(2), "newLines": float64(2),
			"lines": []any{"   two", "+  three"},
		}},
	}

	// Act
	got := writePatch(result)

	// Assert: the DIFF's own hunk, not the vendor's.
	if len(got) != 1 {
		t.Fatalf("hunks = %d, want 1", len(got))
	}
	want := []string{" one", " two", "+three"}
	if lines := got[0].GetLines(); !slices.Equal(lines, want) {
		t.Fatalf("lines = %q, want %q: the producer diffs, it does not restate the vendor's patch", lines, want)
	}
}

// TestWritePatchKeepsTheStatedPatchWhenThereIsNoContentToDiff is the other
// half: nothing can be diffed without the new contents, and the vendor's own
// patch is then the only account of the change there is.
func TestWritePatchKeepsTheStatedPatchWhenThereIsNoContentToDiff(t *testing.T) {
	// Arrange: a result carrying a patch and no content at all.
	result := map[string]any{
		"type": "update",
		"structuredPatch": []any{map[string]any{
			"oldStart": float64(2), "oldLines": float64(1),
			"newStart": float64(2), "newLines": float64(2),
			"lines": []any{"   two", "+  three"},
		}},
	}

	// Act
	got := writePatch(result)

	// Assert
	if len(got) != 1 {
		t.Fatalf("hunks = %d, want the vendor's own patch kept rather than dropped", len(got))
	}
}
