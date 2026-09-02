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
