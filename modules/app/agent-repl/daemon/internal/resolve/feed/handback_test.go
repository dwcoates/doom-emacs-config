package feed

import (
	"slices"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE SUBAGENT'S RETURNED RESULT: a hand-back draws once, as the result unit
// on the subagent's own feed.

// handbackOf is one hand-back frame of unit "unit-hb" carrying ARM.
func handbackOf(arm any) *conversationv1.AgentActivity {
	handback := &conversationv1.AgentSubagentHandback{}
	switch a := arm.(type) {
	case *conversationv1.AgentSubagentHandbackStart:
		handback.Result = &conversationv1.AgentSubagentHandback_Start{Start: a}
	case *conversationv1.AgentToolCallProgress:
		handback.Result = &conversationv1.AgentSubagentHandback_Progress{Progress: a}
	case *conversationv1.AgentSubagentHandbackSuccess:
		handback.Result = &conversationv1.AgentSubagentHandback_Success{Success: a}
	case *conversationv1.AgentSubagentHandbackFailure:
		handback.Result = &conversationv1.AgentSubagentHandback_Failure{Failure: a}
	}
	return bound(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-hb"},
		Item:       &conversationv1.AgentActivity_SubagentHandback{SubagentHandback: handback},
	})
}

func report(text string) *conversationv1.AgentSubagentHandbackReport {
	return &conversationv1.AgentSubagentHandbackReport{Text: text}
}

// handbackHarness is a harness with one spawned subagent whose feed the
// hand-back lands on.
func handbackHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.spawnSubagent("unit-1", subAgent(), "explore", "go and look")
	return h
}

// result is the sub-feed's one subagent-result unit.
func (h *harness) result() *frontendv1.FeedSubagentResult {
	h.t.Helper()
	for _, row := range h.rows(subFeed()) {
		if got := row.GetActivity().GetSubagentResult(); got != nil {
			return got
		}
	}
	h.t.Fatalf("sub-feed rows = %v, want a subagent_result unit", rowIDs(h.rows(subFeed())))
	return nil
}

func TestAHandbackStartDrawsTheResultDelivering(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)

	// Act.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackStart{Report: report("done")}))

	// Assert.
	if h.result().GetDelivering() == nil {
		t.Fatalf("state = %T, want delivering", h.result().GetState())
	}
}

func TestAHandbackDrawsTheReportVerbatim(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)

	// Act.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackStart{Report: report("## Commits\n1. done")}))

	// Assert.
	if got := h.result().GetReport().GetText(); got != "## Commits\n1. done" {
		t.Fatalf("report = %q, want the report verbatim", got)
	}
}

func TestAHandbackBeatKeepsTheHeldReportDelivering(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackStart{Report: report("done")}))

	// Act.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentToolCallProgress{LastProgressAtMs: 5_000}))

	// Assert.
	if got := h.result().GetReport().GetText(); got != "done" {
		t.Fatalf("report after a beat = %q, want the held report", got)
	}
}

func TestAHandbackBeatWithNoReportHeldDrawsNothing(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)

	// Act.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentToolCallProgress{LastProgressAtMs: 5_000}))

	// Assert.
	for _, row := range h.rows(subFeed()) {
		if row.GetActivity().GetSubagentResult() != nil {
			t.Fatal("a beat with no report held drew a result claiming an empty report")
		}
	}
}

func TestAHandbackSuccessDrawsTheResultDelivered(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)

	// Act.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackSuccess{Report: report("done")}))

	// Assert.
	if h.result().GetDelivered() == nil {
		t.Fatalf("state = %T, want delivered", h.result().GetState())
	}
}

func TestAHandbackFailureDrawsTheResultUndeliveredWithTheReason(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)
	failure := &conversationv1.AgentToolFailure{Content: &conversationv1.ToolResultContent{
		Blocks: []*conversationv1.ToolResultContentBlock{{
			Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: "the parent is gone"}},
		}},
	}}

	// Act.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackFailure{Error: failure, Report: report("done")}))

	// Assert.
	if got := h.result().GetUndelivered().GetReason().GetText(); got != "the parent is gone" {
		t.Fatalf("reason = %q, want the vendor's refusal text", got)
	}
}

func TestAHandbackFailureWithNoTextLeavesTheReasonUnset(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)

	// Act.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackFailure{Report: report("done")}))

	// Assert.
	if h.result().GetUndelivered().Reason != nil {
		t.Fatal("a failure stating nothing drew a reason")
	}
}

func TestAHandbackWithNoResultArmIsRefusedLoudly(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)

	// Act.
	h.sendAs(subAgent(), handbackOf(nil))

	// Assert.
	if !slices.Contains(h.anyErrors(), "daemon.feed.activity_undrawable") {
		t.Fatalf("errors = %v, want the hand-back refused as undrawable", h.anyErrors())
	}
}

func TestAHandbackStartAndItsSettleUpsertOneRow(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackStart{Report: report("done")}))

	// Act.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackSuccess{Report: report("done")}))

	// Assert.
	results := 0
	for _, row := range h.rows(subFeed()) {
		if row.GetActivity().GetSubagentResult() != nil {
			results++
		}
	}
	if results != 1 {
		t.Fatalf("result units = %d, want the one upserted unit", results)
	}
}

func TestAReplayedHandbackDrawsWhatTheLiveFrameDrew(t *testing.T) {
	// Arrange: the same settled frame, drawn live on one harness and replayed
	// on another.
	settle := func() *conversationv1.AgentActivity {
		return handbackOf(&conversationv1.AgentSubagentHandbackSuccess{Report: report("done")})
	}
	live := handbackHarness(t)
	replayed := handbackHarness(t)

	// Act.
	live.sendAs(subAgent(), settle())
	replayed.resolver.OnHistoryPage(testWorkspace, subAgent(), historyPage(&conversationv1.HistoryFloor{},
		frameEntry(subAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: settle()}}),
	), noAddress())

	// Assert.
	if !proto.Equal(live.result(), replayed.result()) {
		t.Fatalf("live %v != replayed %v", live.result(), replayed.result())
	}
}

func TestAHandbackDrawsTheResultOnlyOnTheSubagentFeedAndTheBadgeOnlyOnTheRoot(t *testing.T) {
	// Arrange.
	h := handbackHarness(t)

	// Act: the subagent's own hand-back, and its delivery to the parent.
	h.sendAs(subAgent(), handbackOf(&conversationv1.AgentSubagentHandbackSuccess{Report: report("done")}))
	h.peerMessageOf("p1", "agent-sub", handbackPeer())

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetActivity().GetSubagentResult() != nil {
			t.Fatal("the result was drawn on the root feed beside the badge")
		}
	}
	for _, row := range h.rows(subFeed()) {
		if row.GetSubagentHandback() != nil {
			t.Fatal("the badge was drawn on the subagent's feed beside the result")
		}
	}
}
