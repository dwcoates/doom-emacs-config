package feed

import (
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// THE HOST VIEW'S ASK: the root feed's oldest standing card, projected onto
// the numbered choices Emacs answers, kept off the feed's lock and announced
// on every move.

// hostAskOptionLabels answers a question view's option labels, in order.
func hostAskOptionLabels(q *agentreplv1.HostAskQuestion) []string {
	options := q.GetSingleSelect().GetOptions()
	if multi := q.GetMultiSelect(); multi != nil {
		options = multi.GetOptions()
	}
	labels := make([]string, 0, len(options))
	for _, o := range options {
		labels = append(labels, o.GetLabel())
	}
	return labels
}

// openStandingPermission poses a permission that offers a standing form.
func openStandingPermission(h *harness, askID string) {
	h.t.Helper()
	h.ask(askID, "", &conversationv1.AgentPermissionStart{
		Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run ls"},
		OfferedStanding: &conversationv1.AgentPermissionStanding{
			Changes: []*conversationv1.AgentPermissionChange{{
				Destination: conversationv1.AgentPermissionDestination_AGENT_PERMISSION_DESTINATION_SESSION,
			}},
		},
	})
}

func TestNoHostAskStandsOnAQuietFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	got := h.resolver.HostAsk(testWorkspace)

	// Assert.
	if got != nil {
		t.Fatalf("host ask = %+v, want none", got)
	}
}

func TestAStandingPermissionIsTheHostAskWithItsStandingOffer(t *testing.T) {
	tests := []struct {
		name         string
		open         func(h *harness)
		wantStanding bool
	}{
		{"no standing offered", func(h *harness) { openPermission(h, "perm-1") }, false},
		{"a standing offered", func(h *harness) { openStandingPermission(h, "perm-1") }, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			tc.open(h)

			// Assert.
			got := h.resolver.HostAsk(testWorkspace)
			if got.GetPermission() == nil {
				t.Fatalf("host ask = %+v, want a permission", got)
			}
			if got.GetRow().GetValue() != h.only(rootFeed()).GetId().GetValue() {
				t.Fatalf("row = %q, want the card's row %q", got.GetRow().GetValue(), h.only(rootFeed()).GetId().GetValue())
			}
			if (got.GetPermission().GetStandingOffered() != nil) != tc.wantStanding {
				t.Fatalf("standing offered = %v, want %v", got.GetPermission().GetStandingOffered() != nil, tc.wantStanding)
			}
		})
	}
}

func TestAStandingQuestionIsTheHostAskShowingItsActiveQuestion(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.openTwoQuestionAsk()

	// Assert.
	q := h.resolver.HostAsk(testWorkspace).GetQuestion()
	if q.GetQuestionText() != "Which auth method?" || q.GetSingleSelect() == nil {
		t.Fatalf("question = %+v, want the single-select first question", q)
	}
	if got := hostAskOptionLabels(q); len(got) != 2 || got[0] != "OAuth" || got[1] != "API key" {
		t.Fatalf("options = %v, want [OAuth API key] in served order", got)
	}
}

func TestTheHostAskFollowsTheShownQuestionToAMultiSelect(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Act.
	h.edit(pickAuth("OAuth"))

	// Assert.
	q := h.resolver.HostAsk(testWorkspace).GetQuestion()
	if q.GetQuestionText() != "Which suites?" || q.GetMultiSelect() == nil {
		t.Fatalf("question = %+v, want the multi-select second question", q)
	}
	if got := hostAskOptionLabels(q); len(got) != 2 || got[0] != "daemon" || got[1] != "webapp" {
		t.Fatalf("options = %v, want [daemon webapp]", got)
	}
}

func TestTheHostAskIsTheOldestStandingCard(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()

	// Act.
	openPermission(h, "perm-1")

	// Assert.
	if h.resolver.HostAsk(testWorkspace).GetQuestion() == nil {
		t.Fatal("the host ask is not the older question")
	}
}

func TestTheHostAskClearsWhenItsCardSettles(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	openPermission(h, "perm-1")

	// Act.
	h.ask("perm-1", "", &conversationv1.AgentPermissionFailure{})

	// Assert.
	if got := h.resolver.HostAsk(testWorkspace); got != nil {
		t.Fatalf("host ask = %+v, want none once the card settled", got)
	}
}

func TestAClaimedBatchLeavesTheHostAskUntilItsDeliveryIsReleased(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	h.edit(pickAuth("OAuth"))
	h.edit(QuestionEdit{Kind: EditType, Text: "all of them"})
	if got := h.resolver.HostAsk(testWorkspace); got != nil {
		t.Fatalf("host ask = %+v, want none while the batch is claimed", got)
	}

	// Act.
	h.resolver.ReleaseQuestionDelivery(testWorkspace, "ask-1")

	// Assert.
	if h.resolver.HostAsk(testWorkspace).GetQuestion() == nil {
		t.Fatal("the released batch is not the host ask again")
	}
}

func TestAResetClearsTheHostAsk(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	openPermission(h, "perm-1")

	// Act.
	h.resolver.ResetWorkspace(testWorkspace, "bound to another conversation")

	// Assert.
	if got := h.resolver.HostAsk(testWorkspace); got != nil {
		t.Fatalf("host ask = %+v, want none after a reset", got)
	}
}

func TestASubFeedAskIsNotTheHostAsk(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-sub"}
	h.spawnSubagent("unit-1", created, "general-purpose", "")

	// Act.
	h.resolver.OnPermission(testWorkspace, created, &conversationv1.AgentPermission{
		Id: &conversationv1.AgentPermissionId{Value: "perm-sub"},
		Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{
			Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run ls"},
		}},
	}, nil, nil)

	// Assert.
	if got := h.resolver.HostAsk(testWorkspace); got != nil {
		t.Fatalf("host ask = %+v, want none for a sub-feed's card", got)
	}
}

func TestEveryHostAskMoveIsAnnouncedAndARestatementIsNot(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	moves := h.resolver.WatchHostAsks(ctx)

	// Act: open, restate the same card unchanged, then settle.
	openPermission(h, "perm-1")
	openPermission(h, "perm-1")
	h.ask("perm-1", "", &conversationv1.AgentPermissionFailure{})

	// Assert: two moves (open, settle); the restatement moved nothing.
	for i := 0; i < 2; i++ {
		if ws := <-moves; ws != testWorkspace {
			t.Fatalf("move %d named %q, want %q", i, ws, testWorkspace)
		}
	}
	if got := dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Operation: opHostAsk, Level: "info"}); len(got) != 2 {
		t.Fatalf("host ask records = %d, want 2 (one per move)", len(got))
	}
	select {
	case ws := <-moves:
		t.Fatalf("an unexpected third move named %q", ws)
	default:
	}
}

func TestAHostAskMoveReplacesTheStoredViewRatherThanEditingIt(t *testing.T) {
	// Arrange: a reader holds the view of the first question.
	h := newHarness(t)
	h.openTwoQuestionAsk()
	held := h.resolver.HostAsk(testWorkspace)

	// Act: the shown question moves on.
	h.edit(pickAuth("OAuth"))

	// Assert: the held view still names the question it was taken for.
	if held.GetQuestion().GetQuestionText() != "Which auth method?" {
		t.Fatalf("held view = %q, want it untouched by the move", held.GetQuestion().GetQuestionText())
	}
}

func TestAQuestionWithNoSelectionArmIsLeftOutOfTheHostViewLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	batch := &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
		Question: &conversationv1.AgentQuestionText{Text: "Bare?"},
	}}}

	// Act.
	h.pose("ask-1", &conversationv1.AgentQuestionStart{Batch: batch})

	// Assert.
	if got := h.resolver.HostAsk(testWorkspace); got != nil {
		t.Fatalf("host ask = %+v, want none for a question with nothing to number", got)
	}
	errs := dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Operation: opHostAsk, Level: "error"})
	if len(errs) != 1 || errs[0].Context["question"] != 0 {
		t.Fatalf("errors = %+v, want one naming the shown question", errs)
	}
}

func TestTheHostAskAndATypedAnswerNameTheSameCard(t *testing.T) {
	// Both read oldestStandingAsk, so whichever card a typed answer goes to is
	// the card a typed number answers.
	tests := []struct {
		name string
		draw func(h *harness)
	}{
		{"a permission first", func(h *harness) { openPermission(h, "perm-1"); h.openTwoQuestionAsk() }},
		{"a question first", func(h *harness) { h.openTwoQuestionAsk(); openPermission(h, "perm-1") }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tc.draw(h)

			// Act.
			typed, found := h.resolver.StandingAsk(testWorkspace, nil)
			host := h.resolver.HostAsk(testWorkspace)

			// Assert.
			if !found || host == nil {
				t.Fatalf("typed found %v, host %+v: want both standing", found, host)
			}
			wantKind := AskQuestion
			if host.GetPermission() != nil {
				wantKind = AskPermission
			}
			if typed.Kind != wantKind {
				t.Fatalf("typed answer goes to %v, host ask is %v", typed.Kind, wantKind)
			}
		})
	}
}
