package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/resolve/feed"
)

// THE TWO INPUTS TO A STANDING ASK: the card's clicks edit the question draft,
// a typed send answers the oldest standing ask, and the edit that finishes a
// batch delivers it through AnswerQuestion's own checks.

// hasLogRecord reports whether the fixture's log holds LEVEL for OPERATION.
func hasLogRecord(f *fixture, level, operation string) bool {
	for _, r := range f.log.logger.Records() {
		if r.Level == level && r.Operation == operation {
			return true
		}
	}
	return false
}

// standingBatch serves q-1 to agent-2 and makes the draft finish it.
func standingBatch(f *fixture) {
	f.cards.questions["q-1"] = ServedQuestion{Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch()}
	f.cards.draftOutcome = feed.QuestionDraftOutcome{
		Deliver: &conversationv1.AgentQuestionAnswers{Answers: []*conversationv1.AgentQuestionSelection{
			selection("pick one", "a"), selection("pick many", "x"),
		}},
		Agent: &conversationv1.AgentId{Value: "agent-2"},
	}
}

func TestUpdateQuestionDraftAppliesTheEditWithoutDeliveringAnUnfinishedBatch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	edit := feed.QuestionEdit{Kind: feed.EditPick, Question: "pick one", Label: "a"}

	// Act.
	err := f.verbs.UpdateQuestionDraft(context.Background(), "w1", "q-1", edit)

	// Assert.
	if err != nil {
		t.Fatalf("UpdateQuestionDraft: %v", err)
	}
	if len(f.cards.edits) != 1 || f.cards.edits[0].ask != "q-1" || f.cards.edits[0].edit != edit {
		t.Fatalf("edits = %+v, want the one edit on q-1", f.cards.edits)
	}
	if len(f.shim.answers) != 0 {
		t.Fatalf("delivered = %+v, want nothing for an unfinished batch", f.shim.answers)
	}
}

func TestUpdateQuestionDraftDeliversTheBatchItsEditFinished(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingBatch(f)

	// Act.
	err := f.verbs.UpdateQuestionDraft(context.Background(), "w1", "q-1",
		feed.QuestionEdit{Kind: feed.EditConfirm, Question: "pick many"})

	// Assert.
	if err != nil {
		t.Fatalf("UpdateQuestionDraft: %v", err)
	}
	if len(f.shim.answers) != 1 || f.shim.answers[0].Agent != "agent-2" {
		t.Fatalf("delivered = %+v, want the batch delivered to agent-2", f.shim.answers)
	}
	got := f.shim.answers[0].Answer.GetQuestionAnswer()
	if got.GetAsk().GetValue() != "q-1" || len(got.GetAnswers().GetAnswers()) != 2 {
		t.Fatalf("delivered answer = %+v, want both of q-1's answers", got)
	}
	if !hasLogRecord(f, "info", opQuestionDraft) {
		t.Fatalf("records = %+v, want the delivery recorded", f.log.logger.Records())
	}
	if len(f.cards.released) != 0 {
		t.Fatalf("released = %v, want the claim kept by a landed delivery", f.cards.released)
	}
}

func TestAFailedDeliveryReleasesTheBatchsClaim(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingBatch(f)
	f.shim.answerErr = errors.New("shim unreachable")

	// Act.
	err := f.verbs.UpdateQuestionDraft(context.Background(), "w1", "q-1",
		feed.QuestionEdit{Kind: feed.EditConfirm, Question: "pick many"})

	// Assert.
	if err == nil {
		t.Fatal("a failed delivery answered success")
	}
	if len(f.cards.released) != 1 || f.cards.released[0] != "q-1" {
		t.Fatalf("released = %v, want q-1's claim freed", f.cards.released)
	}
	if !hasLogRecord(f, "error", opAnswerQuestion) {
		t.Fatalf("records = %+v, want the failed delivery recorded by AnswerQuestion", f.log.logger.Records())
	}
}

func TestUpdateQuestionDraftRefusesAnAskThatIsNotStanding(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.draftErr = feed.ErrQuestionNotStanding

	// Act.
	err := f.verbs.UpdateQuestionDraft(context.Background(), "w1", "q-1",
		feed.QuestionEdit{Kind: feed.EditShow, Question: "pick one"})

	// Assert.
	refusal := asRefusal(t, err, ArmAskNotStanding)
	if !refusal.NotFound {
		t.Fatal("an ask that is not standing was not refused as not found")
	}
	if !hasLogRecord(f, "info", opRefusal) {
		t.Fatalf("records = %+v, want the refusal recorded", f.log.logger.Records())
	}
}

func TestUpdateQuestionDraftRefusesAnUnservedValueNamingIt(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.draftErr = &feed.UnservedQuestionValueError{Text: "c", Why: "the question never offered the option"}

	// Act.
	err := f.verbs.UpdateQuestionDraft(context.Background(), "w1", "q-1",
		feed.QuestionEdit{Kind: feed.EditPick, Question: "pick one", Label: "c"})

	// Assert.
	refusal := asRefusal(t, err, ArmUnservedAnswer)
	if refusal.Fields["text"] != "c" {
		t.Fatalf("fields = %+v, want the unserved text carried", refusal.Fields)
	}
}

func TestUpdateQuestionDraftSurfacesAnUnexpectedEditFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.draftErr = errors.New("resolver broke")

	// Act.
	err := f.verbs.UpdateQuestionDraft(context.Background(), "w1", "q-1",
		feed.QuestionEdit{Kind: feed.EditShow, Question: "pick one"})

	// Assert.
	if err == nil {
		t.Fatal("an unexpected edit failure answered success")
	}
	if _, refused := AsRefusal(err); refused {
		t.Fatalf("err = %v, want an unexpected failure, not a typed refusal", err)
	}
	if !hasLogRecord(f, "error", opQuestionDraft) {
		t.Fatalf("records = %+v, want the failure recorded", f.log.logger.Records())
	}
}

func TestAnswerStandingAskAnswersFalseWhenNoAskStands(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	answered, err := f.verbs.AnswerStandingAsk(context.Background(), "w1", nil, "hello")

	// Assert.
	if err != nil || answered {
		t.Fatalf("answered = %v, err = %v; want false and no error", answered, err)
	}
	if len(f.cards.edits) != 0 || len(f.shim.answers) != 0 {
		t.Fatalf("edits = %+v, delivered = %+v; want nothing done", f.cards.edits, f.shim.answers)
	}
}

func TestAnswerStandingAskLooksOnTheAddressedFeed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	target := &feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-1"}}

	// Act.
	if _, err := f.verbs.AnswerStandingAsk(context.Background(), "w1", target, "hello"); err != nil {
		t.Fatalf("AnswerStandingAsk: %v", err)
	}

	// Assert.
	if len(f.cards.targets) != 1 || f.cards.targets[0] != target {
		t.Fatalf("targets = %+v, want the addressed bubble", f.cards.targets)
	}
}

func TestAnswerStandingAskTypesTheTextIntoAStandingQuestion(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.standing, f.cards.hasStanding = feed.StandingAsk{Kind: feed.AskQuestion, ID: "q-1"}, true

	// Act.
	answered, err := f.verbs.AnswerStandingAsk(context.Background(), "w1", nil, "something else")

	// Assert.
	if err != nil || !answered {
		t.Fatalf("answered = %v, err = %v; want answered", answered, err)
	}
	want := fakeDraftEdit{ask: "q-1", edit: feed.QuestionEdit{Kind: feed.EditType, Text: "something else"}}
	if len(f.cards.edits) != 1 || f.cards.edits[0] != want {
		t.Fatalf("edits = %+v, want %+v", f.cards.edits, want)
	}
}

func TestAnswerStandingAskDeniesAStandingPermissionWithTheTextAsTheReason(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.standing, f.cards.hasStanding = feed.StandingAsk{Kind: feed.AskPermission, ID: "perm-1"}, true
	f.cards.permissions["perm-1"] = ServedPermission{Agent: &conversationv1.AgentId{Value: "agent-9"}}

	// Act.
	answered, err := f.verbs.AnswerStandingAsk(context.Background(), "w1", nil, "use the read-only path")

	// Assert.
	if err != nil || !answered {
		t.Fatalf("answered = %v, err = %v; want answered", answered, err)
	}
	if len(f.shim.answers) != 1 || f.shim.answers[0].Agent != "agent-9" {
		t.Fatalf("delivered = %+v, want one answer to agent-9", f.shim.answers)
	}
	decision := f.shim.answers[0].Answer.GetPermissionDecision()
	if decision.GetAsk().GetValue() != "perm-1" || decision.GetDenied().GetMessage() != "use the read-only path" {
		t.Fatalf("decision = %+v, want perm-1 denied with the typed reason", decision)
	}
}

func TestAnswerStandingAskSurfacesTheQuestionsRefusal(t *testing.T) {
	// Arrange: the batch was claimed between the lookup and the edit.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.standing, f.cards.hasStanding = feed.StandingAsk{Kind: feed.AskQuestion, ID: "q-1"}, true
	f.cards.draftErr = feed.ErrQuestionNotStanding

	// Act.
	answered, err := f.verbs.AnswerStandingAsk(context.Background(), "w1", nil, "late")

	// Assert.
	if !answered {
		t.Fatal("a send routed to a standing ask reported it was not routed")
	}
	asRefusal(t, err, ArmAskNotStanding)
}

func TestDeniedByUserCarriesTheReasonVerbatim(t *testing.T) {
	tests := []struct {
		name    string
		message string
	}{
		{"a typed reason", "use the read-only path"},
		{"no reason", ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := DeniedByUser(tc.message)

			// Assert.
			if got.Denied.GetMessage() != tc.message {
				t.Fatalf("message = %q, want %q", got.Denied.GetMessage(), tc.message)
			}
		})
	}
}
