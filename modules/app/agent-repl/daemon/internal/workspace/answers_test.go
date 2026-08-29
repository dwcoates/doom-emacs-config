package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// permissionAnswer composes one permission answer around a built decision.
// The decision oneof's wrapper types are unexported at the interface level, so
// each caller builds its own arm and this helper wraps it.
func permissionAnswer(id string, decision *conversationv1.AgentPermissionDecision) *conversationv1.AgentAnswer {
	decision.Ask = &conversationv1.AgentPermissionId{Value: id}
	return &conversationv1.AgentAnswer{
		Answer: &conversationv1.AgentAnswer_PermissionDecision{PermissionDecision: decision},
	}
}

// allowedStanding is the allow_standing decision the offer gate turns on.
func allowedStanding() *conversationv1.AgentPermissionDecision {
	return &conversationv1.AgentPermissionDecision{
		Decision: &conversationv1.AgentPermissionDecision_Allowed{
			Allowed: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Standing{
					Standing: &conversationv1.AgentPermissionAllowedStanding{},
				},
			},
		},
	}
}

// allowedOnce is the once-only allow decision, which needs no offer.
func allowedOnce() *conversationv1.AgentPermissionDecision {
	return &conversationv1.AgentPermissionDecision{
		Decision: &conversationv1.AgentPermissionDecision_Allowed{
			Allowed: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Once{Once: &conversationv1.AgentPermissionAllowedOnce{}},
			},
		},
	}
}

func TestAnswerPermissionDeliversToTheAskingAgent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.permissions["ask-1"] = ServedPermission{Agent: &conversationv1.AgentId{Value: "agent-9"}}

	// Act.
	err := f.verbs.AnswerPermission(context.Background(), "w1", permissionAnswer("ask-1", allowedOnce()))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerPermission: %v", err)
	}
	if len(f.shim.answers) != 1 || f.shim.answers[0].Agent != "agent-9" {
		t.Fatalf("delivered answers = %+v, want one to agent-9", f.shim.answers)
	}
}

func TestAnswerPermissionRefusesAnAskThatIsNotStanding(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.AnswerPermission(context.Background(), "w1", permissionAnswer("ask-1", allowedOnce()))

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
}

func TestAnswerPermissionRefusesAllowStandingWithNoOffer(t *testing.T) {
	// Arrange: granting a standing permission the vendor never offered would
	// write a rule the user was never shown.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.permissions["ask-1"] = ServedPermission{Agent: &conversationv1.AgentId{Value: "agent-9"}}

	// Act.
	err := f.verbs.AnswerPermission(context.Background(), "w1", permissionAnswer("ask-1", allowedStanding()))

	// Assert.
	asRefusal(t, err, ArmNoStandingOffer)
}

func TestAnswerPermissionDeliversTheOfferedStandingGrant(t *testing.T) {
	// Arrange: the grant delivered is the one the ask OFFERED, not the one the
	// client echoed back.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	offered := &conversationv1.AgentPermissionStanding{
		Changes: []*conversationv1.AgentPermissionChange{{
			Destination: conversationv1.AgentPermissionDestination_AGENT_PERMISSION_DESTINATION_SESSION,
		}},
	}
	f.cards.permissions["ask-1"] = ServedPermission{
		Agent: &conversationv1.AgentId{Value: "agent-9"}, StandingFor: offered,
	}

	// Act.
	if err := f.verbs.AnswerPermission(context.Background(), "w1", permissionAnswer("ask-1", allowedStanding())); err != nil {
		t.Fatalf("AnswerPermission: %v", err)
	}

	// Assert.
	got := f.shim.answers[0].Answer.GetPermissionDecision().GetAllowed().GetStanding().GetStanding()
	if len(got.GetChanges()) != 1 {
		t.Fatalf("delivered standing = %v, want the offered grant", got)
	}
}

func TestAnswerPermissionRefusesAnAnswerWithNoDecision(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.AnswerPermission(context.Background(), "w1", &conversationv1.AgentAnswer{})

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
}

func TestAnswerPermissionRefusesWithNoLiveSession(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.permissions["ask-1"] = ServedPermission{Agent: &conversationv1.AgentId{Value: "agent-9"}}
	f.hasSession = false

	// Act.
	err := f.verbs.AnswerPermission(context.Background(), "w1", permissionAnswer("ask-1", allowedOnce()))

	// Assert.
	asRefusal(t, err, ArmNoSession)
}

// servedBatch is a batch with one single-select and one multi-select question.
func servedBatch() *conversationv1.AgentQuestionBatch {
	return &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{
		{
			Question: &conversationv1.AgentQuestionText{Text: "pick one"},
			Choices: &conversationv1.AgentQuestionAsked_SingleSelect{
				SingleSelect: &conversationv1.AgentQuestionSingleSelect{Options: []*conversationv1.AgentQuestionOption{
					{Label: &conversationv1.AgentQuestionOptionLabel{Label: "a"}},
					{Label: &conversationv1.AgentQuestionOptionLabel{Label: "b"}},
				}},
			},
		},
		{
			Question: &conversationv1.AgentQuestionText{Text: "pick many"},
			Choices: &conversationv1.AgentQuestionAsked_MultiSelect{
				MultiSelect: &conversationv1.AgentQuestionMultiSelect{Options: []*conversationv1.AgentQuestionOption{
					{Label: &conversationv1.AgentQuestionOptionLabel{Label: "x"}},
					{Label: &conversationv1.AgentQuestionOptionLabel{Label: "y"}},
				}},
			},
		},
	}}
}

// questionAnswer composes one question answer for ask id.
func questionAnswer(id string, selections ...*conversationv1.AgentQuestionSelection) *conversationv1.AgentAnswer {
	return &conversationv1.AgentAnswer{
		Answer: &conversationv1.AgentAnswer_QuestionAnswer{
			QuestionAnswer: &conversationv1.AgentQuestionAnswer{
				Ask:     &conversationv1.AgentQuestionId{Value: id},
				Answers: &conversationv1.AgentQuestionAnswers{Answers: selections},
			},
		},
	}
}

// selection composes one question's chosen labels.
func selection(question string, labels ...string) *conversationv1.AgentQuestionSelection {
	sel := &conversationv1.AgentQuestionSelection{
		Question: &conversationv1.AgentQuestionText{Text: question},
	}
	for _, label := range labels {
		sel.Chosen = append(sel.Chosen, &conversationv1.AgentQuestionChoice{
			Label: &conversationv1.AgentQuestionOptionLabel{Label: label},
		})
	}
	return sel
}

func TestAnswerQuestionDeliversAServedAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.questions["q-1"] = ServedQuestion{
		Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch(),
	}

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1",
		questionAnswer("q-1", selection("pick one", "a"), selection("pick many", "x", "y")))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQuestion: %v", err)
	}
	if len(f.shim.answers) != 1 {
		t.Fatalf("delivered answers = %+v, want exactly one", f.shim.answers)
	}
}

func TestAnswerQuestionRefusesAnUnservedQuestionText(t *testing.T) {
	// Arrange: an unserved value means the client is answering a stale card.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.questions["q-1"] = ServedQuestion{
		Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch(),
	}

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1",
		questionAnswer("q-1", selection("a question nobody asked", "a")))

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
}

func TestAnswerQuestionRefusesAnUnservedLabel(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.questions["q-1"] = ServedQuestion{
		Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch(),
	}

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1", questionAnswer("q-1", selection("pick one", "z")))

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
}

func TestAnswerQuestionRefusesAMultiPickOnASingleSelect(t *testing.T) {
	// Arrange: a multi-pick is refused rather than silently truncated.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.questions["q-1"] = ServedQuestion{
		Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch(),
	}

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1", questionAnswer("q-1", selection("pick one", "a", "b")))

	// Assert.
	asRefusal(t, err, ArmMultiPickOnSingleSelect)
}

func TestAnswerQuestionAcceptsAMultiPickOnAMultiSelect(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.questions["q-1"] = ServedQuestion{
		Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch(),
	}

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1", questionAnswer("q-1", selection("pick many", "x", "y")))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQuestion: %v", err)
	}
}

func TestAnswerQuestionRefusesABatchThatIsNotStanding(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1", questionAnswer("q-1", selection("pick one", "a")))

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
}

func TestAnswerQuestionSurfacesADeliveryFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.questions["q-1"] = ServedQuestion{
		Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch(),
	}
	f.shim.answerErr = errors.New("no open ask")

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1", questionAnswer("q-1", selection("pick one", "a")))

	// Assert.
	if err == nil {
		t.Fatal("AnswerQuestion() = nil error, want the delivery failure surfaced")
	}
}

// standingGate arranges a cold gate whose menu offers one model and the ALL
// scope.
func standingGate(f *fixture) {
	f.cards.coldGate = &ServedColdGate{
		VendorSessionID: "vendor-1",
		Models:          []*conversationv1.AgentModel{{Name: "opus"}},
		Scopes:          []conversationv1.SessionCompactScope{conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL},
	}
}

func TestAnswerColdGatePayReopensTheConversation(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Pay{Pay: &frontendv1.FeedColdGateResolvedPay{}},
	}

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}
	if len(f.shim.resumes) != 1 || f.shim.resumes[0].VendorSessionID != "vendor-1" {
		t.Fatalf("resumes = %+v, want one of vendor-1", f.shim.resumes)
	}
	if f.shim.resumes[0].Remediation.GetPay() == nil {
		t.Fatalf("remediation = %v, want the pay arm", f.shim.resumes[0].Remediation)
	}
}

func TestAnswerColdGateClearReopensWithTheClearRemediation(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Clear{Clear: &frontendv1.FeedColdGateResolvedClear{}},
	}

	// Act.
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED); err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}

	// Assert.
	if f.shim.resumes[0].Remediation.GetClear() == nil {
		t.Fatalf("remediation = %v, want the clear arm", f.shim.resumes[0].Remediation)
	}
}

func TestAnswerColdGateCompactCarriesTheModelAndScope(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Compact{
			Compact: &frontendv1.FeedColdGateResolvedCompact{
				Model: &frontendv1.FeedColdGateModel{Model: &conversationv1.AgentModel{Name: "opus"}},
			},
		},
	}

	// Act.
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL); err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}

	// Assert.
	compact := f.shim.resumes[0].Remediation.GetCompact()
	if compact.GetModel().GetName() != "opus" ||
		compact.GetScope() != conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL {
		t.Fatalf("compact remediation = %v, want opus over the ALL scope", compact)
	}
}

func TestAnswerColdGateRefusesAModelOutsideTheServedMenu(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Compact{
			Compact: &frontendv1.FeedColdGateResolvedCompact{
				Model: &frontendv1.FeedColdGateModel{Model: &conversationv1.AgentModel{Name: "haiku"}},
			},
		},
	}

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL)

	// Assert.
	asRefusal(t, err, ArmUnservedRemediation)
}

func TestAnswerColdGateRefusesAScopeOutsideTheServedMenu(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Compact{
			Compact: &frontendv1.FeedColdGateResolvedCompact{
				Model: &frontendv1.FeedColdGateModel{Model: &conversationv1.AgentModel{Name: "opus"}},
			},
		},
	}

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_PROMPTS)

	// Assert.
	asRefusal(t, err, ArmUnservedRemediation)
}

func TestAnswerColdGateRefusesWhenNoGateStands(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Pay{Pay: &frontendv1.FeedColdGateResolvedPay{}},
	}

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	asRefusal(t, err, ArmNoColdGate)
}

func TestAnswerColdGateRetiresTheStandingGate(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Pay{Pay: &frontendv1.FeedColdGateResolvedPay{}},
	}

	// Act.
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED); err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}

	// Assert.
	if f.footer.coldGates["w1"].Standing {
		t.Fatal("the footer still reports a standing cold gate")
	}
	if len(f.feed.synthesized) != 1 || f.feed.synthesized[0].GetColdGate().GetResolved() == nil {
		t.Fatalf("synthesized rows = %v, want one resolved gate row", f.feed.synthesized)
	}
}

func TestAnswerColdGateRefusesAnAnswerNamingNoChoice(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", &frontendv1.FeedColdGateResolved{},
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	asRefusal(t, err, ArmUnservedRemediation)
}
