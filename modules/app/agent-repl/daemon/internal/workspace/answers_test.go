package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/health"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/topbar"
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

func TestAnswerQuestionRefusesAnAnswerNamingNoAsk(t *testing.T) {
	// Arrange: a card that is not standing is ask_not_standing, never the
	// unserved-value arm.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1",
		questionAnswer("", selection("pick one", "a")))

	// Assert.
	asRefusal(t, err, ArmAskNotStanding)
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
	asRefusal(t, err, ArmAskNotStanding)
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
	if len(f.fleet.resumes) != 1 || f.fleet.resumes[0].VendorSessionID != "vendor-1" {
		t.Fatalf("resumes = %+v, want one of vendor-1", f.fleet.resumes)
	}
	if f.fleet.resumes[0].Remediation.GetPay() == nil {
		t.Fatalf("remediation = %v, want the pay arm", f.fleet.resumes[0].Remediation)
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
	if f.fleet.resumes[0].Remediation.GetClear() == nil {
		t.Fatalf("remediation = %v, want the clear arm", f.fleet.resumes[0].Remediation)
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
	compact := f.fleet.resumes[0].Remediation.GetCompact()
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
	if len(f.feed.synthesized) != 0 {
		t.Fatalf("synthesized rows = %v, want none: an answered gate leaves no trace row", f.feed.synthesized)
	}
}

func TestAnswerColdGateRetiresTheStripsGateToo(t *testing.T) {
	// Arrange: the strip has its own cold-gate state, and the answer's
	// re-opened session is what fills the full view back in.
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
	want := []topbar.ColdGate{{Standing: false}}
	if len(f.topbarColdGates) != len(want) || f.topbarColdGates[0] != want[0] {
		t.Fatalf("topbar cold gates = %v, want %v", f.topbarColdGates, want)
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

func TestAnswerPermissionPropagatesNotDeliverable(t *testing.T) {
	// Arrange: the SDK has no route to that agent. The caller is told so,
	// rather than seeing a generic failure or nothing at all.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.permissions["ask-1"] = ServedPermission{Agent: &conversationv1.AgentId{Value: "agent-9"}}
	f.shim.answerErr = &ShimRefusal{
		Verb: "UpdateAgent", Arm: ArmShimNotDeliverable, Detail: "no SDK route to a subagent",
	}

	// Act.
	err := f.verbs.AnswerPermission(context.Background(), "w1", permissionAnswer("ask-1", allowedOnce()))

	// Assert.
	refusal := asRefusal(t, err, ArmShimNotDeliverable)
	if refusal.Rpc != "AnswerPermission" {
		t.Fatalf("refusal rpc = %q, want AnswerPermission", refusal.Rpc)
	}
}

func TestAnswerPermissionPropagatesAStaleCard(t *testing.T) {
	// Arrange: a closed ask keeps its own arm, distinct from an absent route.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.permissions["ask-1"] = ServedPermission{Agent: &conversationv1.AgentId{Value: "agent-9"}}
	f.shim.answerErr = &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimNoOpenAsk}

	// Act.
	err := f.verbs.AnswerPermission(context.Background(), "w1", permissionAnswer("ask-1", allowedOnce()))

	// Assert.
	asRefusal(t, err, ArmShimNoOpenAsk)
}

func TestAnswerQuestionPropagatesTheShimArm(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.questions["q-1"] = ServedQuestion{
		Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch(),
	}
	f.shim.answerErr = &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimAnswerMismatch}

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1", questionAnswer("q-1", selection("pick one", "a")))

	// Assert.
	refusal := asRefusal(t, err, ArmShimAnswerMismatch)
	if refusal.Rpc != "AnswerQuestion" {
		t.Fatalf("refusal rpc = %q, want AnswerQuestion", refusal.Rpc)
	}
}

func TestAnswerQuestionStillSurfacesATransportFailure(t *testing.T) {
	// Arrange: a broken link is a FAILURE, not a named refusal.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.questions["q-1"] = ServedQuestion{
		Agent: &conversationv1.AgentId{Value: "agent-2"}, Batch: servedBatch(),
	}
	f.shim.answerErr = errors.New("connection reset")

	// Act.
	err := f.verbs.AnswerQuestion(context.Background(), "w1", questionAnswer("q-1", selection("pick one", "a")))

	// Assert.
	if err == nil {
		t.Fatal("AnswerQuestion() = nil error, want the transport failure surfaced")
	}
	if _, ok := AsRefusal(err); ok {
		t.Fatalf("AnswerQuestion() = %v, want a failure rather than a refusal", err)
	}
}

// TestAnswerColdGateRetiresTheGateItAnswered covers the spend: a gate left
// standing would re-open the session again on every replayed click, and a
// second answer must find nothing standing instead.
func TestAnswerColdGateRetiresTheGateItAnswered(t *testing.T) {
	// Arrange: a standing gate on a live session.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.coldGate = &ServedColdGate{VendorSessionID: "vendor-1"}

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1",
		&frontendv1.FeedColdGateResolved{Choice: &frontendv1.FeedColdGateResolved_Pay{Pay: &frontendv1.FeedColdGateResolvedPay{}}},
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}
	if f.cards.coldGatesCleared != 1 {
		t.Fatalf("cold gates retired = %d, want exactly the answered one", f.cards.coldGatesCleared)
	}
}

// TestASecondAnswerOfTheSameColdGateIsRefused is the consequence the retirement
// exists for: the gate is spent, so answering it again finds none standing.
func TestASecondAnswerOfTheSameColdGateIsRefused(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.coldGate = &ServedColdGate{VendorSessionID: "vendor-1"}
	pay := &frontendv1.FeedColdGateResolved{Choice: &frontendv1.FeedColdGateResolved_Pay{Pay: &frontendv1.FeedColdGateResolvedPay{}}}
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", pay,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED); err != nil {
		t.Fatalf("the first AnswerColdGate: %v", err)
	}

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", pay,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	asRefusal(t, err, ArmNoColdGate)
}

// A FAILED RE-OPEN IS STILL AN ACCEPTED ANSWER (owner ruling, 2026-09-29):
// the gate disappears the moment the daemon has the choice, so the answer is
// acknowledged before the re-open runs, and its failure reaches the user
// through the footer fault and a gate stood again, never through the reply.
func TestAnswerColdGateAcceptsAnAnswerWhoseReopenFails(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumeErr = errors.New("the producer has already written rows")
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Clear{Clear: &frontendv1.FeedColdGateResolvedClear{}},
	}

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerColdGate = %v, want the answer accepted", err)
	}
}

func TestAnswerColdGateOpensTheFaultThatPutsTheFailureOnTheFooter(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumeErr = errors.New("the producer has already written rows")
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Clear{Clear: &frontendv1.FeedColdGateResolvedClear{}},
	}

	// Act.
	_ = f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	if len(f.health.opened) != 1 {
		t.Fatalf("opened %d faults, want 1", len(f.health.opened))
	}
	fault := f.health.opened[0]
	if fault.Kind != health.KindColdGateReopenFailed {
		t.Fatalf("fault kind = %q, want %q", fault.Kind, health.KindColdGateReopenFailed)
	}
	if fault.Workspace == nil || *fault.Workspace != "w1" {
		t.Fatalf("fault workspace = %v, want w1", fault.Workspace)
	}
	if got := fault.Evidence["cause"]; got != "the producer has already written rows" {
		t.Fatalf("fault cause = %q, want the failure's own account", got)
	}
}

func TestAnswerColdGateStandsTheGateAgainWhenTheReopenFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumeErr = errors.New("the producer has already written rows")
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Clear{Clear: &frontendv1.FeedColdGateResolvedClear{}},
	}

	// Act.
	_ = f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	if len(f.cards.reraised) != 1 || f.cards.reraised[0] != "vendor-1" {
		t.Fatalf("gates stood again = %v, want the answered one, vendor-1", f.cards.reraised)
	}
}

func TestAnswerColdGateRetiresTheGateRowAtOnce(t *testing.T) {
	// Arrange: the re-open is held, so the answer's own effects are read
	// before it runs.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumeHold = make(chan struct{})
	f.fleet.resumeSettled = make(chan struct{})
	pay := &frontendv1.FeedColdGateResolved{Choice: &frontendv1.FeedColdGateResolved_Pay{Pay: &frontendv1.FeedColdGateResolvedPay{}}}

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", pay,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert: answered, row retired, and the re-open not yet run.
	retired, resumes := len(f.feed.retired), len(f.fleet.resumes)
	close(f.fleet.resumeHold)
	<-f.fleet.resumeSettled
	if err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}
	want := coldGateRowID("w1", "vendor-1").GetValue()
	if retired != 1 || f.feed.retired[0].GetValue() != want {
		t.Fatalf("retired rows = %v, want the gate row %q retired as the answer returned", f.feed.retired, want)
	}
	if resumes != 0 {
		t.Fatalf("re-opens run before the answer returned = %d, want 0: the answer does not wait on it", resumes)
	}
}

func TestAnswerColdGateStatesAGateThatCouldNotStandAgain(t *testing.T) {
	// Arrange: the re-open fails and the gate's cold facts are gone.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumeErr = errors.New("the producer has already written rows")
	f.cards.reraiseRefused = true
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Clear{Clear: &frontendv1.FeedColdGateResolvedClear{}},
	}

	// Act.
	_ = f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	for _, record := range f.log.logger.Records() {
		if record.Level == "error" && record.Operation == opColdGate &&
			record.Message == "the failed re-open's gate could not stand again: its cold facts are gone" {
			if record.Context["choice"] != "clear" {
				t.Fatalf("record context = %v, want the answered choice", record.Context)
			}
			return
		}
	}
	t.Fatalf("no error record says the gate could not stand again; records = %+v", f.log.logger.Records())
}

func TestAnswerColdGateFilesNoFaultWhenTheDaemonStandsDown(t *testing.T) {
	// Arrange: the re-open is abandoned by this daemon's own exit.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumeErr = context.Canceled
	answer := &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Clear{Clear: &frontendv1.FeedColdGateResolvedClear{}},
	}

	// Act.
	_ = f.verbs.AnswerColdGate(context.Background(), "w1", answer,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

	// Assert.
	if len(f.health.opened) != 0 || len(f.cards.reraised) != 0 {
		t.Fatalf("faults = %v, gates stood again = %v, want neither for a daemon standing down",
			f.health.opened, f.cards.reraised)
	}
}

// ---- the cold gate's answer is a footer act, click to outcome -------------
//
// Owner's report, 2026-09-14: the gate's `compact and resume` was clicked, the
// shim compacted, the session came back a minute later, and the strip said
// nothing for the whole of it. The ruling is that the answer is a footer act
// from the click to the outcome, so these fix the ORDER as well as the
// content: the request reaches the strip BEFORE the shim is dialed, every
// relayed phase is one more line, and the completion line is stated.

// compactAnswer is the gate's `compact and resume`, answered against the menu
// standingGate serves.
func compactAnswer() *frontendv1.FeedColdGateResolved {
	return &frontendv1.FeedColdGateResolved{
		Choice: &frontendv1.FeedColdGateResolved_Compact{
			Compact: &frontendv1.FeedColdGateResolvedCompact{
				Model: &frontendv1.FeedColdGateModel{Model: &conversationv1.AgentModel{Name: "opus"}},
			},
		},
	}
}

// phase is one relayed compaction phase.
func phase(
	p conversationv1.SessionCompactionPhase,
	before, after uint64,
) *conversationv1.SessionCompactionProgress {
	return &conversationv1.SessionCompactionProgress{Phase: p, TokensBefore: before, TokensAfter: after}
}

func TestAnswerColdGatePublishesTheRequestBeforeTheShimIsDialed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	var atDial []string
	f.fleet.observeResume = func() { atDial = f.footer.coldAnswerLines() }

	// Act.
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", compactAnswer(),
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL); err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}

	// Assert.
	if len(atDial) != 1 || !strings.Contains(atDial[0], "compaction requested") {
		t.Fatalf("footer at the dial = %q, want the compaction request already standing", atDial)
	}
}

func TestAnswerColdGatePublishesOneLinePerRelayedPhase(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumePhases = []*conversationv1.SessionCompactionProgress{
		phase(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 101_600, 0),
		phase(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZED, 101_600, 12_400),
		phase(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_RESUMING, 101_600, 12_400),
		phase(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400),
	}

	// Act.
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", compactAnswer(),
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL); err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}

	// Assert. The request, then one line per phase.
	if got := len(f.footer.coldAnswerLines()); got != 5 {
		t.Fatalf("footer lines = %d (%q), want the request plus one per phase",
			got, f.footer.coldAnswerLines())
	}
}

func TestAnswerColdGatePublishesTheCompletionLine(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumePhases = []*conversationv1.SessionCompactionProgress{
		phase(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400),
	}

	// Act.
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", compactAnswer(),
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL); err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}

	// Assert.
	lines := f.footer.coldAnswerLines()
	last := lines[len(lines)-1]
	if last != "compacted and resumed (101.6k → 12.4k)" {
		t.Fatalf("completion line = %q, want the compacted-and-resumed figures", last)
	}
}

func TestAnswerColdGateClearsTheActWhenTheAnswerLands(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)

	// Act.
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", compactAnswer(),
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL); err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}

	// Assert. The last thing the strip is told is that the act is over.
	if last := f.footer.coldAnswers[len(f.footer.coldAnswers)-1]; last != nil {
		t.Fatalf("last cold-gate answer = %+v, want the act cleared", last)
	}
}

func TestAnswerColdGateClearsTheActWhenTheReopenFails(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)
	f.fleet.resumeErr = errors.New("the shim died")

	// Act.
	err := f.verbs.AnswerColdGate(context.Background(), "w1", compactAnswer(),
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerColdGate = %v, want the answer accepted before its re-open ran", err)
	}
	if last := f.footer.coldAnswers[len(f.footer.coldAnswers)-1]; last != nil {
		t.Fatalf("last cold-gate answer = %+v, want the act cleared on the failure too", last)
	}
}

func TestAnswerColdGateNamesTheRemediationInTheAct(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	standingGate(f)

	// Act.
	if err := f.verbs.AnswerColdGate(context.Background(), "w1", compactAnswer(),
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL); err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}

	// Assert.
	if got := f.footer.coldAnswers[0].Choice; got != footer.ChoiceCompact {
		t.Fatalf("act choice = %q, want %q", got, footer.ChoiceCompact)
	}
}

func TestAnswerColdGateEndsTheGateOnlyWhenTheReopenSucceeds(t *testing.T) {
	tests := []struct {
		name      string
		resumeErr error
		wantEnded []string
	}{
		{"a re-open that brings the session back ends the gate", nil, []string{"vendor-1"}},
		{"a re-open that fails leaves it to be stood again", errors.New("the shim died"), nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			standingGate(f)
			f.fleet.resumeErr = tt.resumeErr
			pay := &frontendv1.FeedColdGateResolved{Choice: &frontendv1.FeedColdGateResolved_Pay{Pay: &frontendv1.FeedColdGateResolvedPay{}}}

			// Act.
			_ = f.verbs.AnswerColdGate(context.Background(), "w1", pay,
				conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED)

			// Assert.
			if len(f.cards.ended) != len(tt.wantEnded) || (len(tt.wantEnded) == 1 && f.cards.ended[0] != tt.wantEnded[0]) {
				t.Fatalf("gates ended = %v, want %v", f.cards.ended, tt.wantEnded)
			}
		})
	}
}
