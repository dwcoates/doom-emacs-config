package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
)

// AnswerPermission delivers a permission card's verdict to the agent that
// asked.
//
// The answer is ECHOED against what the daemon SERVED: the ask must still be
// standing, and allow_standing is legal ONLY when the ask actually OFFERED a
// standing grant — granting a standing permission the vendor never offered
// would write a rule the user was never shown.
func (v *verbs) AnswerPermission(ctx context.Context, ws ids.WorkspaceID, answer *conversationv1.AgentAnswer) error {
	_, log, err := v.owned(ctx, "AnswerPermission", ws)
	if err != nil {
		return err
	}

	decision := answer.GetPermissionDecision()
	if decision == nil {
		return refuse(log, "AnswerPermission", ArmUnservedAnswer,
			"the answer carries no permission decision", false)
	}
	ask := decision.GetAsk()
	if ask.GetValue() == "" {
		return refuse(log, "AnswerPermission", ArmUnservedAnswer,
			"the permission decision names no ask", true)
	}

	served, ok := v.deps.Cards.Permission(ws, ask)
	if !ok {
		return refuse(log, "AnswerPermission", ArmUnservedAnswer,
			fmt.Sprintf("no permission ask %q is standing", ask.GetValue()), true)
	}

	if standing := decision.GetAllowed().GetStanding(); standing != nil {
		if served.StandingFor == nil {
			return refuse(log, "AnswerPermission", ArmNoStandingOffer,
				fmt.Sprintf("permission ask %q offered no standing grant", ask.GetValue()), false)
		}
		// The grant DELIVERED is the one the ask offered, never the one the
		// client echoed back: a client that rewrote the changes would be
		// writing rules of its own.
		standing.Standing = served.StandingFor
	}

	shim, live := v.deps.Shim(ws)
	if !live {
		return refuse(log, "AnswerPermission", ArmNoSession,
			fmt.Sprintf("workspace %q has no live session to answer through", ws), false)
	}
	if err := shim.Answer(ctx, served.Agent, answer); err != nil {
		if refusal, ok := AsShimRefusal(err); ok {
			// The shim's own arm reaches the caller: `not_deliverable` (the SDK
			// has no route to that agent) is a different answer from
			// `no_open_ask` (the card is stale), and the user acts differently
			// on each. Neither is hidden.
			return refuse(log, "AnswerPermission", refusal.Arm, refusal.Detail, false)
		}
		log.Error(opAnswerPerm, "the permission answer was not accepted", dlog.Context{
			"ask": ask.GetValue(), "cause": err.Error(),
		})
		return fmt.Errorf("answer permission %q: %w", ask.GetValue(), err)
	}
	log.Info(opAnswerPerm, "delivered the permission verdict", dlog.Context{
		"ask": ask.GetValue(), "agent": served.Agent.GetValue(),
	})
	return nil
}

// AnswerQuestion delivers a question card's answer, echoing the SERVED batch.
//
// Two echo rules are enforced here and nowhere else:
//
//   - every answered question's text, and every chosen label, must be one the
//     batch actually served — an unserved value means the client is answering a
//     stale card;
//   - a SINGLE-SELECT question takes exactly one choice, so a multi-pick on one
//     is refused rather than silently truncated.
func (v *verbs) AnswerQuestion(ctx context.Context, ws ids.WorkspaceID, answer *conversationv1.AgentAnswer) error {
	_, log, err := v.owned(ctx, "AnswerQuestion", ws)
	if err != nil {
		return err
	}

	given := answer.GetQuestionAnswer()
	if given == nil {
		return refuse(log, "AnswerQuestion", ArmUnservedAnswer, "the answer carries no question answer", false)
	}
	ask := given.GetAsk()
	if ask.GetValue() == "" {
		return refuse(log, "AnswerQuestion", ArmAskNotStanding, "the question answer names no ask", true)
	}
	served, ok := v.deps.Cards.Question(ws, ask)
	if !ok {
		return refuse(log, "AnswerQuestion", ArmAskNotStanding,
			fmt.Sprintf("no question batch %q is standing", ask.GetValue()), true)
	}
	if err := validateQuestionEcho(log, served.Batch, given.GetAnswers()); err != nil {
		return err
	}

	shim, live := v.deps.Shim(ws)
	if !live {
		return refuse(log, "AnswerQuestion", ArmNoSession,
			fmt.Sprintf("workspace %q has no live session to answer through", ws), false)
	}
	if err := shim.Answer(ctx, served.Agent, answer); err != nil {
		if refusal, ok := AsShimRefusal(err); ok {
			return refuse(log, "AnswerQuestion", refusal.Arm, refusal.Detail, false)
		}
		log.Error(opAnswerQuestion, "the question answer was not accepted", dlog.Context{
			"ask": ask.GetValue(), "cause": err.Error(),
		})
		return fmt.Errorf("answer question %q: %w", ask.GetValue(), err)
	}
	log.Info(opAnswerQuestion, "delivered the question answer", dlog.Context{
		"ask": ask.GetValue(), "agent": served.Agent.GetValue(), "answers": len(given.GetAnswers().GetAnswers()),
	})
	return nil
}

// validateQuestionEcho checks each selection against the served batch.
func validateQuestionEcho(log dlog.Logger, batch *conversationv1.AgentQuestionBatch, answers *conversationv1.AgentQuestionAnswers) error {
	asked := map[string]*conversationv1.AgentQuestionAsked{}
	for _, q := range batch.GetQuestions() {
		asked[q.GetQuestion().GetText()] = q
	}
	for _, selection := range answers.GetAnswers() {
		text := selection.GetQuestion().GetText()
		question, ok := asked[text]
		if !ok {
			return refuse(log, "AnswerQuestion", ArmUnservedAnswer,
				fmt.Sprintf("question %q was never served in this batch", text), false)
		}
		single := question.GetSingleSelect() != nil
		if single && len(selection.GetChosen()) > 1 {
			return refuse(log, "AnswerQuestion", ArmMultiPickOnSingleSelect,
				fmt.Sprintf("question %q is single-select but %d choices were made", text, len(selection.GetChosen())), false)
		}
		labels := map[string]bool{}
		for _, option := range question.GetSingleSelect().GetOptions() {
			labels[option.GetLabel().GetLabel()] = true
		}
		for _, option := range question.GetMultiSelect().GetOptions() {
			labels[option.GetLabel().GetLabel()] = true
		}
		for _, chosen := range selection.GetChosen() {
			if !labels[chosen.GetLabel().GetLabel()] {
				return refuse(log, "AnswerQuestion", ArmUnservedAnswer,
					fmt.Sprintf("question %q never offered the choice %q", text, chosen.GetLabel().GetLabel()), false)
			}
		}
	}
	return nil
}

// AnswerColdGate resolves a standing cold gate by RE-OPENING the session with
// the chosen remediation. A cold context is refused with its cost named, never
// silently paid, and this is where the user's choice is spent.
//
// The remediation is echoed against the SERVED MENU: a compaction naming a
// model or a scope the menu did not offer is refused rather than sent.
func (v *verbs) AnswerColdGate(ctx context.Context, ws ids.WorkspaceID, answer *frontendv1.FeedColdGateResolved, scope conversationv1.SessionCompactScope) error {
	_, log, err := v.owned(ctx, "AnswerColdGate", ws)
	if err != nil {
		return err
	}
	if answer == nil {
		return refuse(log, "AnswerColdGate", ArmUnservedRemediation, "the answer names no choice", false)
	}
	served, ok := v.deps.Cards.ColdGate(ws)
	if !ok {
		return refuse(log, "AnswerColdGate", ArmNoColdGate,
			fmt.Sprintf("no cold gate is standing on workspace %q", ws), false)
	}

	remediation, err := coldRemediation(log, served, answer, scope)
	if err != nil {
		return err
	}

	shim, live := v.deps.Shim(ws)
	if !live {
		return refuse(log, "AnswerColdGate", ArmNoSession,
			fmt.Sprintf("workspace %q has no live session to re-open", ws), false)
	}
	if err := shim.StartSession(ctx, ColdResume{
		VendorSessionID: served.VendorSessionID,
		Remediation:     remediation,
	}); err != nil {
		log.Error(opColdGate, "the re-open with the remediation failed", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("answer cold gate on %q: %w", ws, err)
	}

	// THE GATE IS SPENT. A second answer against the same id must find nothing
	// standing rather than re-opening the session again.
	v.deps.Cards.ClearColdGate(ws)

	// The gate row becomes its resolved state and the footer stops saying the
	// session is parked, both in the same beat as the re-open.
	v.deps.Feed.UpsertSynthesized(ws, rootFeed(), coldGateRow(ws, served.VendorSessionID, answer))
	v.deps.Footer.SetColdGate(ws, footer.ColdGate{Standing: false})

	log.Info(opColdGate, "answered the cold gate", dlog.Context{"choice": coldChoiceName(answer)})
	return nil
}

// coldRemediation translates the answered gate into the shim's remediation,
// validating a compaction against the served menu.
func coldRemediation(log dlog.Logger, served ServedColdGate, answer *frontendv1.FeedColdGateResolved, scope conversationv1.SessionCompactScope) (*conversationv1.SessionColdRemediation, error) {
	switch {
	case answer.GetPay() != nil:
		return &conversationv1.SessionColdRemediation{
			Remediation: &conversationv1.SessionColdRemediation_Pay{Pay: &conversationv1.SessionColdPay{}},
		}, nil
	case answer.GetClear() != nil:
		return &conversationv1.SessionColdRemediation{
			Remediation: &conversationv1.SessionColdRemediation_Clear{Clear: &conversationv1.SessionColdClear{}},
		}, nil
	case answer.GetCompact() != nil:
		model := answer.GetCompact().GetModel().GetModel()
		if !servedModel(served.Models, model) {
			return nil, refuse(log, "AnswerColdGate", ArmUnservedRemediation,
				fmt.Sprintf("the compact menu never offered the model %q", model.GetName()), false)
		}
		if !servedScope(served.Scopes, scope) {
			return nil, refuse(log, "AnswerColdGate", ArmUnservedRemediation,
				fmt.Sprintf("the compact menu never offered the scope %s", scope), false)
		}
		return &conversationv1.SessionColdRemediation{
			Remediation: &conversationv1.SessionColdRemediation_Compact{
				Compact: &conversationv1.SessionColdCompact{Model: model, Scope: scope},
			},
		}, nil
	default:
		return nil, refuse(log, "AnswerColdGate", ArmUnservedRemediation, "the answer names no choice", false)
	}
}

func servedModel(models []*conversationv1.AgentModel, want *conversationv1.AgentModel) bool {
	for _, m := range models {
		if m.GetName() == want.GetName() {
			return true
		}
	}
	return false
}

func servedScope(scopes []conversationv1.SessionCompactScope, want conversationv1.SessionCompactScope) bool {
	for _, s := range scopes {
		if s == want {
			return true
		}
	}
	return false
}

func coldChoiceName(answer *frontendv1.FeedColdGateResolved) string {
	switch {
	case answer.GetPay() != nil:
		return "pay"
	case answer.GetClear() != nil:
		return "clear"
	case answer.GetCompact() != nil:
		return "compact"
	default:
		return "none"
	}
}
