package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
)

// THE TWO INPUTS TO A STANDING ASK. A question card is answered by clicks on
// the card (UpdateQuestionDraft) and by the user's typed send
// (AnswerStandingAsk); a consent card by its buttons (AnswerPermission) and by
// the typed send, which denies it with the text as the reason. Both question
// inputs edit ONE daemon-owned draft (feed/questiondraft.go), and the edit that
// answers the batch's last question delivers it here, through AnswerQuestion's
// own echo checks.

const (
	opQuestionDraft = "daemon.workspace.update_question_draft"
	opStandingAsk   = "daemon.workspace.answer_standing_ask"
)

// UpdateQuestionDraft applies one click to a standing question card's draft.
func (v *verbs) UpdateQuestionDraft(ctx context.Context, ws ids.WorkspaceID, ask string, edit feed.QuestionEdit) error {
	_, log, err := v.owned(ctx, "UpdateQuestionDraft", ws)
	if err != nil {
		return err
	}
	return v.editQuestionDraft(ctx, "UpdateQuestionDraft", ws, ask, edit, log)
}

// AnswerStandingAsk takes a typed send as the answer to the oldest ask
// standing on the addressed feed.
func (v *verbs) AnswerStandingAsk(ctx context.Context, ws ids.WorkspaceID, target *feedid.Ref, text string) (bool, error) {
	_, log, err := v.owned(ctx, "SubmitPrompt", ws)
	if err != nil {
		return false, err
	}
	ask, standing := v.deps.Cards.StandingAsk(ws, target)
	if !standing {
		log.Debug(opStandingAsk, "no ask stands on the addressed feed; the send is an ordinary prompt",
			dlog.Context{"bubble_addressed": target != nil})
		return false, nil
	}
	log = log.With(dlog.Context{"ask": ask.ID})
	switch ask.Kind {
	case feed.AskQuestion:
		log.Info(opStandingAsk, "the typed send answers the standing question's active question", nil)
		return true, v.editQuestionDraft(ctx, "SubmitPrompt", ws, ask.ID, feed.QuestionEdit{Kind: feed.EditType, Text: text}, log)
	case feed.AskPermission:
		log.Info(opStandingAsk, "the typed send denies the standing permission ask with its text as the reason", nil)
		return true, v.AnswerPermission(ctx, ws, &conversationv1.AgentAnswer{
			Answer: &conversationv1.AgentAnswer_PermissionDecision{PermissionDecision: &conversationv1.AgentPermissionDecision{
				Ask:      &conversationv1.AgentPermissionId{Value: ask.ID},
				Decision: DeniedByUser(text),
			}},
		})
	default:
		panic(fmt.Sprintf("workspace: the feed resolver answered an unknown ask kind %d", ask.Kind))
	}
}

// editQuestionDraft applies one edit and, when it answered the batch's last
// question, delivers the claimed batch — releasing the claim when the delivery
// fails, so the batch can be finished again.
func (v *verbs) editQuestionDraft(ctx context.Context, rpc string, ws ids.WorkspaceID, ask string, edit feed.QuestionEdit, log dlog.Logger) error {
	outcome, err := v.deps.Cards.EditQuestionDraft(ws, ask, edit)
	if err != nil {
		var unserved *feed.UnservedQuestionValueError
		switch {
		case errors.Is(err, feed.ErrQuestionNotStanding):
			return refuse(log, rpc, ArmAskNotStanding,
				fmt.Sprintf("no question batch %q is standing for a %s edit", ask, edit.Kind), true)
		case errors.As(err, &unserved):
			return refuseWith(log, rpc, ArmUnservedAnswer, unserved.Error(), false,
				map[string]any{"text": unserved.Text})
		}
		log.Error(opQuestionDraft, "the draft edit failed", dlog.Context{"edit": edit.Kind.String(), "cause": err.Error()})
		return fmt.Errorf("edit question draft %q: %w", ask, err)
	}
	if outcome.Deliver == nil {
		return nil
	}
	log.Info(opQuestionDraft, "the edit answered the batch's last question; delivering the batch",
		dlog.Context{"edit": edit.Kind.String(), "answers": len(outcome.Deliver.GetAnswers())})
	err = v.AnswerQuestion(ctx, ws, &conversationv1.AgentAnswer{
		Answer: &conversationv1.AgentAnswer_QuestionAnswer{QuestionAnswer: &conversationv1.AgentQuestionAnswer{
			Ask:     &conversationv1.AgentQuestionId{Value: ask},
			Answers: outcome.Deliver,
		}},
	})
	if err != nil {
		// AnswerQuestion recorded the failure as its owner; what is left is
		// freeing the claim, or the batch could never be delivered again.
		v.deps.Cards.ReleaseQuestionDelivery(ws, ask)
		return err
	}
	return nil
}

// DeniedByUser is a permission decision's deny arm carrying the user's reason
// (empty for none): the one builder of the shape, for the card's deny button
// and a typed deny alike.
func DeniedByUser(message string) *conversationv1.AgentPermissionDecision_Denied {
	return &conversationv1.AgentPermissionDecision_Denied{
		Denied: &conversationv1.AgentPermissionDeniedByUser{Message: message},
	}
}
