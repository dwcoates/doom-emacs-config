package convert

// question.go — SETTLING THE ASK.
//
// NOBODY ANSWERING IS AN ANSWER to a legitimate ask rather than a failure of it,
// so an expired ask settles on the success arm's `unanswered` outcome and never
// on the failure arm. The failure arm is scoped to the ASK alone: the question
// could not be put to the user at all.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// settleQuestion converts an AskUserQuestion result, upserting the ask's row
// under the SAME question key its start frame used.
func (c *Converter) settleQuestion(call openCall, result, block map[string]any, failed bool, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	batch := &conversationv1.AgentQuestionBatch{}
	for _, raw := range list(pick(result, "questions")) {
		if q := obj(raw); q != nil {
			batch.Questions = append(batch.Questions, questionAsked(q))
		}
	}
	if len(batch.Questions) == 0 {
		// The result restated no questions; the CALL is the only statement of
		// what was asked, and a settled frame must still describe itself.
		for _, raw := range list(call.input["questions"]) {
			if q := obj(raw); q != nil {
				batch.Questions = append(batch.Questions, questionAsked(q))
			}
		}
	}

	question := &conversationv1.AgentQuestion{Id: &conversationv1.AgentQuestionId{Value: call.activityID}}

	if failed {
		c.log.With(at.ctxWarn("question")).With(logging.Context{ActivityID: call.activityID, UpsertKey: QuestionKey(call.activityID)}).
			Log("the ask could not be put to the user")
		question.Result = &conversationv1.AgentQuestion_Failure{Failure: &conversationv1.AgentQuestionFailure{
			Error: toolFailure(block, env.timestampMs),
		}}
		return c.landFrame(at, agent, QuestionKey(call.activityID), "question_settle", updateFrame(agent, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Question{Question: question},
		}))
	}

	success := &conversationv1.AgentQuestionSuccess{Batch: batch}
	answers := questionAnswers(result)
	if len(answers.GetAnswers()) == 0 {
		c.log.With(at.ctxFor("question")).With(logging.Context{ActivityID: call.activityID, UpsertKey: QuestionKey(call.activityID)}).
			Log("the ask went unanswered; the agent proceeded without a choice")
		success.Outcome = &conversationv1.AgentQuestionSuccess_Unanswered{Unanswered: &conversationv1.AgentQuestionUnanswered{}}
	} else {
		c.log.With(at.ctxFor("question")).With(logging.Context{ActivityID: call.activityID, UpsertKey: QuestionKey(call.activityID)}).
			LogVerbose("the ask was answered with %d selection(s)", len(answers.GetAnswers()))
		success.Outcome = &conversationv1.AgentQuestionSuccess_Answered{Answered: answers}
	}
	question.Result = &conversationv1.AgentQuestion_Success{Success: success}

	return c.landFrame(at, agent, QuestionKey(call.activityID), "question_settle", updateFrame(agent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: question},
	}))
}

// questionAnswers reads the user's choices.
//
// THE PRODUCER'S OWN KEY IS THE QUESTION TEXT: its answers arrive keyed by it, so
// the text is the identity the producer recognises and every selection echoes it
// back verbatim rather than naming an index.
//
// A selection with typed text arrives as ONE comma-joined answer string,
// structurally indistinguishable from a multi-select label join — so this reader
// carries the string as the chosen label and never splits it into a separate
// free-text note it cannot actually observe.
func questionAnswers(result map[string]any) *conversationv1.AgentQuestionAnswers {
	answers := &conversationv1.AgentQuestionAnswers{}
	switch raw := pick(result, "answers").(type) {
	case map[string]any:
		for question, answer := range raw {
			answers.Answers = append(answers.Answers, &conversationv1.AgentQuestionSelection{
				Question: &conversationv1.AgentQuestionText{Text: question},
				Chosen: []*conversationv1.AgentQuestionChoice{{
					Label: &conversationv1.AgentQuestionOptionLabel{Label: str(answer)},
				}},
			})
		}
	case []any:
		for _, el := range raw {
			answer := obj(el)
			if answer == nil {
				continue
			}
			selection := &conversationv1.AgentQuestionSelection{
				Question: &conversationv1.AgentQuestionText{Text: str(pick(answer, "question", "header"))},
			}
			for _, choice := range list(pick(answer, "chosen", "options", "labels")) {
				selection.Chosen = append(selection.Chosen, &conversationv1.AgentQuestionChoice{
					Label: &conversationv1.AgentQuestionOptionLabel{Label: str(choice)},
				})
			}
			if label := str(pick(answer, "answer", "label")); label != "" && len(selection.Chosen) == 0 {
				selection.Chosen = append(selection.Chosen, &conversationv1.AgentQuestionChoice{
					Label: &conversationv1.AgentQuestionOptionLabel{Label: label},
				})
			}
			if free := optionalString(pick(answer, "free_text", "freeText", "text")); free != nil {
				selection.FreeText = &conversationv1.AgentQuestionFreeText{Text: *free}
			}
			answers.Answers = append(answers.Answers, selection)
		}
	}
	return answers
}
