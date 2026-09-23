package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// THE QUESTION CARD: the turn blocked on a choice the agent posed — a batch of
// one to four questions, radios or checkboxes PER QUESTION, because one batch
// can mix them. The answers ride the settled row because a cold repaint has
// nothing else to draw the choices from.

// drawQuestion draws the choice card.
func (r *resolver) drawQuestion(s *wsState, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion) {
	log := r.logger(s.id)
	askID := q.GetId().GetValue()
	if askID == "" {
		log.Error("daemon.feed.question_without_identity",
			"a question frame carried no ask identity",
			dlog.Context{"agent": agent.GetValue()})
		return
	}
	at, ok := r.place(s, agent)
	if !ok {
		return
	}
	card := &frontendv1.FeedQuestion{}
	served, known := s.questionAsks[askID]
	if !known {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!known"})
		served = &questionState{}
		s.questionAsks[askID] = served
	}
	if agent.GetValue() != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "agent.GetValue() != \"\""})
		served.agent = agent
	}

	switch frame := q.GetResult().(type) {
	case *conversationv1.AgentQuestion_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "draw", "branch": "case *conversationv1.AgentQuestion_Start"})
		served.batch = frame.Start.GetBatch()
		card.Questions = questionItems(frame.Start.GetBatch())
		card.State = &frontendv1.FeedQuestion_Open{Open: &frontendv1.FeedQuestionOpen{}}
	case *conversationv1.AgentQuestion_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "draw", "branch": "case *conversationv1.AgentQuestion_Success"})
		card.Questions = questionItems(frame.Success.GetBatch())
		switch outcome := frame.Success.GetOutcome().(type) {
		case *conversationv1.AgentQuestionSuccess_Answered:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "draw", "branch": "case *conversationv1.AgentQuestionSuccess_Answered"})
			card.State = &frontendv1.FeedQuestion_Answered{Answered: &frontendv1.FeedQuestionAnswered{
				AtMs:    r.deps.Now().UnixMilli(),
				Answers: givenAnswers(frame.Success.GetBatch(), outcome.Answered),
			}}
		case *conversationv1.AgentQuestionSuccess_Unanswered:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "draw", "branch": "case *conversationv1.AgentQuestionSuccess_Unanswered"})
			// NOBODY ANSWERING IS AN ANSWER to a legitimate ask: the agent
			// proceeded without a choice, so the card is expired rather than
			// pending forever.
			card.State = &frontendv1.FeedQuestion_Expired{Expired: &frontendv1.FeedQuestionExpired{
				AtMs: r.deps.Now().UnixMilli(),
			}}
		default:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "draw", "branch": "default"})
			return
		}
	case *conversationv1.AgentQuestion_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "draw", "branch": "case *conversationv1.AgentQuestion_Failure"})
		// The ask could not be put to the user at all; nothing was chosen and
		// nothing waits.
		card.State = &frontendv1.FeedQuestion_Expired{Expired: &frontendv1.FeedQuestionExpired{
			AtMs: r.deps.Now().UnixMilli(),
		}}
	default:
		return
	}

	row := &frontendv1.FeedRow{
		Id:  r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindQuestion, ID: askID}),
		Row: &frontendv1.FeedRow_Question{Question: card},
	}
	r.stampTurn(s, row, nil)
	log.Debug("daemon.feed.question",
		"a question card was upserted",
		dlog.Context{"ask": askID, "questions": len(card.GetQuestions())})
	r.upsert(s, at, row, true)
}

// questionItems draws the batch. The ORDER IS PRESENTATIONAL ONLY — every
// answer names the question it answers — but it is preserved as served.
func questionItems(batch *conversationv1.AgentQuestionBatch) []*frontendv1.FeedQuestionItem {
	items := make([]*frontendv1.FeedQuestionItem, 0, len(batch.GetQuestions()))
	for _, asked := range batch.GetQuestions() {
		item := &frontendv1.FeedQuestionItem{
			Header: &frontendv1.FeedQuestionHeader{Text: asked.GetHeader()},
			// The question text is drawn AND handed back verbatim: it is the
			// identity the producer recognizes, not merely a label.
			Text: &frontendv1.FeedQuestionText{Text: asked.GetQuestion().GetText()},
		}
		switch choices := asked.GetChoices().(type) {
		case *conversationv1.AgentQuestionAsked_SingleSelect:
			item.Options = &frontendv1.FeedQuestionItem_SingleSelect{
				SingleSelect: &frontendv1.FeedQuestionSingleSelect{Options: options(choices.SingleSelect.GetOptions())},
			}
		case *conversationv1.AgentQuestionAsked_MultiSelect:
			item.Options = &frontendv1.FeedQuestionItem_MultiSelect{
				MultiSelect: &frontendv1.FeedQuestionMultiSelect{Options: options(choices.MultiSelect.GetOptions())},
			}
		}
		items = append(items, item)
	}
	return items
}

// options draws the offered choices. A label is the ECHO VALUE as well as the
// caption, so it is carried verbatim.
func options(offered []*conversationv1.AgentQuestionOption) []*frontendv1.FeedQuestionOption {
	out := make([]*frontendv1.FeedQuestionOption, 0, len(offered))
	for _, option := range offered {
		drawn := &frontendv1.FeedQuestionOption{
			Label: &frontendv1.FeedQuestionOptionLabel{Text: option.GetLabel().GetLabel()},
		}
		// UNSET when the agent gave none: absence draws no line, never an
		// empty one.
		if option.GetDescription() != "" {
			drawn.Description = &frontendv1.FeedQuestionOptionDescription{Text: option.GetDescription()}
		}
		out = append(out, drawn)
	}
	return out
}

// givenAnswers draws the verdict lines, one per question, IN THE BATCH'S
// ORDER. The producer's answers are unordered and each names its question, so
// they are joined by the question text rather than by position.
func givenAnswers(batch *conversationv1.AgentQuestionBatch, answers *conversationv1.AgentQuestionAnswers) []*frontendv1.FeedQuestionGivenAnswer {
	byQuestion := map[string]*conversationv1.AgentQuestionSelection{}
	for _, selection := range answers.GetAnswers() {
		byQuestion[selection.GetQuestion().GetText()] = selection
	}
	out := make([]*frontendv1.FeedQuestionGivenAnswer, 0, len(batch.GetQuestions()))
	for _, asked := range batch.GetQuestions() {
		given := &frontendv1.FeedQuestionGivenAnswer{
			Header: &frontendv1.FeedQuestionHeader{Text: asked.GetHeader()},
		}
		selection, ok := byQuestion[asked.GetQuestion().GetText()]
		if ok {
			for _, chosen := range selection.GetChosen() {
				given.Chosen = append(given.Chosen, chosen.GetLabel().GetLabel())
			}
			// AN ASK ALWAYS OFFERS A FREE-TEXT ESCAPE, so this may be set even
			// where the options looked exhaustive.
			if free := selection.GetFreeText(); free != nil {
				text := free.GetText()
				given.OtherText = &frontendv1.FeedQuestionOtherText{Text: text}
			}
		}
		out = append(out, given)
	}
	return out
}
