package feed

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// ③ THE SKILL CARD. Populated from EXACTLY TWO shim frames — the invocation
// and the document the skill contributed. NO temporal window folds subsequent
// responses under it: nothing delimits a skill's scope at the source, so
// inventing an end would be inventing a fact.

// drawSkill draws a skill invocation.
func (r *resolver) drawSkill(s *wsState, at placement, act *conversationv1.AgentActivity, skill *conversationv1.AgentSkillUse) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	card := &frontendv1.FeedSkill{}
	switch state := skill.GetResult().(type) {
	case *conversationv1.AgentSkillUse_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSkill", "branch": "case *conversationv1.AgentSkillUse_Start"})
		u.startHeld = true
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.input = composeInvocation(state.Start.GetSkill().GetName(), state.Start.GetArgs(), state.Start.Args != nil)
		card.Invocation = &frontendv1.FeedSkillInvocation{Text: u.input}
		if u.denied {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.denied"})
			card.Outcome = &frontendv1.FeedSkill_Denied{Denied: &frontendv1.FeedSkillDenied{}}
			break
		}
		card.Outcome = &frontendv1.FeedSkill_Running{Running: &frontendv1.FeedSkillRunning{}}
		// A START AFTER THE SETTLE DOES NOT REOPEN THE CARD. The file plane's
		// start can arrive after the stream plane's success under the same
		// key; the invocation it states is taken, the running arm is not,
		// because nothing will settle the skill a second time
		// (TestSkillNamedAndArgsParameterized, 2026-09-23).
		if prior := u.row.GetActivity().GetSkill(); prior != nil && prior.GetRunning() == nil && prior.GetOutcome() != nil {
			r.logger(s.id).Debug("daemon.feed.start_after_settle",
				"a skill's start arrived after it settled; the settled card stands",
				dlog.Context{"unit": unitID})
			card.Outcome = prior.GetOutcome()
		}
	case *conversationv1.AgentSkillUse_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSkill", "branch": "case *conversationv1.AgentSkillUse_Progress"})
		card.Invocation = &frontendv1.FeedSkillInvocation{Text: u.input}
		card.Outcome = &frontendv1.FeedSkill_Running{Running: &frontendv1.FeedSkillRunning{}}
	case *conversationv1.AgentSkillUse_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSkill", "branch": "case *conversationv1.AgentSkillUse_Success"})
		if u.input == "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.input == \"\""})
			u.input = composeInvocation(state.Success.GetSkill().GetName(), "", false)
		}
		card.Invocation = &frontendv1.FeedSkillInvocation{Text: u.input}
		loaded := &frontendv1.FeedSkillLoaded{
			Document: &frontendv1.FeedSkillDocument{Markdown: state.Success.GetDocument().GetMarkdown()},
		}
		// The consent line: WHAT INVOKING IT PERMITS. Absent when the skill
		// declared none — absence draws no line, never an empty one.
		if allowed := state.Success.GetAllowedTools().GetToolNames(); len(allowed) > 0 {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "allowed := state.Success.GetAllowedTools().GetToolNames(); len(allowed) > 0"})
			loaded.Allowances = &frontendv1.FeedSkillAllowances{
				Text: "allows: " + strings.Join(allowed, ", "),
			}
		}
		card.Outcome = &frontendv1.FeedSkill_Loaded{Loaded: loaded}
	case *conversationv1.AgentSkillUse_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSkill", "branch": "case *conversationv1.AgentSkillUse_Failure"})
		// The failure restates the skill so a replayed failure names it. A
		// start this process held stays the drawn line: it carries the
		// invocation's arguments, which no settled arm restates.
		var restated string
		if name := state.Failure.GetSkill().GetName(); name != "" {
			restated = composeInvocation(name, "", false)
		}
		input, err := r.restatedOrHeld(s, act, u, "skill_use", restated, u.input)
		if err != nil {
			return nil, err
		}
		if !u.startHeld {
			u.input = input
		}
		card.Invocation = &frontendv1.FeedSkillInvocation{Text: u.input}
		card.Outcome = &frontendv1.FeedSkill_Failed{Failed: &frontendv1.FeedSkillFailed{
			Text: skillFailureText(state.Failure.GetError()),
		}}
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSkill", "branch": "default"})
		return nil, errNotARow
	}

	row := &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unitID}),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Skill{Skill: card},
		}},
	}
	u.row = row
	return row, nil
}

// composeInvocation composes the line a user would have typed.
func composeInvocation(name, args string, hasArgs bool) string {
	line := "/" + strings.TrimPrefix(name, "/")
	if hasArgs && args != "" {
		line = line + " " + args
	}
	return line
}

// skillFailureText composes the reason a skill could not be loaded, falling
// back to a stated sentence rather than an empty card when the producer gave
// no account at all.
func skillFailureText(failure *conversationv1.AgentToolFailure) string {
	if text := failureText(failure); text != "" {
		return text
	}
	return "the skill could not be loaded, and the producer gave no account"
}
