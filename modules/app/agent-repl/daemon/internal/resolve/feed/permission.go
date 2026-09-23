package feed

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// ④ BLOCKING THE TURN ON THE USER: consent, and a choice. Two cards, mirroring
// the conversation-level split. Both upsert through their states, so the
// settled card is drawn from the row alone — a cold repaint has nothing else
// to draw the decision from.

// drawPermission draws the consent card. THE STANDING'S CONTENT NEVER REACHES
// A CLIENT: the daemon holds the vendor's echo token and hands it back to the
// shim when the answer verb picks standing; the row carries only the fact that
// an "always allow" button is drawable.
func (r *resolver) drawPermission(s *wsState, agent *conversationv1.AgentId, p *conversationv1.AgentPermission) {
	log := r.logger(s.id)
	askID := p.GetId().GetValue()
	if askID == "" {
		log.Error("daemon.feed.permission_without_identity",
			"a permission frame carried no ask identity",
			dlog.Context{"agent": agent.GetValue()})
		return
	}

	state, known := s.permissionRows[askID]
	if !known {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!known"})
		at, ok := r.place(s, agent)
		if !ok {
			return
		}
		state = &permissionState{
			feed: at,
			row:  r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPermission, ID: askID}),
			card: &frontendv1.FeedPermission{},
		}
		s.permissionRows[askID] = state
	}
	// The asking agent is recorded on EVERY frame, not only the opening one:
	// an adoption can meet the ask mid-flight, and an answer with no agent to
	// deliver it to is an ask nobody can settle.
	if agent.GetValue() != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "agent.GetValue() != \"\""})
		state.agent = agent
	}
	card := state.card

	switch frame := p.GetResult().(type) {
	case *conversationv1.AgentPermission_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPermission", "branch": "case *conversationv1.AgentPermission_Start"})
		start := frame.Start
		card.Headline = &frontendv1.FeedPermissionHeadline{Text: start.GetPrompt().GetTitle()}
		if start.GetPrompt().Description != nil && start.GetPrompt().GetDescription() != "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "start.GetPrompt().Description != nil && start.GetPrompt().GetDescription() != \"\""})
			card.Subtitle = &frontendv1.FeedPermissionSubtitle{Text: start.GetPrompt().GetDescription()}
		}
		if note := triggerNote(start.GetTrigger()); note != "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "note := triggerNote(start.GetTrigger()); note != \"\""})
			card.Trigger = &frontendv1.FeedPermissionTriggerNote{Text: note}
		}
		card.Arguments = &frontendv1.FeedPermissionArguments{
			Lines: r.gatedArgumentLines(s, p.GetGatedCall().GetValue()),
		}
		// PRESENCE IS THE FACT, and the token stays here.
		if standing := start.GetOfferedStanding(); standing != nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "standing := start.GetOfferedStanding(); standing != nil"})
			card.StandingOffered = &frontendv1.FeedPermissionStandingOffered{}
			s.standing[state.row.GetValue()] = standing
		}
		card.State = &frontendv1.FeedPermission_Open{Open: &frontendv1.FeedPermissionOpen{}}
		s.gatedCalls[askID] = p.GetGatedCall().GetValue()
	case *conversationv1.AgentPermission_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPermission", "branch": "case *conversationv1.AgentPermission_Success"})
		answered := &frontendv1.FeedPermissionAnswered{AtMs: r.deps.Now().UnixMilli()}
		decisionArm(frame.Success)(answered)
		card.State = &frontendv1.FeedPermission_Answered{Answered: answered}
		// A DENIED CALL NEVER RAN, so its own card says denied rather than
		// sitting running forever waiting on a tool that will not start.
		if denied, ok := frame.Success.GetDecision().(*conversationv1.AgentPermissionSuccess_Denied); ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "denied, ok := frame.Success.GetDecision().(*conversationv1.AgentPermissionSuccess_Denied); ok"})
			// The unit is joined by id: `gated_call` when the frame names one,
			// and otherwise the permission's OWN id, which the shim mints as
			// the gated unit's AgentActivityId.
			r.markCallDenied(s, gatedUnit(p), denialWord(denied.Denied))
		}
	case *conversationv1.AgentPermission_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPermission", "branch": "case *conversationv1.AgentPermission_Failure"})
		card.State = &frontendv1.FeedPermission_Abandoned{Abandoned: &frontendv1.FeedPermissionAbandoned{
			AtMs: r.deps.Now().UnixMilli(),
		}}
	default:
		return
	}

	// THE HEADLINE IS REQUIRED, and a gate that SETTLED WITHOUT EVER ASKING
	// sent no `Start` frame to carry the vendor's sentence. `!perm-undecidable`
	// is exactly that shape -- the classifier reaches no verdict and the vendor
	// denies the call outright -- and the card published without a headline
	// drew as an unreadable row on the client rather than as the denial it is.
	// The vendor's own title, when it composes none of its own, IS the tool
	// name (the gate defaults `title` to it), so the unasked card says the same
	// thing rather than inventing a sentence the vendor never said.
	if card.Headline == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "card.Headline == nil"})
		card.Headline = &frontendv1.FeedPermissionHeadline{Text: r.unaskedHeadline(s, gatedUnit(p))}
	}
	if card.Arguments == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "card.Arguments == nil"})
		card.Arguments = &frontendv1.FeedPermissionArguments{}
	}
	row := &frontendv1.FeedRow{
		Id:  state.row,
		Row: &frontendv1.FeedRow_Permission{Permission: card},
	}
	r.stampTurn(s, row, state.turn)
	log.Debug("daemon.feed.permission",
		"a consent card was upserted",
		dlog.Context{"ask": askID, "row": state.row.GetValue(), "standing_offered": card.GetStandingOffered() != nil})
	r.upsert(s, state.feed, row, true)
}

// triggerNote composes WHY the gate fired. The three facts are INDEPENDENT —
// the vendor can report a blocked path AND a forcing ask rule AND an
// unclassified note on one ask — so all present ones are drawn; dropping any
// would hide part of the reason.
//
// An ask-rule trigger is worded so a reader knows the prompt was
// USER-CONFIGURED and must not be auto-approved.
func triggerNote(trigger *conversationv1.AgentPermissionTrigger) string {
	if trigger == nil {
		return ""
	}
	var parts []string
	if blocked := trigger.GetBlockedPath(); blocked != nil {
		parts = append(parts, "path outside the allowed directories: "+blocked.GetPath())
	}
	if rule := trigger.GetAskRule(); rule != nil {
		line := "you configured a rule that always asks for " + rule.GetToolName()
		if rule.RuleContent != nil && rule.GetRuleContent() != "" {
			line = line + " (" + rule.GetRuleContent() + ")"
		}
		line = line + ", from " + rule.GetSource()
		parts = append(parts, line)
	}
	if note := trigger.GetNote(); note != nil {
		parts = append(parts, note.GetText())
	}
	return strings.Join(parts, " · ")
}

// gatedArgumentLines draws the gated call's argument preview. The vendor states
// no arguments on the ask itself, so the daemon uses the line it already
// composed for that call's own card — one composition, drawn in two places,
// rather than a second phrasing that could disagree with the card.
func (r *resolver) gatedArgumentLines(s *wsState, gatedCall string) []string {
	if gatedCall == "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "gatedCall == \"\""})
		return nil
	}
	u, ok := s.units[gatedCall]
	if !ok || u.input == "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok || u.input == \"\""})
		return nil
	}
	return []string{u.input}
}

// gatedUnit names the unit an ask gates: `gated_call` when the frame carries
// one, and otherwise the permission's OWN id, which the shim mints as the
// gated unit's AgentActivityId.
func gatedUnit(p *conversationv1.AgentPermission) string {
	if gated := p.GetGatedCall().GetValue(); gated != "" {
		return gated
	}
	return p.GetId().GetValue()
}

// unaskedHeadline composes the headline for a card whose ask never opened, by
// naming the gated tool. A gated call the feed never drew leaves nothing to
// name, and the card says so rather than claiming a tool it cannot identify.
func (r *resolver) unaskedHeadline(s *wsState, gatedCall string) string {
	if gatedCall != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "gatedCall != \"\""})
		if u, ok := s.units[gatedCall]; ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u, ok := s.units[gatedCall]; ok"})
			if name := u.row.GetActivity().GetSimpleToolCall().GetName().GetText(); name != "" {
				r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "name := u.row.GetActivity().GetSimpleToolCall().GetName().GetText(); name != \"\""})
				return name
			}
			if skill := u.row.GetActivity().GetSkill(); skill != nil {
				r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "skill := u.row.GetActivity().GetSkill(); skill != nil"})
				return skill.GetInvocation().GetText()
			}
		}
	}
	return "a gated call"
}

// decisionArm renders the verdict line's treatment. A DENIAL IS AN ANSWER, not
// an error.
func decisionArm(success *conversationv1.AgentPermissionSuccess) permissionAnswer {
	switch decision := success.GetDecision().(type) {
	case *conversationv1.AgentPermissionSuccess_Allowed:
		if _, standing := decision.Allowed.GetScope().(*conversationv1.AgentPermissionAllowed_Standing); standing {
			return allowedStandingAnswer()
		}
		return allowedOnceAnswer()
	case *conversationv1.AgentPermissionSuccess_Denied:
		switch by := decision.Denied.GetBy().(type) {
		case *conversationv1.AgentPermissionDenied_User:
			return deniedByUserAnswer()
		case *conversationv1.AgentPermissionDenied_Policy:
			return deniedByPolicyAnswer(policyReason(by.Policy))
		case *conversationv1.AgentPermissionDenied_Undecidable:
			// NOBODY REFUSED: drawn as the user's act it accuses them of
			// something they did not do, and as policy it implies a rule that
			// does not exist.
			text := "denied for want of a decider — the deciding machinery could not be reached"
			if by.Undecidable.Detail != nil && by.Undecidable.GetDetail() != "" {
				text = text + ": " + by.Undecidable.GetDetail()
			}
			return deniedUndecidableAnswer(text)
		}
	}
	return deniedByPolicyAnswer("denied")
}

// permissionAnswer sets the verdict line's treatment. A setter rather than the
// generated oneof interface, whose method is unexported and unimplementable
// from here.
type permissionAnswer func(*frontendv1.FeedPermissionAnswered)

// allowedOnce, allowedStanding, deniedByUser, deniedByPolicy and
// deniedUndecidable are the five verdict setters, spelled once each.
func allowedOnceAnswer() permissionAnswer {
	return func(a *frontendv1.FeedPermissionAnswered) {
		a.Answer = &frontendv1.FeedPermissionAnswered_AllowedOnce{
			AllowedOnce: &frontendv1.FeedPermissionAllowedOnce{},
		}
	}
}

// allowedStandingAnswer is the standing grant: the vendor stops asking for
// this form from now on.
func allowedStandingAnswer() permissionAnswer {
	return func(a *frontendv1.FeedPermissionAnswered) {
		a.Answer = &frontendv1.FeedPermissionAnswered_AllowedStanding{
			AllowedStanding: &frontendv1.FeedPermissionAllowedStanding{},
		}
	}
}

// deniedByUserAnswer is the user's refusal — an answer, not an error.
func deniedByUserAnswer() permissionAnswer {
	return func(a *frontendv1.FeedPermissionAnswered) {
		a.Answer = &frontendv1.FeedPermissionAnswered_DeniedByUser{
			DeniedByUser: &frontendv1.FeedPermissionDeniedByUser{},
		}
	}
}

// deniedByPolicyAnswer is a refusal NOBODY was asked for, worded so it never
// reads as the user's act.
func deniedByPolicyAnswer(text string) permissionAnswer {
	return func(a *frontendv1.FeedPermissionAnswered) {
		a.Answer = &frontendv1.FeedPermissionAnswered_DeniedByPolicy{
			DeniedByPolicy: &frontendv1.FeedPermissionDeniedByPolicy{Text: text},
		}
	}
}

// deniedUndecidableAnswer is the denial NOBODY reached: the classifier came to
// no verdict and no rule applied, so the call was denied for want of a decider.
// Its own arm (landing 10) rather than policy's, which would imply a rule that
// does not exist.
func deniedUndecidableAnswer(text string) permissionAnswer {
	return func(a *frontendv1.FeedPermissionAnswered) {
		a.Answer = &frontendv1.FeedPermissionAnswered_DeniedUndecidable{
			DeniedUndecidable: &frontendv1.FeedPermissionDeniedUndecidable{Text: text},
		}
	}
}

// policyReason composes a policy denial's wording from what the vendor named.
func policyReason(policy *conversationv1.AgentPermissionDeniedByPolicy) string {
	if policy.Reason != nil && policy.GetReason() != "" {
		return policy.GetReason()
	}
	if policy.Decider != nil && policy.GetDecider() != "" {
		return "denied by " + policy.GetDecider()
	}
	return "denied by rule"
}

// denialWord names who refused, for the log record.
func denialWord(denied *conversationv1.AgentPermissionDenied) string {
	switch denied.GetBy().(type) {
	case *conversationv1.AgentPermissionDenied_User:
		return "user"
	case *conversationv1.AgentPermissionDenied_Policy:
		return "policy"
	case *conversationv1.AgentPermissionDenied_Undecidable:
		return "undecidable"
	}
	return "unset"
}

// markCallDenied flips the gated call's card to denied and re-pushes it.
func (r *resolver) markCallDenied(s *wsState, gatedCall, by string) {
	if gatedCall == "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "gatedCall == \"\""})
		return
	}
	u := s.unit(gatedCall)
	u.denied = true
	if u.row == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.row == nil"})
		return
	}
	card := u.row.GetActivity().GetSimpleToolCall()
	if card == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "card == nil"})
		// A skill the gate refused says so on its own card.
		if skill := u.row.GetActivity().GetSkill(); skill != nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "skill := u.row.GetActivity().GetSkill(); skill != nil"})
			skill.Outcome = &frontendv1.FeedSkill_Denied{Denied: &frontendv1.FeedSkillDenied{}}
			r.upsertUnitRow(s, u)
		}
		return
	}
	deniedOutcome()(card)
	r.upsertUnitRow(s, u)
	r.logger(s.id).Debug("daemon.feed.tool_call_denied",
		"the gate refused a call, so its card says it never ran",
		dlog.Context{"unit": gatedCall, "denied_by": by})
}

// upsertUnitRow re-pushes a unit's row onto the feed it last landed on.
func (r *resolver) upsertUnitRow(s *wsState, u *unitState) {
	for key, f := range s.feeds {
		if key != u.feedKey {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "key != u.feedKey"})
			continue
		}
		if _, ok := f.rows[u.row.GetId().GetValue()]; ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "_, ok := f.rows[u.row.GetId().GetValue()]; ok"})
			r.upsert(s, placement{feed: s.feedAddrs[key]}, u.row, true)
			return
		}
	}
	r.upsert(s, placement{feed: feedid.Feed{Root: true}}, u.row, true)
}
