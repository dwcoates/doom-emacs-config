package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/figures"
	"claude-repld/internal/freshinput"

	"google.golang.org/protobuf/proto"
)

// THE FEED'S TOKEN FIGURES ARE FRESH INPUT, ONE AGENT AT A TIME.
//
// Every figure this file draws counts fresh input (freshinput.Of: the input
// tokens that were not cache hits) for ONE agent, tallied in an ACCOUNT:
//   - the MAIN agent's account is its TURN — it starts again at every turn,
//     and it is the same tally the footer's tokens cell draws;
//   - a SUBAGENT's account is its whole LIFETIME, which is the figure its card
//     draws (FeedSubagentTokens).
//
// Usage rides exactly one unit per API response — for the vendor's observed
// `[thinking, text]` shape, the THINKING unit, not the prose one — so usage is
// tallied by the account of the agent that produced it, never by the bubble of
// the unit it rode on. A unit's frames restate the same usage, so each unit's
// fresh input REPLACES its previous figure in the tally rather than adding to
// it.
//
// A RESPONSE BUBBLE'S STAMP IS A DELTA, FROZEN WHEN IT LANDS. A bubble takes,
// when it is first drawn, the tally its account stood at when that account's
// previous bubble LANDED (its base); while it arrives its stamp is the tally
// minus that base, and it grows as usage arrives; when it settles the stamp is
// frozen and the account's landed mark moves to the tally. So an account's
// bubbles partition its tally. The only later change is the final answer's:
// at the turn's end the green bubble is re-stamped ONCE with the main agent's
// whole turn tally (stampFinalAnswerTotal).

// usageAccount names the account an agent's usage is tallied in: the main
// agent's current turn, or a subagent's lifetime. The two key spaces carry
// distinct prefixes, so a turn id can never collide with an agent id.
func (s *wsState) usageAccount(agent *conversationv1.AgentId) string {
	id := agent.GetValue()
	if id == "" || s.mainAgent == "" || id == s.mainAgent {
		turn := ""
		if t := s.rowTurn(); t != nil {
			turn = string(*t)
		}
		return "turn\x00" + turn
	}
	return "agent\x00" + id
}

// recordUsage files one unit's usage in its agent's account. It reports the
// account the usage was tallied in and whether any usage was recorded, so the
// caller can re-stamp what the tally moves. A unit is filed ONCE: its later
// frames restate the same unit and replace its figure in the same account.
func (r *resolver) recordUsage(s *wsState, unit string, agent *conversationv1.AgentId, usage *conversationv1.TokenUsage) (string, bool) {
	if usage == nil {
		return "", false
	}
	account, filed := s.unitAccount[unit]
	if !filed {
		account = s.usageAccount(agent)
		s.unitAccount[unit] = account
	}
	fresh := freshinput.Of(usage)
	previous := s.unitFresh[unit]
	if filed && fresh < previous {
		r.logger(s.id).Error("daemon.feed.fresh_input_regressed",
			"a unit restated a smaller fresh input than it stated before; the tally takes the new figure",
			dlog.Context{"unit": unit, "agent": agent.GetValue(), "previous": previous, "restated": fresh})
	}
	s.unitFresh[unit] = fresh
	s.accountTally[account] = s.accountTally[account] - previous + fresh
	s.accountStated[account] = true
	return account, true
}

// accountStamp formats an account's tally minus BASE, empty when the account
// has stated no usage — absence draws no stamp, never a zero. A tally below
// the base (a unit that restated a smaller figure, already recorded by
// recordUsage) draws zero rather than wrapping.
func (s *wsState) accountStamp(account string, base uint64) string {
	if !s.accountStated[account] {
		return ""
	}
	tally := s.accountTally[account]
	if tally < base {
		return figures.Tokens(0)
	}
	return figures.Tokens(tally - base)
}

// openStamp answers the stamp a response fold draws now. A fold learns its
// account and its base on its first draw; a frozen fold keeps the figure it
// landed with.
func (s *wsState) openStamp(fold *proseState, agent *conversationv1.AgentId) string {
	if fold.account == "" {
		fold.account = s.usageAccount(agent)
		fold.base = s.accountLanded[fold.account]
	}
	if fold.frozen {
		return fold.usage
	}
	return s.accountStamp(fold.account, fold.base)
}

// landStamp freezes a settling fold's stamp and moves its account's landed
// mark to the tally, so the account's next bubble counts from here. A fold
// lands ONCE: a settle restated by the other store plane keeps the figure the
// first settle froze.
func (s *wsState) landStamp(fold *proseState) {
	if fold.frozen {
		return
	}
	fold.frozen = true
	if tally := s.accountTally[fold.account]; tally > s.accountLanded[fold.account] {
		s.accountLanded[fold.account] = tally
	}
}

// restampOpenBubbles re-pushes every drawn, still-arriving response bubble of
// ACCOUNT with the stamp the account's tally now gives it, so a bubble grows
// while its usage arrives on a sibling unit. A landed bubble is never touched.
func (r *resolver) restampOpenBubbles(s *wsState, account string) {
	for unit, fold := range s.responses {
		if fold.frozen || fold.account != account || fold.row == nil {
			continue
		}
		stamp := s.accountStamp(account, fold.base)
		if stamp == "" || stamp == fold.usage {
			continue
		}
		fold.usage = stamp
		r.restampResponseRow(s, fold, unit, stamp)
	}
}

// stampFinalAnswerTotal gives a concluded turn's final-answer fold the main
// agent's whole turn tally — the footer cell's final figure — as its frozen
// stamp, reporting whether the turn stated any usage to stamp. It is the one
// change a landed bubble's figure ever sees.
func (s *wsState) stampFinalAnswerTotal(fold *proseState) bool {
	total := s.accountStamp(fold.account, 0)
	if total == "" {
		return false
	}
	fold.frozen = true
	fold.usage = total
	return true
}

// restampResponseRow re-publishes a fold's drawn row with STAMP as its figure,
// keeping the settled instant. A row that left its feed is not re-published.
func (r *resolver) restampResponseRow(s *wsState, fold *proseState, unit, stamp string) {
	f := r.feed(s, fold.feed)
	row, ok := f.rows[fold.row.GetValue()]
	if !ok || row.GetActivity().GetResponse() == nil {
		return
	}
	restamped, ok := proto.Clone(row).(*frontendv1.FeedRow)
	if !ok {
		r.logger(s.id).Error("daemon.feed.row_not_clonable",
			"a response bubble could not be snapshotted to re-stamp its fresh input",
			dlog.Context{"feed": f.key, "row": fold.row.GetValue(), "unit": unit})
		return
	}
	bubble := restamped.GetActivity().GetResponse()
	bubble.Usage = &frontendv1.FeedResponseUsageStamp{Text: stamp, AtMs: bubble.GetUsage().GetAtMs()}
	r.upsert(s, placement{feed: fold.feed}, restamped, true)
}

// subagentFigure formats a subagent's lifetime fresh input for its card, nil
// before the subagent has stated any usage.
func (s *wsState) subagentFigure(created *conversationv1.AgentId) *frontendv1.FeedSubagentTokens {
	if created.GetValue() == "" {
		return nil
	}
	stamp := s.accountStamp(s.usageAccount(created), 0)
	if stamp == "" {
		return nil
	}
	return &frontendv1.FeedSubagentTokens{Text: stamp + " tok"}
}

// restampSubagentCard re-pushes the card of the subagent AGENT names with its
// lifetime fresh input, when the card is drawn and its figure moved. The main
// agent has no card, so its usage re-stamps nothing here.
func (r *resolver) restampSubagentCard(s *wsState, agent *conversationv1.AgentId) {
	for unit, state := range s.subagents {
		if state.created.GetValue() != agent.GetValue() || state.row == nil {
			continue
		}
		figure := s.subagentFigure(state.created)
		if figure == nil || figure.GetText() == state.bubble.GetTokens().GetText() {
			continue
		}
		state.bubble.Tokens = figure
		f := r.feed(s, state.feed.feed)
		row, ok := f.rows[state.row.GetValue()]
		if !ok {
			continue
		}
		restamped, ok := proto.Clone(row).(*frontendv1.FeedRow)
		if !ok {
			r.logger(s.id).Error("daemon.feed.row_not_clonable",
				"a subagent card could not be snapshotted to re-stamp its fresh input",
				dlog.Context{"feed": f.key, "row": state.row.GetValue(), "unit": unit})
			continue
		}
		switch arm := restamped.GetRow().(type) {
		case *frontendv1.FeedRow_DetachedSubagent:
			arm.DetachedSubagent.GetSubagent().Tokens = figure
		case *frontendv1.FeedRow_Activity:
			arm.Activity.GetSubagent().Tokens = figure
		}
		r.upsert(s, state.feed, restamped, true)
	}
}
