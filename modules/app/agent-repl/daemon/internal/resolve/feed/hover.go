package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// A HOVERED BUBBLE SHOWS WHAT IT IS, HOW LONG IT TOOK AND HOW LONG AGO IT WAS
// SENT OR SETTLED, IN THE FOOTER'S ACTIVITY CELL (owner request, 2026-11-19).
// The daemon states it as FeedRow.hover, by ONE rule over the row's kind,
// stamped on every publication by `upsert`, so every hoverable kind is
// stamped the same way and the client derives nothing per kind.
//
// Every instant is one the row already states (a card's clock, a merge's
// runtime, a response's usage stamp) or, for a prompt, the conversation place
// of the first entry that drew it (`recordPromptSent`), so a replay serves the
// same stamp. A prompt's instant is recorded HERE, on the one write path, for
// every site that draws a prompt (a delivered prompt, a subagent's
// commission, a message one agent sent another, the prompt queue's mirror),
// exactly as order.go mints a row's order key from the entry that first drew
// it; the mirror draws outside any entry, so its row has no instant until the
// conversation's own entry draws it.

// The labels a hover stamp opens with, one per hoverable kind.
const (
	hoverPrompt     = "Prompt"
	hoverResponse   = "Response"
	hoverToolCall   = "Tool call"
	hoverSkill      = "Skill"
	hoverHook       = "Hook"
	hoverPermission = "Permission"
	hoverQuestion   = "Question"
	hoverShell      = "Shell"
	hoverSubagent   = "Subagent"
	hoverMerge      = "Merge"
	hoverFixGate    = "Testing the fix"
)

// stampHover states ROW's hover stamp from the row itself and, for a prompt,
// the instant F recorded for row ID, first recording the place of the entry S
// is drawing when none is known yet. A kind with no hover line gets none.
func stampHover(s *wsState, f *feedState, id string, row *frontendv1.FeedRow) {
	if isPromptRow(row) {
		recordPromptSent(f, id, s.entryPlace.GetAtMs())
	}
	row.Hover = hoverOf(row, f.sentAtMs[id])
}

// hoverOf answers ROW's hover stamp, SENT being the instant its prompt was
// sent (zero when none is known or the row is no prompt), or nil for a kind
// that shows nothing when hovered.
func hoverOf(row *frontendv1.FeedRow, sent int64) *frontendv1.FeedHoverStamp {
	switch {
	case isPromptRow(row):
		return hoverStamp(hoverPrompt, 0, sent)
	case row.GetShellHead() != nil:
		shell := row.GetShellHead()
		return hoverStamp(hoverShell, shell.GetRuntime().GetStartedAtMs(), shell.GetSettled().GetEndedAtMs())
	case row.GetDetachedSubagent() != nil:
		return subagentHover(row.GetDetachedSubagent().GetSubagent())
	case row.GetPermission() != nil:
		return clockHover(hoverPermission, row.GetPermission().GetClock())
	case row.GetQuestion() != nil:
		return clockHover(hoverQuestion, row.GetQuestion().GetClock())
	case row.GetActivity() != nil:
		return activityHover(row.GetActivity())
	}
	return nil
}

// activityHover answers the hover stamp of one unit of a turn's progress, or
// nil for a unit that shows nothing when hovered.
func activityHover(act *frontendv1.FeedTurnActivity) *frontendv1.FeedHoverStamp {
	switch {
	case act.GetResponse() != nil:
		return responseHover(act.GetResponse().GetUsage())
	case act.GetSimpleToolCall() != nil:
		call := act.GetSimpleToolCall()
		return clockHover(named(hoverToolCall, call.GetName().GetText()), call.GetClock())
	case act.GetSkill() != nil:
		skill := act.GetSkill()
		return clockHover(named(hoverSkill, skill.GetInvocation().GetText()), skill.GetClock())
	case act.GetHook() != nil:
		return clockHover(hoverHook, act.GetHook().GetClock())
	case act.GetSubagent() != nil:
		return subagentHover(act.GetSubagent())
	case act.GetMerge() != nil:
		return mergeHover(act.GetMerge())
	case act.GetFixGate() != nil:
		return fixGateHover(act.GetFixGate())
	}
	return nil
}

// responseHover restates a response's usage stamp: its figure in its heat,
// and the span the figure counts. A response that has stated no usage yet
// shows its label alone.
func responseHover(usage *frontendv1.FeedResponseUsageStamp) *frontendv1.FeedHoverStamp {
	stamp := hoverStamp(hoverResponse, usage.GetStartedAtMs(), usage.GetAtMs())
	if usage != nil {
		stamp.Tokens = &frontendv1.FeedHoverStampTokens{Text: usage.GetText(), Heat: usage.GetHeat()}
	}
	return stamp
}

// clockHover answers LABEL over a card's head clock: its start, and its end
// once it stopped. A card whose producer stated no clock shows its label alone.
func clockHover(label string, clock *frontendv1.FeedToolCardClock) *frontendv1.FeedHoverStamp {
	return hoverStamp(label, clock.GetStartedAtMs(), clock.GetStopped().GetEndedAtMs())
}

// subagentHover answers a subagent card's stamp, named by its label.
func subagentHover(sub *frontendv1.FeedSubagent) *frontendv1.FeedHoverStamp {
	return hoverStamp(named(hoverSubagent, sub.GetLabel().GetText()),
		sub.GetRuntime().GetStartedAtMs(), sub.GetSettled().GetEndedAtMs())
}

// mergeHover answers a merge bubble's stamp: its runtime, ended by whichever
// way it ended.
func mergeHover(merge *frontendv1.FeedMerge) *frontendv1.FeedHoverStamp {
	ended := merge.GetSuccess().GetEndedAtMs()
	if failed := merge.GetError(); failed != nil {
		ended = failed.GetEndedAtMs()
	}
	return hoverStamp(hoverMerge, merge.GetHead().GetRuntime().GetStartedAtMs(), ended)
}

// fixGateHover answers a fix gate's stamp: the run's start, and its end once
// it settled.
func fixGateHover(gate *frontendv1.FeedFixGate) *frontendv1.FeedHoverStamp {
	if settled := gate.GetSettled(); settled != nil {
		return hoverStamp(hoverFixGate, settled.GetStartedAtMs(), settled.GetEndedAtMs())
	}
	return hoverStamp(hoverFixGate, gate.GetLive().GetStartedAtMs(), 0)
}

// hoverStamp is the one composition of a stamp: LABEL, the span's start
// STARTED and the sent-or-settled instant AT, zero for either unknown.
func hoverStamp(label string, started, at int64) *frontendv1.FeedHoverStamp {
	return &frontendv1.FeedHoverStamp{
		Label:       &frontendv1.FeedHoverStampLabel{Text: label},
		StartedAtMs: started,
		AtMs:        at,
	}
}

// named answers LABEL followed by NAME, or LABEL alone when NAME is empty.
func named(label, name string) string {
	if name == "" {
		return label
	}
	return label + " " + name
}

// recordPromptSent records that the prompt row ID of feed F was sent at AT,
// epoch ms, the conversation place of the entry drawing it (zero outside any
// entry, which records nothing). The first known instant stays: a prompt
// restated by another store plane, re-drawn by a replay, or re-published
// while a later entry is drawn names the entry that first drew it.
func recordPromptSent(f *feedState, id string, at int64) {
	if at == 0 || f.sentAtMs[id] != 0 {
		return
	}
	f.sentAtMs[id] = at
}
