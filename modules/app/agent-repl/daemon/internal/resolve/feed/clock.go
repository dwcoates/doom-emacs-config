package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE HEAD'S CLOCK. Every purple-titled tool card -- a tool call, a question,
// a permission, a skill, a hook -- heads with one FeedToolCardClock: the
// instant its unit began, and, once it settles, the instant it stopped. The
// client ticks the running one and draws the stopped one's span, so the
// daemon states instants and never a figure. These three builders are the
// only way a card gets its clock, so every card's clock obeys one rule: no
// start, no clock; a stop before the start, no clock.

// runningClock is the clock of a unit under way since STARTMS, or nil when no
// start was stated.
func runningClock(startMs int64) *frontendv1.FeedToolCardClock {
	if startMs <= 0 {
		return nil
	}
	return &frontendv1.FeedToolCardClock{
		StartedAtMs: startMs,
		Span:        &frontendv1.FeedToolCardClock_Running{Running: &frontendv1.FeedToolCardClockRunning{}},
	}
}

// stoppedClock is the clock of a unit that ran from STARTMS to ENDMS, or nil
// when either instant is missing or the stop precedes the start: a settled
// card can neither tick nor show a span it does not know.
func stoppedClock(startMs, endMs int64) *frontendv1.FeedToolCardClock {
	if startMs <= 0 || endMs <= 0 || endMs < startMs {
		return nil
	}
	return &frontendv1.FeedToolCardClock{
		StartedAtMs: startMs,
		Span: &frontendv1.FeedToolCardClock_Stopped{
			Stopped: &frontendv1.FeedToolCardClockStopped{EndedAtMs: endMs},
		},
	}
}

// settledClock is the clock of a unit whose SETTLE carries its instant: from
// its start (clockStart) to the settle. Nil when the settle states no instant.
func settledClock(heldStartMs int64, settled *conversationv1.AgentActivitySettledAt) *frontendv1.FeedToolCardClock {
	if settled == nil {
		return nil
	}
	return stoppedClock(clockStart(heldStartMs, settled), settled.GetAtMs())
}

// clockStart is the start a settled unit's clock counts from. A START THIS
// PROCESS HELD STANDS: the restated start is the same instant, and taking the
// held one keeps a live card's clock exactly where it was. A replayed card,
// whose start the store no longer holds, counts from the start its settle
// RESTATES. Zero when neither is known.
func clockStart(heldStartMs int64, settled *conversationv1.AgentActivitySettledAt) int64 {
	if heldStartMs != 0 {
		return heldStartMs
	}
	return settled.GetStartedAt().GetAtMs()
}
