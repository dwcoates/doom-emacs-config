package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A FAILURE AGENT-REPL RETRIES DRAWS NO ROW (owner ruling, 2026-11-15). The
// shim retries a main turn the vendor failed retryably itself, and states on
// the failed terminal that it will (conversation.v1.AgentFailure
// .retry_promised): the retry continues the work, the footer says the retry
// is pending, and a retry that succeeds leaves nothing to report. So the
// feed withholds that terminal's errored row.
//
// THE PROMISE RIDES THE TERMINAL ITSELF, never the session stream's
// schedule: the two streams have no ordering between them, so reading the
// schedule would draw the row and take it back, and a replayed turn has only
// its terminal.
//
// A PROMISE NEVER KEPT is stated on the session stream as
// conversation.v1.SessionVendorErrorRetryAbandoned naming the failed turn,
// and the withheld row is drawn then. The abandonment is restated on every
// WatchSession open, so it may land before or after the terminal it names
// (a replay draws terminals from history pages): whichever comes second
// draws the row. A chain that gives up at a failure promises that failure
// nothing, so its row draws as any failure's does.
//
// THE TURN'S VENDOR NOTICES ARE WITHHELD WITH IT. A notice and the fault it
// reports are one card (notice.go), so a notice of a turn whose failure is
// withheld folds into the withheld row, whichever arrives first: drawn on
// its own, it would report the retried failure the ruling keeps off the feed.

const (
	opRetriedFailureWithheld = "daemon.feed.retried_failure_withheld"
	opRetryAbandoned         = "daemon.feed.retry_abandoned"
	opNoticeFoldedIntoHeld   = "daemon.feed.notice_folded_into_withheld_failure"
)

// withheldEnding is a retried failure's errored row, kept with the placement
// it would have landed at so an abandonment can draw it there.
type withheldEnding struct {
	at  placement
	row *frontendv1.FeedRow
}

// retriedFailure reports whether FAILURE of TURN is a failure agent-repl
// retries and whose retry was not abandoned: its row is withheld.
func (s *wsState) retriedFailure(turn ids.TurnID, failure *conversationv1.AgentFailure) bool {
	return failure.GetRetryPromised() != nil && !s.abandonedRetries[turn]
}

// withholdRetriedFailure keeps the errored ROW of TURN, placed at AT, off the
// feed while its promised retry stands.
func (r *resolver) withholdRetriedFailure(s *wsState, turn ids.TurnID, at placement, row *frontendv1.FeedRow, failure *conversationv1.AgentFailure) {
	// THE NOTICES ALREADY DRAWN fold into the withheld row and leave the feed.
	for _, notice := range r.foldNoticesIntoEnding(s, r.feed(s, at.feed), row.GetId().GetValue(), row) {
		r.retire(s, at.feed, notice)
	}
	s.withheldEndings[turn] = withheldEnding{at: at, row: row}
	r.logger(s.id).Info(opRetriedFailureWithheld,
		"the turn failed on a failure agent-repl retries; its errored row is withheld while the retry stands",
		dlog.Context{"turn": string(turn), "attempt": failure.GetRetryPromised().GetAttempt(), "outcome": terminalArm(row.GetTurnEnded())})
}

// observeVendorErrorRetry takes the session's retry schedule. Only an
// abandonment changes a row: the withheld failure of the turn it names is
// drawn, or, when that terminal has not been seen yet, is drawn when it is.
func (r *resolver) observeVendorErrorRetry(s *wsState, stated *conversationv1.SessionVendorErrorRetry) {
	abandoned := stated.GetAbandoned()
	if abandoned == nil {
		r.logger(s.id).Debug("daemon.feed.vendor_error_retry_ignored",
			"a vendor-error retry statement that abandons nothing changes no feed row", nil)
		return
	}
	turn := ids.TurnID(abandoned.GetFailedTurn().GetValue())
	if turn == "" {
		r.logger(s.id).Error(opRetryAbandoned,
			"an abandoned vendor-error retry names no failed turn; no withheld failure can be drawn for it",
			dlog.Context{"cause": abandoned.GetCause(), "invariant_violation": "an abandoned vendor-error retry names its failed turn"})
		return
	}
	s.abandonedRetries[turn] = true
	withheld, held := s.withheldEndings[turn]
	if !held {
		r.logger(s.id).Info(opRetryAbandoned,
			"agent-repl abandoned a promised retry; the failed turn's errored row draws when its terminal is seen",
			dlog.Context{"turn": string(turn), "cause": abandoned.GetCause()})
		return
	}
	delete(s.withheldEndings, turn)
	r.upsert(s, withheld.at, withheld.row, true)
	r.logger(s.id).Info(opRetryAbandoned,
		"agent-repl abandoned a promised retry; the failed turn's withheld errored row was drawn",
		dlog.Context{"turn": string(turn), "cause": abandoned.GetCause(), "row": withheld.row.GetId().GetValue()})
}

// noticeFoldedIntoWithheld takes a notice ROW of a turn whose failure is
// withheld: its words fold into the withheld row, and the notice is drawn
// nowhere (retired, had an earlier draw placed it). It reports whether ROW
// was taken that way.
func (r *resolver) noticeFoldedIntoWithheld(s *wsState, f *feedState, at placement, row *frontendv1.FeedRow) bool {
	text, ok := noticeText(row)
	turn := ids.TurnID(row.GetTurn().GetValue())
	if !ok || turn == "" {
		return false
	}
	withheld, held := s.withheldEndings[turn]
	if !held || r.feed(s, withheld.at.feed).key != f.key {
		return false
	}
	added, why := foldNotice(faultEndingMarker(withheld.row), text)
	id := row.GetId().GetValue()
	r.retire(s, at.feed, id)
	r.logger(s.id).Info(opNoticeFoldedIntoHeld,
		"a vendor notice of a turn whose retried failure is withheld was folded into the withheld row instead of being drawn",
		dlog.Context{"feed": f.key, "turn": string(turn), "notice_row": id, "added": added, "not_added_because": why})
	return true
}
