package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// A RESPONSE IS INTERIM ONCE A LATER ROW OF ITS TURN LANDS AFTER IT (owner
// rule, 2026-10-08). An interim response collapses to one line and takes the
// page's background; a final answer — and an answer still arriving, which may
// become the final one — is drawn in full. Which response ANSWERS the turn is
// known only at its end, but which responses CANNOT is known the moment
// anything of the same turn follows them, so that is the fact drawn.
//
// IT IS A FACT ABOUT THE FEED'S OWN ORDER, kept exactly as superseded.go keeps
// a thinking row's flag: decided at the two structural edges (a row PLACED, a
// row RETIRED), stamped on every draw by `upsert`, and the earlier row
// re-pushed after the edge's own publication. The invariant the edges keep is
// that a non-thinking response is interim exactly when a COUNTING row of its
// turn sorts after it, so an edge only looks at the nearest response before it.
//
// A COUNTING ROW is any row of the same turn but the turn's ending row: the
// turn ending after its answer is not something that follows the answer.

// isProseRow reports whether ROW is a non-thinking response: the only row kind
// that can BE interim.
func isProseRow(row *frontendv1.FeedRow) bool {
	return isResponseRow(row) && !isThinkingRow(row)
}

// countsAfter reports whether ROW, sorting after a response of TURN, proves
// that response interim.
func countsAfter(row *frontendv1.FeedRow, turn string) bool {
	return row.GetTurn().GetValue() == turn && row.GetTurnEnded() == nil
}

// stampInterim states a prose row's flag from the feed's record. It runs on
// the one write path (`upsert`), so every redraw of the fold restates it.
func stampInterim(f *feedState, id string, row *frontendv1.FeedRow) {
	if !isProseRow(row) {
		return
	}
	row.GetActivity().GetResponse().Interim = f.interim[id]
}

// countingFrom answers whether a counting row of TURN sorts at or after index
// AT, other than SKIP.
func (f *feedState) countingFrom(at int, turn, skip string) bool {
	for i := at; i < len(f.order); i++ {
		if id := f.order[i]; id != skip && countsAfter(f.rows[id], turn) {
			return true
		}
	}
	return false
}

// proseBefore answers the nearest response row sorting before index AT when it
// is prose of TURN, or "".
func (f *feedState) proseBefore(at int, turn string) string {
	earlier := f.responseBefore(at)
	if earlier == "" {
		return ""
	}
	row := f.rows[earlier]
	if !isProseRow(row) || row.GetTurn().GetValue() != turn {
		return ""
	}
	return earlier
}

// interimOnPlace records what placing row ID (already in the order, not yet
// stored) changes, and answers the EARLIER prose row it proved interim, which
// the caller re-pushes after ID's own publication; "" when none. When ID is
// itself prose with a counting row of its turn already after it (a history
// page landing above live rows), SNAPSHOT is stamped interim here.
func (r *resolver) interimOnPlace(s *wsState, f *feedState, id string, snapshot *frontendv1.FeedRow) string {
	at := f.indexOf(id)
	log := r.logger(s.id)
	if at < 0 {
		log.Error("daemon.feed.interim_unplaced",
			"a row placed in the feed order could not be found in it; no response was proved interim",
			dlog.Context{"feed": f.key, "row": id})
		return ""
	}
	turn := snapshot.GetTurn().GetValue()
	if isProseRow(snapshot) && f.countingFrom(at+1, turn, id) {
		f.interim[id] = true
		snapshot.GetActivity().GetResponse().Interim = true
		log.Debug("daemon.feed.response_placed_interim",
			"a response was placed above a later row of its turn and is interim from its first draw",
			dlog.Context{"feed": f.key, "row": id})
	}
	if !countsAfter(snapshot, turn) {
		return ""
	}
	earlier := f.proseBefore(at, turn)
	if earlier == "" || f.interim[earlier] {
		return ""
	}
	f.interim[earlier] = true
	log.Info("daemon.feed.response_interim",
		"a later row of the turn landed after a response; the response is interim and is re-pushed",
		dlog.Context{"feed": f.key, "row": earlier, "by": id})
	return earlier
}

// interimOnRetire records what retiring row ID of TURN, which stood at index AT
// (already out of the order), changes, and answers the prose row that is its
// turn's latest again, which the caller re-pushes; "" when none.
func (r *resolver) interimOnRetire(s *wsState, f *feedState, id, turn string, at int) string {
	if f.countingFrom(at, turn, "") {
		return ""
	}
	earlier := f.proseBefore(at, turn)
	if earlier == "" || !f.interim[earlier] {
		return ""
	}
	delete(f.interim, earlier)
	r.logger(s.id).Info("daemon.feed.response_not_interim",
		"the only row of the turn after a response was retired; it may be the answer again and is re-pushed",
		dlog.Context{"feed": f.key, "row": earlier, "retired": id})
	return earlier
}

// republishInterim re-pushes the stored row ID with its flag restated from the
// feed's record; `upsert` stamps it.
func (r *resolver) republishInterim(s *wsState, addr feedid.Feed, id string) {
	f := r.feed(s, addr)
	existing, ok := f.rows[id]
	if !ok {
		r.logger(s.id).Error("daemon.feed.interim_row_missing",
			"a response whose interim flag changed is not stored in its feed; it was not re-pushed",
			dlog.Context{"feed": f.key, "row": id})
		return
	}
	r.restateRow(s, placement{feed: addr}, existing, !f.nonDurable[id], unclonable{
		operation: "daemon.feed.interim_row_unclonable",
		message:   "a response whose interim flag changed could not be cloned; it was not re-pushed",
		context:   dlog.Context{"feed": f.key, "row": id},
	}, func(*frontendv1.FeedRow) {})
}
