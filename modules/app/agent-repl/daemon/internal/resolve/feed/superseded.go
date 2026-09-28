package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// A THINKING BUBBLE IS SUPERSEDED ONCE A LATER AGENT RESPONSE LANDS IN ITS FEED
// (owner rule, 2026-09-23). While it is the feed's latest agent response the
// client draws it in full, at the ordinary response bubble's collapsed limit;
// once the next response bubble arrives — another thinking bubble, mid-turn
// prose or a final answer — it collapses to its two-line form. Tool calls,
// prompts and every other row kind never count as the next response.
//
// IT IS A FACT ABOUT THE FEED'S OWN ORDER, and this is the one place it is
// decided. A thinking row is superseded exactly when some response row sorts
// AFTER it in the same feed, so the flag is a pure function of the feed's
// order and rows — which is what makes live, replay and pages agree: pages
// serve the stored rows, and every row reaches the store through `upsert`,
// which stamps the flag from the feed's record on EVERY draw (drawThinking
// composes a fresh bubble on each fragment and never has to remember it).
//
// THE RECORD IS KEPT ONLY AT STRUCTURAL EDGES. A row's rank is fixed at its
// first draw, so the answer can change only when a response row is PLACED or
// RETIRED. The invariant those two edges keep is that every response row but
// the feed's LAST is superseded when it is thinking, so an edge only ever has
// to look at its nearest response neighbours:
//
//   - PLACED at index i: the new row is itself superseded when a response row
//     already stands after it (a history page landing above live rows), and the
//     nearest response BEFORE it — the only one that could have been the
//     feed's latest — is superseded now, and re-pushed so an open feed collapses
//     it live.
//   - RETIRED from index i: when no response row stands after it any more, the
//     nearest response before it is the feed's latest again, and is re-pushed
//     un-superseded.
//
// SCOPE IS ONE FEED. The record lives on the feedState, so a subagent's
// sub-feed follows the same rule within itself and a row on one feed never
// supersedes a row on another.

// isResponseRow reports whether ROW is an agent response bubble, thinking or
// not — the only row kind that supersedes a thinking bubble.
func isResponseRow(row *frontendv1.FeedRow) bool {
	return row.GetActivity().GetResponse() != nil
}

// isThinkingRow reports whether ROW is a thinking bubble — the only row kind
// that can BE superseded.
func isThinkingRow(row *frontendv1.FeedRow) bool {
	return row.GetActivity().GetResponse().GetThinking()
}

// stampSuperseded states a thinking row's flag from the feed's record. It runs
// on the one write path (`upsert`), before the unchanged-row comparison, so a
// redraw of an already-superseded thinking fold restates the flag it carries
// rather than un-collapsing the bubble. A non-thinking row is never stamped.
func stampSuperseded(f *feedState, id string, row *frontendv1.FeedRow) {
	if !isThinkingRow(row) {
		return
	}
	row.GetActivity().GetResponse().Superseded = f.superseded[id]
}

// responseBefore answers the nearest response row sorting before index AT, or
// "" when none does.
func (f *feedState) responseBefore(at int) string {
	for i := at - 1; i >= 0; i-- {
		if isResponseRow(f.rows[f.order[i]]) {
			return f.order[i]
		}
	}
	return ""
}

// responseFrom answers whether any response row sorts at or after index AT,
// other than SKIP (the row being placed, not yet in `rows`).
func (f *feedState) responseFrom(at int, skip string) bool {
	for i := at; i < len(f.order); i++ {
		if f.order[i] != skip && isResponseRow(f.rows[f.order[i]]) {
			return true
		}
	}
	return false
}

// indexOf answers ID's index in the feed order. The scan is backward because
// the ordinary placement is a live row after every row already drawn.
func (f *feedState) indexOf(id string) int {
	for i := len(f.order) - 1; i >= 0; i-- {
		if f.order[i] == id {
			return i
		}
	}
	return -1
}

// supersedeOnPlace records what placing the response row ID (already inserted
// into the order, not yet stored) changes, and answers the EARLIER thinking row
// it superseded, which the caller re-pushes once ID's own publication is done;
// "" when it superseded none. When ID is itself thinking and a response row
// already stands after it, it is placed superseded: SNAPSHOT is stamped here.
func (r *resolver) supersedeOnPlace(s *wsState, f *feedState, id string, snapshot *frontendv1.FeedRow) string {
	at := f.indexOf(id)
	log := r.logger(s.id)
	if at < 0 {
		log.Error("daemon.feed.superseded_unplaced",
			"a response row placed in the feed order could not be found in it; no thinking row was superseded",
			dlog.Context{"feed": f.key, "row": id})
		return ""
	}
	if isThinkingRow(snapshot) && f.responseFrom(at+1, id) {
		f.superseded[id] = true
		snapshot.GetActivity().GetResponse().Superseded = true
		log.Debug("daemon.feed.thinking_placed_superseded",
			"a thinking row was placed above a later response in its feed and is superseded from its first draw",
			dlog.Context{"feed": f.key, "row": id})
	}
	earlier := f.responseBefore(at)
	if earlier == "" || f.superseded[earlier] || !isThinkingRow(f.rows[earlier]) {
		return ""
	}
	f.superseded[earlier] = true
	log.Info("daemon.feed.thinking_superseded",
		"a later response landed in the feed; the thinking row before it is superseded and re-pushed",
		dlog.Context{"feed": f.key, "row": earlier, "by": id})
	return earlier
}

// unsupersedeOnRetire records what retiring the response row that stood at
// index AT changes (it is already out of the order), and answers the thinking
// row that is the feed's latest response again, which the caller re-pushes;
// "" when none became so.
func (r *resolver) unsupersedeOnRetire(s *wsState, f *feedState, id string, at int) string {
	if f.responseFrom(at, "") {
		return ""
	}
	earlier := f.responseBefore(at)
	if earlier == "" || !f.superseded[earlier] {
		return ""
	}
	delete(f.superseded, earlier)
	r.logger(s.id).Info("daemon.feed.thinking_unsuperseded",
		"the only response after a thinking row was retired; it is the feed's latest response again and is re-pushed",
		dlog.Context{"feed": f.key, "row": earlier, "retired": id})
	return earlier
}

// republishSuperseded re-pushes the stored row ID with its flag restated from
// the feed's record. The row the fold already composed is reused verbatim, so
// nothing but the flag can change; `upsert` stamps it.
func (r *resolver) republishSuperseded(s *wsState, addr feedid.Feed, id string) {
	f := r.feed(s, addr)
	existing, ok := f.rows[id]
	if !ok {
		r.logger(s.id).Error("daemon.feed.superseded_row_missing",
			"a thinking row whose superseded flag changed is not stored in its feed; it was not re-pushed",
			dlog.Context{"feed": f.key, "row": id})
		return
	}
	r.restateRow(s, placement{feed: addr}, existing, !f.nonDurable[id], unclonable{
		operation: "daemon.feed.superseded_row_unclonable",
		message:   "a thinking row whose superseded flag changed could not be cloned; it was not re-pushed",
		context:   dlog.Context{"feed": f.key, "row": id},
	}, func(*frontendv1.FeedRow) {})
}
