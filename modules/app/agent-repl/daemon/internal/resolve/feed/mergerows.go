package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// THE FOOTER'S ⇄ CHIP IS READ OFF THE MERGE BUBBLES THIS FEED DRAWS
// (frontend.v1.FooterExpandedMerges). Every merge bubble row is told to
// Deps.MergeRow on the one path every row is published by (upsert), a restored
// durable row's included, and to Deps.MergeRowRetired as it is retired, so the
// footer holds exactly the bubbles the feed holds. What the rows MEAN — which
// merges are in flight, what step each is on — is the footer's to read; this
// file only says which merge each row belongs to.

// mergeRowLease answers the merge ledger ROW belongs to on the feed ADDR, false
// when ROW is no merge bubble row: a merge head on the root feed is keyed by
// its ledger id, and a tab row lives on its merge's own sub-feed.
func (r *resolver) mergeRowLease(s *wsState, addr feedid.Feed, row *frontendv1.FeedRow) (ids.LeaseID, bool) {
	if row.GetMergeTab() != nil {
		if addr.Merge == nil {
			r.logger(s.id).Error("daemon.feed.merge_tab_off_merge_feed",
				"a merge tab row stands on a feed that is no merge's; the footer is not told of it",
				dlog.Context{"row": row.GetId().GetValue(), "feed": r.feedKey(s.id, addr)})
			return "", false
		}
		return *addr.Merge, true
	}
	if _, ok := mergeHeadOf(row); !ok {
		return "", false
	}
	ref, err := r.deps.Decode(row.GetId())
	if err != nil || ref.Row.Kind != feedid.KindMergeHead {
		cause := "the row is not keyed as a merge head"
		if err != nil {
			cause = err.Error()
		}
		r.logger(s.id).Error("daemon.feed.merge_head_unkeyed",
			"a merge head's id names no merge ledger; the footer is not told of it",
			dlog.Context{"row": row.GetId().GetValue(), "cause": cause})
		return "", false
	}
	return ids.LeaseID(ref.Row.ID), true
}

// announceMergeRow tells Deps.MergeRow of ROW, as it now stands on ADDR, when
// it is a merge bubble row. The caller holds the lock.
func (r *resolver) announceMergeRow(s *wsState, addr feedid.Feed, row *frontendv1.FeedRow) {
	if r.deps.MergeRow == nil {
		return
	}
	if lease, ok := r.mergeRowLease(s, addr, row); ok {
		r.deps.MergeRow(s.id, lease, row)
	}
}

// announceMergeRowRetired tells Deps.MergeRowRetired of ROW, about to be
// retired from ADDR, when it is a merge bubble row. The caller holds the lock.
func (r *resolver) announceMergeRowRetired(s *wsState, addr feedid.Feed, row *frontendv1.FeedRow) {
	if r.deps.MergeRowRetired == nil {
		return
	}
	if lease, ok := r.mergeRowLease(s, addr, row); ok {
		r.deps.MergeRowRetired(s.id, lease, row.GetId().GetValue())
	}
}
