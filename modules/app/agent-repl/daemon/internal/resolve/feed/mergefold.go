package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// A MERGE BUBBLE IS OPEN BY DEFAULT AND NEVER COLLAPSES AUTOMATICALLY (owner
// ruling, 2026-10-08). Its head row is durable, so the fold it carries is what
// every push, page, reload and restart draws, and THIS FILE IS THE ONE PLACE
// THAT FOLD IS DECIDED. The merge orchestrator composes a head with no fold;
// the resolver settles it here on every publication (settleMergeFold) and on
// every restore of a durable row (resettleRestoredMergeFold):
//
//   - THE READER'S FOLD WINS (SetMergeFold): the webapp tells the daemon each
//     fold or unfold the reader makes, the stored row is restated with it under
//     the `reader` arm, and no later push, restore or default overrides it.
//   - OTHERWISE THE DAEMON'S DEFAULT (mergeFoldDefault): open, with its one
//     documented exception, a merge that ended in success.

// mergeFoldDefault is THE DAEMON'S DEFAULT FOLD for a merge in MERGE's state,
// and the single site that decides it.
//
// THE ONE EXCEPTION TO "A MERGE BUBBLE NEVER COLLAPSES AUTOMATICALLY" (owner
// ruling, 2026-10-08): a merge that ended in SUCCESS is drawn folded. It holds
// both when the success lands live and when the row is restored or repainted
// from durable history after a reload or a daemon restart, because both paths
// come through here. Every other state (queued, running, waiting on the user,
// in conflicts, failed, abandoned) is drawn open, and nothing folds it but
// the reader.
func mergeFoldDefault(merge *frontendv1.FeedMerge) *frontendv1.FeedMergeFold {
	return &frontendv1.FeedMergeFold{
		Folded:    merge.GetSuccess() != nil,
		DecidedBy: &frontendv1.FeedMergeFold_Daemon{Daemon: &frontendv1.FeedMergeFoldByDaemon{}},
	}
}

// readerFoldOf is the reader's own FOLDED, under the `reader` arm.
func readerFoldOf(folded bool) *frontendv1.FeedMergeFold {
	return &frontendv1.FeedMergeFold{
		Folded:    folded,
		DecidedBy: &frontendv1.FeedMergeFold_Reader{Reader: &frontendv1.FeedMergeFoldByReader{}},
	}
}

// mergeHeadOf answers ROW's merge, false when ROW is not a merge bubble's head.
func mergeHeadOf(row *frontendv1.FeedRow) (*frontendv1.FeedMerge, bool) {
	merge := row.GetActivity().GetMerge()
	if merge == nil || merge.GetHead() == nil {
		return nil, false
	}
	return merge, true
}

// readerFold answers the reader's recorded fold on ROW, false when ROW is not
// a merge head or carries no fold the reader made.
func readerFold(row *frontendv1.FeedRow) (*frontendv1.FeedMergeFold, bool) {
	merge, ok := mergeHeadOf(row)
	if !ok {
		return nil, false
	}
	fold := merge.GetHead().GetFold()
	if fold.GetReader() == nil {
		return nil, false
	}
	return fold, true
}

// settleMergeFold settles the fold of an incoming merge head ROW before it is
// published: the reader's fold on the stored row when there is one, the
// daemon's default otherwise. The caller holds the lock.
func (r *resolver) settleMergeFold(s *wsState, feed feedid.Feed, row *frontendv1.FeedRow) {
	merge, ok := mergeHeadOf(row)
	if !ok {
		return
	}
	if stored, ok := r.feed(s, feed).rows[row.GetId().GetValue()]; ok {
		if fold, ok := readerFold(stored); ok {
			merge.GetHead().Fold = readerFoldOf(fold.GetFolded())
			r.logger(s.id).Debug("daemon.feed.merge_fold_reader_kept",
				"a push carried the reader's recorded fold on a merge bubble over the daemon's default",
				dlog.Context{"row": row.GetId().GetValue(), "folded": fold.GetFolded()})
			return
		}
	}
	merge.GetHead().Fold = mergeFoldDefault(merge)
}

// resettleRestoredMergeFold settles the fold of a merge head ROW read back
// from the durable record: the reader's recorded fold stands, and any other
// (the daemon's, or none on a row recorded before the fold said who decided
// it) is repainted with the daemon's default for the merge's state.
func (r *resolver) resettleRestoredMergeFold(s *wsState, row *frontendv1.FeedRow) {
	merge, ok := mergeHeadOf(row)
	if !ok {
		return
	}
	if _, ok := readerFold(row); ok {
		return
	}
	was := merge.GetHead().GetFold()
	merge.GetHead().Fold = mergeFoldDefault(merge)
	if was.GetDaemon() == nil || was.GetFolded() != merge.GetHead().GetFold().GetFolded() {
		r.logger(s.id).Debug("daemon.feed.merge_fold_restored",
			"a restored merge bubble's fold was repainted with the daemon's default for its state",
			dlog.Context{"row": row.GetId().GetValue(), "folded": merge.GetHead().GetFold().GetFolded(),
				"recorded_by_daemon": was.GetDaemon() != nil})
	}
}

// SetMergeFold records the reader's fold on a merge bubble's head row in the
// workspace's root feed and restates the row with it. False is a row that is
// not a merge head this daemon holds.
func (r *resolver) SetMergeFold(ws ids.WorkspaceID, id *frontendv1.FeedId, folded bool) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return false
	}
	root := feedid.Feed{Root: true}
	f, ok := s.feeds[r.feedKey(ws, root)]
	if !ok {
		return false
	}
	stored, ok := f.rows[id.GetValue()]
	if !ok {
		return false
	}
	if _, ok := mergeHeadOf(stored); !ok {
		return false
	}
	restated := r.restateRow(s, placement{feed: root}, stored, !f.nonDurable[id.GetValue()], unclonable{
		operation: "daemon.feed.merge_fold_unclonable",
		message:   "a merge bubble's head could not be cloned; the reader's fold was not recorded",
		context:   dlog.Context{"row": id.GetValue()},
	}, func(row *frontendv1.FeedRow) {
		row.GetActivity().GetMerge().GetHead().Fold = readerFoldOf(folded)
	})
	if !restated {
		return false
	}
	if !f.nonDurable[id.GetValue()] {
		r.recordDurable(s, root, id.GetValue())
	}
	r.logger(ws).Info("daemon.feed.merge_fold",
		"recorded the reader's fold on a merge bubble", dlog.Context{"row": id.GetValue(), "folded": folded})
	return true
}
