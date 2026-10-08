package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// A MERGE BUBBLE NEVER COLLAPSES ON ITS OWN (owner ruling, 2026-10-08). Its
// head row is durable, so the fold it carries is what every push, page,
// reload and restart draws, and this file keeps two rules on it:
//
//   - A PUSH NEVER FOLDS AN OPEN BUBBLE. The merge orchestrator composes each
//     head folded unless the merge failed; an incoming folded head over a
//     stored open one keeps the stored open fold (keepMergeOpen), so the daemon
//     can open a bubble but never close one.
//   - THE READER'S FOLD IS RECORDED (SetMergeFold): the webapp tells the daemon
//     each fold the reader makes, and the stored row is restated with it, so
//     the reader's open or closed bubble is what every later draw carries.

// mergeHeadFold answers the fold a merge bubble's head row carries, false when
// ROW is not a merge head.
func mergeHeadFold(row *frontendv1.FeedRow) (*frontendv1.FeedMergeFold, bool) {
	merge := row.GetActivity().GetMerge()
	if merge == nil || merge.GetHead().GetFold() == nil {
		return nil, false
	}
	return merge.GetHead().GetFold(), true
}

// keepMergeOpen keeps a stored open merge bubble open under an incoming push
// that would fold it. The caller holds the lock.
func (r *resolver) keepMergeOpen(s *wsState, feed feedid.Feed, row *frontendv1.FeedRow) {
	incoming, ok := mergeHeadFold(row)
	if !ok || !incoming.GetFolded() {
		return
	}
	stored, ok := r.feed(s, feed).rows[row.GetId().GetValue()]
	if !ok {
		return
	}
	if fold, ok := mergeHeadFold(stored); ok && !fold.GetFolded() {
		incoming.Folded = false
		r.logger(s.id).Debug("daemon.feed.merge_kept_open",
			"a push would have folded an open merge bubble; it stays open, as only the reader folds one",
			dlog.Context{"row": row.GetId().GetValue()})
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
	if _, ok := mergeHeadFold(stored); !ok {
		return false
	}
	restated := r.restateRow(s, placement{feed: root}, stored, !f.nonDurable[id.GetValue()], unclonable{
		operation: "daemon.feed.merge_fold_unclonable",
		message:   "a merge bubble's head could not be cloned; the reader's fold was not recorded",
		context:   dlog.Context{"row": id.GetValue()},
	}, func(row *frontendv1.FeedRow) {
		row.GetActivity().GetMerge().GetHead().GetFold().Folded = folded
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
