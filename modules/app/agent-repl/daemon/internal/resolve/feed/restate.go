package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"

	"google.golang.org/protobuf/proto"
)

// unclonable names the record a restatement writes when the stored row cannot
// be snapshotted: its operation, its message, and the context that identifies
// the row.
type unclonable struct {
	operation string
	message   string
	context   dlog.Context
}

// restateRow re-pushes a STORED row with EDIT applied to a snapshot of it. The
// stored row is never edited in place: it is what the feed already published,
// and editing it would change a row under a reader and make it compare equal to
// its successor. A row that cannot be snapshotted is recorded through WHY and
// not re-pushed. It reports whether the row was re-pushed.
func (r *resolver) restateRow(s *wsState, at placement, stored *frontendv1.FeedRow, durable bool, why unclonable, edit func(*frontendv1.FeedRow)) bool {
	snapshot, ok := proto.Clone(stored).(*frontendv1.FeedRow)
	if !ok {
		r.logger(s.id).Error(why.operation, why.message, why.context)
		return false
	}
	edit(snapshot)
	r.upsert(s, at, snapshot, durable)
	return true
}
