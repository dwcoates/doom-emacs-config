package feed

import (
	"context"

	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The operation this file's records carry.
const opDurable = "daemon.feed.durable"

// recordDurable writes one durable row, as just published and at its order key,
// through to Deps.DurableRows. The caller holds the lock.
//
// A FAILED RECORD IS AN ERROR, AND THE ROW STANDS: it is drawn for every reader
// of this daemon, and only a new daemon would miss it.
func (r *resolver) recordDurable(s *wsState, feed feedid.Feed, id string) {
	if r.deps.DurableRows == nil {
		return
	}
	f := r.feed(s, feed)
	published, rank := f.rows[id], f.rank[id]
	fields := dlog.Context{"feed": f.key, "row": id, "key": rank.key}
	encoded, err := proto.Marshal(published)
	if err != nil {
		fields["cause"] = err.Error()
		r.logger(s.id).Error(opDurable, "a durable row could not be encoded; a new daemon will not draw it", fields)
		return
	}
	if err := r.deps.DurableRows.PutDurableFeedRow(context.Background(), wsm.DurableFeedRow{
		Workspace: s.id, RowID: id, Plane: int(rank.plane), OrderKey: rank.key, Row: encoded,
	}); err != nil {
		fields["cause"] = err.Error()
		r.logger(s.id).Error(opDurable, "a durable row could not be recorded; a new daemon will not draw it", fields)
	}
}

// restoreDurable draws a workspace's recorded durable rows again, each on its
// own feed and at the order key it was first drawn at, so the rows stand where
// they stood before this daemon. It runs once, as the workspace's feed state is
// born. The caller holds the lock.
//
// A ROW THAT CANNOT BE READ BACK IS AN ERROR and is drawn nowhere; the others
// are still drawn.
func (r *resolver) restoreDurable(s *wsState) {
	if r.deps.DurableRows == nil {
		return
	}
	log := r.logger(s.id)
	rows, err := r.deps.DurableRows.DurableFeedRows(context.Background(), s.id)
	if err != nil {
		log.Error(opDurable, "the durable rows could not be read; none is drawn again", dlog.Context{"cause": err.Error()})
		return
	}
	drawn := 0
	for _, stored := range rows {
		fields := dlog.Context{"row": stored.RowID, "key": stored.OrderKey}
		row := &frontendv1.FeedRow{}
		if err := proto.Unmarshal(stored.Row, row); err != nil {
			fields["cause"] = err.Error()
			log.Error(opDurable, "a durable row could not be decoded and is drawn nowhere", fields)
			continue
		}
		ref, err := r.deps.Decode(row.GetId())
		if err != nil {
			fields["cause"] = err.Error()
			log.Error(opDurable, "a durable row's id names no feed and is drawn nowhere", fields)
			continue
		}
		rank := rowRank{plane: rowPlane(stored.Plane), key: stored.OrderKey}
		r.upsertOne(s, placement{feed: ref.Feed, inherit: &rank}, row, true)
		drawn++
	}
	if drawn > 0 {
		log.Info(opDurable, "drew this workspace's durable rows again, each where it stood", dlog.Context{"rows": drawn})
	}
}

// forgetDurable drops a workspace's recorded durable rows, as a reset drops
// the rows themselves. The caller holds the lock.
func (r *resolver) forgetDurable(ws ids.WorkspaceID) {
	if r.deps.DurableRows == nil {
		return
	}
	if err := r.deps.DurableRows.ClearDurableFeedRows(context.Background(), ws); err != nil {
		r.logger(ws).Error(opDurable, "the durable rows could not be forgotten; a new daemon would draw the reset conversation's rows again",
			dlog.Context{"cause": err.Error()})
	}
}
