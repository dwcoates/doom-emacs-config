package feed

import (
	"context"
	"fmt"
	"strconv"
	"strings"

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
		if err := reserveOrder(r.feed(s, ref.Feed), stored.OrderKey); err != nil {
			fields["cause"] = err.Error()
			log.Error(opDurable, "a durable row's order key cannot be read and the row is drawn nowhere", fields)
			continue
		}
		rank := rowRank{plane: rowPlane(stored.Plane), key: stored.OrderKey}
		// A restored merge head is repainted with the daemon's default unless
		// the reader folded it (mergefold.go).
		r.resettleRestoredMergeFold(s, row)
		r.upsert(s, placement{feed: ref.Feed, inherit: &rank}, row, true)
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

// reserveOrder advances a feed's key counters past a key restored into it, so
// a row this daemon mints later can never take a restored row's key or sort
// above it: the counters live only in memory, and a new daemon's start at zero.
func reserveOrder(f *feedState, key string) error {
	if i := strings.IndexByte(key, followSep); i >= 0 {
		n, err := strconv.ParseUint(key[i+1:], 16, 32)
		if err != nil {
			return fmt.Errorf("the order key %q has no follow count: %w", key, err)
		}
		if base := key[:i]; uint64(f.followers[base]) < n {
			f.followers[base] = uint32(n)
		}
		return nil
	}
	const subWidth = 8
	if len(key) <= subWidth {
		return fmt.Errorf("the order key %q is too short to be an entry key", key)
	}
	base := key[:len(key)-subWidth]
	sub, err := strconv.ParseUint(key[len(key)-subWidth:], 16, 32)
	if err != nil {
		return fmt.Errorf("the order key %q has no entry index: %w", key, err)
	}
	if uint64(f.entryRows[base]) <= sub {
		f.entryRows[base] = uint32(sub) + 1
	}
	return nil
}
