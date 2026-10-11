package feed

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// Publication is one entry of a feed's publication log: what a tail delivers,
// in the order the feed published it. EXACTLY ONE field is set.
//
// A ROW AND A HELD-SECTION CHANGE SHARE ONE LOG because a held item settling
// into a row is ONE change of the root feed (feed_held.proto): carried on one
// log under one watch-token pin, a reader can never apply the row without the
// section it left, or the section without the row it became.
type Publication struct {
	// Row is a row upsert or a row removal.
	Row *frontendv1.FeedRow
	// Held is a change of the root feed's held section, with the rows that
	// moved between the section and the feed in the same change. Only the
	// root feed publishes one.
	Held *agentreplv1.WatchFeedHeldChange
}

// loggedPublication is one publication and the sequence it took.
type loggedPublication struct {
	seq uint64
	pub Publication
}

// appendLog files one publication in the feed's retained log under the next
// sequence, trimming the log to its retention. It is the ONE place a sequence
// is taken, so a watch token pinned to a sequence names exactly the
// publications that followed it.
func appendLog(f *feedState, pub Publication) {
	f.seq++
	f.log = append(f.log, &loggedPublication{seq: f.seq, pub: pub})
	if len(f.log) > f.retention {
		f.log = f.log[len(f.log)-f.retention:]
	}
}

// emit files one publication in the feed's log and hands it to the feed's
// tails (publish). Every publication of every feed leaves by here or by
// emptyFeed's bulk removal.
func (r *resolver) emit(s *wsState, f *feedState, pub Publication) {
	appendLog(f, pub)
	r.publish(s, f, pub)
}
