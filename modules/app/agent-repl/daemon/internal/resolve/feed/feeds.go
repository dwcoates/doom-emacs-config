package feed

import (
	"sort"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
)

// sortedFeedKeys answers the workspace's feed keys in order, so a sweep over
// every feed publishes in the same order on every run.
func sortedFeedKeys(s *wsState) []string {
	keys := make([]string, 0, len(s.feeds))
	for key := range s.feeds {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	return keys
}

// eachStoredRow visits every row the workspace's feeds hold: feed by feed in
// sortedFeedKeys order, each feed's rows in their feed order. The order is a
// snapshot taken before the feed's first visit, so VISIT may retire or
// restate the row it is handed (or any other) without the walk skipping or
// repeating one.
func (r *resolver) eachStoredRow(s *wsState, visit func(addr feedid.Feed, f *feedState, id string, row *frontendv1.FeedRow)) {
	for _, key := range sortedFeedKeys(s) {
		f := s.feeds[key]
		addr := s.feedAddrs[key]
		for _, id := range append([]string(nil), f.order...) {
			row, ok := f.rows[id]
			if !ok {
				continue
			}
			visit(addr, f, id, row)
		}
	}
}
