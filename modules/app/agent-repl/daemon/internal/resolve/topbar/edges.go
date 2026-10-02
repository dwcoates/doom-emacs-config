package topbar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// sourceEdge is one record of where a drawn fact now comes from.
type sourceEdge struct {
	operation string
	message   string
	ctx       dlog.Context
}

// sourceEdges answers the records owed for a PUBLISHED view whose effort
// selector changed standing, and marks them stated. Taken
// under the lock that built VIEW, so a record never disagrees with what was
// published; nothing is owed for a view that was not published.
//
// AT INFO: "why does the selector show this level" is a question a person
// asks, and each standing is recorded once per change rather than on every
// publication.
func sourceEdges(s *wsState, view *frontendv1.TopbarView) []sourceEdge {
	if view == nil {
		return nil
	}
	var edges []sourceEdge
	if standing := effortStanding(s); standing != s.effortStandingLogged {
		s.effortStandingLogged = standing
		edges = append(edges, sourceEdge{
			operation: "daemon.topbar.effort_source",
			message:   "the effort selector's standing changed",
			ctx: dlog.Context{
				"standing":      standing,
				"drawn":         view.GetEffortSelector() != nil,
				"model":         s.model,
				"settings_path": s.effortSettings.Path,
			},
		})
	}
	return edges
}
