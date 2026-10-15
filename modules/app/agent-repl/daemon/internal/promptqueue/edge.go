package promptqueue

import (
	"errors"

	"agent-repld/internal/bounce"
)

// freenessEdge is what occasioned a registry check: the event that might have
// left the workspace free for a registered bounce.
type freenessEdge int

const (
	// edgeWork is the workspace's own work changing: a turn's end, the last
	// detached item's end, the awaited shim's departure.
	edgeWork freenessEdge = iota
	// edgeLease is the workspace's lease set changing: a merge's release, a
	// handover's, or a bounce's own restart hold.
	edgeLease
)

// edgeSet is the edges a draining bounce swallowed.
type edgeSet struct {
	work  bool
	lease bool
}

// add records one swallowed edge.
func (s *edgeSet) add(edge freenessEdge) {
	switch edge {
	case edgeWork:
		s.work = true
	case edgeLease:
		s.lease = true
	default:
		panic("promptqueue: an unknown freeness edge was swallowed")
	}
}

// rejudges reports whether the swallowed edges re-judge a replacement its run
// deferred with RUNERR: only an edge that can END WHAT IT WAS DEFERRED BEHIND
// does (bounce.ErrDeferredBehindLease). A lease release ends a deferral behind
// another holder's lease. Nothing but the work's own end ends a shim's `live`
// refusal, and the run's own restart hold is released as it gives the bounce
// back, so a lease edge there is the run itself, never news.
func (s edgeSet) rejudges(runErr error) bool {
	if errors.Is(runErr, bounce.ErrDeferredBehindLease) {
		return s.work || s.lease
	}
	return s.work
}
