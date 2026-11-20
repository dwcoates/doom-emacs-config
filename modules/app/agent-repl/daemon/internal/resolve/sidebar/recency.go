package sidebar

import (
	"errors"
	"maps"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// ErrNoEmacsStream is a recency reach reported with no Emacs WatchDaemon
// stream standing: there is no connection for it to belong to.
var ErrNoEmacsStream = errors.New("no Emacs WatchDaemon stream stands, so no recency reach is held")

// RecencyReach is the connected Emacs's recency reach: the gutter each
// workspace's open row carries (frontend.v1.RosterOpenRow.recency_gutter).
// A workspace absent has none.
type RecencyReach map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter

// equal reports whether R and OTHER hold the same gutters.
func (r RecencyReach) equal(other RecencyReach) bool {
	return maps.EqualFunc(r, other, func(a, b *frontendv1.RosterOpenRowRecencyGutter) bool {
		return proto.Equal(a, b)
	})
}

// AttachRecencyReach stands a stream's empty reach; see Resolver.
func (r *resolver) AttachRecencyReach() (release func()) {
	var generation uint64
	r.mutate("daemon.sidebar.attach_recency_reach", "an Emacs stream attached its recency reach, empty until its first report",
		dlog.Context{}, r.log.Global(), func() {
			var superseded bool
			generation, superseded = r.state.recency.Attach(RecencyReach{})
			r.log.Global().Info("daemon.sidebar.attach_recency_reach", "the recency reach's stream generation",
				dlog.Context{"generation": generation, "superseded": superseded})
		})
	return func() { r.releaseRecencyReach(generation) }
}

// releaseRecencyReach ends one stream's reach, unless a later stream already
// superseded it, in which case nothing is drawn differently.
func (r *resolver) releaseRecencyReach(generation uint64) {
	r.mutateIf("daemon.sidebar.release_recency_reach", "the Emacs stream ended; no row carries a recency gutter",
		dlog.Context{"generation": generation}, r.log.Global(), func() bool {
			if !r.state.recency.Release(generation) {
				r.log.Global().Debug("daemon.sidebar.release_recency_reach",
					"a superseded Emacs stream ended; the current stream's recency reach stands",
					dlog.Context{"generation": generation})
				return false
			}
			return true
		})
}

// ReportRecencyReach replaces the standing stream's reach; see Resolver.
func (r *resolver) ReportRecencyReach(reach RecencyReach) error {
	if reach == nil {
		panic("sidebar: ReportRecencyReach was handed no reach; an empty report is an empty map")
	}
	attached := false
	r.mutateIf("daemon.sidebar.report_recency_reach", "the roster took Emacs's recency reach",
		dlog.Context{"reaches": len(reach)}, r.log.Global(), func() bool {
			held, standing := r.state.recency.Get()
			if !standing {
				return false
			}
			attached = true
			if held.equal(reach) {
				return false
			}
			r.state.recency.Update(maps.Clone(reach))
			return true
		})
	if !attached {
		return ErrNoEmacsStream
	}
	return nil
}

// recencyGutter answers WS's gutter as the standing reach holds it, nil when
// it holds none. Caller holds mu.
func (r *resolver) recencyGutter(ws ids.WorkspaceID) *frontendv1.RosterOpenRowRecencyGutter {
	held, ok := r.state.recency.Get()
	if !ok {
		return nil
	}
	gutter, ok := held[ws]
	if !ok {
		return nil
	}
	return proto.Clone(gutter).(*frontendv1.RosterOpenRowRecencyGutter)
}
