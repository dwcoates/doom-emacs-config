package workspace

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/noticeloop"
	"agent-repld/internal/wsm"
)

// StandingCloses judges standing close requests again
// (agentrepl.v1.CloseWorkspaceRequest.stand) on every edge that can clear what
// blocks one, and closes the workspace once nothing does. NOTHING POLLS: each
// edge reports a workspace, or every request, and one loop judges them in
// order, so no two judgments of one workspace ever race.
//
// The edges, and what each judges:
//   - a turn of the workspace ending, observed or unobserved (wsedges);
//   - the workspace falling free, its last turn or detached item ended
//     (FellFree, from the session watcher's freeness edge);
//   - its held prompts changing, one sent or dropped (HoldsChanged, from the
//     prompt queue's holds sink);
//   - its merge landing or leaving the queue unlanded (wsedges);
//   - a request just registered (Reconsider, from the verbs), so a blocker
//     that cleared between its judgment and its write is not missed;
//   - EVERY request on any workspace closing, killed or nuked, or retargeted
//     (wsedges): open descendants and workspaces landing into a workspace
//     block its close, and judging the few standing requests is safer than
//     walking whose family the edge changed.
//
// It implements wsedges.Observer; every method only appends.
type StandingCloses struct {
	loop *noticeloop.Loop[closeTarget]
	one  func(ctx context.Context, ws ids.WorkspaceID) error
	all  func(ctx context.Context) error
	log  dlog.Logger
}

// closeTarget is one judgment owed: one workspace's request, or every
// request when ws is empty.
type closeTarget struct{ ws ids.WorkspaceID }

// NewStandingCloses builds the loop over ONE, the verbs'
// ReconsiderCloseRequest, and ALL, their ReconsiderCloseRequests.
func NewStandingCloses(one func(ctx context.Context, ws ids.WorkspaceID) error, all func(ctx context.Context) error, log dlog.Logger) *StandingCloses {
	s := &StandingCloses{one: one, all: all, log: log}
	s.loop = noticeloop.New(func(target closeTarget) {
		log.Info(opStandingClose, "an edge arrived after the standing closes stopped, as the daemon stood down; the next daemon judges the request", dlog.Context{"workspace": string(target.ws)})
	})
	return s
}

// Reconsider owes WS's request a judgment. It implements CloseRequestReporter.
func (s *StandingCloses) Reconsider(ws ids.WorkspaceID) { s.loop.Report(closeTarget{ws: ws}) }

// ReconsiderAll owes every standing request a judgment.
func (s *StandingCloses) ReconsiderAll() { s.loop.Report(closeTarget{}) }

// FellFree is the session watcher's freeness edge: WS's last turn or detached
// item ended.
func (s *StandingCloses) FellFree(ws ids.WorkspaceID) { s.Reconsider(ws) }

// HoldsChanged is the prompt queue stating WS's held prompts anew: one may
// have been sent or dropped.
func (s *StandingCloses) HoldsChanged(ws ids.WorkspaceID) { s.Reconsider(ws) }

// TurnStarted implements wsedges.Observer: a turn starting clears nothing.
func (s *StandingCloses) TurnStarted(ids.WorkspaceID) {}

// TurnEnded implements wsedges.Observer.
func (s *StandingCloses) TurnEnded(ws ids.WorkspaceID) { s.Reconsider(ws) }

// TurnEndedUnobserved implements wsedges.Observer: turns that ended while no
// daemon watched clear their blocker as one that ended watched does.
func (s *StandingCloses) TurnEndedUnobserved(ws ids.WorkspaceID) { s.Reconsider(ws) }

// NeedsInput implements wsedges.Observer: an ask clears nothing.
func (s *StandingCloses) NeedsInput(ids.WorkspaceID) {}

// Landed implements wsedges.Observer: the merge left the queue.
func (s *StandingCloses) Landed(ws ids.WorkspaceID) { s.Reconsider(ws) }

// Closed implements wsedges.Observer: a closed, killed or nuked workspace
// leaves its ancestors' and its landing target's families.
func (s *StandingCloses) Closed(wsm.Workspace, wsm.CloseHow) { s.ReconsiderAll() }

// Retargeted implements wsedges.Observer: a retarget may leave a family.
func (s *StandingCloses) Retargeted(wsm.ParentChange) { s.ReconsiderAll() }

// PullRequestOwned implements wsedges.Observer: a pull request clears nothing.
func (s *StandingCloses) PullRequestOwned(ids.WorkspaceID, wsm.PullRequestKey) {}

// Deployed implements wsedges.Observer: the merge left the queue when it
// landed, which Landed judged.
func (s *StandingCloses) Deployed(ids.WorkspaceID, string) {}

// Evicted implements wsedges.Observer: the merge left the queue unlanded.
func (s *StandingCloses) Evicted(ws ids.WorkspaceID, _ string) { s.Reconsider(ws) }

// ColdGated implements wsedges.Observer: a cold gate never blocks a close.
func (s *StandingCloses) ColdGated(ids.WorkspaceID) {}

// Run judges every owed request, in order, until ctx ends, then judges every
// one still pending before it returns. A judgment that failed is recorded at
// ERROR; the request keeps standing for the next edge.
func (s *StandingCloses) Run(ctx context.Context) {
	s.loop.Run(ctx, func(ctx context.Context, target closeTarget) {
		var err error
		if target.ws == "" {
			err = s.all(ctx)
		} else {
			err = s.one(ctx, target.ws)
		}
		if err != nil {
			s.log.Error(opStandingClose, "could not judge a standing close request; it keeps standing for the next edge", dlog.WithCause(dlog.Context{"workspace": string(target.ws)}, err))
		}
	})
}
