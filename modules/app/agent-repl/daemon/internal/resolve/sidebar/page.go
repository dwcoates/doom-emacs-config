package sidebar

import (
	"time"

	"agentrepl/logging/logsite"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE PAGE VERDICT IS THE PAGE HALF OF WORKSPACE RESOURCE ALLOCATION (owner
// rulings, 2026-11-24; frontend.v1.RosterRowPage): whether the editor holds a
// workspace's page. The roster derives it on every render, from the
// visibility it derived first, so a workspace that is not visible can never
// be `loaded`: pageVerdict reads visibility before anything else. The editor
// loads and unloads the page to match and decides nothing itself.
//
// Three clocks decide a verdict over time, each a product rule rather than a
// bound on an operation. The render that finds the earliest of them arms one
// timer for it (armPageClock), whose firing re-renders, so a verdict flips on
// time with no reader polling.

// PageUnreadHold is how long after its turn ended a workspace whose result is
// unread keeps its page.
const PageUnreadHold = time.Hour

// PageLeaveGrace is how long after the user switched away a workspace keeps
// its page, so a glance away and back costs no reload.
const PageLeaveGrace = 10 * time.Second

// PageFirstViewGrace is how long after the user switched away a workspace
// keeps its page when the visit being left is the one that first marked it
// viewed: the user just read it, and may well come back to it.
const PageFirstViewGrace = 15 * time.Minute

// ShimIdleHold is how long an idle shim stays up after the later of its
// workspace's last turn end and the user's last interaction with it (owner
// ruling, 2026-11-24): a turn that ends at 0:00 and is viewed at 0:05 parks at
// 0:20 unless the user interacts again.
const ShimIdleHold = 15 * time.Minute

// PageAwayGrace is how long after the user switched away, or after it became
// one, a one-shot or a synchronous merge keeps its page (owner ruling,
// 2026-11-24).
const PageAwayGrace = time.Minute

// syncMergeArms are the status arms of a workspace in its own (synchronous)
// merge: queued or running. Such a workspace holds its page only while the
// user looks at it, and for PageAwayGrace after.
var syncMergeArms = map[string]bool{"merge_queued": true, "merging": true}

// syncMergeActiveArm is the arm of a synchronous merge at an active step (a
// rebase, its gate, a repair, a conflict resolution): its shim is held up.
// Waiting in line (merge_queued) its merge parks it.
const syncMergeActiveArm = "merging"

// pageInputs is what one workspace's page verdict is derived from.
type pageInputs struct {
	// visible is the roster's visibility for the workspace this render.
	visible bool
	// selected reports that the user is looking at it.
	selected bool
	// awayUnloaded reports a one-shot or a synchronous merge: its page is
	// held only while selected, and for PageAwayGrace after awayFrom.
	awayUnloaded bool
	// awayFrom is the later of when the user left it and when it became a
	// one-shot or entered its merge, nil when this daemon saw neither.
	awayFrom *time.Time
	// mergeActive reports a synchronous merge at an active step.
	mergeActive bool
	// oneShot reports a one-shot, whose shim keeps running whatever else
	// holds.
	oneShot bool
	// alwaysShown reports a standing mediator or a queue-bypass holder, which
	// always runs.
	alwaysShown bool
	// working reports a working, starting or top-priority workspace.
	working bool
	// unread reports a turn end whose result the user has not read.
	unread bool
	// turnEndedAt is when its last turn ended, nil when none has.
	turnEndedAt *time.Time
	// interactedAt is when the user last interacted with it, nil when never.
	interactedAt *time.Time
	// leftAt is when the user last switched away from it, nil when this
	// daemon never saw them do so.
	leftAt *time.Time
	// leftFirstViewed reports that the visit left at leftAt first marked it
	// viewed.
	leftFirstViewed bool
}

// pageVerdict answers whether the editor holds the page AT NOW for IN, and
// the instant the verdict flips from loaded to unloaded by a clock alone
// (zero when no clock will flip it). The first rule that holds decides.
func pageVerdict(in pageInputs, now time.Time) (bool, time.Time) {
	switch {
	case !in.visible:
		return false, time.Time{}
	case in.selected:
		return true, time.Time{}
	case in.awayUnloaded && in.awayFrom != nil:
		return heldUntil(in.awayFrom.Add(PageAwayGrace), now)
	case in.awayUnloaded:
		return false, time.Time{}
	case in.working:
		return true, time.Time{}
	case in.unread && in.turnEndedAt != nil:
		return heldUntil(in.turnEndedAt.Add(PageUnreadHold), now)
	case in.unread:
		return true, time.Time{}
	case in.leftAt != nil:
		return heldUntil(in.leftAt.Add(leaveGrace(in)), now)
	default:
		return false, time.Time{}
	}
}

// shimHoldOf answers until when IN's resource allocation holds its idle shim
// up AT NOW (wsm.ShimHold), the first rule that holds deciding:
//   - a workspace that is not visible holds none;
//   - the selection, a working workspace, a standing mediator, a queue-bypass
//     holder, a one-shot, and a synchronous merge at an active step hold it
//     for as long as that stands;
//   - a synchronous merge waiting in line holds none (its merge parks it,
//     drain.Controller.MergeQueuedDown);
//   - any other holds it for ShimIdleHold after the later of its last turn's
//     end and the user's last interaction with it.
//
// The selection holds a RUNNING shim up; nothing here starts a parked one. A
// session brought up for a turn not yet closed is held by the cache keeper
// itself, from its own row (wsm.Session.Initializing).
func shimHoldOf(in pageInputs, now time.Time) wsm.ShimHold {
	switch {
	case !in.visible:
		return wsm.ShimHold{}
	case in.selected, in.working, in.alwaysShown, in.oneShot, in.mergeActive:
		return wsm.ShimHold{Indefinite: true}
	case in.awayUnloaded:
		return wsm.ShimHold{}
	}
	idle := laterOf(in.turnEndedAt, in.interactedAt)
	if idle == nil {
		return wsm.ShimHold{}
	}
	if deadline := idle.Add(ShimIdleHold); now.Before(deadline) {
		return wsm.ShimHold{Until: deadline}
	}
	return wsm.ShimHold{}
}

// laterOf answers the later of A and B, either nil when unknown; nil when
// both are.
func laterOf(a, b *time.Time) *time.Time {
	if b != nil && (a == nil || b.After(*a)) {
		return b
	}
	return a
}

// leaveGrace is how long after the user left it IN's workspace keeps its page:
// PageFirstViewGrace when the visit left first viewed it, else
// PageLeaveGrace.
func leaveGrace(in pageInputs) time.Duration {
	if in.leftFirstViewed {
		return PageFirstViewGrace
	}
	return PageLeaveGrace
}

// heldUntil answers a verdict held until DEADLINE: loaded, flipping at
// DEADLINE, while NOW is before it; unloaded after.
func heldUntil(deadline, now time.Time) (bool, time.Time) {
	if now.Before(deadline) {
		return true, deadline
	}
	return false, time.Time{}
}

// pageOf answers LOADED on the wire.
func pageOf(loaded bool) *frontendv1.RosterRowPage {
	if loaded {
		return &frontendv1.RosterRowPage{Page: &frontendv1.RosterRowPage_Loaded{Loaded: &frontendv1.RosterRowPageLoaded{}}}
	}
	return &frontendv1.RosterRowPage{Page: &frontendv1.RosterRowPage_Unloaded{Unloaded: &frontendv1.RosterRowPageUnloaded{}}}
}

// resolvePages derives every registry workspace's page verdict for this
// render from the visibility and the arms it already derived, keeps it for
// the rows, and arms the clock for the earliest verdict a clock will flip.
// The caller holds r.mu.
func (r *resolver) resolvePages(rc rowContext, arms map[ids.WorkspaceID]string) {
	now := r.now()
	pages := make(map[ids.WorkspaceID]bool, len(r.state.reg.Workspaces))
	var next time.Time
	for _, rec := range r.state.reg.Workspaces {
		in := r.pageInputsOf(rec, rc, arms[rec.ID])
		loaded, flips := pageVerdict(in, now)
		pages[rec.ID] = loaded
		if !flips.IsZero() && (next.IsZero() || flips.Before(next)) {
			next = flips
		}
		// EVERY CHANGE OF A VERDICT IS RECORDED with the inputs that decided
		// it, since the editor loads and unloads pages by it alone.
		if was, seen := r.state.page[rec.ID]; seen && was != loaded {
			ctx := dlog.Context{
				"loaded": loaded, "arm": arms[rec.ID], "visible": in.visible, "selected": in.selected,
				"away_unloaded": in.awayUnloaded, "working": in.working, "unread": in.unread,
				"left_first_viewed": in.leftFirstViewed,
			}
			if in.leftAt != nil {
				ctx["left_at"] = in.leftAt.UTC()
			}
			if in.turnEndedAt != nil {
				ctx["turn_ended_at"] = in.turnEndedAt.UTC()
			}
			if !flips.IsZero() {
				ctx["flips_at"] = flips.UTC()
			}
			r.logFor(rec.ID).Info("daemon.sidebar.page_verdict_changed", "the roster derived a new page verdict for the workspace", ctx)
		}
	}
	r.state.page = pages
	r.armPageClock(next, now)
	r.resolveShimHolds(rc, arms, now)
}

// resolveShimHolds derives every registry workspace's shim hold for this
// render, keeps it for ShimHold, and collects every workspace whose hold
// changed since the last render, reported to the shim-hold sink off the lock.
// The caller holds r.mu.
func (r *resolver) resolveShimHolds(rc rowContext, arms map[ids.WorkspaceID]string, now time.Time) {
	holds := make(map[ids.WorkspaceID]wsm.ShimHold, len(r.state.reg.Workspaces))
	for _, rec := range r.state.reg.Workspaces {
		hold := shimHoldOf(r.pageInputsOf(rec, rc, arms[rec.ID]), now)
		holds[rec.ID] = hold
		if was, seen := r.state.shimHold[rec.ID]; !seen || was != hold {
			r.pendingShimHolds = append(r.pendingShimHolds, rec.ID)
		}
	}
	r.state.shimHold = holds
}

// ShimHoldSink is told every workspace whose shim hold changed, once per
// combined commit, off the resolver's lock; it returns at once. The cache
// keeper reconciles the workspace's stand-down by it.
type ShimHoldSink func(ws ids.WorkspaceID)

// WithShimHoldSink installs the sink the resolver tells its shim hold
// changes.
func WithShimHoldSink(sink ShimHoldSink) Option {
	return func(r *resolver) { r.shimHolds = sink }
}

// ShimHold answers workspace WS's shim hold as the last render derived it. A
// workspace no render has derived holds nothing: its idle shim parks.
func (r *resolver) ShimHold(ws ids.WorkspaceID) wsm.ShimHold {
	r.awaitApplied()
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.state.shimHold[ws]
}

// pageInputsOf states REC's page inputs on ARM, its status arm this render.
// The caller holds r.mu.
func (r *resolver) pageInputsOf(rec wsm.Workspace, rc rowContext, arm string) pageInputs {
	s := r.state.workspace(rec.ID)
	ended := s.lastTurnEnd(rec.LastTurnEndedAt)
	selected := rc.selected != nil && *rc.selected == rec.ID
	away := rec.OneShot || syncMergeArms[arm]
	// WHEN IT BECAME A ONE-SHOT OR ENTERED ITS MERGE is when this roster first
	// saw it so; the page's away grace runs from the later of that and the
	// user's leaving.
	switch {
	case !away:
		s.awaySince = nil
	case s.awaySince == nil:
		at := r.now()
		s.awaySince = &at
	}
	return pageInputs{
		visible:         r.state.visibility[rec.ID].Visible(),
		selected:        selected,
		awayUnloaded:    away,
		awayFrom:        laterOf(s.awaySince, s.leftAt),
		mergeActive:     arm == syncMergeActiveArm,
		interactedAt:    laterOf(s.interactedAt, rec.InteractedAt),
		oneShot:         rec.OneShot,
		alwaysShown:     rec.AlwaysShown(),
		working:         workingArms[arm] || arm == startingArm || arm == noSessionYetArm || rec.TopPriority(),
		unread:          isTurnEndArm(arm) && !s.viewed(),
		turnEndedAt:     ended,
		leftAt:          s.leftAt,
		leftFirstViewed: s.leftFirstViewed,
	}
}

// PageClock arms a one-shot timer: FN runs once after D unless the returned
// stop is called first (WithPageClock). The default is time.AfterFunc.
type PageClock func(d time.Duration, fn func()) (stop func() bool)

// WithPageClock installs the timer the page verdicts' clock is armed on, for
// a test that fires it by hand.
func WithPageClock(clock PageClock) Option {
	return func(r *resolver) { r.pageClock = clock }
}

// armPageClock arms the page clock for NEXT, the earliest instant a verdict
// flips by a clock, re-arming only when it moved; a zero NEXT stops it. Its
// firing re-renders, which derives every verdict again at the new instant.
// The caller holds r.mu.
func (r *resolver) armPageClock(next, now time.Time) {
	if next.Equal(r.pageWakeAt) {
		return
	}
	if r.pageWakeStop != nil {
		r.pageWakeStop()
		r.pageWakeStop = nil
	}
	r.pageWakeAt = next
	if next.IsZero() {
		return
	}
	clock := r.pageClock
	if clock == nil {
		clock = func(d time.Duration, fn func()) func() bool { return time.AfterFunc(d, fn).Stop }
	}
	r.pageWakeStop = clock(next.Sub(now), r.pageClockFired)
}

// pageClockFired re-renders once a page verdict's clock ran out, so the
// verdict it flipped is published.
func (r *resolver) pageClockFired() {
	r.mutate("daemon.sidebar.page_clock", logsite.Msg("a page verdict's clock ran out, so the roster derived every page verdict again"),
		nil, r.log.Global(), func() {
			r.pageWakeAt = time.Time{}
			r.pageWakeStop = nil
		})
}

// noteDeparture records that the user switched away from WS, whose facts
// are S, at AT: when it left and whether the visit it left first marked it
// viewed. The caller holds r.mu.
func noteDeparture(s *wsState, at time.Time) {
	s.leftAt = &at
	s.leftFirstViewed = !s.visitBeganViewed && s.viewed()
}

// noteArrival records that the user switched to the workspace whose facts are
// S: the visit begins, viewed or not. The caller holds r.mu.
func noteArrival(s *wsState) {
	s.visitBeganViewed = s.viewed()
}
