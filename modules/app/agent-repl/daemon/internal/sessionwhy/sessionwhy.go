// Package sessionwhy says WHY each live session is up: the one reason, from a
// closed vocabulary, that agentrepl_sessions_up counts it under. A session is
// up because work runs in it, because something keeps it up on purpose (the
// user looking at it, remote control, a settings visit, a keep-alive, its
// park grace), or because a stand-down that should have taken it has not (the
// drain held back by a busy gate, a withheld stand-down that found the
// workspace shown, an owner whose park is overdue, no owner at all).
//
// The facts are kept where they are decided and only mirrored here: the
// keeper's stand-down owner and its armed idle park (cachelapse), the drain's
// last unfinished word on a stand-down (drain). The work in flight is the
// fleet's, read when the gauge is scraped. The table only MEASURES: nothing
// reads it to decide a park, so a fact missing here changes no behavior.
package sessionwhy

import (
	"sync"
	"time"

	"agent-repld/internal/ids"
)

// Reason is why a live session is up.
type Reason string

// The reasons, in the PRECEDENCE Classify applies: the first that holds is
// the session's reason.
const (
	// Working is a turn running in the session.
	Working Reason = "working"
	// DetachedWork is live detached work (a background task, a subagent) with
	// no turn running.
	DetachedWork Reason = "detached_work"
	// InView is the workspace the user is looking at: the keeper leaves it up.
	InView Reason = "in_view"
	// RemoteControl is a workspace whose remote control is on: claude.ai
	// reaches it only through its shim.
	RemoteControl Reason = "remote_control"
	// SettingsVisit is a session a settings pick started, no turn run since.
	SettingsVisit Reason = "settings_visit"
	// ParkGrace is an idle session whose park waits out its grace
	// (until its shim hold ends, wsm.ShimHold).
	ParkGrace Reason = "park_grace"
	// Keepalive is a session up for its keep-alive: restored for the beat, or
	// awaiting the beat's answer.
	Keepalive Reason = "keepalive"
	// DeferredBusy is a session whose last stand-down attempt was held back by
	// its gate (busy, starting, reviving) or deferred by the park, and waits
	// for the session to fall free.
	DeferredBusy Reason = "stand_down_deferred_busy"
	// WithheldShown is a session whose withheld stand-down (a fold, a lapse)
	// found the workspace shown or reopened, and so kept it.
	WithheldShown Reason = "stand_down_withheld_shown"
	// CacheLapsePending is a backlogged or closed workspace's session left for its
	// cache's lapse to stand down.
	CacheLapsePending Reason = "cache_lapse_pending"
	// IdleSweepPending is a session left for the idle sweep.
	IdleSweepPending Reason = "idle_sweep_pending"
	// ParkOverdue is a session owned by the cold park or the keep-alive cycle
	// whose grace is out and which is still up.
	ParkOverdue Reason = "park_overdue"
	// NoOwner is a session the keeper gave no stand-down owner.
	NoOwner Reason = "no_owner"
)

// All is every reason, in precedence order, for a caller that serves each
// from 0.
var All = []Reason{
	Working, DetachedWork, InView, RemoteControl, SettingsVisit, ParkGrace, Keepalive,
	DeferredBusy, WithheldShown, CacheLapsePending, IdleSweepPending, ParkOverdue, NoOwner,
}

// Owner is the keeper's stand-down owner of a session, or why it left the
// session up (cachelapse's standing).
type Owner string

// The owners, as the keeper names them.
const (
	OwnerInView        Owner = "in_view"
	OwnerRemoteControl Owner = "remote_control"
	OwnerVisit         Owner = "settings_visit"
	OwnerCycle         Owner = "keepalive_cycle"
	OwnerLapse         Owner = "cache_lapse"
	OwnerSweep         Owner = "idle_sweep"
	OwnerCold          Owner = "cold_park"
)

// Hold is the drain's last unfinished word on a session's stand-down.
type Hold string

// The holds.
const (
	// HoldDeferredBusy: the attempt was held back by the gate or the park
	// deferred it.
	HoldDeferredBusy Hold = "deferred_busy"
	// HoldWithheldShown: a withheld stand-down found the workspace shown or
	// reopened.
	HoldWithheldShown Hold = "withheld_shown"
)

// Facts are what the table holds of one workspace's session.
type Facts struct {
	// Owner is the keeper's owner, empty when it gave none.
	Owner Owner
	// ParkDue is the instant the keeper armed the session's idle park for,
	// zero when none is armed.
	ParkDue time.Time
	// Hold is the drain's last unfinished word, empty when none stands.
	Hold Hold
}

// Work is what runs in a session now, as the fleet answers it.
type Work struct {
	// Turn reports a turn in flight.
	Turn bool
	// Detached reports live detached work.
	Detached bool
}

// Classify answers why a live session is up from what runs in it and its
// FACTS, at NOW.
func Classify(work Work, f Facts, now time.Time) Reason {
	switch {
	case work.Turn:
		return Working
	case work.Detached:
		return DetachedWork
	case f.Owner == OwnerInView:
		return InView
	case f.Owner == OwnerRemoteControl:
		return RemoteControl
	case f.Owner == OwnerVisit:
		return SettingsVisit
	case !f.ParkDue.IsZero() && now.Before(f.ParkDue):
		return ParkGrace
	case f.Owner == OwnerCycle && f.Hold == "":
		return Keepalive
	case f.Hold == HoldDeferredBusy:
		return DeferredBusy
	case f.Hold == HoldWithheldShown:
		return WithheldShown
	case f.Owner == OwnerCycle:
		return Keepalive
	case f.Owner == OwnerLapse:
		return CacheLapsePending
	case f.Owner == OwnerSweep:
		return IdleSweepPending
	case f.Owner == OwnerCold:
		return ParkOverdue
	default:
		return NoOwner
	}
}

// Table holds every workspace's facts. A nil *Table holds nothing and
// answers nothing, for a daemon or a test that measures no sessions. Each
// method takes the table's own lock only, held for a map update.
type Table struct {
	mu    sync.Mutex
	facts map[ids.WorkspaceID]Facts
}

// NewTable answers an empty table.
func NewTable() *Table {
	return &Table{facts: map[ids.WorkspaceID]Facts{}}
}

// update applies CHANGE to WS's facts.
func (t *Table) update(ws ids.WorkspaceID, change func(*Facts)) {
	if t == nil {
		return
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	f := t.facts[ws]
	change(&f)
	if f == (Facts{}) {
		delete(t.facts, ws)
		return
	}
	t.facts[ws] = f
}

// SetOwner records the keeper's owner of WS's session.
func (t *Table) SetOwner(ws ids.WorkspaceID, owner Owner) {
	t.update(ws, func(f *Facts) { f.Owner = owner })
}

// SetParkDue records the instant the keeper armed WS's idle park for.
func (t *Table) SetParkDue(ws ids.WorkspaceID, due time.Time) {
	t.update(ws, func(f *Facts) { f.ParkDue = due })
}

// ClearParkDue records that no idle park of WS is armed.
func (t *Table) ClearParkDue(ws ids.WorkspaceID) {
	t.update(ws, func(f *Facts) { f.ParkDue = time.Time{} })
}

// SetHold records the drain's last unfinished word on WS's stand-down.
func (t *Table) SetHold(ws ids.WorkspaceID, hold Hold) {
	t.update(ws, func(f *Facts) { f.Hold = hold })
}

// ClearHold records that no unfinished word of the drain stands on WS.
func (t *Table) ClearHold(ws ids.WorkspaceID) {
	t.update(ws, func(f *Facts) { f.Hold = "" })
}

// Forget drops every fact of WS: no session of it is live here.
func (t *Table) Forget(ws ids.WorkspaceID) {
	if t == nil {
		return
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	delete(t.facts, ws)
}

// Facts answers WS's facts, the zero Facts when the table holds none.
func (t *Table) Facts(ws ids.WorkspaceID) Facts {
	if t == nil {
		return Facts{}
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	return t.facts[ws]
}

// Census answers why every session this daemon holds up is up, from the
// fleet's live sessions and the table. It is THE ONE SOURCE every count of
// sessions up reads (the agentrepl_sessions_up gauge, the session state of
// each loaded page, the sidebar's version hover), so no two can disagree.
type Census struct {
	// Live answers every workspace this daemon holds a shim for
	// (workspace.Fleet.Workspaces).
	Live func() []ids.WorkspaceID
	// Work answers what runs in WS's session now, false when none is held
	// (workspace.Fleet.Running).
	Work func(ws ids.WorkspaceID) (Work, bool)
	// Table holds the keeper's and the drain's facts.
	Table *Table
	// Now is the clock a park's grace is read against.
	Now func() time.Time
}

// Up answers why each held session is up, by workspace.
func (c Census) Up() map[ids.WorkspaceID]Reason {
	now := c.Now()
	out := map[ids.WorkspaceID]Reason{}
	for _, ws := range c.Live() {
		work, held := c.Work(ws)
		if !held {
			continue
		}
		out[ws] = Classify(work, c.Table.Facts(ws), now)
	}
	return out
}
