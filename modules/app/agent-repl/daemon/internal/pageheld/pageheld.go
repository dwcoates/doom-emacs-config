// Package pageheld holds which workspaces' pages Emacs's web view holds now,
// and why each was loaded. It is THE ONE SOURCE every count of loaded pages
// reads (the agentrepl_pages_loaded gauge, the sidebar's version hover), so no
// two can disagree, and a workspace that is visible with its page unloaded is
// never counted loaded.
//
// TWO PARTIES REPORT IT, AND THE WEB HOST IS THE TRUTH OF WHAT IS HELD. Emacs
// reports why each page was loaded and what it asked the host to hold
// (agentrepl.v1 ReportPageState); the web host's census reports the frames it
// actually holds (webapp/src/host/load-census.ts). The two can differ: Emacs
// records a page as held before the host has it (a call made while no host
// stands is replayed only when the host loads, and a host reload retires every
// frame until that replay), and a host call that failed leaves the host short
// of what Emacs asked. So the count (Snapshot) is the host's frames, each
// labelled with the reason Emacs gave or LoadUnreported, and the two parties
// are compared on every census (HostReported) so a disagreement that stands
// is recorded.
//
// THE HOST'S FRAMES ARE SCOPED TO ITS PAGE STREAM, as Emacs's report is to the
// workspace's host stream: a census is taken only from a web host document
// whose WatchPage stream stands here (HostAttached), and its frames are
// forgotten when that stream ends (HostGone), all under the table's one lock.
// So an editor that exits, taking its web host with it, can never leave a
// page counted that no web view holds.
//
// THE STATE IS SCOPED TO THE WORKSPACE'S HOST STREAM: the server forgets a
// workspace's page when its last WatchHostWorkspace stream ends (Forget), so
// an editor that exits or a workspace handed to a successor leaves no page
// counted that no web view holds. Emacs restates what it holds on every host
// subscribe.
package pageheld

import (
	"fmt"
	"maps"
	"slices"
	"sync"

	"agent-repld/internal/ids"
)

// LoadReason is why a page was loaded: the moment Emacs's one load path took.
type LoadReason string

// The load reasons, as the metrics label them.
const (
	LoadShown     LoadReason = "shown"
	LoadAwake     LoadReason = "awake"
	LoadSlot      LoadReason = "slot"
	LoadRequested LoadReason = "load_requested"
	LoadUngated   LoadReason = "ungated"
	// LoadHeldBeforeReasons is a page Emacs held before it recorded why, that
	// no load rule explains now.
	LoadHeldBeforeReasons LoadReason = "held_before_reasons"
)

// LoadReasons is every load reason, for a caller that serves each from 0.
var LoadReasons = []LoadReason{LoadShown, LoadAwake, LoadSlot, LoadRequested, LoadUngated, LoadHeldBeforeReasons}

// LoadUnreported labels a page the web host holds that Emacs reports no load
// of. It is never a reason Emacs gives: only Snapshot answers it, for the
// gauge to count every page the host holds.
const LoadUnreported LoadReason = "unreported"

// UnloadReason is why a page was dropped.
type UnloadReason string

// The unload reasons, as the metrics label them.
const (
	UnloadDormant  UnloadReason = "dormant"
	UnloadDetached UnloadReason = "detached"
	UnloadReleased UnloadReason = "released"
)

// UnloadReasons is every unload reason, for a caller that serves each from 0.
var UnloadReasons = []UnloadReason{UnloadDormant, UnloadDetached, UnloadReleased}

// Telemetry counts every page load and unload (runtimemetrics.Surface). Each
// method is one counter update and never blocks.
type Telemetry interface {
	CountPageLoad(reason LoadReason)
	CountPageUnload(reason UnloadReason)
}

// Table holds every page Emacs reports held, with its load reason, and every
// frame the web host reports holding, by workspace.
type Table struct {
	telemetry Telemetry
	mu        sync.Mutex
	// held is Emacs's report: what it asked the host to hold, and why.
	held map[ids.WorkspaceID]LoadReason
	// hosts counts the standing page streams of each web host document, by
	// its connection.
	hosts map[string]int
	// frames is the web host's last report of the frames it holds; nil until
	// its first, and again once the stream of the document that sent it ends.
	frames map[ids.WorkspaceID]struct{}
	// framesFrom is the connection of the document whose report frames is.
	framesFrom string
	// emacsChanged holds every workspace Emacs reported on since the last
	// census tick.
	emacsChanged map[ids.WorkspaceID]struct{}
	// pending is every disagreement the last census tick found, by workspace.
	pending map[ids.WorkspaceID]Cause
	// standing is every cause recorded and not yet cleared.
	standing map[Cause]bool
}

// New answers an empty table that counts its changes through TELEMETRY, which
// is required: pass the off metrics surface to count nothing.
func New(telemetry Telemetry) *Table {
	if telemetry == nil {
		panic("pageheld: a table counts its changes; pass the off metrics surface to count nothing")
	}
	return &Table{
		telemetry:    telemetry,
		held:         map[ids.WorkspaceID]LoadReason{},
		hosts:        map[string]int{},
		emacsChanged: map[ids.WorkspaceID]struct{}{},
		pending:      map[ids.WorkspaceID]Cause{},
		standing:     map[Cause]bool{},
	}
}

// Loaded records WS's page just loaded for REASON, and counts the load.
func (t *Table) Loaded(ws ids.WorkspaceID, reason LoadReason) {
	t.set(ws, reason)
	t.telemetry.CountPageLoad(reason)
}

// Unloaded records WS's page just dropped for REASON, and counts the unload.
func (t *Table) Unloaded(ws ids.WorkspaceID, reason UnloadReason) {
	t.Forget(ws)
	t.telemetry.CountPageUnload(reason)
}

// Held records Emacs's restatement that it holds WS's page, loaded for
// REASON. Nothing is counted.
func (t *Table) Held(ws ids.WorkspaceID, reason LoadReason) { t.set(ws, reason) }

// Forget records that no page of WS is held: Emacs's restatement that it
// holds none, or the workspace's last host stream ending. Nothing is counted.
func (t *Table) Forget(ws ids.WorkspaceID) {
	t.mu.Lock()
	defer t.mu.Unlock()
	delete(t.held, ws)
	t.emacsChanged[ws] = struct{}{}
}

func (t *Table) set(ws ids.WorkspaceID, reason LoadReason) {
	if !validLoad(reason) {
		panic(fmt.Sprintf("pageheld: a page of %s was loaded for %q, which LoadReasons does not hold", ws, reason))
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	t.held[ws] = reason
	t.emacsChanged[ws] = struct{}{}
}

// Snapshot answers every page the web host holds, labelled with the reason
// Emacs gave for its load or LoadUnreported, by workspace: a copy. Before the
// host's first census it answers none, since nothing yet says what it holds.
func (t *Table) Snapshot() map[ids.WorkspaceID]LoadReason {
	t.mu.Lock()
	defer t.mu.Unlock()
	out := make(map[ids.WorkspaceID]LoadReason, len(t.frames))
	for ws := range t.frames {
		reason, reported := t.held[ws]
		if !reported {
			reason = LoadUnreported
		}
		out[ws] = reason
	}
	return out
}

// Reported answers every page Emacs reports held, with its load reason, by
// workspace: a copy.
func (t *Table) Reported() map[ids.WorkspaceID]LoadReason {
	t.mu.Lock()
	defer t.mu.Unlock()
	return maps.Clone(t.held)
}

// Cause is which way a disagreement between the web host's frames and
// Emacs's report runs.
type Cause string

// The causes of a disagreement.
const (
	// HostOnly: the host holds a frame for a workspace Emacs reports no page
	// of.
	HostOnly Cause = "host_only"
	// EmacsOnly: Emacs reports a page held that the host holds no frame for.
	EmacsOnly Cause = "emacs_only"
)

// Causes is every cause, in the order a check reports them.
var Causes = []Cause{HostOnly, EmacsOnly}

// Disagreement is one cause found standing, with what each party holds.
type Disagreement struct {
	Cause Cause
	// Workspaces are the workspaces that disagree this way, sorted.
	Workspaces []ids.WorkspaceID
	// Host is every workspace the host holds a frame for, sorted.
	Host []ids.WorkspaceID
	// Emacs is every workspace Emacs reports a page held for, sorted.
	Emacs []ids.WorkspaceID
}

// Check is what one host census settled: the causes that began standing and
// the causes that cleared.
type Check struct {
	Began   []Disagreement
	Cleared []Cause
}

// HostAttached records that a page stream of the web host document on
// connection CONN stands: its census is taken from now on.
func (t *Table) HostAttached(conn string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.hosts[conn]++
}

// HostGone records that a page stream of the document on CONN ended. When it
// was the document's last, its census is taken no more, and the frames it
// reported are forgotten along with any disagreement still pending; a cause
// already recorded stands until a later census clears it.
func (t *Table) HostGone(conn string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.hosts[conn] == 0 {
		panic(fmt.Sprintf("pageheld: the page stream of web host document %q ended, and none stood", conn))
	}
	t.hosts[conn]--
	if t.hosts[conn] > 0 {
		return
	}
	delete(t.hosts, conn)
	if t.framesFrom == conn {
		t.frames, t.framesFrom = nil, ""
		t.pending = map[ids.WorkspaceID]Cause{}
	}
}

// HostReported records FRAMES, the workspaces the web host document on CONN
// holds a frame for, as the truth Snapshot counts, and compares them with
// Emacs's report. It answers false, taking nothing, when no page stream of
// that document stands here.
//
// A DISAGREEMENT IS RECORDED ONLY WHEN IT STANDS ACROSS TWO CENSUS TICKS.
// Emacs's reports and the host's census reach the daemon on two separate
// connections, so a load or an unload in flight shows one party ahead of the
// other for the milliseconds the report takes. A workspace counts as
// disagreeing when it disagreed the same way at the previous TICK, at this one
// and at every report between, and Emacs reported nothing of it since the
// previous tick: a report in flight would have to take a whole census period
// (load-census.ts CENSUS_EVERY_MS) to pass for one. A census that is not a
// tick (the host's frames changed) updates the truth and drops any pending
// disagreement it no longer shows, and begins none.
//
// Each cause begins once, while any of its workspaces stands, and clears once
// no workspace disagrees that way.
func (t *Table) HostReported(conn string, frames []ids.WorkspaceID, tick bool) (Check, bool) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.hosts[conn] == 0 {
		return Check{}, false
	}
	t.framesFrom = conn
	t.frames = make(map[ids.WorkspaceID]struct{}, len(frames))
	for _, ws := range frames {
		t.frames[ws] = struct{}{}
	}
	now := t.disagreementsLocked()
	for ws, cause := range t.pending {
		if now[ws] != cause {
			delete(t.pending, ws)
		}
	}
	if !tick {
		return Check{}, true
	}
	confirmed := map[Cause][]ids.WorkspaceID{}
	for ws, cause := range now {
		_, changed := t.emacsChanged[ws]
		if t.pending[ws] == cause && !changed {
			confirmed[cause] = append(confirmed[cause], ws)
		}
	}
	t.pending = now
	t.emacsChanged = map[ids.WorkspaceID]struct{}{}

	var check Check
	for _, cause := range Causes {
		if wss := confirmed[cause]; len(wss) > 0 && !t.standing[cause] {
			t.standing[cause] = true
			check.Began = append(check.Began, Disagreement{
				Cause: cause, Workspaces: slices.Sorted(slices.Values(wss)), Host: slices.Sorted(maps.Keys(t.frames)), Emacs: slices.Sorted(maps.Keys(t.held)),
			})
		}
		if t.standing[cause] && !holds(now, cause) {
			delete(t.standing, cause)
			check.Cleared = append(check.Cleared, cause)
		}
	}
	return check, true
}

// disagreementsLocked answers every workspace the two parties disagree on,
// with which way. The caller holds t.mu.
func (t *Table) disagreementsLocked() map[ids.WorkspaceID]Cause {
	out := map[ids.WorkspaceID]Cause{}
	for ws := range t.frames {
		if _, reported := t.held[ws]; !reported {
			out[ws] = HostOnly
		}
	}
	for ws := range t.held {
		if _, framed := t.frames[ws]; !framed {
			out[ws] = EmacsOnly
		}
	}
	return out
}

func holds(m map[ids.WorkspaceID]Cause, cause Cause) bool {
	for _, c := range m {
		if c == cause {
			return true
		}
	}
	return false
}

func validLoad(r LoadReason) bool {
	for _, known := range LoadReasons {
		if r == known {
			return true
		}
	}
	return false
}
