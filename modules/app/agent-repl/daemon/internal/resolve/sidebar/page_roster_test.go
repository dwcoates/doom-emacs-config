package sidebar_test

import (
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// pageName reads a row's page verdict arm by name.
func pageName(row *frontendv1.RosterRow) string {
	switch row.GetPage().GetPage().(type) {
	case *frontendv1.RosterRowPage_Loaded:
		return "loaded"
	case *frontendv1.RosterRowPage_Unloaded:
		return "unloaded"
	default:
		return "unset"
	}
}

// pageRig is a roster on a clock the test moves, with the page clock's armed
// timer held for the test to fire.
type pageRig struct {
	sidebarResolver
	mu    sync.Mutex
	now   time.Time
	armed func()
	after time.Duration
}

// newPageRig is a roster of workspaces "a" and "b", both live and idle, on a
// clock standing at epoch; MARK, when set, edits a's record first.
func newPageRig(t *testing.T, mark ...func(*wsm.Workspace)) *pageRig {
	t.Helper()
	rig := &pageRig{now: epoch}
	r, surfaces := newResolver(t,
		sidebar.WithClock(func() time.Time { rig.mu.Lock(); defer rig.mu.Unlock(); return rig.now }),
		sidebar.WithPageClock(func(d time.Duration, fn func()) func() bool {
			rig.mu.Lock()
			defer rig.mu.Unlock()
			rig.armed, rig.after = fn, d
			return func() bool { rig.mu.Lock(); defer rig.mu.Unlock(); rig.armed = nil; return true }
		}))
	a := workspace("a", "a")
	for _, m := range mark {
		m(&a)
	}
	reg := registry(a, workspace("b", "b"))
	r.SetRegistry(reg)
	for _, ws := range reg.Workspaces {
		r.OnLink(ws.ID, shimclient.LinkConnected)
		r.OnSessionStarted(ws.ID, &conversationv1.SessionStarted{VendorSessionId: "vendor-" + string(ws.ID)})
	}
	rig.sidebarResolver = sidebarResolver{Resolver: r, surfaces: surfaces}
	return rig
}

// advance moves the clock by D and fires the page clock when it is due.
func (rig *pageRig) advance(t *testing.T, d time.Duration) {
	t.Helper()
	rig.mu.Lock()
	rig.now = rig.now.Add(d)
	fire := rig.armed
	rig.mu.Unlock()
	if fire != nil {
		fire()
	}
}

// pageOf reads workspace ID's page verdict off the last roster.
func (rig *pageRig) pageOf(t *testing.T, id string) string {
	t.Helper()
	return pageName(rowFor(repoRows(t, latest(t, rig.Resolver)), id))
}

func TestTheSelectionHoldsItsPageAndAnIdleOneOffScreenDoesNot(t *testing.T) {
	// Arrange
	rig := newPageRig(t)

	// Act
	rig.SetSelected("a")

	// Assert
	if got := []string{rig.pageOf(t, "a"), rig.pageOf(t, "b")}; got[0] != "loaded" || got[1] != "unloaded" {
		t.Fatalf("pages a, b = %v, want loaded, unloaded", got)
	}
}

func TestAWorkspaceLeftKeepsItsPageForTheLeaveGraceThenLetsItGo(t *testing.T) {
	// Arrange
	rig := newPageRig(t)
	rig.SetSelected("a")

	// Act
	rig.SetSelected("b")
	during := rig.pageOf(t, "a")
	rig.advance(t, sidebar.PageLeaveGrace)
	after := rig.pageOf(t, "a")

	// Assert
	if during != "loaded" || after != "unloaded" {
		t.Fatalf("page during the grace = %s, after = %s; want loaded then unloaded", during, after)
	}
	if !hasRecord(rig.surfaces.Records(), "info", "daemon.sidebar.page_verdict_changed") {
		t.Fatal("no daemon.sidebar.page_verdict_changed record: the clock's flip was not recorded")
	}
}

func TestAVisitThatFirstViewedTheWorkspaceKeepsItsPageForTheFirstViewGrace(t *testing.T) {
	// Arrange: a's turn ended, unread, and the user reads it on a visit.
	rig := newPageRig(t)
	turnFinished(rig.sidebarResolver, "a")
	rig.SetSelected("a")
	rig.advance(t, sidebar.ViewExemption)
	rig.SetInteracted("a", rig.now)

	// Act
	rig.SetSelected("b")
	rig.advance(t, sidebar.PageLeaveGrace)
	kept := rig.pageOf(t, "a")
	rig.advance(t, sidebar.PageFirstViewGrace)
	gone := rig.pageOf(t, "a")

	// Assert
	if kept != "loaded" || gone != "unloaded" {
		t.Fatalf("page past the leave grace = %s, past the first-view grace = %s; want loaded then unloaded", kept, gone)
	}
}

func TestAWorkspaceThatIsNotVisibleHoldsNoPage(t *testing.T) {
	// Arrange: a collapsed section folds an idle, read workspace away.
	reg := foldedOne()
	r := arrange(t)
	r.SetRegistry(reg)
	liveRegistry(r, reg)

	// Act
	row := onlyRow(t, r)

	// Assert
	if got := pageName(row); got != "unloaded" {
		t.Fatalf("page = %s, want unloaded: a workspace that is not visible never holds one", got)
	}
}

func TestAOneShotOffScreenKeepsItsPageForTheAwayGraceEvenWhileWorking(t *testing.T) {
	// Arrange: a working one-shot the user is not looking at.
	rig := newPageRig(t, func(ws *wsm.Workspace) { ws.OneShot = true })
	rig.SetSelected("b")
	working(rig.sidebarResolver, "a")
	during := rig.pageOf(t, "a")

	// Act
	rig.advance(t, sidebar.PageAwayGrace)

	// Assert
	if after := rig.pageOf(t, "a"); during != "loaded" || after != "unloaded" {
		t.Fatalf("page within the away grace = %s, after = %s; want loaded then unloaded", during, after)
	}
}

func TestAWorkingWorkspaceOffScreenHoldsItsPage(t *testing.T) {
	// Arrange
	r := arrange(t, workspace(string(theWS), "one"), workspace("other", "other"))
	r.SetSelected("other")

	// Act
	working(r, theWS)

	// Assert
	if got := pageName(rowFor(repoRows(t, latest(t, r)), string(theWS))); got != "loaded" {
		t.Fatalf("page = %s, want loaded", got)
	}
}

func TestASwitchAwayTellsTheShimHoldSinkAndHoldsTheShimForTheGrace(t *testing.T) {
	// Arrange
	var mu sync.Mutex
	told := map[string]int{}
	r, _ := newResolver(t, sidebar.WithShimHoldSink(func(ws ids.WorkspaceID) {
		mu.Lock()
		defer mu.Unlock()
		told[string(ws)]++
	}))
	reg := registry(workspace("a", "a"), workspace("b", "b"))
	r.SetRegistry(reg)
	liveRegistry(r, reg)
	r.SetSelected("a")
	mu.Lock()
	before := told["a"]
	mu.Unlock()

	// Act
	r.SetSelected("b")

	// Assert
	mu.Lock()
	after := told["a"]
	mu.Unlock()
	if after <= before {
		t.Fatalf("a's shim hold changes told = %d, want more than %d after the switch away", after, before)
	}
	if got := r.ShimHold("a"); got.Indefinite {
		t.Fatalf("a's shim hold = %+v, want it no longer held while selected", got)
	}
}
