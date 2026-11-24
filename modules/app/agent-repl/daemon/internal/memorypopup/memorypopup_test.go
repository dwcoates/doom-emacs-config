package memorypopup

import (
	"errors"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/pageheld"
	"agent-repld/internal/procmetrics"
	"agent-repld/internal/sessionwhy"
	"agent-repld/internal/wsm"
)

type fakeProcesses struct {
	snap    procmetrics.Snapshot
	sampled bool
}

func (f fakeProcesses) Latest() (procmetrics.Snapshot, bool) { return f.snap, f.sampled }

type fakeStalls struct{ count, blocked int64 }

func (f fakeStalls) Recent(time.Time) (int, int64) { return int(f.count), f.blocked }

func group(process, ws string) procmetrics.Group {
	return procmetrics.Group{Process: process, Workspace: ws}
}

// visible are the workspaces the popups' tests read as visible; every other
// one is not visible.
var visible = map[ids.WorkspaceID]bool{"a": true}

// shimsUp are the sessions the popups' tests' census holds up: a, visible,
// and b, not visible.
var shimsUp = []ids.WorkspaceID{"a", "b"}

// pageCounts counts nothing: the popups only read the held pages.
type pageCounts struct{}

func (pageCounts) CountPageLoad(pageheld.LoadReason)     {}
func (pageCounts) CountPageUnload(pageheld.UnloadReason) {}

// webHost is the connection of the web host document the tables attach.
const webHost = "host-1"

// heldPages answers a table whose web host holds a frame for each of HELD,
// as Emacs reports too.
func heldPages(held ...ids.WorkspaceID) *pageheld.Table {
	table := pageheld.New(pageCounts{})
	for _, ws := range held {
		table.Held(ws, pageheld.LoadShown)
	}
	table.HostAttached(webHost)
	if _, taken := table.HostReported(webHost, held, false); !taken {
		panic("memorypopup test: the attached host's census was not taken")
	}
	return table
}

// census answers a census holding UP, each idle.
func census(up ...ids.WorkspaceID) sessionwhy.Census {
	return sessionwhy.Census{
		Live:  func() []ids.WorkspaceID { return up },
		Work:  func(ids.WorkspaceID) (sessionwhy.Work, bool) { return sessionwhy.Work{}, true },
		Table: sessionwhy.NewTable(),
		Now:   func() time.Time { return time.Unix(1_800_000_000, 0) },
	}
}

func newPopups(t *testing.T, procs fakeProcesses, running ...ids.WorkspaceID) *Popups {
	t.Helper()
	return newPopupsWith(t, procs, census(shimsUp...), heldPages(), running...)
}

func newPopupsWith(t *testing.T, procs fakeProcesses, up sessionwhy.Census, pages *pageheld.Table, running ...ids.WorkspaceID) *Popups {
	t.Helper()
	p, err := New(Deps{
		Census: up,
		Pages:  pages,
		Visibility: func(ws ids.WorkspaceID) wsm.Visibility {
			if visible[ws] {
				return wsm.UnselectedVisible
			}
			return wsm.NotVisible
		},
		Processes:   procs,
		Stalls:      fakeStalls{count: 3, blocked: 1200},
		StallWindow: 10 * time.Minute,
		Running: func(ws ids.WorkspaceID) bool {
			for _, r := range running {
				if r == ws {
					return true
				}
			}
			return false
		},
		Now: func() time.Time { return time.Unix(1_800_000_000, 0) },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return p
}

// everyOpen is the open workspaces the overview's tests name.
var everyOpen = []ids.WorkspaceID{"a"}

func sampled(footprints map[procmetrics.Group]uint64) fakeProcesses {
	return fakeProcesses{snap: procmetrics.Snapshot{Footprints: footprints}, sampled: true}
}

func TestNewRequiresEverySource(t *testing.T) {
	// Act
	_, err := New(Deps{})

	// Assert
	if err == nil {
		t.Fatalf("New accepted no sources")
	}
}

func TestTheOverviewIsAbsentBeforeTheFirstSample(t *testing.T) {
	// Arrange
	p := newPopups(t, fakeProcesses{})

	// Act
	out, err := p.Overview(everyOpen)

	// Assert
	if out != nil || err != nil {
		t.Fatalf("Overview = (%v, %v), want no overview and no failure", out, err)
	}
}

func TestAFailedSampleIsTheOverviewsFailure(t *testing.T) {
	// Arrange
	p := newPopups(t, fakeProcesses{snap: procmetrics.Snapshot{Failed: errors.New("listing failed")}, sampled: true})

	// Act
	_, err := p.Overview(everyOpen)

	// Assert
	if !errors.Is(err, ErrMeasurementFailed) {
		t.Fatalf("Overview error = %v, want ErrMeasurementFailed", err)
	}
}

func TestTheOverviewListsEachProcessInDrawnOrder(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{group(procmetrics.ProcessStore, ""): 70}))

	// Act
	out, _ := p.Overview(everyOpen)

	// Assert
	var labels []string
	for _, line := range out.GetProcesses() {
		labels = append(labels, line.GetLabel())
	}
	if want := []string{"daemon", "Emacs", "store", "sidecar"}; len(labels) != 4 || labels[0] != want[0] || labels[2] != want[2] || labels[3] != want[3] {
		t.Fatalf("process lines = %v, want %v", labels, want)
	}
}

func TestAProcessNotRunningHasNoFigure(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{group(procmetrics.ProcessStore, ""): 70}))

	// Act
	out, _ := p.Overview(everyOpen)

	// Assert
	sidecar := out.GetProcesses()[3]
	if sidecar.FootprintBytes != nil {
		t.Fatalf("the sidecar line = %v, want no figure for a process not running", sidecar)
	}
}

func TestTheStrainCarriesTheWindow(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))

	// Act
	out, _ := p.Overview(everyOpen)

	// Assert
	strain := out.GetStrain()
	if strain.GetStalls() != 3 || strain.GetBlockedMs() != 1200 || strain.GetWindowSeconds() != 600 {
		t.Fatalf("strain = %v, want 3 stalls, 1200ms, over 600s", strain)
	}
}

func TestARunningShimsFigureIncludesItsVendorChild(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{
		group(procmetrics.ProcessShim, "a"): 100, group(procmetrics.ProcessShimTools, "a"): 40, group(procmetrics.ProcessShim, "b"): 9,
	}), "a")

	// Act
	out := p.Workspace("a")

	// Assert
	if got := out.GetAgent().GetRunning().GetFootprintBytes(); got != 140 {
		t.Fatalf("agent footprint = %d, want 140", got)
	}
}

func TestAShimNotRunningIsTheNotRunningArm(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))

	// Act
	out := p.Workspace("a")

	// Assert
	if out.GetAgent().GetNotRunning() == nil {
		t.Fatalf("agent = %v, want not running", out.GetAgent())
	}
}

func TestARunningShimNotYetMeasuredHasNoFigure(t *testing.T) {
	// Arrange
	p := newPopups(t, fakeProcesses{}, "a")

	// Act
	out := p.Workspace("a")

	// Assert
	if running := out.GetAgent().GetRunning(); running == nil || running.FootprintBytes != nil {
		t.Fatalf("agent = %v, want running with no figure", out.GetAgent())
	}
}

func TestTheOverviewSumsEveryLine(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{
		group(procmetrics.ProcessWebKitWebContent, ""): 2000,
		group(procmetrics.ProcessShim, "a"):            100,
		group(procmetrics.ProcessShimTools, "a"):       50,
		group(procmetrics.ProcessShim, "b"):            150,
		group(procmetrics.ProcessDaemon, ""):           300,
		group(procmetrics.ProcessDaemonTools, ""):      10,
		group(procmetrics.ProcessEmacs, ""):            400,
	}))

	// Act
	out, err := p.Overview(everyOpen)

	// Assert
	if err != nil {
		t.Fatalf("Overview: %v", err)
	}
	if got := out.GetWorkspaces().GetPages().GetFootprintBytes(); got != 2000 {
		t.Fatalf("pages footprint = %d, want 2000", got)
	}
	shims := out.GetWorkspaces().GetShims()
	if shims.GetUp() != 2 || shims.GetFootprintBytes() != 300 || shims.GetAverageBytes() != 150 {
		t.Fatalf("shims = %v, want 2 up at 300 averaging 150", shims)
	}
	if got := out.GetTotal().GetFootprintBytes(); got != 2000+300+310+400 {
		t.Fatalf("total = %d, want every line summed", got)
	}
}

func TestTheVisibleCountIsTheOpenWorkspacesThatAreVisible(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))

	// Act
	out, _ := p.Overview([]ids.WorkspaceID{"a", "b", "c"})

	// Assert
	if v := out.GetWorkspaces().GetVisible(); v.GetVisible() != 1 || v.GetOpen() != 3 {
		t.Fatalf("visible = %v, want 1 of 3 open", v)
	}
}

func TestTheShimsCountIsEverySessionUpVisibleOrNot(t *testing.T) {
	// Arrange: b's shim is up and b is not visible.
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{group(procmetrics.ProcessShim, "b"): 90}))

	// Act
	out, _ := p.Overview(everyOpen)

	// Assert
	if shims := out.GetWorkspaces().GetShims(); shims.GetUp() != 2 || shims.GetFootprintBytes() != 90 || shims.GetAverageBytes() != 90 {
		t.Fatalf("shims = %v, want both up, b's footprint averaged over the one measured", shims)
	}
}

func TestNoShimUpHasNoFootprint(t *testing.T) {
	// Arrange
	p := newPopupsWith(t, sampled(map[procmetrics.Group]uint64{}), census(), heldPages())

	// Act
	out, _ := p.Overview(everyOpen)

	// Assert
	if shims := out.GetWorkspaces().GetShims(); shims.GetUp() != 0 || shims.FootprintBytes != nil || shims.AverageBytes != nil {
		t.Fatalf("shims = %v, want none up and no figure", shims)
	}
}

func TestThePagesCountIsThePagesTheWebViewHolds(t *testing.T) {
	// Arrange: b's page is held though b is not visible.
	p := newPopupsWith(t, sampled(map[procmetrics.Group]uint64{}), census(), heldPages("b"))

	// Act
	out, _ := p.Overview(everyOpen)

	// Assert
	if got := out.GetWorkspaces().GetPages().GetLoaded(); got != 1 {
		t.Fatalf("pages loaded = %d, want the one the web view holds", got)
	}
}

func TestAVisibleWorkspaceWithItsPageUnloadedCountsNoPage(t *testing.T) {
	// Arrange: a is visible and the web view holds no page of it.
	p := newPopupsWith(t, sampled(map[procmetrics.Group]uint64{}), census(), heldPages())

	// Act
	out, _ := p.Overview(everyOpen)
	page := p.Workspace("a").GetPage()

	// Assert
	if out.GetWorkspaces().GetPages().GetLoaded() != 0 || page.GetNotLoaded() == nil {
		t.Fatalf("pages = %v, page = %v; want no page loaded for a visible workspace whose page is unloaded", out.GetWorkspaces().GetPages(), page)
	}
}

func TestAPageEmacsReportsThatTheHostDoesNotHoldIsNotLoaded(t *testing.T) {
	// Arrange: Emacs reports b's page held, and the web host holds no frame of
	// it.
	pages := heldPages()
	pages.Held("b", pageheld.LoadShown)
	p := newPopupsWith(t, sampled(map[procmetrics.Group]uint64{}), census(), pages)

	// Act
	out, _ := p.Overview(everyOpen)
	page := p.Workspace("b").GetPage()

	// Assert
	if out.GetWorkspaces().GetPages().GetLoaded() != 0 || page.GetNotLoaded() == nil {
		t.Fatalf("pages = %v, page = %v; want none loaded: the host's frames are the truth", out.GetWorkspaces().GetPages(), page)
	}
}

func TestAWorkspaceWhosePageIsHeldHasItLoaded(t *testing.T) {
	// Arrange
	p := newPopupsWith(t, sampled(map[procmetrics.Group]uint64{}), census(), heldPages("b"))

	// Act
	page := p.Workspace("b").GetPage()

	// Assert
	if page.GetLoaded() == nil {
		t.Fatalf("page = %v, want loaded: the web view holds it", page)
	}
}

func TestNewRequiresTheCensusAndThePages(t *testing.T) {
	tests := []struct {
		name  string
		amend func(*Deps)
	}{
		{name: "no census", amend: func(d *Deps) { d.Census = sessionwhy.Census{} }},
		{name: "no held pages", amend: func(d *Deps) { d.Pages = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			deps := newPopups(t, fakeProcesses{}).deps
			tt.amend(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatal("New accepted a missing source")
			}
		})
	}
}

func TestNewRequiresTheVisibility(t *testing.T) {
	// Act
	_, err := New(Deps{Processes: fakeProcesses{}, Stalls: fakeStalls{}, StallWindow: time.Minute,
		Running: func(ids.WorkspaceID) bool { return false }, Now: time.Now})

	// Assert
	if err == nil {
		t.Fatal("New accepted no visibility source")
	}
}
