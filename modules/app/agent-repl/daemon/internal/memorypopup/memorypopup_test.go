package memorypopup

import (
	"errors"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/procmetrics"
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

func newPopups(t *testing.T, procs fakeProcesses, running ...ids.WorkspaceID) *Popups {
	t.Helper()
	p, err := New(Deps{
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
	if got := out.GetPages().GetFootprintBytes(); got != 2000 {
		t.Fatalf("pages footprint = %d, want 2000", got)
	}
	agents := out.GetAgents().GetRunning()
	if agents.GetCount() != 2 || agents.GetFootprintBytes() != 300 || agents.GetAverageBytes() != 150 {
		t.Fatalf("agents = %v, want 2 shims at 300 averaging 150", agents)
	}
	if got := out.GetTotal().GetFootprintBytes(); got != 2000+300+310+400 {
		t.Fatalf("total = %d, want every line summed", got)
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

func TestNoShimRunningIsTheAgentsNoneArm(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))

	// Act
	out, _ := p.Overview(everyOpen)

	// Assert
	if out.GetAgents().GetNone() == nil {
		t.Fatalf("agents = %v, want the none arm", out.GetAgents())
	}
}

func TestThePageCountsFollowTheOpenWorkspacesVisibility(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))

	// Act
	out, _ := p.Overview([]ids.WorkspaceID{"a", "b"})

	// Assert
	if out.GetPages().GetLoaded() != 1 || out.GetPages().GetNotLoaded() != 1 {
		t.Fatalf("pages = %v, want the visible one loaded and the other not", out.GetPages())
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

func TestAVisibleWorkspacesPageIsLoaded(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))

	// Act
	page := p.Workspace("a").GetPage()

	// Assert
	if page.GetLoaded() == nil {
		t.Fatalf("page = %v, want loaded: the workspace is visible", page)
	}
}

func TestAWorkspaceThatIsNotVisibleHasNoPageLoaded(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))

	// Act
	page := p.Workspace("b").GetPage()

	// Assert
	if page.GetNotLoaded() == nil {
		t.Fatalf("page = %v, want not loaded: the workspace is not visible", page)
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
