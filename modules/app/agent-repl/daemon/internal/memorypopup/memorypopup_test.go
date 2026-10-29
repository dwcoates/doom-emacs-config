package memorypopup

import (
	"errors"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/procmetrics"
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

func newPopups(t *testing.T, procs fakeProcesses, running ...ids.WorkspaceID) *Popups {
	t.Helper()
	p, err := New(Deps{
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

func everyOpen(ids.WorkspaceID) bool { return true }

func loaded(when *agentreplv1.PageResidencyRelease) *agentreplv1.PageResidency {
	return &agentreplv1.PageResidency{State: &agentreplv1.PageResidency_Loaded{Loaded: &agentreplv1.PageResidencyLoaded{Release: when}}}
}

func notLoaded() *agentreplv1.PageResidency {
	return &agentreplv1.PageResidency{State: &agentreplv1.PageResidency_NotLoaded{NotLoaded: &agentreplv1.PageResidencyNotLoaded{}}}
}

func held() *agentreplv1.PageResidencyRelease {
	return &agentreplv1.PageResidencyRelease{When: &agentreplv1.PageResidencyRelease_Held{Held: &agentreplv1.PageResidencyReleaseHeld{}}}
}

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

func TestThePageCountsTakeOnlyOpenWorkspacesReports(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))
	for ws, r := range map[ids.WorkspaceID]*agentreplv1.PageResidency{"a": loaded(held()), "b": notLoaded(), "closed": loaded(held())} {
		if err := p.Report(ws, r); err != nil {
			t.Fatalf("Report: %v", err)
		}
	}

	// Act
	out, _ := p.Overview(func(ws ids.WorkspaceID) bool { return ws != "closed" })

	// Assert
	if out.GetPages().GetLoaded() != 1 || out.GetPages().GetNotLoaded() != 1 {
		t.Fatalf("pages = %v, want one loaded and one not, the closed one uncounted", out.GetPages())
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

func TestAnUnreportedPageHasNoLine(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))

	// Act
	out := p.Workspace("a")

	// Assert
	if out.Page != nil {
		t.Fatalf("page = %v, want none before Emacs reports", out.Page)
	}
}

func TestEachReleaseRuleMapsOntoTheRail(t *testing.T) {
	tests := []struct {
		name    string
		release *agentreplv1.PageResidencyRelease
		check   func(*frontendv1.SidebarWorkspaceMemoryPageRelease) bool
	}{
		{"held", held(), func(r *frontendv1.SidebarWorkspaceMemoryPageRelease) bool { return r.GetHeld() != nil }},
		{"on leave", &agentreplv1.PageResidencyRelease{When: &agentreplv1.PageResidencyRelease_OnLeave{OnLeave: &agentreplv1.PageResidencyReleaseOnLeave{}}},
			func(r *frontendv1.SidebarWorkspaceMemoryPageRelease) bool { return r.GetOnLeave() != nil }},
		{"at", &agentreplv1.PageResidencyRelease{When: &agentreplv1.PageResidencyRelease_At{At: &agentreplv1.PageResidencyReleaseAt{UnixMs: 42}}},
			func(r *frontendv1.SidebarWorkspaceMemoryPageRelease) bool { return r.GetAt().GetUnixMs() == 42 }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))
			if err := p.Report("a", loaded(tc.release)); err != nil {
				t.Fatalf("Report: %v", err)
			}

			// Act
			out := p.Workspace("a")

			// Assert
			if !tc.check(out.GetPage().GetLoaded().GetRelease()) {
				t.Fatalf("page = %v, want the %s rule", out.GetPage(), tc.name)
			}
		})
	}
}

func TestANotLoadedReportMapsOntoTheRail(t *testing.T) {
	// Arrange
	p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))
	if err := p.Report("a", notLoaded()); err != nil {
		t.Fatalf("Report: %v", err)
	}

	// Act
	out := p.Workspace("a")

	// Assert
	if out.GetPage().GetNotLoaded() == nil {
		t.Fatalf("page = %v, want not loaded", out.GetPage())
	}
}

func TestAMalformedReportIsRefusedAndTheEarlierKept(t *testing.T) {
	tests := []struct {
		name      string
		residency *agentreplv1.PageResidency
	}{
		{"no state", &agentreplv1.PageResidency{}},
		{"loaded with no rule", loaded(&agentreplv1.PageResidencyRelease{})},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			p := newPopups(t, sampled(map[procmetrics.Group]uint64{}))
			if err := p.Report("a", notLoaded()); err != nil {
				t.Fatalf("Report: %v", err)
			}

			// Act
			err := p.Report("a", tc.residency)

			// Assert
			if !errors.Is(err, ErrMalformedResidency) || p.Workspace("a").GetPage().GetNotLoaded() == nil {
				t.Fatalf("Report = %v, page %v: want refused, the earlier report kept", err, p.Workspace("a").GetPage())
			}
		})
	}
}
