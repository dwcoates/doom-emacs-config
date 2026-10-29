// Package memorypopup resolves the rail's two memory popups (owner request,
// 2026-10-29; frontend.v1 sidebar_memory.proto): the version's hover, which
// says what agent-repl holds in memory taken as a whole, and a workspace
// row's hover, which says what that one workspace holds.
//
// EVERY FIGURE IS MEASURED. The process collector's latest scrape
// (procmetrics.Collector.Latest) gives each process group's physical
// footprint, the stall detector (stallbeat) the page process's recent stalls,
// and Emacs's reports (ReportPageResidency, kept here) each page's residency.
// Nothing is estimated: the pages share one web content process, so no
// per-page byte figure exists, and none is served.
package memorypopup

import (
	"errors"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/procmetrics"
)

// Deps are the popups' sources, every one required.
type Deps struct {
	// Processes is the collector's latest scrape.
	Processes interface {
		Latest() (procmetrics.Snapshot, bool)
	}
	// Stalls is the page process's stall detector.
	Stalls interface {
		Recent(now time.Time) (int, int64)
	}
	// StallWindow is the detector's window, served so the line can name it.
	StallWindow time.Duration
	// Running answers whether a workspace's shim runs.
	Running func(ws ids.WorkspaceID) bool
	// Now is the clock the stall window is read against.
	Now func() time.Time
}

// Popups holds the page residencies Emacs reported and answers both popups.
type Popups struct {
	deps Deps

	mu        sync.Mutex
	residency map[ids.WorkspaceID]*agentreplv1.PageResidency
}

// New builds the popups.
func New(deps Deps) (*Popups, error) {
	if deps.Processes == nil || deps.Stalls == nil || deps.StallWindow <= 0 || deps.Running == nil || deps.Now == nil {
		return nil, errors.New("memorypopup: the collector, the stall detector and its window, the shim presence and a clock are required")
	}
	return &Popups{deps: deps, residency: map[ids.WorkspaceID]*agentreplv1.PageResidency{}}, nil
}

// ErrMalformedResidency is a report that names no state, or a loaded page
// with no release rule: Emacs always states both, so it is refused rather
// than read as some state.
var ErrMalformedResidency = errors.New("memorypopup: a page residency names its state, and a loaded page its release rule")

// Report records what Emacs holds of WS's page, replacing its earlier report.
// A malformed report is refused and the earlier one kept.
func (p *Popups) Report(ws ids.WorkspaceID, residency *agentreplv1.PageResidency) error {
	switch state := residency.GetState().(type) {
	case *agentreplv1.PageResidency_NotLoaded:
	case *agentreplv1.PageResidency_Loaded:
		if state.Loaded.GetRelease().GetWhen() == nil {
			return ErrMalformedResidency
		}
	default:
		return ErrMalformedResidency
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	p.residency[ws] = residency
	return nil
}

// ErrMeasurementFailed is the latest scrape having failed to list the host's
// processes; the collector logged its cause.
var ErrMeasurementFailed = errors.New("memorypopup: the latest process sample could not list the host's processes")

// labelled is one listed process: its line's label and the groups it sums.
type labelled struct {
	label  string
	groups []string
}

// listed is the overview's process lines, in drawn order. A process's tools
// (its child processes) count as the process.
var listed = []labelled{
	{"daemon", []string{procmetrics.ProcessDaemon, procmetrics.ProcessDaemonTools}},
	{"Emacs", []string{procmetrics.ProcessEmacs}},
	{"store", []string{procmetrics.ProcessStore}},
	{"sidecar", []string{procmetrics.ProcessSidecar}},
}

// isShim reports whether a group is a workspace's shim or its vendor child.
func isShim(process string) bool {
	return process == procmetrics.ProcessShim || process == procmetrics.ProcessShimTools
}

// Overview answers the version hover's popup, nil when no scrape has run yet.
// OPEN keeps the reported residencies of workspaces still open; a closed
// workspace's last report is not counted.
func (p *Popups) Overview(open func(ids.WorkspaceID) bool) (*frontendv1.SidebarMemoryOverview, error) {
	snap, sampled := p.deps.Processes.Latest()
	if !sampled {
		return nil, nil
	}
	if snap.Failed != nil {
		return nil, ErrMeasurementFailed
	}
	out := &frontendv1.SidebarMemoryOverview{}
	var total uint64

	pages := &frontendv1.SidebarMemoryPages{}
	if bytes, ok := sumOf(snap, procmetrics.ProcessWebKitWebContent); ok {
		pages.FootprintBytes = &bytes
		total += bytes
	}
	p.mu.Lock()
	for ws, residency := range p.residency {
		if !open(ws) {
			continue
		}
		if residency.GetLoaded() != nil {
			pages.Loaded++
		} else {
			pages.NotLoaded++
		}
	}
	p.mu.Unlock()
	out.Pages = pages

	stalls, blocked := p.deps.Stalls.Recent(p.deps.Now())
	out.Strain = &frontendv1.SidebarMemoryStrain{
		Stalls: uint32(stalls), BlockedMs: uint64(blocked), WindowSeconds: uint32(p.deps.StallWindow / time.Second),
	}

	shims := map[string]uint64{}
	for g, bytes := range snap.Footprints {
		if isShim(g.Process) {
			shims[g.Workspace] += bytes
		}
	}
	if len(shims) == 0 {
		out.Agents = &frontendv1.SidebarMemoryAgents{State: &frontendv1.SidebarMemoryAgents_None{None: &frontendv1.SidebarMemoryAgentsNone{}}}
	} else {
		var sum uint64
		for _, bytes := range shims {
			sum += bytes
		}
		total += sum
		out.Agents = &frontendv1.SidebarMemoryAgents{State: &frontendv1.SidebarMemoryAgents_Running{Running: &frontendv1.SidebarMemoryAgentsRunning{
			Count: uint32(len(shims)), FootprintBytes: sum, AverageBytes: sum / uint64(len(shims)),
		}}}
	}

	for _, l := range listed {
		line := &frontendv1.SidebarMemoryProcess{Label: l.label}
		var sum uint64
		measured := false
		for _, process := range l.groups {
			if bytes, ok := sumOf(snap, process); ok {
				sum += bytes
				measured = true
			}
		}
		if measured {
			line.FootprintBytes = &sum
			total += sum
		}
		out.Processes = append(out.Processes, line)
	}
	out.Total = &frontendv1.SidebarMemoryTotal{FootprintBytes: total}
	return out, nil
}

// sumOf sums every live group of PROCESS in SNAP, false when none is live.
func sumOf(snap procmetrics.Snapshot, process string) (uint64, bool) {
	var sum uint64
	found := false
	for g, bytes := range snap.Footprints {
		if g.Process == process {
			sum += bytes
			found = true
		}
	}
	return sum, found
}

// Workspace answers WS's row hover's lines.
func (p *Popups) Workspace(ws ids.WorkspaceID) *frontendv1.SidebarWorkspaceMemory {
	out := &frontendv1.SidebarWorkspaceMemory{}
	if p.deps.Running(ws) {
		running := &frontendv1.SidebarWorkspaceMemoryAgentRunning{}
		if snap, sampled := p.deps.Processes.Latest(); sampled && snap.Failed == nil {
			var sum uint64
			found := false
			for g, bytes := range snap.Footprints {
				if isShim(g.Process) && g.Workspace == string(ws) {
					sum += bytes
					found = true
				}
			}
			if found {
				running.FootprintBytes = &sum
			}
		}
		out.Agent = &frontendv1.SidebarWorkspaceMemoryAgent{State: &frontendv1.SidebarWorkspaceMemoryAgent_Running{Running: running}}
	} else {
		out.Agent = &frontendv1.SidebarWorkspaceMemoryAgent{State: &frontendv1.SidebarWorkspaceMemoryAgent_NotRunning{NotRunning: &frontendv1.SidebarWorkspaceMemoryAgentNotRunning{}}}
	}
	p.mu.Lock()
	residency := p.residency[ws]
	p.mu.Unlock()
	if residency != nil {
		out.Page = pageOf(residency)
	}
	return out
}

// pageOf maps Emacs's report onto the hover's page line: the one place the
// editor's spelling and the rail's meet.
func pageOf(residency *agentreplv1.PageResidency) *frontendv1.SidebarWorkspaceMemoryPage {
	loaded := residency.GetLoaded()
	if loaded == nil {
		return &frontendv1.SidebarWorkspaceMemoryPage{State: &frontendv1.SidebarWorkspaceMemoryPage_NotLoaded{NotLoaded: &frontendv1.SidebarWorkspaceMemoryPageNotLoaded{}}}
	}
	release := &frontendv1.SidebarWorkspaceMemoryPageRelease{}
	switch when := loaded.GetRelease().GetWhen().(type) {
	case *agentreplv1.PageResidencyRelease_OnLeave:
		release.When = &frontendv1.SidebarWorkspaceMemoryPageRelease_OnLeave{OnLeave: &frontendv1.SidebarWorkspaceMemoryPageReleaseOnLeave{}}
	case *agentreplv1.PageResidencyRelease_At:
		release.When = &frontendv1.SidebarWorkspaceMemoryPageRelease_At{At: &frontendv1.SidebarWorkspaceMemoryPageReleaseAt{UnixMs: when.At.GetUnixMs()}}
	case *agentreplv1.PageResidencyRelease_Held:
		release.When = &frontendv1.SidebarWorkspaceMemoryPageRelease_Held{Held: &frontendv1.SidebarWorkspaceMemoryPageReleaseHeld{}}
	default:
		// Report refuses every residency without a rule, so none is stored.
		panic("memorypopup: a stored residency has no release rule")
	}
	return &frontendv1.SidebarWorkspaceMemoryPage{State: &frontendv1.SidebarWorkspaceMemoryPage_Loaded{Loaded: &frontendv1.SidebarWorkspaceMemoryPageLoaded{Release: release}}}
}
