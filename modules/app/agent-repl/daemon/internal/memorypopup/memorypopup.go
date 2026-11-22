// Package memorypopup resolves the rail's two memory popups (owner request,
// 2026-10-29; frontend.v1 sidebar_memory.proto): the version's hover, which
// says what agent-repl holds in memory taken as a whole, and a workspace
// row's hover, which says what that one workspace holds.
//
// EVERY FIGURE IS MEASURED. The process collector's latest sample
// (procmetrics.Collector.Latest) gives each process group's physical
// footprint, the stall detector (stallbeat) the page process's recent stalls,
// and each workspace's visibility (wsm.Visibility) whether its page is
// loaded: the editor loads exactly the visible workspaces' pages.
// Nothing is estimated: the pages share one web content process, so no
// per-page byte figure exists, and none is served.
package memorypopup

import (
	"errors"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/procmetrics"
	"agent-repld/internal/wsm"
)

// Deps are the popups' sources, every one required.
type Deps struct {
	// Processes is the collector's latest sample.
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
	// Visibility answers a workspace's visibility, which decides whether its
	// page is loaded.
	Visibility func(ws ids.WorkspaceID) wsm.Visibility
	// Now is the clock the stall window is read against.
	Now func() time.Time
}

// Popups answers both popups.
type Popups struct {
	deps Deps
}

// New builds the popups.
func New(deps Deps) (*Popups, error) {
	if deps.Processes == nil || deps.Stalls == nil || deps.StallWindow <= 0 || deps.Running == nil || deps.Visibility == nil || deps.Now == nil {
		return nil, errors.New("memorypopup: the collector, the stall detector and its window, the shim presence, the visibility and a clock are required")
	}
	return &Popups{deps: deps}, nil
}

// ErrMeasurementFailed is the latest sample having failed to list the host's
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

// Overview answers the version hover's popup, nil when no sample has run yet.
// OPEN are the open workspaces, whose pages are counted loaded or not by
// their visibility.
func (p *Popups) Overview(open []ids.WorkspaceID) (*frontendv1.SidebarMemoryOverview, error) {
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
	for _, ws := range open {
		if p.deps.Visibility(ws).Visible() {
			pages.Loaded++
		} else {
			pages.NotLoaded++
		}
	}
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
	out.Page = pageOf(p.deps.Visibility(ws))
	return out
}

// pageOf answers the hover's page line for a workspace of visibility V: its
// page is loaded exactly while it is visible.
func pageOf(v wsm.Visibility) *frontendv1.SidebarWorkspaceMemoryPage {
	if !v.Visible() {
		return &frontendv1.SidebarWorkspaceMemoryPage{State: &frontendv1.SidebarWorkspaceMemoryPage_NotLoaded{NotLoaded: &frontendv1.SidebarWorkspaceMemoryPageNotLoaded{}}}
	}
	return &frontendv1.SidebarWorkspaceMemoryPage{State: &frontendv1.SidebarWorkspaceMemoryPage_Loaded{Loaded: &frontendv1.SidebarWorkspaceMemoryPageLoaded{}}}
}
