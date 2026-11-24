// Package memorypopup resolves the rail's two memory popups (owner request,
// 2026-10-29; frontend.v1 sidebar_memory.proto): the version's hover, which
// says what agent-repl holds in memory taken as a whole, and a workspace
// row's hover, which says what that one workspace holds.
//
// EVERY FIGURE IS MEASURED. The process collector's latest sample
// (procmetrics.Collector.Latest) gives each process group's physical
// footprint and the stall detector (stallbeat) the page process's recent
// stalls. EVERY COUNT READS THE ONE FACT ITS METRIC READS, so the hover and
// the metrics never disagree: a workspace is visible by its visibility
// (wsm.Visibility, agentrepl_workspaces_visible), a shim is up by the
// sessions census (sessionwhy.Census, agentrepl_sessions_up), and a page is
// loaded exactly while Emacs's web view holds it (pageheld.Table,
// agentrepl_pages_loaded): visibility only makes a page eligible.
// Nothing is estimated: the pages share one web content process, so no
// per-page byte figure exists, and none is served.
package memorypopup

import (
	"errors"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/pageheld"
	"agent-repld/internal/procmetrics"
	"agent-repld/internal/sessionwhy"
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
	// Visibility answers a workspace's visibility.
	Visibility func(ws ids.WorkspaceID) wsm.Visibility
	// Census answers why each session this daemon holds up is up: its
	// workspaces are the shims up.
	Census sessionwhy.Census
	// Pages holds which workspaces' pages Emacs's web view holds.
	Pages *pageheld.Table
	// Now is the clock the stall window is read against.
	Now func() time.Time
}

// Popups answers both popups.
type Popups struct {
	deps Deps
}

// New builds the popups.
func New(deps Deps) (*Popups, error) {
	if deps.Processes == nil || deps.Stalls == nil || deps.StallWindow <= 0 || deps.Running == nil || deps.Visibility == nil ||
		deps.Census.Live == nil || deps.Census.Work == nil || deps.Census.Now == nil || deps.Pages == nil || deps.Now == nil {
		return nil, errors.New("memorypopup: the collector, the stall detector and its window, the shim presence, the visibility, the sessions census, the held pages and a clock are required")
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
// OPEN are the open workspaces, out of which the visible ones are counted.
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

	visible := &frontendv1.SidebarMemoryWorkspacesVisible{Open: uint32(len(open))}
	for _, ws := range open {
		if p.deps.Visibility(ws).Visible() {
			visible.Visible++
		}
	}
	pages := &frontendv1.SidebarMemoryWorkspacesPages{Loaded: uint32(len(p.deps.Pages.Snapshot()))}
	if bytes, ok := sumOf(snap, procmetrics.ProcessWebKitWebContent); ok {
		pages.FootprintBytes = &bytes
		total += bytes
	}
	shims := p.shimsUp(snap)
	if shims.FootprintBytes != nil {
		total += *shims.FootprintBytes
	}
	out.Workspaces = &frontendv1.SidebarMemoryWorkspaces{Visible: visible, Shims: shims, Pages: pages}

	stalls, blocked := p.deps.Stalls.Recent(p.deps.Now())
	out.Strain = &frontendv1.SidebarMemoryStrain{
		Stalls: uint32(stalls), BlockedMs: uint64(blocked), WindowSeconds: uint32(p.deps.StallWindow / time.Second),
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

// shimsUp answers the shims line: every session the census holds up, and the
// footprints SNAP measured of those shims.
func (p *Popups) shimsUp(snap procmetrics.Snapshot) *frontendv1.SidebarMemoryWorkspacesShims {
	up := p.deps.Census.Up()
	out := &frontendv1.SidebarMemoryWorkspacesShims{Up: uint32(len(up))}
	measured := map[string]uint64{}
	for g, bytes := range snap.Footprints {
		if !isShim(g.Process) {
			continue
		}
		if _, held := up[ids.WorkspaceID(g.Workspace)]; held {
			measured[g.Workspace] += bytes
		}
	}
	if len(measured) == 0 {
		return out
	}
	var sum uint64
	for _, bytes := range measured {
		sum += bytes
	}
	average := sum / uint64(len(measured))
	out.FootprintBytes, out.AverageBytes = &sum, &average
	return out
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
	_, held := p.deps.Pages.Snapshot()[ws]
	out.Page = pageOf(held)
	return out
}

// pageOf answers the hover's page line for a workspace whose page the web
// view holds (HELD) or not.
func pageOf(held bool) *frontendv1.SidebarWorkspaceMemoryPage {
	if !held {
		return &frontendv1.SidebarWorkspaceMemoryPage{State: &frontendv1.SidebarWorkspaceMemoryPage_NotLoaded{NotLoaded: &frontendv1.SidebarWorkspaceMemoryPageNotLoaded{}}}
	}
	return &frontendv1.SidebarWorkspaceMemoryPage{State: &frontendv1.SidebarWorkspaceMemoryPage_Loaded{Loaded: &frontendv1.SidebarWorkspaceMemoryPageLoaded{}}}
}
