package rostertest

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// FleetRosterWorkspaces is the fleet size a real editor startup publishes, and
// the size [FleetRoster] is called with by the standing publication benchmark.
const FleetRosterWorkspaces = 16

// FleetRoster builds a roster the size of a real startup fleet: `workspaces`
// rows spread over two repository sections, every third row carrying a nested
// child, and a settled row in recently_merged.
//
// It is the LOAD fixture, distinct from [ValidRoster]'s CONTRACT fixture.
// ValidRoster is deliberately small and exhaustive so an assertion can name
// every row it contains; FleetRoster is deliberately realistic so a benchmark
// measures what a boot actually publishes. Both are contract-legal, and the
// frontend package's fixture guard checks BOTH against validateRoster, so a
// tightened contract fails at the amending commit rather than in a later wave.
func FleetRoster(bootID string, revision int64, workspaces int) *frontendv1.WorkspaceRoster {
	sections := []*frontendv1.RosterRepoSection{
		{RepoKey: RepoKey},
		{RepoKey: FoldedRepoKey, Folded: true},
	}
	for i := 0; i < workspaces; i++ {
		row := &frontendv1.RosterRow{
			Dir:     fmt.Sprintf("/Users/dev/.config/doom-worktrees/fleet-workspace-%02d", i),
			Name:    fmt.Sprintf("fleet-workspace-%02d", i),
			Current: i == 0,
		}
		setFleetStatus(row, i)
		if i%3 == 0 {
			child := &frontendv1.RosterRow{
				Dir:  fmt.Sprintf("/Users/dev/.config/doom-worktrees/fleet-workspace-%02d-spawn", i),
				Name: fmt.Sprintf("fleet-workspace-%02d-spawn", i),
			}
			child.Status = &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}}
			row.Children = []*frontendv1.RosterRow{child}
		}
		section := sections[0]
		if i%4 == 3 {
			section = sections[1]
		}
		section.Rows = append(section.Rows, row)
	}
	return &frontendv1.WorkspaceRoster{
		Revision: revision,
		BootId:   bootID,
		View: &frontendv1.WorkspaceRoster_Repository{
			Repository: &frontendv1.RosterRepositoryView{Sections: sections},
		},
		RecentlyMerged: &frontendv1.RosterSection{Rows: []*frontendv1.RosterRow{{
			Dir:    RecentlyMergedRowDir,
			Name:   "doom-settled",
			Status: &frontendv1.RosterRow_Merged{Merged: &frontendv1.RosterRowStatusMerged{}},
		}}},
		CurrentDir: "/Users/dev/.config/doom-worktrees/fleet-workspace-00",
		NavDir:     "/Users/dev/.config/doom-worktrees/fleet-workspace-01",
	}
}

// setFleetStatus spreads the lifecycle arms across the fleet, so the fixture is
// not sixteen copies of one status: a real boot's roster carries ready,
// thinking, merging and hibernated rows at once.
func setFleetStatus(row *frontendv1.RosterRow, i int) {
	switch i % 4 {
	case 0:
		row.Status = &frontendv1.RosterRow_Ready{Ready: &frontendv1.RosterRowStatusReady{}}
	case 1:
		row.Status = &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}}
	case 2:
		row.Status = &frontendv1.RosterRow_Merging{Merging: &frontendv1.RosterRowStatusMerging{}}
	default:
		row.Status = &frontendv1.RosterRow_Hibernated{Hibernated: &frontendv1.RosterRowStatusHibernated{}}
	}
}
