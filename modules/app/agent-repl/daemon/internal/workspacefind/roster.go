package workspacefind

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/protoarm"
	"agent-repld/internal/wsm"
)

// Section is the roster section a workspace's row is drawn in.
type Section int

const (
	// OpenSection is the roster's open section, child rows included.
	OpenSection Section = iota + 1
	// BacklogSection is the roster's backlog.
	BacklogSection
	// MergedSection is the roster's recently merged band, whose rows are
	// paged on demand (agentrepl.v1.ListMergedWorkspaces).
	MergedSection
)

// String names the section as agentrepl.v1.FoundWorkspaceRosterSection names
// its arm.
func (s Section) String() string {
	switch s {
	case OpenSection:
		return "open"
	case BacklogSection:
		return "backlog"
	case MergedSection:
		return "merged"
	default:
		panic(fmt.Sprintf("workspacefind: unknown section %d", int(s)))
	}
}

// Drawn is how the roster draws one workspace: the section carrying its row
// and the proto field name of the row's frontend.v1.RosterRow.status arm.
type Drawn struct {
	Section Section
	Status  string
}

// Placements answers how the roster draws each workspace it draws in its
// open, backlogged and recently merged sections: the open and backlogged rows off
// PUSH's workspaces stream, and the merged ones off MERGED, the rows the
// sidebar's pages resolve for the registry's merged set
// (sidebar.Resolver.MergedRows), since a merged workspace is in no stream. A
// row with no status arm is a roster contract breach, and a workspace drawn
// twice breaks the roster's one-row-per-workspace rule: both fail rather than
// answer a guess.
func Placements(push *frontendv1.RosterPush, merged []*frontendv1.RosterRow) (map[wsm.WorkspaceID]Drawn, error) {
	var open []*frontendv1.RosterRow
	for _, row := range rosterwalk.OpenRows(push.GetWorkspaces()) {
		open = append(open, row.GetRow())
	}
	out := map[wsm.WorkspaceID]Drawn{}
	for _, section := range []struct {
		section Section
		rows    []*frontendv1.RosterRow
	}{
		{OpenSection, open},
		{BacklogSection, rosterwalk.BacklogRows(push.GetWorkspaces())},
		{MergedSection, merged},
	} {
		for _, row := range section.rows {
			id := wsm.WorkspaceID(row.GetWorkspace().GetWorkspace().GetId())
			status, ok := protoarm.Name(row.ProtoReflect(), "status")
			if !ok {
				return nil, fmt.Errorf("workspacefind: the roster's %s row of workspace %s sets no status arm", section.section, id)
			}
			if prior, drawn := out[id]; drawn {
				return nil, fmt.Errorf("workspacefind: the roster draws workspace %s twice, in its %s and %s sections", id, prior.Section, section.section)
			}
			out[id] = Drawn{Section: section.section, Status: string(status)}
		}
	}
	return out, nil
}
