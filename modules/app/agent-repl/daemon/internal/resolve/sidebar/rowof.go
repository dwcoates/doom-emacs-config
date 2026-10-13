package sidebar

import (
	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
)

// Row implements Resolver. The row is looked up in the grouping the roster
// shows first, then the hidden section and the recently-merged band, so it is
// the row a client of the roster draws; a clone is answered, never the
// published roster's own message.
func (r *resolver) Row(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.last == nil {
		return nil, 0, false
	}
	row := findRow(r.last, string(ws))
	if row == nil {
		return nil, 0, false
	}
	cloned, ok := proto.Clone(row).(*frontendv1.RosterRow)
	if !ok {
		return nil, 0, false
	}
	return cloned, r.last.GetRevision(), true
}

// findRow answers WS's row in ROSTER, nil when it carries none.
func findRow(roster *frontendv1.WorkspaceRoster, ws string) *frontendv1.RosterRow {
	var shown, others []*frontendv1.RosterRows
	add := func(shownView bool, rows *frontendv1.RosterRows) {
		if shownView {
			shown = append(shown, rows)
			return
		}
		others = append(others, rows)
	}
	for _, section := range roster.GetRepository().GetSections() {
		add(roster.GetShownRepository() != nil, section.GetRows())
	}
	for _, section := range roster.GetTask().GetSections() {
		add(roster.GetShownTask() != nil, section.GetRows())
	}
	for _, section := range roster.GetMaturity().GetSections() {
		add(roster.GetShownMaturity() != nil, section.GetRows())
	}
	add(true, roster.GetHidden().GetRows())
	for _, group := range roster.GetRecentlyMerged().GetGroups() {
		add(true, group.GetRows())
	}
	for _, set := range [][]*frontendv1.RosterRows{shown, others} {
		for _, rows := range set {
			if row := findInRows(rows.GetRows(), ws); row != nil {
				return row
			}
		}
	}
	return nil
}

// findInRows searches ROWS and their nested families for WS's row.
func findInRows(rows []*frontendv1.RosterRow, ws string) *frontendv1.RosterRow {
	for _, row := range rows {
		if row.GetWorkspace().GetWorkspace().GetId() == ws {
			return row
		}
		if found := findInRows(row.GetChildren(), ws); found != nil {
			return found
		}
	}
	return nil
}
