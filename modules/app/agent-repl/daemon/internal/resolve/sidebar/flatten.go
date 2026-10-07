package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// FlattenRows lists every row of a rows region depth-first, each row before
// its nested family rows. It is THE ONE WALK over RosterRow.children in this
// module: a reader that wants every workspace a region carries flattens it
// here, so a nested (child) workspace can never be missed by one reader and
// seen by another.
func FlattenRows(rows []*frontendv1.RosterRow) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, row := range rows {
		out = append(out, row)
		out = append(out, FlattenRows(row.GetChildren())...)
	}
	return out
}
