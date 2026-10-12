package sidebar

import (
	"sort"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/wsm"
)

// priorityLabel is the badge's drawn label, and the empty string when the
// workspace is unprioritized — which draws NO badge rather than an empty one.
func priorityLabel(p *wsm.Priority) string {
	if p == nil {
		return ""
	}
	switch *p {
	case wsm.PriorityP05:
		return "P0.5"
	case wsm.PriorityP1:
		return "P1"
	case wsm.PriorityP2:
		return "P2"
	case wsm.PriorityP3:
		return "P3"
	default:
		return ""
	}
}

// priorityBadge composes the badge, or nil when the workspace is
// unprioritized. Priority only picks a workspace's landing place; this is only the label.
func priorityBadge(p *wsm.Priority) *frontendv1.RosterRowPriorityBadge {
	label := priorityLabel(p)
	if label == "" {
		return nil
	}
	return &frontendv1.RosterRowPriorityBadge{Label: label}
}

// sortWorkspaces puts workspaces in ROSTER ORDER, which every client — the
// Emacs tab bar included — follows strictly and never re-sorts: ascending
// STORED PLACE (wsm's place.go), unique across the registry, so no tie-break
// exists to disagree about.
//
// THE PLACE IS THE WORKSPACE'S OWN, NEVER THE SELECTION'S, AND NEVER ITS
// PRIORITY'S. Roster order once broke a priority tie on LAST SELECTED, and
// that made the drawn order move under the user: selecting a workspace stamped
// its last_selected_at, the registry re-pushed, and the row the user had just
// landed on hopped to the front of its priority band, so cycling right and
// then left did not return the user where they started. Priority now decides
// only where a workspace LANDS at create and re-open; after that only the
// user's own swaps move it.
//
// It sorts a copy: the slice belongs to the registry's caller.
func sortWorkspaces(in []wsm.Workspace) []wsm.Workspace {
	out := make([]wsm.Workspace, len(in))
	copy(out, in)
	sort.SliceStable(out, func(i, j int) bool { return lessWorkspace(out[i], out[j]) })
	return out
}

// lessWorkspace is roster order's comparison, stated once so sections and
// nested children cannot order themselves differently.
func lessWorkspace(a, b wsm.Workspace) bool {
	return a.Place < b.Place
}
