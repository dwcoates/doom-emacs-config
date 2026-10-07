package sidebar

import (
	"sort"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/wsm"
)

// unprioritized is the rank an unprioritized workspace sorts at: BELOW every
// stated priority, and stated as a rank rather than as a special case so the
// comparison below has exactly one shape.
const unprioritized = 1 << 30

// priorityRank is a workspace's ordering rank. P0.5 is the strongest claim and
// sorts first; a workspace with no priority sorts after every one that has one.
func priorityRank(p *wsm.Priority) int {
	if p == nil {
		return unprioritized
	}
	return int(*p)
}

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
// unprioritized. Ordering is already the resolver's; this is only the label.
func priorityBadge(p *wsm.Priority) *frontendv1.RosterRowPriorityBadge {
	label := priorityLabel(p)
	if label == "" {
		return nil
	}
	return &frontendv1.RosterRowPriorityBadge{Label: label}
}

// sortWorkspaces puts workspaces in ROSTER ORDER, which every client — the
// Emacs tab bar included — follows strictly and never re-sorts:
//
//  1. PRIORITY, strongest first: P0.5, P1, P2, P3, then unprioritized.
//  2. NAME, ascending, so the order is stable for workspaces alike in priority.
//  3. The workspace id, so two workspaces alike in both still draw in one
//     fixed order rather than swapping between pushes.
//
// THE ORDER IS A PROPERTY OF THE WORKSPACES, NEVER OF THE SELECTION. Roster
// order once broke a priority tie on LAST SELECTED, and that made the drawn
// order move under the user: selecting a workspace stamps its
// last_selected_at, the registry re-pushes, and the row the user had just
// landed on hopped to the front of its priority band. Cycling right and then
// left therefore did not return the user where they started — realtest 4 went
// explanation-engine → ABC/chess960-review-failures-enm → rt4-bootstrap-1.
// Every individual switch was correct for the order it saw; the order was
// what moved. Selection changes what is UNDERLINED and nothing about what is
// where, so no field stamped at selection time may order the bar. The
// selection instant still draws, in the row's when column.
//
// It sorts a copy: the slice belongs to the registry's caller.
func sortWorkspaces(in []wsm.Workspace) []wsm.Workspace {
	out := make([]wsm.Workspace, len(in))
	copy(out, in)
	sort.SliceStable(out, func(i, j int) bool { return lessWorkspace(out[i], out[j]) })
	return out
}

// lessWorkspace is roster order's comparison, stated once so sections, nested
// children and the merged section cannot order themselves differently.
func lessWorkspace(a, b wsm.Workspace) bool {
	ap, bp := priorityRank(a.Priority), priorityRank(b.Priority)
	if ap != bp {
		return ap < bp
	}
	if a.Name != b.Name {
		return a.Name < b.Name
	}
	return a.ID < b.ID
}

// compareRecentFirst orders two instants MOST RECENT FIRST, with "never"
// sorting last. It answers a three-way comparison so the caller can fall
// through to the next key on a tie.
func compareRecentFirst(a, b *time.Time) int {
	switch {
	case a == nil && b == nil:
		return 0
	case a == nil:
		return 1
	case b == nil:
		return -1
	case a.After(*b):
		return -1
	case b.After(*a):
		return 1
	default:
		return 0
	}
}

// sortMerged orders the recently-merged section MOST RECENTLY MERGED FIRST.
// The section's whole meaning is recency, so it does NOT take roster order:
// ordering "recently merged" by priority would bury the merge that just
// landed. The workspace id breaks a tie so the order is fixed.
func sortMerged(in []wsm.Workspace) []wsm.Workspace {
	out := make([]wsm.Workspace, len(in))
	copy(out, in)
	sort.SliceStable(out, func(i, j int) bool {
		a, b := out[i].MergedAt, out[j].MergedAt
		if c := compareRecentFirst(a, b); c != 0 {
			return c < 0
		}
		return out[i].ID < out[j].ID
	})
	return out
}
