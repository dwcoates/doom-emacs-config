package wsm

// FoldMembers says which workspaces a fold that hides tabs folds: a
// repository's section (RepositoryMembers) or one subsection of it
// (SubsectionMembers). It is THE ONE MEMBERSHIP every reader of such a fold
// asks, so the sessions a collapse stands down, the refusal of a collapse
// that holds the current workspace, and the roster's word on whether a
// collapse may be asked can never disagree about which workspaces a section
// holds.
type FoldMembers func(Workspace) bool

// RepositoryMembers are the workspaces of REPO, in one of its subsections or
// in none.
func RepositoryMembers(repo RepoID) FoldMembers {
	return func(ws Workspace) bool { return ws.Repo == repo }
}

// SubsectionMembers are the workspaces sitting in SUB.
func SubsectionMembers(sub SubsectionID) FoldMembers {
	return func(ws Workspace) bool { return ws.Subsection != nil && *ws.Subsection == sub }
}

// StandsDown reports whether collapsing the section stands WS's session
// down: WS is a member and still open.
func (m FoldMembers) StandsDown(ws Workspace) bool {
	return m(ws) && !ws.Closed
}

// HoldsCurrent reports whether WS is the current workspace and collapsing
// the section would stand it down, which is the collapse the daemon refuses.
// A nil WS (no workspace is current) is held by no section.
func (m FoldMembers) HoldsCurrent(ws *Workspace) bool {
	return ws != nil && m.StandsDown(*ws)
}

// FoldSpare is why a fold that hides a workspace leaves its session running.
type FoldSpare string

// The spares a workspace's own record answers. A turn in flight or live
// detached work is not among them: that is the stand-down gate's live
// answer (drain's gate), which holds a stand-down back until the session
// falls free.
const (
	// FoldSpareOneShot is a one-shot: its work runs unattended, so a fold
	// never stops it.
	FoldSpareOneShot FoldSpare = "one_shot"
	// FoldSpareMergePending is a workspace with a merge queue entry standing,
	// waiting or running, its deploy hooks included.
	FoldSpareMergePending FoldSpare = "merge_pending"
)

// FoldSpare answers why a fold leaves WS's session running, false when
// nothing in WS's record spares it (owner ruling, 2026-10-25: a fold hides a
// busy workspace and never changes its status).
//
// ONLY A FOLD SPARES. A workspace the user hid, or one closed, is withheld by
// that act rather than by a fold, so it is spared nothing here and its cache
// lapse stands it down as before.
func (ws Workspace) FoldSpare() (FoldSpare, bool) {
	switch {
	case ws.Closed || ws.Hidden:
		return "", false
	case ws.OneShot:
		return FoldSpareOneShot, true
	case ws.MergePending:
		return FoldSpareMergePending, true
	default:
		return "", false
	}
}
