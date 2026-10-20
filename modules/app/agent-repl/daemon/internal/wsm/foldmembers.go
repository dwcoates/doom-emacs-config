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
