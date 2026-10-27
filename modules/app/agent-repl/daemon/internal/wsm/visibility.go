package wsm

// Withholding is whether, and why, the editor's tab bar withholds an open
// workspace's tab. It is THE ONE STATEMENT of a workspace's visibility: the
// roster's tab bar draws it, and the daemon decides from it which workspaces
// run (Runs), so what the user sees and what the daemon runs cannot disagree.
type Withholding int

const (
	// Drawn is a workspace the tab bar draws.
	Drawn Withholding = iota
	// WithheldHidden is a workspace the user hid (SetWorkspaceHidden).
	WithheldHidden
	// WithheldCollapsed is a workspace whose repository section is collapsed.
	WithheldCollapsed
)

// String names the withholding for the records.
func (w Withholding) String() string {
	switch w {
	case Drawn:
		return "drawn"
	case WithheldHidden:
		return "hidden"
	case WithheldCollapsed:
		return "collapsed"
	default:
		return "unknown"
	}
}

// Withheld answers whether the tab bar withholds the workspace's tab, given
// whether its repository's section is collapsed. A hidden workspace has no tab
// at all; a folded repository's open workspaces keep a folded one.
func (ws Workspace) Withheld(repoFolded bool) Withholding {
	switch {
	case ws.InHiddenSection():
		return WithheldHidden
	case repoFolded:
		return WithheldCollapsed
	default:
		return Drawn
	}
}

// InHiddenSection reports whether the user's hide governs the workspace: it
// is hidden and its merge has not settled. OPEN BEATS MERGED, so a merged
// workspace that was reopened and hidden is hidden like any other; only a
// settled merge (merged and closed) is listed under recently merged instead.
func (ws Workspace) InHiddenSection() bool {
	return ws.Hidden && !(ws.Closed && ws.MergedAt != nil)
}

// LetsCacheLapse reports whether the workspace's prompt cache is left to lapse
// (cachelapse.Keeper): its keep-alives are held, its projected lapse is kept on
// its row, and its session is stood down when the lapse comes. A workspace in
// the hidden section does (2026-10-13), and so does a CLOSED one, once its
// one last keep-alive round is spent (owner request, 2026-10-17): closing
// leaves its session up, so reopening it before the lapse finds the cache
// warm, and reopening it after meets the cold gate like any lapsed
// conversation. A hidden workspace the user is looking at keeps its cache
// warm (KeepaliveHeld, owner ruling 2026-10-17); a closed one lapses whether
// or not it is still the selection.
func (ws Workspace) LetsCacheLapse() bool {
	return ws.KeepaliveHeld() || ws.Closed
}

// Runs reports whether the daemon runs the workspace's session (owner
// principle, docs/protobuf-design/open-tabs.md: ONLY OPEN AND VISIBLE
// WORKSPACES RUN). A workspace the user hid, or whose repository section is
// collapsed, is started by nothing; a hidden one already running is kept until
// its cache lapses (cachelapse.Keeper).
//
// EVERY WORKSPACE IN A MERGE QUEUE RUNS (MERGEQUEUED: an entry of it stands
// in a merge queue, waiting or running; owner ruling, 2026-10-26), hidden,
// closed or in a collapsed section alike: its session is what carries the
// merge, and a queue can hold a merge for hours, so the session and its
// cache are kept for as long as the entry stands (cachelapse.Keeper keeps
// beating it). A hidden merge workspace is withheld by its async merge, not by
// the user; it is drawn in no tab bar until revealed, and once revealed it
// follows the ordinary rule. Once its merge has left every queue unlanded (it
// failed, or was taken off) nothing is carried, and it runs only while the
// user looks at it: its requester's bubble opens it, which selects it.
//
// THE WORKSPACE THE USER IS LOOKING AT RUNS (owner ruling, 2026-10-17), even
// hidden or collapsed: selecting one revives it, and a rule that withheld it
// stood that session down the moment it came up, so its cold gate stood again
// after every answer. Moving off it withholds it again.
func (ws Workspace) Runs(repoFolded, mergeQueued bool) bool {
	if ws.Current || mergeQueued {
		return true
	}
	if ws.MergeHidden() {
		return false
	}
	return ws.Withheld(repoFolded) == Drawn
}

// KeepaliveHeld reports whether the workspace's keep-alives are held from
// the start (the keeper's `hold` cadence): it sits in the hidden section and
// the user is not looking at it. The session's start and the keeper's cadence
// read it, and LetsCacheLapse is it or a close.
func (ws Workspace) KeepaliveHeld() bool {
	return ws.InHiddenSection() && !ws.Current
}
