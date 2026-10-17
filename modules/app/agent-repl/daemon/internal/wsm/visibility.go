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
// whether its repository's section is collapsed. A merged workspace files under
// recently merged whatever its hidden flag, so only its repository's fold can
// withhold it, exactly as its merged group follows that fold.
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

// InHiddenSection reports whether the workspace sits in the roster's hidden
// section: the user hid it, and it has not merged, since a merged workspace
// files under recently merged whatever its hidden flag.
func (ws Workspace) InHiddenSection() bool {
	return ws.Hidden && ws.MergedAt == nil
}

// LetsCacheLapse reports whether the workspace's prompt cache is left to lapse
// (cachelapse.Keeper): its keep-alives are held, its projected lapse is kept on
// its row, and its session is stood down when the lapse comes. A workspace in
// the hidden section does (2026-10-13), and so does a CLOSED one, once its
// one last keep-alive round is spent (owner request, 2026-10-17): closing
// leaves its session up, so reopening it before the lapse finds the cache
// warm, and reopening it after meets the cold gate like any lapsed
// conversation.
func (ws Workspace) LetsCacheLapse() bool {
	return ws.InHiddenSection() || ws.Closed
}

// Runs reports whether the daemon runs the workspace's session (owner
// principle, docs/protobuf-design/open-tabs.md: ONLY OPEN AND VISIBLE
// WORKSPACES RUN). A workspace the user hid, or whose repository section is
// collapsed, is started by nothing; a hidden one already running is kept until
// its cache lapses (cachelapse.Keeper).
//
// A HIDDEN MERGE WORKSPACE RUNS. It is withheld by its async merge, not by
// the user, and its session is what carries that merge; it is drawn in no tab
// bar until revealed, and once revealed it follows the ordinary rule.
func (ws Workspace) Runs(repoFolded bool) bool {
	if ws.MergeHidden() {
		return true
	}
	return ws.Withheld(repoFolded) == Drawn
}
