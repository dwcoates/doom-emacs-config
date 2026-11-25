package wsm

import "time"

// Withholding is whether, and why, the editor's tab bar withholds an open
// workspace's tab. It is THE ONE STATEMENT of a workspace's visibility: the
// roster's tab bar draws it, and the daemon decides from it which workspaces
// run (Runs), so what the user sees and what the daemon runs cannot disagree.
type Withholding int

const (
	// Drawn is a workspace the tab bar draws.
	Drawn Withholding = iota
	// WithheldBacklogged is a workspace the user hid (SetWorkspaceBacklogged).
	WithheldBacklogged
	// WithheldCollapsed is a workspace whose repository section or
	// subsection is collapsed.
	WithheldCollapsed
)

// String names the withholding for the records.
func (w Withholding) String() string {
	switch w {
	case Drawn:
		return "drawn"
	case WithheldBacklogged:
		return "backlogged"
	case WithheldCollapsed:
		return "collapsed"
	default:
		return "unknown"
	}
}

// Withheld answers whether the tab bar withholds the workspace's tab by the
// sections alone, given whether a collapsed section covers it (its
// repository's, its subsection's, or an ancestor's children fold). A hidden
// workspace has no tab at all; a covered one keeps a folded one. It is the
// sections' half of the picture: what a fold leaves drawn is the persisted
// visibility (Visibility), which a working or unviewed workspace under a fold
// is visible by, and which is the one fact every allocation reads.
func (ws Workspace) Withheld(folded bool) Withholding {
	switch {
	case ws.InBacklog():
		return WithheldBacklogged
	case folded:
		return WithheldCollapsed
	default:
		return Drawn
	}
}

// InBacklog reports whether the user's hide governs the workspace: it
// is hidden and its merge has not settled. OPEN BEATS MERGED, so a merged
// workspace that was reopened and hidden is hidden like any other; only a
// settled merge (merged and closed) is listed under recently merged instead.
func (ws Workspace) InBacklog() bool {
	return ws.Backlogged && !ws.MergeSettled()
}

// MergeSettled reports whether the workspace's merge has settled: it is
// closed and its merge landed. OPEN BEATS MERGED, so a merged workspace that
// was reopened has not.
func (ws Workspace) MergeSettled() bool {
	return ws.Closed && ws.MergedAt != nil
}

// InMergedBand reports whether the workspace is in the sidebar's merged set:
// the workspaces the Recently Merged band counts and
// agentrepl.v1.ListMergedWorkspaces pages. Its merge has settled, and it is
// no chat thread (listed only in the thread section). A hidden merge
// workspace is never in it: a landed one is an ordinary closed workspace
// (MergeHidden). It is the ONE spelling of that set, for the roster and every
// registry reader that answers for it.
func (ws Workspace) InMergedBand() bool {
	return !ws.IsThread() && ws.MergeSettled()
}

// KeptWarm reports whether the workspace's prompt cache is kept warm by
// keep-alives, given whether a merge of it is queued: whether it would run
// with every section unfolded. COLLAPSING HAS NO EFFECT ON KEEP-ALIVES
// (owner ruling, 2026-11-17): a collapsed section saves the tab bar's space
// and its sessions' memory, never a cache; only a hide lets one lapse
// (LetsCacheLapse). Before it, a collapsed workspace owed no keep-alive, and
// its cache lapsed an hour after its last one (2026-11-16: four workspaces
// cold-gated three hours after their last turn).
//
// IT IS NOT BUILT ON RUNS: Runs follows the persisted visibility, which a fold
// changes, and the visibility is no keep-alive input (owner-settled,
// 2026-11-21). It is the run rule with every section unfolded and no
// visibility: the selection, a queued merge, a deploy mediator, and every
// workspace that is not a hidden merge workspace, a backlogged one for its one
// last beat.
func (ws Workspace) KeptWarm(mergeQueued bool) bool {
	if ws.Current || mergeQueued || ws.IsMediator() {
		return true
	}
	// A BACKLOGGED WORKSPACE IS KEPT WARM FOR ONE LAST BEAT, as a closed one
	// is (owner ruling, 2026-11-24): the keeper's schedule spends that beat
	// and then holds its keep-alives (KeepaliveHeld).
	return !ws.MergeHidden()
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
// WORKSPACES RUN). A workspace the user hid, or whose repository section or
// subsection is collapsed, is started by nothing; a hidden one already running is kept until
// its cache lapses (cachelapse.Keeper).
//
// A STANDING DEPLOY CHECK RUNS (owner ruling, 2026-11-17): the repository's
// deploy intake and fixer are kept warm across deploys, the hidden intake
// included, so no check waits on a cold cache.
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
// THE WORKSPACE THE USER IS LOOKING AT KEEPS A RUNNING SESSION UP (owner
// rulings, 2026-10-17 and 2026-11-24), even backlogged or collapsed: a rule
// that withheld it stood a session down the moment it came up, so its cold
// gate stood again after every answer. Selecting a workspace whose session is
// PARKED never brings it up: only a prompt, a settings pick, a keep-alive
// restore, a cold gate's answer or a restart does. Moving off it withholds it
// again.
//
// A WORKSPACE A FOLD SPARES RUNS UNDER THE FOLD (FoldSpare): a fold never
// stands a one-shot down, so a boot never withholds one a fold hides either.
// A one-shot is filed in subsections like any other workspace (owner request,
// 2026-11-03), so a folded one-shot's unattended work would otherwise stop at
// the next daemon start.
//
// EVERY VISIBLE WORKSPACE RUNS (owner-settled, 2026-11-21): the run rule is
// a superset of the persisted visibility, so a workspace the user can see
// always has its session. The rest of this rule adds the workspaces that run
// while nobody can see them.
func (ws Workspace) Runs(folded, mergeQueued bool) bool {
	if ws.Visibility.Visible() || mergeQueued || ws.AlwaysShown() {
		return true
	}
	if ws.MergeHidden() {
		return false
	}
	if ws.Withheld(folded) != WithheldCollapsed {
		return false
	}
	_, spared := ws.FoldSpare()
	return spared
}

// KeepaliveHeld reports whether the workspace's keep-alives are held after
// one last beat (owner ruling, 2026-11-24, which matches the close's
// 2026-10-17 rule): it sits in the backlog and the user is not looking at it.
// The keeper's cadence reads it, and LetsCacheLapse is it or a close.
func (ws Workspace) KeepaliveHeld() bool {
	return ws.InBacklog() && !ws.Current && !ws.IsMediator()
}

// isTopPriority reports whether P is P0.5 or P1, the priorities whose open
// workspaces are always visible (Visibilities).
func isTopPriority(p Priority) bool { return p == PriorityP05 || p == PriorityP1 }

// TopPriority reports whether the workspace has priority P0.5 or P1, which
// keeps it visible under every fold (VisibilityInput.TopPriority). A fold
// therefore never stands such a workspace down (foldTabs, ResumeFolded).
func (ws Workspace) TopPriority() bool { return ws.Priority != nil && isTopPriority(*ws.Priority) }

// ShimHold is until when a workspace's resource allocation holds its IDLE
// shim up: the shim half of workspace resource allocation (owner rulings,
// 2026-11-24), which the roster derives beside the page verdict and the cache
// keeper parks by. A busy session is never parked by it; it says only how
// long an idle one waits. The zero value holds nothing: an idle session is
// parked at once.
type ShimHold struct {
	// Indefinite holds the shim up for as long as what decided it stands (the
	// selection, work, a mediator, a queue-bypass holder, a one-shot); the
	// roster reports the change that ends it.
	Indefinite bool
	// Until holds the shim up before this instant, when Indefinite is not
	// set; zero holds nothing.
	Until time.Time
}

// HoldsAt reports whether the hold keeps an idle shim up at NOW.
func (h ShimHold) HoldsAt(now time.Time) bool {
	return h.Indefinite || now.Before(h.Until)
}
