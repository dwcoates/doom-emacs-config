package sidebar

import (
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// The roster's status arm names, exactly the RosterRow.status oneof and
// exactly the keys of render-colors.json's roster_status table. The resolver
// asserts the table against this list at construction, so an arm that lands
// without a color fails there rather than drawing an unpainted dot.
var statusArms = []string{
	"submitting", "thinking", "clearing", "compacting", "permission", "done",
	"interrupted", "ready", "idle_async", "vendor_blocked", "init", "severed",
	"start_failed", "degraded", "dead", "merge_enqueuing", "merging",
	"merge_queued", "merge_conflict", "merge_failed", "merged", "none",
	"inactive",
}

// The merge arms of RosterRow.status — the ones that render a RECYCLE GLYPH
// rather than a lifecycle dot, and so are keyed in render-colors.json's
// merge_glyphs table rather than only in roster_status.
var mergeArms = []string{
	"merge_enqueuing", "merging", "merge_queued", "merge_conflict",
	"merge_failed", "merged",
}

// wsState is one workspace's live session accumulation — the half of a row the
// durable registry cannot state. It is in-memory only.
type wsState struct {
	// started reports whether a session has announced itself.
	started bool
	// link is the last observed daemon-to-shim link state.
	link shimclient.LinkState
	// linkSeen reports whether any link state has been observed at all.
	linkSeen bool
	// everConnected distinguishes a shim that died from one that never
	// started, which is `dead` against `start_failed`.
	everConnected bool
	// degraded reports an open degraded window on the last diagnostics push.
	degraded bool

	// turn is the accepted turn in flight, nil when the main thread is idle.
	turn *footer.TurnStarted
	// sawActivity reports whether the turn in flight has produced an activity,
	// which is what moves `submitting` to `thinking`.
	sawActivity bool
	// turnEverRan reports whether any turn has run, which is what tells
	// `ready` (nothing has happened) from a terminal the row can report.
	turnEverRan bool
	// lastClose is how the last turn ended, read only once the turn is over.
	lastClose TurnClose
	// compacting reports a VENDOR-initiated auto-compaction in flight, which
	// no accepted turn of ours announces.
	compacting bool

	// permissions are the open consent asks, by permission id. A count rather
	// than a flag: two gated calls must both be answered before the row leaves
	// `permission`.
	permissions map[string]struct{}
	// detached are the live detached-work items announced on this session, by
	// work id. They are what makes a row `idle_async` after its turn ends.
	detached map[string]struct{}

	// vendorBlocked reports evidence that the block is the vendor's or the
	// account's rather than agent-repl's.
	vendorBlocked bool

	// merge is what the merge orchestrator last told the roster.
	merge footer.MergeFacts
	// summary is the row detail's summary line, empty when none is set.
	summary string
}

// newWSState builds an empty accumulation.
func newWSState() *wsState {
	return &wsState{
		permissions: map[string]struct{}{},
		detached:    map[string]struct{}{},
	}
}

// startTurn folds an accepted turn in, which retires every fact the previous
// turn left standing.
func (s *wsState) startTurn(turn *footer.TurnStarted) {
	s.turn = turn
	if turn == nil {
		return
	}
	s.turnEverRan = true
	s.sawActivity = false
	s.vendorBlocked = false
	s.compacting = turn.Act == footer.ActCompact
	// A new turn is new foreground work: the detached items announced by the
	// turn before it belong to that turn's account, not this one's.
	s.detached = map[string]struct{}{}
	s.permissions = map[string]struct{}{}
}

// live reports whether a live session backs the workspace right now. It is
// what makes a closed workspace `inactive` — no open perspective and nothing
// running behind it.
func (s *wsState) live(session *wsm.Session) bool {
	if session != nil && session.Terminal != nil {
		return false
	}
	if s.linkSeen && s.link == shimclient.LinkDead {
		return false
	}
	return s.started || s.linkSeen || session != nil
}

// rosterState is the whole roster accumulation: the durable registry, every
// workspace's live half, and the selection.
type rosterState struct {
	// reg is the last registry snapshot, whole.
	reg Registry
	// regSeen reports whether any registry has been installed. Nothing
	// publishes before one: the roster is a view OF the registry, and a roster
	// built from live frames alone would name no workspaces.
	regSeen bool
	// workspaces are the live halves, by workspace.
	workspaces map[ids.WorkspaceID]*wsState
	// selected is the user's selection, which the daemon stamps the moment
	// SelectWorkspace arrives rather than waiting for WSM to echo it back.
	selected *ids.WorkspaceID
}

// newRosterState builds an empty roster accumulation.
func newRosterState() *rosterState {
	return &rosterState{workspaces: map[ids.WorkspaceID]*wsState{}}
}

// workspace resolves a workspace's live half, minting it on first sight.
func (r *rosterState) workspace(ws ids.WorkspaceID) *wsState {
	s, ok := r.workspaces[ws]
	if !ok {
		s = newWSState()
		r.workspaces[ws] = s
	}
	return s
}

// sessions indexes the registry's durable session records by workspace.
func (r *rosterState) sessions() map[ids.WorkspaceID]*wsm.Session {
	out := make(map[ids.WorkspaceID]*wsm.Session, len(r.reg.Sessions))
	for i := range r.reg.Sessions {
		out[r.reg.Sessions[i].Workspace] = &r.reg.Sessions[i]
	}
	return out
}
