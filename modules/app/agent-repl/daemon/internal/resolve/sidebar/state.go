package sidebar

import (
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

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
	// stateUnreported reports a shim this daemon took back after a failed
	// handover that has not re-reported its session state. It stands from
	// the take-back's bounded wait running out until the next session start.
	stateUnreported bool

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
	// lastFailure is how the last turn's own terminal classified its failure
	// (ladder.ClassifyFailure). It refines a FAILED close: an expected stop —
	// a Stop hook, a deferred tool — closes as failed but reads as `done`.
	lastFailure ladder.FailureClass
	// lastFailureSignInCures reports that the last turn's own terminal failed
	// on an account cause signing in again cures (ladder.FailureCuredBySignIn).
	lastFailureSignInCures bool
	// endAnswered reports that a completed sign-in answered the last turn's
	// failure (OnSignedIn): the row has no turn end left to report and draws
	// `ready`. A new turn's start or close retires it.
	endAnswered bool
	// turnFault is the TURN FAULT the last turn's close raised
	// (ladder.ResolveTurnFault): `vendor_blocked` on the vendor rung, or
	// `turn_died` on the agent-repl rung. It stands until the next turn
	// starts (owner ruling, 2026-10-06), and the footer raises the same fault
	// from the same close.
	turnFault ladder.TurnFault
	// compacting reports a VENDOR-initiated auto-compaction in flight, which
	// no accepted turn of ours announces.
	compacting bool
	// retrying is the agent whose call the vendor is retrying mid-turn, empty
	// when none is. It stands from the reported failure until that agent is
	// answered (ladder.RetryAnswered), the turn ends, or a new turn opens.
	retrying string

	// permissions are the open consent asks, by permission id. A count rather
	// than a flag: two gated calls must both be answered before the row leaves
	// `permission`.
	permissions map[string]struct{}
	// detached are the live detached-work items announced on this session, by
	// work id. They are what makes a row `idle_async` after its turn ends —
	// but ONLY until the watcher states its own set: an announcement can raise
	// `idle_async` and nothing on the agent's stream can ever retire it.
	detached map[string]struct{}
	// liveWork is the watcher's authoritative live-work set, which retires
	// `idle_async` the moment the last detached item ends.
	liveWork LiveWorkSet
	// liveWorkSeen reports whether the watcher has stated a set at all. Once
	// it has, it is the ONLY source the arm reads: the announcements it
	// supersedes cannot outlive the items they announced.
	liveWorkSeen bool

	// vendorBlocked reports evidence that the block is the vendor's or the
	// account's rather than agent-repl's.
	vendorBlocked bool

	// result is the READ STATE of the last turn's result: whether the user
	// has seen it yet. It is a FACT about the workspace, not a display mode,
	// and it outlives every arm change that is not a new result: detached work
	// starting or ending, a link blip, a merge. The row's viewed (PARTIAL)
	// marker is DERIVED from it (`viewedOn`), so a read result can never be
	// drawn as unread and an unread one never as read.
	//
	// SET to unread when a turn COMPLETES, is INTERRUPTED or FAILS
	// (SetTurnEnded) — except a /clear or a compaction that completes, which
	// leaves nothing to read and is set to read on the spot (contextCut),
	// set to read when the editor reports the user has seen the row on its
	// turn-end arm (SetViewed), and reset to none by a new turn (startTurn) —
	// a new prompt is the user moving on.
	result resultState
	// resultSettled reports that the result has been decided by this
	// resolver: by a live turn event (a turn starting, ending, or being
	// viewed), or by seeding it ONCE from the durable record (restoreResult).
	// A durable record is read only while nothing live has spoken.
	resultSettled bool
	// restoredEnd is the turn-end arm seeded from the durable record, drawn in
	// place of the one lastClose resolves to until a turn of this resolver's
	// own supersedes it. Empty when the result is live.
	restoredEnd string
	// persisted is the result the durable record holds, as far as this
	// resolver knows: what it seeded from, or last reported through the result
	// sink. A render whose result differs reports it (resolver.row).
	persisted *wsm.TurnResult
	// reviving reports a revival of this workspace's parked session in
	// flight (SetReviving). It is a marker beside the status, never an arm:
	// it neither changes the arm nor clears the viewed marker.
	reviving bool
	// bringingUp reports a bring-up of this workspace's session under way
	// (SetBringingUp). It is what holds the row's availability at `pending`.
	bringingUp bool
	// opening reports an OpenTabs in flight that names this workspace and has
	// not answered it `opened` yet (SetOpening). It draws the `inactive` arm
	// with the row's `opening` marker
	// over every other.
	opening bool
	// vendorStart is where the vendor-start run stands (SetVendorStart): the
	// vendor rung draws it, and while it stands the link's own account of a
	// dead route is not agent-repl's fault.
	vendorStart VendorStart
	// networkFaults are the ids of the standing faults that say this machine
	// cannot reach the network (NetworkFaultOpened): the network rung draws
	// them.
	networkFaults map[string]bool
	// lastArm is the status arm last PUBLISHED for this workspace, which is
	// what a status CHANGE is measured against, and what a viewed report is
	// judged against: the arm the user was looking at.
	lastArm string
	// lastArmSeen reports whether any arm has been published at all, so a
	// workspace's first render is not mistaken for a change from "".
	lastArmSeen bool
	// statusChangedAt is when this daemon last saw the published arm MOVE,
	// nil until it has. It is the live half of the row's status change
	// instant; the durable half is wsm's StatusChangedAt, which the status
	// change sink is told, so a restart still ranks the row by it.
	statusChangedAt *time.Time

	// merge is what the merge orchestrator last told the roster.
	merge footer.MergeFacts
	// summary is the row detail's summary line, empty when none is set.
	summary string
}

// resultState is the read state of the last turn's result.
type resultState int

const (
	// resultNone: no result stands whose read state the roster tracks — no
	// turn has ended since the last prompt, or the last close was one this
	// build does not know.
	resultNone resultState = iota
	// resultUnread: the last turn COMPLETED, was INTERRUPTED or FAILED and
	// the user has not seen it. An unread result holds the row on its turn-end
	// arm, except while detached work runs, which draws `idle_async` over it.
	resultUnread
	// resultRead: the user has seen the turn-end row since the last turn
	// ended. A turn-end row whose result is read is drawn PARTIAL.
	resultRead
)

// String names a result state, for the record.
func (r resultState) String() string {
	switch r {
	case resultNone:
		return "none"
	case resultUnread:
		return "unread"
	case resultRead:
		return "read"
	default:
		return "unknown"
	}
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
	// A NEW TURN ENDS THE LAST ONE'S RETRY: the prompt that opened it is
	// working until the API fails again.
	s.retrying = ""
	s.lastFailure = ladder.NoFailure
	s.lastFailureSignInCures = false
	s.endAnswered = false
	// A NEW TURN ENDS THE LAST ONE'S FAULT (owner ruling, 2026-10-06).
	s.turnFault = ladder.NoTurnFault
	s.compacting = turn.Act == footer.ActCompact
	// A new turn is new foreground work: the detached items announced by the
	// turn before it belong to that turn's account, not this one's. The
	// WATCHER's set is not reset here: an item that outlives the turn that
	// spawned it is still running, and only the watcher knows when it ends.
	s.detached = map[string]struct{}{}
	s.permissions = map[string]struct{}{}
	// A new prompt is the user moving on: whatever the last turn left, read
	// or not, is no longer the result the row reports.
	s.result = resultNone
	s.settleResult()
}

// settleResult records that a live turn event decided the result, which
// retires whatever was seeded from the durable record.
func (s *wsState) settleResult() {
	s.resultSettled = true
	s.restoredEnd = ""
}

// restoreResult seeds the result ONCE from the workspace's durable record,
// while no live turn event has decided it. It is how a daemon that did not see
// a workspace's last turn end -- a successor after a handover, a restart --
// draws the row as it stood (owner report, 2026-09-29: every row read `ready`
// FULL after a deploy). It answers whether it seeded a result.
func (s *wsState) restoreResult(rec wsm.Workspace) bool {
	if s.resultSettled {
		return false
	}
	s.resultSettled = true
	if rec.Result == nil {
		return false
	}
	restored := *rec.Result
	s.persisted = &restored
	s.turnEverRan = true
	s.restoredEnd = string(restored.End)
	s.result = resultUnread
	if restored.Read {
		s.result = resultRead
	}
	return true
}

// resultSnapshot is the durable spelling of the result standing now, nil when
// none does.
func (s *wsState) resultSnapshot() *wsm.TurnResult {
	if s.result == resultNone {
		return nil
	}
	return &wsm.TurnResult{End: wsm.TurnResultEnd(s.turnEndArm()), Read: s.result == resultRead}
}

// sameResult reports whether two durable results are the same.
func sameResult(a, b *wsm.TurnResult) bool {
	if a == nil || b == nil {
		return a == b
	}
	return *a == *b
}

// The three TURN-END arms: how the last turn ended, once nothing more urgent
// stands. They are the only arms that report a RESULT, so they are the only
// arms a result can be unread or read on, and the only arms the viewed marker
// may stand on. `ladder.ResolveTurnEnd` is the one table from a close to its arm, and
// `isTurnEndArm` is the one predicate every site asks, so a completion, an
// interruption and a failure can never drift apart on the read rule.
const (
	armDone        = "done"
	armInterrupted = "interrupted"
	armTurnFailed  = "turn_failed"
)

// isTurnEndArm reports whether arm is one of the three turn-end arms.
func isTurnEndArm(arm string) bool {
	return arm == armDone || arm == armInterrupted || arm == armTurnFailed
}

// readsResult reports whether a viewed report on arm READS the last turn's
// result: a turn-end arm, or an arm a failed turn raised that stands over that
// turn's end — `vendor_blocked` until the block or the turn fault lifts (owner
// rulings, 2026-09-28 and 2026-10-06), `turn_died` until the next turn. The
// PARTIAL marker is still drawn on a turn end alone (viewedOn).
func readsResult(arm string) bool {
	return isTurnEndArm(arm) || arm == "vendor_blocked" || arm == "turn_died"
}

// viewReadsResult reports whether a viewed report on arm READS the last
// turn's result: every arm readsResult names, and `idle_async` while the
// result is unread, because live detached work draws that arm in place of the
// unread turn end (idleArm). The user viewing that row is viewing the
// finished turn, so the row comes back PARTIAL when the work ends.
func (s *wsState) viewReadsResult(arm string) bool {
	return readsResult(arm) || (arm == "idle_async" && s.resultUnreadNow())
}

// turnEndArm names the turn-end arm the last close resolves to, through
// ladder.ResolveTurnEnd — the one table the desktop banner reads too, so the
// row's colour and the banner cannot disagree (an expected stop reads `done`
// on both). A close this build does not know was refused loudly when it was
// installed (SetTurnEnded), so it is never unread; it keeps the row on
// `done`, the arm it has always drawn.
func (s *wsState) turnEndArm() string {
	if s.restoredEnd != "" {
		return s.restoredEnd
	}
	end, ok := ladder.ResolveTurnEnd(s.lastClose, s.lastFailure)
	if !ok {
		return armDone
	}
	return end.String()
}

// noteArm records the arm being published for this workspace and reports
// the arm it replaced and whether it CHANGED.
func (s *wsState) noteArm(arm string) (string, bool) {
	previous := s.lastArm
	changed := s.lastArmSeen && previous != arm
	s.lastArm = arm
	s.lastArmSeen = true
	return previous, changed
}

// viewedOn reports whether a row on arm draws the viewed (PARTIAL) marker. It
// is DERIVED, never stored: the marker stands exactly when the row is on a
// turn-end arm and the last turn's result is read.
//
// VIEWED IS TURN-END-ONLY. "You have already seen this" is a claim about a
// FINISHED turn's result; every other arm is live work (thinking, a
// permission ask, detached work) or an exceptional state (severed, dead,
// vendor_blocked, a merge conflict) that must never be drawn deprioritized,
// however long the user has looked at it. So PARTIAL is unrepresentable on
// any other row.
//
// Deriving it from the read FACT is what keeps a read result read across an
// arm change that is not a new result: a turn-end row the user saw, which
// goes to `idle_async` while detached work runs, comes back PARTIAL when that
// work ends rather than FULL — a full row claiming a result nobody has read.
// A NEW result is a new turn, and the turn's start and end are what reset the
// fact (startTurn, SetTurnEnded), so new activity is still drawn FULL.
func (s *wsState) viewedOn(arm string) bool {
	return isTurnEndArm(arm) && s.result == resultRead
}

// resultUnreadNow reports whether the last turn's result is unread, which
// holds the row on its turn-end arm once no detached work runs.
func (s *wsState) resultUnreadNow() bool {
	return s.result == resultUnread
}

// asyncLive reports whether detached work is running right now. The watcher's
// set is authoritative once it has stated one; until then the announcements
// are all the roster has.
func (s *wsState) asyncLive() bool {
	if s.liveWorkSeen {
		return !s.liveWork.Empty()
	}
	return len(s.detached) > 0
}

// live reports whether a live session backs the workspace right now. It is
// what makes a closed workspace `inactive` — no open perspective and nothing
// running behind it.
func (s *wsState) live(session *wsm.Session) bool {
	// A PARKED session is live: the idle sweep stood the shim down and a
	// prompt brings it back, so the workspace is not "nothing running behind
	// it" in the sense `inactive` means.
	if parked(session) {
		return true
	}
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
	// divergence is each repository's last ahead/behind measure
	// (SetRepoDivergence). A repository with none is drawn pending.
	divergence map[ids.RepoID]repodivergence.Measure
	// renderLogs are the decision records each workspace's row last logged,
	// fingerprinted, so a render logs only the rows that changed (repeats.go).
	renderLogs map[string]string
	// queues are the repositories' merge queues as the roster draws them
	// (SetMergeQueue). Each repository's queue is replaced WHOLE, so a merge
	// that left the queue, or a pause that was lifted, can never linger.
	queues map[wsm.RepoKey]mergeQueue
}

// mergeQueue is one repository's merge queue, indexed for the row lookups.
type mergeQueue struct {
	// paused reports whether the queue is paused.
	paused bool
	// members are the workspaces with a row on the queue.
	members map[ids.WorkspaceID]struct{}
	// async are the workspaces whose own branch has an async merge on it.
	async map[ids.WorkspaceID]struct{}
}

// newMergeQueue indexes one stated queue.
func newMergeQueue(q MergeQueue) mergeQueue {
	return mergeQueue{paused: q.Paused, members: setOf(q.Workspaces), async: setOf(q.AsyncRequesters)}
}

// setOf indexes WS as a set.
func setOf(ws []ids.WorkspaceID) map[ids.WorkspaceID]struct{} {
	set := make(map[ids.WorkspaceID]struct{}, len(ws))
	for _, w := range ws {
		set[w] = struct{}{}
	}
	return set
}

// empty reports whether the queue says nothing a row could draw: not paused,
// and no workspace on it.
func (q mergeQueue) empty() bool {
	return !q.paused && len(q.members) == 0 && len(q.async) == 0
}

// newRosterState builds an empty roster accumulation.
func newRosterState() *rosterState {
	return &rosterState{
		workspaces:  map[ids.WorkspaceID]*wsState{},
		divergence:  map[ids.RepoID]repodivergence.Measure{},
		queues:      map[wsm.RepoKey]mergeQueue{},
	}
}

// asyncMerging reports whether WS's own branch has an async merge on any
// repository's merge queue.
func (r *rosterState) asyncMerging(ws ids.WorkspaceID) bool {
	for _, q := range r.queues {
		if _, ok := q.async[ws]; ok {
			return true
		}
	}
	return false
}

// pausedMergeQueue answers the paused repository queue that holds a merge the
// row of WS draws, and false when none does. The row draws its own merge on
// a `merging` or `merge_queued` ARM, which is the queue WS has a row on, and
// an async merge of its branch when it carries the async_merging marker
// (ASYNC), which is the queue that names WS as a requester. A row drawing
// neither draws no merge for a pause to change.
func (r *rosterState) pausedMergeQueue(ws ids.WorkspaceID, arm string, async bool) (wsm.RepoKey, bool) {
	ownMerge := arm == "merging" || arm == "merge_queued"
	for repo, q := range r.queues {
		if !q.paused {
			continue
		}
		if _, on := q.members[ws]; ownMerge && on {
			return repo, true
		}
		if _, on := q.async[ws]; async && on {
			return repo, true
		}
	}
	return "", false
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
