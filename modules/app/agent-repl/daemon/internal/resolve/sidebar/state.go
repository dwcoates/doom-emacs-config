package sidebar

import (
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/semver"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/streamslot"
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
	// rateLimits is the windows whose newest rate-limit verdict rejected the
	// session; a rate-limit event sets vendorBlocked to whether any stands.
	rateLimits ladder.RateLimitRejections

	// result is whether a turn end stands as the row's last result: SET when
	// a turn COMPLETES, is INTERRUPTED or FAILS (SetTurnEnded), and reset to
	// none by a new turn (startTurn) — a new prompt is the user moving on. It
	// is what holds the row on its turn-end arm, and what the `idle_async`
	// row's unread-turn-end marker names.
	result resultState
	// resultSettled reports that the result has been decided by this
	// resolver: by a live turn event (a turn starting or ending), or by
	// seeding it ONCE from the durable record (restoreResult). A durable
	// record is read only while nothing live has spoken.
	resultSettled bool
	// restoredEnd is the turn-end arm seeded from the durable record, drawn in
	// place of the one lastClose resolves to until a turn of this resolver's
	// own supersedes it. Empty when the result is live.
	restoredEnd string
	// persisted is the result the durable record holds, as far as this
	// resolver knows: what it seeded from, or last reported through the result
	// sink. A render whose result differs reports it (resolver.row).
	persisted *wsm.TurnResult

	// viewedArm is the VIEWED FACT: the status arm the user's interaction
	// viewed the workspace on (SetInteracted), empty when the workspace is not
	// viewed. The row's viewed (PARTIAL) marker is DERIVED from it (`viewed`).
	// ONLY ANOTHER TURN CLEARS IT (owner ruling, 2026-11-24): a turn's start
	// or its end (clearViewedForTurn), never any other status change, so a
	// session that parks, revives, blips its link or is folded away and back
	// keeps the row viewed. The arm is kept as evidence of what was viewed.
	viewedArm string
	// persistedViewed is the viewed arm the durable record holds, as far as
	// this resolver knows; a render whose viewed arm differs reports it
	// (noteViewed).
	persistedViewed string
	// leftAt is when the user last switched away from the workspace, nil
	// until this roster saw them do so; leftFirstViewed reports that the visit
	// left then first marked it viewed, and visitBeganViewed whether the
	// current or last visit began viewed (page.go).
	leftAt           *time.Time
	leftFirstViewed  bool
	visitBeganViewed bool
	// turnEndedAt is when this roster saw the workspace's last turn end
	// (SetTurnEnded), nil until it has; the view exemption measures from the
	// later of it and the durable record's LastTurnEndedAt.
	turnEndedAt *time.Time
	// viewedSettled reports that the viewed fact has been decided: by a live
	// interaction or turn, or by seeding it ONCE from the durable record
	// (restoreViewed).
	viewedSettled bool
	// interactedAt is when the user last interacted with the workspace, as
	// this resolver heard it (SetInteracted); nil until it has. The row
	// states the later of it and the durable record's.
	interactedAt *time.Time
	// reviving reports a revival of this workspace's parked session in
	// flight (SetReviving). It is a marker beside the status, never an arm:
	// it neither changes the arm nor clears the viewed marker.
	reviving bool
	// bringingUp reports a bring-up of this workspace's session under way
	// (SetBringingUp). It is what holds the row's availability at `pending`.
	bringingUp bool
	// carried is the incumbent's arm a JOINING daemon keeps drawing for a row
	// the mirror let go of, until the row's own facts are settled (carry.go);
	// empty when nothing is carried.
	carried string
	// settleAsked reports that the adoption declared the row's facts in
	// (SettleFacts, SettleAllFacts): the carried arm is let go once no
	// bring-up of the row stands either.
	settleAsked bool
	// replacing reports a relaunch replacing the session's shim
	// (SetReplacing), whose stand-down's dead link leaves the session live.
	replacing bool
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
	// openOwnedPullRequest reports that the workspace owns a pull request
	// still open (SetOpenOwnedPullRequest), which the row's
	// frontend.v1.RosterRowOpenOwnedPullRequest marker states.
	openOwnedPullRequest bool
	// contextHeat is the window fill (0..1) of a context that has reached
	// ContextHeatTokens (SetContextFill), nil below it; the row's
	// frontend.v1.RosterRowContextHeat marker states it.
	contextHeat *float64
	// outline is a chat thread's outline, as the chat service last handed
	// it; nil while it has none.
	outline *frontendv1.RosterThreadOutline
}

// resultState is whether a turn end stands as the row's last result.
type resultState int

const (
	// resultNone: no turn end stands — no turn has ended since the last
	// prompt, or the last close was one this build does not know.
	resultNone resultState = iota
	// resultEnded: the last turn COMPLETED, was INTERRUPTED or FAILED. It
	// holds the row on its turn-end arm, except while detached work runs,
	// which draws `idle_async` over it.
	resultEnded
)

// String names a result state, for the record.
func (r resultState) String() string {
	switch r {
	case resultNone:
		return "none"
	case resultEnded:
		return "ended"
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

// liftVendorBlock lifts the vendor block wholesale, every window's
// rate-limit rejection with it, so a later verdict on one window cannot
// re-raise a rejection the lift already answered.
func (s *wsState) liftVendorBlock() {
	s.vendorBlocked = false
	s.rateLimits.Clear()
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
	s.liftVendorBlock()
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
	// A new prompt is the user moving on: whatever the last turn left is no
	// longer the result the row reports.
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
	s.result = resultEnded
	return true
}

// restoreViewed seeds the viewed fact ONCE from the workspace's durable
// record, while no live interaction or turn has decided it, so a daemon that
// did not see the user's interaction draws the row viewed as it stood.
func (s *wsState) restoreViewed(rec wsm.Workspace) {
	if s.viewedSettled {
		return
	}
	s.viewedSettled = true
	s.viewedArm = rec.ViewedStatus
	s.persistedViewed = rec.ViewedStatus
}

// clearViewedForTurn clears the viewed fact because a turn started or ended:
// the one change the user has not seen yet. It answers whether the row was
// viewed.
func (s *wsState) clearViewedForTurn() bool {
	s.viewedSettled = true
	if s.viewedArm == "" {
		return false
	}
	s.viewedArm = ""
	return true
}

// resultSnapshot is the durable spelling of the result standing now, nil when
// none does.
func (s *wsState) resultSnapshot() *wsm.TurnResult {
	if s.result == resultNone {
		return nil
	}
	return &wsm.TurnResult{End: wsm.TurnResultEnd(s.turnEndArm())}
}

// sameResult reports whether two durable results are the same.
func sameResult(a, b *wsm.TurnResult) bool {
	if a == nil || b == nil {
		return a == b
	}
	return *a == *b
}

// The three TURN-END arms: how the last turn ended, once nothing more urgent
// stands. They are the only arms that report a RESULT. `ladder.ResolveTurnEnd`
// is the one table from a close to its arm, and `isTurnEndArm` is the one
// predicate every site asks, so a completion, an interruption and a failure
// can never drift apart.
const (
	armDone        = "done"
	armInterrupted = "interrupted"
	armTurnFailed  = "turn_failed"
)

// isTurnEndArm reports whether arm is one of the three turn-end arms.
func isTurnEndArm(arm string) bool {
	return arm == armDone || arm == armInterrupted || arm == armTurnFailed
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

// viewed reports whether the row draws the viewed (PARTIAL) marker: the user
// interacted with the workspace since its last turn started or ended. Every
// arm can be viewed: "you have interacted with this since its last turn" is as
// true of a running turn as of a finished one, and a working workspace stays
// visible whatever this says (visibility.go).
func (s *wsState) viewed() bool {
	return s.viewedArm != ""
}

// interacted records that the user interacted with the workspace at AT, on
// the arm the row draws now, and views the row unless EXEMPT (ViewExemption).
// It answers whether the viewed fact changed. An interaction before any arm
// has been published views nothing: there is no row the user could have
// interacted with. The instant is recorded either way, so the interaction
// still counts toward the user's recency.
func (s *wsState) interacted(at time.Time, exempt bool) bool {
	if s.interactedAt == nil || at.After(*s.interactedAt) {
		s.interactedAt = &at
	}
	if exempt {
		return false
	}
	s.viewedSettled = true
	if !s.lastArmSeen || s.viewedArm != "" {
		return false
	}
	s.viewedArm = s.lastArm
	return true
}

// lastTurnEnd answers when the workspace's last turn ended: the later of the
// end this roster saw (s.turnEndedAt) and the durable record's (RECORDED),
// nil when neither knows one. The view exemption and the allocation's clocks
// all read it, so a daemon that did not see the end measures from the record.
func (s *wsState) lastTurnEnd(recorded *time.Time) *time.Time {
	ended := s.turnEndedAt
	if recorded != nil && (ended == nil || recorded.After(*ended)) {
		ended = recorded
	}
	return ended
}

// ViewExemption is how long after a turn ends the user's interaction with the
// workspace marks nothing viewed (owner ruling, 2026-11-24): a result read in
// the minutes after it lands stays unread, so a fold keeps drawing it and its
// page stays loaded, until the user interacts with it again after the window.
// It is a product rule, not a bound on any operation.
const ViewExemption = 10 * time.Minute

// viewExempt reports whether an interaction at AT falls inside the view
// exemption of the workspace's last turn end: the later of the one this
// roster saw (s.turnEndedAt) and the durable record's (RECORDED). A running
// turn is never exempt: there is no result to keep unread.
func (s *wsState) viewExempt(at time.Time, recorded *time.Time) (time.Time, bool) {
	if s.turn != nil {
		return time.Time{}, false
	}
	ended := s.lastTurnEnd(recorded)
	if ended == nil || at.Before(*ended) {
		return time.Time{}, false
	}
	return *ended, at.Sub(*ended) < ViewExemption
}

// resultEndedNow reports whether a turn end stands, which holds the row on
// its turn-end arm once no detached work runs.
func (s *wsState) resultEndedNow() bool {
	return s.result == resultEnded
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
	if s.linkSeen && s.link == shimclient.LinkDead && !s.replacing {
		return false
	}
	return s.started || s.linkSeen || session != nil
}

// rosterState is the whole roster accumulation: the durable registry, every
// workspace's live half, and the selection.
type rosterState struct {
	// reg is the last registry snapshot, whole.
	reg Registry
	// visibility is every workspace's visibility as the last render derived
	// it (resolveVisibility); visibilityKept is the value last handed to the
	// visibility sink, or read from the registry, per workspace.
	visibility     map[ids.WorkspaceID]wsm.Visibility
	visibilityKept map[ids.WorkspaceID]wsm.Visibility
	// page is every workspace's page verdict as the last render derived it
	// (resolvePages): true when the editor holds its page.
	page map[ids.WorkspaceID]bool
	// shimHold is every workspace's shim hold as the last render derived it
	// (resolveShimHolds).
	shimHold map[ids.WorkspaceID]wsm.ShimHold
	// regSeen reports whether any registry has been installed. Nothing
	// publishes before one: the roster is a view OF the registry, and a roster
	// built from live frames alone would name no workspaces.
	regSeen bool
	// workspaces are the live halves, by workspace.
	workspaces map[ids.WorkspaceID]*wsState
	// selected is the user's selection, which the daemon stamps the moment
	// SelectWorkspace arrives rather than waiting for WSM to echo it back.
	selected *ids.WorkspaceID
	// stamp is the select that made the selection current, nil when no
	// select of this daemon's did (stampedSelection).
	stamp *stampedSelection
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
	// github is the GitHub merge queues' share (Resolver.SetGithubQueue), in
	// queue order.
	github []GithubQueueEntry
	// offQueue are the merges off their queues (Resolver.SetOffQueue),
	// oldest departure first.
	offQueue []OffQueueEntry
	// gates are the shown cold gates by workspace (SetColdGate), each
	// carried to the webhost's dock while its workspace is listed open.
	gates map[ids.WorkspaceID]coldGate
	// version is the agent-repl release the rail's header states
	// (DeployedVersion), nil while none is resolved.
	version *semver.Version
	// incumbentArms are the incumbent's own status arms for the workspaces
	// a joining daemon has not adopted yet (SetIncumbentArms), replaced
	// WHOLE; nil outside a handover's joining window.
	incumbentArms map[ids.WorkspaceID]string
	// recency is the connected Emacs's recency reach (AttachRecencyReach,
	// ReportRecencyReach), scoped to its WatchDaemon stream.
	recency streamslot.Slot[RecencyReach]
	// index is what the roster derived from reg at its install (kept.go).
	index registryIndex
	// kept are the facts each workspace's last render kept, and epoch the
	// version every workspace's kept facts share (kept.go).
	kept  map[ids.WorkspaceID]*kept
	epoch uint64
}

// stampedSelection is a select's stamp with the workspace it selected: the
// roster states the stamp only while that workspace is the selection.
type stampedSelection struct {
	ws    ids.WorkspaceID
	stamp SelectStamp
}

// coldGate is one workspace's shown cold gate: its feed row id and card.
type coldGate struct {
	row  *frontendv1.FeedId
	gate *frontendv1.FeedColdGate
}

// mergeQueue is one repository's merge queue, indexed for the row lookups.
type mergeQueue struct {
	// pause is the queue's standing pause, nil while it runs.
	pause *wsm.MergeQueuePause
	// deployCheck is the queue's hold for its deploy check, nil for none.
	deployCheck *DeployCheckHold
	// members are the workspaces with a row on the queue.
	members map[ids.WorkspaceID]struct{}
	// async are the workspaces whose own branch has an async merge on it.
	async map[ids.WorkspaceID]struct{}
	// line is the merges in line, front first (MergeQueue.Line).
	line []QueueEntry
	// remediating are the merges remediating out of line
	// (MergeQueue.Remediating).
	remediating []QueueEntry
	// mediation is the deploy mediation the queue waits on, nil for none.
	mediation *QueueMediation
	// deploying is the landed merge still deploying (MergeQueue.Deploying),
	// nil for none.
	deploying *QueueEntry
}

// newMergeQueue indexes one stated queue.
func newMergeQueue(q MergeQueue) mergeQueue {
	return mergeQueue{pause: q.Pause, deployCheck: q.DeployCheck, members: setOf(q.Workspaces), async: setOf(q.AsyncRequesters), line: q.Line, remediating: q.Remediating, mediation: q.Mediation, deploying: q.Deploying}
}

// brokenHold names a broken hold: the landed merge still deploying, which
// holds the queue's head until its deploy's check ends, and the merge in
// line admitted beside it.
type brokenHold struct {
	deploying ids.WorkspaceID
	admitted  ids.WorkspaceID
}

// brokenHold answers the queue's broken hold, and whether it holds one: a
// landed merge still deploying while the front of the line is admitted.
func (q mergeQueue) brokenHold() (brokenHold, bool) {
	if q.deploying == nil || len(q.line) == 0 || !q.line[0].Admitted {
		return brokenHold{}, false
	}
	return brokenHold{deploying: q.deploying.Workspace, admitted: q.line[0].Workspace}, true
}

// setOf indexes WS as a set.
func setOf(ws []ids.WorkspaceID) map[ids.WorkspaceID]struct{} {
	set := make(map[ids.WorkspaceID]struct{}, len(ws))
	for _, w := range ws {
		set[w] = struct{}{}
	}
	return set
}

// empty reports whether the queue says nothing the roster could draw: not
// paused, and no workspace on it.
func (q mergeQueue) empty() bool {
	return q.pause == nil && q.deployCheck == nil && len(q.members) == 0 && len(q.async) == 0 && len(q.line) == 0 && len(q.remediating) == 0 && q.mediation == nil && q.deploying == nil
}

// newRosterState builds an empty roster accumulation.
func newRosterState() *rosterState {
	return &rosterState{
		workspaces: map[ids.WorkspaceID]*wsState{},
		divergence: map[ids.RepoID]repodivergence.Measure{},
		gates:      map[ids.WorkspaceID]coldGate{},
		queues:     map[wsm.RepoKey]mergeQueue{},
		kept:       map[ids.WorkspaceID]*kept{},
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

// paused reports whether the queue draws as paused: a standing pause, or its
// deploy check's hold.
func (q mergeQueue) paused() bool { return q.pause != nil || q.deployCheck != nil }

// pausedMergeQueue answers the paused repository queue that holds a merge the
// row of WS draws, and false when none does. The row draws its own merge
// (OWNMERGE, ownMergeDrawn), which is on the queue WS has a row on, and an
// async merge of its branch when it carries the async_merging marker (ASYNC),
// which is the queue that names WS as a requester. A row drawing neither
// draws no merge for a pause to change.
func (r *rosterState) pausedMergeQueue(ws ids.WorkspaceID, ownMerge, async bool) (wsm.RepoKey, bool) {
	for repo, q := range r.queues {
		if !q.paused() {
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

// turnInFlight reports whether WS has an accepted turn in flight, false for
// a workspace with no live half yet.
func (r *rosterState) turnInFlight(ws ids.WorkspaceID) bool {
	s, ok := r.workspaces[ws]
	return ok && s.turn != nil
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
	return r.index.sessions
}
