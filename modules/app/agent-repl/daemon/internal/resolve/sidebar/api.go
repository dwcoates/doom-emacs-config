// Package sidebar is the roster resolver: both groupings, priority order, the
// attention marker, recently merged, the current workspace and each row's
// status arm.
//
// A merged, closed or KILLED workspace's row carries closed = true; a nuked
// workspace LEAVES the roster. The attention marker is set on a notification
// and cleared on SelectWorkspace or on the last open ask settling — the two
// ways a notification becomes SEEN. See ARCHITECTURE.md "resolvers".
//
// ONE GLOBAL ROSTER. The roster is editor-global: one accumulation, one topic,
// one stream serving every webview alike. Per-workspace session facts arrive
// through the sink and are folded into that one accumulation.
package sidebar

import (
	"fmt"
	"strconv"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/deployedversion"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/viewquiet"
	"agent-repld/internal/vocab"
	"agent-repld/internal/wsm"
)

// Registry is the WSM-derived half of the roster: everything the durable
// records say. The resolver re-renders on any WSM change.
type Registry struct {
	// Workspaces are every registered workspace.
	Workspaces []wsm.Workspace
	// Repositories are every repository, for the repo grouping.
	Repositories []wsm.Repository
	// Subsections are every repository's subsections, for the repo grouping
	// and the tab bar's order (arrangement.go). Read in one snapshot with
	// Workspaces and Repositories (wsm.ReadArrangement).
	Subsections []wsm.Subsection
	// RepositoryBlocks are every parent's arranged order of its
	// other-repository blocks (wsm's repositoryblocks.go), read in the same
	// snapshot.
	RepositoryBlocks []wsm.RepositoryBlock
	// Tasks are every task, for the task grouping.
	Tasks []wsm.Task
	// Sessions are the durable session records, one per workspace that has
	// ever had a session. They answer two things no live frame can: that a
	// workspace has NEVER had a session (the `none` arm, which is an assertion
	// rather than the absence of one), and that a session was KILLED, which
	// recedes the row exactly as a close or a merge does.
	Sessions []wsm.Session
	// Current is the selected workspace, nil when none is.
	Current *ids.WorkspaceID
	// View is the sidebar's durable view state that belongs to no row: the
	// recently-merged band's fold and the grouping every page shows
	// (wsm.DefaultSidebarView when nobody has changed it). Its grouping is
	// always one of the two; SetRegistry refuses anything else.
	View wsm.SidebarView
}

// SelectStamp is what the roster states about the select that made a
// workspace current (frontend.v1.RosterCurrentWorkspace.selection_id and
// .switch_started_at_ms). The zero stamp states nothing.
type SelectStamp struct {
	// ID is the select's own id, minted per select.
	ID string
	// SwitchStartedAt is when the user's switch began, nil when the select
	// carried no instant.
	SwitchStartedAt *time.Time
}

// TurnStarted is the daemon's own fact that StartTurn was ACCEPTED. It aliases
// the footer's spelling so the two views cannot disagree about what a turn is
// or what it carries; nothing on the shim's streams states it, because the
// first frame of a turn is an activity, by which time `submitting` is over.
type TurnStarted = footer.TurnStarted

// LiveWorkSet is the watcher's authoritative set of live detached work. It
// aliases the watcher's spelling so the roster's `idle_async` arm and the
// freeness answer cannot disagree about what is still running.
type LiveWorkSet = sessionwatcher.LiveWorkSet

// TurnClose is how a turn ended. It aliases the watcher's spelling, which
// aliases WSM's, so the durable record and the roster's dot cannot disagree.
type TurnClose = sessionwatcher.TurnClose

// Resolver is the roster's whole surface. The roster is EDITOR-GLOBAL: one
// stream serves every webview alike.
type Resolver interface {
	sessionwatcher.SidebarSink

	// SetRegistry installs the durable half, called on any WSM change.
	SetRegistry(reg Registry)
	// Visibility answers a workspace's visibility as the last render derived
	// it (visibility.go): the value the roster just published.
	Visibility(ws ids.WorkspaceID) wsm.Visibility
	// SetMerge installs one workspace's merge facts, which drive its merge
	// status arm and its glyph.
	SetMerge(ws ids.WorkspaceID, facts footer.MergeFacts)
	// SetMergeQueue installs, WHOLE, REPO's merge queue as the roster draws
	// it: whether it is paused, the workspaces with a row on it, the merges
	// in line, and the workspaces whose own branch has an async merge on it.
	// The merge
	// orchestrator is the one caller, on every change of the queue's rows and
	// of its pause.
	SetMergeQueue(repo wsm.RepoKey, queue MergeQueue)
	// SetGithubQueue installs, WHOLE, every workspace whose owned pull request
	// stands in a GitHub merge queue, in queue order: each is drawn in its
	// repository's Merge Queue group at its GitHub place
	// (frontend.v1.RosterRepoMergeQueue.github). The merge orchestrator is
	// the one caller (merge.GithubDeployment).
	SetGithubQueue(entries []GithubQueueEntry)
	// SetOffQueue installs, WHOLE, every merge that left its queue without
	// landing and whose request still stands, oldest departure first: each is
	// drawn in its workspace's repository's Merge Queue group, with its
	// requeue and discard controls (frontend.v1.RosterRepoMergeQueue
	// .off_queue). The merge orchestrator is the one caller.
	SetOffQueue(entries []OffQueueEntry)
	// SetColdGate installs WS's shown cold gate as the webhost's dock draws
	// it (WorkspaceRoster.cold_gates): its feed row id and standing card, or
	// nil for both when none is shown. The fleet is the one caller, on every
	// edge of the gate.
	SetColdGate(ws ids.WorkspaceID, row *frontendv1.FeedId, gate *frontendv1.FeedColdGate)
	// AttachRecencyReach stands an Emacs WatchDaemon stream's recency reach,
	// empty until its first report, and answers the release the stream calls
	// when it ends. A later stream supersedes an earlier one, so a stale
	// stream's release leaves its successor's reach standing.
	AttachRecencyReach() (release func())
	// ReportRecencyReach replaces, WHOLE, the standing stream's recency reach
	// (agentrepl.v1.ReportRecencyReach): each open row whose workspace it
	// names carries that gutter (frontend.v1.RosterOpenRow.recency_gutter),
	// and every other row none. ErrNoEmacsStream when no stream stands.
	ReportRecencyReach(reach RecencyReach) error
	// SetSelected records the user's selection, which also clears that
	// workspace's attention marker.
	SetSelected(ws ids.WorkspaceID)
	// SetRegistrySelected is SetRegistry and then SetSelected in ONE
	// mutation and ONE push: a select's republished registry and the
	// selection reach every client together. STAMP is what the roster
	// states about the select that made ws current; the zero stamp is a
	// selection no select of this daemon's made.
	SetRegistrySelected(reg Registry, ws ids.WorkspaceID, stamp SelectStamp)
	// SetSelection installs a select's change, read back in the store's own
	// write (wsm.Selection), into the registry the roster holds, and the
	// selection with STAMP, in ONE mutation and ONE push, with NO registry
	// read: a switch reaches every client without waiting on the whole
	// registry. It answers false, installing nothing, when the registry the
	// roster holds cannot take the change -- none installed yet, or one
	// carrying no row, repository or subsection the change names -- and the
	// caller installs a whole registry instead (SetRegistrySelected).
	SetSelection(sel wsm.Selection, stamp SelectStamp) bool
	// Selected answers the workspace the roster's selection names right now,
	// and false when it names none. It lets a caller ask whether the roster
	// already agrees with the selection WSM stores before pushing a roster
	// that would change nothing.
	Selected() (ids.WorkspaceID, bool)
	// Row answers one workspace's row exactly as the last push carries it,
	// with that push's revision: both taken from the SAME push under the lock
	// that resolved it, so the row is never older or newer than the revision
	// it is stated with. A MERGED workspace, which no stream carries, is
	// answered the row a page of merged rows resolves for it (MergedPage).
	// False when no push was resolved yet or the workspace is in none of its
	// streams and not merged.
	Row(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool)
	// Thread reports whether the last push lists WS as a chat thread, with
	// its revision.
	Thread(ws ids.WorkspaceID) (uint64, bool)
	// Revision answers the revision of the push last resolved (frontend.v1
	// RosterPush.revision), zero before any: every push at that revision or
	// later carries every change made before the call.
	Revision() uint64
	// MergedPage answers one page of merged workspaces' rows
	// (agentrepl.v1.ListMergedWorkspaces), resolved on demand under the lock
	// from the merged set the last push was resolved from, with that set's
	// token. The rows are READS: resolving them feeds no durable sink and no
	// departure. ErrMergedNotReady before any registry was published, and an
	// *UnknownRepositoryError for a repository the registry does not hold.
	MergedPage(q MergedPageQuery) (MergedPage, error)
	// MergedRows answers every merged workspace's row as a page resolves it,
	// in registry order, with the last push, both under one lock, so no
	// workspace is in both. False before any push.
	MergedRows() ([]*frontendv1.RosterRow, *frontendv1.RosterPush, bool)
	// SetViewed records that the user has READ the last turn's result — the
	// editor's report that the user has now SEEN this workspace. It takes
	// only on a turn-end row (done, interrupted or turn_failed), or on an
	// idle_async row over an unread turn end; a report on any other arm is
	// dropped. A read result draws the row PARTIAL whenever it stands on its
	// turn-end arm. There is no lowering setter: the next turn is what makes
	// a new result unread.
	SetViewed(ws ids.WorkspaceID)
	// SetViewDwell records that the editor started counting a dwell of
	// LENGTH on the workspace, as of the roster's clock now, replacing any
	// standing one. The row carries it (frontend.v1.RosterRowViewDwell) only
	// while the dwell will recede the row's name: the row is selected, on a
	// turn-end arm, with its result unread. A viewed report (SetViewed), a
	// stop (StopViewDwell), the selection leaving the workspace and a status
	// change each end it. LENGTH is positive: the caller refuses any other.
	SetViewDwell(ws ids.WorkspaceID, length time.Duration)
	// StopViewDwell ends the workspace's standing dwell, if any: the editor
	// stopped counting it before it completed.
	StopViewDwell(ws ids.WorkspaceID)
	// SetStateUnreported installs, or lifts, the fact that a shim taken back
	// after a failed handover has not re-reported its session state: the
	// degraded rung, drawn `degraded`.
	SetStateUnreported(ws ids.WorkspaceID, unreported bool)
	// SetReviving raises (true) or lowers (false) the workspace's REVIVING
	// marker: its parked session is being brought back up. The workspace
	// verbs raise it when they decide to revive and lower it when that revival
	// ends, success or failure; it is not a status arm and clears nothing.
	SetReviving(ws ids.WorkspaceID, reviving bool)
	// SetBringingUp raises (true) or lowers (false) the fact that a bring-up
	// of the workspace's session is under way: the boot raises it for every
	// workspace it names for bring-up before it serves, and every start
	// raises it for its own duration. While it stands, and no shim link has
	// connected yet, the row's availability is `pending`.
	SetBringingUp(ws ids.WorkspaceID, bringingUp bool)
	// SetOpening raises (true) or lowers (false) the fact that an OpenTabs in
	// flight names the workspace and has not answered it `opened` yet. While
	// it stands the row draws `inactive` with its `opening` marker, over every
	// other arm. The OpenTabs
	// coordinator is the one caller.
	SetOpening(ws ids.WorkspaceID, opening bool)
	// SetThreadOutline installs a chat thread's outline (the chat service's,
	// chat.Deps.Outline), nil once it has none. The current thread's row
	// carries it (frontend.v1.RosterThreadRow.outline).
	SetThreadOutline(ws ids.WorkspaceID, outline *frontendv1.RosterThreadOutline)
	// SetVendorStart installs where the workspace's vendor-start run stands
	// (the fleet's vendor-start faults). Any run standing draws `vendor_fault`
	// (turquoise); the footer carries which of the three steps it is (owner
	// ruling, 2026-10-02).
	SetVendorStart(ws ids.WorkspaceID, state VendorStart)
	// NetworkFaultOpened installs one standing fault that says this machine
	// cannot reach the network, by its record id: the `network_fault` arm. It
	// is driven from the ONE place faults are opened (health.ObserveFaults),
	// beside the footer's OpenFault, so the two surfaces take the same fact.
	NetworkFaultOpened(ws ids.WorkspaceID, id string)
	// FaultClosed retracts a standing fault by its record id. An id the roster
	// does not hold is a fault it does not draw, and changes nothing.
	FaultClosed(ws ids.WorkspaceID, id string)
	// SetTurn installs the accepted turn, nil when none is in flight. It is
	// what raises `submitting` the instant StartTurn is accepted, and what
	// tells a `/clear` and a compaction apart from an ordinary prompt — the
	// three draw different dots and only the daemon knows which act it sent.
	SetTurn(ws ids.WorkspaceID, turn *TurnStarted)
	// AckTurn records that the SHIM has taken the turn, which is what ends the
	// row's `submitting` window. The shim's answer to StartTurn is the ack; a
	// turn that then produces no activity at all still leaves `submitting`,
	// which names a window that is over.
	AckTurn(ws ids.WorkspaceID)
	// SetTurnEnded installs how the last turn ended, which is what tells
	// `done`, `interrupted` and `turn_failed` apart. No agent terminal states it: a user interrupt
	// is a DAEMON fact, so the roster is told directly.
	SetTurnEnded(ws ids.WorkspaceID, how TurnClose)
	// OnSignedIn takes a completed sign-in for the workspace's account root:
	// a last turn that failed on an account cause a sign-in cures is answered,
	// and the row draws `ready` until the next turn.
	OnSignedIn(ws ids.WorkspaceID)
	// SetSummary installs the row detail's summary line — the workspace's last
	// prompt, first line only. Empty omits the line rather than drawing it
	// blank.
	SetSummary(ws ids.WorkspaceID, text string)
	// SetOpenOwnedPullRequest installs whether the workspace owns a pull
	// request that is still open (wsm.OwnedPullRequestOpen), which the row's
	// open-owned-pull-request marker states. Its sole producer is the
	// workspace verbs, at registration, on every ChangeWorkspaceLinks, and on
	// every end of a followed pull request.
	SetOpenOwnedPullRequest(ws ids.WorkspaceID, open bool)
	// SetRepoDivergence installs a repository's latest ahead/behind measure
	// (repodivergence), drawn as its section label's hover. A repository never
	// measured is drawn pending.
	SetRepoDivergence(repo ids.RepoID, measure repodivergence.Measure)
	// FooterStatusChanged redraws the roster because the footer's status for
	// the workspace moved: the row's status is a projection of it.
	FooterStatusChanged(ws ids.WorkspaceID)
	// SetIncumbentArms installs, WHOLE, the status arm the INCUMBENT's own
	// roster draws for each workspace this JOINING daemon has not adopted
	// yet: each named row draws that arm in place of the one this daemon would
	// project from its own footer, whose state for a workspace it does not
	// serve yet is frozen at what it last read. An empty or nil map is no
	// mirror, and every row is this daemon's own again. The roster mirror
	// (internal/rostermirror) is the one caller; it names exactly the
	// workspaces whose standing is not_yet_adopted, so a row the daemon owns
	// is never drawn from another daemon.
	SetIncumbentArms(arms map[ids.WorkspaceID]string)
	// DeployedVersion installs the agent-repl version the daemon deploys
	// from, drawn in the rail's header as its bare release
	// (frontend.v1.WorkspaceRoster.version); nil withdraws it. The deployed
	// version's loop is the one caller.
	deployedversion.Sink
	// Topic is the one editor-global roster publication: each value a
	// whole RosterPush, EVERY STREAM SET, and a stream unchanged since the
	// value before carrying that value's own message, so a subscriber tells
	// what changed by pointer.
	Topic() *publish.Topic[*frontendv1.RosterPush]
}

// MergeQueue is one repository's merge queue as the roster draws it. It is
// stated whole on every change, so nothing a queue no longer says can linger
// in the roster.
type MergeQueue struct {
	// Pause is the queue's standing pause, nil while it runs. A row whose
	// merge is on a paused queue carries the `merge_paused` marker beside its
	// merge indicator (frontend.v1.RosterRowMergePaused), and the queue's
	// group in the Merge Queue section draws it (frontend.v1.RosterMergeQueuePaused).
	Pause *wsm.MergeQueuePause
	// Workspaces are the workspaces with a row on the queue, in queue order:
	// the queue a `merging` or `merge_queued` row's own merge is on.
	Workspaces []ids.WorkspaceID
	// Line is the merges IN LINE, front first: the running merge (when one
	// runs) first, then every waiting merge in the order it will run --
	// Workspaces less the merges only requested. A repository section
	// showing its merge queue draws the first as its head and the rest as
	// its waiting merges (frontend.v1.RosterRepoMergeQueue).
	Line []QueueEntry
	// Remediating are the merges OUT OF LINE (owner rulings, 2026-10-31 and
	// 2026-11-11), in the order they stepped aside, each carrying the repair
	// slot it holds or awaits (QueueEntry.Repair): drawn as the queue's two
	// repair slots and the merges awaiting each
	// (frontend.v1.RosterRepoMergeQueue.fixing_tests and its siblings).
	Remediating []QueueEntry
	// AsyncRequesters are the workspaces whose own branch has an async merge
	// on the queue, from the moment the async request is recorded until its
	// merge leaves the queue. Each row carries the `async_merging` marker
	// beside its own status arm.
	AsyncRequesters []ids.WorkspaceID
	// Mediation is the deploy mediation the queue waits on, nil while no
	// deploy owes one (frontend.v1.RosterRepoMergeQueue.mediation).
	Mediation *QueueMediation
}

// LineWorkspaces answers the workspace each merge in line runs in, front
// first.
func (q MergeQueue) LineWorkspaces() []ids.WorkspaceID {
	out := make([]ids.WorkspaceID, 0, len(q.Line))
	for _, entry := range q.Line {
		out = append(out, entry.Workspace)
	}
	return out
}

// GithubQueueEntry is one workspace standing in a GitHub merge queue through
// the pull request it owns.
type GithubQueueEntry struct {
	Workspace ids.WorkspaceID
	// Place is the pull request's 1-based place in its GitHub queue; Total
	// the queue's length, every pull request included.
	Place, Total int
	// QueuedAt is when the pull request joined the queue.
	QueuedAt time.Time
}

// OffQueueEntry is one merge off its queue, as its repository's Merge Queue
// group draws it.
type OffQueueEntry struct {
	// Workspace is the merge's own workspace (an async merge's merge
	// workspace): what its requeue and discard controls name, and the
	// workspace whose repository places it.
	Workspace ids.WorkspaceID
	// Credited is the workspace a click on its name selects: Workspace for a
	// workspace's own merge, the requester for an async merge.
	Credited ids.WorkspaceID
	// Async reports an async merge, drawn under Workspace's own name.
	Async bool
	// Cause is the sidebar's words for why it left
	// (frontend.v1.RosterMergeQueueOffQueueCause).
	Cause string
}

// QueueEntry is one merge in line, as a repository section showing its merge
// queue draws it.
type QueueEntry struct {
	// Workspace is the workspace the merge runs in: an async merge's merge
	// workspace once it exists. The roster reads its merge facts.
	Workspace ids.WorkspaceID
	// Credited is the workspace the roster credits the merge to, which a
	// click on it selects: Workspace itself for a workspace landing its own
	// branch, and the requester for an async merge, which is never drawn as
	// its hidden merge workspace's row.
	Credited ids.WorkspaceID
	// Async reports whether the merge is an async merge.
	Async bool
	// AsyncName is an async merge's OWN name, which it is drawn under in
	// place of Credited's (frontend.v1.RosterMergeQueueSubject.name): its
	// merge workspace's name, or before that workspace is forked, the name
	// its request reserved for it. Set iff Async is: a sync merge is drawn
	// under Credited's name.
	AsyncName string
	// Reveal is the merge bubble's head row on Credited's root feed, nil when
	// the merge has no bubble identity in this daemon.
	Reveal *frontendv1.FeedId
	// Admitted reports that the merge's run was admitted: it is being worked
	// on rather than waiting.
	Admitted bool
	// Stages are the merge's stages so far, read from its ledger
	// (frontend.v1.RosterMergeQueueHead.stages). Only the front of the line
	// carries them.
	Stages []*frontendv1.RosterMergeQueueStage
	// Clock is how long the merge has been active, or has waited, durable on
	// its queue entry (wsm.MergeQueueEntry.Clock): every line's clock is
	// drawn from it (frontend.v1.RosterMergeQueueHead.clock and its
	// siblings).
	Clock wsm.MergeClock
	// Repair is the repair slot a merge out of line holds or awaits. Every
	// merge out of line carries one; a merge in line carries RepairNone.
	Repair QueueRepair
}

// QueueRepair is the repair slot a merge out of line holds or awaits: the
// store's stamped slot (wsm.MergeSlot) as the roster draws it.
type QueueRepair int

// The repair slots.
const (
	// RepairNone is a merge in line.
	RepairNone QueueRepair = iota
	// RepairFixingTests holds the fixing-tests slot.
	RepairFixingTests
	// RepairResolvingConflicts resolves its rebase's conflicts, beside every
	// other merge doing so.
	RepairResolvingConflicts
	// RepairAwaitingFixingTests awaits the fixing-tests slot.
	RepairAwaitingFixingTests
)

// String names the repair in log records.
func (r QueueRepair) String() string {
	switch r {
	case RepairNone:
		return "none"
	case RepairFixingTests:
		return "fixing_tests"
	case RepairResolvingConflicts:
		return "resolving_conflicts"
	case RepairAwaitingFixingTests:
		return "awaiting_fixing_tests"
	}
	return fmt.Sprintf("repair(%d)", int(r))
}

// FooterStatus is the footer resolver's answers to what a workspace's strip
// shows: the status every roster row projects (footer.Resolver.Status), and
// the sync merge start and turn clock the open workspaces section's
// when-column counts from (footer.Resolver.MergeStartedAt and TurnClock).
type FooterStatus interface {
	Status(ws ids.WorkspaceID) *frontendv1.FooterStatus
	MergeStartedAt(ws ids.WorkspaceID) *int64
	TurnClock(ws ids.WorkspaceID) *frontendv1.FooterClock
}

// New builds the roster resolver. colors supplies the roster_status and
// merge_glyphs tables, which the resolver asserts its arms against; footer is
// the status every row projects, and the footer's WithStatusChanged sink is
// the resolver's FooterStatusChanged.
func New(colors vocab.RenderColors, log dlog.Surfaces, footer FooterStatus, opts ...Option) (Resolver, error) {
	r, err := newResolver(colors, log, footer)
	if err != nil {
		return nil, err
	}
	for _, opt := range opts {
		opt(r)
	}
	return r, nil
}

// ResultSink is told every change of a workspace's last turn result: how its
// last turn ended and whether the user has seen it, nil when none stands. The
// daemon keeps it durable (wsm.SetResult), so a daemon that did not see the
// turn end draws the row as it stood. It is called off the resolver's lock,
// in the report every caller of a combined commit waits for, so it returns at
// once: the daemon's only submits the change (rosterWrites).
type ResultSink func(ws ids.WorkspaceID, result *wsm.TurnResult)

// StatusChangeSink is told every instant a workspace's published status arm
// moves. The daemon keeps it durable (wsm.SetStatusChangedAt), so a daemon
// that did not see the change still ranks the row by it
// (frontend.v1.RosterRowLastUpdated). It is called off the resolver's lock,
// in the same report, and returns at once just as the ResultSink does.
type StatusChangeSink func(ws ids.WorkspaceID, at time.Time)

// Option adjusts the resolver.
type Option func(*resolver)

// WithResultSink installs the sink the resolver tells its result changes.
func WithResultSink(sink ResultSink) Option {
	return func(r *resolver) { r.results = sink }
}

// WithQuiet installs the lid-closed quiet state's view gate: while it stands
// the roster is neither resolved nor published, its rows' facts still are, and
// leaving it publishes the current roster once (viewquiet). Row, Thread and
// Revision answer the last published roster meanwhile, which only the editor
// reads, and the editor is idle while the lid is closed.
func WithQuiet(g *viewquiet.Gate) Option {
	return func(r *resolver) {
		r.quiet = g
		g.Follow(QuietViewName, r.followQuiet)
	}
}

// WithStatusChangeSink installs the sink the resolver tells its status changes.
func WithStatusChangeSink(sink StatusChangeSink) Option {
	return func(r *resolver) { r.statusChanges = sink }
}

// VendorStart is where a workspace's vendor-start run stands, as the roster
// reads it.
type VendorStart int

const (
	// VendorStartNone is no vendor-start failure standing.
	VendorStartNone VendorStart = iota
	// VendorStartRetrying is a run of retryable failures being retried.
	VendorStartRetrying
	// VendorStartStopped is a rejection, or a run whose window ran out:
	// nothing retries until a restart.
	VendorStartStopped
)

// String names the state for a record.
func (v VendorStart) String() string {
	switch v {
	case VendorStartNone:
		return "none"
	case VendorStartRetrying:
		return "retrying"
	case VendorStartStopped:
		return "stopped"
	default:
		return "vendor_start(" + strconv.Itoa(int(v)) + ")"
	}
}

// QueueMediation is a deploy check a queue waits on: the repository whose
// queue it holds, where the check stands, and the workspaces at work on it.
type QueueMediation struct {
	Repo  ids.RepoID
	Stage MediationStage
	// Mediator is the repository's deploy mediator, the label's target at
	// every stage; empty only while the repository's first check rolls out,
	// before the mediator is made.
	Mediator ids.WorkspaceID
}

// MediationStage is where a deploy check stands, as its label draws it.
type MediationStage int

const (
	// MediationWaiting: a deploy rolls out, a landing's or a landed fix's
	// (`HOT-LOADING ZETTEL`).
	MediationWaiting MediationStage = iota + 1
	// MediationChecking: the mediator checks it (`ASSESSING ZETTEL HEALTH`).
	MediationChecking
	// MediationFixing: the mediator fixes it, or what its own finish's gate
	// failed on (`FIXING ZETTEL`).
	MediationFixing
	// MediationTesting: the mediator's finished fix runs its full gate and
	// lands while the mediator stands by (`TESTING ZETTEL FIXES`).
	MediationTesting
	// MediationEscalated: the check was escalated, and the queue waits on the
	// user until the mediator's fix deploys and is approved or the user
	// releases the hold (`ZETTEL NEEDS YOU`).
	MediationEscalated
)
