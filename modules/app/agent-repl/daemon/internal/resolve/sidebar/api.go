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
	"strconv"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/sessionwatcher"
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
	// SetMerge installs one workspace's merge facts, which drive its merge
	// status arm and its glyph.
	SetMerge(ws ids.WorkspaceID, facts footer.MergeFacts)
	// SetMergeQueue installs, WHOLE, REPO's merge queue as the roster draws
	// it: whether it is paused, the workspaces with a row on it, and the
	// workspaces whose own branch has an async merge on it. The merge
	// orchestrator is the one caller, on every change of the queue's rows and
	// of its pause.
	SetMergeQueue(repo wsm.RepoKey, queue MergeQueue)
	// SetSelected records the user's selection, which also clears that
	// workspace's attention marker.
	SetSelected(ws ids.WorkspaceID)
	// SetRegistrySelected is SetRegistry and then SetSelected in ONE
	// mutation and ONE push: a select's republished registry and the
	// selection reach every client together.
	SetRegistrySelected(reg Registry, ws ids.WorkspaceID)
	// Selected answers the workspace the roster's selection names right now,
	// and false when it names none. It lets a caller ask whether the roster
	// already agrees with the selection WSM stores before pushing a roster
	// that would change nothing.
	Selected() (ids.WorkspaceID, bool)
	// Row answers one workspace's row exactly as the roster last resolved
	// carries it, with that roster's revision: both taken from the SAME
	// roster under the lock that resolved it, so the row is never older or
	// newer than the revision it is stated with. False when no roster was
	// resolved yet or the last one carries no row of the workspace.
	Row(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool)
	// Revision answers the revision of the roster last resolved (frontend.v1
	// WorkspaceRoster.revision), zero before any: every roster at that
	// revision or later carries every change made before the call.
	Revision() uint64
	// SetViewed records that the user has READ the last turn's result — the
	// editor's report that the user has now SEEN this workspace. It takes
	// only on a turn-end row (done, interrupted or turn_failed), or on an
	// idle_async row over an unread turn end; a report on any other arm is
	// dropped. A read result draws the row PARTIAL whenever it stands on its
	// turn-end arm. There is no lowering setter: the next turn is what makes
	// a new result unread.
	SetViewed(ws ids.WorkspaceID)
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
	// SetRepoDivergence installs a repository's latest ahead/behind measure
	// (repodivergence), drawn as its section label's hover. A repository never
	// measured is drawn pending.
	SetRepoDivergence(repo ids.RepoID, measure repodivergence.Measure)
	// FooterStatusChanged redraws the roster because the footer's status for
	// the workspace moved: the row's status is a projection of it.
	FooterStatusChanged(ws ids.WorkspaceID)
	// Topic is the one editor-global roster publication.
	Topic() *publish.Topic[*frontendv1.WorkspaceRoster]
}

// MergeQueue is one repository's merge queue as the roster draws it. It is
// stated whole on every change, so nothing a queue no longer says can linger
// in the roster.
type MergeQueue struct {
	// Paused reports whether the queue is paused. A row whose merge is on a
	// paused queue carries the `merge_paused` marker beside its merge
	// indicator (frontend.v1.RosterRowMergePaused).
	Paused bool
	// Workspaces are the workspaces with a row on the queue, in queue order:
	// the queue a `merging` or `merge_queued` row's own merge is on.
	Workspaces []ids.WorkspaceID
	// AsyncRequesters are the workspaces whose own branch has an async merge
	// on the queue, from the moment the async request is recorded until its
	// merge leaves the queue. Each row carries the `async_merging` marker
	// beside its own status arm.
	AsyncRequesters []ids.WorkspaceID
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
// turn end draws the row as it stood. It is called off the resolver's lock.
type ResultSink func(ws ids.WorkspaceID, result *wsm.TurnResult)

// StatusChangeSink is told every instant a workspace's published status arm
// moves. The daemon keeps it durable (wsm.SetStatusChangedAt), so a daemon
// that did not see the change still ranks the row by it
// (frontend.v1.RosterRowLastUpdated). It is called off the resolver's lock.
type StatusChangeSink func(ws ids.WorkspaceID, at time.Time)

// Option adjusts the resolver.
type Option func(*resolver)

// WithResultSink installs the sink the resolver tells its result changes.
func WithResultSink(sink ResultSink) Option {
	return func(r *resolver) { r.results = sink }
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
