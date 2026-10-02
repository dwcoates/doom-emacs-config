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

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
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
	// SetSelected records the user's selection, which also clears that
	// workspace's attention marker.
	SetSelected(ws ids.WorkspaceID)
	// SetViewed records that the user has READ the last turn's result — the
	// editor's report that the user has now SEEN this workspace. It takes
	// only on a turn-end row (done, interrupted or turn_failed); a report on
	// any other arm is dropped. A read result draws the row PARTIAL whenever it stands on
	// its turn-end arm, and lets an unread-held row yield to idle_async while
	// detached work runs. There is no lowering setter: the next turn is what
	// makes a new result unread.
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
	// SetVendorStart installs where the workspace's vendor-start run stands
	// (the fleet's vendor-start faults). The roster has NO vendor arms: a run
	// being retried draws as the bring-up (`init`), and a rejection or an
	// exhausted window as `start_failed` -- the footer carries the
	// distinction (design record vendor-start-resilience.md, landed change 2).
	SetVendorStart(ws ids.WorkspaceID, state VendorStart)
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
	// SetSummary installs the row detail's summary line — the workspace's last
	// prompt, first line only. Empty omits the line rather than drawing it
	// blank.
	SetSummary(ws ids.WorkspaceID, text string)
	// Topic is the one editor-global roster publication.
	Topic() *publish.Topic[*frontendv1.WorkspaceRoster]
}

// New builds the roster resolver. colors supplies the roster_status and
// merge_glyphs tables, which the resolver asserts its arms against.
func New(colors vocab.RenderColors, log dlog.Surfaces, opts ...Option) (Resolver, error) {
	r, err := newResolver(colors, log)
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

// Option adjusts the resolver.
type Option func(*resolver)

// WithResultSink installs the sink the resolver tells its result changes.
func WithResultSink(sink ResultSink) Option {
	return func(r *resolver) { r.results = sink }
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
