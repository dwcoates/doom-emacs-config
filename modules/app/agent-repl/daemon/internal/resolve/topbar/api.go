// Package topbar is the topbar resolver: title and session line, the model
// selector, the permission-mode picker, the connectivity glyph, the warnings
// and the context chip.
//
// The model fact is LAST-WRITER-WINS in shim order. The connectivity glyph and
// its tone come from the render-colors vocabulary. See ARCHITECTURE.md
// "resolvers".
//
// READINESS — the topbar publishes only once EVERY non-optional element can be
// resolved, which means all five of these have arrived:
//
//	SetNaming              the title and the session line's config dir
//	OnSessionStarted       the vendor session id and the effective model
//	SetAccount             the account arm (a logged-out root is a DRAWN
//	                       warning, so "not yet read" is not the same fact)
//	OnSessionStarted       (also) the permission-mode picker, served from the
//	                       vendor's fixed switchable set
//	SessionContextUsage    the context chip's figure and its breakdown
//
// Connectivity and the warning strip need nothing: an unobserved link is
// `no_session`, and an empty warning list is the daemon saying nothing is
// wrong. Before the five arrive NOTHING is published — the contract's legal
// "not yet resolved" state — because a partial view would violate the
// non-optional fields it would have to leave empty.
package topbar

import (
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/vocab"
)

// Account is the topbar's account cell: which root the session spends as, and
// the whole set it may switch to.
//
// THE OPTIONS ARE NOT DERIVED FROM THE EMAIL. The cell's click is a dropdown
// of the login options (owner ruling, 2026-09-13), and the resolver renders
// exactly what it was handed — a cell whose options the daemon forgot draws
// the empty reveal that ruling was about, and it draws it loudly rather than
// inventing a one-row list from the current root.
type Account struct {
	// Email is the CURRENT root's signed-in address, empty when it is logged
	// out.
	Email string
	// Options is every root the daemon knows, in the account package's order.
	Options []AccountOption
}

// AccountOption is one root the session may switch to.
type AccountOption struct {
	// ConfigDir is the root's path — the echo token SelectAccount takes back.
	ConfigDir string
	// Email is that root's signed-in address, empty when it is logged out.
	Email string
	// Current marks the root the session spends as right now.
	Current bool
}

// Naming is the workspace's title and session line, from WSM.
type Naming struct {
	// Slug is the workspace's short name.
	Slug string
	// Title is the display title.
	Title string
	// Branch is the worktree's branch.
	Branch string
	// DefaultBranch is the REPOSITORY's default branch. The title shows the
	// branch only when it DIFFERS from this, so a workspace sitting on the
	// default branch is named by its name alone; without the comparison the
	// resolver could not tell a branch worth showing from one that says
	// nothing.
	DefaultBranch string
	// ConfigDir is the account root this session spends from — the middle
	// field of the session line. A daemon fact (the repo-under-root rule
	// determines it), so it rides the naming rather than any vendor frame.
	ConfigDir string
}

// DetachedUnmodeled is one live item of UNMODELED work running detached. It
// has no home in the footer's chips by design and no arm in DetachableWork, so
// the daemon states it here rather than letting it vanish.
type DetachedUnmodeled struct {
	// ToolName is the tool as the agent named it.
	ToolName string
	// StartedAt is when it began; the overlay's clock ticks from it.
	StartedAt time.Time
}

// DaemonWarning is one daemon-scoped condition as the warning strip draws it:
// the row's line, and the overlay the row opens when it has one.
type DaemonWarning struct {
	// Line is the row's sentence. Never empty.
	Line string
	// DeployFailed is a failed deploy's overlay; nil is a row with no overlay.
	DeployFailed *DeployFailedOverlay
}

// DeployFailedOverlay is what a failed deploy's row reveals, every line
// composed by the health package out of the fault's own evidence.
type DeployFailedOverlay struct {
	// Step is the step that failed, in words.
	Step string
	// Component is what the step failed on: the build step, or the component.
	Component string
	// Rollback is what became of the install, in words.
	Rollback string
	// Detail is the failure's own account, whole.
	Detail string
	// Log is where the build's whole output is archived; empty for none.
	Log string
}

// Resolver is the topbar's whole surface.
type Resolver interface {
	sessionwatcher.TopbarSink

	// SetParticipants states the OTHER TWO HOPS of connectivity truth: whether
	// this workspace's WatchHostWorkspace and WatchWebWorkspace streams are
	// held right now. The server calls it on every open and close edge of
	// either stream. Per daemon.md invariant 11 the workspace is connected
	// only while all three hops are live, so a hop down is drawn as not
	// connected however healthy the shim link is.
	SetParticipants(ws ids.WorkspaceID, host, web bool)
	// SetParked states that the idle sweep stood this workspace's shim down on
	// purpose. While it stands, a DEAD link draws `no_session` rather than
	// `dead`: the daemon put the route down and a prompt brings it back, so
	// the indicator reports an absent session and never a broken one. It is
	// lifted by the next link state of any kind.
	SetParked(ws ids.WorkspaceID, parked bool)
	// SetColdGate states that this workspace is standing at the COLD GATE: the
	// shim answered `cold` to the session start, so there is no session and
	// there will be none until the reader answers the gate. While it stands
	// the topbar draws the workspace facts plus the gate's own state, exactly
	// as a park does; answering the gate retires it. It is called from the
	// same sites that raise and retire the feed's gate row and the footer's
	// cold-gate status, so the three surfaces cannot disagree about one gate.
	SetColdGate(ws ids.WorkspaceID, gate ColdGate)
	// SetWorkspaceDir binds the workspace's directory, which is what resolves
	// its durable log sink. The daemon calls it at registration, BEFORE any
	// frame can arrive; a frame for an unbound workspace is an invariant
	// violation the resolver records loudly rather than writing globally by
	// default.
	SetWorkspaceDir(ws ids.WorkspaceID, dir string) error
	// SetNaming installs the WSM-derived title and session line.
	SetNaming(ws ids.WorkspaceID, naming Naming)
	// SetSynthesizedTitle installs the daemon's OWN one-line conversation
	// summary, shown in the title's MIDDLE precedence: below the vendor's
	// ai-title, above the workspace name. An empty text retracts it. Its sole
	// producer is the title synthesizer.
	SetSynthesizedTitle(ws ids.WorkspaceID, title string)
	// SetModelCatalog installs the switchable model set the selector renders,
	// in display order.
	SetModelCatalog(ws ids.WorkspaceID, models []*conversationv1.ModelOption)
	// PermissionModes answers exactly the switchable mode set that was served,
	// in the order it was served, reporting false when no picker has been
	// installed. It is what a mode switch is validated against.
	PermissionModes(ws ids.WorkspaceID) ([]string, bool)
	// ModelCatalog answers exactly the model tokens the selector served, in
	// the order served, reporting false before a session has stated one.
	// SetModel validates against what was served here.
	ModelCatalog(ws ids.WorkspaceID) ([]string, bool)
	// SetAccount installs the account cell: the root in force and EVERY root
	// the daemon knows beside it. An EMPTY email is the logged-out arm, which
	// is a drawn warning rather than a blank label.
	SetAccount(ws ids.WorkspaceID, account Account)
	// SetDetachedUnmodeled installs the live detached-unmodeled items, one
	// warning each. Nothing on the shim's streams announces them —
	// DetachableWork has no unmodeled arm — so the daemon states the set.
	SetDetachedUnmodeled(ws ids.WorkspaceID, items []DetachedUnmodeled)
	// RaiseWarning puts a condition the DAEMON raised about its own resolution
	// on the warning strip — the webapp's one error surface — as one line with
	// no overlay. key identifies the condition, so raising it again updates
	// the line rather than adding a second one. The raising site logs the full
	// context itself; this only makes it visible.
	RaiseWarning(ws ids.WorkspaceID, key, line string)
	// RaiseDaemonWarning is RaiseWarning for a condition of the WHOLE DAEMON
	// — a daemon-scoped fault — which stands on EVERY workspace's warning
	// strip, present and future, as its line and, when the warning carries
	// one, the overlay its row opens. key identifies the condition, so
	// raising it again restates it. Unlike a workspace's raised warning it is
	// RETRACTED when the condition ends (RetractDaemonWarning): the fault it
	// draws is closed, not merely past.
	RaiseDaemonWarning(key string, warning DaemonWarning)
	// RetractDaemonWarning takes a daemon-scoped warning off every strip. A
	// key that is not raised retracts nothing.
	RetractDaemonWarning(key string)
	// SetPersistentWifi states the machine's persistent-wifi standing, which
	// every strip draws as its persistent-wifi chip. The persistent-wifi
	// controller calls it on every change.
	SetPersistentWifi(state *agentreplv1.PersistentWifiState)
	// Topic is the workspace's topbar publication.
	Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.TopbarView]
	// StatusFacts answers the session facts the /status panel splices —
	// account, model and permission mode — reporting false before the session
	// has stated them. THE PANEL DEGRADES BY DESIGN: with the vendor handshake
	// deferred there is no cwd, auth, plugin or memory fact to state, so these
	// three plus the daemon's version are the whole panel.
	StatusFacts(ws ids.WorkspaceID) (StatusFacts, bool)
	// ContextPanel resolves the /context panel from the SAME
	// SessionContextUsage fact the context chip resolves from, reporting false
	// when the vendor has answered none yet.
	ContextPanel(ws ids.WorkspaceID) (*frontendv1.ContextPanelView, bool)
	// McpPanel resolves the /mcp panel from the mcp_server healths the session
	// has stated, one row per server in first-named order. It ALWAYS answers:
	// a workspace no server has been stated for has an empty catalog, and an
	// empty catalog is the daemon saying there is no MCP server here — a fact,
	// not a missing one.
	McpPanel(ws ids.WorkspaceID) *frontendv1.McpPanelView
}

// ColdGate is the standing cold-context gate as the topbar states it. It is
// the footer's ColdGate fact seen from this strip: the footer says the
// composer is owned, and this says the whole session-scoped half of the topbar
// is not there to be drawn.
type ColdGate struct {
	// Standing reports whether a gate is open.
	Standing bool
	// ContextTokens is what the cold read would re-read at full price, as the
	// shim's SessionCold stated it. Read only while Standing.
	ContextTokens int64
}

// Option adjusts the resolver's injectable knobs.
type Option func(*options)

// options are the resolver's knobs.
type options struct {
	clock      Clock
	warningCap int
}

// WithClock injects the clock every warning's ordering instant is taken from.
func WithClock(c Clock) Option { return func(o *options) { o.clock = c } }

// WithWarningCap sets how many warnings the dropdown carries.
func WithWarningCap(n int) Option { return func(o *options) { o.warningCap = n } }

// Defaults for the injectable knobs.
const (
	// DefaultWarningCap is how many warnings the dropdown carries, newest
	// first. The strip is a summary, not a log.
	DefaultWarningCap = 20
	// DefaultLineWidth is how wide a composed overlay line may be before the
	// daemon truncates it.
	DefaultLineWidth = 120
)

// Clock is the resolver's whole dependency on time: warnings are ordered by
// when they were observed, and a test must not depend on wall-clock.
type Clock interface {
	// Now is the current instant.
	Now() time.Time
}

// SystemClock is the production Clock.
type SystemClock struct{}

// Now is time.Now.
func (SystemClock) Now() time.Time { return time.Now() }

// New builds the topbar resolver. colors supplies the connectivity tone table,
// which the resolver asserts its emitted tones against.
func New(colors vocab.RenderColors, log dlog.Surfaces, opts ...Option) (Resolver, error) {
	return newResolver(colors, log, opts...)
}

// StatusFacts are the session facts the /status panel splices beside the
// daemon's own version. Each is stated exactly as the session stated it; an
// EMPTY value is a fact the session has not stated, and the panel OMITS its
// row rather than drawing a blank one.
type StatusFacts struct {
	// Account is the logged-in email, empty when the config root is logged out.
	Account string
	// Model is the effective model, the vendor's own spelling.
	Model string
	// PermissionMode is the mode in force, in its wire spelling.
	PermissionMode string
}
