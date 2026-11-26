// Package topbar is the topbar resolver: title and session line, the model
// selector, the permission-mode picker, the connectivity glyph and the
// context chip.
//
// The model fact is LAST-WRITER-WINS in shim order. The connectivity glyph and
// its tone come from the render-colors vocabulary. See ARCHITECTURE.md
// "resolvers".
//
// READINESS — the topbar publishes only once EVERY non-optional element can be
// resolved, which means all five of these have arrived:
//
//	SetNaming              the title and the session line's config dir
//	SetName                the workspace name alone, on a rename
//	OnSessionStarted       the vendor session id and the effective model
//	SetAccount             the account arm (a logged-out root is a DRAWN
//	                       label, so "not yet read" is not the same fact)
//	OnSessionStarted       (also) the permission-mode picker, served from the
//	                       vendor's fixed switchable set
//	SessionContextUsage    the context chip's figure and its breakdown
//
// Connectivity needs nothing: an unobserved link is `no_session`. Before the
// five arrive NOTHING is published — the contract's legal
// "not yet resolved" state — because a partial view would violate the
// non-optional fields it would have to leave empty.
package topbar

import (
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/claudesettings"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/viewquiet"
	"agent-repld/internal/vocab"
	"agent-repld/internal/wsm"
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
	// ConfigDir is the root the session spends as: the LOGIN whose default
	// model the selector marks, and the key SetLoginDefaultModel moves it by.
	ConfigDir string
	// DefaultModel is that login's default model (AgentModel.name): the
	// model a new session on it starts under when nobody names one. Never
	// empty, because every login has one.
	DefaultModel string
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

// Naming is the workspace's name line and session line, from WSM.
type Naming struct {
	// Name is the workspace's registry name, which the topbar draws on its own
	// line beneath the summary (TopbarWorkspaceName). It is never composed
	// into the title.
	Name string
	// NamePrefix is the marks drawn before the name (TopbarWorkspaceName.prefix),
	// decided by nameprefix.Of, the one decision the sidebar row wears too. Nil
	// when the name wears none.
	NamePrefix *frontendv1.WorkspaceNamePrefix
	// ConfigDir is the account root this session spends from — the middle
	// field of the session line. A daemon fact (the repo-under-root rule
	// determines it), so it rides the naming rather than any vendor frame.
	ConfigDir string
	// RemoteControl is the workspace's effective remote-control mode
	// (wsm.RemoteControl.On), which the remote-control chip draws. A daemon
	// fact too, and riding the naming makes the chip as ready as the strip
	// (remotecontrol.go).
	RemoteControl bool
	// QueueBypass is the workspace's queue-bypass chip standing, which the
	// chip draws (queuebypass.go). The zero value is a chip not offered.
	QueueBypass QueueBypass
	// ProductionChecks is the workspace's repository's production-check
	// standing, which the production-checks chip draws (productionchecks.go).
	// The zero value draws no chip.
	ProductionChecks ProductionChecks
}

// ProductionChecks is one repository's production-check standing as a
// workspace's chip draws it, resolved by the workspace verbs.
type ProductionChecks struct {
	// On reports that the repository turns production checks on, the only
	// repository that draws the chip.
	On bool
	// Paused reports that the checks are paused.
	Paused bool
	// Mediator names the repository's production mediator, empty while none
	// is made yet.
	Mediator string
	// Repository and RepositoryDir are the repository a click acts on.
	Repository    ids.RepoID
	RepositoryDir string
}

// QueueBypass is one workspace's queue-bypass chip, resolved by the
// workspace verbs.
type QueueBypass struct {
	// Offered reports that the workspace's session spends as a personal
	// account, the only kind that draws the chip.
	Offered bool
	// Holds reports that this workspace holds its repository's mode.
	Holds bool
	// HeldBy names the sibling holding the mode, empty when none does.
	HeldBy string
	// Offers are the contexts the off chip's menu offers, in display order,
	// as the workspace verbs decide them (workspace.QueueBypassContext.Proto):
	// the two main-context ones always, and the two existing ones while a
	// bypass conversation is parked. Empty while the mode is held.
	Offers []*frontendv1.TopbarQueueBypassContext
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
	// EACH IS THE HOP, NOT THE STREAM: web is false only for a page that
	// connected and went away unannounced; a page Emacs unloaded on purpose,
	// or one not yet connected since the host stream opened, is awaited and
	// passes as true. Host is false only once a host stream this daemon held
	// has gone; an editor not yet attached here (a handover's successor) is
	// awaited and passes as true (server.participantHopsLocked).
	SetParticipants(ws ids.WorkspaceID, host, web bool)
	// SetParked states that the daemon stood this workspace's idle shim down
	// on purpose. There is ONE park, whatever stood the session down: while it
	// stands the strip draws the live idle session it stands in for, since
	// the next prompt, settings pick or keep-alive restore brings it back.
	// false lifts it, and so does the next link state of any kind.
	SetParked(ws ids.WorkspaceID, parked bool)
	// SetNextKeepalive states the cache keeper's next scheduled keep-alive for
	// this workspace, the zero time for none, whether or not a park stands. A
	// standing park carries it (wsm.Park).
	SetNextKeepalive(ws ids.WorkspaceID, next time.Time)
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
	// SetName replaces the workspace name the topbar draws
	// (TopbarWorkspaceName), leaving the session line's config dir as it is.
	// Its sole producer is the workspace verbs, on every SetWorkspaceName.
	SetName(ws ids.WorkspaceID, name string)
	// SetNamePrefix replaces the marks drawn before the workspace name
	// (TopbarWorkspaceName.prefix), leaving the rest of the naming as it is;
	// nil draws none. Its sole producer is the workspace verbs, on every
	// SetWorkspacePriority.
	SetNamePrefix(ws ids.WorkspaceID, prefix *frontendv1.WorkspaceNamePrefix)
	// SetRemoteControl replaces the workspace's remote-control mode the chip
	// draws (TopbarRemoteControl), leaving the rest of the naming as it is.
	// Its sole producer is the fleet, on every UpdateRemoteControlMode.
	SetRemoteControl(ws ids.WorkspaceID, on bool)
	// SetQueueBypass replaces the workspace's queue-bypass chip standing,
	// leaving the rest of the naming as it is. Its sole producer is the
	// workspace verbs, on every UpdateQueueBypassMode, for every workspace of
	// the repository.
	SetQueueBypass(ws ids.WorkspaceID, chip QueueBypass)
	// SetProductionChecks replaces the workspace's production-checks chip
	// standing, leaving the rest of the naming as it is. Its sole producer is
	// the workspace verbs, on every UpdateProductionPause, for every
	// workspace of the repository.
	SetProductionChecks(ws ids.WorkspaceID, chip ProductionChecks)
	// SetSummary installs the workspace summary its agent set
	// (agentrepl.v1.SetWorkspaceSummary): the short summary in the title's
	// FIRST precedence, above the vendor's ai-title, and the long summary as
	// the title's hover description (TopbarTitle.description). The zero
	// summary retracts it. Its sole producer is the summary keeper
	// (wssummary.Keeper).
	SetSummary(ws ids.WorkspaceID, summary wsm.WorkspaceSummary)
	// SetLinks installs the links the workspace name's menu shows, most
	// recently set first, which the name carries for a click to open
	// (TopbarWorkspaceName.links). Empty clears them. Its sole producer is the
	// workspace verbs, at registration and on every ChangeWorkspaceLinks.
	SetLinks(ws ids.WorkspaceID, shown []Link)
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
	// SetEffortSettings installs what the session's config root persists for
	// the effort level — the selector's current level before any pick. The
	// daemon reads it at workspace initialization and on every account switch.
	SetEffortSettings(ws ids.WorkspaceID, settings claudesettings.Effort)
	// SetPickedEffort installs the level the shim confirmed for a SetEffort.
	// It holds for the rest of the workspace's session, across every shim.
	SetPickedEffort(ws ids.WorkspaceID, level conversationv1.AgentEffortLevel)
	// PickedEffort answers the level a pick put in force, false before any
	// pick. A newly spawned shim is put back at it.
	PickedEffort(ws ids.WorkspaceID) (conversationv1.AgentEffortLevel, bool)
	// EffortLevels answers the levels the selected model accepts, false when
	// it takes none or its capabilities were never stated. SetEffort
	// validates against it.
	EffortLevels(ws ids.WorkspaceID) ([]conversationv1.AgentEffortLevel, bool)
	// ContextWindow answers the window, in tokens, the context chip measures
	// the workspace's figure against: the vendor's stated window, else the
	// assumed 1,000,000. The cold gate's figure is measured against it too.
	ContextWindow(ws ids.WorkspaceID) int64
	// ContextTokens answers the tokens of context the chip states for the
	// workspace: the standing cold gate's count, else the last context usage
	// the vendor reported, 0 when it reported none.
	ContextTokens(ws ids.WorkspaceID) int64
	// BindContextFill installs the sink told every workspace's context size
	// and window fill after each fact the strip takes, in the order the facts
	// were taken. The sidebar's duration ink reads it. Bound once, at wiring.
	BindContextFill(sink ContextFillSink)
	// SetAccount installs the account cell: the root in force and EVERY root
	// the daemon knows beside it. An EMPTY email is the logged-out arm, which
	// is a drawn label rather than a blank one.
	SetAccount(ws ids.WorkspaceID, account Account)
	// SetLoginDefaultModel moves the default model of the login CONFIG_DIR on
	// every strip spending as it, so a "Make default" on one workspace is
	// drawn on all of them at once. Its sole producer is SetDefaultModel.
	SetLoginDefaultModel(configDir, model string)
	// RequestSignIn asks every strip spending as WS's account root to open the
	// login by itself: the daemon determined the root must sign in again (a
	// turn or a start the vendor rejected for the credential).
	RequestSignIn(ws ids.WorkspaceID)
	// SignInAnswered ends the request standing on CONFIG_DIR once its login
	// flow ends, signed in or closed.
	SignInAnswered(configDir string)
	// SetPersistentWifi states the machine's persistent-wifi standing, which
	// every strip draws as its persistent-wifi chip. The persistent-wifi
	// controller calls it on every change.
	SetPersistentWifi(state *agentreplv1.PersistentWifiState)
	// SetAgentReplSession states agent-repl's session, which every strip's
	// connectivity indicator carries for its dropdown.
	SetAgentReplSession(session *frontendv1.TopbarAgentReplSession)
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
// ContextFillSink is told a workspace's context size in tokens and its window
// fill (0..1), exactly as the chip states them. It is called under the
// resolver's lock so the facts arrive in order, and must not call back into it.
type ContextFillSink func(ws ids.WorkspaceID, tokens int64, fill float64)

type options struct {
	clock Clock
	// selections keeps the controls for when the shim is down; nil keeps
	// nothing (WithSelectionStore).
	selections SelectionStore
	// quiet is the lid-closed quiet state's view gate; nil is a gate that is
	// never quiet (WithQuiet).
	quiet *viewquiet.Gate
}

// WithQuiet installs the lid-closed quiet state's view gate: while it stands
// no strip is resolved or published (every fact is still taken), and leaving
// it publishes each strip a fact moved, once, at its current state
// (viewquiet).
func WithQuiet(g *viewquiet.Gate) Option { return func(o *options) { o.quiet = g } }

// WithClock injects the clock the cold gate's rise is stamped by.
func WithClock(c Clock) Option { return func(o *options) { o.clock = c } }

// Defaults for the injectable knobs.
const (
	// DefaultLineWidth is how wide a composed overlay line may be before the
	// daemon truncates it.
	DefaultLineWidth = 120
)

// Clock is the resolver's whole dependency on time: the cold gate's rise is
// stamped by it, and a test must not depend on wall-clock.
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

// Link is one link the summary's menu shows.
type Link struct {
	// Label is the agent's label for it, drawn as the menu row's text.
	Label string
	// URL is the absolute http or https url a pick opens.
	URL string
	// Owned is whether the workspace's work made what the link points at.
	Owned bool
	// State is how the GitHub pull request the url names stands, as the
	// pull-request watcher last read it; empty for any other url and for a
	// pull request not yet read. A menu row draws it as its badge.
	State wsm.PullRequestState
	// Subscribed is whether the workspace is told about the link's updates
	// (wsm.WorkspaceLink.Subscribed). A menu row draws it as its
	// "subscribed" badge.
	Subscribed bool
}
