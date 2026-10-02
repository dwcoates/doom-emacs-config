package topbar

import (
	"fmt"
	"strings"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/vocab"
)

// resolver is the topbar resolver. One instance serves every workspace; each
// workspace owns one accumulation and one topic.
type resolver struct {
	colors vocab.RenderColors
	log    dlog.Surfaces
	opts   options

	mu     sync.Mutex
	states map[ids.WorkspaceID]*wsState
	topics map[ids.WorkspaceID]*publish.Topic[*frontendv1.TopbarView]
	// daemonRaised are the standing daemon-scoped warnings, by key. A
	// workspace whose accumulation is made later takes them at once.
	daemonRaised map[string]DaemonWarning
	// persistentWifi is the machine's persistent-wifi standing, which every
	// strip draws alike; nil before the controller's first read.
	persistentWifi *agentreplv1.PersistentWifiState
}

// newResolver builds the resolver with the injectable knobs resolved.
func newResolver(colors vocab.RenderColors, log dlog.Surfaces, opts ...Option) (*resolver, error) {
	if log == nil {
		return nil, fmt.Errorf("topbar resolver needs log surfaces")
	}
	if len(colors.TopbarConnectivity) == 0 {
		return nil, fmt.Errorf(
			"topbar resolver needs the render-colors topbar_connectivity table; it refuses to serve an unpainted state")
	}
	o := options{clock: SystemClock{}, warningCap: DefaultWarningCap}
	for _, apply := range opts {
		apply(&o)
	}
	if o.clock == nil {
		return nil, fmt.Errorf("topbar resolver needs a clock")
	}
	if o.warningCap <= 0 {
		return nil, fmt.Errorf("topbar warning cap must be positive, got %d", o.warningCap)
	}
	return &resolver{
		colors: colors,
		log:    log,
		opts:   o,
		states: map[ids.WorkspaceID]*wsState{},
		topics: map[ids.WorkspaceID]*publish.Topic[*frontendv1.TopbarView]{},

		daemonRaised: map[string]DaemonWarning{},
	}, nil
}

// Topic is the workspace's topbar publication.
func (r *resolver) Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.TopbarView] {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.topicLocked(ws)
}

// topicLocked resolves the workspace's topic under the resolver's lock.
func (r *resolver) topicLocked(ws ids.WorkspaceID) *publish.Topic[*frontendv1.TopbarView] {
	t, ok := r.topics[ws]
	if !ok {
		t = &publish.Topic[*frontendv1.TopbarView]{}
		r.topics[ws] = t
	}
	return t
}

// stateLocked resolves the workspace's accumulation under the lock.
func (r *resolver) stateLocked(ws ids.WorkspaceID) *wsState {
	s, ok := r.states[ws]
	if !ok {
		s = newWSState()
		// A DAEMON-SCOPED WARNING STANDS ON EVERY STRIP, including one made
		// after it was raised.
		for key, warning := range r.daemonRaised {
			s.daemonRaised[key] = &raisedRecord{line: warning.Line, deployFailed: warning.DeployFailed, seq: s.nextSeq()}
		}
		r.states[ws] = s
	}
	return s
}

// SetWorkspaceDir binds the workspace's directory so its records reach that
// workspace's own sink.
func (r *resolver) SetWorkspaceDir(ws ids.WorkspaceID, dir string) error {
	log, err := r.log.Workspace(dir)
	if err != nil {
		return fmt.Errorf("bind topbar resolver to workspace %s at %q: %w", ws, dir, err)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.stateLocked(ws)
	s.dir = dir
	s.log = log.With(dlog.Context{"workspace_id": string(ws)})
	s.log.Debug("daemon.topbar.bind", "the topbar resolver bound a workspace to its log sink",
		dlog.Context{"workspace_dir": dir})
	return nil
}

// logOf answers the workspace's logger. An unbound workspace is an invariant
// violation, recorded as one on the only sink that exists before a directory is
// known rather than silently dropped.
func (r *resolver) logOf(ws ids.WorkspaceID, s *wsState) dlog.Logger {
	if s.log != nil {
		return s.log
	}
	log := r.log.Global().With(dlog.Context{
		"workspace_id":        string(ws),
		"invariant_violation": "topbar resolver frame for a workspace with no bound directory",
		"remediation":         "call SetWorkspaceDir at registration",
	})
	// THE VIOLATION IS AN ERROR, stated ONCE per workspace. Every record after
	// it keeps its own level and carries the violation as context; before
	// this, the only trace was that context on records logged at INFO and
	// DEBUG, so a level sweep never saw the invariant break.
	if !s.unboundReported {
		s.unboundReported = true
		log.Error("daemon.topbar.unbound_workspace", "a topbar record arrived for a workspace whose directory was never bound", nil)
	}
	return log
}

// mutate runs one accumulation change under the lock and republishes the whole
// view. Every sink method and every setter goes through it, so there is exactly
// one publication site and exactly one readiness gate.
func (r *resolver) mutate(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState)) {
	r.mu.Lock()
	s := r.stateLocked(ws)
	apply(s)
	view, err := r.render(s)
	missing := s.missing()
	// THE SESSION-LESS EDGE IS READ OFF THE VIEW ITSELF, under the same lock
	// that built it, so the record cannot disagree with what was published. A
	// view with no model selector is a strip drawn with dashes where its
	// controls belong, which is exactly what the reader sees.
	sessionlessPublished := view != nil && view.GetModelSelector() == nil
	returnedToFull := view != nil && s.sessionlessPublished && !sessionlessPublished
	if view != nil {
		s.sessionlessPublished = sessionlessPublished
	}
	topic := r.topicLocked(ws)
	log := r.logOf(ws, s)
	edges := sourceEdges(s, view)
	r.mu.Unlock()
	for _, edge := range edges {
		log.Info(edge.operation, edge.message, edge.ctx)
	}

	if ctx == nil {
		ctx = dlog.Context{}
	}
	switch {
	case err != nil:
		ctx["cause"] = err.Error()
		log.Error(operation, "the topbar could not be resolved and nothing was published", ctx)
	case view == nil:
		// AT INFO, WITH THE GATES NAMED. A topbar that never completes is a
		// BLANK STRIP the reader stares at, and while this sat at DEBUG the
		// only diagnosis of one was to raise the level and reproduce it. The
		// record fires on an ordinary bring-up too, which is what INFO is for
		// (AGENTS.md: lifecycle a person would ask about); the gates it is
		// still waiting on are the whole content.
		ctx["awaiting"] = strings.Join(missing, ",")
		log.Info(operation, "the topbar took a fact and is not yet complete", ctx)
	case sessionlessPublished:
		// AT INFO. A strip whose model selector, permission picker and fast
		// mode are all drawn as dashes is the loudest thing a reader can see,
		// and "why" has to be answerable from the default level rather than
		// from a reproduction — the same reason the incomplete record above
		// sits at info.
		ctx["parked"] = s.parked
		ctx["cold_gate"] = s.coldGate
		ctx["started"] = s.started
		log.Info(operation, "the topbar published the strip with no session facts", ctx)
		topic.Publish(view)
	case returnedToFull:
		log.Info(operation, "the topbar's session facts arrived and the strip is whole", ctx)
		topic.Publish(view)
	default:
		log.Debug(operation, "the topbar took a fact and republished", ctx)
		topic.Publish(view)
	}
}

// render builds the whole view, or nil while the workspace is not ready.
//
// THE STRIP HAS ONE SHAPE (topbar.proto, FIXED SCHEMA AND ORGANIZATION; owner
// ruling 2026-09-13). Every cell is stated on every publication and in the
// same slot; there is no branch here that draws a different strip. A cell
// whose SESSION fact is unknown says so IN ITS OWN SLOT: the three controls by
// absence, which the client draws as a dash, and the context chip and warning
// strip by their own content — the chip carrying the context the session held
// with the reason in its hover, the strip carrying the state as one line.
func (r *resolver) render(s *wsState) (*frontendv1.TopbarView, error) {
	connectivity, err := r.connectivity(connectivityKey(s.linkSeen, s.link, s.hostStream && s.webStream, s.parked))
	if err != nil {
		return nil, err
	}
	if !s.ready() {
		return nil, nil
	}
	return &frontendv1.TopbarView{
		Title:                &frontendv1.TopbarTitle{Text: r.title(s)},
		SessionLine:          &frontendv1.TopbarSessionLine{Text: r.sessionLine(s)},
		ModelSelector:        r.modelSelector(s),
		EffortSelector:       effortSelector(s),
		Connectivity:         connectivity,
		Warnings:             r.warningStrip(s),
		Context:              r.contextChip(s),
		Account:              r.account(s),
		PermissionModePicker: r.permissionModePicker(s),
		FastMode:             fastMode(s),
		PersistentWifi:       persistentWifiChip(r.persistentWifi),
	}, nil
}

// fastMode projects the vendor's last fast-mode statement onto the strip, arm
// for arm. Nil until the session has stated one: the field is UNSET rather
// than defaulted to off, because "the vendor has not said" and "the vendor
// said no" are different facts and only one of them is a claim.
func fastMode(s *wsState) *frontendv1.TopbarFastMode {
	if s.sessionless() {
		return nil
	}
	switch state := s.fastMode.GetState().(type) {
	case *conversationv1.SessionFastMode_On:
		return &frontendv1.TopbarFastMode{
			State: &frontendv1.TopbarFastMode_On{On: &frontendv1.TopbarFastModeOn{}},
		}
	case *conversationv1.SessionFastMode_Off:
		return &frontendv1.TopbarFastMode{
			State: &frontendv1.TopbarFastMode_Off{
				Off: &frontendv1.TopbarFastModeOff{Reason: state.Off.GetReason()},
			},
		}
	case *conversationv1.SessionFastMode_Cooldown:
		return &frontendv1.TopbarFastMode{
			State: &frontendv1.TopbarFastMode_Cooldown{Cooldown: &frontendv1.TopbarFastModeCooldown{}},
		}
	default:
		return nil
	}
}

// title composes the title line: the workspace's name, plus the branch when the
// branch is worth showing. A branch is worth showing when it says something the
// title does not already say, so two comparisons drop it: a workspace sitting on
// its repository's DEFAULT branch is named by its name alone, and so is one
// whose branch reads exactly like THE NAME ALREADY SHOWN — every worktree
// workspace whose branch is minted from its name would otherwise print that
// name twice and fill the strip's title cap with the repetition. The comparison
// is against the name as shown, after the session-title and naming fallbacks,
// because what is doubled is what the reader sees.
func (r *resolver) title(s *wsState) string {
	// THE TITLE IS THE WORKSPACE SUMMARY when the vendor has one (owner
	// ruling, 2026-09-13). The vendor writes its own one-line summary of the
	// conversation into the transcript and the shim states it; it answers
	// "which conversation is this" far better than a directory's name, so it
	// takes the name's place. The BRANCH is unaffected — it is a different
	// fact, and the rule about when it is worth showing is unchanged.
	name := s.sessionTitle
	if name == "" {
		// THE DAEMON'S OWN SUMMARY is the fallback when the vendor wrote no
		// ai-title (owner-requested feature). The vendor's title above always
		// wins, so a future CLI that emits ai-title transparently supersedes
		// ours; ours in turn always beats the bare workspace name below.
		name = s.synthesizedTitle
	}
	if name == "" {
		name = s.naming.Title
	}
	if name == "" {
		name = s.naming.Slug
	}
	branch := s.naming.Branch
	if branch == "" || branch == s.naming.DefaultBranch || branch == name {
		return name
	}
	return joinNonEmpty(" · ", name, branch)
}

// sessionLine composes the identity line: which vendor session, which account
// root, which model.
func (r *resolver) sessionLine(s *wsState) string {
	return joinNonEmpty(" · ", s.vendorSessionID, s.naming.ConfigDir, s.model)
}

// modelSelector renders the selector: the catalog as served, and the effective
// model as the selection.
//
// LAST-WRITER-WINS IN SHIM ORDER: SessionStarted.effective_model is the first
// writer and every model_changed is a later one, so the accumulated value is
// simply the most recent the shim stated. A model the catalog does not carry is
// still the selection — the vendor said it is what is running — and is served
// as an option of its own rather than dropped, which would render the button as
// having no selection at all.
func (r *resolver) modelSelector(s *wsState) *frontendv1.TopbarModelSelector {
	// ABSENT WITH NO SESSION. A stood-down, cold-gated or unstarted workspace
	// has no model in force, and serving the catalog with no selection would
	// draw a picker offering a switch that would be refused.
	if s.sessionless() {
		return nil
	}
	out := &frontendv1.TopbarModelSelector{Options: s.catalog}
	if s.model == "" {
		return out
	}
	for _, option := range s.catalog {
		if option.GetModel().GetName() == s.model {
			out.Selected = option
			return out
		}
	}
	out.Selected = &conversationv1.ModelOption{
		Model:       &conversationv1.AgentModel{Name: s.model},
		DisplayName: s.model,
	}
	return out
}

// permissionModePicker renders the picker: the daemon serves EXACTLY the
// switchable set SetPermissionMode will accept, and the mode in force is what
// the session facts last stated.
//
// A mode in force that the served set does not carry is still drawn as current
// — it IS what is running — while staying absent from the options, so the
// picker never offers a switch the daemon would refuse.
func (r *resolver) permissionModePicker(s *wsState) *frontendv1.TopbarPermissionModePicker {
	// ABSENT WITH NO SESSION, for the same reason the model selector is, and
	// absent until the served set exists at all.
	if s.sessionless() || s.picker == nil {
		return nil
	}
	out := &frontendv1.TopbarPermissionModePicker{
		Current: s.picker.GetCurrent(),
		Options: s.picker.GetOptions(),
	}
	if s.permissionMode == "" {
		return out
	}
	for _, option := range out.Options {
		if option.GetMode() == s.permissionMode {
			out.Current = option
			return out
		}
	}
	out.Current = &frontendv1.TopbarPermissionModeOption{
		Mode:        s.permissionMode,
		DisplayName: displayMode(s.permissionMode),
	}
	return out
}

// SwitchableModes is the vendor's fixed switchable permission-mode set, in
// display order — the arms conversation.v1 AgentPermissionMode spells, in
// their wire spelling. It is a CONSTANT rather than a catalog because the
// vendor states no catalog: SessionStarted carries a model catalog and nothing
// for modes.
//
// `auto` LEADS AND `default` IS NOT OFFERED (owner ruling 2026-09-14: "there
// should be no `default` option in the dropdown in the topbar, it should just
// default to 'auto' in the dropdown"). Every session this daemon starts runs
// under `auto` unless the reader picks otherwise, so offering `default` would
// offer a mode nothing here selects.
//
// A SESSION THE VENDOR REPORTS AS `default` IS STILL DRAWN AS `default`.
// permissionModePicker fills the current option from the mode in force and
// falls back to a synthesized option when the served set does not carry it —
// so a pre-ruling session shows the vendor's own word, unselectable, until its
// next start moves it to auto. The picker never lies about what is running.
var SwitchableModes = []string{"auto", "accept_edits", "plan", "bypass", "dont_ask"}

// switchablePermissionModes builds the served picker from the fixed set. The
// current option is filled in by permissionModePicker from the mode in force.
func switchablePermissionModes() *frontendv1.TopbarPermissionModePicker {
	out := &frontendv1.TopbarPermissionModePicker{
		Options: make([]*frontendv1.TopbarPermissionModeOption, 0, len(SwitchableModes)),
	}
	for _, mode := range SwitchableModes {
		out.Options = append(out.Options, &frontendv1.TopbarPermissionModeOption{
			Mode: mode, DisplayName: displayMode(mode),
		})
	}
	return out
}

// displayMode renders a mode's wire spelling for a reader: the schema spells
// arms with underscores, and a label never does.
func displayMode(mode string) string {
	return strings.ReplaceAll(mode, "_", " ")
}

// contextChip renders the chip and its always-populated hover content.
//
// ALWAYS DRAWN, SESSION OR NO SESSION. A workspace whose session is stood down
// or standing at the cold gate still HELD a context, and that figure is the
// one thing a reader wants from the chip in either state — what a revival
// would carry, and what a cold read would re-read at full price. The reason
// the figure is not live rides the hover, where it costs the strip no width.
func (r *resolver) contextChip(s *wsState) *frontendv1.TopbarContextChip {
	return &frontendv1.TopbarContextChip{
		Text:      contextChipText(s),
		Breakdown: r.tokenBreakdown(s),
	}
}

// contextUnknownText is what the chip states when a context cut has discarded
// the transcript the last reading described and no fresh reading has landed
// yet: an em dash, not a number. Fabricating a figure the vendor never reported
// — a stale total, or a 0 the context is not at — is exactly the defect this
// state exists to avoid, so the chip says "unknown until the next reading"
// rather than stating a count it cannot honestly state.
const contextUnknownText = "—"

// contextChipText is the chip's stated figure, or the unknown dash while a cut
// stands with no fresh reading to replace the discarded one.
//
// THE COLD GATE STILL WINS: its count is a fresh transcript read for THIS
// resume, so a standing gate is never in the unknown state — it has a figure.
func contextChipText(s *wsState) string {
	if s.coldGate && s.coldGateTokens > 0 {
		return formatTokens(s.coldGateTokens)
	}
	if s.contextCut && s.contextUsage == nil {
		return contextUnknownText
	}
	return formatTokens(contextTokens(s))
}

// contextTokens is the numeric figure the chip and the breakdown resolve from.
//
// THE COLD GATE'S OWN COUNT WINS while a gate stands: it is what the shim read
// off the transcript for THIS resume, whereas a remembered `context_usage` is
// what some earlier session of this workspace last reported. Absent both, it is
// 0 — the honest figure for a workspace that has held no context yet.
func contextTokens(s *wsState) int64 {
	if s.coldGate && s.coldGateTokens > 0 {
		return s.coldGateTokens
	}
	return s.contextUsage.GetTotalTokens()
}

// sessionlessReason is the sentence the session-less states are stated with —
// the context chip's hover heading and the warning strip's line, WORD FOR
// WORD the same in both, because they are one fact drawn in two places.
//
// THE COLD GATE OUTRANKS THE PARK when both hold. A standing gate is waiting
// on THE READER — the feed is showing a card that has to be answered before
// this workspace can do anything — while hibernation waits on nothing and
// lifts itself on the next prompt.
func sessionlessReason(s *wsState) string {
	switch {
	case s.coldGate:
		return "cold context, awaiting your answer"
	case s.parked:
		return "hibernated since " + clockTime(s.parkedAtMs)
	case !s.started:
		return "no session yet"
	default:
		return ""
	}
}

// account renders which account the session spends as. THE ARM IS THE STATE: a
// logged-out root is a drawn warning, because a session whose root is logged
// out cannot run a turn and a blank would read as "loading".
func (r *resolver) account(s *wsState) *frontendv1.TopbarAccount {
	out := &frontendv1.TopbarAccount{Options: accountOptions(s.accountOptions)}
	if s.email == "" {
		out.State = &frontendv1.TopbarAccount_LoggedOut{
			LoggedOut: &frontendv1.TopbarAccountLoggedOut{},
		}
		return out
	}
	out.State = &frontendv1.TopbarAccount_LoggedIn{
		LoggedIn: &frontendv1.TopbarAccountLoggedIn{Email: s.email},
	}
	return out
}

// accountOptions renders the cell's dropdown: every root the daemon knows, in
// the order it served them, each on the same arm rule the cell itself takes —
// a logged-out root is an answer, so it is a row the reader can pick and not a
// row drawn blank.
func accountOptions(options []AccountOption) []*frontendv1.TopbarAccountOption {
	out := make([]*frontendv1.TopbarAccountOption, 0, len(options))
	for _, option := range options {
		row := &frontendv1.TopbarAccountOption{ConfigDir: option.ConfigDir, Current: option.Current}
		if option.Email == "" {
			row.State = &frontendv1.TopbarAccountOption_LoggedOut{
				LoggedOut: &frontendv1.TopbarAccountLoggedOut{},
			}
		} else {
			row.State = &frontendv1.TopbarAccountOption_LoggedIn{
				LoggedIn: &frontendv1.TopbarAccountLoggedIn{Email: option.Email},
			}
		}
		out = append(out, row)
	}
	return out
}

// ContextPanel resolves the /context panel from the same fact the chip resolves
// from, reporting false while the vendor has answered none.
func (r *resolver) ContextPanel(ws ids.WorkspaceID) (*frontendv1.ContextPanelView, bool) {
	r.mu.Lock()
	s := r.stateLocked(ws)
	usage := s.contextUsage
	log := r.logOf(ws, s)
	r.mu.Unlock()

	if usage == nil {
		log.Debug("daemon.topbar.context_panel",
			"the /context panel was asked for before the vendor stated a context usage",
			dlog.Context{"resolved": false})
		return nil, false
	}
	log.Debug("daemon.topbar.context_panel", "the topbar resolved the /context panel",
		dlog.Context{"resolved": true})
	return r.contextPanel(usage), true
}

// McpPanel resolves the /mcp panel from the retained mcp_server healths. It
// answers unconditionally: an empty catalog draws an empty panel rather than
// failing, because "no MCP server is configured here" is a fact the daemon can
// state.
func (r *resolver) McpPanel(ws ids.WorkspaceID) *frontendv1.McpPanelView {
	r.mu.Lock()
	s := r.stateLocked(ws)
	servers := append([]*conversationv1.SessionMcpServer(nil), s.mcpServers...)
	log := r.logOf(ws, s)
	r.mu.Unlock()

	log.Debug("daemon.topbar.mcp_panel", "the topbar resolved the /mcp panel",
		dlog.Context{"servers": len(servers)})
	return mcpPanel(servers)
}

// ---- daemon-fact setters --------------------------------------------------

// SetNaming installs the WSM-derived title and session line.
func (r *resolver) SetNaming(ws ids.WorkspaceID, naming Naming) {
	r.mutate(ws, "daemon.topbar.set_naming", "the topbar took the workspace naming",
		dlog.Context{"branch": naming.Branch, "default_branch": naming.DefaultBranch},
		func(s *wsState) {
			s.naming = naming
			s.namingSet = true
		})
}

// SetSynthesizedTitle installs the daemon's OWN one-line summary of the
// conversation. It is the middle precedence in the title: shown only while the
// vendor has stated no ai-title, and always in preference to the workspace
// name. An empty text retracts it (the title falls back to the name), which is
// how the synthesizer drops a stale title after a /clear.
func (r *resolver) SetSynthesizedTitle(ws ids.WorkspaceID, title string) {
	r.mutate(ws, "daemon.topbar.set_synthesized_title", "the topbar took a synthesized title",
		dlog.Context{"present": title != ""},
		func(s *wsState) { s.synthesizedTitle = title })
}

// SetModelCatalog installs the switchable model set.
func (r *resolver) SetModelCatalog(ws ids.WorkspaceID, models []*conversationv1.ModelOption) {
	r.mutate(ws, "daemon.topbar.set_model_catalog", "the topbar took the model catalog",
		dlog.Context{"options": len(models)},
		func(s *wsState) { s.catalog = models })
}

// SetAccount installs the account cell: the root in force, and every root the
// session may switch to.
func (r *resolver) SetAccount(ws ids.WorkspaceID, account Account) {
	r.mutate(ws, "daemon.topbar.set_account", "the topbar took the account",
		dlog.Context{"logged_in": account.Email != "", "options": len(account.Options)},
		func(s *wsState) {
			s.email = account.Email
			s.accountOptions = account.Options
			s.accountSet = true
		})
}

// SetDetachedUnmodeled installs the live detached-unmodeled items.
func (r *resolver) SetDetachedUnmodeled(ws ids.WorkspaceID, items []DetachedUnmodeled) {
	r.mutate(ws, "daemon.topbar.set_detached_unmodeled",
		"the topbar took the live detached-unmodeled set",
		dlog.Context{"items": len(items)}, func(s *wsState) {
			s.detachedUnmodeled = items
			s.detachedSeq = s.nextSeq()
		})
}

// RaiseWarning puts a condition the daemon raised about its own resolution on
// the warning strip. A key already raised keeps its place in the list and
// takes the new sentence.
func (r *resolver) RaiseWarning(ws ids.WorkspaceID, key, line string) {
	r.mutate(ws, "daemon.topbar.raise_warning",
		"the topbar took a warning the daemon raised",
		dlog.Context{"key": key, "line": line}, func(s *wsState) {
			if held, ok := s.raised[key]; ok {
				held.line = line
				return
			}
			s.raised[key] = &raisedRecord{line: line, seq: s.nextSeq()}
		})
}

// RaiseDaemonWarning puts a daemon-scoped condition on every strip. A key
// already raised keeps its place in each list and takes the new sentence and
// overlay.
func (r *resolver) RaiseDaemonWarning(key string, warning DaemonWarning) {
	r.eachStrip("daemon.topbar.raise_daemon_warning", "the topbar took a warning the daemon raised on every strip",
		dlog.Context{"key": key, "line": warning.Line, "overlay": warning.DeployFailed != nil},
		func() { r.daemonRaised[key] = warning },
		func(s *wsState) {
			if held, ok := s.daemonRaised[key]; ok {
				held.line, held.deployFailed = warning.Line, warning.DeployFailed
				return
			}
			s.daemonRaised[key] = &raisedRecord{line: warning.Line, deployFailed: warning.DeployFailed, seq: s.nextSeq()}
		})
}

// RetractDaemonWarning takes a daemon-scoped condition off every strip.
func (r *resolver) RetractDaemonWarning(key string) {
	r.eachStrip("daemon.topbar.retract_daemon_warning", "the topbar retracted a warning the daemon raised on every strip",
		dlog.Context{"key": key},
		func() { delete(r.daemonRaised, key) },
		func(s *wsState) { delete(s.daemonRaised, key) })
}

// eachStrip changes the daemon-scoped set, then applies the change to every
// workspace's accumulation and republishes each one bound to its sink. An
// unbound accumulation takes the change without a publication, which its
// binding's first mutation makes.
func (r *resolver) eachStrip(operation, message string, ctx dlog.Context, daemon func(), apply func(*wsState)) {
	r.mu.Lock()
	daemon()
	var bound []ids.WorkspaceID
	for ws, s := range r.states {
		if s.log == nil {
			apply(s)
			continue
		}
		bound = append(bound, ws)
	}
	r.mu.Unlock()
	ctx["workspaces"] = len(bound)
	r.log.Global().Info(operation, message, ctx)
	for _, ws := range bound {
		r.mutate(ws, operation, message, dlog.Context{"key": ctx["key"]}, apply)
	}
}

// ---- TopbarSink -----------------------------------------------------------

// OnSessionStarted carries the session's identity and spawn facts. It is the
// FIRST writer of the effective model.
func (r *resolver) OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted) {
	if started == nil {
		return
	}
	r.mutate(ws, "daemon.topbar.on_session_started", "the topbar took the session's opening facts",
		dlog.Context{
			"vendor_session_id": started.GetVendorSessionId(),
			"model":             started.GetEffectiveModel().GetName(),
		}, func(s *wsState) {
			s.started = true
			s.vendorSessionID = started.GetVendorSessionId()
			s.model = started.GetEffectiveModel().GetName()
			s.permissionMode = permissionModeName(started.GetPermissionMode())
			// THE PICKER IS SERVED HERE, from the vendor's fixed switchable
			// set: `SessionStarted` carries a model catalog but NO
			// permission-mode catalog, and nothing else in the daemon serves
			// one, so before this the picker was never installed and every
			// SetPermissionMode was refused as mode_not_served. The set is a
			// constant of the vendor's contract (conversation.v1
			// AgentPermissionMode's arms), not an inference.
			s.picker = switchablePermissionModes()
			if catalog := started.GetModelCatalog(); len(catalog) > 0 {
				s.catalog = catalog
			}
		})
}

// OnLink drives the connectivity glyph and its tone.
func (r *resolver) OnLink(ws ids.WorkspaceID, link sessionwatcher.LinkState) {
	r.mutate(ws, "daemon.topbar.on_link", "the topbar took a link state",
		dlog.Context{"shim_link": connectivityKey(true, link, true, false)}, func(s *wsState) {
			s.link = link
			s.linkSeen = true
			// THE REVIVAL ENDS THE PARK, for the same reason the footer clears
			// it here: the watcher latches a dead link and publishes nothing
			// more on it, so the next link state belongs to the shim the
			// reviving prompt spawned, and a spawn that then dies must hollow
			// the indicator like any other death.
			s.parked = false
		})
}

// SetParked installs, or lifts, the idle sweep's park.
//
// INSTALLING ONE STAMPS THE INSTANT, because the hibernated view's whole
// content is the age the strip ticks from it. Lifting one leaves the stamp
// alone: nothing reads it while the park is off, and the next park restamps.
func (r *resolver) SetParked(ws ids.WorkspaceID, parked bool) {
	r.mutate(ws, "daemon.topbar.set_parked", "the topbar took the idle sweep's park",
		dlog.Context{"parked": parked}, func(s *wsState) {
			if parked && !s.parked {
				s.parkedAtMs = r.opts.clock.Now().UnixMilli()
			}
			s.parked = parked
		})
}

// SetColdGate installs, or retires, the standing cold gate — the same fact,
// from the same call sites, that the feed's gate row and the footer's parked
// status are drawn from.
//
// RAISING ONE STAMPS THE INSTANT, for the same reason a park does: the view's
// age ticks client-side from it. Retiring one leaves the stamp and the token
// count alone; nothing reads either while no gate stands, and the next gate
// restates both.
func (r *resolver) SetColdGate(ws ids.WorkspaceID, gate ColdGate) {
	r.mutate(ws, "daemon.topbar.set_cold_gate", "the topbar took the cold gate",
		dlog.Context{"standing": gate.Standing, "context_tokens": gate.ContextTokens}, func(s *wsState) {
			if gate.Standing && !s.coldGate {
				s.coldGateAtMs = r.opts.clock.Now().UnixMilli()
			}
			if gate.Standing {
				s.coldGateTokens = gate.ContextTokens
			}
			s.coldGate = gate.Standing
		})
}

// SetParticipants states the liveness of this workspace's host and web
// streams, the other two hops of connectivity truth (daemon.md invariant 11).
// The indicator reads connected only while all three are live.
func (r *resolver) SetParticipants(ws ids.WorkspaceID, host, web bool) {
	r.mutate(ws, "daemon.topbar.set_participants", "the topbar took the participant streams' liveness",
		dlog.Context{"host_stream": host, "web_stream": web}, func(s *wsState) {
			s.hostStream = host
			s.webStream = web
		})
}

// OnActivity is here for the unmodeled-tool warning AND for the session's own
// token accounting, which the topbar accumulates itself: each resolver owns
// what it collects, and nothing aggregates on its behalf.
func (r *resolver) OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	if act == nil {
		return
	}
	unit := act.GetActivityId().GetValue()
	arm := "other"
	if _, ok := act.GetItem().(*conversationv1.AgentActivity_Unmodeled); ok {
		arm = "unmodeled"
	}
	r.mutate(ws, "daemon.topbar.on_activity", "the topbar took an activity frame",
		dlog.Context{"arm": arm, "activity_id": unit}, func(s *wsState) {
			s.responses.Observe(unit, act.GetUsage() != nil)
			r.observeUsage(s, unit, act.GetUsage())
			r.observeResponse(s, unit, act)
			r.observeUnmodeled(s, act)
		})
}

// observeUsage folds one unit's usage into the SESSION's totals, keyed by unit
// so a re-report replaces rather than adds.
func (r *resolver) observeUsage(s *wsState, unit string, u *conversationv1.TokenUsage) {
	if u == nil {
		return
	}
	next := usageFingerprint{
		read:      u.GetInputHits().GetRead(),
		written:   u.GetInputMisses().GetWritten(),
		unwritten: u.GetInputMisses().GetUnwritten(),
		output:    u.GetOutputTokens(),
		thinking:  u.GetOutputThinkingTokens(),
	}
	if next.thinking > next.output {
		s.contradictions = append(s.contradictions,
			fmt.Sprintf("unit %s reports more thinking tokens than output tokens", unit))
	}
	previous, held := s.counted[unit]
	if held && previous == next {
		return
	}
	if held {
		s.contradictions = append(s.contradictions,
			fmt.Sprintf("unit %s reported two different usages", unit))
		s.totals = subtractUsage(s.totals, previous)
		if model, ok := s.perModel[s.model]; ok {
			model.figures = subtractUsage(model.figures, previous)
		}
	}
	s.counted[unit] = next
	s.totals = addUsage(s.totals, next)
	model, ok := s.perModel[s.model]
	if !ok {
		model = &modelTotals{model: s.model, order: s.nextSeq()}
		s.perModel[s.model] = model
	}
	model.figures = addUsage(model.figures, next)
}

// observeResponse counts the session's settled responses, each under the API
// RESPONSE it arrived in, which is what the accounting reconciles.
func (r *resolver) observeResponse(s *wsState, unit string, act *conversationv1.AgentActivity) {
	item, ok := act.GetItem().(*conversationv1.AgentActivity_Response)
	if !ok {
		return
	}
	switch item.Response.GetResult().(type) {
	case *conversationv1.AgentResponse_Success, *conversationv1.AgentResponse_Failure:
		s.responses.Settle(unit)
	}
}

// observeUnmodeled records one distinct unmodeled tool, with the daemon's
// abbreviated account of its arguments.
func (r *resolver) observeUnmodeled(s *wsState, act *conversationv1.AgentActivity) {
	item, ok := act.GetItem().(*conversationv1.AgentActivity_Unmodeled)
	if !ok {
		return
	}
	start, running := item.Unmodeled.GetResult().(*conversationv1.AgentUnmodeled_Start)
	if !running {
		return
	}
	name := start.Start.GetToolName()
	if name == "" {
		return
	}
	call, held := s.unmodeled[name]
	if !held {
		call = &unmodeledCall{toolName: name, seq: s.nextSeq()}
		s.unmodeled[name] = call
	}
	if lines := abbreviateArguments(start.Start.GetArguments()); len(lines) > 0 {
		call.argumentLines = lines
	}
}

// OnContextCut is the cut that discards the transcript the chip's figure was
// read off.
//
// A CLEAR OR A COMPLETED COMPACTION invalidates the last `context_usage`: the
// tokens the chip states are the ones the cut just removed. So the chip drops
// that figure and states the count is unknown until the vendor's fresh reading
// arrives, rather than leaving a stale total standing — the defect this fixes.
//
// A FAILED COMPACTION CUT NOTHING. The context is unchanged, so the last
// reading still describes it and the chip keeps stating it.
func (r *resolver) OnContextCut(ws ids.WorkspaceID, agent *conversationv1.AgentId, cut *conversationv1.ContextCut) {
	if cut == nil {
		return
	}
	arm, cutMade := contextCutArm(cut)
	r.mutate(ws, "daemon.topbar.on_context_cut", "the topbar took a context cut",
		dlog.Context{"arm": arm, "cut_made": cutMade}, func(s *wsState) {
			if !cutMade {
				return
			}
			s.contextUsage = nil
			s.contextCut = true
		})
}

// contextCutArm names the cut's arm for the record and reports whether it
// actually removed context. A failed compaction is the only arm that cut
// nothing, so it is the only one the chip does not react to.
func contextCutArm(cut *conversationv1.ContextCut) (string, bool) {
	switch cut.GetCut().(type) {
	case *conversationv1.ContextCut_Cleared:
		return "cleared", true
	case *conversationv1.ContextCut_Compacted:
		return "compacted", true
	case *conversationv1.ContextCut_CompactionFailed:
		return "compaction_failed", false
	default:
		return "unset", false
	}
}

// OnSessionUpdate carries model changes, diagnostics, context usage and the
// permission mode.
func (r *resolver) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	if update == nil {
		return
	}
	arm, apply := r.sessionArm(update)
	r.mutate(ws, "daemon.topbar.on_session_update", "the topbar took a session update",
		dlog.Context{"arm": arm}, apply)
}

// sessionArm names the update's arm and returns what it changes. Every arm has
// a branch, including the ones the topbar deliberately draws nothing from.
func (r *resolver) sessionArm(update *conversationv1.SessionUpdate) (string, func(*wsState)) {
	switch u := update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_ModelChanged:
		return "model_changed", func(s *wsState) {
			s.model = u.ModelChanged.GetEffectiveModel().GetName()
		}
	case *conversationv1.SessionUpdate_PermissionModeChanged:
		return "permission_mode_changed", func(s *wsState) {
			s.permissionMode = permissionModeName(u.PermissionModeChanged.GetPermissionMode())
		}
	case *conversationv1.SessionUpdate_IdentityRotated:
		return "identity_rotated", func(s *wsState) {
			s.vendorSessionID = u.IdentityRotated.GetVendorSessionId()
		}
	case *conversationv1.SessionUpdate_Diagnostics:
		return "diagnostics", func(s *wsState) { r.applyDiagnostics(s, u.Diagnostics) }
	case *conversationv1.SessionUpdate_Title:
		return "title", func(s *wsState) { s.sessionTitle = u.Title.GetText() }
	case *conversationv1.SessionUpdate_ContextUsage:
		return "context_usage", func(s *wsState) {
			// THE FRESH READING ENDS THE UNKNOWN STATE. Whatever a cut
			// discarded, this is the vendor's answer for the context that
			// remains, so it both states the figure and retires the dash.
			s.contextUsage = u.ContextUsage
			s.contextCut = false
		}
	case *conversationv1.SessionUpdate_QueryDied:
		return "query_died", func(*wsState) {}
	case *conversationv1.SessionUpdate_AccountUsage:
		return "account_usage", func(*wsState) {}
	case *conversationv1.SessionUpdate_FastMode:
		return "fast_mode", func(s *wsState) { s.fastMode = u.FastMode }
	case *conversationv1.SessionUpdate_McpServer:
		return "mcp_server", func(s *wsState) { s.putMcpServer(u.McpServer) }
	case *conversationv1.SessionUpdate_RateLimitStatus:
		return "rate_limit_status", func(*wsState) {}
	case *conversationv1.SessionUpdate_Compacting:
		return "compacting", func(*wsState) {}
	default:
		return "unset", func(*wsState) {}
	}
}

// permissionModeName is the mode's wire spelling — the SESSION FACTS' own
// vocabulary, which SetPermissionMode echoes unchanged.
func permissionModeName(mode *conversationv1.AgentPermissionMode) string {
	switch mode.GetMode().(type) {
	case *conversationv1.AgentPermissionMode_Default:
		return "default"
	case *conversationv1.AgentPermissionMode_AcceptEdits:
		return "accept_edits"
	case *conversationv1.AgentPermissionMode_Bypass:
		return "bypass"
	case *conversationv1.AgentPermissionMode_Plan:
		return "plan"
	case *conversationv1.AgentPermissionMode_DontAsk:
		return "dont_ask"
	case *conversationv1.AgentPermissionMode_Auto:
		return "auto"
	default:
		return ""
	}
}

// addUsage sums two fingerprints.
func addUsage(a, b usageFingerprint) usageFingerprint {
	return usageFingerprint{
		read:      a.read + b.read,
		written:   a.written + b.written,
		unwritten: a.unwritten + b.unwritten,
		output:    a.output + b.output,
		thinking:  a.thinking + b.thinking,
	}
}

// subtractUsage removes a superseded report from a sum, so a corrected usage
// replaces its predecessor rather than piling onto it.
func subtractUsage(a, b usageFingerprint) usageFingerprint {
	return usageFingerprint{
		read:      saturatingSub(a.read, b.read),
		written:   saturatingSub(a.written, b.written),
		unwritten: saturatingSub(a.unwritten, b.unwritten),
		output:    saturatingSub(a.output, b.output),
		thinking:  saturatingSub(a.thinking, b.thinking),
	}
}

// saturatingSub subtracts without wrapping an unsigned total below zero.
func saturatingSub(a, b uint64) uint64 {
	if b > a {
		return 0
	}
	return a - b
}

// StatusFacts answers the /status panel's spliced session facts. It reports
// false until the session has started, because a status panel for a workspace
// whose session never opened would state nothing the daemon actually knows.
func (r *resolver) StatusFacts(ws ids.WorkspaceID) (StatusFacts, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.stateLocked(ws)
	if !s.started {
		return StatusFacts{}, false
	}
	return StatusFacts{
		Account:        s.email,
		Model:          s.model,
		PermissionMode: s.permissionMode,
	}, true
}

// PermissionModes answers EXACTLY the switchable mode set the daemon served
// for this workspace, in the order it was served. It reports false when no
// picker has been installed, which is the honest answer for a workspace whose
// session never stated one: a SetPermissionMode validated against a set nobody
// served would be validated against nothing.
// ModelCatalog answers exactly the model tokens the selector served, in the
// order they were served, reporting false before a session has stated one. It
// is what a model switch is validated against, for the same reason
// PermissionModes is: the daemon accepts only what it offered.
func (r *resolver) ModelCatalog(ws ids.WorkspaceID) ([]string, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.stateLocked(ws)
	if len(s.catalog) == 0 {
		return nil, false
	}
	models := make([]string, 0, len(s.catalog))
	for _, option := range s.catalog {
		models = append(models, option.GetModel().GetName())
	}
	return models, true
}

func (r *resolver) PermissionModes(ws ids.WorkspaceID) ([]string, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.stateLocked(ws)
	if s.picker == nil {
		return nil, false
	}
	options := s.picker.GetOptions()
	modes := make([]string, 0, len(options))
	for _, option := range options {
		modes = append(modes, option.GetMode())
	}
	return modes, true
}
