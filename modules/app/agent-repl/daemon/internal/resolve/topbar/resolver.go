package topbar

import (
	"fmt"
	"strings"
	"sync"

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
	return r.log.Global().With(dlog.Context{
		"workspace_id":        string(ws),
		"invariant_violation": "topbar resolver frame for a workspace with no bound directory",
		"remediation":         "call SetWorkspaceDir at registration",
	})
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
	topic := r.topicLocked(ws)
	log := r.logOf(ws, s)
	r.mu.Unlock()

	if ctx == nil {
		ctx = dlog.Context{}
	}
	switch {
	case err != nil:
		ctx["cause"] = err.Error()
		log.Error(operation, "the topbar could not be resolved and nothing was published", ctx)
	case view == nil:
		ctx["awaiting"] = strings.Join(missing, ",")
		log.Debug(operation, "the topbar took a fact and is not yet complete", ctx)
	default:
		log.Debug(operation, "the topbar took a fact and republished", ctx)
		topic.Publish(view)
	}
}

// render builds the whole view, or nil while the workspace is not ready. It
// NEVER builds a partial view: the contract's non-optional fields are
// semantically non-optional, and a push that left one empty would violate them.
func (r *resolver) render(s *wsState) (*frontendv1.TopbarView, error) {
	connectivity, err := r.connectivity(connectivityKey(s.linkSeen, s.link))
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
		Connectivity:         connectivity,
		Warnings:             r.warningStrip(s),
		Context:              r.contextChip(s),
		Account:              r.account(s),
		PermissionModePicker: r.permissionModePicker(s),
	}, nil
}

// title composes the title line: the workspace's name, plus the branch when the
// branch is worth showing. A workspace sitting on its repository's DEFAULT
// branch is named by its name alone — the branch would say nothing — so the
// comparison is what decides, never the branch's mere presence.
func (r *resolver) title(s *wsState) string {
	name := s.naming.Title
	if name == "" {
		name = s.naming.Slug
	}
	branch := s.naming.Branch
	if branch == "" || branch == s.naming.DefaultBranch {
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

// displayMode renders a mode's wire spelling for a reader: the schema spells
// arms with underscores, and a label never does.
func displayMode(mode string) string {
	return strings.ReplaceAll(mode, "_", " ")
}

// contextChip renders the chip and its always-populated hover content.
func (r *resolver) contextChip(s *wsState) *frontendv1.TopbarContextChip {
	return &frontendv1.TopbarContextChip{
		Text:      formatTokens(s.contextUsage.GetTotalTokens()),
		Breakdown: r.tokenBreakdown(s),
	}
}

// account renders which account the session spends as. THE ARM IS THE STATE: a
// logged-out root is a drawn warning, because a session whose root is logged
// out cannot run a turn and a blank would read as "loading".
func (r *resolver) account(s *wsState) *frontendv1.TopbarAccount {
	if s.email == "" {
		return &frontendv1.TopbarAccount{
			State: &frontendv1.TopbarAccount_LoggedOut{
				LoggedOut: &frontendv1.TopbarAccountLoggedOut{},
			},
		}
	}
	return &frontendv1.TopbarAccount{
		State: &frontendv1.TopbarAccount_LoggedIn{
			LoggedIn: &frontendv1.TopbarAccountLoggedIn{Email: s.email},
		},
	}
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

// SetModelCatalog installs the switchable model set.
func (r *resolver) SetModelCatalog(ws ids.WorkspaceID, models []*conversationv1.ModelOption) {
	r.mutate(ws, "daemon.topbar.set_model_catalog", "the topbar took the model catalog",
		dlog.Context{"options": len(models)},
		func(s *wsState) { s.catalog = models })
}

// SetPermissionModePicker installs exactly the switchable set the daemon will
// accept.
func (r *resolver) SetPermissionModePicker(ws ids.WorkspaceID, picker *frontendv1.TopbarPermissionModePicker) {
	r.mutate(ws, "daemon.topbar.set_permission_mode_picker",
		"the topbar took the permission-mode picker",
		dlog.Context{"options": len(picker.GetOptions())},
		func(s *wsState) { s.picker = picker })
}

// SetAccount installs the account read from the config root.
func (r *resolver) SetAccount(ws ids.WorkspaceID, email string) {
	r.mutate(ws, "daemon.topbar.set_account", "the topbar took the account",
		dlog.Context{"logged_in": email != ""}, func(s *wsState) {
			s.email = email
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
			if catalog := started.GetModelCatalog(); len(catalog) > 0 {
				s.catalog = catalog
			}
		})
}

// OnLink drives the connectivity glyph and its tone.
func (r *resolver) OnLink(ws ids.WorkspaceID, link sessionwatcher.LinkState) {
	r.mutate(ws, "daemon.topbar.on_link", "the topbar took a link state",
		dlog.Context{"link": connectivityKey(true, link)}, func(s *wsState) {
			s.link = link
			s.linkSeen = true
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

// observeResponse counts the session's settled responses, which is the
// accounting reconciliation's denominator.
func (r *resolver) observeResponse(s *wsState, unit string, act *conversationv1.AgentActivity) {
	item, ok := act.GetItem().(*conversationv1.AgentActivity_Response)
	if !ok {
		return
	}
	switch item.Response.GetResult().(type) {
	case *conversationv1.AgentResponse_Success, *conversationv1.AgentResponse_Failure:
		s.responses[unit] = true
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
	case *conversationv1.SessionUpdate_ContextUsage:
		return "context_usage", func(s *wsState) { s.contextUsage = u.ContextUsage }
	case *conversationv1.SessionUpdate_QueryDied:
		return "query_died", func(*wsState) {}
	case *conversationv1.SessionUpdate_AccountUsage:
		return "account_usage", func(*wsState) {}
	case *conversationv1.SessionUpdate_FastMode:
		return "fast_mode", func(*wsState) {}
	case *conversationv1.SessionUpdate_McpServer:
		return "mcp_server", func(*wsState) {}
	case *conversationv1.SessionUpdate_ContextBudgetWarning:
		return "context_budget_warning", func(*wsState) {}
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
