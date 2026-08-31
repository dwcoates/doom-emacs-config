package sidebar

import (
	"fmt"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
)

// resolver is the roster resolver. ONE accumulation and ONE topic serve every
// webview: the roster is editor-global, so there is nothing to key by
// workspace except the live half of each row.
type resolver struct {
	colors vocab.RenderColors
	log    dlog.Surfaces

	mu    sync.Mutex
	state *rosterState
	topic publish.Topic[*frontendv1.WorkspaceRoster]
}

// newResolver builds the resolver, asserting the render-colors tables against
// the arms this resolver emits. An arm with no color fails HERE rather than
// drawing an unpainted dot, and a table row no arm claims fails too: a colored
// state nothing can reach means the table and the oneof have drifted.
//
// The assertion is computed here rather than delegated to
// RenderColors.AssertRosterStatusArms because that helper is not landed yet.
// When it lands this collapses to the one call; the guarantee is the same
// either way, and it is the guarantee that matters.
func newResolver(colors vocab.RenderColors, log dlog.Surfaces) (*resolver, error) {
	if log == nil {
		return nil, fmt.Errorf("sidebar resolver needs log surfaces")
	}
	if err := assertTables(colors); err != nil {
		return nil, fmt.Errorf("sidebar resolver refuses to serve an unpainted state: %w", err)
	}
	return &resolver{colors: colors, log: log, state: newRosterState()}, nil
}

// assertTables checks the roster_status table row for row against statusArms,
// and merge_glyphs row for row against the merge arms. Both directions are
// checked: a missing row would draw an unpainted dot, and a surplus row is a
// state the vocabulary paints and the resolver can never emit.
func assertTables(colors vocab.RenderColors) error {
	if len(colors.RosterStatus) == 0 {
		return fmt.Errorf("render-colors carries no roster_status table")
	}
	want := make(map[string]struct{}, len(statusArms))
	for _, arm := range statusArms {
		want[arm] = struct{}{}
		if _, ok := colors.RosterStatus[arm]; !ok {
			return fmt.Errorf("roster_status has no color for the %q arm", arm)
		}
	}
	for arm := range colors.RosterStatus {
		if _, ok := want[arm]; !ok {
			return fmt.Errorf("roster_status colors %q, which is no RosterRow.status arm", arm)
		}
	}
	for _, arm := range mergeArms {
		if _, ok := colors.MergeGlyphs[arm]; !ok {
			return fmt.Errorf("merge_glyphs has no glyph for the %q arm", arm)
		}
	}
	return nil
}

// Topic is the one editor-global roster publication.
func (r *resolver) Topic() *publish.Topic[*frontendv1.WorkspaceRoster] { return &r.topic }

// logFor answers the logger a workspace-scoped record goes to. It resolves the
// workspace's OWN durable sink from the registry's directory; a fact for a
// workspace the registry does not carry is an invariant violation, recorded as
// one rather than written globally by default.
func (r *resolver) logFor(ws ids.WorkspaceID) dlog.Logger {
	for _, rec := range r.state.reg.Workspaces {
		if rec.ID != ws {
			continue
		}
		log, err := r.log.Workspace(rec.Dir)
		if err != nil {
			return r.log.Global().With(dlog.Context{
				"workspace_id":        string(ws),
				"workspace_dir":       rec.Dir,
				"invariant_violation": "the roster could not resolve a registered workspace's log sink",
				"cause":               err.Error(),
			})
		}
		return log.With(dlog.Context{"workspace_id": string(ws)})
	}
	return r.log.Global().With(dlog.Context{
		"workspace_id":        string(ws),
		"invariant_violation": "roster fact for a workspace the registry does not carry",
		"remediation":         "install the registry before routing the workspace's frames",
	})
}

// assertArm records an arm the render-colors table cannot paint. The table is
// asserted whole at construction, so this catches only a table mutated
// underneath the resolver — loudly, never silently.
func (r *resolver) assertArm(arm string, log dlog.Logger) {
	if _, ok := r.colors.RosterStatus[arm]; ok {
		return
	}
	log.Error("daemon.sidebar.assert_arm", "the roster resolved a status arm the render-colors table cannot paint",
		dlog.Context{
			"arm":                 arm,
			"invariant_violation": "roster_status has no row for the arm",
			"remediation":         "add the arm to proto/vocab/render-colors.json",
		})
}

// mutate runs one accumulation change under the lock and republishes the WHOLE
// roster. Every sink method and every setter goes through it, so there is
// exactly one publication site and exactly one readiness gate.
//
// NOTHING PUBLISHES BEFORE A REGISTRY. The roster is a view of the registry: a
// roster built from live frames alone would name no workspaces, which is not a
// smaller roster but a wrong one.
func (r *resolver) mutate(operation, message string, ctx dlog.Context, log dlog.Logger, apply func()) {
	r.mu.Lock()
	apply()
	ready := r.state.regSeen
	var roster *frontendv1.WorkspaceRoster
	if ready {
		roster = r.render(log)
	}
	r.mu.Unlock()

	if ctx == nil {
		ctx = dlog.Context{}
	}
	if !ready {
		log.Debug(operation, "the roster took a fact before any registry and published nothing", ctx)
		return
	}
	log.Debug(operation, message, ctx)
	r.topic.Publish(roster)
}

// mutateWorkspace is mutate for a fact about ONE workspace, resolved onto that
// workspace's own log sink.
func (r *resolver) mutateWorkspace(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState)) {
	r.mu.Lock()
	log := r.logFor(ws)
	r.mu.Unlock()
	if ctx == nil {
		ctx = dlog.Context{}
	}
	ctx["workspace_id"] = string(ws)
	r.mutate(operation, message, ctx, log, func() { apply(r.state.workspace(ws)) })
}

// render builds the WHOLE roster: both groupings, the hoisted merged section
// and the selection.
func (r *resolver) render(log dlog.Logger) *frontendv1.WorkspaceRoster {
	rc := rowContext{sessions: r.state.sessions(), selected: r.state.selected}

	var live, merged []wsm.Workspace
	for _, ws := range r.state.reg.Workspaces {
		if ws.MergedAt != nil {
			merged = append(merged, ws)
			continue
		}
		live = append(live, ws)
	}

	out := &frontendv1.WorkspaceRoster{
		Repository:     r.repositoryView(live, rc, log),
		Task:           r.taskView(live, rc, log),
		RecentlyMerged: r.mergedSection(merged, rc, log),
	}
	if cur := r.currentWorkspace(); cur != nil {
		out.Current = cur
	}
	return out
}

// currentWorkspace resolves the roster's selection, or nil when nothing is
// selected. It is stated by IDENTITY, never by a display name, because names
// collide across repositories.
func (r *resolver) currentWorkspace() *frontendv1.RosterCurrentWorkspace {
	if r.state.selected == nil {
		return nil
	}
	for _, ws := range r.state.reg.Workspaces {
		if ws.ID != *r.state.selected {
			continue
		}
		return &frontendv1.RosterCurrentWorkspace{
			Workspace: &workspacev1.WorkspaceRef{Id: string(ws.ID), Dir: ws.Dir}}
	}
	return nil
}

// ---- daemon-fact setters --------------------------------------------------

// SetRegistry installs the durable half, whole.
func (r *resolver) SetRegistry(reg Registry) {
	r.mutate("daemon.sidebar.set_registry", "the roster took a registry snapshot",
		dlog.Context{
			"workspaces":   len(reg.Workspaces),
			"repositories": len(reg.Repositories),
			"tasks":        len(reg.Tasks),
			"sessions":     len(reg.Sessions),
		}, r.log.Global(), func() {
			r.state.reg = reg
			r.state.regSeen = true
			// The registry's selection is the durable one. A selection the
			// daemon stamped a moment ago stands until WSM catches up, so a
			// registry that carries none does NOT clear it.
			if reg.Current != nil {
				current := *reg.Current
				r.state.selected = &current
			}
			r.forgetNuked()
		})
}

// forgetNuked drops the live half of every workspace the registry no longer
// carries. A NUKED WORKSPACE LEAVES THE ROSTER, and its accumulation leaves
// with it rather than lingering as a leak that would resurrect the row if the
// id were ever reused.
func (r *resolver) forgetNuked() {
	known := make(map[ids.WorkspaceID]struct{}, len(r.state.reg.Workspaces))
	for _, ws := range r.state.reg.Workspaces {
		known[ws.ID] = struct{}{}
	}
	for ws := range r.state.workspaces {
		if _, ok := known[ws]; ok {
			continue
		}
		delete(r.state.workspaces, ws)
	}
	if r.state.selected != nil {
		if _, ok := known[*r.state.selected]; !ok {
			r.state.selected = nil
		}
	}
}

// SetMerge installs one workspace's merge facts.
func (r *resolver) SetMerge(ws ids.WorkspaceID, facts footer.MergeFacts) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_merge", "the roster took the merge facts",
		dlog.Context{"state": facts.State}, func(s *wsState) { s.merge = facts })
}

// SetSelected records the user's selection.
func (r *resolver) SetSelected(ws ids.WorkspaceID) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_selected",
		"the roster took the selection and cleared the workspace's attention marker",
		nil, func(*wsState) {
			selected := ws
			r.state.selected = &selected
		})
}

// SetTurn installs the accepted turn.
func (r *resolver) SetTurn(ws ids.WorkspaceID, turn *footer.TurnStarted) {
	ctx := dlog.Context{"in_flight": turn != nil}
	if turn != nil {
		ctx["act"] = int(turn.Act)
	}
	r.mutateWorkspace(ws, "daemon.sidebar.set_turn", "the roster took the accepted turn", ctx,
		func(s *wsState) { s.startTurn(turn) })
}

// SetTurnEnded installs how the last turn ended.
func (r *resolver) SetTurnEnded(ws ids.WorkspaceID, how TurnClose) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_turn_ended", "the roster took the turn's close",
		dlog.Context{"close": int(how)}, func(s *wsState) {
			s.turn = nil
			s.turnEverRan = true
			s.lastClose = how
			s.compacting = false
		})
}

// SetSummary installs the row detail's summary line.
func (r *resolver) SetSummary(ws ids.WorkspaceID, text string) {
	line := firstLine(text)
	r.mutateWorkspace(ws, "daemon.sidebar.set_summary", "the roster took the row's summary line",
		dlog.Context{"empty": line == ""}, func(s *wsState) { s.summary = line })
}

// ---- SidebarSink ----------------------------------------------------------

// OnSessionStarted marks the row live.
func (r *resolver) OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted) {
	r.mutateWorkspace(ws, "daemon.sidebar.on_session_started", "the roster took a session start",
		dlog.Context{"vendor_session_id": started.GetVendorSessionId()}, func(s *wsState) {
			s.started = true
			s.vendorBlocked = false
		})
}

// OnActivity moves the row to thinking.
func (r *resolver) OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	r.mutateWorkspace(ws, "daemon.sidebar.on_activity", "the roster took a turn's activity",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) { s.sawActivity = true })
}

// OnPermission moves the row to waiting, and moves it off waiting again when
// the gate is DECIDED.
//
// The ask's own result oneof is what says which: `start` opens the gate and
// either `success` or `failure` closes it — opened or closed, a decided gate is
// no longer waiting on the user. Tracking the ask by identity rather than as a
// flag is what lets two gated calls both be answered before the row leaves
// `permission`.
func (r *resolver) OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission) {
	key := permissionKey(agent, p)
	_, open := p.GetResult().(*conversationv1.AgentPermission_Start)
	r.mutateWorkspace(ws, "daemon.sidebar.on_permission", "the roster took a permission ask",
		dlog.Context{"agent_id": agent.GetValue(), "open": open}, func(s *wsState) {
			if open {
				s.permissions[key] = struct{}{}
				return
			}
			delete(s.permissions, key)
		})
}

// OnDetachedWork moves the row to background.
func (r *resolver) OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) {
	r.mutateWorkspace(ws, "daemon.sidebar.on_detached_work", "the roster took a detached-work announcement",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) {
			s.detached[detachedKey(agent, work)] = struct{}{}
		})
}

// OnAgentTerminal retires the row's thinking state.
//
// It retires the AGENT, not the turn: a turn's own close is SetTurnEnded's, and
// the two are different facts — a subagent can end while its turn runs on.
func (r *resolver) OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	ctx := dlog.Context{
		"agent_id": agent.GetValue(),
		"outcome":  terminalOutcome(success, failure),
	}
	if turn != nil {
		ctx["turn_id"] = string(*turn)
	}
	blocked := vendorBlocked(failure)
	ctx["vendor_blocked"] = blocked
	r.mutateWorkspace(ws, "daemon.sidebar.on_agent_terminal", "the roster took an agent terminal", ctx,
		func(s *wsState) {
			for key := range s.permissions {
				if agentOf(key) == agent.GetValue() {
					delete(s.permissions, key)
				}
			}
			if blocked {
				s.vendorBlocked = true
			}
		})
}

// OnSessionUpdate carries the terminals and faults the row reflects.
func (r *resolver) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	if update == nil {
		return
	}
	arm, apply := sessionUpdateArm(update)
	r.mutateWorkspace(ws, "daemon.sidebar.on_session_update", "the roster took a session update",
		dlog.Context{"arm": arm}, apply)
}

// sessionUpdateArm names the update's arm and returns what it changes. EVERY
// arm has a branch, including the ones the roster deliberately draws nothing
// from, so a new arm is a compile-time question rather than a silent default.
func sessionUpdateArm(update *conversationv1.SessionUpdate) (string, func(*wsState)) {
	switch u := update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_QueryDied:
		return "query_died", func(s *wsState) {
			s.vendorBlocked = true
			s.turn = nil
		}
	case *conversationv1.SessionUpdate_RateLimitStatus:
		return "rate_limit_status", func(s *wsState) {
			_, rejected := u.RateLimitStatus.GetStatus().(*conversationv1.SessionRateLimitStatus_Rejected)
			s.vendorBlocked = rejected
		}
	case *conversationv1.SessionUpdate_Compacting:
		return "compacting", func(s *wsState) { s.compacting = true }
	case *conversationv1.SessionUpdate_Diagnostics:
		return "diagnostics", func(s *wsState) { s.degraded = anyWindowOpen(u.Diagnostics) }
	case *conversationv1.SessionUpdate_ModelChanged:
		return "model_changed", func(*wsState) {}
	case *conversationv1.SessionUpdate_PermissionModeChanged:
		return "permission_mode_changed", func(*wsState) {}
	case *conversationv1.SessionUpdate_IdentityRotated:
		return "identity_rotated", func(*wsState) {}
	case *conversationv1.SessionUpdate_FastMode:
		return "fast_mode", func(*wsState) {}
	case *conversationv1.SessionUpdate_McpServer:
		return "mcp_server", func(*wsState) {}
	case *conversationv1.SessionUpdate_AccountUsage:
		return "account_usage", func(*wsState) {}
	case *conversationv1.SessionUpdate_ContextUsage:
		return "context_usage", func(*wsState) {}
	default:
		return "unset", func(*wsState) {}
	}
}

// OnLink drives the init, severed, degraded, dead and start_failed arms.
func (r *resolver) OnLink(ws ids.WorkspaceID, link sessionwatcher.LinkState) {
	r.mutateWorkspace(ws, "daemon.sidebar.on_link", "the roster took a link state",
		dlog.Context{"link": linkName(link)}, func(s *wsState) {
			s.link = link
			s.linkSeen = true
			if link == shimclient.LinkConnected {
				s.everConnected = true
			}
		})
}

// ---- naming ---------------------------------------------------------------

// permissionKey identifies one open consent ask. It is agent-scoped so an
// agent's terminal retires exactly that agent's asks.
func permissionKey(agent *conversationv1.AgentId, p *conversationv1.AgentPermission) string {
	return agent.GetValue() + "\x00" + p.GetId().GetValue()
}

// detachedKey identifies one detached-work item.
func detachedKey(agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) string {
	return agent.GetValue() + "\x00" + work.GetWork().GetValue()
}

// agentOf reads the agent half of a composite key.
func agentOf(key string) string {
	for i := 0; i < len(key); i++ {
		if key[i] == 0 {
			return key[:i]
		}
	}
	return key
}

// terminalOutcome names how an agent's stream ended, for the record.
func terminalOutcome(success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) string {
	switch {
	case success != nil:
		return "success"
	case failure != nil:
		return "failure"
	default:
		return "unset"
	}
}

// vendorBlocked reports whether a failure is the VENDOR's or the ACCOUNT's
// rather than agent-repl's. Only these two arms are: an account-level block and
// a refill breaker are both "the vendor will not serve this account right now",
// which is what the roster's vendor_blocked dot says. Every other failure is a
// run that failed, not a session that cannot proceed.
func vendorBlocked(failure *conversationv1.AgentFailure) bool {
	switch failure.GetFailure().(type) {
	case *conversationv1.AgentFailure_BlockingLimit, *conversationv1.AgentFailure_RapidRefillBreaker:
		return true
	default:
		return false
	}
}

// anyWindowOpen reports whether the diagnostics carry an open degraded window,
// which is what makes a serving link read as degraded.
func anyWindowOpen(d *conversationv1.SessionDiagnostics) bool {
	for _, w := range d.GetDegradedWindows() {
		if _, open := w.GetExtent().(*conversationv1.SessionDegradedWindow_Open); open {
			return true
		}
	}
	return false
}

// linkName spells a link state for the record.
func linkName(link sessionwatcher.LinkState) string {
	switch link {
	case shimclient.LinkDialing:
		return "dialing"
	case shimclient.LinkConnected:
		return "connected"
	case shimclient.LinkRedialing:
		return "redialing"
	case shimclient.LinkDead:
		return "dead"
	default:
		return "unknown"
	}
}
