package sidebar

import (
	"time"

	"fmt"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/ladder"
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

	// results is told every change of a workspace's last turn result, so the
	// daemon keeps it durable (wsm.SetResult). nil tells nobody.
	results ResultSink
	// pendingResults are the changes a render found, reported after the lock
	// is released.
	pendingResults []resultChange
}

// resultChange is one workspace's changed last turn result.
type resultChange struct {
	ws     ids.WorkspaceID
	result *wsm.TurnResult
}

// newResolver builds the resolver, asserting the render-colors tables against
// the arms this resolver emits. An arm with no color fails HERE rather than
// drawing an unpainted dot, and a table row no arm claims fails too: a colored
// state nothing can reach means the table and the oneof have drifted.
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
// and merge_glyphs row for row against the merge arms, through the vocabulary's
// OWN assertions. Both directions are checked: a missing row would draw an
// unpainted dot, and a surplus row is a state the vocabulary paints and the
// resolver can never emit.
func assertTables(colors vocab.RenderColors) error {
	if err := colors.AssertRosterStatusArms(statusArms); err != nil {
		return err
	}
	return colors.AssertMergeGlyphArms(mergeArms)
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
	changes := r.pendingResults
	r.pendingResults = nil
	// PUBLISHED UNDER THE LOCK THAT RENDERED IT, so views reach the topic in
	// the order the changes were made. Published after the unlock, a change
	// rendered first could be published second, and a stale view overwrote a
	// newer one on the wire (2026-10-02: the roster walked submitting, ready,
	// thinking for an accepted prompt). Topic.Publish only enqueues, so it
	// never waits on a subscriber.
	if ready {
		r.topic.Publish(roster)
	}
	r.mu.Unlock()
	for _, change := range changes {
		r.results(change.ws, change.result)
	}

	if ctx == nil {
		ctx = dlog.Context{}
	}
	if !ready {
		log.Debug(operation, "the roster took a fact before any registry and published nothing", ctx)
		return
	}
	log.Debug(operation, message, ctx)
}

// mutateWorkspace is mutate for a fact about ONE workspace, resolved onto that
// workspace's own log sink.
func (r *resolver) mutateWorkspace(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState)) {
	r.mutateWorkspaceLogged(ws, operation, message, ctx, func(s *wsState, _ dlog.Logger) { apply(s) })
}

// mutateWorkspaceLogged is mutateWorkspace for a change that records a
// transition of its own: apply is handed the workspace's log sink, and runs
// under the same lock as the render it precedes.
func (r *resolver) mutateWorkspaceLogged(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState, dlog.Logger)) {
	r.mu.Lock()
	log := r.logFor(ws)
	r.mu.Unlock()
	if ctx == nil {
		ctx = dlog.Context{}
	}
	ctx["workspace_id"] = string(ws)
	r.mutate(operation, message, ctx, log, func() { apply(r.state.workspace(ws), log) })
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

// SetViewed records that the user has READ the last turn's result, which
// draws the row PARTIAL — but only when the report lands on a TURN-END row
// (done, interrupted or turn_failed), or on a VENDOR_BLOCKED row, whose block
// a failed turn raised: viewing it reads that turn's result (owner ruling,
// 2026-09-28), though the marker itself is still drawn only on a turn end.
//
// The editor is the only caller (MarkWorkspaceViewed): dwell is an editor
// fact, and the editor reports it whatever the status. Whether it takes is
// the roster's decision, made against the arm the row last PUBLISHED — the
// one the user was looking at — under the same lock as the render this
// mutation runs. A report on a turn-end row reads the result: an unread one
// held over live detached work then yields to `idle_async`, drawn FULL, and
// the row comes back PARTIAL on its turn-end arm when that work ends. A
// report on any other row is refused and recorded, and changes nothing.
// There is no companion lowering setter, deliberately: a new turn is what
// makes the next result unread (startTurn, SetTurnEnded).
func (r *resolver) SetViewed(ws ids.WorkspaceID) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.set_viewed",
		"the roster took the editor's viewed report; it reads the result only on a turn-end row",
		nil, func(s *wsState, log dlog.Logger) {
			if !s.lastArmSeen || !readsResult(s.lastArm) {
				log.Debug("daemon.sidebar.row_viewed_refused",
					"the row is not on a turn-end arm, so the viewed report was dropped and the row stays FULL",
					dlog.Context{"status": s.lastArm, "result": s.result.String()})
				return
			}
			if s.result == resultRead {
				return
			}
			log.Info("daemon.sidebar.result_read",
				"the user viewed the turn-end row, so the last turn's result is read", dlog.Context{
					"status":     s.lastArm,
					"was":        s.result.String(),
					"async_live": s.asyncLive(),
				})
			s.result = resultRead
			s.resultSettled = true
		})
}

// SetBringingUp raises or lowers the workspace's bring-up-under-way fact,
// which holds its availability at `pending` until a link connects.
func (r *resolver) SetBringingUp(ws ids.WorkspaceID, bringingUp bool) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_bringing_up",
		"the roster took whether a bring-up is under way",
		dlog.Context{"bringing_up": bringingUp}, func(s *wsState) { s.bringingUp = bringingUp })
}

// SetVendorStart installs where the vendor-start run stands.
func (r *resolver) SetVendorStart(ws ids.WorkspaceID, state VendorStart) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_vendor_start",
		"the roster took where the vendor-start run stands",
		dlog.Context{"vendor_start": state.String()}, func(s *wsState) { s.vendorStart = state })
}

// SetReviving raises or lowers the workspace's REVIVING marker, which draws a
// shimmer across the row's name while its parked session comes back up.
//
// Both edges are the workspace verbs' (reviveIfSessionless): the raise when a
// revival is decided, the lower when it ends whichever way it ended. A marker
// left standing past its revival would say "coming back" about a session that
// already did, or never will.
func (r *resolver) SetReviving(ws ids.WorkspaceID, reviving bool) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_reviving",
		"the roster took the workspace's revival edge",
		dlog.Context{"reviving": reviving}, func(s *wsState) { s.reviving = reviving })
}

// SetTurn installs the accepted turn.
func (r *resolver) SetTurn(ws ids.WorkspaceID, turn *footer.TurnStarted) {
	ctx := dlog.Context{"in_flight": turn != nil}
	if turn != nil {
		ctx["act"] = int(turn.Act)
	}
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.set_turn", "the roster took the accepted turn", ctx,
		func(s *wsState, log dlog.Logger) {
			if turn != nil && s.result == resultUnread {
				log.Info("daemon.sidebar.result_unread_cleared",
					"a new prompt started a turn, so the last turn's unread result no longer holds the row", nil)
			}
			s.startTurn(turn)
		})
}

// OnTurnRunningAtAttach stands the turn an adopted shim was already running
// when its watcher attached (sessionwatcher.SidebarSink), so a daemon that took
// a busy workspace over draws it `thinking` rather than the idle rung. A turn
// that already stands is this daemon's own and stays as it is. The turn is
// running, so it is acknowledged; the roster draws no clock, so the start is
// kept only when it is known.
func (r *resolver) OnTurnRunningAtAttach(ws ids.WorkspaceID, turn ids.TurnID, startedAt *time.Time) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.on_turn_running_at_attach", "the roster took a turn the adopted shim is running",
		dlog.Context{"turn_id": string(turn), "row_known": startedAt != nil}, func(s *wsState, log dlog.Logger) {
			if s.turn != nil {
				log.Debug("daemon.sidebar.on_turn_running_at_attach",
					"a turn already stands; the adopted one is this daemon's own", dlog.Context{"turn_id": string(turn)})
				return
			}
			started := &footer.TurnStarted{Act: footer.ActPrompt}
			if startedAt != nil {
				started.At = *startedAt
			}
			s.startTurn(started)
			s.sawActivity = true
		})
}

// AckTurn records the shim's acceptance of the turn, ending `submitting`.
func (r *resolver) AckTurn(ws ids.WorkspaceID) {
	r.mutateWorkspace(ws, "daemon.sidebar.ack_turn", "the roster took the turn's ack", nil,
		func(s *wsState) {
			if s.turn != nil {
				s.sawActivity = true
			}
		})
}

// SetTurnEnded installs how the last turn ended.
func (r *resolver) SetTurnEnded(ws ids.WorkspaceID, how TurnClose) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.set_turn_ended", "the roster took the turn's close",
		dlog.Context{"close": int(how)}, func(s *wsState, log dlog.Logger) {
			cut := contextCut(s.turn)
			s.turn = nil
			// THE TURN'S END IS THE RETRY'S END: nothing is left to retry for.
			s.retrying = ""
			s.turnEverRan = true
			s.lastClose = how
			s.compacting = false
			s.settleResult()
			// A COMPLETED, INTERRUPTED or FAILED turn leaves a result the
			// user has not read. A close this build does not know is a
			// contract breach: it is recorded loudly and leaves no tracked
			// result, and since it is still a NEW ending, a read state from
			// before it does not carry over.
			end, known := ladder.ResolveTurnEnd(how, s.lastFailure)
			if !known {
				s.result = resultNone
				log.Error("daemon.sidebar.set_turn_ended",
					"the roster took a turn close it has no turn-end arm for", dlog.Context{
						"close":               int(how),
						"invariant_violation": "every turn close resolves to a turn-end arm",
						"remediation":         "add the close to ladder.ResolveTurnEnd",
					})
				return
			}
			// A CONTEXT CUT THAT SUCCEEDED HAS NOTHING TO READ (owner ruling,
			// 2026-10-02): a /clear or a compaction the user asked for leaves no
			// response, only the cut, so its `done` is read the moment it lands
			// and the row is drawn PARTIAL on the very push that ends it — no
			// dwell, so no editor timer is involved. A cut that was interrupted
			// or failed is a result like any other turn's and stays unread.
			if cut != "" && end == ladder.TurnEndDone {
				s.result = resultRead
				log.Info("daemon.sidebar.result_read_on_cut",
					"a context cut completed, so its result is read at once and the row is drawn PARTIAL with no dwell", dlog.Context{
						"act":        cut,
						"status":     end.String(),
						"async_live": s.asyncLive(),
					})
				return
			}
			s.result = resultUnread
			log.Info("daemon.sidebar.result_unread",
				"the turn ended, so its result is unread until the user views the row", dlog.Context{
					"status":     end.String(),
					"async_live": s.asyncLive(),
				})
		})
}

// contextCut names the context cut the turn carries — "clear" or "compact" —
// and is empty for an ordinary prompt or when no turn stands. A vendor's
// auto-compaction runs INSIDE an ordinary prompt's turn, so it is not a cut
// the user asked for and is not named here.
func contextCut(turn *footer.TurnStarted) string {
	if turn == nil {
		return ""
	}
	switch turn.Act {
	case footer.ActClear:
		return "clear"
	case footer.ActCompact:
		return "compact"
	default:
		return ""
	}
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
			s.stateUnreported = false
		})
}

// SetStateUnreported installs, or lifts, the fact that a shim taken back after
// a failed handover has not re-reported its session state. A session start
// lifts it too (OnSessionStarted), which is the re-report it waits for.
func (r *resolver) SetStateUnreported(ws ids.WorkspaceID, unreported bool) {
	r.mutateWorkspace(ws, "daemon.sidebar.set_state_unreported", "the roster took whether the shim's session state is unreported",
		dlog.Context{"unreported": unreported}, func(s *wsState) { s.stateUnreported = unreported })
}

// OnActivity moves the row to thinking.
func (r *resolver) OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	r.mutateWorkspaceLogged(ws, "daemon.sidebar.on_activity", "the roster took a turn's activity",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState, log dlog.Logger) {
			s.sawActivity = true
			if s.retrying != "" && s.retrying == agent.GetValue() && ladder.RetryAnswered(act) {
				log.Debug("daemon.sidebar.retry_cleared", "the retried call was answered; the row leaves api_retrying",
					dlog.Context{"agent_id": agent.GetValue()})
				s.retrying = ""
			}
		})
}

// OnApiError stands the vendor's mid-turn retry of AGENT's call: the row is
// `api_retrying` (blue) until that agent is answered, the turn ends, or a new
// turn opens — the same lifetime the footer's `blocked · api_retrying` has.
func (r *resolver) OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed) {
	if failed == nil {
		return
	}
	r.mutateWorkspace(ws, "daemon.sidebar.on_api_error", "the roster took mid-turn api failure evidence",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) { s.retrying = agent.GetValue() })
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

// OnLiveWorkChanged takes the watcher's authoritative live-work set, which is
// what RETIRES `idle_async`. An announcement can only raise the arm: nothing on
// the agent's stream states that a detached item ended, and the watcher — which
// reaps each item's watch at its terminal — is the one party that knows.
func (r *resolver) OnLiveWorkChanged(ws ids.WorkspaceID, live LiveWorkSet) {
	r.mutateWorkspace(ws, "daemon.sidebar.on_live_work_changed", "the roster took the live-work set",
		dlog.Context{
			"agents": len(live.Agents), "shells": len(live.Shells), "monitors": len(live.Monitors),
		}, func(s *wsState) {
			s.liveWork = live
			s.liveWorkSeen = true
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
	// ONLY THE TURN'S OWN TERMINAL SPEAKS FOR THE WORKSPACE. A subagent's
	// failure ends the subagent, not the session, and the footer has only
	// ever read the main thread's terminal: reading every agent's here let a
	// failed subagent paint the whole row blocked beside a strip that was not.
	class := ladder.NoFailure
	if turn != nil {
		class = ladder.ClassifyFailure(failure)
	}
	ctx["failure_class"] = class.String()
	r.mutateWorkspace(ws, "daemon.sidebar.on_agent_terminal", "the roster took an agent terminal", ctx,
		func(s *wsState) {
			for key := range s.permissions {
				if agentOf(key) == agent.GetValue() {
					delete(s.permissions, key)
				}
			}
			if turn == nil {
				return
			}
			s.lastFailure = class
			if class == ladder.VendorBlocked {
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
		// A DEAD QUERY IS A FAILED TURN, NOT A BLOCK (owner ruling,
		// 2026-09-28): the watcher closes the turn it cut as failed, which
		// the roster draws `turn_failed`, and nothing about the vendor or the
		// account refuses the session.
		return "query_died", func(s *wsState) {
			s.turn = nil
			s.retrying = ""
		}
	case *conversationv1.SessionUpdate_RateLimitStatus:
		return "rate_limit_status", func(s *wsState) {
			s.vendorBlocked = ladder.RateLimitBlocks(u.RateLimitStatus)
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

// OnLink drives the init, severed, dead and start_failed arms.
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
