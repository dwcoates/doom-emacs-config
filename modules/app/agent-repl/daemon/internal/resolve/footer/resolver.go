package footer

import (
	"fmt"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
)

// resolver is the footer resolver. One instance serves every workspace; each
// workspace owns one accumulation and one topic.
type resolver struct {
	log  dlog.Surfaces
	opts options

	mu     sync.Mutex
	states map[ids.WorkspaceID]*wsState
	topics map[ids.WorkspaceID]*publish.Topic[*frontendv1.FooterView]
}

// newResolver builds the resolver with the injectable knobs resolved.
func newResolver(colors vocab.RenderColors, log dlog.Surfaces, opts ...Option) (*resolver, error) {
	if log == nil {
		return nil, fmt.Errorf("footer resolver needs log surfaces")
	}
	if err := colors.AssertFooterStatusArms(statusArms); err != nil {
		return nil, fmt.Errorf("footer resolver refuses to serve an unpainted state: %w", err)
	}
	if err := colors.AssertFooterAllowanceArms(allowanceArms); err != nil {
		return nil, fmt.Errorf("footer resolver refuses to serve an unpainted state: %w", err)
	}
	o := options{
		clock:         SystemClock{},
		dwell:         DefaultMomentaryDwell,
		alarmTokens:   DefaultTokenAlarmThreshold,
		rateNewsworth: DefaultRateLimitNewsworthyThreshold,
		encodeFeedID:  feedid.Encode,
	}
	for _, apply := range opts {
		apply(&o)
	}
	if o.clock == nil {
		return nil, fmt.Errorf("footer resolver needs a clock")
	}
	return &resolver{
		log:    log,
		opts:   o,
		states: map[ids.WorkspaceID]*wsState{},
		topics: map[ids.WorkspaceID]*publish.Topic[*frontendv1.FooterView]{},
	}, nil
}

// Topic is the workspace's footer publication. It exists before the first view
// does: a subscriber that arrives early receives the first view ever published
// rather than an empty one.
func (r *resolver) Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView] {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.topicLocked(ws)
}

// topicLocked resolves the workspace's topic under the resolver's lock.
func (r *resolver) topicLocked(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView] {
	t, ok := r.topics[ws]
	if !ok {
		t = &publish.Topic[*frontendv1.FooterView]{}
		r.topics[ws] = t
	}
	return t
}

// SetWorkspaceDir binds the workspace's directory so its records reach that
// workspace's own sink.
func (r *resolver) SetWorkspaceDir(ws ids.WorkspaceID, dir string) error {
	log, err := r.log.Workspace(dir)
	if err != nil {
		return fmt.Errorf("bind footer resolver to workspace %s at %q: %w", ws, dir, err)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.stateLocked(ws)
	s.dir = dir
	s.log = log.With(dlog.Context{"workspace_id": string(ws)})
	s.log.Debug("daemon.footer.bind", "the footer resolver bound a workspace to its log sink",
		dlog.Context{"workspace_dir": dir})
	return nil
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

// logOf answers the workspace's logger. An unbound workspace is an invariant
// violation, recorded as one on the global sink — the only sink that exists
// before a directory is known — rather than silently dropped.
func (r *resolver) logOf(ws ids.WorkspaceID, s *wsState) dlog.Logger {
	if s.log != nil {
		return s.log
	}
	return r.log.Global().With(dlog.Context{
		"workspace_id":        string(ws),
		"invariant_violation": "footer resolver frame for a workspace with no bound directory",
		"remediation":         "call SetWorkspaceDir at registration",
	})
}

// mutate runs one accumulation change under the lock and republishes the whole
// view. Every sink method and every setter goes through it, so there is
// exactly one publication site.
func (r *resolver) mutate(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState)) {
	r.mu.Lock()
	s := r.stateLocked(ws)
	s.seen = true
	apply(s)
	view := r.render(ws, s)
	topic := r.topicLocked(ws)
	log := r.logOf(ws, s)
	r.mu.Unlock()

	if ctx == nil {
		ctx = dlog.Context{}
	}
	log.Debug(operation, message, ctx)
	topic.Publish(view)
}

// render builds the whole view from the accumulation. Nothing partial is ever
// built: every element resolves from state alone.
func (r *resolver) render(ws ids.WorkspaceID, s *wsState) *frontendv1.FooterView {
	return &frontendv1.FooterView{
		Strip: &frontendv1.FooterStrip{
			Status:   r.status(s),
			Clock:    r.clockCell(s),
			Tokens:   s.tok.cell(),
			LiveWork: r.chips(s),
		},
		Expanded: r.expanded(ws, s),
	}
}

// clockCell renders the turn clock. UNSET is no turn in flight, which is what
// makes the cell render idle rather than a frozen duration.
func (r *resolver) clockCell(s *wsState) *frontendv1.FooterClock {
	out := &frontendv1.FooterClock{}
	if s.turn != nil {
		at := epochMs(s.turn.At)
		out.TurnStartedAtMs = &at
	}
	return out
}

// ---- daemon-fact setters --------------------------------------------------

// OnTurnOpened is the watcher handing over the turn-open edge. It raises
// `thinking submitting` and starts the strip's clock. The ACT is not on this
// edge — only the daemon's own caller knows a turn carries a /clear or a
// compaction — so a turn already installed by SetTurn keeps its act rather
// than being demoted to an ordinary prompt.
func (r *resolver) OnTurnOpened(ws ids.WorkspaceID, turn ids.TurnID) {
	r.mutate(ws, "daemon.footer.on_turn_opened", "the footer took the turn-open edge",
		dlog.Context{"turn_id": string(turn)}, func(s *wsState) {
			act := ActPrompt
			if s.turn != nil {
				act = s.turn.Act
			}
			r.applyTurnStarted(s, &TurnStarted{At: r.opts.clock.Now(), Act: act})
		})
}

// SetTurn installs the accepted turn.
func (r *resolver) SetTurn(ws ids.WorkspaceID, turn *TurnStarted) {
	r.mutate(ws, "daemon.footer.set_turn", "the footer took the accepted turn",
		dlog.Context{"in_flight": turn != nil}, func(s *wsState) {
			r.applyTurnStarted(s, turn)
		})
}

// applyTurnStarted installs (or clears) the in-flight turn on an accumulation.
// It is shared by the daemon-fact setter and the watcher's turn-open edge so
// the two can never drift.
func (r *resolver) applyTurnStarted(s *wsState, turn *TurnStarted) {
	s.turn = turn
	if turn == nil {
		return
	}
	s.turnEverRan = true
	s.sawActivity = false
	s.blocked = nil
	s.interrupted = nil
	// COMPACTING IS OR-ED IN, NEVER ASSIGNED. `SessionUpdate.compacting` is the
	// vendor's own start signal and it lands BEFORE the turn-open edge it
	// belongs to, so assigning here would wipe a compaction the vendor had
	// already announced. The flag is cleared by the end signals — the
	// ContextCut that ends the compaction, and the turn's terminal — never by
	// a turn opening.
	s.compacting = s.compacting || turn.Act == ActCompact
	s.retrying = nil
	s.tok.reset()
	r.cancelMomentary(s)
}

// SetMerge installs the merge facts the footer draws.
func (r *resolver) SetMerge(ws ids.WorkspaceID, facts MergeFacts) {
	r.mutate(ws, "daemon.footer.set_merge", "the footer took the merge facts",
		dlog.Context{"state": facts.State, "active_tab": facts.ActiveTab, "round": facts.Round},
		func(s *wsState) { s.merge = facts })
}

// SetParked installs, or lifts, the idle sweep's park.
func (r *resolver) SetParked(ws ids.WorkspaceID, parked bool) {
	r.mutate(ws, "daemon.footer.set_parked", "the footer took the idle sweep's park",
		dlog.Context{"parked": parked}, func(s *wsState) { s.parked = parked })
}

// SetClosing installs a close refusal.
func (r *resolver) SetClosing(ws ids.WorkspaceID, blocked *CloseBlocked) {
	ctx := dlog.Context{"blocked": blocked != nil}
	if blocked != nil {
		ctx["reason"] = blocked.Reason
	}
	r.mutate(ws, "daemon.footer.set_closing", "the footer took the close refusal", ctx,
		func(s *wsState) { s.closing = blocked })
}

// SetColdGate installs the standing cold gate.
func (r *resolver) SetColdGate(ws ids.WorkspaceID, gate ColdGate) {
	r.mutate(ws, "daemon.footer.set_cold_gate", "the footer took the cold gate",
		dlog.Context{"standing": gate.Standing}, func(s *wsState) { s.coldGate = gate })
}

// SetInterrupting fires waiting·interrupting the moment an interrupt registers.
func (r *resolver) SetInterrupting(ws ids.WorkspaceID, on bool) {
	r.mutate(ws, "daemon.footer.set_interrupting", "the footer took the interrupt registration",
		dlog.Context{"interrupting": on}, func(s *wsState) { s.interrupting = on })
}

// ---- the R1 dwell ---------------------------------------------------------

// armMomentary schedules the one-shot successor push that retires a momentary
// status. Nothing on the wire ticks: the dwell is the DAEMON's timer and its
// only effect is one more whole-view publication.
func (r *resolver) armMomentary(ws ids.WorkspaceID, s *wsState) {
	r.cancelMomentary(s)
	s.momentary = r.opts.clock.AfterFunc(r.opts.dwell, func() { r.retireMomentary(ws) })
}

// cancelMomentary stops an in-flight dwell whose status was superseded.
func (r *resolver) cancelMomentary(s *wsState) {
	if s.momentary != nil {
		s.momentary.Stop()
		s.momentary = nil
	}
}

// retireMomentary clears the momentary statuses and republishes the successor.
func (r *resolver) retireMomentary(ws ids.WorkspaceID) {
	r.mutate(ws, "daemon.footer.retire_momentary",
		"the R1 dwell elapsed and the footer published the momentary status's successor",
		nil, func(s *wsState) {
			s.interrupted = nil
			s.loading = nil
			s.injected = nil
			s.momentary = nil
		})
}

// ---- FooterSink -----------------------------------------------------------

// OnContextBudgetWarning installs the vendor's standing context-budget
// warning. It is an AGENT-PLANE fact — a page line of the agent's book — but
// the footer's activity line is session-scoped, so the warning stands for the
// workspace whichever agent's transcript carried it.
func (r *resolver) OnContextBudgetWarning(ws ids.WorkspaceID, agent *conversationv1.AgentId, warning *conversationv1.ContextBudgetWarning) {
	if warning == nil {
		return
	}
	r.mutate(ws, "daemon.footer.on_context_budget_warning", "the footer took a context-budget warning",
		dlog.Context{"agent_id": agent.GetValue()}, func(s *wsState) {
			s.contextBudget = &standing{text: warning.GetText(), at: r.opts.clock.Now()}
		})
}

// OnLink is the connectivity change the footer reflects.
func (r *resolver) OnLink(ws ids.WorkspaceID, link sessionwatcher.LinkState) {
	r.mutate(ws, "daemon.footer.on_link", "the footer took a link state",
		dlog.Context{"link": linkName(link)}, func(s *wsState) {
			s.link = link
			s.linkSeen = true
			if link == shimclient.LinkConnected {
				s.everConnected = true
			}
			// THE REVIVAL ENDS THE PARK. The session watcher latches a dead
			// link ("a later transition is a consequence of the death",
			// sessionwatcher/watcher.go) and publishes nothing more on it, so
			// any link state arriving after the park belongs to the shim the
			// reviving prompt spawned. Clearing it here — rather than only on
			// LinkConnected — is what makes a revival whose spawn then DIES
			// read `dead` again instead of staying masked as a park.
			s.parked = false
		})
}

// OnSessionUpdate carries the session-scoped facts the footer reflects.
func (r *resolver) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	if update == nil {
		return
	}
	arm, apply := r.sessionArm(ws, update)
	r.mutate(ws, "daemon.footer.on_session_update", "the footer took a session update",
		dlog.Context{"arm": arm}, apply)
}

// sessionArm names the update's arm and returns what it changes. Every arm has
// a branch, including the ones the footer deliberately draws nothing from.
func (r *resolver) sessionArm(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) (string, func(*wsState)) {
	switch u := update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_QueryDied:
		return "query_died", func(s *wsState) {
			s.blocked = &blockedState{
				kind: blockedQueryDied,
				line: "vendor query died — the next prompt restarts it",
				at:   r.opts.clock.Now(),
			}
			s.turn = nil
			s.tok.settled = true
		}
	case *conversationv1.SessionUpdate_AccountUsage:
		// THE FIGURES' SOURCE. The sampled account usage carries both
		// windows' utilization and reset, complete from the first sample;
		// the rate-limit event carries the verdict.
		return "account_usage", func(s *wsState) { r.observeAccountUsage(s, u.AccountUsage) }
	case *conversationv1.SessionUpdate_RateLimitStatus:
		return "rate_limit_status", func(s *wsState) { r.observeRateLimitStatus(ws, s, u.RateLimitStatus) }
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
	case *conversationv1.SessionUpdate_ContextUsage:
		return "context_usage", func(*wsState) {}
	default:
		return "unset", func(*wsState) {}
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

// observeAccountUsage takes one usage sample and files BOTH windows' figures:
// five_hour is the session allowance, seven_day the weekly one. A sample that
// could read no figure (the unavailable arm) leaves the figures on hand
// standing, and a sample whose seven_day window the vendor omitted leaves the
// weekly allowance unfigured, which draws it absent rather than invented.
//
// EVERY sample, readable or not, files its OUTCOME. That is the whole point
// of the outcome cell: a failed read left no trace at all before, so the strip
// went on drawing the last percentage as though it were the current one, and
// nothing on the wire could say otherwise.
func (r *resolver) observeAccountUsage(s *wsState, usage *conversationv1.SessionAccountUsage) {
	if usage == nil {
		return
	}
	if sample := allowanceSample(usage); sample != nil {
		_, unavailable := usage.GetOutcome().(*conversationv1.SessionAccountUsage_Unavailable)
		s.rate.sample = sample
		s.rate.sampleUnread = unavailable
		s.rate.at = r.opts.clock.Now()
	}
	available, ok := usage.GetOutcome().(*conversationv1.SessionAccountUsage_Available)
	if !ok {
		return
	}
	at := usage.GetObservedAtMs()
	moved := false
	if five := available.Available.GetFiveHour(); five != nil {
		moved = s.rate.session.observeSampledFigures(five.GetUtilizationPercent(), five.GetResetsAtMs(), at) || moved
	}
	if seven := available.Available.GetSevenDay(); seven != nil {
		moved = s.rate.weekly.observeSampledFigures(seven.GetUtilizationPercent(), seven.GetResetsAtMs(), at) || moved
	}
	if moved {
		s.rate.at = r.opts.clock.Now()
	}
}

// allowanceSample names the sample's outcome for the strip, arm for arm with
// SessionAccountUsage's own oneof. A sample whose outcome oneof is UNSET
// states nothing — the producer chose no arm — so it files nothing rather
// than retiring an outcome that is still the newest one anybody stated.
func allowanceSample(usage *conversationv1.SessionAccountUsage) *frontendv1.FooterAllowanceSample {
	switch outcome := usage.GetOutcome().(type) {
	case *conversationv1.SessionAccountUsage_Available:
		return &frontendv1.FooterAllowanceSample{
			Outcome: &frontendv1.FooterAllowanceSample_Available{
				Available: &frontendv1.FooterAllowanceSampleAvailable{},
			},
		}
	case *conversationv1.SessionAccountUsage_Unavailable:
		return unavailableSample(outcome.Unavailable)
	default:
		return nil
	}
}

// unavailableSample names WHY no figure was read. An unavailable arm whose
// own reason oneof is UNSET is still an unavailability — the sample failed —
// so it files the sample with no reason arm rather than being dropped, which
// would draw it as a success.
func unavailableSample(unavailable *conversationv1.SessionAccountUsageUnavailable) *frontendv1.FooterAllowanceSample {
	sample := &frontendv1.FooterAllowanceSample{}
	switch reason := unavailable.GetReason().(type) {
	case *conversationv1.SessionAccountUsageUnavailable_ServiceUnavailable:
		sample.Outcome = &frontendv1.FooterAllowanceSample_ServiceUnavailable{
			ServiceUnavailable: &frontendv1.FooterAllowanceSampleServiceUnavailable{},
		}
	case *conversationv1.SessionAccountUsageUnavailable_WindowUnavailable:
		sample.Outcome = &frontendv1.FooterAllowanceSample_WindowUnavailable{
			WindowUnavailable: &frontendv1.FooterAllowanceSampleWindowUnavailable{},
		}
	case *conversationv1.SessionAccountUsageUnavailable_UtilizationUnavailable:
		sample.Outcome = &frontendv1.FooterAllowanceSample_UtilizationUnavailable{
			UtilizationUnavailable: &frontendv1.FooterAllowanceSampleUtilizationUnavailable{},
		}
	case *conversationv1.SessionAccountUsageUnavailable_SamplingFailure:
		sample.Outcome = &frontendv1.FooterAllowanceSample_SamplingFailure{
			SamplingFailure: &frontendv1.FooterAllowanceSampleSamplingFailure{
				Cause: reason.SamplingFailure.GetCause(),
			},
		}
	}
	return sample
}

// observeRateLimitStatus takes one rate-limit event and files it under the
// window it is about: five_hour is the session allowance; seven_day and its
// per-model and overage-included aliases are all the weekly one. The event is
// the VERDICT's only source, so filing it is what lets an allowance's status
// arm join. An event that carries a utilization also supplies the figure, and
// as the newest sighting to arrive it is the one drawn — see `fileFigures`,
// which states why arrival rather than a timestamp orders the two sources.
// The overage window has no cell in the contract, so it
// is logged and dropped rather than drawn against a window it is not about;
// a status naming no window is not filable at all.
func (r *resolver) observeRateLimitStatus(ws ids.WorkspaceID, s *wsState, status *conversationv1.SessionRateLimitStatus) {
	if status == nil {
		return
	}
	var window *allowanceWindow
	switch status.GetRateLimitType().GetWindow().(type) {
	case *conversationv1.SessionRateLimitType_FiveHour:
		window = &s.rate.session
	case *conversationv1.SessionRateLimitType_SevenDay,
		*conversationv1.SessionRateLimitType_SevenDayOpus,
		*conversationv1.SessionRateLimitType_SevenDaySonnet,
		*conversationv1.SessionRateLimitType_SevenDayOverageIncluded:
		window = &s.rate.weekly
	case *conversationv1.SessionRateLimitType_Overage:
		r.logOf(ws, s).Warn("daemon.footer.rate_limit_overage",
			"the vendor reported the overage window, which the footer contract has no allowance cell for",
			dlog.Context{})
		return
	default:
		return
	}
	window.verdict = status
	if status.UtilizationPercent != nil {
		window.observeEventFigures(status.GetUtilizationPercent(), status.GetResetsAtMs())
	}
	s.rate.at = r.opts.clock.Now()
}

// OnQuestion moves the footer to waiting.
func (r *resolver) OnQuestion(ws ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion) {
	if q == nil {
		return
	}
	id := q.GetId().GetValue()
	switch res := q.GetResult().(type) {
	case *conversationv1.AgentQuestion_Start:
		r.mutate(ws, "daemon.footer.on_question", "the footer opened a question batch",
			dlog.Context{"question_id": id}, func(s *wsState) {
				if _, seen := s.questions[id]; !seen {
					s.questionOrder = append(s.questionOrder, id)
				}
				s.questions[id] = questionLead(res.Start.GetBatch())
			})
	default:
		r.mutate(ws, "daemon.footer.on_question", "the footer closed a question batch",
			dlog.Context{"question_id": id}, func(s *wsState) { s.dropQuestion(id) })
	}
}

// dropQuestion retires one answered or failed batch.
func (s *wsState) dropQuestion(id string) {
	delete(s.questions, id)
	s.questionOrder = without(s.questionOrder, id)
}

// questionLead composes the open batch's lead line.
func questionLead(batch *conversationv1.AgentQuestionBatch) string {
	questions := batch.GetQuestions()
	if len(questions) == 0 {
		return "a question is open"
	}
	head := questions[0].GetQuestion().GetText()
	if head == "" {
		head = questions[0].GetHeader()
	}
	return fmt.Sprintf("%s · %s", plural(len(questions), "question"), truncate(head, DefaultWarningRowWidth))
}

// OnPermission moves the footer to waiting.
func (r *resolver) OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission) {
	if p == nil {
		return
	}
	id := p.GetId().GetValue()
	switch res := p.GetResult().(type) {
	case *conversationv1.AgentPermission_Start:
		r.mutate(ws, "daemon.footer.on_permission", "the footer opened a consent ask",
			dlog.Context{"permission_id": id}, func(s *wsState) {
				if _, seen := s.permissions[id]; !seen {
					s.permissionOrder = append(s.permissionOrder, id)
				}
				s.permissions[id] = gatedCall(res.Start.GetPrompt())
			})
	default:
		r.mutate(ws, "daemon.footer.on_permission", "the footer closed a consent ask",
			dlog.Context{"permission_id": id}, func(s *wsState) { s.dropPermission(id) })
	}
}

// dropPermission retires one decided or failed ask.
func (s *wsState) dropPermission(id string) {
	delete(s.permissions, id)
	s.permissionOrder = without(s.permissionOrder, id)
}

// gatedCall composes the gated call's line from the vendor's own rendered
// prompt, so no sentence is reconstructed from a tool and its arguments.
func gatedCall(prompt *conversationv1.AgentPermissionPrompt) string {
	if name := prompt.GetDisplayName(); name != "" && prompt.GetTitle() != "" {
		return truncate(fmt.Sprintf("%s: %s", name, prompt.GetTitle()), DefaultWarningRowWidth)
	}
	if title := prompt.GetTitle(); title != "" {
		return truncate(title, DefaultWarningRowWidth)
	}
	return "consent is needed to continue"
}

// OnApiError is mid-turn evidence the footer draws as a retry notice.
func (r *resolver) OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed) {
	if failed == nil {
		return
	}
	r.mutate(ws, "daemon.footer.on_api_error", "the footer took mid-turn api failure evidence",
		dlog.Context{"kind": apiErrorKind(failed)}, func(s *wsState) {
			attempt := int32(2)
			if s.retrying != nil {
				attempt = s.retrying.attempt + 1
			}
			s.retrying = &retryState{
				attempt: attempt,
				status:  truncate(failed.GetMessage(), DefaultWarningRowWidth),
				at:      r.opts.clock.Now(),
			}
		})
}

// without removes one id from an order slice, preserving the rest.
func without(order []string, id string) []string {
	out := order[:0]
	for _, v := range order {
		if v != id {
			out = append(out, v)
		}
	}
	return out
}

// linkName is the link state's spelling in logs and in the render-colors
// tables.
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

// OnContextCut takes the AgentUpdate.context_cut page line, which is what ENDS
// a clear or a compaction.
//
// THE ASYMMETRY IS THE POINT: `SessionUpdate.compacting` only STARTS
// `thinking · compacting` — no progress figure exists upstream — and this
// record is the only end signal there is. Whichever way the cut went the
// session is idle again afterwards, because a failed compaction cut nothing
// and the turn is over either way.
//
// A FAILED COMPACTION IS NOT SILENCE: nothing was cut and the context is still
// too large, which is exactly what the context-budget line says, so the
// producer's account rides there as the idle status's activity evidence and is
// recorded at WARN.
func (r *resolver) OnContextCut(ws ids.WorkspaceID, agent *conversationv1.AgentId, cut *conversationv1.ContextCut) {
	if cut == nil {
		return
	}
	arm := contextCutArm(cut)
	failed, _ := cut.GetCut().(*conversationv1.ContextCut_CompactionFailed)

	r.mu.Lock()
	s := r.stateLocked(ws)
	s.seen = true
	s.turn = nil
	s.turnEverRan = true
	s.compacting = false
	s.tok.settled = true
	if failed != nil {
		s.contextBudget = &standing{
			text: "compaction failed — " + truncate(failed.CompactionFailed.GetError(), DefaultWarningRowWidth),
			at:   r.opts.clock.Now(),
		}
	}
	view := r.render(ws, s)
	topic := r.topicLocked(ws)
	log := r.logOf(ws, s)
	r.mu.Unlock()

	if failed != nil {
		log.Warn("daemon.footer.on_context_cut",
			"a compaction failed, so nothing was cut and the context is still too large",
			dlog.Context{"arm": arm, "error": failed.CompactionFailed.GetError()})
	} else {
		log.Debug("daemon.footer.on_context_cut", "the footer took a context cut",
			dlog.Context{"arm": arm})
	}
	topic.Publish(view)
}

// contextCutArm names the cut's arm for the record.
func contextCutArm(cut *conversationv1.ContextCut) string {
	switch cut.GetCut().(type) {
	case *conversationv1.ContextCut_Cleared:
		return "cleared"
	case *conversationv1.ContextCut_Compacted:
		return "compacted"
	case *conversationv1.ContextCut_CompactionFailed:
		return "compaction_failed"
	default:
		return "unset"
	}
}

// SetParticipants states whether this workspace's host and web streams are
// held. It is the other two hops of connectivity truth (daemon.md invariant
// 11); the footer draws not-connected while either is down.
func (r *resolver) SetParticipants(ws ids.WorkspaceID, host, web bool) {
	r.mutate(ws, "daemon.footer.set_participants", "the footer took the participant streams' liveness",
		dlog.Context{"host_stream": host, "web_stream": web}, func(s *wsState) {
			s.hostStream = host
			s.webStream = web
		})
}
