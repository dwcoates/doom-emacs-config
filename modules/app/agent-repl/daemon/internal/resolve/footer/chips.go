package footer

import (
	"sort"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/figures"
	"claude-repld/internal/ids"
)

// OnActivity advances the status tree, the token accounting and the live-work
// chips. Every arm the footer draws anything from has a branch; the rest are
// recorded as observed and change nothing.
func (r *resolver) OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	if act == nil {
		return
	}
	unit := act.GetActivityId().GetValue()
	arm := activityArm(act)
	r.mutate(ws, "daemon.footer.on_activity", "the footer took an activity frame",
		dlog.Context{"arm": arm, "activity_id": unit}, func(s *wsState) {
			s.sawActivity = true
			// FILED BEFORE THE USAGE IS FOLDED IN: the ledger needs to know
			// whether THIS frame carried usage to decide which API response
			// the unit belongs to, and folding first would tell it nothing new.
			s.tok.responses.Observe(unit, act.GetUsage() != nil)
			s.tok.observeUsage(unit, s.usageAgent(agent.GetValue()), act.Usage)
			s.tok.evaluateAlarm(r.opts.alarmTokens)
			r.applyActivity(ws, s, unit, act)
		})
}

// activityArm names the activity's kind for the record.
func activityArm(act *conversationv1.AgentActivity) string {
	switch act.GetItem().(type) {
	case *conversationv1.AgentActivity_Thinking:
		return "thinking"
	case *conversationv1.AgentActivity_Response:
		return "response"
	case *conversationv1.AgentActivity_Hook:
		return "hook"
	case *conversationv1.AgentActivity_Subagent:
		return "subagent"
	case *conversationv1.AgentActivity_Bash:
		return "bash"
	case *conversationv1.AgentActivity_TaskAct:
		return "task_act"
	case *conversationv1.AgentActivity_Monitor:
		return "monitor"
	case *conversationv1.AgentActivity_Cron:
		return "cron"
	case *conversationv1.AgentActivity_ScheduleWakeup:
		return "schedule_wakeup"
	case *conversationv1.AgentActivity_ContextInjected:
		return "context_injected"
	case *conversationv1.AgentActivity_PushNotification:
		return "push_notification"
	default:
		return "other"
	}
}

// applyActivity folds one activity frame into the accumulation.
func (r *resolver) applyActivity(ws ids.WorkspaceID, s *wsState, unit string, act *conversationv1.AgentActivity) {
	switch item := act.GetItem().(type) {
	case *conversationv1.AgentActivity_Response:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Response"})
		r.applyResponse(s, unit, item.Response)
	case *conversationv1.AgentActivity_Hook:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Hook"})
		r.applyHook(s, item.Hook)
	case *conversationv1.AgentActivity_Subagent:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Subagent"})
		r.applySubagent(s, unit, item.Subagent)
	case *conversationv1.AgentActivity_Bash:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Bash"})
		r.applyBash(s, unit, item.Bash)
	case *conversationv1.AgentActivity_TaskAct:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_TaskAct"})
		r.applyTaskAct(ws, s, item.TaskAct)
	case *conversationv1.AgentActivity_Monitor:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Monitor"})
		r.applyMonitor(s, unit, item.Monitor)
	case *conversationv1.AgentActivity_Cron:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Cron"})
		r.applyCron(s, item.Cron)
	case *conversationv1.AgentActivity_ScheduleWakeup:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_ScheduleWakeup"})
		r.applyWakeup(s, item.ScheduleWakeup)
	case *conversationv1.AgentActivity_ContextInjected:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_ContextInjected"})
		r.applyInjection(ws, s, item.ContextInjected)
	case *conversationv1.AgentActivity_PushNotification:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_PushNotification"})
		r.applyNotification(s, item.PushNotification)
	}
}

// applyResponse tracks the turn's responses: how many settled (the verdict's
// denominator) and the latency to the current response's first token.
func (r *resolver) applyResponse(s *wsState, unit string, resp *conversationv1.AgentResponse) {
	now := r.opts.clock.Now()
	switch resp.GetResult().(type) {
	case *conversationv1.AgentResponse_Start:
		s.tok.responseStart[unit] = now
	case *conversationv1.AgentResponse_Update:
		if start, ok := s.tok.responseStart[unit]; ok && s.tok.firstToken == nil {
			latency := now.Sub(start)
			s.tok.firstToken = &latency
		}
	case *conversationv1.AgentResponse_Success, *conversationv1.AgentResponse_Failure:
		s.tok.responses.Settle(unit)
	}
}

// applyHook raises the hook line while a hook runs and drops it when the hook
// settles, whichever way it settled.
func (r *resolver) applyHook(s *wsState, hook *conversationv1.AgentHook) {
	if start, running := hook.GetResult().(*conversationv1.AgentHook_Start); running {
		s.hook = &hookState{name: start.Start.GetHookName(), at: r.opts.clock.Now()}
		return
	}
	s.hook = nil
}

// applySubagent maintains the ⚙ chip's rows from the SPAWNING CALL's own
// activity stream. A row leaves the list when the spawn reaches a terminal,
// which is what makes the chip mean "live".
//
// A DETACHED RUN IS NOT RETIRED HERE. Once the work has left the turn it is
// addressed by its handle and its terminal arrives on whichever stream carries
// the work -- the caller's book or the agent's own -- so the retirement is the
// handle's (OnSubagent), never an inference from the call that spawned it. The
// spawning call returning is a LAUNCH RECEIPT and says nothing about the run.
func (r *resolver) applySubagent(s *wsState, unit string, sub *conversationv1.AgentSubagent) {
	switch item := sub.GetResult().(type) {
	case *conversationv1.AgentSubagent_Start:
		// A SPAWN UNIT REPLAYED AFTER ITS RUN SETTLED IS THE SAME REPLAY
		// OnSubagent guards against, arriving on the CALLER's stream instead:
		// the handle and the spawn unit are one value, so a start naming a
		// retired handle re-opened a row the terminal had already taken away.
		if _, done := s.retiredWork[unit]; done {
			return
		}
		row, ok := s.agents[unit]
		if !ok {
			row = &agentRow{spawnUnit: unit, order: s.nextOrder()}
			s.agents[unit] = row
		}
		row.createdAgent = item.Start.GetCreatedAgentId().GetValue()
		row.label = subagentLabel(item.Start.GetPrompt())
		row.description = item.Start.GetPrompt().GetDescription()
		row.startedAt = time.UnixMilli(item.Start.GetStartedAt().GetAtMs())
	case *conversationv1.AgentSubagent_Update:
		if row, ok := s.agents[unit]; ok {
			row.tokens = item.Update.GetProgress().GetTotalTokens()
			if desc := item.Update.GetPrompt().GetDescription(); desc != "" {
				row.description = desc
			}
		}
	default:
		if row, ok := s.agents[unit]; ok && row.work != "" {
			return
		}
		delete(s.agents, unit)
	}
}

// subagentLabel is the row's leading label: the subagent type when the caller
// named one, and the description's opening otherwise — never a blank label.
func subagentLabel(prompt *conversationv1.AgentSubagentPrompt) string {
	if t := prompt.GetSubagentType(); t != "" {
		return t
	}
	if d := prompt.GetDescription(); d != "" {
		return truncate(d, 32)
	}
	return "subagent"
}

// applyBash records an in-turn shell so a later detachment can describe it.
// An in-turn shell has no chip: the $ chip counts DETACHED shells.
func (r *resolver) applyBash(s *wsState, unit string, bash *conversationv1.AgentBash) {
	start, ok := bash.GetResult().(*conversationv1.AgentBash_Start)
	if !ok {
		return
	}
	s.bashUnits[unit] = &shellRow{
		command:   truncate(start.Start.GetCommand().GetLine(), DefaultWarningRowWidth),
		startedAt: time.UnixMilli(start.Start.GetStartedAt().GetAtMs()),
	}
}

// applyTaskAct maintains the ☑ chip's checklist. The tracker's `deleted`
// status removes the row rather than drawing a fourth glyph, and a REJECTED
// act still carries the task as it stands, so the state is applied either way.
//
// WHAT AN ACT DID NOT STATE IS NOT A FIELD IT STATED EMPTY, and both halves of
// that were defects a checklist reader could see.
//
//   - AN UNSET STATUS LEAVES THE TASK WHERE IT STANDS. `AgentTaskState.status`
//     is a oneof precisely so "the act said nothing about where this stands"
//     is representable, and the shim's own converter is explicit that it leaves
//     the arm unset for an update that moved nothing ("UNSET, not a default").
//     This read it as `pending` and INVENTED a status the producer refused to
//     state -- so every subject-only or blocked-by-only update knocked a
//     running task back to unstarted. A row that never existed before still
//     starts pending, because "recorded and not begun" IS what a new entry is.
//   - AN UNSTATED SUBJECT IS NOT A SUBJECT. A `TaskUpdate` that names only a
//     status carries no subject at all, and this overwrote the one the create
//     established -- so the whole checklist drew as blank lines beside its
//     glyphs, observed in the G52 playbook.
//
// PRESENCE IS WHAT SETTLES THE SECOND ONE, and it is now on the wire:
// `AgentTaskState.subject` is `optional`, so an act that names no subject is
// UNSET and an act that names an empty one is SET to "". The two are read
// apart here rather than guessed at -- absent leaves the checklist's own
// subject standing, present installs what the act states, whatever it states.
func (r *resolver) applyTaskAct(ws ids.WorkspaceID, s *wsState, act *conversationv1.AgentTaskAct) {
	id := act.GetTask().GetValue()
	state := act.GetState()
	if _, deleted := state.GetStatus().(*conversationv1.AgentTaskState_Deleted); deleted {
		delete(s.tasks, id)
		return
	}
	subject := state.Subject
	row, ok := s.tasks[id]
	if !ok {
		// A CHECKLIST ENTRY IS A SUBJECT, so an act that names none cannot
		// open one. The case that forced this is a `TaskUpdate` the tracker
		// REFUSED for an id it does not hold: its announcement and its
		// rejection both name the task and neither names a subject, and the
		// checklist gained a PHANTOM ROW -- a bare glyph with no words beside
		// it, counted in the ☑ chip's denominator, for a task the tracker had
		// just said it does not have. `AgentTaskRejected` states it outright:
		// "Nothing was added and nothing changed". Photographed by the G52
		// playbook.
		//
		// It is stated rather than dropped quietly, because the other way to
		// reach here is a footer that missed the create's own frame, and that
		// is worth seeing in the log.
		if subject == nil {
			r.logOf(ws, s).Debug("daemon.footer.task_act_unheld",
				"a task act names no subject and no entry is held for it; the checklist is unchanged",
				dlog.Context{"task": id})
			return
		}
		row = &taskRow{id: id, order: s.nextOrder(), status: taskPending}
		s.tasks[id] = row
	}
	if subject != nil {
		row.subject = *subject
	}
	switch status := state.GetStatus().(type) {
	case *conversationv1.AgentTaskState_Running:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentTaskState_Running"})
		row.status = taskRunning
		row.activeForm = status.Running.GetActiveForm()
	case *conversationv1.AgentTaskState_Completed:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentTaskState_Completed"})
		row.status = taskCompleted
		row.activeForm = ""
	case *conversationv1.AgentTaskState_Pending:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentTaskState_Pending"})
		row.status = taskPending
		row.activeForm = ""
	}
}

// applyMonitor maintains the 👁 chip's rows.
func (r *resolver) applyMonitor(s *wsState, unit string, monitor *conversationv1.AgentMonitor) {
	start, armed := monitor.GetResult().(*conversationv1.AgentMonitor_Start)
	if !armed {
		delete(s.monitors, unit)
		return
	}
	_, persistent := start.Start.GetLifetime().(*conversationv1.AgentMonitorStart_Persistent)
	s.monitors[unit] = &monitorRow{
		unit:        unit,
		description: start.Start.GetDescription(),
		persistent:  persistent,
		startedAt:   time.UnixMilli(start.Start.GetStartedAtMs()),
		order:       s.nextOrder(),
	}
}

// applyCron maintains the ⏱ chip's job set. A listing replaces the set whole,
// exactly as the contract states the panel consumes it; a create adds one and
// a delete removes one.
func (r *resolver) applyCron(s *wsState, cron *conversationv1.AgentCron) {
	success, ok := cron.GetState().(*conversationv1.AgentCron_Success)
	if !ok {
		return
	}
	switch act := success.Success.GetAct().(type) {
	case *conversationv1.AgentCronSuccess_Listed:
		s.crons = map[string]*cronRow{}
		for _, job := range act.Listed.GetJobs() {
			s.crons[job.GetJobId()] = &cronRow{
				id:            job.GetJobId(),
				cron:          job.GetCron(),
				humanSchedule: job.GetHumanSchedule(),
				prompt:        job.GetPrompt(),
				recurring:     job.GetRecurring(),
				durable:       job.GetDurable(),
				order:         s.nextOrder(),
			}
		}
	case *conversationv1.AgentCronSuccess_Created:
		s.crons[act.Created.GetJobId()] = &cronRow{
			id:            act.Created.GetJobId(),
			humanSchedule: act.Created.GetHumanSchedule(),
			recurring:     act.Created.GetRecurring(),
			durable:       act.Created.GetDurable(),
			order:         s.nextOrder(),
		}
	case *conversationv1.AgentCronSuccess_Deleted:
		delete(s.crons, act.Deleted.GetJobId())
	}
}

// applyWakeup tracks the pending self-scheduled wakeup that drives the
// waiting·wakeup fallback.
func (r *resolver) applyWakeup(s *wsState, wakeup *conversationv1.AgentScheduleWakeup) {
	switch item := wakeup.GetResult().(type) {
	case *conversationv1.AgentScheduleWakeup_Start:
		if schedule, ok := item.Start.GetAct().(*conversationv1.AgentScheduleWakeupStart_Schedule); ok {
			s.wakeup = &wakeupState{reason: schedule.Schedule.GetReason(), at: r.opts.clock.Now()}
		}
	case *conversationv1.AgentScheduleWakeup_Success:
		switch outcome := item.Success.GetOutcome().(type) {
		case *conversationv1.AgentScheduleWakeupSuccess_Scheduled:
			reason := ""
			if s.wakeup != nil {
				reason = s.wakeup.reason
			}
			s.wakeup = &wakeupState{
				wakeAt: time.UnixMilli(outcome.Scheduled.GetWakeAtMs()),
				reason: reason,
				at:     r.opts.clock.Now(),
			}
		case *conversationv1.AgentScheduleWakeupSuccess_Stopped:
			s.wakeup = nil
		}
	default:
		s.wakeup = nil
	}
}

// applyInjection raises the MOMENTARY loading status. The kind is derived from
// what the injection carries: a memory file is `memory`; ONE skill with
// content is an `invoked` skill; several with content are `discovered`; and
// content-free entries are a `listing`.
func (r *resolver) applyInjection(ws ids.WorkspaceID, s *wsState, injected *conversationv1.AgentContextInjected) {
	now := r.opts.clock.Now()
	switch item := injected.GetInjected().(type) {
	case *conversationv1.AgentContextInjected_Memory:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentContextInjected_Memory"})
		s.loading = &loadingState{kind: loadingMemory, line: item.Memory.GetPath(), at: now}
	case *conversationv1.AgentContextInjected_Skills:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentContextInjected_Skills"})
		skills := item.Skills.GetSkills()
		withContent := 0
		for _, skill := range skills {
			if skill.Content != nil {
				withContent++
			}
		}
		switch {
		case withContent == 1 && len(skills) == 1:
			r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case withContent == 1 && len(skills) == 1"})
			s.loading = &loadingState{kind: loadingInvoked, line: skills[0].GetName(), at: now}
		case withContent > 0:
			r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case withContent > 0"})
			s.loading = &loadingState{
				kind: loadingDiscovered, line: plural(len(skills), "skill"), at: now}
		default:
			r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "default"})
			s.loading = &loadingState{
				kind: loadingListing, line: plural(len(skills), "skill"), at: now}
		}
	default:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "default"})
		return
	}
	s.injected = &standing{text: s.loading.line, at: now}
	r.armMomentary(ws, s)
}

// applyNotification raises the standing notification, the highest-ranking
// activity line there is.
func (r *resolver) applyNotification(s *wsState, note *conversationv1.AgentPushNotification) {
	start, ok := note.GetState().(*conversationv1.AgentPushNotification_Start)
	if !ok {
		return
	}
	s.notification = &standing{
		text: truncate(start.Start.GetMessage(), DefaultWarningRowWidth),
		at:   r.opts.clock.Now(),
	}
}

// OnAgentTerminal retires an agent from the status tree. A terminal carrying a
// TURN is the main thread's: it settles the token accounting, ends the clock,
// and may raise the momentary interrupted status or a standing block.
func (r *resolver) OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	r.mutate(ws, "daemon.footer.on_agent_terminal", "the footer took an agent terminal",
		dlog.Context{"turn": turn != nil, "failed": failure != nil}, func(s *wsState) {
			r.retireAgent(s, agent)
			if turn == nil {
				return
			}
			s.turn = nil
			s.tok.settled = true
			s.hook = nil
			s.interrupting = false
			// THE TURN'S END IS THE COMPACTION'S END: the flag and the line
			// go together (compaction.go, "the line's lifetime").
			s.compacting = false
			r.endCompactionAtTerminal(ws, s, *turn)
			if FailureBlocks(failure) {
				s.blocked = r.blockFor(failure)
				return
			}
			if interrupted, ok := success.GetOutcome().(*conversationv1.AgentSuccess_Interrupted); ok {
				s.interrupted = &interruptedState{
					kind: interruptedCause(interrupted.Interrupted),
					at:   r.opts.clock.Now(),
				}
				r.armMomentary(ws, s)
			}
		})
}

// retireAgent drops a subagent's chip row when its own stream ends.
func (r *resolver) retireAgent(s *wsState, agent *conversationv1.AgentId) {
	id := agent.GetValue()
	if id == "" {
		return
	}
	retired := false
	for unit, row := range s.agents {
		if row.createdAgent == id {
			delete(s.agents, unit)
			retired = true
		}
	}
	// A retired subagent's token units stop counting with it. Gated on a row
	// having actually matched, so the MAIN agent's own terminal -- which retires
	// no chip row -- never drops the settled turn's figure early: that figure
	// stands until the next turn resets it.
	if retired {
		s.tok.forgetAgent(id)
	}
}

// interruptedCause reads who stopped the turn. An unstated cause is the
// ordinary user stop, which is what the contract says a consumer reads it as.
func interruptedCause(interrupted *conversationv1.AgentInterrupted) interruptedKind {
	if _, host := interrupted.GetCause().(*conversationv1.AgentInterrupted_HostShutdown); host {
		return interruptedByHostShutdown
	}
	return interruptedByUser
}

// FailureBlocks reports whether a turn-ending agent failure leaves the session
// standing-blocked. It is THE ONE classifier the footer's `blocked` arm and the
// roster's `vendor_blocked` dot both consult — the footer here in
// OnAgentTerminal, the roster in resolve/sidebar's vendorBlocked — so the strip
// and the dot can never disagree about the same failure. That agreement is the
// owner's 2026-09-14 ruling: blocked is blue, and the roster agrees with the
// footer.
//
// EVERY classified failure blocks. A turn that ended in failure cannot proceed
// until the user acts — a re-prompt, a re-auth, a wait for a limit to reset —
// which is exactly what `blocked` says and what the roster paints blue. The
// specific block KIND (auth, a usage limit, billing, or an unclassified vendor
// error) is `blockFor`'s to name, because only the footer has a substatus to
// spend it on; the roster needs only this yes-or-no. A nil failure — a turn
// that SUCCEEDED — never blocks.
func FailureBlocks(failure *conversationv1.AgentFailure) bool {
	return failure != nil
}

// blockFor respells a turn's failure into the standing block it leaves behind.
func (r *resolver) blockFor(failure *conversationv1.AgentFailure) *blockedState {
	now := r.opts.clock.Now()
	switch item := failure.GetFailure().(type) {
	case *conversationv1.AgentFailure_ApiRequestFailed:
		return &blockedState{kind: apiBlockKind(item.ApiRequestFailed), at: now,
			line: item.ApiRequestFailed.GetMessage()}
	case *conversationv1.AgentFailure_BlockingLimit,
		*conversationv1.AgentFailure_RapidRefillBreaker,
		*conversationv1.AgentFailure_BudgetExhausted:
		return &blockedState{kind: blockedUsageLimit, at: now}
	default:
		return &blockedState{kind: blockedVendorError, at: now}
	}
}

// apiBlockKind maps the vendor's own error taxonomy onto the blocked step.
func apiBlockKind(failed *conversationv1.ApiRequestFailed) blockedKind {
	switch failed.GetKind().(type) {
	case *conversationv1.ApiRequestFailed_AuthenticationFailed,
		*conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		return blockedAuth
	case *conversationv1.ApiRequestFailed_RateLimited:
		return blockedUsageLimit
	case *conversationv1.ApiRequestFailed_BillingError:
		return blockedBilling
	default:
		return blockedVendorError
	}
}

// apiErrorKind names a mid-turn api failure's arm for the record.
func apiErrorKind(failed *conversationv1.ApiRequestFailed) string {
	switch failed.GetKind().(type) {
	case *conversationv1.ApiRequestFailed_RateLimited:
		return "rate_limited"
	case *conversationv1.ApiRequestFailed_Overloaded:
		return "overloaded"
	case *conversationv1.ApiRequestFailed_AuthenticationFailed:
		return "authentication_failed"
	case *conversationv1.ApiRequestFailed_PermissionDenied:
		return "permission_denied"
	case *conversationv1.ApiRequestFailed_InvalidRequest:
		return "invalid_request"
	case *conversationv1.ApiRequestFailed_RequestTooLarge:
		return "request_too_large"
	case *conversationv1.ApiRequestFailed_NotFound:
		return "not_found"
	case *conversationv1.ApiRequestFailed_Internal:
		return "internal"
	case *conversationv1.ApiRequestFailed_BillingError:
		return "billing_error"
	case *conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		return "oauth_org_not_allowed"
	case *conversationv1.ApiRequestFailed_MaxOutputTokens:
		return "max_output_tokens"
	case *conversationv1.ApiRequestFailed_Unmodeled:
		return "unmodeled"
	default:
		return "unset"
	}
}

// OnDetachedWork adds or updates a live-work chip. It is what makes a shell,
// a subagent or a monitor outlive the turn that started it.
func (r *resolver) OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) {
	if work == nil {
		return
	}
	id := work.GetWork().GetValue()
	r.mutate(ws, "daemon.footer.on_detached_work", "the footer took a detached-work announcement",
		dlog.Context{"work_id": id, "adopted": agent == nil}, func(s *wsState) {
			if agent == nil {
				markAdopted(s, id, work)
			}
			r.applyDetached(s, id, work)
		})
}

// applyDetached folds a detachment announcement into the chips.
func (r *resolver) applyDetached(s *wsState, id string, work *conversationv1.AgentDetachedWork) {
	// AN ANNOUNCEMENT AFTER THE TERMINAL IS A REPLAY TOO, for the reason a
	// start is: the announcement reaches the footer once per book, and the
	// second telling can arrive after the run has already settled.
	if _, done := s.retiredWork[id]; done {
		return
	}
	switch origin := work.GetOrigin().(type) {
	case *conversationv1.AgentDetachedWork_Detached:
		unit := origin.Detached.GetDetachedFromId().GetValue()
		if row, ok := s.bashUnits[unit]; ok {
			s.shells[id] = &shellRow{
				work: id, command: row.command, startedAt: row.startedAt, order: s.nextOrder()}
			return
		}
		if row, ok := s.agents[unit]; ok {
			// A detached subagent keeps the row it already has: one identity
			// spans the move, so the chip continues rather than duplicating.
			row.spawnUnit = unit
			row.work = id
			return
		}
	case *conversationv1.AgentDetachedWork_Created:
		r.applyCreatedWork(s, id, origin.Created.GetWorkCreated())
	}
}

// applyCreatedWork describes work that is detached from the moment the footer
// hears of it, so nothing has described it yet.
func (r *resolver) applyCreatedWork(s *wsState, id string, created *conversationv1.DetachableWork) {
	switch item := created.GetWork().(type) {
	case *conversationv1.DetachableWork_Bash:
		if start, ok := item.Bash.GetResult().(*conversationv1.AgentBash_Start); ok {
			s.shells[id] = &shellRow{
				work:      id,
				command:   truncate(start.Start.GetCommand().GetLine(), DefaultWarningRowWidth),
				startedAt: time.UnixMilli(start.Start.GetStartedAt().GetAtMs()),
				order:     s.nextOrder(),
			}
		}
	case *conversationv1.DetachableWork_Subagent:
		if start, ok := item.Subagent.GetResult().(*conversationv1.AgentSubagent_Start); ok {
			s.agents[id] = &agentRow{
				work:         id,
				spawnUnit:    id,
				createdAgent: start.Start.GetCreatedAgentId().GetValue(),
				label:        subagentLabel(start.Start.GetPrompt()),
				description:  start.Start.GetPrompt().GetDescription(),
				startedAt:    time.UnixMilli(start.Start.GetStartedAt().GetAtMs()),
				order:        s.nextOrder(),
			}
		}
	case *conversationv1.DetachableWork_Monitor:
		r.applyMonitor(s, id, item.Monitor)
	}
}

// OnSubagent advances a DETACHED subagent's chip row and retires it at that
// run's own terminal.
//
// THE COUNTERPART OF OnBash, and for the same reason. A shell chip retires at
// its command's terminal because the terminal is addressed to the WORK; a
// detached subagent's terminal is addressed to the work too, and reaches this
// daemon on whichever stream carries the run -- the spawning agent's book when
// the producer settles the unit there, the run's OWN book when the frames
// arrive on it. Reading only the spawning call's stream left a settled run
// counted as live for the rest of the session, which is what the G50 playbook
// read beside two settled placements.
//
// A start or an update leaves the row live; ANY terminal arm retires it,
// however it settled, exactly as a shell's does.
func (r *resolver) OnSubagent(ws ids.WorkspaceID, work *conversationv1.DetachedWorkId, sub *conversationv1.AgentSubagent) {
	if work == nil || sub == nil {
		return
	}
	id := work.GetValue()
	r.mutate(ws, "daemon.footer.on_subagent", "the footer took a detached subagent frame",
		dlog.Context{"work_id": id}, func(s *wsState) {
			switch item := sub.GetResult().(type) {
			case *conversationv1.AgentSubagent_Start:
				// A START AFTER THE TERMINAL IS A REPLAY, never a new run.
				// See wsState.retiredWork: the run's own book carries its
				// opening frames and the caller's book carries the settle,
				// and neither is ordered against the other.
				if _, done := s.retiredWork[id]; done {
					return
				}
				row, ok := s.agents[id]
				if !ok {
					row = &agentRow{spawnUnit: id, order: s.nextOrder()}
					s.agents[id] = row
				}
				row.work = id
				row.createdAgent = item.Start.GetCreatedAgentId().GetValue()
				row.label = subagentLabel(item.Start.GetPrompt())
				row.description = item.Start.GetPrompt().GetDescription()
				row.startedAt = time.UnixMilli(item.Start.GetStartedAt().GetAtMs())
			case *conversationv1.AgentSubagent_Update:
				if row, ok := s.agents[id]; ok {
					row.tokens = item.Update.GetProgress().GetTotalTokens()
					if desc := item.Update.GetPrompt().GetDescription(); desc != "" {
						row.description = desc
					}
				}
			default:
				retireWork(s, id)
			}
		})
}

// retireWork drops the row the detached handle addresses. The handle, the
// spawn unit and the created agent are ONE value by the contract's own ruling
// (`DetachedWorkId.value == AgentActivityId.value`, and for a subagent that is
// its `AgentId` too), so all three are matched rather than the map key alone:
// a row opened from a `created` announcement is keyed by the handle, one that
// detached mid-turn by the spawn unit, and neither reading may miss.
func retireWork(s *wsState, id string) {
	if id == "" {
		return
	}
	s.retiredWork[id] = struct{}{}
	for unit, row := range s.agents {
		if unit == id || row.work == id || row.spawnUnit == id || row.createdAgent == id {
			// A retired detached run's token units stop counting with it.
			s.tok.forgetAgent(row.createdAgent)
			delete(s.agents, unit)
		}
	}
}

// OnBash advances a shell chip and retires it at the command's terminal.
func (r *resolver) OnBash(ws ids.WorkspaceID, work *conversationv1.DetachedWorkId, bash *conversationv1.AgentBash) {
	if work == nil || bash == nil {
		return
	}
	id := work.GetValue()
	r.mutate(ws, "daemon.footer.on_bash", "the footer took a detached shell frame",
		dlog.Context{"work_id": id}, func(s *wsState) {
			switch item := bash.GetResult().(type) {
			case *conversationv1.AgentBash_Start:
				if _, done := s.retiredWork[id]; done {
					return
				}
				row, ok := s.shells[id]
				if !ok {
					row = &shellRow{work: id, order: s.nextOrder()}
					s.shells[id] = row
				}
				row.command = truncate(item.Start.GetCommand().GetLine(), DefaultWarningRowWidth)
				row.startedAt = time.UnixMilli(item.Start.GetStartedAt().GetAtMs())
			case *conversationv1.AgentBash_Update:
			default:
				s.retiredWork[id] = struct{}{}
				delete(s.shells, id)
			}
		})
}

// ---- chips and panels -----------------------------------------------------

// chips renders the right-aligned live-work chips. An UNSET chip is not drawn:
// a quiet workspace shows no chips at all, and a zero count is expressed by
// leaving the chip unset rather than by drawing a zero.
func (r *resolver) chips(s *wsState) *frontendv1.FooterLiveWorkChips {
	out := &frontendv1.FooterLiveWorkChips{}
	if n := len(s.agents); n > 0 {
		out.Agents = &frontendv1.FooterChipAgents{Count: uint32(n)}
	}
	if n := len(s.tasks); n > 0 {
		done := 0
		for _, row := range s.tasks {
			if row.status == taskCompleted {
				done++
			}
		}
		out.Tasks = &frontendv1.FooterChipTasks{Done: uint32(done), Total: uint32(n)}
	}
	if n := len(s.shells); n > 0 {
		out.Shells = &frontendv1.FooterChipShells{Count: uint32(n)}
	}
	if n := len(s.monitors); n > 0 {
		out.Monitors = &frontendv1.FooterChipMonitors{Count: uint32(n)}
	}
	// THE ⏱ CHIP COUNTS EVERY SCHEDULED JOB, not just the crons: footer.proto
	// words it as "live scheduled jobs (cron/wakeup schedules)", and a pending
	// self-scheduled wakeup is one of them. It retires only once both are gone.
	scheduled := len(s.crons)
	if s.wakeup != nil {
		scheduled++
	}
	if scheduled > 0 {
		out.Crons = &frontendv1.FooterChipCrons{Count: uint32(scheduled)}
	}
	return out
}

// expanded renders every panel. ALL of them arrive populated on every push,
// because the selection is webview-local and the daemon never learns it.
func (r *resolver) expanded(ws ids.WorkspaceID, s *wsState) *frontendv1.FooterExpanded {
	return &frontendv1.FooterExpanded{
		Tokens:   s.tok.panel(),
		Agents:   r.agentsPanel(ws, s),
		Tasks:    r.tasksPanel(s),
		Shells:   r.shellsPanel(ws, s),
		Monitors: r.monitorsPanel(s),
		Crons:    r.cronsPanel(s),
	}
}

// agentsPanel renders the ⚙ panel: one jump-target row per live subagent.
func (r *resolver) agentsPanel(ws ids.WorkspaceID, s *wsState) *frontendv1.FooterExpandedAgents {
	rows := make([]*agentRow, 0, len(s.agents))
	for _, row := range s.agents {
		rows = append(rows, row)
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].order < rows[j].order })
	out := &frontendv1.FooterExpandedAgents{}
	for _, row := range rows {
		drawn := &frontendv1.FooterAgentRow{
			Target: r.opts.encodeFeedID(feedid.Ref{
				WS:   ws,
				Feed: feedid.Feed{Root: true},
				Row: feedid.RowKey{
					Kind: feedid.KindActivity,
					ID:   row.spawnUnit,
					Sub:  row.createdAgent,
				},
			}),
			Label:   &frontendv1.FooterAgentRowLabel{Text: row.label},
			Tokens:  &frontendv1.FooterAgentRowTokens{Text: figures.Tokens(row.tokens) + " tok"},
			Runtime: &frontendv1.FooterAgentRowRuntime{StartedAtMs: epochMs(row.startedAt)},
		}
		if row.description != "" {
			drawn.Description = &frontendv1.FooterAgentRowDescription{Text: row.description}
		}
		out.Rows = append(out.Rows, drawn)
	}
	return out
}

// tasksPanel renders the ☑ panel: the tracker's current checklist.
func (r *resolver) tasksPanel(s *wsState) *frontendv1.FooterExpandedTasks {
	rows := make([]*taskRow, 0, len(s.tasks))
	for _, row := range s.tasks {
		rows = append(rows, row)
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].order < rows[j].order })
	out := &frontendv1.FooterExpandedTasks{}
	for _, row := range rows {
		status := &frontendv1.FooterTaskRowStatus{}
		switch row.status {
		case taskRunning:
			running := &frontendv1.FooterTaskRowRunning{}
			if row.activeForm != "" {
				running.ActiveForm = &frontendv1.FooterTaskRowActiveForm{Text: row.activeForm}
			}
			status.Status = &frontendv1.FooterTaskRowStatus_Running{Running: running}
		case taskCompleted:
			status.Status = &frontendv1.FooterTaskRowStatus_Completed{
				Completed: &frontendv1.FooterTaskRowCompleted{}}
		default:
			status.Status = &frontendv1.FooterTaskRowStatus_Pending{
				Pending: &frontendv1.FooterTaskRowPending{}}
		}
		out.Rows = append(out.Rows, &frontendv1.FooterTaskRow{
			Status:  status,
			Subject: &frontendv1.FooterTaskRowSubject{Text: row.subject},
		})
	}
	return out
}

// shellsPanel renders the $ panel: one jump-target row per live shell.
func (r *resolver) shellsPanel(ws ids.WorkspaceID, s *wsState) *frontendv1.FooterExpandedShells {
	rows := make([]*shellRow, 0, len(s.shells))
	for _, row := range s.shells {
		rows = append(rows, row)
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].order < rows[j].order })
	out := &frontendv1.FooterExpandedShells{}
	for _, row := range rows {
		out.Rows = append(out.Rows, &frontendv1.FooterShellRow{
			Target: r.opts.encodeFeedID(feedid.Ref{
				WS:   ws,
				Feed: feedid.Feed{Root: true},
				// The jump lands on the shell bubble's HEAD (KindShellHead), the
				// row a reader expands — not the spool BODY on the sub-feed.
				Row: feedid.RowKey{Kind: feedid.KindShellHead, ID: row.work},
			}),
			Command: &frontendv1.FooterShellRowCommand{Text: row.command},
			Runtime: &frontendv1.FooterShellRowRuntime{StartedAtMs: epochMs(row.startedAt)},
		})
	}
	return out
}

// monitorsPanel renders the 👁 panel. Monitors have no feed bubble, so no row
// is a jump target.
func (r *resolver) monitorsPanel(s *wsState) *frontendv1.FooterExpandedMonitors {
	rows := make([]*monitorRow, 0, len(s.monitors))
	for _, row := range s.monitors {
		rows = append(rows, row)
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].order < rows[j].order })
	out := &frontendv1.FooterExpandedMonitors{}
	for _, row := range rows {
		drawn := &frontendv1.FooterMonitorRow{
			Description: &frontendv1.FooterMonitorRowDescription{Text: row.description},
			Runtime:     &frontendv1.FooterMonitorRowRuntime{StartedAtMs: epochMs(row.startedAt)},
		}
		if row.persistent {
			drawn.Persistent = &frontendv1.FooterMonitorRowPersistent{}
		}
		out.Rows = append(out.Rows, drawn)
	}
	return out
}

// cronsPanel renders the ⏱ panel. The next fire is resolved DAEMON-side from
// the job's cron expression; a job whose expression this daemon cannot resolve
// draws its schedule alone rather than a guessed countdown.
func (r *resolver) cronsPanel(s *wsState) *frontendv1.FooterExpandedCrons {
	rows := make([]*cronRow, 0, len(s.crons))
	for _, row := range s.crons {
		rows = append(rows, row)
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].order < rows[j].order })
	now := r.opts.clock.Now()
	out := &frontendv1.FooterExpandedCrons{}
	for _, row := range rows {
		drawn := &frontendv1.FooterCronRow{
			Schedule: &frontendv1.FooterCronRowSchedule{Text: row.humanSchedule},
			Prompt: &frontendv1.FooterCronRowPrompt{
				Text: truncate(row.prompt, DefaultWarningRowWidth)},
		}
		if fire, ok := cronNextFire(row.cron, now); ok {
			drawn.NextFire = &frontendv1.FooterCronRowNextFire{FireAtMs: epochMs(fire)}
		}
		if row.recurring {
			drawn.Recurring = &frontendv1.FooterCronRowRecurring{}
		}
		if row.durable {
			drawn.Durable = &frontendv1.FooterCronRowDurable{}
		}
		out.Rows = append(out.Rows, drawn)
	}
	return out
}
