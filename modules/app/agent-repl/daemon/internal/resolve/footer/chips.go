package footer

import (
	"google.golang.org/protobuf/proto"

	"sort"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/figures"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/ladder"
)

// OnActivity advances the status tree, the token accounting, the live-work
// chips and the activity cell's transient line. Every arm the footer draws
// anything from has a branch; the rest are recorded as observed and change
// nothing. Work a SUBAGENT does raises its transients under that subagent's
// label, so its lines are never mistaken for the main agent's.
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
			label := s.agentLabel(agent.GetValue())
			r.clearRetry(ws, s, agent.GetValue(), act)
			r.recordSpawnOwner(s, agent, act)
			r.applyActivity(ws, s, agent, label, unit, act)
			r.trackFeed(ws, s, agent, unit, act)
			if call, started := toolCallStart(act); started {
				r.raiseTransient(ws, s, label, &frontendv1.FooterActivityTransient{
					Kind: &frontendv1.FooterActivityTransient_ToolCall{ToolCall: call}})
			}
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

// applyActivity folds one activity frame into the accumulation. `label` is the
// subagent label the frame's transients carry, empty for the main agent.
func (r *resolver) applyActivity(ws ids.WorkspaceID, s *wsState, agent *conversationv1.AgentId, label, unit string, act *conversationv1.AgentActivity) {
	switch item := act.GetItem().(type) {
	case *conversationv1.AgentActivity_Response:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Response"})
		r.applyResponse(s, unit, item.Response)
	case *conversationv1.AgentActivity_Hook:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Hook"})
		if start := item.Hook.GetStart(); start != nil {
			r.raiseHook(ws, s, label, start.GetHookName())
		}
	case *conversationv1.AgentActivity_Subagent:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Subagent"})
		r.applySubagent(s, agent, unit, item.Subagent)
	case *conversationv1.AgentActivity_Bash:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_Bash"})
		r.applyBash(s, unit, item.Bash)
	case *conversationv1.AgentActivity_TaskAct:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_TaskAct"})
		if subject, moved := r.applyTaskAct(ws, s, item.TaskAct); moved {
			r.raiseTask(ws, s, label, subject)
		}
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
		r.applyInjection(ws, s, label, item.ContextInjected)
	case *conversationv1.AgentActivity_PushNotification:
		r.logOf(ws, s).Debug("daemon.footer.transition_decision", "selected a footer state branch", dlog.Context{"function": "chips", "branch": "case *conversationv1.AgentActivity_PushNotification"})
		if start := item.PushNotification.GetStart(); start != nil {
			r.standNotification(ws, s, start.GetMessage())
		}
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

// applySubagent maintains the ⚙ chip's rows from the SPAWNING CALL's own
// activity stream. A row leaves the list when the spawn reaches a terminal,
// which is what makes the chip mean "live".
//
// A DETACHED RUN IS NOT RETIRED HERE. Once the work has left the turn it is
// addressed by its handle and its terminal arrives on whichever stream carries
// the work -- the caller's book or the agent's own -- so the retirement is the
// handle's (OnSubagent), never an inference from the call that spawned it. The
// spawning call returning is a LAUNCH RECEIPT and says nothing about the run.
func (r *resolver) applySubagent(s *wsState, agent *conversationv1.AgentId, unit string, sub *conversationv1.AgentSubagent) {
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
			row = &agentRow{
				spawnUnit: unit, order: s.nextOrder(),
				provenance: provenanceSpawnFrame, spawnedOn: agent.GetValue(),
			}
			s.agents[unit] = row
		}
		row.takeStart(item.Start)
	case *conversationv1.AgentSubagent_Update:
		// BY ANY IDENTITY THE ROW IS ADDRESSED BY: a resumed run's beats ride
		// the send's unit, which is the row's handle and not its key.
		if row := s.subagentRow(unit); row != nil {
			row.takeUpdate(item.Update)
		}
	default:
		if row, ok := s.agents[unit]; ok && row.work != "" {
			return
		}
		delete(s.agents, unit)
	}
}

// takeStart describes the row from a spawn's start frame: the agent it
// created, its label and description, and the run's ORIGINAL start instant.
// The ONE reading of a start, for the spawning call's stream and the run's own
// alike, so the two cannot describe one run differently.
func (row *agentRow) takeStart(start *conversationv1.AgentSubagentStart) {
	row.createdAgent = start.GetCreatedAgentId().GetValue()
	row.label = subagentLabel(start.GetPrompt())
	row.description = start.GetPrompt().GetDescription()
	row.startedAt = time.UnixMilli(start.GetStartedAt().GetAtMs())
}

// takeUpdate folds a running beat into the row: the running token sum, and
// the label and description when the beat restates a commission that names
// them. The ONE reading of an update, for the spawning call's stream and the
// run's own alike.
//
// THE BEAT RESTATES THE COMMISSION so it stands alone, which is what describes
// a row this daemon never saw launch (bindDetachedAgent): a row still drawn
// under the generic label takes the label the commission names. A row already
// labelled keeps its label, which its start established from the same
// commission.
func (row *agentRow) takeUpdate(update *conversationv1.AgentSubagentUpdate) {
	row.tokens = update.GetProgress().GetTotalTokens()
	if row.label == subagentLabel(nil) {
		row.label = subagentLabel(update.GetPrompt())
	}
	if desc := update.GetPrompt().GetDescription(); desc != "" {
		row.description = desc
	}
}

// minimalAgentRow is a row for a subagent the footer has not described: the
// generic label, no description, no tokens, addressed by the run's handle and
// by its agent (which, by the minting rule, is also its spawn unit). The ONE
// shape every row opened ahead of its description takes -- the live set's, a
// network-resume wait's, and a resume's -- so the three cannot drift.
func (s *wsState) minimalAgentRow(work, agent string, startedAt time.Time, provenance rowProvenance) *agentRow {
	return &agentRow{
		work:         work,
		spawnUnit:    agent,
		createdAgent: agent,
		label:        subagentLabel(nil),
		startedAt:    startedAt,
		order:        s.nextOrder(),
		provenance:   provenance,
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
//
// IT ANSWERS THE MOVED TASK'S SUBJECT, and whether the tracker MOVED at all: a
// refused act changed nothing, and an act the checklist could not hold moved
// nothing the reader can see, so neither raises the `task` transient.
func (r *resolver) applyTaskAct(ws ids.WorkspaceID, s *wsState, act *conversationv1.AgentTaskAct) (string, bool) {
	id := act.GetTask().GetValue()
	state := act.GetState()
	_, rejected := act.GetAct().(*conversationv1.AgentTaskAct_Rejected)
	if _, deleted := state.GetStatus().(*conversationv1.AgentTaskState_Deleted); deleted {
		subject := state.GetSubject()
		if row, ok := s.tasks[id]; ok {
			subject = row.subject
			delete(s.tasks, id)
			return subject, !rejected
		}
		return subject, false
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
			return "", false
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
	return row.subject, !rejected
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

// applyInjection raises the MOMENTARY loading status and announces the item
// taken on as the transient `context_injected` line. The kind is derived from
// what the injection carries: a memory file is `memory`; ONE skill with
// content is an `invoked` skill; several with content are `discovered`; and
// content-free entries are a `listing`.
func (r *resolver) applyInjection(ws ids.WorkspaceID, s *wsState, label string, injected *conversationv1.AgentContextInjected) {
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
	r.raiseContextInjected(ws, s, label, s.loading.line)
	r.armMomentary(ws, s)
}

// OnAgentTerminal retires an agent from the status tree. A terminal carrying a
// TURN is the main thread's: it settles the token accounting, ends the clock,
// and may raise the momentary interrupted status or a standing block.
func (r *resolver) OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	r.mutate(ws, "daemon.footer.on_agent_terminal", "the footer took an agent terminal",
		dlog.Context{"turn": turn != nil, "failed": failure != nil}, func(s *wsState) {
			r.retireAgent(s, agent)
			r.endSubagentBudget(ws, s, agent.GetValue())
			if turn == nil {
				return
			}
			s.turn = nil
			r.endTurnMotion(s)
			s.tok.settled = true
			s.interrupting = false
			// THE TURN'S END IS THE COMPACTION'S END: the flag and the line
			// go together (compaction.go, "the line's lifetime").
			s.compacting = false
			r.endCompactionAtTerminal(ws, s, *turn)
			// A QUERY-DIED TERMINAL IS THE DEATH ITSELF, so it stands the
			// dead-query line as the session's push does: the two arrive by
			// independent channels in no fixed order, and the strip must not
			// depend on which came first. A line the push already stood keeps
			// its own instant.
			if _, died := failure.GetFailure().(*conversationv1.AgentFailure_QueryDied); died && s.queryDied == nil {
				s.queryDied = &standing{text: deadQueryLine, at: r.opts.clock.Now()}
			}
			switch ladder.ClassifyFailure(failure) {
			case ladder.VendorBlocked:
				s.blocked = r.blockFor(failure)
				s.turnFailed = true
				return
			case ladder.TurnFailed:
				s.turnFailed = true
				return
			case ladder.ExpectedStop, ladder.NoFailure:
				// An expected stop reads exactly as a completion: idle·done.
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
			s.rememberRetired(row)
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

// blockFor respells a VENDOR OR ACCOUNT failure (ladder.ClassifyFailure) into
// the standing block it leaves behind. Which block is this surface's detail;
// that it blocks at all is the classifier's, shared with the roster.
func (r *resolver) blockFor(failure *conversationv1.AgentFailure) *blockedState {
	now := r.opts.clock.Now()
	switch item := failure.GetFailure().(type) {
	case *conversationv1.AgentFailure_ApiRequestFailed:
		return &blockedState{kind: apiBlockKind(item.ApiRequestFailed), at: now,
			line: item.ApiRequestFailed.GetMessage()}
	case *conversationv1.AgentFailure_BlockingLimit,
		*conversationv1.AgentFailure_RapidRefillBreaker:
		return &blockedState{kind: blockedUsageLimit, at: now}
	default:
		// A model error, the one vendor failure with no finer step of its own.
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
			r.recordAnnouncedOwner(s, work)
			r.applyDetached(ws, s, id, work)
		})
}

// applyDetached folds a detachment announcement into the chips.
func (r *resolver) applyDetached(ws ids.WorkspaceID, s *wsState, id string, work *conversationv1.AgentDetachedWork) {
	// AN ANNOUNCEMENT AFTER THE TERMINAL IS A REPLAY TOO, for the reason a
	// start is: the announcement reaches the footer once per book, and the
	// second telling can arrive after the run has already settled.
	if _, done := s.retiredWork[id]; done {
		return
	}
	switch origin := work.GetOrigin().(type) {
	case *conversationv1.AgentDetachedWork_Detached:
		unit := origin.Detached.GetDetachedFromId().GetValue()
		r.leaveTurn(ws, s, unit)
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
		// THE UNIT IS NOT ALWAYS THE AGENT'S SPAWN. A subagent resumed by a
		// send is detached from the SEND, which drew no row here; the
		// announcement's kind names the agent that is running, and the run
		// is that agent's row. See bindDetachedAgent.
		if agent := work.GetKind().GetSubagent().GetAgentId().GetValue(); agent != "" {
			r.bindDetachedAgent(ws, s, id, unit, agent)
			return
		}
	case *conversationv1.AgentDetachedWork_Created:
		// The work's handle IS the unit that created it
		// (`DetachedWorkId.value == AgentActivityId.value`).
		r.leaveTurn(ws, s, id)
		r.applyCreatedWork(s, id, origin.Created.GetWorkCreated())
	}
}

// bindDetachedAgent makes a detached run that no row stood for under its unit
// the row of the AGENT it runs, addressed by the run's own handle.
//
// A RESUMED SUBAGENT IS THE SAME AGENT AS ITS LAUNCH. The vendor runs a
// subagent resumed by a send under a NEW handle (the send's id) but the SAME
// agent, so its row carries the launch's identity -- label, description,
// tokens -- rather than a minimal "subagent" row with no tokens that nothing
// will describe again (owner's report, workspace footer-activity-updates,
// 2026-09-30: daemon.footer.live_work_taken readded_retired). The identity is
// taken from, in order of what the footer holds:
//
//   - the agent's LIVE row, which is re-addressed by the new handle;
//   - the row retired at the agent's previous terminal (retiredRows), copied
//     into a live row -- a copy, because the retired row stays the record of
//     that earlier run for every identity it was kept under;
//   - neither, when this daemon never saw the agent launch: a minimal row the
//     run's own frames describe, since every one of them restates the
//     commission (AgentSubagentUpdate.prompt, "repeated so this frame stands
//     alone").
//
// The handle joins the row's addresses (row.work), so the run's beats and its
// terminal -- addressed to the handle's unit -- reach this row, and its jump
// names the entry the feed draws for the run under that unit.
func (r *resolver) bindDetachedAgent(ws ids.WorkspaceID, s *wsState, work, unit, agent string) {
	ctx := dlog.Context{"work_id": work, "unit": unit, "agent_id": agent}
	if row := s.subagentRow(agent); row != nil {
		row.work = work
		ctx["identity"] = "live_row"
		r.logOf(ws, s).Info("daemon.footer.detached_agent_bound",
			"a detached run was bound to its agent's row by the run's own handle", ctx)
		return
	}
	row := s.minimalAgentRow(work, agent, r.opts.clock.Now(), provenanceDetachedAnnouncement)
	ctx["identity"] = "undescribed"
	if retired, ok := s.retiredRows[agent]; ok {
		row.spawnUnit = retired.spawnUnit
		row.label = retired.label
		row.description = retired.description
		row.tokens = retired.tokens
		row.provenance = retired.provenance
		row.spawnedOn = retired.spawnedOn
		ctx["identity"] = "retired_row"
	}
	s.agents[agent] = row
	r.logOf(ws, s).Info("daemon.footer.detached_agent_bound",
		"a detached run was bound to its agent's row by the run's own handle", ctx)
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
			row := &agentRow{work: id, spawnUnit: id, order: s.nextOrder(), provenance: provenanceAnnouncement}
			row.takeStart(start.Start)
			s.agents[id] = row
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
				// BY ANY IDENTITY THE ROW IS ADDRESSED BY, exactly as the
				// retirement below: a resumed run's handle is not its row's key.
				row := s.subagentRow(id)
				if row == nil {
					row = &agentRow{spawnUnit: id, order: s.nextOrder(), provenance: provenanceRunFrame}
					s.agents[id] = row
				}
				row.work = id
				row.takeStart(item.Start)
			case *conversationv1.AgentSubagent_Update:
				if row := s.subagentRow(id); row != nil {
					row.takeUpdate(item.Update)
				}
			default:
				if _, done := s.retiredWork[id]; !done {
					r.landBackground(s, "Subagent", detachedPhase(sub.GetFailure() != nil))
				}
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
		if unit == id || row.addressedBy(id) {
			// A retired detached run's token units stop counting with it.
			s.tok.forgetAgent(row.createdAgent)
			s.rememberRetired(row)
			delete(s.agents, unit)
		}
	}
}

// addressedBy reports whether the row is addressed by this id: its handle, its
// spawn unit or its created agent. The three are ONE value by the contract's
// own ruling (`DetachedWorkId.value == AgentActivityId.value`, and for a
// subagent that is its `AgentId` too), so every lookup matches all three, and
// an empty id addresses nothing.
func (row *agentRow) addressedBy(id string) bool {
	return id != "" && (row.work == id || row.spawnUnit == id || row.createdAgent == id)
}

// subagentRow answers the live subagent row addressed by this id — under the
// key it is held by or any identity it is addressed by — or nil. It is the ONE
// lookup of a live row by id: the usage attribution, a transient's agent
// label, a wait's row and the live-work reconciliation all ask it.
func (s *wsState) subagentRow(id string) *agentRow {
	if id == "" {
		return nil
	}
	for unit, row := range s.agents {
		if unit == id || row.addressedBy(id) {
			return row
		}
	}
	return nil
}

// rememberRetired keeps a DETACHED row's description after its run retired,
// under every identity it is addressed by, so a network-resume wait that opens
// after the failure terminal can still draw it (netresume.go). An in-turn row
// is never waited on and is not kept.
func (s *wsState) rememberRetired(row *agentRow) {
	if row.work == "" {
		return
	}
	for _, id := range []string{row.work, row.spawnUnit, row.createdAgent} {
		if id != "" {
			s.retiredRows[id] = row
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
			case *conversationv1.AgentBash_Tail:
			default:
				if _, done := s.retiredWork[id]; !done {
					r.landBackground(s, "Bash", detachedPhase(bash.GetFailure() != nil))
				}
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
	// THE ⚙ CHIP COUNTS THE PANEL'S ROWS, waiting rows included, and carries
	// the waiting-for-the-API glyph while any row waits (netresume.go).
	if rows := r.agentRowsDrawn(s); len(rows) > 0 {
		out.Agents = &frontendv1.FooterChipAgents{Count: uint32(len(rows))}
		waiting := 0
		for _, d := range rows {
			if d.wait != nil {
				waiting++
			}
		}
		if waiting > 0 {
			out.Agents.WaitingForApi = &frontendv1.FooterChipAgentsWaitingForApi{Count: uint32(waiting)}
		}
	}
	if done, total := s.taskCounts(); total > 0 {
		out.Tasks = &frontendv1.FooterChipTasks{Done: done, Total: total}
	}
	if n := len(r.shellRowsDrawn(s)); n > 0 {
		out.Shells = &frontendv1.FooterChipShells{Count: uint32(n)}
	}
	if n := len(r.monitorRowsDrawn(s)); n > 0 {
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
	// THE 🧪 CHIP IS SET IFF THE MERGE TESTS PANEL HOLDS A SUITE: the panel
	// holds the merge's current test round while it tests and nothing
	// otherwise.
	if total := len(s.merge.Tests); total > 0 {
		out.MergeTests = &frontendv1.FooterChipMergeTests{Finished: uint32(finishedSuites(s.merge.Tests)), Total: uint32(total)}
	}
	return out
}

// finishedSuites counts the merge tests panel's rows that passed or failed.
func finishedSuites(rows []*frontendv1.FooterMergeTestRow) int {
	finished := 0
	for _, row := range rows {
		switch row.GetState().GetState().(type) {
		case *frontendv1.FooterMergeTestRowState_Passed, *frontendv1.FooterMergeTestRowState_Failed:
			finished++
		}
	}
	return finished
}

// mergeTestsPanel renders the 🧪 panel: the merge's current test round, one
// row per suite in the gate's order, and no rows when the merge is not
// testing. The rows are the orchestrator's, cloned so a published view never
// shares a message with the facts it was drawn from.
func mergeTestsPanel(s *wsState) *frontendv1.FooterExpandedMergeTests {
	out := &frontendv1.FooterExpandedMergeTests{}
	for _, row := range s.merge.Tests {
		out.Rows = append(out.Rows, proto.Clone(row).(*frontendv1.FooterMergeTestRow))
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
		// The 🧪 panel: empty unless the merge is testing.
		MergeTests: mergeTestsPanel(s),
	}
}

// agentsPanel renders the ⚙ panel: one jump-target row per live subagent, and
// one per subagent waiting for the API to resume it.
func (r *resolver) agentsPanel(ws ids.WorkspaceID, s *wsState) *frontendv1.FooterExpandedAgents {
	out := &frontendv1.FooterExpandedAgents{}
	for _, d := range r.agentRowsDrawn(s) {
		row := d.row
		workID := row.work
		if workID == "" {
			workID = row.spawnUnit
		}
		// THE RUN'S OWN ENTRY FIRST: a resumed run is drawn under its
		// handle's unit (the send), which is on screen whether or not the
		// launch's bubble still is; an ordinary run's handle IS its spawn unit.
		entry := s.entryFor(row.work, row.spawnUnit)
		jump := jumpTo(entry)
		s.noteJump(&row.jump, "agent", workID, jump, dlog.Context{
			"provenance":      string(row.provenance),
			"spawned_on":      row.spawnedOn,
			"detached":        row.work != "",
			"label":           row.label,
			"has_description": row.description != "",
			"tokens":          row.tokens,
			"retired_before":  retiredAny(s, row.spawnUnit, row.work),
			"waiting_for_api": d.wait != nil,
		})
		drawn := &frontendv1.FooterAgentRow{
			Work:    &frontendv1.FooterWorkId{Value: workID},
			Jump:    jump,
			Label:   &frontendv1.FooterAgentRowLabel{Text: row.label},
			Tokens:  &frontendv1.FooterAgentRowTokens{Text: figures.Tokens(row.tokens) + " tok"},
			Runtime: &frontendv1.FooterAgentRowRuntime{StartedAtMs: epochMs(row.startedAt)},
		}
		if row.description != "" {
			drawn.Description = &frontendv1.FooterAgentRowDescription{Text: row.description}
		}
		agentRowState(d, drawn)
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
	out := &frontendv1.FooterExpandedShells{}
	for _, row := range r.shellRowsDrawn(s) {
		// The jump lands on the shell bubble's HEAD, the row a reader expands —
		// not the spool BODY on the sub-feed. The feed announces the head.
		jump := jumpTo(s.entryFor(row.work))
		s.noteJump(&row.jump, "shell", row.work, jump, dlog.Context{
			"retired_before": retiredAny(s, row.work),
		})
		out.Rows = append(out.Rows, &frontendv1.FooterShellRow{
			Work:    &frontendv1.FooterWorkId{Value: row.work},
			Jump:    jump,
			Command: &frontendv1.FooterShellRowCommand{Text: row.command},
			Runtime: &frontendv1.FooterShellRowRuntime{StartedAtMs: epochMs(row.startedAt)},
		})
	}
	return out
}

// monitorsPanel renders the 👁 panel: one jump-target row per live monitor.
func (r *resolver) monitorsPanel(s *wsState) *frontendv1.FooterExpandedMonitors {
	out := &frontendv1.FooterExpandedMonitors{}
	for _, row := range r.monitorRowsDrawn(s) {
		// The jump lands on the Monitor call's tool-call card, which the feed
		// announces by the monitor's id — the same bytes the row is keyed by.
		jump := jumpTo(s.entryFor(row.unit))
		s.noteJump(&row.jump, "monitor", row.unit, jump, dlog.Context{
			"retired_before": retiredAny(s, row.unit),
		})
		drawn := &frontendv1.FooterMonitorRow{
			Work:        &frontendv1.FooterWorkId{Value: row.unit},
			Jump:        jump,
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

// ---- where a detached-work row's click lands --------------------------------

// OnEntryPlaced records where the feed drew one detached-work-capable entry.
// The feed resolver calls it (Deps.EntryPlaced) the moment it first draws the
// entry and whenever the entry's FeedId changes, which is BEFORE the footer
// takes the same frame (the watcher routes every frame to the feed first), so
// a row the frame opens already names its entry.
//
// CALLED UNDER THE FEED RESOLVER'S LOCK. It takes this resolver's lock and
// nothing else, and nothing here calls back into the feed.
func (r *resolver) OnEntryPlaced(ws ids.WorkspaceID, unit string, row *frontendv1.FeedId) {
	if unit == "" || row.GetValue() == "" {
		r.workspaceLog(ws).Error("daemon.footer.entry_unaddressed",
			"the feed announced an entry with no unit or no FeedId; no jump row can name it",
			dlog.Context{"unit": unit, "row": row.GetValue()})
		return
	}
	r.mutate(ws, "daemon.footer.on_entry_placed", "the footer took the feed's placement of a detached-work entry",
		dlog.Context{"unit": unit, "row": row.GetValue()}, func(s *wsState) {
			s.entries[unit] = row
		})
}

// entryFor answers the entry the feed drew under the first of KEYS it has
// announced, nil when it announced none of them. A row is addressed by more
// than one identity (a subagent's spawn unit and its handle are one value by
// contract, but a row may know only one of them), so each is tried.
func (s *wsState) entryFor(keys ...string) *frontendv1.FeedId {
	for _, key := range keys {
		if key == "" {
			continue
		}
		if entry, ok := s.entries[key]; ok {
			return entry
		}
	}
	return nil
}

// jumpTo states a row's jump: the entry when the feed has announced it,
// otherwise that it is not drawn (yet). Every detached-work kind draws an
// entry, so there is no other reason.
func jumpTo(entry *frontendv1.FeedId) *frontendv1.FooterJump {
	if entry != nil {
		return &frontendv1.FooterJump{Target: &frontendv1.FooterJump_Entry{Entry: entry}}
	}
	return &frontendv1.FooterJump{Target: &frontendv1.FooterJump_Unresolved{Unresolved: &frontendv1.FooterJumpUnresolved{
		Reason: &frontendv1.FooterJumpUnresolved_NotDrawn{NotDrawn: &frontendv1.FooterJumpNotDrawn{}},
	}}}
}

// jumpResolution names a jump for the record: the entry's FeedId, or the
// unresolved reason.
func jumpResolution(jump *frontendv1.FooterJump) (resolution, entry string) {
	switch target := jump.GetTarget().(type) {
	case *frontendv1.FooterJump_Entry:
		return "entry", target.Entry.GetValue()
	case *frontendv1.FooterJump_Unresolved:
		switch target.Unresolved.GetReason().(type) {
		case *frontendv1.FooterJumpUnresolved_NotDrawn:
			return "not_drawn", ""
		}
	}
	return "unset", ""
}

// noteJump queues the record of a row's jump resolution when it CHANGED since
// the last one recorded for that row, so a row's resolution is diagnosable
// from the log alone — which kind, which work, what the row is and where it
// came from, and why a click cannot reach it — without a record on every push.
// Written after the lock is released (see mutate).
func (s *wsState) noteJump(memo *jumpMemo, kind, workID string, jump *frontendv1.FooterJump, facts dlog.Context) {
	resolution, entry := jumpResolution(jump)
	value := resolution + "|" + entry
	if memo.recorded && memo.value == value {
		return
	}
	memo.recorded = true
	memo.value = value
	note := dlog.Context{"kind": kind, "work_id": workID, "resolution": resolution, "entry": entry}
	for k, v := range facts {
		note[k] = v
	}
	s.jumpNotes = append(s.jumpNotes, note)
}

// retiredAny reports whether the footer has already seen a terminal for any
// of the identities: a row standing for a retired run is one the live-work set
// re-listed after the run's own terminal said it was over.
func retiredAny(s *wsState, ids ...string) bool {
	for _, id := range ids {
		if id == "" {
			continue
		}
		if _, done := s.retiredWork[id]; done {
			return true
		}
	}
	return false
}

// drainJumpNotes hands over the queued jump records and empties the queue.
func (s *wsState) drainJumpNotes() []dlog.Context {
	notes := s.jumpNotes
	s.jumpNotes = nil
	return notes
}

// logJumpNotes writes the queued jump-resolution records.
func logJumpNotes(log dlog.Logger, notes []dlog.Context) {
	for _, note := range notes {
		log.Info("daemon.footer.jump_resolution",
			"a footer detached-work row's click resolution changed", note)
	}
}
