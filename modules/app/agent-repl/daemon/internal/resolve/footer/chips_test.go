package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// subagentStart is a spawn announcing the agent it created.
func subagentStart(unit, created, subagentType, description string) *conversationv1.AgentActivity {
	prompt := &conversationv1.AgentSubagentPrompt{Text: "go"}
	if subagentType != "" {
		prompt.SubagentType = &subagentType
	}
	if description != "" {
		prompt.Description = &description
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Subagent{
			Subagent: &conversationv1.AgentSubagent{
				Result: &conversationv1.AgentSubagent_Start{
					Start: &conversationv1.AgentSubagentStart{
						CreatedAgentId: &conversationv1.AgentId{Value: created},
						Prompt:         prompt,
						StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: instant.UnixMilli()},
					},
				},
			},
		},
	}
}

// subagentProgress is a mid-run update carrying the running token sum.
func subagentProgress(unit string, tokens uint64) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Subagent{
			Subagent: &conversationv1.AgentSubagent{
				Result: &conversationv1.AgentSubagent_Update{
					Update: &conversationv1.AgentSubagentUpdate{
						Prompt:   &conversationv1.AgentSubagentPrompt{Text: "go"},
						Progress: &conversationv1.AgentSubagentProgress{TotalTokens: tokens},
					},
				},
			},
		},
	}
}

// taskAct is one tracker act leaving the task in the given state. The oneof's
// arm interface is unexported by the generated code, so the whole state rides
// in rather than the arm alone.
func taskAct(id string, state *conversationv1.AgentTaskState) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "task-act-" + id},
		Item: &conversationv1.AgentActivity_TaskAct{
			TaskAct: &conversationv1.AgentTaskAct{
				Task:  &conversationv1.AgentTaskId{Value: id},
				Act:   &conversationv1.AgentTaskAct_Created{Created: &conversationv1.AgentTaskCreated{}},
				State: state,
			},
		},
	}
}

// pendingTask is a recorded, unstarted task.
func pendingTask(subject string) *conversationv1.AgentTaskState {
	return &conversationv1.AgentTaskState{
		Subject: subject,
		Status:  &conversationv1.AgentTaskState_Pending{Pending: &conversationv1.AgentTaskPending{}},
	}
}

// completedTask is a task that achieved what it described.
func completedTask(subject string) *conversationv1.AgentTaskState {
	return &conversationv1.AgentTaskState{
		Subject: subject,
		Status:  &conversationv1.AgentTaskState_Completed{Completed: &conversationv1.AgentTaskCompleted{}},
	}
}

// runningTask is a task being worked on, with the agent's phrasing when it gave
// one.
func runningTask(subject string, activeForm *string) *conversationv1.AgentTaskState {
	return &conversationv1.AgentTaskState{
		Subject: subject,
		Status: &conversationv1.AgentTaskState_Running{
			Running: &conversationv1.AgentTaskRunning{ActiveForm: activeForm},
		},
	}
}

// deletedTask is a task removed from the plan.
func deletedTask(subject string) *conversationv1.AgentTaskState {
	return &conversationv1.AgentTaskState{
		Subject: subject,
		Status:  &conversationv1.AgentTaskState_Deleted{Deleted: &conversationv1.AgentTaskDeleted{}},
	}
}

// monitorStart is an armed background watcher.
func monitorStart(unit, description string, persistent bool) *conversationv1.AgentActivity {
	start := &conversationv1.AgentMonitorStart{
		Description: description,
		StartedAtMs: instant.UnixMilli(),
		Source: &conversationv1.AgentMonitorStart_Command{
			Command: &conversationv1.AgentMonitorCommand{Command: "tail -f log"},
		},
	}
	if persistent {
		start.Lifetime = &conversationv1.AgentMonitorStart_Persistent{
			Persistent: &conversationv1.AgentMonitorPersistent{},
		}
	} else {
		start.Lifetime = &conversationv1.AgentMonitorStart_Deadline{
			Deadline: &conversationv1.AgentMonitorDeadline{TimeoutMs: 60_000},
		}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Monitor{
			Monitor: &conversationv1.AgentMonitor{
				Result: &conversationv1.AgentMonitor_Start{Start: start},
			},
		},
	}
}

// cronListed is a whole-set listing of scheduled jobs.
func cronListed(jobs ...*conversationv1.AgentCronJob) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "cron-1"},
		Item: &conversationv1.AgentActivity_Cron{
			Cron: &conversationv1.AgentCron{
				State: &conversationv1.AgentCron_Success{
					Success: &conversationv1.AgentCronSuccess{
						Act: &conversationv1.AgentCronSuccess_Listed{
							Listed: &conversationv1.AgentCronListed{Jobs: jobs},
						},
					},
				},
			},
		},
	}
}

func TestAQuietWorkspaceDrawsNoChips(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	connected(h)

	// Assert
	chips := h.view(t).GetStrip().GetLiveWork()
	if chips.GetAgents() != nil || chips.GetTasks() != nil || chips.GetShells() != nil ||
		chips.GetMonitors() != nil || chips.GetCrons() != nil {
		t.Fatalf("chips = %+v, want every chip unset on a quiet workspace", chips)
	}
}

func TestALiveSubagentRaisesTheAgentsChip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "map the resolvers"))

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetAgents().GetCount(); got != 1 {
		t.Fatalf("agents chip = %d, want 1", got)
	}
}

func TestTheAgentRowJumpsToTheSubagentBubble(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "map the resolvers"))

	// Assert
	rows := h.view(t).GetExpanded().GetAgents().GetRows()
	if len(rows) != 1 {
		t.Fatalf("rows = %d, want 1", len(rows))
	}
	if got := rows[0].GetTarget().GetValue(); got != "activity|spawn-1|agent-2" {
		t.Fatalf("target = %q, want the bubble keyed by the spawn unit and the created agent", got)
	}
}

func TestTheAgentRowLabelFallsBackToTheDescription(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "", "map the resolvers"))

	// Assert
	rows := h.view(t).GetExpanded().GetAgents().GetRows()
	if rows[0].GetLabel().GetText() != "map the resolvers" {
		t.Fatalf("label = %q, want the description when no type was named", rows[0].GetLabel().GetText())
	}
}

func TestAnUnnamedUndescribedSpawnStillDrawsALabel(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "", ""))

	// Assert
	rows := h.view(t).GetExpanded().GetAgents().GetRows()
	if rows[0].GetLabel().GetText() == "" {
		t.Fatalf("the row drew an empty label")
	}
	if rows[0].Description != nil {
		t.Fatalf("description = %+v, want UNSET rather than a synthesized one", rows[0].Description)
	}
}

func TestTheAgentRowsTokenSumGrows(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", ""))

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentProgress("spawn-1", 12_400))

	// Assert
	rows := h.view(t).GetExpanded().GetAgents().GetRows()
	if got := rows[0].GetTokens().GetText(); got != "12.4k tok" {
		t.Fatalf("tokens = %q, want the running sum", got)
	}
}

func TestASubagentsTerminalRetiresItsRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", ""))

	// Act
	h.r.OnAgentTerminal(testWS, &conversationv1.AgentId{Value: "agent-2"}, nil, completed(), nil)

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetAgents() != nil {
		t.Fatalf("the agents chip survived the subagent's terminal")
	}
}

func TestTheTaskChipCountsDoneOverTotal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", completedTask("write it")))
	h.r.OnActivity(testWS, mainAgent, taskAct("t2", pendingTask("test it")))

	// Assert
	chip := h.view(t).GetStrip().GetLiveWork().GetTasks()
	if chip.GetDone() != 1 || chip.GetTotal() != 2 {
		t.Fatalf("tasks chip = %+v, want 1 of 2", chip)
	}
}

func TestARunningTaskCarriesItsActiveForm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	form := "running the migration"

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", runningTask("migrate", &form)))

	// Assert
	rows := h.view(t).GetExpanded().GetTasks().GetRows()
	if rows[0].GetStatus().GetRunning().GetActiveForm().GetText() != form {
		t.Fatalf("active form = %+v, want the agent's phrasing", rows[0].GetStatus().GetRunning())
	}
}

func TestARunningTaskWithNoPhrasingDrawsNoActiveForm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", runningTask("migrate", nil)))

	// Assert
	rows := h.view(t).GetExpanded().GetTasks().GetRows()
	if rows[0].GetStatus().GetRunning().ActiveForm != nil {
		t.Fatalf("an active form was synthesized where the agent gave none")
	}
}

func TestADeletedTaskLeavesTheChecklist(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", pendingTask("drop me")))

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", deletedTask("drop me")))

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetTasks() != nil {
		t.Fatalf("a deleted task stayed in the tracker")
	}
}

func TestADetachedShellRaisesTheShellsChip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetShells().GetCount(); got != 1 {
		t.Fatalf("shells chip = %d, want 1", got)
	}
	rows := h.view(t).GetExpanded().GetShells().GetRows()
	if rows[0].GetCommand().GetText() != "npm test" {
		t.Fatalf("command = %q, want the announced command line", rows[0].GetCommand().GetText())
	}
	if got := rows[0].GetTarget().GetValue(); got != "detached_shell|work-1|" {
		t.Fatalf("target = %q, want the shell bubble keyed by the work handle", got)
	}
}

func TestAShellDetachedFromAnInTurnUnitKeepsItsCommand(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "bash-1"},
		Item: &conversationv1.AgentActivity_Bash{
			Bash: &conversationv1.AgentBash{
				Result: &conversationv1.AgentBash_Start{
					Start: &conversationv1.AgentBashStart{
						Command:   &conversationv1.AgentBashCommand{Line: "sleep 600"},
						StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: instant.UnixMilli()},
					},
				},
			},
		},
	})

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-9"},
		Origin: &conversationv1.AgentDetachedWork_Detached{
			Detached: &conversationv1.DetachedWorkDetached{
				DetachedFromId: &conversationv1.AgentActivityId{Value: "bash-1"},
				Cause: &conversationv1.DetachedWorkDetached_Requested{
					Requested: &conversationv1.DetachedCauseRequested{},
				},
			},
		},
	})

	// Assert
	rows := h.view(t).GetExpanded().GetShells().GetRows()
	if len(rows) != 1 || rows[0].GetCommand().GetText() != "sleep 600" {
		t.Fatalf("rows = %+v, want the command from the unit it detached from", rows)
	}
}

func TestAShellsTerminalRetiresItsRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))

	// Act
	h.r.OnBash(testWS, &conversationv1.DetachedWorkId{Value: "work-1"}, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}},
	})

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetShells() != nil {
		t.Fatalf("the shells chip survived the command's terminal")
	}
}

func TestAnArmedMonitorRaisesTheMonitorsChip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, monitorStart("mon-1", "watching the build log", true))

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetMonitors().GetCount(); got != 1 {
		t.Fatalf("monitors chip = %d, want 1", got)
	}
	rows := h.view(t).GetExpanded().GetMonitors().GetRows()
	if rows[0].Persistent == nil {
		t.Fatalf("the persistent marker is absent on a persistent watch")
	}
}

func TestADeadlineMonitorCarriesNoPersistentMarker(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, monitorStart("mon-1", "watching", false))

	// Assert
	rows := h.view(t).GetExpanded().GetMonitors().GetRows()
	if rows[0].Persistent != nil {
		t.Fatalf("a deadline watch drew the persistent marker")
	}
}

func TestAnEndedMonitorLeavesTheChip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, monitorStart("mon-1", "watching", true))

	// Act
	h.r.OnActivity(testWS, mainAgent, &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "mon-1"},
		Item: &conversationv1.AgentActivity_Monitor{
			Monitor: &conversationv1.AgentMonitor{
				Result: &conversationv1.AgentMonitor_Ended{Ended: &conversationv1.AgentMonitorEnded{}},
			},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetMonitors() != nil {
		t.Fatalf("the monitors chip survived the watch's end")
	}
}

func TestAJobListingReplacesTheWholeSet(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, cronListed(
		&conversationv1.AgentCronJob{JobId: "j1", Cron: "*/5 * * * *", HumanSchedule: "every 5 min"},
		&conversationv1.AgentCronJob{JobId: "j2", Cron: "0 9 * * *", HumanSchedule: "daily at 9"},
	))

	// Act
	h.r.OnActivity(testWS, mainAgent, cronListed(
		&conversationv1.AgentCronJob{JobId: "j3", Cron: "0 0 * * *", HumanSchedule: "nightly"},
	))

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetCrons().GetCount(); got != 1 {
		t.Fatalf("crons chip = %d, want 1: a listing is replace semantics", got)
	}
}

func TestAJobsNextFireIsResolvedDaemonSide(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, cronListed(&conversationv1.AgentCronJob{
		JobId: "j1", Cron: "*/5 * * * *", HumanSchedule: "every 5 min", Prompt: "check the deploy",
	}))

	// Assert
	rows := h.view(t).GetExpanded().GetCrons().GetRows()
	want := instant.Add(5 * time.Minute).UnixMilli()
	if rows[0].GetNextFire().GetFireAtMs() != want {
		t.Fatalf("next fire = %d, want %d", rows[0].GetNextFire().GetFireAtMs(), want)
	}
}

func TestAnUnresolvableScheduleLeavesTheNextFireUnset(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, cronListed(&conversationv1.AgentCronJob{
		JobId: "j1", Cron: "@hourly", HumanSchedule: "hourly",
	}))

	// Assert
	rows := h.view(t).GetExpanded().GetCrons().GetRows()
	if rows[0].NextFire != nil {
		t.Fatalf("next fire = %+v, want UNSET rather than a guess", rows[0].NextFire)
	}
	if rows[0].GetSchedule().GetText() != "hourly" {
		t.Fatalf("the row must still draw its schedule alone")
	}
}

func TestADeletedJobLeavesTheSet(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, cronListed(
		&conversationv1.AgentCronJob{JobId: "j1", Cron: "* * * * *", HumanSchedule: "every min"},
	))

	// Act
	h.r.OnActivity(testWS, mainAgent, &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "cron-2"},
		Item: &conversationv1.AgentActivity_Cron{
			Cron: &conversationv1.AgentCron{
				State: &conversationv1.AgentCron_Success{
					Success: &conversationv1.AgentCronSuccess{
						Act: &conversationv1.AgentCronSuccess_Deleted{
							Deleted: &conversationv1.AgentCronDeleted{JobId: "j1"},
						},
					},
				},
			},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetCrons() != nil {
		t.Fatalf("the crons chip survived the job's deletion")
	}
}

func TestPanelRowsKeepTheirArrivalOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "First", ""))
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-2", "agent-3", "Second", ""))

	// Assert
	rows := h.view(t).GetExpanded().GetAgents().GetRows()
	if len(rows) != 2 || rows[0].GetLabel().GetText() != "First" {
		t.Fatalf("rows = %+v, want spawn order", rows)
	}
}

func TestAnEmptyPanelStillShips(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	connected(h)

	// Assert
	expanded := h.view(t).GetExpanded()
	if expanded.GetShells() == nil || expanded.GetShells().GetRows() != nil {
		t.Fatalf("shells panel = %+v, want present with an empty row list", expanded.GetShells())
	}
}

// ---- the momentary loading status ----------------------------------------

// memoryInjection is a silently injected memory file.
func memoryInjection(path string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "inject-1"},
		Item: &conversationv1.AgentActivity_ContextInjected{
			ContextInjected: &conversationv1.AgentContextInjected{
				Injected: &conversationv1.AgentContextInjected_Memory{
					Memory: &conversationv1.AgentInjectedMemory{Path: path},
				},
			},
		},
	}
}

// skillInjection is a silently injected set of skills. A skill whose content
// is set was loaded; one without it was only listed.
func skillInjection(names []string, withContent int) *conversationv1.AgentActivity {
	skills := make([]*conversationv1.AgentInjectedSkill, 0, len(names))
	for i, name := range names {
		skill := &conversationv1.AgentInjectedSkill{Name: name}
		if i < withContent {
			content := "the skill document"
			skill.Content = &content
		}
		skills = append(skills, skill)
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "inject-1"},
		Item: &conversationv1.AgentActivity_ContextInjected{
			ContextInjected: &conversationv1.AgentContextInjected{
				Injected: &conversationv1.AgentContextInjected_Skills{
					Skills: &conversationv1.AgentInjectedSkills{Skills: skills},
				},
			},
		},
	}
}

func TestAnInjectedMemoryFileIsLoadingMemory(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, memoryInjection("webapp/CLAUDE.md"))

	// Assert
	loading := h.view(t).GetStrip().GetStatus().GetLoading()
	if loading.GetMemory() == nil {
		t.Fatalf("substatus = %+v, want memory", loading.GetSubstatus())
	}
	if got := loading.GetActivity().GetContextInjected().GetText(); got != "webapp/CLAUDE.md" {
		t.Fatalf("item line = %q, want the injected path", got)
	}
}

func TestOneLoadedSkillIsLoadingInvoked(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, skillInjection([]string{"graphify"}, 1))

	// Assert
	loading := h.view(t).GetStrip().GetStatus().GetLoading()
	if loading.GetInvoked() == nil {
		t.Fatalf("substatus = %+v, want invoked", loading.GetSubstatus())
	}
	if got := loading.GetActivity().GetContextInjected().GetText(); got != "graphify" {
		t.Fatalf("item line = %q, want the skill's name", got)
	}
}

func TestSeveralLoadedSkillsAreLoadingDiscovered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, skillInjection([]string{"a", "b", "c"}, 3))

	// Assert
	loading := h.view(t).GetStrip().GetStatus().GetLoading()
	if loading.GetDiscovered() == nil {
		t.Fatalf("substatus = %+v, want discovered", loading.GetSubstatus())
	}
	if got := loading.GetActivity().GetContextInjected().GetText(); got != "3 skills" {
		t.Fatalf("item line = %q, want the count", got)
	}
}

func TestContentFreeSkillsAreLoadingListing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, skillInjection([]string{"a", "b"}, 0))

	// Assert
	loading := h.view(t).GetStrip().GetStatus().GetLoading()
	if loading.GetListing() == nil {
		t.Fatalf("substatus = %+v, want listing", loading.GetSubstatus())
	}
}

func TestTheLoadingActivityIsAlwaysPresent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, memoryInjection("CLAUDE.md"))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetLoading().GetActivity() == nil {
		t.Fatalf("the loading activity is REQUIRED: the injection IS the status")
	}
}
