package footer

import (
	"fmt"
	"os"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
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

// stated is a subject the act NAMES. `AgentTaskState.subject` carries presence,
// so a test says which of the two things it means -- named, or not named at
// all -- rather than leaning on the empty string to mean either.
func stated(subject string) *string { return &subject }

// pendingTask is a recorded, unstarted task.
func pendingTask(subject *string) *conversationv1.AgentTaskState {
	return &conversationv1.AgentTaskState{
		Subject: subject,
		Status:  &conversationv1.AgentTaskState_Pending{Pending: &conversationv1.AgentTaskPending{}},
	}
}

// completedTask is a task that achieved what it described.
func completedTask(subject *string) *conversationv1.AgentTaskState {
	return &conversationv1.AgentTaskState{
		Subject: subject,
		Status:  &conversationv1.AgentTaskState_Completed{Completed: &conversationv1.AgentTaskCompleted{}},
	}
}

// runningTask is a task being worked on, with the agent's phrasing when it gave
// one.
func runningTask(subject *string, activeForm *string) *conversationv1.AgentTaskState {
	return &conversationv1.AgentTaskState{
		Subject: subject,
		Status: &conversationv1.AgentTaskState_Running{
			Running: &conversationv1.AgentTaskRunning{ActiveForm: activeForm},
		},
	}
}

// deletedTask is a task removed from the plan.
func deletedTask(subject *string) *conversationv1.AgentTaskState {
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

func TestTheMainAgentsRowJumpsToTheEntryTheFeedPlacedOnTheRoot(t *testing.T) {
	// Arrange: the feed draws the main agent's spawn on the root feed.
	h := newHarness(t)
	connected(h)
	placed := &frontendv1.FeedId{Value: "r|activity|spawn-1|agent-2"}
	h.r.OnEntryPlaced(testWS, "spawn-1", placed)

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "map the resolvers"))

	// Assert
	rows := h.view(t).GetExpanded().GetAgents().GetRows()
	if len(rows) != 1 {
		t.Fatalf("rows = %d, want 1", len(rows))
	}
	if got := rows[0].GetJump().GetEntry().GetValue(); got != placed.GetValue() {
		t.Fatalf("jump entry = %q, want the FeedId the feed announced %q", got, placed.GetValue())
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

// detachedSubagentWork announces a spawn that is detached from the moment the
// footer hears of it -- the `created` origin.
func detachedSubagentWork(work, created, subagentType string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Owner: mainAgent,
		Work:  &conversationv1.DetachedWorkId{Value: work},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{
				Work: &conversationv1.DetachableWork_Subagent{Subagent: &conversationv1.AgentSubagent{
					Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
						CreatedAgentId: &conversationv1.AgentId{Value: created},
						Prompt:         &conversationv1.AgentSubagentPrompt{SubagentType: &subagentType},
						StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: instant.UnixMilli()},
					}},
				}},
			},
		}},
	}
}

// movedSubagent announces an in-turn spawn LEAVING the turn under a handle.
func movedSubagent(unit string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Owner: mainAgent,
		Work:  &conversationv1.DetachedWorkId{Value: unit},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: unit},
			Cause:          &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}},
		}},
	}
}

// subagentSettled is a detached run's own terminal, success or failure.
func subagentSettled(failed bool) *conversationv1.AgentSubagent {
	if failed {
		return &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{}},
		}
	}
	return &conversationv1.AgentSubagent{
		Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{}},
	}
}

// workID addresses one piece of detached work.
func workID(v string) *conversationv1.DetachedWorkId {
	return &conversationv1.DetachedWorkId{Value: v}
}

// detachedProgress is a backgrounded run's own running beat, carrying the
// running token sum a `task_progress` message states.
func detachedProgress(tokens uint64) *conversationv1.AgentSubagent {
	return &conversationv1.AgentSubagent{
		Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{
			Progress: &conversationv1.AgentSubagentProgress{TotalTokens: tokens},
		}},
	}
}

// TestSuccessiveDetachedProgressReplacesTheAgentFigure locks the running figure
// of a BACKGROUNDED agent to REPLACE, never sum: each beat states a whole-state
// running total, so the later one stands alone rather than being added.
func TestSuccessiveDetachedProgressReplacesTheAgentFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))
	h.r.OnSubagent(testWS, workID("work-1"), detachedProgress(12_400))

	// Act
	h.r.OnSubagent(testWS, workID("work-1"), detachedProgress(20_000))

	// Assert
	rows := h.view(t).GetExpanded().GetAgents().GetRows()
	if got := rows[0].GetTokens().GetText(); got != "20k tok" {
		t.Fatalf("tokens = %q, want the latest beat's whole sum, never 12.4k + 20k", got)
	}
}

func TestADetachedSubagentCountsInTheAgentsChip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetAgents().GetCount(); got != 1 {
		t.Fatalf("agents chip = %d, want the detached run counted", got)
	}
}

// THE DEFECT G50 READ: two settled placements and one live one, and the chip
// counted all three. A detached run's terminal is addressed to its HANDLE, and
// nothing retired the row from it.
func TestADetachedSubagentsTerminalRetiresItsChip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", ""))
	h.r.OnDetachedWork(testWS, mainAgent, movedSubagent("spawn-1"))

	// Act
	h.r.OnSubagent(testWS, workID("spawn-1"), subagentSettled(false))

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetAgents() != nil {
		t.Fatalf("the agents chip survived the detached run's own terminal")
	}
}

// HOWEVER IT SETTLED. A failed run is as over as a successful one, exactly as a
// shell's is.
func TestADetachedSubagentsFailureRetiresItsChip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))

	// Act
	h.r.OnSubagent(testWS, workID("work-1"), subagentSettled(true))

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetAgents() != nil {
		t.Fatalf("the agents chip survived the detached run's failure")
	}
}

// THE SPAWNING CALL RETURNING IS A LAUNCH RECEIPT. Once the run has left the
// turn, the caller's stream states nothing about whether it is still going, so
// a terminal arm read off that unit must not retire a run that is still live.
func TestTheSpawningCallsReturnDoesNotRetireADetachedRun(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", ""))
	h.r.OnDetachedWork(testWS, mainAgent, movedSubagent("spawn-1"))

	// Act: the spawn unit's own settled arm on the CALLER's activity stream.
	h.r.OnActivity(testWS, mainAgent, &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item:       &conversationv1.AgentActivity_Subagent{Subagent: subagentSettled(false)},
	})

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetAgents().GetCount(); got != 1 {
		t.Fatalf("agents chip = %d, want the detached run still counted", got)
	}
}

func TestTheTaskChipCountsDoneOverTotal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", completedTask(stated("write it"))))
	h.r.OnActivity(testWS, mainAgent, taskAct("t2", pendingTask(stated("test it"))))

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
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", runningTask(stated("migrate"), &form)))

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
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", runningTask(stated("migrate"), nil)))

	// Assert
	rows := h.view(t).GetExpanded().GetTasks().GetRows()
	if rows[0].GetStatus().GetRunning().ActiveForm != nil {
		t.Fatalf("an active form was synthesized where the agent gave none")
	}
}

// unstatedTask is a state that says NOTHING about where the task stands --
// the shape a `TaskUpdate` that moved only an edge or a subject produces, and
// the shape an announcement the tracker has not answered yet produces.
func unstatedTask(subject *string) *conversationv1.AgentTaskState {
	return &conversationv1.AgentTaskState{Subject: subject}
}

// AN UNSET STATUS IS NOT `pending`. The oneof exists so "this act said nothing
// about where the task stands" is representable, and reading it as pending
// knocked a running task back to unstarted on every subject-only or
// edge-only update.
func TestAnUnstatedStatusLeavesARunningTaskRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	form := "running the migration"
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", runningTask(stated("migrate"), &form)))

	// Act: an update that names an edge and no status at all.
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", unstatedTask(stated("migrate"))))

	// Assert
	rows := h.view(t).GetExpanded().GetTasks().GetRows()
	if rows[0].GetStatus().GetRunning() == nil {
		t.Fatalf("task status = %+v, want it still running", rows[0].GetStatus())
	}
}

// A NEW ENTRY STILL STARTS PENDING: "recorded and not begun" is what a task
// nobody has said anything about IS.
func TestAnUnstatedStatusOnANewTaskIsPending(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", unstatedTask(stated("brand new"))))

	// Assert
	rows := h.view(t).GetExpanded().GetTasks().GetRows()
	if rows[0].GetStatus().GetPending() == nil {
		t.Fatalf("task status = %+v, want pending", rows[0].GetStatus())
	}
}

// AN UNSTATED SUBJECT IS NOT A SUBJECT. A `TaskUpdate` naming only a status
// carries none, and overwriting with it drew the whole checklist as blank
// lines beside its glyphs -- observed in the G52 playbook.
func TestAnActThatNamesNoSubjectKeepsTheOneTheTaskHas(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", pendingTask(stated("Land the converter"))))

	// Act: a status-only update, which is what the tracker's own answer to
	// `TaskUpdate(status)` produces -- the subject field UNSET, not empty.
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", runningTask(nil, nil)))

	// Assert
	rows := h.view(t).GetExpanded().GetTasks().GetRows()
	if got := rows[0].GetSubject().GetText(); got != "Land the converter" {
		t.Fatalf("task subject = %q, want the one the create established", got)
	}
}

// A CHECKLIST ENTRY IS A SUBJECT. A `TaskUpdate` the tracker REFUSED for an id
// it does not hold names the task and no subject at either end, and the
// checklist gained a PHANTOM ROW -- a bare glyph with no words, counted in the
// chip's denominator, for a task the tracker had just said it does not have.
// `AgentTaskRejected` says it outright: nothing was added and nothing changed.
func TestAnActWithNoSubjectOpensNoChecklistEntry(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", pendingTask(stated("Land the converter"))))

	// Act: the refused update's own shape -- a task nobody has named.
	h.r.OnActivity(testWS, mainAgent, taskAct("t9", unstatedTask(nil)))

	// Assert
	rows := h.view(t).GetExpanded().GetTasks().GetRows()
	if len(rows) != 1 {
		t.Fatalf("the checklist holds %d rows, want only the one that was named: %+v", len(rows), rows)
	}
	if chip := h.view(t).GetStrip().GetLiveWork().GetTasks(); chip.GetTotal() != 1 {
		t.Fatalf("tasks chip = %+v, want a denominator of 1", chip)
	}
}

// A SUBJECT STATED EMPTY IS A SUBJECT. Presence is what tells the two apart,
// and the reading that mattered for the checklist -- keeping what an act did
// not state -- must not become a reading that IGNORES what an act did state.
func TestAnActThatStatesAnEmptySubjectSetsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", pendingTask(stated("Land the converter"))))

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", runningTask(stated(""), nil)))

	// Assert
	rows := h.view(t).GetExpanded().GetTasks().GetRows()
	if got := rows[0].GetSubject().GetText(); got != "" {
		t.Fatalf("task subject = %q, want the empty subject the act stated", got)
	}
}

func TestADeletedTaskLeavesTheChecklist(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", pendingTask(stated("drop me"))))

	// Act
	h.r.OnActivity(testWS, mainAgent, taskAct("t1", deletedTask(stated("drop me"))))

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
	if got := rows[0].GetWork().GetValue(); got != "work-1" {
		t.Fatalf("work = %q, want the shell's work handle", got)
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
		Owner: mainAgent,
		Work:  &conversationv1.DetachedWorkId{Value: "work-9"},
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
	if got := loading.GetActivity().GetUnpinned().GetTransient().GetContextInjected().GetText(); got != "webapp/CLAUDE.md" {
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
	if got := loading.GetActivity().GetUnpinned().GetTransient().GetContextInjected().GetText(); got != "graphify" {
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
	if got := loading.GetActivity().GetUnpinned().GetTransient().GetContextInjected().GetText(); got != "3 skills" {
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

// wakeupStopped retires the pending self-scheduled wakeup.
func wakeupStopped() *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "wake-1"},
		Item: &conversationv1.AgentActivity_ScheduleWakeup{
			ScheduleWakeup: &conversationv1.AgentScheduleWakeup{
				Result: &conversationv1.AgentScheduleWakeup_Success{
					Success: &conversationv1.AgentScheduleWakeupSuccess{
						Outcome: &conversationv1.AgentScheduleWakeupSuccess_Stopped{
							Stopped: &conversationv1.AgentScheduleWakeupStopped{},
						},
					},
				},
			},
		},
	}
}

// THE ⏱ CHIP IS ABOUT SCHEDULED JOBS, not about crons alone: footer.proto
// words it "live scheduled jobs (cron/wakeup schedules)", so a pending
// self-scheduled wakeup sets it exactly as a cron job does.
func TestTheScheduledJobsChipCountsCronsAndTheWakeup(t *testing.T) {
	tests := []struct {
		name  string
		acts  []*conversationv1.AgentActivity
		want  uint32
		unset bool
	}{
		{
			name: "a cron job alone sets the chip",
			acts: []*conversationv1.AgentActivity{
				cronListed(&conversationv1.AgentCronJob{JobId: "j1", Cron: "* * * * *", HumanSchedule: "every min"}),
			},
			want: 1,
		},
		{
			name: "a pending wakeup alone sets the chip",
			acts: []*conversationv1.AgentActivity{wakeupScheduled(instant.Add(5 * time.Minute))},
			want: 1,
		},
		{
			name: "a cron job and a wakeup both count",
			acts: []*conversationv1.AgentActivity{
				cronListed(&conversationv1.AgentCronJob{JobId: "j1", Cron: "* * * * *", HumanSchedule: "every min"}),
				wakeupScheduled(instant.Add(5 * time.Minute)),
			},
			want: 2,
		},
		{
			name: "stopping the only wakeup retires the chip",
			acts: []*conversationv1.AgentActivity{
				wakeupScheduled(instant.Add(5 * time.Minute)),
				wakeupStopped(),
			},
			unset: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			for _, act := range tc.acts {
				h.r.OnActivity(testWS, mainAgent, act)
			}

			// Assert
			chip := h.view(t).GetStrip().GetLiveWork().GetCrons()
			if tc.unset {
				if chip != nil {
					t.Fatalf("the scheduled-jobs chip = %+v, want UNSET once nothing is scheduled", chip)
				}
				return
			}
			if got := chip.GetCount(); got != tc.want {
				t.Fatalf("scheduled-jobs chip = %d, want %d", got, tc.want)
			}
		})
	}
}

// subagentStartFrame is a detached run's own opening frame, as its OWN book
// carries it -- the frame the caller's book never sends.
func subagentStartFrame(created, subagentType string) *conversationv1.AgentSubagent {
	return &conversationv1.AgentSubagent{
		Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
			CreatedAgentId: &conversationv1.AgentId{Value: created},
			Prompt:         &conversationv1.AgentSubagentPrompt{SubagentType: &subagentType},
			StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: instant.UnixMilli()},
		}},
	}
}

// A TERMINAL IS FINAL, AND THE TWO BOOKS ARE NOT ORDERED AGAINST EACH OTHER.
//
// MEASURED in the G50 playbook: `AgentSubagent_Success` for a handle arrived on
// the spawning agent's book and retired the chip row, and 80ms later the SAME
// handle's `AgentSubagent_Start` arrived on the run's own book and re-opened
// it. The ⚙ chip then read 3 beside two settled placements and one live one,
// for the rest of the session -- the very count the terminal was supposed to
// have retired.
func TestADetachedSubagentsStartAfterItsTerminalDoesNotCountItLiveAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))
	h.r.OnSubagent(testWS, workID("work-1"), subagentSettled(false))

	// Act: the run's own book replaying its opening frame after the settle.
	h.r.OnSubagent(testWS, workID("work-1"), subagentStartFrame("agent-2", "Explore"))

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetAgents(); got != nil {
		t.Fatalf("agents chip = %d after a start replayed behind the run's own terminal, want the run to stay retired",
			got.GetCount())
	}
}

// AND NEITHER DOES THE ANNOUNCEMENT, which reaches the footer once per book for
// the same reason the start does.
func TestADetachedWorkAnnouncementAfterItsTerminalDoesNotCountItLiveAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))
	h.r.OnSubagent(testWS, workID("work-1"), subagentSettled(false))

	// Act: the announcement told a second time, after the settle.
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetAgents(); got != nil {
		t.Fatalf("agents chip = %d after the announcement replayed behind the run's own terminal, want the run to stay retired",
			got.GetCount())
	}
}

// AND THE CALLER'S OWN STREAM REPLAYS TOO. The spawn unit and the handle are
// one value, so a spawn frame re-read off the calling agent's activity stream
// after the run settled re-opened the row the terminal had taken away -- the
// second half of the ⚙ chip reading 3 in the G50 playbook.
func TestASpawnUnitReplayedAfterItsRunSettledDoesNotCountItLiveAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", ""))
	h.r.OnDetachedWork(testWS, mainAgent, movedSubagent("spawn-1"))
	h.r.OnSubagent(testWS, workID("spawn-1"), subagentSettled(false))

	// Act: the caller's stream re-read from its own start.
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", ""))

	// Assert
	if got := h.view(t).GetStrip().GetLiveWork().GetAgents(); got != nil {
		t.Fatalf("agents chip = %d after the spawn unit replayed behind the run's own terminal, want the run to stay retired",
			got.GetCount())
	}
}

// ---- the jump rows ----------------------------------------------------------

// jumpArm names a row's jump the way the record does.
func jumpArm(jump *frontendv1.FooterJump) string {
	resolution, _ := jumpResolution(jump)
	return resolution
}

func TestEveryDetachedWorkRowStatesExactlyOneJumpArm(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		read    func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string)
		want    string
		wantID  string
	}{
		{
			name: "an agent row the feed has not drawn is unresolved(not_drawn)",
			arrange: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "map"))
			},
			read: func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string) {
				row := v.GetExpanded().GetAgents().GetRows()[0]
				return row.GetJump(), row.GetWork().GetValue()
			},
			want: "not_drawn", wantID: "spawn-1",
		},
		{
			name: "an agent row the feed drew names its entry",
			arrange: func(h *harness) {
				h.r.OnEntryPlaced(testWS, "spawn-1", &frontendv1.FeedId{Value: "r|spawn-1"})
				h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "map"))
			},
			read: func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string) {
				row := v.GetExpanded().GetAgents().GetRows()[0]
				return row.GetJump(), row.GetWork().GetValue()
			},
			want: "entry", wantID: "spawn-1",
		},
		{
			name: "a detached agent row carries its handle as its work id",
			arrange: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-9", "work-9", "Explore"))
			},
			read: func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string) {
				row := v.GetExpanded().GetAgents().GetRows()[0]
				return row.GetJump(), row.GetWork().GetValue()
			},
			want: "not_drawn", wantID: "work-9",
		},
		{
			name: "a shell row the feed drew names its head",
			arrange: func(h *harness) {
				h.r.OnEntryPlaced(testWS, "work-1", &frontendv1.FeedId{Value: "r|shell_head|work-1"})
				h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))
			},
			read: func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string) {
				row := v.GetExpanded().GetShells().GetRows()[0]
				return row.GetJump(), row.GetWork().GetValue()
			},
			want: "entry", wantID: "work-1",
		},
		{
			name: "a shell row the feed has not drawn is unresolved(not_drawn)",
			arrange: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))
			},
			read: func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string) {
				row := v.GetExpanded().GetShells().GetRows()[0]
				return row.GetJump(), row.GetWork().GetValue()
			},
			want: "not_drawn", wantID: "work-1",
		},
		{
			name: "a monitor row the feed drew names its tool-call card",
			arrange: func(h *harness) {
				h.r.OnEntryPlaced(testWS, "mon-1", &frontendv1.FeedId{Value: "r|activity|mon-1"})
				h.r.OnActivity(testWS, mainAgent, monitorStart("mon-1", "watch the build", false))
			},
			read: func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string) {
				row := v.GetExpanded().GetMonitors().GetRows()[0]
				return row.GetJump(), row.GetWork().GetValue()
			},
			want: "entry", wantID: "mon-1",
		},
		{
			name: "a monitor row the feed has not drawn is unresolved(not_drawn)",
			arrange: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, monitorStart("mon-1", "watch the build", false))
			},
			read: func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string) {
				row := v.GetExpanded().GetMonitors().GetRows()[0]
				return row.GetJump(), row.GetWork().GetValue()
			},
			want: "not_drawn", wantID: "mon-1",
		},
		{
			name: "a created monitor row the feed drew names its tool-call card",
			arrange: func(h *harness) {
				h.r.OnEntryPlaced(testWS, "mon-1", &frontendv1.FeedId{Value: "r|activity|mon-1"})
				h.r.OnDetachedWork(testWS, mainAgent, &conversationv1.AgentDetachedWork{
					Owner: mainAgent,
					Work:  &conversationv1.DetachedWorkId{Value: "mon-1"},
					Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
						WorkCreated: &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Monitor{
							Monitor: monitorStart("mon-1", "watch the build", false).GetMonitor(),
						}},
					}},
				})
			},
			read: func(v *frontendv1.FooterView) (*frontendv1.FooterJump, string) {
				row := v.GetExpanded().GetMonitors().GetRows()[0]
				return row.GetJump(), row.GetWork().GetValue()
			},
			want: "entry", wantID: "mon-1",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			tt.arrange(h)

			// Act
			jump, id := tt.read(h.view(t))

			// Assert
			if got := jumpArm(jump); got != tt.want {
				t.Fatalf("jump = %q, want %q", got, tt.want)
			}
			if id != tt.wantID {
				t.Fatalf("work = %q, want %q", id, tt.wantID)
			}
		})
	}
}

func TestAMonitorRowNamesTheExactCardTheFeedAnnounced(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, monitorStart("mon-1", "watch the build", false))

	// Act: the feed draws the Monitor call's card.
	h.r.OnEntryPlaced(testWS, "mon-1", &frontendv1.FeedId{Value: "a|sub|activity|mon-1"})

	// Assert
	row := h.view(t).GetExpanded().GetMonitors().GetRows()[0]
	if got := row.GetJump().GetEntry().GetValue(); got != "a|sub|activity|mon-1" {
		t.Fatalf("jump entry = %q, want the card the feed announced", got)
	}
}

func TestAMonitorRowsResolutionChangeIsRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, monitorStart("mon-1", "watch the build", false))

	// Act
	h.r.OnEntryPlaced(testWS, "mon-1", &frontendv1.FeedId{Value: "r|activity|mon-1"})

	// Assert
	var kinds []string
	for _, note := range recordsOf(h.log.Records(), "daemon.footer.jump_resolution") {
		if note.Context["kind"] == "monitor" {
			kinds = append(kinds, fmt.Sprint(note.Context["resolution"]))
		}
	}
	if strings.Join(kinds, ",") != "not_drawn,entry" {
		t.Fatalf("monitor jump records = %v, want not_drawn then entry", kinds)
	}
}

func TestAPlacementAfterTheRowResolvesItsJump(t *testing.T) {
	// Arrange: the row stands unresolved.
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "map"))

	// Act: the feed draws the entry.
	h.r.OnEntryPlaced(testWS, "spawn-1", &frontendv1.FeedId{Value: "r|activity|spawn-1"})

	// Assert
	row := h.view(t).GetExpanded().GetAgents().GetRows()[0]
	if got := row.GetJump().GetEntry().GetValue(); got != "r|activity|spawn-1" {
		t.Fatalf("jump entry = %q, want the placement the feed announced after the row opened", got)
	}
}

func TestAJumpResolutionIsRecordedOncePerChange(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "map"))

	// Act: two pushes that leave the row unresolved, then the placement.
	h.r.OnActivity(testWS, mainAgent, subagentProgress("spawn-1", 10))
	h.r.OnActivity(testWS, mainAgent, subagentProgress("spawn-1", 20))
	h.r.OnEntryPlaced(testWS, "spawn-1", &frontendv1.FeedId{Value: "r|spawn-1"})

	// Assert
	notes := recordsOf(h.log.Records(), "daemon.footer.jump_resolution")
	if len(notes) != 2 {
		t.Fatalf("jump records = %d, want 2 (not_drawn once, then entry): %+v", len(notes), notes)
	}
	if notes[0].Context["resolution"] != "not_drawn" || notes[1].Context["resolution"] != "entry" {
		t.Fatalf("resolutions = %v then %v, want not_drawn then entry", notes[0].Context["resolution"], notes[1].Context["resolution"])
	}
}

func TestAJumpRecordStatesWhatTheMainAgentsRowIs(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", "agent-2", "Explore", "map"))

	// Assert
	note := recordsOf(h.log.Records(), "daemon.footer.jump_resolution")[0]
	want := dlog.Context{
		"kind": "agent", "work_id": "spawn-1", "resolution": "not_drawn", "entry": "",
		"provenance": "spawn_frame", "spawned_on": mainAgent.GetValue(), "detached": false,
		"label": "Explore", "has_description": true, "tokens": uint64(0), "retired_before": false,
	}
	for k, v := range want {
		if note.Context[k] != v {
			t.Fatalf("record[%q] = %#v, want %#v (whole record %+v)", k, note.Context[k], v, note.Context)
		}
	}
	if note.Level != dlog.LevelInfo {
		t.Fatalf("level = %q, want info", note.Level)
	}
}

func TestAnEntryPlacementWithNoUnitIsAnError(t *testing.T) {
	tests := []struct {
		name string
		unit string
		row  *frontendv1.FeedId
	}{
		{name: "no unit", unit: "", row: &frontendv1.FeedId{Value: "r|x"}},
		{name: "no FeedId", unit: "spawn-1", row: &frontendv1.FeedId{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.OnEntryPlaced(testWS, tt.unit, tt.row)

			// Assert
			if !hasLevel(h.log.Records(), dlog.LevelError, "daemon.footer.entry_unaddressed") {
				t.Fatalf("records = %+v, want the ERROR for an unaddressed placement", h.log.Records())
			}
		})
	}
}

func TestSubagentRowFindsALiveRowByAnyOfItsIdentities(t *testing.T) {
	tests := []struct {
		name string
		id   string
		want bool
	}{
		{name: "the key it is held by", id: "key-1", want: true},
		{name: "its handle", id: "work-1", want: true},
		{name: "its spawn unit", id: "unit-1", want: true},
		{name: "its created agent", id: "agent-1", want: true},
		{name: "an id it is not addressed by", id: "other", want: false},
		{name: "the empty id", id: "", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := newWSState()
			s.agents["key-1"] = &agentRow{work: "work-1", spawnUnit: "unit-1", createdAgent: "agent-1"}

			// Act
			got := s.subagentRow(tt.id) != nil

			// Assert
			if got != tt.want {
				t.Fatalf("subagentRow(%q) found = %v, want %v", tt.id, got, tt.want)
			}
		})
	}
}
func TestAnInTurnRowIsNotRememberedAtRetirement(t *testing.T) {
	// Arrange
	s := newWSState()

	// Act
	s.rememberRetired(&agentRow{spawnUnit: "unit-1", createdAgent: "agent-1"})

	// Assert
	if n := len(s.retiredRows); n != 0 {
		t.Fatalf("%d retired rows kept, want none: an in-turn row is never waited on", n)
	}
}

// mergeTestRow is one merge tests panel row.
func mergeTestRow(name string, state *frontendv1.FooterMergeTestRowState) *frontendv1.FooterMergeTestRow {
	return &frontendv1.FooterMergeTestRow{Name: &frontendv1.FooterMergeTestRowName{Text: name}, State: state}
}

func waitingRow() *frontendv1.FooterMergeTestRowState {
	return &frontendv1.FooterMergeTestRowState{State: &frontendv1.FooterMergeTestRowState_Waiting{Waiting: &frontendv1.FooterMergeTestRowWaiting{}}}
}

func runningRow() *frontendv1.FooterMergeTestRowState {
	return &frontendv1.FooterMergeTestRowState{State: &frontendv1.FooterMergeTestRowState_Running{Running: &frontendv1.FooterMergeTestRowRunning{StartedAtMs: 1}}}
}

func passedRow() *frontendv1.FooterMergeTestRowState {
	return &frontendv1.FooterMergeTestRowState{State: &frontendv1.FooterMergeTestRowState_Passed{Passed: &frontendv1.FooterMergeTestRowPassed{DurationMs: 1000}}}
}

func failedRow() *frontendv1.FooterMergeTestRowState {
	return &frontendv1.FooterMergeTestRowState{State: &frontendv1.FooterMergeTestRowState_Failed{Failed: &frontendv1.FooterMergeTestRowFailed{DurationMs: 2000}}}
}

func TestTheMergeTestsChipCountsFinishedSuitesOfAll(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	rows := []*frontendv1.FooterMergeTestRow{
		mergeTestRow("daemon", passedRow()),
		mergeTestRow("webapp", failedRow()),
		mergeTestRow("elisp", runningRow()),
		mergeTestRow("shim", waitingRow()),
	}

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepTesting, TestsRound: 1, Tests: rows})

	// Assert
	chip := h.view(t).GetStrip().GetLiveWork().GetMergeTests()
	if chip.GetFinished() != 2 || chip.GetTotal() != 4 {
		t.Fatalf("merge tests chip = %+v, want 2 of 4 finished", chip)
	}
}

func TestTheMergeTestsChipIsUnsetWithNoSuites(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepCommitting})

	// Assert
	if chip := h.view(t).GetStrip().GetLiveWork().GetMergeTests(); chip != nil {
		t.Fatalf("merge tests chip = %+v, want unset when the panel holds no suite", chip)
	}
}

func TestTheMergeTestsPanelListsTheRoundsSuitesInOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	rows := []*frontendv1.FooterMergeTestRow{mergeTestRow("daemon", passedRow()), mergeTestRow("webapp", runningRow())}

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepTesting, TestsRound: 1, Tests: rows})

	// Assert
	panel := h.view(t).GetExpanded().GetMergeTests().GetRows()
	if len(panel) != 2 || panel[0].GetName().GetText() != "daemon" || panel[1].GetState().GetRunning() == nil {
		t.Fatalf("merge tests panel = %+v, want daemon then a running webapp", panel)
	}
}

func TestTheMergeTestsPanelIsEmptyWhenTheMergeIsNotTesting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepTesting, TestsRound: 1, Tests: []*frontendv1.FooterMergeTestRow{mergeTestRow("daemon", runningRow())}})

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepCommitting, TestsRound: 1})

	// Assert
	if rows := h.view(t).GetExpanded().GetMergeTests().GetRows(); len(rows) != 0 {
		t.Fatalf("merge tests panel = %+v, want empty once testing ended", rows)
	}
}

// ---- the one reading of a start and an update ---------------------------------

func TestTakeStartDescribesTheRowFromTheStart(t *testing.T) {
	// Arrange
	row := &agentRow{}
	start := subagentStartFrame("agent-2", "Explore").GetStart()
	description := "map the resolvers"
	start.Prompt.Description = &description

	// Act
	row.takeStart(start)

	// Assert
	want := agentRow{createdAgent: "agent-2", label: "Explore", description: description, startedAt: time.UnixMilli(instant.UnixMilli())}
	if row.createdAgent != want.createdAgent || row.label != want.label || row.description != want.description || !row.startedAt.Equal(want.startedAt) {
		t.Fatalf("row = %+v, want %+v", *row, want)
	}
}

func TestTakeUpdateFoldsABeat(t *testing.T) {
	description := "restated"
	tests := []struct {
		name            string
		prompt          *conversationv1.AgentSubagentPrompt
		wantDescription string
	}{
		{name: "a beat that restates a description replaces it", prompt: &conversationv1.AgentSubagentPrompt{Description: &description}, wantDescription: description},
		{name: "a beat that restates none keeps the row's", prompt: &conversationv1.AgentSubagentPrompt{}, wantDescription: "original"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			row := &agentRow{description: "original", tokens: 5}

			// Act
			row.takeUpdate(&conversationv1.AgentSubagentUpdate{
				Prompt:   tt.prompt,
				Progress: &conversationv1.AgentSubagentProgress{TotalTokens: 900},
			})

			// Assert
			if row.tokens != 900 || row.description != tt.wantDescription {
				t.Fatalf("row = {tokens %d, description %q}, want {900, %q}", row.tokens, row.description, tt.wantDescription)
			}
		})
	}
}

// EVERY READING OF A SPAWN'S PROMPT INTO A ROW GOES THROUGH takeStart AND
// takeUpdate: a hand-rolled site elsewhere in chips.go would let the spawning
// call's stream and the run's own describe one run differently.
func TestChipsReadsAPromptOnlyThroughTheSharedHelpers(t *testing.T) {
	// Arrange
	source, err := os.ReadFile("chips.go")
	if err != nil {
		t.Fatalf("read chips.go: %v", err)
	}

	// Act
	reads := strings.Count(string(source), "GetPrompt().GetDescription()")

	// Assert
	if reads != 2 {
		t.Fatalf("chips.go reads a prompt's description at %d sites, want 2 (takeStart and takeUpdate)", reads)
	}
}
