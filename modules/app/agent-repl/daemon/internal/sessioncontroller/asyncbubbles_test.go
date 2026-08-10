package sessioncontroller

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// detachedFold is one detached agent's contribution, as CurateEvent would hand
// it over.
func detachedFold(sourceToolUseID, agentID, text string) frontend.DetachedFold {
	return frontend.DetachedFold{
		SourceToolUseID: sourceToolUseID,
		AgentID:         agentID,
		Emissions: []*frontendv1.AgentEmission{{Emission: &frontendv1.AgentEmission_Response{
			Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{
				Content: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
				},
			}},
		}}},
	}
}

// callingFold is a detached agent's record that itself makes a tool call, which
// is how a nested dispatch's parent is learned.
func callingFold(sourceToolUseID, agentID, innerToolUseID, toolName string) frontend.DetachedFold {
	return frontend.DetachedFold{
		SourceToolUseID: sourceToolUseID,
		AgentID:         agentID,
		Emissions: []*frontendv1.AgentEmission{{Emission: &frontendv1.AgentEmission_Response{
			Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{
				Content: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: innerToolUseID, Name: toolName}}},
				},
			}},
		}}},
	}
}

func bashOutcome(toolUseID, taskID string) frontend.ToolOutcome {
	return frontend.ToolOutcome{ToolUseID: toolUseID, Result: &datav1.ToolUseResult{
		Result: &datav1.ToolUseResult_Bash{Bash: &datav1.BashResult{BackgroundTaskId: taskID}},
	}}
}

func retrieval(taskID, output string, status datav1.RawTaskStatus, exit *int32) frontend.ToolOutcome {
	task := &datav1.LocalBashTask{TaskId: taskID, Output: output, Status: status}
	if exit != nil {
		task.ExitCode, task.ExitCodeSet = *exit, true
	}
	return frontend.ToolOutcome{ToolUseID: "tu_out", Result: &datav1.ToolUseResult{
		Result: &datav1.ToolUseResult_TaskOutput{TaskOutput: &datav1.TaskOutputResult{
			Task: &datav1.TaskOutputResult_LocalBash{LocalBash: task},
		}},
	}}
}

func exitCode(v int32) *int32 { return &v }

// --- opening from a detached agent's own records ---------------------------

func TestObserveCurationOpensABubbleForANewDetachedAgent(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Opened) != 1 {
		t.Fatalf("the first record of a detached conversation opens its bubble, got %d", len(push.Opened))
	}
}

func TestObserveCurationFoldsASecondRecordIntoTheSameBubble(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "one")}}, 10); err != nil {
		t.Fatal(err)
	}
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "two")}}, 11)
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Opened) != 0 {
		t.Fatalf("the same detachment must not open a second bubble, got %d opened", len(push.Opened))
	}
}

func TestObserveCurationAddressesTheUpdateToTheOpenedBubble(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if push.Updates[0].GetBubbleId() != push.Opened[0].GetId() {
		t.Fatal("an update must name the bubble the same push opened")
	}
}

func TestObserveCurationRefusesADetachedRecordItCannotAttributeToACall(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	_, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("", "agent_1", "hi")}}, 10)
	if err == nil {
		t.Fatal("a record naming neither a source call nor an open bubble has nothing to attribute the detachment to")
	}
}

func TestObserveCurationLabelsABubbleFromTheToolThatLaunchedIt(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{
		ToolNames: map[string]string{"tu_1": "Agent"},
		Detached:  []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")},
	}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if push.Opened[0].GetLabel() != "Agent" {
		t.Fatalf("want the tool name as the fold's face, got %q", push.Opened[0].GetLabel())
	}
}

// --- the classification verdict on the tool card ---------------------------

func TestSpawnedBubbleIDNamesTheBubbleACallDetached(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if s.spawnedBubbleID("tu_1") != push.Opened[0].GetId() {
		t.Fatal("the launching call must resolve to the bubble it launched")
	}
}

func TestSpawnedBubbleIDIsEmptyForACallThatDetachedNothing(t *testing.T) {
	if got := newAsyncBubbleStore("/ws", nil).spawnedBubbleID("tu_other"); got != "" {
		t.Fatalf("empty is the only reading of a call that detached nothing, got %q", got)
	}
}

// --- nested dispatch -------------------------------------------------------

func TestANestedDispatchPointsAtTheBubbleItWasLaunchedFrom(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	outer, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{callingFold("tu_1", "agent_1", "tu_inner", "Agent")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	inner, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_inner", "agent_2", "hi")}}, 11)
	if err != nil {
		t.Fatal(err)
	}
	if inner.Opened[0].GetParentBubbleId() != outer.Opened[0].GetId() {
		t.Fatalf("want parent=%q, got %q", outer.Opened[0].GetId(), inner.Opened[0].GetParentBubbleId())
	}
}

func TestATopLevelDispatchHasNoParentPointer(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if got := push.Opened[0].GetParentBubbleId(); got != "" {
		t.Fatalf("a top-level detachment has no parent, got %q", got)
	}
}

// --- shell launches --------------------------------------------------------

func TestABackgroundShellLaunchOpensAShellBubble(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if push.Opened[0].GetShell() == nil {
		t.Fatalf("a BashResult carrying a background task id IS a background launch, got %T", push.Opened[0].GetKind())
	}
}

func TestAForegroundShellDetachesNothing(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		{ToolUseID: "tu_1", Result: &datav1.ToolUseResult{Result: &datav1.ToolUseResult_Bash{Bash: &datav1.BashResult{}}}},
	}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if !push.empty() {
		t.Fatal("a shell with no background task id opened no bubble")
	}
}

func TestAnAsyncAgentLaunchOpensAnAgentBubble(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{{
		ToolUseID: "tu_1",
		Result: &datav1.ToolUseResult{Result: &datav1.ToolUseResult_AgentAsyncLaunch{
			AgentAsyncLaunch: &datav1.AgentAsyncLaunch{AgentId: "agent_1", Description: "review"},
		}},
	}}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if push.Opened[0].GetAgent() == nil {
		t.Fatalf("an AgentAsyncLaunch IS an async agent, got %T", push.Opened[0].GetKind())
	}
}

func TestAWorkflowLaunchOpensAJournalBubble(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{{
		ToolUseID: "tu_1",
		Result: &datav1.ToolUseResult{Result: &datav1.ToolUseResult_WorkflowLaunch{
			WorkflowLaunch: &datav1.WorkflowLaunchResult{TaskId: "task_1", WorkflowName: "build"},
		}},
	}}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if push.Opened[0].GetJournal() == nil {
		t.Fatalf("a WorkflowLaunchResult IS a workflow run, got %T", push.Opened[0].GetKind())
	}
}

// --- shell folds -----------------------------------------------------------

func TestARetrievalAppendsOnlyTheNewBytes(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")}}, 10); err != nil {
		t.Fatal(err)
	}
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{retrieval("task_1", "abc", datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING, nil)}}, 11); err != nil {
		t.Fatal(err)
	}
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{retrieval("task_1", "abcde", datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING, nil)}}, 12)
	if err != nil {
		t.Fatal(err)
	}
	if push.Updates[0].GetShell().GetText() != "de" {
		t.Fatalf("a restated retrieval appends only its new bytes, got %q", push.Updates[0].GetShell().GetText())
	}
}

func TestARunningShellIsNotSettledByItsAbsentExitCode(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")}}, 10); err != nil {
		t.Fatal(err)
	}
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{retrieval("task_1", "abc", datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING, nil)}}, 11); err != nil {
		t.Fatal(err)
	}
	if s.snapshot()[0].GetLiveness().GetLive() == nil {
		t.Fatal("an in-flight command's absent exit code is not a zero and must not settle it")
	}
}

func TestAShellSettlesOnItsExitCodeWithANonRunningStatus(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")}}, 10); err != nil {
		t.Fatal(err)
	}
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		retrieval("task_1", "abc", datav1.RawTaskStatus_RAW_TASK_STATUS_COMPLETED, exitCode(0)),
	}}, 11); err != nil {
		t.Fatal(err)
	}
	if s.snapshot()[0].GetLiveness().GetSettled().GetDone() == nil {
		t.Fatal("an exit code of 0 on a completed task is a clean exit")
	}
}

func TestARetrievalForWorkNoLaunchAnnouncedOpensNothing(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		retrieval("task_unknown", "abc", datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING, nil),
	}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if !push.empty() {
		t.Fatal("a retrieval is not evidence of a launch, and a bubble invented from one would have no originating call")
	}
}

// --- task lifecycle --------------------------------------------------------

func TestTaskStartedOpensABubbleForARecognizedKind(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL, ToolUseId: "tu_1", Description: "sleep 9",
	}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Opened) != 1 || push.Opened[0].GetShell() == nil {
		t.Fatalf("want one shell bubble, got %v", push.Opened)
	}
}

// --- the announcement-versus-unfound split ---------------------------------
//
// An announcement that names NO tool call is legitimate: the contract says
// origin_tool_use_id is "Empty only for work that no tool call spawned", and the
// harness's own background shells arrive exactly that way. What stays a fault is
// a detachment the daemon cannot classify at all.

func TestAnAnnouncementBornDetachmentOpensABubble(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	push, err := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 10)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Opened) != 1 {
		t.Fatalf("opened = %d, want 1: work no tool call spawned exists by design and must be shown, not carded", len(push.Opened))
	}
}

func TestAnAnnouncementBornDetachmentCarriesAnEmptyOrigin(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	push, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 10)

	// Assert
	if got := push.Opened[0].GetOriginToolUseId(); got != "" {
		t.Fatalf("origin_tool_use_id = %q, want empty: naming a call that never existed would attach the bubble to the wrong card", got)
	}
}

func TestAnAnnouncementBornDetachmentRaisesNoFault(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	push, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 10)

	// Assert
	if len(push.Faults) != 0 {
		t.Fatalf("faults = %d, want 0: the fault arm is for work a call spawned that the daemon could not find, not for work nothing spawned", len(push.Faults))
	}
}

func TestAnAnnouncementBornDetachmentTakesItsKindFromItsEvidence(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	push, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 10)

	// Assert
	if push.Opened[0].GetShell() == nil {
		t.Fatalf("kind arm = %T, want the shell arm the announcement's own kind names", push.Opened[0].GetKind())
	}
}

func TestAReAnnouncedAnnouncementBornDetachmentOpensNoTwin(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 10); err != nil {
		t.Fatal(err)
	}

	// Act
	push, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 20)

	// Assert
	if len(push.Opened) != 0 {
		t.Fatalf("opened = %d, want 0: the task id is the only handle such a detachment has, and a replay must land on the bubble it already opened", len(push.Opened))
	}
}

func TestAnAnnouncementBornDetachmentOfNoRecognizableKindStillFaults(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	push, err := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1"}, 10)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Faults) != 1 || len(push.Opened) != 0 {
		t.Fatalf("faults = %d opened = %d, want 1 and 0: with neither a kind nor a call to name a tool by, the work can be neither classified nor honestly reported as unclassified",
			len(push.Faults), len(push.Opened))
	}
}

func TestAnUnrecognizedToolOpensTheExplicitUnclassifiedArm(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{ToolNames: map[string]string{"tu_1": "Frobnicate"}}, 10); err != nil {
		t.Fatal(err)
	}
	push, err := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", ToolUseId: "tu_1"}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if push.Opened[0].GetUnclassified().GetToolName() != "Frobnicate" {
		t.Fatalf("an unrecognized tool is a first-class kind that NAMES the tool, got %v", push.Opened[0].GetKind())
	}
}

func TestAnUnrecognizedToolWithNoNameBecomesAFailureCard(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", ToolUseId: "tu_1"}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Faults) != 1 {
		t.Fatal("work that can be neither classified nor honestly reported as unclassified is a fault")
	}
}

func TestAFaultCardIsStableAcrossAReplay(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	first, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1"}, 10)
	second, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1"}, 10)
	if first.Faults[0].UUID != second.Faults[0].UUID {
		t.Fatal("a card whose uuid moves accumulates a twin per replay")
	}
}

func TestTaskStartedEnrichesABubbleAlreadyOpenedByItsFirstRecord(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10); err != nil {
		t.Fatal(err)
	}
	push, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_1", Description: "review the diff",
	}, 11)
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Opened) != 0 {
		t.Fatal("an out-of-order announcement must find the bubble its records already opened, not mint a twin")
	}
}

func TestTaskStartedSuppliesTheLabelABubbleOpenedWithout(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10); err != nil {
		t.Fatal(err)
	}
	if _, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_1", Description: "review the diff",
	}, 11); err != nil {
		t.Fatal(err)
	}
	if got := s.snapshot()[0].GetLabel(); got != "review the diff" {
		t.Fatalf("want the launch's own description, got %q", got)
	}
}

func TestTaskEndedSettlesTheDetachmentsBubble(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_1",
	}, 10); err != nil {
		t.Fatal(err)
	}
	push, err := s.observeTaskEnded(&corev1.TaskEnded{TaskId: "task_1", Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE}, 11)
	if err != nil {
		t.Fatal(err)
	}
	if push.Updates[0].GetLiveness().GetLiveness().GetSettled() == nil {
		t.Fatal("a finished detachment settles its bubble")
	}
}

func TestTaskEndedForATaskThatOpenedNoBubbleIsNotAFailure(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	push, err := s.observeTaskEnded(&corev1.TaskEnded{TaskId: "task_x", Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE}, 11)
	if err != nil {
		t.Fatal(err)
	}
	if !push.empty() {
		t.Fatal("a task the catalog tracks but that detached nothing is not a missing bubble")
	}
}

// --- workflow journal folding ----------------------------------------------

func journalRetrieval(taskID, text string) frontend.ToolOutcome {
	return frontend.ToolOutcome{ToolUseID: "tu_out", Result: &datav1.ToolUseResult{
		Result: &datav1.ToolUseResult_TaskOutput{TaskOutput: &datav1.TaskOutputResult{
			Task: &datav1.TaskOutputResult_LocalBash{LocalBash: &datav1.LocalBashTask{
				TaskId: taskID, Output: text, Status: datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING,
			}},
		}},
	}}
}

func openWorkflow(t *testing.T, s *asyncBubbleStore) {
	t.Helper()
	if _, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_WORKFLOW, ToolUseId: "tu_1",
	}, 10); err != nil {
		t.Fatal(err)
	}
}

func TestAWorkflowsRetrievalFoldsAsJournalRowsNotBytes(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	openWorkflow(t, s)
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a","result":"ok"}`+"\n"),
	}}, 11)
	if err != nil {
		t.Fatal(err)
	}
	if push.Updates[0].GetJournal() == nil {
		t.Fatalf("what the work IS decides how its output is modeled, got %T", push.Updates[0].GetUpdate())
	}
}

func TestAWorkflowsSecondRetrievalAppendsOnlyItsNewRows(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	openWorkflow(t, s)
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a"}`+"\n"),
	}}, 11); err != nil {
		t.Fatal(err)
	}
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a"}`+"\n"+`{"label":"b"}`+"\n"),
	}}, 12)
	if err != nil {
		t.Fatal(err)
	}
	rows := push.Updates[0].GetJournal().GetRows()
	if len(rows) != 1 || rows[0].GetLabel() != "b" {
		t.Fatalf("want only the new row b, got %v", rows)
	}
}

func TestAWorkflowsPartialTrailingRecordIsLeftForTheNextRead(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	openWorkflow(t, s)
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a"}`+"\n"+`{"label":"b`),
	}}, 11); err != nil {
		t.Fatal(err)
	}
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a"}`+"\n"+`{"label":"b"}`+"\n"),
	}}, 12)
	if err != nil {
		t.Fatal(err)
	}
	rows := push.Updates[0].GetJournal().GetRows()
	if len(rows) != 1 || rows[0].GetLabel() != "b" {
		t.Fatalf("advancing past a partial record would drop the step it describes, got %v", rows)
	}
}

func TestAWorkflowsRewoundJournalIsRefusedAsAGap(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	openWorkflow(t, s)
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a"}`+"\n"+`{"label":"b"}`+"\n"),
	}}, 11); err != nil {
		t.Fatal(err)
	}
	_, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a"}`+"\n"),
	}}, 12)
	if err == nil || !strings.Contains(err.Error(), "REWOUND") {
		t.Fatalf("a shorter restatement is a gap rather than an append, got %v", err)
	}
}

// --- snapshot --------------------------------------------------------------

func TestSnapshotServesTheSameFoldTheDeltasWereProducedFrom(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10); err != nil {
		t.Fatal(err)
	}
	if got := len(s.snapshot()[0].GetAgent().GetEmissions()); got != 1 {
		t.Fatalf("want the snapshot carrying everything folded to date, got %d emissions", got)
	}
}

func TestSnapshotListsBubblesInLaunchOrder(t *testing.T) {
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_a", "agent_a", "x")}}, 10); err != nil {
		t.Fatal(err)
	}
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_b", "agent_b", "y")}}, 11); err != nil {
		t.Fatal(err)
	}
	snap := s.snapshot()
	if len(snap) != 2 || snap[0].GetOriginToolUseId() != "tu_a" {
		t.Fatalf("want launch order, got %v", snap)
	}
}

func TestSnapshotIsEmptyForASessionWithNoDetachedWork(t *testing.T) {
	if got := newAsyncBubbleStore("/ws", nil).snapshot(); len(got) != 0 {
		t.Fatalf("want no bubbles, got %d", len(got))
	}
}

// --- retrieval reading -----------------------------------------------------

func TestRetrievalFactsReadsAKilledAgentAsKilledRatherThanDone(t *testing.T) {
	_, _, verdict := retrievalFacts(&datav1.TaskOutputResult{
		Task: &datav1.TaskOutputResult_LocalAgent{LocalAgent: &datav1.LocalAgentTask{
			TaskId: "task_1", Status: datav1.RawTaskStatus_RAW_TASK_STATUS_KILLED,
		}},
	})
	if verdict == nil || verdict.Status != corev1.TerminalStatus_TERMINAL_STATUS_KILLED {
		t.Fatalf("want killed, got %v", verdict)
	}
}

func TestRetrievalFactsLeavesALaunchedAgentUnsettled(t *testing.T) {
	_, _, verdict := retrievalFacts(&datav1.TaskOutputResult{
		Task: &datav1.TaskOutputResult_LocalAgent{LocalAgent: &datav1.LocalAgentTask{
			TaskId: "task_1", Status: datav1.RawTaskStatus_RAW_TASK_STATUS_ASYNC_LAUNCHED,
		}},
	})
	if verdict != nil {
		t.Fatal("a launched agent has not ended")
	}
}

func TestCompleteJournalPrefixStopsAtTheLastRecordBoundary(t *testing.T) {
	if got := completeJournalPrefix("a\nb"); got != 2 {
		t.Fatalf("want 2, got %d", got)
	}
}

func TestCompleteJournalPrefixConsumesNothingFromASoleFragment(t *testing.T) {
	if got := completeJournalPrefix("abc"); got != 0 {
		t.Fatalf("a fragment with no boundary is entirely unconsumed, got %d", got)
	}
}

// --- fold-engine observability ---------------------------------------------
//
// The fold engine carried NO log calls at all, so a session whose detached work
// folded and settled perfectly left exactly the same evidence as one whose
// bubbles silently stopped growing. These assert the two records that close
// that hole, and — just as importantly — their DENSITY: one per state change,
// never one per item inside a batch.

// asyncLogRecorder captures the store's records for assertion.
type asyncLogRecorder struct{ lines []string }

func (r *asyncLogRecorder) logf(format string, args ...any) {
	r.lines = append(r.lines, fmt.Sprintf(format, args...))
}

// matching returns every captured line containing needle.
func (r *asyncLogRecorder) matching(needle string) []string {
	var out []string
	for _, line := range r.lines {
		if strings.Contains(line, needle) {
			out = append(out, line)
		}
	}
	return out
}

const (
	asyncAppendRecord = "async fold append"
	asyncSettleRecord = "async bubble settled"
)

func TestFoldAppendRecordsAnAgentEmissionFold(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)

	// Act
	push, err := s.observeCuration(frontend.Curation{
		Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")},
	}, 10)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	lines := rec.matching(asyncAppendRecord)
	if len(lines) != 1 {
		t.Fatalf("one append is one record, got %d: %v", len(lines), lines)
	}
	for _, want := range []string{
		"bubble=" + push.Opened[0].GetId(),
		"kind=agent",
		"ws=/ws",
		"appended_emissions=1",
		"folded_emissions=1",
	} {
		if !strings.Contains(lines[0], want) {
			t.Errorf("append record must carry %q, got %q", want, lines[0])
		}
	}
}

func TestFoldAppendWritesOneRecordPerBatchNotPerEmission(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	fold := detachedFold("tu_1", "agent_1", "one")
	fold.Emissions = append(fold.Emissions, detachedFold("tu_1", "agent_1", "two").Emissions...)

	// Act
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{fold}}, 10); err != nil {
		t.Fatal(err)
	}

	// Assert
	lines := rec.matching(asyncAppendRecord)
	if len(lines) != 1 {
		t.Fatalf("a two-emission batch is ONE state change, got %d records: %v", len(lines), lines)
	}
	if !strings.Contains(lines[0], "appended_emissions=2") {
		t.Errorf("the record must report the whole batch, got %q", lines[0])
	}
}

func TestFoldAppendIsSilentForAnEmptyEmissionBatch(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	fold := detachedFold("tu_1", "agent_1", "hi")
	fold.Emissions = nil

	// Act
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{fold}}, 10); err != nil {
		t.Fatal(err)
	}

	// Assert
	if lines := rec.matching(asyncAppendRecord); len(lines) != 0 {
		t.Fatalf("nothing folded is not a state change, got %v", lines)
	}
}

func TestFoldAppendRecordsAShellSpoolAdvance(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	if _, err := s.observeCuration(frontend.Curation{
		Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")},
	}, 10); err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		retrieval("task_1", "hello", datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING, nil),
	}}, 11); err != nil {
		t.Fatal(err)
	}

	// Assert
	lines := rec.matching(asyncAppendRecord)
	if len(lines) != 1 {
		t.Fatalf("one spool advance is one record, got %d: %v", len(lines), lines)
	}
	for _, want := range []string{"kind=shell", "appended_bytes=5", "from_offset=0", "through_offset=5"} {
		if !strings.Contains(lines[0], want) {
			t.Errorf("spool record must carry %q, got %q", want, lines[0])
		}
	}
}

func TestFoldAppendReportsTheSpoolCursorItAdvancedFrom(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	if _, err := s.observeCuration(frontend.Curation{
		Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")},
	}, 10); err != nil {
		t.Fatal(err)
	}
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		retrieval("task_1", "hello", datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING, nil),
	}}, 11); err != nil {
		t.Fatal(err)
	}

	// Act — a restatement carrying five more bytes
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		retrieval("task_1", "helloworld", datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING, nil),
	}}, 12); err != nil {
		t.Fatal(err)
	}

	// Assert
	lines := rec.matching(asyncAppendRecord)
	if len(lines) != 2 {
		t.Fatalf("want one record per advance, got %d: %v", len(lines), lines)
	}
	for _, want := range []string{"appended_bytes=5", "from_offset=5", "through_offset=10", "restated_bytes=10"} {
		if !strings.Contains(lines[1], want) {
			t.Errorf("the second advance must carry %q, got %q", want, lines[1])
		}
	}
}

func TestFoldAppendRecordsAJournalRowFold(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	openWorkflow(t, s)

	// Act
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a"}`+"\n"+`{"label":"b"}`+"\n"),
	}}, 11); err != nil {
		t.Fatal(err)
	}

	// Assert
	lines := rec.matching(asyncAppendRecord)
	if len(lines) != 1 {
		t.Fatalf("a two-row batch is ONE state change, got %d records: %v", len(lines), lines)
	}
	for _, want := range []string{"kind=workflow", "appended_rows=2", "folded_rows=2"} {
		if !strings.Contains(lines[0], want) {
			t.Errorf("journal record must carry %q, got %q", want, lines[0])
		}
	}
}

func TestFoldAppendReportsTheJournalBytesHeldBackAsPartial(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	openWorkflow(t, s)

	// Act — a complete row plus a trailing fragment the cursor must not consume
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		journalRetrieval("task_1", `{"label":"a"}`+"\n"+`{"lab`),
	}}, 11); err != nil {
		t.Fatal(err)
	}

	// Assert
	lines := rec.matching(asyncAppendRecord)
	if len(lines) != 1 {
		t.Fatalf("want one journal record, got %d: %v", len(lines), lines)
	}
	if !strings.Contains(lines[0], "held_bytes=5") {
		t.Errorf("the unconsumed partial record must be reported, got %q", lines[0])
	}
}

func TestSettleRecordNamesTheResolvedOutcomeArm(t *testing.T) {
	tests := []struct {
		name   string
		status corev1.TerminalStatus
		want   string
	}{
		{name: "completed work settles done", status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, want: "outcome=done"},
		{name: "failed work settles error", status: corev1.TerminalStatus_TERMINAL_STATUS_ERROR, want: "outcome=error"},
		{name: "killed work settles killed", status: corev1.TerminalStatus_TERMINAL_STATUS_KILLED, want: "outcome=killed"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			rec := &asyncLogRecorder{}
			s := newAsyncBubbleStore("/ws", rec.logf)
			if _, err := s.observeCuration(frontend.Curation{
				Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")},
			}, 10); err != nil {
				t.Fatal(err)
			}

			// Act
			if _, err := s.observeTaskEnded(&corev1.TaskEnded{TaskId: "task_1", Status: tc.status}, 20); err != nil {
				t.Fatal(err)
			}

			// Assert
			lines := rec.matching(asyncSettleRecord)
			if len(lines) != 1 {
				t.Fatalf("one settlement is one record, got %d: %v", len(lines), lines)
			}
			if !strings.Contains(lines[0], tc.want) {
				t.Errorf("settle record must carry %q, got %q", tc.want, lines[0])
			}
		})
	}
}

func TestSettleRecordCarriesTheShellExitCode(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	if _, err := s.observeCuration(frontend.Curation{
		Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")},
	}, 10); err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{
		retrieval("task_1", "boom", datav1.RawTaskStatus_RAW_TASK_STATUS_FAILED, exitCode(137)),
	}}, 11); err != nil {
		t.Fatal(err)
	}

	// Assert
	lines := rec.matching(asyncSettleRecord)
	if len(lines) != 1 {
		t.Fatalf("want one settle record, got %d: %v", len(lines), lines)
	}
	if !strings.Contains(lines[0], "shell_exit=137") {
		t.Errorf("a process settlement must report its exit status, got %q", lines[0])
	}
}

func TestSettleRecordReportsNoExitForWorkThatNeverExited(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	if _, err := s.observeCuration(frontend.Curation{
		Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")},
	}, 10); err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := s.observeTaskEnded(&corev1.TaskEnded{
		TaskId: "agent_1", Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE,
	}, 20); err != nil {
		t.Fatal(err)
	}

	// Assert
	lines := rec.matching(asyncSettleRecord)
	if len(lines) != 1 {
		t.Fatalf("want one settle record, got %d: %v", len(lines), lines)
	}
	if !strings.Contains(lines[0], "shell_exit=none") {
		t.Errorf("an agent concluded rather than exited, got %q", lines[0])
	}
}

func TestARefusedSettlementWritesNoSettleRecord(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)
	if _, err := s.observeCuration(frontend.Curation{
		Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")},
	}, 10); err != nil {
		t.Fatal(err)
	}

	// Act — an unspecified terminal status resolves no outcome
	_, err := s.observeTaskEnded(&corev1.TaskEnded{
		TaskId: "task_1", Status: corev1.TerminalStatus_TERMINAL_STATUS_UNSPECIFIED,
	}, 20)

	// Assert
	if err == nil {
		t.Fatal("a settlement with no resolvable outcome must be refused")
	}
	if lines := rec.matching(asyncSettleRecord); len(lines) != 0 {
		t.Fatalf("a refused settlement must not read as settled, got %v", lines)
	}
}

func TestASettlementForATaskThatOpenedNoBubbleWritesNoRecord(t *testing.T) {
	// Arrange
	rec := &asyncLogRecorder{}
	s := newAsyncBubbleStore("/ws", rec.logf)

	// Act
	if _, err := s.observeTaskEnded(&corev1.TaskEnded{
		TaskId: "task_unknown", Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE,
	}, 20); err != nil {
		t.Fatal(err)
	}

	// Assert
	if lines := rec.matching(asyncSettleRecord); len(lines) != 0 {
		t.Fatalf("a task that detached nothing has no bubble to settle, got %v", lines)
	}
}

// --- settling the bubbles a detached-agent cancel stopped ------------------
//
// The cancel's ack is the shim's DIRECT observation that `stop_task` resolved,
// so it is the same class of evidence a TaskEnded is — arriving on the control
// plane rather than the event plane. Settling from it is what keeps the feed
// and the footer from showing live work the daemon has already stopped.

func TestCancelledTaskSettlesItsBubble(t *testing.T) {
	// Arrange: a detached agent with an open bubble.
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_1", Description: "fan out",
	}, 10); err != nil {
		t.Fatal(err)
	}

	// Act
	ups, err := s.settleCancelledTasks([]string{"task_1"}, frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED, AtMs: 20, Reason: "user cancelled",
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(ups) != 1 {
		t.Fatalf("updates = %d, want 1: a cancelled agent's bubble must not keep rendering as live work", len(ups))
	}
	if ups[0].GetLiveness().GetLiveness().GetSettled() == nil {
		t.Fatalf("update did not settle the bubble: %+v", ups[0])
	}
}

func TestCancelledTaskSettlesToTheKilledArm(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_1",
	}, 10); err != nil {
		t.Fatal(err)
	}

	// Act
	ups, err := s.settleCancelledTasks([]string{"task_1"}, frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED, AtMs: 20, Reason: "user cancelled",
	})
	if err != nil {
		t.Fatal(err)
	}

	// Assert: KILLED, not done and not error — the work did not fail, it was
	// not allowed to conclude. The mapping is SettleAsyncBubble's, unchanged.
	if ups[0].GetLiveness().GetLiveness().GetSettled().GetKilled() == nil {
		t.Fatalf("settled arm = %+v, want killed", ups[0].GetLiveness().GetLiveness().GetSettled().GetOutcome())
	}
}

func TestCancelledTasksSettleEveryNamedBubble(t *testing.T) {
	// Arrange: two agents, both stopped by one cancel.
	s := newAsyncBubbleStore("/ws", nil)
	for _, id := range []string{"task_1", "task_2"} {
		if _, err := s.observeTaskStarted(&corev1.TaskStarted{
			TaskId: id, Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_" + id,
		}, 10); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	ups, err := s.settleCancelledTasks([]string{"task_1", "task_2"}, frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED, AtMs: 20,
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(ups) != 2 {
		t.Fatalf("updates = %d, want 2: one orphaned bubble is as visible as two", len(ups))
	}
}

func TestACancelledTaskWithNoBubbleReportsNothing(t *testing.T) {
	// Arrange: the store never opened detached work for this task.
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	ups, err := s.settleCancelledTasks([]string{"task_unknown"}, frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED, AtMs: 20,
	})

	// Assert: not a missing bubble and not an error — the session may track a
	// task it never opened a bubble for.
	if err != nil {
		t.Fatalf("err = %v, want nil", err)
	}
	if len(ups) != 0 {
		t.Fatalf("updates = %d, want 0", len(ups))
	}
}

func TestALaterTaskEndedMayOverwriteACancelSettlement(t *testing.T) {
	// Arrange: a bubble already settled by the cancel's ack.
	s := newAsyncBubbleStore("/ws", nil)
	if _, err := s.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_AGENT, ToolUseId: "tu_1",
	}, 10); err != nil {
		t.Fatal(err)
	}
	if _, err := s.settleCancelledTasks([]string{"task_1"}, frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED, AtMs: 20,
	}); err != nil {
		t.Fatal(err)
	}

	// Act: the agent had in fact finished on its own in the instant before the
	// stop reached it, and the event plane says so.
	push, err := s.observeTaskEnded(&corev1.TaskEnded{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_AGENT,
		Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE,
	}, 30)

	// Assert: the event plane carries the truer verdict and is NOT suppressed.
	// Pinning the earlier, coarser answer would report a completed agent as
	// killed forever.
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Updates) != 1 {
		t.Fatalf("updates = %d, want 1: the terminal fact must still reach the client", len(push.Updates))
	}
	if push.Updates[0].GetLiveness().GetLiveness().GetSettled().GetDone() == nil {
		t.Fatalf("settled arm = %+v, want done", push.Updates[0].GetLiveness().GetLiveness().GetSettled().GetOutcome())
	}
}
