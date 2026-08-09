package sessioncontroller

import (
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
	s := newAsyncBubbleStore("/ws")
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Opened) != 1 {
		t.Fatalf("the first record of a detached conversation opens its bubble, got %d", len(push.Opened))
	}
}

func TestObserveCurationFoldsASecondRecordIntoTheSameBubble(t *testing.T) {
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if push.Updates[0].GetBubbleId() != push.Opened[0].GetId() {
		t.Fatal("an update must name the bubble the same push opened")
	}
}

func TestObserveCurationRefusesADetachedRecordItCannotAttributeToACall(t *testing.T) {
	s := newAsyncBubbleStore("/ws")
	_, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("", "agent_1", "hi")}}, 10)
	if err == nil {
		t.Fatal("a record naming neither a source call nor an open bubble has nothing to attribute the detachment to")
	}
}

func TestObserveCurationLabelsABubbleFromTheToolThatLaunchedIt(t *testing.T) {
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if s.spawnedBubbleID("tu_1") != push.Opened[0].GetId() {
		t.Fatal("the launching call must resolve to the bubble it launched")
	}
}

func TestSpawnedBubbleIDIsEmptyForACallThatDetachedNothing(t *testing.T) {
	if got := newAsyncBubbleStore("/ws").spawnedBubbleID("tu_other"); got != "" {
		t.Fatalf("empty is the only reading of a call that detached nothing, got %q", got)
	}
}

// --- nested dispatch -------------------------------------------------------

func TestANestedDispatchPointsAtTheBubbleItWasLaunchedFrom(t *testing.T) {
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
	push, err := s.observeCuration(frontend.Curation{Outcomes: []frontend.ToolOutcome{bashOutcome("tu_1", "task_1")}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if push.Opened[0].GetShell() == nil {
		t.Fatalf("a BashResult carrying a background task id IS a background launch, got %T", push.Opened[0].GetKind())
	}
}

func TestAForegroundShellDetachesNothing(t *testing.T) {
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")

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
	s := newAsyncBubbleStore("/ws")

	// Act
	push, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 10)

	// Assert
	if got := push.Opened[0].GetOriginToolUseId(); got != "" {
		t.Fatalf("origin_tool_use_id = %q, want empty: naming a call that never existed would attach the bubble to the wrong card", got)
	}
}

func TestAnAnnouncementBornDetachmentRaisesNoFault(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws")

	// Act
	push, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 10)

	// Assert
	if len(push.Faults) != 0 {
		t.Fatalf("faults = %d, want 0: the fault arm is for work a call spawned that the daemon could not find, not for work nothing spawned", len(push.Faults))
	}
}

func TestAnAnnouncementBornDetachmentTakesItsKindFromItsEvidence(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws")

	// Act
	push, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL}, 10)

	// Assert
	if push.Opened[0].GetShell() == nil {
		t.Fatalf("kind arm = %T, want the shell arm the announcement's own kind names", push.Opened[0].GetKind())
	}
}

func TestAReAnnouncedAnnouncementBornDetachmentOpensNoTwin(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")

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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
	push, err := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1", ToolUseId: "tu_1"}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Faults) != 1 {
		t.Fatal("work that can be neither classified nor honestly reported as unclassified is a fault")
	}
}

func TestAFaultCardIsStableAcrossAReplay(t *testing.T) {
	s := newAsyncBubbleStore("/ws")
	first, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1"}, 10)
	second, _ := s.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_1"}, 10)
	if first.Faults[0].UUID != second.Faults[0].UUID {
		t.Fatal("a card whose uuid moves accumulates a twin per replay")
	}
}

func TestTaskStartedEnrichesABubbleAlreadyOpenedByItsFirstRecord(t *testing.T) {
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
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
	s := newAsyncBubbleStore("/ws")
	if _, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_1", "agent_1", "hi")}}, 10); err != nil {
		t.Fatal(err)
	}
	if got := len(s.snapshot()[0].GetAgent().GetEmissions()); got != 1 {
		t.Fatalf("want the snapshot carrying everything folded to date, got %d emissions", got)
	}
}

func TestSnapshotListsBubblesInLaunchOrder(t *testing.T) {
	s := newAsyncBubbleStore("/ws")
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
	if got := newAsyncBubbleStore("/ws").snapshot(); len(got) != 0 {
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
