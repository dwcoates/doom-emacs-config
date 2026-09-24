package convert

// taskact_test.go — pins taskActSettled (and the taskState/taskIDs helpers it
// calls), reached only through settled_items.go's TaskCreate/TaskUpdate
// dispatch, which no existing fixture drives.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestTaskActSettledReadsARunningTaskWithItsDagEdges(t *testing.T) {
	// Arrange: a TaskUpdate result naming a running task with both a blocks
	// and a blocked_by edge, so the DAG structure — not just the status —
	// must survive the conversion.
	call := openCall{name: "TaskUpdate", input: map[string]any{"task_id": "t1"}}
	result := map[string]any{
		"task": map[string]any{
			"id":          "t1",
			"subject":     "ship the feature",
			"status":      "in_progress",
			"active_form": "Shipping the feature",
			"blocks":      []any{"t2"},
			"blocked_by":  []any{"t3"},
		},
	}

	// Act
	got := taskActSettled(call, result, false, nil)

	// Assert
	act := got.GetTaskAct()
	if act.GetTask().GetValue() != "t1" {
		t.Fatalf("Task = %q, want t1", act.GetTask().GetValue())
	}
	if _, ok := act.GetAct().(*conversationv1.AgentTaskAct_Changed); !ok {
		t.Fatalf("Act = %T, want AgentTaskAct_Changed", act.GetAct())
	}
	running := act.GetState().GetRunning()
	if running == nil {
		t.Fatal("Status = nil, want AgentTaskState_Running")
	}
	if running.GetActiveForm() != "Shipping the feature" {
		t.Fatalf("ActiveForm = %q, want %q", running.GetActiveForm(), "Shipping the feature")
	}
	if len(act.GetState().GetBlocks()) != 1 || act.GetState().GetBlocks()[0].GetValue() != "t2" {
		t.Fatalf("Blocks = %+v, want one id t2", act.GetState().GetBlocks())
	}
	if len(act.GetState().GetBlockedBy()) != 1 || act.GetState().GetBlockedBy()[0].GetValue() != "t3" {
		t.Fatalf("BlockedBy = %+v, want one id t3", act.GetState().GetBlockedBy())
	}
}

func TestTaskActSettledMarksARejectionWithoutChangingTheTask(t *testing.T) {
	// Arrange: a failed TaskUpdate must resolve `rejected`, and describe the
	// task as it STILL stands rather than as the act would have left it.
	call := openCall{name: "TaskUpdate"}
	result := map[string]any{"task": map[string]any{"id": "t1", "status": "pending"}}
	failure := &conversationv1.AgentToolFailure{SettledAt: settledAt(1000, 0)}

	// Act
	got := taskActSettled(call, result, true, failure)

	// Assert
	rejected := got.GetTaskAct().GetRejected()
	if rejected == nil {
		t.Fatal("Act = nil, want AgentTaskAct_Rejected")
	}
	if rejected.GetError() != failure {
		t.Fatal("Rejected did not carry the failure it was handed")
	}
	if got.GetTaskAct().GetState().GetPending() == nil {
		t.Fatal("State = not pending, want the task's own still-pending status preserved")
	}
}

func TestTaskActSettledNeverResolvesTheStatusARejectedActAskedFor(t *testing.T) {
	// Arrange: the board REFUSED `TaskUpdate(9, completed)` and echoed no task
	// of its own -- which is what a refusal for an id the tracker does not hold
	// looks like. The status the call asked for is not the standing one.
	call := openCall{name: "TaskUpdate", input: map[string]any{"taskId": "9", "status": "completed"}}
	failure := &conversationv1.AgentToolFailure{SettledAt: settledAt(1000, 0)}

	// Act
	got := taskActSettled(call, map[string]any{"taskId": "9"}, true, failure)

	// Assert: no status at all, and above all not the one that was refused.
	if got.GetTaskAct().GetState().GetStatus() != nil {
		t.Fatalf("State.Status = %+v, want it UNSET: a refusal states nothing about where the task stands",
			got.GetTaskAct().GetState().GetStatus())
	}
}

func TestTaskActSettledLeavesAnUnstatedUpdateStatusUnset(t *testing.T) {
	// Arrange: an update that moved only an edge. `AgentTaskState.status` is a
	// oneof so "this act said nothing about where the task stands" is
	// representable; resolving `pending` invents one.
	call := openCall{name: "TaskUpdate", input: map[string]any{"taskId": "t2", "addBlockedBy": []any{"t1"}}}

	// Act
	got := taskActSettled(call, map[string]any{"taskId": "t2"}, false, nil)

	// Assert
	if got.GetTaskAct().GetState().GetStatus() != nil {
		t.Fatalf("State.Status = %+v, want it UNSET", got.GetTaskAct().GetState().GetStatus())
	}
}

func TestTaskActSettledStillLeavesACreatePending(t *testing.T) {
	// Arrange: the create tool takes no status, and a new entry IS recorded
	// and not begun -- the one place the default is the right answer.
	call := openCall{name: "TaskCreate", input: map[string]any{"subject": "Land the converter"}}

	// Act
	got := taskActSettled(call, map[string]any{"task": map[string]any{"id": "1"}}, false, nil)

	// Assert
	if got.GetTaskAct().GetState().GetPending() == nil {
		t.Fatalf("State.Status = %+v, want pending", got.GetTaskAct().GetState().GetStatus())
	}
}

// A RECORD THAT NAMES NO SUBJECT STATES NONE. `AgentTaskState.subject` carries
// presence, so an update that moved only a status leaves it UNSET and a
// checklist keeps the subject it already holds.
func TestTaskActSettledLeavesAnUnnamedSubjectUnset(t *testing.T) {
	// Arrange
	call := openCall{name: "TaskUpdate", input: map[string]any{"task_id": "t1", "status": "completed"}}
	result := map[string]any{"task": map[string]any{"id": "t1", "status": "completed"}}

	// Act
	got := taskActSettled(call, result, false, nil)

	// Assert
	if subject := got.GetTaskAct().GetState().Subject; subject != nil {
		t.Fatalf("State.Subject = %q, want it UNSET", *subject)
	}
}

// AND A SUBJECT STATED EMPTY IS STATED. The record carries the key, so what it
// says is what the act says, empty or not.
func TestTaskActSettledStatesASubjectTheRecordNamesEmpty(t *testing.T) {
	// Arrange
	call := openCall{name: "TaskUpdate", input: map[string]any{"task_id": "t1"}}
	result := map[string]any{"task": map[string]any{"id": "t1", "subject": ""}}

	// Act
	got := taskActSettled(call, result, false, nil)

	// Assert
	subject := got.GetTaskAct().GetState().Subject
	if subject == nil || *subject != "" {
		t.Fatalf("State.Subject = %v, want the empty subject the record states", subject)
	}
}

// THE DESCRIPTION IS THE SAME FIELD'S RULE, and it is asserted on its own
// because a producer that fixed one and not the other still blanks a row.
func TestTaskActSettledLeavesAnUnnamedDescriptionUnset(t *testing.T) {
	// Arrange
	call := openCall{name: "TaskUpdate", input: map[string]any{"task_id": "t1", "status": "completed"}}
	result := map[string]any{"task": map[string]any{"id": "t1", "subject": "ship it"}}

	// Act
	got := taskActSettled(call, result, false, nil)

	// Assert
	if description := got.GetTaskAct().GetState().Description; description != nil {
		t.Fatalf("State.Description = %q, want it UNSET", *description)
	}
}

// THE TRACKER'S ECHO STILL WINS over the call's own input when both state one:
// what the task IS is the tracker's business, and presence must not reorder it.
func TestTaskActSettledPrefersTheTrackersEchoedSubject(t *testing.T) {
	// Arrange
	call := openCall{name: "TaskUpdate", input: map[string]any{"task_id": "t1", "subject": "what I asked for"}}
	result := map[string]any{"task": map[string]any{"id": "t1", "subject": "what it is"}}

	// Act
	got := taskActSettled(call, result, false, nil)

	// Assert
	if subject := got.GetTaskAct().GetState().Subject; subject == nil || *subject != "what it is" {
		t.Fatalf("State.Subject = %v, want the tracker's own echo", subject)
	}
}
