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
	failure := &conversationv1.AgentToolFailure{SettledAt: settledAt(1000)}

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
