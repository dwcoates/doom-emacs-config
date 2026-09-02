package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// TestCreateTaskAnswersTheMintedRef pins the ordinary path.
func TestCreateTaskAnswersTheMintedRef(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.createTask = wsm.Task{ID: "task-1", Title: "ship it"}

	// Act.
	resp, err := h.Client.CreateTask(context.Background(),
		connect.NewRequest(&agentreplv1.CreateTaskRequest{Title: "ship it"}))

	// Assert.
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetTask().GetId(); got != "task-1" {
		t.Fatalf("task = %q, want task-1", got)
	}
}

// TestCreateTaskMapsTheBlankTitleArm pins that a blank title is a typed refusal
// rather than a validation failure: the field is present, it just says nothing.
func TestCreateTaskMapsTheBlankTitleArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.createTaskErr = &workspace.Refusal{Arm: workspace.ArmBlankTitle, Reason: "empty"}

	// Act.
	resp, err := h.Client.CreateTask(context.Background(),
		connect.NewRequest(&agentreplv1.CreateTaskRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	if resp.Msg.GetError().GetBlankTitle() == nil {
		t.Fatalf("result = %v, want blank_title", resp.Msg.GetResult())
	}
}

// TestUpdateTaskRequiresAChangeArm pins that an unset change oneof is refused
// rather than treated as a no-op.
func TestUpdateTaskRequiresAChangeArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.UpdateTask(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateTaskRequest{
			Task: &agentreplv1.TaskRef{Id: "task-1"},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestAssignWorkspaceTaskAcceptsAnUnsetTaskAsUnassign pins that the optional
// ref's ABSENCE is the unassign spelling.
func TestAssignWorkspaceTaskAcceptsAnUnsetTaskAsUnassign(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.AssignWorkspaceTask(context.Background(),
		connect.NewRequest(&agentreplv1.AssignWorkspaceTaskRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AssignWorkspaceTask: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}
