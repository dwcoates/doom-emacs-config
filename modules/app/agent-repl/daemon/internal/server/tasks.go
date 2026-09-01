package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The task verbs. Tasks are daemon-global — they belong to no workspace — so
// only AssignWorkspaceTask carries an ownership refusal.

// CreateTask records a new task. A BLANK title is a refusal arm, not a
// validation failure: the field is present, it just says nothing.
func (s *server) CreateTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.CreateTaskRequest],
) (*connect.Response[agentreplv1.CreateTaskResponse], error) {
	const rpc = "CreateTask"
	if err := validateCreateTaskRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.CreateTaskResponse{}
	task, err := s.deps.Verbs.CreateTask(ctx, req.Msg.GetTitle())
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Debug("daemon.server.create_task", "recorded a task",
		dlog.Context{"task": string(task.ID)})
	resp.Result = &agentreplv1.CreateTaskResponse_Success{
		Success: &agentreplv1.CreateTaskSuccess{
			Task: &agentreplv1.TaskRef{Id: string(task.ID)},
		},
	}
	return connect.NewResponse(resp), nil
}

// UpdateTask retitles, completes or reopens one task.
func (s *server) UpdateTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateTaskRequest],
) (*connect.Response[agentreplv1.UpdateTaskResponse], error) {
	const rpc = "UpdateTask"
	if err := validateUpdateTaskRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateTaskResponse{}
	change := wsm.TaskChange{}
	switch {
	case req.Msg.GetSetTitle() != nil:
		title := req.Msg.GetSetTitle().GetTitle()
		change.Title = &title
	case req.Msg.GetSetDone() != nil:
		done := true
		change.Done = &done
	default:
		open := false
		change.Done = &open
	}
	if err := s.deps.Verbs.UpdateTask(ctx, ids.TaskID(req.Msg.GetTask().GetId()), change); err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.UpdateTaskResponse_Success{
		Success: &agentreplv1.UpdateTaskSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// AssignWorkspaceTask assigns a workspace to a task, or UNASSIGNS it when the
// optional ref is absent.
func (s *server) AssignWorkspaceTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.AssignWorkspaceTaskRequest],
) (*connect.Response[agentreplv1.AssignWorkspaceTaskResponse], error) {
	const rpc = "AssignWorkspaceTask"
	if err := validateAssignWorkspaceTaskRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.AssignWorkspaceTaskResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	var task *ids.TaskID
	if req.Msg.Task != nil {
		id := ids.TaskID(req.Msg.GetTask().GetId())
		task = &id
	}
	if err := s.deps.Verbs.AssignTask(ctx, subject.Record.ID, task); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.AssignWorkspaceTaskResponse_Success{
		Success: &agentreplv1.AssignWorkspaceTaskSuccess{},
	}
	return connect.NewResponse(resp), nil
}
