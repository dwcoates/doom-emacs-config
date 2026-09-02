package convert

// taskact.go — THE TASK TRACKER. A task is inherently bound to the agent working
// it, so what travels is the ACT the agent performed, on whatever stream that
// agent is running on. `state` is where the act LEFT the task, resolved by the
// producer, so no consumer replays a sequence of acts to learn what a task is.

import conversationv1 "agentrepl/proto/conversation/v1"

// taskActSettled converts a TaskCreate/TaskUpdate result into the act.
func taskActSettled(call openCall, result map[string]any, failed bool, failure *conversationv1.AgentToolFailure) *conversationv1.AgentActivity {
	task := obj(pick(result, "task"))
	if task == nil {
		task = map[string]any{}
	}
	id := firstNonEmpty(
		str(pick(task, "id", "taskId", "task_id")),
		str(pick(result, "taskId", "task_id")),
		str(pick(call.input, "task_id", "taskId")),
	)

	act := &conversationv1.AgentTaskAct{
		Task:  &conversationv1.AgentTaskId{Value: id},
		State: taskState(task, call.input),
	}
	switch {
	case failed:
		// THE TRACKER REFUSED. Nothing was added and nothing changed, so `state`
		// describes the task as it STILL STANDS rather than as the act would
		// have left it.
		act.Act = &conversationv1.AgentTaskAct_Rejected{Rejected: &conversationv1.AgentTaskRejected{Error: failure}}
	case call.name == "TaskCreate":
		act.Act = &conversationv1.AgentTaskAct_Created{Created: &conversationv1.AgentTaskCreated{}}
	default:
		act.Act = &conversationv1.AgentTaskAct_Changed{Changed: &conversationv1.AgentTaskChanged{}}
	}
	return item(&conversationv1.AgentActivity_TaskAct{TaskAct: act})
}

// taskState reads a task as it stands after an act.
//
// THE TRACKER IS A DAG, NOT A LIST: the blocks/blocked_by edges are the ordering
// the author actually stated, and a surface drawing tracker order without them
// is hiding it.
func taskState(task, input map[string]any) *conversationv1.AgentTaskState {
	state := &conversationv1.AgentTaskState{
		Subject:     firstNonEmpty(str(pick(task, "subject", "title")), str(pick(input, "subject", "title"))),
		Description: firstNonEmpty(str(task["description"]), str(input["description"])),
		Owner:       optionalString(pick(task, "owner", "assignee")),
		Blocks:      taskIDs(pick(task, "blocks")),
		BlockedBy:   taskIDs(pick(task, "blocked_by", "blockedBy")),
	}
	switch firstNonEmpty(str(task["status"]), str(input["status"])) {
	case "in_progress", "running", "active":
		state.Status = &conversationv1.AgentTaskState_Running{Running: &conversationv1.AgentTaskRunning{
			ActiveForm: optionalString(pick(task, "active_form", "activeForm")),
		}}
	case "completed", "done":
		state.Status = &conversationv1.AgentTaskState_Completed{Completed: &conversationv1.AgentTaskCompleted{}}
	case "deleted", "removed", "cancelled":
		state.Status = &conversationv1.AgentTaskState_Deleted{Deleted: &conversationv1.AgentTaskDeleted{}}
	default:
		state.Status = &conversationv1.AgentTaskState_Pending{Pending: &conversationv1.AgentTaskPending{}}
	}
	return state
}

func taskIDs(raw any) []*conversationv1.AgentTaskId {
	var ids []*conversationv1.AgentTaskId
	for _, el := range list(raw) {
		switch value := el.(type) {
		case string:
			ids = append(ids, &conversationv1.AgentTaskId{Value: value})
		case map[string]any:
			if id := str(pick(value, "id", "taskId", "task_id")); id != "" {
				ids = append(ids, &conversationv1.AgentTaskId{Value: id})
			}
		}
	}
	return ids
}
