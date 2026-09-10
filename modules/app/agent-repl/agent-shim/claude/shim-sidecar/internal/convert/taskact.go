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
		State: taskState(task, call.input, failed, call.name == "TaskCreate"),
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
//
// THE STATUS IS THE TRACKER'S TO STATE, NEVER THE CALLER'S ASK, and this read
// it the other way round twice over.
//
//   - A REFUSED act took the status out of the CALL'S OWN INPUT, so a
//     `TaskUpdate(9, completed)` the board answered `success:false` resolved
//     `completed` -- the exact claim the `rejected` arm three lines above
//     refuses to make, drawn as a TICKED checklist row in the running
//     application. Observed in the G50-52 playbook: this file-plane act is
//     re-delivered after the stream-plane refusal and wins. A rejection now
//     reads only what the tracker itself echoed.
//
//   - A SUBJECT NOBODY STATED IS NOT A SUBJECT STATED EMPTY. Both fields carry
//     presence, so a record that names neither leaves them UNSET and a
//     consumer keeps what its checklist already holds; a record that states an
//     empty one states it. Reading the empty string as the value made a
//     status-only update blank every row it touched (G52).
//
//   - AN UNSTATED STATUS RESOLVED `pending`, inventing a status nobody stated:
//     an update that moved only a subject or an edge said nothing about where
//     the task stands, and `AgentTaskState.status` is a oneof precisely so
//     that is representable. It is left UNSET now, exactly as the shim's own
//     TypeScript converter leaves it, and the footer keeps the task where it
//     stands. A CREATE is the one exception and keeps the default, because
//     the create tool takes no status and a new entry IS "recorded and not
//     begun".
func taskState(task, input map[string]any, failed, isCreate bool) *conversationv1.AgentTaskState {
	state := &conversationv1.AgentTaskState{
		Subject:     statedString([]map[string]any{task, input}, "subject", "title"),
		Description: statedString([]map[string]any{task, input}, "description"),
		Owner:       optionalString(pick(task, "owner", "assignee")),
		Blocks:      taskIDs(pick(task, "blocks")),
		BlockedBy:   taskIDs(pick(task, "blocked_by", "blockedBy")),
	}
	named := str(task["status"])
	if !failed {
		named = firstNonEmpty(named, str(input["status"]))
	}
	switch named {
	case "in_progress", "running", "active":
		state.Status = &conversationv1.AgentTaskState_Running{Running: &conversationv1.AgentTaskRunning{
			ActiveForm: optionalString(pick(task, "active_form", "activeForm")),
		}}
	case "completed", "done":
		state.Status = &conversationv1.AgentTaskState_Completed{Completed: &conversationv1.AgentTaskCompleted{}}
	case "deleted", "removed", "cancelled":
		state.Status = &conversationv1.AgentTaskState_Deleted{Deleted: &conversationv1.AgentTaskDeleted{}}
	case "pending":
		state.Status = &conversationv1.AgentTaskState_Pending{Pending: &conversationv1.AgentTaskPending{}}
	default:
		if isCreate {
			state.Status = &conversationv1.AgentTaskState_Pending{Pending: &conversationv1.AgentTaskPending{}}
		}
	}
	return state
}

// statedString is the first STATED spelling of a field across the records that
// could carry it, as a presence-carrying pointer.
//
// PRESENCE IS THE KEY'S, NOT THE VALUE'S: a record that carries the key states
// the field even when what it states is empty, and only a record set that
// carries none of the spellings leaves it unset. A non-empty statement still
// wins over an empty one, which is what keeps the tracker's own echo preferred
// over the call's input.
func statedString(objects []map[string]any, keys ...string) *string {
	var blank *string
	for _, o := range objects {
		for _, key := range keys {
			if !has(o, key) {
				continue
			}
			if v := str(pick(o, key)); v != "" {
				return &v
			}
			if blank == nil {
				empty := ""
				blank = &empty
			}
		}
	}
	return blank
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
