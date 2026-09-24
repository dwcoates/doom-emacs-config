/**
 * convert/tools/task-act.ts — one act on the task tracker.
 *
 * # An act has no lifecycle
 *
 * `AgentTaskAct` declares no start/terminal oneof: it is INSTANTANEOUS — a
 * created, changed or rejected arm plus the state the act left the task in. The
 * `ToolConverter` contract nevertheless has a `start` and a `settle`, and both
 * upsert the SAME unit by the call's identity.
 *
 * So both produce the act, and the second replaces the first: `start` produces
 * the act the INPUT implies (which is all that is knowable at announcement), and
 * `settle` produces it resolved against the tracker's answer. One row either
 * way, and a call whose result never arrives still leaves the act it asked for
 * on the record rather than nothing at all.
 *
 * # Except for a create, which has no identity yet
 *
 * `TaskCreateInput` carries a subject and a description and no id — the tracker
 * mints it and states it on the result. `AgentTaskAct.task` is not optional, so
 * a create's announcement produces NO frame; its settle produces the whole act.
 *
 * # What a rejection may say about the task
 *
 * The proto is explicit that a rejected act's `state` describes the task AS IT
 * STILL STANDS. The tracker states nothing about that — it answers
 * `{success:false, error}` — so a rejection carries the subject and description
 * the caller named and leaves `status` UNSET rather than asserting the status
 * the refused act asked for.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { arr, asRecord, bool, failureOf, obj, str } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-task-act", operation: "shim.convert.task_act" });

/** The vendor tool name that CREATES a task; every other name in this unit changes one. */
const CREATE_TOOL = "TaskCreate";

/** The tracker's identity for one task. Opaque — compared, never parsed. */
function taskId(value: string): conversationv1.AgentTaskId {
  return create(conversationv1.AgentTaskIdSchema, { value });
}

/** A list of task identities from a vendor string array. */
function taskIds(record: Record<string, unknown> | undefined, key: string): conversationv1.AgentTaskId[] {
  const values = arr(record, key);
  if (values === undefined) return [];
  return values
    .filter((value): value is string => typeof value === "string" && value !== "")
    .map(taskId);
}

/** Where an act left a task, from the status the producer resolved. */
function statusOf(
  status: string | undefined,
  activeForm: string | undefined,
): conversationv1.AgentTaskState["status"] {
  switch (status) {
    case "pending":
      return { case: "pending", value: create(conversationv1.AgentTaskPendingSchema, {}) };
    case "in_progress":
      return {
        case: "running",
        value: create(conversationv1.AgentTaskRunningSchema, { activeForm }),
      };
    case "completed":
      return { case: "completed", value: create(conversationv1.AgentTaskCompletedSchema, {}) };
    case "deleted":
      return { case: "deleted", value: create(conversationv1.AgentTaskDeletedSchema, {}) };
    case undefined:
      // UNSET, not a default: an update that changed only a subject says
      // nothing about where the task stands, and `pending` would invent it.
      return { case: undefined };
    default:
      LOGGER.debug(
        { status },
        "the tracker named a status this contract has no arm for; the status is left unset",
      );
      return { case: undefined };
  }
}

/**
 * The task as this act leaves it.
 *
 * A SUBJECT THE ACT DID NOT NAME IS LEFT UNSET, never stated as "". Both
 * fields carry presence, so a `TaskUpdate(status)` -- which names no subject at
 * all -- says nothing about the subject and a consumer keeps the one its
 * checklist already holds. Stating "" here said the caller had BLANKED it, and
 * the checklist drew every row as a bare glyph with no words beside it (G52).
 */
function stateOf(
  call: PendingCall,
  status: conversationv1.AgentTaskState["status"],
): conversationv1.AgentTaskState {
  const input = call.input;
  return create(conversationv1.AgentTaskStateSchema, {
    subject: str(input, "subject"),
    description: str(input, "description"),
    owner: str(input, "owner"),
    status,
    blocks: taskIds(input, "addBlocks"),
    blockedBy: taskIds(input, "addBlockedBy"),
  });
}

/** One task act as an activity item. */
function taskItem(
  task: conversationv1.AgentTaskId,
  act: conversationv1.AgentTaskAct["act"],
  state: conversationv1.AgentTaskState,
): conversationv1.AgentActivity["item"] {
  return {
    case: "taskAct",
    value: create(conversationv1.AgentTaskActSchema, { task, act, state }),
  };
}

/** The `TaskCreate` and `TaskUpdate` pair: one instantaneous act each. */
export const taskActConverter: ToolConverter = {
  kind: "task_act",
  // `AgentTaskAct` declares no arms at all beyond the act itself, so there is
  // nowhere for a liveness beat to land.
  carriesProgress: false,

  start(call) {
    if (call.toolName === CREATE_TOOL) {
      // NO FRAME: the tracker has not minted the task's identity yet, and
      // `AgentTaskAct.task` is not optional.
      LOGGER.logVerbose(
        { tool_use_id: call.toolUseId },
        "a task create has no tracker identity until it returns; no frame is produced at announcement",
      );
      return undefined;
    }
    const id = str(call.input, "taskId");
    if (id === undefined || id === "") {
      LOGGER.debug(
        { tool_use_id: call.toolUseId, tool: call.toolName },
        "a task update names no task; no frame is produced",
      );
      return undefined;
    }
    return taskItem(
      taskId(id),
      { case: "changed", value: create(conversationv1.AgentTaskChangedSchema, {}) },
      // THE ANNOUNCEMENT LEAVES THE TASK WHERE IT STANDS, so it states no
      // status at all. `AgentTaskAct.state` is "where the act LEFT the task,
      // resolved by the producer", and at announcement the act has left it
      // nowhere: the tracker has not answered, and the status the call ASKED
      // FOR is not the standing one -- which is the very claim the `rejected`
      // arm below already refuses to make.
      //
      // It was stated here, and it won: a `TaskUpdate(9, completed)` the
      // tracker REFUSED drew a ticked checklist row, because this optimistic
      // announcement is re-delivered after the refusal and overwrote it.
      // Observed in the G52 playbook, three frames deep.
      stateOf(call, { case: undefined }),
    );
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    const structured = asRecord(outcome.structured);
    return call.toolName === CREATE_TOOL
      ? settleCreate(call, structured, outcome)
      : settleUpdate(call, structured, outcome);
  },
};

/** A create: the tracker mints the identity and echoes the subject. */
function settleCreate(
  call: PendingCall,
  structured: Record<string, unknown> | undefined,
  outcome: ToolOutcome,
): conversationv1.AgentActivity["item"] | undefined {
  const id = str(obj(structured, "task"), "id");
  if (id === undefined || id === "") {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a task create returned no tracker identity; no frame is produced",
    );
    return undefined;
  }
  const state = create(conversationv1.AgentTaskStateSchema, {
    // The tracker's own echo wins over the input: it is what the task IS.
    // UNSET when neither named one, which is not the same as a subject of "".
    subject: str(obj(structured, "task"), "subject") ?? str(call.input, "subject"),
    description: str(call.input, "description"),
    owner: str(call.input, "owner"),
    // A CREATE'S STATE IS `pending` UNLESS STATED: the create tool takes no
    // status, so a new entry is recorded and not begun.
    status: statusOf(str(call.input, "status") ?? "pending", str(call.input, "activeForm")),
    blocks: taskIds(call.input, "addBlocks"),
    blockedBy: taskIds(call.input, "addBlockedBy"),
  });
  if (outcome.isError) {
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the tracker refused a task create");
    return taskItem(
      taskId(id),
      {
        case: "rejected",
        value: create(conversationv1.AgentTaskRejectedSchema, { error: failureOf(call, outcome) }),
      },
      state,
    );
  }
  return taskItem(
    taskId(id),
    { case: "created", value: create(conversationv1.AgentTaskCreatedSchema, {}) },
    state,
  );
}

/** An update: the tracker states whether it took, and where the status moved. */
function settleUpdate(
  call: PendingCall,
  structured: Record<string, unknown> | undefined,
  outcome: ToolOutcome,
): conversationv1.AgentActivity["item"] | undefined {
  const id = str(structured, "taskId") ?? str(call.input, "taskId");
  if (id === undefined || id === "") {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a task update names no task; no frame is produced",
    );
    return undefined;
  }
  const refused =
    outcome.isError || bool(structured, "success") === false || str(structured, "error") !== undefined;
  if (refused) {
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the tracker refused a task update");
    // The status is left UNSET: the tracker states nothing about where the task
    // still stands, and the status the refused act ASKED FOR is not it.
    return taskItem(
      taskId(id),
      {
        case: "rejected",
        value: create(conversationv1.AgentTaskRejectedSchema, { error: failureOf(call, outcome) }),
      },
      stateOf(call, { case: undefined }),
    );
  }
  const status = str(obj(structured, "statusChange"), "to") ?? str(call.input, "status");
  return taskItem(
    taskId(id),
    { case: "changed", value: create(conversationv1.AgentTaskChangedSchema, {}) },
    stateOf(call, statusOf(status, str(call.input, "activeForm"))),
  );
}
