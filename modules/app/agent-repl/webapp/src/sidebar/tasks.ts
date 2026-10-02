/**
 * tasks — the two task verbs the rail calls, and the new-task control.
 *
 * A TASK IS DAEMON-OWNED. The rail creates one and changes one; it never keeps
 * a list of its own — the task view's sections ARE the list, resolved and
 * pushed like everything else. `TaskRef.id` is an opaque echo token: handed
 * back verbatim, never parsed, never constructed.
 *
 * THE ARM IS THE CHANGE on `UpdateTask`, so the three changes are three
 * distinct request shapes rather than one request with a mode field, and the
 * builder below spells each of them out.
 */
import { createControl } from "../control.js";
import { create } from "@bufbuild/protobuf";
import {
  CreateTaskRequestSchema,
  CreateTaskResponseSchema,
  type CreateTaskRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_task_pb";
import {
  UpdateTaskRequestSchema,
  type UpdateTaskError,
  type UpdateTaskRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_task_pb";
import type { CreateTaskError } from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_task_pb";
import { log } from "../log.js";
import { unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import { fireVerb } from "./verbs.js";

/** The three changes `UpdateTask` accepts, as this end names them. */
export type TaskChange =
  | { case: "setTitle"; title: string }
  | { case: "setDone" }
  | { case: "setOpen" };

/** CreateTask: a new user task. The title is non-blank by the contract. */
export function buildCreateTaskRequest(title: string): CreateTaskRequest {
  return create(CreateTaskRequestSchema, { title });
}

/** UpdateTask: one task, one change, both echoed. */
export function buildUpdateTaskRequest(taskId: string, change: TaskChange): UpdateTaskRequest {
  switch (change.case) {
    case "setTitle":
      return create(UpdateTaskRequestSchema, {
        task: { id: taskId },
        change: { case: "setTitle", value: { title: change.title } },
      });
    case "setDone":
      return create(UpdateTaskRequestSchema, {
        task: { id: taskId },
        change: { case: "setDone", value: {} },
      });
    case "setOpen":
      return create(UpdateTaskRequestSchema, {
        task: { id: taskId },
        change: { case: "setOpen", value: {} },
      });
  }
}

/**
 * The cause union of a task error, narrowed to a SET arm.
 *
 * Neither task verb is addressed to a workspace, so NONE of the cross-cutting
 * four can reach them: every arm here is the endpoint's own, and this file
 * words all of them.
 */
type TaskCause<E extends { cause: { case?: string | undefined } }> = NonNullable<E["cause"]> & {
  case: string;
};

/**
 * CreateTask's one arm.
 *
 * The rail refuses to SEND a blank title, so this answers a race the form
 * cannot win rather than the ordinary path — which is exactly why it is drawn
 * rather than assumed away.
 */
export function createTaskRefusal(cause: TaskCause<CreateTaskError>): string {
  switch (cause.case) {
    case "blankTitle":
      return "a task needs a title";
    default: {
      // The union is exhausted, so a newer daemon's arm arrives here as the
      // widened shape rather than as a case this build can name.
      const other: { case: string } = cause;
      return unreachableArm("CreateTaskError.cause", other.case);
    }
  }
}

/** UpdateTask's three arms. */
export function updateTaskRefusal(cause: TaskCause<UpdateTaskError>): string {
  switch (cause.case) {
    case "blankTitle":
      return "a task needs a title";
    case "noChange":
      return "that change would leave the task exactly as it is";
    case "unknownTask":
      return "the daemon does not know that task";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("UpdateTaskError.cause", other.case);
    }
  }
}

/**
 * The new-task control at the head of the task grouping.
 *
 * It opens a one-field form rather than a browser prompt: the rail owns its
 * own chrome, and a native dialog would be the one place in this app that
 * looks like something else. A blank title is not sent — the contract says the
 * title is non-blank, so the refusal is avoided rather than provoked.
 */
export function drawCreateTaskControl(sc: SidebarContext): HTMLElement {
  const host = document.createElement("div");
  host.className = "sb-task-create";

  // The DISCLOSURE carries no hook: `[data-task-create]` is the control that
  // actually issues CreateTask, which is the submit below.
  const open = createControl();
  open.className = "sb-add";
  open.textContent = "+ new task";
  host.appendChild(open);

  const form = document.createElement("div");
  form.className = "sb-task-form";
  form.hidden = true;
  const input = document.createElement("input");
  input.type = "text";
  input.className = "sb-task-title";
  input.setAttribute("data-task-title", "");
  input.placeholder = "task title";
  const submit = createControl();
  submit.className = "sb-form-go";
  submit.setAttribute("data-task-create", "");
  submit.textContent = "Create";
  const send = (): void => {
    const title = input.value.trim();
    if (title === "") return;
    log.info("creating a task from the rail", {
      operation: "sidebar.tasks.create",
      context: { length: title.length },
    });
    void fireVerb(submit, {
      sc,
      rpc: "CreateTask",
      call: (client) => client.createTask(buildCreateTaskRequest(title)),
      schema: CreateTaskResponseSchema,
      refusalText: (cause) => createTaskRefusal(cause as TaskCause<CreateTaskError>),
    }).then((ok) => {
      // The section arrives on the roster push; the form only has to get out
      // of the way, and only when the daemon actually took the task.
      if (!ok) return;
      input.value = "";
      form.hidden = true;
    });
  };
  submit.addEventListener("click", (event) => {
    event.preventDefault();
    send();
  });
  input.addEventListener("keydown", (event) => {
    if (event.key === "Enter") send();
  });
  form.append(input, submit);
  host.appendChild(form);

  open.addEventListener("click", (event) => {
    event.preventDefault();
    form.hidden = !form.hidden;
    if (!form.hidden) input.focus();
  });
  return host;
}
