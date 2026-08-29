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
import { create } from "@bufbuild/protobuf";
import {
  CreateTaskRequestSchema,
  CreateTaskResponseSchema,
  type CreateTaskRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_task_pb";
import {
  UpdateTaskRequestSchema,
  type UpdateTaskRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_task_pb";
import { log } from "../log.js";
import type { SidebarContext } from "./context.js";
import { runVerb } from "./verbs.js";

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

  const open = document.createElement("button");
  open.type = "button";
  open.className = "sb-add";
  open.setAttribute("data-task-create", "");
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
  const submit = document.createElement("button");
  submit.type = "button";
  submit.className = "sb-form-go";
  submit.textContent = "Create";
  const send = (): void => {
    const title = input.value.trim();
    if (title === "") return;
    log("info", "creating a task from the rail", {
      operation: "sidebar.tasks.create",
      context: { length: title.length },
    });
    void runVerb(submit, {
      sc,
      rpc: "CreateTask",
      call: (client) => client.createTask(buildCreateTaskRequest(title)),
      schema: CreateTaskResponseSchema,
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
