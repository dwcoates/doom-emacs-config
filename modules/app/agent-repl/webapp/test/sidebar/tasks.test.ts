// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { CreateTaskResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_task_pb";
import {
  buildCreateTaskRequest,
  buildUpdateTaskRequest,
  drawCreateTaskControl,
} from "../../src/sidebar/tasks.js";
import { appContext, sidebarContext } from "./harness.js";

/** Click and let the verb's promise chain drain. */
async function click(control: Element): Promise<void> {
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

describe("CreateTask", () => {
  it("carries the title the user typed", () => {
    expect(buildCreateTaskRequest("ship the rail").title).toBe("ship the rail");
  });
});

describe("UpdateTask", () => {
  it("echoes the task's id rather than its title", () => {
    expect(buildUpdateTaskRequest("task-3", { case: "setDone" }).task?.id).toBe("task-3");
  });

  it("retitles through the set_title arm", () => {
    const request = buildUpdateTaskRequest("task-3", { case: "setTitle", title: "renamed" });
    expect(request.change.case).toBe("setTitle");
  });

  it("carries the new title on that arm", () => {
    const request = buildUpdateTaskRequest("task-3", { case: "setTitle", title: "renamed" });
    expect(request.change.case === "setTitle" ? request.change.value.title : null).toBe("renamed");
  });

  it("completes through the set_done arm", () => {
    expect(buildUpdateTaskRequest("task-3", { case: "setDone" }).change.case).toBe("setDone");
  });

  it("reopens through the set_open arm", () => {
    expect(buildUpdateTaskRequest("task-3", { case: "setOpen" }).change.case).toBe("setOpen");
  });
});

describe("the new-task control", () => {
  it("keeps its form folded until it is asked for", () => {
    const host = drawCreateTaskControl(sidebarContext());
    expect((host.querySelector(".sb-task-form") as HTMLElement).hidden).toBe(true);
  });

  it("opens the form on the control the suite targets", async () => {
    const host = drawCreateTaskControl(sidebarContext());
    await click(host.querySelector("[data-task-create]") as Element);
    expect((host.querySelector(".sb-task-form") as HTMLElement).hidden).toBe(false);
  });

  it("sends nothing for a blank title, because the contract forbids one", async () => {
    let calls = 0;
    const sc = sidebarContext(
      appContext({
        createTask: () => {
          calls += 1;
          return create(CreateTaskResponseSchema, {
            result: { case: "success", value: { task: { id: "task-9" } } },
          });
        },
      }),
    );
    const host = drawCreateTaskControl(sc);
    await click(host.querySelector("[data-task-create]") as Element);
    await click(host.querySelector(".sb-form-go") as Element);
    expect(calls).toBe(0);
  });

  it("sends the trimmed title", async () => {
    let sent = "";
    const sc = sidebarContext(
      appContext({
        createTask: (request) => {
          sent = request.title;
          return create(CreateTaskResponseSchema, {
            result: { case: "success", value: { task: { id: "task-9" } } },
          });
        },
      }),
    );
    const host = drawCreateTaskControl(sc);
    await click(host.querySelector("[data-task-create]") as Element);
    (host.querySelector("[data-task-title]") as HTMLInputElement).value = "  ship it  ";
    await click(host.querySelector(".sb-form-go") as Element);
    expect(sent).toBe("ship it");
  });

  it("closes the form once the daemon took the task", async () => {
    const sc = sidebarContext(
      appContext({
        createTask: () =>
          create(CreateTaskResponseSchema, {
            result: { case: "success", value: { task: { id: "task-9" } } },
          }),
      }),
    );
    const host = drawCreateTaskControl(sc);
    await click(host.querySelector("[data-task-create]") as Element);
    (host.querySelector("[data-task-title]") as HTMLInputElement).value = "ship it";
    await click(host.querySelector(".sb-form-go") as Element);
    expect((host.querySelector(".sb-task-form") as HTMLElement).hidden).toBe(true);
  });

  it("keeps the form open, with the words in it, when the daemon refused", async () => {
    const sc = sidebarContext(
      appContext({
        createTask: () => create(CreateTaskResponseSchema, { result: { case: "error", value: {} } }),
      }),
    );
    const host = drawCreateTaskControl(sc);
    await click(host.querySelector("[data-task-create]") as Element);
    (host.querySelector("[data-task-title]") as HTMLInputElement).value = "ship it";
    await click(host.querySelector(".sb-form-go") as Element);
    expect((host.querySelector("[data-task-title]") as HTMLInputElement).value).toBe("ship it");
  });

  it("says the refusal at the form's own control", async () => {
    const sc = sidebarContext(
      appContext({
        createTask: () => create(CreateTaskResponseSchema, { result: { case: "error", value: {} } }),
      }),
    );
    const host = drawCreateTaskControl(sc);
    await click(host.querySelector("[data-task-create]") as Element);
    (host.querySelector("[data-task-title]") as HTMLInputElement).value = "ship it";
    await click(host.querySelector(".sb-form-go") as Element);
    expect(host.querySelector(".refusal")?.textContent).toBe("CreateTask refused");
  });
});
