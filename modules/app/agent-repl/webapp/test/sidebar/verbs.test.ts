// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { CloseWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_workspace_pb";
import { OpenWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  buildAssignWorkspaceTaskRequest,
  buildCloseWorkspaceRequest,
  buildKillWorkspaceRequest,
  buildMergeWorkspaceRequest,
  buildNukeWorkspaceRequest,
  buildOpenWorkspaceRequest,
  buildRestartWorkspaceRequest,
  buildSelectWorkspaceRequest,
  buildSetWorkspacePriorityRequest,
  drawKillConfirm,
  drawNukeConfirm,
  drawRowMenu,
  fillAssignSubmenu,
  refusalArm,
  runVerb,
} from "../../src/sidebar/verbs.js";
import { appContext, sidebarContext } from "./harness.js";

const TARGET_WS = create(WorkspaceRefSchema, { id: "ws-7", dir: "/w/seven" });

function target(impl = {}) {
  const sc = sidebarContext(appContext(impl));
  return { sc, workspace: TARGET_WS, name: "seven" };
}

/** Click a control and let the verb's promise settle. */
async function click(control: Element): Promise<void> {
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  // The verb's own promise chain: let every pending microtask and the
  // transport's task drain before asserting on what it drew.
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

describe("the requests each verb is", () => {
  it("addresses SelectWorkspace with the row's echoed identity", () => {
    expect(buildSelectWorkspaceRequest(TARGET_WS).workspace).toEqual(TARGET_WS);
  });

  it("addresses OpenWorkspace with the row's echoed identity", () => {
    expect(buildOpenWorkspaceRequest(TARGET_WS).workspace).toEqual(TARGET_WS);
  });

  it("addresses CloseWorkspace with the row's echoed identity", () => {
    expect(buildCloseWorkspaceRequest(TARGET_WS).workspace).toEqual(TARGET_WS);
  });

  it("addresses KillWorkspace with the row's echoed identity", () => {
    expect(buildKillWorkspaceRequest(TARGET_WS).workspace).toEqual(TARGET_WS);
  });

  it("addresses NukeWorkspace with the row's echoed identity", () => {
    expect(buildNukeWorkspaceRequest(TARGET_WS).workspace).toEqual(TARGET_WS);
  });

  it("addresses MergeWorkspace with the row's echoed identity", () => {
    expect(buildMergeWorkspaceRequest(TARGET_WS).workspace).toEqual(TARGET_WS);
  });

  it("asks for a graceful restart with force false", () => {
    expect(buildRestartWorkspaceRequest(TARGET_WS, false).force).toBe(false);
  });

  it("asks for a forced restart with force true", () => {
    expect(buildRestartWorkspaceRequest(TARGET_WS, true).force).toBe(true);
  });

  it.each(["p05", "p1", "p2", "p3"] as const)("sets the %s priority arm", (choice) => {
    expect(buildSetWorkspacePriorityRequest(TARGET_WS, choice).priority?.level.case).toBe(choice);
  });

  it("clears a priority by omitting the field, never by a sentinel level", () => {
    expect(buildSetWorkspacePriorityRequest(TARGET_WS, "clear").priority).toBeUndefined();
  });

  it("assigns a task by echoing its id", () => {
    expect(buildAssignWorkspaceTaskRequest(TARGET_WS, "task-3").task?.id).toBe("task-3");
  });

  it("unassigns by omitting the task, never by an empty id", () => {
    expect(buildAssignWorkspaceTaskRequest(TARGET_WS, null).task).toBeUndefined();
  });
});

describe("the menu", () => {
  it("offers every verb the sidebar section declares", () => {
    const menu = drawRowMenu(target());
    const verbs = [...menu.querySelectorAll("[data-verb]")].map((el) =>
      el.getAttribute("data-verb"),
    );
    expect(verbs).toEqual([
      "open",
      "close",
      "merge",
      "restart",
      "restartForce",
      "priority",
      "assign",
      "kill",
      "nuke",
    ]);
  });

  it("delimits its rows with the one shared list class", () => {
    expect(drawRowMenu(target()).classList.contains("list-rows")).toBe(true);
  });

  it("keeps the priority levels folded until the entry is opened", () => {
    const menu = drawRowMenu(target());
    const submenu = menu.querySelector("[data-verb='priority']")?.nextElementSibling;
    expect((submenu as HTMLElement).hidden).toBe(true);
  });

  it("offers the four levels and the clear", () => {
    const menu = drawRowMenu(target());
    const levels = [...menu.querySelectorAll("[data-priority]")].map((el) =>
      el.getAttribute("data-priority"),
    );
    expect(levels).toEqual(["p05", "p1", "p2", "p3", "clear"]);
  });

  it("offers the task view's own sections as assignment choices", () => {
    const t = target();
    t.sc.tasks.push({ id: "task-3", label: "ship the rail" });
    const submenu = document.createElement("div");
    fillAssignSubmenu(submenu, t);
    const ids = [...submenu.querySelectorAll("[data-assign-task]")].map((el) =>
      el.getAttribute("data-assign-task"),
    );
    expect(ids).toEqual(["", "task-3"]);
  });
});

describe("the two destructive verbs", () => {
  it("asks before killing a session", async () => {
    const t = target();
    const menu = drawRowMenu(t);
    await click(menu.querySelector("[data-verb='kill']") as Element);
    expect(menu.querySelector(".sb-confirm")).not.toBeNull();
  });

  it("does not open a second confirmation over the first", async () => {
    const t = target();
    const menu = drawRowMenu(t);
    await click(menu.querySelector("[data-verb='kill']") as Element);
    await click(menu.querySelector("[data-verb='kill']") as Element);
    expect(menu.querySelectorAll(".sb-confirm").length).toBe(1);
  });

  it("keeps the nuke confirmation disabled until the name is typed back", () => {
    const confirm = drawNukeConfirm(target());
    const go = confirm.querySelector(".sb-confirm-go") as HTMLButtonElement;
    expect(go.disabled).toBe(true);
  });

  it("refuses a near-miss of the typed name", () => {
    const confirm = drawNukeConfirm(target());
    const typed = confirm.querySelector(".sb-confirm-name") as HTMLInputElement;
    const go = confirm.querySelector(".sb-confirm-go") as HTMLButtonElement;
    typed.value = "seve";
    typed.dispatchEvent(new Event("input"));
    expect(go.disabled).toBe(true);
  });

  it("arms once the name matches exactly", () => {
    const confirm = drawNukeConfirm(target());
    const typed = confirm.querySelector(".sb-confirm-name") as HTMLInputElement;
    const go = confirm.querySelector(".sb-confirm-go") as HTMLButtonElement;
    typed.value = "seven";
    typed.dispatchEvent(new Event("input"));
    expect(go.disabled).toBe(false);
  });

  it("says what a kill costs and what it spares", () => {
    const note = drawKillConfirm(target()).querySelector(".sb-confirm-note");
    expect(note?.textContent).toContain("worktree and branch survive");
  });
});

describe("a refusal", () => {
  it("names the close-blocked cause", () => {
    const error = create(CloseWorkspaceResponseSchema, {
      result: { case: "error", value: { cause: { case: "blocked", value: {} } } },
    });
    expect(refusalArm((error.result as { value: unknown }).value)).toBe("blocked");
  });

  it("falls back to the error itself where the arm carries no cause", () => {
    const error = create(OpenWorkspaceResponseSchema, {
      result: { case: "error", value: {} },
    });
    expect(refusalArm((error.result as { value: unknown }).value)).toBe("error");
  });

  it("renders at the control that made the call", async () => {
    const t = target({
      openWorkspace: () => create(OpenWorkspaceResponseSchema, { result: { case: "error", value: {} } }),
    });
    const menu = drawRowMenu(t);
    document.body.replaceChildren(menu);
    await click(menu.querySelector("[data-verb='open']") as Element);
    const refusal = menu.querySelector(".refusal[data-arm]");
    expect(refusal?.getAttribute("data-arm")).toBe("error");
  });

  it("names the rpc when the error message is empty on purpose", async () => {
    const t = target({
      openWorkspace: () => create(OpenWorkspaceResponseSchema, { result: { case: "error", value: {} } }),
    });
    const menu = drawRowMenu(t);
    await click(menu.querySelector("[data-verb='open']") as Element);
    expect(menu.querySelector(".refusal")?.textContent).toBe("OpenWorkspace refused");
  });

  it("points a blocked close at the footer, which carries the reasons", async () => {
    const t = target({
      closeWorkspace: () =>
        create(CloseWorkspaceResponseSchema, {
          result: { case: "error", value: { cause: { case: "blocked", value: {} } } },
        }),
    });
    const menu = drawRowMenu(t);
    await click(menu.querySelector("[data-verb='close']") as Element);
    expect(menu.querySelector(".refusal")?.textContent).toBe(
      "close refused: work in flight (see the footer)",
    );
  });

  it("re-enables the control so the user can try again", async () => {
    const t = target({
      openWorkspace: () => create(OpenWorkspaceResponseSchema, { result: { case: "error", value: {} } }),
    });
    const menu = drawRowMenu(t);
    const button = menu.querySelector("[data-verb='open']") as HTMLButtonElement;
    await click(button);
    expect(button.disabled).toBe(false);
  });

  it("replaces a previous attempt's refusal rather than stacking one", async () => {
    const t = target({
      openWorkspace: () => create(OpenWorkspaceResponseSchema, { result: { case: "error", value: {} } }),
    });
    const menu = drawRowMenu(t);
    const button = menu.querySelector("[data-verb='open']") as Element;
    await click(button);
    await click(button);
    expect(menu.querySelectorAll(".sb-refusal").length).toBe(1);
  });
});

describe("a transport failure", () => {
  it("says the daemon could not be reached", async () => {
    const t = target({
      openWorkspace: () => {
        throw new Error("no route");
      },
    });
    const menu = drawRowMenu(t);
    await click(menu.querySelector("[data-verb='open']") as Element);
    const refusal = menu.querySelector(".refusal");
    expect(refusal?.getAttribute("data-arm")).toBe("transport");
  });
});

describe("a successful verb", () => {
  it("draws nothing, because the roster push carries the new state", async () => {
    const t = target({
      openWorkspace: () =>
        create(OpenWorkspaceResponseSchema, { result: { case: "success", value: {} } }),
    });
    const menu = drawRowMenu(t);
    await click(menu.querySelector("[data-verb='open']") as Element);
    expect(menu.querySelector(".refusal")).toBeNull();
  });

  it("answers true, so a form can dismiss itself on the answer", async () => {
    const t = target({
      openWorkspace: () =>
        create(OpenWorkspaceResponseSchema, { result: { case: "success", value: {} } }),
    });
    const button = document.createElement("button");
    const ok = await runVerb(button, {
      sc: t.sc,
      rpc: "OpenWorkspace",
      call: (client) => client.openWorkspace(buildOpenWorkspaceRequest(TARGET_WS)),
      schema: OpenWorkspaceResponseSchema,
    });
    expect(ok).toBe(true);
  });
});

describe("a response with no outcome arm", () => {
  it("is a malformed view, never a quiet success", async () => {
    const t = target({
      openWorkspace: () => create(OpenWorkspaceResponseSchema, {}),
    });
    const button = document.createElement("button");
    await expect(
      runVerb(button, {
        sc: t.sc,
        rpc: "OpenWorkspace",
        call: (client) => client.openWorkspace(buildOpenWorkspaceRequest(TARGET_WS)),
        schema: OpenWorkspaceResponseSchema,
      }),
    ).rejects.toThrow(MalformedView);
  });
});
