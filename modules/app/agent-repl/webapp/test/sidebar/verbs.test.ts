// @vitest-environment jsdom
import { createControl, type Control } from "../../src/control.js";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  CloseWorkspaceErrorSchema,
  CloseWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_workspace_pb";
import {
  OpenWorkspaceErrorSchema,
  OpenWorkspaceResponseSchema,
  OpenWorkspaceLockHolderUnavailableSchema,
  OpenWorkspaceVendorStartFailedSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import {
  AssignWorkspaceTaskErrorSchema,
  AssignWorkspaceTaskResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_assign_workspace_task_pb";
import {
  KillWorkspaceErrorSchema,
  KillWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_kill_workspace_pb";
import {
  MergeWorkspaceErrorSchema,
  MergeWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_merge_workspace_pb";
import {
  NukeWorkspaceErrorSchema,
  NukeWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_nuke_workspace_pb";
import {
  RestartWorkspaceErrorSchema,
  RestartWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_restart_workspace_pb";
import {
  SelectWorkspaceErrorSchema,
  SelectWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import {
  SetWorkspacePriorityErrorSchema,
  SetWorkspacePriorityResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_priority_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { LockHolderFailureSchema } from "../../../proto/gen/ts/conversation/v1/session_pb";
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
  assignWorkspaceTaskRefusal,
  closeWorkspaceRefusal,
  drawRowMenu,
  fillAssignSubmenu,
  mergeWorkspaceRefusal,
  nukeWorkspaceRefusal,
  lockHolderHowText,
  openWorkspaceRefusal,
  restartWorkspaceRefusal,
  fireVerb,
  refusalCause,
  refusalDetail,
  runVerb,
} from "../../src/sidebar/verbs.js";
import { oneofArms } from "../arms.js";
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

  it("asks MergeWorkspace to merge the row's own branch", () => {
    expect(buildMergeWorkspaceRequest(TARGET_WS).source?.source.case).toBe("ownBranch");
  });

  it("asks MergeWorkspace to close the workspace once its branch lands", () => {
    const source = buildMergeWorkspaceRequest(TARGET_WS).source?.source;
    expect(source?.case === "ownBranch" ? source.value.keepOpen : "not own branch").toBe(false);
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
    // The verb hook sits on the control that ISSUES the request: the priority
    // menu's five choices are five priority controls, and the two destructive
    // verbs live on their confirmation's go button rather than on the entry
    // that only reveals it.
    expect(verbs).toEqual([
      "open",
      "close",
      "merge",
      "restart",
      "restartForce",
      "priority",
      "priority",
      "priority",
      "priority",
      "priority",
      "kill",
      "nuke",
    ]);
  });

  it("delimits its rows with the one shared list class", () => {
    expect(drawRowMenu(target()).classList.contains("list-rows")).toBe(true);
  });

  it("keeps the priority levels folded until the entry is opened", () => {
    const menu = drawRowMenu(target());
    const submenu = menu.querySelector("[data-verb='priority']")?.parentElement;
    expect((submenu as HTMLElement).hidden).toBe(true);
  });

  it("offers the four levels and the clear", () => {
    const menu = drawRowMenu(target());
    // The choice rides on the button's `value`: `data-priority` inside a row
    // is the row's priority BADGE, and a row served none carries none.
    const levels = [...menu.querySelectorAll<Control>("[data-verb='priority']")].map(
      (el) => el.getAttribute("value"),
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

  it("draws exactly one confirmation, revealed rather than stacked", async () => {
    const t = target();
    const menu = drawRowMenu(t);
    const entry = (menu.querySelector("[data-verb='kill']") as HTMLElement).closest(
      ".sb-menu-row",
    ) as HTMLElement;
    await click(entry.querySelector(".sb-menu-item") as Element);
    await click(entry.querySelector(".sb-menu-item") as Element);
    expect(entry.querySelectorAll(".sb-confirm").length).toBe(1);
  });

  it("keeps the nuke confirmation unarmed until the name is typed back", () => {
    const confirm = drawNukeConfirm(target());
    const go = confirm.querySelector(".sb-confirm-go") as Control;
    expect(go.classList.contains("armed")).toBe(false);
  });

  it("refuses a near-miss of the typed name", () => {
    const confirm = drawNukeConfirm(target());
    const typed = confirm.querySelector(".sb-confirm-name") as HTMLInputElement;
    const go = confirm.querySelector(".sb-confirm-go") as Control;
    typed.value = "seve";
    typed.dispatchEvent(new Event("input"));
    expect(go.classList.contains("armed")).toBe(false);
  });

  it("arms once the name matches exactly", () => {
    const confirm = drawNukeConfirm(target());
    const typed = confirm.querySelector(".sb-confirm-name") as HTMLInputElement;
    const go = confirm.querySelector(".sb-confirm-go") as Control;
    typed.value = "seven";
    typed.dispatchEvent(new Event("input"));
    expect(go.classList.contains("armed")).toBe(true);
  });

  it("says what a kill costs and what it spares", () => {
    const note = drawKillConfirm(target()).querySelector(".sb-confirm-note");
    expect(note?.textContent).toContain("worktree and branch survive");
  });
});

/**
 * A payload for every arm that carries a fact, so the schema-driven sweep can
 * build a WELL-FORMED refusal for each arm without a per-arm test body.
 */
const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  workspaceRefMismatch: { registryDir: "/w/registry" },
  transferringAway: { address: "127.0.0.1:7777" },
  transcriptMissing: { vendorSessionId: "vs-1", searchedPaths: ["/a", "/b"] },
  spawnFailed: { detail: "exec format error" },
  vendorStartFailed: { detail: "the sdk threw before its first message" },
  lockHolderUnavailable: {
    failure: { binary: "/b/shim-lock", how: { case: "exited", value: { code: 1, stderr: "EACCES" } } },
  },
  gitFailed: { detail: "worktree is dirty" },
};

/** One verb, enough to issue it and to enumerate what it can refuse with. */
interface VerbUnderTest {
  rpc: string;
  method: string;
  responseSchema: Parameters<typeof create>[0];
  errorSchema: Parameters<typeof oneofArms>[0];
  call: (t: ReturnType<typeof target>, control: HTMLElement) => Promise<boolean>;
}

const VERBS: readonly VerbUnderTest[] = [
  {
    rpc: "OpenWorkspace",
    method: "openWorkspace",
    responseSchema: OpenWorkspaceResponseSchema,
    errorSchema: OpenWorkspaceErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "OpenWorkspace",
        call: (client) => client.openWorkspace(buildOpenWorkspaceRequest(TARGET_WS)),
        schema: OpenWorkspaceResponseSchema,
        refusalText: (cause) => openWorkspaceRefusal(cause as never),
      }),
  },
  {
    rpc: "CloseWorkspace",
    method: "closeWorkspace",
    responseSchema: CloseWorkspaceResponseSchema,
    errorSchema: CloseWorkspaceErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "CloseWorkspace",
        call: (client) => client.closeWorkspace(buildCloseWorkspaceRequest(TARGET_WS)),
        schema: CloseWorkspaceResponseSchema,
        refusalText: (cause) => closeWorkspaceRefusal(cause as never),
      }),
  },
  {
    rpc: "KillWorkspace",
    method: "killWorkspace",
    responseSchema: KillWorkspaceResponseSchema,
    errorSchema: KillWorkspaceErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "KillWorkspace",
        call: (client) => client.killWorkspace(buildKillWorkspaceRequest(TARGET_WS)),
        schema: KillWorkspaceResponseSchema,
      }),
  },
  {
    rpc: "NukeWorkspace",
    method: "nukeWorkspace",
    responseSchema: NukeWorkspaceResponseSchema,
    errorSchema: NukeWorkspaceErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "NukeWorkspace",
        call: (client) => client.nukeWorkspace(buildNukeWorkspaceRequest(TARGET_WS)),
        schema: NukeWorkspaceResponseSchema,
        refusalText: (cause) => nukeWorkspaceRefusal(cause as never),
      }),
  },
  {
    rpc: "MergeWorkspace",
    method: "mergeWorkspace",
    responseSchema: MergeWorkspaceResponseSchema,
    errorSchema: MergeWorkspaceErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "MergeWorkspace",
        call: (client) => client.mergeWorkspace(buildMergeWorkspaceRequest(TARGET_WS)),
        schema: MergeWorkspaceResponseSchema,
        refusalText: (cause) => mergeWorkspaceRefusal(cause as never),
      }),
  },
  {
    rpc: "RestartWorkspace",
    method: "restartWorkspace",
    responseSchema: RestartWorkspaceResponseSchema,
    errorSchema: RestartWorkspaceErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "RestartWorkspace",
        call: (client) => client.restartWorkspace(buildRestartWorkspaceRequest(TARGET_WS, false)),
        schema: RestartWorkspaceResponseSchema,
        refusalText: (cause) => restartWorkspaceRefusal(cause as never),
      }),
  },
  {
    rpc: "SetWorkspacePriority",
    method: "setWorkspacePriority",
    responseSchema: SetWorkspacePriorityResponseSchema,
    errorSchema: SetWorkspacePriorityErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "SetWorkspacePriority",
        call: (client) =>
          client.setWorkspacePriority(buildSetWorkspacePriorityRequest(TARGET_WS, "p1")),
        schema: SetWorkspacePriorityResponseSchema,
      }),
  },
  {
    rpc: "AssignWorkspaceTask",
    method: "assignWorkspaceTask",
    responseSchema: AssignWorkspaceTaskResponseSchema,
    errorSchema: AssignWorkspaceTaskErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "AssignWorkspaceTask",
        call: (client) =>
          client.assignWorkspaceTask(buildAssignWorkspaceTaskRequest(TARGET_WS, "task-3")),
        schema: AssignWorkspaceTaskResponseSchema,
        refusalText: (cause) => assignWorkspaceTaskRefusal(cause as never),
      }),
  },
  {
    rpc: "SelectWorkspace",
    method: "selectWorkspace",
    responseSchema: SelectWorkspaceResponseSchema,
    errorSchema: SelectWorkspaceErrorSchema,
    call: (t, control) =>
      runVerb(control, {
        sc: t.sc,
        rpc: "SelectWorkspace",
        call: (client) => client.selectWorkspace(buildSelectWorkspaceRequest(TARGET_WS)),
        schema: SelectWorkspaceResponseSchema,
      }),
  },
];

/** Issue VERB against a daemon that refuses with ARM, and answer the element. */
async function refuseWith(verb: VerbUnderTest, arm: string): Promise<Element | null> {
  const t = target({
    [verb.method]: () =>
      create(verb.responseSchema as typeof OpenWorkspaceResponseSchema, {
        result: {
          case: "error",
          value: { cause: { case: arm, value: CAUSE_FILL[arm] ?? {} } },
        },
      } as never),
  });
  const host = document.createElement("div");
  const control = createControl();
  host.appendChild(control);
  await verb.call(t, control);
  return host.querySelector(".refusal[data-arm]");
}

describe.each(VERBS.map((verb) => [verb.rpc, verb] as const))(
  "%s's typed refusal",
  (_rpc, verb) => {
    const arms = oneofArms(verb.errorSchema, "cause");

    it.each(arms)("labels the %s arm with its own case name", async (arm) => {
      expect((await refuseWith(verb, arm))?.getAttribute("data-arm")).toBe(arm);
    });

    it.each(arms)("says something about the %s arm", async (arm) => {
      expect((await refuseWith(verb, arm))?.textContent).not.toBe("");
    });
  },
);

describe("the cross-cutting causes, worded once", () => {
  it("names the registry's directory on a mismatch", async () => {
    const refusal = await refuseWith(VERBS[0], "workspaceRefMismatch");
    expect(refusal?.textContent).toContain("/w/registry");
  });

  it("names the successor daemon on a transfer", async () => {
    const refusal = await refuseWith(VERBS[0], "transferringAway");
    expect(refusal?.textContent).toContain("127.0.0.1:7777");
  });
});

describe("the per-rpc causes, worded at their own site", () => {
  it("points a blocked close at the footer, which carries the reasons", async () => {
    const refusal = await refuseWith(VERBS[1], "blocked");
    expect(refusal?.textContent).toBe("close refused: work in flight (see the footer)");
  });

  it("names the session a missing transcript belongs to", async () => {
    const refusal = await refuseWith(VERBS[0], "transcriptMissing");
    expect(refusal?.textContent).toContain("vs-1");
  });

  it("lists the paths a missing transcript was searched for in", async () => {
    const refusal = await refuseWith(VERBS[0], "transcriptMissing");
    expect([...(refusal?.querySelectorAll("[data-searched-paths] li") ?? [])].map((li) => li.textContent)).toEqual([
      "/a",
      "/b",
    ]);
  });

  it("counts no paths in the sentence, because a count is not actionable", async () => {
    const refusal = await refuseWith(VERBS[0], "transcriptMissing");
    expect(refusal?.textContent).not.toContain("2 searched");
  });

  it("carries the spawn failure's own detail", async () => {
    const refusal = await refuseWith(VERBS[0], "spawnFailed");
    expect(refusal?.textContent).toContain("exec format error");
  });

  it("names the vendor as what failed to start the session", async () => {
    const refusal = await refuseWith(VERBS[0], "vendorStartFailed");
    expect(refusal?.textContent).toContain("the vendor failed to start the session");
  });

  it("appends the shim's own detail to a vendor start failure", async () => {
    const refusal = await refuseWith(VERBS[0], "vendorStartFailed");
    expect(refusal?.textContent).toContain("(the sdk threw before its first message)");
  });

  it("leaves no empty parenthetical when the vendor start failure has no detail", () => {
    // Arrange / Act: the arm worded directly, since the sweep's fill is never empty.
    const text = openWorkspaceRefusal({
      case: "vendorStartFailed",
      value: create(OpenWorkspaceVendorStartFailedSchema, { detail: "" }),
    } as never);
    // Assert
    expect(text).toBe("the vendor failed to start the session");
  });

  it("says the shim's lock helper failed and how, naming the binary", async () => {
    const refusal = await refuseWith(VERBS[0], "lockHolderUnavailable");
    expect(refusal?.textContent).toContain(
      "the shim's lock helper /b/shim-lock exited with code 1 before taking the lock (EACCES)",
    );
  });

  it("refuses a lock_holder_unavailable arm that carries no failure as malformed", () => {
    // Arrange / Act / Assert: the contract says the failure is always set.
    expect(() =>
      openWorkspaceRefusal({
        case: "lockHolderUnavailable",
        value: create(OpenWorkspaceLockHolderUnavailableSchema, {}),
      } as never),
    ).toThrow(MalformedView);
  });

  it.each([
    ["a spawn failure", { case: "spawnFailed", value: { osError: "spawn ENOENT" } }, "could not be spawned: spawn ENOENT"],
    ["an exit with stderr", { case: "exited", value: { code: 1, stderr: "EACCES" } }, "exited with code 1 before taking the lock (EACCES)"],
    ["an exit with no stderr", { case: "exited", value: { code: 2, stderr: "" } }, "exited with code 2 before taking the lock"],
    ["a signal", { case: "signaled", value: { signal: "SIGSEGV", stderr: "" } }, "was killed by SIGSEGV before taking the lock"],
    ["a wrong line", { case: "misanswered", value: { line: "ok" } }, 'answered "ok" instead of "locked" and was killed'],
    ["no answer", { case: "silent", value: { timeoutMs: 5000 } }, 'gave no "locked" answer within 5000 ms and was killed'],
  ])("words %s as the lock helper failing", (_name, how, want) => {
    // Arrange
    const failure = create(LockHolderFailureSchema, { binary: "/b/shim-lock", how } as never);
    // Act / Assert
    expect(lockHolderHowText(failure)).toBe(want);
  });

  it("refuses a LockHolderFailure that states no how as malformed", () => {
    // Arrange
    const failure = create(LockHolderFailureSchema, { binary: "/b/shim-lock" });
    // Act / Assert
    expect(() => lockHolderHowText(failure)).toThrow(MalformedView);
  });

  it("never words an unavailable lock helper as another owner", async () => {
    const refusal = await refuseWith(VERBS[0], "lockHolderUnavailable");
    expect(refusal?.textContent).toContain("no other process owns this conversation");
  });

  it("carries git's own detail when a nuke fails", async () => {
    const refusal = await refuseWith(VERBS[3], "gitFailed");
    expect(refusal?.textContent).toContain("worktree is dirty");
  });

  it("says a merge is already queued", async () => {
    const refusal = await refuseWith(VERBS[4], "alreadyQueued");
    expect(refusal?.textContent).toContain("already in the merge queue");
  });

  it("says a restart has no session to restart", async () => {
    const refusal = await refuseWith(VERBS[5], "noSession");
    expect(refusal?.textContent).toContain("no session to restart");
  });

  it("says an assignment names a task the daemon does not know", async () => {
    const refusal = await refuseWith(VERBS[7], "unknownTask");
    expect(refusal?.textContent).toContain("does not know that task");
  });
});

describe("a refusal", () => {
  it("renders at the control that made the call", async () => {
    const t = target({
      closeWorkspace: () =>
        create(CloseWorkspaceResponseSchema, {
          result: { case: "error", value: { cause: { case: "blocked", value: {} } } },
        }),
    });
    const menu = drawRowMenu(t);
    document.body.replaceChildren(menu);
    await click(menu.querySelector("[data-verb='close']") as Element);
    const refusal = menu.querySelector(".refusal[data-arm]");
    expect(refusal?.getAttribute("data-arm")).toBe("blocked");
  });

  it("re-enables the control so the user can try again", async () => {
    const t = target({
      closeWorkspace: () =>
        create(CloseWorkspaceResponseSchema, {
          result: { case: "error", value: { cause: { case: "blocked", value: {} } } },
        }),
    });
    const menu = drawRowMenu(t);
    const button = menu.querySelector("[data-verb='close']") as Control;
    await click(button);
    expect(button.disabled).toBe(false);
  });

  it("replaces a previous attempt's refusal rather than stacking one", async () => {
    const t = target({
      closeWorkspace: () =>
        create(CloseWorkspaceResponseSchema, {
          result: { case: "error", value: { cause: { case: "blocked", value: {} } } },
        }),
    });
    const menu = drawRowMenu(t);
    const button = menu.querySelector("[data-verb='close']") as Element;
    await click(button);
    await click(button);
    expect(menu.querySelectorAll(".sb-refusal").length).toBe(1);
  });
});

describe("an error with no cause", () => {
  it("is a malformed view, because every refusal has been typed since landing 4", async () => {
    const t = target({
      openWorkspace: () =>
        create(OpenWorkspaceResponseSchema, { result: { case: "error", value: {} } }),
    });
    const button = createControl();
    await expect(
      runVerb(button, {
        sc: t.sc,
        rpc: "OpenWorkspace",
        call: (client) => client.openWorkspace(buildOpenWorkspaceRequest(TARGET_WS)),
        schema: OpenWorkspaceResponseSchema,
      }),
    ).rejects.toThrow(MalformedView);
  });

  it("is a malformed view for an arm no build knows", async () => {
    const t = target({
      openWorkspace: () =>
        create(OpenWorkspaceResponseSchema, {
          result: { case: "error", value: { cause: { case: "sessionDeleted", value: {} } } },
        }),
    });
    const button = createControl();
    await expect(
      runVerb(button, {
        sc: t.sc,
        rpc: "OpenWorkspace",
        call: (client) => client.openWorkspace(buildOpenWorkspaceRequest(TARGET_WS)),
        schema: OpenWorkspaceResponseSchema,
        // A site whose hook does not know the arm must refuse, never draw blank.
        refusalText: () => undefined as unknown as string,
      }),
    ).rejects.toThrow(MalformedView);
  });
});

describe("fireVerb", () => {
  it("answers true when the verb succeeded", async () => {
    const t = target({
      openWorkspace: () =>
        create(OpenWorkspaceResponseSchema, { result: { case: "success", value: {} } }),
    });
    const button = createControl();
    await expect(
      fireVerb(button, {
        sc: t.sc,
        rpc: "OpenWorkspace",
        call: (client) => client.openWorkspace(buildOpenWorkspaceRequest(TARGET_WS)),
        schema: OpenWorkspaceResponseSchema,
      }),
    ).resolves.toBe(true);
  });

  it("absorbs a malformed answer rather than letting a click's rejection escape", async () => {
    const t = target({
      openWorkspace: () =>
        create(OpenWorkspaceResponseSchema, { result: { case: "error", value: {} } }),
    });
    const button = createControl();
    await expect(
      fireVerb(button, {
        sc: t.sc,
        rpc: "OpenWorkspace",
        call: (client) => client.openWorkspace(buildOpenWorkspaceRequest(TARGET_WS)),
        schema: OpenWorkspaceResponseSchema,
      }),
    ).resolves.toBe(false);
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
    const button = createControl();
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
    const button = createControl();
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

describe("the menu's own controls, clicked", () => {
  it("merges through MergeWorkspace with the row's echoed identity", async () => {
    // Arrange
    let seen: unknown = null;
    const t = target({
      mergeWorkspace: (req: unknown) => {
        seen = (req as { workspace?: unknown }).workspace;
        return create(MergeWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    const menu = drawRowMenu(t);
    // Act
    await click(menu.querySelector("[data-verb='merge']") as Element);
    // Assert
    expect(seen).toEqual(TARGET_WS);
  });

  it("draws a merge refusal at the merge control itself", async () => {
    const t = target({
      mergeWorkspace: () =>
        create(MergeWorkspaceResponseSchema, {
          result: { case: "error", value: { cause: { case: "alreadyMerging", value: {} } } },
        }),
    });
    const menu = drawRowMenu(t);
    const button = menu.querySelector("[data-verb='merge']") as HTMLElement;
    await click(button);
    expect(button.nextElementSibling?.textContent).toBe("this workspace is already merging");
  });

  it("restarts gracefully from the plain restart entry", async () => {
    let force: unknown = null;
    const t = target({
      restartWorkspace: (req: unknown) => {
        force = (req as { force?: unknown }).force;
        return create(RestartWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    const menu = drawRowMenu(t);
    await click(menu.querySelector("[data-verb='restart']") as Element);
    expect(force).toBe(false);
  });

  it("forces the restart from the forced entry", async () => {
    let force: unknown = null;
    const t = target({
      restartWorkspace: (req: unknown) => {
        force = (req as { force?: unknown }).force;
        return create(RestartWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    const menu = drawRowMenu(t);
    await click(menu.querySelector("[data-verb='restartForce']") as Element);
    expect(force).toBe(true);
  });

  it("draws a restart refusal at the restart control itself", async () => {
    const t = target({
      restartWorkspace: () =>
        create(RestartWorkspaceResponseSchema, {
          result: { case: "error", value: { cause: { case: "noSession", value: {} } } },
        }),
    });
    const menu = drawRowMenu(t);
    const button = menu.querySelector("[data-verb='restart']") as HTMLElement;
    await click(button);
    expect(button.nextElementSibling?.textContent).toBe("this workspace has no session to restart");
  });

  it("keeps the nuke confirmation folded until its entry is opened", () => {
    const menu = drawRowMenu(target());
    const confirm = menu.querySelector(".sb-confirm-nuke") as HTMLElement;
    expect(confirm.hidden).toBe(true);
  });

  it("reveals the nuke confirmation when its entry is clicked", async () => {
    const menu = drawRowMenu(target());
    const entry = (menu.querySelector("[data-verb='nuke']") as HTMLElement).closest(
      ".sb-menu-row",
    ) as HTMLElement;
    await click(entry.querySelector(".sb-menu-item") as Element);
    expect((entry.querySelector(".sb-confirm-nuke") as HTMLElement).hidden).toBe(false);
  });

  it("folds the nuke confirmation away again on a second click", async () => {
    const menu = drawRowMenu(target());
    const entry = (menu.querySelector("[data-verb='nuke']") as HTMLElement).closest(
      ".sb-menu-row",
    ) as HTMLElement;
    const disclosure = entry.querySelector(".sb-menu-item") as Element;
    await click(disclosure);
    await click(disclosure);
    expect((entry.querySelector(".sb-confirm-nuke") as HTMLElement).hidden).toBe(true);
  });

  it("nukes through NukeWorkspace with the row's echoed identity", async () => {
    let seen: unknown = null;
    const t = target({
      nukeWorkspace: (req: unknown) => {
        seen = (req as { workspace?: unknown }).workspace;
        return create(NukeWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    const confirm = drawNukeConfirm(t);
    await click(confirm.querySelector("[data-verb='nuke']") as Element);
    expect(seen).toEqual(TARGET_WS);
  });

  it("draws a nuke refusal at the go button that made the call", async () => {
    const t = target({
      nukeWorkspace: () =>
        create(NukeWorkspaceResponseSchema, {
          result: {
            case: "error",
            value: { cause: { case: "gitFailed", value: { detail: "worktree is dirty" } } },
          },
        }),
    });
    const confirm = drawNukeConfirm(t);
    const go = confirm.querySelector("[data-verb='nuke']") as HTMLElement;
    await click(go);
    expect(go.nextElementSibling?.textContent).toBe(
      "git refused to remove the worktree: worktree is dirty",
    );
  });

  it("kills through KillWorkspace with the row's echoed identity", async () => {
    let seen: unknown = null;
    const t = target({
      killWorkspace: (req: unknown) => {
        seen = (req as { workspace?: unknown }).workspace;
        return create(KillWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    const confirm = drawKillConfirm(t);
    await click(confirm.querySelector("[data-verb='kill']") as Element);
    expect(seen).toEqual(TARGET_WS);
  });

  it("reveals the priority levels when their entry is clicked", async () => {
    const menu = drawRowMenu(target());
    const submenu = (menu.querySelector("[data-verb='priority']") as HTMLElement)
      .parentElement as HTMLElement;
    await click(submenu.previousElementSibling as Element);
    expect(submenu.hidden).toBe(false);
  });

  it.each(["p05", "p1", "p2", "p3"] as const)(
    "sends the %s level from its own choice button",
    async (choice) => {
      let level: unknown = null;
      const t = target({
        setWorkspacePriority: (req: unknown) => {
          level = (req as { priority?: { level: { case?: string } } }).priority?.level.case;
          return create(SetWorkspacePriorityResponseSchema, {
            result: { case: "success", value: {} },
          });
        },
      });
      const menu = drawRowMenu(t);
      const entry = [...menu.querySelectorAll<Control>("[data-verb='priority']")].find(
        (el) => el.getAttribute("value") === choice,
      ) as HTMLElement;
      await click(entry);
      expect(level).toBe(choice);
    },
  );

  it("clears the priority by omitting the field from the Clear choice", async () => {
    let priority: unknown = "unset-was-not-read";
    const t = target({
      setWorkspacePriority: (req: unknown) => {
        priority = (req as { priority?: unknown }).priority;
        return create(SetWorkspacePriorityResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
    });
    const menu = drawRowMenu(t);
    const entry = [...menu.querySelectorAll<Control>("[data-verb='priority']")].find(
      (el) => el.getAttribute("value") === "clear",
    ) as HTMLElement;
    await click(entry);
    expect(priority).toBeUndefined();
  });

  it("reveals the assignment choices when their entry is clicked", async () => {
    const t = target();
    const menu = drawRowMenu(t);
    const submenu = (menu.querySelector("[data-assign-task]") as HTMLElement)
      .parentElement as HTMLElement;
    await click(submenu.previousElementSibling as Element);
    expect(submenu.hidden).toBe(false);
  });

  it("rebuilds the assignment choices from the last push each time it opens", async () => {
    const t = target();
    const menu = drawRowMenu(t);
    const submenu = (menu.querySelector("[data-assign-task]") as HTMLElement)
      .parentElement as HTMLElement;
    // Arrange: a roster push landed a task AFTER the menu was drawn.
    t.sc.tasks.push({ id: "task-9", label: "land the rail" });
    // Act
    await click(submenu.previousElementSibling as Element);
    // Assert
    expect([...submenu.querySelectorAll("[data-assign-task]")].map((el) => el.textContent)).toEqual(
      ["Unassign", "land the rail"],
    );
  });

  it("assigns the task the clicked choice names", async () => {
    let id: unknown = null;
    const t = target({
      assignWorkspaceTask: (req: unknown) => {
        id = (req as { task?: { id: string } }).task?.id;
        return create(AssignWorkspaceTaskResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
    });
    t.sc.tasks.push({ id: "task-3", label: "ship the rail" });
    const submenu = document.createElement("div");
    fillAssignSubmenu(submenu, t);
    await click(submenu.querySelector("[data-assign-task='task-3']") as Element);
    expect(id).toBe("task-3");
  });

  it("unassigns by omitting the task from the Unassign choice", async () => {
    let task: unknown = "unset-was-not-read";
    const t = target({
      assignWorkspaceTask: (req: unknown) => {
        task = (req as { task?: unknown }).task;
        return create(AssignWorkspaceTaskResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
    });
    const submenu = document.createElement("div");
    fillAssignSubmenu(submenu, t);
    await click(submenu.querySelector("[data-assign-task='']") as Element);
    expect(task).toBeUndefined();
  });

  it("draws an assignment refusal at the choice that made the call", async () => {
    const t = target({
      assignWorkspaceTask: () =>
        create(AssignWorkspaceTaskResponseSchema, {
          result: { case: "error", value: { cause: { case: "unknownTask", value: {} } } },
        }),
    });
    t.sc.tasks.push({ id: "task-3", label: "ship the rail" });
    const submenu = document.createElement("div");
    fillAssignSubmenu(submenu, t);
    const entry = submenu.querySelector("[data-assign-task='task-3']") as HTMLElement;
    await click(entry);
    expect(entry.nextElementSibling?.textContent).toBe("the daemon does not know that task");
  });
});

describe("an arm no wording table knows", () => {
  /** Each per-endpoint wording function, with an arm none of them declares. */
  const WORDINGS: ReadonlyArray<readonly [string, (cause: never) => string]> = [
    ["OpenWorkspaceError.cause", openWorkspaceRefusal],
    ["CloseWorkspaceError.cause", closeWorkspaceRefusal],
    ["NukeWorkspaceError.cause", nukeWorkspaceRefusal],
    ["MergeWorkspaceError.cause", mergeWorkspaceRefusal],
    ["RestartWorkspaceError.cause", restartWorkspaceRefusal],
    ["AssignWorkspaceTaskError.cause", assignWorkspaceTaskRefusal],
  ];

  it.each(WORDINGS)("refuses %s rather than inventing a sentence", (path, word) => {
    expect(() => word({ case: "aFutureArm", value: {} } as never)).toThrow(
      new MalformedView(path, "arm 'aFutureArm' is not one this build can draw"),
    );
  });
});

describe("the cause behind a refusal", () => {
  it("is a malformed view when the error message itself is absent", () => {
    // Arrange / Act / Assert: the `?? {}` side — nothing to require a case of.
    expect(() => refusalCause("OpenWorkspace", undefined)).toThrow(MalformedView);
  });
});

describe("the facts drawn under a refusal sentence", () => {
  it("draws no list for an arm that carries no paths", () => {
    expect(refusalDetail({ case: "spawnFailed", value: { detail: "x" } })).toBeNull();
  });

  it("draws no list when the search-paths field was never set", () => {
    expect(refusalDetail({ case: "transcriptMissing", value: {} })).toBeNull();
  });

  it("draws no empty list when the daemon searched nowhere", () => {
    expect(
      refusalDetail({ case: "transcriptMissing", value: { searchedPaths: [] } }),
    ).toBeNull();
  });
});

describe("the open control, refused", () => {
  it("draws OpenWorkspace's own sentence at the open control itself", async () => {
    const t = target({
      openWorkspace: () =>
        create(OpenWorkspaceResponseSchema, {
          result: { case: "error", value: { cause: { case: "sessionDeleted", value: {} } } },
        }),
    });
    const menu = drawRowMenu(t);
    const button = menu.querySelector("[data-verb='open']") as HTMLElement;
    await click(button);
    expect(button.nextElementSibling?.textContent).toBe(
      "this workspace's session has been deleted",
    );
  });
});
