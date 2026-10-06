// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  CreateWorkspaceErrorSchema,
  CreateWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_workspace_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { createWorkspaceRefusal } from "../../src/sidebar/create.js";
import { oneofArms } from "../arms.js";
import { RepositoryRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import {
  buildCreateWorkspaceRequest,
  collect,
  drawCreateWorkspaceControl,
  drawCreateWorkspaceForm,
  type CreateWorkspaceSpec,
} from "../../src/sidebar/create.js";
import { WORKSPACE, appContext, sidebarContext } from "./harness.js";

const REPO = create(RepositoryRefSchema, { id: "repo-1", dir: "/repo/one" });

/** The raw form with every field blank, for a test to vary one at a time. */
function raw(over: Partial<Parameters<typeof collect>[0]> = {}): Parameters<typeof collect>[0] {
  return {
    repository: REPO,
    workspace: WORKSPACE,
    oneShotMode: false,
    initialPrompt: "",
    baseRef: "",
    name: "",
    beforeWsMerge: "",
    postprocessingPrompt: "",
    oneShotPrompt: "",
    parent: false,
    fork: false,
    model: "",
    priority: "",
    allowUngated: false,
    ...over,
  };
}

/** The words inside a `UserSaid`'s first text block. */
function said(value: unknown): string | undefined {
  const blocks = (value as { content?: { blocks?: Array<{ block: { case: string; value: { text: string } } }> } })
    ?.content?.blocks;
  return blocks?.[0]?.block.case === "text" ? blocks[0].block.value.text : undefined;
}

async function click(control: Element): Promise<void> {
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

describe("what the form collects", () => {
  it("takes the standard form when the one-shot mode is not picked", () => {
    expect(collect(raw())?.form.case).toBe("standard");
  });

  it("omits a blank initial prompt rather than sending an empty UserSaid", () => {
    const form = collect(raw())?.form;
    expect(form?.case === "standard" ? form.initialPrompt : "set").toBeUndefined();
  });

  it("omits a blank base ref rather than sending an empty string", () => {
    const form = collect(raw())?.form;
    expect(form?.case === "standard" ? form.baseRef : "set").toBeUndefined();
  });

  it("omits a blank name, leaving the daemon to mint one", () => {
    const form = collect(raw())?.form;
    expect(form?.case === "standard" ? form.name : "set").toBeUndefined();
  });

  it("omits both merge actions when neither prompt was written", () => {
    const form = collect(raw())?.form;
    const both =
      form?.case === "standard" ? [form.beforeWsMerge, form.postprocessingPrompt] : ["set"];
    expect(both).toEqual([undefined, undefined]);
  });

  it("omits a blank model, leaving the daemon's default", () => {
    expect(collect(raw())?.model).toBeUndefined();
  });

  it("omits an unpicked priority rather than sending a level", () => {
    expect(collect(raw())?.priority).toBeUndefined();
  });

  it("omits the parent when the spawn option is not taken", () => {
    expect(collect(raw())?.parent).toBeUndefined();
  });

  it("takes this webview's own workspace as the parent when it is", () => {
    expect(collect(raw({ parent: true }))?.parent?.workspace).toEqual(WORKSPACE);
  });

  it("carries the fork only under a parent, because the schema confines it", () => {
    expect(collect(raw({ parent: true, fork: true }))?.parent?.fork).toBe(true);
  });

  it("drops a fork with no parent, which is unrepresentable on the wire", () => {
    expect(collect(raw({ parent: false, fork: true }))?.parent).toBeUndefined();
  });

  it("trims what the user typed", () => {
    const form = collect(raw({ name: "  rail  " }))?.form;
    expect(form?.case === "standard" ? form.name : null).toBe("rail");
  });

  it("answers nothing for a one-shot with no prompt, since a one-shot IS its prompt", () => {
    expect(collect(raw({ oneShotMode: true }))).toBeNull();
  });

  it("collects a one-shot as its commission and nothing else", () => {
    const form = collect(raw({ oneShotMode: true, oneShotPrompt: "do it" }))?.form;
    expect(form).toEqual({ case: "oneShot", prompt: "do it" });
  });

  it("passes the consent through as the flag the request turns into presence", () => {
    expect(collect(raw({ allowUngated: true }))?.allowUngated).toBe(true);
  });
});

describe("the request the spec becomes", () => {
  const standard = (over: Record<string, unknown> = {}): CreateWorkspaceSpec => ({
    repository: REPO,
    form: { case: "standard", ...over },
    allowUngated: false,
  });

  it("echoes the repository the section served", () => {
    expect(buildCreateWorkspaceRequest(standard()).repository).toEqual(REPO);
  });

  it("sets the standard arm, which IS the creation form", () => {
    expect(buildCreateWorkspaceRequest(standard()).form.case).toBe("standard");
  });

  it("leaves every optional standard field unset when the form was blank", () => {
    const form = buildCreateWorkspaceRequest(standard()).form;
    const value = form.case === "standard" ? form.value : null;
    expect([value?.initialPrompt, value?.baseRef, value?.name, value?.mergeActions]).toEqual([
      undefined,
      undefined,
      undefined,
      undefined,
    ]);
  });

  it("wraps the initial prompt as a UserSaid text block", () => {
    const form = buildCreateWorkspaceRequest(standard({ initialPrompt: "build the rail" })).form;
    expect(said(form.case === "standard" ? form.value.initialPrompt : undefined)).toBe(
      "build the rail",
    );
  });

  it("carries only the pre-merge action when only it was written", () => {
    const form = buildCreateWorkspaceRequest(standard({ beforeWsMerge: "check" })).form;
    const actions = form.case === "standard" ? form.value.mergeActions : undefined;
    expect([said(actions?.beforeWsMerge), actions?.postprocessingPrompt]).toEqual([
      "check",
      undefined,
    ]);
  });

  it("carries only the post-merge action when only it was written", () => {
    const form = buildCreateWorkspaceRequest(standard({ postprocessingPrompt: "tidy" })).form;
    const actions = form.case === "standard" ? form.value.mergeActions : undefined;
    expect([actions?.beforeWsMerge, said(actions?.postprocessingPrompt)]).toEqual([
      undefined,
      "tidy",
    ]);
  });

  it("sets the one-shot arm for a one-shot", () => {
    const request = buildCreateWorkspaceRequest({
      repository: REPO,
      form: { case: "oneShot", prompt: "do it" },
      allowUngated: false,
    });
    expect(request.form.case).toBe("oneShot");
  });

  it("carries the one-shot's commission as its prompt", () => {
    const request = buildCreateWorkspaceRequest({
      repository: REPO,
      form: { case: "oneShot", prompt: "do it" },
      allowUngated: false,
    });
    expect(said(request.form.case === "oneShot" ? request.form.value.prompt : undefined)).toBe(
      "do it",
    );
  });

  it("leaves the parent unset for a top-level create", () => {
    expect(buildCreateWorkspaceRequest(standard()).parent).toBeUndefined();
  });

  it("names the parent workspace when the create is spawned from one", () => {
    const request = buildCreateWorkspaceRequest({
      ...standard(),
      parent: { workspace: WORKSPACE, fork: false },
    });
    expect(request.parent?.workspace).toEqual(WORKSPACE);
  });

  it("leaves the fork unset when the conversation is not forked", () => {
    const request = buildCreateWorkspaceRequest({
      ...standard(),
      parent: { workspace: WORKSPACE, fork: false },
    });
    expect(request.parent?.fork).toBeUndefined();
  });

  it("marks the fork by presence when it is", () => {
    const request = buildCreateWorkspaceRequest({
      ...standard(),
      parent: { workspace: WORKSPACE, fork: true },
    });
    expect(request.parent?.fork).toBeDefined();
  });

  it.each(["p05", "p1", "p2", "p3"] as const)("sets the %s priority level", (level) => {
    expect(buildCreateWorkspaceRequest({ ...standard(), priority: level }).priority?.level.case).toBe(
      level,
    );
  });

  it("leaves the consent unset when it was not given", () => {
    expect(buildCreateWorkspaceRequest(standard()).allowUngated).toBeUndefined();
  });

  it("states the consent by presence when it was", () => {
    expect(
      buildCreateWorkspaceRequest({ ...standard(), allowUngated: true }).allowUngated,
    ).toBeDefined();
  });
});

describe("the form on screen", () => {
  it("carries the hook the integration suite targets", () => {
    const form = drawCreateWorkspaceForm(REPO, sidebarContext());
    expect(form.hasAttribute("data-create-form")).toBe(true);
  });

  it("carries the submit hook the integration suite targets", () => {
    const form = drawCreateWorkspaceForm(REPO, sidebarContext());
    expect(form.querySelector("[data-create-submit]")).not.toBeNull();
  });

  it("shows the standard fields first", () => {
    const form = drawCreateWorkspaceForm(REPO, sidebarContext());
    expect((form.querySelector(".sb-create-one-shot") as HTMLElement).hidden).toBe(true);
  });

  it("swaps the field sets when the one-shot form is picked", () => {
    const form = drawCreateWorkspaceForm(REPO, sidebarContext());
    const oneShot = form.querySelectorAll<HTMLInputElement>("input[name='form']")[1];
    oneShot.checked = true;
    oneShot.dispatchEvent(new Event("change"));
    expect((form.querySelector(".sb-create-standard") as HTMLElement).hidden).toBe(true);
  });

  it("keeps the fork option unavailable until a parent is chosen", () => {
    const form = drawCreateWorkspaceForm(REPO, sidebarContext());
    expect((form.querySelector("input[name='fork']") as HTMLInputElement).disabled).toBe(true);
  });

  it("offers the fork once the parent option is taken", () => {
    const form = drawCreateWorkspaceForm(REPO, sidebarContext());
    const parent = form.querySelector("input[name='parent']") as HTMLInputElement;
    parent.checked = true;
    parent.dispatchEvent(new Event("change"));
    expect((form.querySelector("input[name='fork']") as HTMLInputElement).disabled).toBe(false);
  });

  it("un-forks when the parent option is dropped", () => {
    const form = drawCreateWorkspaceForm(REPO, sidebarContext());
    const parent = form.querySelector("input[name='parent']") as HTMLInputElement;
    const fork = form.querySelector("input[name='fork']") as HTMLInputElement;
    parent.checked = true;
    parent.dispatchEvent(new Event("change"));
    fork.checked = true;
    parent.checked = false;
    parent.dispatchEvent(new Event("change"));
    expect(fork.checked).toBe(false);
  });

  it("leaves the consent unchecked by default", () => {
    const form = drawCreateWorkspaceForm(REPO, sidebarContext());
    expect((form.querySelector("input[name='allow_ungated']") as HTMLInputElement).checked).toBe(
      false,
    );
  });

  it("says a one-shot needs a prompt rather than sending a doomed request", async () => {
    let calls = 0;
    const sc = sidebarContext(
      appContext({
        createWorkspace: () => {
          calls += 1;
          return create(CreateWorkspaceResponseSchema, {
            result: { case: "success", value: { workspace: WORKSPACE } },
          });
        },
      }),
    );
    const form = drawCreateWorkspaceForm(REPO, sc);
    const oneShot = form.querySelectorAll<HTMLInputElement>("input[name='form']")[1];
    oneShot.checked = true;
    oneShot.dispatchEvent(new Event("change"));
    await click(form.querySelector("[data-create-submit]") as Element);
    expect([calls, form.querySelector(".refusal")?.getAttribute("data-arm")]).toEqual([
      0,
      "missing_prompt",
    ]);
  });

  it("closes itself once the daemon created the workspace", async () => {
    const sc = sidebarContext(
      appContext({
        createWorkspace: () =>
          create(CreateWorkspaceResponseSchema, {
            result: { case: "success", value: { workspace: WORKSPACE } },
          }),
      }),
    );
    const form = drawCreateWorkspaceForm(REPO, sc);
    document.body.replaceChildren(form);
    await click(form.querySelector("[data-create-submit]") as Element);
    expect(document.body.querySelector("[data-create-form]")).toBeNull();
  });

  it("stands, with a refusal at its submit, when the daemon refused", async () => {
    const sc = sidebarContext(
      appContext({
        createWorkspace: () =>
          create(CreateWorkspaceResponseSchema, {
            result: { case: "error", value: { cause: { case: "noSlug", value: {} } } },
          }),
      }),
    );
    const form = drawCreateWorkspaceForm(REPO, sc);
    document.body.replaceChildren(form);
    await click(form.querySelector("[data-create-submit]") as Element);
    expect(form.querySelector(".refusal")?.textContent).toBe(
      "the daemon could not derive a name for this workspace",
    );
  });

  it("labels that refusal with the cause's own arm", async () => {
    const sc = sidebarContext(
      appContext({
        createWorkspace: () =>
          create(CreateWorkspaceResponseSchema, {
            result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } },
          }),
      }),
    );
    const form = drawCreateWorkspaceForm(REPO, sc);
    document.body.replaceChildren(form);
    await click(form.querySelector("[data-create-submit]") as Element);
    expect(form.querySelector(".refusal")?.getAttribute("data-arm")).toBe("unknownRepository");
  });
});

/** What each fact-carrying arm must carry for its sentence to be complete. */
const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  briefMissing: { name: "ship-the-rail" },
  baseRefUnresolved: { ref: "origin/nope" },
  worktreeCreationFailed: { detail: "index.lock exists" },
  spawnFailed: { detail: "the shim would not come up" },
  oneShotPolicyMissing: {
    repositoryRoot: "/src/p",
    policyDir: "/src/p/.agent-repl/prompts",
    missingFiles: ["oneshot-success-suffix.md"],
  },
  namingFailed: { model: "haiku", cause: "invalid_answer", attempts: 2, answer: "Fix The Login" },
  insideTemporaryDirectory: { dir: "/private/tmp/scratch", temporaryRoot: "/private/tmp" },
};

describe("CreateWorkspace's typed refusal", () => {
  it.each(oneofArms(CreateWorkspaceErrorSchema, "cause"))("words the %s arm", (arm) => {
    const said = createWorkspaceRefusal({ case: arm, value: CAUSE_FILL[arm] ?? {} } as never);
    expect(said).not.toBe("");
  });

  it("names the workspace whose brief is missing", () => {
    expect(
      createWorkspaceRefusal({ case: "briefMissing", value: { name: "ship-the-rail" } } as never),
    ).toContain("ship-the-rail");
  });

  it("names the base ref that would not resolve", () => {
    expect(
      createWorkspaceRefusal({ case: "baseRefUnresolved", value: { ref: "origin/nope" } } as never),
    ).toContain("origin/nope");
  });

  it("carries git's own detail when the worktree could not be made", () => {
    expect(
      createWorkspaceRefusal({
        case: "worktreeCreationFailed",
        value: { detail: "index.lock exists" },
      } as never),
    ).toContain("index.lock exists");
  });

  it("carries the spawn's own detail when the created session would not start", () => {
    expect(
      createWorkspaceRefusal({
        case: "spawnFailed",
        value: { detail: "the shim would not come up" },
      } as never),
    ).toContain("the shim would not come up");
  });

  it("names the policy directory a repository stating no one-shot policy must write", () => {
    expect(
      createWorkspaceRefusal({
        case: "oneShotPolicyMissing",
        value: {
          repositoryRoot: "/src/p",
          policyDir: "/src/p/.agent-repl/prompts",
          missingFiles: ["oneshot-success-suffix.md"],
        },
      } as never),
    ).toContain("oneshot-success-suffix.md in /src/p/.agent-repl/prompts");
  });

  it("names the policy directory alone when the arm lists no files", () => {
    expect(
      createWorkspaceRefusal({
        case: "oneShotPolicyMissing",
        value: {
          repositoryRoot: "/src/p",
          policyDir: "/src/p/.agent-repl/prompts",
          missingFiles: [],
        },
      } as never),
    ).toContain("write /src/p/.agent-repl/prompts");
  });

  it("names the cause when the workspace could not be named", () => {
    expect(
      createWorkspaceRefusal({
        case: "namingFailed",
        value: { model: "haiku", cause: "timeout", attempts: 2, answer: "" },
      } as never),
    ).toContain("timeout");
  });

  it("carries the model's last answer when it gave one", () => {
    expect(
      createWorkspaceRefusal({
        case: "namingFailed",
        value: { model: "haiku", cause: "invalid_answer", attempts: 2, answer: "Fix The Login" },
      } as never),
    ).toContain("Fix The Login");
  });

  it("names the temporary folder a create was refused for, and why", () => {
    expect(
      createWorkspaceRefusal({
        case: "insideTemporaryDirectory",
        value: { dir: "/private/tmp/scratch", temporaryRoot: "/private/tmp" },
      } as never),
    ).toBe(
      "/private/tmp/scratch is inside the temporary directory /private/tmp; agent-repl does not register temporary folders",
    );
  });

  it("refuses an arm this build does not know", () => {
    expect(() => createWorkspaceRefusal({ case: "somethingNewer", value: {} } as never)).toThrow(
      MalformedView,
    );
  });
});

describe("what the form collects, for the fields a blank form drops", () => {
  it("keeps a model the user typed", () => {
    expect(collect(raw({ model: "opus" }))?.model).toBe("opus");
  });

  it("keeps a priority the user picked", () => {
    expect(collect(raw({ priority: "p1" }))?.priority).toBe("p1");
  });
});

describe("the request the spec becomes, for the fields a blank form drops", () => {
  const standard = (over: Record<string, unknown> = {}): CreateWorkspaceSpec => ({
    repository: REPO,
    form: { case: "standard", ...over },
    allowUngated: false,
  });

  it("carries the base ref the user named", () => {
    const form = buildCreateWorkspaceRequest(standard({ baseRef: "origin/main" })).form;
    expect(form.case === "standard" ? form.value.baseRef : null).toBe("origin/main");
  });

  it("carries the name the user chose", () => {
    const form = buildCreateWorkspaceRequest(standard({ name: "ship-the-rail" })).form;
    expect(form.case === "standard" ? form.value.name : null).toBe("ship-the-rail");
  });

  it("carries the model the user named", () => {
    const spec: CreateWorkspaceSpec = { ...standard(), model: "opus" };
    expect(buildCreateWorkspaceRequest(spec).model).toBe("opus");
  });

  it("sets the priority level the user picked", () => {
    const spec: CreateWorkspaceSpec = { ...standard(), priority: "p2" };
    expect(buildCreateWorkspaceRequest(spec).priority?.level.case).toBe("p2");
  });
});

describe("the section's create control", () => {
  /** A repository section with a header, the shape the roster draws. */
  function section(): { section: HTMLElement; header: HTMLElement } {
    const host = document.createElement("div");
    host.className = "sb-section";
    const header = document.createElement("div");
    header.className = "repo-head";
    host.appendChild(header);
    return { section: host, header };
  }

  it("names the repository it would create in", () => {
    const { section: host } = section();
    expect(drawCreateWorkspaceControl(REPO, host, sidebarContext()).title).toBe(
      "new workspace in /repo/one",
    );
  });

  it("opens the form directly under the section's header", () => {
    const { section: host, header } = section();
    const button = drawCreateWorkspaceControl(REPO, host, sidebarContext());
    header.appendChild(button);
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(header.nextElementSibling?.getAttribute("data-create-form")).toBe("");
  });

  it("appends the form to the section when there is no header to open under", () => {
    const host = document.createElement("div");
    const button = drawCreateWorkspaceControl(REPO, host, sidebarContext());
    host.appendChild(button);
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(host.lastElementChild?.getAttribute("data-create-form")).toBe("");
  });

  it("closes an open form rather than drawing a second one", () => {
    const { section: host, header } = section();
    const button = drawCreateWorkspaceControl(REPO, host, sidebarContext());
    header.appendChild(button);
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(host.querySelectorAll("[data-create-form]").length).toBe(0);
  });
});
