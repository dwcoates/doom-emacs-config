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
    openPr: false,
    selfCertified: false,
    addToMergeQueue: false,
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

  it("takes self-merge as the default finish", () => {
    const form = collect(raw({ oneShotMode: true, oneShotPrompt: "do it" }))?.form;
    expect(form?.case === "oneShot" ? form.finish.case : null).toBe("selfMerge");
  });

  it("takes the PR finish when it is picked", () => {
    const form = collect(raw({ oneShotMode: true, oneShotPrompt: "do it", openPr: true }))?.form;
    expect(form?.case === "oneShot" ? form.finish.case : null).toBe("openPr");
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
      form: { case: "oneShot", prompt: "do it", finish: { case: "selfMerge" } },
      allowUngated: false,
    });
    expect(request.form.case).toBe("oneShot");
  });

  it("carries the one-shot's commission as its prompt", () => {
    const request = buildCreateWorkspaceRequest({
      repository: REPO,
      form: { case: "oneShot", prompt: "do it", finish: { case: "selfMerge" } },
      allowUngated: false,
    });
    expect(said(request.form.case === "oneShot" ? request.form.value.prompt : undefined)).toBe(
      "do it",
    );
  });

  it("finishes a one-shot by self-merging when that arm is chosen", () => {
    const request = buildCreateWorkspaceRequest({
      repository: REPO,
      form: { case: "oneShot", prompt: "do it", finish: { case: "selfMerge" } },
      allowUngated: false,
    });
    expect(request.form.case === "oneShot" ? request.form.value.finish.case : null).toBe(
      "selfMerge",
    );
  });

  it("carries the PR's two flags on the open_pr arm", () => {
    const request = buildCreateWorkspaceRequest({
      repository: REPO,
      form: {
        case: "oneShot",
        prompt: "do it",
        finish: { case: "openPr", selfCertified: true, addToMergeQueue: true },
      },
      allowUngated: false,
    });
    const finish = request.form.case === "oneShot" ? request.form.value.finish : undefined;
    expect(finish?.case === "openPr" ? [finish.value.selfCertified, finish.value.addToMergeQueue] : null).toEqual([
      true,
      true,
    ]);
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

  it("refuses an arm this build does not know", () => {
    expect(() => createWorkspaceRefusal({ case: "somethingNewer", value: {} } as never)).toThrow(
      MalformedView,
    );
  });
});
