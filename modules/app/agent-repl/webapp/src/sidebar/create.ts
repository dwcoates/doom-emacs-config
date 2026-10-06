/**
 * create — the create-workspace form, and the request it builds.
 *
 * TWO CREATION FORMS, ONE REQUEST: the form arm IS the kind of create. A
 * STANDARD create is the ordinary workspace, where every field is optional and
 * the daemon fills in what the user leaves blank; a ONE-SHOT is
 * fire-and-forget, where the prompt is the whole commission and the finish
 * action is chosen up front.
 *
 * PRESENCE, NEVER SENTINELS. Every optional field is OMITTED when the user
 * left it blank — never sent as an empty string, an empty `UserSaid`, or a
 * default level. That is what the request's `optional` markers mean, and it is
 * the whole difference between "the user asked for nothing" and "the user
 * asked for the empty thing".
 *
 * THE FORK IS CONFINED TO THE PARENT by the schema (a fork lives inside
 * `CreateWorkspaceParent`), so a fork without a parent is unrepresentable on
 * the wire — and the form matches that: the fork option is disabled until the
 * parent option is taken.
 *
 * CONSENT IS A PRESENT MESSAGE. `allow_ungated` is unchecked by default and
 * sends nothing when unchecked; a creation that would produce an ungated
 * session is refused without it, which is the point.
 */
import { createControl } from "../control.js";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  CreateWorkspaceRequestSchema,
  CreateWorkspaceResponseSchema,
  type CreateWorkspaceError,
  type CreateWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_workspace_pb";
import type { RepositoryRef, WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { buildUserSaid } from "../composer/composer.js";
import { log } from "../log.js";
import { unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import { fireVerb, type PriorityChoice } from "./verbs.js";

/** What the form collected, before it becomes a request. */
export interface CreateWorkspaceSpec {
  /** The repository's served token, echoed back untouched. */
  repository: RepositoryRef;
  form:
    | {
        case: "standard";
        /** The opening prompt, and the naming input. Blank = omitted. */
        initialPrompt?: string;
        /** The ref to branch from. Blank = the daemon's own resolution. */
        baseRef?: string;
        /** A chosen name. Blank = the daemon mints one. */
        name?: string;
        /** The configured pre-merge prompt. Blank = omitted. */
        beforeWsMerge?: string;
        /** The configured post-merge prompt. Blank = omitted. */
        postprocessingPrompt?: string;
      }
    | { case: "oneShot"; prompt: string };
  /** The spawning parent, and whether its conversation forks with it. */
  parent?: { workspace: WorkspaceRef; fork: boolean };
  /** The session model. Blank = the daemon's default. */
  model?: string;
  /** The creation-time priority. Absent = unprioritized. */
  priority?: Exclude<PriorityChoice, "clear">;
  /** Presence IS the consent to an ungated session. */
  allowUngated: boolean;
}

/**
 * CreateWorkspace, built from what the form collected.
 *
 * Every omission below is deliberate: a blank field contributes NOTHING to the
 * request rather than an empty value, so the daemon sees the same absence the
 * user expressed.
 */
export function buildCreateWorkspaceRequest(spec: CreateWorkspaceSpec): CreateWorkspaceRequest {
  return create(CreateWorkspaceRequestSchema, {
    repository: spec.repository,
    form: creationForm(spec.form),
    ...(spec.parent === undefined
      ? {}
      : {
          parent: {
            workspace: spec.parent.workspace,
            ...(spec.parent.fork ? { fork: {} } : {}),
          },
        }),
    ...(spec.model === undefined ? {} : { model: spec.model }),
    ...(spec.priority === undefined
      ? {}
      : { priority: { level: { case: spec.priority, value: {} } } }),
    ...(spec.allowUngated ? { allowUngated: {} } : {}),
  });
}

/**
 * The form arm, which IS the kind of create.
 *
 * A switch rather than a computed case name: the two forms are two distinct
 * messages with two distinct field sets, and spelling them out is what makes a
 * third form a compile error here.
 */
function creationForm(
  form: CreateWorkspaceSpec["form"],
): MessageInitShape<typeof CreateWorkspaceRequestSchema>["form"] {
  if (form.case === "standard") {
    return {
      case: "standard",
      value: {
        ...(form.initialPrompt === undefined
          ? {}
          : { initialPrompt: buildUserSaid(form.initialPrompt) }),
        ...(form.baseRef === undefined ? {} : { baseRef: form.baseRef }),
        ...(form.name === undefined ? {} : { name: form.name }),
        ...(form.beforeWsMerge === undefined && form.postprocessingPrompt === undefined
          ? {}
          : {
              mergeActions: {
                ...(form.beforeWsMerge === undefined
                  ? {}
                  : { beforeWsMerge: buildUserSaid(form.beforeWsMerge) }),
                ...(form.postprocessingPrompt === undefined
                  ? {}
                  : { postprocessingPrompt: buildUserSaid(form.postprocessingPrompt) }),
              },
            }),
      },
    };
  }
  // A one-shot IS its prompt and nothing else: what happens on completion is
  // the repository's own directive, which the daemon appends and the agent
  // carries out, so there is no finish for the form to collect.
  return { case: "oneShot", value: { prompt: buildUserSaid(form.prompt) } };
}

/**
 * The "+" in a repository section's header, and the form it opens.
 *
 * The form lands INSIDE the section, directly under the header, so it opens
 * downward from the control that asked for it and cannot clip off the top of
 * the rail.
 */
export function drawCreateWorkspaceControl(
  repository: RepositoryRef,
  section: HTMLElement,
  sc: SidebarContext,
): HTMLElement {
  const button = createControl();
  button.className = "sb-add";
  button.textContent = "+";
  button.title = `new workspace in ${repository.dir}`;
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    const open = section.querySelector(":scope > .sb-create");
    if (open !== null) {
      open.remove();
      return;
    }
    const form = drawCreateWorkspaceForm(repository, sc);
    const header = button.closest(".repo-head");
    if (header === null) section.appendChild(form);
    else header.after(form);
  });
  return button;
}

/** The form itself: the standard fields, the one-shot fields, and the shared ones. */
export function drawCreateWorkspaceForm(
  repository: RepositoryRef,
  sc: SidebarContext,
): HTMLElement {
  log.debug("drawing the create-workspace form", {
    operation: "sidebar.create.form",
    context: { repository: repository.id },
  });
  const form = document.createElement("div");
  form.className = "sb-create";
  // The two hooks the integration suite targets on this form; every field is
  // reached by its `name` instead, so the form adds no vocabulary of its own.
  form.setAttribute("data-create-form", "");
  form.addEventListener("click", (event) => event.stopPropagation());

  const mode = radioPair(form, "form", [
    ["standard", "Standard"],
    ["one_shot", "One-shot"],
  ]);

  const standard = block(form, "sb-create-standard");
  const initialPrompt = textarea(standard, "initial_prompt", "initial prompt");
  const baseRef = text(standard, "base_ref", "base ref");
  const name = text(standard, "name", "name");
  const beforeWsMerge = textarea(standard, "before_ws_merge", "pre-merge prompt");
  const postprocessing = textarea(standard, "postprocessing_prompt", "post-merge prompt");

  const oneShot = block(form, "sb-create-one-shot");
  oneShot.hidden = true;
  const oneShotPrompt = textarea(oneShot, "one_shot_prompt", "what it should do");

  const shared = block(form, "sb-create-shared");
  const parent = check(shared, "parent", "spawn from this workspace");
  const fork = check(shared, "fork", "fork the conversation");
  fork.disabled = true;
  const model = text(shared, "model", "model");
  const priority = prioritySelect(shared);
  const allowUngated = check(shared, "allow_ungated", "allow an ungated session");

  // The fork is confined to the parent on the wire, so it is confined in the
  // form too rather than being sent and refused.
  parent.addEventListener("change", () => {
    fork.disabled = !parent.checked;
    if (!parent.checked) fork.checked = false;
  });
  // The arm IS the kind of create, so the two field sets are never both on
  // screen: picking a form shows exactly the fields that form has.
  for (const option of mode) {
    option.addEventListener("change", () => {
      standard.hidden = mode[1].checked;
      oneShot.hidden = !mode[1].checked;
    });
  }

  const submit = createControl();
  submit.className = "sb-form-go";
  submit.setAttribute("data-create-submit", "");
  submit.textContent = "Create";
  submit.addEventListener("click", (event) => {
    event.preventDefault();
    const spec = collect({
      repository,
      workspace: sc.ctx.workspace,
      oneShotMode: mode[1].checked,
      initialPrompt: initialPrompt.value,
      baseRef: baseRef.value,
      name: name.value,
      beforeWsMerge: beforeWsMerge.value,
      postprocessingPrompt: postprocessing.value,
      oneShotPrompt: oneShotPrompt.value,
      parent: parent.checked,
      fork: fork.checked,
      model: model.value,
      priority: priority.value,
      allowUngated: allowUngated.checked,
    });
    if (spec === null) {
      // A one-shot IS its prompt: there is nothing to send without one, so the
      // form says so rather than provoking a refusal.
      submit.after(missingPromptNote());
      return;
    }
    void fireVerb(submit, {
      sc,
      rpc: "CreateWorkspace",
      call: (client) => client.createWorkspace(buildCreateWorkspaceRequest(spec)),
      schema: CreateWorkspaceResponseSchema,
      refusalText: (cause) => createWorkspaceRefusal(cause as CreateWorkspaceCause),
    }).then((ok) => {
      // The new row arrives on the roster push; the form's only job on success
      // is to get out of the way.
      if (ok) form.remove();
    });
  });
  form.appendChild(submit);
  return form;
}

/** `CreateWorkspaceError`'s cause union, narrowed to a SET arm. */
type CreateWorkspaceCause = NonNullable<CreateWorkspaceError["cause"]> & { case: string };

/**
 * What each of CreateWorkspace's fourteen refusals says.
 *
 * NONE OF THE CROSS-CUTTING FOUR CAN REACH THIS RPC: a creation is addressed
 * to a repository, not to an existing workspace, so every arm here is the
 * endpoint's own and this table is the whole vocabulary.
 *
 * The four that name a fact — the brief's workspace, the unresolved base ref,
 * git's own message — carry it into the sentence, because the form's next move
 * depends on which of them it was.
 */
export function createWorkspaceRefusal(cause: CreateWorkspaceCause): string {
  switch (cause.case) {
    case "ungatedWithoutConsent":
      return "this repository is ungated: tick the consent box to create here anyway";
    case "noSlug":
      return "the daemon could not derive a name for this workspace";
    case "forkParentHasNoConversation":
      return "the parent workspace has no conversation to fork";
    case "briefMissing":
      return `the brief for ${cause.value.name} was not found`;
    case "unknownRepository":
      return "the daemon does not know this repository";
    case "unknownParent":
      return "the daemon does not know the parent workspace";
    case "baseRefUnresolved":
      return `the base ref ${cause.value.ref} could not be resolved`;
    case "worktreeCreationFailed":
      return `the worktree could not be created: ${cause.value.detail}`;
    case "spawnFailed":
      return `the workspace was created but its session would not start: ${cause.value.detail}`;
    case "oneShotPolicyMissing":
      return `${cause.value.repositoryRoot} states no one-shot policy: write ${
        cause.value.missingFiles.length > 0
          ? `${cause.value.missingFiles.join(", ")} in ${cause.value.policyDir}`
          : cause.value.policyDir
      }`;
    case "namingFailed":
      return `the workspace could not be named (${cause.value.cause}, ${cause.value.attempts} attempt${
        cause.value.attempts === 1 ? "" : "s"
      })${cause.value.answer !== "" ? `: the model answered "${cause.value.answer}"` : ""}`;
    case "insideTemporaryDirectory":
      return `${cause.value.dir} is inside the temporary directory ${cause.value.temporaryRoot}; agent-repl does not register temporary folders`;
    default: {
      const other: { case: string } = cause;
      return unreachableArm("CreateWorkspaceError.cause", other.case);
    }
  }
}

/** What the form's controls read as, before blanks are dropped. */
interface RawForm {
  repository: RepositoryRef;
  workspace: WorkspaceRef;
  oneShotMode: boolean;
  initialPrompt: string;
  baseRef: string;
  name: string;
  beforeWsMerge: string;
  postprocessingPrompt: string;
  oneShotPrompt: string;
  parent: boolean;
  fork: boolean;
  model: string;
  priority: string;
  allowUngated: boolean;
}

/**
 * Turn the raw form into a spec, dropping every blank.
 *
 * Answers null for the ONE required field in either form — a one-shot's
 * prompt — so the caller can say so at the form instead of sending a request
 * that cannot succeed.
 */
export function collect(raw: RawForm): CreateWorkspaceSpec | null {
  const trimmed = (value: string): string | undefined => {
    const text = value.trim();
    return text === "" ? undefined : text;
  };
  const spec: CreateWorkspaceSpec = {
    repository: raw.repository,
    form: raw.oneShotMode
      ? { case: "oneShot", prompt: raw.oneShotPrompt.trim() }
      : {
          case: "standard",
          initialPrompt: trimmed(raw.initialPrompt),
          baseRef: trimmed(raw.baseRef),
          name: trimmed(raw.name),
          beforeWsMerge: trimmed(raw.beforeWsMerge),
          postprocessingPrompt: trimmed(raw.postprocessingPrompt),
        },
    allowUngated: raw.allowUngated,
  };
  if (spec.form.case === "oneShot" && spec.form.prompt === "") return null;
  if (raw.parent) spec.parent = { workspace: raw.workspace, fork: raw.fork };
  const model = trimmed(raw.model);
  if (model !== undefined) spec.model = model;
  if (raw.priority !== "") spec.priority = raw.priority as Exclude<PriorityChoice, "clear">;
  return spec;
}

function missingPromptNote(): HTMLElement {
  const note = document.createElement("div");
  note.className = "refusal sb-refusal";
  note.setAttribute("data-arm", "missing_prompt");
  note.textContent = "a one-shot needs a prompt";
  return note;
}

function block(host: HTMLElement, className: string): HTMLElement {
  const element = document.createElement("div");
  element.className = className;
  host.appendChild(element);
  return element;
}

function text(host: HTMLElement, name: string, placeholder: string): HTMLInputElement {
  const input = document.createElement("input");
  input.type = "text";
  input.name = name;
  input.placeholder = placeholder;
  host.appendChild(labelled(placeholder, input));
  return input;
}

function textarea(host: HTMLElement, name: string, placeholder: string): HTMLTextAreaElement {
  const area = document.createElement("textarea");
  area.name = name;
  area.placeholder = placeholder;
  area.rows = 2;
  host.appendChild(labelled(placeholder, area));
  return area;
}

function check(host: HTMLElement, name: string, text: string): HTMLInputElement {
  const input = document.createElement("input");
  input.type = "checkbox";
  input.name = name;
  const label = document.createElement("label");
  label.className = "sb-create-check";
  label.appendChild(input);
  label.appendChild(document.createTextNode(text));
  host.appendChild(label);
  return input;
}

function radioPair(
  host: HTMLElement,
  name: string,
  options: ReadonlyArray<readonly [string, string]>,
): HTMLInputElement[] {
  const row = document.createElement("div");
  row.className = "sb-create-radios";
  const inputs: HTMLInputElement[] = [];
  for (const [value, text] of options) {
    const input = document.createElement("input");
    input.type = "radio";
    input.name = name;
    input.value = value;
    input.checked = inputs.length === 0;
    const label = document.createElement("label");
    label.appendChild(input);
    label.appendChild(document.createTextNode(text));
    row.appendChild(label);
    inputs.push(input);
  }
  host.appendChild(row);
  return inputs;
}

function prioritySelect(host: HTMLElement): HTMLSelectElement {
  const select = document.createElement("select");
  select.name = "priority";
  for (const [value, text] of [
    ["", "no priority"],
    ["p05", "P0.5"],
    ["p1", "P1"],
    ["p2", "P2"],
    ["p3", "P3"],
  ] as const) {
    const option = document.createElement("option");
    option.value = value;
    option.textContent = text;
    select.appendChild(option);
  }
  host.appendChild(labelled("priority", select));
  return select;
}

function labelled(text: string, control: HTMLElement): HTMLElement {
  const label = document.createElement("label");
  label.className = "sb-create-field";
  const caption = document.createElement("span");
  caption.textContent = text;
  label.appendChild(caption);
  label.appendChild(control);
  return label;
}
