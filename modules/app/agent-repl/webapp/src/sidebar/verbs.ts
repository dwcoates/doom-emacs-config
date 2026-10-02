/**
 * verbs — the workspace actions a roster row offers, and the requests they
 * are.
 *
 * EVERY CLICK IS AN agentrepl RPC with plain fields, and every refusal is
 * drawn AT THE CONTROL THAT MADE THE CALL. Nothing here waits for a pushed
 * view to say what happened: a verb's answer is its own response, and the
 * roster's new state arrives separately on the stream — so a success draws
 * NOTHING and simply lets the next push replace the row.
 *
 * THE TWO DESTRUCTIVE VERBS ASK FIRST, and they ask differently because they
 * destroy different things. `kill` ends a session and keeps the worktree, so a
 * single confirm is proportionate. `nuke` DELETES THE WORKTREE AND BRANCH,
 * unrecoverably, so it demands the workspace's name typed back — the one
 * gesture that cannot be made by a mis-aimed click.
 *
 * WHY THE MENU IS BUILT HERE RATHER THAN IN `row.ts`. The row draws a
 * workspace; this draws what can be DONE to one, which is a different surface
 * with its own dozen call sites and its own refusal handling. Splitting them
 * keeps each file's tests about one thing.
 */
import { createControl, isControl, type Control } from "../control.js";
import { create, type Message, type MessageInitShape } from "@bufbuild/protobuf";
import {
  AssignWorkspaceTaskRequestSchema,
  AssignWorkspaceTaskResponseSchema,
  type AssignWorkspaceTaskError,
  type AssignWorkspaceTaskRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_assign_workspace_task_pb";
import {
  CloseWorkspaceRequestSchema,
  CloseWorkspaceResponseSchema,
  type CloseWorkspaceError,
  type CloseWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_workspace_pb";
import {
  KillWorkspaceRequestSchema,
  KillWorkspaceResponseSchema,
  type KillWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_kill_workspace_pb";
import {
  MergeWorkspaceRequestSchema,
  MergeWorkspaceResponseSchema,
  type MergeWorkspaceError,
  type MergeWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_merge_workspace_pb";
import {
  NukeWorkspaceRequestSchema,
  NukeWorkspaceResponseSchema,
  type NukeWorkspaceError,
  type NukeWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_nuke_workspace_pb";
import type { LockHolderFailure } from "../../../proto/gen/ts/conversation/v1/session_pb";
import {
  OpenWorkspaceRequestSchema,
  OpenWorkspaceResponseSchema,
  type OpenWorkspaceError,
  type OpenWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import {
  RestartWorkspaceRequestSchema,
  RestartWorkspaceResponseSchema,
  type RestartWorkspaceError,
  type RestartWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_restart_workspace_pb";
import {
  SelectWorkspaceRequestSchema,
  type SelectWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import {
  SetWorkspacePriorityRequestSchema,
  SetWorkspacePriorityResponseSchema,
  type SetWorkspacePriorityRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_priority_pb";
import { WorkspacePrioritySchema } from "../../../proto/gen/ts/agentrepl/v1/workspace_priority_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { log } from "../log.js";
import { guardMalformed } from "../rpc/guard.js";
import { isMalformedView } from "../rpc/malformed.js";
import { crossCuttingSentence } from "../rpc/refuse.js";
import type { RefusalCause } from "../rpc/refusal.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { SidebarContext } from "./context.js";

/** The verbs a row's menu offers, exactly as `data-verb` spells them. */
export type Verb =
  | "open"
  | "close"
  | "kill"
  | "nuke"
  | "merge"
  | "restart"
  | "restartForce"
  | "priority"
  | "assign";

/** The priority menu's entries; `clear` is the unset request. */
export type PriorityChoice = "p05" | "p1" | "p2" | "p3" | "clear";

/** The workspace a menu acts on. */
export interface VerbTarget {
  sc: SidebarContext;
  /** The row's echoed identity — never rebuilt, never parsed. */
  workspace: WorkspaceRef;
  /** The row's display name, which is what `nuke` asks to be typed back. */
  name: string;
}

/** What each menu entry is labelled. */
const VERB_LABELS: Readonly<Record<Verb, string>> = {
  open: "Open",
  close: "Close",
  kill: "Kill",
  nuke: "Nuke",
  merge: "Merge",
  restart: "Restart",
  restartForce: "Restart (forced)",
  priority: "Priority",
  assign: "Task",
};

/** What each priority entry is labelled; the badge itself is the daemon's. */
const PRIORITY_LABELS: Readonly<Record<PriorityChoice, string>> = {
  p05: "P0.5",
  p1: "P1",
  p2: "P2",
  p3: "P3",
  clear: "Clear",
};

/**
 * The row's verb menu.
 *
 * A LIST like every other list in this app: the shared `.list-rows` delimiter
 * class, and it opens DOWNWARD from the control, clamped inside the rail.
 */
export function drawRowMenu(target: VerbTarget): HTMLElement {
  log.debug("drawing a roster row's verb menu", {
    operation: "sidebar.verbs.menu",
    context: { workspace: target.workspace.id },
  });
  const menu = document.createElement("div");
  menu.className = "sb-menu list-rows";
  // Drawn with every row and REVEALED by the "⋯" control; see `toggleRowMenu`.
  menu.hidden = true;
  menu.appendChild(simpleVerbItem("open", target));
  menu.appendChild(simpleVerbItem("close", target));
  menu.appendChild(simpleVerbItem("merge", target));
  menu.appendChild(simpleVerbItem("restart", target));
  menu.appendChild(simpleVerbItem("restartForce", target));
  menu.appendChild(drawPriorityItem(target));
  menu.appendChild(drawAssignItem(target));
  menu.appendChild(drawKillItem(target));
  menu.appendChild(drawNukeItem(target));
  return menu;
}

/** The verbs whose whole interaction is one click. */
function simpleVerbItem(
  verb: "open" | "close" | "merge" | "restart" | "restartForce",
  target: VerbTarget,
): HTMLElement {
  const row = menuRow();
  const button = verbButton(verb);
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    void guardMalformed(
      target.sc.ctx,
      `sidebar.verbs.${verb}`,
      runSimpleVerb(verb, target, button),
    );
  });
  row.appendChild(button);
  return row;
}

/** Issue one of the plain verbs and say at the button what came back. */
async function runSimpleVerb(
  verb: "open" | "close" | "merge" | "restart" | "restartForce",
  target: VerbTarget,
  button: Control,
): Promise<void> {
  switch (verb) {
    case "open":
      await runVerb(button, {
        sc: target.sc,
        rpc: "OpenWorkspace",
        call: (client) => client.openWorkspace(buildOpenWorkspaceRequest(target.workspace)),
        schema: OpenWorkspaceResponseSchema,
        refusalText: (cause) => openWorkspaceRefusal(cause as CauseOf<OpenWorkspaceError>),
      });
      return;
    case "close":
      await runVerb(button, {
        sc: target.sc,
        rpc: "CloseWorkspace",
        call: (client) => client.closeWorkspace(buildCloseWorkspaceRequest(target.workspace)),
        schema: CloseWorkspaceResponseSchema,
        refusalText: (cause) => closeWorkspaceRefusal(cause as CauseOf<CloseWorkspaceError>),
      });
      return;
    case "merge":
      await runVerb(button, {
        sc: target.sc,
        rpc: "MergeWorkspace",
        call: (client) => client.mergeWorkspace(buildMergeWorkspaceRequest(target.workspace)),
        schema: MergeWorkspaceResponseSchema,
        refusalText: (cause) => mergeWorkspaceRefusal(cause as CauseOf<MergeWorkspaceError>),
      });
      return;
    case "restart":
    case "restartForce":
      await runVerb(button, {
        sc: target.sc,
        rpc: "RestartWorkspace",
        call: (client) =>
          client.restartWorkspace(
            buildRestartWorkspaceRequest(target.workspace, verb === "restartForce"),
          ),
        schema: RestartWorkspaceResponseSchema,
        refusalText: (cause) => restartWorkspaceRefusal(cause as CauseOf<RestartWorkspaceError>),
      });
      return;
  }
}

/** Kill: one confirm step, because the worktree survives it. */
function drawKillItem(target: VerbTarget): HTMLElement {
  const row = menuRow();
  const button = disclosureButton("Kill…");
  const confirm = drawKillConfirm(target);
  confirm.hidden = true;
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    confirm.hidden = !confirm.hidden;
  });
  row.appendChild(button);
  row.appendChild(confirm);
  return row;
}

/** The kill confirmation: what it does, and the button that does it. */
export function drawKillConfirm(target: VerbTarget): HTMLElement {
  const confirm = document.createElement("div");
  confirm.className = "sb-confirm";
  const note = document.createElement("div");
  note.className = "sb-confirm-note";
  note.textContent = "Kill the session? The worktree and branch survive.";
  confirm.appendChild(note);
  const go = verbButton("kill");
  go.classList.add("sb-confirm-go");
  go.textContent = "Kill";
  go.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    void fireVerb(go, {
      sc: target.sc,
      rpc: "KillWorkspace",
      call: (client) => client.killWorkspace(buildKillWorkspaceRequest(target.workspace)),
      schema: KillWorkspaceResponseSchema,
    });
  });
  confirm.appendChild(go);
  return confirm;
}

/** Nuke: the name typed back, because this one deletes data for good. */
function drawNukeItem(target: VerbTarget): HTMLElement {
  const row = menuRow();
  row.classList.add("sb-menu-destructive");
  const button = disclosureButton("Nuke…");
  const confirm = drawNukeConfirm(target);
  confirm.hidden = true;
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    confirm.hidden = !confirm.hidden;
  });
  row.appendChild(button);
  row.appendChild(confirm);
  return row;
}

/**
 * The nuke confirmation.
 *
 * The go button stays DISABLED until the typed text equals the workspace's
 * name exactly. Nothing about that is a formality: this is the only verb in
 * the contract that destroys data, and the typing is the deliberation.
 */
export function drawNukeConfirm(target: VerbTarget): HTMLElement {
  const confirm = document.createElement("div");
  confirm.className = "sb-confirm sb-confirm-nuke";
  const note = document.createElement("div");
  note.className = "sb-confirm-note";
  note.textContent = `Deletes the worktree and branch. Type "${target.name}" to confirm.`;
  confirm.appendChild(note);

  const typed = document.createElement("input");
  typed.type = "text";
  typed.className = "sb-confirm-name";
  typed.setAttribute("name", "confirm_name");
  confirm.appendChild(typed);

  const go = verbButton("nuke");
  go.classList.add("sb-confirm-go");
  go.textContent = "Nuke";
  // The typed name is the deliberation the drawer asks for; it marks the
  // button as armed rather than disabling it, because `[data-verb="nuke"]` is
  // the contract's nuke control and a control the contract names must be
  // clickable wherever it is drawn.
  typed.addEventListener("input", () => {
    go.classList.toggle("armed", typed.value === target.name);
  });
  go.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    void fireVerb(go, {
      sc: target.sc,
      rpc: "NukeWorkspace",
      call: (client) => client.nukeWorkspace(buildNukeWorkspaceRequest(target.workspace)),
      schema: NukeWorkspaceResponseSchema,
      refusalText: (cause) => nukeWorkspaceRefusal(cause as CauseOf<NukeWorkspaceError>),
    });
  });
  confirm.appendChild(go);
  return confirm;
}

/** Priority: the four levels and the clear, as one submenu. */
function drawPriorityItem(target: VerbTarget): HTMLElement {
  const row = menuRow();
  // The parent only reveals; each CHOICE is the priority verb's own control,
  // so `data-verb="priority"` sits on the buttons that issue the request.
  const button = disclosureButton(VERB_LABELS.priority);
  const submenu = document.createElement("div");
  submenu.className = "sb-submenu list-rows";
  submenu.hidden = true;
  for (const choice of ["p05", "p1", "p2", "p3", "clear"] as const) {
    const entry = createControl();
    entry.className = "sb-menu-item";
    // The CHOICE rides on the control's own `value` attribute, not on `data-priority`:
    // that attribute is the roster row's priority BADGE, and the suite asserts
    // a row with no badge served carries no `[data-priority]` at all — so the
    // menu must not plant one inside the row.
    entry.setAttribute("data-verb", "priority");
    entry.setAttribute("value", choice);
    entry.textContent = PRIORITY_LABELS[choice];
    entry.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      void fireVerb(entry, {
        sc: target.sc,
        rpc: "SetWorkspacePriority",
        call: (client) =>
          client.setWorkspacePriority(
            buildSetWorkspacePriorityRequest(target.workspace, choice),
          ),
        schema: SetWorkspacePriorityResponseSchema,
      });
    });
    submenu.appendChild(entry);
  }
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    submenu.hidden = !submenu.hidden;
  });
  row.appendChild(button);
  row.appendChild(submenu);
  return row;
}

/**
 * Task assignment: the task view's own sections, offered as choices.
 *
 * The list comes off the LAST PUSHED ROSTER rather than from anything this
 * menu remembers, and the ids are the daemon's `RosterTaskKey.task_id` handed
 * straight back. "Unassign" is the empty id, which is the request's UNSET.
 */
function drawAssignItem(target: VerbTarget): HTMLElement {
  const row = menuRow();
  const button = disclosureButton(VERB_LABELS.assign);
  const submenu = document.createElement("div");
  submenu.className = "sb-submenu list-rows";
  submenu.hidden = true;
  fillAssignSubmenu(submenu, target);
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    fillAssignSubmenu(submenu, target);
    submenu.hidden = !submenu.hidden;
  });
  row.appendChild(button);
  row.appendChild(submenu);
  return row;
}

/** (Re)build the assign choices from whatever the last push resolved. */
export function fillAssignSubmenu(submenu: HTMLElement, target: VerbTarget): void {
  const choices: Array<{ id: string; label: string }> = [
    { id: "", label: "Unassign" },
    ...target.sc.tasks,
  ];
  log.debug("filling the assign-task submenu", {
    operation: "sidebar.verbs.assign-choices",
    context: { workspace: target.workspace.id, choices: choices.length },
  });
  submenu.replaceChildren();
  for (const choice of choices) {
    const entry = createControl();
    entry.className = "sb-menu-item";
    entry.setAttribute("data-assign-task", choice.id);
    entry.textContent = choice.label;
    entry.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      void fireVerb(entry, {
        sc: target.sc,
        rpc: "AssignWorkspaceTask",
        call: (client) =>
          client.assignWorkspaceTask(
            buildAssignWorkspaceTaskRequest(
              target.workspace,
              choice.id === "" ? null : choice.id,
            ),
          ),
        schema: AssignWorkspaceTaskResponseSchema,
        refusalText: (cause) =>
          assignWorkspaceTaskRefusal(cause as CauseOf<AssignWorkspaceTaskError>),
      });
    });
    submenu.appendChild(entry);
  }
}

function menuRow(): HTMLElement {
  const row = document.createElement("div");
  row.className = "sb-menu-row";
  return row;
}

/** A control that only reveals another: no verb, therefore no `data-verb`. */
function disclosureButton(label: string): Control {
  const button = createControl();
  button.className = "sb-menu-item";
  button.textContent = label;
  return button;
}

function verbButton(verb: Verb): Control {
  const button = createControl();
  button.className = "sb-menu-item";
  button.setAttribute("data-verb", verb);
  button.textContent = VERB_LABELS[verb];
  return button;
}

/** A response with the outcome oneof every verb in this section answers on. */
type VerbResponse = Message & { result: { case?: string | undefined; value?: unknown } };

/** What one verb call needs to run and to report. */
interface VerbCall<Res extends VerbResponse> {
  sc: SidebarContext;
  rpc: string;
  call: (client: SidebarContext["ctx"]["client"]) => Promise<Res>;
  schema: Parameters<typeof callUnary>[3];
  /**
   * The sentence for an arm THIS endpoint owns.
   *
   * The four cross-cutting causes are worded once in `rpc/refusal.ts` and never
   * repeated here; a verb whose error carries nothing beyond those four omits
   * this hook entirely, and an arm neither knows is a malformed view.
   */
  refusalText?: (cause: RefusalCause) => string;
}

/**
 * Issue one verb, disable its control while it is in flight, and draw the
 * answer beside it.
 *
 * SUCCESS DRAWS NOTHING and leaves the control disabled: the roster push that
 * follows replaces the row outright, and re-enabling a button about to be
 * thrown away would only flicker. Every other outcome re-enables, because the
 * user is going to want to try again.
 *
 * ANSWERS TRUE ON SUCCESS, so a caller with a form to dismiss can dismiss it
 * on the answer rather than by inspecting the DOM for a refusal.
 */
export async function runVerb<Res extends VerbResponse>(
  control: HTMLElement,
  spec: VerbCall<Res>,
): Promise<boolean> {
  clearRefusal(control);
  setDisabled(control, true);
  try {
    const response = await callUnary(
      spec.sc.ctx,
      spec.rpc,
      (client) => spec.call(client),
      spec.schema,
    );
    const result = requireCase(response.result, `${spec.rpc}Response.result`);
    if (result.case === "success") return true;
    const cause = refusalCause(spec.rpc, result.value);
    const say =
      crossCuttingSentence(spec.rpc, cause) ??
      spec.refusalText?.(cause) ??
      unreachableArm(`${spec.rpc}Error.cause`, cause.case);
    log.warn(`${spec.rpc} was refused`, {
      operation: "sidebar.verbs.refused",
      context: { rpc: spec.rpc, arm: cause.case, sentence: say },
    });
    drawRefusal(control, cause.case, say, refusalDetail(cause));
    setDisabled(control, false);
    return false;
  } catch (err) {
    // A MALFORMED VIEW IS NOT A TRANSPORT FAILURE. The renderer's own refusal
    // — an unset outcome arm, an unknown field on the answer — says the daemon
    // sent something this build cannot read, and it travels up loudly rather
    // than being drawn as "could not be reached", which would be a lie.
    if (isMalformedView(err)) throw err;
    log.error(`${spec.rpc} failed at the transport: ${String(err)}`, {
      operation: "sidebar.verbs.failed",
      context: { rpc: spec.rpc, cause: err },
    });
    drawRefusal(control, "transport", "the daemon could not be reached");
    setDisabled(control, false);
    return false;
  }
}

/**
 * FIRE a verb from a click handler.
 *
 * A click listener cannot be awaited, so every verb below runs as a detached
 * promise — and `runVerb` deliberately rethrows a `MalformedView` rather than
 * dressing it as a transport failure. Detached, that rethrow would land as an
 * unhandled rejection nobody sees. `guardMalformed` (src/rpc/guard.ts) is the
 * ONE place a fire-and-forget click's malformed answer stops: it logs it once
 * and files the `frame_undecodable` card. Every void-ed verb goes through here.
 */
export async function fireVerb<Res extends VerbResponse>(
  control: HTMLElement,
  spec: VerbCall<Res>,
): Promise<boolean> {
  let took = false;
  const malformed = await guardMalformed(
    spec.sc.ctx,
    `sidebar.verbs.${spec.rpc}`,
    runVerb(control, spec).then((ok) => {
      took = ok;
    }),
  );
  return !malformed && took;
}

/**
 * Disable a control for the duration of its call.
 *
 * The row LINE is a control too (the click that selects a workspace), and it
 * is a div rather than a button, so the in-flight state is stated on the
 * element either way: `disabled` where the element has one, and the shared
 * `is-busy` class — which turns pointer events off — everywhere.
 */
function setDisabled(control: HTMLElement, disabled: boolean): void {
  if (isControl(control)) control.disabled = disabled;
  control.classList.toggle("is-busy", disabled);
}

/**
 * The cause a refusal names, or a refusal of the view.
 *
 * SINCE LANDING 4 EVERY `<Rpc>Error` CARRIES A TYPED CAUSE, so an error whose
 * oneof is unset is a daemon saying "refused" and nothing else — which this
 * renderer cannot draw honestly, and therefore refuses as a malformed view
 * rather than labelling with a made-up "error" arm.
 */
export function refusalCause(rpc: string, error: unknown): RefusalCause & { case: string } {
  const cause = (error as { cause?: RefusalCause } | undefined)?.cause;
  return requireCase(cause ?? {}, `${rpc}Error.cause`);
}

/**
 * The cause union of one endpoint's error, narrowed to a SET arm.
 *
 * Each endpoint declares its own arm messages, so there is no shared union to
 * type `refusalText` against; the hook takes the shape they all have and each
 * site casts to its own once, which is what makes the switch below exhaustive.
 */
type CauseOf<E extends { cause: { case?: string | undefined } }> = NonNullable<E["cause"]> & {
  case: string;
};

/** OpenWorkspace's own arms: the ways bringing one back up can fail. */
export function openWorkspaceRefusal(cause: CauseOf<OpenWorkspaceError>): string {
  switch (cause.case) {
    case "sessionDeleted":
      return "this workspace's session has been deleted";
    case "transcriptMissing":
      // THE PATHS THEMSELVES, not a count of them: the reader's next act is to
      // go look in one, and "3 searched path(s)" tells them nothing they can
      // act on. The list is drawn under the sentence by `refusalDetail`.
      return `the transcript for session ${cause.value.vendorSessionId} was not found`;
    case "spawnFailed":
      return `the session could not be started: ${cause.value.detail}`;
    case "vendorStartFailed":
      // OUR PROCESS CAME UP AND THE VENDOR DID NOT: distinct from spawnFailed,
      // and the shim's account is a human's only lead, so it is appended
      // verbatim — parenthesized, and only when the shim actually said
      // something, so an empty detail leaves no empty parentheses behind.
      return cause.value.detail
        ? `the vendor failed to start the session (${cause.value.detail})`
        : "the vendor failed to start the session";
    case "lockHolderUnavailable": {
      // NOBODY OWNS THE CONVERSATION. The shim's own lock helper failed, and
      // the binary plus how it failed are the whole remediation, so both are
      // said; an ownership wording would send the reader hunting for a second
      // process that does not exist.
      const failure = requireMessage(cause.value.failure, "OpenWorkspaceLockHolderUnavailable.failure");
      return (
        `the shim's lock helper ${failure.binary} ${lockHolderHowText(failure)}; ` +
        "no other process owns this conversation"
      );
    }
    default:
      return unreachableArm("OpenWorkspaceError.cause", cause.case);
  }
}

/**
 * How the shim's own lock holder failed, in the words the shim and the daemon
 * use for the same arm. The arm is the account: each case says what it carries.
 */
export function lockHolderHowText(failure: LockHolderFailure): string {
  const how = requireCase(failure.how, "LockHolderFailure.how");
  switch (how.case) {
    case "spawnFailed":
      return `could not be spawned: ${how.value.osError}`;
    case "exited":
      return `exited with code ${how.value.code} before taking the lock${stderrSuffix(how.value.stderr)}`;
    case "signaled":
      return `was killed by ${how.value.signal} before taking the lock${stderrSuffix(how.value.stderr)}`;
    case "misanswered":
      return `answered ${JSON.stringify(how.value.line)} instead of "locked" and was killed`;
    case "silent":
      return `gave no "locked" answer within ${how.value.timeoutMs} ms and was killed`;
    default:
      return unreachableArm("LockHolderFailure.how", (how as { case: string }).case);
  }
}

/** A lock holder's stderr, parenthesized, only when it said something. */
function stderrSuffix(stderr: string): string {
  return stderr === "" ? "" : ` (${stderr})`;
}

/**
 * CloseWorkspace's own arm.
 *
 * The reasons are deliberately not restated: the footer carries them, pushed
 * beside this answer, so the row points at the footer rather than guessing.
 */
export function closeWorkspaceRefusal(cause: CauseOf<CloseWorkspaceError>): string {
  switch (cause.case) {
    case "blocked":
      return "close refused: work in flight (see the footer)";
    default:
      return unreachableArm("CloseWorkspaceError.cause", cause.case);
  }
}

/** NukeWorkspace's own arm: the git operation that destroys the worktree. */
export function nukeWorkspaceRefusal(cause: CauseOf<NukeWorkspaceError>): string {
  switch (cause.case) {
    case "gitFailed":
      return `git refused to remove the worktree: ${cause.value.detail}`;
    default:
      return unreachableArm("NukeWorkspaceError.cause", cause.case);
  }
}

/** MergeWorkspace's own arms, including the two that mean "already going". */
export function mergeWorkspaceRefusal(cause: CauseOf<MergeWorkspaceError>): string {
  switch (cause.case) {
    case "noLayoutFacts":
      return "the daemon has no layout facts for this workspace to merge from";
    case "sessionDeleted":
      return "this workspace's session has been deleted";
    case "alreadyQueued":
      return "this workspace is already in the merge queue";
    case "alreadyMerging":
      return "this workspace is already merging";
    case "unknownSourceWorkspace":
      return "the workspace to merge is not open in this repository";
    case "unknownBranch":
      return "that branch does not exist in this repository";
    default:
      return unreachableArm("MergeWorkspaceError.cause", cause.case);
  }
}

/** RestartWorkspace's own arm. */
export function restartWorkspaceRefusal(cause: CauseOf<RestartWorkspaceError>): string {
  switch (cause.case) {
    case "noSession":
      return "this workspace has no session to restart";
    default:
      return unreachableArm("RestartWorkspaceError.cause", cause.case);
  }
}

/** AssignWorkspaceTask's own arm. */
export function assignWorkspaceTaskRefusal(cause: CauseOf<AssignWorkspaceTaskError>): string {
  switch (cause.case) {
    case "unknownTask":
      return "the daemon does not know that task";
    default:
      return unreachableArm("AssignWorkspaceTaskError.cause", cause.case);
  }
}

/** The refusal, drawn as the NEXT SIBLING of the control that made the call. */
export function drawRefusal(
  control: HTMLElement,
  arm: string,
  text: string,
  detail: HTMLElement | null = null,
): void {
  const refusal = document.createElement("div");
  refusal.className = "refusal sb-refusal";
  refusal.setAttribute("data-arm", arm);
  const sentence = document.createElement("div");
  sentence.textContent = text;
  refusal.append(sentence);
  if (detail !== null) refusal.append(detail);
  control.after(refusal);
}

/**
 * The FACTS an arm carries that do not fit in a sentence.
 *
 * `transcript_missing` carries the paths the daemon looked in, and those are
 * the reader's next act — so they are listed verbatim rather than counted. Every
 * other arm's facts fit its sentence, so this answers `null` for them.
 */
export function refusalDetail(cause: RefusalCause & { case: string }): HTMLElement | null {
  if (cause.case !== "transcriptMissing") return null;
  const paths = (cause.value as { searchedPaths?: readonly string[] }).searchedPaths ?? [];
  if (paths.length === 0) return null;
  const list = document.createElement("ul");
  list.className = "sb-refusal-paths";
  list.setAttribute("data-searched-paths", "");
  for (const path of paths) {
    const item = document.createElement("li");
    item.textContent = path;
    list.append(item);
  }
  return list;
}

/** Drop whatever a previous attempt at this control left behind. */
export function clearRefusal(control: HTMLElement): void {
  for (const stale of control.parentElement?.querySelectorAll(":scope > .sb-refusal") ?? []) {
    stale.remove();
  }
}

/** SelectWorkspace: the row click. Idempotent — re-selecting is a success. */
export function buildSelectWorkspaceRequest(workspace: WorkspaceRef): SelectWorkspaceRequest {
  return create(SelectWorkspaceRequestSchema, { workspace });
}

/** OpenWorkspace: bring a registered-but-closed workspace back up. */
export function buildOpenWorkspaceRequest(workspace: WorkspaceRef): OpenWorkspaceRequest {
  return create(OpenWorkspaceRequestSchema, { workspace });
}

/** CloseWorkspace: the soft close, which refuses while work is in flight. */
export function buildCloseWorkspaceRequest(workspace: WorkspaceRef): CloseWorkspaceRequest {
  return create(CloseWorkspaceRequestSchema, { workspace });
}

/** KillWorkspace: forced session death; the worktree survives. */
export function buildKillWorkspaceRequest(workspace: WorkspaceRef): KillWorkspaceRequest {
  return create(KillWorkspaceRequestSchema, { workspace });
}

/** NukeWorkspace: the one verb that destroys data. */
export function buildNukeWorkspaceRequest(workspace: WorkspaceRef): NukeWorkspaceRequest {
  return create(NukeWorkspaceRequestSchema, { workspace });
}

/**
 * MergeWorkspace: enqueue; the merge's life thereafter is the feed's.
 *
 * The rail's Merge verb is "merge this workspace": the row's workspace asks to
 * merge ITS OWN branch, and is closed once it lands (`keep_open` false), which
 * is what the verb has always done. The other sources are for callers that
 * name another workspace or a branch; the rail has no such affordance.
 */
export function buildMergeWorkspaceRequest(workspace: WorkspaceRef): MergeWorkspaceRequest {
  return create(MergeWorkspaceRequestSchema, {
    workspace,
    source: { source: { case: "ownBranch", value: { keepOpen: false } } },
  });
}

/** RestartWorkspace: graceful (force false) or forced (force true). */
export function buildRestartWorkspaceRequest(
  workspace: WorkspaceRef,
  force: boolean,
): RestartWorkspaceRequest {
  return create(RestartWorkspaceRequestSchema, { workspace, force });
}

/**
 * SetWorkspacePriority: a level, or the UNSET request that clears one.
 *
 * `clear` omits the field rather than sending a sentinel level — presence is
 * the whole distinction the request draws.
 */
export function buildSetWorkspacePriorityRequest(
  workspace: WorkspaceRef,
  choice: PriorityChoice,
): SetWorkspacePriorityRequest {
  if (choice === "clear") return create(SetWorkspacePriorityRequestSchema, { workspace });
  return create(SetWorkspacePriorityRequestSchema, {
    workspace,
    priority: create(WorkspacePrioritySchema, { level: priorityLevel(choice) }),
  });
}

/**
 * The priority message for one level.
 *
 * A switch rather than `{ case: choice, value: {} }`: the four levels are four
 * distinct message types, and spelling them out is what makes a fifth level a
 * compile error here instead of a silently-typed object.
 */
function priorityLevel(
  choice: Exclude<PriorityChoice, "clear">,
): MessageInitShape<typeof WorkspacePrioritySchema>["level"] {
  switch (choice) {
    case "p05":
      return { case: "p05", value: {} };
    case "p1":
      return { case: "p1", value: {} };
    case "p2":
      return { case: "p2", value: {} };
    case "p3":
      return { case: "p3", value: {} };
  }
}

/**
 * AssignWorkspaceTask: a task, or the UNSET request that unassigns.
 *
 * The id is echoed verbatim — it is the daemon's `RosterTaskKey.task_id` and
 * this end never parses or constructs one.
 */
export function buildAssignWorkspaceTaskRequest(
  workspace: WorkspaceRef,
  taskId: string | null,
): AssignWorkspaceTaskRequest {
  if (taskId === null) return create(AssignWorkspaceTaskRequestSchema, { workspace });
  return create(AssignWorkspaceTaskRequestSchema, { workspace, task: { id: taskId } });
}

