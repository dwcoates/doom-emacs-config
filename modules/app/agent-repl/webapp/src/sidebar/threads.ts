/**
 * THE SIDEBAR'S APPS: the rail title's switcher, and the Chat app's thread list.
 *
 * The rail's head is UNCHANGED but for its title, which is the switcher (owner
 * ruling, 2026-10-24): clicking it opens the apps (Workspaces, Chat), and
 * picking one is ShowSidebarApp, which selects that app's most recent
 * workspace. Which app is shown follows the selected workspace, so the title,
 * the menu's mark and the rail's body are all drawn from the roster
 * (`WorkspaceRoster.app_switcher`, `.threads`); this page decides none of it.
 *
 * With Chat shown, the body is the thread list: the new-thread button
 * (RequestNewThread: the daemon asks Emacs to open its new-thread prompt,
 * since the webapp takes no typed input), then one row per open thread, the
 * one last interacted with first, then every archived thread under
 * "Archived". A row is drawn in the rail's row shape (`[data-roster-row]`
 * holding `.row > .name`), so the rail's search narrows threads as it narrows
 * workspaces, and its "⋯" opens the workspace verbs a thread shares under
 * the thread's names for them (`verbs.ts`): archive or restore, and delete.
 */
import { RequestNewThreadResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_request_new_thread_pb.js";
import { SelectWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb.js";
import { ShowSidebarAppResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_show_sidebar_app_pb.js";
import type {
  RosterAppSwitcher,
  RosterThreadRow,
  RosterThreadSection,
  SidebarApp,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb.js";
import { createControl } from "../control.js";
import { log } from "../log.js";
import type { DaemonContext } from "../rpc/context.js";
import { noteRefusedArm } from "../rpc/refusal.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { RailContext, SidebarContext } from "./context.js";
import { openScope, type DrawScope } from "./scope.js";
import type { Dropdowns } from "./dropdowns.js";
import { drawMenuControl } from "./row.js";
import { drawThreadMenu, type VerbTarget } from "./verbs.js";

/** The switcher drawn in the rail's head. */
export interface AppSwitcher {
  /**
   * The title's anchor, which stands where the title did: the title button
   * and the apps menu hung centered under it (styles.css).
   */
  element: HTMLElement;
  /** The title button. */
  title: HTMLElement;
  /** The apps menu. */
  menu: HTMLElement;
  /** Draw the roster's switcher: the title, and the menu's options. */
  update(switcher: RosterAppSwitcher): void;
}

/** Whether the roster shows the Chat app. */
export function showsChat(switcher: RosterAppSwitcher): boolean {
  return requireCase(requireMessage(switcher.shown, "RosterAppSwitcher.shown").app, "SidebarApp.app").case === "chat";
}

/** A refused verb's arm, recorded at INFO: the next roster is the answer. */
function refusedArm(rpc: string, result: { case?: string | undefined; value?: unknown }): string | null {
  return noteRefusedArm(rpc, "sidebar.app-refused", result);
}

/** Create the switcher: the title button and the menu under it. */
export function createAppSwitcher(ctx: DaemonContext, dropdowns: Dropdowns): AppSwitcher {
  const title = createControl();
  title.className = "sb-title";
  title.setAttribute("data-app-title", "");
  const name = document.createElement("span");
  name.textContent = "Workspaces";
  const caret = document.createElement("span");
  caret.className = "sb-title-caret";
  caret.textContent = "▾";
  title.append(name, caret);
  const menu = document.createElement("div");
  menu.className = "sb-apps-menu";
  menu.setAttribute("data-apps-menu", "");
  menu.hidden = true;
  const anchor = document.createElement("span");
  anchor.className = "sb-title-anchor";
  anchor.append(title, menu);

  const close = (): void => {
    menu.hidden = true;
    title.classList.remove("open");
    dropdowns.released(menu);
  };
  title.addEventListener("click", () => {
    if (!menu.hidden) {
      close();
      return;
    }
    menu.hidden = false;
    title.classList.add("open");
    dropdowns.opened({ kind: "apps-menu", element: menu, openers: [title], close });
  });

  const show = (app: SidebarApp, label: string): void => {
    close();
    log.info(`showing the ${label} app`, { operation: "sidebar.show-app", context: { app: app.app.case } });
    callUnary(ctx, "ShowSidebarApp", (client) => client.showSidebarApp({ app }), ShowSidebarAppResponseSchema).then(
      (response) => {
        refusedArm("ShowSidebarApp", requireCase(response.result, "ShowSidebarAppResponse.result"));
      },
      () => undefined, // the transport failure is callUnary's to record
    );
  };

  return {
    element: anchor,
    title,
    menu,
    update(switcher) {
      name.textContent = requireMessage(switcher.title, "RosterAppSwitcher.title").text;
      const options = switcher.options.map((option, i) => {
        const label = requireMessage(option.label, `RosterAppSwitcher.options[${i}].label`).text;
        const app = requireMessage(option.app, `RosterAppSwitcher.options[${i}].app`);
        const button = createControl();
        button.setAttribute("data-app-option", label);
        const check = document.createElement("span");
        check.className = "sb-app-check";
        check.textContent = option.shown === undefined ? "" : "✓";
        const text = document.createElement("span");
        text.textContent = label;
        button.append(check, text);
        button.addEventListener("click", () => show(app, label));
        return button;
      });
      menu.replaceChildren(...options);
    },
  };
}

/**
 * THE THREAD LIST IS DRAWN THROUGH A SCOPE (scope.ts), as every section of the
 * workspace roster is, so what its rows register (their menus' dropdowns)
 * lives exactly as long as the drawing: each draw disposes the last one's.
 */
const drawnLists = new WeakMap<RailContext, { scope: DrawScope; root: HTMLElement }>();

/** Draw the thread list into RAIL, replacing (and disposing) the last one. */
export function drawThreadList(section: RosterThreadSection, rail: RailContext): HTMLElement {
  disposeThreadList(rail);
  const scope = openScope(rail);
  const root = drawThreadSection(section, scope.context);
  drawnLists.set(rail, { scope, root });
  return root;
}

/** Tear down RAIL's thread list, when there is one. */
export function disposeThreadList(rail: RailContext): void {
  const drawn = drawnLists.get(rail);
  if (drawn === undefined) return;
  drawn.scope.dispose(drawn.root);
  drawnLists.delete(rail);
}

/** The heading the archived threads are listed under. */
export const ARCHIVED_HEADING = "Archived";

/** The Chat app's body: the new-thread button and the thread rows. */
export function drawThreadSection(section: RosterThreadSection, sc: SidebarContext): HTMLElement {
  const ctx = sc.ctx;
  const box = document.createElement("div");
  box.className = "sb-threads";
  box.setAttribute("data-thread-section", "");

  const button = createControl();
  button.className = "sb-new-thread";
  button.setAttribute("data-new-thread", "");
  button.textContent = `＋ ${requireMessage(section.newThread, "RosterThreadSection.new_thread").label}`;
  const note = document.createElement("div");
  note.className = "sb-new-thread-note";
  note.hidden = true;
  button.addEventListener("click", () => {
    log.info("asking the editor for a new thread", { operation: "sidebar.new-thread" });
    note.hidden = true;
    callUnary(ctx, "RequestNewThread", (client) => client.requestNewThread({}), RequestNewThreadResponseSchema).then(
      (response) => {
        if (refusedArm("RequestNewThread", requireCase(response.result, "RequestNewThreadResponse.result")) === "noEditor") {
          note.textContent = "no Emacs is connected to type the question in";
          note.hidden = false;
        }
      },
      () => undefined,
    );
  });

  const open = document.createElement("div");
  open.className = "rows";
  const archived = document.createElement("div");
  archived.className = "rows sb-threads-archived";
  section.rows.forEach((row, i) => {
    const path = `RosterThreadSection.rows[${i}]`;
    const standing = requireCase(requireMessage(row.standing, `${path}.standing`).standing, `${path}.standing.standing`);
    switch (standing.case) {
      case "open":
        open.append(drawThreadRow(row, sc, path, false));
        return;
      case "archived":
        archived.append(drawThreadRow(row, sc, path, true));
        return;
      default: {
        const other: { case: string } = standing;
        unreachableArm(`${path}.standing.standing`, other.case);
      }
    }
  });
  box.append(button, note, open);
  if (archived.childElementCount > 0) {
    const heading = document.createElement("div");
    heading.className = "sb-threads-heading";
    heading.textContent = ARCHIVED_HEADING;
    box.append(heading, archived);
  }
  return box;
}

/** One thread's row; an ARCHIVED one is drawn receded and offers restoring. */
function drawThreadRow(row: RosterThreadRow, sc: SidebarContext, path: string, archived: boolean): HTMLElement {
  const ctx = sc.ctx;
  const thread = requireMessage(row.thread, `${path}.thread`);
  const ws = document.createElement("div");
  ws.className = row.current === undefined ? "ws sb-thread" : "ws sb-thread current";
  if (archived) ws.classList.add("sb-thread-archived");
  ws.setAttribute("data-roster-row", thread.id);
  if (row.current !== undefined) ws.setAttribute("data-current", "true");
  const line = document.createElement("div");
  line.className = "row";
  const name = document.createElement("span");
  name.className = "name";
  name.textContent = requireMessage(row.name, `${path}.name`).text;
  const target: VerbTarget = { sc, workspace: thread, name: name.textContent, kind: "thread" };
  line.append(name, drawMenuControl(ws, target));
  ws.append(line);
  if (row.summary !== undefined) {
    const summary = document.createElement("div");
    summary.className = "sb-thread-summary";
    summary.textContent = row.summary.text;
    ws.append(summary);
  }
  ws.append(drawThreadMenu(target, archived));
  line.addEventListener("click", () => {
    log.info("selecting a thread from the rail", { operation: "sidebar.thread.select", context: { thread: thread.id } });
    callUnary(ctx, "SelectWorkspace", (client) => client.selectWorkspace({ workspace: thread }), SelectWorkspaceResponseSchema).then(
      (response) => {
        refusedArm("SelectWorkspace", requireCase(response.result, "SelectWorkspaceResponse.result"));
      },
      () => undefined,
    );
  });
  return ws;
}
