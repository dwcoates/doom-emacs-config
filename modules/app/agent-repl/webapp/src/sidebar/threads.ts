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
import { ShowChatNodeResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_show_chat_node_pb.js";
import { ShowSidebarAppResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_show_sidebar_app_pb.js";
import type {
  RosterAppSwitcher,
  RosterThreadOutline,
  RosterThreadOutlineBreadcrumb,
  RosterThreadOutlineCrumb,
  RosterThreadRow,
  RosterThreadSection,
  SidebarApp,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb.js";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb.js";
import type { NodeId } from "../../../proto/gen/ts/zettel/v1/zettel_pb.js";
import { createControl } from "../control.js";
import { log } from "../log.js";
import type { DaemonContext } from "../rpc/context.js";
import { sendGesture } from "../rpc/gesture.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
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

/** The operation every refused rail gesture here is recorded under: the next roster is the answer. */
const GESTURE_REFUSED = "sidebar.app-refused";

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
    void sendGesture(ctx, "ShowSidebarApp", GESTURE_REFUSED, (client) => client.showSidebarApp({ app }), ShowSidebarAppResponseSchema);
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

/** The heading the open threads are listed under, beside the new-thread "+". */
export const OPEN_HEADING = "Open";

/** The heading the archived threads are listed under. */
export const ARCHIVED_HEADING = "Archived";

/** The Chat app's body: the new-thread button and the thread rows. */
export function drawThreadSection(section: RosterThreadSection, sc: SidebarContext): HTMLElement {
  const ctx = sc.ctx;
  const box = document.createElement("div");
  box.className = "sb-threads";
  box.setAttribute("data-thread-section", "");

  const button = createControl();
  button.className = "sb-add";
  button.setAttribute("data-new-thread", "");
  button.textContent = "+";
  button.title = requireMessage(section.newThread, "RosterThreadSection.new_thread").label;
  const note = document.createElement("div");
  note.className = "sb-new-thread-note";
  note.hidden = true;
  button.addEventListener("click", () => {
    log.info("asking the editor for a new thread", { operation: "sidebar.new-thread" });
    note.hidden = true;
    void sendGesture(ctx, "RequestNewThread", GESTURE_REFUSED, (client) => client.requestNewThread({}), RequestNewThreadResponseSchema).then(
      (arm) => {
        if (arm === "noEditor") {
          note.textContent = "no Emacs is connected to type the question in";
          note.hidden = false;
        }
      },
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
  const openHeading = document.createElement("div");
  openHeading.className = "sb-threads-heading sb-threads-open";
  openHeading.append(OPEN_HEADING, button);
  box.append(openHeading, note, open);
  if (archived.childElementCount > 0) {
    const heading = document.createElement("div");
    heading.className = "sb-threads-heading sb-threads-archived-heading";
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
  if (row.outline !== undefined) ws.append(drawOutline(row.outline, sc, thread, `${path}.outline`));
  ws.append(drawThreadMenu(target, archived));
  line.addEventListener("click", () => {
    log.info("selecting a thread from the rail", { operation: "sidebar.thread.select", context: { thread: thread.id } });
    void sendGesture(ctx, "SelectWorkspace", GESTURE_REFUSED, (client) => client.selectWorkspace({ workspace: thread }), SelectWorkspaceResponseSchema);
  });
  return ws;
}

/** The class of a thread's outline, drawn under its row. */
export const OUTLINE_CLASS = "sb-outline";

/**
 * THE CURRENT THREAD'S BRANCHES (frontend.v1.RosterThreadOutline; owner
 * ruling, 2026-10-25): the breadcrumb of the folded levels above, then one
 * row per run, indented by its depth, which the daemon keeps within the
 * rail's width. A click on a row or a crumb shows that run's first question
 * (ShowChatNode); the chat page then brings it into view.
 */
function drawOutline(outline: RosterThreadOutline, sc: SidebarContext, thread: WorkspaceRef, path: string): HTMLElement {
  const box = document.createElement("div");
  box.className = OUTLINE_CLASS;
  box.setAttribute("data-thread-outline", "");
  const show = (node: NodeId | undefined, at: string): void => {
    const id = requireMessage(node, at).id;
    log.info("showing a node of the thread's outline", { operation: "sidebar.outline.show", context: { thread: thread.id, node: id } });
    void sendGesture(sc.ctx, "ShowChatNode", GESTURE_REFUSED, (client) => client.showChatNode({ thread, node: { id } }), ShowChatNodeResponseSchema);
  };
  if (outline.breadcrumb !== undefined) box.append(drawCrumbs(outline.breadcrumb, show, `${path}.breadcrumb`));
  outline.rows.forEach((row, i) => {
    const at = `${path}.rows[${i}]`;
    const line = document.createElement("div");
    line.className = "sb-outline-row";
    line.setAttribute("data-outline-node", requireMessage(row.node, `${at}.node`).id);
    line.style.setProperty("--outline-depth", String(row.depth));
    const place = requireCase(requireMessage(row.place, `${at}.place`).place, `${at}.place.place`);
    switch (place.case) {
      case "off":
        break;
      case "onPath":
        line.classList.add("on-path");
        break;
      case "shown":
        line.classList.add("shown");
        break;
      default: {
        const other: { case: string } = place;
        unreachableArm(`${at}.place.place`, other.case);
      }
    }
    const dot = document.createElement("span");
    dot.className = "sb-outline-dot";
    const label = document.createElement("span");
    label.className = "sb-outline-label";
    label.textContent = requireMessage(row.label, `${at}.label`).text;
    line.append(dot, label);
    if (row.turns > 1) {
      const turns = document.createElement("span");
      turns.className = "sb-outline-turns";
      turns.textContent = String(row.turns);
      line.append(turns);
    }
    if (row.folded !== undefined) {
      const folded = document.createElement("span");
      folded.className = "sb-outline-folded";
      folded.textContent = `+${row.folded.runs}`;
      folded.title = `${row.folded.runs} more below`;
      line.append(folded);
    }
    line.addEventListener("click", () => show(row.node, `${at}.node`));
    box.append(line);
  });
  return box;
}

/** The breadcrumb: every folded level, or the oldest and newest with the rest counted. */
function drawCrumbs(
  breadcrumb: RosterThreadOutlineBreadcrumb,
  show: (node: NodeId | undefined, at: string) => void,
  path: string,
): HTMLElement {
  const line = document.createElement("div");
  line.className = "sb-outline-crumbs";
  const crumb = (c: RosterThreadOutlineCrumb, at: string): HTMLElement => {
    const el = document.createElement("span");
    el.className = "sb-outline-crumb";
    el.textContent = requireMessage(c.label, `${at}.label`).text;
    el.addEventListener("click", () => show(c.node, `${at}.node`));
    return el;
  };
  const parts: (HTMLElement | string)[] = [];
  const extent = requireCase(breadcrumb.extent, `${path}.extent`);
  switch (extent.case) {
    case "whole":
      extent.value.crumbs.forEach((c, i) => parts.push(crumb(c, `${path}.whole.crumbs[${i}]`)));
      break;
    case "elided":
      parts.push(
        crumb(requireMessage(extent.value.first, `${path}.elided.first`), `${path}.elided.first`),
        `… ${extent.value.omitted} more`,
        crumb(requireMessage(extent.value.last, `${path}.elided.last`), `${path}.elided.last`),
      );
      break;
    default: {
      const other: { case: string } = extent;
      unreachableArm(`${path}.extent`, other.case);
    }
  }
  parts.forEach((part, i) => {
    if (i > 0) line.append(" › ");
    line.append(part);
  });
  return line;
}
