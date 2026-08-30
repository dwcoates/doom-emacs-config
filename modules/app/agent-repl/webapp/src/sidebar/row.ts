/**
 * row — one workspace in the rail: its status mark, name, badges, when-column,
 * detail panel, family, and the verb menu.
 *
 * THE MESSAGE TREE IS THE UI TREE. Every drawn box below is one message of
 * `frontend.v1.RosterRow` and every message is one box, so nothing here is
 * derived: the highlight comes from `RosterRowCurrent.current` (never from
 * comparing against `WorkspaceRoster.current`), the receded styling from
 * `RosterRowClosed.closed`, the badge from the resolver-composed label, and
 * the when-column from whichever arm the daemon chose — this end applies no
 * precedence of its own.
 *
 * THE ROW CLICK IS SelectWorkspace AND NOTHING ELSE (R8). Cross-workspace
 * navigation is the host's to perform; the rail only says which workspace the
 * user picked, and the roster's new `current` arrives on the stream.
 *
 * WHAT TICKS AND WHAT DOES NOT. `last_selected` and `merged` ship INSTANTS,
 * so the age beside a row is animated here off the shared ticker — never a
 * `setInterval` of this module's own. An unset `shown` oneof means there is
 * nothing to show: the column is empty, not "0ms ago".
 */
import type {
  RosterRow,
  RosterRowAttention,
  RosterRowClosed,
  RosterRowCurrent,
  RosterRowDetail,
  RosterRowDetailBranch,
  RosterRowDetailParentBranch,
  RosterRowDetailSummary,
  RosterRowName,
  RosterRowPriorityBadge,
  RosterRowWhen,
  RosterRowWhenLastSelected,
  RosterRowWhenMerged,
  RosterRowWorkspace,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { SelectWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { formatAge } from "../duration.js";
import { log } from "../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import { armBreathes, armSpins, rosterArmMark, type RosterStatusCase } from "./tones.js";
import { buildSelectWorkspaceRequest, drawRowMenu, runVerb, type VerbTarget } from "./verbs.js";

/**
 * One roster row, with its family nested under it.
 *
 * Returns the WHOLE subtree — the row line, the detail panel and the children
 * — because a row IS its family in this view: nesting is the message's own
 * `children`, and a child is drawn by this same function, one generation down.
 */
export function drawRosterRow(u: RosterRow, sc: SidebarContext, path: string): HTMLElement {
  const workspace = drawRosterRowWorkspace(
    requireMessage(u.workspace, `${path}.workspace`),
    `${path}.workspace`,
  );
  const name = drawRosterRowName(requireMessage(u.name, `${path}.name`), `${path}.name`);
  const status = requireCase(u.status, `${path}.status`);
  const current = drawRosterRowCurrent(
    requireMessage(u.current, `${path}.current`),
    `${path}.current`,
  );
  const closed = drawRosterRowClosed(
    requireMessage(u.closed, `${path}.closed`),
    `${path}.closed`,
  );
  log("debug", "drawing a roster row", {
    operation: "sidebar.row",
    context: {
      workspace: workspace.id,
      status: status.case,
      current,
      closed,
      children: u.children.length,
    },
  });

  const ws = document.createElement("div");
  ws.className = "ws";
  ws.setAttribute("data-roster-row", workspace.id);
  ws.setAttribute("data-arm", status.case);
  if (current) ws.setAttribute("data-current", "true");
  if (closed) ws.setAttribute("data-closed", "true");
  if (current) ws.classList.add("current");
  if (closed) ws.classList.add("gone");
  const expanded = sc.prefs.isExpanded(workspace.id);
  if (expanded) ws.classList.add("open");

  const line = document.createElement("div");
  line.className = "row";
  line.setAttribute("data-select", "");
  line.addEventListener("click", (event) => {
    event.preventDefault();
    void selectWorkspace(line, sc, workspace);
  });
  line.addEventListener("contextmenu", (event) => {
    event.preventDefault();
    toggleRowMenu(ws, { sc, workspace, name });
  });

  line.appendChild(drawExpandChevron(ws, sc, workspace.id));
  line.appendChild(drawStatusMark(status.case as RosterStatusCase, `${path}.status`));

  const label = document.createElement("span");
  label.className = "name";
  label.textContent = name;
  line.appendChild(label);

  // PRESENCE, NEVER A SENTINEL: an absent marker or badge draws nothing at
  // all rather than an empty element holding its place.
  if (u.attention !== undefined) {
    const marker = drawRosterRowAttention(u.attention, `${path}.attention`);
    ws.setAttribute("data-attention", "");
    line.appendChild(marker);
    sc.attention.mark(workspace.id, ws);
  }
  if (u.priority !== undefined) {
    line.appendChild(drawRosterRowPriorityBadge(u.priority, `${path}.priority`));
  }

  line.appendChild(
    drawRosterRowWhen(requireMessage(u.when, `${path}.when`), sc, `${path}.when`),
  );
  line.appendChild(drawMenuControl(ws, { sc, workspace, name }));
  ws.appendChild(line);

  ws.appendChild(
    drawRosterRowDetail(requireMessage(u.detail, `${path}.detail`), `${path}.detail`),
  );

  if (u.children.length > 0) {
    const kids = document.createElement("div");
    kids.className = "kids";
    for (const [index, child] of u.children.entries()) {
      kids.appendChild(drawRosterRow(child, sc, `${path}.children[${index}]`));
    }
    ws.appendChild(kids);
  }
  return ws;
}

/** The workspace the row IS — the echo token every gesture travels back with. */
export function drawRosterRowWorkspace(u: RosterRowWorkspace, path: string): WorkspaceRef {
  return requireMessage(u.workspace, `${path}.workspace`);
}

/** The row's display name. Never an identity; never a join key. */
export function drawRosterRowName(u: RosterRowName, path: string): string {
  void path;
  return u.text;
}

/** The selected-workspace highlight, as the RESOLVER states it. */
export function drawRosterRowCurrent(u: RosterRowCurrent, path: string): boolean {
  void path;
  return u.current;
}

/** The receded styling: the workspace exists, its panes are dismissed. */
export function drawRosterRowClosed(u: RosterRowClosed, path: string): boolean {
  void path;
  return u.closed;
}

/**
 * The attention marker.
 *
 * The message is EMPTY — presence is the fact — so the element carries no text
 * of its own; the blink registry drives it, on the one cadence the message
 * specifies.
 */
export function drawRosterRowAttention(u: RosterRowAttention, path: string): HTMLElement {
  void u;
  log("debug", "drawing an attention marker", {
    operation: "sidebar.row.attention",
    context: { path },
  });
  const marker = document.createElement("span");
  marker.className = "sb-attn";
  marker.setAttribute("data-attention", "");
  marker.title = "unseen notification";
  return marker;
}

/** The priority badge, drawn with the resolver's composed label verbatim. */
export function drawRosterRowPriorityBadge(
  u: RosterRowPriorityBadge,
  path: string,
): HTMLElement {
  log("debug", "drawing a priority badge", {
    operation: "sidebar.row.priority",
    context: { path, label: u.label },
  });
  const badge = document.createElement("span");
  badge.className = "sb-prio";
  badge.setAttribute("data-priority", u.label);
  badge.textContent = u.label;
  return badge;
}

/**
 * The when-column. ONE value, chosen by the daemon.
 *
 * An UNSET oneof is legitimate here (never selected, not merged) and draws an
 * empty column — the one place in this file where an unset oneof is not a
 * malformed view, because the message says so.
 */
export function drawRosterRowWhen(
  u: RosterRowWhen,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const when = document.createElement("span");
  when.className = "when";
  if (u.shown.case === undefined) {
    log("debug", "drawing an empty when-column", {
      operation: "sidebar.row.when-empty",
      context: { path },
    });
    return when;
  }
  const shown = requireCase(u.shown, `${path}.shown`);
  when.setAttribute("data-when", shown.case);
  switch (shown.case) {
    case "lastSelected":
      tickAge(
        when,
        sc,
        drawRosterRowWhenLastSelected(shown.value, `${path}.last_selected`),
        (age) => age,
      );
      return when;
    case "merged":
      tickAge(when, sc, drawRosterRowWhenMerged(shown.value, `${path}.merged`), (age) =>
        `merged ${age}`,
      );
      return when;
    default: {
      const other: { case: string } = shown;
      return unreachableArm(`${path}.shown`, other.case);
    }
  }
}

/** When the user last selected the workspace, as an epoch instant. */
export function drawRosterRowWhenLastSelected(
  u: RosterRowWhenLastSelected,
  path: string,
): number {
  return msOf(u.atMs, `${path}.at_ms`);
}

/** When the workspace's merge settled, as an epoch instant. */
export function drawRosterRowWhenMerged(u: RosterRowWhenMerged, path: string): number {
  return msOf(u.atMs, `${path}.at_ms`);
}

/**
 * The detail panel: three lines, each PRESENT OR ABSENT by message presence.
 *
 * An absent line is OMITTED, never drawn blank — which is why each field is
 * checked for presence rather than for a non-empty string. The panel itself is
 * always built; whether it is shown is the row's local expansion.
 */
export function drawRosterRowDetail(u: RosterRowDetail, path: string): HTMLElement {
  const detail = document.createElement("div");
  detail.className = "detail";
  const list = document.createElement("dl");
  if (u.branch !== undefined) {
    appendDetailLine(list, "branch", drawRosterRowDetailBranch(u.branch, `${path}.branch`));
  }
  if (u.parentBranch !== undefined) {
    appendDetailLine(
      list,
      "from",
      drawRosterRowDetailParentBranch(u.parentBranch, `${path}.parent_branch`),
    );
  }
  if (list.childElementCount > 0) detail.appendChild(list);
  if (u.summary !== undefined) {
    const summary = document.createElement("p");
    summary.className = "summary";
    summary.textContent = drawRosterRowDetailSummary(u.summary, `${path}.summary`);
    detail.appendChild(summary);
  }
  log("debug", "drawing a row's detail panel", {
    operation: "sidebar.row.detail",
    context: {
      path,
      branch: u.branch !== undefined,
      parent_branch: u.parentBranch !== undefined,
      summary: u.summary !== undefined,
    },
  });
  return detail;
}

/** The workspace's own git branch. Display only. */
export function drawRosterRowDetailBranch(u: RosterRowDetailBranch, path: string): string {
  void path;
  return u.name;
}

/** The branch this workspace was cut from and merges back into. */
export function drawRosterRowDetailParentBranch(
  u: RosterRowDetailParentBranch,
  path: string,
): string {
  void path;
  return u.name;
}

/** The workspace's last prompt summary, in its own voice. */
export function drawRosterRowDetailSummary(u: RosterRowDetailSummary, path: string): string {
  void path;
  return u.text;
}

/**
 * The status mark: a dot for a lifecycle, a glyph for the merge pipeline.
 *
 * The color is the shared vocabulary's, through `tones.ts`; the animation is
 * this surface's, and it says the same thing everywhere — a breath means work
 * is in flight, a spin means the merge queue is on this run.
 */
export function drawStatusMark(arm: RosterStatusCase, path: string): HTMLElement {
  const mark = rosterArmMark(arm);
  log("debug", "drawing a row's status mark", {
    operation: "sidebar.row.status",
    context: { path, arm, glyph: mark.glyph, tone: mark.toneClass },
  });
  const dot = document.createElement("span");
  dot.className = `st st-${arm} ${mark.toneClass}`;
  dot.setAttribute("data-glyph", mark.glyph);
  dot.title = arm;
  if (mark.char !== "") dot.textContent = mark.char;
  if (mark.glyph !== "dot") dot.classList.add("st-glyph");
  if (armBreathes(arm)) dot.classList.add("breathes");
  if (armSpins(arm)) dot.classList.add("spins");
  return dot;
}

/** The detail panel's toggle. Local, persisted, and never on the wire. */
function drawExpandChevron(
  ws: HTMLElement,
  sc: SidebarContext,
  workspaceId: string,
): HTMLElement {
  const chevron = document.createElement("span");
  chevron.className = "chev";
  chevron.textContent = "▸";
  chevron.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    const open = !ws.classList.contains("open");
    ws.classList.toggle("open", open);
    sc.prefs.setExpanded(workspaceId, open);
  });
  return chevron;
}

/** The "…" control that opens the verb menu, and the right-click's twin. */
function drawMenuControl(ws: HTMLElement, target: VerbTarget): HTMLElement {
  const more = document.createElement("button");
  more.type = "button";
  more.className = "sb-more";
  more.textContent = "⋯";
  more.title = "workspace actions";
  more.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    toggleRowMenu(ws, target);
  });
  return more;
}

/**
 * Open (or close) the row's verb menu.
 *
 * It is appended INSIDE the row's own box, after the line, so it opens
 * DOWNWARD from the control and can never clip off the top of the rail; the
 * stylesheet caps its height and scrolls it if the rail is short.
 */
export function toggleRowMenu(ws: HTMLElement, target: VerbTarget): void {
  const open = ws.querySelector(":scope > .sb-menu");
  if (open !== null) {
    open.remove();
    return;
  }
  const menu = drawRowMenu(target);
  const line = ws.querySelector(":scope > .row");
  if (line === null) ws.appendChild(menu);
  else line.after(menu);
}

/** The row click: SelectWorkspace, echoed, idempotent, and nothing else. */
async function selectWorkspace(
  line: HTMLElement,
  sc: SidebarContext,
  workspace: WorkspaceRef,
): Promise<void> {
  log("info", "selecting a workspace from the rail", {
    operation: "sidebar.row.select",
    context: { workspace: workspace.id },
  });
  // The row LINE is the control: a refusal lands as its next sibling, inside
  // the row's own box, exactly where a menu item's refusal lands.
  await runVerb(line, {
    sc,
    rpc: "SelectWorkspace",
    call: (client) => client.selectWorkspace(buildSelectWorkspaceRequest(workspace)),
    schema: SelectWorkspaceResponseSchema,
  });
}

/** One `<dt>/<dd>` pair in the detail panel. */
function appendDetailLine(list: HTMLElement, label: string, value: string): void {
  const dt = document.createElement("dt");
  dt.textContent = label;
  const dd = document.createElement("dd");
  const code = document.createElement("code");
  code.textContent = value;
  dd.appendChild(code);
  list.appendChild(dt);
  list.appendChild(dd);
}

/** Paint an age now and on every tick, through the SHARED ticker. */
function tickAge(
  host: HTMLElement,
  sc: SidebarContext,
  atMs: number,
  compose: (age: string) => string,
): void {
  const paint = (nowMs: number): void => {
    host.textContent = compose(formatAge(nowMs - atMs));
  };
  paint(sc.ctx.ticker.now());
  sc.onDispose(sc.ctx.ticker.subscribe(paint));
}
