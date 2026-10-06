/**
 * row — one workspace in the rail: its status mark, name, badges, when-column,
 * detail panel, family, and the verb menu.
 *
 * THE MESSAGE TREE IS THE UI TREE. Every drawn box below is one message of
 * `frontend.v1.RosterRow` and every message is one box, so nothing here is
 * derived: the highlight comes from `RosterRowCurrent.current` (never from
 * comparing against `WorkspaceRoster.current`), the receded styling from
 * `RosterRowClosed.closed`, the display mode from `RosterRowViewed` (see
 * `viewed.ts`, which decides it from the wire alone), the badge from the
 * resolver-composed label, and
 * the when-column from whichever arm the daemon chose — this end applies no
 * precedence of its own.
 *
 * THE ROW CLICK IS SelectWorkspace AND NOTHING ELSE (R8). Cross-workspace
 * navigation is the host's to perform; the rail only says which workspace the
 * user picked, and the roster's new `current` arrives on the stream.
 *
 * WHAT TICKS AND WHAT DOES NOT. Every when-column arm ships an INSTANT
 * (`active` = last activity, `created` = the never-active fallback, and
 * `merged`), so the age beside a row is animated here
 * off the shared ticker — never a `setInterval` of this module's own. An unset
 * `shown` oneof means there is nothing to show: the column is empty, not
 * "0ms ago". The column reflects last ACTIVITY, never last VIEWING, so it does
 * not jump when the user selects the workspace.
 */
import { createControl } from "../control.js";
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
  RosterRowWhenActive,
  RosterRowWhenCreated,
  RosterRowWhenMerged,
  RosterRowReviving,
  RosterRowViewed,
  RosterRowWorkspace,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { SelectWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { formatTickedAge } from "../duration.js";
import { log } from "../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import { markReviving } from "./reviving.js";
import { viewedMode } from "./viewed.js";
import { armBreathes, armSpins, rosterArmMark, type RosterStatusCase } from "./tones.js";
import { guardMalformed } from "../rpc/guard.js";
import { clampReveal } from "../topbar/clamp.js";
import { buildSelectWorkspaceRequest, drawRowMenu, runVerb, type VerbTarget } from "./verbs.js";

/** A row that survived the closed filter, carrying its own message path. */
export interface VisibleRow {
  row: RosterRow;
  path: string;
}

/** Whether a row is CLOSED on the wire — closed, killed, or merged all read here. */
export function rowIsClosed(u: RosterRow, path: string): boolean {
  return drawRosterRowClosed(requireMessage(u.closed, `${path}.closed`), `${path}.closed`);
}

/**
 * The rows to actually draw from a received list, with CLOSED rows dropped.
 *
 * A closed, killed or nuked workspace never appears in the rail (owner ruling,
 * 2026-09-14): the daemon still EMITS a closed row because Emacs reconciles its
 * tab set from the `closed` flag, so the omission is this renderer's, applied
 * where a section's rows are laid out. Merged rows are also `closed = true` and
 * are the whole point of the recently-merged band, so that band draws its rows
 * directly and never through this filter.
 *
 * A dropped row's NON-CLOSED descendants are HOISTED into its place rather than
 * vanishing with it: a live child cut from a killed parent's branch is still a
 * live workspace and keeps its row, one level up.
 */
export function expandVisibleRows(rows: readonly RosterRow[], basePath: string): VisibleRow[] {
  const out: VisibleRow[] = [];
  rows.forEach((row, index) => {
    const path = `${basePath}[${index}]`;
    if (rowIsClosed(row, path)) {
      out.push(...expandVisibleRows(row.children, `${path}.children`));
      return;
    }
    out.push({ row, path });
  });
  return out;
}

/**
 * One roster row, with its family nested under it.
 *
 * Returns the WHOLE subtree — the row line, the detail panel and the children
 * — because a row IS its family in this view: nesting is the message's own
 * `children`, and a child is drawn by this same function, one generation down.
 * A row's CLOSED children are hoisted away by `expandVisibleRows`, so a killed
 * child never draws while its live siblings do.
 */
export function drawRosterRow(
  u: RosterRow,
  sc: SidebarContext,
  path: string,
  merged = false,
): HTMLElement {
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
  log.debug("drawing a roster row", {
    operation: "sidebar.row",
    context: {
      workspace: workspace.id,
      status: status.case,
      current,
      closed,
      children: u.children.length,
    },
  });

  // THE ROW CARRIES ITS OWN TONE. The status mark repeats it on the dot, but
  // the semantic color of a workspace belongs to the whole row: the hook
  // contract targets `[data-roster-row]` for `tone-<color>`, so the class is
  // set here, from the same one vocabulary table the mark reads.
  const mark = rosterArmMark(status.case);
  const ws = document.createElement("div");
  ws.className = `ws ${mark.toneClass}`;
  ws.setAttribute("data-roster-row", workspace.id);
  ws.setAttribute("data-arm", status.case);
  if (current) ws.setAttribute("data-current", "true");
  if (closed) ws.setAttribute("data-closed", "true");
  if (current) ws.classList.add("current");
  if (closed) ws.classList.add("gone");
  // THE DETAIL IS A DROPDOWN, transient to this page (owner ruling,
  // 2026-10-06), so its openness is the page's own and survives a redraw.
  if (sc.openDetails.has(workspace.id)) ws.classList.add("open");

  const line = document.createElement("div");
  line.className = "row";
  line.setAttribute("data-select", "");
  line.addEventListener("click", (event) => {
    event.preventDefault();
    void guardMalformed(sc.ctx, "sidebar.row.select", selectWorkspace(line, sc, workspace));
  });
  line.addEventListener("contextmenu", (event) => {
    event.preventDefault();
    toggleRowMenu(ws, { sc, workspace, name });
  });

  // A RECENTLY-MERGED ROW DRAWS PLAIN (owner request, 2026-10-02): no status
  // dot, and its name in the grey a viewed workspace's wears. The dot is the
  // detail panel's pointer trigger, so such a row opens that panel by keyboard
  // focus alone.
  let statusMark: HTMLElement | null = null;
  if (!merged) {
    statusMark = drawStatusMark(status.case, `${path}.status`);
    line.appendChild(statusMark);
  }

  const label = document.createElement("span");
  label.className = "name";
  label.textContent = name;
  // THE DISPLAY MODE IS THE NAME'S ALONE. `viewed.ts` decides it, from the
  // wire's marker alone: the daemon resolves it with the status it rides on.
  const mode = viewedMode(
    workspace.id,
    status.case,
    u.viewed !== undefined && drawRosterRowViewed(u.viewed, `${path}.viewed`),
  );
  if (mode === "partial") {
    label.classList.add("viewed");
    ws.setAttribute("data-viewed", "true");
  }
  // A MERGED ROW'S NAME TAKES THE REPOSITORY NAMES' COLOUR (owner request,
  // 2026-10-03); its timestamp keeps the secondary grey.
  if (merged) label.classList.add("merged-name");
  // THE REVIVING SHIMMER IS THE NAME'S TOO, and only while the wire carries
  // the marker: the daemon lowers it when the revival ends, whichever way.
  if (u.reviving !== undefined && drawRosterRowReviving(u.reviving, `${path}.reviving`)) {
    markReviving(ws, label);
  }
  line.appendChild(label);

  // PRESENCE, NEVER A SENTINEL: an absent marker or badge draws nothing at
  // all rather than an empty element holding its place.
  if (u.attention !== undefined) {
    const marker = drawRosterRowAttention(u.attention, `${path}.attention`);
    ws.setAttribute("data-attention", "");
    line.appendChild(marker);
    // The MARKER blinks, not the row: `data-blink` belongs to the element the
    // cadence paints, which is the one the hook contract points at.
    sc.attention.mark(workspace.id, marker);
  }
  if (u.priority !== undefined) {
    line.appendChild(drawRosterRowPriorityBadge(u.priority, `${path}.priority`));
  }

  line.appendChild(
    drawRosterRowWhen(requireMessage(u.when, `${path}.when`), sc, `${path}.when`),
  );
  const target: VerbTarget = { sc, workspace, name };
  line.appendChild(drawMenuControl(ws, target));
  ws.appendChild(line);
  // THE MENU IS ALWAYS DRAWN, hidden until it is asked for. Building it lazily
  // made the row's verbs exist only after a click, which is a different DOM
  // than the contract describes; the reveal is now visibility alone.
  ws.appendChild(drawRowMenu(target));

  ws.appendChild(
    drawRosterRowDetail(requireMessage(u.detail, `${path}.detail`), `${path}.detail`),
  );
  // AFTER the panel is in the tree: the hover wiring listens on the panel too,
  // so the pointer can travel from the row into it without it closing.
  installHoverPanel(ws, line, statusMark, sc, workspace.id);

  const visibleChildren = expandVisibleRows(u.children, `${path}.children`);
  if (visibleChildren.length > 0) {
    const kids = document.createElement("div");
    kids.className = "kids";
    for (const child of visibleChildren) {
      kids.appendChild(drawRosterRow(child.row, sc, child.path, merged));
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

/**
 * The VIEWED marker: the row's display mode, PARTIAL when it is present.
 *
 * The message is EMPTY — presence is the fact — so this answers `true` for a
 * marker that is there at all. Whether the row actually DRAWS partial is not
 * decided here: `viewed.ts` owns that decision.
 */
export function drawRosterRowViewed(u: RosterRowViewed, path: string): boolean {
  void u;
  void path;
  return true;
}

/**
 * The REVIVING marker: the daemon is bringing this parked workspace's session
 * back up. EMPTY — presence is the fact — so this answers `true` for a marker
 * that is there at all; `reviving.ts` draws the shimmer.
 */
export function drawRosterRowReviving(u: RosterRowReviving, path: string): boolean {
  void u;
  void path;
  return true;
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
  log.debug("drawing an attention marker", {
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
  log.debug("drawing a priority badge", {
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
    log.debug("drawing an empty when-column", {
      operation: "sidebar.row.when-empty",
      context: { path },
    });
    return when;
  }
  const shown = requireCase(u.shown, `${path}.shown`);
  when.setAttribute("data-when", shown.case);
  switch (shown.case) {
    case "active":
      tickAge(
        when,
        sc,
        drawRosterRowWhenActive(shown.value, `${path}.active`),
        (age) => age,
      );
      return when;
    case "created":
      tickAge(
        when,
        sc,
        drawRosterRowWhenCreated(shown.value, `${path}.created`),
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

/** When the workspace last did real work, as an epoch instant. */
export function drawRosterRowWhenActive(u: RosterRowWhenActive, path: string): number {
  return msOf(u.atMs, `${path}.at_ms`);
}

/** When the workspace was created — the never-active fallback — as an instant. */
export function drawRosterRowWhenCreated(u: RosterRowWhenCreated, path: string): number {
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
  log.debug("drawing a row's detail panel", {
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
  log.debug("drawing a row's status mark", {
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

/**
 * The detail panel opens on HOVER. There is no chevron (owner ruling,
 * 2026-09-14): "in the sidebar, when you hover a workspace, the chevron
 * appears. There should be no chevron. Hovering the workspace should have the
 * same effect as opening the chevron currently does."
 *
 * Two delays, and they are the whole mechanism. `HOVER_OPEN_DELAY_MS` is
 * intent: a pointer crossing the rail on its way somewhere else passes over
 * several rows in a few milliseconds, and opening on the bare `mouseenter`
 * flickers a panel under each of them. `HOVER_CLOSE_GRACE_MS` is travel: the
 * panel hangs BELOW the row rather than touching it, so the pointer is
 * briefly over neither on its way in, and a close on the bare `mouseleave`
 * would snatch the panel away as it is being reached for.
 *
 * Openness is the row's `.open` class, remembered by THIS page
 * (`SidebarContext.openDetails`), so a redraw arriving while the pointer rests
 * on a row keeps that row's panel open. It is a dropdown, transient to the
 * page that opened it, never the daemon's view state (owner ruling,
 * 2026-10-06).
 *
 * THE POINTER'S TRIGGER IS THE STATUS DOT ALONE (owner request, 2026-10-02):
 * hovering the rest of the row opens nothing, and the pointer leaving the dot
 * for the row closes the panel as it does for anywhere else.
 *
 * The keyboard gets the same panel through `focusin`/`focusout`, without the
 * intent delay — a focus move is deliberate in a way a pointer's path is not.
 */
export const HOVER_OPEN_DELAY_MS = 150;
export const HOVER_CLOSE_GRACE_MS = 120;

function installHoverPanel(
  ws: HTMLElement,
  line: HTMLElement,
  dot: HTMLElement | null,
  sc: SidebarContext,
  workspaceId: string,
): void {
  let pointerOverRow = false;
  let pointerOverPanel = false;
  let focusWithin = false;
  let openTimer: ReturnType<typeof setTimeout> | undefined;
  let closeTimer: ReturnType<typeof setTimeout> | undefined;

  const cancelTimers = (): void => {
    if (openTimer !== undefined) {
      clearTimeout(openTimer);
      openTimer = undefined;
    }
    if (closeTimer !== undefined) {
      clearTimeout(closeTimer);
      closeTimer = undefined;
    }
  };

  const setOpen = (open: boolean, via: string): void => {
    if (ws.classList.contains("open") === open) return;
    log.debug("toggling a row's detail panel", {
      operation: "sidebar.row.detail-toggle",
      context: { workspace: workspaceId, open, via },
    });
    if (open) sc.openDetails.add(workspaceId);
    else sc.openDetails.delete(workspaceId);
    paintRowDetail(ws, open);
  };

  const scheduleOpen = (): void => {
    cancelTimers();
    openTimer = setTimeout(() => {
      openTimer = undefined;
      setOpen(true, "hover");
    }, HOVER_OPEN_DELAY_MS);
  };

  const scheduleClose = (): void => {
    cancelTimers();
    closeTimer = setTimeout(() => {
      closeTimer = undefined;
      if (pointerOverRow || pointerOverPanel || focusWithin) return;
      setOpen(false, "leave");
    }, HOVER_CLOSE_GRACE_MS);
  };

  // A row with no dot (a recently-merged one) has no pointer trigger.
  dot?.addEventListener("mouseenter", () => {
    pointerOverRow = true;
    if (ws.classList.contains("open")) cancelTimers();
    else scheduleOpen();
  });
  dot?.addEventListener("mouseleave", () => {
    pointerOverRow = false;
    scheduleClose();
  });

  const detail = ws.querySelector<HTMLElement>(":scope > .detail");
  if (detail !== null) {
    detail.addEventListener("mouseenter", () => {
      pointerOverPanel = true;
      cancelTimers();
    });
    detail.addEventListener("mouseleave", () => {
      pointerOverPanel = false;
      scheduleClose();
    });
  }

  line.addEventListener("focusin", () => {
    focusWithin = true;
    cancelTimers();
    setOpen(true, "focus");
  });
  // `focusout` fires BEFORE the next element takes focus, so `activeElement`
  // is not yet the answer; the event's own `relatedTarget` is.
  line.addEventListener("focusout", (event) => {
    const next = event.relatedTarget;
    focusWithin = next instanceof Node && line.contains(next);
    if (!focusWithin) scheduleClose();
  });
}

/**
 * Show or hide a row's detail panel. The panel is fixed-positioned, so it is
 * placed the moment it is shown, measured where it now stands rather than
 * where the last draw left it.
 */
export function paintRowDetail(ws: HTMLElement, open: boolean): void {
  ws.classList.toggle("open", open);
  if (open) placeRowDetail(ws);
}

/**
 * Where an OPEN row's detail panel lands.
 *
 * THE PANEL LEAVES THE RAIL (owner ruling 3, 2026-09-13). It may extend past
 * the sidebar's width into the feed to gain room, and it must never be cut off
 * by the window. `#ws-sidebar` clips its content (`overflow: hidden`) and its
 * scroller clips it again, so an in-flow panel could only ever be as wide as
 * the rail and was cropped at the rail's bottom edge; `position: fixed` is the
 * one positioning that escapes BOTH, since neither ancestor is transformed.
 *
 * Placing it is then arithmetic over rectangles, and it is the topbar reveals'
 * arithmetic — `clampReveal`, which puts a panel under its anchor, slides it
 * left only as far as the right edge demands, and caps its height at what is
 * left below it so it scrolls inside itself instead of running off the bottom.
 * A second implementation of "stay inside the window" is exactly the drift the
 * reveals' helper exists to prevent.
 */
export function placeRowDetail(ws: HTMLElement): void {
  const line = ws.querySelector<HTMLElement>(":scope > .row");
  const detail = ws.querySelector<HTMLElement>(":scope > .detail");
  if (line === null || detail === null) return;
  const placement = clampReveal(line.getBoundingClientRect(), detail.getBoundingClientRect(), {
    width: window.innerWidth,
    height: window.innerHeight,
  });
  log.debug("placing a row's detail panel", {
    operation: "sidebar.row.detail-place",
    verbosity: "verbose",
    context: {
      workspace: ws.getAttribute("data-roster-row"),
      left: placement.left,
      top: placement.top,
      max_height: placement.maxHeight,
    },
  });
  // Viewport coordinates, applied verbatim: a fixed box is positioned against
  // the window, so there is no host box to translate them back into.
  detail.style.left = `${placement.left}px`;
  detail.style.top = `${placement.top}px`;
  detail.style.maxHeight = `${placement.maxHeight}px`;
}

/**
 * Re-place every open row's panel under ROOT.
 *
 * The rail calls it once after each draw — a detached tree measures as zeros,
 * so a row drawn already-expanded can only be placed once it is on the page —
 * and again whenever the window resizes or anything scrolls, because a fixed
 * panel does not travel with the row that anchors it.
 */
export function placeOpenRowDetails(root: ParentNode): void {
  for (const ws of root.querySelectorAll<HTMLElement>(".ws.open")) placeRowDetail(ws);
}

/** The "…" control that opens the verb menu, and the right-click's twin. */
function drawMenuControl(ws: HTMLElement, target: VerbTarget): HTMLElement {
  const more = createControl();
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
  void target;
  const menu = ws.querySelector<HTMLElement>(":scope > .sb-menu");
  if (menu === null) return;
  menu.hidden = !menu.hidden;
}

/** The row click: SelectWorkspace, echoed, idempotent, and nothing else. */
async function selectWorkspace(
  line: HTMLElement,
  sc: SidebarContext,
  workspace: WorkspaceRef,
): Promise<void> {
  log.info("selecting a workspace from the rail", {
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
    host.textContent = compose(formatTickedAge(nowMs - atMs));
  };
  paint(sc.ctx.ticker.now());
  sc.onDispose(sc.ctx.ticker.subscribe(paint));
}
