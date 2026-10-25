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
 * THE ROW CLICK IS SelectInEditor AND NOTHING ELSE (R8). Cross-workspace
 * navigation is the host's to perform; the rail only says which workspace the
 * user picked, the daemon relays it to the editor, which switches as it does
 * for a key, and the roster's new `current` arrives on the stream.
 *
 * WHAT TICKS AND WHAT DOES NOT. The when-column's arms that ship an INSTANT
 * (`merged`, `hidden`, and the starts of `sync_merging` and `turn_running`)
 * are animated here off the shared ticker — never a `setInterval` of this
 * module's own. A sync merge and a running turn read exactly what the footer
 * strip's turn clock reads (`liveElapsedReading`, from the start instant the
 * daemon chose), and `no_turn_running` reads "0s" and never ticks (owner
 * request, 2026-10-13).
 */
import { createControl } from "../control.js";
import type {
  RosterRow,
  RosterRowAttention,
  RosterRowClosed,
  RosterRowCurrent,
  RosterRowIdleAsyncUnread,
  RosterRowDetail,
  RosterRowDetailBranch,
  RosterRowDetailChild,
  RosterRowDetailChildReceded,
  RosterRowDetailCommits,
  RosterRowDetailParentBranch,
  RosterRowDetailSummary,
  RosterRowName,
  RosterRowOneShot,
  RosterRowOpening,
  RosterRowAsyncMerging,
  RosterRowMergePaused,
  RosterRowPriorityBadge,
  RosterRowWhen,
  RosterRowWhenHidden,
  RosterRowWhenHiddenCache,
  RosterRowWhenMerged,
  RosterRowWhenNoTurnRunning,
  RosterRowWhenSyncMerging,
  RosterRowWhenTurnRunning,
  RosterRowReviving,
  RosterRowViewed,
  RosterRowWorkspace,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  SelectInEditorResponseSchema,
  type SelectInEditorResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_in_editor_pb";
import { SetWorkspaceHiddenResponseSchema, type SetWorkspaceHiddenError } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_hidden_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { formatTickedAge } from "../duration.js";
import { sameLocalDate } from "../clock.js";
import { liveElapsedReading, settledElapsedClock } from "../elapsed-clock.js";
import { heatColor } from "../heat.js";
import { cacheLapseHeat, cacheLapseText } from "./cache-lapse.js";
import { log } from "../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { MalformedView } from "../rpc/malformed.js";
import type { SidebarContext } from "./context.js";
import { toggleHiddenDropdown } from "./dropdowns.js";
import { markReviving } from "./reviving.js";
import { viewedMode } from "./viewed.js";
import { rosterArmMark, rosterRowMark, type RosterStatusCase, type RowMarkers } from "./tones.js";
import { drawTabNumberSlot } from "./tab-numbers.js";
import { toneClass } from "../vocab.js";
import { guardMalformed, guardMalformedNow } from "../rpc/guard.js";
import { clampBeside } from "../topbar/clamp.js";
import {
  buildSelectInEditorRequest,
  buildSetWorkspaceHiddenRequest,
  setWorkspaceHiddenRefusal,
  type CauseOf,
  buildOpenWorkspaceRequest,
  buildRevertMergeRequest,
  drawRowMenu,
  runOpenWorkspace,
  runVerb,
  type VerbTarget,
} from "./verbs.js";
import { SELECT_WORKSPACE_EXPECTED_ARMS } from "../rpc/refusal.js";
import { queryAll } from "../dom.js";

/**
 * ONE ROW OF A LIST AND ITS FAMILY, as each list of the roster spells it: an
 * open row (frontend.v1.RosterOpenRow), a hidden row (RosterHiddenRow) or a
 * merged row (RosterMergedRow, which has no family). The list a row sits in is
 * its state, and every member of its family is in the same list, so a row
 * draws its family exactly as the wire nests it: the daemon has already put an
 * open workspace whose parent is not open in its parent's place, and no list
 * carries a closed row for a client to drop.
 */
export interface RowFamily {
  /** The row as every list draws it. */
  row?: RosterRow;
  /** The row's family in the same list, in drawn order. */
  children?: readonly RowFamily[];
}

/** One family of a rows region, with its message path. */
export interface PathedFamily {
  family: RowFamily;
  path: string;
}

/** The families of a rows region ROWS at PATH, in drawn order, each with its message path. */
export function pathedFamilies(rows: readonly RowFamily[], path: string): PathedFamily[] {
  return rows.map((family, index) => ({ family, path: `${path}[${index}]` }));
}

/** Draw FAMILY, a row of one list and its family, at PATH (`drawRosterRow`). */
export function drawRosterFamily(family: RowFamily, sc: SidebarContext, path: string, place: RowPlace = "grouping"): HTMLElement {
  return drawRosterRow(requireMessage(family.row, `${path}.row`), sc, `${path}.row`, place, family.children ?? [], path);
}

/**
 * Which kind of section a row is drawn in, which decides what it looks like
 * and what a click on it does:
 *
 *   - `grouping`: a live grouping's section. Drawn whole; a click selects it.
 *   - `merged`: the recently-merged band. Drawn plain; a click REOPENS the
 *     workspace and then selects it (`OpenWorkspace`, owner request
 *     2026-10-14): a merged workspace is taken back to revise what it landed.
 *   - `hidden`: the hidden-workspaces section. Drawn with NO STATE (owner
 *     request, 2026-10-13): no status color, no status mark, no read greying,
 *     no reviving shimmer and no attention marker; its name is in the
 *     Recently Merged band's grey, struck through only at a cold gate. A click SHOWS it again (`SetWorkspaceHidden`), which
 *     is the section's one gesture.
 */
export type RowPlace = "grouping" | "merged" | "hidden";

/**
 * One roster row, with CHILDREN, its family in its list, nested under it.
 *
 * Returns the WHOLE subtree — the row line, the detail panel and the children
 * — because a row IS its family in this view: a child is drawn by this same
 * function, one generation down. FAMILY_PATH is the list row's path, the one
 * the children's paths hang off.
 */
export function drawRosterRow(
  u: RosterRow,
  sc: SidebarContext,
  path: string,
  place: RowPlace = "grouping",
  children: readonly RowFamily[] = [],
  familyPath: string = path,
): HTMLElement {
  const merged = place === "merged";
  const hidden = place === "hidden";
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
      children: children.length,
    },
  });

  // THE ROW CARRIES ITS OWN TONE. The status mark repeats it on the dot, but
  // the semantic color of a workspace belongs to the whole row: the hook
  // contract targets `[data-roster-row]` for `tone-<color>`, so the class is
  // set here, from the same one vocabulary table the mark reads.
  // PRESENCE IS THE FACT: only a one-shot carries the mark, and its glyph
  // replaces the dot while the one-shot works; an async merge of the
  // workspace's own branch draws the purple merge glyph in place of a plain
  // dot (`tones.rosterRowMark`).
  const markers: RowMarkers = {
    oneShot: u.oneShot !== undefined && drawRosterRowOneShot(u.oneShot, `${path}.oneShot`),
    asyncMerging: u.asyncMerging !== undefined && drawRosterRowAsyncMerging(u.asyncMerging, `${path}.asyncMerging`),
    mergePaused: u.mergePaused !== undefined && drawRosterRowMergePaused(u.mergePaused, `${path}.mergePaused`),
  };
  const mark = rosterRowMark(status.case, markers);
  const ws = document.createElement("div");
  // A HIDDEN ROW WEARS NO STATUS COLOR (owner request, 2026-10-13): its name
  // takes the rail's plain text; `data-arm` still carries the arm, which is
  // what draws a cold gate's strike.
  ws.className = `ws ${hidden ? toneClass("none") : mark.toneClass}`;
  ws.setAttribute("data-roster-row", workspace.id);
  ws.setAttribute("data-arm", status.case);
  sc.drawnStatus.note(workspace.id, { arm: status.case, source: "daemon" });
  // THE PAGE ON SCREEN LEADS THE ROSTER (sidebar.ts `markShown`): a row the
  // webhost has just shown is highlighted before the daemon's push names it
  // current, and no push from before that moment moves the highlight back.
  const ahead = sc.shownAhead();
  const highlighted = ahead === null ? current : ahead === workspace.id;
  if (highlighted) ws.setAttribute("data-current", "true");
  if (closed) ws.setAttribute("data-closed", "true");
  if (highlighted) ws.classList.add("current");
  // A CLOSED OR HIDDEN ROW RECEDES (owner request, 2026-10-13): a Hidden
  // row's name reads in the Recently Merged band's grey, which is the band
  // name colour at the receded opacity.
  if (closed || hidden) ws.classList.add("receded");
  // THE HIGHLIGHTED ROW'S NUMBER IS WHITE (selection-fade.ts): a row drawn
  // highlighted moves the highlight to it, and a redraw mid-fade joins the
  // fade where the clock says it has got to.
  sc.selectionFade.apply(workspace.id, ws, highlighted);
  // THE DETAIL IS A DROPDOWN, transient to this page (owner ruling,
  // 2026-10-06), so its openness is the page's own and survives a redraw.
  if (sc.openDetails.has(workspace.id)) ws.classList.add("open");

  // A WORKSPACE BEING OPENED IS NOT SELECTABLE (owner ruling, 2026-10-12):
  // its row carries the `opening` marker over the grey `inactive` arm, and a
  // click does nothing until the daemon answers it open, when the marker
  // drops and the row draws its real status and selects again.
  const opening = u.opening !== undefined && drawRosterRowOpening(u.opening, `${path}.opening`);
  if (opening) ws.setAttribute("data-opening", "true");
  const line = document.createElement("div");
  line.className = "row";
  if (opening) line.setAttribute("aria-disabled", "true");
  else line.setAttribute("data-select", "");
  line.addEventListener("click", (event) => {
    event.preventDefault();
    if (opening) {
      log.debug("a click on a workspace still opening selects nothing", {
        operation: "sidebar.row.select-opening",
        context: { workspace: workspace.id },
      });
      return;
    }
    if (place !== "grouping") {
      clickBandRow(BAND_RETURNS[place], ws, line, sc, workspace);
      return;
    }
    void guardMalformed(sc.ctx, "sidebar.row.select", selectWorkspace(line, sc, workspace));
  });
  line.addEventListener("contextmenu", (event) => {
    event.preventDefault();
    toggleRowMenu(ws, { sc, workspace, name, kind: "workspace" });
  });

  // THE BANDS DRAW NO STATUS MARK (owner requests, 2026-10-02 and
  // 2026-10-13): a recently-merged row and a hidden row show no dot or glyph.
  // Both bands' names are in the repository names' grey; a hidden row's is
  // struck through only at a cold gate.
  // AN OPEN ROW LEADS WITH ITS EMACS TAB NUMBER (owner request, 2026-10-17),
  // filled in by the roster drawer once the push is drawn (`tab-numbers.ts`).
  if (place === "grouping") {
    line.appendChild(drawTabNumberSlot());
    line.appendChild(drawStatusMark(status.case, `${path}.status`, markers));
  }

  const label = document.createElement("span");
  label.className = "name";
  label.textContent = name;
  // THE DISPLAY MODE GREYS THE NAME ALONE: the dot keeps its tone. `viewed.ts`
  // decides it, from the wire's marker alone: the daemon resolves it with the
  // status it rides on.
  const mode = viewedMode(
    workspace.id,
    status.case,
    u.viewed !== undefined && drawRosterRowViewed(u.viewed, `${path}.viewed`),
  );
  if (mode === "partial" && !hidden) {
    label.classList.add("viewed");
    ws.setAttribute("data-viewed", "true");
  }
  // THE UNREAD TURN END INKS THE NAME (owner request, 2026-10-20): a row in
  // the background over a result nobody has read keeps the background's
  // yellow on its dot, and its name takes the color of the turn end beneath,
  // until a dwell reads the result and the daemon drops the marker.
  if (u.idleAsyncUnread !== undefined && !hidden) {
    const end = drawRosterRowIdleAsyncUnread(u.idleAsyncUnread, status.case, `${path}.idleAsyncUnread`);
    label.classList.add(rosterArmMark(end).toneClass);
    ws.setAttribute("data-unread-turn-end", end);
  }
  // A BAND ROW'S NAME TAKES THE REPOSITORY NAMES' COLOUR, Recently Merged and
  // Hidden alike (owner requests, 2026-10-03 and 2026-10-13); a merged row's
  // timestamp keeps the secondary grey.
  if (merged || hidden) label.classList.add("band-name");
  // THE REVIVING SHIMMER IS THE NAME'S TOO, and only while the wire carries
  // the marker: the daemon lowers it when the revival ends, whichever way.
  if (!hidden && u.reviving !== undefined && drawRosterRowReviving(u.reviving, `${path}.reviving`)) {
    markReviving(ws, label);
  }
  // THE NAME BREATHES, NOT THE MARK (owner request, 2026-10-14): a row whose
  // mark's motion is a breath fades its name light to dark and back, and the
  // mark stands still. Only a row that draws a mark breathes, so the bands'
  // names never do.
  if (place === "grouping" && mark.motion !== "still") label.classList.add(mark.motion);
  line.appendChild(label);

  // PRESENCE, NEVER A SENTINEL: an absent marker or badge draws nothing at
  // all rather than an empty element holding its place.
  if (!hidden && u.attention !== undefined) {
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

  const when = requireMessage(u.when, `${path}.when`);
  const duration = drawRosterRowWhen(when, sc, `${path}.when`);
  line.appendChild(duration);
  // A MERGE THAT LANDED TODAY CARRIES A GUTTER (owner request, 2026-10-11):
  // the row draws an off-white bar while its merge is on today's local date,
  // and drops it once the shared ticker crosses midnight.
  if (merged && when.shown.case === "merged") {
    tickMergedToday(line, sc, drawRosterRowWhenMerged(when.shown.value, `${path}.when.merged`));
  }
  const target: VerbTarget = { sc, workspace, name, kind: "workspace" };
  line.appendChild(drawMenuControl(ws, target));
  ws.appendChild(line);
  // THE MENU IS ALWAYS DRAWN, hidden until it is asked for. Building it lazily
  // made the row's verbs exist only after a click, which is a different DOM
  // than the contract describes; the reveal is now visibility alone.
  ws.appendChild(drawRowMenu(target));
  // A BAND WITH SEVERAL RETURNS OFFERS THEM, drawn with the row and hidden
  // until the row is clicked, like the verb menu.
  if (place !== "grouping" && BAND_RETURNS[place].length > 1) {
    ws.appendChild(drawBandChoices(BAND_RETURNS[place], line, sc, workspace));
  }

  const detail = drawRosterRowDetail(requireMessage(u.detail, `${path}.detail`), `${path}.detail`);
  ws.appendChild(detail);
  // The hover wiring listens on the panel too, so the pointer can travel from
  // the row into it without it closing.
  installHoverPanel(ws, line, duration, detail, sc, workspace.id);

  appendRowKids(ws, children, familyPath, (child, childPath) => drawRosterFamily(child, sc, childPath, place));
  return ws;
}

/** The attribute a row's stand-in carries until the row is drawn in its place. */
export const STAND_IN_ATTRIBUTE = "data-stand-in";

/**
 * A row's STAND-IN: the row's box, its line and its name, and its visible
 * children's stand-ins, and nothing else (`reveal.ts`). It is as tall as the
 * row wherever the row's line height is pinned (the Recently Merged band's
 * is), and it is what the band's fit counts and the search narrows, so
 * neither can tell it from the row. Only the parts it reads are validated;
 * the rest of the row is validated when it is drawn.
 */
export function drawRosterRowStandIn(family: RowFamily, familyPath: string): HTMLElement {
  const path = `${familyPath}.row`;
  const u = requireMessage(family.row, path);
  const workspace = drawRosterRowWorkspace(requireMessage(u.workspace, `${path}.workspace`), `${path}.workspace`);
  const ws = document.createElement("div");
  ws.className = "ws";
  ws.setAttribute("data-roster-row", workspace.id);
  ws.setAttribute(STAND_IN_ATTRIBUTE, "");
  const line = document.createElement("div");
  line.className = "row";
  const label = document.createElement("span");
  label.className = "name";
  label.textContent = drawRosterRowName(requireMessage(u.name, `${path}.name`), `${path}.name`);
  line.appendChild(label);
  ws.appendChild(line);
  appendRowKids(ws, family.children ?? [], familyPath, drawRosterRowStandIn);
  return ws;
}

/**
 * Append CHILDREN, the family of the list row at FAMILY_PATH, to its box WS,
 * each drawn by DRAW, in the one `.kids` box the search and the stylesheet
 * look for a nested row in. A row with no child draws no box.
 */
export function appendRowKids(
  ws: HTMLElement,
  children: readonly RowFamily[],
  familyPath: string,
  draw: (child: RowFamily, childPath: string) => HTMLElement,
): void {
  if (children.length === 0) return;
  const kids = document.createElement("div");
  kids.className = "kids";
  for (const child of pathedFamilies(children, `${familyPath}.children`)) kids.appendChild(draw(child.family, child.path));
  ws.appendChild(kids);
}

/**
 * Draw U's stand-in into REGION, and the row in its place the first time the
 * stand-in comes into view (`reveal.ts`), FINISH run on the drawn row. The wait
 * is the section's: its scope stops it when the section's drawing goes.
 */
export function drawRosterRowWhenShown(
  region: HTMLElement,
  family: RowFamily,
  sc: SidebarContext,
  path: string,
  place: RowPlace,
  finish: (row: HTMLElement) => void,
): void {
  const standIn = drawRosterRowStandIn(family, path);
  finish(standIn);
  region.appendChild(standIn);
  sc.onDispose(
    sc.reveal.whenShown(standIn, () => {
      // The push that carried the row was drawn frames ago, so a row found
      // malformed now is filed here (`guardMalformedNow`).
      guardMalformedNow(sc.ctx, "sidebar.row.reveal", () => {
        const row = drawRosterFamily(family, sc, path, place);
        finish(row);
        standIn.replaceWith(row);
      });
    }),
  );
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
 * The UNREAD-TURN-END marker of an `idle_async` row: which turn end's result
 * stands unread under the background, as the status arm it draws once the
 * background ends. The row's name is inked with that arm's tone.
 *
 * A marker on any arm but `idleAsync`, or one whose oneof sets no arm, is a
 * contract breach and throws rather than guessing a color.
 */
export function drawRosterRowIdleAsyncUnread(u: RosterRowIdleAsyncUnread, arm: string, path: string): RosterStatusCase {
  if (arm !== "idleAsync") {
    throw new MalformedView(path, `the marker stands on arm '${arm}', and only an idleAsync row may carry it`);
  }
  const end = requireCase(u.turnEnd, `${path}.turnEnd`);
  switch (end.case) {
    case "done":
    case "interrupted":
    case "turnFailed":
      return end.case;
    default: {
      const other: { case: string } = end;
      return unreachableArm(`${path}.turnEnd`, other.case);
    }
  }
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

/**
 * The ONE-SHOT mark: the workspace was created as a one-shot. EMPTY —
 * presence is the fact — so this answers `true` for a mark that is there at
 * all; `tones.rosterRowMark` decides on which arms its glyph is drawn.
 */
export function drawRosterRowOneShot(u: RosterRowOneShot, path: string): boolean {
  void u;
  void path;
  return true;
}

/**
 * The ASYNC-MERGING marker: the workspace's own branch merges async in a
 * hidden merge workspace. Presence is the whole fact; `tones.rosterRowMark`
 * decides which marks it inks.
 */
export function drawRosterRowAsyncMerging(u: RosterRowAsyncMerging, path: string): boolean {
  void u;
  void path;
  return true;
}

/**
 * The MERGE-PAUSED marker: the merge queue holding the row's merge is paused.
 * EMPTY -- presence is the fact -- so this answers `true` for a marker that is
 * there at all; what it changes is `tones.rosterRowMark`'s.
 */
export function drawRosterRowMergePaused(u: RosterRowMergePaused, path: string): boolean {
  void u;
  void path;
  return true;
}

/**
 * The OPENING marker: the editor is opening this workspace. EMPTY — presence
 * is the fact — so this answers `true` for a marker that is there at all; the
 * row takes no click while it stands.
 */
export function drawRosterRowOpening(u: RosterRowOpening, path: string): boolean {
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
 * The when-column. ONE value, chosen by the daemon; never unset.
 *
 * A sync merge ticks its elapsed time from when it joined its queue, and a
 * running turn from the start instant the daemon read off the footer's turn
 * clock, both with that clock's own reading, so the row and the strip read
 * the same number; with neither the column reads the zero reading, "0s".
 */
export function drawRosterRowWhen(
  u: RosterRowWhen,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const shown = requireCase(u.shown, `${path}.shown`);
  if (shown.case === "noTurnRunning") {
    drawRosterRowWhenNoTurnRunning(shown.value, `${path}.no_turn_running`);
    const idle = settledElapsedClock("when", 0);
    idle.setAttribute("data-when", shown.case);
    return idle;
  }
  const when = document.createElement("span");
  when.className = "when";
  when.setAttribute("data-when", shown.case);
  switch (shown.case) {
    case "syncMerging":
      tickElapsed(when, sc, drawRosterRowWhenSyncMerging(shown.value, `${path}.sync_merging`));
      return when;
    case "turnRunning":
      tickElapsed(when, sc, drawRosterRowWhenTurnRunning(shown.value, `${path}.turn_running`));
      return when;
    case "merged":
      tickAge(when, sc, drawRosterRowWhenMerged(shown.value, `${path}.merged`));
      return when;
    case "hidden":
      // THE COLUMN COUNTS DOWN TO THE CACHE'S LAPSE once the daemon knows it,
      // and up from the hide until then.
      if (shown.value.cache !== undefined) {
        tickCacheLapse(
          when,
          sc,
          drawRosterRowWhenHiddenCache(shown.value.cache, `${path}.hidden.cache`),
          `${path}.hidden.cache`,
        );
        return when;
      }
      tickAge(when, sc, drawRosterRowWhenHidden(shown.value, `${path}.hidden`));
      return when;
    default: {
      const other: { case: string } = shown;
      return unreachableArm(`${path}.shown`, other.case);
    }
  }
}

/** When the sync merge joined its repository's queue, as an epoch instant. */
export function drawRosterRowWhenSyncMerging(u: RosterRowWhenSyncMerging, path: string): number {
  return msOf(u.startedAtMs, `${path}.started_at_ms`);
}

/** When the running turn started, as an epoch instant: the footer clock's own start. */
export function drawRosterRowWhenTurnRunning(u: RosterRowWhenTurnRunning, path: string): number {
  return msOf(u.startedAtMs, `${path}.started_at_ms`);
}

/** No turn is running. EMPTY: the arm is the fact. */
export function drawRosterRowWhenNoTurnRunning(u: RosterRowWhenNoTurnRunning, path: string): void {
  void u;
  void path;
}

/** When the workspace's merge settled, as an epoch instant. */
export function drawRosterRowWhenMerged(u: RosterRowWhenMerged, path: string): number {
  return msOf(u.atMs, `${path}.at_ms`);
}

/** When the workspace was hidden, as an epoch instant. */
export function drawRosterRowWhenHidden(u: RosterRowWhenHidden, path: string): number {
  return msOf(u.atMs, `${path}.at_ms`);
}

/** When a hidden workspace's prompt cache lapses, as an epoch instant. */
export function drawRosterRowWhenHiddenCache(u: RosterRowWhenHiddenCache, path: string): number {
  return msOf(u.lapsesAtMs, `${path}.lapses_at_ms`);
}

/**
 * The detail panel: its lines, each PRESENT OR ABSENT by message presence.
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
  if (u.commits !== undefined) {
    appendDetailLine(list, "commits", String(drawRosterRowDetailCommits(u.commits, `${path}.commits`)));
  }
  if (list.childElementCount > 0) detail.appendChild(list);
  if (u.summary !== undefined) {
    const summary = document.createElement("p");
    summary.className = "summary";
    summary.textContent = drawRosterRowDetailSummary(u.summary, `${path}.summary`);
    detail.appendChild(summary);
  }
  // THE CHILDREN BLOCK IS DRAWN IF AND ONLY IF THERE ARE CHILDREN (owner
  // request, 2026-10-20): every workspace whose recorded parent is this one,
  // wherever the rail draws it, so a child that is closed, merged or hidden
  // is still named here.
  if (u.children.length > 0) {
    const block = document.createElement("div");
    block.className = "children";
    const head = document.createElement("div");
    head.className = "children-head";
    head.textContent = "children";
    block.appendChild(head);
    const items = document.createElement("ul");
    u.children.forEach((child, i) => items.appendChild(drawRosterRowDetailChild(child, `${path}.children[${i}]`)));
    block.appendChild(items);
    detail.appendChild(block);
  }
  log.debug("drawing a row's detail panel", {
    operation: "sidebar.row.detail",
    context: {
      path,
      branch: u.branch !== undefined,
      parent_branch: u.parentBranch !== undefined,
      commits: u.commits !== undefined,
      summary: u.summary !== undefined,
      children: u.children.length,
    },
  });
  return detail;
}

/**
 * One child in the detail panel's children block: its name, receded exactly
 * when its own row recedes.
 */
export function drawRosterRowDetailChild(u: RosterRowDetailChild, path: string): HTMLElement {
  const workspace = requireMessage(u.workspace, `${path}.workspace`);
  const item = document.createElement("li");
  item.className = "child";
  item.setAttribute("data-child", workspace.id);
  item.textContent = u.name;
  if (u.receded !== undefined && drawRosterRowDetailChildReceded(u.receded, `${path}.receded`)) {
    item.classList.add("receded");
  }
  return item;
}

/** The receded marker of a detail panel child. EMPTY: presence is the fact. */
export function drawRosterRowDetailChildReceded(u: RosterRowDetailChildReceded, path: string): boolean {
  void u;
  void path;
  return true;
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

/**
 * How many commits the workspace's branch holds beyond its merge-base with
 * the branch it was cut from; on a merged row, the commits its merge landed.
 */
export function drawRosterRowDetailCommits(u: RosterRowDetailCommits, path: string): number {
  void path;
  return u.count;
}

/** The workspace's last prompt summary, in its own voice. */
export function drawRosterRowDetailSummary(u: RosterRowDetailSummary, path: string): string {
  void path;
  return u.text;
}

/**
 * The status mark: a dot for a lifecycle, a glyph for a merge, a gate or a
 * working one-shot.
 *
 * The color and the ink are the shared vocabulary's, through `tones.ts`. The
 * mark itself NEVER MOVES (owner request, 2026-10-14): the breath that says
 * something is in progress is the row's NAME's (`drawRosterRow`).
 */
export function drawStatusMark(arm: RosterStatusCase, path: string, markers: RowMarkers): HTMLElement {
  const mark = rosterRowMark(arm, markers);
  log.debug("drawing a row's status mark", {
    operation: "sidebar.row.status",
    context: {
      path,
      arm,
      one_shot: markers.oneShot,
      async_merging: markers.asyncMerging,
      merge_paused: markers.mergePaused,
      glyph: mark.glyph,
      tone: mark.toneClass,
      ink: mark.ink ?? "",
    },
  });
  const dot = document.createElement("span");
  dot.className = `st ${mark.toneClass}`;
  dot.setAttribute("data-glyph", mark.glyph);
  dot.title = arm;
  // The picture is bundled markup from images/glyphs/, never data off the wire.
  if (mark.image !== "") dot.innerHTML = mark.image;
  if (mark.glyph !== "dot") dot.classList.add("st-glyph");
  if (mark.ink !== undefined) dot.classList.add(`ink-${mark.ink}`);
  return dot;
}

/**
 * Wire a drawn row's duration field, panel and line to the page's hover gesture
 * (`row-hover.ts`), which owns the timers and the openness so a redraw can
 * neither drop a gesture in flight nor leave a thrown-away row acting.
 *
 * Openness is the row's `.open` class, remembered by THIS page
 * (`SidebarContext.openDetails`), so a redraw arriving while the pointer rests
 * on a row keeps that row's panel open. It is a dropdown, transient to the
 * page that opened it, never the daemon's view state (owner ruling,
 * 2026-10-06).
 *
 * THE POINTER'S TRIGGER IS THE ROW'S DURATION FIELD, in every section alike:
 * the open workspaces section, Hidden and Recently Merged (owner request,
 * 2026-10-14, which retired the name as the trigger after 2026-10-13 had
 * retired the status dot). Hovering the rest of the row, the name and the dot
 * included, opens nothing, and the pointer leaving the duration for the row
 * closes the panel as it does for anywhere else. The panel opens the moment
 * the pointer reaches the duration (owner request, 2026-10-14).
 *
 * The keyboard gets the same panel through `focusin`/`focusout`.
 */
function installHoverPanel(
  ws: HTMLElement,
  line: HTMLElement,
  trigger: HTMLElement,
  detail: HTMLElement,
  sc: SidebarContext,
  workspaceId: string,
): void {
  const hover = sc.rowHover;
  hover.drawn(workspaceId, { ws, line, trigger, detail });
  trigger.addEventListener("mouseenter", () => {
    hover.enter(workspaceId, "trigger");
  });
  trigger.addEventListener("mouseleave", () => {
    hover.leave(workspaceId, "trigger");
  });
  detail.addEventListener("mouseenter", () => {
    hover.enter(workspaceId, "panel");
  });
  detail.addEventListener("mouseleave", () => {
    hover.leave(workspaceId, "panel");
  });
  line.addEventListener("focusin", () => {
    hover.focusIn(workspaceId);
  });
  line.addEventListener("focusout", (event) => {
    hover.focusOut(workspaceId, event.relatedTarget);
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
 * THE PANEL GROWS OUT OF THE ROW'S RIGHT EDGE (owner request, 2026-10-14):
 * its top-left corner sits at the top-right corner of the row's own line, so
 * it covers no workspace name or duration in the rail and spills into the
 * feed instead, on the line of the workspace it describes. It may cover the
 * topbar or the footer when the window demands it.
 *
 * Placing it is then arithmetic over rectangles, shared with the topbar
 * reveals — `clampBeside`, which slides it left or up only as far as the
 * window's edges demand and caps its height so it scrolls inside itself
 * instead of running off the bottom. A second implementation of "stay inside
 * the window" is exactly the drift the shared helpers exist to prevent.
 */
export function placeRowDetail(ws: HTMLElement): void {
  const line = ws.querySelector<HTMLElement>(":scope > .row");
  const detail = ws.querySelector<HTMLElement>(":scope > .detail");
  if (line === null || detail === null) return;
  // Measured at its natural height: a cap the last placement applied would
  // otherwise hold a panel that moved up the window at its old, shorter size.
  detail.style.maxHeight = "";
  const placement = clampBeside(line.getBoundingClientRect(), detail.getBoundingClientRect(), {
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
  for (const ws of queryAll<HTMLElement>(root, ".ws.open")) placeRowDetail(ws);
}

/** The "…" control that opens a row's verb menu, and the right-click's twin. */
export function drawMenuControl(ws: HTMLElement, target: VerbTarget): HTMLElement {
  const more = createControl();
  more.className = "sb-more";
  more.textContent = "⋯";
  more.title = `${target.kind} actions`;
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
  const menu = ws.querySelector<HTMLElement>(":scope > .sb-menu");
  if (menu === null) return;
  const more = ws.querySelector<HTMLElement>(":scope > .row .sb-more");
  toggleHiddenDropdown(target.sc.dropdowns, "row-menu", menu, more === null ? [] : [more]);
}

/**
 * The row click: SelectInEditor, echoed, idempotent, and nothing else.
 *
 * THE EDITOR MAKES THE SWITCH, as it makes a key's (owner request,
 * 2026-10-23): the daemon relays the click to Emacs, which moves its frame on
 * the request. A page that asked the daemon to select left the frame to
 * follow the roster push the select provoked, which waited on the daemon
 * re-reading its whole registry, some 350ms where a key took 10. A workspace
 * Emacs holds no tab for is selected by the daemon instead; either way the
 * answer says which party made the switch, recorded here at DEBUG.
 */
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
    rpc: "SelectInEditor",
    call: (client) => client.selectInEditor(buildSelectInEditorRequest(workspace)),
    schema: SelectInEditorResponseSchema,
    expectedArms: SELECT_WORKSPACE_EXPECTED_ARMS,
    took: (response) => noteSelectRoute(response, workspace),
  });
}

/**
 * Record which party made a row click's switch: the editor the click was
 * relayed to, or the daemon, for a workspace the editor holds no tab for. A
 * success naming neither is a malformed view.
 */
export function noteSelectRoute(response: SelectInEditorResponse, workspace: WorkspaceRef): void {
  const result = requireCase(response.result, "SelectInEditorResponse.result");
  if (result.case !== "success") return;
  const route = requireCase(result.value.route, "SelectInEditorResponse.result.success.route");
  switch (route.case) {
    case "relayed":
    case "selected":
      log.debug(`the rail's click was ${route.case === "relayed" ? "relayed to the editor" : "selected by the daemon"}`, {
        operation: "sidebar.row.select-route",
        context: { workspace: workspace.id, route: route.case },
      });
      return;
    default: {
      const other: { case: string } = route;
      unreachableArm("SelectInEditorResponse.result.success.route", other.case);
    }
  }
}

/**
 * How a band row's click brings its workspace back before selecting it: the
 * returns each band offers. Every return is the same two steps
 * (`returnFromBand`) and only the first differs, so a band added later says
 * its returns here rather than spelling a click of its own. A band with ONE
 * return runs it on the click; a band with several offers them as a choice
 * list under the row (`drawBandChoices`), and the user's pick runs.
 *
 *   - `hidden`: SHOW the workspace again (owner request, 2026-10-13); the push
 *     that follows moves it back into its groupings and onto the tab bar.
 *   - `merged`: REOPEN it, or REOPEN IT AND REVERT ITS MERGE (owner requests,
 *     2026-10-14 and 2026-10-15). Either way the daemon restores its worktree
 *     and brings its session back, and the push moves it out of the band; the
 *     revert also queues the brief that has its agent revert the merge, which
 *     the user then follows from the workspace.
 */
export interface BandReturn {
  /** The return's label in the choice list. */
  label: string;
  /** The click's log operation. */
  operation: string;
  /** The click's info record. */
  message: string;
  /** The verb that brings the workspace back; answers whether it did. */
  bringBack: (line: HTMLElement, sc: SidebarContext, workspace: WorkspaceRef) => Promise<boolean>;
}

export const BAND_RETURNS: Readonly<Record<Exclude<RowPlace, "grouping">, readonly BandReturn[]>> = {
  hidden: [
    {
      label: "Show",
      operation: "sidebar.row.show",
      message: "showing a hidden workspace from the rail",
      bringBack: (line, sc, workspace) =>
        runVerb(line, {
          sc,
          rpc: "SetWorkspaceHidden",
          call: (client) => client.setWorkspaceHidden(buildSetWorkspaceHiddenRequest(workspace, false)),
          schema: SetWorkspaceHiddenResponseSchema,
          refusalText: (cause) => setWorkspaceHiddenRefusal(cause as CauseOf<SetWorkspaceHiddenError>),
        }),
    },
  ],
  merged: [
    {
      label: "Reopen",
      operation: "sidebar.row.reopen",
      message: "reopening a merged workspace from the rail",
      bringBack: (line, sc, workspace) => runOpenWorkspace(line, sc, buildOpenWorkspaceRequest(workspace)),
    },
    {
      label: "Reopen and revert the merge",
      operation: "sidebar.row.revert-merge",
      message: "reopening a merged workspace to revert its merge from the rail",
      bringBack: (line, sc, workspace) => runOpenWorkspace(line, sc, buildRevertMergeRequest(workspace)),
    },
  ],
};

/**
 * A band row's click: its one return runs at once, and several are offered
 * (the row's `.sb-band-choices` list is revealed or hidden).
 */
function clickBandRow(
  returns: readonly BandReturn[],
  ws: HTMLElement,
  line: HTMLElement,
  sc: SidebarContext,
  workspace: WorkspaceRef,
): void {
  const [only] = returns;
  if (returns.length === 1 && only !== undefined) {
    void guardMalformed(sc.ctx, only.operation, returnFromBand(only, line, sc, workspace));
    return;
  }
  const choices = ws.querySelector<HTMLElement>(":scope > .sb-band-choices");
  if (choices === null) throw new Error(`band row ${workspace.id} offers ${returns.length} returns and drew no choice list`);
  toggleHiddenDropdown(sc.dropdowns, "band-choices", choices, [line]);
}

/**
 * The choice list a band with several returns offers: one entry per return,
 * each running it. It opens DOWNWARD inside the row's own box, as the verb
 * menu does, and a pick closes it before the return runs; a refusal lands on
 * the row line, where every band return's refusal lands.
 */
export function drawBandChoices(
  returns: readonly BandReturn[],
  line: HTMLElement,
  sc: SidebarContext,
  workspace: WorkspaceRef,
): HTMLElement {
  const choices = document.createElement("div");
  choices.className = "sb-band-choices list-rows";
  choices.hidden = true;
  for (const band of returns) {
    const entry = createControl();
    entry.className = "sb-menu-item";
    entry.setAttribute("data-band-return", band.operation);
    entry.textContent = band.label;
    entry.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      choices.hidden = true;
      sc.dropdowns.released(choices);
      void guardMalformed(sc.ctx, band.operation, returnFromBand(band, line, sc, workspace));
    });
    choices.appendChild(entry);
  }
  return choices;
}

/**
 * A band return run: bring the workspace back (`BandReturn.bringBack`), then
 * select it, so the user lands on what they clicked. A refused first step
 * selects nothing: the refusal stands on the row instead.
 */
export async function returnFromBand(
  band: BandReturn,
  line: HTMLElement,
  sc: SidebarContext,
  workspace: WorkspaceRef,
): Promise<void> {
  log.info(band.message, {
    operation: band.operation,
    context: { workspace: workspace.id },
  });
  const back = await band.bringBack(line, sc, workspace);
  if (!back) return;
  await selectWorkspace(line, sc, workspace);
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

/**
 * Ticks EL with the footer turn clock's own reading of the time since
 * STARTED_MS: the one reading every counting arm of the column shows.
 */
function tickElapsed(el: HTMLElement, sc: SidebarContext, startedMs: number): void {
  tickPaint(sc, (nowMs) => {
    el.textContent = liveElapsedReading(startedMs, nowMs);
  });
}

/**
 * Paint an age now and on every tick, through the SHARED ticker. Every arm
 * draws the bare age (owner request, 2026-10-11: no "merged " prefix), so the
 * column's position in the band says what the age counts from.
 */
function tickAge(host: HTMLElement, sc: SidebarContext, atMs: number): void {
  tickPaint(sc, (nowMs) => {
    host.textContent = formatTickedAge(nowMs - atMs);
  });
}

/**
 * Count HOST down to a hidden workspace's cache lapse now and on every tick: its
 * text, its place on the heat gradient, and `data-cache-cold` once it has come.
 */
function tickCacheLapse(host: HTMLElement, sc: SidebarContext, lapsesAtMs: number, path: string): void {
  host.setAttribute("data-cache", "");
  tickPaint(sc, (nowMs) => {
    const remainingMs = lapsesAtMs - nowMs;
    host.textContent = cacheLapseText(remainingMs);
    host.style.color = heatColor(cacheLapseHeat(remainingMs), path);
    host.toggleAttribute("data-cache-cold", remainingMs <= 0);
  });
}

/** Mark a merged ROW's gutter now and on every tick while its merge is today's. */
function tickMergedToday(row: HTMLElement, sc: SidebarContext, mergedAtMs: number): void {
  tickPaint(sc, (nowMs) => {
    row.classList.toggle(MERGED_TODAY_CLASS, mergedToday(mergedAtMs, nowMs));
  });
}

/** Run PAINT now and on every tick of the shared ticker, until the row's section is drawn anew. */
export function tickPaint(sc: SidebarContext, paint: (nowMs: number) => void): void {
  paint(sc.ctx.ticker.now());
  sc.onDispose(sc.ctx.ticker.subscribe(paint));
}

/** The class a merged row carries while its merge landed today. */
export const MERGED_TODAY_CLASS = "merged-today";

/** Whether MERGED_AT_MS falls on the same LOCAL calendar date as NOW_MS. */
export function mergedToday(mergedAtMs: number, nowMs: number): boolean {
  return sameLocalDate(mergedAtMs, nowMs);
}
