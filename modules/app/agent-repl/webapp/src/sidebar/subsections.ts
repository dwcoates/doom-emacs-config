/**
 * subsections — a repository section's user-named subsections, the "+" that
 * adds one, and the drags that arrange the repository grouping.
 *
 * A SUBSECTION IS A SEPARATION INSIDE ONE REPOSITORY'S SECTION
 * (frontend.v1.RosterSubsection), standing among the section's rows wherever
 * it was arranged: the section's body is ONE LIST of its rows in no
 * subsection and its subsections, in the daemon's order (`entries.ts`). A
 * workspace is put in one by dragging its row there, and the Emacs tab bar
 * follows: entry by entry, a subsection's workspaces where it stands
 * (`arrange.ts`). Its rows are indented under its header. An empty
 * subsection still draws its header, and a row dropped onto it lands in its
 * (empty) rows list.
 *
 * A ONE-SHOT IS FILED LIKE ANY OTHER WORKSPACE (owner request, 2026-11-03):
 * no subsection is derived from the one-shot mark, and a drag places it
 * without touching the mark.
 *
 * A SUBSECTION FOLDS AS A REPOSITORY SECTION DOES (`section-fold.ts`): its
 * triangle asks UpdateSidebarView for the other fold, a collapsed
 * subsection's rows are hidden, and the daemon folds its workspaces' tabs off
 * the Emacs tab bar. Only the triangle folds it: the header's label renames
 * on a double-click, which a fold on every click would undo.
 *
 * A FOLDED SUBSECTION SAYS ITS WORKSPACES WORK (owner request, 2026-11-18):
 * while any of its rows, nested ones included, stands on a working arm
 * (`armWorks`), its NAME, never its count, breathes in that row's tone, as
 * the row's own name does. The header carries the breath whatever its fold,
 * and the stylesheet draws it only while the subsection is folded, so a fold
 * painted ahead of the daemon's push (`section-fold.ts`) shows it at once. A
 * push that changed only rows keeps the header (`roster.ts`'s row patch), so
 * the patch repaints the breath through the same `paintSubsectionWork`
 * (`roster.ts` `paintRepoSection`).
 *
 * WHAT DRAGS. In the open workspaces' repository sections only, which are the
 * ones whose order the tab bar walks:
 *
 *   - a workspace row's family, by any of its rows (a nested row lifts its
 *     family's top row, so a child takes its ancestors along), among its own
 *     repository's lists: the section's list, where
 *     it may stand just above a subsection, and each subsection's rows. Over
 *     a subsection (its header or its rows) it lands in that subsection,
 *     which lights up (`DROP_TARGET_CLASS`) while it would. A folded or
 *     empty subsection shows none of its rows, so over one the drag also
 *     draws the row going inside it (`DragSpec.intoUnseen`, `drag.ts`);
 *   - a subsection, by its header, among its repository's rows and
 *     subsections: dropped onto a row's line it stands just above that row;
 *   - a repository block (a parent's children of one other repository), by
 *     its divider, among that parent's blocks only, always after the parent's
 *     own-repository children; the rows inside it move with it;
 *   - a repository, by its header, among the repositories.
 *
 * ONE LISTENER FOR EVERY DRAG (`installArrangeDrag`). A push redraws only the
 * rows and sections it changed, so a handle drawn by an earlier push may stand
 * beside one drawn by this push. The drag is found from the press's target at
 * the moment of the press, through the DOM's own attributes, so whatever the
 * drawer kept or redrew is draggable the same way.
 *
 * THE NAME IS ASKED IN A POPUP that outlives every push: it lives in the
 * rail's layer (`ArrangeContext.layer`), not in a drawing a push replaces,
 * so a status change elsewhere cannot throw away what is being typed.
 */
import { FOLDED_CLASS } from "./folded-class.js";
import { CONTROL_SELECTOR, createControl, type Control } from "../control.js";
import type { RosterOpenRows, RosterSubsection } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { openKin, pathedOpenRows, type PathedOpenRow } from "./entries.js";
import { BREATHES_CLASS, NAME_INK_CLASS, drawNameInk } from "./name-ink.js";
import { NAME_STEADY_CLASS } from "./name-tint.js";
import { armWorks, rosterArmMark } from "./tones.js";
import type { RepositoryRef, WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { queryAll } from "../dom.js";
import { EMPTY_SUBSECTION_ATTRIBUTE, OPEN_REGION_CLASS } from "./band-fit.js";
import { requireMessage } from "../rpc/strict.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { sendArrangement } from "./arrange.js";
import { openNamePopup } from "./name-popup.js";
import { subsectionFoldKey, type SidebarContext } from "./context.js";
import { drawRosterSectionFold, foldOnClick, sectionFold, trackDaemonFold } from "./section-fold.js";
import { pressToDrag, type DragSpec } from "../drag.js";
import { REPOSITORY_BLOCK_ATTRIBUTE, REPOSITORY_BLOCK_CLASS, REPOSITORY_DIVIDER_CLASS } from "./row.js";
import { drawRosterOpenCount, openCountText } from "./open-count.js";

/** The class of one subsection. */
export const SUBSECTION_CLASS = "sb-subsection";

/** The class of a subsection's header: its drag handle. */
export const SUBSECTION_HEAD_CLASS = "sb-subsection-head";

/** The attribute a rows list states its subsection on; empty for the section's own. */
export const BUCKET_ATTRIBUTE = "data-subsection";

/** Every rows list a top-level row may stand in: the section's own and each subsection's. */
export const BUCKET_LIST_SELECTOR = `.rows[${BUCKET_ATTRIBUTE}]`;

/**
 * Which list of a repository's section a rows list or subsection element is:
 * the section's own (`none`), or a subsection the user named.
 */
export type Bucket = { case: "none" } | { case: "named"; id: string };

/** Stamp BUCKET on ELEMENT, a rows list or subsection, as `bucketOf` reads it. */
export function stampBucket(element: HTMLElement, bucket: Bucket): void {
  switch (bucket.case) {
    case "none":
      element.setAttribute(BUCKET_ATTRIBUTE, "");
      return;
    case "named":
      element.setAttribute(BUCKET_ATTRIBUTE, bucket.id);
      return;
  }
}

/** The bucket ELEMENT, a rows list or subsection, was stamped with (`stampBucket`). */
export function bucketOf(element: HTMLElement): Bucket {
  const id = element.getAttribute(BUCKET_ATTRIBUTE);
  if (id === null) throw new Error("invariant: a drop list states its subsection");
  return id === "" ? { case: "none" } : { case: "named", id };
}

/** Whether ELEMENT is stamped with a bucket: a rows list a row may stand in, or a subsection. */
function isBucketed(element: Element | null): boolean {
  return element !== null && element.hasAttribute(BUCKET_ATTRIBUTE);
}

/** The bucket subsection U is: named by its id. EVERY kind is named; unset is malformed. */
export function subsectionBucket(u: RosterSubsection, path: string): Bucket {
  const kind = requireCase(u.kind, `${path}.kind`);
  switch (kind.case) {
    case "named":
      return { case: "named", id: requireMessage(kind.value.key, `${path}.kind.named.key`).subsectionId };
    default: {
      const other: { case: string } = kind;
      return unreachableArm(`${path}.kind`, other.case);
    }
  }
}

/** BUCKET as the records name it: a named subsection by its id. */
function bucketName(bucket: Bucket): string {
  switch (bucket.case) {
    case "none":
      return "the section's own rows";
    case "named":
      return bucket.id;
  }
}

/** Whether A and B are the same bucket. */
export function sameBucket(a: Bucket, b: Bucket): boolean {
  return a.case === b.case && (a.case !== "named" || (b.case === "named" && a.id === b.id));
}

/** Throw unless the subsection drawn as DRAWN is WIRE, the one the wire stands at PATH. */
export function requireDrawnBucket(drawn: HTMLElement, wire: Bucket, path: string): void {
  if (!sameBucket(bucketOf(drawn), wire)) throw new Error(`${path}: subsection ${bucketName(wire)} is not the one drawn in its place`);
}

/** The attribute a repository section states its repository's id on. */
export const REPOSITORY_ATTRIBUTE = "data-repository";

/** The attribute a subsection's count wears when it counts no workspaces. */
export const ZERO_COUNT_ATTRIBUTE = "data-zero";

/** One subsection as drawn: its element and its rows list. */
export interface DrawnSubsection {
  element: HTMLElement;
  rows: HTMLElement;
}

/**
 * One subsection: its header (fold, name, count and delete) and its rows. It
 * renames on a double-click of its name.
 */
export function drawRosterSubsection(
  u: RosterSubsection,
  sc: SidebarContext,
  path: string,
  drawRows: (rows: RosterOpenRows, path: string) => HTMLElement,
): DrawnSubsection {
  const bucket = subsectionBucket(u, path);
  const header = requireMessage(u.header, `${path}.header`);
  const name = requireMessage(header.label, `${path}.header.label`).text;
  const count = drawRosterOpenCount(requireMessage(header.count, `${path}.header.count`), `${path}.header.count`);
  const element = document.createElement("div");
  element.className = SUBSECTION_CLASS;
  stampBucket(element, bucket);

  const head = document.createElement("div");
  head.className = SUBSECTION_HEAD_CLASS;
  const label = document.createElement("span");
  label.className = SUBSECTION_LABEL_CLASS;
  label.textContent = name;
  paintSubsectionWork(label, u, path);
  const shown = document.createElement("span");
  shown.className = "sb-count";
  shown.textContent = openCountText(count);
  if (count.workspaces === 0) shown.setAttribute(ZERO_COUNT_ATTRIBUTE, "");
  if (bucket.case !== "named") throw new Error("invariant: a drawn subsection is a named one");
  const id = bucket.id;
  const foldKey = subsectionFoldKey(id);
  trackDaemonFold(element, foldKey, drawRosterSectionFold(u.fold, `${path}.fold`), sc);
  foldOnClick(head, element, sectionFold(sc, foldKey, { case: "subsection", value: { id } }));
  label.title = "double-click to rename";
  label.addEventListener("dblclick", (event) => {
    event.preventDefault();
    openNamePopup(sc, label, {
      subject: "subsection",
      placeholder: "subsection name",
      value: name,
      submitLabel: "Rename",
      send: (newName, control) =>
        sendArrangement(sc, control, { case: "renameSubsection", value: { subsection: { id }, name: newName } }),
    });
  });
  const remove = createControl();
  remove.className = "sb-subsection-delete";
  remove.setAttribute("data-subsection-delete", id);
  remove.textContent = "×";
  remove.title = "remove this subsection; its workspaces return to the repository";
  remove.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    void sendArrangement(sc, sc.arrange.status, { case: "deleteSubsection", value: { subsection: { id } } });
  });
  head.append(label, shown, remove);
  element.appendChild(head);

  const rows = drawRows(requireMessage(u.openRows, `${path}.open_rows`), `${path}.open_rows`);
  stampBucket(rows, bucket);
  element.appendChild(rows);
  if (rows.children.length === 0) element.setAttribute(EMPTY_SUBSECTION_ATTRIBUTE, "");
  return { element, rows };
}

/**
 * The tone class of the first of ROWS, depth-first in drawn order and nested
 * rows included, that stands on a working arm (`armWorks`), or null when
 * none does: what a folded subsection's name breathes in.
 */
export function workingTone(rows: RosterOpenRows, path: string): string | null {
  const walk = (at: PathedOpenRow): string | null => {
    const row = requireMessage(at.row.row, `${at.path}.row`);
    const arm = requireCase(row.status, `${at.path}.row.status`).case;
    if (armWorks(arm)) return rosterArmMark(arm).toneClass;
    for (const kin of openKin(at.row, at.path)) {
      const tone = walk(kin);
      if (tone !== null) return tone;
    }
    return null;
  };
  for (const top of pathedOpenRows(rows, path)) {
    const tone = walk(top);
    if (tone !== null) return tone;
  }
  return null;
}

/** The attribute a subsection's name states the tone it breathes in on, while it does. */
export const WORKING_TONE_ATTRIBUTE = "data-working-tone";

/** The class of a subsection's name. */
export const SUBSECTION_LABEL_CLASS = "sb-subsection-label";

/**
 * Paint on LABEL, the drawn name of subsection U, the breath its working rows
 * give it (`workingTone`): the name held at that tone and breathing, with its
 * ink box (`name-ink.ts`), or still. A breath already drawn in the same tone
 * is kept as it is, so a repaint never restarts its loop.
 */
export function paintSubsectionWork(label: HTMLElement, u: RosterSubsection, path: string): void {
  const header = requireMessage(u.header, `${path}.header`);
  const name = requireMessage(header.label, `${path}.header.label`).text;
  const tone = workingTone(requireMessage(u.openRows, `${path}.open_rows`), `${path}.open_rows`);
  const drawn = label.getAttribute(WORKING_TONE_ATTRIBUTE);
  if (drawn === tone) return;
  if (drawn !== null) {
    label.classList.remove(NAME_STEADY_CLASS, drawn, BREATHES_CLASS);
    label.removeAttribute(WORKING_TONE_ATTRIBUTE);
    for (const ink of [...label.children].filter((kid) => kid.classList.contains(NAME_INK_CLASS))) ink.remove();
  }
  if (tone === null) return;
  label.classList.add(NAME_STEADY_CLASS, tone, BREATHES_CLASS);
  label.setAttribute(WORKING_TONE_ATTRIBUTE, tone);
  drawNameInk(label, name);
}

/**
 * The "+" in a repository section's header: it asks for a name and adds a
 * subsection at the bottom of the repository's section.
 */
export function drawCreateSubsectionControl(repository: RepositoryRef, sc: SidebarContext): Control {
  const button = createControl();
  button.className = "sb-add";
  button.setAttribute("data-subsection-create", repository.id);
  button.textContent = "+";
  button.title = `new subsection in ${repository.dir}`;
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    openNamePopup(sc, button, {
      subject: "subsection",
      placeholder: "new subsection name",
      value: "",
      submitLabel: "Add",
      send: (name, control) => sendArrangement(sc, control, { case: "createSubsection", value: { repository, name } }),
    });
  });
  return button;
}

/** The refs a drop names, read from the roster last drawn. */
export interface ArrangeRefs {
  /** The workspace whose row is drawn with ID. */
  workspace(id: string): WorkspaceRef;
  /** The repository whose section is drawn with ID. */
  repository(id: string): RepositoryRef;
}

/**
 * Make every repository, subsection and top-level row the open workspaces'
 * grouping draws under ROOT draggable, through one listener on ROOT; see the
 * file comment. Answers the listener's teardown.
 */
export function installArrangeDrag(sc: SidebarContext, root: HTMLElement, refs: ArrangeRefs): () => void {
  const press = (event: PointerEvent): void => {
    if (event.button !== 0 || !event.isPrimary) return;
    const target = event.target as Element | null;
    if (target === null) return;
    // A CONTROL KEEPS ITS OWN PRESS: the row's menu, a header's "+", a
    // delete button and a text field are not grips.
    if (target.closest(`${CONTROL_SELECTOR}, button, input, textarea, select, a`) !== null) return;
    if (target.closest(`.${OPEN_REGION_CLASS}`) === null) return;
    const spec =
      subsectionDrag(sc, target, refs) ??
      repositoryBlockDrag(sc, target, refs) ??
      rowDrag(sc, target, refs) ??
      repositoryDrag(sc, target, refs);
    if (spec !== null) pressToDrag(sc.arrange, spec, event);
  };
  root.addEventListener("pointerdown", press);
  return () => root.removeEventListener("pointerdown", press);
}

/**
 * A press on a subsection's header drags it among its repository's entries:
 * dropped above a row it stands just before that row, above another
 * subsection just before that one, below them all at the bottom.
 */
function subsectionDrag(sc: SidebarContext, target: Element, refs: ArrangeRefs): DragSpec | null {
  const head = target.closest<HTMLElement>(`.${SUBSECTION_HEAD_CLASS}`);
  if (head === null) return null;
  const sub = head.parentElement;
  const list = sub?.parentElement ?? null;
  const section = sub?.closest<HTMLElement>(`[${REPOSITORY_ATTRIBUTE}]`) ?? null;
  if (sub === null || list === null || section === null) {
    throw new Error("invariant: a subsection's header sits in its subsection, in its repository's list");
  }
  const bucket = bucketOf(sub);
  // THE ONE-SHOT SUBSECTION STANDS WHERE THE DAEMON DRAWS IT, last.
  if (bucket.case !== "named") return null;
  return {
    kind: "subsection",
    item: sub,
    box: section,
    scroller: openRegion(section),
    lists: () => [list],
    itemsOf: entryItems,
    dropped: (_l, before) =>
      void sendArrangement(sc, sc.arrange.status, {
        case: "moveSubsection",
        value: {
          subsection: { id: bucket.id },
          to:
            before === null
              ? { case: "end", value: {} }
              : isSubsection(before)
                ? { case: "before", value: { id: namedId(before) } }
                : { case: "beforeWorkspace", value: refs.workspace(rowId(before)) },
        },
      }),
  };
}

/**
 * A press on a top-level row's line drags it among its repository's lists:
 * the section's list (its rows in no subsection and its subsections) and
 * each subsection's rows, the subsection it is over lit as the one it joins.
 */
function rowDrag(sc: SidebarContext, target: Element, refs: ArrangeRefs): DragSpec | null {
  const line = target.closest<HTMLElement>(".row");
  const pressed = line?.parentElement ?? null;
  if (pressed === null || !pressed.hasAttribute("data-roster-row")) return null;
  // A FAMILY MOVES WHOLE (owner request, 2026-10-20): a nested row is a grip
  // for its family, so the press lifts the family's top row, the one a
  // bucket's list holds, and a child dragged takes its ancestors along.
  const row = familyTop(pressed);
  if (row === null) return null;
  const section = row.closest<HTMLElement>(`[${REPOSITORY_ATTRIBUTE}]`);
  if (section === null) throw new Error("invariant: a bucketed row sits in its repository's section");
  const workspace = refs.workspace(rowId(row));
  return {
    kind: "workspace",
    item: row,
    box: section,
    scroller: openRegion(section),
    lists: () => queryAll<HTMLElement>(section, BUCKET_LIST_SELECTOR),
    itemsOf: entryItems,
    zoneOf: (l) => subsectionOf(l) ?? l,
    targetOf: subsectionOf,
    intoUnseen: showsNoRows,
    dropped: (l, before) => {
      const bucket = bucketOf(l);
      void sendArrangement(sc, sc.arrange.status, {
        case: "placeWorkspace",
        value: {
          workspace,
          into: bucket.case === "none" ? { case: "noSubsection", value: {} } : { case: "subsection", value: { id: bucket.id } },
          at:
            before === null
              ? { case: "end", value: {} }
              : isSubsection(before)
                ? { case: "beforeSubsection", value: { id: namedId(before) } }
                : { case: "before", value: refs.workspace(rowId(before)) },
        },
      });
    },
  };
}

/**
 * A press on a repository block's divider drags the block among its parent's
 * blocks, as one unit (agentrepl.v1.ArrangeMoveRepositoryBlock). ITS ONE LIST
 * IS THE PARENT'S `.kids` BOX, and its items are only the blocks there, so a
 * block can stand before another block or after them all, and never among the
 * parent's own-repository children, which always come first; nor can it land
 * under another parent.
 */
function repositoryBlockDrag(sc: SidebarContext, target: Element, refs: ArrangeRefs): DragSpec | null {
  const divider = target.closest<HTMLElement>(`.${REPOSITORY_DIVIDER_CLASS}`);
  const block = divider?.parentElement ?? null;
  if (block === null || !block.classList.contains(REPOSITORY_BLOCK_CLASS)) return null;
  const kids = block.parentElement;
  const parentRow = kids?.parentElement ?? null;
  if (kids === null || parentRow === null || !parentRow.hasAttribute("data-roster-row")) {
    throw new Error("invariant: a repository block sits in its parent row's family box");
  }
  const section = block.closest<HTMLElement>(`[${REPOSITORY_ATTRIBUTE}]`);
  if (section === null) throw new Error("invariant: a repository block sits in its repository's section");
  const parent = refs.workspace(rowId(parentRow));
  const repository = refs.repository(blockRepositoryId(block));
  return {
    kind: "repository-block",
    item: block,
    box: section,
    scroller: openRegion(section),
    lists: () => [kids],
    itemsOf: repositoryBlocks,
    dropped: (_l, before) =>
      void sendArrangement(sc, sc.arrange.status, {
        case: "moveRepositoryBlock",
        value: {
          parent,
          repository,
          to: before === null ? { case: "end", value: {} } : { case: "before", value: refs.repository(blockRepositoryId(before)) },
        },
      }),
  };
}

/** A family box's repository blocks, in order: the items a block drag moves among. */
export function repositoryBlocks(kids: HTMLElement): HTMLElement[] {
  return [...kids.children].filter((el): el is HTMLElement => el.classList.contains(REPOSITORY_BLOCK_CLASS));
}

/** The repository a drawn repository block is. */
function blockRepositoryId(block: HTMLElement): string {
  const id = block.getAttribute(REPOSITORY_BLOCK_ATTRIBUTE);
  if (id === null) throw new Error("invariant: a repository block states its repository");
  return id;
}

/**
 * The top row of ROW's family: ROW itself, or the nearest ancestor row,
 * that stands in a bucket's list. Null when none does (a row in no
 * arranged list).
 */
export function familyTop(row: HTMLElement): HTMLElement | null {
  for (let at: HTMLElement | null = row; at !== null; at = at.parentElement?.closest<HTMLElement>("[data-roster-row]") ?? null) {
    if (isBucketed(at.parentElement)) return at;
  }
  return null;
}

/**
 * A list's entries, in order: its top-level rows and, in a section's list,
 * its subsections.
 */
function entryItems(list: HTMLElement): HTMLElement[] {
  return [...list.children].filter(
    (el): el is HTMLElement => el.hasAttribute("data-roster-row") || el.classList.contains(SUBSECTION_CLASS),
  );
}

/** The id of a named subsection ITEM, an entry a drop names as its anchor. */
function namedId(item: HTMLElement): string {
  const bucket = bucketOf(item);
  if (bucket.case !== "named") throw new Error("invariant: a drop's anchor subsection is a named one");
  return bucket.id;
}

/** Whether ITEM is a subsection rather than a row. */
function isSubsection(item: HTMLElement): boolean {
  return item.classList.contains(SUBSECTION_CLASS);
}

/** The subsection whose rows LIST is, null for the section's own list. */
function subsectionOf(list: HTMLElement): HTMLElement | null {
  if (bucketOf(list).case === "none") return null;
  const sub = list.parentElement;
  if (sub === null || !isSubsection(sub)) throw new Error("invariant: a subsection's rows list sits in its subsection");
  return sub;
}

/**
 * Whether subsection SUB shows none of its rows: folded, or drawn empty
 * (`EMPTY_SUBSECTION_ATTRIBUTE`), as the drag found it when it lifted.
 */
export function showsNoRows(sub: HTMLElement): boolean {
  if (!isSubsection(sub)) throw new Error("invariant: a row drag's target is a subsection");
  return sub.classList.contains(FOLDED_CLASS) || sub.hasAttribute(EMPTY_SUBSECTION_ATTRIBUTE);
}

/** A press on a repository section's header drags it among the repositories. */
function repositoryDrag(sc: SidebarContext, target: Element, refs: ArrangeRefs): DragSpec | null {
  const head = target.closest<HTMLElement>(".repo-head");
  const section = head?.parentElement ?? null;
  if (section === null || !section.hasAttribute(REPOSITORY_ATTRIBUTE)) return null;
  const list = section.parentElement;
  if (list === null) throw new Error("invariant: a repository section sits in the grouping");
  const repository = refs.repository(repositoryId(section));
  return {
    kind: "repository",
    item: section,
    box: section,
    scroller: openRegion(section),
    lists: () => [list],
    itemsOf: (l) => [...l.children].filter((el): el is HTMLElement => el.hasAttribute(REPOSITORY_ATTRIBUTE)),
    dropped: (_l, before) =>
      void sendArrangement(sc, sc.arrange.status, {
        case: "moveRepository",
        value: {
          repository,
          to: before === null ? { case: "end", value: {} } : { case: "before", value: refs.repository(repositoryId(before)) },
        },
      }),
  };
}

/** The Open section's rows region SECTION scrolls in. */
function openRegion(section: HTMLElement): HTMLElement {
  const region = section.closest<HTMLElement>(`.${OPEN_REGION_CLASS}`);
  if (region === null) throw new Error("invariant: a repository section sits in the Open section's rows region");
  return region;
}

/** The workspace a drawn row is. */
function rowId(row: HTMLElement): string {
  const id = row.getAttribute("data-roster-row");
  if (id === null) throw new Error("invariant: a dragged row states its workspace");
  return id;
}

/** The repository a drawn section is. */
function repositoryId(section: HTMLElement): string {
  const id = section.getAttribute(REPOSITORY_ATTRIBUTE);
  if (id === null) throw new Error("invariant: a repository section states its repository");
  return id;
}
