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
 * (`arrange.ts`). Its top-level rows are not indented under its header. An empty
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
 *   - a nested row, with its own descendants (`childDrag`): among its
 *     parent's children it reorders the siblings; over the middle of another
 *     row's line it becomes that row's child; anywhere among its own
 *     repository's top-level rows and subsections it leaves its parent and
 *     stands there, top-level;
 *   - a top-level workspace row's whole family among its own
 *     repository's lists: the section's list, where
 *     it may stand just above a subsection, and each subsection's rows. Over
 *     a subsection (its header or its rows) it lands in that subsection,
 *     which lights up (`DROP_TARGET_CLASS`) while it would. A folded or
 *     empty subsection shows none of its rows, so over one the drag also
 *     draws the row going inside it (`DragSpec.intoUnseen`, `drag.ts`).
 *     Over the middle of another row's line it lands among that row's
 *     children, and the row lights up the same way: the drop makes the
 *     dragged workspace that row's child (`nest.ts`). The line's top and
 *     bottom quarters still place it before or after the row, as a drop
 *     between rows does;
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
import { familyRows, pathedOpenRows } from "./entries.js";
import { BREATHES_CLASS, NAME_INK_CLASS, drawNameInk } from "./name-ink.js";
import { NAME_STEADY_CLASS } from "./name-tint.js";
import { armWorks, rosterArmMark } from "./tones.js";
import type { RepositoryRef, WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { childrenMatching, queryAll } from "../dom.js";
import { EMPTY_SUBSECTION_ATTRIBUTE, OPEN_REGION_CLASS } from "./band-fit.js";
import { requireMessage } from "../rpc/strict.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { sendArrangement, type ArrangeChange } from "./arrange.js";
import { openNamePopup } from "./name-popup.js";
import { subsectionFoldKey, type SidebarContext } from "./context.js";
import { drawRosterSectionFold, foldOnClick, sectionFold, trackDaemonFold } from "./section-fold.js";
import { pressToDrag, type DragSpec, type Zone } from "../drag.js";
import { sendNest, sendUnnest } from "./nest.js";
import {
  KIDS_CLASS,
  kidsBoxOf,
  REPOSITORY_BLOCK_ATTRIBUTE,
  REPOSITORY_BLOCK_CLASS,
  REPOSITORY_DIVIDER_CLASS,
} from "./row.js";
import { drawRosterOpenCount, showWorkspaceCountParenthetical, statusCountOf } from "./open-count.js";

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
  // A count of no workspaces draws nothing; any other count is drawn.
  const shown =
    count.workspaces === 0
      ? null
      : showWorkspaceCountParenthetical(
          count.workspaces,
          statusCountOf(
            pathedOpenRows(requireMessage(u.openRows, `${path}.open_rows`), `${path}.open_rows`)
              .flatMap(({ row, path: rowPath }) => familyRows(row, rowPath))
              .map(({ row, path: rowPath }) => ({ row: row.row, path: `${rowPath}.row` })),
          ),
        );
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
  head.append(label, ...(shown === null ? [] : [shown]), remove);
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
  for (const top of pathedOpenRows(rows, path)) {
    for (const at of familyRows(top.row, top.path)) {
      const arm = requireCase(requireMessage(at.row.row, `${at.path}.row`).status, `${at.path}.row.status`).case;
      if (armWorks(arm)) return rosterArmMark(arm).toneClass;
    }
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
    boxes: [section],
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
 * the section's list (its rows in no subsection and its subsections), each
 * subsection's rows, the subsection it is over lit as the one it joins, and
 * every other row's children (`nestLists`), the row it is over lit as the
 * parent it joins. A press on a NESTED row drags that row alone among its
 * parent's children (`childDrag`).
 */
function rowDrag(sc: SidebarContext, target: Element, refs: ArrangeRefs): DragSpec | null {
  const line = target.closest<HTMLElement>(".row");
  const pressed = line?.parentElement ?? null;
  if (pressed === null || !pressed.hasAttribute("data-roster-row")) return null;
  if (!isBucketed(pressed.parentElement)) return childDrag(sc, pressed, refs);
  const row = pressed;
  const section = row.closest<HTMLElement>(`[${REPOSITORY_ATTRIBUTE}]`);
  if (section === null) throw new Error("invariant: a bucketed row sits in its repository's section");
  const workspace = refs.workspace(rowId(row));
  return {
    kind: "workspace",
    item: row,
    boxes: [section],
    scroller: openRegion(section),
    lists: () => [...queryAll<HTMLElement>(section, BUCKET_LIST_SELECTOR), ...nestLists(section, row)],
    itemsOf: (l) => (isNestList(l) ? nestItems(l) : entryItems(l)),
    zoneOf: (l) => (isNestList(l) ? nestZone(l, row) : (subsectionOf(l) ?? l).getBoundingClientRect()),
    targetOf: (l) => (isNestList(l) ? nestParent(l) : subsectionOf(l)),
    intoUnseen: (target) => !isNestTarget(target) && showsNoRows(target),
    dropped: (l, before) => {
      if (isNestList(l)) {
        void sendNest(sc, sc.arrange.status, workspace, refs.workspace(rowId(nestParent(l))));
        return;
      }
      void sendArrangement(sc, sc.arrange.status, placement(workspace, l, before, refs));
    },
  };
}

/**
 * THE ONE PLACEMENT A ROW DROPPED INTO A BUCKET'S LIST ASKS FOR, whether it
 * was top-level already (`rowDrag`) or is made top-level first (`childDrag`):
 * WORKSPACE into LIST's bucket, just before BEFORE (a row or a subsection),
 * or after every entry there when BEFORE is null.
 */
export function placement(workspace: WorkspaceRef, list: HTMLElement, before: HTMLElement | null, refs: ArrangeRefs): ArrangeChange {
  const bucket = bucketOf(list);
  return {
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
  };
}

/**
 * A press on a NESTED row drags that row, with its own descendants, and
 * offers it three kinds of place (owner request, 2026-11-23):
 *
 *   - ITS OWN BOX, the parent's `.kids` or, for a child of another
 *     repository, that repository's block: a drop there reorders it among its
 *     siblings (agentrepl.v1.ArrangeMoveChild);
 *   - every other row's box of nested rows outside its own family and other
 *     than its parent's (`nestLists`): a drop there makes it that row's child
 *     (`sendNest`), as a top-level row's drop does;
 *   - the bucket lists of ITS OWN REPOSITORY's section (`ownBucketLists`): a
 *     drop there makes it top-level, standing where it was dropped
 *     (`sendUnnest`). A child of another repository than its section's goes
 *     top-level only in its own repository's section, so those are the lists
 *     it is offered, and that section is rearranged too.
 *
 * Its own box claims its own span, so over its siblings it reorders; a line's
 * middle half still nests, and anywhere outside the box (a top-level row's
 * line, a subsection) frees it from its parent.
 */
function childDrag(sc: SidebarContext, row: HTMLElement, refs: ArrangeRefs): DragSpec {
  const box = row.parentElement;
  if (box === null || !(box.classList.contains(KIDS_CLASS) || box.classList.contains(REPOSITORY_BLOCK_CLASS))) {
    throw new Error("invariant: a nested row stands in its parent's box of nested rows or in a repository block");
  }
  const section = row.closest<HTMLElement>(`[${REPOSITORY_ATTRIBUTE}]`);
  if (section === null) throw new Error("invariant: a nested row sits in its repository's section");
  const parentRow = box.closest<HTMLElement>("[data-roster-row]");
  if (parentRow === null) throw new Error("invariant: a nested row's box sits in its parent's row");
  const parentKids = kidsBoxOf(parentRow);
  const home = ownSection(section, row);
  const workspace = refs.workspace(rowId(row));
  return {
    kind: "child",
    item: row,
    boxes: home === null || home === section ? [section] : [section, home],
    scroller: openRegion(section),
    lists: () => [
      box,
      ...nestLists(section, row).filter((kids) => kids !== parentKids),
      ...(home === null ? [] : queryAll<HTMLElement>(home, BUCKET_LIST_SELECTOR)),
    ],
    itemsOf: (l) =>
      l === box ? childrenMatching<HTMLElement>(l, "[data-roster-row]") : isNestList(l) ? nestItems(l) : entryItems(l),
    zoneOf: (l) =>
      l === box ? l.getBoundingClientRect() : isNestList(l) ? nestZone(l, row) : (subsectionOf(l) ?? l).getBoundingClientRect(),
    targetOf: (l) => (l === box ? null : isNestList(l) ? nestParent(l) : subsectionOf(l)),
    intoUnseen: (target) => !isNestTarget(target) && showsNoRows(target),
    dropped: (l, before) => {
      if (l === box) {
        void sendArrangement(sc, sc.arrange.status, {
          case: "moveChild",
          value: { workspace, to: before === null ? { case: "end", value: {} } : { case: "before", value: refs.workspace(rowId(before)) } },
        });
        return;
      }
      if (isNestList(l)) {
        void sendNest(sc, sc.arrange.status, workspace, refs.workspace(rowId(nestParent(l))));
        return;
      }
      void sendUnnest(sc, sc.arrange.status, workspace, placement(workspace, l, before, refs));
    },
  };
}

/**
 * The repository section a nested ROW drawn in SECTION would stand in at the
 * top level: its own repository's. A row in a repository block is of the
 * block's repository; any other nested row is of its section's, or of the
 * nearest enclosing block's. Null when that repository draws no section in
 * the Open grouping, so there is no top level to drop the row at.
 */
function ownSection(section: HTMLElement, row: HTMLElement): HTMLElement | null {
  const block = row.parentElement?.closest<HTMLElement>(`.${REPOSITORY_BLOCK_CLASS}`) ?? null;
  if (block === null) return section;
  const grouping = section.parentElement;
  if (grouping === null) throw new Error("invariant: a repository section sits in the grouping");
  const id = blockRepositoryId(block);
  return childrenMatching<HTMLElement>(grouping, `[${REPOSITORY_ATTRIBUTE}]`).find((other) => repositoryId(other) === id) ?? null;
}

/**
 * The lists a row ITEM dragged in SECTION nests through: the box of nested
 * rows of every other row there, outside ITEM's own family, made empty for a
 * row that has none (`kidsBoxOf`). An empty box draws nothing, and the drag
 * marks SECTION for a redraw from its wire (`REARRANGED_ATTRIBUTE`), so one
 * the drop did not fill never outlives the next push.
 */
function nestLists(section: HTMLElement, item: HTMLElement): HTMLElement[] {
  return queryAll<HTMLElement>(section, "[data-roster-row]")
    .filter((ws) => !item.contains(ws))
    .map(kidsBoxOf);
}

/** Whether LIST is a row's box of nested rows rather than a bucket's list. */
function isNestList(list: HTMLElement): boolean {
  return list.classList.contains(KIDS_CLASS);
}

/**
 * Whether TARGET, a row drag's lit target, is a row a drop would nest under
 * rather than a subsection. A nest drop's slot stands in the row's own box
 * of nested rows, under its line, so it is never drawn going inside unseen.
 */
function isNestTarget(target: HTMLElement): boolean {
  return target.hasAttribute("data-roster-row");
}

/** The row whose nested rows KIDS is: the parent a drop there joins. */
function nestParent(kids: HTMLElement): HTMLElement {
  const ws = kids.parentElement;
  if (ws === null || !ws.hasAttribute("data-roster-row")) throw new Error("invariant: a box of nested rows sits in its row");
  return ws;
}

/** A box of nested rows' entries, in order: its rows and its repository blocks. */
function nestItems(kids: HTMLElement): HTMLElement[] {
  return [...kids.children].filter(
    (el): el is HTMLElement => el.hasAttribute("data-roster-row") || el.classList.contains(REPOSITORY_BLOCK_CLASS),
  );
}

/**
 * The span KIDS claims while ITEM is dragged: the middle half of its row's
 * line, so the line's top and bottom quarters still place ITEM before or
 * after the row. ONCE ITEM STANDS THERE the span reaches down to ITEM's
 * slot: the slot's arrival below the line moves the rows above it when ITEM
 * came from above, and the pointer, now over the slot, still nests rather
 * than throwing the slot back out.
 */
function nestZone(kids: HTMLElement, item: HTMLElement): Zone {
  const line = childrenMatching<HTMLElement>(nestParent(kids), ".row")[0];
  if (line === undefined) throw new Error("invariant: a row draws its line");
  const box = line.getBoundingClientRect();
  const quarter = (box.bottom - box.top) / 4;
  const bottom = item.parentElement === kids ? item.getBoundingClientRect().bottom : box.bottom - quarter;
  return { top: box.top + quarter, bottom };
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
    boxes: [section],
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
    boxes: [section],
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
