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
 * THE ONE-SHOT SUBSECTION (frontend.v1.RosterSubsectionOneShot) is the
 * daemon's, not the user's: it holds every family with an open workspace
 * marked one-shot, and stands last in its repository while it has any. It
 * is drawn as a subsection is, without the rename or the delete, and it never
 * collapses. A row dragged into it marks its workspace one-shot
 * (SetWorkspaceOneShot), and a row dragged out of it is placed where it was
 * dropped (ArrangeSidebar), which clears the mark. Its own header does not
 * drag, and nothing is dropped before it: it is always last.
 *
 * A SUBSECTION FOLDS AS A REPOSITORY SECTION DOES (`section-fold.ts`): its
 * triangle asks UpdateSidebarView for the other fold, a collapsed
 * subsection's rows are hidden, and the daemon folds its workspaces' tabs off
 * the Emacs tab bar. Only the triangle folds it: the header's label renames
 * on a double-click, which a fold on every click would undo.
 *
 * WHAT DRAGS. In the open workspaces' repository sections only, which are the
 * ones whose order the tab bar walks:
 *
 *   - a workspace row's family, by any of its rows (a nested row lifts its
 *     family's top row, so a child takes its ancestors along), among its own
 *     repository's lists: the section's list, where
 *     it may stand just above a subsection, and each subsection's rows. Over
 *     a subsection (its header or its rows) it lands in that subsection,
 *     which lights up (`DROP_TARGET_CLASS`) while it would;
 *   - a subsection, by its header, among its repository's rows and
 *     subsections: dropped onto a row's line it stands just above that row;
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
import { CONTROL_SELECTOR, createControl, type Control } from "../control.js";
import type { RosterOpenRows, RosterSubsection } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { RepositoryRef, WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { queryAll, windowOf } from "../dom.js";
import { EMPTY_SUBSECTION_ATTRIBUTE, OPEN_REGION_CLASS } from "./band-fit.js";
import { log } from "../log.js";
import { requireMessage } from "../rpc/strict.js";
import {
  SetWorkspaceOneShotResponseSchema,
  type SetWorkspaceOneShotError,
  type SetWorkspaceOneShotResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_one_shot_pb";
import { MalformedView } from "../rpc/malformed.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { sendArrangement, sendHeld, type ArrangeChange } from "./arrange.js";
import { subsectionFoldKey, type SidebarContext } from "./context.js";
import { drawFoldToggle, drawRosterSectionFold, neverFolds, sectionFold, trackDaemonFold } from "./section-fold.js";
import { pressToDrag, type DragSpec } from "./drag.js";

/** The class of one subsection. */
export const SUBSECTION_CLASS = "sb-subsection";

/** The class of a subsection's header: its drag handle. */
export const SUBSECTION_HEAD_CLASS = "sb-subsection-head";

/** The class of the popup that asks for a subsection's name. */
export const SUBSECTION_POPUP_CLASS = "sb-subsection-popup";

/** The attribute a rows list states its subsection on; empty for the section's own. */
export const BUCKET_ATTRIBUTE = "data-subsection";

/**
 * The attribute the one-shot subsection and its rows list carry, in place of
 * `BUCKET_ATTRIBUTE`: the one-shot subsection has no subsection id, so no id
 * could ever be mistaken for it.
 */
export const ONE_SHOT_ATTRIBUTE = "data-one-shot";

/** Every rows list a top-level row may stand in: the section's own and each subsection's. */
export const BUCKET_LIST_SELECTOR = `.rows[${BUCKET_ATTRIBUTE}], .rows[${ONE_SHOT_ATTRIBUTE}]`;

/**
 * Which list of a repository's section a rows list or subsection element is:
 * the section's own (`none`), a subsection the user named, or the one-shot
 * subsection.
 */
export type Bucket = { case: "none" } | { case: "named"; id: string } | { case: "oneShot" };

/** Stamp BUCKET on ELEMENT, a rows list or subsection, as `bucketOf` reads it. */
export function stampBucket(element: HTMLElement, bucket: Bucket): void {
  switch (bucket.case) {
    case "none":
      element.setAttribute(BUCKET_ATTRIBUTE, "");
      return;
    case "named":
      element.setAttribute(BUCKET_ATTRIBUTE, bucket.id);
      return;
    case "oneShot":
      element.setAttribute(ONE_SHOT_ATTRIBUTE, "");
      return;
  }
}

/** The bucket ELEMENT, a rows list or subsection, was stamped with (`stampBucket`). */
export function bucketOf(element: HTMLElement): Bucket {
  if (element.hasAttribute(ONE_SHOT_ATTRIBUTE)) {
    if (element.hasAttribute(BUCKET_ATTRIBUTE)) throw new Error("invariant: a one-shot list states no subsection id");
    return { case: "oneShot" };
  }
  const id = element.getAttribute(BUCKET_ATTRIBUTE);
  if (id === null) throw new Error("invariant: a drop list states its subsection");
  return id === "" ? { case: "none" } : { case: "named", id };
}

/** Whether ELEMENT is stamped with a bucket: a rows list a row may stand in, or a subsection. */
function isBucketed(element: Element | null): boolean {
  return element !== null && (element.hasAttribute(BUCKET_ATTRIBUTE) || element.hasAttribute(ONE_SHOT_ATTRIBUTE));
}

/** The bucket subsection U is: named by its id, or the one-shot subsection. EVERY kind is named; unset is malformed. */
export function subsectionBucket(u: RosterSubsection, path: string): Bucket {
  const kind = requireCase(u.kind, `${path}.kind`);
  switch (kind.case) {
    case "named":
      return { case: "named", id: requireMessage(kind.value.key, `${path}.kind.named.key`).subsectionId };
    case "oneShot":
      return { case: "oneShot" };
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
    case "oneShot":
      return "the one-shot subsection";
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
 * One subsection: its header (fold, name, count, and for a named one the
 * delete) and its rows. A named subsection renames on a double-click of its
 * name; the one-shot subsection's name is the daemon's, and it never folds.
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
  const count = requireMessage(header.count, `${path}.header.count`).workspaces;
  const element = document.createElement("div");
  element.className = SUBSECTION_CLASS;
  stampBucket(element, bucket);

  const head = document.createElement("div");
  head.className = SUBSECTION_HEAD_CLASS;
  const label = document.createElement("span");
  label.className = "sb-subsection-label";
  label.textContent = name;
  const shown = document.createElement("span");
  shown.className = "sb-count";
  shown.textContent = `(${count})`;
  const neverCollapses = u.fold.case === "expanded" && u.fold.value.collapse.case === "neverCollapses";
  if (bucket.case === "named") {
    if (neverCollapses) throw new MalformedView(`${path}.fold`, "a named subsection is never one that never collapses");
    const id = bucket.id;
    const foldKey = subsectionFoldKey(id);
    trackDaemonFold(element, foldKey, drawRosterSectionFold(u.fold, `${path}.fold`), sc);
    const triangle = drawFoldToggle(element, sectionFold(sc, foldKey, { case: "subsection", value: { id } }));
    label.title = "double-click to rename";
    label.addEventListener("dblclick", (event) => {
      event.preventDefault();
      openSubsectionNamePopup(sc, label, {
        placeholder: "subsection name",
        value: name,
        submitLabel: "Rename",
        change: (newName) => ({ case: "renameSubsection", value: { subsection: { id }, name: newName } }),
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
    head.append(triangle, label, shown, remove);
  } else {
    if (!neverCollapses) throw new MalformedView(`${path}.fold`, "the one-shot subsection is always expanded and never collapses");
    head.append(drawFoldToggle(element, neverFolds(ONE_SHOT_FOLD_KEY)), label, shown);
  }
  element.appendChild(head);

  const rows = drawRows(requireMessage(u.openRows, `${path}.open_rows`), `${path}.open_rows`);
  stampBucket(rows, bucket);
  element.appendChild(rows);
  if (rows.children.length === 0) element.setAttribute(EMPTY_SUBSECTION_ATTRIBUTE, "");
  return { element, rows };
}

/** The fold key the one-shot subsection's gesture names in its records; it holds no view state. */
const ONE_SHOT_FOLD_KEY = "one-shot";

/**
 * SetWorkspaceOneShot's arms of its own. `unknown_workspace` and
 * `workspace_ref_mismatch` are cross-cutting, worded once in `rpc/refusal.ts`.
 */
export function setWorkspaceOneShotRefusal(cause: NonNullable<SetWorkspaceOneShotError["cause"]> & { case: string }): string {
  switch (cause.case) {
    case "closed":
      return "the workspace is closed";
    case "noCreationJob":
      return "a registered workspace cannot be a one-shot";
    case "ownerLands":
      return "its owner lands this workspace, so it is never a one-shot";
    case "requesterMayNotSet":
      return "only the workspace, its owner or the user may mark it";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("SetWorkspaceOneShotError.cause", other.case);
    }
  }
}

/**
 * Mark WORKSPACE a one-shot, as the user asks by dropping its row into the
 * one-shot subsection. Its family is drawn there once the daemon's push
 * carries the mark, so the drawing is held until then (`sendHeld`).
 */
export function markOneShot(sc: SidebarContext, workspace: WorkspaceRef): Promise<boolean> {
  log.info("marking a workspace one-shot from the sidebar", {
    operation: "sidebar.one-shot.mark",
    context: { workspace: workspace.id },
  });
  return sendHeld(sc, sc.arrange.status, {
    rpc: "SetWorkspaceOneShot",
    call: (client) =>
      client.setWorkspaceOneShot({ workspace, requester: { asker: { case: "user", value: {} } }, oneShot: true }),
    schema: SetWorkspaceOneShotResponseSchema,
    refusalText: (cause) => setWorkspaceOneShotRefusal(cause as never),
    revision: (response: SetWorkspaceOneShotResponse) =>
      response.result.case === "success" ? response.result.value.rosterRevision : null,
  });
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
    openSubsectionNamePopup(sc, button, {
      placeholder: "new subsection name",
      value: "",
      submitLabel: "Add",
      change: (name) => ({ case: "createSubsection", value: { repository, name } }),
    });
  });
  return button;
}

/** What a name popup asks for and what it sends. */
export interface NamePopupSpec {
  placeholder: string;
  value: string;
  submitLabel: string;
  /** The change the popup sends with the name typed. */
  change: (name: string) => ArrangeChange;
}

/**
 * Open the name popup under ANCHOR, replacing any open one.
 *
 * Enter or the button sends; Escape or a press anywhere outside it closes it.
 * It is one of the rail's dropdowns (`dropdowns.ts`), so opening another
 * dropdown closes it, and so does the rail losing the user (`transients.ts`:
 * the page losing focus, as a click into Emacs outside the page does, which
 * the page never sees as a press). A blank name is not sent: the contract
 * refuses one, so the popup keeps waiting rather than provoking the refusal.
 */
export function openSubsectionNamePopup(sc: SidebarContext, anchor: HTMLElement, spec: NamePopupSpec): HTMLElement {
  const popup = document.createElement("div");
  popup.className = SUBSECTION_POPUP_CLASS;
  popup.setAttribute("data-subsection-popup", "");
  const box = anchor.getBoundingClientRect();
  popup.style.top = `${box.bottom + 4}px`;
  popup.style.left = `${Math.max(4, box.right - 220)}px`;

  const input = document.createElement("input");
  input.type = "text";
  input.className = "sb-subsection-name";
  input.setAttribute("data-subsection-name", "");
  input.placeholder = spec.placeholder;
  input.value = spec.value;
  const submit = createControl();
  submit.className = "sb-form-go";
  submit.setAttribute("data-subsection-submit", "");
  submit.textContent = spec.submitLabel;
  popup.append(input, submit);

  const win = windowOf(anchor);
  const outside = (event: PointerEvent): void => {
    if (!popup.contains(event.target as Node)) close("a press outside it");
  };
  const close = (why: string): void => {
    log.debug("closing the subsection name popup", { operation: "sidebar.subsection.popup-close", context: { why } });
    popup.remove();
    win.document.removeEventListener("pointerdown", outside, true);
    sc.dropdowns.released(popup);
  };
  const send = (): void => {
    const name = input.value.trim();
    if (name === "") return;
    log.info("naming a sidebar subsection", { operation: "sidebar.subsection.name", context: { length: name.length } });
    void sendArrangement(sc, submit, spec.change(name)).then((ok) => {
      if (ok) close("the name was sent");
    });
  };
  submit.addEventListener("click", (event) => {
    event.preventDefault();
    send();
  });
  input.addEventListener("keydown", (event) => {
    if (event.key === "Enter") send();
    if (event.key === "Escape") close("Escape");
  });
  win.document.addEventListener("pointerdown", outside, true);
  // Registered before it is drawn, so the popup it replaces is closed (and
  // stops listening) first.
  sc.dropdowns.opened({ kind: "subsection-popup", element: popup, openers: [anchor], close });
  log.debug("opening the subsection name popup", {
    operation: "sidebar.subsection.popup-open",
    context: { submit: spec.submitLabel },
  });
  sc.arrange.layer.appendChild(popup);
  input.focus();
  return popup;
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
    const spec = subsectionDrag(sc, target, refs) ?? rowDrag(sc, target, refs) ?? repositoryDrag(sc, target, refs);
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
  // Read at the press: the drag moves ROW itself into the list it would join.
  const fromOneShot = bucketOf(row.parentElement as HTMLElement).case === "oneShot";
  return {
    kind: "workspace",
    item: row,
    box: section,
    scroller: openRegion(section),
    lists: () => queryAll<HTMLElement>(section, BUCKET_LIST_SELECTOR),
    itemsOf: entryItems,
    zoneOf: (l) => subsectionOf(l) ?? l,
    targetOf: subsectionOf,
    dropped: (l, before) => {
      const bucket = bucketOf(l);
      if (bucket.case === "oneShot") {
        // INTO THE ONE-SHOT SUBSECTION IS A MARK, never a placement: its
        // rows stand in roster order, so where among them it fell says
        // nothing, and a row already there was not moved out of it.
        if (fromOneShot) {
          log.debug("a row was reordered inside the one-shot subsection; its order is the roster's", {
            operation: "sidebar.one-shot.reorder",
            context: { workspace: workspace.id },
          });
          sc.arrange.hold.cancel();
          return;
        }
        void markOneShot(sc, workspace);
        return;
      }
      // OUT OF THE ONE-SHOT SUBSECTION IS A PLACEMENT, which clears the
      // family's one-shot marks (ArrangePlaceWorkspace).
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
 * its named subsections. The one-shot subsection is no entry: it is always
 * last, so nothing stands before it, and a drop below every entry lands at the
 * end, just above it.
 */
function entryItems(list: HTMLElement): HTMLElement[] {
  return [...list.children].filter(
    (el): el is HTMLElement =>
      el.hasAttribute("data-roster-row") || (el.classList.contains(SUBSECTION_CLASS) && !el.hasAttribute(ONE_SHOT_ATTRIBUTE)),
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
