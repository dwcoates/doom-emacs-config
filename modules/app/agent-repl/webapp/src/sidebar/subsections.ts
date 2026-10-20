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
 * A SUBSECTION FOLDS AS A REPOSITORY SECTION DOES (`section-fold.ts`): its
 * triangle asks UpdateSidebarView for the other fold, a collapsed
 * subsection's rows are hidden, and the daemon folds its workspaces' tabs off
 * the Emacs tab bar. Only the triangle folds it: the header's label renames
 * on a double-click, which a fold on every click would undo.
 *
 * WHAT DRAGS. In the open workspaces' repository sections only, which are the
 * ones whose order the tab bar walks:
 *
 *   - a workspace row at the top of its list (a nested row moves with its
 *     parent), among its own repository's lists: the section's list, where
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
import { sendArrangement, type ArrangeChange } from "./arrange.js";
import { subsectionFoldKey, type SidebarContext } from "./context.js";
import { drawFoldToggle, drawRosterSectionFold, sectionFold, trackDaemonFold } from "./section-fold.js";
import { pressToDrag, type DragSpec } from "./drag.js";

/** The class of one subsection. */
export const SUBSECTION_CLASS = "sb-subsection";

/** The class of a subsection's header: its drag handle. */
export const SUBSECTION_HEAD_CLASS = "sb-subsection-head";

/** The class of the popup that asks for a subsection's name. */
export const SUBSECTION_POPUP_CLASS = "sb-subsection-popup";

/** The attribute a rows list states its subsection on; empty for the section's own. */
export const BUCKET_ATTRIBUTE = "data-subsection";

/** The attribute a repository section states its repository's id on. */
export const REPOSITORY_ATTRIBUTE = "data-repository";

/** One subsection as drawn: its element and its rows list. */
export interface DrawnSubsection {
  element: HTMLElement;
  rows: HTMLElement;
}

/** One subsection: its header (fold, name, count, delete) and its rows. */
export function drawRosterSubsection(
  u: RosterSubsection,
  sc: SidebarContext,
  path: string,
  drawRows: (rows: RosterOpenRows, path: string) => HTMLElement,
): DrawnSubsection {
  const id = requireMessage(u.key, `${path}.key`).subsectionId;
  const header = requireMessage(u.header, `${path}.header`);
  const name = requireMessage(header.label, `${path}.header.label`).text;
  const count = requireMessage(header.count, `${path}.header.count`).workspaces;
  const element = document.createElement("div");
  element.className = SUBSECTION_CLASS;
  element.setAttribute(BUCKET_ATTRIBUTE, id);
  const foldKey = subsectionFoldKey(id);
  trackDaemonFold(element, foldKey, drawRosterSectionFold(u.fold, `${path}.fold`), sc);

  const head = document.createElement("div");
  head.className = SUBSECTION_HEAD_CLASS;
  const triangle = drawFoldToggle(element, sectionFold(sc, foldKey, { case: "subsection", value: { id } }));
  const label = document.createElement("span");
  label.className = "sb-subsection-label";
  label.textContent = name;
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
  const shown = document.createElement("span");
  shown.className = "sb-count";
  shown.textContent = `(${count})`;
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
  element.appendChild(head);

  const rows = drawRows(requireMessage(u.openRows, `${path}.open_rows`), `${path}.open_rows`);
  rows.setAttribute(BUCKET_ATTRIBUTE, id);
  element.appendChild(rows);
  if (rows.children.length === 0) element.setAttribute(EMPTY_SUBSECTION_ATTRIBUTE, "");
  return { element, rows };
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
 * Enter or the button sends; Escape, a press anywhere outside it, or the page
 * losing focus (a click into Emacs outside the page, which the page never
 * sees as a press) closes it. A blank name is not sent: the contract refuses one, so the popup keeps
 * waiting rather than provoking the refusal.
 */
export function openSubsectionNamePopup(sc: SidebarContext, anchor: HTMLElement, spec: NamePopupSpec): HTMLElement {
  for (const open of queryAll(sc.arrange.layer, `.${SUBSECTION_POPUP_CLASS}`)) open.remove();
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
  const blurred = (): void => close("the page lost focus");
  const close = (why: string): void => {
    log.debug("closing the subsection name popup", { operation: "sidebar.subsection.popup-close", context: { why } });
    popup.remove();
    win.document.removeEventListener("pointerdown", outside, true);
    win.removeEventListener("blur", blurred);
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
  win.addEventListener("blur", blurred);
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
          subsection: { id: bucketOf(sub) },
          to:
            before === null
              ? { case: "end", value: {} }
              : isSubsection(before)
                ? { case: "before", value: { id: bucketOf(before) } }
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
  const row = line?.parentElement ?? null;
  // A NESTED ROW IS NO GRIP: it moves only with its parent, whose list is
  // the one a bucket names.
  if (row === null || !row.hasAttribute("data-roster-row") || row.parentElement?.hasAttribute(BUCKET_ATTRIBUTE) !== true) {
    return null;
  }
  const section = row.closest<HTMLElement>(`[${REPOSITORY_ATTRIBUTE}]`);
  if (section === null) throw new Error("invariant: a bucketed row sits in its repository's section");
  const workspace = refs.workspace(rowId(row));
  return {
    kind: "workspace",
    item: row,
    box: section,
    scroller: openRegion(section),
    lists: () => queryAll<HTMLElement>(section, `.rows[${BUCKET_ATTRIBUTE}]`),
    itemsOf: entryItems,
    zoneOf: (l) => subsectionOf(l) ?? l,
    targetOf: subsectionOf,
    dropped: (l, before) => {
      const bucket = bucketOf(l);
      void sendArrangement(sc, sc.arrange.status, {
        case: "placeWorkspace",
        value: {
          workspace,
          into: bucket === "" ? { case: "noSubsection", value: {} } : { case: "subsection", value: { id: bucket } },
          at:
            before === null
              ? { case: "end", value: {} }
              : isSubsection(before)
                ? { case: "beforeSubsection", value: { id: bucketOf(before) } }
                : { case: "before", value: refs.workspace(rowId(before)) },
        },
      });
    },
  };
}

/** A list's entries, in order: its top-level rows and, in a section's list, its subsections. */
function entryItems(list: HTMLElement): HTMLElement[] {
  return [...list.children].filter(
    (el): el is HTMLElement => el.hasAttribute("data-roster-row") || el.classList.contains(SUBSECTION_CLASS),
  );
}

/** Whether ITEM is a subsection rather than a row. */
function isSubsection(item: HTMLElement): boolean {
  return item.classList.contains(SUBSECTION_CLASS);
}

/** The subsection whose rows LIST is, null for the section's own list. */
function subsectionOf(list: HTMLElement): HTMLElement | null {
  if (bucketOf(list) === "") return null;
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

/** The subsection a list or subsection element states, "" for none. */
function bucketOf(element: HTMLElement): string {
  const bucket = element.getAttribute(BUCKET_ATTRIBUTE);
  if (bucket === null) throw new Error("invariant: a drop list states its subsection");
  return bucket;
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
