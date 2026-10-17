/**
 * subsections — a repository section's user-named subsections, the "+" that
 * adds one, and the drags that arrange the repository grouping.
 *
 * A SUBSECTION IS A SEPARATION INSIDE ONE REPOSITORY'S SECTION
 * (frontend.v1.RosterSubsection), drawn below the section's own rows in the
 * daemon's order. A workspace is put in one by dragging its row there, and
 * the Emacs tab bar follows: a repository's unsectioned workspaces, then each
 * subsection's (`arrange.ts`). An empty subsection still draws, with a slot to
 * drop into.
 *
 * WHAT DRAGS. In the open workspaces' repository sections only, which are the
 * ones whose order the tab bar walks:
 *
 *   - a workspace row at the top of its list (a nested row moves with its
 *     parent), among its own repository's lists: the section's own rows and
 *     each subsection's;
 *   - a subsection, by its header, among its repository's subsections;
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
import type { RosterRepoSection, RosterRows, RosterSubsection } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { RepositoryRef, WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { childrenMatching, queryAll } from "../dom.js";
import { OPEN_REGION_CLASS } from "./band-fit.js";
import { log } from "../log.js";
import { requireMessage } from "../rpc/strict.js";
import { sendArrangement, type ArrangeChange } from "./arrange.js";
import type { SidebarContext } from "./context.js";
import { pressToDrag, type DragSpec } from "./drag.js";

/** The class of a repository section's subsections box, and of one subsection. */
export const SUBSECTIONS_CLASS = "sb-subsections";
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

/**
 * A repository's subsections, each drawn by DRAW_ROWS (the rail's one row
 * drawer, handed the subsection's index), or null when the repository has
 * none, so a repository without subsections keeps the one headed shape every
 * section shares.
 */
export function drawRosterSubsections(
  u: RosterRepoSection,
  sc: SidebarContext,
  path: string,
  drawRows: (rows: RosterRows, path: string, index: number) => HTMLElement,
): HTMLElement | null {
  if (u.subsections.length === 0) return null;
  const box = document.createElement("div");
  box.className = SUBSECTIONS_CLASS;
  u.subsections.forEach((sub, index) => {
    const subPath = `${path}.subsections[${index}]`;
    box.appendChild(drawRosterSubsection(sub, sc, subPath, (rows, rowsPath) => drawRows(rows, rowsPath, index)).element);
  });
  return box;
}

/** One subsection: its header (name, count, delete) and its rows. */
export function drawRosterSubsection(
  u: RosterSubsection,
  sc: SidebarContext,
  path: string,
  drawRows: (rows: RosterRows, path: string) => HTMLElement,
): DrawnSubsection {
  const id = requireMessage(u.key, `${path}.key`).subsectionId;
  const header = requireMessage(u.header, `${path}.header`);
  const name = requireMessage(header.label, `${path}.header.label`).text;
  const count = requireMessage(header.count, `${path}.header.count`).workspaces;
  const element = document.createElement("div");
  element.className = SUBSECTION_CLASS;
  element.setAttribute(BUCKET_ATTRIBUTE, id);

  const head = document.createElement("div");
  head.className = SUBSECTION_HEAD_CLASS;
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
  head.append(label, shown, remove);
  element.appendChild(head);

  const rows = drawRows(requireMessage(u.rows, `${path}.rows`), `${path}.rows`);
  rows.setAttribute(BUCKET_ATTRIBUTE, id);
  element.appendChild(rows);
  const empty = document.createElement("div");
  empty.className = "sb-subsection-empty";
  empty.textContent = "drag workspaces here";
  element.appendChild(empty);
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
 * Enter or the button sends; Escape, or a press anywhere outside it, closes
 * it. A blank name is not sent: the contract refuses one, so the popup keeps
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

  const outside = (event: PointerEvent): void => {
    if (!popup.contains(event.target as Node)) close();
  };
  const close = (): void => {
    popup.remove();
    document.removeEventListener("pointerdown", outside, true);
  };
  const send = (): void => {
    const name = input.value.trim();
    if (name === "") return;
    log.info("naming a sidebar subsection", { operation: "sidebar.subsection.name", context: { length: name.length } });
    void sendArrangement(sc, submit, spec.change(name)).then((ok) => {
      if (ok) close();
    });
  };
  submit.addEventListener("click", (event) => {
    event.preventDefault();
    send();
  });
  input.addEventListener("keydown", (event) => {
    if (event.key === "Enter") send();
    if (event.key === "Escape") close();
  });
  document.addEventListener("pointerdown", outside, true);
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
    const spec = subsectionDrag(sc, target) ?? rowDrag(sc, target, refs) ?? repositoryDrag(sc, target, refs);
    if (spec !== null) pressToDrag(sc.arrange, spec, event);
  };
  root.addEventListener("pointerdown", press);
  return () => root.removeEventListener("pointerdown", press);
}

/** A press on a subsection's header drags it among its repository's subsections. */
function subsectionDrag(sc: SidebarContext, target: Element): DragSpec | null {
  const head = target.closest<HTMLElement>(`.${SUBSECTION_HEAD_CLASS}`);
  if (head === null) return null;
  const sub = head.parentElement;
  const box = sub?.parentElement ?? null;
  const section = sub?.closest<HTMLElement>(`[${REPOSITORY_ATTRIBUTE}]`) ?? null;
  if (sub === null || box === null || section === null) {
    throw new Error("invariant: a subsection's header sits in its subsection, in its repository's section");
  }
  return {
    kind: "subsection",
    item: sub,
    box: section,
    scroller: openRegion(section),
    lists: () => [box],
    itemsOf: (l) => childrenMatching<HTMLElement>(l, `.${SUBSECTION_CLASS}`),
    dropped: (_l, before) =>
      void sendArrangement(sc, sc.arrange.status, {
        case: "moveSubsection",
        value: {
          subsection: { id: bucketOf(sub) },
          to: before === null ? { case: "end", value: {} } : { case: "before", value: { id: bucketOf(before) } },
        },
      }),
  };
}

/** A press on a top-level row's line drags it among its repository's lists. */
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
  const rowsOf = (l: HTMLElement): HTMLElement[] => [...l.children].filter((el): el is HTMLElement => el.hasAttribute("data-roster-row"));
  return {
    kind: "workspace",
    item: row,
    box: section,
    scroller: openRegion(section),
    lists: () => queryAll<HTMLElement>(section, `.rows[${BUCKET_ATTRIBUTE}]`),
    itemsOf: rowsOf,
    dropped: (l, before) => {
      const bucket = bucketOf(l);
      void sendArrangement(sc, sc.arrange.status, {
        case: "placeWorkspace",
        value: {
          workspace,
          into: bucket === "" ? { case: "noSubsection", value: {} } : { case: "subsection", value: { id: bucket } },
          at: before === null ? { case: "end", value: {} } : { case: "before", value: refs.workspace(rowId(before)) },
        },
      });
    },
  };
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
