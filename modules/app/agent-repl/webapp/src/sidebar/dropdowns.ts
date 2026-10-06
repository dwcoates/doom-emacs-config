/**
 * dropdowns — the ONE dismiss mechanism every sidebar dropdown shares (owner
 * ruling, 2026-10-06): a row's "⋯" menu, a task's "⋯" menu and a row's detail
 * popover (the status indicator's dropdown).
 *
 * - A click anywhere in the rail outside every open dropdown closes them all;
 *   the user never has to click the opener again.
 * - A click inside an open dropdown, or on its own opener, keeps it open.
 * - Opening one closes whichever other one was open: at most one stands.
 *
 * WHY NOT THE TOPBAR'S REVEAL LAYER. `topbar/reveal.ts` answers the same two
 * gestures, but it is a positioned layer that rebuilds its reveals from
 * registered builders, because the topbar's reveals live outside the strip.
 * The rail's dropdowns live INSIDE the boxes they belong to and are drawn with
 * them, so what they share is only the dismiss rule, and this is it.
 *
 * TRANSIENT, NEVER VIEW STATE. An open dropdown is the page's own gesture in
 * progress; it is never the daemon's shared view (`view.ts`).
 */
import { log } from "../log.js";

/** One open dropdown, as the registry holds it. */
export interface Dropdown {
  /** What it is, for the log ("row-menu", "task-menu", "row-detail"). */
  kind: string;
  /**
   * The ONE logical dropdown this box is a copy of, when it has copies: a row
   * is drawn once per grouping, so its detail popover is too, and opening one
   * copy must not close the other. Omitted, the box is its own identity.
   */
  key?: string;
  /** Its own box: a click inside it keeps it open. */
  element: HTMLElement;
  /** The controls that open it: a click on one is the opener's, not a dismiss. */
  openers: readonly HTMLElement[];
  /** Close it: hide the box and forget whatever the page remembered of it. */
  close(): void;
}

export interface Dropdowns {
  /** A new roster is being drawn: every dropdown of the last one is gone. */
  beginDraw(): void;
  /** DROPDOWN is open: close every other open one first. */
  opened(dropdown: Dropdown): void;
  /** The dropdown boxed by ELEMENT closed by its own gesture. */
  released(element: HTMLElement): void;
  /** A click landed on TARGET: close every open dropdown it is outside of. */
  dismissOutside(target: EventTarget | null): void;
}

/** Whether A and B are the same logical dropdown. */
function same(a: Dropdown, b: Dropdown): boolean {
  return a.element === b.element || (a.key !== undefined && a.key === b.key);
}

export function createDropdowns(): Dropdowns {
  let open: Dropdown[] = [];

  const close = (dropdown: Dropdown, why: string): void => {
    log.debug("closing a sidebar dropdown", {
      operation: "sidebar.dropdowns.close",
      context: { kind: dropdown.kind, why },
    });
    dropdown.close();
  };

  return {
    beginDraw(): void {
      open = [];
    },
    opened(dropdown: Dropdown): void {
      const others = open.filter((d) => !same(d, dropdown));
      open = [...open.filter((d) => same(d, dropdown) && d.element !== dropdown.element), dropdown];
      for (const other of others) close(other, "another dropdown opened");
    },
    released(element: HTMLElement): void {
      open = open.filter((d) => d.element !== element);
    },
    dismissOutside(target: EventTarget | null): void {
      if (open.length === 0) return;
      const node = target instanceof Node ? target : null;
      const hit = (d: Dropdown): boolean =>
        node !== null && (d.element.contains(node) || d.openers.some((o) => o.contains(node)));
      // A click inside one copy keeps every copy of that dropdown.
      const kept = open.filter((d) => open.some((c) => hit(c) && same(c, d)));
      const closing = open.filter((d) => !kept.includes(d));
      open = kept;
      for (const dropdown of closing) close(dropdown, "a click outside it");
    },
  };
}
