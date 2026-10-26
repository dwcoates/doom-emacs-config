/**
 * transients — the ONE place the rail lets go of everything the pointer and
 * the clicks raised in it, the moment the user is no longer there (owner
 * request, 2026-10-21): the hover highlight of a row or header, a row's detail
 * panel, a "⋯" menu, the subsection name popup.
 *
 * THE USER LEAVES THE RAIL when
 *   - the page loses the focus: a click into a workspace page's feed (an
 *     iframe, so the focus leaves this document), into another Emacs window,
 *     or Emacs leaving the screen's front;
 *   - the page is hidden;
 *   - the selection moves: whichever workspace the rail highlights changed,
 *     by a click, by Emacs (`M-{`), or by the daemon, even with the pointer
 *     still resting on another row.
 * Each of those `dismiss`es: every dropdown closes (`dropdowns.ts`), every
 * detail panel closes and none reopens (`row-hover.ts`), and the rail stops
 * drawing hover highlights (`HOVER_SUSPENDED_CLASS`).
 *
 * WHY THE HOVER HIGHLIGHT IS SUSPENDED, NOT LEFT TO `:hover`. WebKit keeps an
 * element `:hover` until it sees the pointer leave, and it does not see a
 * focus change, or a pointer it stopped tracking when the focus went to
 * Emacs, so a row stayed highlighted long after the user had gone. The page
 * cannot clear `:hover`; it can stop drawing it. The suspension lifts only
 * when the user is back: the pointer MOVES in the rail (a move WebKit
 * synthesizes after a redraw under a resting pointer stays where it was, so it
 * lifts nothing), or a key is pressed in it.
 *
 * The pointer leaving the rail suspends the highlight too, since a highlight
 * there would be the same stuck `:hover`, but closes nothing: a "⋯" menu stays
 * open while the pointer travels.
 */
import { log } from "../log.js";
import { createPointerRest } from "../pointer-rest.js";
import type { Dropdowns } from "./dropdowns.js";
import type { RowHover } from "./row-hover.js";

/** The class the rail wears while it draws no hover highlight. */
export const HOVER_SUSPENDED_CLASS = "sb-hover-suspended";

/** What the transients close, and where they listen. */
export interface TransientsDeps {
  dropdowns: Dropdowns;
  rowHover: RowHover;
  /** The window whose focus the rail's document holds. */
  win: Window;
}

export interface Transients {
  /** The rail highlights workspace ID now; a change from another dismisses. */
  selected(id: string | null): void;
  /** Stop listening. */
  dispose(): void;
}

/** Install the transients' dismissal on the rail HOST. */
export function installTransients(host: HTMLElement, deps: TransientsDeps): Transients {
  const { dropdowns, rowHover, win } = deps;
  const doc = win.document;
  let selection: string | null = null;
  /** Whether the hover highlight is suspended, and until when (pointer-rest.ts). */
  const rest = createPointerRest();

  const suspend = (): void => {
    if (!rest.suspend()) return;
    host.classList.add(HOVER_SUSPENDED_CLASS);
  };

  /** The suspension ended, VIA the pointer or a key. */
  const resumed = (via: string): void => {
    host.classList.remove(HOVER_SUSPENDED_CLASS);
    log.debug("the user is back in the rail; hover answers again", {
      operation: "sidebar.transients.resume",
      context: { via },
    });
    rowHover.resume();
  };

  const dismiss = (why: string): void => {
    log.debug("the user left the rail; closing everything the pointer raised in it", {
      operation: "sidebar.transients.dismiss",
      context: { why },
    });
    dropdowns.dismissAll(why);
    rowHover.dismiss(why);
    suspend();
  };

  const moved = (event: MouseEvent): void => {
    const back = rest.moved(event.clientX, event.clientY);
    rowHover.pointerAt(event.target);
    if (back) resumed("pointer");
  };
  const left = (): void => {
    rowHover.pointerAt(null);
    suspend();
  };
  const keyed = (): void => {
    if (rest.resume()) resumed("key");
  };
  const blurred = (): void => {
    dismiss("the page lost focus");
  };
  const visibility = (): void => {
    if (doc.visibilityState === "hidden") dismiss("the page was hidden");
  };

  host.addEventListener("mousemove", moved);
  host.addEventListener("mouseleave", left);
  host.addEventListener("keydown", keyed);
  win.addEventListener("blur", blurred);
  doc.addEventListener("visibilitychange", visibility);

  return {
    selected(id: string | null): void {
      const before = selection;
      selection = id;
      if (before === null || id === before) return;
      dismiss("the selection moved");
    },
    dispose(): void {
      host.removeEventListener("mousemove", moved);
      host.removeEventListener("mouseleave", left);
      host.removeEventListener("keydown", keyed);
      win.removeEventListener("blur", blurred);
      doc.removeEventListener("visibilitychange", visibility);
    },
  };
}
