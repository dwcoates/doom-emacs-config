/**
 * selection-fade — the tab number of the rail's highlighted row is white.
 *
 * THE NUMBER RISES AND STAYS (owner request, 2026-10-18): when the highlight
 * moves to a row, its tab number fades from its own grey into white over a
 * second and stays white for as long as the row stays highlighted. The row
 * the highlight left turns back to its grey AT ONCE, with no fade (owner
 * request, 2026-10-18), a rise still running on it included. The row itself
 * stays still: the wash it once swelled in on a click is gone.
 *
 * THE FADE FOLLOWS THE HIGHLIGHT, NOT THE CLICK: a selection made anywhere
 * (a rail click, a tab chord in Emacs, the picker) moves the highlight, and a
 * click on the row already highlighted moves nothing, so nothing fades. The
 * highlight moves in two places, and both report here: a row drawn
 * highlighted (`apply`), and the webhost showing a page ahead of the roster
 * (`sidebar.ts` `markShown`, through `highlight`).
 *
 * THE FIRST HIGHLIGHT IS NOT A MOVE: the rail's first draw finds a row
 * already highlighted, and its number is drawn white at once.
 *
 * A FADE OUTLIVES THE ROW IT STARTED ON. A switch is answered by a roster
 * push, and the push redraws the rows whose wire changed (the highlight
 * moved), so the element is usually replaced mid-fade; a band return even
 * moves the row to another section. Each fade is therefore the rail's, keyed
 * by workspace id and stamped with its start instant, and every drawn copy of
 * the row joins it at the phase the clock says: a negative delay seeks the
 * fresh element's animation to the elapsed time, so a redraw continues the
 * fade rather than restarting it.
 *
 * EACH LENGTH LIVES HERE, ONCE: a row is handed its animation's length in
 * the animation's duration property, which the stylesheet's animation reads.
 */
import { queryAll } from "../dom.js";
import { log } from "../log.js";

/** How long the highlighted row's number takes to rise into white, in milliseconds. */
export const SELECTION_NUMBER_RISE_MS = 1000;

/** One animation the number runs when the highlight moves, seeked by the rail's clock. */
export interface SelectionAnimation {
  /** The class the row carries while the animation runs. */
  className: string;
  /** The custom property the row is handed the animation's length in. */
  durationProperty: string;
  /** The custom property the row is handed minus the time already run in. */
  delayProperty: string;
  /** How long the animation runs, in milliseconds. */
  ms: number;
}

/** The number's rise into white, on the row the highlight moved to. */
export const SELECTION_NUMBER_RISE: SelectionAnimation = {
  className: "selection-number-rise",
  durationProperty: "--selection-number-rise-duration",
  delayProperty: "--selection-number-rise-delay",
  ms: SELECTION_NUMBER_RISE_MS,
};

/** Every animation a move of the highlight runs: the rise alone, since the row it left turns grey at once. */
export const SELECTION_ANIMATIONS: readonly SelectionAnimation[] = [SELECTION_NUMBER_RISE];

/** The rail's number fades. */
export interface SelectionFade {
  /** The rail highlights workspace ID now: rise its number, and turn the number of the row highlighted before grey at once. */
  highlight(id: string): void;
  /** ROW was just drawn for workspace ID, HIGHLIGHTED or not: follow the highlight, and join a fade still running for it. */
  apply(id: string, row: HTMLElement, highlighted: boolean): void;
}

/** One running fade: which animation, and its start instant in epoch milliseconds. */
interface Fade {
  animation: SelectionAnimation;
  start: number;
}

/**
 * The number fades of the rail drawn under ROOT, on the wall clock NOW.
 */
export function createSelectionFade(root: ParentNode, now: () => number = () => Date.now()): SelectionFade {
  /** The highlighted workspace: undefined before the first highlight, null once the highlight left every row. */
  let current: string | null | undefined;
  /** Each workspace's running fade. */
  const fades = new Map<string, Fade>();

  /** Workspace ID's running fade and how far into it the clock is, or null when none runs. */
  const runningOf = (id: string): { fade: Fade; elapsed: number } | null => {
    const fade = fades.get(id);
    if (fade === undefined) return null;
    const elapsed = now() - fade.start;
    if (elapsed < fade.animation.ms) return { fade, elapsed };
    fades.delete(id);
    return null;
  };

  /** Draw ROW in workspace ID's running fade, if any, seeked to the clock and restarted. */
  const paint = (id: string, row: HTMLElement): void => {
    const running = runningOf(id);
    // A row already animating only restarts its animation if the style is
    // flushed between the class leaving and coming back. A freshly drawn row
    // carries no class, so a draw never pays for the flush.
    if (SELECTION_ANIMATIONS.some((animation) => row.classList.contains(animation.className))) {
      for (const animation of SELECTION_ANIMATIONS) row.classList.remove(animation.className);
      void row.offsetWidth;
    }
    if (running === null) return;
    const { animation } = running.fade;
    row.style.setProperty(animation.durationProperty, `${animation.ms}ms`);
    row.style.setProperty(animation.delayProperty, `${-running.elapsed}ms`);
    row.classList.add(animation.className);
  };

  /** Paint every drawn copy of workspace ID's row. */
  const paintDrawn = (id: string): void => {
    for (const row of queryAll<HTMLElement>(root, "[data-roster-row]")) {
      if (row.getAttribute("data-roster-row") === id) paint(id, row);
    }
  };

  /** Move the highlight from wherever it is to NEXT (null: no row): the left row's number turns grey at once, NEXT's rises. */
  const move = (next: string | null): void => {
    const previous = current;
    current = next;
    // The first highlight is the rail's first draw, not a move.
    if (previous === undefined || previous === next) return;
    const start = now();
    log.debug("the rail's highlight moved; rising the new row's number", {
      operation: "sidebar.selection-fade",
      context: { workspace: next, previous },
    });
    if (previous !== null) {
      // No fall: the left row drops any rise still running on it, and its
      // number is the plain grey at once.
      fades.delete(previous);
      paintDrawn(previous);
    }
    if (next !== null) {
      fades.set(next, { animation: SELECTION_NUMBER_RISE, start });
      paintDrawn(next);
    }
  };

  return {
    highlight(id: string): void {
      move(id);
    },
    apply(id: string, row: HTMLElement, highlighted: boolean): void {
      if (highlighted) move(id);
      else if (id === current) move(null);
      paint(id, row);
    },
  };
}
