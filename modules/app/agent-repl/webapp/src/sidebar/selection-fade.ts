/**
 * selection-fade — the tab number of a row the user selects from the rail
 * fades quickly into white and, a second later, back to its own grey, once
 * per click (owner request, 2026-10-17).
 *
 * THE ROW ITSELF STAYS STILL (owner request, 2026-10-18): the wash the row
 * once swelled in on a click is gone, and only its number answers the click.
 *
 * EVERY CLICK FADES, even on the row already selected: the fade answers the
 * click, not the roster, so it is started where the rail's one select verb
 * runs (`row.ts` `selectWorkspace`) and never from `WorkspaceRoster.current`,
 * which a re-selection does not change.
 *
 * THE FADE OUTLIVES THE ROW IT STARTED ON. A selection is answered by a
 * roster push, and the push redraws the rows whose wire changed (the
 * highlight moves), so the clicked element is usually replaced mid-fade; a
 * band return even moves the row to another section. The fade is therefore
 * the rail's, keyed by workspace id and stamped with its start instant, and
 * every drawn copy of the row joins it at the phase the clock says
 * (`apply`): a negative delay (`--selection-number-fade-delay`) seeks the
 * fresh element's animation to the elapsed time, so a redraw continues the
 * fade rather than restarting it. Once every animation has run its length
 * the click is forgotten.
 *
 * EACH LENGTH LIVES HERE, ONCE: a row is handed each animation's length in
 * its duration property, which the stylesheet's animation reads.
 */
import { queryAll } from "../dom.js";
import { log } from "../log.js";

/** How long the tab number's fade runs, in milliseconds: a quick rise to white, a second held, then the fall back. */
export const SELECTION_NUMBER_FADE_MS = 1600;

/** The class a row whose tab number is fading carries. */
export const SELECTION_NUMBER_FADE_CLASS = "selection-number-fade";

/** The custom property a row is handed the number fade's length in. */
export const SELECTION_NUMBER_FADE_DURATION_PROPERTY = "--selection-number-fade-duration";

/** The custom property a row is handed the number fade's delay in: minus the time already faded. */
export const SELECTION_NUMBER_FADE_DELAY_PROPERTY = "--selection-number-fade-delay";

/** One animation a selection runs on the clicked row, seeked by the rail's clock. */
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

/** Every animation one selection runs: the number's fade. */
export const SELECTION_ANIMATIONS: readonly SelectionAnimation[] = [
  {
    className: SELECTION_NUMBER_FADE_CLASS,
    durationProperty: SELECTION_NUMBER_FADE_DURATION_PROPERTY,
    delayProperty: SELECTION_NUMBER_FADE_DELAY_PROPERTY,
    ms: SELECTION_NUMBER_FADE_MS,
  },
];

/** How long a selection is remembered: until its longest animation has run. */
const SELECTION_REMEMBERED_MS = Math.max(...SELECTION_ANIMATIONS.map((animation) => animation.ms));

/** The rail's selection fades. */
export interface SelectionFade {
  /** Workspace ID was selected from the rail: fade every drawn copy of its row from now. */
  start(id: string): void;
  /** ROW was just drawn for workspace ID: join a fade still running for it. */
  apply(id: string, row: HTMLElement): void;
}

/**
 * The selection fades of the rail drawn under ROOT, on the wall clock NOW.
 */
export function createSelectionFade(root: ParentNode, now: () => number = () => Date.now()): SelectionFade {
  /** Each fading workspace's start instant, in epoch milliseconds. */
  const started = new Map<string, number>();

  /** How far into its fade workspace ID is, or null when it is not fading. */
  const elapsedOf = (id: string): number | null => {
    const start = started.get(id);
    if (start === undefined) return null;
    const elapsed = now() - start;
    if (elapsed < SELECTION_REMEMBERED_MS) return elapsed;
    started.delete(id);
    return null;
  };

  /** Seek each of ROW's animations still running at ELAPSED to it, restarting them. */
  const paint = (row: HTMLElement, elapsed: number): void => {
    const running = SELECTION_ANIMATIONS.filter((animation) => elapsed < animation.ms);
    for (const animation of running) {
      row.style.setProperty(animation.durationProperty, `${animation.ms}ms`);
      row.style.setProperty(animation.delayProperty, `${-elapsed}ms`);
    }
    // A row already animating only restarts its animation if the style is
    // flushed between the class leaving and coming back. A freshly drawn row
    // carries no class, so a draw never pays for the flush.
    if (running.some((animation) => row.classList.contains(animation.className))) {
      for (const animation of running) row.classList.remove(animation.className);
      void row.offsetWidth;
    }
    for (const animation of running) row.classList.add(animation.className);
  };

  return {
    start(id: string): void {
      started.set(id, now());
      const rows = queryAll<HTMLElement>(root, "[data-roster-row]").filter(
        (row) => row.getAttribute("data-roster-row") === id,
      );
      log.debug("fading the number of the row selected from the rail", {
        operation: "sidebar.selection-fade",
        context: { workspace: id, drawn: rows.length },
      });
      for (const row of rows) paint(row, 0);
    },
    apply(id: string, row: HTMLElement): void {
      const elapsed = elapsedOf(id);
      if (elapsed !== null) paint(row, elapsed);
    },
  };
}
