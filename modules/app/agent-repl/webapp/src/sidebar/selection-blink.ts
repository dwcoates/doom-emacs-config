/**
 * selection-blink — a row the user selects from the rail blinks, softly and
 * slowly, once per click (owner request, 2026-10-17).
 *
 * EVERY CLICK BLINKS, even on the row already selected: the blink answers the
 * click, not the roster, so it is started where the rail's one select verb
 * runs (`row.ts` `selectWorkspace`) and never from `WorkspaceRoster.current`,
 * which a re-selection does not change.
 *
 * THE BLINK OUTLIVES THE ROW IT STARTED ON. A selection is answered by a
 * roster push, and the push redraws the rows whose wire changed (the
 * highlight moves), so the clicked element is usually replaced mid-blink; a
 * band return even moves the row to another section. The blink is therefore
 * the rail's, keyed by workspace id and stamped with its start instant, and
 * every drawn copy of the row joins it at the phase the clock says
 * (`apply`): a negative delay (`--selection-blink-delay`) seeks the fresh
 * element's animation to the elapsed time, so a redraw continues the blink
 * rather than restarting it. Once the blink has run its length the entry is dropped.
 *
 * THE LENGTH LIVES HERE, ONCE: each blinking row is handed it as
 * `--selection-blink-duration`, which the stylesheet's animation reads.
 */
import { queryAll } from "../dom.js";
import { log } from "../log.js";

/** How long one selection blink runs, in milliseconds. */
export const SELECTION_BLINK_MS = 1800;

/** The class a blinking row's box carries. */
export const SELECTION_BLINK_CLASS = "selection-blink";

/** The custom property a blinking row is handed the blink's length in. */
export const SELECTION_BLINK_DURATION_PROPERTY = "--selection-blink-duration";

/** The custom property a blinking row is handed its animation's delay in: minus the time already blinked. */
export const SELECTION_BLINK_DELAY_PROPERTY = "--selection-blink-delay";

/** The rail's selection blinks. */
export interface SelectionBlink {
  /** Workspace ID was selected from the rail: blink every drawn copy of its row from now. */
  start(id: string): void;
  /** ROW was just drawn for workspace ID: join a blink still running for it. */
  apply(id: string, row: HTMLElement): void;
}

/**
 * The selection blinks of the rail drawn under ROOT, on the wall clock NOW.
 */
export function createSelectionBlink(root: ParentNode, now: () => number = () => Date.now()): SelectionBlink {
  /** Each blinking workspace's start instant, in epoch milliseconds. */
  const started = new Map<string, number>();

  /** How far into its blink workspace ID is, or null when it is not blinking. */
  const elapsedOf = (id: string): number | null => {
    const start = started.get(id);
    if (start === undefined) return null;
    const elapsed = now() - start;
    if (elapsed < SELECTION_BLINK_MS) return elapsed;
    started.delete(id);
    return null;
  };

  /** Seek ROW's blink to ELAPSED, restarting its animation. */
  const paint = (row: HTMLElement, elapsed: number): void => {
    row.style.setProperty(SELECTION_BLINK_DURATION_PROPERTY, `${SELECTION_BLINK_MS}ms`);
    row.style.setProperty(SELECTION_BLINK_DELAY_PROPERTY, `${-elapsed}ms`);
    // A row already blinking only restarts its animation if the style is
    // flushed between the class leaving and coming back. A freshly drawn row
    // carries no class, so a draw never pays for the flush.
    if (row.classList.contains(SELECTION_BLINK_CLASS)) {
      row.classList.remove(SELECTION_BLINK_CLASS);
      void row.offsetWidth;
    }
    row.classList.add(SELECTION_BLINK_CLASS);
  };

  return {
    start(id: string): void {
      started.set(id, now());
      const rows = queryAll<HTMLElement>(root, "[data-roster-row]").filter(
        (row) => row.getAttribute("data-roster-row") === id,
      );
      log.debug("blinking the row selected from the rail", {
        operation: "sidebar.selection-blink",
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
