/**
 * fold-fade — a row a fold drew stays drawn as it fades away.
 *
 * A COLLAPSED FOLD DRAWS A ROW IT COVERS only while the daemon marks it
 * surfaced (frontend.v1.RosterOpenRow.surfaced): the workspace the user is on,
 * or one whose turn started under the fold and still runs. The moment the
 * mark goes (the user switched away, the turn ended), the row stays drawn and
 * fades out over FOLD_FADE_MS, then the fold hides it (owner request,
 * 2026-11-17), so a row never blinks out from under the eye.
 *
 * THE FADE IS THE RAIL'S, keyed by workspace id and stamped with its start
 * instant: the push that takes the mark off redraws the row, and every drawn
 * copy joins the fade at the phase the clock says (a negative delay seeks
 * the fresh element's animation), as `selection-fade.ts` does.
 *
 * A fading row wears the surfaced class too, so the fold's rules draw it and
 * the rows standing above it exactly as they draw a surfaced row. Every one
 * of those rules, and the fade's own, is scoped to a fold, so a row whose
 * mark went because the fold opened is drawn as any open row is.
 */
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { SURFACED_ROW_CLASS } from "./row.js";

/** How long a row a fold stops drawing takes to fade away, in milliseconds. */
export const FOLD_FADE_MS = 5000;

/** The class a row carries while it fades out of its fold. */
export const FOLD_FADING_CLASS = "fold-fading";

/** The custom property a fading row is handed the fade's length in. */
export const FOLD_FADE_DURATION_PROPERTY = "--fold-fade-duration";

/** The custom property a fading row is handed minus the time already faded. */
export const FOLD_FADE_DELAY_PROPERTY = "--fold-fade-delay";

/** The rail's fold fades. */
export interface FoldFade {
  /** ROW was just drawn for workspace ID, SURFACED or not: start, join or end its fade. */
  apply(id: string, row: HTMLElement, surfaced: boolean): void;
}

/**
 * The fold fades of the rail drawn under ROOT, on the wall clock NOW, each
 * fade's end run through AFTER (a timer: run FN after MS).
 */
export function createFoldFade(
  root: ParentNode,
  now: () => number = () => Date.now(),
  after: (fn: () => void, ms: number) => void = (fn, ms) => {
    setTimeout(fn, ms);
  },
): FoldFade {
  /** Whether each workspace's row was last drawn surfaced. */
  const surfacedBefore = new Map<string, boolean>();
  /** Each fading workspace's start instant, epoch milliseconds. */
  const fades = new Map<string, number>();

  /** Draw ROW faded as far as workspace ID's fade has run, or plain when none runs. */
  const paint = (id: string, row: HTMLElement): void => {
    const start = fades.get(id);
    if (start === undefined) {
      if (row.classList.contains(FOLD_FADING_CLASS)) {
        row.classList.remove(FOLD_FADING_CLASS, SURFACED_ROW_CLASS);
      }
      return;
    }
    row.style.setProperty(FOLD_FADE_DURATION_PROPERTY, `${FOLD_FADE_MS}ms`);
    row.style.setProperty(FOLD_FADE_DELAY_PROPERTY, `${-(now() - start)}ms`);
    row.classList.add(SURFACED_ROW_CLASS, FOLD_FADING_CLASS);
  };

  /** End workspace ID's fade begun at START, unless a later fade or a mark replaced it. */
  const end = (id: string, start: number): void => {
    if (fades.get(id) !== start) return;
    fades.delete(id);
    for (const row of queryAll<HTMLElement>(root, "[data-roster-row]")) {
      if (row.getAttribute("data-roster-row") === id) paint(id, row);
    }
  };

  return {
    apply(id: string, row: HTMLElement, surfaced: boolean): void {
      const was = surfacedBefore.get(id) === true;
      surfacedBefore.set(id, surfaced);
      if (surfaced) {
        fades.delete(id);
      } else if (was) {
        const start = now();
        fades.set(id, start);
        log.debug("a fold stopped drawing a row; fading it out", {
          operation: "sidebar.fold-fade",
          context: { workspace: id, ms: FOLD_FADE_MS },
        });
        after(() => end(id, start), FOLD_FADE_MS);
      }
      paint(id, row);
    },
  };
}
