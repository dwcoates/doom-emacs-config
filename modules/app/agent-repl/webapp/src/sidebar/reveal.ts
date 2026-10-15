/**
 * reveal — a rail row drawn only once it comes into view.
 *
 * WHY IT EXISTS (owner ruling, 2026-10-15). The Recently Merged band holds the
 * hundred newest merges, and its viewport shows a handful of them; the rest
 * are reached by scrolling the band. A drawn row is some forty elements (its
 * line, its menu, its detail panel, its hover wiring), so drawing all hundred
 * cost thousands of elements nobody could see. A row past the first few is
 * drawn as a stand-in (`row.ts`, `drawRosterRowStandIn`), exactly as tall as
 * the row and carrying its name, so the band fits and the search narrows the
 * same lines; it is drawn for real the first time it nears the viewport.
 *
 * ONE OBSERVER PER RAIL, rooted on the page's viewport, which every ancestor
 * scroller (the band's own, the rail's) clips. A row is drawn on the batch
 * that first reports it intersecting, then forgotten; `onRevealed` runs once
 * per batch that drew any, for what the rail does over its whole drawing after
 * a draw (the search, the open detail panels' placement).
 */
import { pageIntersectionObserver } from "../dom.js";
import { log } from "../log.js";

/** How far outside the viewport a stand-in is already drawn, so a scroll meets a drawn row. */
export const REVEAL_MARGIN = "240px 0px";

/** What a section reaches of the rail's reveal. */
export interface RowReveal {
  /**
   * Run DRAW the first time TARGET is in view. Answers the teardown that
   * stops waiting, which the section's scope runs when its drawing goes.
   */
  whenShown(target: Element, draw: () => void): () => void;
}

/** The rail's reveal, and its own teardown. */
export interface RailReveal extends RowReveal {
  /** Stop observing every row still waiting. */
  dispose(): void;
}

/** The rail's reveal over Observer, running ON_REVEALED after each batch that drew a row. */
export function createRowReveal(
  onRevealed: () => void,
  Observer: typeof IntersectionObserver | null = pageIntersectionObserver(),
): RailReveal {
  if (Observer === null) {
    // Every page the rail is drawn in is a WebKit page, which has one.
    log.error("the rail cannot defer its rows: the page has no IntersectionObserver", {
      operation: "sidebar.reveal.unsupported",
      context: {},
    });
    throw new Error("sidebar reveal: the page has no IntersectionObserver");
  }
  const waiting = new Map<Element, () => void>();
  const observer = new Observer(
    (entries) => {
      let drew = 0;
      for (const entry of entries) {
        if (!entry.isIntersecting) continue;
        const draw = waiting.get(entry.target);
        if (draw === undefined) continue;
        waiting.delete(entry.target);
        observer.unobserve(entry.target);
        draw();
        drew += 1;
      }
      if (drew === 0) return;
      log.debug("drew the rail rows that came into view", {
        operation: "sidebar.reveal",
        context: { drew, waiting: waiting.size },
      });
      onRevealed();
    },
    { root: null, rootMargin: REVEAL_MARGIN },
  );
  return {
    whenShown(target: Element, draw: () => void): () => void {
      waiting.set(target, draw);
      observer.observe(target);
      return () => {
        if (!waiting.delete(target)) return;
        observer.unobserve(target);
      };
    },
    dispose(): void {
      waiting.clear();
      observer.disconnect();
    },
  };
}
