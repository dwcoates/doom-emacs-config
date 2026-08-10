/**
 * fold — the click-to-open fold skeleton, and the label cap that goes with it.
 *
 * Extracted from `render.ts` when the async-bubble surface arrived: a detached
 * agent's bubble, a workflow's journal and a shell's spool all fold exactly the
 * way a tool card's activity section and a final response's watcher panel do,
 * and a second copy of that markup would be a second set of class names and
 * caret glyphs to keep in step by review.
 *
 * It lives BELOW both renderers rather than beside either, so `async-render.ts`
 * can use it without importing `render.ts` — which would be a cycle, since
 * `render.ts` is what draws the async bubbles.
 */
import { escapeHtml } from "./highlight.js";

/** Cap a label, marking the truncation rather than silently shortening. */
export function capLabel(label: string, max: number): string {
  return label.length > max ? `${label.slice(0, max - 1)}…` : label;
}

/**
 * The class a fold wears when it DOES NOT FOLD: the panel is always in the
 * HTML, and the stylesheet caps its height so the reader scrolls the body in
 * place instead of opening and closing it. The teal async cards wear it (see
 * `ASYNC_TEAL_TOOLS` in `render.ts`), and `styles.test.ts` holds the class
 * here to the cap rule there.
 */
export const FIXED_FOLD_CLASS = "fold-fixed";

/**
 * The shared skeleton of a click-to-open fold: a pill ticker as the
 * collapsed face and a panel body that exists in the HTML only while open,
 * with open state carried on the wrapper's class and a `data-panel-toggle`
 * the FeedRenderer's delegated handler flips. The activity fold on a
 * spawning card (ActivitySection) and the watcher fold on a final-response
 * bubble (WatcherPanel) both render through this, differing only in their
 * classes, ticker face, and body.
 *
 * BODY is a thunk, not a string: it is called only when the fold is open,
 * so a hundred buffered children (or watcher tails) cost nothing to render
 * while the fold stays closed.
 *
 * FIXED turns the fold into a fixed, always-laid-out panel: no
 * `data-panel-toggle` (so no click of the reader's can collapse it), no
 * caret (there is no state for one to announce), and the body rendered
 * unconditionally. `open` is not consulted at all in that mode — a fixed
 * panel has no open state to carry — and the ticker stays as the panel's
 * heading, since the step count and last line it names are as useful above
 * an open panel as they were in place of a closed one.
 */
export function Fold(opts: {
  id: string;
  foldClass: string;
  tickerClass: string;
  ticker: string;
  body: () => string;
  open: boolean;
  fixed?: boolean;
}): string {
  if (opts.fixed === true) {
    return `<div class="${opts.foldClass} ${FIXED_FOLD_CLASS}">
      <div class="${opts.tickerClass}">${opts.ticker}</div><div class="agent-panel">${opts.body()}</div>
    </div>`;
  }
  const panel = opts.open ? `<div class="agent-panel">${opts.body()}</div>` : "";
  return `<div class="${opts.foldClass}${opts.open ? " open" : ""}" data-panel-toggle="${escapeHtml(opts.id)}">
      <div class="${opts.tickerClass}">${opts.ticker} <span class="agent-caret" aria-hidden="true">${
        opts.open ? "▴" : "▾"
      }</span></div>${panel}
    </div>`;
}
