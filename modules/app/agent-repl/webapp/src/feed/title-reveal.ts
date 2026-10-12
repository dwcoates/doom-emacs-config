/**
 * title-reveal — a tool card's TITLE types out as it arrives, the way a
 * response's prose does (owner request, 2026-10-12).
 *
 * The title is the one line a card heads with: a call's input line ("$ go test
 * ./..."), a shell bubble's command, a skill's invocation, a hook's headline, a
 * subagent's description. Every one is folded through `foldTitle`
 * (title-fold.ts), and that is where this is armed, so no title site can miss
 * it.
 *
 * ONLY A PUSHED ROW TYPES (`RowContext.arrival`). A tool call's input arrives
 * whole, so the title has nothing streaming to follow: it types out when the
 * card first appears on a reader's live tail. A row drawn from a page (the
 * opening page, a re-open, history the walk loaded) is drawn whole.
 *
 * THE TYPE-OUT RESUMES ACROSS REDRAWS. A card is redrawn on every push of its
 * row (running, then returned), and a bubble's head on every push of its
 * bubble, so the shown length rides the title element
 * (`TITLE_REVEALED_ATTRIBUTE`) while the title is part shown, and the next
 * draw resumes from the previous draw's title. A title shown whole carries no
 * attribute, so its redraws are whole too.
 *
 * The loop is the one type-out (src/type-out.ts): it stops typing once the
 * collapsed one-line title is full (the rest of the line is under the ellipsis
 * anyway), and does not type at all on a page no reader can see.
 */
import { applyTextReveal, clearTextReveal, fullTextOf } from "../bubble/text-reveal.js";
import type { RevealPace } from "../reveal-pace.js";
import { typeOut } from "../type-out.js";
import { shouldShowMore, TITLE_FOLD_CLASS } from "./bubble-more.js";
import type { RowContext } from "./cards/context.js";
import { carriedNumber } from "./cards/response.js";

/** The attribute a part-shown title carries its shown length on. */
export const TITLE_REVEALED_ATTRIBUTE = "data-title-revealed";

/**
 * The pace a title types at: the daemon's built-in prose pace
 * (daemon/internal/revealpace `defaults`), since a title is read like prose
 * and has no arrival speed of its own to learn.
 */
export const TITLE_REVEAL_PACE: RevealPace = { minCps: 49, maxCps: 90, bankLimitSeconds: 1.5 };

/** Type TITLE out when RC's row is new to a reader; otherwise leave it whole. */
export function revealTitle(title: HTMLElement, rc: RowContext): void {
  const resumed = resumedFrom(rc);
  if (resumed === undefined) return;
  show(title, resumed);
  typeOut({
    ctx: rc.ctx,
    pace: TITLE_REVEAL_PACE,
    resumed,
    operation: "feed.title-reveal",
    logContext: { row: rc.row.id?.value ?? "unset" },
    element: title,
    // A title is drawn fresh on every redraw, so no later draw re-owns this
    // one in place: a redraw replaces it, which ends the loop.
    reowned: () => false,
    total: () => fullTextOf(title).length,
    whenRendered: (fn) => fn(),
    reveal: (shown) => show(title, shown),
    showWhole: () => {
      clearTextReveal(title);
      title.removeAttribute(TITLE_REVEALED_ATTRIBUTE);
    },
    full: () => shouldShowMore(title),
  });
}

/** Show TITLE's first SHOWN characters, and carry the count for a redraw. */
function show(title: HTMLElement, shown: number): void {
  applyTextReveal(title, shown);
  title.setAttribute(TITLE_REVEALED_ATTRIBUTE, String(shown));
}

/**
 * Where the title's type-out starts: from nothing on a pushed row's first
 * draw, from what the previous draw's title showed while it was part shown,
 * and undefined (drawn whole) otherwise.
 */
function resumedFrom(rc: RowContext): number | undefined {
  if (rc.previous === undefined) return rc.arrival === "push" ? 0 : undefined;
  const previous = rc.previous.classList.contains(TITLE_FOLD_CLASS)
    ? rc.previous
    : rc.previous.querySelector<HTMLElement>(`.${TITLE_FOLD_CLASS}`);
  if (previous === null) return undefined;
  const carried = carriedNumber(previous, TITLE_REVEALED_ATTRIBUTE);
  return carried === undefined ? undefined : Math.floor(carried);
}
