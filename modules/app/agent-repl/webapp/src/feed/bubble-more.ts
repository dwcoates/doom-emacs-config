/**
 * bubble-more — THE ONE has-more measurer: the "more below" affordance on a
 * COLLAPSED bubble (any kind) whose text overruns its height cap.
 *
 * FIX2 (owner ruling, 2026-09-15): the owner asked for "a signal that there's
 * more to reveal" on the response and prompt bubbles. When a bubble is collapsed
 * and its body's rendered lines actually run past the cap
 * (`hidesContentBeyondCap`), the `.bubble-scroll` box wears `has-more`; the
 * stylesheet draws a bottom fade into the bubble's own background from that class — the fade
 * ONLY, never a chevron (owner ruling, 2026-09-23). Every bubble's scroll box
 * is served (owner ruling, 2026-09-23: one measurer for every blue and purple
 * bubble) — never a tool-call section or any other capped box — which is what
 * MORE_BUBBLE_SELECTOR encodes.
 *
 * TITLE FOLDS (owner ruling, 2026-09-23): a tool card's TITLE — the command a
 * shell bubble runs, a tool call's input line, and the other title lines
 * title-fold.ts marks — is capped at two lines while its fold is collapsed and
 * wears the SAME fade when it overflows them. It is the one other
 * kind in MORE_KINDS; an output section still never wears the affordance.
 *
 * Nothing here draws the fade: it is pure CSS keyed on
 * `has-more` (styles.css). This module only MEASURES the overflow and toggles
 * the class, at the three moments it can change: on draw and on width re-wrap
 * (the `ResizeObserver` in `installHasMore`, which also catches a viewport
 * change moving the 50vh cap), and on expand/collapse (a synchronous refresh
 * driven from the expand toggle, since a short-window expand leaves the box the
 * same height and so fires no resize).
 */
import { BUBBLE_BODY_CLASS } from "../bubble/body.js";
import { EXPANDED_CLASS } from "../expand.js";
import { log } from "../log.js";
import { onDiscard } from "./ticking.js";

/** The class the stylesheet turns into the bottom fade. */
export const HAS_MORE_CLASS = "has-more";

/**
 * The bubble scroll boxes the "more below" affordance serves: EVERY bubble's
 * (src/bubble/draw.ts builds them all, and only it builds a `.bubble`), so a
 * response, a prompt, a peer message, a held prompt, a compaction summary and
 * an agentic card all fade the same way. A tool-call section is not a
 * `.bubble` and can never match it.
 *
 * The class name here is the LITERAL of `BUBBLE_SCROLL_CLASS` (bubble-scroll.ts)
 * rather than an import of it: bubble-scroll.ts imports `installHasMore` from
 * this module, and importing the constant back would form a module cycle that
 * reads the const in its temporal dead zone. bubble-more.test.ts holds this
 * literal to the exported constant so the two cannot drift.
 */
export const MORE_BUBBLE_SELECTOR = ".bubble > .bubble-scroll";

/**
 * The class every TITLE fold wears (title-fold.ts): a tool card's title line —
 * the command a shell runs, a tool call's input line, a skill's invocation, a
 * hook's headline, a subagent's description — capped at two lines while its
 * fold is collapsed. Declared HERE rather than in title-fold.ts because the
 * measurer must know every kind it serves, and title-fold.ts imports the
 * measurer; the reverse import would be a module cycle.
 */
export const TITLE_FOLD_CLASS = "title-fold";

/**
 * A title fold that is its OWN fold, for a card with no card-level fold to
 * defer to (a hook card, a skill card that is not loaded). It is a
 * CAPPED_CLASSES entry in expand.ts, so the feed-wide click toggles `.expanded`
 * on the title itself; title-fold.test.ts holds the two literals together.
 */
export const TITLE_FOLD_STANDALONE_CLASS = "title-fold-standalone";

/**
 * When a title fold's FOLD is open — whichever fold owns it: the title itself
 * (a standalone title), the tool-call/skill card it sits in (`.tool-fold`,
 * toggled by expand.ts), or the bubble whose head it heads (`.bubble-fold`,
 * toggled by bubble.ts). The stylesheet lifts the two-line cap on exactly this
 * selector list, and styles.test.ts holds the two to each other.
 *
 * Child combinators on the bubble arm keep an OPEN bubble from lifting the cap
 * on the titles of the cards inside its sub-feed; a `.tool-fold` card holds no
 * other card, so its descendant combinator reaches only its own titles.
 */
export const TITLE_FOLD_OPEN_SELECTOR = [
  `.${TITLE_FOLD_CLASS}.${EXPANDED_CLASS}`,
  `.tool-fold.${EXPANDED_CLASS} .${TITLE_FOLD_CLASS}`,
  `.bubble-fold[data-expanded="true"] > .bubble-head .${TITLE_FOLD_CLASS}`,
].join(", ");

/** True when a title's text is taller than its two-line clamp can show. */
export function overflowsCap(box: { scrollHeight: number; clientHeight: number }): boolean {
  return box.scrollHeight > box.clientHeight;
}

/** The class of the one bubble body (src/bubble/body.ts), as its box holds it. */
const BODY_SELECTOR = `:scope > .${BUBBLE_BODY_CLASS}`;

/** The operation a has-more measurement that cannot be taken is recorded under. */
export const HAS_MORE_UNMEASURABLE = "feed.has-more.unmeasurable";

/**
 * True when a bubble's scroll box ACTUALLY HIDES CONTENT: its body — the
 * rendered lines — is taller than the box shows at its collapsed cap
 * (`--bubble-cap-lines` lines, or the 50vh ceiling, whichever is less).
 *
 * STRUCTURAL, NOT A PIXEL READING (owner ruling, 2026-09-23). This used to be
 * the box's own `scrollHeight > clientHeight`, and `scrollHeight` is the box's
 * SCROLLABLE OVERFLOW: every descendant's border box, not only its lines. The
 * response's usage corner (styles.css `.usage-corner`) floats inside the box
 * with a `0.4rem` vertical padding cancelled by `-0.4rem` margins — a hit area
 * that is cancelled in LAYOUT (its margin box is one line) but not in the
 * overflow, so its border box hangs `0.4rem` above the box's top and `0.4rem`
 * below its first line. On a one-line answer ("?") that bottom edge sat a few
 * px below the body's one line, `scrollHeight` beat `clientHeight`, and a
 * bubble with nothing hidden drew "more below". The body holds exactly the
 * rendered lines and nothing floated beside them, so the comparison is lines
 * against the cap: a bubble at or under its cap never wears the fade, whatever
 * decoration its box carries.
 *
 * A bubble box with no body is not a bubble this app drew: an invariant
 * violation, recorded once and thrown, never read as "nothing hidden".
 */
export function hidesContentBeyondCap(box: HTMLElement): boolean {
  const body = box.querySelector<HTMLElement>(BODY_SELECTOR);
  if (body === null) {
    log.error("a bubble's has-more cannot be measured: its scroll box holds no body", {
      operation: HAS_MORE_UNMEASURABLE,
      context: { box: box.className, children: box.childElementCount },
    });
    throw new Error("has-more unmeasurable: the bubble's scroll box holds no body");
  }
  return body.offsetHeight > box.clientHeight;
}

/**
 * One kind of box the affordance serves: the selector a box is recognized by,
 * when that box's FOLD is open, and whether it hides content. The affordance
 * only ever points at content a collapsed fold is hiding, so each kind states
 * whose fold it reads and what it measures.
 */
interface MoreKind {
  readonly selector: string;
  isOpen(box: HTMLElement): boolean;
  hidesContent(box: HTMLElement): boolean;
}

/**
 * Every kind of box that may wear `has-more`, and nothing else. A bubble's
 * scroll box is its own fold (open when it is `.expanded`) and measures its
 * body's lines against its cap; a title fold measures its clamped text.
 */
const MORE_KINDS: readonly MoreKind[] = [
  {
    selector: MORE_BUBBLE_SELECTOR,
    isOpen: (box) => box.classList.contains(EXPANDED_CLASS),
    hidesContent: hidesContentBeyondCap,
  },
  {
    selector: `.${TITLE_FOLD_CLASS}`,
    isOpen: (box) => box.matches(TITLE_FOLD_OPEN_SELECTOR),
    hidesContent: overflowsCap,
  },
];

/**
 * True when the box should wear `has-more`: it is one of the kinds the
 * affordance serves, its fold is COLLAPSED, and it hides content. An open fold
 * never shows it (the whole content is reachable), and a box that fits its cap
 * has nothing below the fold to point at.
 */
export function shouldShowMore(box: HTMLElement): boolean {
  const kind = MORE_KINDS.find((k) => box.matches(k.selector));
  return kind !== undefined && !kind.isOpen(box) && kind.hidesContent(box);
}

/** Add or drop `has-more` on BOX to match its current overflow and state. */
export function refreshHasMore(box: HTMLElement): void {
  if (shouldShowMore(box)) box.classList.add(HAS_MORE_CLASS);
  else box.classList.remove(HAS_MORE_CLASS);
}

/** The content a bubble's scroll box holds: its body, never the corner beside it. */
export function bubbleBodyOf(scroll: HTMLElement): Element | null {
  return scroll.querySelector(BODY_SELECTOR);
}

/**
 * Keep SCROLL's `has-more` in step with its size for the life of the box. A
 * `ResizeObserver` watches the box (its `clientHeight` — the cap moving under a
 * viewport change or an expand/collapse) and its body (its `scrollHeight` — the
 * prose growing or re-wrapping), refreshing on either. The observer is torn
 * down with the bubble through `onDiscard`/`stopTicking` (ticking.ts), so it
 * never outlives the box it watches. A host with no `ResizeObserver` (a test
 * with none installed) simply never auto-refreshes; the expand toggle's
 * synchronous refresh still keeps it correct across a click. REFRESH is what a
 * resize runs, `refreshHasMore` unless the caller wraps it (title-fold.ts adds
 * its orphan check). CONTENT names the child whose growth moves the answer —
 * a bubble's box passes `bubbleBodyOf`, since a response's usage corner sits
 * BEFORE its body and a first-child reading followed the corner instead, so a
 * body growing past a cap the box had already reached was never re-measured.
 */
export function installHasMore(
  scroll: HTMLElement,
  refresh: (box: HTMLElement) => void = refreshHasMore,
  content: (box: HTMLElement) => Element | null = (box) => box.firstElementChild,
): void {
  const view = scroll.ownerDocument?.defaultView;
  if (view === null || view === undefined || typeof view.ResizeObserver !== "function") return;
  const observer = new view.ResizeObserver(() => refresh(scroll));
  observer.observe(scroll);
  // THE BODY IS FOLLOWED, NOT CAPTURED: a redraw that keeps the box a reader
  // is scrolled inside hands it a new body (keep-scroll.ts), and the observer
  // must measure the body the box now holds rather than the one it was built
  // with.
  let body = content(scroll);
  if (body !== null) observer.observe(body);
  const retarget = (): void => {
    const next = content(scroll);
    if (next === body) return;
    if (body !== null) observer.unobserve(body);
    body = next;
    if (body !== null) observer.observe(body);
    refresh(scroll);
  };
  const children = typeof view.MutationObserver === "function" ? new view.MutationObserver(retarget) : null;
  children?.observe(scroll, { childList: true });
  onDiscard(scroll, () => {
    observer.disconnect();
    children?.disconnect();
  });
}
