/**
 * bubble-more — the "more below" affordance on a COLLAPSED response or
 * user-prompt bubble whose text overruns its height cap.
 *
 * FIX2 (owner ruling, 2026-09-15): the owner asked for "a signal that there's
 * more to reveal" on the response and prompt bubbles. When a bubble is collapsed
 * and its content actually overflows the cap (`scrollHeight > clientHeight` on
 * the `.bubble-scroll` box), the box wears `has-more`; the stylesheet draws a
 * bottom fade into the bubble's own background plus a small chevron from that
 * class. The affordance is RESTRICTED to `.bubble.assistant` and `.bubble.user`
 * scroll boxes only (the response and prompt bubbles) — never a tool-call
 * section or any other capped box — which is what MORE_BUBBLE_SELECTOR encodes.
 *
 * Nothing here draws the fade or the chevron: those are pure CSS keyed on
 * `has-more` (styles.css). This module only MEASURES the overflow and toggles
 * the class, at the three moments it can change: on draw and on width re-wrap
 * (the `ResizeObserver` in `installHasMore`, which also catches a viewport
 * change moving the 50vh cap), and on expand/collapse (a synchronous refresh
 * driven from the expand toggle, since a short-window expand leaves the box the
 * same height and so fires no resize).
 */
import { EXPANDED_CLASS } from "../expand.js";
import { onDiscard } from "./ticking.js";

/** The class the stylesheet turns into the bottom fade + chevron. */
export const HAS_MORE_CLASS = "has-more";

/**
 * The only scroll boxes the "more below" affordance is allowed on: a response
 * bubble (`.bubble.assistant`) and a prompt bubble (`.bubble.user`, which also
 * covers an agent-addressed prompt — a user-kind bubble). One entry per bubble
 * kind, so a tool-call section's capped box can never match it.
 *
 * The class name here is the LITERAL of `BUBBLE_SCROLL_CLASS` (bubble-scroll.ts)
 * rather than an import of it: bubble-scroll.ts imports `installHasMore` from
 * this module, and importing the constant back would form a module cycle that
 * reads the const in its temporal dead zone. bubble-more.test.ts holds this
 * literal to the exported constant so the two cannot drift.
 */
export const MORE_BUBBLE_SELECTOR =
  ".bubble.assistant > .bubble-scroll, .bubble.user > .bubble-scroll";

/** The box as the measurement sees it: its overflow, its state, its identity. */
export interface MoreBox {
  classList: {
    add(name: string): void;
    remove(name: string): void;
    contains(name: string): boolean;
  };
  matches(selector: string): boolean;
  scrollHeight: number;
  clientHeight: number;
}

/** True when the content is taller than the box can show at its current cap. */
export function overflowsCap(box: { scrollHeight: number; clientHeight: number }): boolean {
  return box.scrollHeight > box.clientHeight;
}

/**
 * One kind of box the affordance serves: the selector a box is recognized by,
 * and when that box's FOLD is open. The affordance only ever points at content
 * a collapsed fold is hiding, so each kind states whose fold it reads.
 */
interface MoreKind {
  readonly selector: string;
  isOpen(box: MoreBox): boolean;
}

/**
 * Every kind of box that may wear `has-more`, and nothing else. A response or
 * prompt bubble's scroll box is its own fold: it is open when it is `.expanded`.
 */
const MORE_KINDS: readonly MoreKind[] = [
  {
    selector: MORE_BUBBLE_SELECTOR,
    isOpen: (box) => box.classList.contains(EXPANDED_CLASS),
  },
];

/**
 * True when the box should wear `has-more`: it is one of the kinds the
 * affordance serves, its fold is COLLAPSED, and its content overflows the cap.
 * An open fold never shows it (the whole content is reachable), and a box that
 * fits its cap has nothing below the fold to point at.
 */
export function shouldShowMore(box: MoreBox): boolean {
  const kind = MORE_KINDS.find((k) => box.matches(k.selector));
  return kind !== undefined && !kind.isOpen(box) && overflowsCap(box);
}

/** Add or drop `has-more` on BOX to match its current overflow and state. */
export function refreshHasMore(box: MoreBox): void {
  if (shouldShowMore(box)) box.classList.add(HAS_MORE_CLASS);
  else box.classList.remove(HAS_MORE_CLASS);
}

/**
 * Keep SCROLL's `has-more` in step with its size for the life of the box. A
 * `ResizeObserver` watches the box (its `clientHeight` — the cap moving under a
 * viewport change or an expand/collapse) and its body (its `scrollHeight` — the
 * prose growing or re-wrapping), refreshing on either. The observer is torn
 * down with the bubble through `onDiscard`/`stopTicking` (ticking.ts), so it
 * never outlives the box it watches. A host with no `ResizeObserver` (a test
 * with none installed) simply never auto-refreshes; the expand toggle's
 * synchronous refresh still keeps it correct across a click.
 */
export function installHasMore(scroll: HTMLElement): void {
  const view = scroll.ownerDocument?.defaultView;
  if (view === null || view === undefined || typeof view.ResizeObserver !== "function") return;
  const observer = new view.ResizeObserver(() => refreshHasMore(scroll));
  observer.observe(scroll);
  const body = scroll.firstElementChild;
  if (body !== null) observer.observe(body);
  onDiscard(scroll, () => observer.disconnect());
}
