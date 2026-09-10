/**
 * ticking — subscribing a piece of drawn DOM to the shared clock, and
 * unsubscribing it when the feed throws that DOM away.
 *
 * CLOCKS TICK CLIENT-SIDE: the wire ships instants and the client animates the
 * count-up, the countdown and the "quiet for N s" itself, through the ONE
 * shared `Ticker` — never a `setInterval` of its own. What that leaves open is
 * the LIFETIME question: a feed row is redrawn whole on every push, so a
 * subscription taken by the drawing of a row that has since been replaced would
 * go on ticking against a detached element forever, and a busy feed would
 * accumulate one such subscription per push.
 *
 * The subscription is therefore tied to the ELEMENT that shows it. A renderer
 * calls `tick`, which subscribes and marks the element; whoever discards the
 * element calls `stopTicking`, which unsubscribes that element and every
 * marked descendant of it. Neither side has to know how many subscriptions the
 * other took.
 *
 * WHY A MARKER ATTRIBUTE AND A WEAK MAP RATHER THAN A REGISTRY OF ELEMENTS. The
 * attribute is what makes the descendants FINDABLE from the element being
 * discarded (one `querySelectorAll`), and the weak map is what keeps a
 * discarded element's entry collectable even if nobody ever calls
 * `stopTicking` — a leak of one closure rather than a leak of the whole subtree.
 */
import type { Ticker } from "../clock.js";

/** The attribute marking an element that holds clock subscriptions. */
export const TICKING_ATTRIBUTE = "data-ticking";

/** Every live unsubscribe, per element that owns one. */
const subscriptions = new WeakMap<Element, Set<() => void>>();

/**
 * Subscribe FN to the shared clock for as long as EL is on screen, and run it
 * ONCE immediately so the element paints its current reading rather than
 * waiting up to a whole tick to say anything.
 */
export function tick(el: Element, ticker: Ticker, fn: (nowMs: number) => void): void {
  fn(ticker.now());
  const unsubscribe = ticker.subscribe(fn);
  const existing = subscriptions.get(el);
  if (existing === undefined) {
    subscriptions.set(el, new Set([unsubscribe]));
  } else {
    existing.add(unsubscribe);
  }
  el.setAttribute(TICKING_ATTRIBUTE, "1");
}

/**
 * Drop every clock subscription EL and its descendants hold.
 *
 * Called by whoever is discarding the DOM — the row controller replacing a
 * row's body, a bubble tearing down its sub-feed — so a renderer never has to
 * be handed a disposer to return.
 */
export function stopTicking(el: Element): void {
  release(el);
  for (const descendant of el.querySelectorAll(`[${TICKING_ATTRIBUTE}]`)) release(descendant);
}

/** One element's subscriptions, dropped and forgotten. */
function release(el: Element): void {
  const held = subscriptions.get(el);
  if (held === undefined) return;
  for (const unsubscribe of held) unsubscribe();
  subscriptions.delete(el);
  el.removeAttribute(TICKING_ATTRIBUTE);
}
