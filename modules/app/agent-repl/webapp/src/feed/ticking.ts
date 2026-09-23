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

/**
 * The attribute marking an element that holds DISCARD hooks (`onDiscard`) —
 * teardown that is not a clock, such as a `ResizeObserver` measuring a box.
 *
 * Its own marker, apart from `data-ticking`, because a hook must outlive a
 * CLOCK stop: a card that settles stops its clocks, and a finished turn's
 * backstop stops whatever clocks its rows still hold (`stopClocks`), yet the
 * row stays on screen and its measurers must keep measuring. Only a DISCARD
 * (`stopTicking`) ends them. It also keeps a measured element out of every
 * `[data-ticking]` query, which asks about clocks.
 */
export const DISCARD_ATTRIBUTE = "data-discard";

/** Every live unsubscribe, per element that owns one. */
const subscriptions = new WeakMap<Element, Set<() => void>>();

/** Every discard hook, per element that registered one. */
const discards = new WeakMap<Element, Set<() => void>>();

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
 * Register DISPOSE to run when EL is discarded, so `stopTicking` (called by
 * whoever throws the DOM away) tears it down with no disposer for the renderer
 * to hold. A CLOCK stop (`stopClocks`) leaves it in place. It is for the
 * non-clock teardown a drawn element still needs — a `ResizeObserver` watching a
 * bubble's width, say — which would otherwise outlive the element it observes.
 */
export function onDiscard(el: Element, dispose: () => void): void {
  const existing = discards.get(el);
  if (existing === undefined) {
    discards.set(el, new Set([dispose]));
  } else {
    existing.add(dispose);
  }
  el.setAttribute(DISCARD_ATTRIBUTE, "1");
}

/**
 * DISCARD EL: drop every clock subscription EL and its descendants hold, run
 * every discard hook they registered, and say HOW MANY elements held a clock.
 *
 * Called by whoever is discarding the DOM — the row controller replacing a
 * row's body, a bubble tearing down its sub-feed — so a renderer never has to
 * be handed a disposer to return. The COUNT is what lets a backstop say
 * whether it stopped anything, rather than logging on every sweep.
 */
export function stopTicking(el: Element): number {
  const stopped = stopClocks(el);
  releaseDiscards(el);
  for (const descendant of el.querySelectorAll(`[${DISCARD_ATTRIBUTE}]`)) {
    releaseDiscards(descendant);
  }
  return stopped;
}

/**
 * Drop every CLOCK subscription EL and its descendants hold, leaving their
 * discard hooks in place, and say how many elements held a clock.
 *
 * For an element that stays on screen while its clocks end — a finished turn's
 * rows, swept by feed-view's backstop — so a measurer drawn on it keeps working.
 */
export function stopClocks(el: Element): number {
  let stopped = release(el);
  for (const descendant of el.querySelectorAll(`[${TICKING_ATTRIBUTE}]`)) {
    stopped += release(descendant);
  }
  return stopped;
}

/**
 * Put NEXT in HOST's place of children, stopping every child being DROPPED.
 *
 * THE ONE REPLACE HELPER. A bare `replaceChildren` detaches whatever it is
 * throwing away without unsubscribing it, and a detached element that still
 * holds a subscription ticks against a node nobody can see for as long as the
 * page lives. Every site that replaces a host's children with a set drawn from
 * live elements goes through here, so the stop cannot be forgotten at one of
 * them. An element that appears in NEXT is being MOVED, not discarded, and
 * keeps its subscriptions.
 *
 * Returns how many dropped elements were still ticking.
 */
export function replaceTicking(host: Element, next: readonly Node[] = []): number {
  const kept = new Set<Node>(next);
  let stopped = 0;
  for (const child of [...host.children]) {
    if (kept.has(child)) continue;
    stopped += stopTicking(child);
  }
  host.replaceChildren(...next);
  return stopped;
}

/** One element's discard hooks, run and forgotten. */
function releaseDiscards(el: Element): void {
  const held = discards.get(el);
  if (held === undefined) return;
  for (const dispose of held) dispose();
  discards.delete(el);
  el.removeAttribute(DISCARD_ATTRIBUTE);
}

/** One element's subscriptions, dropped and forgotten. 1 if it held any. */
function release(el: Element): number {
  const held = subscriptions.get(el);
  if (held === undefined) return 0;
  for (const unsubscribe of held) unsubscribe();
  subscriptions.delete(el);
  el.removeAttribute(TICKING_ATTRIBUTE);
  return 1;
}
