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

/**
 * Which stop ends a clock.
 *
 * WORK clocks count the unit a row is about — an elapsed time, a "quiet for
 * N s", a countdown the turn is waiting on — so they end with the turn: a
 * `stopClocks` sweep ends them, and so does a discard.
 *
 * PRESENT clocks measure the reader's now against a fixed instant — "5m 30s
 * ago" — so they stay true after the turn ends and only a DISCARD ends them.
 * A finished turn's backstop leaves them running.
 */
type ClockKind = "work" | "present";

/** One live subscription and the stop that ends it. */
interface Subscription {
  readonly kind: ClockKind;
  readonly unsubscribe: () => void;
}

/** Every live subscription, per element that owns one. */
const subscriptions = new WeakMap<Element, Set<Subscription>>();

/** Every discard hook, per element that registered one. */
const discards = new WeakMap<Element, Set<() => void>>();

/**
 * Subscribe FN to the shared clock for as long as EL is on screen, and run it
 * ONCE immediately so the element paints its current reading rather than
 * waiting up to a whole tick to say anything.
 */
export function tick(el: Element, ticker: Ticker, fn: (nowMs: number) => void): void {
  subscribe(el, ticker, fn, "work");
}

/**
 * `tick`, for a PRESENT clock: a reading of the reader's now against a fixed
 * instant ("5m 30s ago"), which stays true after the row's turn has ended.
 * Only a discard (`stopTicking`) ends it; a `stopClocks` sweep leaves it
 * running.
 */
export function tickWhileShown(el: Element, ticker: Ticker, fn: (nowMs: number) => void): void {
  subscribe(el, ticker, fn, "present");
}

/** The subscription both `tick` flavors take, painting once immediately. */
function subscribe(
  el: Element,
  ticker: Ticker,
  fn: (nowMs: number) => void,
  kind: ClockKind,
): void {
  fn(ticker.now());
  const held: Subscription = { kind, unsubscribe: ticker.subscribe(fn) };
  const existing = subscriptions.get(el);
  if (existing === undefined) {
    subscriptions.set(el, new Set([held]));
  } else {
    existing.add(held);
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
  const stopped = releaseAcross(el, ALL_KINDS);
  releaseDiscards(el);
  for (const descendant of el.querySelectorAll(`[${DISCARD_ATTRIBUTE}]`)) {
    releaseDiscards(descendant);
  }
  return stopped;
}

/**
 * Drop every WORK clock EL and its descendants hold, leaving their PRESENT
 * clocks (`tickWhileShown`) and discard hooks in place, and say how many
 * elements had a work clock stopped.
 *
 * For an element that stays on screen while its work ends — a finished turn's
 * rows, swept by feed-view's backstop — so a measurer drawn on it keeps
 * working and an "ago" drawn on it keeps counting.
 */
export function stopClocks(el: Element): number {
  return releaseAcross(el, WORK_ONLY);
}

const ALL_KINDS: ReadonlySet<ClockKind> = new Set(["work", "present"]);
const WORK_ONLY: ReadonlySet<ClockKind> = new Set(["work"]);

/** Release KINDS on EL and every ticking descendant; count elements released. */
function releaseAcross(el: Element, kinds: ReadonlySet<ClockKind>): number {
  let stopped = release(el, kinds);
  for (const descendant of el.querySelectorAll(`[${TICKING_ATTRIBUTE}]`)) {
    stopped += release(descendant, kinds);
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

/**
 * One element's subscriptions of KINDS, dropped and forgotten. 1 if it held
 * any. The marker stays while a subscription of another kind is still live.
 */
function release(el: Element, kinds: ReadonlySet<ClockKind>): number {
  const held = subscriptions.get(el);
  if (held === undefined) return 0;
  let released = 0;
  for (const sub of [...held]) {
    if (!kinds.has(sub.kind)) continue;
    sub.unsubscribe();
    held.delete(sub);
    released = 1;
  }
  if (held.size === 0) {
    subscriptions.delete(el);
    el.removeAttribute(TICKING_ATTRIBUTE);
  }
  return released;
}
