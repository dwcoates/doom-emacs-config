/**
 * Intent-armed inner scrolling.
 *
 * Every capped section in the feed (Read previews, Bash command/output,
 * diffs, tool input/output, a bubble's own scroll box) is its own scroll
 * box, so a wheel gesture aimed at the feed gets swallowed whenever the
 * pointer happens to sit over one of them. The gate: a section takes the
 * wheel ONLY when it is the ARMED section; over any other section the
 * wheel is redirected to the feed, so scrolling past a section is the
 * default and scrolling the section itself is the deliberate act.
 *
 * A section arms ONLY by a deliberate POINTER act — a pointermove that
 * moves INTO it, or a pointerdown/click inside it — never by the feed
 * scrolling it under a still cursor. That distinction is the whole point:
 * when the feed scrolls a section up under a stationary pointer the browser
 * fires mouseenter/mouseover but NO pointermove, so the section that
 * "landed under" the cursor was never entered and stays unarmed, and the
 * next wheel gesture redirects to the feed instead of getting stuck in the
 * bubble. Moving the cursor out and back in re-arms, matching the reader's
 * own sense of which box they mean to scroll. The pure helpers below decide;
 * `installIntentScroll` wires the decision to real events and elements.
 *
 * A purely horizontal wheel is always left to the browser (see
 * `armedWheelAction`), so a wide code block inside a section still pans.
 *
 * The feed's own tail-follow owner lives here too: it is the other half of the
 * same question of who owns the scroll position, the user or the feed.
 *
 * THE USER OWNS THE SCROLL (owner rule, 2026-09-23). This module is the ONE
 * place anything writes a scroll position, and every write is either the
 * reader's own input (the wheel redirect, a collapse click) or one of the
 * closed set of named causes in `SCROLL_CAUSES`. A bubble's own scroll box has
 * no implicit writer at all. `test/scroll.test.ts` scans every other source
 * module and fails on any scroll write found outside this one.
 */
import { ancestorMatching } from "./dom.js";
import { log } from "./log.js";

/** Slack below which a downward gesture still counts as arriving at the tail. */
const PIN_PX = 40;

/** Wheel deltaMode units (WheelEvent.DOM_DELTA_*). */
const DELTA_LINE = 1;
const DELTA_PAGE = 2;
/** Line height assumed when a wheel event reports its delta in lines. */
const LINE_PX = 16;

/** The geometry that makes an element a scroll box. */
export interface ScrollMetrics {
  scrollHeight: number;
  clientHeight: number;
  overflowY: string;
}

/** Where a scroll box currently sits along its own scrollable height. */
export interface ScrollPosition {
  scrollHeight: number;
  scrollTop: number;
  clientHeight: number;
}

/**
 * THE CLOSED SET OF IMPLICIT FEED-SCROLL CAUSES (owner rule, 2026-09-23).
 *
 * The feed moves without the reader's own scroll input for exactly these
 * reasons, and for no other:
 *
 * - `promptSent`: a prompt this client sent was drawn; the feed parks at its
 *   tail and follows until the reader scrolls away.
 * - `selectionMoved`: the reader stepped the reply-to-a-past-response
 *   selection by keybinding; the selected row is centered, and a cleared
 *   selection returns to the tail.
 * - `detachedWorkSelected`: the reader picked a detached-work item in the
 *   expanded footer; the feed brings that item's card into view.
 * - `initialPlacement`: a feed's FIRST paint lands at its tail. Placement, not
 *   a scroll change.
 * - `replaceRestore`: a page REPLACE (re-open after reconnect or handover)
 *   lands at the tail, by the earlier owner ruling of 2026-09-23.
 * - `prependCompensation`: older rows landing above the reader shift the view
 *   by exactly their height, so the content under the reader stays put.
 * - `collapseCompensation`: a thinking bubble wholly ABOVE the reader collapsed
 *   because the daemon marked it superseded (a later response landed in its
 *   feed); the view shifts by exactly the height it lost, so the content under
 *   the reader stays put. Same semantics as `prependCompensation`.
 *
 * Every move is recorded at DEBUG as `scroll.feed-moved` with its cause.
 */
export const SCROLL_CAUSES = [
  "promptSent",
  "selectionMoved",
  "detachedWorkSelected",
  "initialPlacement",
  "replaceRestore",
  "prependCompensation",
  "collapseCompensation",
] as const;

/** One named reason the feed may move without the reader's scroll input. */
export type ScrollCause = (typeof SCROLL_CAUSES)[number];

/** The causes that land the feed at its tail and latch the follow. */
type ParkCause = "promptSent" | "selectionMoved" | "initialPlacement" | "replaceRestore";

/** Registering a listener for a box's own scroll events. */
export type SubscribeScroll = (onScroll: () => void) => void;

/** Registering a listener for a box's own size changes. */
export type SubscribeResize = (onResize: () => void) => void;

/** Registering a listener for the reader's own input on a box. */
export type SubscribeInput = (onInput: () => void) => void;

/** Everything the tail owner reads and writes on the box it guards. */
export type ReanchorBox = ScrollPosition;

/**
 * THE SINGLE OWNER OF THE FEED'S SCROLL POSITION.
 *
 * Every implicit move of the feed is one of its cause-named methods, and
 * nothing else in the webapp writes the feed's position. Its park and shift
 * primitives are private, so a caller cannot move the feed without naming why.
 *
 * FOLLOW IS LATCHED, NEVER SAMPLED. Only a parking cause starts it: a sent
 * prompt, a first placement, a replace, a cleared selection. The follow then
 * keeps the tail on screen as later content arrives (`follow`, `onResize`),
 * attributed to the cause that started it, until the READER scrolls away.
 * The reader coming back to the tail does not restart it, and neither does
 * geometry: an empty box is not "following" just because it is at its bottom.
 *
 * WHY THE READER MUST HAVE DONE SOMETHING. The box writes its own position too
 * -- `scrollTop` cannot sit past the end of the scrollable range, so content
 * shrinking drags it down, and the drag is indistinguishable from a gesture
 * upward by looking at the number. So a movement ends the follow ONLY once a
 * real user input has reached the box (`onInput`): the clamp arrives with
 * nothing behind it and is inert, and the gesture arrives with an input and
 * ends the follow on its first upward pixel. Measured, from the hibernated
 * tab's playbook under load: `scrollTop=52` with `scrollHeight=853
 * clientHeight=637` and again `scrollHeight=1099` -- a clamp, held while 400px
 * of new rows arrived beneath it.
 *
 * WHY IT RECONCILES ON EVERY READ. The browser dispatches scroll
 * asynchronously, so a render running between the reader's gesture and its
 * event would read the pre-gesture answer and park the feed under them.
 * `sync` compares the box's live position against the last one this owner
 * knows about, so a read can never precede the movement it is about.
 */
export class TailFollow {
  private following = false;
  /** The cause whose follow is standing, while one is. */
  private cause: ParkCause | null = null;
  /** The last position this owner knows about: what it wrote, or what it saw. */
  private lastTop: number;
  /** Whether the READER has reached this box since the tail was last parked. */
  private touched = false;

  constructor(private readonly box: ReanchorBox) {
    this.lastTop = box.scrollTop;
  }

  /** Whether new content should pull the view. The one question, one answer. */
  isFollowing(): boolean {
    this.sync();
    return this.following;
  }

  /** A prompt this client sent was drawn: park at the tail and follow. */
  promptSent(): void {
    this.park("promptSent");
  }

  /** A feed's first paint: land at the tail and follow. */
  initialPlacement(): void {
    this.park("initialPlacement");
  }

  /** A page replace (re-open after reconnect or handover): land at the tail and follow. */
  replaceRestore(): void {
    this.park("replaceRestore");
  }

  /** The reader cleared the reply selection: return to the tail and follow. */
  selectionCleared(): void {
    this.park("selectionMoved");
  }

  /**
   * The reader stepped the reply selection: stop following, so streaming rows
   * cannot pull them off the selection, and center the selected row when the
   * feed has drawn it (GEOMETRY null: nothing to center on).
   */
  selectionMoved(geometry: CenterGeometry | null): void {
    this.release();
    if (geometry !== null) this.shift("selectionMoved", centerDelta(geometry));
  }

  /**
   * The reader picked a detached-work item in the footer: stop following, and
   * bring the item's card as far into view as fits (`revealDelta`).
   */
  detachedWorkSelected(geometry: RevealGeometry): void {
    this.release();
    this.shift("detachedWorkSelected", revealDelta(geometry));
  }

  /**
   * Older rows grew GROWN px above the reader: shift by exactly that, so the
   * content under them stays put. A following reader is already at the tail,
   * which the follow keeps, so nothing is added on top of it.
   */
  prependCompensation(grown: number): void {
    if (this.isFollowing()) return;
    this.shift("prependCompensation", grown);
  }

  /**
   * A thinking bubble collapsed when the daemon marked it superseded: when it
   * lies wholly ABOVE the viewport, shift by exactly the height it lost, so the
   * content under the reader stays put (`collapseDelta`). A following reader is
   * already kept at the tail by the follow, so nothing is added on top of it; a
   * bubble the reader can see, or one whose height did not change (the reader
   * had expanded it), moves nothing and records nothing.
   */
  collapseCompensation(geometry: CollapseGeometry): void {
    if (this.isFollowing()) return;
    const delta = collapseDelta(geometry);
    if (delta === 0) return;
    this.shift("collapseCompensation", delta);
  }

  /** Keep the tail on screen while a follow stands; nothing otherwise. */
  follow(): void {
    this.sync();
    if (!this.following || this.cause === null) return;
    this.park(this.cause);
  }

  /** A scroll event on the box. Everything it decides lives in `sync`. */
  onScroll(): void {
    this.sync();
  }

  /**
   * A USER INPUT reached the box -- a wheel, a touch, a pointer on its bar, a
   * key while something in it has focus. It decides nothing on its own; it is
   * what makes the NEXT movement of the box attributable to the reader.
   */
  onInput(): void {
    this.touched = true;
  }

  /**
   * A resize of the box or of its content. A standing follow keeps the tail on
   * the settled layout (the docked footer taking height, a deferred card
   * settling); a reader who scrolled away is left exactly where they are.
   */
  onResize(): void {
    this.follow();
  }

  /** Wire the box's own events into the owner. */
  observe(
    subscribeScroll: SubscribeScroll,
    subscribeResize: SubscribeResize,
    subscribeInput: SubscribeInput,
  ): void {
    subscribeScroll(() => this.onScroll());
    subscribeResize(() => this.onResize());
    subscribeInput(() => this.onInput());
  }

  /** Land the box at its tail and latch the follow under CAUSE. */
  private park(cause: ParkCause): void {
    const from = this.box.scrollTop;
    const wasFollowing = this.following && this.cause === cause;
    this.box.scrollTop = this.box.scrollHeight;
    this.lastTop = this.box.scrollTop;
    this.following = true;
    this.cause = cause;
    // The reader's last input spoke about a position this park has replaced.
    this.touched = false;
    // A follow that found the box already at its tail moved nothing, and is
    // not recorded; every cause's own act is, moved or not.
    if (wasFollowing && this.lastTop === from) return;
    recordMove(cause, from, this.lastTop, wasFollowing);
  }

  /** Move the box BY delta without starting a follow. */
  private shift(cause: ScrollCause, delta: number): void {
    this.sync();
    const from = this.box.scrollTop;
    if (delta !== 0) this.box.scrollTop += delta;
    this.lastTop = this.box.scrollTop;
    recordMove(cause, from, this.lastTop, false);
  }

  /** Stop following: only a parking cause starts it again. */
  private release(): void {
    this.sync();
    this.following = false;
    this.cause = null;
  }

  /**
   * Fold any movement this owner did not write into the decision.
   *
   * A position equal to the last one it knows about decides nothing, which is
   * what makes its own writes -- and the scroll events the browser dispatches
   * for them afterward -- inert. THE BOX'S OWN CLAMP IS NOT THE READER: content
   * shrinking drags the position down, so the baseline is lowered into the
   * reachable range first. A movement with the reader's input behind it ends
   * the follow unless it went down to the tail; nothing here ever starts one.
   */
  private sync(): void {
    const reachable = Math.max(0, this.box.scrollHeight - this.box.clientHeight);
    if (this.lastTop > reachable) this.lastTop = reachable;
    const top = this.box.scrollTop;
    if (top === this.lastTop) return;
    // The reader's movement ends a standing follow unless it went DOWN and
    // landed at the tail: any upward pixel ends it (a flick upward starts with
    // a few px), and so does a downward one that stops short of the tail.
    const stayed = top > this.lastTop && this.box.scrollHeight - top - this.box.clientHeight < PIN_PX;
    if (this.touched && this.following && !stayed) {
      log.debug("the reader scrolled away from the tail; the follow ends", {
        operation: "scroll.follow-ended",
        context: { cause: this.cause ?? "none", from: this.lastTop, to: top },
      });
      this.following = false;
      this.cause = null;
    }
    this.lastTop = top;
  }
}

/** One implicit move of the feed, recorded with the cause that made it. */
function recordMove(cause: ScrollCause, from: number, to: number, follow: boolean): void {
  log.debug(`the feed moved for ${cause}`, {
    operation: "scroll.feed-moved",
    context: { cause, from, to, follow },
  });
}

/**
 * Wire a REAL scroll box to its tail owner: the box's own scroll events, the
 * box's own size changes, and the size of the CONTENT inside it.
 *
 * THE SIZE HALF IS WHAT KEEPS THE LAST BUBBLE OUT FROM UNDER THE FOOTER.
 * The progress footer is a flex sibling laid out BELOW the scroll box
 * (index.html), so the box's height is the window's minus whatever the footer
 * currently occupies. Every time the footer appears, gains a row, or opens its
 * panel, the box loses exactly that much height — and losing height moves
 * nothing on its own: `scrollTop` stays where it was, so the tail the reader
 * was parked at now sits that many pixels below the fold and the last bubble
 * is clipped by the footer's top edge. Reserving space would not help, because
 * the space is already reserved by the layout; what is stale is the POSITION.
 *
 * It came and went between otherwise identical runs because it turns entirely
 * on ORDER: a footer that settles BEFORE the render that parks the tail is
 * already accounted for, and one that settles after is not. `TailFollow` was
 * written for exactly this (`onResize`), but nothing in production had ever
 * subscribed it to anything — the owner only ever heard about renders. This is
 * the subscription.
 *
 * A ResizeObserver reports the box's new size before paint, so a STANDING
 * follow (one a named cause latched) is kept at the tail of the settled
 * viewport rather than a frame later; a reader who scrolled away, or a feed no
 * cause ever parked, is left exactly where it is -- `onResize`'s own rule.
 *
 * THE CONTENT HALF IS THE OTHER WAY THE TAIL GOES STALE. `scrollHeight`
 * growing under a `scrollTop` nobody moved leaves the tail as far below the
 * fold as a shrinking viewport does: a deferred card settling to its real
 * height, a bubble's sub-feed painting its page, a font relayout. A standing
 * follow keeps the tail through those too.
 *
 * `scrollHeight` is not observable, but it is the sum of the box's children's
 * heights, so the children are what is watched — and the child set is kept in
 * step with the DOM, so a content root mounted after this call is watched too
 * rather than silently exempt.
 *
 * Returns the unsubscriber. A mount that drops it leaks an observer onto an
 * element the next workspace will mount over.
 */
export function observeScrollBox(box: HTMLElement, tail: TailFollow): () => void {
  const observer = new ResizeObserver(() => tail.onResize());
  const onScroll = (): void => tail.onScroll();
  const onInput = (): void => tail.onInput();
  const watched = new Set<Element>();
  const watchChildren = (): void => {
    for (const child of box.children) {
      if (watched.has(child)) continue;
      watched.add(child);
      observer.observe(child);
    }
    for (const child of watched) {
      if (child.parentElement === box) continue;
      watched.delete(child);
      observer.unobserve(child);
    }
  };
  const children = new MutationObserver(watchChildren);
  tail.observe(
    () => box.addEventListener("scroll", onScroll, { passive: true }),
    () => {
      observer.observe(box);
      watchChildren();
      children.observe(box, { childList: true });
    },
    () => {
      for (const kind of READER_INPUTS) {
        box.addEventListener(kind, onInput, { passive: true, capture: true });
      }
    },
  );
  return () => {
    box.removeEventListener("scroll", onScroll);
    for (const kind of READER_INPUTS) {
      box.removeEventListener(kind, onInput, { capture: true });
    }
    children.disconnect();
    observer.disconnect();
    watched.clear();
  };
}

/**
 * EVERY WAY THE READER CAN MOVE THIS BOX, as the events that arrive first.
 *
 * `wheel` is the trackpad and the mouse wheel, `touchstart` the drag on a
 * touch screen, `pointerdown` the grab on the scrollbar (and the click that
 * starts a jump), `keydown` the arrows, page keys and Home/End while something
 * inside the box has focus. Each of them PRECEDES the movement it causes, and
 * each bubbles to the box, so listening on the box catches them wherever inside
 * it they land -- captured and passive, so nothing here can alter or delay what
 * the reader asked for.
 *
 * A source missing from this list would be a gesture the follow latch cannot
 * see, and the feed would pull the reader back off it (`TailFollow.onInput`).
 * That is the failure to look for if a new input path is added -- a
 * scroll-snap control, a custom scrollbar, a gamepad -- rather than a silent
 * degradation.
 */
const READER_INPUTS = ["wheel", "touchstart", "pointerdown", "keydown"] as const;

/**
 * The two boxes a reveal compares, in ONE coordinate system.
 *
 * Viewport coordinates (what `getBoundingClientRect` answers) rather than
 * content-relative offsets, because the node whose reveal is asked for sits an
 * arbitrary number of positioned ancestors below the scroll box — a sub-feed
 * panel nested inside another bubble's panel — and `offsetTop` would then be
 * measured against whichever of them happens to be the offset parent. The two
 * rects are read off the same layout in the same units, so their difference is
 * a scroll delta and nothing has to be reconstructed.
 */
export interface RevealGeometry {
  /** The scroll box's own top edge. */
  boxTop: number;
  /** The scroll box's visible height. */
  boxHeight: number;
  /** The revealed node's top edge. */
  nodeTop: number;
  /** The revealed node's full height, however far past the fold it runs. */
  nodeHeight: number;
}

/**
 * How far the box must move for NODE to be as visible as it can be, WITHOUT
 * pushing the node's own top off the viewport.
 *
 * This is the detached-work selection (`TailFollow.detachedWorkSelected`), as
 * an arithmetic. The node is the card the reader picked in the footer:
 *
 * - a card already wholly on screen is not moved at all (0);
 * - a card running BELOW the fold is scrolled up by exactly its overhang,
 *   capped at the card's distance from the top of the viewport, so a card
 *   taller than the viewport lands with its own top flush with the box's;
 * - a card above the viewport top is brought down to it.
 *
 * Positive is downward, matching `scrollTop`.
 */
export function revealDelta(g: RevealGeometry): number {
  const boxBottom = g.boxTop + g.boxHeight;
  const nodeBottom = g.nodeTop + g.nodeHeight;
  if (g.nodeTop < g.boxTop) return g.nodeTop - g.boxTop;
  if (nodeBottom <= boxBottom) return 0;
  return Math.min(nodeBottom - boxBottom, g.nodeTop - g.boxTop);
}

/** Read BOX and NODE's reveal geometry off the live layout (reading moves nothing). */
export function revealGeometry(box: Element, node: Element): RevealGeometry {
  const b = box.getBoundingClientRect();
  const n = node.getBoundingClientRect();
  return { boxTop: b.top, boxHeight: b.height, nodeTop: n.top, nodeHeight: n.height };
}

/**
 * What a collapse compensation reads: the scroll box's top edge and the
 * collapsing row's bottom edge before and after its redraw, all in viewport
 * coordinates (`getBoundingClientRect`), so their differences are scroll deltas.
 */
export interface CollapseGeometry {
  /** The scroll box's own top edge. */
  boxTop: number;
  /** The row's bottom edge before the redraw that collapsed it. */
  rowBottomBefore: number;
  /** The row's bottom edge after it. */
  rowBottomAfter: number;
}

/**
 * How far the box must move for a collapse to leave the content under the
 * reader where it was.
 *
 * Only a row that ended at or above the viewport's top edge is compensated:
 * everything the reader sees sits below it, so it all moved by exactly the
 * row's change in height, which is the change in its bottom edge (its top did
 * not move). A row the reader can see any part of is not: the reader is
 * watching it collapse. Negative is upward, matching `scrollTop`.
 */
export function collapseDelta(g: CollapseGeometry): number {
  if (g.rowBottomBefore > g.boxTop) return 0;
  return g.rowBottomAfter - g.rowBottomBefore;
}

/**
 * The geometry a CENTER-SCROLL reads, in the box's OWN coordinate space, where
 * a row's `offsetTop` and the box's `scrollTop` are the same units, so their
 * difference is a scroll position with nothing to reconstruct.
 */
export interface CenterGeometry {
  /** The node's top, in the box's own scroll coordinates (`offsetTop`). */
  nodeOffsetTop: number;
  /** The node's full height (`offsetHeight`), however far past the fold it runs. */
  nodeHeight: number;
  /** The box's visible height (`clientHeight`). */
  clientHeight: number;
  /** The box's full scrollable height (`scrollHeight`). */
  scrollHeight: number;
  /** The box's current `scrollTop`. */
  scrollTop: number;
}

/**
 * How far the box must move for NODE to sit CENTERED in the viewport, clamped
 * at the feed's own edges.
 *
 * This is the reply-to-a-past-response center-scroll, as an arithmetic. The
 * daemon names the selected final-response row; the webapp puts it in the
 * middle of the feed — UNLESS the row is near the feed's start or end, where
 * there is not room to center it and the best that fits is the edge:
 *
 * - a row with room on both sides is placed with its top `(clientHeight -
 *   nodeHeight) / 2` below the box's top, which puts its center on the box's
 *   center;
 * - a row so near the START that centering would scroll above the top is
 *   clamped at `scrollTop = 0` — the feed's first row cannot rise past the top;
 * - a row so near the END that centering would scroll past the last reachable
 *   position is clamped at `scrollHeight - clientHeight`.
 *
 * Positive is downward, matching `scrollTop` (`TailFollow.selectionMoved`).
 */
export function centerDelta(g: CenterGeometry): number {
  const idealTop = g.nodeOffsetTop - (g.clientHeight - g.nodeHeight) / 2;
  const maxScrollTop = Math.max(0, g.scrollHeight - g.clientHeight);
  const target = Math.min(Math.max(idealTop, 0), maxScrollTop);
  return target - g.scrollTop;
}

/** True when the element both clips its content and scrolls it vertically. */
export function isScrollBox(m: ScrollMetrics): boolean {
  if (m.overflowY !== "auto" && m.overflowY !== "scroll") return false;
  return m.scrollHeight - m.clientHeight > 1;
}

/** Wheel delta in pixels, whatever unit the event reported it in. */
export function wheelDeltaPx(e: { deltaY: number; deltaMode: number }, viewportPx: number): number {
  if (e.deltaMode === DELTA_LINE) return e.deltaY * LINE_PX;
  if (e.deltaMode === DELTA_PAGE) return e.deltaY * viewportPx;
  return e.deltaY;
}

/**
 * THE INTENT-ARM DECISION: does the section under the wheel keep it?
 *
 * A section keeps its own wheel ONLY when it is the section the reader armed
 * — a real, identity comparison of the armed scroll box against the one the
 * wheel landed over. Every other section (and there is no third case) hands
 * its wheel to the feed, which is exactly what a section the FEED scrolled
 * under a still cursor is: never armed, because arming is a pointer act
 * (`installIntentScroll`) and the feed moving is not one, so its wheel
 * redirects rather than getting stuck. A wheel over no section at all
 * (`wheelScroller === null`) is not "kept" here either — the feed is already
 * the browser's target for it, so there is nothing to redirect.
 */
export function sectionTakesWheel<T>(armed: T | null, wheelScroller: T | null): boolean {
  return wheelScroller !== null && wheelScroller === armed;
}

/**
 * The whole wheel decision: null leaves the event to the browser, a
 * number is the pixel delta to add to the feed's scrollTop instead.
 *
 * A purely horizontal wheel is always the browser's, so a wide code block
 * inside a section still pans on shift-wheel. A wheel over the armed section,
 * over no section, or with no scrollable feed to redirect to is likewise the
 * browser's; only a wheel over a NON-armed section is redirected to the feed.
 */
export function armedWheelAction<T>(opts: {
  armed: T | null;
  wheelScroller: T | null;
  feedScrollable: boolean;
  deltaY: number;
  deltaMode: number;
  feedHeight: number;
}): number | null {
  if (opts.deltaY === 0) return null;
  if (!opts.feedScrollable) return null;
  if (opts.wheelScroller === null) return null;
  if (sectionTakesWheel(opts.armed, opts.wheelScroller)) return null;
  return wheelDeltaPx(opts, opts.feedHeight);
}

/**
 * Innermost scroll box at or above `start`, stopping below `feed`.
 * Returns null when nothing between the pointer and the feed scrolls.
 */
export function innerScrollerAt<T extends { parentElement: T | null }>(
  start: T | null,
  feed: T,
  metrics: (node: T) => ScrollMetrics,
): T | null {
  return ancestorMatching(start, feed, (node) => isScrollBox(metrics(node)));
}

/**
 * The section a scroll box belongs to: the nearest enclosing card, or
 * the box itself when no card encloses it. The lit gutters ride the
 * section rather than the box, so they sit flush with the section's
 * left/right edges and run its FULL height, not just the height of
 * whichever sub-box the pointer happens to be over.
 */
export function sectionFor<T extends { parentElement: T | null }>(
  box: T,
  feed: T,
  isSection: (node: T) => boolean,
): T {
  for (let node: T | null = box; node && node !== feed; node = node.parentElement) {
    if (isSection(node)) return node;
  }
  return box;
}

/** The scroll metrics of a real element, for `innerScrollerAt`. */
const domMetrics = (el: HTMLElement): ScrollMetrics => ({
  scrollHeight: el.scrollHeight,
  clientHeight: el.clientHeight,
  overflowY: getComputedStyle(el).overflowY,
});

/**
 * Arm intent-based inner scrolling on `feed` (the scrollable feed region).
 *
 * A wheel over a NON-armed inner scroll box is redirected to the feed; a
 * wheel over the armed box, or over no inner box at all, is the browser's.
 * A section arms ONLY by a deliberate pointer act:
 *
 *   - a `pointermove` that moves INTO a box (the box under the pointer became
 *     the box the pointer is now over), and
 *   - a `pointerdown`/click inside a box.
 *
 * and NEVER by `mouseenter`/`mouseover`. THAT is the fix, and it turns on one
 * fact about the browser: when the feed scrolls a box up UNDER A STATIONARY
 * pointer, mouseenter/mouseover fire but pointermove does NOT. So a box that
 * was scrolled-into arrives unarmed and its wheel redirects to the feed, while
 * a box that was moved-into arrives armed and keeps its wheel — the exact
 * difference between "the reader put the cursor here" and "the feed slid this
 * under the cursor". Setting `armed` from every pointermove also re-arms on
 * entry to a different box and disarms over bare feed, so "move the cursor out
 * and back in" re-arms, which is what the reader expects.
 *
 * The wheel listener is the ONE non-passive piece: it must `preventDefault`
 * to stop the browser scrolling the section it is redirecting off of. The
 * pointer listeners only READ, so they stay passive.
 *
 * Returns `{ uninstall, arm }`. `uninstall` removes the listeners, exactly as
 * the bare unsubscriber did before. `arm` is a SECOND, PROGRAMMATIC entry to
 * the very same armed-state latch a pointermove writes — not a parallel
 * notion of "armed" — for the one gesture that arms nothing on its own: a
 * click that expands a bubble fires no pointermove, so the box it just
 * revealed would otherwise sit unarmed until the reader's cursor happened to
 * move (see `installClickExpand`'s `afterToggle`, which calls this on
 * expand). It resolves the same innermost-scroll-box lookup a pointer event
 * would, from any element inside (or equal to) that box.
 */
export function installIntentScroll(feed: HTMLElement): { uninstall: () => void; arm: (el: Element) => void } {
  const scrollerUnder = (target: EventTarget | null): HTMLElement | null =>
    innerScrollerAt(target instanceof HTMLElement ? target : null, feed, domMetrics);

  // The armed box: the inner scroll box the reader last deliberately entered.
  // Written ONLY from pointer events below and from `arm` — never from the
  // wheel, and never from mouseenter/mouseover — which is what keeps a
  // scrolled-into box unarmed.
  let armed: HTMLElement | null = null;

  const onWheel = (e: WheelEvent): void => {
    const delta = armedWheelAction({
      armed,
      wheelScroller: scrollerUnder(e.target),
      feedScrollable: feed.scrollHeight - feed.clientHeight > 1,
      deltaY: e.deltaY,
      deltaMode: e.deltaMode,
      feedHeight: feed.clientHeight,
    });
    if (delta === null) return;
    e.preventDefault();
    // NOT through TailFollow, and deliberately so: this IS the reader's own
    // wheel, merely redirected off a section onto the feed, so it is user input
    // and no implicit cause. The owner reads it as the gesture it is (up ends
    // the follow), exactly the treatment a wheel on the feed itself gets.
    feed.scrollTop += delta;
  };

  // A pointer act names the box the reader means: the one under the pointer
  // now. Over bare feed that is null, which disarms. Because this fires on
  // pointermove/pointerdown ONLY, a box the feed scrolled under a still pointer
  // (mouseenter/mouseover, no pointermove) never reaches here and stays unarmed.
  const arm = (e: PointerEvent): void => {
    armed = scrollerUnder(e.target);
  };

  feed.addEventListener("wheel", onWheel, { capture: true, passive: false });
  feed.addEventListener("pointermove", arm, { passive: true });
  feed.addEventListener("pointerdown", arm, { passive: true });
  return {
    uninstall: () => {
      feed.removeEventListener("wheel", onWheel, { capture: true });
      feed.removeEventListener("pointermove", arm);
      feed.removeEventListener("pointerdown", arm);
    },
    arm: (el: Element) => {
      armed = scrollerUnder(el);
    },
  };
}

/**
 * THE READER COLLAPSED A SECTION: show its collapsed preview from the top.
 *
 * Owner ruling, 2026-09-15 (FIX3): "unselecting the expanded bubble should
 * return it to the original state -- scrolled to the top, not where you left
 * it". Collapsing clips the box and would otherwise keep whatever position the
 * expanded view was left at. This runs ONLY from the reader's own collapse
 * click (`installClickExpand`, expand.ts), so it is user input and not an
 * implicit move: a bubble's scroll box has no other writer.
 */
export function collapseClicked(section: { scrollTop: number }): void {
  section.scrollTop = 0;
}
