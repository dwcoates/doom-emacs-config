/**
 * Edge-gated inner scrolling.
 *
 * Every capped section in the feed (Read previews, Bash command/output,
 * diffs, tool input/output) is its own scroll box, so a wheel gesture
 * aimed at the feed gets swallowed whenever the pointer happens to sit
 * over one of them. The gate: a section only takes the wheel when the
 * pointer is in its left- or right-most gutter (EDGE_PX wide). Anywhere
 * else over the section the wheel is redirected to the feed, so
 * scrolling past a section is the default and scrolling the section
 * itself is the deliberate act. The pure helpers below decide; a DOM-facing
 * caller wires the decision to real events and elements.
 *
 * The feed's own tail-following metric (isPinnedToBottom) lives here too:
 * it is the other half of the same question of who owns the scroll
 * position, the user or the feed.
 */
import { ancestorMatching } from "./dom.js";

/** Width of the left/right gutters that arm a section's own scrolling. */
export const EDGE_PX = 32;

/** Slack below which the feed still counts as parked at its tail. */
export const PIN_PX = 40;

/** Wheel deltaMode units (WheelEvent.DOM_DELTA_*). */
const DELTA_LINE = 1;
const DELTA_PAGE = 2;
/** Line height assumed when a wheel event reports its delta in lines. */
const LINE_PX = 16;

/** The horizontal span of an element's box. */
export interface Box {
  left: number;
  right: number;
}

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
 * True when the box is parked at its tail (within PIN_PX of the bottom).
 * A pinned feed follows new content; an unpinned one holds the user's
 * place, so this is what a render consults before moving scrollTop.
 */
export function isPinnedToBottom(pos: ScrollPosition, pinPx: number = PIN_PX): boolean {
  return pos.scrollHeight - pos.scrollTop - pos.clientHeight < pinPx;
}

/** The one mutable field parking a box at its tail touches. */
export interface ScrollTail {
  scrollTop: number;
  scrollHeight: number;
}

/**
 * Put a scroll box at its tail in a single jump. Assigning scrollTop is
 * what makes it a jump rather than an animation: the tail is simply THERE
 * on the next frame, with no crawl down the history to watch. Every site
 * that wants the newest content on screen goes through here — the
 * restored-session render, the tail-following render, and the Emacs
 * host's workspace-switch snap (host.ts).
 */
export function parkAtTail(box: ScrollTail): void {
  box.scrollTop = box.scrollHeight;
}

/**
 * The reader's PLACE in a feed, as something that survives the feed being
 * rebuilt from nothing.
 *
 * A scrollTop cannot survive a rebuild: every element is discarded and
 * recreated, so the number it named describes a layout that no longer exists.
 * What does survive is an ITEM — the topmost one still on screen — and how far
 * down the viewport it sat. Restoring the pair puts the same content back under
 * the reader's eyes whatever the rebuild did to the heights above it.
 *
 * `key` is the feed's own per-item key (`data-key`), so this depends on nothing
 * about how the items are rendered.
 */
export interface FeedAnchor {
  key: string;
  /** The anchor item's offset from the viewport top, in px, at capture. */
  offsetPx: number;
  /** Whether the reader was following the tail: then the tail IS the anchor. */
  pinned: boolean;
}

/** The box operations anchoring reads and writes. */
export interface AnchorBox extends ScrollTail {
  scrollTop: number;
  clientHeight: number;
  querySelector(selectors: string): { offsetTop: number } | null;
}

/** One rendered feed item, as the anchor capture reads it. */
export interface AnchorItem {
  key: string;
  offsetTop: number;
}

/**
 * Sample where the reader is, BEFORE a rebuild discards the elements.
 *
 * The topmost item whose bottom is still below the viewport top is the one the
 * reader is looking at; anything above it has already scrolled away. A feed
 * with no items, or one the reader is following the tail of, anchors on the
 * tail — which is what `pinned` says and what `restoreFeedAnchor` then does.
 *
 * FOLLOWING IS TOLD TO IT, NOT MEASURED HERE. Deriving it from geometry made
 * this a second owner of the tail question (see `TailFollow`), and a second
 * owner that read the PIN_PX slack band: a reader who had just nudged a few px
 * off the bottom was captured as "following", and the rebuild's restore then
 * parked them at the tail — the same yank the render used to produce, on a
 * different trigger. The caller passes `TailFollow.isFollowing()`, and the
 * answer to the question is one answer.
 */
export function captureFeedAnchor(
  box: AnchorBox,
  items: readonly AnchorItem[],
  following: boolean,
): FeedAnchor | null {
  if (following) return { key: "", offsetPx: 0, pinned: true };
  for (const item of items) {
    if (item.offsetTop >= box.scrollTop) {
      return { key: item.key, offsetPx: item.offsetTop - box.scrollTop, pinned: false };
    }
  }
  return null;
}

/**
 * Put the reader back where `captureFeedAnchor` found them, AFTER the rebuild.
 *
 * A pinned reader is parked at the tail, which is where they were. Anyone else
 * is placed so the anchor item sits at the same offset from the viewport top it
 * sat at before — the identical pixels, however the heights above it changed.
 *
 * AN ANCHOR THAT NO LONGER EXISTS IS NOT GUESSED AT. The item may have been
 * cleared or compacted away, and inventing a position for it would move the
 * reader somewhere they never were; the box is left exactly as the rebuild left
 * it, and the caller's own tail rule applies. Returns whether the anchor was
 * restored, so a caller can say which happened.
 */
export function restoreFeedAnchor(
  box: AnchorBox,
  anchor: FeedAnchor | null,
  tail: TailWriter,
): boolean {
  if (anchor === null) return false;
  if (anchor.pinned) {
    tail.park();
    return true;
  }
  const node = box.querySelector(`[data-key="${cssEscapeKey(anchor.key)}"]`);
  if (node === null) return false;
  // Through the owner, not a bare assignment: a restore that wrote scrollTop
  // itself would be seen by the owner as the READER scrolling — upward, on a
  // rebuild that shortened the feed — and would end a follow the reader never
  // ended. `place` moves the pixels and leaves the intent alone.
  tail.place(node.offsetTop - anchor.offsetPx);
  return true;
}

/**
 * Did this render put a DIFFERENT item at the feed's top?
 *
 * The load-more prepend's whole hazard: a page of older messages lands above
 * everything the reader is looking at, the feed grows by the height of ten
 * messages, and without compensation the viewport is left showing content it
 * was never showing. The reader asked for MORE of what they had, not to be
 * moved off it.
 *
 * A key comparison rather than a height comparison, because height changes for
 * reasons that are not a prepend at all — a card expanding, a deferred item
 * settling — and compensating those would move the reader instead. Only the
 * item AT THE TOP changing says content was inserted above.
 *
 * An empty feed on either side answers false: there was no reading position to
 * preserve, and the caller's own tail rule owns where an empty feed lands.
 */
export function feedTopChanged(previousTopKey: string | null, nextTopKey: string | null): boolean {
  if (previousTopKey === null || nextTopKey === null) return false;
  return previousTopKey !== nextTopKey;
}

/**
 * Escape a feed key for use inside an attribute selector.
 *
 * Keys are the daemon's uuids and derived strings, but a selector built from an
 * unescaped one is a parse error waiting for the first key with a quote in it —
 * and a throwing selector inside a render is a frozen feed.
 */
function cssEscapeKey(key: string): string {
  return key.replace(/["\\]/g, "\\$&");
}

/** Registering a listener for a box's own scroll events. */
export type SubscribeScroll = (onScroll: () => void) => void;

/** Registering a listener for a box's own size changes. */
export type SubscribeResize = (onResize: () => void) => void;

/** Everything the tail owner reads and writes on the box it guards. */
export type ReanchorBox = ScrollTail & ScrollPosition;

/**
 * The two writes anything moving the feed is allowed to make (see `TailFollow`).
 *
 * Narrower than the owner itself so a caller that only needs to MOVE the feed
 * cannot reach the decision, and so a test can hand one in without a box.
 */
export interface TailWriter {
  park(): void;
  place(top: number): void;
}

/**
 * THE SINGLE OWNER OF "SHOULD THE FEED BE FOLLOWING ITS TAIL".
 *
 * The feed's position used to be written by four parties that each derived the
 * answer for themselves — the render's fresh `isPinnedToBottom` sample, a
 * separate nested-view freeze flag, the resize re-anchor's own latch, and the
 * rebuild anchor's own pin test. Four derivations of one question is four
 * chances to disagree, and they did: a render sampling geometry while the user
 * was mid-gesture answered about the pixels rather than about the intent, and
 * parked the feed under them.
 *
 * So intent is LATCHED here and nowhere else. Every mechanism that wants to
 * know asks `isFollowing()`; every mechanism that wants to move the feed calls
 * `park()` or `place()`. Nothing else writes the feed's scrollTop toward the
 * tail, and nothing else reads geometry to decide whether it should.
 *
 * WHY LATCHED AND NOT SAMPLED. `isPinnedToBottom` has a PIN_PX slack band, and
 * the first moments of every upward gesture live inside it — a trackpad flick
 * begins with deltas of a few px. A render landing in that window sampled
 * "still pinned", parked at the tail, and undid the gesture; the user pushed
 * again, and got the erratic downward yank they reported. It hurt scrolling UP
 * far more than DOWN because leaving the tail is the only direction that has to
 * cross the band against a mechanism actively pulling the other way.
 *
 * The latch's rule makes DIRECTION decisive rather than distance: any movement
 * of the box UP ends the follow, whatever the slack says, and only a movement
 * that arrives back AT the tail resumes it. There is no band for a gesture to
 * be trapped inside.
 *
 * WHY IT RECONCILES ON EVERY READ. A latch fed only by the scroll EVENT is
 * still stale where it matters most: the browser dispatches scroll
 * asynchronously, so a render running between the user's gesture and its event
 * would read the pre-gesture answer, see "following", and park the feed —
 * the same yank, now on a timing rather than a geometry mistake. `sync` closes
 * that window by comparing the box's live scrollTop against the last position
 * this owner knows about, so a read can never precede the movement it is
 * about.
 */
export class TailFollow {
  private following: boolean;
  /** The last position this owner knows about: what it wrote, or what it saw. */
  private lastTop: number;

  constructor(
    private readonly box: ReanchorBox,
    private readonly pinPx: number = PIN_PX,
  ) {
    this.following = isPinnedToBottom(box, pinPx);
    this.lastTop = box.scrollTop;
  }

  /** Whether new content should pull the view. The one question, one answer. */
  isFollowing(): boolean {
    this.sync();
    return this.following;
  }

  /**
   * Park the box at its tail and follow from here on. Every "show me the
   * newest" act routes through this: the host's workspace-switch snap, the
   * restored-session render, a freshly sent prompt, a render that is following.
   *
   * It LATCHES the follow rather than only moving the pixels, which is what
   * makes a workspace switch land at the bottom reliably: content that arrives
   * after the snap (a deferred item upgrading, a board mounting, the relayout
   * itself) is parked on again instead of being left as a gap the next render
   * would have read as "the reader is scrolled up".
   */
  park(): void {
    parkAtTail(this.box);
    this.lastTop = this.box.scrollTop;
    this.following = true;
  }

  /**
   * Put the box at TOP without changing the follow decision — a rebuild
   * restoring the reader's place, or a backfill shifting the view by exactly
   * the height it grew above the viewport. Both move the pixels precisely so
   * that nothing about what the reader is looking at has changed, so neither
   * may be mistaken for the reader moving.
   */
  place(top: number): void {
    this.sync();
    this.box.scrollTop = top;
    this.lastTop = this.box.scrollTop;
  }

  /**
   * Move the box BY delta without changing the follow decision — a backfill
   * that grew the feed above the viewport shifting the view by exactly that
   * growth, so what the reader is looking at does not move.
   *
   * RELATIVE, where `place` is absolute, because the growth is only ever known
   * as a difference. Expressing it as "read the position, add, write it back"
   * at the call site would read one box and write another the moment the two
   * ever differ; keeping the whole arithmetic inside the owner makes them the
   * same box by construction.
   */
  shift(delta: number): void {
    this.sync();
    this.box.scrollTop += delta;
    this.lastTop = this.box.scrollTop;
  }

  /**
   * Stop following: the user deliberately opened content to read (a nested view
   * inside a bubble), so streaming output must not pull the view off it. Only a
   * return to the tail, or an explicit `park`, resumes following.
   */
  release(): void {
    this.sync();
    this.following = false;
  }

  /** A scroll event on the box. Everything it decides lives in `sync`. */
  onScroll(): void {
    this.sync();
  }

  /**
   * A resize of the box. A workspace switch relayouts the feed asynchronously
   * relative to the lisp that triggered it, so the host's snap and the resize
   * land in either order — a snap that lands FIRST is otherwise undone by the
   * resize growing the scrollable height under a scrollTop that stays put.
   * Re-parking on the resize removes the ordering question instead of betting
   * on one order.
   *
   * IT RECONCILES FIRST, like every other entry point, and that is the whole
   * of this method's history. It used to skip `sync` because a resize moves
   * scrollTop by itself — a shrinking viewport clamps it downward — and a
   * reconcile that read the clamp as a gesture would drop the follow the
   * switch just asked for. Skipping the reconcile bought that at the price of
   * being the ONE path where a stale `following` could park the feed: a reader
   * who has already begun scrolling up is only known to have done so through
   * `sync`, since the browser dispatches their scroll event asynchronously and
   * may throttle it behind a whole layout. A resize landing in that window
   * parked the feed back at its tail under the gesture — and a resize only
   * lands there while the page is still laying itself out, which is why it was
   * the first upward scroll after a load that got yanked and no later one.
   *
   * `sync` now attributes the clamp itself (see there), so the reason to skip
   * it is gone and the window with it.
   */
  onResize(): void {
    this.sync();
    if (this.following) this.park();
  }

  /** Wire the box's own events into the owner. */
  observe(subscribeScroll: SubscribeScroll, subscribeResize: SubscribeResize): void {
    subscribeScroll(() => this.onScroll());
    subscribeResize(() => this.onResize());
  }

  /**
   * Fold any movement this owner did not write into the decision.
   *
   * A position equal to the last one it knows about decides nothing, which is
   * what makes its own `park`/`place` writes — and the scroll events the
   * browser dispatches for them afterward — inert. Anything else is the reader,
   * and the reader moving up ends the follow while only the reader arriving at
   * the tail resumes it.
   *
   * THE BOX'S OWN CLAMP IS NOT THE READER, and reconciling against a baseline
   * that ignored it is what made hydration attributable to them. scrollTop can
   * never sit past the end of the scrollable range, so content SHRINKING —
   * a deferred item settling to a smaller real height, a card collapsing, a
   * relayout narrowing the feed — drags the position down with it, and a
   * baseline still standing above the new range reads that drag as an upward
   * gesture and ends a follow nobody ended. Lowering the baseline into the
   * range first is what leaves only the reader on the other side of the
   * comparison. Growth needs no such treatment and gets none: it moves
   * scrollTop nowhere, so the clamp is the ONLY movement the box makes on its
   * own and this is the whole of the correction.
   */
  private sync(): void {
    const reachable = Math.max(0, this.box.scrollHeight - this.box.clientHeight);
    if (this.lastTop > reachable) this.lastTop = reachable;
    const top = this.box.scrollTop;
    if (top === this.lastTop) return;
    this.following = top > this.lastTop && isPinnedToBottom(this.box, this.pinPx);
    this.lastTop = top;
  }
}

/**
 * Wire a REAL scroll box to its tail owner: the box's own scroll events, and
 * the box's own size changes.
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
 * A ResizeObserver reports the box's new size before paint, so a following
 * feed is re-parked on the settled viewport rather than a frame later; a
 * reader who scrolled away is left where they are, which is `onResize`'s own
 * rule and not re-decided here.
 *
 * Returns the unsubscriber. A mount that drops it leaks an observer onto an
 * element the next workspace will mount over.
 */
export function observeScrollBox(box: HTMLElement, tail: TailFollow): () => void {
  const observer = new ResizeObserver(() => tail.onResize());
  const onScroll = (): void => tail.onScroll();
  tail.observe(
    () => box.addEventListener("scroll", onScroll, { passive: true }),
    () => observer.observe(box),
  );
  return () => {
    box.removeEventListener("scroll", onScroll);
    observer.disconnect();
  };
}

/** Where a revealed node lands: flush with the top, or as little as possible. */
export type RevealBlock = "start" | "nearest";

/** The one method bringing a node into view needs. */
export interface RevealTarget {
  scrollIntoView(arg: { block: RevealBlock }): void;
}

/**
 * Bring NODE into view inside the feed. The single "show me this bubble"
 * primitive: the roster's agent reveal (render.ts), the keyboard cycle
 * (nav.ts), and any later match-stepping (iterative search) must agree on
 * the mechanic, or the feed lurches differently depending on which one
 * moved it.
 *
 * `start` puts the node flush with the top, for a jump ARRIVING from
 * elsewhere. `nearest` scrolls only as far as it must, which is what a
 * cycle wants: a target already fully on screen should not be yanked
 * anywhere, since the current-marker is what says where the cycle sits.
 */
export function revealNode(node: RevealTarget, block: RevealBlock = "nearest"): void {
  node.scrollIntoView({ block });
}

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
 * This is "expanding a bubble reveals what it expands", as an arithmetic. The
 * node is the sub-feed panel that just appeared beneath a bubble's head, so:
 *
 * - a panel already wholly on screen is not moved at all (0), because a reader
 *   who can already see what they opened has nothing to be scrolled toward;
 * - a panel running BELOW the fold is scrolled up by exactly its overhang,
 *   capped at the panel's distance from the top of the viewport — the cap is
 *   what keeps the bubble's HEAD where it was, and it binds whenever the panel
 *   is taller than the viewport, where the best that fits is the panel's own
 *   top edge flush with the box's;
 * - a panel above the viewport top (a bubble opened while its head is scrolled
 *   off) is brought down to it.
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

/** What a reveal needs of the tail owner, and nothing more. */
export interface RevealWriter {
  shift(delta: number): void;
  release(): void;
}

/**
 * WHAT A BUBBLE'S CARET DOES TO THE VIEW, as the one thing that may do it.
 *
 * Expanding a fold grows the feed BELOW the fold, and growth alone moves
 * nothing: the reader clicked a caret and the sub-feed it opened unrolled
 * entirely off the bottom of the screen (measured at the click: `below=208
 * scrollTop=40`). Revealing what an expansion revealed is therefore part of
 * expanding, not a separate courtesy — and it goes through the tail owner,
 * because a second party writing `scrollTop` is exactly the arrangement
 * `TailFollow` exists to prevent.
 *
 * TWO CASES, DECIDED BEFORE THE CLICK IS ACTED ON. A reader following the tail
 * stays following it: the expansion's new rows are the newest content, so the
 * tail re-lands and they are at the bottom of it. Anyone else is holding a
 * place, so the view moves by the least that puts the opened panel on screen
 * and the follow decision is left alone — `shift` is relative and decides
 * nothing, which is precisely why it is what a reveal uses.
 */
export interface FeedReveal {
  /** Was the feed following its tail when the caret was clicked? */
  isFollowing(): boolean;
  /** Re-land the tail, for a reader who was following it. */
  park(): void;
  /** Bring NODE as far into view as fits, for a reader who was not. */
  reveal(node: HTMLElement): void;
}

/** Bind a REAL scroll box and its tail owner into the caret's view rule. */
export function feedReveal(box: HTMLElement, tail: TailFollow): FeedReveal {
  return {
    isFollowing: () => tail.isFollowing(),
    park: () => tail.park(),
    reveal: (node) => revealInBox(box, node, tail),
  };
}

/**
 * Move BOX so NODE is as visible as it fits, through TAIL.
 *
 * `release` first, which is the state this reader is now in whatever the
 * geometry says: they deliberately opened content to read, so streaming output
 * arriving into the feed underneath them must not pull the view off it. That is
 * the sentence `TailFollow.release` was written for, and until this call site
 * existed nothing in production said it.
 */
export function revealInBox(box: HTMLElement, node: HTMLElement, tail: RevealWriter): void {
  const b = box.getBoundingClientRect();
  const n = node.getBoundingClientRect();
  tail.release();
  const delta = revealDelta({
    boxTop: b.top,
    boxHeight: b.height,
    nodeTop: n.top,
    nodeHeight: n.height,
  });
  if (delta !== 0) tail.shift(delta);
}

/** True when the element both clips its content and scrolls it vertically. */
export function isScrollBox(m: ScrollMetrics): boolean {
  if (m.overflowY !== "auto" && m.overflowY !== "scroll") return false;
  return m.scrollHeight - m.clientHeight > 1;
}

/** True when clientX sits in the box's left or right gutter. */
export function inEdgeZone(box: Box, clientX: number, edgePx: number = EDGE_PX): boolean {
  return clientX - box.left <= edgePx || box.right - clientX <= edgePx;
}

/**
 * True when a wheel over `scroller` belongs to the feed instead.
 * A null scroller means the pointer is over no section at all, and an
 * unscrollable feed means there is nowhere to redirect the wheel to.
 */
export function redirectsToFeed(opts: {
  scroller: Box | null;
  clientX: number;
  feedScrollable: boolean;
  edgePx?: number;
}): boolean {
  if (!opts.scroller || !opts.feedScrollable) return false;
  return !inEdgeZone(opts.scroller, opts.clientX, opts.edgePx ?? EDGE_PX);
}

/** Wheel delta in pixels, whatever unit the event reported it in. */
export function wheelDeltaPx(e: { deltaY: number; deltaMode: number }, viewportPx: number): number {
  if (e.deltaMode === DELTA_LINE) return e.deltaY * LINE_PX;
  if (e.deltaMode === DELTA_PAGE) return e.deltaY * viewportPx;
  return e.deltaY;
}

/**
 * The whole wheel decision: null leaves the event to the browser, a
 * number is the pixel delta to add to the feed's scrollTop instead.
 * A purely horizontal wheel is always the browser's, so a wide code
 * block inside a section still pans on shift-wheel.
 */
export function wheelAction(opts: {
  scroller: Box | null;
  clientX: number;
  deltaY: number;
  deltaMode: number;
  feedScrollable: boolean;
  feedHeight: number;
  edgePx?: number;
}): number | null {
  if (opts.deltaY === 0) return null;
  if (
    !redirectsToFeed({
      scroller: opts.scroller,
      clientX: opts.clientX,
      feedScrollable: opts.feedScrollable,
      edgePx: opts.edgePx,
    })
  ) {
    return null;
  }
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

