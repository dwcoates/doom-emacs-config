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
 * The feed's own tail-following metric (isPinnedToBottom) lives here too:
 * it is the other half of the same question of who owns the scroll
 * position, the user or the feed.
 */
import { ancestorMatching } from "./dom.js";

/** Slack below which the feed still counts as parked at its tail. */
export const PIN_PX = 40;

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

/** Registering a listener for a box's own scroll events. */
export type SubscribeScroll = (onScroll: () => void) => void;

/** Registering a listener for a box's own size changes. */
export type SubscribeResize = (onResize: () => void) => void;

/** Registering a listener for the reader's own input on a box. */
export type SubscribeInput = (onInput: () => void) => void;

/** Everything the tail owner reads and writes on the box it guards. */
export type ReanchorBox = ScrollTail & ScrollPosition;

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
 * `park()` or `shift()`. Nothing else writes the feed's scrollTop toward the
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
 *
 * AND WHY A MOVEMENT IS NOT ENOUGH: THE READER MUST HAVE DONE SOMETHING. The
 * box writes its own position too — `scrollTop` cannot sit past the end of the
 * scrollable range, so content shrinking drags it down, and the drag is
 * indistinguishable from a gesture upward by looking at the number. Correcting
 * for that inside `sync` only works while the range is still short, and nothing
 * schedules `sync` there; a shrink and a regrowth between two reconciles leave
 * the position at the old bottom under a range that has moved on. So intent is
 * read off the box's position ONLY once a real user input has reached it
 * (`onInput`): the clamp arrives with nothing behind it and is inert, and the
 * gesture arrives with an input and ends the follow on its first upward pixel.
 */
export class TailFollow {
  private following: boolean;
  /** The last position this owner knows about: what it wrote, or what it saw. */
  private lastTop: number;
  /** Whether the READER has reached this box since the tail was last parked. */
  private touched = false;

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
   * restored-session render, a replaced page (feed-view.ts's
   * `parkAfterReplace`), and a render that is following.
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
    // The reader's last input spoke about a position this park has replaced,
    // so it stops speaking here (see `onInput`).
    this.touched = false;
  }

  /**
   * Move the box BY delta without changing the follow decision — a backfill
   * that grew the feed above the viewport shifting the view by exactly that
   * growth, so what the reader is looking at does not move.
   *
   * RELATIVE, because the growth is only ever known as a difference.
   * Expressing it as "read the position, add, write it back" at the call site
   * would read one box and write another the moment the two ever differ;
   * keeping the whole arithmetic inside the owner makes them the
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
   * A USER INPUT reached the box — a wheel, a touch, a pointer on its bar, a
   * key while something in it has focus. It decides nothing on its own; it is
   * what makes the NEXT movement of the box attributable to the reader.
   *
   * WHY THE LATCH NEEDS THIS, and it is a measured defect rather than a
   * precaution. `sync` reads intent out of the box's position, and the box
   * writes that position too: `scrollTop` can never sit past the end of the
   * scrollable range, so content shrinking drags it down and the drag looks
   * exactly like a gesture upward. `sync` corrects for that by lowering its
   * baseline into the range — which works only if it is LOOKING while the range
   * is still short. Nothing guarantees that it is. A shrink and a regrowth that
   * land between two reconciles (a `ResizeObserver` reports one coalesced size
   * per frame, and any forced layout inside the frame applies the clamp) leave
   * the position at the OLD bottom under a range that has moved on, and the
   * next reconcile reads a gesture nobody made. The follow then ends for good:
   * only arriving back at the tail or an explicit `park` resumes it, and
   * nothing parks a feed that is not following.
   *
   * Measured, from the hibernated tab's playbook under load: `scrollTop=52`
   * with `scrollHeight=853 clientHeight=637` and again `scrollHeight=1099`,
   * where 52 is exactly the reachable extent the feed had had one turn
   * earlier -- a clamp, held while 400px of new rows arrived beneath it.
   *
   * So the reader must have DONE something before a movement may be read as
   * theirs. A clamp arrives with no input behind it and is therefore inert,
   * whatever the timing; a gesture arrives with one and the direction rule
   * above applies to it verbatim, on the very first upward pixel.
   */
  onInput(): void {
    this.touched = true;
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
   *
   * IT IS ALSO WHAT THE CONTENT'S OWN SIZE REPORTS THROUGH. A box that keeps
   * the same viewport while the rows inside it grow has moved its tail exactly
   * as far as one that shrank, and the correction is identical — so the two
   * arrive at the same method rather than at a twin that could drift from it.
   */
  onResize(): void {
    this.sync();
    if (this.following) this.park();
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

  /**
   * Fold any movement this owner did not write into the decision.
   *
   * A position equal to the last one it knows about decides nothing, which is
   * what makes its own `park`/`shift` writes — and the scroll events the
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
    // AND ONLY THE READER IS ON THE OTHER SIDE OF THIS COMPARISON. A movement
    // with no user input behind it is the box's own (see `onInput`), so it
    // re-baselines and decides nothing.
    if (this.touched) {
      this.following = top > this.lastTop && isPinnedToBottom(this.box, this.pinPx);
    }
    this.lastTop = top;
  }
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
 * A ResizeObserver reports the box's new size before paint, so a following
 * feed is re-parked on the settled viewport rather than a frame later; a
 * reader who scrolled away is left where they are, which is `onResize`'s own
 * rule and not re-decided here.
 *
 * THE CONTENT HALF IS THE OTHER WAY THE TAIL GOES STALE, and it is the half
 * that outlived the footer fix. Watching only the box hears every change to
 * the VIEWPORT and none to what is inside it, yet `scrollHeight` growing under
 * a `scrollTop` nobody moved leaves the tail exactly as far below the fold as
 * a shrinking viewport does. The renders that append rows re-park themselves
 * (`feed-view.ts`'s followTail), so the growth that escapes is the growth NO
 * render performs: a bubble the wire pushed unfolded fetches its own page and
 * paints it into its panel milliseconds later (`bubble.ts`'s `initialFolded`
 * open), a deferred card settles to its real height, a font or a highlighted
 * block relayouts. Each of those grows the feed after the last park, and under
 * load — where the fetch behind that unfold is slowest — it is the LAST thing
 * that happens, so nothing follows it to correct the position. That is the
 * `awaitTailClearsFooter` failure that survived subscribing the box.
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
 * Positive is downward, matching `scrollTop`, so the caller hands the result
 * straight to `TailFollow.shift`.
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
    // wheel, merely redirected off a section onto the feed. The owner reads it
    // as the gesture it is — up ends the follow, back to the tail resumes it —
    // exactly the treatment a wheel on the feed itself gets.
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

