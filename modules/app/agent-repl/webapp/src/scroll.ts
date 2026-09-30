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
 * - `promptHeld`: a held prompt's card was drawn in the hold tray for the FIRST
 *   time (one this client just sent that the daemon held included); the feed
 *   parks at its tail and follows, exactly as for `promptSent`. A re-push of a
 *   card already drawn, or its removal, moves nothing.
 * - `selectionMoved`: the reader stepped the reply-to-a-past-response
 *   selection by keybinding; the selected row is centered, and a cleared
 *   selection returns to the tail.
 * - `detachedWorkSelected`: the reader picked a detached-work item in the
 *   expanded footer; the feed CENTERS that item's card in its viewport
 *   (owner ruling, 2026-09-23), clamped at the feed's edges, and a card
 *   taller than the viewport lands with its top at the viewport's top.
 * - `itemExpanded`: the reader expanded a feed item — a capped bubble, a tool
 *   card, a subagent's bubble, a compaction's summary; the feed puts that
 *   item's vertical MIDDLE on the viewport's vertical middle at once (owner
 *   request, 2026-09-30, widening the bubble-only ruling of 2026-09-29),
 *   clamped at the feed's edges, however tall the item is
 *   (`expandCenterDelta`).
 * - `initialPlacement`: a feed's FIRST paint lands at its tail. Placement, not
 *   a scroll change.
 * - `replaceRestore`: a page REPLACE (re-open after reconnect or handover)
 *   lands at the tail, by the earlier owner ruling of 2026-09-23.
 * - `prependCompensation`: content above the reader changed height, and the
 *   view shifts by exactly that, so the content under the reader stays put.
 *   This is THE FEED'S SCROLL ANCHORING (`TailFollow`, "THE FEED OWNS ITS
 *   SCROLL ANCHORING"): any row above the anchor changing height — its first
 *   layout under `content-visibility: auto`, a late render, older rows landing
 *   above, a bubble whose sub-feed lies wholly above the viewport collapsing.
 * - `collapseCompensation`: a thinking bubble wholly ABOVE the reader collapsed
 *   because its own final text landed (the daemon re-pushed it settled); the
 *   view shifts by exactly the height it lost, so the content under
 *   the reader stays put. Same semantics as `prependCompensation`, and the
 *   same anchoring carries it out; the cause names the case.
 * - `latestVisible`: the reader can SEE the feed's latest entry
 *   (`latestEntryVisible`), so the follow latches where the view already is.
 *   Latching moves nothing; later content then keeps the tail in view, and
 *   those follow moves are recorded under this cause.
 *
 * Every move is recorded at DEBUG as `scroll.feed-moved` with its cause.
 */
export const SCROLL_CAUSES = [
  "promptSent",
  "promptHeld",
  "selectionMoved",
  "detachedWorkSelected",
  "itemExpanded",
  "initialPlacement",
  "replaceRestore",
  "prependCompensation",
  "collapseCompensation",
  "latestVisible",
] as const;

/** One named reason the feed may move without the reader's scroll input. */
export type ScrollCause = (typeof SCROLL_CAUSES)[number];

/**
 * The causes that latch the follow. All but `latestVisible` also land the feed
 * at its tail; `latestVisible` latches where the view already stands.
 */
type ParkCause =
  | "promptSent"
  | "promptHeld"
  | "selectionMoved"
  | "initialPlacement"
  | "replaceRestore"
  | "latestVisible";

/** Registering a listener for a box's own scroll events. */
export type SubscribeScroll = (onScroll: () => void) => void;

/** Registering a listener for a box's own size changes. */
export type SubscribeResize = (onResize: () => void) => void;

/** Registering a listener for the reader's own input on a box. */
export type SubscribeInput = (onInput: () => void) => void;

/** Everything the tail owner reads and writes on the box it guards. */
export type ReanchorBox = ScrollPosition;

/**
 * Reading where the feed's LATEST ENTRY sits against the box's viewport, or
 * null when the feed has no entry drawn at all. Reading moves nothing.
 */
export type ReadLatest = () => RevealGeometry | null;

/**
 * THE ONE DEFINITION OF "THE READER CAN SEE THE LATEST ENTRY" (owner rule,
 * 2026-09-23): ANY part of the entry intersects the box's visible viewport. An
 * entry whose bottom edge sits exactly on the viewport's top, or whose top
 * edge sits exactly on its bottom, shows no pixel and is not visible.
 *
 * Geometry that is not a real layout (a non-finite edge, a negative height) is
 * a fault in whatever read it, and is reported and thrown rather than guessed.
 */
export function latestEntryVisible(g: RevealGeometry): boolean {
  const edges = [g.boxTop, g.boxHeight, g.nodeTop, g.nodeHeight];
  if (!edges.every(Number.isFinite) || g.boxHeight < 0 || g.nodeHeight < 0) {
    log.error("the latest entry's geometry is not a real layout", {
      operation: "scroll.latest-geometry-invalid",
      context: { ...g },
    });
    throw new Error(
      `scroll: the latest entry's geometry is not a real layout (${JSON.stringify(g)})`,
    );
  }
  return g.nodeTop < g.boxTop + g.boxHeight && g.nodeTop + g.nodeHeight > g.boxTop;
}

/** A row's top and bottom edges, in viewport coordinates. */
export interface RowEdges {
  top: number;
  bottom: number;
}

/**
 * The feed's rows, as the scroll anchor reads them. Reading moves nothing.
 *
 * Only the ROOT rows are candidates: a sub-feed's rows scroll inside their
 * bubble's own capped box, so their edges move whenever the reader scrolls
 * that box, which says nothing about the feed.
 */
export interface AnchorRows {
  /** The element whose element children are the feed's rows, in document order. */
  readonly host: Element;
  /** The scroll box's own top edge. */
  viewportTop(): number;
  /** ROW's edges, or null when it draws no box (a hidden, empty row). */
  edges(row: Element): RowEdges | null;
}

/** The root feed's rows in HOST, read off BOX's live layout. */
export function feedAnchorRows(box: Element, host: Element): AnchorRows {
  return {
    host,
    viewportTop: () => box.getBoundingClientRect().top,
    edges: (row) => {
      if (row.getClientRects().length === 0) return null;
      const r = row.getBoundingClientRect();
      return { top: r.top, bottom: r.bottom };
    },
  };
}

/** A row found to anchor on, where its top sat, and whether it starts in view. */
interface FoundAnchor {
  row: Element;
  top: number;
  /** False for the fallback: no row starts in view, so the last one is taken. */
  starts: boolean;
}

/**
 * THE ANCHOR: the first row whose top sits at or below the viewport's top, so
 * the first row the reader sees whole from its top. Its top moves only when a
 * row BEFORE it changes height, which is exactly the movement the reader must
 * not see; its own growth runs downward from a top that stays put, and a row
 * after it moves nothing above it. When no row starts in view (one row fills
 * the viewport and nothing follows it), the last row that draws a box is taken,
 * flagged as such.
 *
 * Rows stand in document order with ascending tops, so the walk starts at HINT
 * (the previous anchor) and moves only as far as the reader scrolled.
 */
function findAnchor(rows: AnchorRows, hint: Element | null, viewTop: number): FoundAnchor | null {
  let row = hint !== null && hint.parentElement === rows.host ? hint : rows.host.firstElementChild;
  for (let prev = row?.previousElementSibling ?? null; prev !== null; prev = prev.previousElementSibling) {
    const edges = rows.edges(prev);
    if (edges === null) continue;
    if (edges.top < viewTop) break;
    row = prev;
  }
  let last: FoundAnchor | null = null;
  for (; row !== null; row = row.nextElementSibling) {
    const edges = rows.edges(row);
    if (edges === null) continue;
    if (edges.top >= viewTop) return { row, top: edges.top, starts: true };
    last = { row, top: edges.top, starts: false };
  }
  return last;
}

/** What set an anchoring pass off: a size change, a scroll event, or a caller's own measure. */
type AnchorTrigger = "resize" | "scroll" | "measured";

/** A row's name in a record: its FeedId where it carries one. */
function rowName(row: Element): string {
  return row.getAttribute("data-feed-row") ?? row.tagName.toLowerCase();
}

/**
 * THE SINGLE OWNER OF THE FEED'S SCROLL POSITION.
 *
 * Every implicit move of the feed is one of its cause-named methods, and
 * nothing else in the webapp writes the feed's position. Its park and shift
 * primitives are private, so a caller cannot move the feed without naming why.
 *
 * FOLLOW IS LATCHED, NEVER SAMPLED. Only a parking cause starts it: a sent
 * prompt, a first placement, a replace, a cleared selection -- or the reader
 * being able to SEE the latest entry (`latestVisible`, owner rule 2026-09-23),
 * checked on every scroll event, resize and row upsert. That last latch moves
 * nothing: the view stays where the reader has it, and only LATER content is
 * what the follow then keeps in view. The follow keeps the tail on screen as
 * later content arrives (`follow`, `onResize`), attributed to the cause that
 * started it, until the READER scrolls away. The bare position is not the
 * test: an empty box is not "following" just because it is at its bottom.
 *
 * AN ACTIVE REPLY SELECTION HOLDS THE LATEST-VISIBLE LATCH OFF: `selectionMoved`
 * released the tail so streaming rows cannot pull the reader off the selected
 * response, and seeing the latest entry does not undo that. Clearing the
 * selection parks (`selectionCleared`) as it always has.
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
 *
 * THE FEED OWNS ITS SCROLL ANCHORING (2026-09-27). Every `.feed-item` is
 * `content-visibility: auto`, so a row above the reader is first laid out when
 * the overscan band (feed/overscan.ts) reaches it, and its height changes from
 * the stylesheet's guess to its real one. Chromium's native CSS scroll
 * anchoring absorbs that; WebKit, the Emacs webview's engine, has none, and the
 * content under the reader jumped on every first scroll up (measured: 52 of 120
 * steps, up to 436px, test/webkit/anchoring.webkit.test.ts). So this owner
 * anchors, for every change in row height, whatever caused it:
 *
 * - it holds ONE anchor (`findAnchor`: the first root row starting in view)
 *   and that row's top in the box's CONTENT coordinates (viewport top minus
 *   box top plus `scrollTop`), which the reader's own scrolling never changes;
 * - on every size change and every scroll event it measures the anchor again
 *   and, when it moved, shifts `scrollTop` by exactly that before the frame is
 *   painted (`prependCompensation`), then takes the anchor afresh;
 * - the callers that already measure a change of their own (older rows landing
 *   above, a bubble or thinking row wholly above collapsing) go through the
 *   same pass (`compensate`), so a change is never counted twice;
 * - a reader following the tail holds no anchor: the follow keeps the tail.
 *
 * It measures BEFORE it re-takes, on the scroll event too. A layout read
 * between a height change and its resize callback (any scroll handler's)
 * already carries the change, so an anchor taken from it would hide the change
 * from the pass that should have moved the view.
 */
export class TailFollow {
  private following = false;
  /** The cause whose follow is standing, while one is. */
  private cause: ParkCause | null = null;
  /** The last position this owner knows about: what it wrote, or what it saw. */
  private lastTop: number;
  /** Whether the READER has reached this box since the tail was last parked. */
  private touched = false;
  /** Whether a reply selection is active, holding the latest-visible latch off. */
  private selectionActive = false;
  /** Whether the held-off latch was last reported, so it is reported once per spell. */
  private heldOffReported = false;
  /** The row the anchoring holds, and its top in the box's content coordinates. */
  private anchor: { row: Element; at: number; starts: boolean } | null = null;
  /** Where the next anchor search starts: the last anchor, kept across a follow. */
  private anchorHint: Element | null = null;
  /** The part of the last correction the box's own rounding did not take. */
  private anchorCarry = 0;
  /** Whether a missing anchor was last reported, so it is reported once per spell. */
  private missingReported = false;

  /**
   * READLATEST is where the feed's latest entry sits; a box with no feed to
   * read (a fixture) has no latest entry, and the latest-visible latch never
   * fires on it. ROWS are the rows the anchoring reads; without them (a
   * fixture, a box with no feed) nothing is anchored, and a caller's own
   * measure is all a compensation has.
   */
  constructor(
    private readonly box: ReanchorBox,
    private readonly readLatest: ReadLatest = () => null,
    private readonly rows: AnchorRows | null = null,
  ) {
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

  /** A held prompt's card was drawn for the first time: park at the tail and follow. */
  promptHeld(): void {
    this.park("promptHeld");
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
    this.selectionActive = false;
    this.park("selectionMoved");
  }

  /**
   * The reader stepped the reply selection: stop following, so streaming rows
   * cannot pull them off the selection, and center the selected row when the
   * feed has drawn it (GEOMETRY null: nothing to center on).
   */
  selectionMoved(geometry: CenterGeometry | null): void {
    this.selectionActive = true;
    this.release();
    if (geometry !== null) this.shift("selectionMoved", centerDelta(geometry));
    this.takeAnchor();
  }

  /**
   * The reader picked a detached-work item in the footer: stop following, and
   * CENTER the item's card in the viewport (`revealCenterDelta`).
   */
  detachedWorkSelected(geometry: RevealGeometry): void {
    this.centerReveal("detachedWorkSelected", revealCenterDelta(geometry, this.box));
  }

  /**
   * The reader expanded a feed item: stop following, and put the item's
   * vertical middle on the viewport's (`expandCenterDelta`).
   */
  itemExpanded(geometry: RevealGeometry): void {
    this.centerReveal("itemExpanded", expandCenterDelta(geometry, this.box));
  }

  /**
   * The content above the reader changed by GROWN px (older rows landing: a
   * positive figure; a sub-feed wholly above the viewport collapsing: a
   * negative one): shift by exactly that, so the content under them stays put
   * (`compensate`, the anchoring's own pass). A following reader is already at
   * the tail, which the follow keeps, so nothing is added on top of it.
   */
  prependCompensation(grown: number): void {
    if (this.isFollowing()) return;
    this.compensate("prependCompensation", grown);
  }

  /**
   * A thinking bubble collapsed when its own final text landed: when it
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
    this.compensate("collapseCompensation", delta);
  }

  /**
   * Keep the tail on screen while a follow stands. Without one, a latest entry
   * the reader can see latches it where the view stands, moving nothing now.
   */
  follow(): void {
    this.sync();
    if (!this.following || this.cause === null) {
      this.latchIfLatestVisible();
      return;
    }
    this.park(this.cause);
  }

  /**
   * A scroll event on the box: `sync` folds the movement in, then a latest
   * entry the reader can see latches the follow.
   */
  onScroll(): void {
    this.sync();
    this.reanchor("scroll");
    this.latchIfLatestVisible();
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
   * settling); for a reader who scrolled away, the anchoring keeps the content
   * under them where it was.
   */
  onResize(): void {
    this.sync();
    this.reanchor("resize");
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
    this.anchor = null;
    // The reader's last input spoke about a position this park has replaced.
    this.touched = false;
    // A follow that found the box already at its tail moved nothing, and is
    // not recorded; every cause's own act is, moved or not.
    if (wasFollowing && this.lastTop === from) return;
    recordMove(cause, from, this.lastTop, wasFollowing);
  }

  /**
   * THE ONE CENTERING REVEAL: stop following, move by the centering DELTA its
   * caller computed under CAUSE, take the anchor there, and latch where the
   * view lands if the latest entry is then in sight.
   */
  private centerReveal(cause: "detachedWorkSelected" | "itemExpanded", delta: number): void {
    this.release();
    this.shift(cause, delta);
    this.takeAnchor();
    this.latchIfLatestVisible();
  }

  /** Move the box BY delta without starting a follow. */
  private shift(cause: ScrollCause, delta: number): void {
    this.sync();
    const from = this.box.scrollTop;
    if (delta !== 0) this.box.scrollTop += delta;
    this.lastTop = this.box.scrollTop;
    recordMove(cause, from, this.lastTop, false);
  }

  /**
   * A CALLER MEASURED A CHANGE ABOVE THE READER (older rows landing, a row or a
   * sub-feed wholly above collapsing) and hands its own figure, MEASURED, in
   * the same task as the change. It is the same anchoring pass, run at once
   * rather than at the next size change: the anchor, when one starts in view,
   * is measured and the view follows it -- the one figure that also carries
   * any change the caller did not make -- and the anchor is taken afresh, so
   * the size change that follows finds nothing left to correct. With no such
   * anchor (no rows to read, or only the fallback row, whose top a change
   * inside it does not move) the caller's figure is the change.
   */
  private compensate(cause: ScrollCause, measured: number): void {
    const held = this.anchor;
    const now = held !== null && held.starts ? this.anchorTop(held.row) : null;
    if (held === null || now === null) {
      this.shift(cause, measured);
      this.takeAnchor();
      return;
    }
    const delta = now - held.at;
    if (delta !== measured) {
      log.debug(`a ${cause} measured ${measured.toString()}px; the anchor moved ${delta.toString()}px, and the view follows the anchor`, {
        operation: "scroll.anchor-measure-differs",
        context: { cause, measured, delta, anchor: rowName(held.row) },
      });
    }
    if (delta !== 0) this.correct(cause, "measured", delta, held.row);
    this.takeAnchor();
  }

  /**
   * THE ANCHORING PASS: measure the held anchor, move the view by however far
   * it moved, and take the anchor afresh. A following reader holds none.
   */
  private reanchor(trigger: AnchorTrigger): void {
    if (this.rows === null) return;
    if (this.following) {
      this.anchor = null;
      return;
    }
    const held = this.anchor;
    if (held !== null) {
      const now = this.anchorTop(held.row);
      if (now === null) {
        log.debug("the scroll anchor left the feed or stopped drawing; a new one is taken", {
          operation: "scroll.anchor-lost",
          context: { trigger, anchor: rowName(held.row) },
        });
      } else if (now !== held.at) {
        this.correct("prependCompensation", trigger, now - held.at, held.row);
      }
    }
    this.takeAnchor();
    this.reportMissingAnchor(trigger);
  }

  /**
   * Move the view by DELTA, the distance ROW's top moved in the content, so it
   * sits where the reader last saw it. The box rounds `scrollTop`, so what it
   * did not take is carried into the next correction instead of accumulating.
   */
  private correct(cause: ScrollCause, trigger: AnchorTrigger, delta: number, row: Element): void {
    this.sync();
    const from = this.box.scrollTop;
    const wanted = from + delta + this.anchorCarry;
    this.shift(cause, wanted - from);
    const carry = wanted - this.box.scrollTop;
    // More than a pixel short is the box's clamp at an edge, not rounding.
    this.anchorCarry = Math.abs(carry) < 1 ? carry : 0;
    log.debug(`the rows above the reader moved ${delta.toString()}px; the view moved with them`, {
      operation: "scroll.anchor-corrected",
      context: { cause, trigger, delta, anchor: rowName(row), from, to: this.box.scrollTop },
    });
  }

  /** Take the anchor from the live layout: none while following, or with no rows to read. */
  private takeAnchor(): void {
    this.anchor = null;
    if (this.rows === null || this.following) return;
    const viewTop = this.rows.viewportTop();
    const found = findAnchor(this.rows, this.anchorHint, viewTop);
    if (found === null) return;
    this.anchorHint = found.row;
    this.anchor = { row: found.row, at: found.top - viewTop + this.box.scrollTop, starts: found.starts };
  }

  /** ROW's top in the box's content coordinates, or null when it is no longer a drawn row. */
  private anchorTop(row: Element): number | null {
    if (this.rows === null || row.parentElement !== this.rows.host) return null;
    const edges = this.rows.edges(row);
    if (edges === null) return null;
    return edges.top - this.rows.viewportTop() + this.box.scrollTop;
  }

  /**
   * A SIZE CHANGE WITH NOTHING TO ANCHOR ON is the anchoring failing: the
   * reader is off the tail, the box scrolls, the feed draws rows, and not one
   * of them draws a box to hold still. Reported once per spell.
   */
  private reportMissingAnchor(trigger: AnchorTrigger): void {
    if (this.anchor !== null || this.rows === null) {
      this.missingReported = false;
      return;
    }
    const scrolls = this.box.scrollHeight > this.box.clientHeight;
    const drawn = this.rows.host.childElementCount;
    if (trigger !== "resize" || !scrolls || drawn === 0 || this.missingReported) return;
    this.missingReported = true;
    log.error("a height changed while the reader is off the tail, and no feed row can anchor the view", {
      operation: "scroll.anchor-missing",
      context: {
        trigger,
        rows: drawn,
        scroll_top: this.box.scrollTop,
        scroll_height: this.box.scrollHeight,
        client_height: this.box.clientHeight,
      },
    });
  }

  /**
   * Latch the follow, moving nothing, when the reader can see the latest entry
   * and no follow already stands -- unless an active reply selection holds it
   * off, which is reported once each time it starts to.
   */
  private latchIfLatestVisible(): void {
    if (this.following) return;
    const latest = this.readLatest();
    const visible = latest !== null && latestEntryVisible(latest);
    if (!visible) {
      this.heldOffReported = false;
      return;
    }
    if (this.selectionActive) {
      if (this.heldOffReported) return;
      this.heldOffReported = true;
      log.debug("the latest entry is visible, but an active reply selection holds the follow off", {
        operation: "scroll.follow-held-by-selection",
        context: { at: this.box.scrollTop },
      });
      return;
    }
    this.following = true;
    this.cause = "latestVisible";
    this.anchor = null;
    // The input that brought the entry into view has been spent on this latch.
    this.touched = false;
    this.lastTop = this.box.scrollTop;
    log.debug("the reader can see the latest entry; the follow starts where the view stands", {
      operation: "scroll.follow-started",
      context: { cause: "latestVisible", at: this.lastTop },
    });
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
   * the follow unless it went down to the tail; nothing here ever starts one
   * (the latest-visible latch is decided after it, by its callers).
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
 * How far the box must move to CENTER NODE in its viewport — every centering
 * reveal (`TailFollow.centerReveal`) as an arithmetic. First ruled for the
 * detached-work selection (owner ruling, 2026-09-23: the item the reader picked
 * in the footer lands in the middle of their view of the feed, where it used to
 * land at the fold's edge).
 *
 * - a card that fits is placed with its vertical MIDPOINT on the viewport's;
 * - a card TALLER than the viewport lands with its own top at the viewport's
 *   top, so its head is what the reader sees;
 * - either way the move is CLAMPED at the feed's edges: a card near the start
 *   cannot pull the feed above its first row, one near the end cannot push it
 *   past the last reachable position, and there it lands as near center as
 *   the feed allows.
 *
 * The rects are viewport coordinates (see `RevealGeometry`); BOX supplies the
 * scroll range the clamp needs. Positive is downward, matching `scrollTop`.
 */
export function revealCenterDelta(g: RevealGeometry, box: ScrollPosition): number {
  const offset = g.nodeHeight > g.boxHeight ? g.nodeTop - g.boxTop : midpointOffset(g);
  return clampedDelta(box, offset);
}

/**
 * How far the box must move to put an EXPANDED item's vertical middle on the
 * viewport's vertical middle (owner request, 2026-09-30), whatever the item's
 * height, clamped at the feed's edges exactly as `revealCenterDelta` is.
 */
export function expandCenterDelta(g: RevealGeometry, box: ScrollPosition): number {
  return clampedDelta(box, midpointOffset(g));
}

/** How far NODE's vertical midpoint sits below the viewport's. */
function midpointOffset(g: RevealGeometry): number {
  return g.nodeTop + g.nodeHeight / 2 - (g.boxTop + g.boxHeight / 2);
}

/** The move by OFFSET, clamped so the box stays inside its scroll range. */
function clampedDelta(box: ScrollPosition, offset: number): number {
  const maxScrollTop = Math.max(0, box.scrollHeight - box.clientHeight);
  const target = Math.min(Math.max(box.scrollTop + offset, 0), maxScrollTop);
  return target - box.scrollTop;
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
 * True when a scroll box at M can still move in DELTA_Y's direction: down while
 * content remains below its viewport, up while it is scrolled off the top.
 */
export function movesToward(m: ScrollMetrics & { scrollTop: number }, deltaY: number): boolean {
  if (!isScrollBox(m)) return false;
  if (deltaY > 0) return m.scrollTop + m.clientHeight < m.scrollHeight - 1;
  if (deltaY < 0) return m.scrollTop > 0;
  return false;
}

/**
 * True when some scroll box from START up to and including SECTION can still
 * move in DELTA_Y's direction — the boxes a wheel inside SECTION may scroll
 * without leaving it. SECTION must contain START: a section that does not is a
 * caller's broken invariant, and fails loudly rather than reading as "stuck".
 */
export function sectionTakesDelta(start: HTMLElement, section: HTMLElement, deltaY: number): boolean {
  for (let node: HTMLElement | null = start; node !== null; node = node.parentElement) {
    if (movesToward({ ...domMetrics(node), scrollTop: node.scrollTop }, deltaY)) return true;
    if (node === section) return false;
  }
  throw new Error("scroll: sectionTakesDelta was handed a section that does not contain its start");
}

/**
 * The open (expanded) section a wheel at an element lands inside, or null. The
 * feed supplies it from expand.ts (`expandedSectionAt`), which owns what "open"
 * means; this module only contains the wheel.
 */
export type ExpandedSectionAt = (el: HTMLElement) => HTMLElement | null;

/**
 * Arm intent-based inner scrolling on `feed` (the scrollable feed region).
 *
 * A wheel inside an OPEN (expanded) section — `expandedAt` answers which — is
 * contained in it and never moves the feed (see `onWheel`). Otherwise, a
 * wheel over a NON-armed inner scroll box is redirected to the feed; a
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
export function installIntentScroll(
  feed: HTMLElement,
  expandedAt: ExpandedSectionAt,
): { uninstall: () => void; arm: (el: Element) => void } {
  const scrollerUnder = (target: EventTarget | null): HTMLElement | null =>
    innerScrollerAt(target instanceof HTMLElement ? target : null, feed, domMetrics);

  // The armed box: the inner scroll box the reader last deliberately entered.
  // Written ONLY from pointer events below and from `arm` — never from the
  // wheel, and never from mouseenter/mouseover — which is what keeps a
  // scrolled-into box unarmed.
  let armed: HTMLElement | null = null;

  const onWheel = (e: WheelEvent): void => {
    // AN OPEN SECTION KEEPS ITS WHEEL, ALL OF IT (owner ruling, 2026-09-27).
    // A wheel inside an expanded box is that box's, armed or not, and never
    // reaches the feed: while a box inside the section can still move, the
    // browser scrolls it (and `overscroll-behavior: contain` on the open box
    // keeps a gesture's overshoot from chaining out); once none can — the box
    // at its top or bottom edge, or short enough not to scroll at all — the
    // wheel is consumed here, so there is nothing left for the feed to take.
    const target = e.target instanceof HTMLElement ? e.target : null;
    const open = target === null ? null : expandedAt(target);
    if (target !== null && open !== null) {
      if (e.deltaY !== 0 && !sectionTakesDelta(target, open, e.deltaY)) e.preventDefault();
      return;
    }
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
