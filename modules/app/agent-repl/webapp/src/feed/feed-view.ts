/**
 * feed-view — the FeedController: ONE feed's rows, its DOM, its page walk.
 *
 * There is one of these per OPEN feed — the root feed while the view is up,
 * and one per expanded bubble — and they are the same class, because a bubble
 * IS a feed. The only thing a caller varies is which body renderer lays the
 * rows out and which head a bubble row wears.
 *
 * WHAT IT OWNS
 *  - THE ORDER, keyed by `FeedId.value`. A page arrives oldest → newest; a tail
 *    push with a known id REPLACES that row in place (which is how a response
 *    grows and how a settled card lands), and an unknown id APPENDS. Nothing is
 *    accumulated across pushes — a row is replaced whole, never merged.
 *  - THE ELEMENTS. Each row gets one `<article>` of chrome that outlives its
 *    body: the body is redrawn on each push while the chrome, the nesting slot
 *    and (for a bubble) the open sub-feed inside it survive.
 *  - THE WALK. `has_more` shows the load-more control, `at_start` hides it, and
 *    a prepend is anchored so the reader is not moved by content arriving above
 *    them.
 *
 * WHAT IT DOES NOT OWN. It draws no card itself beyond the four row kinds the
 * feed core is responsible for; every other arm goes to an injected renderer,
 * and a bubble's expansion is bubble.ts's.
 *
 * A MALFORMED ROW COSTS THAT ROW. The renderer's refusal is caught per row: the
 * row draws as a placeholder naming the path, the failure is reported once, and
 * the rest of the feed goes on working — a feed that blanks itself because one
 * card was unreadable would lose the reader everything, including the evidence.
 */
import { log } from "../log.js";
import { SELECTED_RESPONSE_ATTRIBUTE, syncSelectedEntry } from "./selected-entry.js";
import {
  applyExpanded,
  cappedSectionsOf,
  carryExpanded,
  retainRows,
  snapshotExpanded,
} from "../expand.js";
import { MalformedView, isMalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { frameUndecodable } from "../failure/sink.js";
import { TOPBAR_TONES, toneClass, type Color } from "../vocab.js";
import type { ScrollPosition, TailFollow } from "../scroll.js";
import type { AppContext } from "../rpc/context.js";
import { clone, equals } from "@bufbuild/protobuf";
import {
  FeedRowSchema,
  type FeedBreadcrumb,
  type FeedId,
  type FeedPage,
  type FeedPageError,
  type FeedResponse,
  type FeedSelection,
  type FeedTurnActivity,
  type FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  GetFeedPageResponseSchema,
  type GetFeedPageResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_feed_page_pb";
import { buildGetFeedPageRequest } from "./requests.js";
import { armName } from "./renderers.js";
import type {
  BubbleBodyRenderer,
  Handle,
  RowContext,
  RowRenderers,
  SubfeedView,
} from "./renderers.js";
import { replaceTicking, stopClocks, stopTicking } from "./ticking.js";
import { keepScrolled } from "./keep-scroll.js";
import type { Overscan } from "./overscan.js";
import { thinkingLanded } from "./cards/response.js";
import { drawFeedUserPrompt } from "./rows/user-prompt.js";
import { drawFeedAgentPrompt } from "./rows/agent-prompt.js";
import { drawFeedPeerMessage } from "./rows/peer-message.js";
import { drawFeedTurnEnded } from "./rows/turn-ended.js";
import {
  drawFeedSessionSeparation,
  separationBoundsFeed,
} from "./rows/separation.js";
import { drawFeedMergeTabRow } from "./merge/tab-row.js";
import { isOwnTurn } from "../composer/own-turns.js";
import { setPromptWave } from "../breathing.js";

/** A collapsing row's place, sampled before the redraw that collapses it. */
interface CollapseSample {
  id: string;
  element: HTMLElement;
  /** The scroll box's top edge. */
  boxTop: number;
  /** The row's bottom edge before the redraw. */
  bottom: number;
}

/**
 * Whether NEXT is the daemon's re-push of a thinking row whose own final text
 * has just LANDED (`thinkingLanded`, cards/response.ts): the row was drawn still
 * arriving and now arrives in a terminal arm, so its redraw collapses it to the
 * thinking cap. Read off the two pushed rows of the SAME row; no other row is
 * consulted.
 */
function isLandingEdge(drawn: FeedRow, next: FeedRow): boolean {
  const before = responseOf(drawn);
  const after = responseOf(next);
  return before !== null && after !== null && !thinkingLanded(before) && thinkingLanded(after);
}

/** The response bubble a row carries, or null. */
function responseOf(row: FeedRow): FeedResponse | null {
  if (row.row.case !== "activity" || row.row.value.unit.case !== "response") return null;
  return row.row.value.unit.value;
}

/** A bubble, as the controller holds it: an element plus its own lifecycle. */
export interface BubbleLike extends Handle {
  /** The element that goes in the row's body slot. */
  readonly element: HTMLElement;
  /** Redraw the head from a re-pushed row, leaving the fold alone. */
  update(row: FeedRow): void;
  /** Open the sub-feed if it is not open. Answers whether it is open now. */
  expand(): Promise<boolean>;
  /** Whether the sub-feed is open right now. */
  isExpanded(): boolean;
  /** The sub-feed's controller, once it has been opened at least once. */
  child(): FeedController | null;
}

/** Builds the bubble for a bubble-shaped row. Injected, to avoid a cycle. */
export type BubbleFactory = (row: FeedRow, rc: RowContext) => BubbleLike;

export interface FeedControllerOptions {
  ctx: AppContext;
  /** The element this feed draws into. */
  host: HTMLElement;
  /** This feed's address: the root feed, or the bubble row's own id. */
  feed: FeedId | "root";
  renderers: RowRenderers;
  /** How the rows are laid out — the default body, or the merge tab strip. */
  body: BubbleBodyRenderer;
  readonly revealRow: (id: FeedId) => Promise<boolean>;
  bubble: BubbleFactory;
  /**
   * The row context the BODY renderer is handed. A bubble passes its own
   * bubble row's context; the root feed passes a context standing for the feed
   * itself, which is not a row.
   */
  bodyContext: RowContext;
  /** The bubble's own composer slot, when this build mounts one (R7). */
  composerSlot?: HTMLElement;
  /**
   * The page's scroll box and tail owner. Root feed only. The box's own rect is
   * read to tell whether a collapsing row lies above the viewport.
   */
  scroll?: { box: ScrollPosition & Pick<Element, "getBoundingClientRect">; tail: TailFollow };
  /**
   * The overscan buffer, rooted on the page's scroll box. One instance is
   * shared across the root feed and every sub-feed nested inside the same box,
   * so a row's chrome is observed the moment it is held and unobserved the
   * moment it is dropped — no matter which feed holds it. Absent where the feed
   * has no scroll box (a fixture) or the environment ships no
   * `IntersectionObserver`.
   */
  overscan?: Overscan;
}

/** One row, as the controller holds it. */
interface RowState {
  row: FeedRow;
  element: HTMLElement;
  body: HTMLElement | null;
  bubble: BubbleLike | null;
  /** Whether the message changed since the body was last drawn. */
  dirty: boolean;
}

export interface FeedController extends Handle {
  readonly element: HTMLElement;
  /** Paint a page: the newest page from OpenFeed, or an older one prepended. */
  applyPage(page: FeedPage, placement: "replace" | "prepend"): void;
  /** One live upsert. */
  upsert(row: FeedRow): void;
  /**
   * Apply the daemon's reply-to-a-past-response selection state: recolor the
   * selected final-response bubble, center-scroll it, and suppress tail-follow
   * while a selection is active — returning to the bottom when it clears.
   */
  applySelection(selection: FeedSelection): void;
  /** Whether the daemon's last pushed reply selection is active. */
  selectionActive(): boolean;
  rows(): readonly FeedRow[];
  breadcrumbs(): readonly FeedBreadcrumb[];
  onChange(fn: () => void): () => void;
  drawRow(row: FeedRow): HTMLElement;
  findRowElement(id: FeedId): HTMLElement | null;
  /** The bubbles on this feed, for the reveal walk. */
  bubbles(): readonly BubbleLike[];
  /** The view a body renderer draws from. */
  view(): SubfeedView;
}


/** Build a controller for ONE feed and draw its shell into the host. */
export function createFeedController(opts: FeedControllerOptions): FeedController {
  const order: string[] = [];
  const states = new Map<string, RowState>();
  const listeners = new Set<() => void>();
  let crumbs: readonly FeedBreadcrumb[] = [];
  let disposed = false;
  // THE READER'S FOLDS ACROSS A PAGE REPLACE. A replace rebuilds every row from
  // scratch, so `carryExpanded`'s previous element is gone; this is the
  // per-row-id snapshot taken just before the teardown and spent on each row's
  // first draw after it (see `applyPage` and `carryForRow`).
  let carriedFolds = new Map<string, string[]>();
  // WHETHER THIS FEED HAS PAINTED A PAGE YET. Its first replace is the feed's
  // initial PLACEMENT; every later one is a re-open (`replaceRestore`).
  let placed = false;
  // THE READER'S REPLY SELECTION, as last applied: whether one stood and which
  // row it centered, so a re-push of the same state moves nothing.
  let selectionActive = false;
  let centeredOn: string | null = null;

  opts.host.setAttribute("data-feed", opts.feed === "root" ? "root" : opts.feed.value);

  const loadMore = document.createElement("button");
  loadMore.type = "button";
  loadMore.className = "feed-load-more";
  loadMore.setAttribute("data-load-more", "");
  loadMore.textContent = "older";

  const errorSlot = document.createElement("div");
  errorSlot.className = "feed-page-error-slot";

  const bodyMount = document.createElement("div");
  bodyMount.className = "feed-body";

  // THE WALK CONTROL IS PRESENT ONLY WHEN THERE IS A WALK. A feed that has
  // reached its start offers no way back further, and an inert control the
  // reader can see is a promise the feed cannot keep — so it is ATTACHED on
  // `has_more` and detached otherwise, never merely hidden.
  opts.host.append(errorSlot, bodyMount);

  const controller: FeedController = {
    element: opts.host,
    applyPage,
    upsert,
    applySelection,
    selectionActive: () => selectionActive,
    rows,
    breadcrumbs: () => crumbs,
    onChange,
    drawRow,
    findRowElement,
    bubbles,
    view: () => subfeed,
    dispose,
  };

  const rowContextFor = (row: FeedRow, previous?: HTMLElement): RowContext => ({
    ctx: opts.ctx,
    feed: opts.feed,
    row,
    revealRow: opts.revealRow,
    previous,
    findRowElement,
  });

  const subfeed: SubfeedView = {
    rows,
    onChange,
    drawRow,
    breadcrumbs: () => crumbs,
    composerSlot: opts.composerSlot,
  };

  loadMore.addEventListener("click", () => {
    void loadOlder();
  });

  const bodyHandle = opts.body(bodyMount, subfeed, opts.bodyContext);

  return controller;

  // ---- the row store ----------------------------------------------------

  function rows(): readonly FeedRow[] {
    return order.map((id) => {
      const state = states.get(id);
      if (state === undefined) throw new Error(`feed: row ${id} is ordered but not held`);
      return state.row;
    });
  }

  function onChange(fn: () => void): () => void {
    listeners.add(fn);
    return () => {
      listeners.delete(fn);
    };
  }

  function announce(): void {
    for (const fn of [...listeners]) fn();
    markLatestPrompt();
    stopEndedTurns();
    followTail();
  }

  /**
   * THE BACKSTOP: every timer in a turn stops when the turn ends.
   *
   * A card is meant to stop its own clock the moment its unit settles, and a
   * replace site is meant to stop the element it discards. This runs AFTER the
   * listeners have redrawn, and sweeps whatever those two missed: once a turn
   * has a `turn_ended` row on the page, nothing inside that turn's rows is
   * still counting, because nothing in a finished turn can still be running.
   *
   * THE TURN'S OWN END ROW IS EXEMPT. Its retry countdown is a clock about
   * what happens NEXT, not about the work that just stopped, and it stops
   * itself when it expires.
   *
   * A stop here is a DEFECT SIGNAL, not routine housekeeping — the card that
   * needed it failed the first rule — so it is recorded with the rows it
   * stopped, which is what makes that card findable afterwards.
   */
  function stopEndedTurns(): void {
    const ended = new Set<string>();
    for (const state of states.values()) {
      if (state.row.row.case !== "turnEnded") continue;
      const turn = state.row.turn;
      if (turn !== undefined) ended.add(turn.value);
    }
    if (ended.size === 0) return;
    const stopped: string[] = [];
    for (const state of states.values()) {
      if (state.row.row.case === "turnEnded") continue;
      const turn = state.row.turn;
      if (turn === undefined || !ended.has(turn.value)) continue;
      // CLOCKS ONLY: the row stays on screen, so its measurers (a bubble's or a
      // title's "more below", bubble-more.ts) keep measuring.
      if (stopClocks(state.element) === 0) continue;
      stopped.push(requireMessage(state.row.id, "FeedRow.id").value);
    }
    if (stopped.length === 0) return;
    log.debug("a turn ended with clocks still running; the backstop stopped them", {
      operation: "feed.turn-end-stopped-clocks",
      context: { feed: feedName(), turns: [...ended].join(","), rows: stopped.join(",") },
    });
  }

  /**
   * Paint a page.
   *
   * REPLACE is the newest page (an open, or a re-open on re-expand): the feed's
   * rows become this page's rows, and the feed then parks at its tail
   * (`placeAfterReplace`). PREPEND is the walk into the past: older rows land
   * above what is already there, and the view shifts by exactly the height that
   * grew above it, so the reader stays on what they were reading
   * (`keepPlaceAbovePrepend`).
   */
  function applyPage(page: FeedPage, placement: "replace" | "prepend"): void {
    const result = requireCase(page.result, "FeedPage.result");
    log.debug(`applying a ${placement} page as ${result.case}`, {
      operation: "feed.apply-page",
      context: { feed: feedName(), placement, arm: result.case },
    });
    switch (result.case) {
      case "success": {
        replaceTicking(errorSlot);
        const edge = requireCase(result.value.edge, "FeedPageSuccess.edge");
        if (edge.case === "hasMore") opts.host.prepend(loadMore);
        else loadMore.remove();
        crumbs = requireMessage(result.value.breadcrumbs, "FeedPageSuccess.breadcrumbs").crumbs;
        const above = placement === "prepend" ? sampleFirstRow() : null;
        if (placement === "replace") {
          // OWNER RULING (2026-09-18): A REDRAW NEVER UN-TOGGLES, WHATEVER ITS
          // SHAPE. A fold the reader opened survives a full page replace
          // exactly as it survives a single-card upsert, so the expansions are
          // snapshotted by row id here, across the teardown.
          carriedFolds = snapshotExpanded(
            [...states].flatMap(([id, state]) =>
              state.body === null ? [] : [[id, state.body] as [string, HTMLElement]],
            ),
          );
          clearRows();
        }
        const incoming = result.value.rows;
        for (let i = 0; i < incoming.length; i += 1) {
          adopt(incoming[i], placement === "prepend" ? i : order.length);
        }
        // A ROW THE REPLACE DID NOT SERVE AGAIN IS GONE: its keys drop rather
        // than linger for a row that will never be drawn.
        if (placement === "replace") retainRows(carriedFolds, new Set(states.keys()));
        announce();
        if (placement === "replace") placeAfterReplace();
        else keepPlaceAbovePrepend(above);
        return;
      }
      case "error":
        drawPageError(result.value);
        announce();
        return;
      default:
        unreachableArm("FeedPage.result", armName(result));
    }
  }

  /**
   * The page could not be completed: drawn WHERE THE ROWS WOULD BE, with the
   * daemon's own sentence and the typed evidence, so a reader is never shown a
   * feed with a silent hole in it.
   */
  function drawPageError(error: FeedPageError): void {
    const headline = requireMessage(error.headline, "FeedPageError.headline");
    const kind = requireCase(error.kind, "FeedPageError.kind");
    const el = document.createElement("div");
    el.className = `feed-page-error ${headlineToneClass(headline.tone)}`;
    el.setAttribute("data-page-error", kind.case);

    const text = document.createElement("div");
    text.className = "feed-page-error-headline";
    text.textContent = headline.text;
    el.append(text);

    switch (kind.case) {
      case "historyReplayTruncated": {
        const evidence = document.createElement("div");
        evidence.className = "feed-page-error-evidence";
        evidence.textContent = kind.value.reason;
        el.append(evidence);
        break;
      }
      default:
        unreachableArm("FeedPageError.kind", armName(kind));
    }
    log.error(`a feed page could not be served: ${headline.text}`, {
      operation: "feed.page-error",
      context: { feed: feedName(), arm: kind.case },
    });
    replaceTicking(errorSlot, [el]);
  }

  /** One live upsert: replace in place if seen, append if new. */
  function upsert(row: FeedRow): void {
    const id = requireMessage(row.id, "FeedRow.id").value;
    // A REMOVAL is the DUAL of an upsert, delivered on the same tail: the
    // daemon retired the row it keys, so drop it live rather than replace it.
    if (row.row.case === "removed") {
      remove(id);
      announce();
      return;
    }
    const held = states.get(id);
    // A RE-PUSH OF THE ROW EXACTLY AS DRAWN CHANGES NOTHING, so it draws
    // nothing. The daemon repaints its opening page on every turn open (up to
    // 200 rows re-pushed unchanged); redrawing each one rebuilt its element and
    // could move the content under a reader (owner rule, 2026-09-23: the user
    // owns the scroll).
    if (held !== undefined && equals(FeedRowSchema, held.row, row)) {
      log.debug(`feed row ${id} was re-pushed unchanged; nothing is redrawn`, {
        operation: "feed.row-unchanged",
        context: { feed: feedName(), row: id },
      });
      return;
    }
    const known = held !== undefined;
    log.debug(`${known ? "replacing" : "appending"} feed row ${id}`, {
      operation: known ? "feed.row-replaced" : "feed.row-appended",
      context: { feed: feedName(), row: id, kind: row.row.case ?? "unset" },
    });
    const collapsing = held !== undefined && isLandingEdge(held.row, row) ? sampleCollapse(id, held) : null;
    adopt(row, known ? -1 : order.length);
    truncateAtSeparation(row, id);
    announce();
    keepPlaceAboveCollapse(collapsing);
    if (!known && isSentPrompt(row)) parkOnSentPrompt(id);
  }

  /**
   * Sample where a thinking row that just landed ends, BEFORE its
   * redraw collapses it, with the scroll box's top edge. Null when the feed has
   * no scroll box (a sub-feed; the same standing a prepend has there).
   */
  function sampleCollapse(id: string, held: RowState): CollapseSample | null {
    if (opts.scroll === undefined) return null;
    return {
      id,
      element: held.element,
      boxTop: opts.scroll.box.getBoundingClientRect().top,
      bottom: held.element.getBoundingClientRect().bottom,
    };
  }

  /**
   * KEEP THE READER'S CONTENT IN PLACE WHEN A THINKING BUBBLE ABOVE THEM
   * COLLAPSES (owner rule, 2026-09-23). The daemon re-pushes a thinking row in
   * a terminal arm once its own final text has arrived, and its redraw drops it
   * from the response cap to the one-line thinking cap. When it lay wholly above the viewport, everything
   * the reader sees moved up by exactly the height it lost, so the view shifts
   * by that, through the tail owner (`collapseCompensation`); a following reader
   * was already kept at the tail by `announce`. The row the redraw DETACHED is an
   * invariant violation (an upsert redraws a row in place), recorded as one.
   */
  function keepPlaceAboveCollapse(sample: CollapseSample | null): void {
    if (opts.scroll === undefined || sample === null) return;
    if (!sample.element.isConnected) {
      log.error("a landed thinking row's redraw detached the row its collapse was measured from", {
        operation: "feed.collapse-anchor-detached",
        context: { feed: feedName(), row: sample.id },
      });
      return;
    }
    const after = sample.element.getBoundingClientRect().bottom;
    log.debug(`a landed thinking row above ${sample.boxTop}px changed by ${after - sample.bottom}px`, {
      operation: "feed.collapse-kept-place",
      context: { feed: feedName(), row: sample.id, box_top: sample.boxTop, before: sample.bottom, after },
    });
    opts.scroll.tail.collapseCompensation({
      boxTop: sample.boxTop,
      rowBottomBefore: sample.bottom,
      rowBottomAfter: after,
    });
  }

  /**
   * THE FEED LANDS AT ITS TAIL AFTER A REPLACE.
   *
   * A feed's FIRST page is its initial placement (`initialPlacement`); a later
   * replace -- a re-open after reconnect, stream recovery or daemon handover --
   * lands at the tail too (`replaceRestore`, owner ruling 2026-09-23).
   * Regression (fix/feed-scroll-anchor-and-prompt-park): a replace used to
   * restore an anchor by an attribute no row carries, leaving the reader at the
   * TOP. Root feed only: a sub-feed has no scroll box of its own.
   */
  function placeAfterReplace(): void {
    const first = !placed;
    placed = true;
    if (opts.scroll === undefined) return;
    log.debug(`a ${first ? "first" : "replaced"} page landed the feed at its tail`, {
      operation: "feed.replace-parked",
      context: { feed: feedName(), rows: order.length, first },
    });
    if (first) opts.scroll.tail.initialPlacement();
    else opts.scroll.tail.replaceRestore();
  }

  /**
   * A PROMPT JUST SENT PUTS THE READER AT THE TAIL (owner ruling, 2026-09-23).
   *
   * Called only after the prompt's bubble has been painted, and only on its
   * FIRST LIVE placement (see `isSentPrompt`), so a redraw of a held prompt, a
   * replace and a history prepend never re-park. It parks at the feed's TAIL
   * rather than on the prompt: whatever was drawn after the prompt is where
   * the feed lands, and `park` latches the follow so the turn's output keeps
   * autoscrolling. Root feed only, like every other move of the scroll box.
   */
  function parkOnSentPrompt(id: string): void {
    if (opts.scroll === undefined) return;
    log.debug(`a newly sent prompt ${id} parked the feed at its tail`, {
      operation: "feed.sent-prompt-parked",
      context: { feed: feedName(), row: id },
    });
    opts.scroll.tail.promptSent();
  }

  /**
   * Sample the first row already drawn and where its top sits, BEFORE a prepend
   * lands rows above it. Null when the feed draws no row yet.
   */
  function sampleFirstRow(): { id: string; element: HTMLElement; top: number } | null {
    const id = order[0];
    if (id === undefined) return null;
    const state = states.get(id);
    if (state === undefined) return null;
    return { id, element: state.element, top: state.element.getBoundingClientRect().top };
  }

  /**
   * KEEP THE READER'S CONTENT IN PLACE WHEN OLDER ROWS LAND ABOVE IT.
   *
   * Everything a prepend adds sits above every row the reader could be looking
   * at, so the height that grew above the viewport is exactly how far the row
   * that USED to be first moved down; the view shifts by that, through the tail
   * owner (`prependCompensation`). A reader who is following has already been
   * kept at the tail by `announce`, so the growth is not applied on top of it.
   *
   * A first row the prepend DETACHED is an invariant violation (a prepend only
   * ever adds rows above), and is recorded as one rather than guessed around.
   */
  function keepPlaceAbovePrepend(above: { id: string; element: HTMLElement; top: number } | null): void {
    if (opts.scroll === undefined || above === null) return;
    if (!above.element.isConnected) {
      log.error("a prepend detached the row the reader's place was measured from", {
        operation: "feed.prepend-anchor-detached",
        context: { feed: feedName(), row: above.id },
      });
      return;
    }
    const grown = above.element.getBoundingClientRect().top - above.top;
    log.debug(`a prepend grew ${grown}px above the reader; the view shifts by it`, {
      operation: "feed.prepend-kept-place",
      context: { feed: feedName(), row: above.id, grown },
    });
    opts.scroll.tail.prependCompensation(grown);
  }

  /**
   * Apply the daemon's reply-to-a-past-response selection state.
   *
   * THREE EFFECTS, in the order the reader experiences them:
   *  - the BLUE border moves to the selected final-response bubble and off
   *    every other row's -- a re-push (C-p/C-n) recolors one row and clears the
   *    rest, and a cleared selection (`selected` unset) clears them all;
   *  - while a selection is ACTIVE the follow is released, so new rows append
   *    without pulling the viewport off the centered selection. When the
   *    selection CLEARS the feed returns to its tail (`selectionCleared`);
   *  - the `center` row is scrolled to the middle of the viewport, clamped at
   *    the feed edges (`selectionMoved`).
   *
   * ONLY A CHANGE MOVES THE FEED (owner rule, 2026-09-23: the user owns the
   * scroll). The selection is the reader's keybinding act, so a push that
   * restates the state already applied -- an inactive selection re-sent on a
   * re-open, the same center pushed again -- recolors and moves nothing.
   */
  function applySelection(selection: FeedSelection): void {
    markSelectedResponse(selection.selected);
    const wasActive = selectionActive;
    selectionActive = selection.active;
    if (!selection.active) {
      centeredOn = null;
      if (!wasActive) {
        log.debug("an inactive selection was restated; the feed stays", {
          operation: "feed.selection-unchanged",
          context: { feed: feedName(), active: false },
        });
        return;
      }
      opts.scroll?.tail.selectionCleared();
      return;
    }
    const center = selection.center?.value ?? null;
    if (wasActive && center === centeredOn) {
      log.debug("the selection restated its center; the feed stays", {
        operation: "feed.selection-unchanged",
        context: { feed: feedName(), active: true, center: center ?? "none" },
      });
      return;
    }
    centeredOn = center;
    centerOnRow(selection.center);
  }

  /**
   * Put the BLUE selected-response mark on the named row and strip it from
   * every other row of this feed. An unset id clears the mark everywhere, which
   * is the cleared-selection case. A named row this feed has not drawn (a page
   * not walked back to) is reported and nothing is marked — the same tolerance
   * `markFinalAnswer` has, since guessing which row to recolor is worse than
   * none.
   */
  function markSelectedResponse(selected: FeedId | undefined): void {
    const target = selected === undefined ? null : findRowElement(selected);
    if (selected !== undefined && target === null) {
      log.debug("the selection names a row this feed has not drawn", {
        operation: "feed.selection-row-absent",
        context: { feed: feedName(), row: selected.value },
      });
    }
    for (const state of states.values()) {
      const chosen = state.element === target;
      if (chosen) state.element.setAttribute(SELECTED_RESPONSE_ATTRIBUTE, "true");
      else state.element.removeAttribute(SELECTED_RESPONSE_ATTRIBUTE);
      // The mark goes on the row's CARD, the same bubble the green
      // `.final-response` rule reads, so the blue replaces the green there.
      syncSelectedEntry(state.element);
    }
  }

  /**
   * Scroll the box so the CENTER row sits in the middle of the viewport,
   * clamped at the feed's edges (`centerDelta`), read in the box's own scroll
   * coordinates through `offsetTop`. A row this feed has not drawn (or no
   * center at all) centers nothing, but the follow is still released.
   */
  function centerOnRow(center: FeedId | undefined): void {
    if (opts.scroll === undefined) return;
    const node = center === undefined ? null : findRowElement(center);
    if (center !== undefined && node === null) {
      log.debug("the selection centers a row this feed has not drawn", {
        operation: "feed.selection-center-absent",
        context: { feed: feedName(), row: center.value },
      });
    }
    const box = opts.scroll.box;
    opts.scroll.tail.selectionMoved(
      node === null
        ? null
        : {
            nodeOffsetTop: node.offsetTop,
            nodeHeight: node.offsetHeight,
            clientHeight: box.clientHeight,
            scrollHeight: box.scrollHeight,
            scrollTop: box.scrollTop,
          },
    );
  }

  /**
   * Drop one row on a live removal: dispose its bubble, stop its clocks,
   * detach its element, and forget it — the same teardown a row dropped above
   * a separation gets, so nothing keeps ticking or leaks after the daemon
   * retired the row. A removal for a row this feed never held is a no-op.
   */
  function remove(id: string): void {
    const at = order.indexOf(id);
    if (at < 0) {
      log.debug(`a removal named a row this feed does not hold: ${id}`, {
        operation: "feed.row-removed-absent",
        context: { feed: feedName(), row: id },
      });
      return;
    }
    const state = states.get(id);
    if (state !== undefined) {
      state.bubble?.dispose();
      stopTicking(state.element);
      opts.overscan?.unobserve(state.element);
      state.element.remove();
      states.delete(id);
    }
    order.splice(at, 1);
    log.info(`removed feed row ${id}`, {
      operation: "feed.row-removed",
      context: { feed: feedName(), row: id },
    });
  }

  /**
   * THE FEED BEGINS AT THE NEWEST SEPARATION, on this side too.
   *
   * A compaction or a clear arriving on the tail says the rows above it are no
   * longer the conversation — the daemon stops serving them from this moment,
   * so a client that kept them on screen would be the only place they still
   * existed, and after a second compaction the reader would be looking at two
   * dividers and a superseded summary.
   *
   * The divider itself STAYS, and for a compaction so does the summary it
   * carries: that summary is the whole of what survived, and it lives on this
   * row rather than above it.
   */
  function truncateAtSeparation(row: FeedRow, id: string): void {
    if (!separationBoundsFeed(row)) return;
    const at = order.indexOf(id);
    if (at <= 0) return;
    const dropped = order.splice(0, at);
    for (const gone of dropped) {
      const state = states.get(gone);
      if (state === undefined) continue;
      state.bubble?.dispose();
      stopTicking(state.element);
      opts.overscan?.unobserve(state.element);
      state.element.remove();
      states.delete(gone);
    }
    log.info(`dropped ${dropped.length.toString()} rows above the newest separation`, {
      operation: "feed.truncated-at-separation",
      context: { feed: feedName(), row: id, dropped: dropped.length },
    });
  }

  /**
   * Put ROW in the store at INDEX (-1 = keep its place).
   *
   * A row already held keeps its element and its place and is only marked
   * dirty — that is what makes an upsert a REPLACEMENT of the drawing rather
   * than a rebuild of the row, and what lets an open bubble survive a re-push
   * of its own head.
   */
  function adopt(row: FeedRow, index: number): void {
    const id = requireMessage(row.id, "FeedRow.id").value;
    const held = states.get(id);
    if (held !== undefined && movePromptWaveInPlace(held, row)) return;
    if (held !== undefined) {
      held.row = row;
      held.dirty = true;
      applyRowAttributes(held.element, row);
      return;
    }
    const element = document.createElement("article");
    element.className = "feed-item";
    applyRowAttributes(element, row);
    const state: RowState = { row, element, body: null, bubble: null, dirty: true };
    states.set(id, state);
    order.splice(index < 0 ? order.length : index, 0, id);
    // WATCH THE NEW ROW so the overscan buffer can pre-render it before the
    // reader reaches it. Every row born on this feed passes here exactly once,
    // bubble or not, so this is the one place a row starts being watched.
    opts.overscan?.observe(element);
    if (!isBubbleRow(row)) return;
    // A bubble owns its own element for its whole life: the head redraws inside
    // it and the sub-feed hangs beneath it, so the row's body is never replaced.
    const bubble = opts.bubble(row, rowContextFor(row));
    state.bubble = bubble;
    state.body = bubble.element;
    state.dirty = false;
    carryForRow(id, bubble.element);
    element.append(bubble.element);
    mirrorState(state);
  }

  /** Drop every row: the feed is being repainted from a fresh newest page. */
  function clearRows(): void {
    for (const state of states.values()) {
      state.bubble?.dispose();
      stopTicking(state.element);
      opts.overscan?.unobserve(state.element);
      state.element.remove();
    }
    states.clear();
    order.length = 0;
  }

  // ---- drawing ----------------------------------------------------------

  /**
   * The element for ROW, built once and updated only when the row changed.
   *
   * Reuse is what preserves everything the reader did inside a row — an open
   * fold, an expanded output, a bubble's own sub-feed — across pushes that
   * touched some OTHER row.
   */
  function drawRow(row: FeedRow): HTMLElement {
    const id = requireMessage(row.id, "FeedRow.id").value;
    const state = states.get(id);
    if (state === undefined) throw new Error(`feed: asked to draw unheld row ${id}`);
    if (!state.dirty) return state.element;
    state.dirty = false;
    drawBody(state);
    return state.element;
  }

  /**
   * Re-open the folds the reader had open on this row before a page replace.
   *
   * Spent once: the snapshot describes the page that was torn down, and a row's
   * later redraws carry their own state forward through `carryExpanded`.
   */
  function carryForRow(rowId: string, body: HTMLElement): void {
    const keys = carriedFolds.get(rowId);
    if (keys === undefined) return;
    carriedFolds.delete(rowId);
    applyExpanded(cappedSectionsOf(body), keys);
  }

  /** Draw (or redraw) one row's body inside its chrome. */
  function drawBody(state: RowState): void {
    const previous = state.body ?? undefined;
    if (state.bubble !== null) {
      // A bubble redraws its own head and keeps its sub-feed; replacing the
      // element here would tear down an open bubble on every push.
      state.bubble.update(state.row);
      mirrorState(state);
      return;
    }
    let body: HTMLElement;
    try {
      body = drawRowBody(state.row, rowContextFor(state.row, previous));
    } catch (err) {
      if (!isMalformedView(err)) throw err;
      body = malformedPlaceholder(err, state.row);
    }
    if (previous === undefined) {
      carryForRow(requireMessage(state.row.id, "FeedRow.id").value, body);
    }
    if (body === previous) {
      // THE RENDERER UPDATED ITS OWN ELEMENT IN PLACE (the response bubble
      // does): nothing is replaced, so the reader's folds, the scroll position
      // inside the bubble and the content under it all stay where they are.
      mirrorState(state);
      return;
    }
    if (previous !== undefined) {
      // R2: THE WIRE'S FOLD IS THE INITIAL FOLD. A push says how a section
      // STARTS; after that the reader's own toggle wins, so a re-push of the
      // same row may never re-collapse what they opened. The NAMED folds read
      // their state back off `rc.previous` themselves; the CAPPED sections
      // (`.tool-fold` and its siblings — a whole skill or tool-call card is
      // one) are keyed by class, and this is the one seam that carries them.
      carryExpanded(previous, body);
      // A CARD THE READER IS SCROLLED INSIDE IS MORPHED, NOT REPLACED
      // (keep-scroll.ts): the box they scrolled never leaves the document, so
      // a push cannot throw them back to its top. It runs after the fold
      // carry, so the morph brings `.expanded` along.
      if (keepScrolled(previous, body)) {
        mirrorState(state);
        return;
      }
      stopTicking(previous);
      previous.remove();
    }
    state.body = body;
    state.element.prepend(body);
    mirrorState(state);
    // THE GREEN FINAL-ANSWER TREATMENT IS NO LONGER RE-ASSERTED HERE. It is a
    // DATA property the daemon stamps on the answering response row
    // (`FeedResponse.final_answer`), which cards/response.ts draws from on every
    // (re)draw — so a rebuilt bubble carries the green from its own data and
    // needs no per-redraw re-assertion from the chrome.
  }

  /**
   * THE CARD'S STATE, REPEATED ON THE ROW CHROME.
   *
   * A card carries `data-state` with its own state/outcome arm (preamble §5).
   * The chrome is what a reader — and every query that starts from a row —
   * holds, so the arm is mirrored up onto the `<article>`: one row, one place
   * to ask what state it is in. It is COPIED, never derived: the row chrome
   * knows nothing about which arms exist and states only what the card stated.
   */
  function mirrorState(state: RowState): void {
    mirror(state, "data-state");
    // A bubble says whether it is open on itself; the row is what a reader —
    // and the reveal walk — holds, so it says the same thing.
    mirror(state, "data-expanded");
    // THE SELECTION MARK IS THE ROW'S FACT, DRAWN ON ITS CARD: a card a push
    // replaced inherits it from the row it is drawn in (selected-entry.ts).
    syncSelectedEntry(state.element);
  }

  /** Copy one attribute from the row's body up onto its chrome. */
  function mirror(state: RowState, attribute: string): void {
    const drawn = state.body?.getAttribute(attribute) ?? null;
    if (drawn === null) {
      state.element.removeAttribute(attribute);
      return;
    }
    state.element.setAttribute(attribute, drawn);
  }

  /**
   * The body of one row, by arm.
   *
   * The four kinds the feed core draws itself are here; every other arm is an
   * injected renderer's, and the bubble arms never reach this function at all
   * (they are recognized when the chrome is built).
   */
  function drawRowBody(row: FeedRow, rc: RowContext): HTMLElement {
    const arm = requireCase(row.row, "FeedRow.row");
    switch (arm.case) {
      case "userPrompt":
        return drawFeedUserPrompt(arm.value, rc.previous);
      case "agentPrompt":
        return drawFeedAgentPrompt(arm.value, rc.previous);
      case "peerMessage":
        return drawFeedPeerMessage(arm.value, rc.previous);
      case "turnEnded":
        return drawFeedTurnEnded(arm.value, rc);
      case "separation":
        return drawFeedSessionSeparation(arm.value, rc);
      case "activity":
        return drawActivity(arm.value, rc);
      case "detachedShell":
        return opts.renderers.shell(
          requireMessage(arm.value.shell, "FeedDetachedShell.shell"),
          rc,
        );
      case "permission":
        return opts.renderers.permission(arm.value, rc);
      case "question":
        return opts.renderers.question(arm.value, rc);
      case "coldGate":
        return opts.renderers.coldGate(arm.value, rc);
      case "commandPanel":
        return opts.renderers.commandPanel(arm.value, rc);
      case "commandRefused":
        return opts.renderers.commandRefused(arm.value, rc);
      case "mergeTab":
        // INSIDE a merge bubble the strip consumes these and this path is never
        // reached. On any other feed the tab is still a row the daemon served,
        // and dropping it would hide a phase of a real run — so it draws as its
        // own row through the same badge and body the strip uses.
        return drawFeedMergeTabRow(row, arm.value, subfeed, rc);
      case "detachedSubagent":
        throw new MalformedView(
          "FeedRow.row.detached_subagent",
          "a bubble row reached the ordinary row path",
        );
      case "shellHead":
        throw new MalformedView(
          "FeedRow.row.shell_head",
          "a bubble row reached the ordinary row path",
        );
      default:
        return unreachableArm("FeedRow.row", armName(arm));
    }
  }

  /** One unit of synchronous turn progress, by arm. */
  function drawActivity(activity: FeedTurnActivity, rc: RowContext): HTMLElement {
    const unit = requireCase(activity.unit, "FeedTurnActivity.unit");
    switch (unit.case) {
      case "response":
        return opts.renderers.response(unit.value, rc);
      case "simpleToolCall":
        return opts.renderers.simpleToolCall(unit.value, rc);
      case "skill":
        return opts.renderers.skill(unit.value, rc);
      case "hook":
        return opts.renderers.hook(unit.value, rc);
      case "artifact":
        return opts.renderers.artifact(unit.value, rc);
      case "plan":
        return opts.renderers.plan(unit.value, rc);
      case "findings":
        return opts.renderers.findings(unit.value, rc);
      case "merge":
      case "subagent":
        throw new MalformedView(
          `FeedTurnActivity.unit.${unit.case}`,
          "a bubble unit reached the ordinary row path",
        );
      default:
        return unreachableArm("FeedTurnActivity.unit", armName(unit));
    }
  }

  /**
   * The identity attributes every row element carries.
   *
   * TOLERANT OF AN UNREADABLE ARM ON PURPOSE: the chrome is what carries the
   * row's identity to the reader and to the integration suite, and a row whose
   * BODY cannot be drawn still has an id and still occupies its place. The
   * refusal happens where the body is drawn, and produces the placeholder.
   *
   * The ID is the exception — it is the upsert key, so a row without one cannot
   * be held at all and the refusal is the whole row's.
   */
  function applyRowAttributes(el: HTMLElement, row: FeedRow): void {
    el.setAttribute("data-feed-row", requireMessage(row.id, "FeedRow.id").value);
    el.setAttribute("data-row-kind", row.row.case ?? "malformed");
    if (row.row.case === "activity" && row.row.value.unit.case !== undefined) {
      el.setAttribute("data-unit", row.row.value.unit.case);
    }
    if (row.turn !== undefined) {
      el.setAttribute("data-turn", row.turn.value);
      // THIS PAGE'S OWN SUBMISSION, claimed from the TurnId `SubmitPrompt`
      // minted here and echoed back on the row — never guessed from the text.
      if (isOwnTurn(row.turn)) el.setAttribute("data-mine", "true");
      else el.removeAttribute("data-mine");
      return;
    }
    // A RE-PUSH THAT NAMES NO TURN CLEARS THE OLD CLAIM: the element is reused
    // across upserts, so a turn left on it would attribute the row by a stamp
    // the daemon no longer states.
    el.removeAttribute("data-turn");
    el.removeAttribute("data-mine");
  }

  /** The compact stand-in for a row this build could not draw. */
  function malformedPlaceholder(err: MalformedView, row: FeedRow): HTMLElement {
    log.error(`a feed row could not be drawn: ${err.message}`, {
      operation: "feed.row-malformed",
      context: {
        feed: feedName(),
        row: row.id?.value ?? "unset",
        path: err.path,
        detail: err.detail,
      },
    });
    opts.ctx.failures.report(frameUndecodable(err.detail, err.path));
    const el = document.createElement("div");
    el.className = "row-malformed";
    el.setAttribute("data-arm", "malformed");
    el.textContent = `unreadable row at ${err.path}`;
    return el;
  }

  // ---- the walk ---------------------------------------------------------

  /**
   * Ask for the next-older page.
   *
   * `next` continues the walk the DAEMON holds for this feed, so there is
   * nothing to send but which feed and which direction — and a `next` with no
   * walk standing is the daemon's refusal, drawn at this control.
   */
  async function loadOlder(): Promise<void> {
    log.info("walking one page older", {
      operation: "feed.load-older",
      context: { feed: feedName() },
    });
    clearRefusals();
    loadMore.disabled = true;
    let response: GetFeedPageResponse;
    try {
      response = await callUnary(
        opts.ctx,
        "GetFeedPage",
        (client) =>
          client.getFeedPage(
            buildGetFeedPageRequest(opts.ctx.workspace, feedId(), "next"),
          ),
        GetFeedPageResponseSchema,
      );
    } catch {
      loadMore.after(refusalAt("transport", "the daemon could not be reached"));
      loadMore.disabled = false;
      return;
    }
    loadMore.disabled = false;
    const result = requireCase(response.result, "GetFeedPageResponse.result");
    switch (result.case) {
      case "success":
        applyPage(result.value, "prepend");
        return;
      case "error":
        loadMore.after(refusalAt("error", "older pages are not available"));
        return;
      default:
        unreachableArm("GetFeedPageResponse.result", armName(result));
    }
  }

  /** Drop whatever a previous walk's refusal left beside the control. */
  function clearRefusals(): void {
    for (const stale of opts.host.querySelectorAll(":scope > .refusal")) stale.remove();
  }

  /** Keep the tail on screen while a named cause's follow stands. */
  function followTail(): void {
    opts.scroll?.tail.follow();
  }

  // ---- presentation the client owns -------------------------------------

  /**
   * The rolling highlight: the NEWEST user prompt wears the marker and every
   * older one loses it. Client presentation, unchanged by the port — nothing
   * about the message that now carries prompts suggests it should move.
   */
  function markLatestPrompt(): void {
    let latest: HTMLElement | null = null;
    for (const id of order) {
      const state = states.get(id);
      if (state?.row.row.case === "userPrompt") latest = state.element;
    }
    for (const el of opts.host.querySelectorAll<HTMLElement>("[data-latest-prompt]")) {
      if (el !== latest) el.removeAttribute("data-latest-prompt");
    }
    latest?.setAttribute("data-latest-prompt", "true");
  }

  /**
   * THE THINKING WAVE IS THE ROW'S `working` FLAG, VERBATIM.
   *
   * Whether a prompt's turn is still working is the daemon's fact
   * (`FeedUserPrompt.working`, `FeedAgentPrompt.working`), drawn by the prompt
   * renderers at construction. The daemon RE-PUSHES the prompt row on the
   * turn's terminal with the flag unset, and a re-push that changes nothing
   * but the flag is applied HERE, on the bubble already on screen: redrawing
   * the prompt for it would rebuild its text and lose the reader's scroll
   * inside a long one, for a change that is only the band stopping.
   *
   * Answers whether the re-push was applied in place. Anything else about the
   * row changing — or a row not yet drawn — takes the ordinary redraw, which
   * draws the flag from the new row anyway.
   */
  function movePromptWaveInPlace(held: RowState, row: FeedRow): boolean {
    const working = promptWorking(row);
    if (working === null || held.dirty) return false;
    if (promptWorking(held.row) === null) return false;
    const bubble = held.element.querySelector<HTMLElement>(".bubble.user");
    if (bubble === null) return false;
    if (!equals(FeedRowSchema, withWorking(held.row, working), row)) return false;
    held.row = row;
    setPromptWave(bubble, working);
    log.debug(`moved the prompt wave in place: working=${String(working)}`, {
      operation: "feed.prompt-wave-moved",
      context: { feed: feedName(), row: requireMessage(row.id, "FeedRow.id").value, working },
    });
    return true;
  }

  // ---- lookups ----------------------------------------------------------

  function findRowElement(id: FeedId): HTMLElement | null {
    return states.get(id.value)?.element ?? null;
  }

  function bubbles(): readonly BubbleLike[] {
    const found: BubbleLike[] = [];
    for (const state of states.values()) {
      if (state.bubble !== null) found.push(state.bubble);
    }
    return found;
  }

  function feedId(): FeedId | undefined {
    return opts.feed === "root" ? undefined : opts.feed;
  }

  function feedName(): string {
    return opts.feed === "root" ? "root" : opts.feed.value;
  }

  function dispose(): void {
    if (disposed) return;
    disposed = true;
    bodyHandle.dispose();
    clearRows();
    listeners.clear();
    opts.host.replaceChildren();
  }
}

/** A prompt row's `working` flag, or null for a row that is not a prompt. */
function promptWorking(row: FeedRow): boolean | null {
  switch (row.row.case) {
    case "userPrompt":
    case "agentPrompt":
      return row.row.value.working;
    default:
      return null;
  }
}

/**
 * Whether ROW, placed live for the FIRST time, is a prompt the person just
 * sent. Read off the row alone: a user prompt the daemon marks `working`, which
 * it sets from the prompt's first draw until its turn's terminal. The flag is
 * what keeps a terminal re-push (published with `working` unset) of a prompt
 * this feed never drew, one scrolled out of the newest page, from counting as
 * a send.
 */
function isSentPrompt(row: FeedRow): boolean {
  return row.row.case === "userPrompt" && row.row.value.working;
}

/** A copy of prompt row ROW with its `working` flag set to WORKING. */
function withWorking(row: FeedRow, working: boolean): FeedRow {
  const copy = clone(FeedRowSchema, row);
  if (copy.row.case === "userPrompt" || copy.row.case === "agentPrompt") {
    copy.row.value.working = working;
  }
  return copy;
}

/** Whether this row's arm is a bubble — a sub-feed with a collapsed head. */
export function isBubbleRow(row: FeedRow): boolean {
  if (row.row.case === "detachedSubagent") return true;
  // A detached shell HEAD is a canonical bubble: its spool streams on the
  // sub-feed its FeedId addresses (the detached_shell BODY row rides there).
  if (row.row.case === "shellHead") return true;
  if (row.row.case !== "activity") return false;
  return row.row.value.unit.case === "subagent" || row.row.value.unit.case === "merge";
}

/** The refusal drawn beside the control that made the call. */
function refusalAt(arm: string, text: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "refusal";
  el.setAttribute("data-arm", arm);
  el.textContent = text;
  return el;
}

/**
 * A tone from the wire, validated against the shared vocabulary.
 *
 * The page error's tone is a STRING rather than an arm, so this is one of the
 * few places a colour is not already typed by the schema; the file is
 * authoritative, and a tone outside it is refused rather than dropped into a
 * `.tone-` rule that does not exist.
 */
function headlineToneClass(tone: string): string {
  if (!TOPBAR_TONES.includes(tone)) {
    throw new MalformedView(
      "FeedPageErrorHeadline.tone",
      `tone '${tone}' is not one of render-colors.json#topbar_tones`,
    );
  }
  return toneClass(tone as Color);
}
