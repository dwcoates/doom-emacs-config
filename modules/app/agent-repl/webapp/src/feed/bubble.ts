/**
 * bubble — the chrome every sub-feed wears, and the ONE expansion path.
 *
 * PARITY INVARIANT (stated in feed.proto and in the merge-flow ruling): a merge
 * bubble uses the SAME sub-feed plumbing as a subagent bubble. Expand →
 * `OpenFeed` on the bubble row's own `FeedId` → `WatchFeed` on the minted
 * token; collapse → cancel the watch. A merge-specific loader is a DEFECT, so
 * there is exactly one implementation here and the two kinds differ only in
 * which HEAD renderer and which BODY renderer they are handed.
 *
 * COLLAPSE CANCELS ONLY THE CLIENT LEG. Closing a watch stream is a normal
 * client act that ends nothing: the work goes on daemon-side, and stopping it
 * is always the `Interrupt` rpc.
 *
 * COLLAPSE WIPES THE SUB-FEED (owner ruling, 2026-09-23). An expansion renders
 * ONLY the sub-feed's newest page (`OpenFeed`) and then streams new rows in
 * while the bubble stays open; a collapse disposes the child controller, every
 * row it holds and the bubble's composer, so nothing of the sub-feed remains.
 * The next expansion starts fresh from a new `OpenFeed` page — never from the
 * history an earlier expansion accumulated. A collapse moves nothing the
 * reader is looking at: the one case that would, a bubble whose sub-feed lies
 * wholly ABOVE the viewport, is compensated through the scroll module's
 * content-preserving cause (`prependCompensation`).
 *
 * THE FOLD IS THE READER'S AFTER THE FIRST DRAW (R2). A merge row ships
 * `FeedMergeFold.folded` and a subagent row ships nothing, so a subagent bubble
 * starts collapsed and a merge bubble starts where the daemon says — ONCE. A
 * re-push redraws the head and never touches the fold, because a bubble
 * snapping shut under a reader who opened it is the whole failure R2 names.
 */
import { armButtonRole, CONTROL_SELECTOR } from "../control.js";
import { log } from "../log.js";
import { announceItemExpanded } from "../expand.js";
import { applyFeedTextScale } from "./feed-text-scale.js";
import { reportClientFailure } from "../rpc/link.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import type { AgentReplClient } from "../rpc/client.js";
import {
  OpenFeedResponseSchema,
  type OpenFeedResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import {
  WatchFeedResponseSchema,
  type WatchFeedResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import type { FeedId, FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FeedWatchToken } from "../../../proto/gen/ts/agentrepl/v1/feed_token_pb";
import type { AppContext } from "../rpc/context.js";
import { buildOpenFeedRequest, buildWatchFeedRequest } from "./requests.js";
import {
  createFeedController,
  type BubbleFactory,
  type BubbleLike,
  type FeedController,
} from "./feed-view.js";
import { armName } from "./renderers.js";
import type {
  BubbleBodyRenderer,
  ComposerFactory,
  Handle,
  RowContext,
  RowRenderers,
} from "./renderers.js";
import { stopTicking } from "./ticking.js";
import { refreshTitleFolds } from "./title-fold.js";
import type { Overscan } from "./overscan.js";
import type { TailFollow } from "../scroll.js";

export interface BubbleOptions {
  ctx: AppContext;
  /** The bubble row, as the parent feed served it. */
  row: FeedRow;
  /** The bubble row's own context, handed to the head and the body renderer. */
  rc: RowContext;
  /** The collapsed head: the subagent head, or the merge head renderer. */
  head(row: FeedRow, rc: RowContext): HTMLElement;
  /** The body: the default row list, or the merge tab strip. */
  body: BubbleBodyRenderer;
  renderers: RowRenderers;
  readonly revealRow: (id: FeedId) => Promise<boolean>;
  /** How a bubble nested INSIDE this sub-feed is built. */
  bubble: BubbleFactory;
  /** The per-bubble composer, when this build has one (R7). */
  composerFactory?: ComposerFactory;
  /** The fold this bubble takes on its FIRST draw only. */
  initialFolded: boolean;
  /**
   * The overscan buffer rooted on the page's scroll box, threaded down so the
   * rows this bubble's sub-feed holds are pre-rendered by the same instance
   * that watches the root feed. Absent with the scroll box.
   */
  overscan?: Overscan;
  /**
   * The page's scroll box and its one tail owner, so a collapse whose sub-feed
   * lies wholly above the viewport keeps the reader's content where it was.
   * Absent with the scroll box (a fixture rendering a feed on its own).
   */
  scroll?: { readonly box: Element; readonly tail: TailFollow };
}

/** Build the bubble chrome for one bubble row. */
export function mountBubble(opts: BubbleOptions): BubbleLike {
  const id = requireMessage(opts.row.id, "FeedRow.id");

  // A DETACHED/SUBAGENT BUBBLE IS FRAMED AS A NORMAL TOOL-CALL CARD (owner
  // ruling, 2026-09-14). The outer wears `.tool-card`, so it takes the SAME
  // chrome (border, radius, `--card` fill) and the SAME track cap every other
  // feed tool card takes (`.feed-item > .tool-card`) rather than the old
  // full-width async spread. `.bubble-fold` adds only the head-over-sub-feed
  // stacking; the teal wash and the dashed fold separator are gone.
  const el = document.createElement("div");
  el.className = "tool-card bubble-fold";

  // The collapsed head reads as a tool-call card's head row (`.tool-head`),
  // not the old async pill: the whole head IS the fold's toggle now (owner
  // ruling, 2026-09-15). The chevron is gone; clicking anywhere on the head
  // that is not itself an interactive control expands or collapses the bubble.
  // The head is therefore the accessible toggle in its own right — `role`,
  // `tabIndex`, `aria-expanded` and Enter/Space activation all ride it, the
  // affordance the removed `<button>` used to carry.
  const headLine = document.createElement("div");
  headLine.className = "tool-head bubble-head";
  headLine.setAttribute("data-expand", id.value);
  armButtonRole(headLine);
  headLine.setAttribute("aria-expanded", "false");

  const headSlot = document.createElement("span");
  headSlot.className = "bubble-head-slot";

  headLine.append(headSlot);

  const panel = document.createElement("div");
  panel.className = "agent-panel bubble-subfeed";
  panel.setAttribute("data-subfeed", "");

  el.append(headLine, panel);

  let row = opts.row;
  let expanded = false;
  let child: FeedController | null = null;
  let watch: StreamHandle | null = null;
  let composer: Handle | null = null;
  let opening: Promise<boolean> | null = null;
  let disposed = false;

  drawHead();
  applyExpanded(false);

  // CLICKING THE HEAD IS THE TOGGLE. A click that lands on an interactive
  // control the head carries — the stop button (`data-interrupt`), a link — is
  // that control's own act and never the fold's, so those are ignored here and
  // do their own thing.
  headLine.addEventListener("click", (event) => {
    if (isInteractiveTarget(event.target)) return;
    toggleFold();
  });
  // KEYBOARD PARITY WITH THE REMOVED BUTTON comes from `armButtonRole`
  // above: Enter and Space click the head, and the click is the toggle. A key
  // pressed while an inner control holds focus belongs to that control.

  // The wire's fold is the INITIAL state, so an unfolded merge bubble opens
  // itself once, here, and never again on a re-push.
  if (!opts.initialFolded) void expand();

  return {
    element: el,
    update(next: FeedRow): void {
      row = next;
      drawHead();
    },
    expand,
    isExpanded: () => expanded,
    collapse: () => {
      if (expanded) collapse();
    },
    child: () => child,
    dispose,
  };

  /** The collapsed head, redrawn whole from the row's latest push. */
  function drawHead(): void {
    const previous = headSlot.firstElementChild;
    if (previous !== null) stopTicking(previous);
    const head = opts.head(row, { ...opts.rc, row });
    headSlot.replaceChildren(head);
    // THE HEAD'S STATE IS THE BUBBLE'S. The head states the arm (live, settled
    // succeeded, …); the bubble is the element the feed hands upward, so it
    // repeats what the head said rather than deciding anything of its own.
    const state = head.getAttribute("data-state");
    if (state === null) el.removeAttribute("data-state");
    else el.setAttribute("data-state", state);
  }

  /** Show or hide the sub-feed, and say so on the element and the head. */
  function applyExpanded(next: boolean): void {
    expanded = next;
    el.setAttribute("data-expanded", next ? "true" : "false");
    // The ROW says it too. A fold is a fact about the row a reader is looking
    // at, and every query — the reveal walk included — starts from the row
    // rather than from the bubble element inside it. (The first call happens
    // before the bubble is mounted; the feed copies the attribute up when it
    // adopts the row, and this keeps the two agreeing on every toggle after.)
    el.closest("[data-feed-row]")?.setAttribute("data-expanded", next ? "true" : "false");
    // `aria-expanded` rides the head now that the head is the toggle.
    headLine.setAttribute("aria-expanded", next ? "true" : "false");
    panel.hidden = !next;
    // The head's title fold follows this fold (title-fold.ts): re-measure it on
    // the toggle, which lifts or restores its two-line cap.
    refreshTitleFolds(headLine);
  }

  /**
   * The fold's one activation, shared by a head click and Enter/Space on the
   * focused head.
   */
  function toggleFold(): void {
    clearRefusal();
    // A COLLAPSE NEVER MOVES THE FEED (owner rule, 2026-09-23: the user owns
    // the scroll). AN EXPANSION THE READER ASKED FOR CENTERS THE BUBBLE (owner
    // request, 2026-09-30): once its sub-feed is drawn, the bubble announces
    // itself expanded and the root feed puts its middle on the viewport's
    // (`itemExpanded`). A jump that opens bubbles calls `expand` directly and
    // announces nothing: it centers the entry it landed on itself.
    if (expanded) {
      collapse();
      return;
    }
    void expand().then((opened) => {
      if (opened) announceItemExpanded(el);
    });
  }

  /**
   * Whether an event's target is an interactive control the head carries (the
   * stop button, a link), which owns the event rather than the fold. The head
   * itself is a `<div>` and matches none of these, so a bare head click always
   * falls through to the toggle.
   */
  function isInteractiveTarget(target: EventTarget | null): boolean {
    return (
      target instanceof Element &&
      target.closest(`${CONTROL_SELECTOR}, a[href], input, select, textarea, [data-interrupt]`) !== null
    );
  }

  /**
   * Open the sub-feed: page first, then tail.
   *
   * The page/tail seam cannot gap because the token OpenFeed minted pins the
   * tail to begin exactly after the page it answered with — so the page is
   * painted first and the watch is opened on that token, never the other way
   * around.
   *
   * A second call while an open is in flight joins the first rather than
   * issuing a second `OpenFeed`: two opens would mint two tokens and leave one
   * watch running with nothing cancelling it.
   */
  async function expand(): Promise<boolean> {
    if (disposed) return false;
    if (expanded) return true;
    if (opening !== null) return opening;
    opening = open();
    try {
      return await opening;
    } finally {
      opening = null;
    }
  }

  async function open(): Promise<boolean> {
    log.info("expanding a bubble", {
      operation: "feed.bubble-expand",
      context: { row: id.value, kind: row.row.case ?? "unset" },
    });
    let response: OpenFeedResponse;
    try {
      response = await callUnary(
        opts.ctx,
        "OpenFeed",
        (client) => client.openFeed(buildOpenFeedRequest(opts.ctx.workspace, id)),
        OpenFeedResponseSchema,
      );
    } catch {
      drawRefusal("transport", "the daemon could not be reached");
      return false;
    }
    const result = requireCase(response.result, "OpenFeedResponse.result");
    switch (result.case) {
      case "success": {
        const controller = ensureChild();
        controller.applyPage(
          requireMessage(result.value.page, "OpenFeedSuccess.page"),
          "replace",
        );
        openWatch(requireMessage(result.value.watch, "OpenFeedSuccess.watch"));
        applyExpanded(true);
        return true;
      }
      case "error":
        // The refusal is this click's own answer, so it marks the control that
        // made the call rather than appearing as pushed state anywhere else.
        log.warn("the daemon refused to open a sub-feed", {
          operation: "feed.bubble-open-refused",
          context: { row: id.value },
        });
        drawRefusal("error", "this bubble's feed could not be opened");
        return false;
      default:
        return unreachableArm("OpenFeedResponse.result", armName(result));
    }
  }

  /** The sub-feed's controller, built on an expansion and disposed by the collapse. */
  function ensureChild(): FeedController {
    if (child !== null) return child;
    const composerSlot = mountComposerSlot();
    child = createFeedController({
      ctx: opts.ctx,
      host: panel,
      feed: id,
      renderers: opts.renderers,
      body: opts.body,
      revealRow: opts.revealRow,
      bubble: opts.bubble,
      bodyContext: opts.rc,
      composerSlot,
      overscan: opts.overscan,
    });
    return child;
  }

  /** The bubble's own composer, when this build mounts one (R7). */
  function mountComposerSlot(): HTMLElement | undefined {
    if (opts.composerFactory === undefined) return undefined;
    const slot = document.createElement("div");
    slot.className = "bubble-composer";
    composer = opts.composerFactory(slot, id);
    return slot;
  }

  /**
   * The tail.
   *
   * STANDING: it spans turns and ends only when this client cancels it, so an
   * end this bubble did not ask for is a transport failure the shared stream
   * machinery reports and reopens — never a bubble quietly going dead.
   *
   * A REOPEN GOES BACK THROUGH `OpenFeed`, exactly as the root feed's tail does
   * (feed.ts). `FeedWatchToken` pins a tail to begin exactly after the page the
   * open answered with, so re-echoing the DEAD token after a transport death
   * would resume a tail pinned to a page painted before the link broke: every
   * row produced during the outage silently missing, and the stale page still
   * standing as though it were current. So the FIRST attempt tails the token
   * the expanding click already minted, and every attempt after it opens the
   * feed again, PAINTS THE FRESH PAGE OVER THE ROWS, and tails that page's own
   * token.
   *
   * A REFUSED REOPEN DRAWS NOTHING HERE. Where such a refusal belongs is an
   * open UX question — the expanding click's refusal marks the toggle, but a
   * reopen has no click to mark — so this logs it and lets the shared backoff
   * try again, which is what the root feed does with the same case.
   */
  function openWatch(first: FeedWatchToken): void {
    watch?.cancel();
    let pending: FeedWatchToken | null = first;
    watch = watchStream<WatchFeedResponse>(opts.ctx, {
      name: "WatchFeed",
      schema: WatchFeedResponseSchema,
      open: (client, signal) => tail(client, signal),
      onPush: (response) => {
        // THE FEED TEXT ZOOM RIDES EVERY FEED'S WATCH, sub-feeds included
        // (endpoint_watch_feed.proto), and the daemon replays the current
        // scale the instant a tail is accepted. So a sub-feed's first frame is
        // ordinarily a scale push with no row, and it must be routed BEFORE
        // `requireMessage(response.row)`. Regression, 2026-09-28: every merge
        // bubble's tail filed `rpc.stream-frame-undecodable` on
        // `WatchFeedResponse.row` for exactly that frame. A selection push is
        // root-only by contract, so one here still fails loudly below.
        if (response.feedTextScale !== undefined) {
          applyFeedTextScale(response.feedTextScale.scale);
          return;
        }
        child?.upsert(requireMessage(response.row, "WatchFeedResponse.row"));
      },
    });

    async function* tail(
      client: AgentReplClient,
      signal: AbortSignal,
    ): AsyncGenerator<WatchFeedResponse> {
      const token = pending ?? (await reopen(signal));
      pending = null;
      if (token === null || disposed || signal.aborted) return;
      yield* opts.ctx.streams.watch("feed", buildWatchFeedRequest(token), signal);
    }
  }

  /**
   * One reopen attempt: `OpenFeed` on this bubble's own `FeedId`, the fresh
   * page painted over the sub-feed's rows, and the token that page came with.
   */
  async function reopen(signal: AbortSignal): Promise<FeedWatchToken | null> {
    const response: OpenFeedResponse = await callUnary(
      opts.ctx,
      "OpenFeed",
      (client) => client.openFeed(buildOpenFeedRequest(opts.ctx.workspace, id)),
      OpenFeedResponseSchema,
    );
    if (disposed || signal.aborted) return null;
    const result = requireCase(response.result, "OpenFeedResponse.result");
    switch (result.case) {
      case "success": {
        const controller = ensureChild();
        controller.applyPage(requireMessage(result.value.page, "OpenFeedSuccess.page"), "replace");
        return requireMessage(result.value.watch, "OpenFeedSuccess.watch");
      }
      case "error":
        log.error("the daemon refused to re-open a sub-feed after its tail died", {
          operation: "feed.bubble-reopen-refused",
          context: { row: id.value },
        });
        // The sub-feed stops tailing here and the bubble looks merely idle
        // (the audit's N3 row 15), so the footer carries the fact instead.
        reportClientFailure(
          "feed_not_tailing",
          "the daemon refused to re-open a sub-feed after its tail died",
        );
        return null;
      default:
        return unreachableArm("OpenFeedResponse.result", armName(result));
    }
  }

  /**
   * Collapse: abandon the token AND the sub-feed. The watch is cancelled, the
   * child controller disposed with every row it drew, and the composer with
   * it, so the next expansion begins from a fresh `OpenFeed` page.
   */
  function collapse(): void {
    const above = heightWhollyAboveViewport();
    log.info("collapsing a bubble", {
      operation: "feed.bubble-collapse",
      context: { row: id.value, above_viewport_px: above },
    });
    watch?.cancel();
    watch = null;
    applyExpanded(false);
    discardSubFeed();
    // THE READER'S CONTENT STAYS PUT. Hiding a sub-feed that lies wholly above
    // the viewport shrinks the content above the reader by its height, which
    // would slide what they are reading up; the content-preserving cause moves
    // the feed back by exactly that. Anything at or below the viewport's top
    // is the reader's own view, and a collapse there moves nothing.
    if (above > 0) opts.scroll?.tail.prependCompensation(-above);
  }

  /**
   * The sub-feed's height when it lies WHOLLY above the scroll box's top, else
   * 0. Read before the collapse hides it, off the live layout (reading moves
   * nothing).
   */
  function heightWhollyAboveViewport(): number {
    if (opts.scroll === undefined || !expanded) return 0;
    const box = opts.scroll.box.getBoundingClientRect();
    const sub = panel.getBoundingClientRect();
    return sub.bottom <= box.top ? sub.height : 0;
  }

  /** Dispose the child controller, its rows and the composer: nothing remains. */
  function discardSubFeed(): void {
    const rows = child === null ? 0 : panel.querySelectorAll("[data-feed-row]").length;
    composer?.dispose();
    composer = null;
    child?.dispose();
    child = null;
    log.debug("a collapsed bubble's sub-feed was discarded", {
      operation: "feed.bubble-subfeed-discarded",
      context: { row: id.value, rows },
    });
  }

  /** The refusal, drawn at the control that made the call. */
  function drawRefusal(arm: string, text: string): void {
    clearRefusal();
    const el2 = document.createElement("span");
    el2.className = "refusal";
    el2.setAttribute("data-arm", arm);
    el2.textContent = text;
    // The refusal marks the head, which is the control the reader clicked.
    headLine.append(el2);
  }

  function clearRefusal(): void {
    for (const stale of headLine.querySelectorAll(".refusal")) stale.remove();
  }

  function dispose(): void {
    if (disposed) return;
    disposed = true;
    watch?.cancel();
    composer?.dispose();
    child?.dispose();
    stopTicking(el);
    el.remove();
  }
}
