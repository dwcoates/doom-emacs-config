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
 * is always the `Interrupt` rpc. The last drawn DOM is kept so a re-expand is
 * cheap to look at while it re-opens.
 *
 * THE FOLD IS THE READER'S AFTER THE FIRST DRAW (R2). A merge row ships
 * `FeedMergeFold.folded` and a subagent row ships nothing, so a subagent bubble
 * starts collapsed and a merge bubble starts where the daemon says — ONCE. A
 * re-push redraws the head and never touches the fold, because a bubble
 * snapping shut under a reader who opened it is the whole failure R2 names.
 */
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
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
  revealRow(id: FeedId): Promise<boolean>;
  /** How a bubble nested INSIDE this sub-feed is built. */
  bubble: BubbleFactory;
  /** The per-bubble composer, when this build has one (R7). */
  composerFactory?: ComposerFactory;
  /** The fold this bubble takes on its FIRST draw only. */
  initialFolded: boolean;
}

/** Build the bubble chrome for one bubble row. */
export function mountBubble(opts: BubbleOptions): BubbleLike {
  const id = requireMessage(opts.row.id, "FeedRow.id");

  const el = document.createElement("div");
  el.className = "async-fold bubble";

  const headLine = document.createElement("div");
  headLine.className = "async-ticker bubble-head";

  const toggle = document.createElement("button");
  toggle.type = "button";
  toggle.className = "bubble-toggle agent-caret";
  toggle.setAttribute("data-expand", id.value);
  toggle.setAttribute("aria-expanded", "false");

  const headSlot = document.createElement("span");
  headSlot.className = "bubble-head-slot";

  headLine.append(toggle, headSlot);

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

  toggle.addEventListener("click", () => {
    clearRefusal();
    if (expanded) {
      collapse();
      return;
    }
    void expand();
  });

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

  /** Show or hide the sub-feed, and say so on the element and the toggle. */
  function applyExpanded(next: boolean): void {
    expanded = next;
    el.setAttribute("data-expanded", next ? "true" : "false");
    // The ROW says it too. A fold is a fact about the row a reader is looking
    // at, and every query — the reveal walk included — starts from the row
    // rather than from the bubble element inside it. (The first call happens
    // before the bubble is mounted; the feed copies the attribute up when it
    // adopts the row, and this keeps the two agreeing on every toggle after.)
    el.closest("[data-feed-row]")?.setAttribute("data-expanded", next ? "true" : "false");
    toggle.setAttribute("aria-expanded", next ? "true" : "false");
    toggle.textContent = next ? "▾" : "▸";
    panel.hidden = !next;
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
    log("info", "expanding a bubble", {
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
        log("warn", "the daemon refused to open a sub-feed", {
          operation: "feed.bubble-open-refused",
          context: { row: id.value },
        });
        drawRefusal("error", "this bubble's feed could not be opened");
        return false;
      default:
        return unreachableArm("OpenFeedResponse.result", armName(result));
    }
  }

  /** The sub-feed's controller, built on the first expansion and kept after. */
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
   */
  function openWatch(token: FeedWatchToken): void {
    watch?.cancel();
    watch = watchStream<WatchFeedResponse>(opts.ctx, {
      name: "WatchFeed",
      schema: WatchFeedResponseSchema,
      open: (client, signal) =>
        client.watchFeed(buildWatchFeedRequest(token), { signal }),
      onPush: (response) => {
        child?.upsert(requireMessage(response.row, "WatchFeedResponse.row"));
      },
    });
  }

  /** Collapse: abandon the token, keep the DOM. */
  function collapse(): void {
    log("info", "collapsing a bubble", {
      operation: "feed.bubble-collapse",
      context: { row: id.value },
    });
    watch?.cancel();
    watch = null;
    applyExpanded(false);
  }

  /** The refusal, drawn at the control that made the call. */
  function drawRefusal(arm: string, text: string): void {
    clearRefusal();
    const el2 = document.createElement("span");
    el2.className = "refusal";
    el2.setAttribute("data-arm", arm);
    el2.textContent = text;
    toggle.after(el2);
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
