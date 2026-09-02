/**
 * feed — the mount: the ROOT feed, the bubble factory, and `revealRow`.
 *
 * THE ROOT FEED IS OPENED LIKE ANY OTHER FEED — `OpenFeed` with no address
 * answers the newest page and mints the watch token, the page is painted, then
 * the tail is opened on that token. That is the same two-step every bubble
 * takes, which is why the controller and the bubble chrome beneath this are
 * shared rather than mirrored.
 *
 * A WORKSPACE HAS A UNIVERSE OF FEEDS, and this module is where the universe is
 * assembled: which head and which body each bubble kind gets, and how a jump
 * target is found across the open ones.
 */
import { log } from "../log.js";
import { create } from "@bufbuild/protobuf";
import { MalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { installClickExpand } from "../expand.js";
import { TailFollow, revealNode } from "../scroll.js";
import {
  OpenFeedResponseSchema,
  type OpenFeedResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import {
  WatchFeedResponseSchema,
  type WatchFeedResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import {
  FeedRowSchema,
  type FeedBreadcrumb,
  type FeedDetachedSubagent,
  type FeedId,
  type FeedMerge,
  type FeedRow,
  type FeedSubagent,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { AppContext } from "../rpc/context.js";
import type { AgentReplClient } from "../rpc/client.js";
import { buildOpenFeedRequest, buildWatchFeedRequest } from "./requests.js";
import { mountBubble } from "./bubble.js";
import {
  createFeedController,
  type BubbleLike,
  type FeedController,
} from "./feed-view.js";
import {
  armName,
  defaultBubbleBody,
  type ComposerFactory,
  type Handle,
  type RowContext,
  type RowRenderers,
} from "./renderers.js";
import { drawFeedSubagent, drawFeedDetachedSubagent } from "./rows/subagent.js";
import { tick, stopTicking } from "./ticking.js";

/** How long a revealed row wears the highlight that says "here". */
export const REVEAL_HIGHLIGHT_MS = 1500;

/** The class a revealed row wears while the reader's eye lands on it. */
export const REVEAL_CLASS = "row-revealed";

/** The attribute a row wears while it is the one a jump landed on. */
export const REVEAL_ATTRIBUTE = "data-revealed";

export interface FeedDeps {
  renderers: RowRenderers;
  /** Mounts a composer inside each bubble. Absent in production (R7). */
  composerFactory?: ComposerFactory;
  /**
   * The scrolling region the feed lives in. The page's own `#feed-scroll`;
   * defaults to the host's parent, and the tail-follow is skipped when there
   * is neither (a fixture rendering the feed on its own).
   */
  scrollBox?: HTMLElement;
}

export interface FeedHandle extends Handle {
  /** Bring a row into view, opening whatever bubbles stand above it. */
  revealRow(id: FeedId): Promise<boolean>;
}

/** Mount the root feed into HOST. */
export function mountFeed(host: HTMLElement, ctx: AppContext, deps: FeedDeps): FeedHandle {
  log("info", "mounting the feed", { operation: "feed.mount", context: {} });

  const scrollBox = deps.scrollBox ?? host.parentElement ?? null;
  const tail = scrollBox === null ? null : new TailFollow(scrollBox);
  let watch: StreamHandle | null = null;
  let disposed = false;

  // Click-to-expand on every capped section the cards draw, armed once for the
  // whole feed rather than per card — the existing behaviour, kept.
  installClickExpand(host);

  const root: FeedController = createFeedController({
    ctx,
    host,
    feed: "root",
    renderers: deps.renderers,
    body: defaultBubbleBody,
    revealRow,
    bubble: bubbleFor,
    bodyContext: feedContext(),
    scroll: scrollBox === null || tail === null ? undefined : { box: scrollBox, tail },
  });

  openWatch();

  return { revealRow, dispose };

  /**
   * A context standing for THE FEED ITSELF, which is not a row.
   *
   * The body renderer's signature carries a `RowContext` because a bubble's
   * body has one; the root feed has no bubble row above it, so the row here is
   * the empty message — and the default body reads only `revealRow` off it.
   */
  function feedContext(): RowContext {
    return {
      ctx,
      feed: "root",
      row: create(FeedRowSchema, {}),
      revealRow,
    };
  }

  /**
   * THE ROOT FEED'S STANDING TAIL, opened THROUGH `OpenFeed` on every attempt.
   *
   * THE OPEN IS PART OF THE STREAM, not a step before it. `FeedWatchToken`
   * pins the tail "to begin exactly after the page the open answered with, so
   * the page/tail seam cannot gap or overlap" (feed_token.proto). A reopen that
   * re-echoed the DEAD token would resume a tail pinned to a page painted
   * before the link broke: every row produced during the outage silently
   * missing, and every row from the stale page still standing as though it were
   * current. So each attempt opens the feed again, PAINTS THE FRESH PAGE OVER
   * THE ROWS — a page is a whole view, and `"replace"` is what makes it one —
   * and tails the token that page came with.
   *
   * Putting it inside `open` rather than in a reconnect hook keeps ONE handle
   * for the whole life of the tail, which is what lets `watchStream` own the
   * backoff, the `daemon_unreachable` window and its retraction exactly once.
   */
  function openWatch(): void {
    watch?.cancel();
    watch = watchStream<WatchFeedResponse>(ctx, {
      name: "WatchFeed",
      schema: WatchFeedResponseSchema,
      open: (client, signal) => openAndTail(client, signal),
      onPush: (response) => {
        root.upsert(requireMessage(response.row, "WatchFeedResponse.row"));
      },
    });
  }

  /**
   * One attempt: `OpenFeed`, paint, then tail.
   *
   * A refusal ENDS the attempt rather than throwing something the loop would
   * mislabel. The loop then treats the end as a stream that stopped on its own
   * — which it is — files its card and retries with backoff, which is right for
   * `not_yet_adopted` and no worse than a permanently empty feed for the rest.
   */
  async function* openAndTail(
    client: AgentReplClient,
    signal: AbortSignal,
  ): AsyncGenerator<WatchFeedResponse> {
    const response: OpenFeedResponse = await callUnary(
      ctx,
      "OpenFeed",
      (c) => c.openFeed(buildOpenFeedRequest(ctx.workspace)),
      OpenFeedResponseSchema,
    );
    if (disposed || signal.aborted) return;
    const result = requireCase(response.result, "OpenFeedResponse.result");
    switch (result.case) {
      case "success": {
        root.applyPage(requireMessage(result.value.page, "OpenFeedSuccess.page"), "replace");
        const token = requireMessage(result.value.watch, "OpenFeedSuccess.watch");
        yield* client.watchFeed(buildWatchFeedRequest(token), { signal });
        return;
      }
      case "error":
        log("error", "the daemon refused to open the workspace's root feed", {
          operation: "feed.root-open-refused",
          context: { arm: requireCase(result.value.cause ?? {}, "OpenFeedError.cause").case },
        });
        return;
      default:
        unreachableArm("OpenFeedResponse.result", armName(result));
    }
  }

  /**
   * Build the bubble for one bubble row.
   *
   * THE ONLY DIFFERENCES BETWEEN THE KINDS live here: which head renderer draws
   * the collapsed line, which body renderer lays the sub-feed out, and what the
   * fold starts at. Everything below is the one shared plumbing path.
   */
  function bubbleFor(row: FeedRow, rc: RowContext): BubbleLike {
    const shared = {
      ctx,
      row,
      rc,
      renderers: deps.renderers,
      revealRow,
      bubble: bubbleFor,
      composerFactory: deps.composerFactory,
    };
    if (row.row.case === "detachedSubagent") {
      return mountBubble({
        ...shared,
        head: (current, context) => drawFeedDetachedSubagent(detachedSubagentOf(current), context),
        body: defaultBubbleBody,
        // A subagent row ships no fold, so it starts collapsed and transfers
        // nothing but its head until the reader asks for more.
        initialFolded: true,
      });
    }
    if (unitCase(row) === "merge") {
      return mountBubble({
        ...shared,
        head: (current, context) => deps.renderers.mergeHead(mergeOf(current), context),
        body: deps.renderers.mergeBody,
        initialFolded: requireMessage(
          requireMessage(mergeOf(row).head, "FeedMerge.head").fold,
          "FeedMergeHead.fold",
        ).folded,
      });
    }
    return mountBubble({
      ...shared,
      head: (current, context) => drawFeedSubagent(subagentOf(current), context),
      body: defaultBubbleBody,
      initialFolded: true,
    });
  }

  // ---- reveal -----------------------------------------------------------

  /**
   * Bring a row into view.
   *
   * FOUND FIRST: a row already drawn — on the root feed or inside any OPEN
   * sub-feed — is simply scrolled to. Only when it is nowhere on screen is the
   * daemon asked where it lives, and the answer is the page's BREADCRUMBS: the
   * chain of containers above it, outermost first, each of which is expanded in
   * turn before the row is looked for again.
   *
   * A REFUSAL IS AN ANSWER, NOT AN ERROR. `OpenFeedError` means the target is
   * not a feed (a shell bubble's row, say) or is not this workspace's, and a
   * walk that cannot complete leaves the reader where they were rather than
   * moving them somewhere plausible.
   */
  async function revealRow(id: FeedId): Promise<boolean> {
    const here = findAcrossOpenFeeds(root, id);
    if (here !== null) {
      land(here);
      return true;
    }
    log("debug", "the reveal target is not drawn; asking the daemon where it lives", {
      operation: "feed.reveal-probe",
      context: { row: id.value },
    });
    let response: OpenFeedResponse;
    try {
      response = await callUnary(
        ctx,
        "OpenFeed",
        (client) => client.openFeed(buildOpenFeedRequest(ctx.workspace, id)),
        OpenFeedResponseSchema,
      );
    } catch {
      return false;
    }
    const result = requireCase(response.result, "OpenFeedResponse.result");
    if (result.case === "error") {
      // The target is not a feed of its own — a shell bubble's row is the case
      // the ruling names — so the jump degrades to scroll-if-rendered, which is
      // exactly what the search above already tried.
      log("warn", "the daemon refused to open the reveal target's feed", {
        operation: "feed.reveal-refused",
        context: { row: id.value },
      });
      return false;
    }
    if (result.case !== "success") return unreachableArm("OpenFeedResponse.result", armName(result));
    // The probe's token is deliberately ABANDONED: the walk below opens each
    // bubble properly, which mints the token that bubble will actually tail.
    const page = requireCase(
      requireMessage(result.value.page, "OpenFeedSuccess.page").result,
      "FeedPage.result",
    );
    if (page.case !== "success") return false;
    const crumbs = requireMessage(page.value.breadcrumbs, "FeedPageSuccess.breadcrumbs").crumbs;
    const walked = await walk(crumbs);
    if (!walked) return false;
    const found = findAcrossOpenFeeds(root, id);
    if (found === null) {
      log("warn", "the breadcrumb walk finished without the reveal target appearing", {
        operation: "feed.reveal-lost",
        context: { row: id.value, crumbs: crumbs.length },
      });
      return false;
    }
    land(found);
    return true;
  }

  /** Expand each container top-down, each awaiting its own open. */
  async function walk(crumbs: readonly FeedBreadcrumb[]): Promise<boolean> {
    let controller: FeedController = root;
    for (const crumb of crumbs) {
      const target = requireMessage(crumb.target, "FeedBreadcrumb.target");
      const bubble = bubbleOn(controller, target);
      if (bubble === null) {
        log("warn", "a breadcrumb names a bubble this feed does not hold", {
          operation: "feed.reveal-crumb-absent",
          context: { crumb: target.value },
        });
        return false;
      }
      if (!(await bubble.expand())) return false;
      const child = bubble.child();
      if (child === null) return false;
      controller = child;
    }
    return true;
  }

  /** The bubble for TARGET on CONTROLLER, if it holds one. */
  function bubbleOn(controller: FeedController, target: FeedId): BubbleLike | null {
    const element = controller.findRowElement(target);
    if (element === null) return null;
    for (const bubble of controller.bubbles()) {
      if (element.contains(bubble.element)) return bubble;
    }
    return null;
  }

  /** Scroll the row into view and mark it, briefly, as the one meant. */
  function land(element: HTMLElement): void {
    revealNode(element);
    element.classList.add(REVEAL_CLASS);
    // STATED, not only styled: "this is the row you asked for" is a fact about
    // the row while it stands, and a jump's caller has no other way to see that
    // the landing happened.
    element.setAttribute(REVEAL_ATTRIBUTE, "true");
    const deadline = ctx.ticker.now() + REVEAL_HIGHLIGHT_MS;
    tick(element, ctx.ticker, (nowMs) => {
      if (nowMs < deadline) return;
      stopTicking(element);
      element.classList.remove(REVEAL_CLASS);
      element.removeAttribute(REVEAL_ATTRIBUTE);
    });
  }

  function dispose(): void {
    if (disposed) return;
    disposed = true;
    watch?.cancel();
    root.dispose();
  }
}

/** The row's element, searched across the root feed and every OPEN sub-feed. */
export function findAcrossOpenFeeds(
  controller: FeedController,
  id: FeedId,
): HTMLElement | null {
  const here = controller.findRowElement(id);
  if (here !== null) return here;
  for (const bubble of controller.bubbles()) {
    if (!bubble.isExpanded()) continue;
    const child = bubble.child();
    if (child === null) continue;
    const found = findAcrossOpenFeeds(child, id);
    if (found !== null) return found;
  }
  return null;
}

/** Which activity unit a row carries, or undefined when it is not activity. */
function unitCase(row: FeedRow): string | undefined {
  if (row.row.case !== "activity") return undefined;
  return row.row.value.unit.case;
}

/** The detached wrapper on a row the caller has established is one. */
function detachedSubagentOf(row: FeedRow): FeedDetachedSubagent {
  if (row.row.case !== "detachedSubagent") {
    throw new MalformedView("FeedRow.row", "the row is not a detached subagent");
  }
  return row.row.value;
}

/** The subagent unit on a row the caller has established is one. */
function subagentOf(row: FeedRow): FeedSubagent {
  if (row.row.case !== "activity" || row.row.value.unit.case !== "subagent") {
    throw new MalformedView("FeedTurnActivity.unit", "the row is not a subagent bubble");
  }
  return row.row.value.unit.value;
}

/** The merge unit on a row the caller has established is one. */
function mergeOf(row: FeedRow): FeedMerge {
  if (row.row.case !== "activity" || row.row.value.unit.case !== "merge") {
    throw new MalformedView("FeedTurnActivity.unit", "the row is not a merge bubble");
  }
  return row.row.value.unit.value;
}
