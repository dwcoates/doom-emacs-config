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
import { reportClientFailure } from "../rpc/link.js";
import { MalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { installClickExpand } from "../expand.js";
import { refreshHasMore } from "./bubble-more.js";
import { refreshTitleFolds } from "./title-fold.js";
import { applyFeedTextScale } from "./feed-text-scale.js";
import {
  TailFollow,
  installIntentScroll,
  observeScrollBox,
  revealGeometry,
} from "../scroll.js";
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
  type FeedShell,
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
import { FEED_GROUP_CLASS, activateGroupedMember } from "./tool-group.js";
import { HELD_ENTRY_SELECTOR } from "../tray/tray.js";
import { tick, stopTicking } from "./ticking.js";
import { createOverscan } from "./overscan.js";

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
  /**
   * The reader picked a detached-work item in the expanded footer: open
   * whatever bubbles stand above its card, mark it, and scroll it into view
   * (`detachedWorkSelected`, one of the closed set of scroll causes).
   */
  readonly selectDetachedWork: (id: FeedId) => Promise<boolean>;
}

/** Mount the root feed into HOST. */
export function mountFeed(host: HTMLElement, ctx: AppContext, deps: FeedDeps): FeedHandle {
  log.info("mounting the feed", { operation: "feed.mount", context: {} });

  const scrollBox = deps.scrollBox ?? host.parentElement ?? null;
  const tail =
    scrollBox === null
      ? null
      : new TailFollow(scrollBox, () => {
          const entry = latestEntry(scrollBox);
          return entry === null ? null : revealGeometry(scrollBox, entry);
        });
  // The tail owner's OTHER two inputs, which only a mount holding the real
  // element can give it: the box's scroll events and the box's size changes.
  // The size half is the footer occlusion (see `observeScrollBox`) — the
  // docked footer settling after a render shrinks this box, and a tail parked
  // before that shrink is left below the fold with the last bubble clipped.
  const unobserve = scrollBox === null || tail === null ? null : observeScrollBox(scrollBox, tail);
  // INTENT-ARMED INNER SCROLLING, on the same box. A capped section keeps the
  // wheel only while the reader has deliberately entered it; otherwise the
  // wheel redirects to the feed. Without it, a section the feed scrolled under
  // a still cursor captures the next gesture and scrolling gets stuck in the
  // bubble (scroll.ts's installIntentScroll). No box, no sections to gate.
  const intentScroll = scrollBox === null ? null : installIntentScroll(scrollBox);
  // THE OVERSCAN BUFFER, rooted on the same scroll box, blows the pre-render
  // band out to ~5 viewport heights so a row within it lays out at its true
  // height before the reader scrolls to it — the cure for the first-scroll
  // `content-visibility: auto` jitter. `null` where there is no box (a fixture)
  // or the environment ships no `IntersectionObserver`, in which case the feed
  // works exactly as before, minus the pre-render. ONE instance is shared with
  // every sub-feed, since they all scroll inside this one box.
  const overscan = scrollBox === null ? null : createOverscan(scrollBox);
  let watch: StreamHandle | null = null;
  let disposed = false;

  // Click-to-expand on every capped section the cards draw, armed once for the
  // whole feed rather than per card — the existing behaviour, kept. The
  // afterToggle hook (FIX2) refreshes the bubble's "more below" affordance on an
  // expand/collapse whose height did not change, which the ResizeObserver in
  // bubble-more.ts cannot catch; refreshHasMore self-restricts to response/prompt
  // bubbles, so a click on a tool section does nothing.
  //
  // OWNER BUG FIX: a click that EXPANDS a bubble fires no pointermove, so
  // scroll.ts's intent-arm latch never sees the box the click just revealed —
  // the first wheel over it, cursor unmoved, redirects to the feed instead of
  // scrolling the bubble. `intentScroll.arm` is the same armed-state latch a
  // pointermove writes; calling it here on EXPAND ONLY (never on collapse)
  // arms the just-expanded section immediately, so the wheel scrolls it
  // without requiring the cursor to move first.
  installClickExpand(host, undefined, (section, expanded) => {
    refreshHasMore(section);
    // A card's title fold follows the card's fold (title-fold.ts), so a toggle
    // re-measures the titles it owns as well as the section itself.
    refreshTitleFolds(section);
    if (expanded) intentScroll?.arm(section);
  });

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
    overscan: overscan ?? undefined,
  });

  openWatch();

  return { selectDetachedWork, dispose };

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
        // A frame carries EXACTLY ONE of a row upsert, a response-selection
        // push, or a feed-text-scale push (endpoint_watch_feed.proto). The two
        // non-row frames carry no row, so they must be routed BEFORE
        // `requireMessage(response.row)`, which would otherwise reject a
        // well-formed push as a malformed row. A frame with none still fails
        // loudly there, as before.
        if (response.feedTextScale !== undefined) {
          applyFeedTextScale(response.feedTextScale.scale);
          return;
        }
        if (response.selection !== undefined) {
          root.applySelection(response.selection);
          return;
        }
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
        yield* ctx.streams.watch("feed", buildWatchFeedRequest(token), signal);
        return;
      }
      case "error":
        log.error("the daemon refused to open the workspace's root feed", {
          operation: "feed.root-open-refused",
          context: { arm: requireCase(result.value.cause ?? {}, "OpenFeedError.cause").case },
        });
        // THE DAEMON ANSWERED AND REFUSED, so the loop's own card would name a
        // daemon that is plainly reachable (the audit's N3 row 14). The footer
        // says what actually stopped: this feed is no longer tailing.
        reportClientFailure(
          "feed_not_tailing",
          "the daemon refused to open the workspace's root feed",
        );
        return;
      default:
        unreachableArm("OpenFeedResponse.result", armName(result));
    }
  }

  /**
   * The collapsed head of whatever the row IS AT THIS PUSH.
   *
   * A BUBBLE ROW CAN CHANGE ARM UNDER ITS OWN FeedId, and that is the contract
   * rather than an oddity: a spawn is announced as a SYNCHRONOUS `subagent`
   * unit and, the moment the vendor answers `async_launched`, the daemon
   * re-pushes the SAME row as the `detached_subagent` placement
   * ("daemon.feed.detached_subagent -- a subagent bubble moved to its detached
   * placement"). The bubble itself is deliberately kept across that push, since
   * tearing it down would drop an open sub-feed under a reader.
   *
   * So the head is chosen per DRAW, from the current row, never captured at
   * mount. It was captured at mount, and every detached spawn paid for it: the
   * head renderer refused the re-pushed row with `MalformedView`
   * ("FeedTurnActivity.unit: the row is not a subagent bubble"), the shared
   * stream machinery skipped the frame, and the page filed a
   * `frameUndecodable` failure -- so a detached subagent's head froze on
   * the state it was announced with and never moved again. Caught by the G51
   * playbook, whose fan-wide cancel launches two of them.
   */
  function bubbleHead(current: FeedRow, context: RowContext): HTMLElement {
    if (current.row.case === "detachedSubagent") {
      return drawFeedDetachedSubagent(detachedSubagentOf(current), context);
    }
    if (current.row.case === "shellHead") {
      return deps.renderers.shellHead(shellHeadOf(current), context);
    }
    if (unitCase(current) === "merge") {
      return deps.renderers.mergeHead(mergeOf(current), context);
    }
    return drawFeedSubagent(subagentOf(current), context);
  }

  /**
   * Build the bubble for one bubble row.
   *
   * THE ONLY DIFFERENCES BETWEEN THE KINDS live here: which body renderer lays
   * the sub-feed out and what the fold starts at. The HEAD is not one of them
   * -- see `bubbleHead` -- because the arm a row wears is not fixed for the
   * row's life. Everything below is the one shared plumbing path.
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
      head: bubbleHead,
      overscan: overscan ?? undefined,
    };
    if (unitCase(row) === "merge") {
      return mountBubble({
        ...shared,
        body: deps.renderers.mergeBody,
        initialFolded: requireMessage(
          requireMessage(mergeOf(row).head, "FeedMerge.head").fold,
          "FeedMergeHead.fold",
        ).folded,
      });
    }
    // A subagent row ships no fold, so it starts collapsed and transfers
    // nothing but its head until the reader asks for more -- synchronous and
    // detached alike, since the two are one bubble that moved placement.
    return mountBubble({ ...shared, body: defaultBubbleBody, initialFolded: true });
  }

  // ---- reveal -----------------------------------------------------------

  /**
   * Find a row and mark it, WITHOUT moving the feed: the breadcrumb and the
   * hook's gated-call link open the bubbles above their target and mark it,
   * and the reader scrolls to it themselves (owner rule, 2026-09-23: the user
   * owns the scroll). Only the footer's detached-work selection scrolls.
   */
  function revealRow(id: FeedId): Promise<boolean> {
    return reveal(id, false);
  }

  /** The footer's detached-work selection: find, mark and scroll to the card. */
  function selectDetachedWork(id: FeedId): Promise<boolean> {
    return reveal(id, true);
  }

  /**
   * Bring a row onto the page and mark it; SCROLL says whether the feed then
   * moves to it (the detached-work selection) or stays where the reader has it.
   *
   * FOUND FIRST: a row already drawn — on the root feed or inside any OPEN
   * sub-feed — is simply landed on. Only when it is nowhere on screen is the
   * daemon asked where it lives, and the answer is the page's BREADCRUMBS: the
   * chain of containers above it, outermost first, each of which is expanded in
   * turn before the row is looked for again.
   *
   * A REFUSAL IS AN ANSWER, NOT AN ERROR. `OpenFeedError` means the target is
   * not a feed (a shell bubble's row, say) or is not this workspace's, and a
   * walk that cannot complete leaves the reader where they were rather than
   * moving them somewhere plausible.
   */
  async function reveal(id: FeedId, scroll: boolean): Promise<boolean> {
    const here = findAcrossOpenFeeds(root, id);
    if (here !== null) {
      land(here, scroll);
      return true;
    }
    log.debug("the reveal target is not drawn; asking the daemon where it lives", {
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
      // The probe never reached the daemon. Reported as a TRANSPORT failure
      // rather than as a refused feed, because that is what it was, and the
      // line names the probe so the footer does not have to guess.
      reportClientFailure(
        "unary_transport",
        "OpenFeed (the feed's reveal probe) could not reach the daemon",
      );
      return false;
    }
    const result = requireCase(response.result, "OpenFeedResponse.result");
    if (result.case === "error") {
      // The target is not a feed of its own — a shell bubble's row is the case
      // the ruling names — so the jump degrades to scroll-if-rendered, which is
      // exactly what the search above already tried.
      log.warn("the daemon refused to open the reveal target's feed", {
        operation: "feed.reveal-refused",
        context: { row: id.value },
      });
      reportClientFailure("feed_not_tailing", "the daemon refused the feed's reveal probe");
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
      log.warn("the breadcrumb walk finished without the reveal target appearing", {
        operation: "feed.reveal-lost",
        context: { row: id.value, crumbs: crumbs.length },
      });
      return false;
    }
    land(found, scroll);
    return true;
  }

  /** Expand each container top-down, each awaiting its own open. */
  async function walk(crumbs: readonly FeedBreadcrumb[]): Promise<boolean> {
    let controller: FeedController = root;
    for (const crumb of crumbs) {
      const target = requireMessage(crumb.target, "FeedBreadcrumb.target");
      const bubble = bubbleOn(controller, target);
      if (bubble === null) {
        log.warn("a breadcrumb names a bubble this feed does not hold", {
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

  /** Mark the row, briefly, as the one meant; scroll to it when SCROLL says so. */
  function land(element: HTMLElement, scroll: boolean): void {
    // A grouped member sits in an inactive tab is HIDDEN and has no layout box;
    // bring its tab to the front, so the landing is on a member that is
    // actually drawn (tool-group.ts). A member outside any group is left alone.
    activateGroupedMember(element);
    log.debug(`landed on a revealed row${scroll ? ", scrolling to it" : ""}`, {
      operation: "feed.reveal-landed",
      context: { row: element.getAttribute("data-feed-row") ?? "unset", scroll },
    });
    if (scroll && scrollBox !== null && tail !== null) {
      tail.detachedWorkSelected(revealGeometry(scrollBox, element));
    }
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
    unobserve?.();
    intentScroll?.uninstall();
    overscan?.dispose();
    root.dispose();
  }
}

/** The row's element, searched across the root feed and every OPEN sub-feed. */
/** Every feed row's element, at whatever depth of sub-feed it is drawn. */
const FEED_ROW_SELECTOR = "[data-feed-row]";

/**
 * THE FEED COLUMN'S LATEST ENTRY inside the scroll BOX: the last thing drawn in
 * it, whatever it is, or null when nothing is drawn.
 *
 * - A held entry in the hold tray, when there is one: the tray is laid out
 *   after the feed in the same scroll zone, so its last card is the column's
 *   last entry.
 * - Otherwise the last ROOT-level row: a response, a tool call, a prompt. A row
 *   nested in a bubble's sub-feed is part of its bubble, not an entry of its
 *   own, so the last row in document order is walked up to its outermost row.
 * - A root row drawn as a member of a tool group is shown behind a tab strip
 *   and may be the hidden one, so the group's own bubble stands for it.
 */
export function latestEntry(box: Element): Element | null {
  const held = box.querySelectorAll(HELD_ENTRY_SELECTOR);
  if (held.length > 0) return held[held.length - 1] ?? null;
  const rows = box.querySelectorAll(FEED_ROW_SELECTOR);
  let row = rows[rows.length - 1] ?? null;
  if (row === null) return null;
  const outer = (el: Element): Element | null => el.parentElement?.closest(FEED_ROW_SELECTOR) ?? null;
  for (let up = outer(row); up !== null; up = outer(up)) row = up;
  return row.closest(`.${FEED_GROUP_CLASS}`) ?? row;
}

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

/** The shell head on a row the caller has established is one. */
function shellHeadOf(row: FeedRow): FeedShell {
  if (row.row.case !== "shellHead") {
    throw new MalformedView("FeedRow.row", "the row is not a shell head bubble");
  }
  return row.row.value;
}
