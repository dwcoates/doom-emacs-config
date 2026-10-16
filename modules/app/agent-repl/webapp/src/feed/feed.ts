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
import { markConversationDrawn } from "./conversation-drawn.js";
import { installUnshownTrim } from "./unshown-trim.js";
import { pagePresence, type Presence } from "../presence.js";
import { log } from "../log.js";
import { create } from "@bufbuild/protobuf";
import { reportClientFailure } from "../rpc/link.js";
import { MalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import {
  READER_TOGGLE_EVENT,
  autoCollapseFor,
  cappedSectionsOf,
  collapseSection,
  expandedSectionAt,
  installClickExpand,
  isExpanded,
  expandSection,
} from "../expand.js";
import { controlPlaneFailed } from "../failure/sink.js";
import {
  createOutOfViewCollapse,
  type CollapsingEntry,
  type OutOfViewCause,
} from "./out-of-view-collapse.js";
import { installBackgroundClear } from "./background-click.js";
import {
  governedRowAt,
  installBubbleSelect,
  OUT_OF_VIEW_COLLAPSE_ATTRIBUTE,
} from "./bubble-selection.js";
import {
  LEFT_VIEW_REQUEST,
  createSelectionVisibility,
} from "./selection-visibility.js";
import { guardMalformed } from "../rpc/guard.js";
import { selectFeedRow } from "./select-feed-row.js";
import { foldMergeBubble } from "./fold-merge-bubble.js";
import { mergeQueueOfferOverlay } from "./merge/queue-offer.js";
import { refreshHasMore } from "./bubble-more.js";
import { refreshTitleFolds } from "./title-fold.js";
import { applyFeedTextScale } from "./feed-text-scale.js";
import {
  TailFollow,
  holdViewOnClicks,
  installIntentScroll,
  observeScrollBox,
  scrollFloor,
  feedAnchorRows,
  revealGeometry,
} from "../scroll.js";
import {
  OpenFeedResponseSchema,
  type OpenFeedResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import {
  WatchFeedResponseSchema,
  type WatchFeedHeldChange,
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
import { mountBubble, type WireFold } from "./bubble.js";
import { isMergeBubble } from "./merge-bubble.js";
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
import {
  HELD_ENTRY_SELECTOR,
  mountHeldSection,
  type HeldSection,
} from "../held/section.js";
import { tick, stopTicking } from "./ticking.js";
import { REVEAL_ATTRIBUTE, syncSelectedEntry } from "./selected-entry.js";

/** How long a revealed row wears the highlight that says "here". */
export const REVEAL_HIGHLIGHT_MS = 1500;

/** What the chip's `request` evidence names a jump's failed expansion as. */
export const JUMP_EXPAND_REQUEST = "expand the entry a jump landed on";

/** The scroll cause a jump centers under: the footer's own, or every other jump's. */
type JumpCause = "detachedWorkSelected" | "entryJumped";

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
  /**
   * Elements OUTSIDE the scroll box that draw part of the root feed (the
   * page's gate dock, gate-dock.ts). A click in one holds the feed's view
   * exactly as a click in the box does (`holdViewOnClicks`).
   */
  clickZones?: readonly HTMLElement[];
  /**
   * Whether the host shows this page (`presence.ts`); the root feed frees its
   * older rows each time it stops (`unshown-trim.ts`). Defaults to the page's.
   */
  presence?: Pick<Presence, "shown" | "onChange">;
  /**
   * Where the root feed's held section draws: the page's own `#hold-tray`,
   * after the rows in the same scroll zone. Absent in a fixture rendering the
   * feed on its own, which then draws no held section (and still requires the
   * daemon to state one).
   */
  heldHost?: HTMLElement;
}

export interface FeedHandle extends Handle {
  /**
   * The reader picked a detached-work item in the expanded footer: open
   * whatever bubbles stand above its card, mark it, and scroll it into view
   * (`detachedWorkSelected`, one of the closed set of scroll causes).
   */
  readonly selectDetachedWork: (id: FeedId) => Promise<boolean>;
  /**
   * This page's workspace was just switched to (`sidebar/selection-edge.ts`):
   * park the feed at its tail and follow (`workspaceSelected`, one of the
   * closed set of scroll causes).
   */
  readonly workspaceSelected: () => void;
}

/** Mount the root feed into HOST. */
export function mountFeed(
  host: HTMLElement,
  ctx: AppContext,
  deps: FeedDeps,
): FeedHandle {
  log.info("mounting the feed", { operation: "feed.mount", context: {} });

  const scrollBox = deps.scrollBox ?? host.parentElement ?? null;
  const tail =
    scrollBox === null
      ? null
      : new TailFollow(
          scrollBox,
          () => {
            const entry = latestEntry(scrollBox);
            return entry === null ? null : revealGeometry(scrollBox, entry);
          },
          // THE SCROLL ANCHOR'S ROWS: the root feed's own, in HOST. WebKit
          // has no native scroll anchoring, so the tail owner holds the
          // content under a reader who is off the tail (scroll.ts).
          // THE ROWS ARE THE ROOT BODY'S ROW LIST, not the feed host's own
          // children (the page-error slot and the body mount), which held one
          // box for the whole feed and so never moved when a row above the
          // reader changed height.
          feedAnchorRows(scrollBox, () => rootRows(host)),
          // THE FLOOR UNDER A CLICK-HELD VIEW: the box cannot clamp the view
          // a reader's click holds (scroll.ts, `ScrollFloor`).
          scrollFloor(scrollBox),
        );
  // The tail owner's OTHER two inputs, which only a mount holding the real
  // element can give it: the box's scroll events and the box's size changes.
  // The size half is the footer occlusion (see `observeScrollBox`) — the
  // docked footer settling after a render shrinks this box, and a tail parked
  // before that shrink is left below the fold with the last bubble clipped.
  const unobserve =
    scrollBox === null || tail === null
      ? null
      : observeScrollBox(scrollBox, tail);
  // A click in a zone outside the box that draws part of the feed holds the
  // view as a click in the box does.
  const unholdZones = tail === null ? [] : (deps.clickZones ?? []).map((zone) => holdViewOnClicks(zone, tail));
  // INTENT-ARMED INNER SCROLLING, on the same box. A capped section keeps the
  // wheel only while the reader has deliberately entered it; otherwise the
  // wheel redirects to the feed. Without it, a section the feed scrolled under
  // a still cursor captures the next gesture and scrolling gets stuck in the
  // bubble (scroll.ts's installIntentScroll). No box, no sections to gate.
  // An OPEN (expanded) section is the exception: it keeps its whole wheel, and
  // nothing chains from it to the feed (expand.ts says which sections are open).
  const intentScroll =
    scrollBox === null
      ? null
      : installIntentScroll(scrollBox, (el) =>
          expandedSectionAt(el, scrollBox),
        );
  // THE SELECTED ROW LEAVING THE VIEWPORT ENDS THE SELECTION: the daemon is
  // told (`left_view`), and its `none.stay` push drops the mark in place.
  const selectionVisibility =
    scrollBox === null
      ? null
      : createSelectionVisibility(scrollBox, (row) => {
          void guardMalformed(
            ctx,
            "feed.selection-left-view",
            selectFeedRow(
              ctx,
              { case: "leftView", value: { row } },
              LEFT_VIEW_REQUEST,
            ),
          );
        });
  // AN ENTRY A JUMP EXPANDED CLOSES ONCE IT IS WHOLLY OUT OF VIEW (owner
  // request, 2026-10-01; out-of-view-collapse.ts). Only what the jump itself
  // opened is watched; the reader's own expansions are left to them.
  const outOfView = scrollBox === null ? null : createOutOfViewCollapse(scrollBox);
  // THE READER RETURNING TO THE TAIL CLOSES EVERY EXPANDED ENTRY (owner ruling,
  // 2026-10-01), hooked to the tail owner's one re-latch.
  tail?.onTailReached(() => {
    collapseEveryExpandedEntry();
  });
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
  //
  // The same hook runs when the page's AutoCollapse owner (expand.ts) closes an
  // open section the reader scrolled away from or left, since that close goes
  // through the one collapse a click uses.
  //
  // THE READER'S CLICK NEVER MOVES THE FEED (owner requests, 2026-10-15 and
  // 2026-10-16, superseding the expand-centering rulings of 2026-09-29,
  // 2026-09-30 and 2026-10-14). ANY click in the feed (expanding or
  // collapsing an item, answering a question or a permission, pressing any
  // card's control) holds the view before anything changes: the scroll box
  // takes the hold itself on every click on its way in (`holdViewOnClicks`,
  // scroll.ts), so no click site asks for it. This is specific to the click:
  // a jump that expands an entry, or a selection stepped from Emacs, still
  // centers it.
  //
  // A READER'S TOGGLE MAKES THE ENTRY THEIRS: an entry a jump expanded and the
  // reader then toggled by hand is no longer the jump's to close. The one
  // exception is a root-feed THINKING bubble or INTERIM response a click
  // opens: it closes again once wholly out of view (`watchOwnClick`).
  //
  // A ROOT-FEED PROMPT OR RESPONSE BUBBLE'S BOX IS THE SELECTION'S
  // (bubble-selection.ts), a thinking or interim one's apart: a click on it selects
  // instead of toggling, and the auto-collapse never closes it, because
  // selected and expanded are one state the daemon owns.
  const ownedBySelection = (section: HTMLElement): boolean =>
    governedRowAt(section, host) !== null;
  const uninstallExpand = installClickExpand(
    host,
    undefined,
    (section, expanded) => {
      afterSectionToggle(section);
      releaseOutOfView(section);
      if (!expanded) return;
      watchOwnClick(section);
      intentScroll?.arm(section);
    },
    ownedBySelection,
  );
  const onReaderToggle = (event: Event): void => {
    if (!(event.target instanceof Element)) return;
    releaseOutOfView(event.target);
  };
  host.addEventListener(READER_TOGGLE_EVENT, onReaderToggle);

  const root: FeedController = createFeedController({
    ctx,
    host,
    feed: "root",
    renderers: deps.renderers,
    body: defaultBubbleBody,
    revealRow,
    bubble: bubbleFor,
    bodyContext: feedContext(),
    scroll:
      scrollBox === null || tail === null
        ? undefined
        : { box: scrollBox, tail },
    selectionVisibility: selectionVisibility ?? undefined,
    onSelectionExpand: afterSelectionToggle,
  });

  // A PAGE THE HOST STOPS SHOWING FREES ITS OLDER ROWS (unshown-trim.ts),
  // while its stream keeps drawing every update for the switch back.
  const uninstallTrim = installUnshownTrim({
    presence: deps.presence ?? pagePresence(),
    ctx,
    trim: () => root.freeOlderThanNewestPage(),
  });

  // A CLICK ON A ROOT-FEED PROMPT OR RESPONSE BUBBLE selects it, or, on the
  // selected one, deselects it where the reader is (owner ruling, 2026-10-01).
  const uninstallSelect = installBubbleSelect(host, ctx);

  // A CLICK ON THE FEED OUTSIDE ANY BUBBLE ends the feed's selection
  // (owner ruling, 2026-09-23). It asks the daemon, which owns the selection;
  // the daemon's cleared push then parks the tail through `applySelection`.
  const uninstallClear =
    scrollBox === null
      ? null
      : installBackgroundClear(scrollBox, ctx, () => root.selectionActive());

  // THE ROOT FEED'S HELD SECTION (held/section.ts): drawn after the rows, fed
  // by this feed's own open and watch.
  const held: HeldSection | null =
    deps.heldHost === undefined ? null : mountHeldSection(deps.heldHost, ctx, { promptHeld });

  openWatch();

  return {
    selectDetachedWork,
    workspaceSelected,
    dispose,
  };

  /**
   * A HELD PROMPT JUST LANDED PUTS THE READER AT THE TAIL (owner ruling,
   * 2026-09-23), as a sent prompt does: called only after the held section
   * drew the card, and only on its first draw. It parks at the TAIL rather than on the
   * card, so whatever was drawn after it is included and the follow keeps
   * tracking. A fixture feed with no scroll box has no tail to park.
   */
  function promptHeld(turn: string): void {
    if (tail === null) {
      log.debug(`a newly held prompt ${turn} found no scroll box to park`, {
        operation: "feed.held-prompt-unparked",
        context: { turn },
      });
      return;
    }
    log.debug(`a newly held prompt ${turn} parked the feed at its tail`, {
      operation: "feed.held-prompt-parked",
      context: { turn },
    });
    tail.promptHeld();
  }

  /**
   * A SWITCH TO THIS WORKSPACE PUTS THE READER AT THE TAIL (owner ruling,
   * 2026-10-02): the feed lands on its newest row with the follow re-armed,
   * whichever path made the switch. A fixture feed with no scroll box has no
   * tail to park.
   */
  function workspaceSelected(): void {
    if (tail === null) {
      log.debug("a switch to this workspace found no scroll box to park", {
        operation: "feed.workspace-selected-unparked",
        context: {},
      });
      return;
    }
    log.debug("a switch to this workspace parked the feed at its tail", {
      operation: "feed.workspace-selected-parked",
      context: {},
    });
    tail.workspaceSelected();
  }

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
      // No row arrived: the feed itself was opened, as a page is.
      arrival: "page",
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
        if (response.held !== undefined) {
          applyHeldChange(response.held);
          return;
        }
        root.upsert(requireMessage(response.row, "WatchFeedResponse.row"));
      },
    });
  }

  /**
   * One change of the held section, applied whole BEFORE the page can paint:
   * every row that moved first (an item that settled, drawn at the bottom, or
   * a row whose item returned, as its removal), then the section itself with
   * the folds the change made. So no frame ever draws an item in both places
   * or in neither (WatchFeedHeldChange).
   */
  function applyHeldChange(change: WatchFeedHeldChange): void {
    for (const row of change.rows) root.upsert(row);
    const section = requireMessage(change.held, "WatchFeedHeldChange.held");
    held?.apply(section, change.folds);
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
    // THE OPEN IS TIMED AT THE PAGE: what a page waits between asking for
    // its conversation and holding the answer is the first paint's critical
    // path, and the daemon's own record (daemon.server.open_feed) says how
    // much of it the daemon spent.
    const askedAt = performance.now();
    log.info("asked for the root feed's newest page", {
      operation: "feed.open-asked",
      context: {},
    });
    const response: OpenFeedResponse = await callUnary(
      ctx,
      "OpenFeed",
      (c) => c.openFeed(buildOpenFeedRequest(ctx.workspace)),
      OpenFeedResponseSchema,
    );
    log.info("the root feed's newest page arrived", {
      operation: "feed.open-answered",
      context: { rpc_ms: Math.round(performance.now() - askedAt) },
    });
    if (disposed || signal.aborted) return;
    const result = requireCase(response.result, "OpenFeedResponse.result");
    switch (result.case) {
      case "success": {
        root.applyPage(
          requireMessage(result.value.page, "OpenFeedSuccess.page"),
          "replace",
        );
        // THE ROOT FEED'S OPEN STATES ITS HELD SECTION, as of the same pin as
        // the page: the tail then carries every later change of it.
        const section = requireMessage(result.value.held, "OpenFeedSuccess.held");
        held?.apply(section);
        markConversationDrawn("page");
        const token = requireMessage(
          result.value.watch,
          "OpenFeedSuccess.watch",
        );
        yield* ctx.streams.watch("feed", buildWatchFeedRequest(token), signal);
        return;
      }
      case "error":
        // Marked FIRST: a refusal whose cause cannot be read still ends the
        // wait of a tab held for this page.
        markConversationDrawn("refused");
        log.error("the daemon refused to open the workspace's root feed", {
          operation: "feed.root-open-refused",
          context: {
            arm: requireCase(result.value.cause ?? {}, "OpenFeedError.cause")
              .case,
          },
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
      scroll:
        scrollBox === null || tail === null
          ? undefined
          : { box: scrollBox, tail },
    };
    if (unitCase(row) === "merge") {
      return mountBubble({
        ...shared,
        body: deps.renderers.mergeBody,
        initialFolded: mergeFoldOf(row).folded,
        foldOf: mergeFoldOf,
        merge: true,
        // A PINNED MERGE BUBBLE CANNOT BE FOLDED: while the merge is in
        // flight it stands at the feed's bottom as the one thing happening
        // there. Once it settles where it stands, it folds like any bubble.
        collapsible: (current) => current.order?.pinned === undefined,
        // The merge-queue question an interrupt raised stands over the tabs.
        overlay: mergeQueueOfferOverlay(ctx, mergeOf),
        // THE READER'S FOLD OUTLIVES THE PAGE AND WINS (owner ruling,
        // 2026-10-08): it is recorded on the daemon's durable head, so a
        // reload, a page and a daemon restart all draw the bubble the way the
        // reader left it, over the daemon's default.
        readerFold: (folded) =>
          void guardMalformed(
            ctx,
            "feed.merge-fold",
            foldMergeBubble(ctx, requireMessage(row.id, "FeedRow.id"), folded),
          ),
      });
    }
    // A subagent row ships no fold, so it starts collapsed and transfers
    // nothing but its head until the reader asks for more -- synchronous and
    // detached alike, since the two are one bubble that moved placement.
    return mountBubble({
      ...shared,
      body: defaultBubbleBody,
      initialFolded: true,
    });
  }

  /**
   * The fold on a merge row's head, and who decided it. An unset decider is a
   * malformed row.
   */
  function mergeFoldOf(row: FeedRow): WireFold {
    const fold = requireMessage(
      requireMessage(mergeOf(row).head, "FeedMerge.head").fold,
      "FeedMergeHead.fold",
    );
    const decided = requireCase(fold.decidedBy, "FeedMergeFold.decided_by");
    switch (decided.case) {
      case "daemon":
        return { folded: fold.folded, byReader: false };
      case "reader":
        return { folded: fold.folded, byReader: true };
      default: {
        const other: { case: string } = decided;
        return unreachableArm("FeedMergeFold.decided_by", other.case);
      }
    }
  }

  // ---- reveal -----------------------------------------------------------

  /**
   * Every jump but the footer's (a breadcrumb, a hook's gated-call link): the
   * same jump the footer makes (`reveal`), centered under `entryJumped`. Owner
   * request, 2026-10-01, replacing the 2026-09-23 rule that these two only
   * marked their target and left the scroll to the reader: every jump to a
   * feed entry expands it and centers it.
   */
  function revealRow(id: FeedId): Promise<boolean> {
    return reveal(id, "entryJumped");
  }

  /** The footer's detached-work selection (`detachedWorkSelected`). */
  function selectDetachedWork(id: FeedId): Promise<boolean> {
    return reveal(id, "detachedWorkSelected");
  }

  /**
   * THE ONE JUMP TO A FEED ENTRY: bring the row onto the page, EXPAND the
   * entry, CENTER it under CAUSE, and mark it (`land`). The expansion is the
   * jump's, and is undone once the entry is wholly out of view (`outOfView`).
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
  async function reveal(
    id: FeedId,
    cause: JumpCause,
    loaded = false,
  ): Promise<boolean> {
    const here = findAcrossOpenFeeds(root, id);
    if (here !== null) {
      await land(here, id, cause);
      return true;
    }
    log.debug(
      "the reveal target is not drawn; asking the daemon where it lives",
      {
        operation: "feed.reveal-probe",
        context: { row: id.value },
      },
    );
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
      // A row older than the loaded pages is not drawn and answers no feed
      // either: walk the pages in before giving up on it.
      const through = await loadThenReveal(id, id, cause, loaded);
      if (through !== null) return through;
      // The target is not a feed of its own — a shell bubble's row is the case
      // the ruling names — so the jump degrades to scroll-if-rendered, which is
      // exactly what the search above already tried.
      log.warn("the daemon refused to open the reveal target's feed", {
        operation: "feed.reveal-refused",
        context: { row: id.value },
      });
      reportClientFailure(
        "feed_not_tailing",
        "the daemon refused the feed's reveal probe",
      );
      return false;
    }
    if (result.case !== "success")
      return unreachableArm("OpenFeedResponse.result", armName(result));
    // The probe's token is deliberately ABANDONED: the walk below opens each
    // bubble properly, which mints the token that bubble will actually tail.
    const page = requireCase(
      requireMessage(result.value.page, "OpenFeedSuccess.page").result,
      "FeedPage.result",
    );
    if (page.case !== "success") return false;
    const crumbs = requireMessage(
      page.value.breadcrumbs,
      "FeedPageSuccess.breadcrumbs",
    ).crumbs;
    const opened: BubbleLike[] = [];
    const absent: { target: FeedId | null } = { target: null };
    const walked = await walk(crumbs, id, opened, absent);
    // A container the walk opened is the jump's expansion too, and is watched
    // like the entry itself, whether or not the walk went on to the end.
    for (const bubble of opened) trackJumpedBubble(bubble);
    if (!walked) {
      // A ROOT container the page does not hold sits on an older page: the
      // walk to THAT row loads it, and the jump then starts over.
      if (absent.target === null) return false;
      return (await loadThenReveal(absent.target, id, cause, loaded)) ?? false;
    }
    const found = findAcrossOpenFeeds(root, id);
    if (found === null) {
      const through = await loadThenReveal(id, id, cause, loaded);
      if (through !== null) return through;
      log.warn(
        "the breadcrumb walk finished without the reveal target appearing",
        {
          operation: "feed.reveal-lost",
          context: { row: id.value, crumbs: crumbs.length },
        },
      );
      return false;
    }
    await land(found, id, cause);
    return true;
  }

  /**
   * The row the jump wants is on a page the reader has not loaded: ask the
   * daemon to walk every page down to TARGET into the ROOT feed, then jump
   * again to WANTED (never walking twice: LOADED). Null when the walk does not
   * apply (already walked, or the root feed holds no older page), so the
   * caller's own failure path stands. A failed walk answers false and moves
   * nothing: the daemon words the failure in the footer, not this end.
   */
  async function loadThenReveal(
    target: FeedId,
    wanted: FeedId,
    cause: JumpCause,
    loaded: boolean,
  ): Promise<boolean | null> {
    if (loaded || !root.hasMoreHistory()) return null;
    log.debug("the reveal target is on an unloaded page; walking to it", {
      operation: "feed.reveal-load-through",
      context: { target: target.value, row: wanted.value },
    });
    if ((await root.loadThrough(target)) !== "reached") return false;
    return reveal(wanted, cause, true);
  }

  /**
   * Expand each container top-down, each awaiting its own open — and STOP the
   * moment the row being revealed is on the page.
   *
   * ONLY WHAT SELECTING THE ROW REQUIRES IS OPENED. The probe answers the crumbs
   * of the row's OWN feed, and for a bubble head (a subagent's, a shell's) the
   * last crumb is that head itself: the row is already drawn once its container
   * is open, so expanding it too would open a bubble the reader never asked
   * for. Each crumb is therefore expanded only while the row is still not
   * found.
   */
  async function walk(
    crumbs: readonly FeedBreadcrumb[],
    id: FeedId,
    opened: BubbleLike[],
    absent: { target: FeedId | null },
  ): Promise<boolean> {
    let controller: FeedController = root;
    for (const crumb of crumbs) {
      if (findAcrossOpenFeeds(root, id) !== null) {
        log.debug(
          "the reveal target is drawn; the walk opens nothing further",
          {
            operation: "feed.reveal-walk-stopped",
            context: { row: id.value, crumb: crumb.target?.value ?? "unset" },
          },
        );
        return true;
      }
      const target = requireMessage(crumb.target, "FeedBreadcrumb.target");
      const bubble = bubbleOn(controller, target);
      if (bubble === null) {
        if (controller === root) absent.target = target;
        log.warn("a breadcrumb names a bubble this feed does not hold", {
          operation: "feed.reveal-crumb-absent",
          context: { crumb: target.value },
        });
        return false;
      }
      const wasOpen = bubble.isExpanded();
      if (!(await bubble.expand())) return false;
      if (!wasOpen) opened.push(bubble);
      const child = bubble.child();
      if (child === null) return false;
      controller = child;
    }
    return true;
  }

  /** The bubble for TARGET on CONTROLLER, if it holds one. */
  function bubbleOn(
    controller: FeedController,
    target: FeedId,
  ): BubbleLike | null {
    const element = controller.findRowElement(target);
    if (element === null) return null;
    for (const bubble of controller.bubbles()) {
      if (element.contains(bubble.element)) return bubble;
    }
    return null;
  }

  /**
   * Land a jump on ELEMENT (the row ID names): expand the entry, then center
   * it under CAUSE, reading the EXPANDED layout, and mark it, briefly, as the
   * one meant.
   */
  async function land(
    element: HTMLElement,
    id: FeedId,
    cause: JumpCause,
  ): Promise<void> {
    // A grouped member sits in an inactive tab is HIDDEN and has no layout box;
    // bring its tab to the front, so the landing is on a member that is
    // actually drawn (tool-group.ts). A member outside any group is left alone.
    activateGroupedMember(element);
    await expandForJump(element, id);
    if (disposed) return;
    log.debug("landed on a jump's row, centering it", {
      operation: "feed.reveal-landed",
      context: { row: id.value, cause },
    });
    if (scrollBox !== null && tail !== null) {
      const geometry = revealGeometry(scrollBox, element);
      if (cause === "detachedWorkSelected") tail.detachedWorkSelected(geometry);
      else tail.entryJumped(geometry);
    }
    // STATED on the row, not only styled: "this is the row you asked for" is
    // a fact about the row while it stands, and a jump's caller has no other
    // way to see that the landing happened. The MARK is drawn on the row's
    // card (selected-entry.ts), derived from this fact.
    element.setAttribute(REVEAL_ATTRIBUTE, "true");
    syncSelectedEntry(element);
    const deadline = ctx.ticker.now() + REVEAL_HIGHLIGHT_MS;
    tick(element, ctx.ticker, (nowMs) => {
      if (nowMs < deadline) return;
      stopTicking(element);
      element.removeAttribute(REVEAL_ATTRIBUTE);
      syncSelectedEntry(element);
    });
  }

  /**
   * EXPAND THE ENTRY A JUMP LANDED ON (owner request, 2026-10-01), and watch
   * it so it closes again once wholly out of view. The entry is the row's
   * sub-feed bubble when it holds one (a subagent's, a shell's, a merge's),
   * otherwise every capped section the row itself owns (a tool card's fold, a
   * hook card's boxes). An entry ALREADY open is the reader's: it is left as
   * it is and never watched. A bubble the daemon would not open is an error
   * on the chip; the jump still centers what is drawn.
   */
  async function expandForJump(row: HTMLElement, id: FeedId): Promise<void> {
    const bubble = bubbleOfRow(root, row);
    if (bubble !== null) {
      if (bubble.isExpanded()) {
        log.debug(
          "a jump landed on a bubble already open; it is left as it is",
          {
            operation: "feed.jump-expand-skipped",
            context: { row: id.value, entry: "subFeed" },
          },
        );
        return;
      }
      if (!(await bubble.expand())) {
        log.error("a jump could not expand the bubble it landed on", {
          operation: "feed.jump-expand-failed",
          context: { row: id.value },
        });
        ctx.failures.report(
          controlPlaneFailed(
            JUMP_EXPAND_REQUEST,
            "the daemon did not open the entry's feed",
          ),
        );
        return;
      }
      if (isMergeBubble(bubble.element)) {
        // A MERGE BUBBLE NEVER COLLAPSES AUTOMATICALLY (owner ruling,
        // 2026-10-08): the one a jump opened stays open until the reader
        // closes it, so no watch closes it on leaving the view.
        log.debug("a jump opened a merge bubble; it is never watched, only the reader folds it", {
          operation: "feed.jump-merge-unwatched",
          context: { row: id.value },
        });
        return;
      }
      trackJumpedBubble(bubble);
      return;
    }
    const sections = sectionsOwnedBy(row);
    if (
      sections.length === 0 ||
      sections.some((section) => isExpanded(section))
    ) {
      log.debug("a jump landed on an entry with nothing closed to expand", {
        operation: "feed.jump-expand-skipped",
        context: {
          row: id.value,
          entry: sections.length === 0 ? "none" : "open",
        },
      });
      return;
    }
    for (const section of sections) expandSection(section, afterSectionToggle);
    log.debug("a jump expanded the entry it landed on", {
      operation: "feed.jump-expanded",
      context: { row: id.value, sections: sections.length },
    });
    outOfView?.track(ownSectionsEntry(row, "jump"));
  }

  /**
   * ROW as an entry that closes once out of view, opened by CAUSE: open while
   * any section it owns itself is, and closed through the one collapse of
   * each. The sections are read off the row at the moment of asking, so a
   * section a redraw replaced is found on the element that is drawn.
   */
  function ownSectionsEntry(row: HTMLElement, cause: OutOfViewCause): CollapsingEntry {
    return {
      row,
      cause,
      isExpanded: () =>
        sectionsOwnedBy(row).some((section) => isExpanded(section)),
      collapse: () => {
        for (const section of sectionsOwnedBy(row)) {
          if (isExpanded(section)) collapseSection(section, afterSectionToggle);
        }
      },
    };
  }

  /**
   * A CLICK THAT OPENS A ROOT-FEED THINKING BUBBLE OR INTERIM RESPONSE (owner
   * specification, 2026-10-14) only expands it, with no selection
   * (bubble-selection.ts `expandsOnOwnClick`), so the selection's
   * leaving-the-view end no longer closes it: the out-of-view watch does
   * instead. Any other section a click opens stays the reader's.
   */
  function watchOwnClick(section: HTMLElement): void {
    const row = section.closest<HTMLElement>(FEED_ROW_SELECTOR);
    if (row === null || !row.hasAttribute(OUT_OF_VIEW_COLLAPSE_ATTRIBUTE)) return;
    outOfView?.track(ownSectionsEntry(row, "ownClick"));
  }

  /** Watch a bubble a jump opened, keyed by its row. */
  function trackJumpedBubble(bubble: BubbleLike): void {
    const row = bubble.element.closest<HTMLElement>(FEED_ROW_SELECTOR);
    if (row === null) {
      log.error("a bubble a jump opened hangs in no feed row", {
        operation: "feed.jump-bubble-rowless",
        context: { element: bubble.element.className },
      });
      throw new Error("feed: a bubble a jump opened hangs in no feed row");
    }
    outOfView?.track({
      row,
      cause: "jump",
      isExpanded: () => bubble.isExpanded(),
      collapse: () => bubble.collapse(),
    });
  }

  /**
   * What a capped section's toggle redraws, whoever toggled it: the bubble's
   * "more below" affordance and the title folds the section owns.
   */
  function afterSectionToggle(section: HTMLElement): void {
    refreshHasMore(section);
    // A card's title fold follows the card's fold (title-fold.ts), so a toggle
    // re-measures the titles it owns as well as the section itself.
    refreshTitleFolds(section);
  }

  /**
   * What the selection's expand and collapse of a governed bubble re-measures:
   * a click's toggle minus the jump release, since the selection push, not a
   * toggle of the reader's, opened or closed it. Its viewport (centered or
   * held) is the push's own (`applySelection`).
   */
  function afterSelectionToggle(section: HTMLElement, expanded: boolean): void {
    afterSectionToggle(section);
    if (expanded) intentScroll?.arm(section);
  }

  /** The reader toggled something inside EL's row: the row is theirs now. */
  function releaseOutOfView(el: Element): void {
    const row = el.closest<HTMLElement>(FEED_ROW_SELECTOR);
    if (row !== null) outOfView?.release(row);
  }

  /**
   * THE READER RETURNED TO THE TAIL (owner ruling, 2026-10-01): every expanded
   * entry collapses, ONCE. The jump watches are dropped first, since what they
   * would close is closed here; then every open capped section on the page
   * (the auto-collapse owner's `tailReached`), then every open root bubble,
   * whose collapse discards whatever was open inside it.
   *
   * A MERGE BUBBLE IS EXEMPT (owner ruling, 2026-10-08): a merge bubble the
   * reader opened stays open until the reader closes it.
   */
  function collapseEveryExpandedEntry(): void {
    outOfView?.clear();
    const sections = autoCollapseFor(host.ownerDocument).collapseOutside(
      "tailReached",
      null,
    );
    let bubbles = 0;
    for (const bubble of root.bubbles()) {
      if (!bubble.isExpanded()) continue;
      if (isMergeBubble(bubble.element)) continue;
      bubble.collapse();
      bubbles += 1;
    }
    const record = {
      operation: "feed.tail-reached-collapse",
      context: { sections, bubbles },
    };
    if (sections + bubbles === 0) {
      log.debug(
        "the reader returned to the tail; nothing was expanded",
        record,
      );
      return;
    }
    log.info(
      "the reader returned to the tail; every expanded entry collapses",
      record,
    );
  }

  function dispose(): void {
    if (disposed) return;
    disposed = true;
    watch?.cancel();
    uninstallTrim();
    unobserve?.();
    for (const unhold of unholdZones) unhold();
    intentScroll?.uninstall();
    uninstallExpand();
    uninstallSelect();
    host.removeEventListener(READER_TOGGLE_EVENT, onReaderToggle);
    held?.dispose();
    uninstallClear?.();
    selectionVisibility?.dispose();
    outOfView?.dispose();
    root.dispose();
  }
}

/** Where the root feed's body draws its rows: the row list in the body mount. */
const ROOT_ROWS_SELECTOR = ":scope > .feed-body > .feed-rows";

/**
 * The root feed's row list inside HOST. The body mounts it when the root
 * controller is built, before any layout is read; a host without one is a
 * drawing fault, reported once per host at ERROR, and the host stands in so
 * the scroll owner reads a box rather than throwing inside a scroll event.
 */
export function rootRows(host: Element): Element {
  const rows = host.querySelector(ROOT_ROWS_SELECTOR);
  if (rows !== null) return rows;
  if (!missingRootRows.has(host)) {
    missingRootRows.add(host);
    log.error("the root feed draws no row list; the scroll anchor reads the feed host", {
      operation: "feed.root-rows-missing",
      context: { selector: ROOT_ROWS_SELECTOR },
    });
  }
  return host;
}

/** The hosts whose missing row list was already reported. */
const missingRootRows = new WeakSet<Element>();

/** Every feed row's element, at whatever depth of sub-feed it is drawn. */
const FEED_ROW_SELECTOR = "[data-feed-row]";

/**
 * The capped sections ROW owns itself: every one whose nearest feed row is
 * ROW, so a section of a row nested inside it (a sub-feed's, a grouped
 * member's) is that row's, not this one's.
 */
function sectionsOwnedBy(row: HTMLElement): HTMLElement[] {
  return cappedSectionsOf(row).filter(
    (section) => section.closest(FEED_ROW_SELECTOR) === row,
  );
}

/**
 * The sub-feed bubble ROW holds itself, searched across CONTROLLER and every
 * OPEN sub-feed under it, or null when the row is no bubble's.
 */
function bubbleOfRow(
  controller: FeedController,
  row: HTMLElement,
): BubbleLike | null {
  for (const bubble of controller.bubbles()) {
    if (bubble.element.closest(FEED_ROW_SELECTOR) === row) return bubble;
    const child = bubble.isExpanded() ? bubble.child() : null;
    if (child === null || !bubble.element.contains(row)) continue;
    const found = bubbleOfRow(child, row);
    if (found !== null) return found;
  }
  return null;
}

/** The row's element, searched across the root feed and every OPEN sub-feed. */

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
  const outer = (el: Element): Element | null =>
    el.parentElement?.closest(FEED_ROW_SELECTOR) ?? null;
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
    throw new MalformedView(
      "FeedRow.row",
      "the row is not a detached subagent",
    );
  }
  return row.row.value;
}

/** The subagent unit on a row the caller has established is one. */
function subagentOf(row: FeedRow): FeedSubagent {
  if (row.row.case !== "activity" || row.row.value.unit.case !== "subagent") {
    throw new MalformedView(
      "FeedTurnActivity.unit",
      "the row is not a subagent bubble",
    );
  }
  return row.row.value.unit.value;
}

/** The merge unit on a row the caller has established is one. */
function mergeOf(row: FeedRow): FeedMerge {
  if (row.row.case !== "activity" || row.row.value.unit.case !== "merge") {
    throw new MalformedView(
      "FeedTurnActivity.unit",
      "the row is not a merge bubble",
    );
  }
  return row.row.value.unit.value;
}

/** The shell head on a row the caller has established is one. */
function shellHeadOf(row: FeedRow): FeedShell {
  if (row.row.case !== "shellHead") {
    throw new MalformedView(
      "FeedRow.row",
      "the row is not a shell head bubble",
    );
  }
  return row.row.value;
}
