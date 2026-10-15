/**
 * requests — every agentrepl request the feed universe sends, built in ONE
 * place, symmetrically with the drawing side's `draw<Message>` functions.
 *
 * WHY A BUILDER PER REQUEST. The four identifier spaces are never
 * interchangeable, and the mistakes they invite are all mistakes of
 * CONSTRUCTION: a `FeedId` passed where a `FeedWatchToken` belongs, a
 * workspace omitted from a request that requires it, a bubble's own id
 * forgotten so a sub-feed's page walk answers for the root feed. A named
 * builder per request type is where the compiler can hold that: the argument
 * list names what the value IS, and every call site in the feed reads as the
 * verb it is issuing.
 *
 * NOTHING HERE PARSES OR MINTS AN IDENTIFIER. Every id and token is a value
 * the daemon served, echoed verbatim.
 */
import { create } from "@bufbuild/protobuf";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FeedId, FeedRowOrder } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  OpenFeedRequestSchema,
  type OpenFeedRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import {
  WatchFeedRequestSchema,
  type WatchFeedRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import {
  GetFeedPageFirstSchema,
  GetFeedPageOlderThanSchema,
  GetFeedPageRequestSchema,
  type GetFeedPageRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_feed_page_pb";
import {
  LoadFeedThroughRequestSchema,
  type LoadFeedThroughRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_load_feed_through_pb";
import type { FeedWatchToken } from "../../../proto/gen/ts/agentrepl/v1/feed_token_pb";
import {
  InterruptRequestSchema,
  type InterruptRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";

/**
 * Open ONE feed: the workspace's root feed when FEED is undefined, or the
 * sub-feed a bubble row addresses when it is that row's own id.
 *
 * The `feed` field is `optional` on the wire precisely so that "the root feed"
 * is the ABSENCE of an address rather than a sentinel value, so an undefined
 * argument is left unset instead of being filled with anything.
 */
export function buildOpenFeedRequest(
  workspace: WorkspaceRef,
  feed?: FeedId,
): OpenFeedRequest {
  return create(OpenFeedRequestSchema, feed === undefined ? { workspace } : { workspace, feed });
}

/**
 * Tail the feed the TOKEN names.
 *
 * The token is workspace-scoped by mint and pins the tail to begin exactly
 * after the page `OpenFeed` answered with, which is why this request carries
 * neither a workspace nor a feed id of its own: adding either would be a
 * second, drift-prone statement of what the token already says.
 */
export function buildWatchFeedRequest(watch: FeedWatchToken): WatchFeedRequest {
  return create(WatchFeedRequestSchema, { watch });
}

/**
 * Which page of a feed to ask for: the newest, or the page above the oldest
 * row the reader holds, named by that row's order exactly as the daemon
 * served it.
 */
export type FeedPageAsk = { case: "first" } | { case: "olderThan"; oldestHeld: FeedRowOrder };

/**
 * Ask for one page of a feed: its newest page, or the page of rows sorting
 * before the reader's oldest held row.
 *
 * THE READER'S OLDEST HELD ROW IS THE POSITION. The daemon keeps no walk, so
 * whatever this feed freed, repainted or loaded through a jump, the page asked
 * for is exactly the one above what it holds.
 */
export function buildGetFeedPageRequest(
  workspace: WorkspaceRef,
  feed: FeedId | undefined,
  ask: FeedPageAsk,
): GetFeedPageRequest {
  const request = create(GetFeedPageRequestSchema, feed === undefined ? { workspace } : { workspace, feed });
  request.page =
    ask.case === "first"
      ? { case: "first", value: create(GetFeedPageFirstSchema) }
      : { case: "olderThan", value: create(GetFeedPageOlderThanSchema, { oldestHeld: ask.oldestHeld }) };
  return request;
}

/**
 * Stop a DETACHED bubble's work, by the bubble row's own FeedId.
 *
 * `confirm_agents` is deliberately left at its default: the challenge it
 * answers exists only for the TURN target (interrupting a turn while detached
 * agents are live), and the proto states it is meaningless on this one.
 */
export function buildInterruptDetachedRequest(
  workspace: WorkspaceRef,
  detached: FeedId,
): InterruptRequest {
  return create(InterruptRequestSchema, {
    workspace,
    target: { case: "detached", value: detached },
  });
}

/**
 * Stop the agent a live AGENTIC merge tab draws, by the tab row's own FeedId:
 * the merge workspace's agent for an async merge (which also reveals that
 * workspace), the workspace's own for a sync merge.
 */
export function buildInterruptMergeTabRequest(
  workspace: WorkspaceRef,
  tab: FeedId,
): InterruptRequest {
  return create(InterruptRequestSchema, {
    workspace,
    target: { case: "mergeTab", value: tab },
  });
}

/**
 * Bring ONE root-feed row into the loaded pages, every page between included.
 * TARGET is the row's FeedId exactly as the daemon served it.
 */
export function buildLoadFeedThroughRequest(
  workspace: WorkspaceRef,
  target: FeedId,
  oldestHeld?: FeedRowOrder,
): LoadFeedThroughRequest {
  // The walk begins above the reader's oldest held row; a reader holding none
  // names none, and the daemon begins at the newest page.
  return create(
    LoadFeedThroughRequestSchema,
    oldestHeld === undefined ? { workspace, target } : { workspace, target, oldestHeld },
  );
}
