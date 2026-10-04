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
import type { FeedId, FeedWalkId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
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
  GetFeedPageNextSchema,
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

/** Which page of a feed's walk to ask for. */
export type FeedPageAsk = "first" | "next";

/**
 * Walk one feed's pages. `first` resets the walk to the newest page; `next`
 * continues older from wherever the daemon's walk for this feed stands.
 *
 * NO CURSOR EXISTS ON THE WIRE, so there is nothing for a caller to hold
 * between calls and nothing here to thread through.
 */
export function buildGetFeedPageRequest(
  workspace: WorkspaceRef,
  feed: FeedId | undefined,
  ask: FeedPageAsk,
  walk?: FeedWalkId,
): GetFeedPageRequest {
  // A `next` NAMES ITS WALK (the `has_more.walk` of the page it continues), so
  // the daemon finds it whichever connection carries the request.
  const request = create(GetFeedPageRequestSchema, feed === undefined ? { workspace } : { workspace, feed });
  request.page =
    ask === "first"
      ? { case: "first", value: create(GetFeedPageFirstSchema) }
      : { case: "next", value: create(GetFeedPageNextSchema, walk === undefined ? {} : { walk }) };
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
 * Bring ONE root-feed row into the loaded pages, every page between included.
 * TARGET is the row's FeedId exactly as the daemon served it.
 */
export function buildLoadFeedThroughRequest(
  workspace: WorkspaceRef,
  target: FeedId,
  walk?: FeedWalkId,
): LoadFeedThroughRequest {
  // The reader's own walk, advanced; unnamed, the daemon begins one at the top.
  return create(
    LoadFeedThroughRequestSchema,
    walk === undefined ? { workspace, target } : { workspace, target, walk },
  );
}
