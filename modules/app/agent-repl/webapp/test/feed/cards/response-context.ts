/**
 * The response card's fixtures: a row context on a page, and a response
 * message. The card's drawing tests (test/feed/cards/response.test.ts) and its
 * stylesheet rules (rules/feed/cards/response.rules.test.ts) draw through them.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import { FeedIdSchema, FeedResponseSchema, FeedRowSchema, type FeedResponse } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import type { RowContext } from "../../../src/feed/cards/context.js";
import type { PageSeen } from "../../../src/page-seen.js";
import { testAppContext } from "../../rpc/app-context.js";
import { fakePageSeen } from "../../page-seen-fake.js";
import { orderFor } from "../../feed-order.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };

/** The reveal pace every live draw carries: the daemon's thinking default. */
export const PACE = { minCharsPerSecond: 45, maxCharsPerSecond: 55, bankLimitMs: 1500 };

/** A row context on a page SEEN says; the response card calls no verb of its own. */
export function rowContext(previous?: HTMLElement, seen: PageSeen = fakePageSeen()): RowContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {});
  });
  return {
    ctx: testAppContext({
      client: createAgentReplClient(transport),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker: createTicker(1000),
      failures: SINK,
      seen,
      composerEnabled: false,
    }),
    feed: "root",
    arrival: "page",
    row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: "row-1" }), order: orderFor("row-1") }),
    revealRow: async () => true,
    previous,
  };
}

export function response(init: MessageInitShape<typeof FeedResponseSchema>): FeedResponse {
  return create(FeedResponseSchema, init);
}
