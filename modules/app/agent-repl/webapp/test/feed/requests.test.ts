import { FeedWalkIdSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedWatchTokenSchema } from "../../../proto/gen/ts/agentrepl/v1/feed_token_pb";
import {
  buildGetFeedPageRequest,
  buildInterruptDetachedRequest,
  buildLoadFeedThroughRequest,
  buildOpenFeedRequest,
  buildWatchFeedRequest,
} from "../../src/feed/requests.js";
import { WORKSPACE, feedId } from "./harness.js";

describe("buildOpenFeedRequest", () => {
  it("addresses the workspace", () => {
    expect(buildOpenFeedRequest(WORKSPACE).workspace?.id).toBe("ws-1");
  });

  it("leaves the feed UNSET for the root feed, since absence is the address", () => {
    expect(buildOpenFeedRequest(WORKSPACE).feed).toBeUndefined();
  });

  it("echoes a sub-feed's id verbatim", () => {
    expect(buildOpenFeedRequest(WORKSPACE, feedId("row-9")).feed?.value).toBe("row-9");
  });
});

describe("buildWatchFeedRequest", () => {
  it("echoes the minted token verbatim", () => {
    const token = create(FeedWatchTokenSchema, { value: "tok:1" });
    expect(buildWatchFeedRequest(token).watch?.value).toBe("tok:1");
  });

  it("carries no workspace of its own, the token being workspace-scoped", () => {
    const token = create(FeedWatchTokenSchema, { value: "tok:1" });
    expect(Object.keys(buildWatchFeedRequest(token))).not.toContain("workspace");
  });
});

describe("buildGetFeedPageRequest", () => {
  it("asks for the newest page as `first`", () => {
    expect(buildGetFeedPageRequest(WORKSPACE, undefined, "first").page.case).toBe("first");
  });

  it("continues the daemon's own walk as `next`", () => {
    expect(buildGetFeedPageRequest(WORKSPACE, undefined, "next").page.case).toBe("next");
  });

  it("leaves the feed unset for the root feed's walk", () => {
    expect(buildGetFeedPageRequest(WORKSPACE, undefined, "next").feed).toBeUndefined();
  });

  it("echoes a sub-feed's address on its own walk", () => {
    expect(buildGetFeedPageRequest(WORKSPACE, feedId("b"), "next").feed?.value).toBe("b");
  });
});

describe("buildInterruptDetachedRequest", () => {
  it("targets the detached arm with the bubble row's own id", () => {
    const req = buildInterruptDetachedRequest(WORKSPACE, feedId("bubble-1"));
    expect(req.target).toEqual({ case: "detached", value: feedId("bubble-1") });
  });

  it("leaves confirm_agents false, being meaningless on this target", () => {
    expect(buildInterruptDetachedRequest(WORKSPACE, feedId("b")).confirmAgents).toBe(false);
  });
});

describe("buildLoadFeedThroughRequest", () => {
  it("addresses the workspace", () => {
    expect(buildLoadFeedThroughRequest(WORKSPACE, feedId("r-1")).workspace?.id).toBe("ws-1");
  });

  it("echoes the target's id verbatim", () => {
    expect(buildLoadFeedThroughRequest(WORKSPACE, feedId("r-1")).target?.value).toBe("r-1");
  });

  it("names the reader's walk when it has one", () => {
    expect(buildLoadFeedThroughRequest(WORKSPACE, feedId("r-1"), create(FeedWalkIdSchema, { value: "w-1" })).walk?.value).toBe("w-1");
  });

  it("names no walk when the reader has none", () => {
    expect(buildLoadFeedThroughRequest(WORKSPACE, feedId("r-1")).walk).toBeUndefined();
  });
});

describe("buildGetFeedPageRequest's walk", () => {
  it("names the walk a next continues", () => {
    const ask = buildGetFeedPageRequest(WORKSPACE, undefined, "next", create(FeedWalkIdSchema, { value: "w-2" })).page;
    expect(ask.case === "next" ? ask.value.walk?.value : undefined).toBe("w-2");
  });
});
