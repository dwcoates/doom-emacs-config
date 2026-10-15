import { FeedRowOrderSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedWatchTokenSchema } from "../../../proto/gen/ts/agentrepl/v1/feed_token_pb";
import {
  buildGetFeedPageRequest,
  buildInterruptDetachedRequest,
  buildInterruptMergeTabRequest,
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
  const OLDEST = create(FeedRowOrderSchema, { key: "k-1" });

  it("asks for the newest page as `first`", () => {
    expect(buildGetFeedPageRequest(WORKSPACE, undefined, { case: "first" }).page.case).toBe("first");
  });

  it("asks for the page above the oldest held row as `older_than`", () => {
    const ask = buildGetFeedPageRequest(WORKSPACE, undefined, { case: "olderThan", oldestHeld: OLDEST }).page;
    expect(ask.case === "olderThan" ? ask.value.oldestHeld?.key : undefined).toBe("k-1");
  });

  it("leaves the feed unset for the root feed", () => {
    expect(buildGetFeedPageRequest(WORKSPACE, undefined, { case: "first" }).feed).toBeUndefined();
  });

  it("echoes a sub-feed's address", () => {
    expect(buildGetFeedPageRequest(WORKSPACE, feedId("b"), { case: "first" }).feed?.value).toBe("b");
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

describe("buildInterruptMergeTabRequest", () => {
  it("targets the merge_tab arm with the tab row's own id", () => {
    const req = buildInterruptMergeTabRequest(WORKSPACE, feedId("tab-1"));
    expect(req.target).toEqual({ case: "mergeTab", value: feedId("tab-1") });
  });

  it("leaves confirm_agents false, being meaningless on this target", () => {
    expect(buildInterruptMergeTabRequest(WORKSPACE, feedId("t")).confirmAgents).toBe(false);
  });
});

describe("buildLoadFeedThroughRequest", () => {
  it("addresses the workspace", () => {
    expect(buildLoadFeedThroughRequest(WORKSPACE, feedId("r-1")).workspace?.id).toBe("ws-1");
  });

  it("echoes the target's id verbatim", () => {
    expect(buildLoadFeedThroughRequest(WORKSPACE, feedId("r-1")).target?.value).toBe("r-1");
  });

  it("names the reader's oldest held row when it holds one", () => {
    expect(
      buildLoadFeedThroughRequest(WORKSPACE, feedId("r-1"), create(FeedRowOrderSchema, { key: "k-1" })).oldestHeld?.key,
    ).toBe("k-1");
  });

  it("names no oldest held row when the reader holds none", () => {
    expect(buildLoadFeedThroughRequest(WORKSPACE, feedId("r-1")).oldestHeld).toBeUndefined();
  });
});
