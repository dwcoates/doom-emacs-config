/**
 * `sameOptional` IS HOW THE RAIL'S HEAD AND THE FEED'S HOVER STAMP COMPARE THEIR
 * OPTIONAL MESSAGES (`src/rpc/optional-equal.ts`).
 */
import { describe, expect, it } from "vitest";
import { codeOf } from "../../test/source-text.js";
import sidebarSource from "../../src/sidebar/sidebar.ts?raw";
import feedSource from "../../src/feed/feed-view.ts?raw";

describe("sameOptional", () => {
  it("is how the rail's head and the feed's hover stamp compare their optional messages", () => {
    // Arrange
    const sidebar = codeOf(sidebarSource);
    const feed = codeOf(feedSource);

    // Act / Assert
    expect([sidebar.includes("sameOptional(RosterVersionSchema"), feed.includes("sameOptional(FeedHoverStampSchema")]).toEqual([true, true]);
    expect([sidebar, feed].map((code) => /=== undefined \|\| \w+(\.\w+)? === undefined\) return/.test(code))).toEqual([false, false]);
  });
});
