// The relay that holds a row the daemon asked this page to reveal until the
// feed can reveal it. See src/lifecycle/reveal-relay.ts.
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";

import { createRevealRelay } from "../../src/lifecycle/reveal-relay.js";
import { FeedIdSchema, type FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";

const row = (value: string): FeedId => create(FeedIdSchema, { value });

/** A feed reveal that records every row it was asked for. */
function recordingReveal(): { asked: string[]; reveal: (id: FeedId) => Promise<boolean> } {
  const asked: string[] = [];
  return { asked, reveal: (id) => (asked.push(id.value), Promise.resolve(true)) };
}

describe("createRevealRelay", () => {
  it("reveals a row asked before the feed bound once it binds", () => {
    // Arrange
    const relay = createRevealRelay();
    const feed = recordingReveal();
    relay.request(row("r1"));
    // Act
    relay.bind(feed.reveal);
    // Assert
    expect(feed.asked).toEqual(["r1"]);
  });

  it("reveals a row asked after the feed bound at once", () => {
    // Arrange
    const relay = createRevealRelay();
    const feed = recordingReveal();
    relay.bind(feed.reveal);
    // Act
    relay.request(row("r1"));
    // Assert
    expect(feed.asked).toEqual(["r1"]);
  });

  it("keeps only the latest row asked before the feed bound", () => {
    // Arrange
    const relay = createRevealRelay();
    const feed = recordingReveal();
    relay.request(row("r1"));
    relay.request(row("r2"));
    // Act
    relay.bind(feed.reveal);
    // Assert
    expect(feed.asked).toEqual(["r2"]);
  });

  it("reveals nothing on a bind with no row asked", () => {
    // Arrange
    const relay = createRevealRelay();
    const feed = recordingReveal();
    // Act
    relay.bind(feed.reveal);
    // Assert
    expect(feed.asked).toEqual([]);
  });
});
