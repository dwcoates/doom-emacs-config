/**
 * THE NEWS DIGEST — the daemon's daily Claude news, drawn over the feed in
 * every webview from the `WatchDaemon` stream's `news_digest` standing, and
 * dismissed through `DismissNewsDigest` with the id it was served. The overlay
 * comes down only on the daemon's `none` push.
 */
import { afterEach, describe, expect, it } from "vitest";
import type { MessageInitShape } from "@bufbuild/protobuf";

import type { DismissNewsDigestRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_dismiss_news_digest_pb";
import type { NewsDigestStandingSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";

import { bootColdOnce, startHarness, type Harness } from "./harness";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** A standing digest of one backend item. */
const shown: MessageInitShape<typeof NewsDigestStandingSchema> = {
  standing: {
    case: "shown",
    value: {
      id: { value: "digest-1" },
      header: { title: { text: "Claude news · Oct 2" }, period: { fromMs: 1_727_769_600_000n, toMs: 1_727_856_000_000n } },
      sections: [
        {
          heading: { text: "Affects the agent-repl backend" },
          kind: { kind: { case: "backend", value: {} } },
          items: [
            {
              title: { text: "SDK drops subscription billing" },
              summary: { text: "The SDK now needs an API key." },
              links: [{ label: "Release notes", url: "https://fixture.test/releases/v9" }],
            },
          ],
        },
      ],
      sources: {
        sources: [{ name: "Agent SDK releases", url: "https://fixture.test/releases", outcome: { case: "read", value: { newEntries: 1 } } }],
      },
    },
  },
};

/** A page with the digest pushed and drawn. */
async function withDigest(): Promise<void> {
  harness = await startHarness();
  await harness.fake.awaitStream("watchDaemon");
  harness.fake.pushNewsDigest(shown);
  await harness.settle();
}

describe("the news digest overlay", () => {
  it("draws a pushed digest over the feed", async () => {
    // Arrange / Act
    await withDigest();
    // Assert
    expect(harness.$('[data-component="news-digest"]')?.hidden).toBe(false);
    expect(harness.$(".news-digest-title")?.textContent).toBe("Claude news · Oct 2");
  });

  it("dismisses with the served id", async () => {
    // Arrange
    await withDigest();
    // Act
    harness.$("[data-news-digest-close]")?.click();
    await harness.settle();
    // Assert
    expect(harness.fake.calls<DismissNewsDigestRequest>("dismissNewsDigest").map((req) => req.id?.value)).toEqual([
      "digest-1",
    ]);
  });

  it("takes the digest down on the daemon's none", async () => {
    // Arrange
    await withDigest();
    // Act
    harness.fake.pushNewsDigest({ standing: { case: "none", value: {} } });
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="news-digest"]')?.hidden).toBe(true);
  });

  it("opens an item's link through OpenExternal", async () => {
    // Arrange
    await withDigest();
    // Act
    harness.$(".news-digest-links [data-external-link]")?.click();
    await harness.settle();
    // Assert
    expect(harness.fake.calls<{ url: string }>("openExternal").map((req) => req.url)).toEqual([
      "https://fixture.test/releases/v9",
    ]);
  });
});
