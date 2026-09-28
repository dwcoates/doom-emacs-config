/**
 * THE PAGE'S ONE CONNECTION, END TO END.
 *
 * `test/rpc/page-streams.test.ts` covers the mux against a scripted transport:
 * its routing, its ordering, its latch. This file covers what only the whole
 * page can show — that the SIX standing views plus every bubble tail the
 * conversation grows really do land on ONE stream, and that the properties a
 * dedicated stream apiece used to give away for free still hold once they all
 * share a socket.
 *
 * WHY THIS IS NOT A REPEAT OF `streams.integration.test.ts`. That file asserts
 * each view's own decode contract and passes identically through the mux,
 * which is the point: nothing about a component's stream contract changed. The
 * cases here are the ones the mux INTRODUCED — a shared connection whose loss
 * takes every view at once, a subscription set that can leak, and a refusal
 * that must stay local to the view that earned it.
 */
import { afterEach, describe, expect, it } from "vitest";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import {
  WORKSPACE_ID,
  activityRow,
  feedId,
  feedPageSuccess,
  footerView,
  subagentUnit,
  topbarView,
  userPromptRow,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

describe("the page's one connection", () => {
  it("holds a single stream for every view it watches", async () => {
    // Arrange / Act: an ordinary boot, which mounts every standing view.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();

    // Assert: ONE page attached, and more subscriptions than the six
    // connections a browser would have allowed.
    expect(harness.fake.attachedPages()).toHaveLength(1);
    expect(harness.fake.pageSubscriptions().length).toBeGreaterThanOrEqual(6);
  });

  it("carries an expanded bubble's extra feed tail on that same stream", async () => {
    // Arrange: a subagent bubble, whose tail is the stream that used to make
    // the page's connection count grow with the conversation.
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    await harness.fake.awaitStream("watchFeed");
    const before = harness.fake.pageSubscriptions();

    // Act.
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
    await harness.settle();

    // Assert: a SECOND feed tail, and still one connection. This is the case
    // no fixed connection budget could have contained, because the count grows
    // with how many bubbles a reader opens.
    expect(harness.fake.attachedPages()).toHaveLength(1);
    expect(harness.fake.pageSubscriptions().length).toBe(before.length + 1);
    expect(harness.fake.liveStreams("watchFeed")).toBe(2);
  });

  it("ends exactly the bubble's subscription when it collapses", async () => {
    // Arrange: the bubble is open and its tail is subscribed.
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    await harness.fake.awaitStream("watchFeed");
    const before = harness.fake.pageSubscriptions();
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
    await harness.settle();

    // Act.
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStreamClosed("watchFeed", WORKSPACE_ID, "bubble");
    await harness.settle();

    // Assert: NOTHING LEAKS. The subscription set is exactly what it was
    // before the bubble opened — not one more, and not one fewer, so the root
    // feed's own tail was not taken down with it.
    expect(harness.fake.pageSubscriptions()).toEqual(before);
  });
});

describe("the page's stream dropping", () => {
  it("reports the degraded state the views share", async () => {
    // Arrange.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();

    // Act: the one connection dies, which is every view at once.
    harness.fake.endPageStream();
    await harness.settle();

    // Assert: a stream ending on its own is a transport failure, and the page
    // says so rather than sitting on views that will never update again.
    expect(harness.failureArms()).toContain("daemonUnreachable");
  });

  it("re-subscribes every view once the stream is back", async () => {
    // Arrange.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();
    const before = harness.fake.pageSubscriptions();

    // Act.
    harness.fake.endPageStream();
    await harness.settle();
    await harness.tick(60_000);
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();

    // Assert: the page attached again and every view that ended with the old
    // stream is riding the new one. The ids are freshly minted, so it is the
    // COUNT that carries the guarantee, not the names.
    expect(harness.fake.attachedPages()).toHaveLength(1);
    expect(harness.fake.pageSubscriptions()).toHaveLength(before.length);
  });

  it("replays each view exactly as its own rpc replays it", async () => {
    // Arrange: the footer says one thing while the page is connected.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    harness.fake.setFooter(
      WORKSPACE_ID,
      footerView({ status: "idle", substatus: "ready", tokensText: "before-the-drop" }),
    );
    await harness.settle();

    // Act: the stream drops, and the view MOVES ON while nothing is watching.
    // Nothing is pushed — there is no live subscriber to push to — so the only
    // way this text ever reaches the page is the re-subscribe's own replay.
    harness.fake.endPageStream();
    await harness.settle();
    harness.fake.setFooter(
      WORKSPACE_ID,
      footerView({ status: "idle", substatus: "ready", tokensText: "while-it-was-down" }),
    );
    await harness.tick(60_000);
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();

    // Assert: THE SAME REPLAY THE DEDICATED RPC GAVE. A reopened subscription
    // that came back empty would leave the reader looking at a view frozen at
    // whatever the dead stream last said — which is indistinguishable from a
    // healthy page until the moment it matters.
    expect(harness.$(".footer-tokens")?.textContent).toContain("while-it-was-down");
  });

  it("retracts the degraded state once the views are drawing again", async () => {
    // Arrange.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();

    // Act.
    harness.fake.endPageStream();
    await harness.settle();
    await harness.tick(60_000);
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();

    // Assert: the card is a statement about NOW, so a recovered page must not
    // keep wearing it.
    expect(harness.failureArms()).not.toContain("daemonUnreachable");
  });
});

describe("a view's planned ending", () => {
  it.each(["watchDaemon", "watchWorkspaceRoster"] as const)(
    "files no daemonUnreachable when %s ends after the daemon's planned ending",
    async (rpc) => {
      // Arrange.
      harness = await startHarness();
      await harness.fake.awaitStream(rpc);
      await harness.settle();

      // Act: the planned-ending frame, then the clean end it announces.
      harness.fake.pushPlannedEnding(rpc);
      await harness.settle();
      harness.fake.endStream(rpc);
      await harness.settle();

      // Assert: the daemon said the end was planned, so it is not a fault.
      expect(harness.failureArms()).not.toContain("daemonUnreachable");
    },
  );

  it.each(["watchDaemon", "watchWorkspaceRoster"] as const)(
    "still files daemonUnreachable when %s ends WITHOUT the planned ending",
    async (rpc) => {
      // Arrange.
      harness = await startHarness();
      await harness.fake.awaitStream(rpc);
      await harness.settle();

      // Act.
      harness.fake.endStream(rpc);
      await harness.settle();

      // Assert.
      expect(harness.failureArms()).toContain("daemonUnreachable");
    },
  );
});

describe("one view's subscription refused", () => {
  it("leaves every other view drawing", async () => {
    // Arrange: the footer's own open is refused, and nothing else is.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ title: "still-here" }));
    await harness.settle();

    // Act: the footer's subscription dies and its reopen is refused.
    harness.fake.failNext("watchFooter", "this view is refused");
    harness.fake.endStream("watchFooter");
    await harness.settle();
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ title: "drawn-after" }));
    await harness.settle();

    // Assert: A REFUSAL IS ONE VIEW'S, NOT THE PAGE'S. The topbar shares the
    // socket the footer just failed on and keeps drawing on it.
    expect(harness.$(".topbar-title")?.textContent).toContain("drawn-after");
  });

  it("reports the refused view's own failure", async () => {
    // Arrange.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();

    // Act.
    harness.fake.failNext("watchFooter", "this view is refused");
    harness.fake.endStream("watchFooter");
    await harness.settle();

    // Assert: the refusal reaches the caller's own stream loop as an ordinary
    // open failure, so it is filed rather than swallowed by the mux.
    expect(harness.failureArms()).toContain("daemonUnreachable");
  });

  it("keeps the page's one stream up through it", async () => {
    // Arrange.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    await harness.settle();

    // Act.
    harness.fake.failNext("watchFooter", "this view is refused");
    harness.fake.endStream("watchFooter");
    await harness.settle();

    // Assert: one view's refusal is not the connection's death. Taking the
    // stream down would turn a single refused view into every view's outage.
    expect(harness.fake.attachedPages()).toHaveLength(1);
  });
});

describe("the workspace moving away", () => {
  /**
   * THE PAGE IS PINNED TO ONE WORKSPACE. `ctx.workspace` is readonly and a
   * successor daemon is a different origin, so there is no in-page switch to
   * cover: the move is the whole of it. The page draws the notice, goes quiet,
   * and stops — and under the mux "stops" is one connection to let go of
   * rather than seven, which is what these two assert.
   */
  const moveAway = async (): Promise<void> => {
    harness = await startHarness();
    await harness.fake.awaitStream("watchWebWorkspace");
    const second = await harness.startSecondDaemon();
    harness.fake.transfer(WORKSPACE_ID, second.baseUrl);
    await harness.settle();
  };

  // The server-side close acknowledgement crosses the real Unix socket.
  it("lets go of the page's one stream", async () => {
    // Arrange / Act.
    await moveAway();
    const [page] = harness.fake.attachedPages();
    if (page !== undefined) await harness.fake.awaitPageDetached(page);

    // Assert: the connection is released rather than left reopening on backoff
    // for a workspace this page can never get back.
    expect(harness.fake.attachedPages()).toEqual([]);
  }, 1_500);

  it("draws nothing the old workspace pushes afterwards", async () => {
    // Arrange.
    await moveAway();
    const drawnBefore = harness.$(".topbar-title")?.textContent;

    // Act: the daemon it left behind keeps talking.
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ title: "from-the-old-daemon" }));
    await harness.settle();

    // Assert: A PAGE THAT HAS GONE QUIET DOES NOT DRAW. A frame from the
    // workspace's old home would be a view of a workspace that has moved on.
    expect(harness.$(".topbar-title")?.textContent).toBe(drawnBefore);
    expect(harness.$(".topbar-title")?.textContent).not.toContain("from-the-old-daemon");
  });
});

describe("a live feed tail on the shared stream", () => {
  it("draws a row pushed after the page loaded", async () => {
    // Arrange: THE ORIGINAL DEFECT. The root feed's tail was the seventh
    // stream, so it queued forever and no row produced after the page loaded
    // was ever drawn.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    await harness.settle();

    // Act.
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow("live-after-load"));
    await harness.settle();

    // Assert.
    expect(harness.$('[data-feed-row="row-1"]')?.textContent).toContain("live-after-load");
  });
});
