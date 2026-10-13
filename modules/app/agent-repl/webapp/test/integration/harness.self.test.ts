/**
 * THE HARNESS'S OWN CLAIMS, tested against the real harness.
 *
 * `settle()` is how every integration test waits, so a wrong settle is a
 * wrong suite. What is pinned here is the one property a healthy page under
 * load depends on: an outstanding request is waited for, not spun on.
 *
 * THE MUX CHANGED WHAT "IN FLIGHT" MEANS, so it is pinned here too. `settle`
 * counts a request in flight until its RESPONSE HEAD lands, which for a
 * standing stream is the accept. The page now holds ONE stream whose head
 * lands at boot and stays landed for the life of the document, so a view's
 * opening is no longer a request head at all — it is `SubscribePage`, a unary,
 * and a settle that returned before that unary answered would return before
 * the view it opens can have drawn anything.
 *
 * AND WHO PAYS THE COLD BOOT. Every file that boots the app pays its first
 * boot in `bootColdOnce`'s `beforeAll`, never in a test body; the check that
 * no file forgets to lives here, beside the harness it guards.
 */
import { readFileSync, readdirSync } from "node:fs";
import { resolve } from "node:path";
import { afterEach, describe, expect, it } from "vitest";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { log } from "../../src/log";
import { TITLE_REVEALED_ATTRIBUTE } from "../../src/feed/title-reveal.js";
import { CONVERSATION_DRAWN_ATTRIBUTE } from "../../src/feed/conversation-drawn.js";
import { ROOT_FEED } from "./fake-daemon";
import { WORKSPACE_ID, activityRow, feedId, toolCallRunningUnit } from "./fixtures";

let harness: Harness | undefined;
let restoreFetch: (() => void) | undefined;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
  harness = undefined;
  restoreFetch?.();
  restoreFetch = undefined;
});

/**
 * Hold every `SubscribePage` answer back by `ms` of REAL time, the same way.
 *
 * A view's open is this unary now, so this is what a slow view opening looks
 * like from the page's side.
 */
function holdSubscribeAnswers(ms: number): void {
  const realFetch = globalThis.fetch;
  const realSetTimeout = globalThis.setTimeout;
  globalThis.fetch = async (input, init) => {
    const url = typeof input === "string" ? input : input instanceof URL ? input.href : input.url;
    if (url.endsWith("/SubscribePage")) {
      await new Promise<void>((resolve) => realSetTimeout(resolve, ms));
    }
    return realFetch(input, init);
  };
  restoreFetch = () => {
    globalThis.fetch = realFetch;
  };
}

/**
 * Hold every ClientLog answer back by `ms` of REAL time. The harness captures
 * `globalThis.fetch` at start, so the hold is installed before it; the real
 * timer is taken now, before the harness fakes the clock.
 */
function holdClientLogAnswers(ms: number): void {
  const realFetch = globalThis.fetch;
  const realSetTimeout = globalThis.setTimeout;
  globalThis.fetch = async (input, init) => {
    const url = typeof input === "string" ? input : input instanceof URL ? input.href : input.url;
    if (url.endsWith("/ClientLog")) {
      await new Promise<void>((resolve) => realSetTimeout(resolve, ms));
    }
    return realFetch(input, init);
  };
  restoreFetch = () => {
    globalThis.fetch = realFetch;
  };
}

describe("settle with a request outstanding", () => {
  it("waits for a slow answer instead of exhausting the round cap", async () => {
    // Arrange: a quiet round costs ~1ms and the cap is 60 rounds, so a unary
    // that takes 200ms of real time outlives any amount of spinning.
    holdClientLogAnswers(200);
    harness = await startHarness();
    harness.installClientLogSink();
    harness.fake.clearCalls();
    log.warn("the view arrived thin", { operation: "harness.self.slow-answer" });
    // Act: the throttle releases the record on its two second window, and the
    // settle that follows finds the request in flight.
    await harness.tick(2_000);
    // Assert: the settle returned once the answer landed, and the record went.
    expect(harness.fake.calls("clientLog")).toHaveLength(1);
  }, 1_500); // the 200ms hold on top of the boot; the integration global is for pages that wait on nothing real
});

describe("settle with a view's subscription outstanding", () => {
  it("does not return before the page's own stream has attached", async () => {
    // Arrange: every view's open is held back by real time, so a settle that
    // counted only the ONE stream's head — which lands at accept, long before
    // any view is subscribed — would come back with nothing subscribed at all.
    holdSubscribeAnswers(50);

    // Act: the boot's own settle is the one under test.
    harness = await startHarness();

    // Assert: the page is attached and its views are subscribed by the time
    // the boot's settle returned. A settle that returned early would leave a
    // suite asserting on a page whose views had not opened yet — the exact
    // shape of a flake that only appears under load.
    expect(harness.fake.attachedPages()).toHaveLength(1);
    expect(harness.fake.pageSubscriptions().length).toBeGreaterThanOrEqual(6);
  }, 1_500); // several 50ms holds on top of the boot, for the same reason as above

  it("waits for a view's first frame, not merely for its subscription", async () => {
    // Arrange: a footer with text on it, so there is a first frame to miss.
    holdSubscribeAnswers(50);
    harness = await startHarness();

    // Act: no further settle — the boot's own is all this gets.
    // Assert: THE REPLAY IS ALREADY DRAWN. Each source registers and queues
    // its replay before `SubscribePage` answers, so the frame is on the wire
    // by the time the unary this settle waited for has landed; a subscription
    // whose registration happened after the answer would leave this empty.
    expect(harness.$(".footer-tokens")).not.toBeNull();
  }, 1_500); // same holds, same reason
});

/** This directory's test files that boot the app, read off disk. */
function bootingFiles(): { file: string; source: string }[] {
  const dir = resolve(process.cwd(), "test/integration");
  return readdirSync(dir)
    .filter((file) => file.endsWith(".test.ts"))
    .map((file) => ({ file, source: readFileSync(resolve(dir, file), "utf8") }))
    .filter(({ source }) => source.includes("startHarness("));
}

describe("the cold boot", () => {
  it("finds the files that boot the app", () => {
    // Arrange / Act: the scan the per-file check below runs over.
    const files = bootingFiles().map(({ file }) => file);

    // Assert: a scan that found nothing would pass every file vacuously.
    expect(files).toContain("harness.self.test.ts");
  });

  it.each(bootingFiles())("is paid before the first test of $file", ({ source }) => {
    // Arrange / Act: the file's own top-level statements.
    const calls = source.match(/^bootColdOnce\(\);$/gm) ?? [];

    // Assert: exactly one top-level call, so the file's first boot is warm.
    expect(calls).toHaveLength(1);
  });
});

/**
 * A ROW A LIVE PUSH DRAWS TYPES ITS TITLE OUT (title-reveal.ts), and
 * `finishTypeOuts` runs that type-out to its end on the faked frames.
 */
describe("finishing the page's title type-outs", () => {
  /** Boot, push a running tool call live, and settle. */
  async function pushedToolCall(): Promise<HTMLElement> {
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(toolCallRunningUnit(), { id: feedId("call-1") }));
    await harness.settle();
    const row = harness.row("call-1");
    if (row === null || row === undefined) throw new Error("the feed drew no row for call-1");
    return row;
  }

  it("shows the title whole once the type-outs are finished", async () => {
    // Arrange
    const row = await pushedToolCall();
    // Act
    await harness!.finishTypeOuts();
    // Assert
    expect([row.querySelector(`[${TITLE_REVEALED_ATTRIBUTE}]`), row.textContent?.includes("npm test")]).toEqual([
      null,
      true,
    ]);
  });
});

/**
 * A BOOTED PAGE HAS ITS CONVERSATION ON SCREEN: the boot runs the frames the
 * drawn mark waits for, so its record never lands inside a case, and every
 * test's page starts unmarked.
 */
describe("the boot's drawn mark", () => {
  it("is set by the time the boot returns", async () => {
    // Arrange, Act
    harness = await startHarness();
    // Assert
    expect(document.documentElement.hasAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe(true);
  });

  it("is cleared when the harness stops", async () => {
    // Arrange
    harness = await startHarness();
    // Act
    await harness.stop();
    // Assert
    expect(document.documentElement.hasAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe(false);
  });
});
