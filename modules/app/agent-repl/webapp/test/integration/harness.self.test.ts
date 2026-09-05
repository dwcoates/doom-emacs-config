/**
 * THE HARNESS'S OWN CLAIMS, tested against the real harness.
 *
 * `settle()` is how every integration test waits, so a wrong settle is a
 * wrong suite. What is pinned here is the one property a healthy page under
 * load depends on: an outstanding request is waited for, not spun on.
 */
import { afterEach, describe, expect, it } from "vitest";

import { startHarness, type Harness } from "./harness";
import { log } from "../../src/log";

let harness: Harness | undefined;
let restoreFetch: (() => void) | undefined;

afterEach(async () => {
  await harness?.stop();
  harness = undefined;
  restoreFetch?.();
  restoreFetch = undefined;
});

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
    log("warn", "the view arrived thin", { operation: "harness.self.slow-answer" });
    // Act: the throttle releases the record on its two second window, and the
    // settle that follows finds the request in flight.
    await harness.tick(2_000);
    // Assert: the settle returned once the answer landed, and the record went.
    expect(harness.fake.calls("clientLog")).toHaveLength(1);
  }, 1_500); // the 200ms hold on top of the boot; the 900ms global is for pages that wait on nothing real
});
