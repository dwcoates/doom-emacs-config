/**
 * The keep-alive cadence and the yield it owes a real prompt.
 *
 * WHAT THIS GUARDS: that a user's prompt never builds on the harness's own
 * housekeeping. The failure mode being excluded is a real turn answered in a
 * context whose last several exchanges are keep-alives — the model reads them,
 * the user paid for them, and nothing on any surface says they are there.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  isKeepalivePrompt,
  KEEPALIVE_INTERVAL_MS,
  KEEPALIVE_PROMPT_MARKER,
  KeepaliveCadence,
  KeepaliveRewind,
  keepalivePromptText,
  REAL_SCHEDULER,
} from "../../src/engine/keepalive.js";
import { CACHE_TTL_5M_MS } from "../../src/engine/cold.js";
import { ManualScheduler } from "./fakes.js";

describe("the keep-alive prompt", () => {
  it("BEGINS with the marker the store and sidecar classify on", () => {
    expect(keepalivePromptText().startsWith(KEEPALIVE_PROMPT_MARKER)).toBe(true);
  });

  it("recognizes its own prompts by that prefix", () => {
    expect(isKeepalivePrompt(keepalivePromptText())).toBe(true);
  });

  it("does not mistake a user's prompt for one", () => {
    expect(isKeepalivePrompt("ship the feature")).toBe(false);
  });
});

describe("the interval", () => {
  it("beats inside the vendor's five-minute ephemeral tier", () => {
    // A beat at or past the tier would let the cache lapse between beats, which
    // is the exact cost the cadence exists to avoid paying.
    expect(KEEPALIVE_INTERVAL_MS).toBeLessThan(CACHE_TTL_5M_MS);
  });
});

describe("the yield obligation", () => {
  it("owes nothing when no keep-alive has run", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord("uuid-1", false);

    expect(rewind.obligation()).toBeUndefined();
  });

  it("names the last REAL record as the resume anchor", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord("real-1", false);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on a keep-alive's own record", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord("real-1", false);
    rewind.noteRecord("keepalive-1", true);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("counts the keep-alive turns being discarded", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord("real-1", false);
    rewind.noteKeepaliveTurn();
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.discardedKeepaliveTurns).toBe(2);
  });

  it("owes nothing when no real record exists to rewind TO", () => {
    // Truncating to nothing would discard the session's own opening.
    const rewind = new KeepaliveRewind();
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()).toBeUndefined();
  });

  it("clears the debt once the rewind is settled", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord("real-1", false);
    rewind.noteKeepaliveTurn();
    rewind.settled();

    expect(rewind.obligation()).toBeUndefined();
  });
});

describe("the cadence", () => {
  it("beats when it is running", () => {
    const scheduler = new ManualScheduler();
    let beats = 0;
    new KeepaliveCadence(() => beats++, 1, scheduler).start();

    scheduler.fire();

    expect(beats).toBe(1);
  });

  it("does not double the cadence on a second start", () => {
    const scheduler = new ManualScheduler();
    const cadence = new KeepaliveCadence(() => undefined, 1, scheduler);

    cadence.start();
    cadence.start();

    expect(scheduler.handlers).toHaveLength(1);
  });

  it("SKIPS the beat while a turn is in flight", () => {
    // A keep-alive submitted into an open turn would be a second submitter.
    const scheduler = new ManualScheduler();
    let beats = 0;
    const cadence = new KeepaliveCadence(() => beats++, 1, scheduler);
    cadence.start();
    cadence.pause();

    scheduler.fire();

    expect(beats).toBe(0);
  });

  it("beats again once the turn ends", () => {
    const scheduler = new ManualScheduler();
    let beats = 0;
    const cadence = new KeepaliveCadence(() => beats++, 1, scheduler);
    cadence.start();
    cadence.pause();
    cadence.resume();

    scheduler.fire();

    expect(beats).toBe(1);
  });

  it("clears its interval when stopped", () => {
    const scheduler = new ManualScheduler();
    const cadence = new KeepaliveCadence(() => undefined, 1, scheduler);
    cadence.start();

    cadence.stop();

    expect(scheduler.cleared).toBe(1);
  });

  it("is a no-op to stop one that never started", () => {
    const scheduler = new ManualScheduler();

    new KeepaliveCadence(() => undefined, 1, scheduler).stop();

    expect(scheduler.cleared).toBe(0);
  });
});

/**
 * REAL_SCHEDULER is the cadence's default (every unit test above injects
 * ManualScheduler instead), wired in production whenever `createEngine` is
 * not handed a scheduler override. Pin the real setInterval/clearInterval
 * wiring directly rather than only through the fake.
 */
describe("REAL_SCHEDULER", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("setInterval beats the handler on the given cadence", () => {
    const beats: number[] = [];
    REAL_SCHEDULER.setInterval(() => beats.push(1), 10);

    vi.advanceTimersByTime(35);

    expect(beats.length).toBe(3);
  });

  it("clearInterval stops further beats", () => {
    const beats: number[] = [];
    const handle = REAL_SCHEDULER.setInterval(() => beats.push(1), 10);

    vi.advanceTimersByTime(15);
    REAL_SCHEDULER.clearInterval(handle);
    vi.advanceTimersByTime(100);

    expect(beats.length).toBe(1);
  });
});
