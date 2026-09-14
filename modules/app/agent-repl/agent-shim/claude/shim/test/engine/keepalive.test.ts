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
import type { SdkMessage } from "../../src/sdk/types.js";
import { ManualScheduler } from "./fakes.js";

/** An assistant record, the ONE shape the SDK says `resumeSessionAt` accepts. */
const assistant = (uuid: string): SdkMessage =>
  ({
    type: "assistant",
    uuid,
    session_id: "vendor-1",
    parent_tool_use_id: null,
    message: { role: "assistant", content: [] },
  }) as unknown as SdkMessage;

/** A vendor message of some other type that nonetheless carries a uuid. */
const other = (type: string, uuid: string, subtype?: string): SdkMessage =>
  ({ type, uuid, session_id: "vendor-1", ...(subtype === undefined ? {} : { subtype }) }) as unknown as SdkMessage;

/** The real turn a record arrived under. */
const realTurn = { turnId: "turn-1", keepalive: false } as const;
/** The shim's own keep-alive turn. */
const keepaliveTurn = { turnId: "keepalive-1", keepalive: true } as const;

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
    rewind.noteRecord(assistant("uuid-1"), realTurn);

    expect(rewind.obligation()).toBeUndefined();
  });

  it("names the last REAL assistant record as the resume anchor", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("reports which turn the anchor came from", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.anchorTurnId).toBe("turn-1");
  });

  it("never anchors on a keep-alive's own assistant record", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(assistant("keepalive-answer-1"), keepaliveTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on the `system:init` uuid", () => {
    // The uuid that killed the owner's session on 2026-09-14: required on the
    // init message, and naming no transcript record.
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(other("system", "19e047a0-init", "init"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on a `result` uuid", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(other("result", "b64f2741-result", "success"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on the user echo", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(other("user", "echo-1"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on a stream event", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(other("stream_event", "stream-1"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on an assistant record that belongs to no open turn", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("idle-1"), undefined);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()).toBeUndefined();
  });

  it("counts the keep-alive turns being discarded", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
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

  it("owes nothing once the anchor is cleared at a boundary", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.clearAnchor("the vendor compacted the conversation");
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()).toBeUndefined();
  });

  it("reports no anchor at all once one is cleared", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.clearAnchor("the query was replaced without a rewind");

    expect(rewind.anchorUuid()).toBeUndefined();
  });

  it("clears the debt once the rewind is settled", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();
    rewind.settled();

    expect(rewind.obligation()).toBeUndefined();
  });

  it("KEEPS the anchor across a settled rewind, which does not cross a boundary", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();
    rewind.settled();

    expect(rewind.anchorUuid()).toBe("real-1");
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
