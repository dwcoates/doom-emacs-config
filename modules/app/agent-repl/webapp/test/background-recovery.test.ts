import { describe, it, expect } from "vitest";
import {
  BackgroundRecovery,
  RECOVERY_FULL_CHECK_EVERY_TICKS,
  type RecoveryTimerHost,
} from "../src/background-recovery.js";

interface Harness {
  recovery: BackgroundRecovery;
  dials: number;
  resyncs: string[];
  bannerClears: number;
  /** Run every registered heartbeat tick once. */
  tick: () => void;
  intervals: number[];
}

/**
 * A recovery wired to a hand-driven timer host, so a tick is an explicit act
 * rather than a wait. `hidden` sets document.visibilityState for the whole
 * harness: every assertion below is about what happens with NOBODY looking.
 */
function harness(opts: { current: boolean; hidden: boolean }): Harness {
  const state = { dials: 0, resyncs: [] as string[], bannerClears: 0 };
  const ticks: Array<() => void> = [];
  const intervals: number[] = [];
  const timers: RecoveryTimerHost = {
    setInterval: (callback, ms) => {
      ticks.push(callback);
      intervals.push(ms);
      return ticks.length;
    },
    clearInterval: () => {
      ticks.length = 0;
    },
  };
  // The page's visibility as the recovery path would see it. Nothing in
  // BackgroundRecovery reads it — that is the guarantee under test — so it is
  // installed as ambient truth the assertions can be read against.
  Object.defineProperty(globalThis, "document", {
    configurable: true,
    value: { visibilityState: opts.hidden ? "hidden" : "visible" },
  });
  const recovery = new BackgroundRecovery(
    {
      ensureConnected: () => {
        state.dials++;
      },
      isCurrent: () => opts.current,
      resync: (reason) => {
        state.resyncs.push(reason);
      },
      clearConnectionBanner: () => {
        state.bannerClears++;
      },
    },
    timers,
  );
  return {
    recovery,
    get dials() {
      return state.dials;
    },
    get resyncs() {
      return state.resyncs;
    },
    get bannerClears() {
      return state.bannerClears;
    },
    tick: () => {
      for (const t of [...ticks]) t();
    },
    intervals,
  };
}

describe("BackgroundRecovery", () => {
  it("dials the socket on a heartbeat tick while the page is hidden", () => {
    // Arrange
    const h = harness({ current: false, hidden: true });
    h.recovery.start();
    // Act
    h.tick();
    // Assert
    expect(h.dials).toBe(1);
  });

  it("resyncs on a hidden page's heartbeat tick without any visibility change", () => {
    // Arrange
    const h = harness({ current: true, hidden: true });
    h.recovery.start();
    // Act
    h.tick();
    // Assert
    expect(h.resyncs).toEqual(["recovery_heartbeat"]);
  });

  it("resyncs immediately on socket restore rather than waiting for a tick", () => {
    // Arrange
    const h = harness({ current: true, hidden: true });
    // Act
    h.recovery.recover("socket_restored");
    // Assert
    expect(h.resyncs).toEqual(["socket_restored"]);
  });

  it("clears the connection banner on a repair completed while hidden", () => {
    // Arrange
    const h = harness({ current: true, hidden: true });
    // Act
    h.recovery.recover("socket_restored");
    // Assert
    expect(h.bannerClears).toBe(1);
  });

  it("leaves the banner up when the socket is not current yet", () => {
    // Arrange
    const h = harness({ current: false, hidden: true });
    // Act
    h.recovery.recover("recovery_heartbeat");
    // Assert
    expect(h.bannerClears).toBe(0);
  });

  it("reports that no resync ran while the socket is still coming back", () => {
    // Arrange
    const h = harness({ current: false, hidden: true });
    // Act
    const repaired = h.recovery.recover("recovery_heartbeat");
    // Assert
    expect(repaired).toBe(false);
  });

  it("does not send a command over a socket that is not current", () => {
    // Arrange
    const h = harness({ current: false, hidden: true });
    // Act
    h.recovery.recover("recovery_heartbeat");
    // Assert
    expect(h.resyncs).toEqual([]);
  });

  it("arms only one heartbeat across repeated starts", () => {
    // Arrange
    const h = harness({ current: true, hidden: true });
    h.recovery.start();
    // Act
    h.recovery.start();
    // Assert
    expect(h.intervals.length).toBe(1);
  });

  it("stops ticking once stopped", () => {
    // Arrange
    const h = harness({ current: true, hidden: true });
    h.recovery.start();
    h.recovery.stop();
    // Act
    h.tick();
    // Assert
    expect(h.resyncs).toEqual([]);
  });

  it("repairs a visible page through the identical path", () => {
    // Arrange
    const h = harness({ current: true, hidden: false });
    // Act
    h.recovery.recover("visibilitychange_visible");
    // Assert
    expect(h.resyncs).toEqual(["visibilitychange_visible"]);
  });
});

/**
 * A heartbeat wired to a page that can report whether anything moved.
 *
 * THE COST BEING MEASURED IS A FULL StateSnapshot PER TICK. The daemon answers
 * every resync with the whole world, so an unconditional heartbeat asks it to
 * rebuild and ship that for every open page every interval whether or not
 * anything changed — 3,140 `re-armed on recovery_heartbeat` lines and 233
 * resyncs in a two-minute window with nothing wrong.
 */
function gatedHarness(opts: { current: boolean; pending: () => boolean; fullCheckEveryTicks?: number }) {
  const state = { dials: 0, resyncs: [] as string[], bannerClears: 0 };
  const ticks: Array<() => void> = [];
  const timers: RecoveryTimerHost = {
    setInterval: (callback) => {
      ticks.push(callback);
      return ticks.length;
    },
    clearInterval: () => {
      ticks.length = 0;
    },
  };
  const recovery = new BackgroundRecovery(
    {
      ensureConnected: () => {
        state.dials++;
      },
      isCurrent: () => opts.current,
      resync: (reason) => {
        state.resyncs.push(reason);
      },
      clearConnectionBanner: () => {
        state.bannerClears++;
      },
      hasPendingWork: opts.pending,
    },
    timers,
    5_000,
    opts.fullCheckEveryTicks ?? RECOVERY_FULL_CHECK_EVERY_TICKS,
  );
  return {
    recovery,
    state,
    tick: () => {
      for (const t of [...ticks]) t();
    },
  };
}

describe("a heartbeat on a page that is already current", () => {
  it("issues no resync at all while nothing has moved", () => {
    // Arrange — a current, caught-up, idle page: the snapshot a resync would
    // fetch is the one already applied here.
    const h = gatedHarness({ current: true, pending: () => false });
    h.recovery.start();

    // Act — several ordinary intervals pass with the agent idle.
    for (let i = 0; i < RECOVERY_FULL_CHECK_EVERY_TICKS - 1; i++) h.tick();

    // Assert — the whole world was not shipped even once.
    expect(h.state.resyncs).toEqual([]);
  });

  it("still dials on every tick, because that costs nothing and is local", () => {
    // Arrange
    const h = gatedHarness({ current: true, pending: () => false });
    h.recovery.start();
    // Act
    h.tick();
    h.tick();
    // Assert — the dial half is never what was expensive.
    expect(h.state.dials).toBe(2);
  });

  it("asks anyway on the periodic full check, because a page cannot always see that it is behind", () => {
    // Arrange — the skip is an optimization over what this page can observe,
    // and the page this module exists for is the one that observes wrongly.
    const h = gatedHarness({ current: true, pending: () => false, fullCheckEveryTicks: 3 });
    h.recovery.start();

    // Act
    h.tick();
    h.tick();
    expect(h.state.resyncs).toEqual([]);
    h.tick();

    // Assert — the unconditional probe survives, at a twelfth of the rate.
    expect(h.state.resyncs).toEqual(["recovery_heartbeat"]);
  });

  it("resyncs immediately on the tick where something did move", () => {
    // Arrange — quiet, then the mark moves.
    let moved = false;
    const h = gatedHarness({ current: true, pending: () => moved });
    h.recovery.start();
    h.tick();
    expect(h.state.resyncs).toEqual([]);

    // Act
    moved = true;
    h.tick();

    // Assert — the gate delays nothing that had a reason to go.
    expect(h.state.resyncs).toEqual(["recovery_heartbeat"]);
  });

  it("does not gate a repair driven by evidence rather than by the clock", () => {
    // Arrange — a socket that just came back is a FACT about the world, not a
    // guess, and is owed its resync no matter how quiet the page looks.
    const h = gatedHarness({ current: true, pending: () => false });
    h.recovery.start();
    h.tick();

    // Act
    h.recovery.recover("socket_restored");

    // Assert
    expect(h.state.resyncs).toEqual(["socket_restored"]);
  });

  it("dials but never resyncs while the socket is not current", () => {
    // Arrange — nothing can carry a command yet.
    const h = gatedHarness({ current: false, pending: () => true });
    h.recovery.start();
    // Act
    h.tick();
    // Assert
    expect(h.state.dials).toBe(1);
    expect(h.state.resyncs).toEqual([]);
  });
});
