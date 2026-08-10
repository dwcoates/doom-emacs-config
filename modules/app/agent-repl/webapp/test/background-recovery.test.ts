import { describe, it, expect } from "vitest";
import {
  BackgroundRecovery,
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
