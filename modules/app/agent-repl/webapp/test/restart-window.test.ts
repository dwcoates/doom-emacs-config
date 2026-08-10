import { describe, it, expect } from "vitest";
import {
  RestartWindow,
  RESTARTING_INDICATOR,
  MAX_QUIET_WINDOW_MS,
  type RestartAnnouncement,
} from "../src/restart-window.js";

const MINT = 1_000_000;

function announcement(overrides: Partial<RestartAnnouncement> = {}): RestartAnnouncement {
  return {
    cause: "deploy-all rebuilt the daemon",
    expectedOutageMs: 60_000,
    stopShims: true,
    atMs: MINT,
    ...overrides,
  };
}

/** A window on a hand-driven clock: every passage of time is an explicit act. */
function harness(startMs = MINT) {
  const clock = { nowMs: startMs };
  const logs: Array<{ level: string; message: string }> = [];
  const window = new RestartWindow({
    now: () => clock.nowMs,
    log: (level, message) => logs.push({ level, message }),
  });
  return { window, clock, logs };
}

describe("RestartWindow", () => {
  it("enters quiet mode on an announcement", () => {
    const { window } = harness();
    expect(window.announce(announcement())).toBe(true);
    expect(window.isRestarting()).toBe(true);
  });

  it("renders the quiet indicator while the window is open", () => {
    const { window } = harness();
    window.announce(announcement());
    expect(window.indicator()).toBe(RESTARTING_INDICATOR);
  });

  it("suppresses the disconnect alarm while the window is open", () => {
    const { window } = harness();
    window.announce(announcement());
    expect(window.suppressesDisconnectAlarm()).toBe(true);
  });

  it("resumes honest alarms once the window expires", () => {
    const { window, clock } = harness();
    window.announce(announcement({ expectedOutageMs: 60_000 }));
    clock.nowMs = MINT + 60_000;
    expect(window.suppressesDisconnectAlarm()).toBe(false);
  });

  it("shows no indicator once the window expires", () => {
    const { window, clock } = harness();
    window.announce(announcement({ expectedOutageMs: 60_000 }));
    clock.nowMs = MINT + 60_001;
    expect(window.indicator()).toBeNull();
  });

  it("warns when the window expires with no revived daemon", () => {
    const { window, clock, logs } = harness();
    window.announce(announcement({ expectedOutageMs: 60_000 }));
    clock.nowMs = MINT + 60_000;
    window.isRestarting();
    expect(logs.some((l) => l.level === "warn" && l.message.includes("EXPIRED"))).toBe(true);
  });

  it("clears the mode on a resynced socket before the window would expire", () => {
    const { window, clock } = harness();
    window.announce(announcement({ expectedOutageMs: 60_000 }));
    clock.nowMs = MINT + 5_000;
    window.onResynced();
    expect(window.isRestarting()).toBe(false);
  });

  it("measures the window from the mint time, not from receipt", () => {
    const { window, clock } = harness();
    clock.nowMs = MINT + 50_000; // the notice took 50s to arrive
    window.announce(announcement({ expectedOutageMs: 60_000 }));
    clock.nowMs = MINT + 60_000;
    expect(window.isRestarting()).toBe(false);
  });

  it("clamps an outage hint beyond the ceiling", () => {
    const { window, clock } = harness();
    window.announce(announcement({ expectedOutageMs: MAX_QUIET_WINDOW_MS * 10 }));
    clock.nowMs = MINT + MAX_QUIET_WINDOW_MS;
    expect(window.isRestarting()).toBe(false);
  });

  it("refuses an announcement with a non-positive outage hint", () => {
    const { window } = harness();
    expect(window.announce(announcement({ expectedOutageMs: 0 }))).toBe(false);
  });

  it("refuses an announcement with no mint time", () => {
    const { window } = harness();
    expect(window.announce(announcement({ atMs: 0 }))).toBe(false);
  });

  it("refuses an announcement whose window already elapsed on arrival", () => {
    const { window, clock } = harness();
    clock.nowMs = MINT + 120_000;
    expect(window.announce(announcement({ expectedOutageMs: 60_000 }))).toBe(false);
  });

  it("is quiet-free before any announcement", () => {
    const { window } = harness();
    expect(window.suppressesDisconnectAlarm()).toBe(false);
  });

  it("ignores a resync with no window open", () => {
    const { window, logs } = harness();
    window.onResynced();
    expect(logs).toHaveLength(0);
  });

  it("replaces an open window with a later announcement's bound", () => {
    const { window, clock } = harness();
    window.announce(announcement({ expectedOutageMs: 10_000 }));
    clock.nowMs = MINT + 5_000;
    window.announce(announcement({ expectedOutageMs: 60_000, atMs: MINT + 5_000 }));
    clock.nowMs = MINT + 20_000;
    expect(window.isRestarting()).toBe(true);
  });
});
