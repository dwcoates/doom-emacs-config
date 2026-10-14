/**
 * A pulled reading whose probes run one at a time.
 *
 * WHAT THIS GUARDS: the two rules every often-refreshed reading shares —
 * probes run serially in the order asked, and a refresh queued but not yet
 * started absorbs further refreshes — plus that one failed probe neither hides
 * its failure from its caller nor stops later probes. The last case pins that
 * the session's serial readings go through this one helper rather than each
 * hand-rolling a chain.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { SerialProbe } from "../../src/engine/serial-probe.js";

/** A probe whose every call blocks until the test releases it. */
function gatedProbe(): {
  probe: () => Promise<void>;
  started: number;
  release: (err?: Error) => void;
} {
  const gates: Array<{ resolve: () => void; reject: (err: Error) => void }> = [];
  const state = {
    started: 0,
    probe: () =>
      new Promise<void>((resolve, reject) => {
        state.started += 1;
        gates.push({ resolve, reject });
      }),
    release: (err?: Error) => {
      const gate = gates.shift();
      if (gate === undefined) throw new Error("no probe is waiting to be released");
      if (err === undefined) gate.resolve();
      else gate.reject(err);
    },
  };
  return state;
}

/** Let every settled promise's continuations run. */
async function drain(): Promise<void> {
  for (let i = 0; i < 10; i += 1) await Promise.resolve();
}

describe("SerialProbe", () => {
  it("starts a probe only after the one before it has settled", async () => {
    // Arrange.
    const gated = gatedProbe();
    const probe = new SerialProbe(gated.probe);

    // Act.
    const first = probe.run();
    const second = probe.run();
    await drain();
    const startedBeforeRelease = gated.started;
    gated.release();
    await first;
    await drain();

    // Assert.
    expect(startedBeforeRelease).toBe(1);
    expect(gated.started).toBe(2);
    gated.release();
    await second;
  });

  it("folds a refresh into one already queued and not yet started", async () => {
    // Arrange.
    const gated = gatedProbe();
    const probe = new SerialProbe(gated.probe);
    const running = probe.run();
    await drain();

    // Act.
    const firstRefresh = probe.refresh();
    const secondRefresh = probe.refresh();

    // Assert.
    expect(secondRefresh).toBe(firstRefresh);
    expect(probe.refreshPending).toBe(true);
    gated.release();
    await running;
    await drain();
    expect(probe.refreshPending).toBe(false);
    gated.release();
    await firstRefresh;
    expect(gated.started).toBe(2);
  });

  it("queues a fresh refresh once the queued one has started", async () => {
    // Arrange.
    const gated = gatedProbe();
    const probe = new SerialProbe(gated.probe);
    const firstRefresh = probe.refresh();
    await drain();

    // Act.
    const secondRefresh = probe.refresh();

    // Assert.
    expect(secondRefresh).not.toBe(firstRefresh);
    gated.release();
    await firstRefresh;
    await drain();
    gated.release();
    await secondRefresh;
    expect(gated.started).toBe(2);
  });

  it("never folds a run into a queued refresh", async () => {
    // Arrange.
    const gated = gatedProbe();
    const probe = new SerialProbe(gated.probe);
    const running = probe.run();
    const refresh = probe.refresh();

    // Act.
    const second = probe.run();

    // Assert.
    expect(second).not.toBe(refresh);
    for (const pending of [running, refresh, second]) {
      await drain();
      gated.release();
      await pending;
    }
    expect(gated.started).toBe(3);
  });

  it("rejects the failed probe's caller and still runs the next probe", async () => {
    // Arrange.
    const gated = gatedProbe();
    const probe = new SerialProbe(gated.probe);
    const failing = probe.run();
    const next = probe.run();
    await drain();

    // Act.
    gated.release(new Error("the vendor refused"));

    // Assert.
    await expect(failing).rejects.toThrow("the vendor refused");
    await drain();
    expect(gated.started).toBe(2);
    gated.release();
    await expect(next).resolves.toBeUndefined();
  });

  it("is the one queue every serial reading of the session goes through", () => {
    // A reading that hand-rolls its chain is a second copy free to drift.
    const source = readFileSync(path.join(import.meta.dirname, "../../src/engine/session.ts"), "utf8");
    expect(source).toMatch(/const contextProbe = new SerialProbe\(\(\) => probeContextUsage\(\)\);/);
    expect(source).not.toMatch(/Chain: Promise<void> = Promise\.resolve\(\)/);
  });
});
