/**
 * LIFECYCLE — the graceful rollout, and the outage window it hides.
 *
 * The transfer is the one place in the webapp where ORDER is the contract, not
 * just the outcome: the client must open the new daemon, adopt the workspace
 * THERE, and only then drop the old streams. Get that backwards and a rollout
 * loses whatever arrives in the gap. Two fake daemons and their two call logs
 * are how that ordering is made assertable rather than assumed.
 *
 * The rest is the drain banner and the quiet window: a shutdown announcement
 * carries an expected outage, and the client must NOT cry `daemonUnreachable`
 * during it — shortened by however long the announcement took to arrive, since
 * the daemon minted it before the socket died.
 */
import { afterEach, describe, expect, it } from "vitest";

import { WatchDaemonResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import { DrainReasonSchema } from "../../../proto/gen/ts/agentrepl/v1/drain_reason_pb";

import { startHarness, type Harness } from "./harness";
import {
  DRAIN_REASON_ARMS,
  SHUTDOWN_CAUSE_ARMS,
  WATCH_DAEMON_PUSHES,
  WORKSPACE_ID,
  assertCoversOneof,
  drainReason,
  footerView,
} from "./fixtures";

let harness: Harness;

afterEach(async () => {
  await harness?.stop();
});

describe("arm coverage", () => {
  it("covers every WatchDaemon push", () => {
    assertCoversOneof(WatchDaemonResponseSchema, "push", [...WATCH_DAEMON_PUSHES]);
  });

  it("covers every drain reason arm", () => {
    assertCoversOneof(DrainReasonSchema, "kind", [...DRAIN_REASON_ARMS]);
  });
});

describe("the transfer", () => {
  /**
   * THE WEBAPP NEVER REDIALS (project lead, final — see the CONFIRMED SEQUENCE
   * block in docs/overhaul/reports/webapp-briefs/lifecycle.md). This block used
   * to assert the opposite: a second transport built at the announced address,
   * `AdoptWebWorkspace` called THERE, and every view stream re-opened on the
   * successor. That whole path is retired. A successor on another loopback port
   * is a DIFFERENT ORIGIN, so re-pointing the view is Emacs's job; this page
   * draws "workspace moved to <address>", goes quiet, cancels every stream, and
   * stops. Adoption happens ONCE, at boot, on the daemon the page was addressed
   * to — which is why the old daemon's adoption log reads 1 here and not 0.
   */
  const transfer = async (): Promise<{ second: Awaited<ReturnType<Harness["startSecondDaemon"]>> }> => {
    harness = await startHarness();
    await harness.fake.awaitStream("watchWebWorkspace");
    const second = await harness.startSecondDaemon();
    harness.fake.transfer(WORKSPACE_ID, second.baseUrl);
    await harness.settle();
    return { second };
  };

  it("draws the page-wide moved notice", async () => {
    // Arrange / Act
    const { second } = await transfer();
    // Assert
    expect(harness.$(`[data-moved="${second.baseUrl}"]`)).not.toBeNull();
  });

  it("names the successor's address in the notice", async () => {
    // Arrange / Act
    const { second } = await transfer();
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent).toContain(second.baseUrl);
  });

  it("goes quiet", async () => {
    // Arrange / Act
    await transfer();
    // Assert
    expect(harness.ctx.isQuiesced()).toBe(true);
  });

  it("cancels the view streams on the old daemon", async () => {
    // Arrange / Act
    await transfer();
    // Assert
    expect(harness.fake.liveStreams("watchFooter", WORKSPACE_ID)).toBe(0);
  });

  it("cancels the roster stream on the old daemon", async () => {
    // Arrange / Act
    await transfer();
    // Assert
    expect(harness.fake.liveStreams("watchWorkspaceRoster")).toBe(0);
  });

  it("cancels its own web-link stream on the old daemon", async () => {
    // Arrange / Act
    await transfer();
    // Assert
    expect(harness.fake.liveStreams("watchWebWorkspace", WORKSPACE_ID)).toBe(0);
  });

  it("sends nothing at all to the successor", async () => {
    // Arrange / Act
    const { second } = await transfer();
    // Assert: no transport to the new address is ever created.
    expect(second.log()).toHaveLength(0);
  });

  it("does not adopt on the successor", async () => {
    // Arrange / Act
    const { second } = await transfer();
    // Assert
    expect(second.calls("adoptWebWorkspace")).toHaveLength(0);
  });

  it("adopted once at boot, on the daemon the page was addressed to", async () => {
    // Arrange / Act
    await transfer();
    // Assert
    expect(harness.fake.calls("adoptWebWorkspace")).toHaveLength(1);
  });

  it("echoed the workspace on the boot adoption", async () => {
    // Arrange / Act
    await transfer();
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("adoptWebWorkspace");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("reports no failure for a transfer, which is orderly", async () => {
    // Arrange / Act
    await transfer();
    await harness.tick(5_000);
    // Assert: the streams stopped because the page cancelled them, and a
    // cancel is never a transport failure.
    expect(harness.failureArms()).not.toContain("daemonUnreachable");
  });
});

describe("the drain banner", () => {
  it.each(DRAIN_REASON_ARMS)("draws a scheduled drain for the %s reason", async (arm) => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.scheduleDrain(60_000n, drainReason(arm));
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="drain-banner"] [data-arm]')?.dataset.arm).toBe(arm);
  });

  it("draws the operator's own note verbatim", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.scheduleDrain(60_000n, drainReason("operator"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent).toContain("the operator asked");
  });

  it("counts down to the served instant", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    harness.fake.scheduleDrain(60_000n, drainReason("deploy"));
    await harness.settle();
    const before = harness.$('[data-component="drain-banner"]')?.textContent;
    // Act
    await harness.tick(10_000);
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent).not.toBe(before);
  });

  it("stands page-wide rather than per-workspace", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.scheduleDrain(60_000n, drainReason("deploy"));
    await harness.settle();
    // Assert: the banner's host is outside the feed's scroll zone.
    expect(harness.shell.feedScroll.contains(harness.shell.drainBanner)).toBe(false);
  });

  it("removes the banner on a cancellation", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    harness.fake.scheduleDrain(60_000n, drainReason("deploy"));
    await harness.settle();
    // Act
    harness.fake.cancelDrain();
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent?.trim() || "").toBe("");
  });

  it("draws no banner before any drain is scheduled", async () => {
    // Arrange / Act
    harness = await startHarness();
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent?.trim() || "").toBe("");
  });
});

describe("the announced outage window", () => {
  /**
   * KEEPING THE LINK DOWN. A stream that dies and comes straight back retracts
   * its own card on the reopen's first frame (src/rpc/streams.ts: "retracted on
   * the first successful push"), which is the app behaving correctly and would
   * make "the card stands" unobservable. An announced outage means the daemon
   * is GONE for the window, so the reopens are scripted to fail for as long as
   * the test looks.
   */
  const keepDown = (rpc: "watchFooter", attempts = 20): void => {
    for (let i = 0; i < attempts; i += 1) harness.fake.failNext(rpc, "the daemon is down");
  };

  it("suppresses the unreachable overlay during the quiet window", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    harness.fake.announceShutdown({ expectedOutageMs: 10_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    // Act: the streams die, as an announced bounce means they will.
    harness.fake.endStream("watchFooter");
    await harness.tick(2_000);
    // Assert
    expect(harness.failureArms()).not.toContain("daemonUnreachable");
  });

  it("reports the failure once the quiet window lapses", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    harness.fake.announceShutdown({ expectedOutageMs: 4_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    // Act
    keepDown("watchFooter");
    harness.fake.endStream("watchFooter");
    await harness.tick(20_000);
    // Assert
    expect(harness.failureArms()).toContain("daemonUnreachable");
  });

  it("shortens the window by the time since the announcement was minted", async () => {
    // Arrange: minted 8 s ago with a 10 s outage leaves only ~2 s of quiet.
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    harness.fake.announceShutdown({
      expectedOutageMs: 10_000n,
      mintedAtMs: BigInt(Date.now() - 8_000),
    });
    await harness.settle();
    // Act
    keepDown("watchFooter");
    harness.fake.endStream("watchFooter");
    await harness.tick(5_000);
    // Assert
    expect(harness.failureArms()).toContain("daemonUnreachable");
  });

  it("reconnects after the announced bounce", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    harness.fake.announceShutdown({ expectedOutageMs: 2_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    const before = harness.fake.calls("watchFooter").length;
    // Act
    harness.fake.endStream("watchFooter");
    await harness.tick(10_000);
    // Assert
    expect(harness.fake.calls("watchFooter").length).toBeGreaterThan(before);
  });

  it("retracts the report once a push lands again", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    harness.fake.announceShutdown({ expectedOutageMs: 1_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    harness.fake.endStream("watchFooter");
    await harness.tick(20_000);
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
    await harness.settle();
    // Assert
    expect(harness.failureArms()).not.toContain("daemonUnreachable");
  });
});

describe("a plain bounce", () => {
  it("draws the restarting notice when the announcement carries no address", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act: no `address` means the same daemon is coming back, not a transfer.
    harness.fake.announceShutdown({ expectedOutageMs: 4_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    // Assert
    expect(harness.$("[data-restarting]")).not.toBeNull();
  });

  it("does not open a second transport for an address-less announcement", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.announceShutdown({ expectedOutageMs: 4_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    // Assert: an address-less bounce is not a transfer, so nothing is adopted
    // beyond the ONE adoption every fresh page makes at boot.
    expect(harness.fake.calls("adoptWebWorkspace")).toHaveLength(1);
  });

  it("clears the restarting notice once the streams are back", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    harness.fake.announceShutdown({ expectedOutageMs: 1_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
    await harness.settle();
    // Assert
    expect(harness.$("[data-restarting]")).toBeNull();
  });
});

describe.each(SHUTDOWN_CAUSE_ARMS)("a %s shutdown cause", (cause) => {
  it("is drawn from the announcement", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.announceShutdown({
      cause,
      expectedOutageMs: 4_000n,
      mintedAtMs: BigInt(Date.now()),
    });
    await harness.settle();
    // Assert
    expect(harness.$(`[data-shutdown-cause="${cause}"]`)).not.toBeNull();
  });
});
