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

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { REFUSAL_FACTS, ROOT_FEED, type RpcName } from "./fake-daemon";
import {
  DRAIN_REASON_ARMS,
  SHUTDOWN_CAUSE_ARMS,
  WATCH_DAEMON_PUSHES,
  WORKSPACE_ID,
  activityRow,
  assertCoversOneof,
  drainReason,
  feedId,
  feedPageSuccess,
  responseRow,
  subagentUnit,
  footerView,
  heldPromptItem,
  holdTray,
  permissionRow,
  roster,
  rosterRow,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

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

// FORWARD-COMPAT SKEW IS NOT A BAD FRAME. `mutation_progress` is a TOP-LEVEL
// WatchDaemon push arm this bundle has no case for: a newer daemon setting it
// is version skew a reload resolves, so the pipeline logs it and skips it —
// raising no card, filing no `frame_undecodable`, and leaving the stream
// standing. Every OTHER malformation stays loud.
describe("a WatchDaemon push arm this build has no case for", () => {
  it("raises no failure card", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.pushMutationProgress("op-1");
    await harness.settle();
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });

  it("draws nothing for it", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.pushMutationProgress("op-1");
    await harness.settle();
    // Assert: the banner is the only thing this stream draws, and this arm is
    // not one of its.
    expect(harness.$('[data-component="drain-banner"]')?.textContent?.trim() ?? "").toBe("");
  });

  it("leaves the stream standing, so the next push it DOES know still lands", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    harness.fake.pushMutationProgress("op-1");
    await harness.settle();
    // Act
    harness.fake.scheduleDrain(60_000n, drainReason("deploy"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="drain-banner"] [data-arm]')?.dataset.arm).toBe("deploy");
  });
});

// `reload_elisp` IS EMACS'S. The daemon addresses it to stale Emacs streams
// alone, so a webview never meets it; one that did would skip it as skew,
// raising nothing and keeping the stream.
describe("a reload_elisp push reaching a webview", () => {
  it("raises no failure card", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.pushReloadElisp("/checkout", "elisp-build");
    await harness.settle();
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });

  it("leaves the stream standing, so the next push it DOES know still lands", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    harness.fake.pushReloadElisp("/checkout", "elisp-build");
    await harness.settle();
    // Act
    harness.fake.scheduleDrain(60_000n, drainReason("deploy"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="drain-banner"] [data-arm]')?.dataset.arm).toBe("deploy");
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
    expect(harness.$('[data-component="drain-banner"]')?.textContent?.trim() ?? "").toBe("");
  });

  it("draws no banner before any drain is scheduled", async () => {
    // Arrange / Act
    harness = await startHarness();
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent?.trim() ?? "").toBe("");
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
    // Arrange: the announcement, then the bounce itself -- the link drops.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    harness.fake.announceShutdown({ expectedOutageMs: 1_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    harness.fake.endPageStream();
    await harness.settle();
    // Act: the page re-attaches and a view reads again.
    await harness.tick(60_000);
    await harness.fake.awaitStream("watchFooter");
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
    await harness.settle();
    // Assert
    expect(harness.$("[data-restarting]")).toBeNull();
  });

  it("keeps the restarting notice while the announcing daemon's streams still push", async () => {
    // Arrange: the outgoing daemon goes on pushing until it exits, and a frame
    // on a link that never dropped is that daemon, not the one coming back.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    harness.fake.announceShutdown({ expectedOutageMs: 4_000n, mintedAtMs: BigInt(Date.now()) });
    await harness.settle();
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
    await harness.settle();
    // Assert
    expect(harness.$("[data-restarting]")).not.toBeNull();
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

// ---------------------------------------------------------------------------
// THE MOVE ARRIVES AS A REFUSAL, TOO (audit 1, item 18)
//
// RULED (the ONE refusal hook, src/rpc/refuse.ts): "any rpc's transferringAway
// raises the moved notice, quiesces, cancels every stream." The arm is the same
// fact `WatchWebWorkspace`'s `transferred` push carries, arriving as an ANSWER
// to a click instead of as a push — and the hook raises the page-wide notice
// itself (`crossCuttingSentence` → `moved.ts`) precisely so that no call site
// can opt out of it. Drawing it only as a sentence beside one button would
// leave the rest of the page looking live while nothing it shows can be true.
//
// So the same assertions the `transferred` push gets are made here, once per
// rpc SHAPE the app calls: a footer control, a feed card, the tray, the
// sidebar and the topbar.
// ---------------------------------------------------------------------------

/** How to provoke one rpc's `transferring_away`, and where its click is. */
interface MoveSite {
  name: string;
  rpc: RpcName;
  click: string;
  arrange?(h: Harness): void;
  before?(h: Harness): Promise<void>;
}

const MOVE_SITES: MoveSite[] = [
  {
    name: "Interrupt (the footer's stop)",
    rpc: "interrupt",
    click: ".footer-clock [data-interrupt]",
    arrange: (h) => h.fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" })),
  },
  {
    name: "AnswerPermission (a feed card)",
    rpc: "answerPermission",
    click: '[data-permission="allowOnce"]',
    arrange: (h) =>
      h.fake.pushRow(WORKSPACE_ID, ROOT_FEED, permissionRow("open", undefined, { id: feedId("perm") })),
  },
  {
    name: "UpdateHeldPrompt (the hold tray)",
    rpc: "updateHeldPrompt",
    click: '[data-held-action="drop"]',
    arrange: (h) => h.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem()] })),
  },
  {
    name: "CloseWorkspace (a sidebar verb)",
    rpc: "closeWorkspace",
    click: '[data-roster-row="ws-1"] [data-verb="close"]',
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "SetModel (the topbar picker)",
    rpc: "setModel",
    click: '[data-model-option="sonnet"]',
    before: async (h) => {
      await h.click(".topbar-model");
    },
  },
];

describe.each(MOVE_SITES)("$name refused as transferring_away", (site) => {
  /** Boot, refuse this rpc with the move arm, and click its control. */
  const refuseAsMoved = async (): Promise<void> => {
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.refuse(site.rpc, "transferringAway");
    site.arrange?.(harness);
    await harness.settle();
    await site.before?.(harness);
    await harness.click(site.click);
  };

  it("raises the page-wide moved notice", async () => {
    // Arrange / Act
    await refuseAsMoved();
    // Assert
    expect(harness.$(`[data-moved="${REFUSAL_FACTS.address}"]`)).not.toBeNull();
  });

  it("names the successor's address in the notice", async () => {
    // Arrange / Act
    await refuseAsMoved();
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent).toContain(
      REFUSAL_FACTS.address,
    );
  });

  it("goes quiet", async () => {
    // Arrange / Act
    await refuseAsMoved();
    // Assert
    expect(harness.ctx.isQuiesced()).toBe(true);
  });

  it("cancels the view streams", async () => {
    // Arrange / Act
    await refuseAsMoved();
    // Assert
    expect(harness.fake.liveStreams("watchFooter", WORKSPACE_ID)).toBe(0);
  });

  it("cancels the feed's tail", async () => {
    // Arrange / Act
    await refuseAsMoved();
    // Assert
    expect(harness.fake.liveStreams("watchFeed", WORKSPACE_ID)).toBe(0);
  });

  it("cancels the global roster stream", async () => {
    // Arrange / Act
    await refuseAsMoved();
    // Assert
    expect(harness.fake.liveStreams("watchWorkspaceRoster")).toBe(0);
  });

  it("cancels its own web-link stream", async () => {
    // Arrange / Act
    await refuseAsMoved();
    // Assert
    expect(harness.fake.liveStreams("watchWebWorkspace", WORKSPACE_ID)).toBe(0);
  });

  it("reports no transport failure, because the page cancelled them", async () => {
    // Arrange / Act
    await refuseAsMoved();
    await harness.tick(10_000);
    // Assert
    expect(harness.failureArms()).not.toContain("daemonUnreachable");
  });

  it("still draws the refusal at the clicked control", async () => {
    // Arrange / Act: the page-wide notice does not replace the call site's own
    // answer — the user clicked here and is looking here.
    await refuseAsMoved();
    // Assert
    expect(harness.refusalArms()).toContain("transferringAway");
  });

  it("does not reopen a stream after the move", async () => {
    // Arrange
    await refuseAsMoved();
    const attempts = harness.fake.calls("watchFooter").length;
    // Act
    await harness.tick(30_000);
    // Assert: the webapp never redials a successor.
    expect(harness.fake.calls("watchFooter").length).toBe(attempts);
  });
});

/**
 * AN EXPANDED BUBBLE'S TAIL IS A STREAM LIKE ANY OTHER.
 *
 * `quiesce()` cancels EVERY registered handle, and a sub-feed's tail is
 * registered by the same `watchStream` the views use. A bubble left tailing
 * after the workspace moved would keep drawing rows from a daemon this page has
 * been told it no longer belongs to.
 */
describe("the transfer with a bubble expanded", () => {
  const transferWithBubble = async (): Promise<void> => {
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
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
    const second = await harness.startSecondDaemon();
    harness.fake.transfer(WORKSPACE_ID, second.baseUrl);
    await harness.settle();
  };

  it("cancels the sub-feed's tail too", async () => {
    // Arrange / Act
    await transferWithBubble();
    // Assert
    expect(harness.fake.liveStreams("watchFeed", WORKSPACE_ID, "bubble")).toBe(0);
  });

  it("draws nothing more in the bubble afterwards", async () => {
    // Arrange
    await transferWithBubble();
    // Act: the daemon still has the feed; nothing is listening for it.
    harness.fake.pushRow(
      WORKSPACE_ID,
      "bubble",
      responseRow("success", "after the move", { id: feedId("inner-after") }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("inner-after")).toBeNull();
  });

  it("does not reopen the sub-feed after the move", async () => {
    // Arrange
    await transferWithBubble();
    const opens = harness.fake.calls("openFeed").length;
    // Act
    await harness.tick(30_000);
    // Assert: a quiesced page never redials, the bubble included.
    expect(harness.fake.calls("openFeed").length).toBe(opens);
  });
});
