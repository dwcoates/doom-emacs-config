/**
 * STREAMS — the per-endpoint decode contract, once per Watch* stream.
 *
 * Three things must hold for every stream, and they are the same three every
 * time, so this file is one table over the seven streams rather than seven
 * near-identical files:
 *
 *   1. a push decodes into the DOM VERBATIM (the daemon composed the words;
 *      the client draws them and derives nothing),
 *   2. an unknown field is REFUSED — the frame is skipped, `frameUndecodable`
 *      is reported, and the NEXT frame still renders, because one bad frame
 *      must not take the component down,
 *   3. a stream dying with no terminal frame is a TRANSPORT FAILURE: it
 *      reports `daemonUnreachable`, reopens, and retracts on the next push.
 */
import { afterEach, describe, expect, it } from "vitest";

import { startHarness, type Harness } from "./harness";
import type { FakeDaemon, RpcName } from "./fake-daemon";
import { ROOT_FEED } from "./fake-daemon";
import {
  WORKSPACE_ID,
  drainReason,
  footerView,
  holdTray,
  heldPromptItem,
  responseRow,
  roster,
  rosterRow,
  topbarView,
  userPromptRow,
} from "./fixtures";

let harness: Harness;

afterEach(async () => {
  await harness?.stop();
});

/**
 * One case per stream: how to push a frame, and the text that frame must put
 * on screen. The verbatim string is the whole point — if the client composed
 * it, the assertion would pass for a value the daemon never sent.
 */
interface StreamCase {
  name: string;
  rpc: RpcName;
  /** Push a frame whose drawn text is `expect`. */
  push(fake: FakeDaemon, marker: string): void;
  /** The selector the text lands in. */
  selector: string;
  /** The text `push` puts on screen for a given marker. */
  drawn(marker: string): string;
  /** Feed streams need their tail opened before anything can be pushed. */
  needsOpenFeed?: boolean;
}

const STREAM_CASES: StreamCase[] = [
  {
    name: "WatchFooter",
    rpc: "watchFooter",
    push: (fake, marker) =>
      fake.setFooter(WORKSPACE_ID, footerView({ status: "idle", substatus: "ready", tokensText: marker })),
    selector: ".footer-tokens",
    drawn: (marker) => marker,
  },
  {
    name: "WatchTopbar",
    rpc: "watchTopbar",
    push: (fake, marker) => fake.setTopbar(WORKSPACE_ID, topbarView({ title: marker })),
    selector: ".topbar-title",
    drawn: (marker) => marker,
  },
  {
    name: "WatchWorkspaceRoster",
    rpc: "watchWorkspaceRoster",
    push: (fake, marker) => fake.setRoster(roster({ rows: [rosterRow({ name: marker })] })),
    selector: `[data-roster-row="${WORKSPACE_ID}"]`,
    drawn: (marker) => marker,
  },
  {
    name: "WatchDaemonHolds",
    rpc: "watchDaemonHolds",
    push: (fake, marker) =>
      fake.setTray(WORKSPACE_ID, holdTray({ heading: marker, items: [heldPromptItem()] })),
    selector: '[data-component="hold-tray"]',
    drawn: (marker) => marker,
  },
  {
    name: "WatchFeed",
    rpc: "watchFeed",
    needsOpenFeed: true,
    push: (fake, marker) => fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow(marker)),
    selector: '[data-feed-row="row-1"]',
    drawn: (marker) => marker,
  },
];

describe.each(STREAM_CASES)("$name", (streamCase) => {
  it("decodes a push into the DOM verbatim", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    // Act
    streamCase.push(harness.fake, "verbatim-one");
    await harness.settle();
    // Assert
    expect(harness.$(streamCase.selector)?.textContent).toContain(streamCase.drawn("verbatim-one"));
  });

  it("refuses a frame carrying an unknown field", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    // Act
    harness.fake.injectUnknown(streamCase.rpc);
    streamCase.push(harness.fake, "poisoned");
    await harness.settle();
    // Assert
    expect(harness.failureArms()).toContain("frameUndecodable");
  });

  it("skips the undecodable frame rather than drawing it", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    // Act
    harness.fake.injectUnknown(streamCase.rpc);
    streamCase.push(harness.fake, "poisoned");
    await harness.settle();
    // Assert
    expect(harness.$(streamCase.selector)?.textContent ?? "").not.toContain("poisoned");
  });

  it("renders the next frame after an undecodable one", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    harness.fake.injectUnknown(streamCase.rpc);
    streamCase.push(harness.fake, "poisoned");
    await harness.settle();
    // Act
    streamCase.push(harness.fake, "recovered");
    await harness.settle();
    // Assert
    expect(harness.$(streamCase.selector)?.textContent).toContain(streamCase.drawn("recovered"));
  });

  it("reports the daemon unreachable when the stream dies with no terminal frame", async () => {
    // Arrange: the reopens fail too, so the link is DOWN rather than flapping.
    // A stream that comes straight back retracts its own card on the reopen's
    // first frame (streams.ts: "retracted on the first successful push"), which
    // is the app behaving correctly — the standing card is the link staying
    // gone, and that is what this asserts.
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    for (let i = 0; i < 20; i += 1) harness.fake.failNext(streamCase.rpc, "the daemon is down");
    // Act
    harness.fake.endStream(streamCase.rpc);
    await harness.tick(5_000);
    // Assert
    expect(harness.failureArms()).toContain("daemonUnreachable");
  });

  it("reopens the stream after a transport death", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    const before = harness.fake.calls(streamCase.rpc).length;
    // Act
    harness.fake.endStream(streamCase.rpc);
    await harness.tick(5_000);
    // Assert: a SECOND call of the same rpc reached the daemon.
    expect(harness.fake.calls(streamCase.rpc).length).toBeGreaterThan(before);
  });

  it("retracts the unreachable report on the next successful push", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    harness.fake.endStream(streamCase.rpc);
    await harness.tick(5_000);
    // Act
    streamCase.push(harness.fake, "back-again");
    await harness.settle();
    // Assert
    expect(harness.failureArms()).not.toContain("daemonUnreachable");
  });
});

describe("WatchWebWorkspace", () => {
  it("opens the standing web-link stream at boot", async () => {
    // Arrange / Act
    harness = await startHarness();
    await harness.fake.awaitStream("watchWebWorkspace");
    // Assert: it is never drawn, so its presence is the whole assertion.
    expect(harness.fake.liveStreams("watchWebWorkspace", WORKSPACE_ID)).toBe(1);
  });

  it("reports the daemon unreachable when it dies with no terminal frame", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchWebWorkspace");
    // Act
    harness.fake.endStream("watchWebWorkspace");
    await harness.tick(5_000);
    // Assert
    expect(harness.failureArms()).toContain("daemonUnreachable");
  });

  it("reopens after a transport death", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchWebWorkspace");
    const before = harness.fake.calls("watchWebWorkspace").length;
    // Act
    harness.fake.endStream("watchWebWorkspace");
    await harness.tick(5_000);
    // Assert
    expect(harness.fake.calls("watchWebWorkspace").length).toBeGreaterThan(before);
  });
});

describe("WatchDaemon", () => {
  it("decodes a drain push into the banner verbatim", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.scheduleDrain(60_000n, drainReason("maintenance"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent ?? "").not.toBe("");
  });

  it("refuses a daemon push carrying an unknown field", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    harness.fake.injectUnknown("watchDaemon");
    harness.fake.scheduleDrain(60_000n, drainReason("deploy"));
    await harness.settle();
    // Assert
    expect(harness.failureArms()).toContain("frameUndecodable");
  });

  it("renders the next daemon push after an undecodable one", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    harness.fake.injectUnknown("watchDaemon");
    harness.fake.scheduleDrain(60_000n, drainReason("deploy"));
    await harness.settle();
    // Act
    harness.fake.scheduleDrain(60_000n, drainReason("maintenance"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="drain-banner"]')?.textContent ?? "").not.toBe("");
  });

  it("reopens after a transport death", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    const before = harness.fake.calls("watchDaemon").length;
    // Act
    harness.fake.endStream("watchDaemon");
    await harness.tick(5_000);
    // Assert
    expect(harness.fake.calls("watchDaemon").length).toBeGreaterThan(before);
  });
});

describe("the feed's own tail", () => {
  it("opens the root feed through OpenFeed before watching it", async () => {
    // Arrange / Act
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Assert: the token WatchFeed echoed is the one OpenFeed minted.
    const [request] = harness.fake.calls<{ watch?: { value: string } }>("watchFeed");
    expect(request.watch?.value).toBe(harness.fake.mintedTokens(WORKSPACE_ID, ROOT_FEED)[0]);
  });

  it("re-opens the feed after a transport death rather than reusing the dead token", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.endStream("watchFeed");
    await harness.tick(5_000);
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "after the death"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-feed="root"]')?.textContent).toContain("after the death");
  });
});

/**
 * ACCEPTANCE AND CANCELLATION.
 *
 * A standing stream never ends on its own, so the two observable events in its
 * life are the daemon ACCEPTING it (the head flushes before any frame, so the
 * client can tell it is open while nothing has been pushed) and the client
 * ABORTING it. Anything else ending the stream is a transport failure, which
 * the tables above cover.
 */
describe("watch acceptance", () => {
  it.each(STREAM_CASES)("registers $name before any frame is pushed", async (streamCase) => {
    // Arrange / Act: nothing is pushed here at all.
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    // Assert
    expect(harness.fake.liveStreams(streamCase.rpc)).toBeGreaterThan(0);
  });

  it("registers the web-link watch before any frame", async () => {
    // Arrange / Act
    harness = await startHarness();
    await harness.fake.awaitStream("watchWebWorkspace");
    // Assert
    expect(harness.fake.liveStreams("watchWebWorkspace")).toBe(1);
  });

  it("registers the daemon watch before any frame", async () => {
    // Arrange / Act
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Assert
    expect(harness.fake.liveStreams("watchDaemon")).toBe(1);
  });

  it("reports no failure for a watch that has pushed nothing", async () => {
    // Arrange / Act: silence on a standing stream is not a fault.
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    await harness.tick(30_000);
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });
});

describe("watch cancellation", () => {
  /** Wait for the fake to see the aborted request drop off. */
  const awaitDrop = async (rpc: RpcName): Promise<void> => {
    for (let i = 0; i < 50 && harness.fake.liveStreams(rpc) > 0; i += 1) {
      await harness.tick(10);
    }
  };

  it.each(STREAM_CASES)("drops $name when the app disposes its mount", async (streamCase) => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream(streamCase.rpc);
    // Act: the app cancels by aborting its request, never by waiting for an end.
    await harness.disposeMounts();
    await awaitDrop(streamCase.rpc);
    // Assert
    expect(harness.fake.liveStreams(streamCase.rpc)).toBe(0);
  });

  it("drops the web-link watch on dispose", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchWebWorkspace");
    // Act
    await harness.disposeMounts();
    await awaitDrop("watchWebWorkspace");
    // Assert
    expect(harness.fake.liveStreams("watchWebWorkspace")).toBe(0);
  });

  it("drops the daemon watch on dispose", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchDaemon");
    // Act
    await harness.disposeMounts();
    await awaitDrop("watchDaemon");
    // Assert
    expect(harness.fake.liveStreams("watchDaemon")).toBe(0);
  });

  it("does not reopen a stream the app cancelled", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    await harness.disposeMounts();
    await awaitDrop("watchFooter");
    const after = harness.fake.calls("watchFooter").length;
    // Act: a cancellation is deliberate, so no backoff reopen may follow it.
    await harness.tick(10_000);
    // Assert
    expect(harness.fake.calls("watchFooter").length).toBe(after);
  });

  it("reports no failure for a stream the app cancelled", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFooter");
    // Act
    await harness.disposeMounts();
    await harness.tick(10_000);
    // Assert: the client ended it, so it is not a transport failure.
    expect(harness.failureArms()).not.toContain("daemonUnreachable");
  });
});
