// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { WatchDaemonHoldsResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_holds_pb";
import {
  DaemonHoldItemSchema,
  DaemonHoldTraySchema,
  type DaemonHoldItem,
  type DaemonHoldTray,
} from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Ticker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import type { TrayContext } from "../../src/tray/context.js";
import {
  HELD_ENTRY_SELECTOR,
  drawDaemonHoldItem,
  drawDaemonHoldTray,
  mountHoldTray,
} from "../../src/tray/tray.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const NOW = 1_700_000_000_000;
const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

function fakeTicker(): Ticker & { subscribers(): number } {
  const listeners = new Set<(nowMs: number) => void>();
  return {
    now: () => NOW,
    subscribe(fn) {
      listeners.add(fn);
      return () => listeners.delete(fn);
    },
    subscribers: () => listeners.size,
  };
}

/** A context whose WatchDaemonHolds yields whatever PUSHES supplies. */
function streamingContext(
  pushes: () => AsyncIterable<DaemonHoldTray>,
  ticker: Ticker = fakeTicker(),
): AppContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      watchDaemonHolds: async function* () {
        for await (const tray of pushes()) {
          yield create(WatchDaemonHoldsResponseSchema, { tray });
        }
        // A standing stream never concludes on its own; the test's iterable
        // ends only after the assertions it exists for.
        await new Promise<never>(() => undefined);
      },
    });
  });
  return testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker,
    failures: SINK,
    composerEnabled: false,
  });
}

function trayContext(ticker: Ticker = fakeTicker()): TrayContext {
  const ctx = testAppContext({
    client: createAgentReplClient(createRouterTransport(({ service }) => service(AgentRepl, {}))),
    workspace: WORKSPACE,
    ticker,
    failures: SINK,
    composerEnabled: false,
  });
  return { ctx, onDispose: () => undefined };
}

const promptItem = (turn: string): DaemonHoldItem =>
  create(DaemonHoldItemSchema, {
    item: {
      case: "prompt",
      value: {
        turn: { value: turn },
        said: { content: { blocks: [{ block: { case: "text", value: { text: "hi" } } }] } },
        queuedAt: { atMs: BigInt(NOW) },
        classification: { case: "classifying", value: {} },
      },
    },
  });

/** A held prompt on TURN whose words run to a second line. */
const multiLineItem = (turn: string): DaemonHoldItem =>
  create(DaemonHoldItemSchema, {
    item: {
      case: "prompt",
      value: {
        turn: { value: turn },
        said: {
          content: { blocks: [{ block: { case: "text", value: { text: "first\nsecond" } } }] },
        },
        queuedAt: { atMs: BigInt(NOW) },
        classification: { case: "classifying", value: {} },
      },
    },
  });

const offerItem = (): DaemonHoldItem =>
  create(DaemonHoldItemSchema, {
    item: {
      case: "offer",
      value: {
        offer: { case: "mergeDequeue", value: { headline: { text: "keep the slot?" } } },
      },
    },
  });

const tray = (items: DaemonHoldItem[]): DaemonHoldTray =>
  create(DaemonHoldTraySchema, { items });

/** Let the stream's first push reach the mount. */
const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

describe("drawDaemonHoldTray", () => {
  it("accepts a tray carrying no heading and draws no counter", () => {
    // Arrange / Act: the heading is RETIRED from the proto (owner ruling 5).
    const drawn = drawDaemonHoldTray(tray([promptItem("t1")]), trayContext());
    // Assert
    expect(drawn).not.toBeNull();
    expect(drawn?.textContent).not.toContain("held (");
  });

  it("draws nothing at all for an empty tray", () => {
    // Arrange / Act
    const drawn = drawDaemonHoldTray(tray([]), trayContext());
    // Assert
    expect(drawn).toBeNull();
  });

  it("draws the region once something is held", () => {
    // Arrange / Act
    const drawn = drawDaemonHoldTray(tray([promptItem("t1")]), trayContext());
    // Assert
    expect(drawn?.classList.contains("hold-tray")).toBe(true);
  });

  it("keeps the served display order", () => {
    const drawn = drawDaemonHoldTray(
      tray([promptItem("t1"), promptItem("t2")]),
      trayContext(),
    );
    const turns = [...(drawn?.querySelectorAll("[data-held-turn]") ?? [])].map((node) =>
      node.getAttribute("data-held-turn"),
    );
    expect(turns).toEqual(["t1", "t2"]);
  });

  it("delimits its rows with the one shared list rule", () => {
    const drawn = drawDaemonHoldTray(tray([promptItem("t1")]), trayContext());
    expect(drawn?.querySelector(".hold-tray-items")?.classList.contains("list-rows")).toBe(true);
  });

  it("names every held card, in order, by its held-entry selector", () => {
    // Arrange
    const drawn = drawDaemonHoldTray(tray([promptItem("t1"), offerItem(), promptItem("t2")]), trayContext());
    // Act
    const entries = [...(drawn?.querySelectorAll(HELD_ENTRY_SELECTOR) ?? [])];
    // Assert
    expect(entries.map((node) => node.getAttribute("data-held-turn"))).toEqual(["t1", null, "t2"]);
  });

});

describe("drawDaemonHoldItem", () => {
  it("routes a prompt to the held-prompt card", () => {
    const drawn = drawDaemonHoldItem(promptItem("t1"), trayContext(), "p");
    expect(drawn.getAttribute("data-held-turn")).toBe("t1");
  });

  it("routes an offer to the offer card", () => {
    const drawn = drawDaemonHoldItem(offerItem(), trayContext(), "p");
    expect(drawn.getAttribute("data-offer")).toBe("mergeDequeue");
  });

  it("refuses an item whose oneof is unset", () => {
    const bare = create(DaemonHoldItemSchema, {});
    expect(() => drawDaemonHoldItem(bare, trayContext(), "p")).toThrow(MalformedView);
  });
});

describe("mountHoldTray", () => {
  it("draws the first push into the host", async () => {
    const host = document.createElement("section");
    const ctx = streamingContext(async function* () {
      yield tray([promptItem("t1")]);
    });
    const handle = mountHoldTray(host, ctx);
    await settle();
    expect(host.querySelector('[data-held-turn="t1"]')).not.toBeNull();
    handle.dispose();
  });

  it("replaces the tray whole on the next push", async () => {
    const host = document.createElement("section");
    const ctx = streamingContext(async function* () {
      yield tray([promptItem("t1")]);
      yield tray([promptItem("t2")]);
    });
    const handle = mountHoldTray(host, ctx);
    await settle();
    expect(host.querySelector('[data-held-turn="t1"]')).toBeNull();
    expect(host.querySelector('[data-held-turn="t2"]')).not.toBeNull();
    handle.dispose();
  });

  it("takes the previous drawing's ticker subscriptions down with it", async () => {
    const ticker = fakeTicker();
    const host = document.createElement("section");
    const ctx = streamingContext(async function* () {
      yield tray([promptItem("t1")]);
      yield tray([promptItem("t2")]);
    }, ticker);
    const handle = mountHoldTray(host, ctx);
    await settle();
    expect(ticker.subscribers()).toBe(1);
    handle.dispose();
  });

  it("leaves the host with no children at all when the tray is empty", async () => {
    // Arrange
    const host = document.createElement("section");
    const ctx = streamingContext(async function* () {
      yield tray([promptItem("t1")]);
      yield tray([]);
    });
    // Act
    const handle = mountHoldTray(host, ctx);
    await settle();
    // Assert
    expect(host.childElementCount).toBe(0);
    handle.dispose();
  });

  it("empties the host and drops every subscription on dispose", async () => {
    const ticker = fakeTicker();
    const host = document.createElement("section");
    const ctx = streamingContext(async function* () {
      yield tray([promptItem("t1")]);
    }, ticker);
    const handle = mountHoldTray(host, ctx);
    await settle();
    handle.dispose();
    expect(host.childElementCount).toBe(0);
    expect(ticker.subscribers()).toBe(0);
  });

  it("opens a held prompt through the one toggle, armed on the tray's host", async () => {
    // Arrange
    const host = document.createElement("section");
    const ctx = streamingContext(async function* () {
      yield tray([multiLineItem("t1")]);
    });
    const handle = mountHoldTray(host, ctx);
    await settle();
    // Act — a click on the header strip, as on any bubble.
    host.querySelector<HTMLElement>(".queued-head")?.click();
    // Assert
    expect(host.querySelector('[data-held-turn="t1"] > .bubble-scroll')?.classList.contains("expanded")).toBe(true);
    handle.dispose();
  });

  it("redraws a re-served held prompt in place, the same card", async () => {
    // Arrange
    const host = document.createElement("section");
    let pushed!: () => void;
    const next = new Promise<void>((resolve) => (pushed = resolve));
    const ctx = streamingContext(async function* () {
      yield tray([multiLineItem("t1")]);
      await next;
      yield tray([multiLineItem("t1")]);
    });
    const handle = mountHoldTray(host, ctx);
    await settle();
    const first = host.querySelector('[data-held-turn="t1"]');
    // Act
    pushed();
    await settle();
    // Assert
    expect(host.querySelector('[data-held-turn="t1"]')).toBe(first);
    handle.dispose();
  });

  it("keeps a held prompt's open fold open across a push that re-serves it", async () => {
    // Arrange — the second push waits until the reader has opened the fold.
    const host = document.createElement("section");
    let opened!: () => void;
    const reader = new Promise<void>((resolve) => (opened = resolve));
    const ctx = streamingContext(async function* () {
      yield tray([multiLineItem("t1")]);
      await reader;
      yield tray([multiLineItem("t1")]);
    });
    const handle = mountHoldTray(host, ctx);
    await settle();
    host.querySelector<HTMLElement>(".queued-text")?.click();
    // Act
    opened();
    await settle();
    // Assert — the card, redrawn in place, still open.
    expect(host.querySelector('[data-held-turn="t1"] > .bubble-scroll')?.classList.contains("expanded"))
      .toBe(true);
    handle.dispose();
  });

  it("does not open another turn's fold on the push after one was opened", async () => {
    // Arrange
    const host = document.createElement("section");
    let opened!: () => void;
    const reader = new Promise<void>((resolve) => (opened = resolve));
    const ctx = streamingContext(async function* () {
      yield tray([multiLineItem("t1")]);
      await reader;
      yield tray([multiLineItem("t2")]);
    });
    const handle = mountHoldTray(host, ctx);
    await settle();
    host.querySelector<HTMLElement>(".queued-text")?.click();
    // Act
    opened();
    await settle();
    // Assert
    expect(host.querySelector('[data-held-turn="t2"] > .bubble-scroll')?.classList.contains("expanded"))
      .toBe(false);
    handle.dispose();
  });
});
