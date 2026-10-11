// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  FeedHeldItemSchema,
  FeedHeldSchema,
  type FeedHeld,
  type FeedHeldItem,
} from "../../../proto/gen/ts/frontend/v1/feed_held_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Ticker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import type { HeldContext } from "../../src/held/context.js";
import { testHeldContext } from "./held-context.js";
import {
  HELD_ENTRY_SELECTOR,
  HELD_ITEM_ATTRIBUTE,
  drawHeldItem,
  drawSection,
  mountHeldSection,
} from "../../src/held/section.js";
import { resetLoggingForTests } from "../../src/log.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

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

/** An app context over a daemon that answers nothing; the section calls none. */
function appContext(ticker: Ticker = fakeTicker()): AppContext {
  return testAppContext({
    client: createAgentReplClient(createRouterTransport(({ service }) => service(AgentRepl, {}))),
    workspace: WORKSPACE,
    ticker,
    failures: SINK,
    composerEnabled: false,
  });
}

function heldContext(ticker: Ticker = fakeTicker()): HeldContext {
  return testHeldContext(appContext(ticker));
}

/** A held prompt item on TURN, keyed by its prompt row's id. */
const promptItem = (turn: string, text = "hi"): FeedHeldItem =>
  create(FeedHeldItemSchema, {
    id: { value: `prompt:${turn}` },
    item: {
      case: "prompt",
      value: {
        turn: { value: turn },
        said: { content: { blocks: [{ block: { case: "text", value: { text } } }] } },
        queuedAt: { atMs: BigInt(NOW) },
        classification: { case: "classifying", value: {} },
        badge: {
          label: "classifying",
          detail: "queued — classifying",
          standsFor: { case: "classifying", value: {} },
        },
      },
    },
  });

/** A held prompt on TURN whose words run to a second line. */
const multiLineItem = (turn: string): FeedHeldItem => promptItem(turn, "first\nsecond");

const offerItem = (): FeedHeldItem =>
  create(FeedHeldItemSchema, {
    id: { value: "offer:merge_dequeue" },
    item: {
      case: "offer",
      value: {
        offer: { case: "mergeDequeue", value: { headline: { text: "keep the slot?" } } },
      },
    },
  });

const section = (items: FeedHeldItem[]): FeedHeld => create(FeedHeldSchema, { items });

/** A section mounted on a fresh host. */
function mounted(
  opts: { ticker?: Ticker; promptHeld?: (turn: string) => void } = {},
): {
  host: HTMLElement;
  held: ReturnType<typeof mountHeldSection>;
} {
  const host = document.createElement("section");
  const held = mountHeldSection(host, appContext(opts.ticker), {
    promptHeld: opts.promptHeld ?? vi.fn(),
  });
  return { host, held };
}

describe("drawSection", () => {
  const draw = (u: FeedHeld): HTMLElement | null =>
    drawSection(u, heldContext(), null);

  it("draws nothing at all for an empty section", () => {
    expect(draw(section([]))).toBeNull();
  });

  it("draws the region once something is held", () => {
    expect(draw(section([promptItem("t1")]))?.classList.contains("held-section")).toBe(true);
  });

  it("keeps the served display order", () => {
    const drawn = draw(section([promptItem("t1"), promptItem("t2")]));
    const turns = [...(drawn?.querySelectorAll("[data-held-turn]") ?? [])].map((node) =>
      node.getAttribute("data-held-turn"),
    );
    expect(turns).toEqual(["t1", "t2"]);
  });

  it("lays its items out with no delimiter, as the feed's rows are", () => {
    const drawn = draw(section([promptItem("t1")]));
    expect(drawn?.querySelector(".held-section-items")?.classList.contains("list-rows")).toBe(false);
  });

  it("names every held item, in order, by its held-entry selector", () => {
    // Arrange
    const drawn = draw(section([promptItem("t1"), offerItem(), promptItem("t2")]));
    // Act
    const entries = [...(drawn?.querySelectorAll(HELD_ENTRY_SELECTOR) ?? [])];
    // Assert
    expect(entries.map((node) => node.getAttribute(HELD_ITEM_ATTRIBUTE))).toEqual([
      "prompt:t1",
      "offer:merge_dequeue",
      "prompt:t2",
    ]);
  });
});

describe("drawHeldItem", () => {
  const draw = (u: FeedHeldItem): HTMLElement =>
    drawHeldItem(u, heldContext(), "p", new Map());

  it("routes a prompt to the held-prompt card", () => {
    expect(draw(promptItem("t1")).getAttribute("data-held-turn")).toBe("t1");
  });

  it("routes an offer to the offer card", () => {
    expect(draw(offerItem()).getAttribute("data-offer")).toBe("mergeDequeue");
  });

  it("marks the item's element with the item's id", () => {
    expect(draw(offerItem()).getAttribute(HELD_ITEM_ATTRIBUTE)).toBe("offer:merge_dequeue");
  });

  it("refuses an item whose oneof is unset", () => {
    const bare = create(FeedHeldItemSchema, { id: { value: "x" } });
    expect(() => draw(bare)).toThrow(MalformedView);
  });

  it("refuses an item with no id", () => {
    const anonymous = create(FeedHeldItemSchema, { item: offerItem().item });
    expect(() => draw(anonymous)).toThrow(MalformedView);
  });
});

describe("mountHeldSection", () => {
  it("draws nothing before a section is applied", () => {
    const m = mounted();
    expect(m.host.childElementCount).toBe(0);
    m.held.dispose();
  });

  it("draws the applied section into the host", () => {
    const m = mounted();
    m.held.apply(section([promptItem("t1")]));
    expect(m.host.querySelector('[data-held-turn="t1"]')).not.toBeNull();
    m.held.dispose();
  });

  it("replaces the section whole on the next apply", () => {
    const m = mounted();
    m.held.apply(section([promptItem("t1")]));
    m.held.apply(section([promptItem("t2")]));
    expect([m.host.querySelector('[data-held-turn="t1"]'), m.host.querySelector('[data-held-turn="t2"]') !== null]).toEqual([
      null,
      true,
    ]);
    m.held.dispose();
  });

  it("takes the previous drawing's ticker subscriptions down with it", () => {
    const ticker = fakeTicker();
    const m = mounted({ ticker });
    m.held.apply(section([promptItem("t1")]));
    m.held.apply(section([promptItem("t2")]));
    expect(ticker.subscribers()).toBe(1);
    m.held.dispose();
  });

  it("leaves the host with no children at all when the section is empty", () => {
    const m = mounted();
    m.held.apply(section([promptItem("t1")]));
    m.held.apply(section([]));
    expect(m.host.childElementCount).toBe(0);
    m.held.dispose();
  });

  it("empties the host and drops every subscription on dispose", () => {
    const ticker = fakeTicker();
    const m = mounted({ ticker });
    m.held.apply(section([promptItem("t1")]));
    m.held.dispose();
    expect([m.host.childElementCount, ticker.subscribers()]).toEqual([0, 0]);
  });

  it("opens a held prompt through the one toggle, armed on the section's host", () => {
    // Arrange
    const m = mounted();
    m.held.apply(section([multiLineItem("t1")]));
    // Act — a click on the header strip, as on any bubble.
    m.host.querySelector<HTMLElement>(".queued-head")?.click();
    // Assert
    expect(m.host.querySelector('[data-held-turn="t1"] > .bubble-scroll')?.classList.contains("expanded")).toBe(true);
    m.held.dispose();
  });

  it("redraws a re-served held prompt in place, the same card", () => {
    // Arrange
    const m = mounted();
    m.held.apply(section([multiLineItem("t1")]));
    const first = m.host.querySelector('[data-held-turn="t1"]');
    // Act
    m.held.apply(section([multiLineItem("t1")]));
    // Assert
    expect(m.host.querySelector('[data-held-turn="t1"]')).toBe(first);
    m.held.dispose();
  });

  it("keeps a held prompt's open fold open across an apply that re-serves it", () => {
    // Arrange
    const m = mounted();
    m.held.apply(section([multiLineItem("t1")]));
    m.host.querySelector<HTMLElement>(".queued-text")?.click();
    // Act
    m.held.apply(section([multiLineItem("t1")]));
    // Assert
    expect(m.host.querySelector('[data-held-turn="t1"] > .bubble-scroll')?.classList.contains("expanded")).toBe(true);
    m.held.dispose();
  });

  it("does not open another turn's fold on the apply after one was opened", () => {
    // Arrange
    const m = mounted();
    m.held.apply(section([multiLineItem("t1")]));
    m.host.querySelector<HTMLElement>(".queued-text")?.click();
    // Act
    m.held.apply(section([multiLineItem("t2")]));
    // Assert
    expect(m.host.querySelector('[data-held-turn="t2"] > .bubble-scroll')?.classList.contains("expanded")).toBe(false);
    m.held.dispose();
  });
});

describe("mountHeldSection's park on a held prompt's first draw", () => {
  /** A section given APPLIES in order, and a promptHeld spy recording whether its card was placed. */
  function applied(applies: FeedHeld[]): {
    promptHeld: ReturnType<typeof vi.fn>;
    placed: boolean[];
    dispose(): void;
  } {
    const placed: boolean[] = [];
    let host: HTMLElement | null = null;
    const promptHeld = vi.fn((turn: string) => {
      placed.push(host?.querySelector(`[data-held-turn="${turn}"]`) !== null);
    });
    const m = mounted({ promptHeld });
    host = m.host;
    for (const u of applies) m.held.apply(u);
    return { promptHeld, placed, dispose: () => m.held.dispose() };
  }

  it("parks once when a held prompt's card is first drawn", () => {
    const m = applied([section([promptItem("t1")])]);
    expect(m.promptHeld.mock.calls).toEqual([["t1"]]);
    m.dispose();
  });

  it("parks only after the card is placed in the section", () => {
    const m = applied([section([promptItem("t1")])]);
    expect(m.placed).toEqual([true]);
    m.dispose();
  });

  it("parks again for a second held prompt landing beside the first", () => {
    const m = applied([section([promptItem("t1")]), section([promptItem("t1"), promptItem("t2")])]);
    expect(m.promptHeld.mock.calls).toEqual([["t1"], ["t2"]]);
    m.dispose();
  });

  it("does not park on a re-push of a card already drawn", () => {
    const m = applied([section([promptItem("t1")]), section([multiLineItem("t1")])]);
    expect(m.promptHeld.mock.calls).toEqual([["t1"]]);
    m.dispose();
  });

  it("does not park when a held prompt leaves the section", () => {
    const m = applied([section([promptItem("t1"), promptItem("t2")]), section([promptItem("t1")])]);
    expect(m.promptHeld.mock.calls).toEqual([["t2"]]);
    m.dispose();
  });

  it("does not park for an offer, which is no prompt", () => {
    const m = applied([section([offerItem()])]);
    expect(m.promptHeld).not.toHaveBeenCalled();
    m.dispose();
  });

  it("logs the landing with the turns it parked for", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    const m = applied([section([promptItem("t1")])]);
    // Assert
    const record = await forwardedRecord(capture, "held.held-prompt-landed");
    expect(record.context).toMatchObject({ turns: ["t1"] });
    m.dispose();
    resetLoggingForTests();
  });
});
