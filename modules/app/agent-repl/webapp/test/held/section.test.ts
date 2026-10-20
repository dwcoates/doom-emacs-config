// @vitest-environment jsdom
import { createEditorIngressView, type EditorIngressView } from "../../src/held/editor-ingress.js";
import { afterEach, describe, expect, it, vi } from "vitest";
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
import {
  WatchFeedHeldFoldSchema,
  type WatchFeedHeldFold,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import type { Ticker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import type { HeldContext } from "../../src/held/context.js";
import { testHeldContext } from "./held-context.js";
import { editorIngressEntry, heldIngressItem, heldPromptItem } from "../feed/harness.js";
import {
  FOLDING_BADGE_CLASS,
  FOLDING_BADGE_LABEL,
  FOLD_FLASH_CLASS,
} from "../../src/held/fold-motion.js";
import {
  EDITOR_INGRESS_ID_PREFIX,
  FOLDING_ATTRIBUTE,
  HELD_ENTRY_SELECTOR,
  HELD_ITEM_ATTRIBUTE,
  drawHeldItem,
  drawSection,
  mountHeldSection,
} from "../../src/held/section.js";
import { resetLoggingForTests } from "../../src/log.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { type DeferredAnimation, deferredAnimation } from "../deferred-animation.js";

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
const promptItem = (turn: string, text = "hi"): FeedHeldItem => heldPromptItem(`prompt:${turn}`, turn, text);

/** A held prompt on TURN whose words run to a second line. */
const multiLineItem = (turn: string): FeedHeldItem => promptItem(turn, "first\nsecond");

const section = (items: FeedHeldItem[]): FeedHeld => create(FeedHeldSchema, { items });

/** A section mounted on a fresh host. */
function mounted(
  opts: { ticker?: Ticker; promptHeld?: (turn: string) => void; editorIngress?: EditorIngressView } = {},
): {
  host: HTMLElement;
  held: ReturnType<typeof mountHeldSection>;
} {
  const host = document.createElement("section");
  const held = mountHeldSection(host, appContext(opts.ticker), {
    promptHeld: opts.promptHeld ?? vi.fn(),
    editorIngress: opts.editorIngress ?? createEditorIngressView(),
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
    const drawn = draw(section([promptItem("t1"), promptItem("t2")]));
    // Act
    const entries = [...(drawn?.querySelectorAll(HELD_ENTRY_SELECTOR) ?? [])];
    // Assert
    expect(entries.map((node) => node.getAttribute(HELD_ITEM_ATTRIBUTE))).toEqual([
      "prompt:t1",
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

  it("marks the item's element with the item's id", () => {
    expect(draw(promptItem("t1")).getAttribute(HELD_ITEM_ATTRIBUTE)).toBe("prompt:t1");
  });

  it("refuses an item whose oneof is unset", () => {
    const bare = create(FeedHeldItemSchema, { id: { value: "x" } });
    expect(() => draw(bare)).toThrow(MalformedView);
  });

  it("refuses an item with no id", () => {
    const anonymous = create(FeedHeldItemSchema, { item: promptItem("t1").item });
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

describe("mountHeldSection's folds", () => {
  /** The fold of item FOLDED into item INTO, as a held change names it. */
  const fold = (folded: string, into: string): WatchFeedHeldFold =>
    create(WatchFeedHeldFoldSchema, { folded: { value: folded }, into: { value: into } });

  /** Every fade the section starts finishes when the test says. */
  function deferredFades(): DeferredAnimation {
    const fade = deferredAnimation();
    vi.spyOn(Element.prototype, "animate").mockReturnValue(fade.animation);
    return fade;
  }

  /** A section holding t1 then t2, whose next change folds t2 into t1. */
  function foldedT2(): ReturnType<typeof mounted> {
    const m = mounted();
    m.held.apply(section([promptItem("t1"), promptItem("t2")]));
    m.held.apply(section([promptItem("t1")]), [fold("prompt:t2", "prompt:t1")]);
    return m;
  }

  /** The list's children, each named by its held item or its leaving id. */
  const listed = (host: HTMLElement): string[] =>
    [...(host.querySelector(".held-section-items")?.children ?? [])].map(
      (el) => el.getAttribute(HELD_ITEM_ATTRIBUTE) ?? `leaving ${el.getAttribute(FOLDING_ATTRIBUTE) ?? "?"}`,
    );

  afterEach(() => {
    vi.restoreAllMocks();
    resetLoggingForTests();
  });

  it("flashes the entry folded into", () => {
    // Arrange
    deferredFades();
    // Act
    const m = foldedT2();
    // Assert
    expect(m.host.querySelector('[data-held-turn="t1"]')?.classList.contains(FOLD_FLASH_CLASS)).toBe(true);
    m.held.dispose();
  });

  it("keeps the folded card, leaving, just under the entry it was folded into", () => {
    // Arrange
    deferredFades();
    const m = mounted();
    m.held.apply(section([promptItem("t1"), promptItem("t2"), promptItem("t3")]));
    // Act
    m.held.apply(section([promptItem("t1"), promptItem("t3")]), [fold("prompt:t2", "prompt:t1")]);
    // Assert
    expect(listed(m.host)).toEqual(["prompt:t1", "leaving prompt:t2", "prompt:t3"]);
    m.held.dispose();
  });

  it("badges the leaving card folding above", () => {
    // Arrange
    deferredFades();
    // Act
    const m = foldedT2();
    // Assert
    expect(m.host.querySelector(`[${FOLDING_ATTRIBUTE}] .${FOLDING_BADGE_CLASS}`)?.textContent).toBe(FOLDING_BADGE_LABEL);
    m.held.dispose();
  });

  it("does not count the leaving card among the held entries", () => {
    // Arrange
    deferredFades();
    // Act
    const m = foldedT2();
    // Assert
    expect([...m.host.querySelectorAll(HELD_ENTRY_SELECTOR)].map((el) => el.getAttribute(HELD_ITEM_ATTRIBUTE))).toEqual([
      "prompt:t1",
    ]);
    m.held.dispose();
  });

  it("keeps the leaving card under its entry across a later change", () => {
    // Arrange
    deferredFades();
    const m = foldedT2();
    // Act
    m.held.apply(section([promptItem("t1"), promptItem("t4")]));
    // Assert
    expect(listed(m.host)).toEqual(["prompt:t1", "leaving prompt:t2", "prompt:t4"]);
    m.held.dispose();
  });

  it("takes the leaving card down once its fade has run", async () => {
    // Arrange
    const fades = deferredFades();
    const m = foldedT2();
    // Act
    await fades.finish();
    // Assert
    expect(listed(m.host)).toEqual(["prompt:t1"]);
    m.held.dispose();
  });

  it("leaves the host empty once the last leaving card goes from an emptied section", async () => {
    // Arrange: the entry folded into is one this section draws no card for.
    const fades = deferredFades();
    const m = mounted();
    m.held.apply(section([promptItem("t2")]));
    m.held.apply(section([]), [fold("prompt:t2", "prompt:hidden")]);
    // Act
    await fades.finish();
    // Assert
    expect(m.host.childElementCount).toBe(0);
    m.held.dispose();
  });

  it("draws the leaving card last when the entry folded into is not drawn", () => {
    // Arrange
    deferredFades();
    const m = mounted();
    m.held.apply(section([promptItem("t1"), promptItem("t2")]));
    // Act
    m.held.apply(section([promptItem("t1")]), [fold("prompt:t2", "prompt:hidden")]);
    // Assert
    expect(listed(m.host)).toEqual(["prompt:t1", "leaving prompt:t2"]);
    m.held.dispose();
  });

  it("draws no leaving card for a prompt it never drew, and still flashes", () => {
    // Arrange
    const animate = vi.spyOn(Element.prototype, "animate");
    const m = mounted();
    m.held.apply(section([promptItem("t1")]));
    // Act
    m.held.apply(section([promptItem("t1")]), [fold("prompt:t2", "prompt:t1")]);
    // Assert
    expect([listed(m.host), animate.mock.calls.length, m.host.querySelector(`.${FOLD_FLASH_CLASS}`) !== null]).toEqual([
      ["prompt:t1"],
      0,
      true,
    ]);
    m.held.dispose();
  });

  it("logs a fold of a prompt it never drew", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const m = mounted();
    m.held.apply(section([promptItem("t1")]));
    // Act
    m.held.apply(section([promptItem("t1")]), [fold("prompt:t2", "prompt:t1")]);
    // Assert
    expect((await forwardedRecord(capture, "held.fold.undrawn")).context).toMatchObject({
      folded: "prompt:t2",
      into: "prompt:t1",
    });
    m.held.dispose();
  });

  it("logs a fold into an entry it draws no card for", async () => {
    // Arrange
    deferredFades();
    const capture = captureLogRecords("debug");
    const m = mounted();
    m.held.apply(section([promptItem("t2")]));
    // Act
    m.held.apply(section([]), [fold("prompt:t2", "prompt:hidden")]);
    // Assert
    expect((await forwardedRecord(capture, "held.fold.into-undrawn")).context).toMatchObject({ into: "prompt:hidden" });
    m.held.dispose();
  });

  it("refuses a fold of a prompt the same change still holds, logging it", async () => {
    // Arrange
    const capture = captureLogRecords();
    const m = mounted();
    m.held.apply(section([promptItem("t1"), promptItem("t2")]));
    // Act
    const act = (): void => m.held.apply(section([promptItem("t1"), promptItem("t2")]), [fold("prompt:t2", "prompt:t1")]);
    // Assert
    expect(act).toThrow(MalformedView);
    expect((await forwardedRecord(capture, "held.fold.still-held")).context).toMatchObject({
      folded: "prompt:t2",
      into: "prompt:t1",
    });
    m.held.dispose();
  });

  it("leaves the drawing as it was when it refuses a fold", () => {
    // Arrange
    const m = mounted();
    m.held.apply(section([promptItem("t1"), promptItem("t2")]));
    // Act
    expect(() => m.held.apply(section([promptItem("t1"), promptItem("t2")]), [fold("prompt:t2", "prompt:t1")])).toThrow();
    // Assert
    expect([listed(m.host), m.host.querySelector(`.${FOLD_FLASH_CLASS}`)]).toEqual([["prompt:t1", "prompt:t2"], null]);
    m.held.dispose();
  });

  it("refuses a fold naming no folded item", () => {
    // Arrange
    const m = mounted();
    const anonymous = create(WatchFeedHeldFoldSchema, { into: { value: "prompt:t1" } });
    // Act + Assert
    expect(() => m.held.apply(section([promptItem("t1")]), [anonymous])).toThrow(MalformedView);
    m.held.dispose();
  });

  it("refuses a fold naming no item folded into", () => {
    // Arrange
    const m = mounted();
    const unaimed = create(WatchFeedHeldFoldSchema, { folded: { value: "prompt:t2" } });
    // Act + Assert
    expect(() => m.held.apply(section([promptItem("t1")]), [unaimed])).toThrow(MalformedView);
    m.held.dispose();
  });

  it("does not redraw once disposed when a leaving card's fade ends", async () => {
    // Arrange
    const fades = deferredFades();
    const m = foldedT2();
    m.held.dispose();
    // Act
    await fades.finish();
    // Assert
    expect(m.host.childElementCount).toBe(0);
  });
});

/** The held item ids HOST draws, in order. */
const drawnIds = (host: HTMLElement): string[] =>
  [...host.querySelectorAll(HELD_ENTRY_SELECTOR)].map((card) => card.getAttribute(HELD_ITEM_ATTRIBUTE) ?? "");

/** The item id an editor ingress card is drawn under. */
const editorId = (key: string): string => `${EDITOR_INGRESS_ID_PREFIX}${key}`;

describe("mountHeldSection: who draws a prompt waiting in the ingress", () => {
  it("draws the editor's entries before the root feed has opened", () => {
    // Arrange
    const editorIngress = createEditorIngressView();
    const { host } = mounted({ editorIngress });
    // Act
    editorIngress.set([editorIngressEntry("k-1")]);
    // Assert
    expect(drawnIds(host)).toEqual([editorId("k-1")]);
  });

  it("draws only the daemon's section once the root feed opened", () => {
    // Arrange
    const editorIngress = createEditorIngressView();
    editorIngress.set([editorIngressEntry("k-1")]);
    const { host, held } = mounted({ editorIngress });
    // Act
    held.opened(section([promptItem("t1"), heldIngressItem("ingress:k-1")]));
    // Assert
    expect(drawnIds(host)).toEqual(["prompt:t1", "ingress:k-1"]);
  });

  it("ignores the editor's list while the root feed is live", () => {
    // Arrange
    const editorIngress = createEditorIngressView();
    const { host, held } = mounted({ editorIngress });
    held.opened(section([]));
    // Act
    editorIngress.set([editorIngressEntry("k-1")]);
    // Assert
    expect(drawnIds(host)).toEqual([]);
  });

  it("draws the editor's entries in place of the daemon's ingress items once the feed is out of reach", () => {
    // Arrange
    const editorIngress = createEditorIngressView();
    editorIngress.set([editorIngressEntry("k-1"), editorIngressEntry("k-2")]);
    const { host, held } = mounted({ editorIngress });
    held.opened(section([promptItem("t1"), heldIngressItem("ingress:k-1")]));
    // Act
    held.unreachable();
    // Assert
    expect(drawnIds(host)).toEqual(["prompt:t1", editorId("k-1"), editorId("k-2")]);
  });

  it("redraws the editor's entries as they change while the feed is out of reach", () => {
    // Arrange
    const editorIngress = createEditorIngressView();
    const { host, held } = mounted({ editorIngress });
    held.opened(section([]));
    held.unreachable();
    // Act
    editorIngress.set([editorIngressEntry("k-3")]);
    // Assert
    expect(drawnIds(host)).toEqual([editorId("k-3")]);
  });

  it("hands the section back to the daemon when the feed opens again", () => {
    // Arrange
    const editorIngress = createEditorIngressView();
    editorIngress.set([editorIngressEntry("k-1")]);
    const { host, held } = mounted({ editorIngress });
    held.opened(section([]));
    held.unreachable();
    // Act
    held.opened(section([heldIngressItem("ingress:k-1")]));
    // Assert
    expect(drawnIds(host)).toEqual(["ingress:k-1"]);
  });

  it("parks the feed when an editor entry is first drawn", () => {
    // Arrange
    const editorIngress = createEditorIngressView();
    const promptHeld = vi.fn();
    mounted({ editorIngress, promptHeld });
    // Act
    editorIngress.set([editorIngressEntry("k-1")]);
    editorIngress.set([editorIngressEntry("k-1")]);
    // Assert
    expect(promptHeld.mock.calls).toEqual([["k-1"]]);
  });

  it("stops redrawing for the editor's list once disposed", () => {
    // Arrange
    const editorIngress = createEditorIngressView();
    const { host, held } = mounted({ editorIngress });
    held.dispose();
    // Act
    editorIngress.set([editorIngressEntry("k-1")]);
    // Assert
    expect(host.childElementCount).toBe(0);
  });

  it("records each change of source", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { held } = mounted();
    // Act
    held.opened(section([]));
    held.unreachable();
    // Assert
    await forwardedRecord(capture, "held.source-daemon");
    await forwardedRecord(capture, "held.source-editor");
  });
});

describe("drawHeldItem: an ingress item", () => {
  it("draws the daemon's ingress item under its own id", () => {
    // Act
    const card = drawHeldItem(heldIngressItem("ingress:k-1"), heldContext(), "item", new Map());
    // Assert
    expect([card.getAttribute(HELD_ITEM_ATTRIBUTE), card.getAttribute("data-held-ingress")]).toEqual(["ingress:k-1", "daemon"]);
  });
});
