// @vitest-environment jsdom
/**
 * hover-stamp — a row's hover stamp, read and recorded on its element on
 * every push, and handed to the footer while the pointer is on its card.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { FeedHoverStampLabelSchema, FeedHoverStampSchema, FeedRowSchema, type FeedHoverStamp } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  hoverStampOf,
  hoveredRowAt,
  installHoverStamp,
  readHoverStamp,
  recordRowHover,
  setRowHover,
  type HoverStamp,
} from "../../src/feed/hover-stamp.js";
import { heatColor } from "../../src/heat.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { FEED_ROW_ATTRIBUTE } from "../../src/feed/row-id.js";
import { createFocusLeft, type FocusLeftSource } from "../../src/focus-left.js";
import { createPresence } from "../../src/presence.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

const uninstalls: (() => void)[] = [];
afterEach(() => {
  while (uninstalls.length > 0) uninstalls.pop()?.();
  document.body.replaceChildren();
});

/** A read stamp labelled LABEL. */
function usage(label: string): HoverStamp {
  return { label, tokens: null, atMs: 1_000, startedAtMs: 500 };
}

/**
 * A row ROW in HOST, its card a bubble of ROLE holding a paragraph; answers
 * the CARD. The row's element is the card's parent.
 */
function bubble(host: HTMLElement, role = "response", row = "row-1"): HTMLElement {
  const item = document.createElement("article");
  item.setAttribute(FEED_ROW_ATTRIBUTE, row);
  const el = document.createElement("div");
  el.className = "bubble";
  el.setAttribute("data-role", role);
  el.innerHTML = "<p>prose</p>";
  item.append(el);
  host.append(item);
  return el;
}

/** The row element CARD is drawn in. */
function rowOf(card: HTMLElement): HTMLElement {
  const row = card.parentElement;
  if (row === null) throw new Error("the card is in no row");
  return row;
}

/** Record STAMP on CARD's row. */
function stampCard(card: HTMLElement, stamp: HoverStamp | undefined): void {
  setRowHover(rowOf(card), stamp);
}

/** A focus-left signal of the test's own, fired by hand. */
function focusSource(): FocusLeftSource {
  const presence = createPresence(document, false);
  const focus = createFocusLeft(window, presence);
  uninstalls.push(() => {
    focus.dispose();
    presence.dispose();
  });
  return focus;
}

/**
 * A feed host in the document with the hover installed, what SHOW was called
 * with, and the focus-left signal the hover listens to.
 */
function hovering(): { host: HTMLElement; shown: (HoverStamp | null)[]; focus: FocusLeftSource } {
  const host = document.createElement("div");
  document.body.append(host);
  const shown: (HoverStamp | null)[] = [];
  const focus = focusSource();
  uninstalls.push(installHoverStamp(host, (u: HoverStamp | null) => shown.push(u), focus));
  return { host, shown, focus };
}

/** Move the pointer over EL to the window point (X, Y). */
function moveTo(el: Element, x: number, y: number): void {
  el.dispatchEvent(new MouseEvent("mousemove", { bubbles: true, clientX: x, clientY: y }));
}

/** Move the pointer onto EL. */
function over(el: Element): void {
  el.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
}

/** A wire stamp labelled "Tool call Bash" with the given fields. */
function wire(init: MessageInitShape<typeof FeedHoverStampSchema> = {}): FeedHoverStamp {
  const stamp = create(FeedHoverStampSchema, init);
  stamp.label = create(FeedHoverStampLabelSchema, { text: "Tool call Bash" });
  return stamp;
}

describe("readHoverStamp", () => {
  it("reads the label, the figure in its heat's color, the instant and the span's start", () => {
    // Arrange
    const stamp = wire({ atMs: 5_000n, startedAtMs: 2_000n, tokens: { text: "2.1k", heat: { position: 0.25 } } });
    // Act / Assert
    expect(readHoverStamp(stamp, "p")).toEqual({
      label: "Tool call Bash",
      tokens: { text: "2.1k", color: heatColor(0.25, "p") },
      atMs: 5_000,
      startedAtMs: 2_000,
    });
  });

  it("reads a stamp with no figure as having none", () => {
    // Act / Assert
    expect(readHoverStamp(wire({ atMs: 5_000n }), "p").tokens).toBeNull();
  });

  it("reads a unit still under way as not settled", () => {
    // Act / Assert
    expect(readHoverStamp(wire({ startedAtMs: 2_000n }), "p").atMs).toBeNull();
  });

  it("reads a stamp with no span start as having none", () => {
    // Act / Assert
    expect(readHoverStamp(wire({ atMs: 5_000n }), "p").startedAtMs).toBeNull();
  });

  it("reads a span that settled where it began", () => {
    // Act / Assert
    expect(readHoverStamp(wire({ atMs: 5_000n, startedAtMs: 5_000n }), "p").startedAtMs).toBe(5_000);
  });

  it("refuses a span that settled before it began, naming its field", () => {
    // Act / Assert
    expect(() => readHoverStamp(wire({ atMs: 5_000n, startedAtMs: 6_000n }), "FeedRow.hover")).toThrow(
      new MalformedView("FeedRow.hover.started_at_ms", "the span began at 6000, after it settled at 5000"),
    );
  });

  it("refuses a span start beyond a safe integer", () => {
    // Act / Assert
    expect(() => readHoverStamp(wire({ startedAtMs: BigInt(Number.MAX_SAFE_INTEGER) + 1n }), "p")).toThrow(MalformedView);
  });

  it("refuses an instant beyond a safe integer", () => {
    // Act / Assert
    expect(() => readHoverStamp(wire({ atMs: BigInt(Number.MAX_SAFE_INTEGER) + 1n }), "p")).toThrow(MalformedView);
  });

  it("refuses a figure with no heat, naming its field", () => {
    // Act / Assert
    expect(() => readHoverStamp(wire({ tokens: { text: "2.1k" } }), "FeedRow.hover")).toThrow("FeedRow.hover.tokens.heat");
  });

  it("refuses a heat outside [0, 1]", () => {
    // Act / Assert
    expect(() => readHoverStamp(wire({ tokens: { text: "2.1k", heat: { position: 2 } } }), "p")).toThrow(MalformedView);
  });

  it("refuses a stamp with no label, naming its field", () => {
    // Arrange
    const stamp = create(FeedHoverStampSchema, { atMs: 5_000n });
    // Act / Assert
    expect(() => readHoverStamp(stamp, "FeedRow.hover")).toThrow("FeedRow.hover.label");
  });
});

describe("recordRowHover", () => {
  it("records the row's stamp on its element", () => {
    // Arrange
    const el = document.createElement("article");
    // Act
    recordRowHover(el, create(FeedRowSchema, { hover: wire({ atMs: 5_000n }) }));
    // Assert
    expect(hoverStampOf(el)).toEqual({ label: "Tool call Bash", tokens: null, atMs: 5_000, startedAtMs: null });
  });

  it("forgets the stamp when a push of the row carries none", () => {
    // Arrange
    const el = document.createElement("article");
    recordRowHover(el, create(FeedRowSchema, { hover: wire() }));
    // Act
    recordRowHover(el, create(FeedRowSchema, {}));
    // Assert
    expect(hoverStampOf(el)).toBeNull();
  });

  it("refuses a malformed stamp as a malformed view", () => {
    // Arrange
    const el = document.createElement("article");
    const row = create(FeedRowSchema, { hover: wire({ atMs: 5_000n, startedAtMs: 6_000n }) });
    // Act / Assert
    expect(() => recordRowHover(el, row)).toThrow(MalformedView);
  });

  it("keeps no stamp for a row whose stamp it refused", () => {
    // Arrange
    const el = document.createElement("article");
    recordRowHover(el, create(FeedRowSchema, { hover: wire() }));
    // Act
    expect(() => recordRowHover(el, create(FeedRowSchema, { hover: wire({ tokens: { text: "1k" } }) }))).toThrow(MalformedView);
    // Assert
    expect(hoverStampOf(el)).toBeNull();
  });
});

describe("setRowHover", () => {
  it("records a row's stamp", () => {
    // Arrange
    const el = document.createElement("div");
    const u = usage("Prompt");
    // Act
    setRowHover(el, u);
    // Assert
    expect(hoverStampOf(el)).toBe(u);
  });

  it("forgets it when handed none", () => {
    // Arrange
    const el = document.createElement("div");
    setRowHover(el, usage("Prompt"));
    // Act
    setRowHover(el, undefined);
    // Assert
    expect(hoverStampOf(el)).toBeNull();
  });
});

describe("hoveredRowAt", () => {
  it("answers the row whose card the pointer is on", () => {
    // Arrange
    const host = document.createElement("div");
    const card = bubble(host);
    // Act / Assert
    expect(hoveredRowAt(host, card.querySelector("p"))).toBe(rowOf(card));
  });

  it("answers the innermost row of a nested card", () => {
    // Arrange
    const host = document.createElement("div");
    const outer = bubble(host, "merge", "outer");
    const inner = bubble(outer as HTMLElement, "tool", "inner");
    // Act / Assert
    expect(hoveredRowAt(host, inner)).toBe(rowOf(inner));
  });

  it("answers the row around a nested row when the pointer is on the nested row's own element", () => {
    // Arrange
    const host = document.createElement("div");
    const outer = bubble(host, "merge", "outer");
    const inner = bubble(outer as HTMLElement, "tool", "inner");
    // Act / Assert
    expect(hoveredRowAt(host, rowOf(inner))).toBe(rowOf(outer));
  });

  it("answers none for a pointer on a top-level row's own element", () => {
    // Arrange
    const host = document.createElement("div");
    const card = bubble(host);
    // Act / Assert
    expect(hoveredRowAt(host, rowOf(card))).toBeNull();
  });

  it("answers none for a row outside the host", () => {
    // Arrange
    const host = document.createElement("div");
    const outside = document.createElement("div");
    // Act / Assert
    expect(hoveredRowAt(host, bubble(outside))).toBeNull();
  });
});

describe("installHoverStamp", () => {
  it("shows the stamp of the row whose card is under the pointer", () => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    const u = usage("1k");
    stampCard(el, u);
    // Act
    over(el.querySelector("p")!);
    // Assert
    expect(shown).toEqual([u]);
  });

  it("shows nothing again while the pointer moves inside the same card", () => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    over(el);
    // Act
    over(el.querySelector("p")!);
    // Assert
    expect(shown).toHaveLength(1);
  });

  it("shows the next row's stamp when the pointer moves to it", () => {
    // Arrange
    const { host, shown } = hovering();
    const first = bubble(host, "response", "row-1");
    const second = bubble(host, "response", "row-2");
    stampCard(first, usage("1k"));
    stampCard(second, usage("2k"));
    over(first);
    // Act
    over(second);
    // Assert
    expect(shown.map((u) => u?.label)).toEqual(["1k", "2k"]);
  });

  it.each([
    ["a row with no stamp", "prompt"],
    ["no bubble at all", null],
  ])("gives the cell back when the pointer moves onto %s", (_name, role) => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    over(el);
    const elsewhere = role === null ? document.createElement("span") : bubble(host, role, "row-2");
    if (role === null) host.append(elsewhere);
    // Act
    over(elsewhere);
    // Assert
    expect(shown.map((u) => u?.label ?? null)).toEqual(["1k", null]);
  });

  it("shows nothing for a row with no stamp", () => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    // Act
    over(el);
    // Assert
    expect(shown).toEqual([]);
  });

  it("gives the cell back when the pointer leaves the feed", () => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    over(el);
    // Act
    host.dispatchEvent(new MouseEvent("mouseleave"));
    // Assert
    expect(shown.map((u) => u?.label ?? null)).toEqual(["1k", null]);
  });

  it("restates the hovered row's stamp when a push records a new one", () => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    over(el);
    // Act
    stampCard(el, usage("2k"));
    // Assert
    expect(shown.map((u) => u?.label)).toEqual(["1k", "2k"]);
  });

  it("ignores a new stamp recorded for a row the pointer is not on", () => {
    // Arrange
    const { host, shown } = hovering();
    const hovered = bubble(host, "response", "row-1");
    const other = bubble(host, "response", "row-2");
    stampCard(hovered, usage("1k"));
    over(hovered);
    // Act
    stampCard(other, usage("9k"));
    // Assert
    expect(shown.map((u) => u?.label)).toEqual(["1k"]);
  });

  it("ignores a row outside its host", () => {
    // Arrange
    const { shown } = hovering();
    const outside = document.createElement("div");
    document.body.append(outside);
    const el = bubble(outside);
    stampCard(el, usage("1k"));
    // Act
    over(el);
    // Assert
    expect(shown).toEqual([]);
  });

  it("gives the cell back, and stops following the pointer, once uninstalled", () => {
    // Arrange
    const host = document.createElement("div");
    document.body.append(host);
    const show = vi.fn();
    const uninstall = installHoverStamp(host, show, focusSource());
    const el = bubble(host);
    stampCard(el, usage("1k"));
    over(el);
    // Act
    uninstall();
    over(el);
    stampCard(el, usage("2k"));
    // Assert
    expect(show.mock.calls.map(([u]) => (u as HoverStamp | null)?.label ?? null)).toEqual(["1k", null]);
  });

  it("records what it shows, naming the row", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { host } = hovering();
    const el = bubble(host, "response", "row-7");
    stampCard(el, usage("1k"));
    // Act
    over(el);
    // Assert
    const record = await forwardedRecord(capture, "feed.hover-stamp");
    expect([record.context?.row, record.context?.label]).toEqual(["row-7", "1k"]);
  });
});

describe("installHoverStamp, when the focus leaves the page", () => {
  it("gives the cell back at once", () => {
    // Arrange
    const { host, shown, focus } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    over(el);
    // Act
    focus.left("Emacs moved its focus");
    // Assert
    expect(shown.map((u) => u?.label ?? null)).toEqual(["1k", null]);
  });

  it("shows nothing while the pointer rests, even when it is reported over a bubble again", () => {
    // Arrange
    const { host, shown, focus } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    moveTo(el, 10, 10);
    over(el);
    focus.left("the page lost focus");
    // Act
    over(el);
    moveTo(el, 10, 10);
    // Assert
    expect(shown.map((u) => u?.label ?? null)).toEqual(["1k", null]);
  });

  it("answers again once the pointer moves, showing the bubble under it", () => {
    // Arrange
    const { host, shown, focus } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    moveTo(el, 10, 10);
    over(el);
    focus.left("the page lost focus");
    // Act
    moveTo(el, 11, 10);
    // Assert
    expect(shown.map((u) => u?.label ?? null)).toEqual(["1k", null, "1k"]);
  });

  it("ignores a restated stamp of the bubble it let go of", () => {
    // Arrange
    const { host, shown, focus } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    over(el);
    focus.left("the page was hidden");
    // Act
    stampCard(el, usage("2k"));
    // Assert
    expect(shown.map((u) => u?.label ?? null)).toEqual(["1k", null]);
  });

  it("records the letting go, naming why", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { host, focus } = hovering();
    const el = bubble(host);
    stampCard(el, usage("1k"));
    over(el);
    // Act
    focus.left("Emacs moved its focus");
    // Assert
    const record = await forwardedRecord(capture, "feed.hover-stamp.focus-left");
    expect([record.level.case, record.context?.why, record.context?.shown]).toEqual(["debug", "Emacs moved its focus", true]);
  });

  it("stops listening once uninstalled", () => {
    // Arrange
    const host = document.createElement("div");
    document.body.append(host);
    const show = vi.fn();
    const focus = focusSource();
    const uninstall = installHoverStamp(host, show, focus);
    uninstall();
    show.mockClear();
    // Act
    focus.left("Emacs moved its focus");
    // Assert
    expect(show).not.toHaveBeenCalled();
  });
});
