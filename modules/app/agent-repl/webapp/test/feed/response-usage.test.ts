// @vitest-environment jsdom
/**
 * response-usage — a response bubble's usage stamp, read at its draw and
 * handed to the footer while the pointer is on the bubble.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedResponseUsageStampSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  installResponseUsageHover,
  readResponseUsage,
  responseUsageOf,
  setResponseUsage,
  type ResponseUsage,
} from "../../src/feed/response-usage.js";
import { heatColor } from "../../src/heat.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { FEED_ROW_ATTRIBUTE } from "../../src/feed/row-id.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

const uninstalls: (() => void)[] = [];
afterEach(() => {
  while (uninstalls.length > 0) uninstalls.pop()?.();
  document.body.replaceChildren();
});

/** A read usage with figure TEXT. */
function usage(text: string): ResponseUsage {
  return { text, color: "red", settledAtMs: 1_000, startedAtMs: 500 };
}

/** A bubble of ROLE in a feed row ROW, inside HOST. */
function bubble(host: HTMLElement, role = "response", row = "row-1"): HTMLElement {
  const item = document.createElement("div");
  item.setAttribute(FEED_ROW_ATTRIBUTE, row);
  const el = document.createElement("div");
  el.className = "bubble";
  el.setAttribute("data-role", role);
  el.innerHTML = "<p>prose</p>";
  item.append(el);
  host.append(item);
  return el;
}

/** A feed host in the document with the hover installed, and what SHOW was called with. */
function hovering(): { host: HTMLElement; shown: (ResponseUsage | null)[] } {
  const host = document.createElement("div");
  document.body.append(host);
  const shown: (ResponseUsage | null)[] = [];
  uninstalls.push(installResponseUsageHover(host, (u) => shown.push(u)));
  return { host, shown };
}

/** Move the pointer onto EL. */
function over(el: Element): void {
  el.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
}

describe("readResponseUsage", () => {
  it("reads the figure, its heat's color, the settle instant and the span's start", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, {
      text: "2.1k",
      atMs: 5_000n,
      startedAtMs: 2_000n,
      heat: { position: 0.25 },
    });
    // Act / Assert
    expect(readResponseUsage(stamp, "p")).toEqual({
      text: "2.1k",
      color: heatColor(0.25, "p"),
      settledAtMs: 5_000,
      startedAtMs: 2_000,
    });
  });

  it("reads an arriving stamp as not settled", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, { text: "2.1k", heat: { position: 0.25 } });
    // Act / Assert
    expect(readResponseUsage(stamp, "p").settledAtMs).toBeNull();
  });

  it("reads a stamp with no span start as having none", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, { text: "2.1k", atMs: 5_000n, heat: { position: 0.25 } });
    // Act / Assert
    expect(readResponseUsage(stamp, "p").startedAtMs).toBeNull();
  });

  it("reads a span that settled where it began", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, {
      text: "2.1k",
      atMs: 5_000n,
      startedAtMs: 5_000n,
      heat: { position: 0.25 },
    });
    // Act / Assert
    expect(readResponseUsage(stamp, "p").startedAtMs).toBe(5_000);
  });

  it("refuses a span that settled before it began, naming its field", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, {
      text: "2.1k",
      atMs: 5_000n,
      startedAtMs: 6_000n,
      heat: { position: 0.25 },
    });
    // Act / Assert
    expect(() => readResponseUsage(stamp, "FeedResponse.usage")).toThrow(
      new MalformedView("FeedResponse.usage.started_at_ms", "the span began at 6000, after the response settled at 5000"),
    );
  });

  it("refuses a span start beyond a safe integer", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, {
      text: "2.1k",
      startedAtMs: BigInt(Number.MAX_SAFE_INTEGER) + 1n,
      heat: { position: 0.25 },
    });
    // Act / Assert
    expect(() => readResponseUsage(stamp, "p")).toThrow(MalformedView);
  });

  it("refuses a stamp with no heat, naming its field", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, { text: "2.1k" });
    // Act / Assert
    expect(() => readResponseUsage(stamp, "FeedResponse.usage")).toThrow("FeedResponse.usage.heat");
  });

  it("refuses a heat outside [0, 1]", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, { text: "2.1k", heat: { position: 2 } });
    // Act / Assert
    expect(() => readResponseUsage(stamp, "p")).toThrow(MalformedView);
  });

  it("refuses a settle instant beyond a safe integer", () => {
    // Arrange
    const stamp = create(FeedResponseUsageStampSchema, {
      text: "2.1k",
      atMs: BigInt(Number.MAX_SAFE_INTEGER) + 1n,
      heat: { position: 0.25 },
    });
    // Act / Assert
    expect(() => readResponseUsage(stamp, "p")).toThrow(MalformedView);
  });
});

describe("setResponseUsage", () => {
  it("records a bubble's usage", () => {
    // Arrange
    const el = document.createElement("div");
    const u = usage("1k");
    // Act
    setResponseUsage(el, u);
    // Assert
    expect(responseUsageOf(el)).toBe(u);
  });

  it("forgets it when handed none", () => {
    // Arrange
    const el = document.createElement("div");
    setResponseUsage(el, usage("1k"));
    // Act
    setResponseUsage(el, undefined);
    // Assert
    expect(responseUsageOf(el)).toBeNull();
  });
});

describe("installResponseUsageHover", () => {
  it("shows the usage of the response bubble under the pointer", () => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    const u = usage("1k");
    setResponseUsage(el, u);
    // Act
    over(el.querySelector("p")!);
    // Assert
    expect(shown).toEqual([u]);
  });

  it("shows nothing again while the pointer moves inside the same bubble", () => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    setResponseUsage(el, usage("1k"));
    over(el);
    // Act
    over(el.querySelector("p")!);
    // Assert
    expect(shown).toHaveLength(1);
  });

  it("shows the next bubble's usage when the pointer moves to it", () => {
    // Arrange
    const { host, shown } = hovering();
    const first = bubble(host, "response", "row-1");
    const second = bubble(host, "response", "row-2");
    setResponseUsage(first, usage("1k"));
    setResponseUsage(second, usage("2k"));
    over(first);
    // Act
    over(second);
    // Assert
    expect(shown.map((u) => u?.text)).toEqual(["1k", "2k"]);
  });

  it.each([
    ["a prompt bubble", "prompt"],
    ["no bubble at all", null],
  ])("gives the cell back when the pointer moves onto %s", (_name, role) => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    setResponseUsage(el, usage("1k"));
    over(el);
    const elsewhere = role === null ? document.createElement("span") : bubble(host, role, "row-2");
    if (role === null) host.append(elsewhere);
    // Act
    over(elsewhere);
    // Assert
    expect(shown.map((u) => u?.text ?? null)).toEqual(["1k", null]);
  });

  it("shows nothing for a response bubble with no usage", () => {
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
    setResponseUsage(el, usage("1k"));
    over(el);
    // Act
    host.dispatchEvent(new MouseEvent("mouseleave"));
    // Assert
    expect(shown.map((u) => u?.text ?? null)).toEqual(["1k", null]);
  });

  it("restates the hovered bubble's usage when its draw records a new one", () => {
    // Arrange
    const { host, shown } = hovering();
    const el = bubble(host);
    setResponseUsage(el, usage("1k"));
    over(el);
    // Act
    setResponseUsage(el, usage("2k"));
    // Assert
    expect(shown.map((u) => u?.text)).toEqual(["1k", "2k"]);
  });

  it("ignores a new usage recorded for a bubble the pointer is not on", () => {
    // Arrange
    const { host, shown } = hovering();
    const hovered = bubble(host, "response", "row-1");
    const other = bubble(host, "response", "row-2");
    setResponseUsage(hovered, usage("1k"));
    over(hovered);
    // Act
    setResponseUsage(other, usage("9k"));
    // Assert
    expect(shown.map((u) => u?.text)).toEqual(["1k"]);
  });

  it("ignores a response bubble outside its host", () => {
    // Arrange
    const { shown } = hovering();
    const outside = document.createElement("div");
    document.body.append(outside);
    const el = bubble(outside);
    setResponseUsage(el, usage("1k"));
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
    const uninstall = installResponseUsageHover(host, show);
    const el = bubble(host);
    setResponseUsage(el, usage("1k"));
    over(el);
    // Act
    uninstall();
    over(el);
    setResponseUsage(el, usage("2k"));
    // Assert
    expect(show.mock.calls.map(([u]) => (u as ResponseUsage | null)?.text ?? null)).toEqual(["1k", null]);
  });

  it("records what it shows, naming the row", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { host } = hovering();
    const el = bubble(host, "response", "row-7");
    setResponseUsage(el, usage("1k"));
    // Act
    over(el);
    // Assert
    const record = await forwardedRecord(capture, "feed.response-usage-hover");
    expect([record.context?.row, record.context?.tokens]).toEqual(["row-7", "1k"]);
  });
});
