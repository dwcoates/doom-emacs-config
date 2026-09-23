// @vitest-environment jsdom
//
// jsdom lays nothing out, but it keeps an element's `scrollTop` as a plain
// number, which is exactly the fact keep-scroll reads: whether the reader has
// scrolled a box. The invariants are therefore asserted on the DOM itself --
// which elements are still in the document, and what their position reads.
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { keepScrolled } from "../../src/feed/keep-scroll.js";
import { HAS_MORE_CLASS } from "../../src/feed/bubble-more.js";
import { TICKING_ATTRIBUTE, onDiscard, tick } from "../../src/feed/ticking.js";
import { createTicker } from "../../src/clock.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

/**
 * A tool card the way the cards draw one: a root, a head line, and an output
 * box holding TEXT. Answers the root and the box.
 */
function card(text: string, state = "running"): { root: HTMLElement; box: HTMLElement; head: HTMLElement } {
  const root = document.createElement("div");
  root.className = "tool-card tool-fold expanded";
  root.setAttribute("data-state", state);
  const head = document.createElement("div");
  head.className = "tool-head";
  head.textContent = `head ${state}`;
  const box = document.createElement("pre");
  box.className = "tool-output";
  box.textContent = text;
  root.append(head, box);
  return { root, box, head };
}

/** A live card in the document whose output box the reader scrolled by TOP. */
function scrolled(top: number): ReturnType<typeof card> {
  const live = card("line 1\nline 2");
  document.body.append(live.root);
  live.box.scrollTop = top;
  return live;
}

describe("keepScrolled", () => {
  it("declines a card the reader has scrolled nothing in", () => {
    // Arrange
    const live = card("old");
    document.body.append(live.root);
    // Act + Assert -- the ordinary case: the caller replaces the card.
    expect(keepScrolled(live.root, card("new").root)).toBe(false);
  });

  it("keeps the scrolled box itself in the document", () => {
    // Arrange
    const live = scrolled(120);
    // Act
    keepScrolled(live.root, card("line 1\nline 2\nline 3").root);
    // Assert
    expect(live.root.querySelector(".tool-output")).toBe(live.box);
  });

  it("leaves the scrolled box's position where the reader put it", () => {
    // Arrange
    const live = scrolled(120);
    // Act
    keepScrolled(live.root, card("line 1\nline 2\nline 3").root);
    // Assert
    expect(live.box.scrollTop).toBe(120);
  });

  it("removes neither the box nor its ancestors from the document", () => {
    // Arrange
    const live = scrolled(120);
    const observer = new MutationObserver(() => undefined);
    observer.observe(document.body, { childList: true, subtree: true });
    // Act
    keepScrolled(live.root, card("line 1\nline 2\nline 3").root);
    // Assert
    const removed = observer.takeRecords().flatMap((record) => [...record.removedNodes]);
    expect(removed.filter((node) => node === live.box || node === live.root)).toEqual([]);
  });

  it("draws the fresh content inside the kept box", () => {
    // Arrange
    const live = scrolled(120);
    // Act
    keepScrolled(live.root, card("line 1\nline 2\nline 3").root);
    // Assert
    expect(live.box.textContent).toBe("line 1\nline 2\nline 3");
  });

  it("takes the fresh draw's elements off the scrolled path", () => {
    // Arrange
    const live = scrolled(120);
    // Act
    keepScrolled(live.root, card("x", "returned").root);
    // Assert
    expect(live.root.querySelector(".tool-head")?.textContent).toBe("head returned");
  });

  it("brings the kept elements' attributes to the fresh draw's", () => {
    // Arrange
    const live = scrolled(120);
    // Act
    keepScrolled(live.root, card("x", "returned").root);
    // Assert
    expect(live.root.getAttribute("data-state")).toBe("returned");
  });

  it("drops an attribute the fresh draw no longer carries", () => {
    // Arrange
    const live = scrolled(120);
    live.root.setAttribute("data-stale", "1");
    // Act
    keepScrolled(live.root, card("x").root);
    // Assert
    expect(live.root.hasAttribute("data-stale")).toBe(false);
  });

  it("keeps the measured has-more class on a kept box", () => {
    // Arrange
    const live = scrolled(120);
    live.box.classList.add(HAS_MORE_CLASS);
    // Act
    keepScrolled(live.root, card("x").root);
    // Assert
    expect(live.box.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("keeps a kept element's own clock subscription marker", () => {
    // Arrange
    const live = scrolled(120);
    tick(live.root, createTicker(1000), () => undefined);
    // Act
    keepScrolled(live.root, card("x").root);
    // Assert
    expect(live.root.getAttribute(TICKING_ATTRIBUTE)).toBe("1");
  });

  it("stops the clock of a live element it dropped", () => {
    // Arrange -- the live head holds a clock; the fresh head replaces it.
    const live = scrolled(120);
    let ticks = 0;
    tick(live.head, createTicker(1000), () => (ticks += 1));
    // Act
    keepScrolled(live.root, card("x").root);
    vi.advanceTimersByTime(5000);
    // Assert -- only the immediate first paint ever ran.
    expect(ticks).toBe(1);
  });

  it("releases the fresh draw's own registrations on what it did not use", () => {
    // Arrange -- the fresh box registered a discard hook; the live box is kept.
    const live = scrolled(120);
    const fresh = card("x");
    let released = 0;
    onDiscard(fresh.box, () => (released += 1));
    // Act
    keepScrolled(live.root, fresh.root);
    // Assert
    expect(released).toBe(1);
  });

  it("declines, and says so, when the redraw changed the card's root shape", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const live = scrolled(120);
    const other = document.createElement("section");
    // Act
    const kept = keepScrolled(live.root, other);
    // Assert
    const record = await forwardedRecord(capture, "feed.keep-scroll.shape-changed");
    expect([kept, record.context]).toEqual([false, expect.objectContaining({ from: "DIV", to: "SECTION" })]);
  });

  it("records a kept box at DEBUG", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const live = scrolled(120);
    // Act
    keepScrolled(live.root, card("x").root);
    // Assert
    const record = await forwardedRecord(capture, "feed.keep-scroll.kept");
    expect(record.context).toMatchObject({ boxes: 1 });
  });
});
