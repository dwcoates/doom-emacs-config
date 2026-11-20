// @vitest-environment jsdom
/**
 * node-reveal — a response tree's node brought into view inside the bubble
 * boxes around it, and the span those boxes leave visible.
 */
import { afterEach, describe, expect, it } from "vitest";
import { BUBBLE_SCROLL_CLASS } from "../../src/feed/bubble-scroll.js";
import { revealInBubbles } from "../../src/feed/node-reveal.js";
import { boxRect, settableScrollTop } from "../dom-rect.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

afterEach(() => {
  document.body.replaceChildren();
});

/** A bubble scroll box whose view is TOP to TOP + HEIGHT, holding CHILD; its content moves with its scrollTop. */
function bubbleBox(top: number, height: number, child: HTMLElement): HTMLElement {
  const box = document.createElement("div");
  box.className = BUBBLE_SCROLL_CLASS;
  box.getBoundingClientRect = () => boxRect({ top, height });
  settableScrollTop(box);
  box.append(child);
  return box;
}

/** A line laid out at TOP (before any box around it scrolled) with HEIGHT, moved up by every box's scrollTop. */
function line(top: number, height: number): HTMLElement {
  const element = document.createElement("div");
  element.getBoundingClientRect = () => {
    let moved = top;
    for (let box = element.parentElement?.closest<HTMLElement>(`.${BUBBLE_SCROLL_CLASS}`); box; box = box.parentElement?.closest<HTMLElement>(`.${BUBBLE_SCROLL_CLASS}`)) {
      moved -= box.scrollTop;
    }
    return boxRect({ top: moved, height });
  };
  return element;
}

/** LINES held in one body, appended to the document inside WRAP (the body itself when none). */
function drawn(lines: HTMLElement[], wrap: (body: HTMLElement) => HTMLElement = (body) => body): HTMLElement[] {
  const body = document.createElement("div");
  body.append(...lines);
  document.body.append(wrap(body));
  return lines;
}

describe("revealInBubbles", () => {
  it("answers lines in no bubble box as laid out", () => {
    // Arrange
    const lines = drawn([line(280, 20), line(300, 20)]);
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "nearest");
    // Assert
    expect(got).toEqual({ top: 280, bottom: 320 });
  });

  it("is null when no line is laid out", () => {
    // Arrange
    const lines = drawn([line(280, 0)], (body) => bubbleBox(0, 100, body));
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "nearest");
    // Assert
    expect(got).toBeNull();
  });

  it("scrolls a bubble box that clips the lines only as far as they need", () => {
    // Arrange: the box shows 100-200, and the node's lines run 250-290.
    let box: HTMLElement | undefined;
    const lines = drawn([line(250, 20), line(270, 20)], (body) => (box = bubbleBox(100, 100, body)));
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "nearest");
    // Assert
    expect([box?.scrollTop, got]).toEqual([90, { top: 160, bottom: 200 }]);
  });

  it("leaves a bubble box alone when the lines are in its view", () => {
    // Arrange
    let box: HTMLElement | undefined;
    const lines = drawn([line(120, 20)], (body) => (box = bubbleBox(100, 100, body)));
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "nearest");
    // Assert
    expect([box?.scrollTop, got]).toEqual([0, { top: 120, bottom: 140 }]);
  });

  it("answers only the span a box shorter than the lines leaves visible, from their head", () => {
    // Arrange: the box shows 100-200, and the node's lines run 300-450.
    let box: HTMLElement | undefined;
    const lines = drawn([line(300, 150)], (body) => (box = bubbleBox(100, 100, body)));
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "nearest");
    // Assert
    expect([box?.scrollTop, got]).toEqual([200, { top: 100, bottom: 200 }]);
  });

  it("brings the lines into view in every bubble box around them, innermost first", () => {
    // Arrange: the inner box shows 150-200 inside the outer box showing 100-180.
    let inner: HTMLElement | undefined;
    let outer: HTMLElement | undefined;
    const lines = drawn([line(230, 10)], (body) => {
      inner = bubbleBox(150, 50, body);
      outer = bubbleBox(100, 80, inner);
      return outer;
    });
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "nearest");
    // Assert: the inner box moves the line to 190-200, then the outer box to its own bottom, 170-180.
    expect([inner?.scrollTop, outer?.scrollTop, got]).toEqual([40, 20, { top: 170, bottom: 180 }]);
  });

  it("records the bubble box it scrolled", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const lines = drawn([line(250, 20)], (body) => bubbleBox(100, 100, body));
    // Act
    revealInBubbles(lines, "the node 1.1", "nearest");
    // Assert
    const record = await forwardedRecord(capture, "feed.node-reveal.bubble-scrolled");
    expect(record.context).toMatchObject({ what: "the node 1.1", placement: "nearest", from: 0, to: 70 });
  });

  it("centers the lines in a bubble box that clips them, for a jump", () => {
    // Arrange: the box shows 100-200, and the node's lines run 250-290.
    let box: HTMLElement | undefined;
    const lines = drawn([line(250, 20), line(270, 20)], (body) => (box = bubbleBox(100, 100, body)));
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "center");
    // Assert: midpoint 270 onto the box's 150.
    expect([box?.scrollTop, got]).toEqual([120, { top: 130, bottom: 170 }]);
  });

  it("centers lines already in a bubble box's view, for a jump", () => {
    // Arrange: the lines run 170-190 inside a box showing 100-200.
    let box: HTMLElement | undefined;
    const lines = drawn([line(170, 20)], (body) => (box = bubbleBox(100, 100, body)));
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "center");
    // Assert: midpoint 180 onto the box's 150.
    expect([box?.scrollTop, got]).toEqual([30, { top: 140, bottom: 160 }]);
  });

  it("puts the head of lines taller than the box at its top, for a jump", () => {
    // Arrange: the box shows 100-200, and the node's lines run 300-450.
    let box: HTMLElement | undefined;
    const lines = drawn([line(300, 150)], (body) => (box = bubbleBox(100, 100, body)));
    // Act
    const got = revealInBubbles(lines, "the node 1.1", "center");
    // Assert
    expect([box?.scrollTop, got]).toEqual([200, { top: 100, bottom: 200 }]);
  });

  it("records the placement it scrolled the bubble box for", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const lines = drawn([line(250, 20)], (body) => bubbleBox(100, 100, body));
    // Act
    revealInBubbles(lines, "the node 1.1", "center");
    // Assert
    const record = await forwardedRecord(capture, "feed.node-reveal.bubble-scrolled");
    expect(record.context).toMatchObject({ what: "the node 1.1", placement: "center", from: 0, to: 110 });
  });
});
