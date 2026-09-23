// @vitest-environment jsdom
/**
 * THE TITLE FOLD (owner ruling, 2026-09-23): every tool card's title is capped
 * at two lines while the fold that owns it is collapsed, and wears the response
 * bubble's fade + chevron (`has-more`) only when it actually overflows them.
 *
 * This suite pins the marking, the measurement against each owner's fold, the
 * two error paths, and — reading the sources — that every title site comes
 * through `foldTitle` rather than rolling its own cap.
 */
import { describe, expect, it } from "vitest";
import {
  CARD_FOLD_SELECTOR,
  TITLE_FOLD_CLASS,
  TITLE_FOLD_STANDALONE_CLASS,
  foldTitle,
  refreshTitleFolds,
} from "../../src/feed/title-fold.js";
import { HAS_MORE_CLASS } from "../../src/feed/bubble-more.js";
import { CAPPED_CLASSES, EXPANDED_CLASS } from "../../src/expand.js";
import { stopTicking } from "../../src/feed/ticking.js";
import { fireResize } from "../resize-observer.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** Give EL a measured box: a two-line client height, and its content's. */
function measure(el: HTMLElement, overflow: boolean): void {
  Object.defineProperty(el, "clientHeight", { configurable: true, value: 40 });
  Object.defineProperty(el, "scrollHeight", { configurable: true, value: overflow ? 120 : 40 });
}

/** A connected `.tool-card.tool-fold` holding one card-owned title. */
function toolFoldTitle(overflow: boolean): { card: HTMLElement; title: HTMLElement } {
  const card = document.createElement("div");
  card.className = "tool-card tool-fold";
  const title = document.createElement("pre");
  title.className = "cmd bash-input";
  card.append(title);
  document.body.append(card);
  foldTitle(title, "card");
  measure(title, overflow);
  return { card, title };
}

/** A connected `.bubble-fold` whose head holds one card-owned title. */
function bubbleFoldTitle(): { bubble: HTMLElement; title: HTMLElement; panel: HTMLElement } {
  const bubble = document.createElement("div");
  bubble.className = "tool-card bubble-fold";
  bubble.setAttribute("data-expanded", "false");
  const head = document.createElement("div");
  head.className = "tool-head bubble-head";
  const title = document.createElement("span");
  title.className = "shell-command";
  head.append(title);
  const panel = document.createElement("div");
  panel.className = "agent-panel bubble-subfeed";
  bubble.append(head, panel);
  document.body.append(bubble);
  foldTitle(title, "card");
  measure(title, true);
  return { bubble, title, panel };
}

describe("foldTitle: the marking", () => {
  it("marks a card-owned title with the one title-fold class", () => {
    // Arrange
    const title = document.createElement("span");

    // Act
    foldTitle(title, "card");

    // Assert
    expect([...title.classList]).toEqual([TITLE_FOLD_CLASS]);
  });

  it("marks a standalone title as its own fold too", () => {
    // Arrange
    const title = document.createElement("span");

    // Act
    foldTitle(title, "standalone");

    // Assert
    expect([...title.classList]).toEqual([TITLE_FOLD_CLASS, TITLE_FOLD_STANDALONE_CLASS]);
  });

  it("hands back the element it was given", () => {
    // Arrange
    const title = document.createElement("span");

    // Act / Assert
    expect(foldTitle(title, "card")).toBe(title);
  });

  it("makes a standalone title a click-to-expand section of expand.ts", () => {
    // Arrange / Act / Assert — the literal is held to the toggle's own list.
    expect(CAPPED_CLASSES as readonly string[]).toContain(TITLE_FOLD_STANDALONE_CLASS);
  });

  it("never makes a card-owned title a section of its own", () => {
    // Arrange / Act / Assert — a click on it must open the whole card.
    expect(CAPPED_CLASSES as readonly string[]).not.toContain(TITLE_FOLD_CLASS);
  });
});

describe("the measurement: has-more follows overflow and the owner's fold", () => {
  it("shows on a collapsed title that overflows its two lines", () => {
    // Arrange
    const { title } = toolFoldTitle(true);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("stays off a collapsed title that fits its two lines", () => {
    // Arrange
    const { title } = toolFoldTitle(false);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("comes off once the owning tool-fold card is expanded", () => {
    // Arrange
    const { card, title } = toolFoldTitle(true);
    fireResize(title);
    card.classList.add(EXPANDED_CLASS);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("comes off once the owning bubble is expanded", () => {
    // Arrange
    const { bubble, title } = bubbleFoldTitle();
    fireResize(title);
    bubble.setAttribute("data-expanded", "true");

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("shows on a collapsed bubble head's overflowing title", () => {
    // Arrange
    const { title } = bubbleFoldTitle();

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("keeps a nested card's title folded inside an OPEN bubble", () => {
    // Arrange — the bubble is open, but the card in its sub-feed is not.
    const { bubble, panel } = bubbleFoldTitle();
    bubble.setAttribute("data-expanded", "true");
    const { card, title } = toolFoldTitle(true);
    panel.append(card);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("comes off a standalone title once the title itself is expanded", () => {
    // Arrange
    const title = document.createElement("span");
    document.body.append(title);
    foldTitle(title, "standalone");
    measure(title, true);
    fireResize(title);
    title.classList.add(EXPANDED_CLASS);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("tears its observer down when the card is discarded", () => {
    // Arrange
    const { card, title } = toolFoldTitle(true);

    // Act
    stopTicking(card);

    // Assert — nothing watches it any more, so firing throws.
    expect(() => fireResize(title)).toThrow();
  });
});

describe("refreshTitleFolds: a toggle re-measures the titles it owns", () => {
  it("re-measures a title under the toggled root", () => {
    // Arrange
    const { card, title } = toolFoldTitle(true);
    card.classList.add(EXPANDED_CLASS);
    title.classList.add(HAS_MORE_CLASS);

    // Act
    refreshTitleFolds(card);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("re-measures the root itself when it is a title", () => {
    // Arrange
    const title = document.createElement("span");
    document.body.append(title);
    foldTitle(title, "standalone");
    measure(title, true);

    // Act
    refreshTitleFolds(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("leaves a root with no title in it alone", () => {
    // Arrange
    const root = document.createElement("div");
    root.className = "tool-card tool-fold";

    // Act
    refreshTitleFolds(root);

    // Assert
    expect(root.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });
});

describe("the error paths, logged through the canonical logger", () => {
  it("logs an error when the page has no ResizeObserver to measure with", async () => {
    // Arrange — a document with no window behind it has no observer.
    const capture = captureLogRecords();
    const orphanDoc = document.implementation.createHTMLDocument("no-view");
    const title = orphanDoc.createElement("span");

    // Act
    foldTitle(title, "card");

    // Assert
    const record = await forwardedRecord(capture, "feed.title-fold.unmeasured");
    expect(record.level.case).toBe("error");
  });

  it("still marks a title it cannot measure, so the cap is never lost", () => {
    // Arrange
    captureLogRecords();
    const title = document.implementation.createHTMLDocument("no-view").createElement("span");

    // Act
    foldTitle(title, "card");

    // Assert
    expect(title.classList.contains(TITLE_FOLD_CLASS)).toBe(true);
  });

  it("logs an error for a card-owned title that no card fold holds", async () => {
    // Arrange — connected, but in no `.tool-fold` or `.bubble-fold`.
    const capture = captureLogRecords();
    const title = document.createElement("span");
    document.body.append(title);
    foldTitle(title, "card");
    measure(title, true);

    // Act
    fireResize(title);

    // Assert
    const record = await forwardedRecord(capture, "feed.title-fold.orphan");
    expect(record.level.case).toBe("error");
  });

  it("logs an orphaned title once, however often it resizes", () => {
    // Arrange
    const capture = captureLogRecords();
    const title = document.createElement("span");
    document.body.append(title);
    foldTitle(title, "card");
    measure(title, false);

    // Act
    fireResize(title);
    fireResize(title);
    capture.logger.flush();

    // Assert
    expect(capture.sent.filter((r) => r.operation === "feed.title-fold.orphan")).toHaveLength(1);
  });

  it("does not report a standalone title, which is its own fold", () => {
    // Arrange
    const capture = captureLogRecords();
    const title = document.createElement("span");
    document.body.append(title);
    foldTitle(title, "standalone");
    measure(title, true);

    // Act
    fireResize(title);
    capture.logger.flush();

    // Assert
    expect(capture.sent.some((r) => r.operation === "feed.title-fold.orphan")).toBe(false);
  });

  it("does not report a card-owned title that sits in a card fold", () => {
    // Arrange
    const capture = captureLogRecords();
    const { title } = toolFoldTitle(true);

    // Act
    fireResize(title);
    capture.logger.flush();

    // Assert
    expect(capture.sent.some((r) => r.operation === "feed.title-fold.orphan")).toBe(false);
  });

  it("names both card folds a card-owned title may defer to", () => {
    // Arrange / Act / Assert
    expect(CARD_FOLD_SELECTOR).toBe(".tool-fold, .bubble-fold");
  });
});
