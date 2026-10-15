// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi, type MockInstance } from "vitest";
import {
  FOLDING_BADGE_CLASS,
  FOLDING_BADGE_LABEL,
  FOLD_FLASH_ANIMATION,
  FOLD_FLASH_CLASS,
  FOLD_LEAVE_MS,
  flashFoldedInto,
  showFolding,
} from "../../src/held/fold-motion.js";
import { HELD_BADGE_CLASS } from "../../src/held/held-prompt.js";
import { resetLoggingForTests } from "../../src/log.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

afterEach(() => {
  vi.restoreAllMocks();
  resetLoggingForTests();
});

/** A held card as the section draws one: a status badge in its strip. */
function heldCard(): HTMLElement {
  const card = document.createElement("article");
  card.innerHTML = `<div class="queued-head"><span class="badge ${HELD_BADGE_CLASS} run" data-held-status="classifying">classifying</span></div>`;
  return card;
}

/** An animationend of NAME, dispatched on TARGET and bubbling. */
function animationEnd(target: Element, name: string): void {
  const event = new Event("animationend", { bubbles: true }) as AnimationEvent;
  Object.defineProperty(event, "animationName", { value: name });
  target.dispatchEvent(event);
}

/** An `animate` whose fade finishes when the test says. */
function deferredFade(el: HTMLElement): {
  animate: MockInstance<Element["animate"]>;
  finish: () => Promise<void>;
  cancel: () => Promise<void>;
} {
  let resolve: () => void = () => undefined;
  let reject: (cause: unknown) => void = () => undefined;
  const finished = new Promise<void>((res, rej) => {
    resolve = res;
    reject = rej;
  });
  const animate = vi.spyOn(el, "animate").mockReturnValue({ finished } as unknown as Animation);
  return {
    animate,
    finish: async () => {
      resolve();
      await finished.catch(() => undefined);
      await Promise.resolve();
    },
    cancel: async () => {
      reject(new DOMException("cancelled", "AbortError"));
      await finished.catch(() => undefined);
      await Promise.resolve();
    },
  };
}

describe("flashFoldedInto", () => {
  it("starts the flash on the card", () => {
    // Arrange
    const card = heldCard();
    // Act
    flashFoldedInto(card, "prompt:t1");
    // Assert
    expect(card.classList.contains(FOLD_FLASH_CLASS)).toBe(true);
  });

  it("takes the flash off once its animation ends", () => {
    // Arrange
    const card = heldCard();
    flashFoldedInto(card, "prompt:t1");
    // Act
    animationEnd(card, FOLD_FLASH_ANIMATION);
    // Assert
    expect(card.classList.contains(FOLD_FLASH_CLASS)).toBe(false);
  });

  it("keeps the flash through another animation's end on the card", () => {
    // Arrange
    const card = heldCard();
    flashFoldedInto(card, "prompt:t1");
    // Act
    animationEnd(card, "hold-breathe");
    // Assert
    expect(card.classList.contains(FOLD_FLASH_CLASS)).toBe(true);
  });

  it("keeps the flash through an animation's end bubbling from inside the card", () => {
    // Arrange
    const card = heldCard();
    flashFoldedInto(card, "prompt:t1");
    // Act
    animationEnd(card.querySelector(`.${HELD_BADGE_CLASS}`) as Element, FOLD_FLASH_ANIMATION);
    // Assert
    expect(card.classList.contains(FOLD_FLASH_CLASS)).toBe(true);
  });

  it("flashes again for a second fold while the first flash runs", () => {
    // Arrange
    const card = heldCard();
    flashFoldedInto(card, "prompt:t1");
    // Act
    flashFoldedInto(card, "prompt:t1");
    animationEnd(card, FOLD_FLASH_ANIMATION);
    // Assert: one end takes the one flash off; no stale listener lingers.
    expect(card.classList.contains(FOLD_FLASH_CLASS)).toBe(false);
  });

  it("logs the flash with the item folded into", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    flashFoldedInto(heldCard(), "prompt:t1");
    // Assert
    expect((await forwardedRecord(capture, "held.fold.flash")).context).toMatchObject({ item: "prompt:t1" });
  });
});

describe("showFolding", () => {
  it("replaces the status badge with the folding badge", () => {
    // Arrange
    const card = heldCard();
    deferredFade(card);
    // Act
    showFolding(card, "prompt:t2", () => undefined);
    // Assert
    const badges = [...card.querySelectorAll(`.${HELD_BADGE_CLASS}`)];
    expect(badges.map((b) => [b.textContent, b.classList.contains(FOLDING_BADGE_CLASS)])).toEqual([
      [FOLDING_BADGE_LABEL, true],
    ]);
  });

  it("makes the card take no input", () => {
    // Arrange
    const card = heldCard();
    deferredFade(card);
    // Act
    showFolding(card, "prompt:t2", () => undefined);
    // Assert
    expect(card.hasAttribute("inert")).toBe(true);
  });

  it("fades the card over the leave duration, held at its end", () => {
    // Arrange
    const card = heldCard();
    const { animate } = deferredFade(card);
    // Act
    showFolding(card, "prompt:t2", () => undefined);
    // Assert
    expect(animate.mock.calls[0]?.[1]).toEqual({ duration: FOLD_LEAVE_MS, fill: "forwards" });
  });

  it("does not call gone before the fade has run", () => {
    // Arrange
    const card = heldCard();
    deferredFade(card);
    const gone = vi.fn();
    // Act
    showFolding(card, "prompt:t2", gone);
    // Assert
    expect(gone).not.toHaveBeenCalled();
  });

  it("calls gone once the fade has run", async () => {
    // Arrange
    const card = heldCard();
    const fade = deferredFade(card);
    const gone = vi.fn();
    showFolding(card, "prompt:t2", gone);
    // Act
    await fade.finish();
    // Assert
    expect(gone).toHaveBeenCalledTimes(1);
  });

  it("calls gone when the fade is cancelled", async () => {
    // Arrange
    const card = heldCard();
    const fade = deferredFade(card);
    const gone = vi.fn();
    showFolding(card, "prompt:t2", gone);
    // Act
    await fade.cancel();
    // Assert
    expect(gone).toHaveBeenCalledTimes(1);
  });

  it("logs a cancelled fade with its cause", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const card = heldCard();
    const fade = deferredFade(card);
    showFolding(card, "prompt:t2", () => undefined);
    // Act
    await fade.cancel();
    // Assert
    const record = await forwardedRecord(capture, "held.fold.left");
    expect(record.context).toMatchObject({ item: "prompt:t2", cause: "AbortError: cancelled" });
  });

  it("refuses a card with no status badge, logging it", async () => {
    // Arrange
    const capture = captureLogRecords();
    const card = document.createElement("article");
    // Act
    const act = (): void => showFolding(card, "prompt:t2", () => undefined);
    // Assert
    expect(act).toThrow(/no \.held-badge/);
    expect((await forwardedRecord(capture, "held.fold.no-badge")).context).toMatchObject({ item: "prompt:t2" });
  });

  it("leaves a card with no status badge as it was", () => {
    // Arrange
    const card = document.createElement("article");
    const animate = vi.spyOn(card, "animate");
    // Act
    expect(() => showFolding(card, "prompt:t2", () => undefined)).toThrow();
    // Assert
    expect([card.hasAttribute("inert"), animate.mock.calls.length]).toEqual([false, 0]);
  });
});
