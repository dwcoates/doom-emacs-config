// @vitest-environment jsdom
/**
 * FIX2 — the "more below" affordance (owner ruling, 2026-09-15).
 *
 * A collapsed response or prompt bubble whose content overruns its cap wears
 * `has-more`, which the stylesheet turns into a bottom fade + chevron. This
 * suite pins the measurement and the toggle: WHEN the class goes on (collapsed,
 * overflowing, a response/prompt bubble) and WHEN it comes off (expanded, no
 * overflow, or a box that is not one of the two speaker bubbles), plus the
 * ResizeObserver wiring that keeps it in step on draw and re-wrap.
 */
import { describe, expect, it } from "vitest";
import {
  HAS_MORE_CLASS,
  MORE_BUBBLE_SELECTOR,
  overflowsCap,
  refreshHasMore,
  shouldShowMore,
  type MoreBox,
} from "../../src/feed/bubble-more.js";
import { BUBBLE_SCROLL_CLASS, bubbleScroll } from "../../src/feed/bubble-scroll.js";
import { EXPANDED_CLASS } from "../../src/expand.js";
import { stopTicking } from "../../src/feed/ticking.js";
import { fireResize } from "../resize-observer.js";

/** A fake scroll box: the four facts refreshHasMore reads, all controllable. */
function fakeBox(opts: {
  matches: boolean;
  expanded?: boolean;
  scrollHeight: number;
  clientHeight: number;
}): MoreBox & { has(name: string): boolean } {
  const classes = new Set<string>();
  if (opts.expanded === true) classes.add(EXPANDED_CLASS);
  return {
    classList: {
      add: (name) => classes.add(name),
      remove: (name) => classes.delete(name),
      contains: (name) => classes.has(name),
    },
    matches: (selector) => selector === MORE_BUBBLE_SELECTOR && opts.matches,
    scrollHeight: opts.scrollHeight,
    clientHeight: opts.clientHeight,
    has: (name) => classes.has(name),
  };
}

describe("MORE_BUBBLE_SELECTOR: held to the scroll-box class", () => {
  it("targets the shared bubble-scroll class (the literal must not drift)", () => {
    // Arrange / Act / Assert — the inlined literal equals the exported constant.
    expect(MORE_BUBBLE_SELECTOR).toContain(`.${BUBBLE_SCROLL_CLASS}`);
  });

  it("scopes to the assistant and user bubbles only", () => {
    // Arrange / Act / Assert
    expect(MORE_BUBBLE_SELECTOR).toBe(
      `.bubble.assistant > .${BUBBLE_SCROLL_CLASS}, .bubble.user > .${BUBBLE_SCROLL_CLASS}`,
    );
  });
});

describe("overflowsCap: content taller than the box", () => {
  it("is true when the content overruns the cap", () => {
    // Arrange / Act / Assert
    expect(overflowsCap({ scrollHeight: 900, clientHeight: 540 })).toBe(true);
  });

  it("is false when the content exactly fills the cap", () => {
    // Arrange / Act / Assert
    expect(overflowsCap({ scrollHeight: 540, clientHeight: 540 })).toBe(false);
  });
});

describe("shouldShowMore: the four conditions", () => {
  it("shows on a collapsed, overflowing response/prompt bubble", () => {
    // Arrange
    const box = fakeBox({ matches: true, scrollHeight: 900, clientHeight: 540 });

    // Act / Assert
    expect(shouldShowMore(box)).toBe(true);
  });

  it("never shows once expanded, even while it overflows", () => {
    // Arrange — expanded reveals scroll, so there is no hidden 'more' to point at.
    const box = fakeBox({ matches: true, expanded: true, scrollHeight: 900, clientHeight: 540 });

    // Act / Assert
    expect(shouldShowMore(box)).toBe(false);
  });

  it("never shows when the content fits the cap", () => {
    // Arrange
    const box = fakeBox({ matches: true, scrollHeight: 540, clientHeight: 540 });

    // Act / Assert
    expect(shouldShowMore(box)).toBe(false);
  });

  it("never shows on a box that is not a response/prompt bubble", () => {
    // Arrange — a tool-call section overflows but does not match the selector.
    const box = fakeBox({ matches: false, scrollHeight: 900, clientHeight: 540 });

    // Act / Assert
    expect(shouldShowMore(box)).toBe(false);
  });
});

describe("refreshHasMore: the class follows the measurement", () => {
  it("adds has-more when the box should show it", () => {
    // Arrange
    const box = fakeBox({ matches: true, scrollHeight: 900, clientHeight: 540 });

    // Act
    refreshHasMore(box);

    // Assert
    expect(box.has(HAS_MORE_CLASS)).toBe(true);
  });

  it("drops has-more when the box should not show it", () => {
    // Arrange — an expanded box that already wears the class.
    const box = fakeBox({ matches: true, expanded: true, scrollHeight: 900, clientHeight: 540 });
    box.classList.add(HAS_MORE_CLASS);

    // Act
    refreshHasMore(box);

    // Assert
    expect(box.has(HAS_MORE_CLASS)).toBe(false);
  });
});

/** A real `.bubble.<kind>` with a `.bubble-scroll` (observer armed) inside. */
function bubble(kind: "assistant" | "user", overflow: boolean): HTMLElement {
  const el = document.createElement("div");
  el.className = `bubble ${kind}`;
  const body = document.createElement("div");
  body.className = "bubble-body";
  const scroll = bubbleScroll(body);
  el.append(scroll);
  Object.defineProperty(scroll, "clientHeight", { configurable: true, value: 540 });
  Object.defineProperty(scroll, "scrollHeight", { configurable: true, value: overflow ? 900 : 540 });
  return el;
}

describe("installHasMore: the box tracks its own overflow", () => {
  it("adds has-more to a collapsed, overflowing assistant bubble on resize", () => {
    // Arrange — the observer is armed by bubbleScroll.
    const el = bubble("assistant", true);
    const scroll = el.firstElementChild as HTMLElement;

    // Act
    fireResize(scroll);

    // Assert
    expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("keeps has-more off a bubble whose content fits, on resize", () => {
    // Arrange
    const el = bubble("user", false);
    const scroll = el.firstElementChild as HTMLElement;

    // Act
    fireResize(scroll);

    // Assert
    expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("removes has-more once the box is expanded, on resize", () => {
    // Arrange — an overflowing box already showing the signal, now expanded.
    const el = bubble("assistant", true);
    const scroll = el.firstElementChild as HTMLElement;
    fireResize(scroll);
    scroll.classList.add(EXPANDED_CLASS);

    // Act
    fireResize(scroll);

    // Assert
    expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("tears the observer down when the bubble is discarded", () => {
    // Arrange — a discarded box must not keep answering resizes.
    const el = bubble("assistant", true);
    const scroll = el.firstElementChild as HTMLElement;

    // Act
    stopTicking(el);

    // Assert — nothing watches it any more, so firing throws.
    expect(() => fireResize(scroll)).toThrow();
  });
});
