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
  TITLE_FOLD_CLASS,
  TITLE_FOLD_OPEN_SELECTOR,
  installHasMore,
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

/** A real title-fold element under an owner of the given shape. */
function titleUnder(owner: "tool-fold" | "bubble-fold" | "none", open: boolean): HTMLElement {
  const title = document.createElement("span");
  title.className = TITLE_FOLD_CLASS;
  Object.defineProperty(title, "clientHeight", { configurable: true, value: 40 });
  Object.defineProperty(title, "scrollHeight", { configurable: true, value: 120 });
  if (owner === "none") {
    if (open) title.classList.add(EXPANDED_CLASS);
    return title;
  }
  const card = document.createElement("div");
  if (owner === "tool-fold") {
    card.className = open ? "tool-card tool-fold expanded" : "tool-card tool-fold";
    card.append(title);
    return title;
  }
  card.className = "tool-card bubble-fold";
  card.setAttribute("data-expanded", open ? "true" : "false");
  const head = document.createElement("div");
  head.className = "tool-head bubble-head";
  head.append(title);
  card.append(head);
  return title;
}

describe("shouldShowMore: a card title is the other kind it serves", () => {
  it.each([
    ["a collapsed tool-fold card", "tool-fold", false, true],
    ["an expanded tool-fold card", "tool-fold", true, false],
    ["a collapsed bubble", "bubble-fold", false, true],
    ["an expanded bubble", "bubble-fold", true, false],
    ["a collapsed standalone title", "none", false, true],
    ["an expanded standalone title", "none", true, false],
  ] as const)("an overflowing title under %s shows: %s", (_label, owner, open, shows) => {
    // Arrange
    const title = titleUnder(owner, open);

    // Act / Assert
    expect(shouldShowMore(title)).toBe(shows);
  });

  it("never shows on a title that fits its two lines", () => {
    // Arrange
    const title = titleUnder("tool-fold", false);
    Object.defineProperty(title, "scrollHeight", { configurable: true, value: 40 });

    // Act / Assert
    expect(shouldShowMore(title)).toBe(false);
  });

  it("reads each owner's open state through one selector list", () => {
    // Arrange / Act / Assert — the stylesheet's lift rule lists the same three.
    expect(TITLE_FOLD_OPEN_SELECTOR).toBe(
      `.${TITLE_FOLD_CLASS}.${EXPANDED_CLASS}, .tool-fold.${EXPANDED_CLASS} .${TITLE_FOLD_CLASS}, ` +
        `.bubble-fold[data-expanded="true"] > .bubble-head .${TITLE_FOLD_CLASS}`,
    );
  });
});

describe("installHasMore: a caller's refresh", () => {
  it("runs the refresh the caller hands it on a resize", () => {
    // Arrange
    const box = document.createElement("div");
    const seen: HTMLElement[] = [];
    installHasMore(box, (b) => seen.push(b));

    // Act
    fireResize(box);

    // Assert
    expect(seen).toEqual([box]);
  });
});
