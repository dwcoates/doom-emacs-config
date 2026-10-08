// @vitest-environment jsdom
/**
 * FIX2 — the "more below" affordance (owner ruling, 2026-09-15).
 *
 * A collapsed bubble whose rendered lines run past its cap wears `has-more`,
 * which the stylesheet turns into a bottom fade (never a chevron). This
 * suite pins the measurement and the toggle: WHEN the class goes on (collapsed,
 * overflowing, a response/prompt bubble) and WHEN it comes off (expanded, no
 * overflow, or a box that is not one of the two speaker bubbles), plus the
 * ResizeObserver wiring that keeps it in step on draw and re-wrap.
 */
import { describe, expect, it } from "vitest";
import {
  BUBBLE_MORE_ATTRIBUTE,
  BUBBLE_MORE_ELLIPSIS,
  BUBBLE_MORE_FADE,
  HAS_MORE_CLASS,
  MORE_BUBBLE_SELECTOR,
  TITLE_FOLD_CLASS,
  TITLE_FOLD_OPEN_SELECTOR,
  HAS_MORE_UNMEASURABLE,
  hidesContentBeyondCap,
  installHasMore,
  overflowsCap,
  refreshHasMore,
  shouldShowMore,
} from "../../src/feed/bubble-more.js";
import { BUBBLE_BODY_CLASS } from "../../src/bubble/body.js";
import { quoteSlot } from "../../src/bubble/quote.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { BUBBLE_SCROLL_CLASS, bubbleBox as sharedBubbleBox } from "../../src/feed/bubble-scroll.js";
import { EXPANDED_CLASS } from "../../src/expand.js";
import { stopTicking } from "../../src/feed/ticking.js";
import { fireResize } from "../resize-observer.js";

/** Pixels per rendered line in these fixtures. */
const LINE_PX = 20;

/** A collapsed bubble's line cap in these fixtures (the feed cap is 27.5; any count serves). */
const CAP_LINES = 25;

/**
 * A real `.bubble` holding its `.bubble-scroll` box and `.bubble-body`, the box
 * collapsed at CAP LINES: the body is LINES tall, the box shows at most the cap,
 * and the box's scrollable overflow is the body plus EXTRAPX (a decoration's
 * border box hanging past the last line, as the usage corner's hit area does).
 */
function bubbleBox(opts: {
  lines: number;
  cap?: number;
  expanded?: boolean;
  extraPx?: number;
}): HTMLElement {
  const bubble = document.createElement("div");
  bubble.className = "bubble";
  const scroll = document.createElement("div");
  scroll.className = BUBBLE_SCROLL_CLASS;
  if (opts.expanded === true) scroll.classList.add(EXPANDED_CLASS);
  const body = document.createElement("div");
  body.className = BUBBLE_BODY_CLASS;
  scroll.append(body);
  bubble.append(scroll);
  const bodyPx = opts.lines * LINE_PX;
  const shownPx = Math.min(opts.lines, opts.cap ?? CAP_LINES) * LINE_PX;
  Object.defineProperty(body, "offsetHeight", { configurable: true, value: bodyPx });
  Object.defineProperty(scroll, "clientHeight", { configurable: true, value: shownPx });
  Object.defineProperty(scroll, "scrollHeight", {
    configurable: true,
    value: Math.max(bodyPx, shownPx) + (opts.extraPx ?? 0),
  });
  return scroll;
}

/**
 * A collapsed ONE-line bubble in MORE mode whose body the stylesheet's ellipsis
 * clamp would hold at its one line: its own box (`offsetHeight`) is one line,
 * and its rendered lines are LINES tall (`scrollHeight`), clamped or not.
 */
function clampedBox(more: string, lines: number): HTMLElement {
  const scroll = bubbleBox({ lines, cap: 1 });
  scroll.parentElement?.setAttribute(BUBBLE_MORE_ATTRIBUTE, more);
  const body = scroll.querySelector<HTMLElement>(`.${BUBBLE_BODY_CLASS}`);
  if (body === null) throw new Error("fixture: no body");
  Object.defineProperty(body, "offsetHeight", { configurable: true, value: LINE_PX });
  Object.defineProperty(body, "scrollHeight", { configurable: true, value: lines * LINE_PX });
  return scroll;
}

describe("MORE_BUBBLE_SELECTOR: held to the scroll-box class", () => {
  it("targets the shared bubble-scroll class (the literal must not drift)", () => {
    // Arrange / Act / Assert — the inlined literal equals the exported constant.
    expect(MORE_BUBBLE_SELECTOR).toContain(`.${BUBBLE_SCROLL_CLASS}`);
  });

  it("serves every bubble's scroll box, and only a bubble's", () => {
    // Arrange / Act / Assert
    expect(MORE_BUBBLE_SELECTOR).toBe(`.bubble > .${BUBBLE_SCROLL_CLASS}`);
  });
});

describe("overflowsCap: a title's text taller than its clamp", () => {
  it("is true when the content overruns the cap", () => {
    // Arrange / Act / Assert
    expect(overflowsCap({ scrollHeight: 900, clientHeight: 540 })).toBe(true);
  });

  it("is false when the content exactly fills the cap", () => {
    // Arrange / Act / Assert
    expect(overflowsCap({ scrollHeight: 540, clientHeight: 540 })).toBe(false);
  });
});

describe("hidesContentBeyondCap: the body's lines against the collapsed cap", () => {
  it.each([
    ["a one-line bubble", 1, false],
    ["a bubble one line under its cap", CAP_LINES - 1, false],
    ["a bubble exactly at its cap", CAP_LINES, false],
    ["a bubble one line past its cap", CAP_LINES + 1, true],
  ] as const)("%s hides content: %s", (_label, lines, hides) => {
    // Arrange
    const box = bubbleBox({ lines });

    // Act / Assert
    expect(hidesContentBeyondCap(box)).toBe(hides);
  });

  it("ignores a decoration hanging below the last line (the usage corner's hit area)", () => {
    // Arrange — a one-character answer whose box overflows by the corner's padding.
    const box = bubbleBox({ lines: 1, extraPx: 6 });

    // Act / Assert
    expect(hidesContentBeyondCap(box)).toBe(false);
  });

  it("counts every line below a zero-line cap as hidden", () => {
    // Arrange — a peer message shows its strip and none of its body.
    const box = bubbleBox({ lines: 1, cap: 0 });

    // Act / Assert
    expect(hidesContentBeyondCap(box)).toBe(true);
  });

  it("reads an ellipsis bubble's lines past the clamp that holds its body at one line", () => {
    // Arrange — three rendered lines, the clamped body's own box one line tall.
    const box = clampedBox(BUBBLE_MORE_ELLIPSIS, 3);

    // Act / Assert
    expect(hidesContentBeyondCap(box)).toBe(true);
  });

  it("finds nothing hidden on an ellipsis bubble whose one line is its whole content", () => {
    // Arrange
    const box = clampedBox(BUBBLE_MORE_ELLIPSIS, 1);

    // Act / Assert
    expect(hidesContentBeyondCap(box)).toBe(false);
  });

  it("reads a fade bubble's own body box, never its scrollable overflow", () => {
    // Arrange — an unclamped body is its own lines; an overflow reading would lie.
    const box = clampedBox(BUBBLE_MORE_FADE, 3);

    // Act / Assert
    expect(hidesContentBeyondCap(box)).toBe(false);
  });

  it("counts a reply's quote as hidden, however short the words beside it", () => {
    // Arrange — one line of words, and the quote the collapsed bubble does not draw.
    const box = bubbleBox({ lines: 1 });
    box.querySelector(`.${BUBBLE_BODY_CLASS}`)?.append(quoteSlot("prompt-block", "the quote"));

    // Act / Assert
    expect(hidesContentBeyondCap(box)).toBe(true);
  });

  it("refuses a box that holds no body, and records it at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    const box = bubbleBox({ lines: 1 });
    box.replaceChildren();

    // Act
    const measure = (): boolean => hidesContentBeyondCap(box);

    // Assert
    expect(measure).toThrow(/has-more unmeasurable/);
    const record = await forwardedRecord(capture, HAS_MORE_UNMEASURABLE);
    expect(record.level.case).toBe("error");
  });
});

describe("shouldShowMore: the four conditions", () => {
  it("shows on a collapsed bubble past its cap", () => {
    // Arrange
    const box = bubbleBox({ lines: CAP_LINES + 5 });

    // Act / Assert
    expect(shouldShowMore(box)).toBe(true);
  });

  it("never shows once expanded, even while it overflows", () => {
    // Arrange — expanded reveals scroll, so there is no hidden 'more' to point at.
    const box = bubbleBox({ lines: CAP_LINES + 5, expanded: true });

    // Act / Assert
    expect(shouldShowMore(box)).toBe(false);
  });

  it("never shows when the content fits the cap", () => {
    // Arrange
    const box = bubbleBox({ lines: CAP_LINES });

    // Act / Assert
    expect(shouldShowMore(box)).toBe(false);
  });

  it("never shows on an uncapped bubble's box, however far its body runs", () => {
    // Arrange — the box an uncapped bubble is drawn with, its body far taller.
    const bubble = document.createElement("div");
    bubble.className = "bubble";
    const body = document.createElement("div");
    body.className = BUBBLE_BODY_CLASS;
    const box = sharedBubbleBox(body, false);
    bubble.append(box);
    Object.defineProperty(box, "clientHeight", { configurable: true, value: 540 });
    Object.defineProperty(body, "offsetHeight", { configurable: true, value: 900 });

    // Act / Assert
    expect(shouldShowMore(box)).toBe(false);
  });

  it("never shows on a box that is not a bubble's", () => {
    // Arrange — a tool-call section overflows but does not match the selector.
    const section = document.createElement("div");
    section.className = "tool-output";
    Object.defineProperty(section, "clientHeight", { configurable: true, value: 540 });
    Object.defineProperty(section, "scrollHeight", { configurable: true, value: 900 });

    // Act / Assert
    expect(shouldShowMore(section)).toBe(false);
  });
});

describe("refreshHasMore: the class follows the measurement", () => {
  it("adds has-more when the box should show it", () => {
    // Arrange
    const box = bubbleBox({ lines: CAP_LINES + 5 });

    // Act
    refreshHasMore(box);

    // Assert
    expect(box.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("drops has-more when the box should not show it", () => {
    // Arrange — an expanded box that already wears the class.
    const box = bubbleBox({ lines: CAP_LINES + 5, expanded: true });
    box.classList.add(HAS_MORE_CLASS);

    // Act
    refreshHasMore(box);

    // Assert
    expect(box.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });
});

/** A real `.bubble.<kind>` with a `.bubble-scroll` (observer armed) inside. */
function bubble(kind: "assistant" | "user", overflow: boolean): HTMLElement {
  const el = document.createElement("div");
  el.className = `bubble ${kind}`;
  const body = document.createElement("div");
  body.className = BUBBLE_BODY_CLASS;
  const scroll = sharedBubbleBox(body, true);
  el.append(scroll);
  Object.defineProperty(scroll, "clientHeight", { configurable: true, value: 540 });
  Object.defineProperty(body, "offsetHeight", { configurable: true, value: overflow ? 900 : 540 });
  return el;
}

describe("installHasMore: the box tracks its own overflow", () => {
  it("adds has-more to a collapsed, overflowing assistant bubble on resize", () => {
    // Arrange — the observer is armed by sharedBubbleBox.
    const el = bubble("assistant", true);
    const scroll = el.firstElementChild as HTMLElement;

    // Act
    fireResize(scroll);

    // Assert
    expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("arms no measurer on an uncapped bubble's box", () => {
    // Arrange — an uncapped box has nothing hidden to point at.
    const body = document.createElement("div");
    body.className = BUBBLE_BODY_CLASS;
    const box = sharedBubbleBox(body, false);

    // Act / Assert — the stub throws on a resize nothing observes.
    expect(() => fireResize(box)).toThrow();
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

  it("re-measures when the body grows, even with a corner placed before it", async () => {
    // Arrange — the response's usage corner lands ahead of the body in the box.
    const el = bubble("assistant", false);
    const scroll = el.firstElementChild as HTMLElement;
    const body = scroll.querySelector(`.${BUBBLE_BODY_CLASS}`) as HTMLElement;
    scroll.prepend(document.createElement("span"));
    await Promise.resolve();
    Object.defineProperty(body, "offsetHeight", { configurable: true, value: 900 });

    // Act — the box is at its cap and does not resize; only the body does.
    fireResize(body);

    // Assert
    expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(true);
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

  it("never shows on a title that fits its one line", () => {
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

describe("installHasMore: the body is followed, not captured", () => {
  /** Let the MutationObserver watching the box's children deliver. */
  async function flushMutations(): Promise<void> {
    await Promise.resolve();
  }

  it("measures a body a redraw handed the kept box", async () => {
    // Arrange -- keep-scroll.ts keeps a scrolled box and swaps its body.
    const box = document.createElement("div");
    box.append(document.createElement("div"));
    const seen: HTMLElement[] = [];
    installHasMore(box, (b) => seen.push(b));
    const next = document.createElement("div");
    box.replaceChildren(next);
    await flushMutations();
    seen.length = 0;
    // Act
    fireResize(next);
    // Assert
    expect(seen).toEqual([box]);
  });

  it("stops measuring the body the box no longer holds", async () => {
    // Arrange
    const box = document.createElement("div");
    const old = document.createElement("div");
    box.append(old);
    installHasMore(box, () => undefined);
    box.replaceChildren(document.createElement("div"));
    await flushMutations();
    // Act + Assert
    expect(() => fireResize(old)).toThrow(/no ResizeObserver/);
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
