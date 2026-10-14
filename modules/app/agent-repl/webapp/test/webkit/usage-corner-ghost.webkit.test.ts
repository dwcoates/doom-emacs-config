/**
 * THE SIZING GHOST WIDENS A SHORT RESPONSE BY THE CORNER'S FOOTPRINT, AND
 * CHANGES NOTHING ABOUT THE CORNER, IN WEBKIT.
 *
 * A float before a block adds nothing to the box's fit-content width, so a
 * short response used to size itself to its text and then wrap its first line
 * around the cost corner well short of its cap. The ghost (styles.css "THE
 * SIZING GHOST") is a zero-height float of the corner's footprint inside the
 * first paragraph, where the engine DOES add a float to the line's width.
 * jsdom lays nothing out, so this draws response bubbles in real headless
 * WebKit over the real stylesheet and measures them with the ghost and with
 * the ghost suppressed (`content: none`), which is the layout before the fix.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import {
  USAGE_AGE_RESERVE_LABELS,
  USAGE_AGO_WIDTH_PROPERTY,
  USAGE_RESERVE_LABELS_PROPERTY,
  USAGE_TOKEN_TEXT_PROPERTY,
  usageReserveLabelsCss,
} from "../../src/feed/cards/response.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/** Suppresses the ghost: the layout the feed drew before it existed. */
const NO_GHOST = `.response-prose p::before { content: none !important; }`;

/** Every transition off, so a revealed corner is measured at its end state. */
const NO_TRANSITIONS = `* { transition: none !important; }`;

/** Words for prose of a chosen length. */
const WORDS = (
  "the daemon agent is done with one contract question about a compaction concluded line that " +
  "is salient but still retired by a timer so settling that in the proto first since the design is mine"
).split(" ");

/** Prose of N words. */
const prose = (n: number): string => [...WORDS, ...WORDS].slice(0, n).join(" ");

/** What one drawn bubble measured, every x relative to the bubble's right edge and y to its top. */
interface Drawn {
  bubble: number;
  lines: number;
  /** The words on the first line, in order. */
  firstLine: string;
  /**
   * The words the first line shows WHOLE, in order: a word the clamp cut
   * mid-word (styles.css "THE ONE-LINE CLAMP'S SHAPE") breaks across two
   * lines, so it is not among them.
   */
  firstLineWhole: string;
  firstLineRight: number;
  corner: { right: number; top: number; width: number; height: number };
  stamp: { right: number; top: number };
  ago: { right: number; top: number; opacity: string };
  /** The same edges taken from the bubble's LEFT edge, which a growing bubble keeps. */
  fromLeft: { lineRight: number; stamp: { left: number; right: number }; ago: { left: number; right: number } };
}

/**
 * Draw one response bubble holding TEXT, its cap mode CAP, and measure it. With
 * ELLIPSIS it is a one-line ellipsis bubble (an interim response), open or closed,
 * handed the token text and the duration's drawn width the response draw hands
 * it, and REVEALED by focusing its corner, as a hover or a keyboard does.
 */
async function draw(
  page: Page,
  opts: { text: string; ghost: boolean; revealed?: boolean; cap?: string; ellipsis?: "open" | "closed" },
): Promise<Drawn> {
  return page.evaluate(
    ({ text, ghost, revealed, cap, ellipsis, labels, property, labelsCss, noGhost, tokenProperty, agoProperty }) => {
      document.getElementById("ghost-switch")!.textContent = ghost ? "" : noGhost;
      const reserve = labels.map((l) => `<span class="usage-reserve-label">${l}</span>`).join("");
      const feed = document.getElementById("feed")!;
      feed.innerHTML = `<div class="feed-item">
        <div class="bubble md assistant${ellipsis === null ? "" : " interim-response"}" data-role="response" data-variant="response" data-cap-lines="${ellipsis === null ? cap : "1"}"${ellipsis === null ? "" : ' data-more="ellipsis"'} data-state="success">
          <div class="bubble-box${ellipsis === null ? "" : " bubble-scroll"}${ellipsis === "open" ? " expanded" : ""}"><span class="usage-corner${revealed ? " usage-corner--revealed" : ""}" data-tokens="12.3k" tabindex="0"><span class="usage-reserve" aria-hidden="true">${reserve}</span><span class="usage-slider"><span class="usage-stamp">12.3k</span><span class="usage-ago">5m 30s ago</span></span></span><bubble-body class="bubble-body"><div class="response-prose" data-markdown><p>${text
            .split(" ")
            .map((w) => `<span class="w">${w}</span>`)
            .join(" ")}</p></div></bubble-body></div>
        </div></div>`;
      const bubble = feed.querySelector<HTMLElement>(".bubble")!;
      bubble.style.setProperty(property, labelsCss);
      if (ellipsis !== null) {
        bubble.style.setProperty(tokenProperty, '"12.3k"');
        const agoWidth = feed.querySelector(".usage-ago")!.getBoundingClientRect().width;
        bubble.style.setProperty(agoProperty, `${String(agoWidth)}px`);
        if (revealed) feed.querySelector<HTMLElement>(".usage-corner")!.focus();
      }
      const b = bubble.getBoundingClientRect();
      const rel = (el: Element): DOMRect => el.getBoundingClientRect();
      // Hundredths of a px: the bubble's own left edge moves with its width,
      // so a position taken from its right edge carries float noise below that.
      const px = (v: number): number => Math.round(v * 100) / 100;
      const tops = new Map<number, number>();
      const words = new Map<number, string[]>();
      const whole = new Map<number, string[]>();
      for (const w of feed.querySelectorAll(".w")) {
        const r = rel(w);
        const top = Math.round(r.top);
        tops.set(top, Math.max(tops.get(top) ?? 0, r.right));
        words.set(top, [...(words.get(top) ?? []), w.textContent ?? ""]);
        if (w.getClientRects().length === 1) whole.set(top, [...(whole.get(top) ?? []), w.textContent ?? ""]);
      }
      const firstTop = Math.min(...tops.keys());
      const corner = rel(feed.querySelector(".usage-corner")!);
      const stamp = rel(feed.querySelector(".usage-stamp")!);
      const agoEl = feed.querySelector(".usage-ago")!;
      const ago = rel(agoEl);
      return {
        bubble: b.width,
        lines: tops.size,
        firstLine: words.get(firstTop)!.join(" "),
        firstLineWhole: (whole.get(firstTop) ?? []).join(" "),
        firstLineRight: b.right - tops.get(firstTop)!,
        corner: {
          right: px(b.right - corner.right),
          top: px(corner.top - b.top),
          width: px(corner.width),
          height: px(corner.height),
        },
        stamp: { right: px(b.right - stamp.right), top: px(stamp.top - b.top) },
        ago: { right: px(b.right - ago.right), top: px(ago.top - b.top), opacity: getComputedStyle(agoEl).opacity },
        fromLeft: {
          lineRight: px(tops.get(firstTop)! - b.left),
          stamp: { left: px(stamp.left - b.left), right: px(stamp.right - b.left) },
          ago: { left: px(ago.left - b.left), right: px(ago.right - b.left) },
        },
      };
    },
    {
      text: opts.text,
      ghost: opts.ghost,
      revealed: opts.revealed ?? false,
      cap: opts.cap ?? "none",
      ellipsis: opts.ellipsis ?? null,
      labels: [...USAGE_AGE_RESERVE_LABELS],
      property: USAGE_RESERVE_LABELS_PROPERTY,
      labelsCss: usageReserveLabelsCss(),
      noGhost: NO_GHOST,
      tokenProperty: USAGE_TOKEN_TEXT_PROPERTY,
      agoProperty: USAGE_AGO_WIDTH_PROPERTY,
    },
  );
}

/** A headless WebKit page holding an empty feed over the real stylesheet, transitions off, with the ghost switch. */
async function openFeed(): Promise<{ browser: Browser; page: Page }> {
  const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
  const browser = await webkit.launch();
  const page = await browser.newPage({ viewport: { width: 1100, height: 900 } });
  await page.setContent(`<!doctype html><html><head></head><body>
    <div id="main-col">
      <div id="feed-scroll" class="scroll-zone">
        <main id="feed" data-feed="root"></main>
      </div>
    </div></body></html>`);
  await page.addStyleTag({ content: css });
  await page.addStyleTag({ content: NO_TRANSITIONS });
  await page.evaluate(() => {
    const sw = document.createElement("style");
    sw.id = "ghost-switch";
    document.head.appendChild(sw);
  });
  return { browser, page };
}

/** The corner's outer footprint, margins included: what the ghost must add. */
async function cornerFootprint(page: Page): Promise<number> {
  // A response at the cap, so the corner is drawn at its full width.
  await draw(page, { text: prose(40), ghost: false });
  return page.evaluate(() => {
    const corner = document.querySelector(".usage-corner")!;
    const s = getComputedStyle(corner);
    return corner.getBoundingClientRect().width + parseFloat(s.marginLeft) + parseFloat(s.marginRight);
  });
}

describe("the usage corner's sizing ghost in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    ({ browser, page } = await openFeed());
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each([6, 10, 14])("keeps a %i-word response on one line beside the corner", async (n) => {
    // Arrange / Act
    const drawn = await draw(page, { text: prose(n), ghost: true });

    // Assert
    expect(drawn.lines).toBe(1);
  });

  it.each([6, 10, 14])("widens a %i-word response by exactly the corner's footprint", async (n) => {
    // Arrange
    const footprint = await cornerFootprint(page);
    const before = await draw(page, { text: prose(n), ghost: false });

    // Act
    const after = await draw(page, { text: prose(n), ghost: true });

    // Assert
    expect(Math.abs(after.bubble - before.bubble - footprint)).toBeLessThanOrEqual(1);
  });

  it("wraps a response that fills the cap exactly as it did before", async () => {
    // Arrange
    const before = await draw(page, { text: prose(40), ghost: false });

    // Act
    const after = await draw(page, { text: prose(40), ghost: true });

    // Assert
    expect([after.bubble, after.lines, after.firstLineRight]).toEqual([before.bubble, before.lines, before.firstLineRight]);
  });

  it.each([false, true])("draws the corner, the token and the duration where they were (revealed: %s)", async (revealed) => {
    // Arrange
    const before = await draw(page, { text: prose(40), ghost: false, revealed });

    // Act
    const after = await draw(page, { text: prose(40), ghost: true, revealed });

    // Assert
    expect([after.corner, after.stamp, after.ago]).toEqual([before.corner, before.stamp, before.ago]);
  });

  it.each([false, true])("keeps a short response's corner where it was against the bubble's corner (revealed: %s)", async (revealed) => {
    // Arrange
    const before = await draw(page, { text: prose(6), ghost: false, revealed });

    // Act
    const after = await draw(page, { text: prose(6), ghost: true, revealed });

    // Assert
    expect([after.corner, after.stamp, after.ago]).toEqual([before.corner, before.stamp, before.ago]);
  });

  it.each([false, true])("keeps a one-word response's token and duration where they were (revealed: %s)", async (revealed) => {
    // Arrange: a bubble narrower than the corner, whose corner is squeezed by
    // its own `max-width: 100%` both before and after; only its invisible
    // left reserve can differ, so the visible figures are what is compared.
    const before = await draw(page, { text: "ok", ghost: false, revealed });

    // Act
    const after = await draw(page, { text: "ok", ghost: true, revealed });

    // Assert
    expect([after.stamp, after.ago]).toEqual([before.stamp, before.ago]);
  });

  it("gives a capped bubble no ghost", async () => {
    // Arrange
    const before = await draw(page, { text: prose(6), ghost: false, cap: "feed" });

    // Act
    const after = await draw(page, { text: prose(6), ghost: true, cap: "feed" });

    // Assert
    expect(after.bubble).toBe(before.bubble);
  });
});

/**
 * AN ELLIPSIS BUBBLE OPENS TO WHAT IT SHOWED CLOSED (owner request,
 * 2026-10-10). A closed interim's clamped body is a box of its own beside the
 * corner, so its bubble is the text plus the corner; an open one used to shrink
 * to the text alone and wrap its first line around the corner. The ghost on the
 * open bubble keeps its width and its first line.
 */
describe("an ellipsis bubble with a corner in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    ({ browser, page } = await openFeed());
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each([4, 10, 16])("keeps a %i-word interim on one line once opened", async (n) => {
    // Arrange / Act
    const drawn = await draw(page, { text: prose(n), ghost: true, ellipsis: "open" });

    // Assert
    expect(drawn.lines).toBe(1);
  });

  it.each([4, 10, 16, 40])("keeps a %i-word interim's width once opened", async (n) => {
    // Arrange
    const closed = await draw(page, { text: prose(n), ghost: true, ellipsis: "closed" });

    // Act
    const open = await draw(page, { text: prose(n), ghost: true, ellipsis: "open" });

    // Assert
    expect(Math.abs(open.bubble - closed.bubble)).toBeLessThanOrEqual(1);
  });

  it.each([4, 10, 16])("keeps a %i-word interim's first line once opened", async (n) => {
    // Arrange
    const closed = await draw(page, { text: prose(n), ghost: true, ellipsis: "closed" });

    // Act
    const open = await draw(page, { text: prose(n), ghost: true, ellipsis: "open" });

    // Assert
    expect(open.firstLine).toBe(closed.firstLine);
  });

  it("keeps every whole word an overflowing interim showed, in place, once opened", async () => {
    // Arrange: 40 words overflow the one clamped line, which ends in "…" at
    // the edge, mid-word if need be (owner request, 2026-10-14); the opened
    // line wraps at its words, so a word the ellipsis cut may move to the next
    // line, and the words before it may not.
    const closed = await draw(page, { text: prose(40), ghost: true, ellipsis: "closed" });

    // Act
    const open = await draw(page, { text: prose(40), ghost: true, ellipsis: "open" });

    // Assert: the closed line showed words whole, and none of them moves.
    expect([closed.firstLineWhole !== "", open.firstLine.startsWith(closed.firstLineWhole)]).toEqual([true, true]);
  });
});

/**
 * AN INTERIM'S CORNER GROWS ITS BUBBLE TO THE RIGHT (owner request,
 * 2026-10-13). At rest the bubble fits its line and its token, a text gap
 * apart; revealed, it opens to the right by exactly the duration and its gap,
 * past the cap if the line is truncated there, and neither the line nor the
 * token moves.
 */
describe("an interim's growing corner in WebKit", () => {
  let browser: Browser;
  let page: Page;

  /** The text gap and the pair gap: 0.35rem at the page's 16px root. */
  const GAP = 0.35 * 16;

  beforeAll(async () => {
    ({ browser, page } = await openFeed());
  });

  afterAll(async () => {
    await browser.close();
  });

  /** The duration's drawn width in the bubble last drawn. */
  async function agoWidth(): Promise<number> {
    return page.evaluate(() => document.querySelector(".usage-ago")!.getBoundingClientRect().width);
  }

  it.each([4, 10])("sets a closed %i-word interim's token a text gap after its line", async (n) => {
    // Arrange / Act
    const drawn = await draw(page, { text: prose(n), ghost: true, ellipsis: "closed" });

    // Assert
    expect(Math.abs(drawn.fromLeft.stamp.left - drawn.fromLeft.lineRight - GAP)).toBeLessThanOrEqual(1);
  });

  it.each([4, 40])("keeps an open %i-word interim's first line clear of its token", async (n) => {
    // Arrange / Act
    const drawn = await draw(page, { text: prose(n), ghost: true, ellipsis: "open" });

    // Assert
    expect(drawn.fromLeft.lineRight).toBeLessThanOrEqual(drawn.fromLeft.stamp.left - GAP + 1);
  });

  it.each(["closed", "open"] as const)("opens a short %s interim by exactly the duration and its gap", async (ellipsis) => {
    // Arrange
    const rest = await draw(page, { text: prose(6), ghost: true, ellipsis });
    const width = await agoWidth();

    // Act
    const revealed = await draw(page, { text: prose(6), ghost: true, ellipsis, revealed: true });

    // Assert
    expect(Math.abs(revealed.bubble - rest.bubble - GAP - width)).toBeLessThanOrEqual(1);
  });

  it("opens a closed interim truncated at the cap past the cap, by the same width", async () => {
    // Arrange
    const rest = await draw(page, { text: prose(40), ghost: true, ellipsis: "closed" });
    const width = await agoWidth();

    // Act
    const revealed = await draw(page, { text: prose(40), ghost: true, ellipsis: "closed", revealed: true });

    // Assert
    expect(Math.abs(revealed.bubble - rest.bubble - GAP - width)).toBeLessThanOrEqual(1);
  });

  it.each([
    ["a short closed", 6, "closed"],
    ["a truncated closed", 40, "closed"],
    ["a short open", 6, "open"],
    ["a long open", 40, "open"],
  ] as const)("keeps %s interim's line and token where they were", async (_name, n, ellipsis) => {
    // Arrange
    const rest = await draw(page, { text: prose(n), ghost: true, ellipsis });

    // Act
    const revealed = await draw(page, { text: prose(n), ghost: true, ellipsis, revealed: true });

    // Assert
    expect([revealed.lines, revealed.firstLine, revealed.fromLeft.lineRight, revealed.fromLeft.stamp]).toEqual([
      rest.lines,
      rest.firstLine,
      rest.fromLeft.lineRight,
      rest.fromLeft.stamp,
    ]);
  });

  it.each(["closed", "open"] as const)("lands a %s interim's duration a pair gap after the token, shown", async (ellipsis) => {
    // Arrange / Act
    const revealed = await draw(page, { text: prose(6), ghost: true, ellipsis, revealed: true });

    // Assert
    expect([Math.abs(revealed.fromLeft.ago.left - revealed.fromLeft.stamp.right - GAP) <= 1, revealed.ago.opacity]).toEqual([
      true,
      "1",
    ]);
  });

  it.each(["closed", "open"] as const)("keeps a %s interim's right gap after the duration what it was after the token", async (ellipsis) => {
    // Arrange
    const rest = await draw(page, { text: prose(6), ghost: true, ellipsis });

    // Act
    const revealed = await draw(page, { text: prose(6), ghost: true, ellipsis, revealed: true });

    // Assert
    expect(Math.abs(revealed.ago.right - rest.stamp.right)).toBeLessThanOrEqual(1);
  });

  it("hides a resting interim's duration past the bubble's edge", async () => {
    // Arrange / Act
    const rest = await draw(page, { text: prose(6), ghost: true, ellipsis: "closed" });

    // Assert: its left edge sits past the bubble's right edge, and it is faded out.
    expect([rest.ago.right + (rest.fromLeft.ago.right - rest.fromLeft.ago.left) <= 0, rest.ago.opacity]).toEqual([true, "0"]);
  });
});
