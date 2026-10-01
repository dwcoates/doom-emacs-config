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
import { USAGE_AGE_RESERVE_LABELS, USAGE_RESERVE_LABELS_PROPERTY, usageReserveLabelsCss } from "../../src/feed/cards/response.js";

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
  firstLineRight: number;
  corner: { right: number; top: number; width: number; height: number };
  stamp: { right: number; top: number };
  ago: { right: number; top: number; opacity: string };
}

/** Draw one response bubble holding TEXT, its cap mode CAP, and measure it. */
async function draw(page: Page, opts: { text: string; ghost: boolean; revealed?: boolean; cap?: string }): Promise<Drawn> {
  return page.evaluate(
    ({ text, ghost, revealed, cap, labels, property, labelsCss, noGhost }) => {
      document.getElementById("ghost-switch")!.textContent = ghost ? "" : noGhost;
      const reserve = labels.map((l) => `<span class="usage-reserve-label">${l}</span>`).join("");
      const feed = document.getElementById("feed")!;
      feed.innerHTML = `<div class="feed-item" style="content-visibility: visible">
        <div class="bubble md assistant" data-role="response" data-variant="response" data-cap-lines="${cap}" data-state="success">
          <div class="bubble-box"><span class="usage-corner${revealed ? " usage-corner--revealed" : ""}" data-tokens="12.3k"><span class="usage-reserve" aria-hidden="true">${reserve}</span><span class="usage-slider"><span class="usage-stamp">12.3k</span><span class="usage-ago">5m 30s ago</span></span></span><bubble-body class="bubble-body"><div class="response-prose" data-markdown><p>${text
            .split(" ")
            .map((w) => `<span class="w">${w}</span>`)
            .join(" ")}</p></div></bubble-body></div>
        </div></div>`;
      const bubble = feed.querySelector<HTMLElement>(".bubble")!;
      bubble.style.setProperty(property, labelsCss);
      const b = bubble.getBoundingClientRect();
      const rel = (el: Element): DOMRect => el.getBoundingClientRect();
      // Hundredths of a px: the bubble's own left edge moves with its width,
      // so a position taken from its right edge carries float noise below that.
      const px = (v: number): number => Math.round(v * 100) / 100;
      const tops = new Map<number, number>();
      for (const w of feed.querySelectorAll(".w")) {
        const r = rel(w);
        const top = Math.round(r.top);
        tops.set(top, Math.max(tops.get(top) ?? 0, r.right));
      }
      const firstTop = Math.min(...tops.keys());
      const corner = rel(feed.querySelector(".usage-corner")!);
      const stamp = rel(feed.querySelector(".usage-stamp")!);
      const agoEl = feed.querySelector(".usage-ago")!;
      const ago = rel(agoEl);
      return {
        bubble: b.width,
        lines: tops.size,
        firstLineRight: b.right - tops.get(firstTop)!,
        corner: {
          right: px(b.right - corner.right),
          top: px(corner.top - b.top),
          width: px(corner.width),
          height: px(corner.height),
        },
        stamp: { right: px(b.right - stamp.right), top: px(stamp.top - b.top) },
        ago: { right: px(b.right - ago.right), top: px(ago.top - b.top), opacity: getComputedStyle(agoEl).opacity },
      };
    },
    {
      text: opts.text,
      ghost: opts.ghost,
      revealed: opts.revealed ?? false,
      cap: opts.cap ?? "none",
      labels: [...USAGE_AGE_RESERVE_LABELS],
      property: USAGE_RESERVE_LABELS_PROPERTY,
      labelsCss: usageReserveLabelsCss(),
      noGhost: NO_GHOST,
    },
  );
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
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 1100, height: 900 } });
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
