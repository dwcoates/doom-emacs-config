/**
 * A TITLED CARD GROWS WITH ITS TITLE AS IT TYPES OUT, IN WEBKIT (owner
 * request, 2026-10-24: "the bubble should be the canonical distance from the
 * text at every point in that streaming process", for "everything with a
 * blue title").
 *
 * The title type-out (src/feed/title-reveal.ts) shows a growing prefix of the
 * title's text. A card at the column's full cap left that prefix far short of
 * its right edge, so the card never moved with it. styles.css "A TITLED CARD
 * GROWS WITH ITS TITLE" fits every `.tool-head` card to its content on the
 * response's left rail. jsdom lays nothing out, so this draws the cards in
 * real headless WebKit over the real stylesheet and measures them at each
 * typed length.
 */
import type { Browser, Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { openFeedPage } from "./feed-page";

/** The words a title types out, long enough to reach the cap at the page's width. */
const TITLE =
  "$ go test ./daemon/internal/workspace/... -run TestTheWorkspaceFinishesItsMergeAndRollsOutTheBuild -count 1 -v";

/** The typed lengths measured: past the head's own width, short of the cap. */
const SHORT_LENGTHS = [20, 35, 50, 65] as const;

/** A typed length whose title is narrower than the head above it. */
const TINY_LENGTH = 1;

/** A tool card as `drawFeedSimpleToolCall` draws it, its title typed to TITLE's first N characters. */
const toolCard = (n: number): string => `
  <div class="feed-item"><div class="tool-card tool-fold" data-state="running">
    <div class="tool-head" data-tool-card-head=""><span class="tool-name">Bash</span><span class="badge run">running…</span></div>
    <pre class="tool-input title-fold">${TITLE.slice(0, n)}</pre>
  </div></div>`;

/** A response bubble, whose left rail the card shares. */
const RESPONSE = `
  <div class="feed-item"><div class="bubble md assistant" data-role="response" data-variant="response">
    <div class="bubble-box"><div class="bubble-body"><p>hi</p></div></div>
  </div></div>`;

/** A merge-style sub-feed bubble, expanded when EXPANDED says. */
const subfeedBubble = (expanded: boolean): string => `
  <div class="feed-item"><div class="tool-card bubble-fold" data-expanded="${String(expanded)}">
    <div class="tool-head bubble-head"><span class="tool-name">merge</span></div>
    <div class="agent-panel bubble-subfeed"></div>
  </div></div>`;

/** A tool group showing one short-titled member. */
const GROUP = `
  <div class="feed-item"><div class="feed-group"><div class="feed-group-panel">${toolCard(4)}</div></div></div>`;

/** One drawn card's edges, with the typed title's right edge, in CSS px. */
interface Measured {
  left: number;
  right: number;
  textRight: number;
  /** The containing row's width: what the cap is a share of. */
  row: number;
}

/** What the suite measured. */
interface Measures {
  short: Measured[];
  tiny: Measured;
  long: Measured;
  responseLeft: number;
  collapsedFold: Measured;
  expandedFold: Measured;
  groupCard: Measured;
  group: Measured;
  /** The cap as a share of the row, read off the stylesheet. */
  cap: number;
}

/** Draw HTML alone in the feed and measure the first element matching SELECTOR. */
async function measure(page: Page, html: string, selector: string): Promise<Measured> {
  return page.evaluate(
    ([markup, sel]) => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = markup;
      const el = feed.querySelector(sel);
      if (el === null) throw new Error(`nothing drew ${sel}`);
      const box = el.getBoundingClientRect();
      const title = el.querySelector(".title-fold");
      let textRight = Number.NaN;
      if (title !== null) {
        const range = document.createRange();
        range.selectNodeContents(title);
        textRight = range.getBoundingClientRect().right;
      }
      const row = (el.closest(".feed-item") ?? feed).getBoundingClientRect().width;
      return { left: box.left, right: box.right, textRight, row };
    },
    [html, selector] as const,
  );
}

describe("a titled card in WebKit", () => {
  let browser: Browser;
  // THE MEASURES ARE THE beforeAll'S, AND THE TESTS ONLY READ THEM, as every
  // webkit suite does (vitest.webkit.config.ts).
  let m: Measures;

  beforeAll(async () => {
    let page: Page;
    ({ browser, page } = await openFeedPage({ width: 800, height: 800 }));
    const short: Measured[] = [];
    for (const n of SHORT_LENGTHS) short.push(await measure(page, toolCard(n), ".tool-card"));
    const cap = await page.evaluate(() => {
      const col = document.getElementById("main-col");
      if (col === null) throw new Error("no column");
      return Number.parseFloat(getComputedStyle(col).getPropertyValue("--agent-bubble-cap")) / 100;
    });
    m = {
      short,
      tiny: await measure(page, toolCard(TINY_LENGTH), ".tool-card"),
      long: await measure(page, toolCard(TITLE.length), ".tool-card"),
      responseLeft: (await measure(page, RESPONSE, ".bubble")).left,
      collapsedFold: await measure(page, subfeedBubble(false), ".tool-card"),
      expandedFold: await measure(page, subfeedBubble(true), ".tool-card"),
      groupCard: await measure(page, GROUP, ".feed-group-panel .tool-card"),
      group: await measure(page, GROUP, ".feed-group"),
      cap,
    };
  });

  afterAll(async () => {
    await browser.close();
  });

  it("keeps the card's right edge one fixed gap from the typed title at every length", () => {
    // Arrange / Act
    const gaps = m.short.map((s) => s.right - s.textRight);

    // Assert — the same padding at every length, within a subpixel.
    for (const gap of gaps) expect(Math.abs(gap - gaps[0])).toBeLessThan(1);
  });

  it("grows the card as the title types out", () => {
    // Arrange / Act
    const widths = m.short.map((s) => s.right - s.left);

    // Assert
    expect(widths).toEqual([...widths].sort((a, b) => a - b));
    expect(widths[widths.length - 1] - widths[0]).toBeGreaterThan(100);
  });

  it("keeps a card whose title is narrower than its head at the head's width", () => {
    // Arrange / Act
    const [tiny, first] = [m.tiny.right - m.tiny.left, m.short[0].right - m.short[0].left];

    // Assert — wider than the one typed character needs, narrower than a typed title past the head.
    expect([tiny > m.tiny.textRight - m.tiny.left + 40, tiny < first]).toEqual([true, true]);
  });

  it("hangs the card on the response's left rail", () => {
    // Arrange / Act
    const lefts = m.short.map((s) => s.left);

    // Assert
    for (const left of lefts) expect(Math.abs(left - m.responseLeft)).toBeLessThan(0.5);
  });

  it("stops a long title's card at the cap", () => {
    // Arrange / Act
    const width = m.long.right - m.long.left;

    // Assert
    expect(Math.abs(width - m.long.row * m.cap)).toBeLessThan(1);
  });

  it("fits a collapsed sub-feed bubble to its head", () => {
    // Arrange / Act
    const width = m.collapsedFold.right - m.collapsedFold.left;

    // Assert
    expect(width).toBeLessThan(m.collapsedFold.row * m.cap - 100);
  });

  it("spans an expanded sub-feed bubble across the cap on the left rail", () => {
    // Arrange / Act
    const width = m.expandedFold.right - m.expandedFold.left;

    // Assert
    expect([Math.abs(width - m.expandedFold.row * m.cap) < 1, Math.abs(m.expandedFold.left - m.responseLeft) < 0.5]).toEqual([
      true,
      true,
    ]);
  });

  it("fills a tool group with the member it shows", () => {
    // Arrange / Act / Assert
    expect([Math.abs(m.groupCard.left - m.group.left) < 0.5, Math.abs(m.groupCard.right - m.group.right) < 0.5]).toEqual([
      true,
      true,
    ]);
  });
});
