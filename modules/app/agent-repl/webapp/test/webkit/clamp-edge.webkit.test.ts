/**
 * A COLLAPSED ONE-LINE CLAMP ENDS ITS `…` AT THE CONTENT'S RIGHT EDGE, IN
 * WEBKIT (owner report, 2026-10-14: the ellipsis in response bubbles and tool
 * cards often fell well short of the bubble's or card's end).
 *
 * The engine writes the clamp's `…` at the end of the last line it keeps, and
 * that line used to end at its last word break, a whole word short of the
 * edge. styles.css ("THE ONE-LINE CLAMP'S SHAPE") now lets the kept line break
 * anywhere, so the `…` sits as far from the right side as the text starts from
 * the left. jsdom lays nothing out and an ellipsis has no client rect, so this
 * paints the text a solid sentinel color in real headless WebKit over the real
 * stylesheet, screenshots it, and finds the sentinel's leftmost and rightmost
 * pixels against the box's own edges.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { ELLIPSIS_CAP_LINES } from "../../src/bubble/draw.js";
import { BUBBLE_MORE_ELLIPSIS } from "../../src/feed/bubble-more.js";
import { TITLE_FOLD_CLASS } from "../../src/feed/title-fold.js";
import { screenshotPixels } from "./pixels";

const here = path.dirname(fileURLToPath(import.meta.url));

/**
 * The text in solid red over solid blue boxes with no shadow or motion, so a
 * red pixel is a glyph and nothing else. Only the paint changes: the geometry
 * under test is the stylesheet's own.
 */
const SENTINEL = `
  .tool-card, .bubble { background: rgb(0, 0, 255) !important; box-shadow: none !important; animation: none !important; }
  .tool-card *, .bubble * { color: rgb(255, 0, 0) !important; }`;

/**
 * Long words, each far wider than the ellipsis, so a line kept to its last
 * word break ends a whole word short of the edge.
 */
const LONG_WORDS = "cd /a/really/long/path extraordinarilylongwordthatoverrunsthewholeline andthenevenmorecharacters tail";

/** A collapsed tool card whose title is the one-line fold. */
const CARD = `<div class="tool-card tool-fold" style="width: 360px">
  <div class="tool-input ${TITLE_FOLD_CLASS}">${LONG_WORDS}</div></div>`;

/** A collapsed ellipsis bubble: a held prompt on its one-line cap. */
const BUBBLE = `<div class="bubble md" data-role="prompt" data-variant="held" data-cap-lines="${ELLIPSIS_CAP_LINES}"
  data-more="${BUBBLE_MORE_ELLIPSIS}" style="width: 360px">
  <div class="bubble-box bubble-scroll"><div class="bubble-body"><p>${LONG_WORDS}</p></div></div></div>`;

/**
 * How much farther the last red pixel may sit from the right edge than the
 * first does from the left, in CSS px: under one character's width, since the
 * kept line breaks between characters and the `…` replaces the last ones that
 * do not leave it room. A line kept to its last word break ends a whole word
 * short, here well over 100px.
 */
const EDGE_SLACK = 10;

/** The gaps between a box's border edges and its painted text, in CSS px. */
interface Gaps {
  left: number;
  right: number;
}

async function gaps(page: Page, html: string, textSelector: string): Promise<Gaps> {
  const rects = await page.evaluate(
    ({ html, textSelector }) => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = html;
      const box = feed.firstElementChild;
      const text = feed.querySelector(textSelector);
      if (box === null || text === null) throw new Error("the box did not draw");
      const b = box.getBoundingClientRect();
      const t = text.getBoundingClientRect();
      return { left: b.left, right: b.right, top: t.top, bottom: t.bottom };
    },
    { html, textSelector },
  );
  // The text's row alone, at deviceScaleFactor 1, so a device pixel is a CSS
  // pixel and the shot's column x is the page's column x.
  const top = Math.floor(rects.top);
  const px = await screenshotPixels(page, { x: 0, y: top, width: 800, height: Math.ceil(rects.bottom) - top });
  let first = Infinity;
  let last = -Infinity;
  for (let y = 0; y < px.height; y++) {
    for (let x = Math.floor(rects.left); x < Math.ceil(rects.right); x++) {
      const [r, g, b] = px.rgba(x, y);
      if (r > 200 && g < 60 && b < 60) {
        first = Math.min(first, x);
        last = Math.max(last, x + 1);
      }
    }
  }
  if (first === Infinity) throw new Error("no text painted");
  return { left: first - rects.left, right: rects.right - last };
}

describe("the collapsed one-line clamp's ellipsis in WebKit", () => {
  let browser: Browser;
  // THE PAINTS ARE THE beforeAll'S, AND THE TESTS ONLY READ THEM, as every
  // webkit suite does (vitest.webkit.config.ts).
  let card: Gaps;
  let bubble: Gaps;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: { width: 800, height: 400 }, deviceScaleFactor: 1 });
    await page.setContent(`<!doctype html><html><head></head><body>
      <div id="main-col"><div id="feed-scroll"><main id="feed" data-feed="root"></main></div></div>
      </body></html>`);
    await page.addStyleTag({ content: css });
    await page.addStyleTag({ content: SENTINEL });
    card = await gaps(page, CARD, `.${TITLE_FOLD_CLASS}`);
    bubble = await gaps(page, BUBBLE, ".bubble-body");
  });

  afterAll(async () => {
    await browser.close();
  });

  it("ends a tool card title's ellipsis as far from the right edge as the title starts from the left", () => {
    // Arrange / Act
    const g = card;

    // Assert
    expect(g.right - g.left).toBeLessThanOrEqual(EDGE_SLACK);
  });

  it("ends an ellipsis bubble's ellipsis as far from the right edge as the text starts from the left", () => {
    // Arrange / Act
    const g = bubble;

    // Assert
    expect(g.right - g.left).toBeLessThanOrEqual(EDGE_SLACK);
  });
});
