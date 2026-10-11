/**
 * A COLLAPSED BUBBLE'S "MORE BELOW" FADE ENDS AT THE BUBBLE'S BOTTOM EDGE, IN
 * WEBKIT (owner report, 2026-10-11: the fade sat "offset vertically some
 * distance from the bottom of the prompt bubble").
 *
 * Anchored to the scroll box, the fade ended the bubble's bottom padding above
 * its edge. styles.css ("FIX2 MORE BELOW") now hands a box shown last the
 * bubble as containing block, and makes the box it when a footer follows.
 * jsdom lays nothing out and a pseudo-element has no client rect, so this
 * paints the fade a solid sentinel color in real headless WebKit over the real
 * stylesheet, screenshots the page, and finds the sentinel's pixels.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { BUBBLE_EXPAND_ONLY_CLASS, BUBBLE_FOOTER_CLASS } from "../../src/bubble/draw.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/**
 * The fade in solid red over a solid blue bubble with no shadow, so a red
 * pixel is the fade and nothing else. Only the paint changes: the geometry
 * under test is the stylesheet's own.
 */
const SENTINEL = `
  .bubble > .bubble-scroll.has-more::after { background: rgb(255, 0, 0) !important; }
  .bubble { --bubble-bg: rgb(0, 0, 255); box-shadow: none !important; color: rgb(0, 0, 255); }`;

/** Prose far past a five-line cap, so the box clips and the fade is due. */
const TALL = Array.from({ length: 40 }, (_, i) => `line ${i}`).join("<br>");

/** A collapsed prompt bubble wearing has-more, with FOOTER after its box when given. */
const bubbleHtml = (footer: string): string => `
  <div class="bubble md" data-role="prompt" data-variant="user" data-cap-lines="5" data-more="fade">
    <div class="bubble-box bubble-scroll has-more"><div class="bubble-body">${TALL}</div></div>
    <div class="${BUBBLE_EXPAND_ONLY_CLASS}">details</div>${footer}
  </div>`;

/**
 * How far, in CSS px, the lowest fully red pixel row may sit from an edge: the
 * edge is fractional, so its last row is only partly covered, and the bubble's
 * hairline border lies between its padding box and its border box. The old
 * offset this guards against was the whole 0.6rem padding, ~9.6px.
 */
const EDGE_SLACK = 1.5;

/** Where the red fade painted, against the bubble's and the box's edges, in CSS px. */
interface Painted {
  bubbleBottom: number;
  boxBottom: number;
  /** One past the lowest red row in the bubble's middle column. */
  redBottom: number;
  /** Whether the fade paints inside the bubble's left padding, left of the box. */
  redInLeftPadding: boolean;
  /** Whether the fade paints the bubble's bottom-left corner pixel, outside its rounding. */
  redInCorner: boolean;
}

async function paint(page: Page, footer: string): Promise<Painted> {
  const rects = await page.evaluate((html) => {
    const feed = document.getElementById("feed");
    if (feed === null) throw new Error("the page's shell did not mount");
    feed.innerHTML = html;
    const bubble = feed.querySelector(".bubble");
    const box = feed.querySelector(".bubble-box");
    if (bubble === null || box === null) throw new Error("the bubble did not draw");
    const b = bubble.getBoundingClientRect();
    const x = box.getBoundingClientRect();
    return { left: b.left, right: b.right, bottom: b.bottom, boxLeft: x.left, boxBottom: x.bottom };
  }, bubbleHtml(footer));
  const shot = await page.screenshot();
  return page.evaluate(
    async ({ b64, r }) => {
      const img = new Image();
      img.src = `data:image/png;base64,${b64}`;
      await img.decode();
      const canvas = document.createElement("canvas");
      canvas.width = img.width;
      canvas.height = img.height;
      const g = canvas.getContext("2d");
      if (g === null) throw new Error("no 2d context");
      g.drawImage(img, 0, 0);
      const d = g.getImageData(0, 0, img.width, img.height).data;
      const red = (px: number, py: number): boolean => {
        const k = (Math.floor(py) * img.width + Math.floor(px)) * 4;
        return (d[k] ?? 0) > 200 && (d[k + 1] ?? 255) < 60 && (d[k + 2] ?? 255) < 60;
      };
      const mid = (r.left + r.right) / 2;
      let redBottom = 0;
      for (let y = 0; y < img.height; y++) if (red(mid, y)) redBottom = y + 1;
      return {
        bubbleBottom: r.bottom,
        boxBottom: r.boxBottom,
        redBottom,
        redInLeftPadding: red((r.left + r.boxLeft) / 2, redBottom - 6),
        redInCorner: red(r.left + 0.5, r.bottom - 0.5),
      };
    },
    { b64: shot.toString("base64"), r: rects },
  );
}

describe("the collapsed bubble's fade in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 800, height: 800 }, deviceScaleFactor: 1 });
    await page.setContent(`<!doctype html><html><head></head><body>
      <div id="main-col"><div id="feed-scroll"><main id="feed" data-feed="root"></main></div></div>
      </body></html>`);
    await page.addStyleTag({ content: css });
    await page.addStyleTag({ content: SENTINEL });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("ends the fade at the bubble's bottom edge when the box is the last thing shown", async () => {
    // Arrange / Act — the hidden expand-only chrome is all that follows the box.
    const p = await paint(page, "");

    // Assert — a whole padding band below the box, and the fade reaches through
    // it to the bubble's edge, short of it by no more than the hairline border
    // and the fractional edge's last partly covered pixel row.
    expect(p.bubbleBottom - p.boxBottom).toBeGreaterThan(5);
    expect(Math.abs(p.redBottom - p.bubbleBottom)).toBeLessThanOrEqual(EDGE_SLACK);
  });

  it("spans the bubble's left padding when the box is the last thing shown", async () => {
    // Arrange / Act
    const p = await paint(page, "");

    // Assert
    expect(p.redInLeftPadding).toBe(true);
  });

  it("keeps the bubble's rounded bottom corner unpainted by the fade", async () => {
    // Arrange / Act
    const p = await paint(page, "");

    // Assert
    expect(p.redInCorner).toBe(false);
  });

  it("ends the fade at the box's bottom when a footer follows it", async () => {
    // Arrange / Act — a footer after the box, as an errored response draws its
    // cut-short marker (unstyled here, so its own red cannot pass for the fade).
    const p = await paint(page, `<div class="${BUBBLE_FOOTER_CLASS}">cut short</div>`);

    // Assert — the fade stops above the footer, at the box's own bottom edge.
    expect(Math.abs(p.redBottom - p.boxBottom)).toBeLessThanOrEqual(EDGE_SLACK);
  });
});
