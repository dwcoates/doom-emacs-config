/**
 * THE RAIL'S STATUS MARKS SHARE ONE CENTER AND ONE SIZE, IN WEBKIT (owner
 * requests, 2026-10-08 and 2026-10-13).
 *
 * Every glyph (the merge glyph's three bars, the gate emojis ❗ ❓ ❄️ and a
 * working one-shot's 🎯) must sit centered on the same vertical line as the
 * filled status dots, draw at the dot's size, and hold one column width with
 * every other mark, the hidden `none` one included, so the names beside them
 * stay aligned. jsdom lays nothing out, so this draws rail rows as
 * `drawStatusMark` (src/sidebar/row.ts) shapes them, in real headless WebKit
 * over the real stylesheet, and measures them.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

/** One row's mark: its arm, glyph name and the character drawn (none for a disc). */
interface Mark {
  arm: string;
  glyph: string;
  char: string;
}

const DOT: Mark = { arm: "ready", glyph: "dot", char: "" };
const GLYPHS: readonly Mark[] = [
  { arm: "mergeQueued", glyph: "merge", char: "" },
  { arm: "permission", glyph: "exclaim", char: "❗" },
  { arm: "question", glyph: "ask", char: "❓" },
  { arm: "coldGate", glyph: "snowflake", char: "\u2744\uFE0F" },
  { arm: "thinking", glyph: "target", char: "🎯" },
];
const NONE: Mark = { arm: "none", glyph: "none", char: "" };

/** One rail row, as `drawStatusMark` and the row draw shape it. */
function row(m: Mark): string {
  const glyphClass = m.glyph === "dot" ? "" : " st-glyph";
  return `<div class="row"><span class="st tone-green${glyphClass}" data-glyph="${m.glyph}">${m.char}</span><span class="name">workspace</span></div>`;
}

/**
 * What one row measured: the mark's ink center and height (the disc's box, or
 * the character's own box) and the name's left edge.
 */
interface Measured {
  center: number;
  height: number;
  nameLeft: number;
}

/** Draw MARKS as rail rows and measure each. */
async function measure(page: Page, marks: readonly Mark[]): Promise<Measured[]> {
  return page.evaluate((markup) => {
    const host = document.getElementById("ws-sidebar");
    if (host === null) throw new Error("the page's rail did not mount");
    host.innerHTML = markup;
    return [...host.querySelectorAll(".row")].map((r) => {
      const st = r.querySelector(".st") as HTMLElement;
      let box = st.getBoundingClientRect();
      const text = st.firstChild;
      if (text !== null) {
        const range = document.createRange();
        range.selectNodeContents(text);
        box = range.getBoundingClientRect();
      }
      const name = (r.querySelector(".name") as HTMLElement).getBoundingClientRect();
      return { center: box.left + box.width / 2, height: box.height, nameLeft: name.left };
    });
  }, marks.map(row).join(""));
}

describe("the rail's status marks in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 400 } });
    await page.setContent(`<!doctype html><html><head></head><body><div id="ws-sidebar"></div></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(GLYPHS)("centers the $glyph glyph on the dots' center", async (glyph) => {
    // Arrange / Act
    const [dot, mark] = await measure(page, [DOT, glyph]);
    // Assert
    expect(Math.abs((mark?.center ?? NaN) - (dot?.center ?? NaN))).toBeLessThanOrEqual(0.5);
  });

  // ONE SIZE (owner ruling, 2026-10-13): no mark reads bigger than the dot.
  // A character's box is its line box, a little taller than its ink, so the
  // bound is the dot's height plus the line's own leading.
  it.each(GLYPHS)("draws the $glyph glyph at the dot's size", async (glyph) => {
    // Arrange / Act
    const [dot, mark] = await measure(page, [DOT, glyph]);
    // Assert
    expect(mark?.height ?? NaN).toBeGreaterThanOrEqual((dot?.height ?? NaN) - 1);
    expect(mark?.height ?? NaN).toBeLessThanOrEqual((dot?.height ?? NaN) + 3);
  });

  it.each([...GLYPHS, NONE])("starts the name beside a $glyph mark where it starts beside a dot", async (glyph) => {
    // Arrange / Act
    const [dot, mark] = await measure(page, [DOT, glyph]);
    // Assert
    expect(mark?.nameLeft).toBeCloseTo(dot?.nameLeft ?? NaN, 1);
  });
});
