/**
 * THE RAIL'S STATUS MARKS SHARE ONE CENTER AND ONE SIZE, IN WEBKIT (owner
 * requests, 2026-10-08 and 2026-10-13).
 *
 * Every glyph (the pictures in images/glyphs/: the merge glyph's three bars,
 * the gates' exclamation mark, question mark and snowflake, and a working
 * one-shot's target) must sit centered on the same vertical line as the
 * filled status dots, draw at the dot's size, and hold one column width with
 * every other mark, the hidden `none` one included, so the names beside them
 * stay aligned. jsdom lays nothing out, so this draws rail rows as
 * `drawStatusMark` (src/sidebar/row.ts) shapes them, in real headless WebKit
 * over the real stylesheet, and measures them.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { readGlyphPicture } from "../glyph-pictures";
import { pageStylesheet } from "./stylesheets";

/** One row's mark: its arm, glyph name and the picture drawn (none for a disc). */
interface Mark {
  arm: string;
  glyph: string;
  image: string;
}

const DOT: Mark = { arm: "ready", glyph: "dot", image: "" };
const GLYPHS: readonly Mark[] = [
  { arm: "mergeQueued", glyph: "merge" },
  { arm: "permission", glyph: "exclaim" },
  { arm: "question", glyph: "ask" },
  { arm: "coldGate", glyph: "snowflake" },
  { arm: "thinking", glyph: "target" },
].map((m) => ({ ...m, image: readGlyphPicture(m.glyph) }));
const NONE: Mark = { arm: "none", glyph: "none", image: "" };

/** One rail row, as `drawStatusMark` and the row draw shape it. */
function row(m: Mark): string {
  const glyphClass = m.glyph === "dot" ? "" : " st-glyph";
  return `<div class="row"><span class="st tone-green${glyphClass}" data-glyph="${m.glyph}">${m.image}</span><span class="name">workspace</span></div>`;
}

/**
 * What one row measured: the mark's ink center and height (the disc's box, or
 * the picture's own box) and the name's left edge.
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
      const box = (st.querySelector("svg") ?? st).getBoundingClientRect();
      const name = (r.querySelector(".name") as HTMLElement).getBoundingClientRect();
      return { center: box.left + box.width / 2, height: box.height, nameLeft: name.left };
    });
  }, marks.map(row).join(""));
}

describe("the rail's status marks in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await pageStylesheet();
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
  // Every picture fills its file's full height, so its box is its ink and
  // draws exactly the dot's height.
  it.each(GLYPHS)("draws the $glyph glyph at the dot's size", async (glyph) => {
    // Arrange / Act
    const [dot, mark] = await measure(page, [DOT, glyph]);
    // Assert
    expect(mark?.height ?? NaN).toBeCloseTo(dot?.height ?? NaN, 1);
  });

  it.each([...GLYPHS, NONE])("starts the name beside a $glyph mark where it starts beside a dot", async (glyph) => {
    // Arrange / Act
    const [dot, mark] = await measure(page, [DOT, glyph]);
    // Assert
    expect(mark?.nameLeft).toBeCloseTo(dot?.nameLeft ?? NaN, 1);
  });
});
