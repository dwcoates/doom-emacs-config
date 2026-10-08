/**
 * THE RAIL'S STATUS MARKS SHARE ONE CENTER, IN WEBKIT (owner request,
 * 2026-10-08).
 *
 * A merge glyph (≡ queued, ⟳ merging, ✕ failed) must sit centered on the
 * same vertical line as the filled status dots (ready, working, …), and every
 * mark, the hidden `none` one included, must hold one column width so the
 * names beside them stay aligned. jsdom lays nothing out, so this draws rail
 * rows as `drawStatusMark` (src/sidebar/row.ts) shapes them, in real headless
 * WebKit over the real stylesheet, and measures them.
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
  { arm: "mergeQueued", glyph: "queue", char: "≡" },
  { arm: "merging", glyph: "recycle", char: "⟳" },
  { arm: "mergeFailed", glyph: "failed", char: "✕" },
];
const NONE: Mark = { arm: "none", glyph: "none", char: "" };
const MERGED: Mark = { arm: "merged", glyph: "check", char: "" };

/** One rail row, as `drawStatusMark` and the row draw shape it. */
function row(m: Mark): string {
  const glyphClass = m.glyph === "dot" ? "" : " st-glyph";
  return `<div class="row"><span class="st st-${m.arm} tone-green${glyphClass}" data-glyph="${m.glyph}">${m.char}</span><span class="name">workspace</span></div>`;
}

/** What one row measured: the mark's ink center (the disc, or the character's own box) and the name's left edge. */
interface Measured {
  center: number;
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
      return { center: box.left + box.width / 2, nameLeft: name.left };
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

  it.each([...GLYPHS, NONE, MERGED])("starts the name beside a $glyph mark where it starts beside a dot", async (glyph) => {
    // Arrange / Act
    const [dot, mark] = await measure(page, [DOT, glyph]);
    // Assert
    expect(mark?.nameLeft).toBeCloseTo(dot?.nameLeft ?? NaN, 1);
  });
});
