/**
 * THE AGENTS PANEL'S FIGURE COLUMNS ARE SHARED BY EVERY ROW, IN WEBKIT (owner
 * request, 2026-10-01).
 *
 * The expanded footer's agents panel is one grid whose header and rows take its
 * columns through `subgrid`, so every row's tokens and duration share one width:
 * at least wide enough for the largest common value ("999.9k", a "5hr 30m 30s"-
 * sized duration), growing when a longer value is drawn and shrinking back to
 * that floor when it goes. jsdom lays nothing out, so this draws the panel as
 * `drawFooterExpandedAgents` shapes it (its cells are pinned in
 * test/footer/expanded.test.ts) in real headless WebKit over the real
 * stylesheet, and measures the cells.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

/** One agent row as `drawFooterAgentRow` shapes it. */
function row(tokens: string, clock: string): string {
  return `
    <div class="footer-row footer-row-agents footer-row-jump footer-columns" data-row="" data-state="running">
      <span class="footer-row-main"><span class="footer-glyph" data-glyph="agents">⚙</span><span class="footer-row-label">Explore</span><span class="footer-row-description">sweep the repo</span></span>
      <span class="footer-row-tokens">${tokens}</span>
      <span class="footer-row-clock">${clock}</span>
      <span class="footer-row-caret">▸</span>
    </div>`;
}

/** The panel's header as `drawFooterExpandedAgents` shapes it. */
const HEADER = `
  <div class="footer-panel-header footer-columns">
    <span class="footer-stop footer-stop-all"><button class="footer-stop-button" data-interrupt="">stop all</button></span>
    <span class="footer-column-header" data-column="tokens">tokens</span>
    <span class="footer-column-header" data-column="duration">duration</span>
    <span></span>
  </div>`;

/** A cell's left edge and width. */
interface Box {
  left: number;
  width: number;
}

/** Every row's (and the header's) tokens and duration cells, drawn from ROWS. */
interface Columns {
  tokens: Box[];
  duration: Box[];
  /** The width the floor values themselves take, in the cells' own font. */
  floors: { tokens: number; duration: number };
  /** The header's stop control's left edge, against the sheet's content edge. */
  stop: { left: number; contentLeft: number };
}

/** Draw the panel with ROWS and measure its columns. */
async function measure(page: Page, rows: ReadonlyArray<[string, string]>): Promise<Columns> {
  const html = HEADER + rows.map(([t, c]) => row(t, c)).join("");
  return page.evaluate((markup) => {
    const host = document.getElementById("host");
    if (host === null) throw new Error("the page's host did not mount");
    host.innerHTML = `<div class="pfooter-sheet footer-expanded list-rows" data-panel="agents" style="--pfooter-sheet-rows: 12">${markup}</div>`;
    const sheet = host.firstElementChild as HTMLElement;
    const boxes = (selector: string): Array<{ left: number; width: number }> =>
      [...sheet.querySelectorAll(selector)].map((el) => {
        const r = el.getBoundingClientRect();
        return { left: r.left, width: r.width };
      });
    /** How wide TEXT draws in the same cell font, with no padding or bar. */
    const textWidth = (cell: string, text: string): number => {
      const probe = document.createElement("span");
      probe.className = cell;
      probe.style.cssText = "position:absolute;visibility:hidden;padding:0;border:0;white-space:nowrap";
      probe.textContent = text;
      sheet.querySelector(".footer-row")?.append(probe);
      const width = probe.getBoundingClientRect().width;
      probe.remove();
      return width;
    };
    const stop = sheet.querySelector(".footer-stop")?.getBoundingClientRect().left ?? Number.NaN;
    const style = getComputedStyle(sheet);
    return {
      tokens: boxes(".footer-row-tokens, [data-column='tokens']"),
      duration: boxes(".footer-row-clock, [data-column='duration']"),
      floors: {
        tokens: textWidth("footer-row-tokens", "999.9k"),
        duration: textWidth("footer-row-clock", "5hr 30m 30s"),
      },
      stop: { left: stop, contentLeft: sheet.getBoundingClientRect().left + parseFloat(style.paddingLeft) },
    };
  }, html);
}

/** The distinct values of a list, rounded to the pixel. */
const distinct = (xs: number[]): number[] => [...new Set(xs.map((x) => Math.round(x)))];

describe("the agents panel's figure columns in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 700, height: 400 } });
    await page.setContent(`<!doctype html><html><head></head><body><div id="host" style="width: 640px"></div></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("gives every row's tokens cell the header's left edge and width", async () => {
    const c = await measure(page, [["1.2k", "5s"], ["12.4k", "4m 59s"], ["900", "1h 5m"]]);
    expect([distinct(c.tokens.map((b) => b.left)), distinct(c.tokens.map((b) => b.width))].map((d) => d.length)).toEqual([1, 1]);
  });

  it("gives every row's duration cell the header's left edge and width", async () => {
    const c = await measure(page, [["1.2k", "5s"], ["12.4k", "4m 59s"], ["900", "1h 5m"]]);
    expect([distinct(c.duration.map((b) => b.left)), distinct(c.duration.map((b) => b.width))].map((d) => d.length)).toEqual([1, 1]);
  });

  it("keeps the token count where it is when a clock goes from '4m 59s' to '5m'", async () => {
    const before = await measure(page, [["12.4k", "4m 59s"]]);
    const after = await measure(page, [["12.4k", "5m"]]);
    expect(Math.round(after.tokens[1].left)).toBe(Math.round(before.tokens[1].left));
  });

  it("makes the tokens column at least wide enough for '999.9k'", async () => {
    const c = await measure(page, [["1", "1s"]]);
    expect(c.tokens[1].width).toBeGreaterThanOrEqual(c.floors.tokens - 0.5);
  });

  it("makes the duration column at least wide enough for '5hr 30m 30s'", async () => {
    const c = await measure(page, [["1", "1s"]]);
    // The cell holds the bar and its padding too, so the text room is what is
    // left of its width after them.
    expect(c.duration[1].width - 11).toBeGreaterThanOrEqual(c.floors.duration - 0.5);
  });

  it("widens the tokens column for every row when one row draws a longer value", async () => {
    const floor = await measure(page, [["1.2k", "5s"], ["12.4k", "5s"]]);
    const grown = await measure(page, [["1.2k", "5s"], ["123456789.9k", "5s"]]);
    expect([
      grown.tokens[1].width > floor.tokens[1].width,
      distinct(grown.tokens.map((b) => b.width)).length,
    ]).toEqual([true, 1]);
  });

  it("shrinks the tokens column back to its floor when the longer value goes", async () => {
    const floor = await measure(page, [["1.2k", "5s"], ["12.4k", "5s"]]);
    await measure(page, [["1.2k", "5s"], ["123456789.9k", "5s"]]);
    const back = await measure(page, [["1.2k", "5s"], ["12.4k", "5s"]]);
    expect(Math.round(back.tokens[1].width)).toBe(Math.round(floor.tokens[1].width));
  });

  it("widens the duration column for every row when one row draws a longer clock", async () => {
    const floor = await measure(page, [["1.2k", "5s"], ["1.2k", "5s"]]);
    const grown = await measure(page, [["1.2k", "5s"], ["1.2k", "123h 59m 59s 999ms"]]);
    expect([
      grown.duration[1].width > floor.duration[1].width,
      distinct(grown.duration.map((b) => b.width)).length,
    ]).toEqual([true, 1]);
  });

  it("puts 'stop all' at the header's left edge", async () => {
    const c = await measure(page, [["1.2k", "5s"]]);
    expect(Math.round(c.stop.left)).toBe(Math.round(c.stop.contentLeft));
  });
});
