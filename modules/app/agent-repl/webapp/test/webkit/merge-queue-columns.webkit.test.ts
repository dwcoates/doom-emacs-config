/**
 * THE MERGE QUEUE TAB IS A COLUMN TABLE, IN WEBKIT (owner request,
 * 2026-10-01).
 *
 * The queue tab is one grid whose header and rows take its columns through
 * `subgrid` -- the expanded footer's agents panel's shared column table
 * (test/webkit/footer-columns.webkit.test.ts) -- so every row's duration
 * shares one width: at least wide enough for a "5hr 30m 30s"-sized value,
 * growing when a longer one is drawn, and holding still while a value changes
 * width. Every row is the same height with its name vertically centered.
 * jsdom lays nothing out, so this draws the table as `drawFeedMergeQueue`
 * shapes it (its cells are pinned in test/feed/merge/queue.test.ts) in real
 * headless WebKit over the real stylesheet, and measures the cells.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

/** One queue row as `drawFeedMergeQueueEntry` shapes it. */
interface Row {
  place: "ahead" | "current" | "behind";
  name: string;
  /** The front's active tab label, or undefined for a waiting entry. */
  tab?: string;
  clock: string;
}

/** One entry's markup. */
function entry(r: Row): string {
  const status = r.tab === undefined ? "waiting" : "merging";
  const stage = r.tab === undefined ? "waiting" : `<span class="merge-tab-label">${r.tab}</span>`;
  const moves =
    r.tab === undefined
      ? `<ar-button role="button" class="merge-queue-move" data-merge-move="up" aria-label="move up the merge queue"></ar-button>` +
        `<ar-button role="button" class="merge-queue-move" data-merge-move="down" aria-label="move down the merge queue"></ar-button>`
      : "";
  return `
    <div class="merge-queue-entry footer-columns" data-queue-place="${r.place}" data-queue-status="${status}">
      <ar-button role="button" class="merge-queue-line footer-columns" data-select="">
        <span class="merge-queue-workspace">
          <span class="merge-queue-moves">${moves}</span>
          <span class="merge-queue-label">${r.name}</span>
        </span>
        <span class="merge-queue-stage">${stage}</span>
        <span class="footer-row-clock merge-queue-duration">${r.clock}</span>
      </ar-button>
    </div>`;
}

/** The header as `drawFeedMergeQueue` shapes it. */
const HEADER = `
  <div class="merge-queue-header footer-columns">
    <span class="footer-column-header" data-column="workspace">workspace</span>
    <span class="footer-column-header" data-column="stage">stage</span>
    <span class="footer-column-header" data-column="duration">duration</span>
  </div>`;

/** A cell's box. */
interface Box {
  left: number;
  width: number;
  top: number;
  height: number;
}

/** What one draw measured. */
interface Measured {
  /** The header's and every row's duration cell. */
  duration: Box[];
  /** The header's and every row's stage cell. */
  stage: Box[];
  /** Every entry row. */
  rows: Box[];
  /** Every row's name, as drawn. */
  names: Box[];
  /** Every move chevron, in row order, up before down. */
  moves: Box[];
  /** The width "5hr 30m 30s" takes in the duration cell's own font. */
  floor: number;
  /** Every entry row's stage cell's size, font and color. */
  stageLooks: string[];
  /** Every entry row's duration cell's size, font and color. */
  durationLooks: string[];
}

/** Draw the table with ROWS and measure it. */
async function measure(page: Page, rows: readonly Row[]): Promise<Measured> {
  const html = HEADER + rows.map(entry).join("");
  return page.evaluate((markup) => {
    const host = document.getElementById("host");
    if (host === null) throw new Error("the page's host did not mount");
    host.innerHTML = `<div class="merge-queue list-rows">${markup}</div>`;
    const table = host.firstElementChild as HTMLElement;
    const boxes = (selector: string): Array<{ left: number; width: number; top: number; height: number }> =>
      [...table.querySelectorAll(selector)].map((el) => {
        const r = el.getBoundingClientRect();
        return { left: r.left, width: r.width, top: r.top, height: r.height };
      });
    const probe = document.createElement("span");
    probe.className = "footer-row-clock";
    probe.style.cssText = "position:absolute;visibility:hidden;padding:0;border:0;white-space:nowrap";
    probe.textContent = "5hr 30m 30s";
    table.querySelector(".merge-queue-line")?.append(probe);
    const floor = probe.getBoundingClientRect().width;
    probe.remove();
    const looks = (selector: string): string[] =>
      [...table.querySelectorAll(selector)].map((el) => {
        const c = getComputedStyle(el);
        return [c.fontSize, c.fontFamily, c.fontWeight, c.fontStyle, c.fontVariantNumeric, c.letterSpacing, c.color].join(" / ");
      });
    return {
      duration: boxes(".merge-queue-duration, [data-column='duration']"),
      stage: boxes(".merge-queue-stage, [data-column='stage']"),
      rows: boxes(".merge-queue-entry"),
      names: boxes(".merge-queue-label"),
      moves: boxes(".merge-queue-move"),
      floor,
      stageLooks: looks(".merge-queue-stage"),
      durationLooks: looks(".merge-queue-duration"),
    };
  }, html);
}

/** The distinct values of a list, rounded to the pixel. */
const distinct = (xs: number[]): number[] => [...new Set(xs.map((x) => Math.round(x)))];

/** A front, this workspace, and one behind. */
const QUEUE: readonly Row[] = [
  { place: "ahead", name: "front-workspace", tab: "tests (2)", clock: "4m 59s" },
  { place: "current", name: "mine", clock: "1h 5m" },
  { place: "behind", name: "after", clock: "5s" },
];

describe("the merge queue tab's columns in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 700, height: 400 } });
    await page.setContent(`<!doctype html><html><head></head><body><div id="host" style="width: 520px"></div></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("gives every row's duration cell the header's left edge and width", async () => {
    const m = await measure(page, QUEUE);
    expect([distinct(m.duration.map((b) => b.left)), distinct(m.duration.map((b) => b.width))].map((d) => d.length)).toEqual([1, 1]);
  });

  it("gives every row's stage cell the header's left edge", async () => {
    const m = await measure(page, QUEUE);
    expect(distinct(m.stage.map((b) => b.left)).length).toBe(1);
  });

  it("makes the duration column at least wide enough for '5hr 30m 30s'", async () => {
    const m = await measure(page, [{ place: "current", name: "mine", clock: "1s" }]);
    // The cell holds the bar and its padding too, so the text room is what is
    // left of its width after them.
    expect(m.duration[1].width - 11).toBeGreaterThanOrEqual(m.floor - 0.5);
  });

  it("holds the duration column still when a value goes from '4m 59s' to '5m'", async () => {
    const before = await measure(page, QUEUE);
    const after = await measure(page, [{ ...QUEUE[0], clock: "5m" }, QUEUE[1], QUEUE[2]]);
    expect([Math.round(after.duration[1].left), Math.round(after.duration[1].width), Math.round(after.stage[1].left)]).toEqual([
      Math.round(before.duration[1].left),
      Math.round(before.duration[1].width),
      Math.round(before.stage[1].left),
    ]);
  });

  it("widens the duration column for every row when one row draws a longer value", async () => {
    const floor = await measure(page, QUEUE);
    const grown = await measure(page, [{ ...QUEUE[0], clock: "123h 59m 59s 999ms" }, QUEUE[1], QUEUE[2]]);
    expect([
      grown.duration[1].width > floor.duration[1].width,
      distinct(grown.duration.map((b) => b.width)).length,
    ]).toEqual([true, 1]);
  });

  it("shrinks the duration column back to its floor when the longer value goes", async () => {
    const floor = await measure(page, QUEUE);
    await measure(page, [{ ...QUEUE[0], clock: "123h 59m 59s 999ms" }, QUEUE[1], QUEUE[2]]);
    const back = await measure(page, QUEUE);
    expect(Math.round(back.duration[1].width)).toBe(Math.round(floor.duration[1].width));
  });

  it("draws every row -- merging, current and waiting -- the same height", async () => {
    const m = await measure(page, QUEUE);
    expect(distinct(m.rows.map((b) => b.height)).length).toBe(1);
  });

  it("centers every row's name vertically in its row", async () => {
    const m = await measure(page, QUEUE);
    const offsets = m.rows.map((row, i) => Math.abs(row.top + row.height / 2 - (m.names[i].top + m.names[i].height / 2)));
    expect(offsets.every((o) => o <= 1)).toBe(true);
  });

  it("starts every row's name at one left edge, the merge running's beside an empty chevron slot", async () => {
    const m = await measure(page, QUEUE);
    expect(distinct(m.names.map((b) => b.left)).length).toBe(1);
  });

  it("draws a waiting row's chevrons left of its name, up before down", async () => {
    const m = await measure(page, QUEUE);
    const [up, down] = m.moves;
    expect([up.left < down.left, down.left + down.width <= m.names[1].left]).toEqual([true, true]);
  });

  it("keeps the chevrons inside their row's height", async () => {
    const m = await measure(page, QUEUE);
    const row = m.rows[1];
    expect(m.moves.slice(0, 2).every((b) => b.top >= row.top && b.top + b.height <= row.top + row.height)).toBe(true);
  });

  it("draws every row's duration in its stage's size, font and color", async () => {
    const m = await measure(page, QUEUE);
    expect(m.durationLooks).toEqual(m.stageLooks);
  });

  it("keeps a long name to its row's height, ending it rather than wrapping", async () => {
    const m = await measure(page, [
      { place: "ahead", name: "a-very-long-workspace-name-".repeat(6), tab: "tests", clock: "5s" },
      QUEUE[1],
    ]);
    expect(distinct(m.rows.map((b) => b.height)).length).toBe(1);
  });
});
