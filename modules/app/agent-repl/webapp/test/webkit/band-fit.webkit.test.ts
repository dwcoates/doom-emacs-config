/**
 * THE BAND FIT, IN WEBKIT (owner request, 2026-10-11).
 *
 * Once the rail is out of room, Hidden Workspaces and Recently Merged are
 * scrolling viewports of at most six lines each, the one line short of both
 * going to Hidden Workspaces. jsdom lays nothing out, so this draws a rail in
 * the shape the sidebar draws it, in real headless WebKit over the real
 * stylesheet, runs the REAL `fitBands` (src/sidebar/band-fit.ts, bundled), and
 * measures the viewports the engine draws.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

/** One rail row, as the sidebar draws its line. */
function wsRow(id: string, merged: boolean): string {
  return `<div class="ws${merged ? " receded" : ""}" data-roster-row="${id}"><div class="row"><span class="st st-ready tone-green" data-glyph="dot"></span><span class="name${merged ? " band-name" : ""}">${id}</span></div></div>`;
}

/** One band's section around its rows region's INNER markup, drawn as roster.ts draws it. */
function bandSection(cls: string, label: string, count: number, inner: string): string {
  return `<div class="repo ${cls}"><div class="repo-head"><span class="sb-label">${label}</span><span class="sb-count" data-section-count>(${count})</span></div><div class="rows">${inner}</div></div>`;
}

/** The Recently Merged band's rows region: MERGED rows dealt round-robin into GROUPS repository groups. */
function mergedGroups(merged: number, groups: number): string {
  return Array.from({ length: groups }, (_, g) => {
    const rows = Array.from({ length: merged }, (_, i) => i)
      .filter((i) => i % groups === g)
      .map((i) => wsRow(`merged-${i}`, true))
      .join("");
    return `<div class="merged-group" data-repository="repo-${g}" data-repository-collapsed="false"><div class="merged-group-head"><span class="sb-label">repo-${g}</span></div><div class="rows">${rows}</div></div>`;
  }).join("");
}

/** A rail HEIGHT px tall, under the rail's head: LIVE live rows above HIDDEN hidden and MERGED merged rows in GROUPS repository groups. */
function rail(height: number, live: number, hidden: number, merged: number, groups = 1): string {
  const ids = (prefix: string, n: number): string[] => Array.from({ length: n }, (_, i) => `${prefix}-${i}`);
  return `<nav id="ws-sidebar" style="height:${height}px"><div class="sb-head"><span class="sb-title">Workspaces</span></div><div class="sb-scroll"><div class="sb-roster">
<div class="sb-pane"><div class="sb-sections"><div class="repo"><div class="repo-head"><span class="sb-label">alpha</span></div><div class="rows">${ids("live", live).map((id) => wsRow(id, false)).join("")}</div></div></div>
${bandSection("hidden-section", "Hidden Workspaces", hidden, ids("hidden", hidden).map((id) => wsRow(id, false)).join(""))}
${bandSection("merged-section", "Recently Merged", merged, mergedGroups(merged, groups))}
</div></div></div></nav>`;
}

/** What one band's fitted viewport measured. */
interface Viewport {
  /** Lines wholly inside the viewport. */
  whole: number;
  /** Lines the viewport cuts through. */
  cut: number;
  /** Whether a repository group's header is shown without the line under it. */
  danglingHead: boolean;
  scrolls: boolean;
}

/** What one fitted rail measured. */
interface Fitted {
  hidden: Viewport;
  merged: Viewport;
  railScrolls: boolean;
}

/** Draw a rail, fit its bands with the real module, and measure. */
async function fit(page: Page, markup: string): Promise<Fitted> {
  return page.evaluate((html) => {
    document.body.innerHTML = html;
    const scroller = document.querySelector(".sb-scroll") as HTMLElement;
    const pane = document.querySelector(".sb-pane") as HTMLElement;
    const api = (window as unknown as { BandFit: { fitBands: (s: HTMLElement, p: HTMLElement) => unknown } }).BandFit;
    api.fitBands(scroller, pane);
    const measure = (cls: string): Viewport => {
      const region = document.querySelector(`.${cls} > .rows`) as HTMLElement;
      const box = region.getBoundingClientRect();
      let whole = 0;
      let cut = 0;
      for (const line of region.querySelectorAll(".row")) {
        const r = line.getBoundingClientRect();
        if (r.top >= box.top - 0.5 && r.bottom <= box.bottom + 0.5) whole += 1;
        else if (r.top < box.bottom - 0.5 && r.bottom > box.top + 0.5) cut += 1;
      }
      let danglingHead = false;
      for (const head of region.querySelectorAll(".merged-group-head")) {
        const h = head.getBoundingClientRect();
        const next = head.parentElement?.querySelector(".row")?.getBoundingClientRect();
        if (h.top >= box.top - 0.5 && h.top < box.bottom - 0.5 && next !== undefined && next.bottom > box.bottom + 0.5) danglingHead = true;
      }
      return { whole, cut, danglingHead, scrolls: region.scrollHeight > region.clientHeight };
    };
    return {
      hidden: measure("hidden-section"),
      merged: measure("merged-section"),
      railScrolls: scroller.scrollHeight > scroller.clientHeight,
    };
  }, markup);
}

/** The lines each band shows after a first fit and after a second, and whether either fit refused. */
interface Refit {
  height: number;
  first: string;
  second: string;
  refused: string | null;
}

/**
 * Fit the bands of a rail of every height in HEIGHTS, twice each, and report
 * every height whose second fit drew other lines than its first or refused.
 */
async function unsettled(page: Page, heights: number[], live: number, hidden: number, merged: number): Promise<Refit[]> {
  const markups = heights.map((h) => [h, rail(h, live, hidden, merged)] as const);
  return page.evaluate((cases) => {
    const api = (window as unknown as { BandFit: { fitBands: (s: HTMLElement, p: HTMLElement) => unknown } }).BandFit;
    const out: Refit[] = [];
    for (const [height, html] of cases) {
      document.body.innerHTML = html;
      const scroller = document.querySelector(".sb-scroll") as HTMLElement;
      const pane = document.querySelector(".sb-pane") as HTMLElement;
      try {
        const first = JSON.stringify(api.fitBands(scroller, pane));
        const second = JSON.stringify(api.fitBands(scroller, pane));
        if (first !== second) out.push({ height, first, second, refused: null });
      } catch (err) {
        out.push({ height, first: "", second: "", refused: String(err) });
      }
    }
    return out;
  }, markups);
}

/** The drawn heights of the rail's head and scroller with its rows drawn as MARKUP. */
async function railBoxes(page: Page, markup: string): Promise<{ head: number; scroller: number }> {
  return page.evaluate((html) => {
    document.body.innerHTML = html;
    return {
      head: (document.querySelector(".sb-head") as HTMLElement).getBoundingClientRect().height,
      scroller: (document.querySelector(".sb-scroll") as HTMLElement).getBoundingClientRect().height,
    };
  }, markup);
}

describe("the band fit in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    const script = await bundlePage(path.join(here, "band-fit-page.ts"), "BandFit");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 900 } });
    await page.setContent(`<!doctype html><html><head></head><body></body></html>`);
    await page.addStyleTag({ content: css });
    await page.addScriptTag({ content: script });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("shows six whole lines of each band in a rail out of room", async () => {
    const got = await fit(page, rail(300, 30, 20, 20));
    expect([got.hidden.whole, got.merged.whole]).toEqual([6, 6]);
  });

  it("cuts no line through in a rail out of room", async () => {
    const got = await fit(page, rail(300, 30, 20, 20));
    expect([got.hidden.cut, got.merged.cut]).toEqual([0, 0]);
  });

  it("shows six whole merged lines across several repository groups", async () => {
    const got = await fit(page, rail(300, 30, 20, 20, 3));
    expect([got.merged.whole, got.merged.cut]).toEqual([6, 0]);
  });

  it("never leaves a repository group's header dangling at the viewport's foot", async () => {
    const got = await fit(page, rail(300, 30, 20, 20, 6));
    expect(got.merged.danglingHead).toBe(false);
  });

  it("scrolls both bands in a rail out of room", async () => {
    const got = await fit(page, rail(300, 30, 20, 20));
    expect([got.hidden.scrolls, got.merged.scrolls]).toEqual([true, true]);
  });

  it("shows every line of both bands, unscrolled, in a rail that holds them", async () => {
    const got = await fit(page, rail(880, 2, 4, 5));
    expect([got.hidden.whole, got.hidden.scrolls, got.merged.whole, got.merged.scrolls]).toEqual([4, false, 5, false]);
  });

  it("keeps the rail's head its own height when the rows overflow", async () => {
    const short = await railBoxes(page, rail(400, 1, 1, 1));
    const overflowing = await railBoxes(page, rail(400, 40, 20, 20));
    expect(overflowing.head).toBe(short.head);
  });

  it("gives the rail's scroller the same height however many rows it holds", async () => {
    const short = await railBoxes(page, rail(400, 1, 1, 1));
    const overflowing = await railBoxes(page, rail(400, 40, 20, 20));
    expect(overflowing.scroller).toBe(short.scroller);
  });

  it("draws the same lines when a rail of any height is fitted again", async () => {
    // Every pixel of rail height crosses every line boundary of both bands,
    // the rail overflowing below the capped viewports and fitting above them.
    const heights = Array.from({ length: 601 }, (_, i) => 200 + i);
    expect(await unsettled(page, heights, 2, 9, 9)).toEqual([]);
  });

  it("grows the bands into a tall rail's spare room without scrolling it", async () => {
    const got = await fit(page, rail(880, 2, 40, 40));
    expect([got.hidden.whole > 6, got.merged.whole > 6, got.railScrolls]).toEqual([true, true, false]);
  });
});
