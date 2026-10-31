/**
 * THE SECTION FIT, IN WEBKIT (owner requests, 2026-10-11, 2026-10-18 and
 * 2026-10-27).
 *
 * The rail never scrolls. Once it is out of room, the Open section lengthens
 * until Deploys and Hidden Workspaces below it are left three lines each, the
 * two sharing those six lines and Deploys claiming only its queues and
 * today's merges, any room the Open section cannot use goes to
 * Deploys' today's merges, then Hidden Workspaces, then older merges, the
 * lowest open section's viewport reaches the rail's foot, and each section
 * scrolls the rest. jsdom lays nothing out, so this draws a rail in
 * the shape the sidebar draws it, in real headless WebKit over the real
 * stylesheet, runs the REAL `fitBands` (src/sidebar/band-fit.ts, bundled), and
 * measures the viewports the engine draws.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";
import { pageStylesheet } from "./stylesheets";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A merge instant of today (local date) and one of a week ago, for a stamped row. */
const TODAY = "today";
const OLDER = "older";

/** One rail row, as the sidebar draws its line; a merged one stamped with MERGED's instant. */
function wsRow(id: string, merged: typeof TODAY | typeof OLDER | null): string {
  const stamp = merged === null ? "" : ` data-merged-at="${merged === TODAY ? Date.now() : Date.now() - 7 * 86_400_000}"`;
  return `<div class="ws${merged === null ? "" : " receded"}" data-roster-row="${id}"${stamp}><div class="row"><span class="st st-ready tone-green" data-glyph="dot"></span><span class="name${merged ? " band-name" : ""}">${id}</span></div></div>`;
}

/** One band's section around its rows region's INNER markup, drawn as roster.ts draws it. */
function bandSection(cls: string, label: string, count: number, inner: string): string {
  return `<div class="repo ${cls}"><div class="repo-head"><span class="sb-label">${label}</span><span class="sb-count" data-section-count>(${count})</span></div><div class="rows">${inner}</div></div>`;
}

/**
 * The Deploys band's idle repository groups: MERGED rows, the first TODAY of
 * them today's, dealt round-robin into GROUPS groups, each folded
 * (deploys-fold.ts) unless OPENED by hand.
 */
function mergedGroups(merged: number, today: number, groups: number, opened: boolean): string {
  return Array.from({ length: groups }, (_, g) => {
    const rows = Array.from({ length: merged }, (_, i) => i)
      .filter((i) => i % groups === g)
      .map((i) => wsRow(`merged-${i}`, i < today ? TODAY : OLDER))
      .join("");
    return `<div class="merged-group${opened ? "" : " folded"}" data-repository="repo-${g}"><div class="merged-group-head"><span class="sb-label">repo-${g}</span><span class="sb-count sb-today-count">(${today} today, last 1m ago)</span></div><div class="rows">${rows}</div></div>`;
  }).join("");
}

/** What a rail is drawn with: its rows per section, how many merges are today's, the merged groups, and the Open section's repositories. */
interface Drawing {
  live: number;
  hidden: number;
  merged: number;
  today?: number;
  groups?: number;
  repos?: number;
  /** Whether the idle Deploys groups were opened by hand; folded, as they draw, unless said. */
  expanded?: boolean;
}

/** A rail HEIGHT px tall, under the rail's head, drawn as DRAWING says. */
function rail(height: number, { live, hidden, merged, today = merged, groups = 1, repos = 1, expanded = false }: Drawing): string {
  const ids = (prefix: string, n: number): string[] => Array.from({ length: n }, (_, i) => `${prefix}-${i}`);
  const repoSections = Array.from({ length: repos }, (_, r) => {
    const rows = ids("live", live).filter((_, i) => Math.floor((i * repos) / live) === r);
    return `<div class="repo repo-section"><div class="repo-head"><span class="sb-label">repo-${r}</span></div><div class="rows">${rows.map((id) => wsRow(id, null)).join("")}</div></div>`;
  }).join("");
  return `<nav id="ws-sidebar" style="height:${height}px"><div class="sb-head"><span class="sb-title">Workspaces</span></div><div class="sb-scroll"><div class="sb-roster">
<div class="sb-pane"><div class="repo open-section"><div class="repo-head"><span class="sb-label">Open</span></div><div class="sb-sections">${repoSections}</div></div>
${bandSection("merged-section", "Deploys", merged, merged === 0 ? "" : mergedGroups(merged, today, groups, expanded))}
${bandSection("hidden-section", "Hidden Workspaces", hidden, ids("hidden", hidden).map((id) => wsRow(id, null)).join(""))}
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
  open: Viewport;
  hidden: Viewport;
  merged: Viewport;
  railScrolls: boolean;
  /** The rail's room left under the pane, in px. */
  slack: number;
  /** Deploys' viewport's computed mask, as fitted and once scrolled to its end. */
  mergedMask: { fitted: string; atEnd: string };
}

/** Draw a rail, fit its bands with the real module, and measure. */
async function fit(page: Page, markup: string): Promise<Fitted> {
  return page.evaluate((html) => {
    document.body.innerHTML = html;
    const scroller = document.querySelector(".sb-scroll") as HTMLElement;
    const pane = document.querySelector(".sb-pane") as HTMLElement;
    const api = (
      window as unknown as {
        BandFit: { fitBands: (s: HTMLElement, p: HTMLElement, nowMs: number) => unknown; paintBandMore: (region: HTMLElement) => void };
      }
    ).BandFit;
    api.fitBands(scroller, pane, Date.now());
    const measure = (region: HTMLElement): Viewport => {
      const box = region.getBoundingClientRect();
      let whole = 0;
      let cut = 0;
      for (const line of region.querySelectorAll(".row, .merged-group.folded > .merged-group-head")) {
        if (line.getBoundingClientRect().height === 0) continue;
        const r = line.getBoundingClientRect();
        if (r.top >= box.top - 0.5 && r.bottom <= box.bottom + 0.5) whole += 1;
        else if (r.top < box.bottom - 0.5 && r.bottom > box.top + 0.5) cut += 1;
      }
      let danglingHead = false;
      for (const head of region.querySelectorAll(".merged-group-head, .repo-section > .repo-head")) {
        const h = head.getBoundingClientRect();
        const next = head.parentElement?.querySelector(".row")?.getBoundingClientRect();
        if (h.top >= box.top - 0.5 && h.top < box.bottom - 0.5 && next !== undefined && next.bottom > box.bottom + 0.5) danglingHead = true;
      }
      return { whole, cut, danglingHead, scrolls: region.scrollHeight > region.clientHeight };
    };
    const region = (selector: string): HTMLElement => document.querySelector(selector) as HTMLElement;
    return {
      open: measure(region(".open-section > .sb-sections")),
      hidden: measure(region(".hidden-section > .rows")),
      merged: measure(region(".merged-section > .rows")),
      railScrolls: scroller.scrollHeight > scroller.clientHeight,
      slack:
        scroller.getBoundingClientRect().bottom -
        Number.parseFloat(getComputedStyle(scroller).paddingBottom) -
        Number.parseFloat(getComputedStyle(scroller).borderBottomWidth) -
        pane.getBoundingClientRect().bottom,
      mergedMask: (() => {
        const rows = region(".merged-section > .rows");
        const fitted = getComputedStyle(rows).maskImage;
        rows.scrollTop = rows.scrollHeight;
        api.paintBandMore(rows);
        return { fitted, atEnd: getComputedStyle(rows).maskImage };
      })(),
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
async function unsettled(page: Page, heights: number[], drawing: Drawing): Promise<Refit[]> {
  const markups = heights.map((h) => [h, rail(h, drawing)] as const);
  return page.evaluate((cases) => {
    const api = (window as unknown as { BandFit: { fitBands: (s: HTMLElement, p: HTMLElement, nowMs: number) => unknown } }).BandFit;
    const out: Refit[] = [];
    for (const [height, html] of cases) {
      document.body.innerHTML = html;
      const scroller = document.querySelector(".sb-scroll") as HTMLElement;
      const pane = document.querySelector(".sb-pane") as HTMLElement;
      try {
        const first = JSON.stringify(api.fitBands(scroller, pane, Date.now()));
        const second = JSON.stringify(api.fitBands(scroller, pane, Date.now()));
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
    const css = await pageStylesheet();
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

  it("shares six lines in a rail the Open section fills: Deploys' one folded group, Hidden Workspaces five", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20 }));
    expect([got.hidden.whole, got.merged.whole]).toEqual([5, 1]);
  });

  it("keeps an open Deploys group's five rows under its header, taking Hidden Workspaces' lines", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20, expanded: true }));
    expect([got.hidden.whole, got.merged.whole, got.open.whole > 0]).toEqual([1, 5, true]);
  });

  it("scrolls the Open section in a rail it fills", async () => {
    const got = await fit(page, rail(800, { live: 40, hidden: 20, merged: 20 }));
    expect([got.open.whole > 4, got.open.scrolls]).toEqual([true, true]);
  });

  it("never scrolls the rail itself", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20 }));
    expect(got.railScrolls).toBe(false);
  });

  it("cuts no line of the Open section or Deploys through in a rail out of room", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20 }));
    expect([got.open.cut, got.merged.cut]).toEqual([0, 0]);
  });

  it("cuts at most Hidden Workspaces' last line through, at the rail's foot", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20 }));
    expect(got.hidden.cut).toBeLessThanOrEqual(1);
  });

  it("gives Hidden Workspaces all six lines beside an empty Deploys", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 0 }));
    expect([got.hidden.whole, got.merged.whole]).toEqual([6, 0]);
  });

  it("gives Hidden Workspaces five lines beside the folded group of one merge today", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 1 }));
    expect([got.hidden.whole, got.merged.whole]).toEqual([5, 1]);
  });

  it("shows no older merge while the Open section scrolls", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20, today: 0 }));
    expect([got.hidden.whole, got.merged.whole]).toEqual([5, 1]);
  });

  it("shrinks Hidden Workspaces before Deploys, at every short rail height", async () => {
    // A line the rail still has room for is never left unused, so a Deploys
    // line too tall for the room can leave a shorter Hidden line shown; but
    // Hidden Workspaces never keeps its share while Deploys loses a line of
    // its own, and some rail keeps Deploys whole while Hidden shrinks.
    const heights = Array.from({ length: 41 }, (_, i) => 150 + i * 5);
    const fitted = [];
    for (const height of heights) fitted.push(await fit(page, rail(height, { live: 40, hidden: 20, merged: 20, expanded: true })));
    expect([
      fitted.some((got) => got.merged.whole < 3 && got.hidden.whole >= 3),
      fitted.some((got) => got.merged.whole === 3 && got.hidden.whole < 3),
    ]).toEqual([false, true]);
  });

  it("shows open Deploys groups' five rows across several repositories", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20, groups: 3, expanded: true }));
    expect(got.merged.whole).toBe(5);
  });

  it("never leaves a group's header dangling at a viewport's foot", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20, groups: 6, expanded: true }));
    expect([got.open.danglingHead, got.merged.danglingHead]).toEqual([false, false]);
  });

  it("scrolls both bands in a rail out of room", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20, expanded: true }));
    expect([got.hidden.scrolls, got.merged.scrolls]).toEqual([true, true]);
  });

  it("shows every line of every section, unscrolled, in a rail that holds them", async () => {
    const got = await fit(page, rail(880, { live: 2, hidden: 4, merged: 5, expanded: true }));
    expect([got.open.whole, got.open.scrolls, got.hidden.whole, got.hidden.scrolls, got.merged.whole, got.merged.scrolls]).toEqual([
      2,
      false,
      4,
      false,
      5,
      false,
    ]);
  });

  it("keeps the rail's head its own height when the rows overflow", async () => {
    const short = await railBoxes(page, rail(400, { live: 1, hidden: 1, merged: 1 }));
    const overflowing = await railBoxes(page, rail(400, { live: 40, hidden: 20, merged: 20 }));
    expect(overflowing.head).toBe(short.head);
  });

  it("gives the rail's scroller the same height however many rows it holds", async () => {
    const short = await railBoxes(page, rail(400, { live: 1, hidden: 1, merged: 1 }));
    const overflowing = await railBoxes(page, rail(400, { live: 40, hidden: 20, merged: 20 }));
    expect(overflowing.scroller).toBe(short.scroller);
  });

  it("draws the same lines when a rail of any height is fitted again", async () => {
    // Every pixel of rail height crosses every line boundary of every
    // section, from a rail too short for the bands' share to one holding all.
    const heights = Array.from({ length: 701 }, (_, i) => 200 + i);
    expect(await unsettled(page, heights, { live: 12, hidden: 9, merged: 9, today: 4 })).toEqual([]);
    // MEASURED: 1,402 fits in one page pass run 1.0-1.2s healthy since each
    // fit also reads the open Deploys groups' floor (2026-10-27), at and over the
    // suite's 850ms. Its own bound is just over twice that.
  }, 2_500);

  it("grows an open Deploys group's merges of today, not Hidden Workspaces, into the room a whole Open section leaves", async () => {
    const got = await fit(page, rail(880, { live: 2, hidden: 40, merged: 40, expanded: true }));
    // Hidden Workspaces, the lowest section, keeps only the line at the
    // rail's foot that no further merged row fits in.
    expect([got.merged.whole > 6, got.hidden.whole, got.railScrolls]).toEqual([true, 1, false]);
  });

  it("grows Hidden Workspaces, not older merges, into the room a whole Open section leaves", async () => {
    const got = await fit(page, rail(880, { live: 2, hidden: 40, merged: 40, today: 0, expanded: true }));
    expect([got.hidden.whole > 5, got.merged.whole, got.railScrolls]).toEqual([true, 5, false]);
  });

  it("grows older merges into the room a whole Open section and Hidden Workspaces leave", async () => {
    const got = await fit(page, rail(880, { live: 2, hidden: 4, merged: 40, today: 0, expanded: true }));
    expect([got.hidden.whole, got.merged.whole > 3, got.railScrolls]).toEqual([4, true, false]);
  });

  it("reaches the rail's foot at every rail height while a band scrolls", async () => {
    const heights = Array.from({ length: 31 }, (_, i) => 400 + i * 7);
    const short: number[] = [];
    for (const height of heights) {
      const got = await fit(page, rail(height, { live: 12, repos: 4, hidden: 20, merged: 20, today: 0 }));
      if (Math.abs(got.slack) > 0.5) short.push(height);
    }
    expect(short).toEqual([]);
  });

  it("fades the foot of a capped Deploys viewport while rows lie below it", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20, expanded: true }));
    expect(got.mergedMask.fitted).toContain("linear-gradient");
  });

  it("drops the fade once Deploys is scrolled to its end", async () => {
    const got = await fit(page, rail(500, { live: 40, hidden: 20, merged: 20, expanded: true }));
    expect(got.mergedMask.atEnd).toBe("none");
  });

  it("reaches the rail's foot when every line fits", async () => {
    const got = await fit(page, rail(880, { live: 2, hidden: 2, merged: 2 }));
    expect(Math.abs(got.slack)).toBeLessThanOrEqual(0.5);
  });
});
