/**
 * THE RECENTLY MERGED BAND SHOWS WHAT FITS, IN WEBKIT (owner request,
 * 2026-10-08).
 *
 * The band shows as many merges as fit in the rail without scrolling, and its
 * folded count is that number. jsdom lays nothing out, so this draws a rail
 * in the shape the sidebar draws it, in real headless WebKit over the real
 * stylesheet, runs the REAL `fitMergedSection` (src/sidebar/merged-fit.ts,
 * bundled), and measures: the fitted rail does not scroll, one more row would
 * make it scroll, and a folded band counts the same number.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

/** One rail row, as the sidebar draws its line; a merged one states its merged instant. */
function wsRow(id: string, mergedAt: number | null): string {
  const merged = mergedAt !== null;
  return `<div class="ws${merged ? " gone" : ""}" data-roster-row="${id}"${merged ? ` data-merged-at="${mergedAt}"` : ""}><div class="row"><span class="st st-ready tone-green" data-glyph="dot"></span><span class="name${merged ? " merged-name" : ""}">${id}</span></div></div>`;
}

/**
 * A rail HEIGHT px tall: LIVE live rows above a band whose repository groups
 * hold GROUPS merges each, folded or not. The merges interleave in recency
 * across the groups, most recent first.
 */
function rail(height: number, live: number, groups: number[], folded: boolean): string {
  const liveRows = Array.from({ length: live }, (_, i) => wsRow(`live-${i}`, null)).join("");
  const total = groups.reduce((sum, n) => sum + n, 0);
  const groupBoxes = groups
    .map((n, g) => {
      const rows = Array.from({ length: n }, (_, i) => wsRow(`merged-${g}-${i}`, 1_000_000 - (i * groups.length + g))).join("");
      return `<div class="merged-group" data-repository="repo-${g}" data-repository-collapsed="false"><div class="merged-group-head"><span class="sb-label">repo-${g}</span></div><div class="rows">${rows}</div></div>`;
    })
    .join("");
  return `<nav id="ws-sidebar" style="height:${height}px"><div class="sb-scroll"><div class="sb-roster">
<div class="sb-pane" data-grouping="repository"><div class="sb-sections"><div class="repo"><div class="repo-head"><span class="sb-label">alpha</span></div><div class="rows">${liveRows}</div></div></div>
<div class="repo merged-section${folded ? " folded" : ""}"><div class="repo-head"><span class="sb-label">Recently Merged</span><span class="sb-count" data-section-count>(${total})</span></div><div class="rows">${groupBoxes}</div><div class="merged-row-probe" aria-hidden="true"></div><div class="merged-group-head-probe" aria-hidden="true"></div></div>
</div></div></div></nav>`;
}

/** What one fitted rail measured. */
interface Fitted {
  shown: number | null;
  count: string;
  scrolls: boolean;
  scrollsWithOneMore: boolean;
}

/** Draw a rail and fit its band with the real module. */
async function fit(page: Page, markup: string): Promise<Fitted> {
  return page.evaluate((html) => {
    document.body.innerHTML = html;
    const scroller = document.querySelector(".sb-scroll") as HTMLElement;
    const pane = document.querySelector(".sb-pane") as HTMLElement;
    const api = (window as unknown as { MergedFit: { fitMergedSection: (s: HTMLElement, p: HTMLElement) => number | null } }).MergedFit;
    const shown = api.fitMergedSection(scroller, pane);
    const scrolls = scroller.scrollHeight > scroller.clientHeight;
    // The next merge the fit left out is the most recent hidden one; showing
    // it may also show its group's header.
    const hiddenRows = [...document.querySelectorAll<HTMLElement>(".merged-section .merged-group > .rows > .ws")].filter(
      (row) => row.hidden || (row.closest(".merged-group") as HTMLElement).hidden,
    );
    hiddenRows.sort((a, b) => Number(b.getAttribute("data-merged-at")) - Number(a.getAttribute("data-merged-at")));
    const next = hiddenRows[0] ?? null;
    let scrollsWithOneMore = true;
    if (next !== null && !pane.querySelector(".merged-section")?.classList.contains("folded")) {
      const group = next.closest(".merged-group") as HTMLElement;
      const groupWasHidden = group.hidden;
      group.hidden = false;
      next.hidden = false;
      scrollsWithOneMore = scroller.scrollHeight > scroller.clientHeight;
      next.hidden = true;
      group.hidden = groupWasHidden;
    }
    return { shown, count: document.querySelector("[data-section-count]")?.textContent ?? "", scrolls, scrollsWithOneMore };
  }, markup);
}

describe("the Recently Merged fit in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    const script = await bundlePage(path.join(here, "merged-fit-page.ts"), "MergedFit");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 900 } });
    await page.setContent(`<!doctype html><html><head></head><body></body></html>`);
    await page.addStyleTag({ content: css });
    await page.addScriptTag({ content: script });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("shows some but not all of many merges in a short rail", async () => {
    const got = await fit(page, rail(400, 5, [40], false));
    expect([got.shown !== null && got.shown > 0, got.shown !== null && got.shown < 40]).toEqual([true, true]);
  });

  it("leaves the fitted rail without a scroll", async () => {
    const got = await fit(page, rail(400, 5, [40], false));
    expect(got.scrolls).toBe(false);
  });

  it("shows as many as fit: one more would make the rail scroll", async () => {
    const got = await fit(page, rail(400, 5, [40], false));
    expect(got.scrollsWithOneMore).toBe(true);
  });

  it("counts exactly the merges it shows", async () => {
    const got = await fit(page, rail(400, 5, [40], false));
    expect(got.count).toBe(`(${got.shown})`);
  });

  it("counts the same number while folded as it shows unfolded", async () => {
    const unfolded = await fit(page, rail(400, 5, [40], false));
    const folded = await fit(page, rail(400, 5, [40], true));
    expect(folded.count).toBe(unfolded.count);
  });

  it("shows every merge when the rail holds them all", async () => {
    const got = await fit(page, rail(800, 2, [12], false));
    expect([got.shown, got.count]).toEqual([12, "(12)"]);
  });

  it("leaves a rail of several repository groups without a scroll", async () => {
    const got = await fit(page, rail(400, 5, [15, 15, 15], false));
    expect(got.scrolls).toBe(false);
  });

  it("shows as many as fit across groups: one more would make the rail scroll", async () => {
    const got = await fit(page, rail(400, 5, [15, 15, 15], false));
    expect(got.scrollsWithOneMore).toBe(true);
  });

  it("counts the same number folded as unfolded across groups", async () => {
    const unfolded = await fit(page, rail(400, 5, [15, 15, 15], false));
    const folded = await fit(page, rail(400, 5, [15, 15, 15], true));
    expect(folded.count).toBe(unfolded.count);
  });
});
