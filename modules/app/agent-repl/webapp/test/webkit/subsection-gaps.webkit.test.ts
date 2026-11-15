/**
 * SUBSECTIONS WITH NOTHING UNDER THEM STAND CLOSE TOGETHER, IN WEBKIT (owner
 * request, 2026-11-13).
 *
 * A subsection that is folded or empty shows its header alone, so the gap
 * between its header and a neighbouring subsection's header is considerably
 * smaller than the gaps a workspace row keeps. Every gap that meets a
 * workspace row, and a populated open subsection's header over its rows,
 * keeps what it was. jsdom lays nothing out, so this draws a repository's
 * rows and subsections in the shape roster.ts and subsections.ts draw them,
 * in real headless WebKit over the real stylesheet, and measures the space
 * between each visible line's text and the next's.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { pageStylesheet } from "./stylesheets";

/** One entry of a repository's list: a workspace row, or a subsection and what it shows. */
type Entry = { kind: "row" } | { kind: "subsection"; shows: "empty" | "folded" | { rows: number } };

const ROW: Entry = { kind: "row" };
const EMPTY: Entry = { kind: "subsection", shows: "empty" };
const FOLDED: Entry = { kind: "subsection", shows: "folded" };
const populated = (rows: number): Entry => ({ kind: "subsection", shows: { rows } });

/** One workspace row, as row.ts draws it. */
function row(name: string): string {
  return `<div class="ws" data-roster-row="${name}"><div class="row"><span class="name">${name}</span></div></div>`;
}

/** One subsection, as subsections.ts draws it: a folded one keeps its rows, hidden. */
function subsection(id: number, shows: "empty" | "folded" | { rows: number }): string {
  const rows = shows === "empty" ? 0 : shows === "folded" ? 1 : shows.rows;
  const cls = shows === "folded" ? "sb-subsection folded" : "sb-subsection";
  const empty = rows === 0 ? " data-empty" : "";
  const body = Array.from({ length: rows }, (_, i) => row(`s${id}-${i}`)).join("");
  return `<div class="${cls}"${empty} data-subsection="u${id}"><div class="sb-subsection-head"><span class="sb-subsection-label">u${id}</span><span class="sb-count">(${rows})</span></div><div class="rows" data-subsection="u${id}">${body}</div></div>`;
}

/** A repository section whose list holds ENTRIES, inside the open workspaces section. */
function rail(entries: readonly Entry[]): string {
  const list = entries.map((e, i) => (e.kind === "row" ? row(`r${i}`) : subsection(i, e.shows))).join("");
  const repo = `<div class="repo repo-section"><div class="repo-head"><span class="sb-label">repo</span><span class="sb-count">(0)</span></div><div class="rows" data-subsection="">${list}</div></div>`;
  return `<div class="sb-scroll"><div class="sb-roster"><div class="sb-pane"><div class="repo open-section"><div class="repo-head"><span class="sb-label">Open</span></div><div class="sb-sections">${repo}</div></div></div></div></div>`;
}

/** The space between each visible line's text and the next visible line's, in px, top to bottom. */
async function gaps(page: Page, entries: readonly Entry[]): Promise<number[]> {
  return page.evaluate((markup) => {
    const host = document.getElementById("ws-sidebar");
    if (host === null) throw new Error("the page's rail did not mount");
    host.innerHTML = markup;
    const texts = Array.from(host.querySelectorAll<HTMLElement>(".repo-section .name, .repo-section .sb-subsection-label"))
      .map((el) => el.getBoundingClientRect())
      .filter((box) => box.height > 0);
    return texts.slice(1).map((box, i) => box.top - texts[i].bottom);
  }, rail(entries));
}

describe("the gaps around a subsection with nothing under it, in WebKit", () => {
  let browser: Browser;
  let page: Page;
  // The owner's picture: an empty open subsection, a folded one, a populated
  // one, between workspace rows. Its gaps are: row → empty, empty → folded,
  // folded → populated, populated → its first row, its first row → its
  // second, its second → the row after.
  let tight: number[];
  // The same list with every subsection populated, whose gaps are today's.
  let reference: number[];

  beforeAll(async () => {
    const css = await pageStylesheet();
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 900 } });
    await page.setContent(`<!doctype html><html><head></head><body><nav id="ws-sidebar"></nav></body></html>`);
    await page.addStyleTag({ content: css });
    tight = await gaps(page, [ROW, EMPTY, FOLDED, populated(2), ROW]);
    // row → u1, u1 → its row, its row → u2, u2 → its row, its row → u3,
    // u3 → its first row, first → second, second → the row after.
    reference = await gaps(page, [ROW, populated(1), populated(1), populated(2), ROW]);
  });

  afterAll(async () => {
    await browser?.close();
  });

  /** The smaller of today's workspace gaps: a header over its row, and a row over a row. */
  const workspaceGap = (): number => Math.min(reference[1], reference[6]);

  it("draws an empty subsection's header close under the folded one after it", () => {
    // ARRANGE / ACT: done in beforeAll. ASSERT
    expect(tight[1]).toBeLessThan(workspaceGap() * 0.5);
  });

  it("draws a populated subsection's header close under a folded one", () => {
    // ARRANGE / ACT: done in beforeAll. ASSERT
    expect(tight[2]).toBeLessThan(workspaceGap() * 0.5);
  });

  it("keeps the gap from a workspace row down to an empty subsection's header", () => {
    // ARRANGE / ACT: done in beforeAll. ASSERT
    expect(tight[0]).toBeCloseTo(reference[0], 1);
  });

  it("keeps the gap from a populated subsection's header down to its rows", () => {
    // ARRANGE / ACT: done in beforeAll. ASSERT
    expect(tight[3]).toBeCloseTo(reference[5], 1);
  });

  it("keeps the gap between workspace rows", () => {
    // ARRANGE / ACT: done in beforeAll. ASSERT
    expect([tight[4], tight[5]]).toEqual([reference[6], reference[7]]);
  });

  it("keeps the gap from an empty subsection's header down to a workspace row", async () => {
    // ARRANGE / ACT
    const measured = await gaps(page, [EMPTY, ROW]);
    // ASSERT: as far as a header stands over its own row.
    expect(measured[0]).toBeCloseTo(reference[1], 1);
  });

  it("keeps the gap from a folded subsection's header down to a workspace row", async () => {
    // ARRANGE / ACT
    const measured = await gaps(page, [FOLDED, ROW]);
    // ASSERT
    expect(measured[0]).toBeCloseTo(reference[1], 1);
  });

  it("keeps the gap from a workspace row down to a folded subsection's header", async () => {
    // ARRANGE / ACT
    const measured = await gaps(page, [ROW, FOLDED, ROW]);
    // ASSERT
    expect(measured[0]).toBeCloseTo(reference[0], 1);
  });
});
