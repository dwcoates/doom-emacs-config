/**
 * A MERGE TAB'S DURATION SITS QUIETLY BESIDE ITS LABEL, IN WEBKIT (owner
 * request, 2026-10-01).
 *
 * Each tab in the merge bubble's strip shows how long it was (or has been) in
 * its state, beside its label. It must not break the strip: it stays on the
 * label's line inside its tab, adds no height to the tab or the strip, and a
 * ticking value changing width moves nothing ahead of it. jsdom lays nothing
 * out, so this draws the strip as `drawFeedMergeTab` shapes it (its parts are
 * pinned in test/feed/merge/tab-strip.test.ts) in real headless WebKit over
 * the real stylesheet, and measures it.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

/** One tab: its label and its duration, or none to draw it without one. */
interface Tab {
  label: string;
  duration?: string;
  live?: boolean;
}

/** One tab's markup, as `drawFeedMergeTab` shapes it. */
function tab(t: Tab): string {
  const duration = t.duration === undefined ? "" : `<span class="merge-tab-duration">${t.duration}</span>`;
  const glyph = t.live === true
    ? `<span class="merge-tab-glyph is-live" aria-hidden="true">●</span>`
    : `<span class="merge-tab-glyph is-succeeded" aria-hidden="true">✓</span>`;
  return `<ar-button role="button" class="merge-tab" aria-selected="false"><span class="merge-tab-label">${t.label}</span>${duration}${glyph}</ar-button>`;
}

/** A box's edges. */
interface Box {
  left: number;
  top: number;
  height: number;
}

/** What one strip measured. */
interface Measured {
  strip: Box;
  tabs: Box[];
  labels: Box[];
  durations: Box[];
}

/** Draw a strip of TABS at WIDTH and measure it. */
async function measure(page: Page, tabs: readonly Tab[], width = 640): Promise<Measured> {
  const html = tabs.map(tab).join("");
  return page.evaluate(
    ({ markup, px }) => {
      const host = document.getElementById("host");
      if (host === null) throw new Error("the page's host did not mount");
      host.style.width = `${px}px`;
      host.innerHTML = `<div class="merge-strip">${markup}</div>`;
      const strip = host.firstElementChild as HTMLElement;
      const box = (el: Element): { left: number; top: number; height: number } => {
        const r = el.getBoundingClientRect();
        return { left: r.left, top: r.top, height: r.height };
      };
      return {
        strip: box(strip),
        tabs: [...strip.querySelectorAll(".merge-tab")].map(box),
        labels: [...strip.querySelectorAll(".merge-tab-label")].map(box),
        durations: [...strip.querySelectorAll(".merge-tab-duration")].map(box),
      };
    },
    { markup: html, px: width },
  );
}

/** A run's strip: settled tabs, then the live one ticking. */
const RUN: readonly Tab[] = [
  { label: "queue", duration: "1m 5s" },
  { label: "rebasing", duration: "12s" },
  { label: "tests", duration: "4m 59s", live: true },
];

/** The same strip with no durations drawn. */
const BARE: readonly Tab[] = RUN.map(({ label, live }) => ({ label, live }));

describe("a merge tab's duration in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 700, height: 400 } });
    await page.setContent(`<!doctype html><html><head></head><body><div id="host"></div></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("sits on its label's line, to the label's right", async () => {
    const m = await measure(page, RUN);
    const sameLine = m.durations.every((d, i) => Math.abs(d.top + d.height / 2 - (m.labels[i].top + m.labels[i].height / 2)) <= 2);
    const rightOf = m.durations.every((d, i) => d.left > m.labels[i].left);
    expect([sameLine, rightOf]).toEqual([true, true]);
  });

  it("adds no height to a tab", async () => {
    const bare = await measure(page, BARE);
    const timed = await measure(page, RUN);
    expect(timed.tabs.map((t) => Math.round(t.height))).toEqual(bare.tabs.map((t) => Math.round(t.height)));
  });

  it("keeps the strip to one line at an ordinary width", async () => {
    const bare = await measure(page, BARE);
    const timed = await measure(page, RUN);
    expect(Math.round(timed.strip.height)).toBe(Math.round(bare.strip.height));
  });

  it("moves no tab ahead of the live one as its value changes width", async () => {
    const before = await measure(page, RUN);
    const after = await measure(page, [RUN[0], RUN[1], { ...RUN[2], duration: "5m" }]);
    expect(after.tabs.slice(0, 2).map((t) => Math.round(t.left))).toEqual(before.tabs.slice(0, 2).map((t) => Math.round(t.left)));
  });

  it("never wraps its own value when the strip is narrow", async () => {
    const narrow = await measure(page, [{ label: "tests", duration: "123h 59m 59s", live: true }], 60);
    const wide = await measure(page, [{ label: "tests", duration: "123h 59m 59s", live: true }], 640);
    expect(Math.round(narrow.durations[0].height)).toBe(Math.round(wide.durations[0].height));
  });
});
