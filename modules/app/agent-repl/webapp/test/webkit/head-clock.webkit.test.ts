/**
 * EVERY HEAD'S CLOCK CLOSES ITS HEAD AT THE RIGHT END, IN WEBKIT (owner
 * request, 2026-10-18; `.head-clock` in styles.css, `asHeadClock` in
 * src/feed/cards/tool-head.ts).
 *
 * jsdom lays nothing out, so this draws each head as its renderer shapes it --
 * a tool card's, and a shell's, a subagent's and a merge's inside the shared
 * bubble head row -- in a card wider than the head's content, in real headless
 * WebKit over the real stylesheet, and measures where the clock lands: at the
 * head row's right content edge, after everything else on the head, including
 * what the renderer draws after it.
 *
 * THE READING ENDS THERE TOO (owner request, 2026-10-19): a clock is a reserved
 * box as wide as its widest reading, so each clock here carries the real
 * reserve and the measure is of the painted reading's right edge, not only the
 * box's. A tool group's tabline clock, which closes the tabline the same way,
 * is measured against the tabline's right edge.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { CLOCK_RESERVE_PROPERTY, ELAPSED_RESERVE_LABELS } from "../../src/elapsed-clock.js";
import { FEED_TABLINE_CLASS, FEED_TABLINE_CLOCK_CLASS, FEED_TAB_STRIP_CLASS } from "../../src/feed/tabs.js";
import { reserveLabelsCss } from "../../src/width-reserve.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A bubble's head row holding HEAD in its slot, as bubble.ts builds it. */
function inBubble(head: string): string {
  return `<div class="tool-card bubble-fold" style="width:600px"><div class="tool-head bubble-head"><span class="bubble-head-slot">${head}</span></div></div>`;
}

/** Each head, keyed by name, with its clock and the badge before it. */
const HEADS: Readonly<Record<string, string>> = {
  "a tool card": `<div class="tool-card" style="width:600px"><div class="tool-head" data-tool-card-head><span class="tool-name">Bash</span><span class="badge ok">done</span><span class="tool-clock reserved-clock head-clock">3s</span></div></div>`,
  "a shell bubble": inBubble(
    `<div class="shell-bubble"><div class="shell-head"><span class="shell-command">ls</span><span class="shell-outcome badge ok">done</span><span class="shell-clock reserved-clock head-clock">3s</span><span class="work-id">w1</span></div></div>`,
  ),
  "a subagent bubble": inBubble(
    `<div class="subagent-head"><span class="subagent-label">Explore</span><span class="subagent-outcome badge ok">done</span><span class="subagent-clock reserved-clock head-clock">30s</span><span class="work-id">w1</span></div>`,
  ),
  "a merge bubble": inBubble(
    `<div class="merge-head"><span class="merge-head-props"><span class="merge-glyph">⇄</span><span class="merge-label">a → master</span></span><span class="merge-badge is-live">waiting</span><span class="merge-head-end"><span class="merge-clock reserved-clock head-clock">3s</span></span></div>`,
  ),
};

/** Where one head's clock landed. */
interface Measured {
  /** The clock's right edge less the head row's right content edge. */
  rightGap: number;
  /** The painted reading's right edge less the head row's right content edge. */
  readingGap: number;
  /** Whether every other item on the head ends left of the clock's start. */
  lastOnTheLine: boolean;
  /** The clock's and the badge's computed font sizes. */
  clockFont: string;
  badgeFont: string;
}

/** The reserve every clock here is held at, as `reserveWidth` sets it. */
const RESERVE = { property: CLOCK_RESERVE_PROPERTY, labels: reserveLabelsCss(ELAPSED_RESERVE_LABELS) };

/** Draw HTML and measure its clock against its head row. */
async function measure(page: Page, html: string): Promise<Measured> {
  return page.evaluate(({ markup, reserve }) => {
    const host = document.getElementById("host");
    if (host === null) throw new Error("the page's host did not mount");
    host.innerHTML = markup;
    const row = host.querySelector<HTMLElement>(".tool-head");
    const clock = host.querySelector<HTMLElement>(".head-clock");
    const badge = host.querySelector<HTMLElement>(".badge, .merge-badge");
    if (row === null || clock === null || badge === null) throw new Error("the head did not draw");
    clock.style.setProperty(reserve.property, reserve.labels);
    const reading = document.createRange();
    reading.selectNodeContents(clock);
    const rowRect = row.getBoundingClientRect();
    const contentRight = rowRect.right - parseFloat(getComputedStyle(row).paddingRight);
    const clockRect = clock.getBoundingClientRect();
    const others = [...row.querySelectorAll<HTMLElement>("span, div")].filter(
      (el) => el !== clock && !el.contains(clock) && el.children.length === 0,
    );
    return {
      rightGap: clockRect.right - contentRight,
      readingGap: reading.getBoundingClientRect().right - contentRight,
      lastOnTheLine: others.every((el) => el.getBoundingClientRect().right <= clockRect.left),
      clockFont: getComputedStyle(clock).fontSize,
      badgeFont: getComputedStyle(badge).fontSize,
    };
  }, { markup: html, reserve: RESERVE });
}

/** A tool group's tabline, as `drawFeedTabline` builds it, holding a tool card clock reading "3s". */
const TABLINE = `<div class="feed-group" style="width:600px"><div class="feed-group-tabline ${FEED_TABLINE_CLASS}"><div class="${FEED_TAB_STRIP_CLASS}" role="tablist"><span class="feed-group-tab is-active">1</span></div><span class="${FEED_TABLINE_CLOCK_CLASS}"><span class="tool-clock reserved-clock">3s</span></span></div></div>`;

/** The tabline clock's painted reading's right edge less the tabline's right edge. */
async function measureTabline(page: Page): Promise<number> {
  return page.evaluate(({ markup, reserve }) => {
    const host = document.getElementById("host");
    if (host === null) throw new Error("the page's host did not mount");
    host.innerHTML = markup;
    const line = host.querySelector<HTMLElement>(".feed-group-tabline");
    const clock = host.querySelector<HTMLElement>(".reserved-clock");
    if (line === null || clock === null) throw new Error("the tabline did not draw");
    clock.style.setProperty(reserve.property, reserve.labels);
    const reading = document.createRange();
    reading.selectNodeContents(clock);
    return reading.getBoundingClientRect().right - line.getBoundingClientRect().right;
  }, { markup: TABLINE, reserve: RESERVE });
}

describe("a head's clock in WebKit", () => {
  let browser: Browser;
  const measured = new Map<string, Measured>();
  let tablineGap = Infinity;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: { width: 900, height: 400 } });
    await page.setContent('<!doctype html><html><head></head><body><div id="host" style="width:800px"></div></body></html>');
    await page.addStyleTag({ content: css });
    for (const [name, html] of Object.entries(HEADS)) measured.set(name, await measure(page, html));
    tablineGap = await measureTabline(page);
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(Object.keys(HEADS))("ends %s's head row on its clock", (name) => {
    expect(Math.abs(measured.get(name)?.rightGap ?? Infinity)).toBeLessThan(1);
  });

  it.each(Object.keys(HEADS))("ends %s's clock reading on its head row's right edge, not short of it in the reserve", (name) => {
    expect(Math.abs(measured.get(name)?.readingGap ?? Infinity)).toBeLessThan(1);
  });

  it("ends a tool group's tabline clock reading on the tabline's right edge", () => {
    expect(Math.abs(tablineGap)).toBeLessThan(1);
  });

  it.each(Object.keys(HEADS))("draws %s's clock after everything else on its head", (name) => {
    expect(measured.get(name)?.lastOnTheLine).toBe(true);
  });

  it("reads a merge head's clock at its badge's size", () => {
    const merge = measured.get("a merge bubble");
    expect(merge?.clockFont).toBe(merge?.badgeFont);
  });
});
