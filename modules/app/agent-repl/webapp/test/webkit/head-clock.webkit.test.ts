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
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

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
  /** Whether every other item on the head ends left of the clock's start. */
  lastOnTheLine: boolean;
  /** The clock's and the badge's computed font sizes. */
  clockFont: string;
  badgeFont: string;
}

/** Draw HTML and measure its clock against its head row. */
async function measure(page: Page, html: string): Promise<Measured> {
  return page.evaluate((markup) => {
    const host = document.getElementById("host");
    if (host === null) throw new Error("the page's host did not mount");
    host.innerHTML = markup;
    const row = host.querySelector<HTMLElement>(".tool-head");
    const clock = host.querySelector<HTMLElement>(".head-clock");
    const badge = host.querySelector<HTMLElement>(".badge, .merge-badge");
    if (row === null || clock === null || badge === null) throw new Error("the head did not draw");
    const rowRect = row.getBoundingClientRect();
    const contentRight = rowRect.right - parseFloat(getComputedStyle(row).paddingRight);
    const clockRect = clock.getBoundingClientRect();
    const others = [...row.querySelectorAll<HTMLElement>("span, div")].filter(
      (el) => el !== clock && !el.contains(clock) && el.children.length === 0,
    );
    return {
      rightGap: clockRect.right - contentRight,
      lastOnTheLine: others.every((el) => el.getBoundingClientRect().right <= clockRect.left),
      clockFont: getComputedStyle(clock).fontSize,
      badgeFont: getComputedStyle(badge).fontSize,
    };
  }, html);
}

describe("a head's clock in WebKit", () => {
  let browser: Browser;
  const measured = new Map<string, Measured>();

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: { width: 900, height: 400 } });
    await page.setContent('<!doctype html><html><head></head><body><div id="host" style="width:800px"></div></body></html>');
    await page.addStyleTag({ content: css });
    for (const [name, html] of Object.entries(HEADS)) measured.set(name, await measure(page, html));
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(Object.keys(HEADS))("ends %s's head row on its clock", (name) => {
    expect(Math.abs(measured.get(name)?.rightGap ?? Infinity)).toBeLessThan(1);
  });

  it.each(Object.keys(HEADS))("draws %s's clock after everything else on its head", (name) => {
    expect(measured.get(name)?.lastOnTheLine).toBe(true);
  });

  it("reads a merge head's clock at its badge's size", () => {
    const merge = measured.get("a merge bubble");
    expect(merge?.clockFont).toBe(merge?.badgeFont);
  });
});
