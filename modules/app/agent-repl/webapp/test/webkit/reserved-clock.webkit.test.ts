/**
 * A CARD HEAD'S CLOCK HOLDS ITS WIDTH, IN WEBKIT (owner ruling, 2026-10-11).
 *
 * A reserved clock (src/elapsed-clock.ts, `.reserved-clock` in styles.css) is
 * as wide as the widest reading it can tick to, from its first paint: a tick
 * never slides what follows it in the head, and its ghost adds no height. jsdom
 * lays nothing out, so this draws a subagent head's clock and the badge after
 * it, as the head shapes them, in real headless WebKit over the real
 * stylesheet, and measures every reading the reserve covers.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { CLOCK_RESERVE_PROPERTY, ELAPSED_RESERVE_LABELS } from "../../src/elapsed-clock.js";
import { reserveLabelsCss } from "../../src/width-reserve.js";
import { pageStylesheet } from "./stylesheets";

/** Readings of every shape the reserve covers, narrowest to widest. */
const READINGS = ["0s", "9s", "59s", "1m", "5m 3s", "59m 59s", "1h", "23h 59m", "999h 59m"];

/** What one reading measured: the clock's box, the next item's left, and a bare line's height. */
interface Measured {
  width: number;
  height: number;
  nextLeft: number;
  lineHeight: number;
}

/** Draw a head whose clock reads READING, with a badge after it, and measure it. */
async function measure(page: Page, reading: string): Promise<Measured> {
  return page.evaluate(
    ({ text, property, labels }) => {
      const host = document.getElementById("host");
      if (host === null) throw new Error("the page's host did not mount");
      host.innerHTML = `<div class="subagent-head"><span class="subagent-label">Explore</span><span class="subagent-clock reserved-clock"></span><span class="subagent-outcome badge ok">done</span><span class="subagent-clock" id="bare">0s</span></div>`;
      const clock = host.querySelector<HTMLElement>(".reserved-clock");
      const next = host.querySelector(".subagent-outcome");
      const bare = document.getElementById("bare");
      if (clock === null || next === null || bare === null) throw new Error("the head did not draw");
      clock.style.setProperty(property, labels);
      clock.textContent = text;
      const r = clock.getBoundingClientRect();
      return { width: r.width, height: r.height, nextLeft: next.getBoundingClientRect().left, lineHeight: bare.getBoundingClientRect().height };
    },
    { text: reading, property: CLOCK_RESERVE_PROPERTY, labels: reserveLabelsCss(ELAPSED_RESERVE_LABELS) },
  );
}

describe("a reserved clock in WebKit", () => {
  let browser: Browser;
  let measured: Measured[];

  beforeAll(async () => {
    const css = await pageStylesheet();
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: { width: 900, height: 400 } });
    await page.setContent('<!doctype html><html><head></head><body><div id="host" style="width:800px"></div></body></html>');
    await page.addStyleTag({ content: css });
    measured = [];
    for (const reading of READINGS) measured.push(await measure(page, reading));
  });

  afterAll(async () => {
    await browser.close();
  });

  it("keeps one width through every reading", () => {
    expect(new Set(measured.map((m) => m.width)).size).toBe(1);
  });

  it("never moves the badge after it", () => {
    expect(new Set(measured.map((m) => m.nextLeft)).size).toBe(1);
  });

  it("takes no more height than one line of its reading", () => {
    expect(measured.every((m) => m.height === m.lineHeight)).toBe(true);
  });

  it("reserves a real width, wider than its narrowest reading", () => {
    // A ghost that never laid out would leave the box at the reading's own width.
    expect(measured[0].width).toBeGreaterThan(20);
  });
});
