/**
 * A RECENTLY MERGED ROW IS DRAWN AS A WAITING MERGE'S LINE IS, IN WEBKIT
 * (owner request, 2026-11-09): the same height, the same name size and the
 * same indent, and in both sections the duration on the right reads in the
 * name's own size. jsdom lays nothing out, so this draws both lines in the
 * shape the roster draws them over the real stylesheet and measures them.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { pageStylesheet } from "./stylesheets";

const MARKUP = `<nav id="ws-sidebar" style="width:260px">
<div class="repo merge-queue-section"><div class="rows"><div class="merged-group"><div class="rows sb-merge-queue">
<div class="sb-mq-waiting"><span class="sb-mq-name">behind</span><span class="sb-mq-clock">1m</span></div>
</div></div></div></div>
<div class="repo merged-section"><div class="rows"><div class="merged-group"><div class="rows">
<div class="ws"><div class="row"><span class="name band-name">landed</span><span class="when">3h</span></div></div>
</div></div></div></div></nav>`;

interface Drawn {
  height: number;
  nameSize: string;
  nameLeft: number;
  durationSize: string;
}

describe("a recently merged row against a waiting merge's line in WebKit", () => {
  let browser: Browser;
  let page: Page;
  let waiting: Drawn;
  let merged: Drawn;

  beforeAll(async () => {
    const css = await pageStylesheet();
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 700 } });
    await page.setContent(`<!doctype html><html><head></head><body></body></html>`);
    await page.addStyleTag({ content: css });
    [waiting, merged] = await page.evaluate((markup) => {
      document.body.innerHTML = markup;
      const draw = (line: string, name: string, duration: string): Drawn => {
        const el = document.querySelector(line) as HTMLElement;
        const n = el.querySelector(name) as HTMLElement;
        return {
          height: el.getBoundingClientRect().height,
          nameSize: getComputedStyle(n).fontSize,
          nameLeft: n.getBoundingClientRect().left,
          durationSize: getComputedStyle(el.querySelector(duration) as HTMLElement).fontSize,
        };
      };
      return [draw(".sb-mq-waiting", ".sb-mq-name", ".sb-mq-clock"), draw(".merged-section .row", ".name", ".when")];
    }, MARKUP);
  });

  afterAll(async () => {
    await browser.close();
  });

  it("gives a merged row a waiting line's height", () => {
    expect(merged.height).toBeCloseTo(waiting.height, 1);
  });

  it("gives a merged row's name a waiting name's size", () => {
    expect(merged.nameSize).toBe(waiting.nameSize);
  });

  it("starts a merged row's name where a waiting name starts", () => {
    expect(merged.nameLeft).toBeCloseTo(waiting.nameLeft, 1);
  });

  it("draws a waiting merge's clock in its name's size", () => {
    expect(waiting.durationSize).toBe(waiting.nameSize);
  });

  it("draws a merged row's duration in its name's size", () => {
    expect(merged.durationSize).toBe(merged.nameSize);
  });
});
