/**
 * THE MARGIN STAMP CENTERS IN THE FEED'S RIGHT MARGIN, in WebKit (owner
 * request, 2026-11-23). The hovered row's time and age (src/feed/margin-stamp.ts)
 * stand in the margin between the column's right rail and the feed's right
 * edge, centered across it, and centered down on the `top` the script sets.
 * Beside a response, whose bubble ends at the one bubble width short of the
 * right rail, the margin starts at that width's end (owner request,
 * 2026-11-24). Its grey is a slight step darker than `--muted`.
 */
import type { Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { openFeedPage } from "./feed-page";

const WORDS = Array.from({ length: 200 }, (_, i) => `word${i}`).join(" ");
const ROWS = `<article class="feed-item"><div class="clear-divider" id="rail"></div></article><article class="feed-item" data-feed-row="r"><div class="bubble" data-role="response" id="response">${WORDS}</div></article><div id="muted" style="color: var(--muted)">grey</div>`;
const STAMP = `<div class="feed-margin-stamp" id="stamp" data-shown style="top: 300px"><div class="feed-margin-stamp-lines" id="lines"><div id="clock">14:03:22</div><div>5m 30s ago</div></div></div><div class="feed-margin-stamp" id="rstamp" data-shown data-row-role="response" style="top: 300px"><div class="feed-margin-stamp-lines" id="rlines"><div id="rclock">14:03:22</div><div>5m 30s ago</div></div></div>`;

interface Measured {
  railRight: number;
  feedRight: number;
  linesLeft: number;
  linesRight: number;
  clockCenter: number;
  stampMiddle: number;
  feedTop: number;
  responseRight: number;
  responseLinesLeft: number;
  responseLinesRight: number;
  responseClockCenter: number;
  stampInk: string;
  mutedInk: string;
}

/**
 * The red, green and blue of a computed CSS color on a 0..255 scale, whether
 * it is stated as `rgb(...)` or, as a color-mix computes to, `color(srgb ...)`
 * on a 0..1 scale.
 */
function channels(color: string): number[] {
  const scale = color.startsWith("color(") ? 255 : 1;
  return (color.match(/[\d.]+/g) ?? []).slice(0, 3).map((n) => Number(n) * scale);
}

describe("the margin stamp, in WebKit", () => {
  let browser: Browser;
  let measured: Measured;

  beforeAll(async () => {
    let page;
    ({ browser, page } = await openFeedPage({ width: 900, height: 700 }));
    measured = await page.evaluate(
      ([rows, stamp]) => {
        const feed = document.getElementById("feed");
        const scroll = document.getElementById("feed-scroll");
        if (feed === null || scroll === null) throw new Error("the page's shell did not mount");
        feed.innerHTML = rows;
        scroll.insertAdjacentHTML("beforeend", stamp);
        const rect = (id: string): DOMRect => {
          const el = document.getElementById(id);
          if (el === null) throw new Error(`no #${id}`);
          return el.getBoundingClientRect();
        };
        const textCenter = (id: string): number => {
          const range = document.createRange();
          range.selectNodeContents(document.getElementById(id)!);
          const text = range.getBoundingClientRect();
          return (text.left + text.right) / 2;
        };
        const box = scroll.getBoundingClientRect();
        // The script spans each stamp over the scroll box's client area.
        for (const id of ["stamp", "rstamp"]) {
          const el = document.getElementById(id)!;
          el.style.left = "0px";
          el.style.width = `${scroll.clientWidth}px`;
        }
        const ink = (id: string): string => getComputedStyle(document.getElementById(id)!).color;
        const stampRect = rect("stamp");
        return {
          railRight: rect("rail").right,
          feedRight: box.left + scroll.clientLeft + scroll.clientWidth,
          linesLeft: rect("lines").left,
          linesRight: rect("lines").right,
          clockCenter: textCenter("clock"),
          stampMiddle: (stampRect.top + stampRect.bottom) / 2,
          feedTop: box.top,
          responseRight: rect("response").right,
          responseLinesLeft: rect("rlines").left,
          responseLinesRight: rect("rlines").right,
          responseClockCenter: textCenter("rclock"),
          stampInk: ink("stamp"),
          mutedInk: ink("muted"),
        };
      },
      [ROWS, STAMP],
    );
  });

  afterAll(async () => {
    await browser.close();
  });

  it("starts its lines' box at the column's right rail", () => {
    expect(measured.linesLeft).toBeCloseTo(measured.railRight, 0);
  });

  it("ends its lines' box at the feed's right edge", () => {
    expect(measured.linesRight).toBeCloseTo(measured.feedRight, 0);
  });

  it("centers its text across the margin", () => {
    expect(measured.clockCenter).toBeCloseTo((measured.railRight + measured.feedRight) / 2, 0);
  });

  it("centers itself down on the top the script sets", () => {
    expect(measured.stampMiddle).toBeCloseTo(measured.feedTop + 300, 0);
  });

  it("starts its lines' box beside a response where the response's full width ends", () => {
    expect(measured.responseLinesLeft).toBeCloseTo(measured.responseRight, 0);
  });

  it("ends its lines' box beside a response at the feed's right edge", () => {
    expect(measured.responseLinesRight).toBeCloseTo(measured.feedRight, 0);
  });

  it("centers its text beside a response across all the room the response leaves", () => {
    expect(measured.responseClockCenter).toBeCloseTo((measured.responseRight + measured.feedRight) / 2, 0);
  });

  it("inks its text a step darker than the muted grey", () => {
    const [r, g, b] = channels(measured.stampInk);
    const [mr, mg, mb] = channels(measured.mutedInk);
    expect([r < mr, g < mg, b < mb]).toEqual([true, true, true]);
  });
});
