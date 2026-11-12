/**
 * THE INTERRUPTED MARKER'S BAR SITS UNDER THE BUBBLE'S LEFT EDGE, in WebKit.
 *
 * A turn the user stopped ends in a neutral marker pill (src/feed/marker.ts)
 * whose left edge is a vertical bar. The bar starts at the same horizontal
 * position where the response bubble's and tool card's rounded bottom-left
 * corner (8px) stops curving. The notice goes once a later prompt is sent.
 */
import type { Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { openFeedPage } from "./feed-page";

const ROWS = `
  <article class="feed-item"><div class="bubble" data-role="response" data-variant="response" id="answer">answer</div></article>
  <article class="feed-item"><div class="tool-card" id="card"><div class="tool-head">card</div></div></article>
  <article class="feed-item" id="row"><div class="turn-ended turn-ended-interrupted"><span class="outcome-marker tone-none"><span class="outcome-marker-pill" id="pill"><span class="outcome-marker-text">interrupted</span></span></span></div></article>
`;
const HELD = `<article class="feed-item"><div class="bubble" data-role="prompt" data-variant="held">queued</div></article>`;
const SENT = `<article class="feed-item"><div class="bubble" data-role="prompt" data-variant="user">next</div></article>`;

describe("the interrupted marker's bar, in WebKit", () => {
  let browser: Browser;
  let lefts: { answer: number; card: number; pill: number };
  let rowShown: { alone: boolean; withHeld: boolean; withSent: boolean };

  beforeAll(async () => {
    let page;
    ({ browser, page } = await openFeedPage({ width: 700, height: 900 }));
    lefts = await page.evaluate((rows) => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = rows;
      const left = (id: string): number => {
        const el = document.getElementById(id);
        if (el === null) throw new Error(`no #${id}`);
        return el.getBoundingClientRect().left;
      };
      return { answer: left("answer"), card: left("card"), pill: left("pill") };
    }, ROWS);
    rowShown = await page.evaluate(
      ([held, sent]) => {
        const feed = document.getElementById("feed");
        const row = document.getElementById("row");
        if (feed === null || row === null) throw new Error("the page's rows are missing");
        const shown = (): boolean => getComputedStyle(row).display !== "none";
        const alone = shown();
        feed.insertAdjacentHTML("beforeend", held);
        const withHeld = shown();
        feed.insertAdjacentHTML("beforeend", sent);
        return { alone, withHeld, withSent: shown() };
      },
      [HELD, SENT],
    );
  });

  afterAll(async () => {
    await browser.close();
  });

  it("starts where the response bubble's rounded corner stops curving", () => {
    expect(lefts.pill).toBeCloseTo(lefts.answer + 8, 0);
  });

  it("starts where the tool card's rounded corner stops curving", () => {
    expect(lefts.pill).toBeCloseTo(lefts.card + 8, 0);
  });

  it("stays while no later prompt exists", () => {
    expect(rowShown.alone).toBe(true);
  });

  it("stays while the only later prompt is held", () => {
    expect(rowShown.withHeld).toBe(true);
  });

  it("goes once a later prompt is sent", () => {
    expect(rowShown.withSent).toBe(false);
  });
});
