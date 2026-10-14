/**
 * THE TURN'S ANSWER SITS ONE FEED GAP ABOVE THE NEXT ROW (owner request,
 * 2026-10-14), in WebKit, the Emacs webview's engine.
 *
 * A concluded turn's end row (src/feed/rows/turn-ended.ts) is an empty marker
 * between the answer and the next row. It used to take a slot of #feed's
 * column, so the answer sat two gaps above the next bubble where a tool card
 * sits one. This suite draws both sequences over the real stylesheet and holds
 * the answer's gap to the tool card's.
 */
import type { Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { openFeedPage } from "./feed-page";

/** The gaps measured, in CSS pixels. */
interface Gaps {
  /** A tool card's bottom edge to the next prompt bubble's top edge. */
  afterToolCard: number;
  /** A concluded turn's answer's bottom edge to the next prompt bubble's top edge. */
  afterAnswer: number;
  /** An interrupted turn's end row's height: its outcome marker keeps its slot. */
  interruptedRowHeight: number;
}

/** The page's rows: each sequence's lower and upper edge are tagged by id. */
const ROWS = `
  <article class="feed-item"><div class="tool-card" id="card">card</div></article>
  <article class="feed-item"><div class="bubble" data-role="prompt" data-variant="user" id="after-card">next</div></article>
  <article class="feed-item"><div class="bubble final-response" data-role="response" data-variant="response" id="answer">answer</div></article>
  <article class="feed-item"><div class="turn-ended turn-ended-concluded" data-arm="concluded"></div></article>
  <article class="feed-item"><div class="bubble" data-role="prompt" data-variant="user" id="after-answer">next</div></article>
  <article class="feed-item" id="interrupted"><div class="turn-ended turn-ended-interrupted"><span class="outcome-marker">interrupted</span></div></article>
`;

describe("a concluded turn's answer and the next row, in WebKit", () => {
  let browser: Browser;
  let gaps: Gaps;

  beforeAll(async () => {
    let page;
    ({ browser, page } = await openFeedPage({ width: 700, height: 900 }));
    gaps = await page.evaluate((rows) => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = rows;
      const rect = (id: string): DOMRect => {
        const el = document.getElementById(id);
        if (el === null) throw new Error(`no #${id}`);
        return el.getBoundingClientRect();
      };
      return {
        afterToolCard: rect("after-card").top - rect("card").bottom,
        afterAnswer: rect("after-answer").top - rect("answer").bottom,
        interruptedRowHeight: rect("interrupted").height,
      };
    }, ROWS);
  });

  afterAll(async () => {
    await browser.close();
  });

  it("puts the answer as far above the next bubble as a tool card", () => {
    expect(gaps.afterAnswer).toBeCloseTo(gaps.afterToolCard, 1);
  });

  it("keeps an interrupted turn's end row in the column", () => {
    expect(gaps.interruptedRowHeight).toBeGreaterThan(0);
  });
});
