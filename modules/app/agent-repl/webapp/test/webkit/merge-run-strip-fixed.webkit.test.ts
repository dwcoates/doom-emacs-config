/**
 * THE TESTS TAB'S RUN STRIP STAYS PUT WHILE ONLY THE RUN'S VIEW SCROLLS, IN
 * WEBKIT (owner request, 2026-10-14).
 *
 * jsdom lays nothing out, so this draws an expanded merge bubble in the merge
 * body's real nesting (feed-body > merge-tabs > tests panel > substrip > run
 * panel > merge-run > views), over a run view far taller than the bubble's
 * cap, in real headless WebKit over the real stylesheet, and measures which
 * box scrolls and whether the run strip moves.
 */
import type { Browser, Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { openFeedPage } from "./feed-page";

/** Content far taller than the bubble's cap. */
const TALL = `<div style="height: 4000px">content</div>`;

/** An expanded merge bubble showing TOP's tab body over BODY. */
function bubble(top: string, body: string): string {
  return `
  <div class="tool-card bubble-fold" data-merge-bubble data-expanded="true" data-probe-bubble>
    <div class="bubble-head">merge</div>
    <div class="agent-panel bubble-subfeed" data-probe-subfeed>
      <div class="feed-page-error-slot"></div>
      <div class="feed-body">
        <div class="merge-tabs">
          <div class="merge-strip">
            <ar-button role="button" class="merge-tab">queue</ar-button>
            <ar-button role="button" class="merge-tab is-active">tests</ar-button>
          </div>
          <div class="merge-tab-summary" hidden></div>
          <div class="merge-tab-panel" data-merge-tab-body="${top}">${body}</div>
        </div>
        <div class="merge-loose-rows"></div>
      </div>
    </div>
  </div>`;
}

/** The tests tab's body: its run strip over a run whose tests view is TALL. */
const TESTS_BODY = `
  <div class="merge-substrip">
    <div class="merge-strip is-nested" data-probe-runs>
      <ar-button role="button" class="merge-tab is-active" data-merge-tab="testsRun">run 1</ar-button>
      <ar-button role="button" class="merge-tab" data-merge-tab="testsRun">run 2</ar-button>
    </div>
    <div class="merge-tab-summary" hidden></div>
    <div class="merge-tab-panel" data-merge-tab-body="testsRun">
      <div class="merge-run" data-run-view="tests">
        <div class="merge-run-views list-rows" role="menu" hidden></div>
        <div class="merge-run-output merge-run-view" data-run-view="tests" data-probe-view>${TALL}</div>
        <div class="merge-run-remediation merge-run-view" data-run-view="remediation" hidden></div>
      </div>
    </div>
  </div>`;

interface Measured {
  bubble: number;
  ceiling: number;
  subfeedScrolls: boolean;
  viewScrolls: boolean;
  runsBefore: number;
  runsAfter: number;
  viewScrolled: number;
}

/** Draw HTML, then scroll the run's view by SCROLL px, measuring before and after. */
async function measure(page: Page, html: string, scroll: number): Promise<Measured> {
  return page.evaluate(
    ([markup, by]) => {
      const feed = document.getElementById("feed");
      const box = document.getElementById("feed-scroll");
      if (feed === null || box === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = `<div class="feed-item" data-feed-row="r1">${markup}</div>`;
      const probe = (attr: string): HTMLElement | null => feed.querySelector<HTMLElement>(`[${attr}]`);
      const bubbleEl = probe("data-probe-bubble");
      const subfeed = probe("data-probe-subfeed");
      const view = probe("data-probe-view");
      const runs = probe("data-probe-runs");
      if (bubbleEl === null || subfeed === null || view === null || runs === null) throw new Error("the bubble drew no probe");
      const runsBefore = runs.getBoundingClientRect().top;
      view.scrollTop = by;
      return {
        bubble: bubbleEl.getBoundingClientRect().height,
        ceiling: box.clientHeight * 0.8 * 0.45,
        subfeedScrolls: subfeed.scrollHeight > subfeed.clientHeight + 1,
        viewScrolls: view.scrollHeight > view.clientHeight + 1,
        runsBefore,
        runsAfter: runs.getBoundingClientRect().top,
        viewScrolled: view.scrollTop,
      };
    },
    [html, scroll] as const,
  );
}

describe("the tests tab's run strip in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    ({ browser, page } = await openFeedPage({ width: 800, height: 800 }));
  });

  afterAll(async () => {
    await browser.close();
  });

  it("caps the expanded merge bubble at 45% of the item ceiling", async () => {
    // Arrange / Act.
    const { bubble: height, ceiling } = await measure(page, bubble("tests", TESTS_BODY), 0);
    // Assert.
    expect(ceiling).toBeGreaterThan(0);
    expect(Math.abs(height - ceiling)).toBeLessThanOrEqual(1);
  });

  it("scrolls the run's view rather than the sub-feed", async () => {
    // Arrange / Act.
    const { subfeedScrolls, viewScrolls } = await measure(page, bubble("tests", TESTS_BODY), 0);
    // Assert.
    expect([subfeedScrolls, viewScrolls]).toEqual([false, true]);
  });

  it("keeps the run strip in place while the run's view scrolls", async () => {
    // Arrange / Act.
    const { runsBefore, runsAfter, viewScrolled } = await measure(page, bubble("tests", TESTS_BODY), 1500);
    // Assert.
    expect(viewScrolled).toBeGreaterThan(0);
    expect(Math.abs(runsAfter - runsBefore)).toBeLessThanOrEqual(1);
  });

  it("leaves any other tab scrolling in the sub-feed", async () => {
    // Arrange / Act.
    const { subfeedScrolls } = await measure(page, bubble("queue", `${TALL}<div data-probe-view></div><div data-probe-runs></div>`), 0);
    // Assert.
    expect(subfeedScrolls).toBe(true);
  });
});
