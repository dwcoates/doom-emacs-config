/**
 * THE READER'S CLICK NEVER MOVES THE FEED, IN WEBKIT (owner requests,
 * 2026-10-15 and 2026-10-16).
 *
 * Answering a question or a permission at the feed's tail redraws its card
 * shorter, and answering the docked gate hides the dock, growing the feed's
 * box under the view. Either makes a real box clamp `scrollTop` upward, which
 * the reader saw as the feed scrolling up on a click. jsdom lays nothing out
 * and so cannot clamp; this runs the real stylesheet and the real scroll
 * wiring in real headless WebKit (click-hold-page.ts) and asserts no painted
 * frame moves the clicked card.
 *
 * Its browser is Playwright's WebKit build pinned by `playwright-core`
 * (package.json); `npx playwright-core install webkit` fetches it once.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bootPage, pageAssets } from "./boot-page";

/** How much shorter the answered card redraws. */
const SHRINK_PX = 300;
/** Painted frames measured after each change: a correction settles well inside it. */
const FRAMES = 6;
/** A painted deviation at or under this is subpixel rounding, not a jump. */
const TOLERANCE_PX = 1;

let browser: Browser;
let assets: { script: string; css: string };

/** A fresh page over the click-hold shell, parked at its tail. */
async function freshPage(): Promise<Page> {
  const { page } = await bootPage(browser, { ...assets, global: "clickHold", viewport: { width: 800, height: 800 } });
  await page.evaluate(() => window.clickHold.park());
  return page;
}

/** Every record PAGE wrote, as `level operation`. */
const recordsOf = (page: Page): Promise<string[]> => page.evaluate(() => window.clickHold.records());

describe("a click in the feed holds its view, in WebKit", () => {
  /** The painted move of the card each case read, and the records its page wrote. */
  let unclicked: number;
  let clicked: number;
  let clickedRecords: string[];
  let docked: number;
  let dockedRecords: string[];

  beforeAll(async () => {
    assets = await pageAssets("click-hold-page.ts", "clickHoldPage");
    browser = await webkit.launch();
    const control = await freshPage();
    unclicked = await control.evaluate(
      ([px, frames]) => window.clickHold.answerTailCard(false, px, frames),
      [SHRINK_PX, FRAMES] as const,
    );
    const answered = await freshPage();
    clicked = await answered.evaluate(
      ([px, frames]) => window.clickHold.answerTailCard(true, px, frames),
      [SHRINK_PX, FRAMES] as const,
    );
    clickedRecords = await recordsOf(answered);
    const gate = await freshPage();
    docked = await gate.evaluate((frames) => window.clickHold.answerDockedGate(frames), FRAMES);
    dockedRecords = await recordsOf(gate);
  });

  afterAll(async () => {
    await browser.close();
  });

  it("moves a tail card that shrinks with no click before it, so the clamp is real here", () => {
    expect(unclicked).toBeGreaterThan(SHRINK_PX - TOLERANCE_PX);
  });

  it("holds the clicked tail card still when its answer redraws it shorter", () => {
    expect(clicked).toBeLessThanOrEqual(TOLERANCE_PX);
  });

  it("holds the tail card still when the docked gate's answer hides the dock", () => {
    expect(docked).toBeLessThanOrEqual(TOLERANCE_PX);
  });

  it("owes the docked gate's stillness to the floor undoing the box's clamp", () => {
    // A still pass with no restore would mean the dock's height never reached
    // the feed's box: a vacuous pass.
    expect(dockedRecords).toContain("info scroll.click-floor-restored");
  });

  it("writes no error through any answer", () => {
    expect([...clickedRecords, ...dockedRecords].filter((r) => r.startsWith("error "))).toEqual([]);
  });
});
