/**
 * THE FEED KEEPS THE READER'S CONTENT STILL, IN WEBKIT.
 *
 * The Emacs webview is WebKit, which has no native CSS scroll anchoring.
 *
 * - EVERY ROW IS LAID OUT AT ITS REAL HEIGHT (owner ruling, 2026-10-11): no
 *   row stands in a guessed height, so the first scroll up changes no row's
 *   height and the anchoring has nothing to correct. Under the retired
 *   `content-visibility: auto` the same pass drifted (measured 2026-09-27: 54
 *   of 120 steps, 10,431px) and leaned on corrections to hold still.
 * - A ROW ABOVE THE READER THAT DOES CHANGE HEIGHT (a late render, a card
 *   settling) is absorbed by the feed's own anchoring.
 *
 * This runs the real stylesheet and the real scroll module in real headless
 * WebKit (anchoring-page.ts) and asserts that no painted frame moves the
 * tracked row by a pixel more than the reader scrolled.
 *
 * Its browser is Playwright's WebKit build pinned by `playwright-core`
 * (package.json); `npx playwright-core install webkit` fetches it once.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import type { StepResult } from "./anchoring-page";
import { bootPage, pageAssets } from "./boot-page";

/** The reader's step, and how many of them one pass takes (as the reproduction did). */
const STEP_PX = 150;
const STEPS = 120;
/** Painted frames measured after each step: the correction cascade settles well inside it. */
const FRAMES_PER_STEP = 6;
/** A painted deviation at or under this is subpixel rounding, not a jump. */
const TOLERANCE_PX = 1;

/** One pass's drift, in the reproduction's terms. */
interface PassDrift {
  steps: number;
  total: number;
  max: number;
}

/** How far a row above the reader grows, and how many times. */
const GROW_PX = 230;
const GROWTHS = 10;

/** Fold one measured step into DRIFT when it moved past rounding. */
function addDrift(drift: PassDrift, step: StepResult): void {
  if (step.worst <= TOLERANCE_PX) return;
  drift.steps += 1;
  drift.total += step.worst;
  drift.max = Math.max(drift.max, step.worst);
}

/** DRIFT with its pixel figures rounded, as the assertions compare it. */
function rounded(drift: PassDrift): PassDrift {
  return { steps: drift.steps, total: Math.round(drift.total), max: Math.round(drift.max) };
}

/** Scroll the page's feed up STEPS times from its tail, summing what drifted. */
async function scrollUpPass(page: Page): Promise<PassDrift> {
  await page.evaluate(() => window.anchoring.park());
  const drift: PassDrift = { steps: 0, total: 0, max: 0 };
  for (let i = 0; i < STEPS; i++) {
    const step: StepResult = await page.evaluate(
      ([px, frames]) => window.anchoring.stepUp(px, frames),
      [STEP_PX, FRAMES_PER_STEP] as const,
    );
    addDrift(drift, step);
    if (step.top <= 0) break;
  }
  return rounded(drift);
}

/** Grow the row just above the reader GROWTHS times, summing what drifted. */
async function growAbovePass(page: Page): Promise<PassDrift> {
  const drift: PassDrift = { steps: 0, total: 0, max: 0 };
  for (let i = 0; i < GROWTHS; i++) {
    const step: StepResult = await page.evaluate(
      ([px, frames]) => window.anchoring.growAbove(px, frames),
      [GROW_PX, FRAMES_PER_STEP] as const,
    );
    addDrift(drift, step);
  }
  return rounded(drift);
}

/** How many anchoring corrections RECORDS hold. */
function corrections(records: string[]): number {
  return records.filter((r) => r === "debug scroll.anchor-corrected").length;
}

describe("the feed's scroll anchoring in WebKit", () => {
  let browser: Browser;
  /** The first upward pass over a fresh page, and every record it wrote. */
  let first: PassDrift;
  let firstRecords: string[];
  /** Rows above the reader growing after that pass, and every record since. */
  let grown: PassDrift;
  let grownRecords: string[];

  beforeAll(async () => {
    const assets = await pageAssets("anchoring-page.ts", "anchoringPage");
    browser = await webkit.launch();
    const { page } = await bootPage(browser, { ...assets, global: "anchoring", viewport: { width: 800, height: 800 } });
    first = await scrollUpPass(page);
    firstRecords = await page.evaluate(() => window.anchoring.records());
    grown = await growAbovePass(page);
    grownRecords = (await page.evaluate(() => window.anchoring.records())).slice(firstRecords.length);
  });

  afterAll(async () => {
    await browser.close();
  });

  it("moves the reader's content by nothing but their own scroll on the first pass up", () => {
    expect(first).toEqual({ steps: 0, total: 0, max: 0 });
  });

  it("changes no row's height on the first pass up, so nothing is corrected", () => {
    // A correction here would be a row standing in a guessed height.
    expect(corrections(firstRecords)).toBe(0);
  });

  it("holds the reader's content still while a row above them grows", () => {
    expect(grown).toEqual({ steps: 0, total: 0, max: 0 });
  });

  it("owes that stillness to the feed's own anchoring, which corrected the view", () => {
    // WebKit has no native anchoring, so a still pass with no correction would
    // mean the growth never reached the layout: a vacuous pass.
    expect(corrections(grownRecords)).toBeGreaterThan(0);
  });

  it("writes no error through either pass", () => {
    expect([...firstRecords, ...grownRecords].filter((r) => r.startsWith("error "))).toEqual([]);
  });
});
