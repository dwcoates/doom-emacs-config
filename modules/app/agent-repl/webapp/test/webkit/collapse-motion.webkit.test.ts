/**
 * A WHOLE RESPONSE TURNING INTERIM EASES INTO IT, IN WEBKIT (owner request,
 * 2026-10-13): the interim bubble's height runs from the whole bubble's down to
 * its own over `COLLAPSE_MS`, rather than snapping (src/bubble/collapse-motion.ts).
 * jsdom runs no animation, so this measures the real engine.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { webkit, type Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import type { CollapseMotionPage } from "./collapse-motion-page";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("the whole-to-interim easing in WebKit", () => {
  let browser: Browser;
  let heights: number[];

  beforeAll(async () => {
    const script = await bundlePage(path.join(here, "collapse-motion-page.ts"), "collapseMotionPage");
    browser = await webkit.launch();
    const page = await browser.newPage();
    await page.setContent("<!doctype html><html><head></head><body></body></html>");
    await page.addScriptTag({ content: script });
    await page.waitForFunction(() => (window as { collapseMotion?: CollapseMotionPage }).collapseMotion !== undefined);
    heights = await page.evaluate(() => window.collapseMotion.heightsDuring([60, 600]));
  });

  afterAll(async () => {
    await browser.close();
  });

  it("is partway between the two heights while it eases", () => {
    expect(heights[0]).toBeGreaterThan(30);
    expect(heights[0]).toBeLessThan(180);
  });

  it("lands on the interim bubble's own height", () => {
    expect(heights[1]).toBe(30);
  });
});
