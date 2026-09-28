/**
 * THE FEED KEEPS THE READER'S CONTENT STILL ON THE FIRST SCROLL UP, IN WEBKIT.
 *
 * The Emacs webview is WebKit, which has no native CSS scroll anchoring. Every
 * `.feed-item` is `content-visibility: auto`, so a row above the reader has
 * never been laid out until the overscan band reaches it, and its first layout
 * changes its height from the stylesheet's guess to its real one. Chromium's
 * native anchoring absorbs that; WebKit moved the content under the reader
 * (measured 2026-09-27: 54 of 120 steps drifting, 10,431px, before the feed
 * owned its anchoring). The second pass was always still, because
 * `contain-intrinsic-size: auto` remembers each row's real size.
 *
 * This runs the real stylesheet and the real scroll module in real headless
 * WebKit (anchoring-page.ts) and asserts that no painted frame of the first
 * upward pass moves the tracked row by a pixel more than the reader scrolled.
 *
 * Its browser is Playwright's WebKit build pinned by `playwright-core`
 * (package.json); `npx playwright-core install webkit` fetches it once.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { build, type Rollup } from "vite";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { protobufRuntimeAliases } from "../../protobuf-runtime-aliases";
import type { AnchoringPage, StepResult } from "./anchoring-page";

const here = path.dirname(fileURLToPath(import.meta.url));

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

/** Bundle the page into ONE classic script, so it runs from `setContent` with no server. */
async function bundlePage(): Promise<string> {
  const out = await build({
    configFile: false,
    logLevel: "silent",
    resolve: { alias: protobufRuntimeAliases },
    build: {
      write: false,
      minify: false,
      lib: { entry: path.join(here, "anchoring-page.ts"), formats: ["iife"], name: "anchoringPage" },
    },
  });
  const outputs = (Array.isArray(out) ? out : [out]) as Rollup.RollupOutput[];
  const chunk = outputs.flatMap((o) => o.output).find((o) => o.type === "chunk");
  if (chunk === undefined || chunk.type !== "chunk") throw new Error("the page bundle has no script chunk");
  return chunk.code;
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
    if (step.worst > TOLERANCE_PX) {
      drift.steps += 1;
      drift.total += step.worst;
      drift.max = Math.max(drift.max, step.worst);
    }
    if (step.top <= 0) break;
  }
  return { steps: drift.steps, total: Math.round(drift.total), max: Math.round(drift.max) };
}

describe("the feed's scroll anchoring in WebKit", () => {
  let browser: Browser;
  /** The first upward pass over a fresh page, and every record the page wrote. */
  let first: PassDrift;
  let records: string[];

  beforeAll(async () => {
    const [script, css] = await Promise.all([
      bundlePage(),
      readFile(path.join(here, "../../src/styles.css"), "utf8"),
    ]);
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: { width: 800, height: 800 } });
    const errors: string[] = [];
    page.on("pageerror", (err) => errors.push(err.message));
    await page.setContent("<!doctype html><html><head></head><body></body></html>");
    await page.addStyleTag({ content: css });
    await page.addScriptTag({ content: script });
    await page.waitForFunction(() => (window as { anchoring?: AnchoringPage }).anchoring !== undefined);
    if (errors.length > 0) throw new Error(`the page failed to boot: ${errors.join("; ")}`);
    // Every row above the tail has never been laid out yet.
    first = await scrollUpPass(page);
    records = await page.evaluate(() => window.anchoring.records());
  });

  afterAll(async () => {
    await browser.close();
  });

  it("moves the reader's content by nothing but their own scroll on the first pass up", () => {
    expect(first).toEqual({ steps: 0, total: 0, max: 0 });
  });

  it("owes that stillness to the feed's own anchoring, which corrected the view", () => {
    // WebKit has no native anchoring, so a still pass with no correction would
    // mean the rows never changed height: a vacuous pass.
    expect(records.filter((r) => r === "debug scroll.anchor-corrected").length).toBeGreaterThan(0);
  });

  it("writes no error through the pass", () => {
    expect(records.filter((r) => r.startsWith("error "))).toEqual([]);
  });
});
