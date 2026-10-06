/**
 * NOTHING IN THE FEED SPANS PAST THE FEED STREAM AREA (owner rule,
 * 2026-10-06), in WebKit, the Emacs webview's engine.
 *
 * containment-page.ts mounts the real feed over one row of every kind the
 * contract declares, with every drawn text stretched into a long unbroken run,
 * over the real stylesheet; this suite asserts no row's box, and nothing its
 * content paints, reaches past the feed's stream area, and that the stream
 * never scrolls sideways. It also holds the page to the contract: a row kind
 * the page does not serve is a kind this guarantee does not cover, so the
 * served kinds must cover every FeedRow arm and every activity unit.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { FeedRowSchema, FeedTurnActivitySchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { ContainmentPage } from "./containment-page";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The FeedRow arms the feed never draws as a row of its own. */
const NOT_A_ROW: ReadonlySet<string> = new Set([
  // A shell's head and a removed row are drawn by a bubble's own chrome or by
  // nothing at all; the detached shell row stands for the shell.
  "shellHead",
  "removed",
]);

/** A oneof's arm names, in the generated spelling. */
function arms(schema: { oneofs: readonly { name: string; fields: readonly { localName: string }[] }[] }, oneof: string): string[] {
  const group = schema.oneofs.find((o) => o.name === oneof);
  if (group === undefined) throw new Error(`no oneof '${oneof}'`);
  return group.fields.map((f) => f.localName);
}

describe("the feed stays inside its stream area, in WebKit", () => {
  let browser: Browser;
  let drawn: number;
  let spills: { id: string; kind: string; over: number; culprit: string }[];
  let kinds: string[];
  let records: string[];
  let openMarkers: number;

  beforeAll(async () => {
    const [script, css] = await Promise.all([
      bundlePage(path.join(here, "containment-page.ts"), "containmentPage"),
      readFile(path.join(here, "../../src/styles.css"), "utf8"),
    ]);
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: { width: 700, height: 900 } });
    const errors: string[] = [];
    page.on("pageerror", (err) => errors.push(err.message));
    await page.setContent("<!doctype html><html><head></head><body></body></html>");
    await page.addStyleTag({ content: css });
    await page.addScriptTag({ content: script });
    await page.waitForFunction(() => (window as { containment?: ContainmentPage }).containment !== undefined);
    drawn = await page.evaluate(() => window.containment.mount());
    if (errors.length > 0) throw new Error(`the page failed: ${errors.join("; ")}`);
    spills = await page.evaluate(() => window.containment.spills());
    kinds = await page.evaluate(() => window.containment.kinds());
    records = await page.evaluate(() => window.containment.records());
    openMarkers = await page.evaluate(
      () => document.querySelectorAll('.outcome-marker-pill[aria-expanded="true"]').length,
    );
  }, 60_000);

  afterAll(async () => {
    await browser.close();
  });

  it("serves a row of every kind the contract declares", () => {
    const rowArms = arms(FeedRowSchema, "row").filter((arm) => arm !== "activity" && !NOT_A_ROW.has(arm));
    const unitArms = arms(FeedTurnActivitySchema, "unit").map((unit) => `activity.${unit}`);
    const missing = [...rowArms, ...unitArms].filter((kind) => !kinds.includes(kind));
    expect(missing).toEqual([]);
  });

  it("draws every row it served", () => {
    expect(drawn).toBe(kinds.length);
  });

  it("lets no row spill past the stream area", () => {
    expect(spills).toEqual([]);
  });

  it("measured the outcome markers open", () => {
    expect(openMarkers).toBeGreaterThan(0);
  });

  it("writes no error while drawing", () => {
    expect(records.filter((r) => r.startsWith("error "))).toEqual([]);
  });
});
