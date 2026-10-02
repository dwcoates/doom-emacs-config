/**
 * THE TOPBAR AS WEBKIT PAINTS IT (owner request, 2026-10-02).
 *
 * jsdom lays nothing out and paints nothing, so these draw the REAL topbar
 * cells (topbar-page.ts) in headless WebKit over the real stylesheet and
 * measure the painted result.
 *
 * THE WIFI GLYPH IS CENTERED IN ITS DISC. The owner saw the glyph sit "a
 * pixel or two" right of the disc's center. Measured before the fix: half a
 * CSS pixel right (one device pixel on a 2x screen), because the disc was the
 * chip's background and the glyph a separately placed svg, which WebKit snaps
 * to whole pixels while it paints the background where layout put it. The
 * test reads the painted pixels at fractional placements and both screen
 * densities, and compares the glyph's horizontal centroid with the disc's.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";
import type { TopbarPage } from "./topbar-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/**
 * The painted glyph may sit this far from the disc's center, in CSS px. The
 * glyph's own arcs are symmetric to a few hundredths of a unit, so anything
 * above this is placement, not shape; the defect measured 0.5.
 */
const CENTERING_TOLERANCE_PX = 0.1;

/** Fractional placements of the chip, in CSS px past a whole pixel. */
const OFFSETS = [0, 0.25, 0.5, 0.75];

/** The page and stylesheet one screen density's tests run against. */
interface Rig {
  page: Page;
  dpr: number;
}

/**
 * The horizontal centroids, in CSS px, of what the chip painted: the disc
 * (every pixel darker than the page) and the glyph (the green excess over red
 * and blue). The glyph is drawn inside the disc, so it blends only with black
 * and its green excess is proportional to its coverage.
 */
async function wifiCentroids(rig: Rig, offset: number): Promise<{ disc: number; glyph: number }> {
  await rig.page.evaluate((o) => window.topbarPage.drawWifi(o), offset);
  const shot = await rig.page.screenshot({ clip: { x: 0, y: 0, width: 80, height: 40 } });
  const dev = await rig.page.evaluate(async (b64) => {
    const img = new Image();
    img.src = `data:image/png;base64,${b64}`;
    await img.decode();
    const canvas = document.createElement("canvas");
    canvas.width = img.width;
    canvas.height = img.height;
    const g = canvas.getContext("2d");
    if (g === null) throw new Error("no 2d context");
    g.drawImage(img, 0, 0);
    const d = g.getImageData(0, 0, img.width, img.height).data;
    let discW = 0;
    let discX = 0;
    let glyphW = 0;
    let glyphX = 0;
    for (let y = 0; y < img.height; y++) {
      for (let x = 0; x < img.width; x++) {
        const k = (y * img.width + x) * 4;
        const [r, gr, b] = [d[k], d[k + 1], d[k + 2]];
        const cover = 1 - Math.min(r, gr, b) / 255;
        discW += cover;
        discX += cover * (x + 0.5);
        const green = Math.max(0, gr - Math.max(r, b));
        glyphW += green;
        glyphX += green * (x + 0.5);
      }
    }
    if (discW === 0 || glyphW === 0) throw new Error("the chip painted no disc or no glyph");
    return { disc: discX / discW, glyph: glyphX / glyphW };
  }, shot.toString("base64"));
  return { disc: dev.disc / rig.dpr, glyph: dev.glyph / rig.dpr };
}

describe.each([1, 2])("the topbar in WebKit at %ix", (dpr) => {
  let browser: Browser;
  const rig = { dpr } as Rig;

  beforeAll(async () => {
    const [script, css] = await Promise.all([
      bundlePage(path.join(here, "topbar-page.ts"), "topbarPage"),
      readFile(path.join(here, "../../src/styles.css"), "utf8"),
    ]);
    browser = await webkit.launch();
    rig.page = await browser.newPage({ viewport: { width: 1200, height: 200 }, deviceScaleFactor: dpr });
    const errors: string[] = [];
    rig.page.on("pageerror", (err) => errors.push(err.message));
    await rig.page.setContent(`<!doctype html><html><head></head><body style="margin:0;background:#fff"></body></html>`);
    await rig.page.addStyleTag({ content: css });
    await rig.page.addScriptTag({ content: script });
    await rig.page.waitForFunction(() => (window as { topbarPage?: TopbarPage }).topbarPage !== undefined);
    if (errors.length > 0) throw new Error(`the page failed to boot: ${errors.join("; ")}`);
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(OFFSETS)("centers the wifi glyph in its disc at +%fpx", async (offset) => {
    // Arrange / Act
    const c = await wifiCentroids(rig, offset);

    // Assert
    expect(Math.abs(c.glyph - c.disc)).toBeLessThanOrEqual(CENTERING_TOLERANCE_PX);
  });

  it("writes no error while drawing", async () => {
    const records = await rig.page.evaluate(() => window.topbarPage.records());
    expect(records.filter((r) => r.startsWith("error "))).toEqual([]);
  });
});
