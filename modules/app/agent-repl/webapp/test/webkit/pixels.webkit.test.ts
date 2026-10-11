/**
 * The shared screenshot decoder (pixels.ts): every webkit suite that reads
 * painted pixels reads them through it, in device px from the clip's corner.
 */
import path from "node:path";
import { readdir, readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { screenshotPixels } from "./pixels";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A white page with one 10x10 pure red square at (20, 30). */
const PAGE = `<!doctype html><html><body style="margin:0;background:#fff">
  <div style="position:absolute;left:20px;top:30px;width:10px;height:10px;background:rgb(255,0,0)"></div>
  </body></html>`;

describe("screenshotPixels", () => {
  let browser: Browser;
  let pages: Record<1 | 2, Page>;

  beforeAll(async () => {
    browser = await webkit.launch();
    const open = async (dpr: number): Promise<Page> => {
      const page = await browser.newPage({ viewport: { width: 100, height: 100 }, deviceScaleFactor: dpr });
      await page.setContent(PAGE);
      return page;
    };
    pages = { 1: await open(1), 2: await open(2) };
  });

  afterAll(async () => {
    await browser.close();
  });

  it("decodes the whole viewport without a clip", async () => {
    // Arrange / Act
    const px = await screenshotPixels(pages[1]);

    // Assert
    expect([px.width, px.height, px.rgba(25, 35), px.rgba(5, 5)]).toEqual([
      100,
      100,
      [255, 0, 0, 255],
      [255, 255, 255, 255],
    ]);
  });

  it("puts the origin at the clip's top-left corner", async () => {
    // Arrange / Act
    const px = await screenshotPixels(pages[1], { x: 20, y: 30, width: 10, height: 10 });

    // Assert
    expect([px.width, px.height, px.rgba(0, 0), px.rgba(9, 9)]).toEqual([10, 10, [255, 0, 0, 255], [255, 0, 0, 255]]);
  });

  it("measures in device px on a 2x screen", async () => {
    // Arrange / Act
    const px = await screenshotPixels(pages[2], { x: 10, y: 20, width: 20, height: 20 });

    // Assert — the square starts 10 CSS px (20 device px) into the clip.
    expect([px.width, px.rgba(19, 19), px.rgba(20, 20)]).toEqual([40, [255, 255, 255, 255], [255, 0, 0, 255]]);
  });

  it("floors a fractional position to the pixel holding it", async () => {
    // Arrange / Act
    const px = await screenshotPixels(pages[1], { x: 15, y: 25, width: 10, height: 10 });

    // Assert
    expect([px.rgba(4.9, 4.9), px.rgba(5.1, 5.1)]).toEqual([
      [255, 255, 255, 255],
      [255, 0, 0, 255],
    ]);
  });

  it("refuses a pixel outside the shot", async () => {
    // Arrange
    const px = await screenshotPixels(pages[1], { x: 0, y: 0, width: 10, height: 10 });

    // Act / Assert
    expect(() => px.rgba(10, 0)).toThrow("pixel (10, 0) is outside the 10x10 shot");
  });

  it("is the only screenshot any webkit suite takes", async () => {
    // Arrange — every webkit source but the helper itself.
    const files = (await readdir(here)).filter((f) => f.endsWith(".ts") && f !== "pixels.ts");

    // Act
    const shooting: string[] = [];
    for (const f of files) {
      if (/\.screenshot\(/.test(await readFile(path.join(here, f), "utf8"))) shooting.push(f);
    }

    // Assert
    expect(shooting).toEqual([]);
  });
});
