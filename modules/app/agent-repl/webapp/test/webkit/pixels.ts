/**
 * Screenshot a WebKit test's page and decode the PNG into its pixels. Shared by
 * every webkit suite that reads what the engine PAINTED rather than what it
 * laid out (a pseudo-element has no client rect, a snapped glyph no rect that
 * says where it landed). The page decodes its own screenshot through a canvas,
 * so no PNG decoder rides in node.
 *
 * THE PIXELS CROSS BACK AS ONE BASE64 STRING OF RAW RGBA BYTES, never as an
 * array of numbers: an 800px-wide shot is about 640k channel values, and
 * serializing those as a JSON array took most of a second per shot, which put
 * every screenshot test over the suite's 850ms test timeout under a full run
 * (bubble-fade.webkit.test.ts, measured 2026-10-13).
 */
import type { Page } from "playwright-core";

/** A region of the page, in CSS px. */
export interface Clip {
  x: number;
  y: number;
  width: number;
  height: number;
}

/** A decoded screenshot, in DEVICE px, its origin at the clip's top-left corner. */
export interface Pixels {
  readonly width: number;
  readonly height: number;
  /** The [r, g, b, a] of the device pixel holding (X, Y); fractions floor to it. */
  rgba(x: number, y: number): [number, number, number, number];
}

/** PAGE's painted pixels inside CLIP, or the whole viewport without one. */
export async function screenshotPixels(page: Page, clip?: Clip): Promise<Pixels> {
  const shot = await page.screenshot(clip === undefined ? {} : { clip });
  const { width, height, rgba } = await page.evaluate(async (b64) => {
    const img = new Image();
    img.src = `data:image/png;base64,${b64}`;
    await img.decode();
    const canvas = document.createElement("canvas");
    canvas.width = img.width;
    canvas.height = img.height;
    const g = canvas.getContext("2d");
    if (g === null) throw new Error("no 2d context");
    g.drawImage(img, 0, 0);
    const bytes = g.getImageData(0, 0, img.width, img.height).data;
    // btoa takes a binary string; build it in chunks, as one spread of the
    // whole array overflows the argument limit.
    let binary = "";
    for (let i = 0; i < bytes.length; i += 0x8000) {
      binary += String.fromCharCode(...bytes.subarray(i, i + 0x8000));
    }
    return { width: img.width, height: img.height, rgba: btoa(binary) };
  }, shot.toString("base64"));
  const data = Buffer.from(rgba, "base64");
  return {
    width,
    height,
    rgba(x, y) {
      const px = Math.floor(x);
      const py = Math.floor(y);
      if (px < 0 || py < 0 || px >= width || py >= height) {
        throw new Error(`pixel (${x}, ${y}) is outside the ${width}x${height} shot`);
      }
      const k = (py * width + px) * 4;
      return [data[k] ?? 0, data[k + 1] ?? 0, data[k + 2] ?? 0, data[k + 3] ?? 0];
    },
  };
}
