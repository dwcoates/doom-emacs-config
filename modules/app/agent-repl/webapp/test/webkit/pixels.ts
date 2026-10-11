/**
 * Screenshot a WebKit test's page and decode the PNG into its pixels. Shared by
 * every webkit suite that reads what the engine PAINTED rather than what it
 * laid out (a pseudo-element has no client rect, a snapped glyph no rect that
 * says where it landed). The page decodes its own screenshot through a canvas,
 * so no PNG decoder rides in node.
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
  const { width, height, data } = await page.evaluate(async (b64) => {
    const img = new Image();
    img.src = `data:image/png;base64,${b64}`;
    await img.decode();
    const canvas = document.createElement("canvas");
    canvas.width = img.width;
    canvas.height = img.height;
    const g = canvas.getContext("2d");
    if (g === null) throw new Error("no 2d context");
    g.drawImage(img, 0, 0);
    return { width: img.width, height: img.height, data: Array.from(g.getImageData(0, 0, img.width, img.height).data) };
  }, shot.toString("base64"));
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
