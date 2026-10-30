/**
 * A WORKSPACE PAGE NOT YET PAINTED SHOWS THE WEBHOST'S BACKGROUND, IN WEBKIT.
 *
 * The webhost paints the background every workspace page shares, and stacks
 * the pages' iframes over it (src/host/host.css). Until a page paints, its
 * frame holds a blank document, which WebKit paints as an opaque canvas of
 * that document's own (light) scheme whenever the frame's color scheme
 * differs: in the dark theme every switch to a page still loading flashed a
 * white sheet, and the page then came up whole over it (owner, 2026-10-30).
 * This lays the webhost out over the real stylesheets in real headless WebKit
 * and reads the PAINTED pixels inside a shown frame: the webhost's background
 * through a frame whose page has not painted, in either theme, and the page's
 * own background through one whose page has.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { HOST_ROOT_CLASS, WEBHOST_FRAMES_CLASS } from "../../src/host/main.js";
import { screenshotPixels } from "./pixels";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The color a painted page draws, apart from either theme's background. */
const PAGE_RGB = [10, 120, 30];

/** One painted pixel's [r, g, b]. */
type Rgb = number[];

/** A webhost holding one shown workspace frame, whose page is SRCDOC (none: not yet painted). */
function webhost(srcdoc: string | null): string {
  const content = srcdoc === null ? "" : ` srcdoc="${srcdoc.replaceAll('"', "&quot;")}"`;
  return `<!doctype html><html class="${HOST_ROOT_CLASS}"><head></head><body>
    <div class="${WEBHOST_FRAMES_CLASS}"><iframe class="workspace-frame" data-shown${content}></iframe></div>
  </body></html>`;
}

/** A page that has painted: the real page's scheme, its own background. */
const PAINTED_PAGE = `<!doctype html><html style="color-scheme: light dark"><body style="margin: 0; height: 100vh; background: rgb(${PAGE_RGB.join(", ")})"></body></html>`;

describe("a shown workspace frame's canvas in WebKit", () => {
  let browser: Browser;
  // Every shot is taken in the hook: a test body only reads what the hook
  // produced (vitest.webkit.config.ts).
  const unpainted = new Map<"light" | "dark", { frame: Rgb; host: Rgb }>();
  let painted: Rgb;

  beforeAll(async () => {
    const styles = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    const host = await readFile(path.join(here, "../../src/host/host.css"), "utf8");
    const css = `${styles}\n${host}`;
    browser = await webkit.launch();

    for (const scheme of ["light", "dark"] as const) {
      const page = await browser.newPage({ viewport: { width: 400, height: 300 }, colorScheme: scheme });
      await page.setContent(webhost(null));
      await page.addStyleTag({ content: css });
      const host = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
      const channels = host.match(/\d+/g)?.slice(0, 3).map(Number);
      if (channels === undefined || channels.length !== 3) throw new Error(`the webhost's background is no rgb: ${host}`);
      const pixels = await screenshotPixels(page, { x: 200, y: 150, width: 1, height: 1 });
      unpainted.set(scheme, { frame: pixels.rgba(0, 0).slice(0, 3), host: channels });
      await page.close();
    }

    const page = await browser.newPage({ viewport: { width: 400, height: 300 }, colorScheme: "dark" });
    await page.setContent(webhost(PAINTED_PAGE));
    await page.addStyleTag({ content: css });
    painted = (await screenshotPixels(page, { x: 200, y: 150, width: 1, height: 1 })).rgba(0, 0).slice(0, 3);
    await page.close();
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(["light", "dark"] as const)("a frame whose page has not painted shows the webhost's background in the %s theme", (scheme) => {
    const seen = unpainted.get(scheme)!;
    expect(seen.frame).toEqual(seen.host);
  });

  it("a frame whose page has painted shows the page's own background", () => {
    expect(painted).toEqual(PAGE_RGB);
  });
});
