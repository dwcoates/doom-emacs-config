/**
 * BOOT A WEBKIT PAGE over the real stylesheet: the one sequence every suite
 * that drives a bundled page module runs before it measures anything.
 *
 * A fresh page at the suite's viewport, an empty document, the stylesheet,
 * then the page's script, waiting until the script has published its API on
 * `window[global]`. An error the page throws while it boots fails the boot,
 * loudly, rather than leaving a suite to measure a page that never mounted.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { Browser, Page } from "playwright-core";
import { bundlePage } from "./bundle";
import { pageStylesheet } from "./stylesheets";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The page module ENTRY (beside this file) bundled as NAME, and the real stylesheet. */
export async function pageAssets(entry: string, name: string): Promise<{ script: string; css: string }> {
  const [script, css] = await Promise.all([
    bundlePage(path.join(here, entry), name),
    pageStylesheet(),
  ]);
  return { script, css };
}

/** How a suite boots its page. */
export interface BootOptions {
  /** The page's bundled script. */
  script: string;
  /** The stylesheet laid under it. */
  css: string;
  /** The `window` property the script publishes its API on once it has mounted. */
  global: string;
  /** The page's viewport. */
  viewport: { width: number; height: number };
  /** The page's device pixel ratio, when the suite reads pixels at one. */
  deviceScaleFactor?: number;
  /** The empty body's inline style, when the suite needs one. */
  bodyStyle?: string;
}

/**
 * Boot a page in BROWSER as OPTIONS says. Answers the page and the list every
 * later page error lands in, for a suite that checks it again after it drives
 * the page. Throws when the page errored while booting.
 */
export async function bootPage(browser: Browser, options: BootOptions): Promise<{ page: Page; errors: string[] }> {
  const page = await browser.newPage({
    viewport: options.viewport,
    ...(options.deviceScaleFactor === undefined ? {} : { deviceScaleFactor: options.deviceScaleFactor }),
  });
  const errors: string[] = [];
  page.on("pageerror", (err) => errors.push(err.message));
  const body = options.bodyStyle === undefined ? "<body>" : `<body style="${options.bodyStyle}">`;
  await page.setContent(`<!doctype html><html><head></head>${body}</body></html>`);
  await page.addStyleTag({ content: options.css });
  await page.addScriptTag({ content: options.script });
  await page.waitForFunction(
    (global) => (window as unknown as Record<string, unknown>)[global] !== undefined,
    options.global,
  );
  if (errors.length > 0) throw new Error(`the page failed to boot: ${errors.join("; ")}`);
  return { page, errors };
}
