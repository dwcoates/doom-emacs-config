/**
 * A headless WebKit page holding an empty feed over the real stylesheet: the
 * shell every webkit suite that draws feed items into `#feed` and measures
 * them opens. The shell is the app's own nesting (`#main-col` >
 * `#feed-scroll.scroll-zone` > `main#feed`), so a feed rule keyed on any of
 * them matches here as it does in the app.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The page's markup: the feed column, with nothing in the feed. */
export const FEED_SHELL = `<!doctype html><html><head></head><body>
  <div id="main-col">
    <div id="feed-scroll" class="scroll-zone">
      <main id="feed" data-feed="root"></main>
    </div>
  </div></body></html>`;

/** How the page is opened. */
export interface FeedPageOptions {
  width: number;
  height: number;
  /** Device pixels per CSS pixel; WebKit's own default when omitted. */
  deviceScaleFactor?: number;
  /** Sheets added after the real stylesheet, in order, so each may override it. */
  styles?: readonly string[];
}

/** A launched browser and its one feed page; the caller closes the browser. */
export interface FeedPage {
  browser: Browser;
  page: Page;
}

/** Launch WebKit and open the feed shell over the real stylesheet, then OPTS.styles. */
export async function openFeedPage(opts: FeedPageOptions): Promise<FeedPage> {
  const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
  const browser = await webkit.launch();
  const page = await browser.newPage({
    viewport: { width: opts.width, height: opts.height },
    ...(opts.deviceScaleFactor === undefined ? {} : { deviceScaleFactor: opts.deviceScaleFactor }),
  });
  await page.setContent(FEED_SHELL);
  await page.addStyleTag({ content: css });
  for (const style of opts.styles ?? []) await page.addStyleTag({ content: style });
  return { browser, page };
}
