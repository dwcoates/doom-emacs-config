/**
 * AN EXPANDED NON-PROMPT, NON-RESPONSE FEED ITEM IS NEVER TALLER THAN 80% OF
 * THE FEED, IN WEBKIT (owner request, 2026-09-30).
 *
 * The ceiling is one variable, `--feed-item-max-h: 80cqh` on `#feed-scroll`'s
 * size container, so it must follow the feed's height whatever changes it.
 * jsdom lays nothing out, so this draws each expandable item kind over far more
 * content than any feed holds, in real headless WebKit over the real
 * stylesheet, and measures it against the feed's visible height at two window
 * heights.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

/** Content far taller than any feed, so only the ceiling can bound the item. */
const TALL = `<div style="height: 4000px">content</div>`;

/** Each expandable item kind, expanded, as its drawing code shapes it. */
const ITEMS: Record<string, string> = {
  "a tool card": `
    <div class="tool-card tool-fold expanded" data-probe>
      <div class="tool-head">Bash</div>
      <div class="tool-output bash-output">${TALL}</div>
    </div>`,
  "a subagent's or detached work's bubble": `
    <div class="tool-card bubble-fold" data-expanded="true" data-probe>
      <div class="bubble-head">Explore</div>
      <div class="agent-panel bubble-subfeed">${TALL}</div>
    </div>`,
  "a detached shell with its tail open": `
    <div class="shell-bubble" data-probe>
      <div class="shell-head">npm test</div>
      <div class="shell-bubble-body">
        <div class="shell-command">npm test</div>
        <div class="shell-spool"><pre class="tool-output bash-output shell-tail expanded">${TALL}</pre></div>
      </div>
    </div>`,
  "a hook card with its output open": `
    <div class="tool-card" data-probe>
      <div class="tool-head">PreToolUse</div>
      <pre class="tool-output bash-output hook-output expanded">${TALL}</pre>
    </div>`,
};

/** The probe's height and 80% of the feed's visible height, drawn at HEIGHT. */
async function measure(page: Page, height: number, item: string): Promise<{ item: number; ceiling: number }> {
  await page.setViewportSize({ width: 800, height });
  return page.evaluate((html) => {
    const feed = document.getElementById("feed");
    const box = document.getElementById("feed-scroll");
    if (feed === null || box === null) throw new Error("the page's shell did not mount");
    feed.innerHTML = `<div class="feed-item" data-feed-row="r1">${html}</div>`;
    const probe = feed.querySelector("[data-probe]");
    if (probe === null) throw new Error("the item drew no probe");
    return { item: probe.getBoundingClientRect().height, ceiling: box.clientHeight * 0.8 };
  }, item);
}

describe("the expanded-item ceiling in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 800, height: 800 } });
    await page.setContent(`<!doctype html><html><head></head><body>
      <div id="main-col">
        <div id="feed-scroll" class="scroll-zone">
          <main id="feed" data-feed="root"></main>
        </div>
      </div></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(Object.entries(ITEMS).flatMap(([name, html]) => [800, 500].map((h) => [name, html, h] as const)))(
    "stops %s at 80%% of the feed in a %ipx-tall window",
    async (_name, html, height) => {
      const { item, ceiling } = await measure(page, height, html);
      expect(ceiling).toBeGreaterThan(0);
      expect(Math.abs(item - ceiling)).toBeLessThanOrEqual(1);
    },
  );
});
