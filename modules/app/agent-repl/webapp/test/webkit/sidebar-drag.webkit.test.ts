/**
 * A SIDEBAR DRAG SELECTS NO TEXT, IN WEBKIT (owner request, 2026-10-23:
 * dragging an item highlighted every row between pickup and drop).
 *
 * jsdom neither lays out nor turns a press-drag into a selection, so this
 * drags the real mouse in headless WebKit, the Emacs webview's engine, over
 * the real stylesheet and the real drag (src/sidebar/drag.ts). The press
 * starts a selection before the threshold, as a hand's creep does, and the
 * carry holds the row at the list's bottom edge until the list has scrolled
 * to its end: WebKit extends a press's selection as the content scrolls under
 * it, with no mousemove at all.
 *
 *   - the drag leaves nothing selected;
 *   - the row lands where it was dropped;
 *   - once the drag ends, a row's text selects again.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

/**
 * Press row FROM just short of the middle of its third character and creep
 * across that middle, still short of the drag threshold, which starts
 * WebKit's own selection; then carry the row to TO's lower part, hold it
 * there until the list has scrolled to its end, and release.
 */
async function carry(page: Page, from: string, to: string): Promise<void> {
  const middle = await page.evaluate((id) => window.sidebarDragPage.charMiddle(id, 2), from);
  const end = await page.locator(`#${to}`).boundingBox();
  if (end === null) throw new Error(`cannot carry ${from} to ${to}: not laid out`);
  await page.mouse.move(middle.x - 1.5, middle.y);
  await page.mouse.down();
  await page.mouse.move(middle.x + 1.5, middle.y);
  await page.mouse.move(end.x + 60, end.y + end.height * 0.8, { steps: 12 });
  await page.waitForFunction(() => window.sidebarDragPage.scrolledToEnd(), undefined, { timeout: 2_000 });
  await page.mouse.up();
}

/** Drag-select row ID's first word with the mouse, outside any drag of the rail's. */
async function selectFirstWord(page: Page, id: string): Promise<void> {
  const first = await page.evaluate((row) => window.sidebarDragPage.charMiddle(row, 0), id);
  const space = await page.evaluate((row) => window.sidebarDragPage.charMiddle(row, 9), id);
  await page.mouse.move(first.x - 3, first.y);
  await page.mouse.down();
  await page.mouse.move(space.x - 1, space.y, { steps: 4 });
  await page.mouse.up();
}

/** What the drag, and a selection after it, left behind. */
interface Seen {
  dragSelection: string;
  order: string[];
  afterSelection: string;
}

describe("dragging a sidebar row in WebKit", () => {
  let browser: Browser;
  let seen: Seen;

  beforeAll(async () => {
    const [css, script] = await Promise.all([
      readFile(path.join(here, "../../src/styles.css"), "utf8"),
      bundlePage(path.join(here, "sidebar-drag-page.ts"), "sidebarDragPage"),
    ]);
    browser = await webkit.launch();
    const page = await browser.newPage();
    await page.setContent(
      `<!doctype html><html><head><style>${css}</style></head><body><div id="host"></div></body></html>`,
    );
    await page.addScriptTag({ content: script });
    await page.evaluate(() => window.sidebarDragPage.install());

    await carry(page, "one", "five");
    const dragSelection = await page.evaluate(() => window.sidebarDragPage.takeSelection());
    const order = await page.evaluate(() => window.sidebarDragPage.order());
    await selectFirstWord(page, "two");
    seen = { dragSelection, order, afterSelection: await page.evaluate(() => window.sidebarDragPage.takeSelection()) };
  });

  afterAll(async () => {
    await browser.close();
  });

  it("selects no text across the rows the drag passes", () => {
    expect(seen.dragSelection).toBe("");
  });

  it("drops the row where the pointer let go", () => {
    expect(seen.order).toEqual(["two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "one"]);
  });

  it("selects a row's text again once the drag ends", () => {
    expect(seen.afterSelection).toBe("workspace");
  });
});
