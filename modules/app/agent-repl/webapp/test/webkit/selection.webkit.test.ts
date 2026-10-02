/**
 * ALL TEXT IS SELECTABLE, AND SELECTING IT ACTIVATES NOTHING, IN WEBKIT (owner
 * ruling, 2026-10-02).
 *
 * jsdom neither lays out nor turns a press-drag-release into a selection and a
 * click, so this drags the real mouse in headless WebKit, the Emacs webview's
 * engine, over the real stylesheet, the real controls (src/control.ts) and the
 * real click guard (src/selection.ts):
 *
 *   - a drag ACROSS a control selects its label, and so does a drag that
 *     STARTS INSIDE one (which a `<button>` never allowed);
 *   - the click that ends either drag reaches no handler;
 *   - a plain click, Enter and Space each activate an enabled control once;
 *   - a disabled control activates on none of them.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

/** Drag the mouse from FROM's left edge (or INSET into it) to TO's right edge. */
async function drag(page: Page, from: string, to: string, inset = 2): Promise<void> {
  const start = await page.locator(from).boundingBox();
  const end = await page.locator(to).boundingBox();
  if (start === null || end === null) throw new Error(`cannot drag from ${from} to ${to}: not laid out`);
  await page.mouse.move(start.x + inset, start.y + start.height / 2);
  await page.mouse.down();
  await page.mouse.move(end.x + end.width - 2, end.y + end.height / 2, { steps: 8 });
  await page.mouse.up();
}

/** What one gesture left behind. */
interface Gesture {
  selection: string;
  clicks: number;
}

/** Run GESTURE on PAGE and read what it selected and how many clicks reached control ID. */
async function observe(page: Page, id: string, gesture: () => Promise<void>): Promise<Gesture> {
  await gesture();
  const selection = await page.evaluate(() => window.selectionPage.takeSelection());
  const clicks = await page.evaluate((target) => window.selectionPage.takeClicks(target), id);
  return { selection, clicks };
}

/** Focus control ID and press KEY on it. */
async function key(page: Page, id: string, name: string): Promise<void> {
  await page.evaluate((target) => window.selectionPage.focus(target), id);
  await page.keyboard.press(name);
}

describe("selecting text in WebKit", () => {
  let browser: Browser;
  const seen: Record<string, Gesture> = {};

  beforeAll(async () => {
    const [css, script] = await Promise.all([
      readFile(path.join(here, "../../src/styles.css"), "utf8"),
      bundlePage(path.join(here, "selection-page.ts"), "selectionPage"),
    ]);
    browser = await webkit.launch();
    const page = await browser.newPage();
    await page.setContent(
      `<!doctype html><html><head><style>${css}</style></head><body><div id="host"></div></body></html>`,
    );
    await page.addScriptTag({ content: script });
    await page.evaluate(() => window.selectionPage.install());

    seen.across = await observe(page, "control", () => drag(page, "#before", "#after"));
    seen.fromInside = await observe(page, "control", () => drag(page, "#control", "#after", 6));
    seen.click = await observe(page, "control", () => page.locator("#control").click());
    seen.enter = await observe(page, "control", () => key(page, "control", "Enter"));
    seen.space = await observe(page, "control", () => key(page, "control", "Space"));
    seen.disabledClick = await observe(page, "off", () => page.locator("#off").click({ force: true }));
    seen.disabledEnter = await observe(page, "off", () => key(page, "off", "Enter"));
  });

  afterAll(async () => {
    await browser.close();
  });

  it("selects a control's label a drag runs across", () => {
    expect(seen.across.selection).toContain("Send now");
  });

  it("activates nothing with the click that ends a drag across a control", () => {
    expect(seen.across.clicks).toBe(0);
  });

  it("selects text from a drag that starts inside a control", () => {
    expect(seen.fromInside.selection).toContain("charlie charlie");
  });

  it("activates nothing with the click that ends a drag started inside a control", () => {
    expect(seen.fromInside.clicks).toBe(0);
  });

  it("lets a plain click on an enabled control through", () => {
    expect(seen.click.clicks).toBe(1);
  });

  it("activates an enabled control on Enter", () => {
    expect(seen.enter.clicks).toBe(1);
  });

  it("activates an enabled control on Space", () => {
    expect(seen.space.clicks).toBe(1);
  });

  it("refuses a click on a disabled control", () => {
    expect(seen.disabledClick.clicks).toBe(0);
  });

  it("refuses Enter on a disabled control", () => {
    expect(seen.disabledEnter.clicks).toBe(0);
  });
});
