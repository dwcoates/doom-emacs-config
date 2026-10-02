/**
 * ALL TEXT IS SELECTABLE, AND SELECTING IT ACTIVATES NOTHING, IN WEBKIT (owner
 * ruling, 2026-10-02).
 *
 * jsdom neither lays out nor turns a press-drag-release into a selection and a
 * click, so this drags the real mouse in headless WebKit, the Emacs webview's
 * engine, over the real stylesheet and the real click guard
 * (src/selection.ts): a control's label a drag crosses is selected, and the
 * click that ends the drag reaches no handler, while a plain click still does.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { build, type Rollup } from "vite";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { protobufRuntimeAliases } from "../../protobuf-runtime-aliases";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The markup every drag runs over: prose with a control inside it. */
const MARKUP = `
  <p style="font-size: 20px">
    <span id="before">alpha alpha</span>
    <button id="control" type="button">Send now</button>
    <span id="after">charlie charlie</span>
  </p>`;

/** Bundle the page into ONE classic script, so it runs from `setContent` with no server. */
async function bundlePage(): Promise<string> {
  const out = await build({
    configFile: false,
    logLevel: "silent",
    resolve: { alias: protobufRuntimeAliases },
    build: {
      write: false,
      minify: false,
      lib: { entry: path.join(here, "selection-page.ts"), formats: ["iife"], name: "selectionPage" },
    },
  });
  const outputs = (Array.isArray(out) ? out : [out]) as Rollup.RollupOutput[];
  const chunk = outputs.flatMap((o) => o.output).find((o) => o.type === "chunk");
  if (chunk === undefined || chunk.type !== "chunk") throw new Error("the page bundle has no script chunk");
  return chunk.code;
}

/** Drag the mouse from FROM's left edge to TO's right edge, as a reader selects. */
async function drag(page: Page, from: string, to: string): Promise<void> {
  const start = await page.locator(from).boundingBox();
  const end = await page.locator(to).boundingBox();
  if (start === null || end === null) throw new Error(`cannot drag from ${from} to ${to}: not laid out`);
  await page.mouse.move(start.x + 2, start.y + start.height / 2);
  await page.mouse.down();
  await page.mouse.move(end.x + end.width - 2, end.y + end.height / 2, { steps: 8 });
  await page.mouse.up();
}

/** What one gesture left behind. */
interface Gesture {
  selection: string;
  clicks: number;
}

/** Run GESTURE on PAGE and read what it selected and how many clicks got through. */
async function observe(page: Page, gesture: () => Promise<void>): Promise<Gesture> {
  await gesture();
  const selection = await page.evaluate(() => window.selectionPage.takeSelection());
  const clicks = await page.evaluate(() => window.selectionPage.takeClicks());
  return { selection, clicks };
}

describe("selecting text in WebKit", () => {
  let browser: Browser;
  let acrossControl: Gesture;
  let plainClick: Gesture;

  beforeAll(async () => {
    const [css, script] = await Promise.all([
      readFile(path.join(here, "../../src/styles.css"), "utf8"),
      bundlePage(),
    ]);
    browser = await webkit.launch();
    const page = await browser.newPage();
    await page.setContent(
      `<!doctype html><html><head><style>${css}</style></head><body><div id="host">${MARKUP}</div></body></html>`,
    );
    await page.addScriptTag({ content: script });
    await page.evaluate(() => window.selectionPage.install());

    acrossControl = await observe(page, () => drag(page, "#before", "#after"));
    plainClick = await observe(page, () => page.locator("#control").click());
  });

  afterAll(async () => {
    await browser.close();
  });

  it("selects a control's label a drag runs across", () => {
    expect(acrossControl.selection).toContain("Send now");
  });

  it("activates nothing with the click that ends the drag", () => {
    expect(acrossControl.clicks).toBe(0);
  });

  it("still lets a plain click on the control through", () => {
    expect(plainClick.clicks).toBe(1);
  });
});
