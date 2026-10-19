/**
 * THE WEBHOST'S SIDEBAR SITS EXACTLY WHERE A PAGE'S OWN SIDEBAR WOULD, IN
 * WEBKIT.
 *
 * The sidebar is drawn once, by the webhost, over every workspace page
 * (src/host/sidebar.ts). The page keeps its sidebar column, empty and
 * invisible (`html.webhost-sidebar`), and the webhost places its sidebar over
 * that column, ending above the gate band of the page on screen
 * (`--webhost-sidebar-bottom`). Nothing about the look may change, so this
 * lays out all three in real headless WebKit over the real stylesheets and
 * compares the boxes: the webhost's sidebar against a page's own, and a
 * docked page's main column against a page drawing its own sidebar.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { DOCK_HEIGHT_PROPERTY } from "../../src/feed/gate-dock.js";
import { WEBHOST_SIDEBAR_CLASS } from "../../src/host/embed.js";
import { SIDEBAR_BOTTOM_PROPERTY } from "../../src/host/host.js";
import { HOST_ROOT_CLASS } from "../../src/host/main.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The gate band's height when one stands, in px. */
const BAND = 160;

/** One box, in px. */
interface Box {
  left: number;
  top: number;
  width: number;
  height: number;
}

/** The page's shell: its sidebar column, its main column, its gate band. */
function pageShell(sidebar: string, gate: boolean): string {
  return `<nav id="ws-sidebar"${sidebar === "" ? " hidden" : ""}>${sidebar}</nav>
    <div id="main-col"><header id="topbar">topbar</header><main id="feed">feed</main></div>
    <div id="gate-dock"${gate ? "" : " hidden"}>gate</div>`;
}

/** Lay out HTML with ROOT_CLASS on the root, and measure the element at SELECTOR. */
async function measure(page: Page, css: string, html: string, rootClass: string, rootStyle: string, selector: string): Promise<Box> {
  await page.setContent(
    `<!doctype html><html class="${rootClass}" style="${rootStyle}"><head></head><body>${html}</body></html>`,
  );
  await page.addStyleTag({ content: css });
  return page.evaluate((sel) => {
    const r = document.querySelector(sel)!.getBoundingClientRect();
    return { left: r.left, top: r.top, width: r.width, height: r.height };
  }, selector);
}

describe("the webhost's sidebar in WebKit", () => {
  let browser: Browser;
  // Every layout is measured in the hook: a test body only reads what the hook
  // produced (vitest.webkit.config.ts).
  const covered = new Map<boolean, { own: Box; webhost: Box }>();
  let column: { own: Box; docked: Box };
  let reservedVisibility: string;

  beforeAll(async () => {
    const styles = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    const host = await readFile(path.join(here, "../../src/host/host.css"), "utf8");
    const css = `${styles}\n${host}`;
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: { width: 1200, height: 800 } });
    const told = `${DOCK_HEIGHT_PROPERTY}: ${BAND}px`;

    for (const gate of [false, true]) {
      const own = await measure(page, css, pageShell("rows", gate), "", told, "#ws-sidebar");
      const webhost = await measure(
        page,
        css,
        `<div class="webhost-frames"></div><nav id="ws-sidebar">rows</nav>`,
        HOST_ROOT_CLASS,
        `${SIDEBAR_BOTTOM_PROPERTY}: ${gate ? BAND : 0}px`,
        "#ws-sidebar",
      );
      covered.set(gate, { own, webhost });
    }

    column = {
      own: await measure(page, css, pageShell("rows", true), "", told, "#main-col"),
      docked: await measure(page, css, pageShell("", true), WEBHOST_SIDEBAR_CLASS, told, "#main-col"),
    };

    await page.setContent(
      `<!doctype html><html class="${WEBHOST_SIDEBAR_CLASS}"><head></head><body>${pageShell("", false)}</body></html>`,
    );
    await page.addStyleTag({ content: css });
    reservedVisibility = await page.evaluate(() => getComputedStyle(document.getElementById("ws-sidebar")!).visibility);
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each([
    { name: "with no gate standing", gate: false },
    { name: "above a standing gate", gate: true },
  ])("covers exactly the box a page's own sidebar takes, $name", ({ gate }) => {
    const { own, webhost } = covered.get(gate)!;
    expect(webhost.left).toBeCloseTo(own.left, 1);
    expect(webhost.top).toBeCloseTo(own.top, 1);
    expect(webhost.width).toBeCloseTo(own.width, 1);
    expect(webhost.height).toBeCloseTo(own.height, 1);
  });

  it("leaves a docked page's main column where it is beside a sidebar of its own", () => {
    expect(column.docked).toEqual(column.own);
  });

  it("draws nothing in a docked page's reserved column", () => {
    expect(reservedVisibility).toBe("hidden");
  });
});
