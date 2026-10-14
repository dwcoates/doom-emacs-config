/**
 * AN OPEN THINKING BUBBLE PAINTS EXACTLY WHAT A CLOSED ONE DOES, IN WEBKIT
 * (owner specification, 2026-10-14).
 *
 * A click on a root-feed thinking bubble only expands it: its scroll box takes
 * `.expanded`, its row takes no selection, and so its card never wears
 * `.entry-selected`. Nothing the stylesheet keys on the open box may then draw
 * a background, a border color, a shadow or an outline the closed bubble does
 * not wear. jsdom resolves no cascade, so this reads the computed paint in
 * real headless WebKit over the real stylesheet, against an interim response
 * whose open box DOES take the eggshell border, so the probe is shown to see
 * the rule it guards.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A capped response bubble of VARIANT (and CLASSES), its box open when OPEN. */
const bubbleHtml = (variant: string, classes: string, open: boolean): string => `
  <div class="feed-item" data-feed-row="r1">
    <div class="bubble md ${classes}" data-role="response" data-variant="${variant}" data-cap-lines="1" data-more="ellipsis" data-probe>
      <div class="bubble-box bubble-scroll${open ? " expanded" : ""}"><div class="bubble-body"><p>reasoning</p></div></div>
    </div>
  </div>`;

/** Every paint a highlight could arrive through. */
interface Paint {
  background: string;
  border: string;
  shadow: string;
  outline: string;
}

/** The probe bubble's computed paint, drawn from HTML. */
async function paintOf(page: Page, html: string): Promise<Paint> {
  return page.evaluate((markup) => {
    const feed = document.getElementById("feed");
    if (feed === null) throw new Error("the page's shell did not mount");
    feed.innerHTML = markup;
    const probe = feed.querySelector("[data-probe]");
    if (probe === null) throw new Error("the bubble drew no probe");
    const style = getComputedStyle(probe);
    return {
      background: style.backgroundColor,
      border: [style.borderTopColor, style.borderRightColor, style.borderBottomColor, style.borderLeftColor].join(" "),
      shadow: style.boxShadow,
      outline: `${style.outlineStyle} ${style.outlineColor}`,
    };
  }, html);
}

describe("an expanded thinking bubble in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 800, height: 800 } });
    await page.setContent(`<!doctype html><html><head></head><body>
      <div id="feed-scroll" class="scroll-zone"><main id="feed" data-feed="root"></main></div>
      </body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("paints the same background, border, shadow and outline open as closed", async () => {
    // Arrange
    const closed = await paintOf(page, bubbleHtml("thinking", "", false));
    // Act
    const open = await paintOf(page, bubbleHtml("thinking", "", true));
    // Assert
    expect(open).toEqual(closed);
  });

  it("is probed by a paint that does change: an interim response's open box takes the eggshell border", async () => {
    // Arrange
    const closed = await paintOf(page, bubbleHtml("response", "interim-response", false));
    // Act
    const open = await paintOf(page, bubbleHtml("response", "interim-response", true));
    // Assert
    expect(open.border).not.toBe(closed.border);
  });
});
