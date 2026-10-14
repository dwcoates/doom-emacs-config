/**
 * AN OPEN THINKING BUBBLE OR INTERIM RESPONSE PAINTS EXACTLY WHAT A CLOSED ONE
 * DOES, IN WEBKIT (owner specification, 2026-10-14, extended to the interim the
 * same day).
 *
 * A click on a root-feed thinking bubble or interim response only expands it:
 * its scroll box takes
 * `.expanded`, its row takes no selection, and so its card never wears
 * `.entry-selected`. Nothing the stylesheet keys on the open box may then draw
 * a background, a border color, a shadow or an outline the closed bubble does
 * not wear. jsdom resolves no cascade, so this reads the computed paint in
 * real headless WebKit over the real stylesheet, against a capped response
 * that is neither (an agentic card) whose open box DOES take the eggshell
 * border, so the probe is shown to see the rule it guards.
 */
import type { Browser, Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { openFeedPage } from "./feed-page";

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

describe("an expanded thinking bubble or interim response in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    ({ browser, page } = await openFeedPage({ width: 800, height: 800 }));
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each([
    ["a thinking bubble", "thinking", ""],
    ["an interim response", "response", "interim-response"],
  ] as const)("paints %s the same background, border, shadow and outline open as closed", async (_kind, variant, classes) => {
    // Arrange
    const closed = await paintOf(page, bubbleHtml(variant, classes, false));
    // Act
    const open = await paintOf(page, bubbleHtml(variant, classes, true));
    // Assert
    expect(open).toEqual(closed);
  });

  it("is probed by a paint that does change: an agentic card's open box takes the eggshell border", async () => {
    // Arrange
    const closed = await paintOf(page, bubbleHtml("agentic", "", false));
    // Act
    const open = await paintOf(page, bubbleHtml("agentic", "", true));
    // Assert
    expect(open.border).not.toBe(closed.border);
  });
});
