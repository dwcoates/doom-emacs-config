/**
 * A PAUSED MERGE QUEUE READS AT A GLANCE, IN WEBKIT (owner request,
 * 2026-10-23).
 *
 * The merge bubble's head draws a "paused" pill and its queue tab a "queue
 * paused" banner (src/feed/merge/paused.ts), both in --merge-paused, and the
 * bubble's frame takes the same amber while its head is paused. jsdom resolves
 * no `:has()` and lays nothing out, so this draws the markup the head and the
 * tab shape (pinned in test/feed/merge/merge.test.ts and queue.test.ts) in real
 * headless WebKit over the real stylesheet.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { pageStylesheet } from "./stylesheets";

/** The paused mark's markup, as drawPausedMark shapes it. */
const mark = (cls: string, word: string): string =>
  `<span class="merge-paused-mark ${cls}"><span class="merge-paused-glyph" aria-hidden="true"></span><span class="merge-paused-word">${word}</span></span>`;

/** A merge bubble whose head is PAUSED or not. */
function bubble(paused: boolean): string {
  return `
    <div class="tool-card bubble-fold" data-merge-bubble="">
      <div class="bubble-head">
        <div class="merge-head" data-state="update"${paused ? " data-paused" : ""}>
          <span class="merge-head-props"><span class="merge-glyph">⇄</span><span class="merge-label">fix → doom</span></span>
          <span class="merge-badge is-live">waiting</span>
          ${paused ? mark("merge-badge is-paused", "paused") : ""}
        </div>
      </div>
    </div>
    <div class="merge-queue list-rows" style="margin-top: 8px">${paused ? mark("merge-queue-paused", "queue paused") : ""}</div>`;
}

/** What one draw resolved. */
interface Looks {
  frame: string;
  pill: string | null;
  banner: string | null;
  amber: string;
  glyphBars: number;
}

async function look(page: Page, paused: boolean): Promise<Looks> {
  return page.evaluate((markup) => {
    const host = document.getElementById("host") as HTMLElement;
    host.innerHTML = markup;
    const color = (sel: string): string | null => {
      const el = host.querySelector(sel);
      return el === null ? null : getComputedStyle(el).color;
    };
    const swatch = document.createElement("span");
    swatch.style.color = "var(--merge-paused)";
    host.append(swatch);
    const amber = getComputedStyle(swatch).color;
    swatch.remove();
    const glyph = host.querySelector(".merge-paused-glyph");
    const bars =
      glyph === null
        ? 0
        : ["::before", "::after"].filter((p) => getComputedStyle(glyph, p).width !== "auto" && getComputedStyle(glyph, p).content !== "none").length;
    return {
      frame: getComputedStyle(host.querySelector(".tool-card") as Element).borderTopColor,
      pill: color(".merge-badge.is-paused"),
      banner: color(".merge-queue-paused"),
      amber,
      glyphBars: bars,
    };
  }, bubble(paused));
}

describe("a paused merge queue in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await pageStylesheet();
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 560, height: 160 } });
    await page.setContent(`<!doctype html><html><head></head><body><div id="host" style="width: 520px; padding: 8px"></div></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("draws the head's pill and the queue tab's banner in the paused amber", async () => {
    const m = await look(page, true);
    expect([m.pill, m.banner]).toEqual([m.amber, m.amber]);
  });

  it("tints the bubble's frame amber while its head is paused", async () => {
    const m = await look(page, true);
    expect(m.frame).toBe(m.amber);
  });

  it("leaves the frame its own border while the queue runs", async () => {
    const m = await look(page, false);
    expect(m.frame).not.toBe(m.amber);
  });

  it("draws the pause glyph as two bars", async () => {
    const m = await look(page, true);
    expect(m.glyphBars).toBe(2);
  });
});
