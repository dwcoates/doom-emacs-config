/**
 * THE DOCKED GATE TAKES THE INPUT WINDOW'S HEIGHT, ITS CONTENT CENTERED,
 * IN WEBKIT (owner request, 2026-10-13), AND GROWS ONLY WHILE A TALLER GATE
 * NEEDS IT (owner request, 2026-10-29).
 *
 * While a cold gate stands, its card is moved into `#gate-dock`
 * (src/feed/gate-dock.ts), the slot the hidden Emacs input window had. The
 * dock is at least the height Emacs told (--gate-dock-height) and grows to
 * fit a taller gate; the card fills it, its title, token and model line and button row each centered on the
 * card's middle, and the spare height split evenly above the title and below
 * the buttons while the gaps between them stay what the undocked card draws.
 * jsdom lays nothing out, so this draws the card in the shape cold-gate.ts
 * draws it, in real headless WebKit over the real stylesheet, and measures.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { DOCK_HEIGHT_PROPERTY, DOCKED_CARD_CLASS } from "../../src/feed/gate-dock.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The input window's height Emacs tells the page, in px. */
const TOLD_HEIGHT = 160;

/** The standing card, drawn as cold-gate.ts draws it; SUBMENU opens the compact submenu. */
function card(docked: boolean, submenu: boolean): string {
  const choices = ["haiku", "sonnet", "opus", "this session's model"]
    .map((m) => `<label class="cold-gate-choice"><input type="radio" name="m"><span>${m}</span></label>`)
    .join("");
  return `<div class="cold-gate${docked ? ` ${DOCKED_CARD_CLASS}` : ""}" data-state="standing">
    <div class="hibernation-head"><span class="hibernation-heading">Cold context</span><span class="hibernation-since">last request 2h ago</span></div>
    <div class="hibernation-context">Resuming re-reads <span class="cold-gate-tokens">412k</span> tokens with claude-opus.</div>
    <div class="hibernation-actions">
      <div class="cold-gate-buttons"><ar-button class="hibernation-compact">Compact</ar-button><ar-button class="hibernation-clear">Clear</ar-button><ar-button class="hibernation-direct">Pay and resume</ar-button></div>
      <div class="cold-gate-submenu"${submenu ? "" : " hidden"}><div class="cold-gate-submenu-label">Summarizer</div><div class="cold-gate-choices list-rows">${choices}</div><div class="cold-gate-submenu-label">Scope</div><div class="cold-gate-choices list-rows">${choices}</div><ar-button class="hibernation-compact">Compact</ar-button></div>
      <div class="hibernation-pending" hidden></div>
    </div>
  </div>`;
}

/** One element's box, in px. */
interface Box {
  top: number;
  bottom: number;
  center: number;
}

/** What one drawn card measured. */
interface Drawn {
  dockHeight: number;
  dockScrolls: boolean;
  card: Box & { height: number };
  dockTop: number;
  dockInnerHeight: number;
  head: Box;
  lead: Box;
  buttons: Box;
}

/** Draw CARD in the dock (or, undocked, in the main column) and measure it. */
async function draw(page: Page, docked: boolean, submenu: boolean): Promise<Drawn> {
  return page.evaluate(
    ({ markup, docked }) => {
      const dock = document.getElementById("gate-dock")!;
      const inline = document.getElementById("inline")!;
      dock.innerHTML = docked ? markup : "";
      inline.innerHTML = docked ? "" : markup;
      dock.hidden = !docked;
      dock.scrollTop = 0;
      // The box of what the element DRAWS, not of the element: a full-width
      // block is centered on the card whatever its text does.
      const box = (el: Element): { top: number; bottom: number; center: number } => {
        const range = document.createRange();
        range.selectNodeContents(el);
        const r = range.getBoundingClientRect();
        return { top: r.top, bottom: r.bottom, center: (r.left + r.right) / 2 };
      };
      const c = (docked ? dock : inline).querySelector(".cold-gate")!;
      const d = dock.getBoundingClientRect();
      const cs = getComputedStyle(dock);
      return {
        dockHeight: d.height,
        dockScrolls: dock.scrollHeight > dock.clientHeight,
        card: (() => {
          const r = c.getBoundingClientRect();
          return { top: r.top, bottom: r.bottom, center: (r.left + r.right) / 2, height: r.height };
        })(),
        dockTop: d.top,
        dockInnerHeight: dock.clientHeight - parseFloat(cs.paddingTop) - parseFloat(cs.paddingBottom),
        head: box(c.querySelector(".hibernation-head")!),
        lead: box(c.querySelector(".hibernation-context")!),
        buttons: box(c.querySelector(".cold-gate-buttons")!),
      };
    },
    { markup: card(docked, submenu), docked },
  );
}

describe("the docked gate in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 900, height: 700 } });
    await page.setContent(
      `<!doctype html><html style="${DOCK_HEIGHT_PROPERTY}: ${TOLD_HEIGHT}px"><head></head><body><div id="main-col"><div id="inline"></div></div><div id="gate-dock" hidden></div></body></html>`,
    );
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("is the input window's height while the gate fits", async () => {
    const got = await draw(page, true, false);
    expect(got.dockHeight).toBeCloseTo(TOLD_HEIGHT, 1);
  });

  it("fills the dock with its card", async () => {
    const got = await draw(page, true, false);
    expect(got.card.height).toBeCloseTo(got.dockInnerHeight, 1);
  });

  it("centers the title on the card", async () => {
    const got = await draw(page, true, false);
    expect(got.head.center).toBeCloseTo(got.card.center, 0);
  });

  it("centers the token and model line on the card", async () => {
    const got = await draw(page, true, false);
    expect(got.lead.center).toBeCloseTo(got.card.center, 0);
  });

  it("centers the button row on the card", async () => {
    const got = await draw(page, true, false);
    expect(got.buttons.center).toBeCloseTo(got.card.center, 0);
  });

  it("keeps the button row centered while the compact submenu is open", async () => {
    const got = await draw(page, true, true);
    expect(got.buttons.center).toBeCloseTo(got.card.center, 0);
  });

  it("keeps the undocked card's gaps between title, line and buttons", async () => {
    const docked = await draw(page, true, false);
    const inline = await draw(page, false, false);
    const gaps = (d: Drawn): number[] => [d.lead.top - d.head.bottom, d.buttons.top - d.lead.bottom];
    expect(gaps(docked).map((g) => g.toFixed(1))).toEqual(gaps(inline).map((g) => g.toFixed(1)));
  });

  it("splits the spare height evenly above the title and below the buttons", async () => {
    const docked = await draw(page, true, false);
    const inline = await draw(page, false, false);
    const above = (d: Drawn): number => d.head.top - d.card.top;
    const below = (d: Drawn): number => d.card.bottom - d.buttons.bottom;
    const extraAbove = above(docked) - above(inline);
    const extraBelow = below(docked) - below(inline);
    expect(extraAbove).toBeGreaterThan(0);
    expect(extraAbove).toBeCloseTo(extraBelow, 0);
  });

  it("grows past the input window's height for a taller gate", async () => {
    const got = await draw(page, true, true);
    expect(got.dockHeight).toBeGreaterThan(TOLD_HEIGHT);
  });

  it("fits a taller gate without scrolling", async () => {
    const got = await draw(page, true, true);
    expect(got.dockScrolls).toBe(false);
  });

  it("shows a taller gate's top", async () => {
    const got = await draw(page, true, true);
    expect(got.card.top).toBeCloseTo(got.dockTop, 1);
  });

  it("stands at the input window's height once the submenu closes", async () => {
    await draw(page, true, true);
    const height = await page.evaluate(() => {
      const dock = document.getElementById("gate-dock")!;
      dock.querySelector<HTMLElement>(".cold-gate-submenu")!.hidden = true;
      return dock.getBoundingClientRect().height;
    });
    expect(height).toBeCloseTo(TOLD_HEIGHT, 1);
  });
});
