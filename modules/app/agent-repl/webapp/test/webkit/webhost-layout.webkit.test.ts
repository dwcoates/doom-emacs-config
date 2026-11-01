/**
 * EMACS'S TWO WEB VIEWS LAY OUT AS THEY SHOULD, IN WEBKIT.
 *
 * The sidebar is a web view of its own (src/host/sidebar-page.ts), in an Emacs
 * window running the frame's whole height, and the rail fills it edge to edge.
 * A workspace page in the host keeps no sidebar column, so its main column
 * starts at the window's left edge. The host's gate (`#webhost-gate`,
 * src/host/gate.ts, measured into `--webhost-gate-band`) lays out exactly the
 * composer's slot, and the pages end exactly at its top. This lays them out in
 * real headless WebKit over the real stylesheets and compares the boxes.
 *
 * THE GATE IS THE COMPOSER'S EXACT HEIGHT (owner, 2026-10-29): the webhost's
 * gate lays out exactly the slot Emacs told, grows to fit a taller selection
 * without scrolling, and the pages end exactly at its top.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { DOCK_HEIGHT_PROPERTY } from "../../src/feed/gate-dock.js";
import { GATE_BAND_PROPERTY } from "../../src/host/gate.js";
import { HOST_ROOT_CLASS } from "../../src/host/main.js";
import { SIDEBAR_ROOT_CLASS } from "../../src/host/sidebar-page.js";
import { sidebarPageStylesheet, webhostStylesheet } from "./stylesheets";

/** The gate band's height when one stands, in px. */
const BAND = 160;

/** The viewport every layout is measured in, in px. */
const VIEWPORT = { width: 1200, height: 800 };

/** One box, in px. */
interface Box {
  left: number;
  top: number;
  width: number;
  height: number;
}

/** The page's shell: its (hidden) sidebar, its main column, its gate band. */
function pageShell(): string {
  return `<nav id="ws-sidebar" hidden></nav>
    <div id="main-col"><header id="topbar">topbar</header><main id="feed">feed</main></div>
    <div id="gate-dock" hidden>gate</div>`;
}

/** Lay out HTML with ROOT_CLASS on the root, and measure the element at SELECTOR. */
async function measure(page: Page, css: string, html: string, rootClass: string, selector: string): Promise<Box & { shadow: string; radius: string }> {
  await page.setContent(`<!doctype html><html class="${rootClass}"><head></head><body>${html}</body></html>`);
  await page.addStyleTag({ content: css });
  return page.evaluate((sel) => {
    const el = document.querySelector(sel)!;
    const r = el.getBoundingClientRect();
    const style = getComputedStyle(el);
    return { left: r.left, top: r.top, width: r.width, height: r.height, shadow: style.boxShadow, radius: style.borderTopLeftRadius };
  }, selector);
}

describe("Emacs's two web views in WebKit", () => {
  let browser: Browser;
  // Every layout is measured in the hook: a test body only reads what the hook
  // produced (vitest.webkit.config.ts).
  let rail: Box & { shadow: string; radius: string };
  let mainColumn: Box;
  /** The webhost's gate and the pages above it, with content SHORT or TALL. */
  const gated = new Map<"short" | "tall", { gate: Box; scrolls: boolean; frames: Box }>();

  beforeAll(async () => {
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: VIEWPORT });

    rail = await measure(page, await sidebarPageStylesheet(), `<nav id="ws-sidebar">rows</nav>`, SIDEBAR_ROOT_CLASS, "#ws-sidebar");

    const css = await webhostStylesheet();
    mainColumn = await measure(page, css, pageShell(), "", "#main-col");

    // THE WEBHOST'S GATE, laid out over the real stylesheets with content
    // shorter and taller than the told slot; the band is set as gate.ts sets
    // it, from the gate's own measured height.
    for (const [kind, contentHeight] of [["short", 40], ["tall", BAND + 120]] as const) {
      await page.setContent(
        `<!doctype html><html class="${HOST_ROOT_CLASS}"><head></head><body>
          <div class="webhost-frames"></div>
          <div id="webhost-gate" style="${DOCK_HEIGHT_PROPERTY}: ${BAND}px">
            <div class="cold-gate cold-gate-docked" data-state="standing"><div style="height: ${contentHeight}px"></div></div>
          </div></body></html>`,
      );
      await page.addStyleTag({ content: css });
      gated.set(
        kind,
        await page.evaluate((property) => {
          const box = (el: Element) => {
            const r = el.getBoundingClientRect();
            return { left: r.left, top: r.top, width: r.width, height: r.height };
          };
          const gate = document.getElementById("webhost-gate")!;
          document.documentElement.style.setProperty(property, `${gate.getBoundingClientRect().height}px`);
          return {
            gate: box(gate),
            scrolls: gate.scrollHeight > gate.clientHeight || gate.scrollWidth > gate.clientWidth,
            frames: box(document.querySelector(".webhost-frames")!),
          };
        }, GATE_BAND_PROPERTY),
      );
    }
  });

  afterAll(async () => {
    await browser.close();
  });

  it("fills the sidebar page's window with the rail, edge to edge", () => {
    expect({ left: rail.left, top: rail.top, width: rail.width, height: rail.height }).toEqual({
      left: 0,
      top: 0,
      width: VIEWPORT.width,
      height: VIEWPORT.height,
    });
  });

  it("draws the sidebar page's rail with no lift", () => {
    expect(rail.shadow).toBe("none");
  });

  it("draws the sidebar page's rail with square corners", () => {
    expect(rail.radius).toBe("0px");
  });

  it("starts a hosted page's main column at the window's left edge", () => {
    expect(mainColumn.left).toBe(0);
  });

  it("lays the webhost's gate out exactly the composer's told height", () => {
    expect(gated.get("short")!.gate.height).toBeCloseTo(BAND, 1);
  });

  it("grows the webhost's gate to fit a taller selection", () => {
    expect(gated.get("tall")!.gate.height).toBeGreaterThan(BAND + 100);
  });

  it.each(["short", "tall"] as const)("never scrolls the webhost's gate, with %s content", (kind) => {
    expect(gated.get(kind)!.scrolls).toBe(false);
  });

  it.each(["short", "tall"] as const)("ends the pages exactly at the gate's top, with %s content", (kind) => {
    const { gate, frames } = gated.get(kind)!;
    expect(frames.top + frames.height).toBeCloseTo(gate.top, 1);
  });
});
