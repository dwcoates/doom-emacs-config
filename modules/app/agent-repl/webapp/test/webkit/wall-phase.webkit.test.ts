/**
 * THE RAIL'S LOOPS RUN ON THE WALL CLOCK, IN WEBKIT.
 *
 * `src/sidebar/wall-phase.ts` seeks every looping rail animation to
 * `now mod period` after each draw, reading the animations through
 * `Element.getAnimations({ subtree: true })` and their periods from their own
 * timing. jsdom runs no animations, so this draws the rail's animated marks
 * over the real stylesheet in real headless WebKit — the engine the Emacs
 * webview runs — and checks the engine finds every one, takes the seek, and
 * keeps running from the phase it was given.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A wall-clock instant mid-cycle for every rail loop. */
const NOW = 1_700_000_000_500;

/** The rail's animated names: two breathing names and a reviving one. The marks stand still. */
const RAIL = `<nav id="ws-sidebar"><div class="row"><span class="st tone-red" data-glyph="dot"></span><span class="name breathes">a</span></div>
<div class="row"><span class="st st-glyph tone-purple" data-glyph="merge"></span><span class="name breathes">b</span></div>
<div class="row"><span class="st tone-green" data-glyph="dot"></span><span class="name reviving">c</span></div></nav>`;

/** What the seek left on one animated element. */
interface Sought {
  name: string;
  duration: number;
  currentTime: number;
}

/** Draw the rail, seek it at NOW with the real module, and read each animation. */
async function seekRail(page: Page): Promise<Sought[]> {
  return page.evaluate(
    ({ markup, now }) => {
      document.body.innerHTML = markup;
      const host = document.getElementById("ws-sidebar") as HTMLElement;
      type Api = { installWallPhase: (h: Element, env: { now: () => number }) => { seek(): void } };
      (window as unknown as { WallPhase: Api }).WallPhase.installWallPhase(host, { now: () => now }).seek();
      return host.getAnimations({ subtree: true }).map((animation) => ({
        name: (animation as CSSAnimation).animationName,
        duration: Number(animation.effect?.getTiming().duration),
        currentTime: Number(animation.currentTime),
      }));
    },
    { markup: RAIL, now: NOW },
  );
}

describe("the rail's loops on the wall clock in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    const script = await bundlePage(path.join(here, "wall-phase-page.ts"), "WallPhase");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 400 } });
    await page.setContent(`<!doctype html><html><head></head><body></body></html>`);
    await page.addStyleTag({ content: css });
    await page.addScriptTag({ content: script });
  });

  afterAll(async () => {
    await browser.close();
  });

  it("finds the two breathing names and the reviving shimmer", async () => {
    // Arrange / Act
    const sought = await seekRail(page);

    // Assert
    expect(sought.map((s) => s.name).sort()).toEqual(["ws-pulse", "ws-pulse", "ws-revive-shimmer"]);
  });

  // ONE ANIMATION (owner ruling, 2026-10-13): every breathing name breathes
  // with the same parameters.
  it("breathes every name alike", async () => {
    // Arrange / Act
    const breaths = (await seekRail(page)).filter((s) => s.name === "ws-pulse");

    // Assert
    expect(breaths.map((s) => s.duration)).toEqual([breaths[0]?.duration, breaths[0]?.duration]);
    expect(breaths[0]?.duration).toBeGreaterThan(0);
  });

  it("puts each one at the wall clock's phase in its own period", async () => {
    // Arrange / Act
    const sought = await seekRail(page);

    // Assert
    for (const s of sought) expect(s.currentTime).toBeCloseTo(NOW % s.duration, 0);
  });

  it("keeps a sought animation running on from its phase", async () => {
    // Arrange
    const [first] = await seekRail(page);

    // Act: two frames, so the compositor has advanced the animation.
    const later = await page.evaluate(
      () =>
        new Promise<number>((resolve) => {
          requestAnimationFrame(() =>
            requestAnimationFrame(() => {
              const host = document.getElementById("ws-sidebar") as HTMLElement;
              resolve(Number(host.getAnimations({ subtree: true })[0]?.currentTime));
            }),
          );
        }),
    );

    // Assert
    expect(later).toBeGreaterThan(first?.currentTime ?? Infinity);
  });
});
