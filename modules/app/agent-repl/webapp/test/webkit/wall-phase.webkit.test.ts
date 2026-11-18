/**
 * THE RAIL'S LOOPS RUN ON THE WALL CLOCK, IN WEBKIT.
 *
 * `src/sidebar/wall-phase.ts` seeks every looping rail animation to
 * `now mod period` after each draw, reading each element's and pseudo-element's
 * loops off computed style and moving them by their delays. The suite reads
 * them back through `getAnimations({ subtree: true })`, which only a test
 * may ask for (the webapp's AGENTS.md, Performance lessons). jsdom runs no animations, so this draws the rail's animated marks
 * over the real stylesheet in real headless WebKit — the engine the Emacs
 * webview runs — and checks the engine finds every one, takes the seek, and
 * keeps running from the phase it was given.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";
import { pageStylesheet } from "./stylesheets";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A wall-clock instant mid-cycle for every rail loop. */
const NOW = 1_700_000_000_500;

/** A name's ink box, as name-ink.ts draws it: the copies its loops move. */
const ink = (name: string): string => `<span class="name-ink" aria-hidden="true" data-name="${name}"></span>`;

/**
 * The rail's animated names: two breathing names and a reviving one, each
 * with its ink box, whose copies (pseudo-elements) carry the loops. The marks
 * stand still.
 */
const RAIL = `<nav id="ws-sidebar"><div class="row"><span class="st tone-red" data-glyph="dot"></span><span class="name breathes">a${ink("a")}</span></div>
<div class="row"><span class="st st-glyph tone-purple" data-glyph="merge"></span><span class="name breathes">b${ink("b")}</span></div>
<div class="row"><span class="st tone-green" data-glyph="dot"></span><span class="name reviving">c${ink("c")}</span></div></nav>`;

/** What the seek left on one animated element: its position is its current time less its delay. */
interface Sought {
  name: string;
  duration: number;
  position: number;
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
        position: Number(animation.currentTime) - Number(animation.effect?.getTiming().delay),
      }));
    },
    { markup: RAIL, now: NOW },
  );
}

/**
 * Draw the rail and seek it at NOW, let WAIT_MS pass, then hide and show the
 * page so the reveal restarts and re-seeks every loop; read each loop two
 * frames after the reveal's own frame.
 */
async function revealRail(page: Page, waitMs: number): Promise<Sought[]> {
  return page.evaluate(
    async ({ markup, now, waitMs }) => {
      document.body.innerHTML = markup;
      const host = document.getElementById("ws-sidebar") as HTMLElement;
      type Presence = { setShown(shown: boolean): void };
      type Api = {
        createPresence: (doc: Document, embedded: boolean) => Presence;
        installWallPhase: (h: Element, env: { now: () => number; presence: Presence }) => { seek(): void };
      };
      const api = (window as unknown as { WallPhase: Api }).WallPhase;
      const presence = api.createPresence(document, true);
      presence.setShown(true);
      api.installWallPhase(host, { now: () => now, presence }).seek();
      await new Promise((resolve) => setTimeout(resolve, waitMs));
      presence.setShown(false);
      presence.setShown(true);
      const frame = (): Promise<void> => new Promise((resolve) => requestAnimationFrame(() => resolve()));
      await frame();
      await frame();
      await frame();
      return host.getAnimations({ subtree: true }).map((animation) => ({
        name: (animation as CSSAnimation).animationName,
        duration: Number(animation.effect?.getTiming().duration),
        position: Number(animation.currentTime) - Number(animation.effect?.getTiming().delay),
      }));
    },
    { markup: RAIL, now: NOW, waitMs },
  );
}

/** How long the rail runs before the reveal: far more than the frames after it. */
const BEFORE_REVEAL_MS = 400;

/** How far past its phase a loop restarted at the reveal may be read: the frames since. */
const REVEAL_SLACK_MS = 120;

describe("the rail's loops on the wall clock in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await pageStylesheet();
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

  // The copies are pseudo-elements, which the seek must reach through their
  // element.
  it("finds both copies of the two breathing names and the reviving shimmer's band and copy", async () => {
    // Arrange / Act
    const sought = await seekRail(page);

    // Assert
    expect(sought.map((s) => s.name).sort()).toEqual([
      "ws-pulse",
      "ws-pulse",
      "ws-pulse-rest",
      "ws-pulse-rest",
      "ws-revive-counter",
      "ws-revive-shimmer",
    ]);
  });

  // ONE ANIMATION (owner ruling, 2026-10-13): every breathing name breathes
  // with the same parameters.
  it("breathes every name alike", async () => {
    // Arrange / Act
    const breaths = (await seekRail(page)).filter((s) => s.name === "ws-pulse" || s.name === "ws-pulse-rest");

    // Assert
    expect(breaths.map((s) => s.duration)).toEqual(breaths.map(() => breaths[0]?.duration));
    expect(breaths[0]?.duration).toBeGreaterThan(0);
  });

  it("puts each one at the wall clock's phase in its own period", async () => {
    // Arrange / Act
    const sought = await seekRail(page);

    // Assert
    for (const s of sought) expect(s.position).toBeCloseTo(NOW % s.duration, 0);
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
              const animation = host.getAnimations({ subtree: true })[0];
              resolve(Number(animation?.currentTime) - Number(animation?.effect?.getTiming().delay));
            }),
          );
        }),
    );

    // Assert
    expect(later).toBeGreaterThan(first?.position ?? Infinity);
  });

  // A RUNNING loop moved by its delay would land BEFORE_REVEAL_MS past its
  // phase; only a loop the reveal made fresh lands on it.
  it("restarts every loop at the reveal, pseudo-elements' too, so each lands on its phase", async () => {
    // Arrange / Act
    const sought = await revealRail(page, BEFORE_REVEAL_MS);

    // Assert
    expect(sought.length).toBe(6);
    for (const s of sought) {
      const past = (((s.position - (NOW % s.duration)) % s.duration) + s.duration) % s.duration;
      expect(past).toBeLessThan(REVEAL_SLACK_MS);
    }
  });
});
