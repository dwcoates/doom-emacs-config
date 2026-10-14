/**
 * A PAGE THE WEBHOST IS NOT SHOWING RUNS NO ANIMATIONS, AND RESUMES EACH WHERE
 * IT WOULD HAVE BEEN, IN WEBKIT.
 *
 * `src/host/unshown-animations.ts` pauses every animation under a root flag
 * the real stylesheet keys on, and on show moves each one on by the time it
 * was frozen. jsdom runs no animations, and the order the show takes is the
 * engine's own behavior (a finished one-shot rewinds when its play state
 * changes), so this runs the real module over the real stylesheet in headless
 * WebKit, the engine the Emacs web view runs.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

/** How far a resumed time may sit from the one expected: a frame and a little. */
const SLACK_MS = 40;

/** Test-only keyframes: a loop, and a one-shot that ends 300ms after it is made. */
const KEYFRAMES = `
@keyframes test-loop { from { opacity: 0 } to { opacity: 1 } }
@keyframes test-once { from { opacity: 0 } to { opacity: 1 } }
.test-loop { animation: test-loop 1000ms linear infinite; }
.test-once { opacity: 0; animation: test-once 200ms linear 100ms forwards; }`;

/** One animation's state, read in the page. */
interface Read {
  playState: string;
  currentTime: number;
}

describe("pausing a page the webhost does not show, in WebKit", () => {
  let browser: Browser;
  let page: Page;
  let script: string;
  let css: string;

  beforeAll(async () => {
    css = `${await readFile(path.join(here, "../../src/styles.css"), "utf8")}\n${KEYFRAMES}`;
    script = await bundlePage(path.join(here, "unshown-animations-page.ts"), "UnshownAnimations");
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 400 } });
  });

  beforeEach(async () => {
    await page.setContent(`<!doctype html><html><head></head><body></body></html>`);
    await page.addStyleTag({ content: css });
    await page.addScriptTag({ content: script });
    // The page under test: a presence the test drives, and the pause over it.
    await page.evaluate(() => {
      type Api = {
        createPresence: (doc: Document, embedded: boolean) => { setShown(shown: boolean): void };
        liveUnshownAnimationsEnv: (doc: Document, presence: unknown) => unknown;
        pauseAnimationsWhileUnshown: (env: unknown) => () => void;
      };
      const api = (window as unknown as { UnshownAnimations: Api }).UnshownAnimations;
      const presence = api.createPresence(document, true);
      presence.setShown(true);
      api.pauseAnimationsWhileUnshown(api.liveUnshownAnimationsEnv(document, presence));
      (window as unknown as { presence: unknown }).presence = presence;
    });
  });

  afterAll(async () => {
    await browser.close();
  });

  /** Add an element of CLASS (with STYLE) to the page; answers its id. */
  async function add(cls: string, style = ""): Promise<string> {
    return page.evaluate(
      ({ cls, style }) => {
        const el = document.createElement("div");
        el.id = `el-${document.body.children.length}`;
        el.className = cls;
        if (style !== "") el.setAttribute("style", style);
        document.body.append(el);
        return el.id;
      },
      { cls, style },
    );
  }

  /** Show or hide the page, as the webhost would. */
  async function setShown(shown: boolean): Promise<void> {
    await page.evaluate((s) => {
      (window as unknown as { presence: { setShown(shown: boolean): void } }).presence.setShown(s);
    }, shown);
  }

  /** The animation of element ID, and the timeline's time now. */
  async function read(id: string): Promise<Read & { now: number }> {
    return page.evaluate((elId) => {
      const animation = document.getElementById(elId)!.getAnimations()[0];
      return {
        playState: animation.playState,
        currentTime: Number(animation.currentTime),
        now: Number(document.timeline.currentTime),
      };
    }, id);
  }

  /** Let MS of real time pass in the page. */
  async function wait(ms: number): Promise<void> {
    await page.evaluate((t) => new Promise((resolve) => setTimeout(resolve, t)), ms);
  }

  it("holds a running loop still while the page is not shown", async () => {
    // Arrange
    const id = await add("test-loop");
    await wait(50);

    // Act
    await setShown(false);
    const first = await read(id);
    await wait(200);
    const second = await read(id);

    // Assert
    expect([first.playState, second.currentTime]).toEqual(["paused", first.currentTime]);
  });

  it("holds a loop drawn with an inline animation shorthand still too", async () => {
    // Arrange
    const id = await add("", "animation: test-loop 1000ms linear infinite");
    await setShown(false);

    // Act
    const state = await read(id);

    // Assert
    expect(state.playState).toBe("paused");
  });

  it("resumes a loop running at the hide where it would have been", async () => {
    // Arrange
    const id = await add("test-loop");
    await wait(50);
    const before = await read(id);
    await setShown(false);
    await wait(400);

    // Act
    await setShown(true);
    const after = await read(id);

    // Assert
    const expected = before.currentTime + (after.now - before.now);
    expect([after.playState, Math.abs(after.currentTime - expected) <= SLACK_MS]).toEqual(["running", true]);
  });

  it("resumes a loop made while hidden where it would have been", async () => {
    // Arrange
    await setShown(false);
    await wait(100);
    const id = await add("test-loop");
    const made = await read(id);
    await wait(400);

    // Act
    await setShown(true);
    const after = await read(id);

    // Assert
    const expected = after.now - made.now;
    expect([after.playState, Math.abs(after.currentTime - expected) <= SLACK_MS]).toEqual(["running", true]);
  });

  it("leaves a one-shot that would have ended while hidden ended, at its last frame", async () => {
    // Arrange
    await setShown(false);
    const id = await add("test-once");
    await wait(500);

    // Act
    await setShown(true);
    const after = await read(id);
    const opacity = await page.evaluate((elId) => getComputedStyle(document.getElementById(elId)!).opacity, id);

    // Assert
    expect([after.playState, opacity]).toEqual(["finished", "1"]);
  });

  it("pauses the footer's breathing word by the real stylesheet's rule", async () => {
    // Arrange
    const id = await add("pfooter-wave-letter");
    await setShown(false);

    // Act
    const state = await read(id);

    // Assert
    expect(state.playState).toBe("paused");
  });
});
