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
import { afterAll, beforeAll, describe, expect, it } from "vitest";
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

/** Start a fresh page under the real pause, shown; the presence rides on `window.presence`. */
async function freshPage(page: Page, css: string, script: string): Promise<void> {
  await page.setContent(`<!doctype html><html><head></head><body></body></html>`);
  await page.addStyleTag({ content: css });
  await page.addScriptTag({ content: script });
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
}

/** Add an element of CLS (with STYLE) to the page; answers its id. */
async function add(page: Page, cls: string, style = ""): Promise<string> {
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
async function setShown(page: Page, shown: boolean): Promise<void> {
  await page.evaluate((s) => {
    (window as unknown as { presence: { setShown(shown: boolean): void } }).presence.setShown(s);
  }, shown);
}

/**
 * The animation of element ID, its computed opacity, and the timeline's time
 * now, once the animation has no pending play or pause. A pause the hide asks
 * for lands at the next frame, and until it does the animation's time still
 * runs: a read taken inside that window saw a time the held animation then
 * moved past, by however late a loaded machine drew that frame.
 */
async function read(page: Page, id: string): Promise<Read & { now: number; opacity: string }> {
  return page.evaluate(async (elId) => {
    const el = document.getElementById(elId)!;
    const animation = el.getAnimations()[0];
    await animation.ready;
    return {
      playState: animation.playState,
      currentTime: Number(animation.currentTime),
      now: Number(document.timeline.currentTime),
      opacity: getComputedStyle(el).opacity,
    };
  }, id);
}

/** Let MS of the page's own time pass. The assertions measure against its timeline, so a late wake costs nothing. */
async function wait(page: Page, ms: number): Promise<void> {
  await page.evaluate((t) => new Promise((resolve) => setTimeout(resolve, t)), ms);
}

/** Whether A is within SLACK_MS of B. */
const near = (a: number, b: number): boolean => Math.abs(a - b) <= SLACK_MS;

describe("pausing a page the webhost does not show, in WebKit", () => {
  let browser: Browser;
  // EVERY SCENARIO RUNS IN THE beforeAll, AND THE TESTS ONLY READ ITS RESULT,
  // as every webkit suite does (vitest.webkit.config.ts).
  let held: { first: Read; second: Read };
  let inline: Read;
  let runningResumed: { after: Read & { now: number }; expected: number };
  let madeResumed: { after: Read & { now: number }; expected: number };
  let oneShot: Read & { opacity: string };
  let footerWord: Read;

  beforeAll(async () => {
    const css = `${await readFile(path.join(here, "../../src/styles.css"), "utf8")}\n${KEYFRAMES}`;
    const script = await bundlePage(path.join(here, "unshown-animations-page.ts"), "UnshownAnimations");
    browser = await webkit.launch();
    const page = await browser.newPage({ viewport: { width: 400, height: 400 } });

    // A running loop, held across a hidden spell.
    await freshPage(page, css, script);
    let id = await add(page, "test-loop");
    await wait(page, 50);
    await setShown(page, false);
    const first = await read(page, id);
    await wait(page, 200);
    held = { first, second: await read(page, id) };

    // A loop drawn with an inline animation shorthand.
    await freshPage(page, css, script);
    id = await add(page, "", "animation: test-loop 1000ms linear infinite");
    await setShown(page, false);
    inline = await read(page, id);

    // A loop running at the hide, shown again.
    await freshPage(page, css, script);
    id = await add(page, "test-loop");
    await wait(page, 50);
    const before = await read(page, id);
    await setShown(page, false);
    await wait(page, 400);
    await setShown(page, true);
    let after = await read(page, id);
    runningResumed = { after, expected: before.currentTime + (after.now - before.now) };

    // A loop made while hidden, shown again.
    await freshPage(page, css, script);
    await setShown(page, false);
    await wait(page, 100);
    id = await add(page, "test-loop");
    const made = await read(page, id);
    await wait(page, 400);
    await setShown(page, true);
    after = await read(page, id);
    madeResumed = { after, expected: after.now - made.now };

    // A one-shot made while hidden that would have ended, shown again.
    await freshPage(page, css, script);
    await setShown(page, false);
    id = await add(page, "test-once");
    await wait(page, 500);
    await setShown(page, true);
    oneShot = await read(page, id);

    // The footer's breathing word, under the real stylesheet's rule.
    await freshPage(page, css, script);
    id = await add(page, "pfooter-wave-letter");
    await setShown(page, false);
    footerWord = await read(page, id);
  });

  afterAll(async () => {
    await browser.close();
  });

  it("holds a running loop still while the page is not shown", () => {
    expect([held.first.playState, held.second.currentTime]).toEqual(["paused", held.first.currentTime]);
  });

  it("holds a loop drawn with an inline animation shorthand still too", () => {
    expect(inline.playState).toBe("paused");
  });

  it("resumes a loop running at the hide where it would have been", () => {
    const { after, expected } = runningResumed;
    expect([after.playState, near(after.currentTime, expected)]).toEqual(["running", true]);
  });

  it("resumes a loop made while hidden where it would have been", () => {
    const { after, expected } = madeResumed;
    expect([after.playState, near(after.currentTime, expected)]).toEqual(["running", true]);
  });

  it("leaves a one-shot that would have ended while hidden ended, at its last frame", () => {
    expect([oneShot.playState, oneShot.opacity]).toEqual(["finished", "1"]);
  });

  it("pauses the footer's breathing word by the real stylesheet's rule", () => {
    expect(footerWord.playState).toBe("paused");
  });
});
