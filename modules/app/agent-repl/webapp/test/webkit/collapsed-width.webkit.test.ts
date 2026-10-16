/**
 * A COLLAPSED BUBBLE IS AS WIDE AS THE TEXT IT SHOWS, IN WEBKIT (owner
 * request, 2026-10-16: a bubble showing only "hello world" collapsed is never
 * as wide as a long line only its expanded view shows).
 *
 * Each case draws a capped bubble in real headless WebKit over the real
 * stylesheet, runs the REAL `refreshHasMore` (bundled) over its box, and
 * compares its width with a reference bubble holding only what the collapsed
 * one shows, drawn uncapped and never measured: the engine's own fit-content
 * width for exactly that text.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { Browser, Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { BUBBLE_STRIP_CLASS, BUBBLE_UNCAPPED, ELLIPSIS_CAP_LINES } from "../../src/bubble/draw.js";
import { BUBBLE_MORE_ELLIPSIS, BUBBLE_MORE_FADE } from "../../src/feed/bubble-more.js";
import { COLLAPSED_FIT_CLASS } from "../../src/feed/collapsed-width.js";
import { EXPANDED_CLASS } from "../../src/expand.js";
import { bundlePage } from "./bundle";
import { openFeedPage } from "./feed-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/** How far, in CSS px, a fitted width may sit from its reference: the fit rounds up to a whole pixel. */
const FIT_SLACK = 1.5;

const SHORT = "<p>hello world</p>";
const LONG = "<p>this is a very very very very very very very long line</p>";
const WRAPPING = `<p>${"a long paragraph that wraps across the cap ".repeat(12)}</p>`;
const LABEL = '<div class="BUBBLE_STRIP">a header label wider than the line below it</div>';

/** A prompt bubble: CAP lines, MORE signal, STRIP before its box, BODY inside it. */
function bubble(cap: string, more: string | null, body: string, strip = ""): string {
  const moreAttr = more === null ? "" : ` data-more="${more}"`;
  const boxClass = cap === BUBBLE_UNCAPPED ? "bubble-box" : "bubble-box bubble-scroll";
  return (
    `<div class="bubble md" data-role="prompt" data-variant="agent" data-cap-lines="${cap}"${moreAttr}>` +
    strip.replace("BUBBLE_STRIP", BUBBLE_STRIP_CLASS) +
    `<div class="${boxClass}"><div class="bubble-body">${body}</div></div></div>`
  );
}

/** Each case: the capped bubble, and the uncapped reference of what it shows. */
const CASES = {
  fade: { html: bubble("1", BUBBLE_MORE_FADE, SHORT + LONG), ref: bubble(BUBBLE_UNCAPPED, null, SHORT) },
  full: { html: bubble("1", BUBBLE_MORE_FADE, SHORT + LONG), ref: bubble(BUBBLE_UNCAPPED, null, SHORT + LONG) },
  strip: {
    html: bubble("1", BUBBLE_MORE_FADE, SHORT + LONG, LABEL),
    ref: bubble(BUBBLE_UNCAPPED, null, SHORT, LABEL),
  },
  wrapping: { html: bubble("1", BUBBLE_MORE_FADE, WRAPPING + LONG), ref: bubble(BUBBLE_UNCAPPED, null, WRAPPING) },
  ellipsis: {
    html: bubble(String(ELLIPSIS_CAP_LINES), BUBBLE_MORE_ELLIPSIS, SHORT + LONG),
    ref: bubble(BUBBLE_UNCAPPED, null, "<p>hello world…</p>"),
  },
} as const;

type CaseKey = keyof typeof CASES;

/** What one case laid out: the widths, the fit class, and what the collapsed box shows. */
interface Measured {
  collapsed: number;
  expanded: number;
  reference: number;
  fitted: boolean;
  /** The first paragraph's height over its line height: 1 when it stays one line. */
  firstLines: number;
  /** The collapsed bubble's first line, as the engine lays it out. */
  firstLine: string;
  /** The reference bubble's first line. */
  refFirstLine: string;
}

async function measure(page: Page, c: { html: string; ref: string }): Promise<Measured> {
  return page.evaluate(
    ({ html, ref, fit, expanded }) => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = `<div class="row" id="case">${html}</div><div class="row" id="ref">${ref}</div>`;
      const api = (
        window as unknown as {
          CollapsedWidth: {
            refreshHasMore(box: HTMLElement): void;
            domClampGeometry: { firstLine(clamp: HTMLElement): { ch: string }[] };
          };
        }
      ).CollapsedWidth;
      const bubble = document.querySelector<HTMLElement>("#case .bubble")!;
      const box = bubble.querySelector<HTMLElement>(".bubble-scroll")!;
      const body = box.querySelector<HTMLElement>(".bubble-body")!;
      api.refreshHasMore(box);
      const collapsed = bubble.getBoundingClientRect().width;
      const fitted = box.classList.contains(fit);
      const p = body.querySelector("p")!;
      const firstLines = p.getBoundingClientRect().height / Number.parseFloat(getComputedStyle(p).lineHeight);
      const lineOf = (el: HTMLElement): string =>
        api.domClampGeometry
          .firstLine(el)
          .map((c) => c.ch)
          .join("");
      const firstLine = lineOf(body);
      const refFirstLine = lineOf(document.querySelector<HTMLElement>("#ref .bubble-body")!);
      box.classList.add(expanded);
      api.refreshHasMore(box);
      return {
        collapsed,
        expanded: bubble.getBoundingClientRect().width,
        reference: document.querySelector("#ref .bubble")!.getBoundingClientRect().width,
        fitted,
        firstLines: Math.round(firstLines),
        firstLine,
        refFirstLine,
      };
    },
    { ...c, fit: COLLAPSED_FIT_CLASS, expanded: EXPANDED_CLASS },
  );
}

describe("a collapsed bubble's width in WebKit", () => {
  let browser: Browser;
  // THE LAYOUTS ARE THE beforeAll'S, AND THE TESTS ONLY READ THEM.
  const measured = {} as Record<CaseKey, Measured>;

  beforeAll(async () => {
    let page: Page;
    ({ browser, page } = await openFeedPage({ width: 1200, height: 900, deviceScaleFactor: 1 }));
    await page.addScriptTag({ content: await bundlePage(path.join(here, "bubble-more-page.ts"), "CollapsedWidth") });
    for (const key of Object.keys(CASES) as CaseKey[]) measured[key] = await measure(page, CASES[key]);
  });

  afterAll(async () => {
    await browser.close();
  });

  it("fits a collapsed fade bubble to the one line it shows", () => {
    // Arrange / Act
    const m = measured.fade;

    // Assert
    expect(m.fitted).toBe(true);
    expect(Math.abs(m.collapsed - m.reference)).toBeLessThanOrEqual(FIT_SLACK);
  });

  it("keeps the shown line on one line once fitted", () => {
    // Arrange / Act / Assert
    expect(measured.fade.firstLines).toBe(1);
  });

  it("widens an expanded bubble to the long line only it shows", () => {
    // Arrange / Act
    const m = measured.full;

    // Assert
    expect(m.expanded).toBeCloseTo(m.reference, 0);
    expect(m.expanded).toBeGreaterThan(m.collapsed + 50);
  });

  it("lets a header strip wider than the shown line widen the bubble", () => {
    // Arrange / Act
    const m = measured.strip;

    // Assert
    expect(Math.abs(m.collapsed - m.reference)).toBeLessThanOrEqual(FIT_SLACK);
    expect(m.collapsed).toBeGreaterThan(measured.fade.collapsed + 50);
  });

  it("never widens a bubble whose shown line wraps at the cap past the cap", () => {
    // Arrange / Act
    const m = measured.wrapping;

    // Assert
    expect(m.collapsed).toBeLessThanOrEqual(m.reference + FIT_SLACK);
  });

  it("keeps a wrapping line's break where the cap put it once fitted", () => {
    // Arrange / Act
    const m = measured.wrapping;

    // Assert
    expect(m.firstLine).toBe(m.refFirstLine);
  });

  it("fits a collapsed ellipsis bubble to its line and the ellipsis after it", () => {
    // Arrange / Act
    const m = measured.ellipsis;

    // Assert
    expect(m.fitted).toBe(true);
    expect(Math.abs(m.collapsed - m.reference)).toBeLessThanOrEqual(FIT_SLACK + 1);
  });

  it("keeps an ellipsis bubble's whole line once fitted", () => {
    // Arrange / Act / Assert
    expect(measured.ellipsis.firstLine).toBe("hello world");
  });
});
