/**
 * THE FEED TEXT ZOOM CHANGES ONLY THE TEXT (owner ruling, 2026-10-15), in
 * WebKit, the Emacs webview's engine.
 *
 * `C-+` / `C--` move `--feed-text-scale`, which multiplies into feed
 * font-sizes alone. Every bubble keeps the breadth its content has at 100%
 * (src/bubble/body.ts `holdBreadth`) — a short one as much as one at its cap —
 * and a card keeps the column's, so a zoom leaves every box's horizontal
 * geometry where it was and re-flows only the text inside it: prose re-wraps
 * on its own, and a metaprompt tree, wrapped to a measured column budget, is
 * re-wrapped to the columns the held breadth holds at the new size.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { Browser, Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";
import { openFeedPage } from "./feed-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The zooms measured, each against the unzoomed feed. */
const SCALES = [0.7, 1, 1.6] as const;

/** A paragraph far longer than the cap at every measured zoom. */
const PROSE = "The feed text zoom changes the size of the text and nothing else. ".repeat(12);

/** A prompt far shorter than the cap at every measured zoom. */
const SHORT = "hello there";

/** A tree whose longest branch is well under the cap at 100%. */
const SHORT_TREE = ["1 🌳 A short tree", "├── 1.1 A short branch", "└── 1.2 Another short branch"].join("\n");

/** A tree whose branches run far past the cap at every measured zoom. */
const TREE = [
  "Response (✏️ changes made)",
  "",
  `1 🔧 ${"A branch that runs far past the bubble's cap at every zoom the suite measures. ".repeat(4)}`,
  `├── 1.1 ${"Another long branch that must wrap onto continuation lines of its own. ".repeat(4)}`,
  `└── 1.2 ${"A last long branch, also wrapped at the cap the bubble holds. ".repeat(4)}`,
].join("\n");

/** What one zoom drew. */
interface Drawn {
  /** Each measured box's left and right edges, by name. */
  edges: Record<string, [number, number]>;
  /** How many lines the tree wrapped into. */
  treeLines: number;
  /** Tree lines the engine itself broke, which the tree wrapper never should leave it to. */
  brokenTreeLines: number;
}

/** Zoom the feed to SCALE, let a frame deliver the re-wrap, and measure. */
async function drawnAt(page: Page, scale: number): Promise<Drawn> {
  return page.evaluate(async (s) => {
    document.documentElement.style.setProperty("--feed-text-scale", String(s));
    for (let i = 0; i < 3; i++) await new Promise((resolve) => requestAnimationFrame(() => resolve(null)));
    const edge = (sel: string): [number, number] => {
      const el = document.querySelector(sel);
      if (el === null) throw new Error(`nothing drew ${sel}`);
      const r = el.getBoundingClientRect();
      return [r.left, r.right];
    };
    const lines = [...document.querySelectorAll<HTMLElement>("#tree .mp-tree .mp-line")];
    const lineHeight = (el: HTMLElement): number => Number.parseFloat(getComputedStyle(el).lineHeight);
    return {
      edges: {
        prose: edge("#prose > .bubble"),
        tree: edge("#tree > .bubble"),
        short: edge("#short > .bubble"),
        shortTree: edge("#short-tree > .bubble"),
        card: edge("#card > .tool-card"),
      },
      treeLines: lines.length,
      brokenTreeLines: lines.filter((el) => el.getBoundingClientRect().height > lineHeight(el) * 1.5).length,
    };
  }, scale);
}

describe("the feed text zoom, in WebKit", () => {
  let browser: Browser;
  const drawn = new Map<number, Drawn>();

  beforeAll(async () => {
    const script = await bundlePage(path.join(here, "feed-zoom-page.ts"), "FeedZoom");
    let page: Page;
    ({ browser, page } = await openFeedPage({ width: 900, height: 700 }));
    const errors: string[] = [];
    page.on("pageerror", (err) => errors.push(err.message));
    await page.addScriptTag({ content: script });
    await page.evaluate(
      ([prose, tree, short, shortTree]) => {
        const zoom = (window as unknown as { FeedZoom: typeof import("./feed-zoom-page") }).FeedZoom;
        zoom.drawResponse("prose", prose);
        zoom.drawResponse("tree", tree);
        zoom.drawResponse("short", short);
        zoom.drawResponse("short-tree", shortTree);
        const card = document.createElement("article");
        card.className = "feed-item";
        card.id = "card";
        card.innerHTML = '<div class="tool-card"><div class="tool-head">Bash</div></div>';
        document.getElementById("feed")?.append(card);
      },
      [PROSE, TREE, SHORT, SHORT_TREE],
    );
    for (const scale of SCALES) drawn.set(scale, await drawnAt(page, scale));
    if (errors.length > 0) throw new Error(`the page failed: ${errors.join("; ")}`);
  });

  afterAll(async () => {
    await browser.close();
  });

  /** What SCALE drew, which the beforeAll measured. */
  function at(scale: number): Drawn {
    const d = drawn.get(scale);
    if (d === undefined) throw new Error(`no measurement at ${scale}`);
    return d;
  }

  /** Every measured box, by name. */
  const BOXES = ["prose", "tree", "short", "shortTree", "card"] as const;

  it.each(BOXES.flatMap((box) => [0.7, 1.6].map((scale) => [box, scale] as const)))(
    "leaves the %s box at its 100%% edges at %sx",
    (box, scale) => {
      expect(at(scale).edges[box]).toEqual(at(1).edges[box]);
    },
  );

  it("re-wraps a tree into fewer lines when the text shrinks", () => {
    expect(at(0.7).treeLines).toBeLessThan(at(1).treeLines);
  });

  it("re-wraps a tree into more lines when the text grows", () => {
    expect(at(1.6).treeLines).toBeGreaterThan(at(1).treeLines);
  });

  it.each(SCALES)("leaves no tree line for the engine to break at %sx", (scale) => {
    expect(at(scale).brokenTreeLines).toBe(0);
  });
});
