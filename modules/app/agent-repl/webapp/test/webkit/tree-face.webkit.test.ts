/**
 * A METAPROMPT TREE IS SET IN THE BUBBLE'S FACE WITH MONOSPACE RAILS, AND
 * STILL LINES UP, IN WEBKIT (owner rulings, 2026-10-26 and 2026-10-27).
 *
 * The tree's labels and text are set in the bubble's face and its connectors
 * in the monospace rail face, so the wrap measures every piece as it draws
 * (bubble/body.ts `treeMeasure`); a proportional bubble face is held to the
 * same lines by the `proportional` sheet below. jsdom lays nothing out, so
 * this draws a real final response holding a wrapping tree over the real
 * stylesheet in headless WebKit and measures its lines: a wrap line's text
 * hangs under its head line's, every rule runs straight down from the
 * connector it continues, no line is broken again by the engine or runs past
 * the body, and the text end the renderer records is the one drawn.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import type { DrawnLine } from "./tree-face-page";
import { bundlePage } from "./bundle";
import { openFeedPage, type FeedPage } from "./feed-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A tree whose branches wrap at the bubble's cap, one of them over a child rail. */
const TREE = [
  "1 🌳 A bare Unicode tree, the shape the metaprompt answers in.",
  "├── 1.1 This branch is deliberately much longer than one hundred and five rendered columns, so the webapp must wrap it onto a continuation line beneath its own text column right here.",
  "│   └── 1.1.1 A child beneath the wrapped branch, so the rail through the wrap is load-bearing here too.",
  "└── 1.2 The last branch, also long enough to run past the limit and wrap, whose continuation carries no rail because nothing follows it once the wrap lands.",
].join("\n");

/** Within half a CSS pixel. */
const NEAR = 0.5;

/** The faces drawn: the stylesheet's own, and a proportional one put over it. */
const FACES = [
  { name: "the stylesheet's bubble face", styles: [], face: "monospace" },
  { name: "a proportional bubble face", styles: [":root { --bubble-font-family: Georgia, serif; }"], face: "Georgia" },
];

describe.each(FACES)("a metaprompt tree in $name, in WebKit", ({ styles, face }) => {
  let feed: FeedPage;
  let lines: DrawnLine[];
  let bodyRight: number;
  let bubbleFace: string;

  beforeAll(async () => {
    const script = await bundlePage(path.join(here, "tree-face-page.ts"), "treeFacePage");
    feed = await openFeedPage({ width: 700, height: 900, styles });
    await feed.page.addScriptTag({ content: script });
    ({ lines, bodyRight, bubbleFace } = await feed.page.evaluate((text) => window.treeFace.measure(text), TREE));
  });

  afterAll(async () => {
    await feed?.browser.close();
  });

  const head = (node: string): DrawnLine => {
    const line = lines.find((l) => l.node === node && l.head);
    if (line === undefined) throw new Error(`no head line draws ${node}`);
    return line;
  };
  const wraps = (node: string): DrawnLine[] => lines.filter((l) => l.node === node && !l.head);

  it("wraps both long branches", () => {
    expect([wraps("1.1").length > 0, wraps("1.2").length > 0]).toEqual([true, true]);
  });

  it("sets the bubble and the tree's text in the bubble face", () => {
    expect([bubbleFace, ...new Set(lines.map((l) => l.textFace))]).toEqual([expect.stringContaining(face), expect.stringContaining(face)]);
  });

  it("sets the rails in the monospace rail face", () => {
    expect([...new Set(lines.map((l) => l.railsFace))]).toEqual([expect.stringContaining("monospace")]);
  });

  it.each(["1.1", "1.2"])("hangs %s's wrap lines under its own text", (node) => {
    const left = head(node).textLeft;
    expect(wraps(node).map((l) => Math.abs(l.textLeft - left) <= NEAR)).toEqual(wraps(node).map(() => true));
  });

  it("draws a wrap line's rails as wide as its head line's", () => {
    expect(lines.filter((l) => !l.head).every((l) => Math.abs(l.railsWidth - head(l.node).railsWidth) <= NEAR)).toBe(true);
  });

  it("runs 1.1's child rule straight down from 1.1.1's connector", () => {
    // Each 1.1 wrap line draws the rail under 1 and the rule under 1.1's label.
    const stem = head("1.1.1").stem;
    expect(wraps("1.1").map((l) => l.rules.some((x) => stem !== null && Math.abs(x - stem) <= NEAR))).toEqual(wraps("1.1").map(() => true));
  });

  it("runs the root's rail straight down from 1.1's connector through every line until 1.2", () => {
    const stem = head("1.1").stem;
    const between = lines.filter((l) => l.node.startsWith("1.1") && !(l.node === "1.1" && l.head));
    expect(between.map((l) => l.rules.some((x) => stem !== null && Math.abs(x - stem) <= NEAR))).toEqual(between.map(() => true));
  });

  it("lets the engine break no line again", () => {
    expect(lines.map((l) => l.textLines)).toEqual(lines.map(() => 1));
  });

  it("keeps every line inside the body", () => {
    // A line starts at its rails, so its text ends at the rails' left edge plus its text end.
    expect(lines.every((l) => l.railsLeft + l.textEnd <= bodyRight + NEAR)).toBe(true);
  });

  it("records the text end each line draws", () => {
    expect(lines.map((l) => Math.abs(l.textEnd - l.recordedEnd) <= 1)).toEqual(lines.map(() => true));
  });
});

describe("a metaprompt tree when the feed text scale moves, in WebKit", () => {
  let feed: FeedPage;
  let lines: DrawnLine[];
  let bodyRight: number;

  beforeAll(async () => {
    const script = await bundlePage(path.join(here, "tree-face-page.ts"), "treeFacePage");
    feed = await openFeedPage({ width: 700, height: 900 });
    await feed.page.addScriptTag({ content: script });
    await feed.page.evaluate((text) => window.treeFace.measure(text), TREE);
    ({ lines, bodyRight } = await feed.page.evaluate(() => window.treeFace.rescale(1.3)));
  });

  afterAll(async () => {
    await feed?.browser.close();
  });

  it("re-wraps so the engine breaks no line again", () => {
    expect(lines.map((l) => l.textLines)).toEqual(lines.map(() => 1));
  });

  it("keeps every line inside the body", () => {
    expect(lines.every((l) => l.railsLeft + l.textEnd <= bodyRight + NEAR)).toBe(true);
  });
});
