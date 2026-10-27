/**
 * THE PAGE THE WEBKIT TREE-FACE TEST DRIVES (tree-face.webkit.test.ts): a real
 * final response bubble holding a wrapping metaprompt tree, drawn by the real
 * row module over the real stylesheet, its tree lines measured.
 */
import { create } from "@bufbuild/protobuf";
import { FeedResponseSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawFeedResponse } from "../../src/feed/cards/response.js";
import { treeProse } from "../prose.js";
import { pagedRowContext } from "../feed/harness.js";
import { installPageLogger } from "./page-log";
import { applyFeedTextScale } from "../../src/feed/feed-text-scale.js";

installPageLogger("tree-face");

/** One drawn tree line, measured. */
export interface DrawnLine {
  /** The node the line draws. */
  node: string;
  /** Whether the line is its node's head line (it draws the label). */
  head: boolean;
  /** The rails' left edge and width, in CSS pixels. */
  railsLeft: number;
  railsWidth: number;
  /** The text's left edge, in CSS pixels. */
  textLeft: number;
  /** How many line boxes the text draws in: one, unless the engine broke it. */
  textLines: number;
  /** The text's right edge less the line's left edge, in CSS pixels. */
  textEnd: number;
  /** The text end the renderer recorded on the line (`data-text-end`), in CSS pixels. */
  recordedEnd: number;
  /** The x of every rule glyph (`│`) the line's rails and label pad draw, at its middle. */
  rules: number[];
  /** The x of the line's connector stem (`├` or `└`), at its middle, if it draws one. */
  stem: number | null;
  /** The computed faces of the line's text and its rails. */
  textFace: string;
  railsFace: string;
}

/** What the test calls, on `window.treeFace`. */
export interface TreeFacePage {
  /** Draw a final response saying TEXT, and measure its tree's lines and its body's right edge. */
  measure(text: string): { lines: DrawnLine[]; bodyRight: number; bubbleFace: string };
  /** Land SCALE as a feed watch's push would, let two frames run, and measure the tree drawn last again. */
  rescale(scale: number): Promise<{ lines: DrawnLine[]; bodyRight: number; bubbleFace: string }>;
}

declare global {
  interface Window {
    treeFace: TreeFacePage;
  }
}

/** The middle x of every character of EL's text that is one of GLYPHS. */
function glyphMiddles(el: Element, glyphs: string): number[] {
  const out: number[] = [];
  const walker = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    const text = node.textContent ?? "";
    for (let i = 0; i < text.length; i++) {
      if (!glyphs.includes(text[i])) continue;
      const range = document.createRange();
      range.setStart(node, i);
      range.setEnd(node, i + 1);
      const r = range.getBoundingClientRect();
      out.push((r.left + r.right) / 2);
    }
  }
  return out;
}

/** The bubble `measure` drew last. */
let drawn: HTMLElement | null = null;

window.treeFace = {
  async rescale(scale) {
    applyFeedTextScale(scale);
    for (let i = 0; i < 2; i++) await new Promise((resolve) => requestAnimationFrame(resolve));
    return measureDrawn(drawn!);
  },
  measure(text) {
    const bubble = drawFeedResponse(
      create(FeedResponseSchema, {
        finalAnswer: true,
        result: { case: "success", value: { prose: { prose: treeProse(text) } } },
      }),
      pagedRowContext(),
    );
    const item = document.createElement("div");
    item.className = "feed-item";
    item.append(bubble);
    document.getElementById("feed")!.replaceChildren(item);
    drawn = bubble;
    return measureDrawn(bubble);
  },
};

/** BUBBLE's tree lines, measured, with its body's right edge and its face. */
function measureDrawn(bubble: HTMLElement): { lines: DrawnLine[]; bodyRight: number; bubbleFace: string } {
  {
    const body = bubble.querySelector<HTMLElement>(".bubble-body")!;
    const lines = [...bubble.querySelectorAll<HTMLElement>(".mp-line[data-node]")].map((line): DrawnLine => {
      const rails = line.querySelector(".mp-connectors")!;
      const content = line.querySelector(".mp-content")!;
      const range = document.createRange();
      range.selectNodeContents(content);
      const boxes = [...range.getClientRects()].filter((r) => r.width > 0);
      const l = line.getBoundingClientRect();
      const stems = glyphMiddles(rails, "├└");
      return {
        node: line.dataset.node ?? "",
        head: line.querySelector(".mp-label") !== null,
        railsLeft: rails.getBoundingClientRect().left,
        railsWidth: rails.getBoundingClientRect().width,
        textLeft: content.getBoundingClientRect().left,
        // An inline element answers one client rect per line box it is broken across.
        textLines: content.getClientRects().length,
        textEnd: Math.max(...boxes.map((r) => r.right)) - l.left,
        recordedEnd: Number.parseFloat(line.dataset.textEnd ?? "NaN"),
        rules: glyphMiddles(line.querySelector(".mp-prefix")!, "│"),
        stem: stems.length === 0 ? null : stems[stems.length - 1],
        textFace: getComputedStyle(content).fontFamily,
        railsFace: getComputedStyle(rails).fontFamily,
      };
    });
    const b = body.getBoundingClientRect();
    return { lines, bodyRight: b.right - Number.parseFloat(getComputedStyle(body).paddingRight), bubbleFace: getComputedStyle(bubble).fontFamily };
  }
}
