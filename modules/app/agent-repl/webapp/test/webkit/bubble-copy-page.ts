/**
 * THE PAGE THE WEBKIT BUBBLE-COPY TEST DRIVES (bubble-copy.webkit.test.ts):
 * real user prompt bubbles and final response bubbles, drawn by the real row
 * modules over the real stylesheet, selected or not and folded or opened, with
 * their copy control measured against the bubble's own edges and its text.
 */
import { create } from "@bufbuild/protobuf";
import { FeedResponseSchema, FeedUserPromptSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawFeedUserPrompt } from "../../src/feed/rows/user-prompt.js";
import { drawFeedResponse } from "../../src/feed/cards/response.js";
import { BUBBLE_COPY_CLASS } from "../../src/bubble/copy-room.js";
import { BUBBLE_SCROLL_CLASS } from "../../src/feed/bubble-scroll.js";
import { EXPANDED_CLASS } from "../../src/expand.js";
import { SELECTED_ENTRY_CLASS } from "../../src/feed/selected-entry.js";
import { promptProse, servedProse } from "../prose.js";
import { applyFeedTextScale } from "../../src/feed/feed-text-scale.js";
import { pagedRowContext } from "../feed/harness.js";
import { installPageLogger } from "./page-log";

installPageLogger("bubble-copy");

/** One drawn bubble's copy control, as gaps from the bubble's border box and its text. */
export interface CopyGaps {
  /** Whether the control is drawn at all: a box with size, not `display: none`. */
  shown: boolean;
  /** The control's right edge to the bubble's right edge, in CSS pixels. */
  right: number;
  /** The control's top edge to the bubble's top edge, in CSS pixels. */
  top: number;
  /** Whether the control is drawn whole inside the bubble's padding box, where nothing clips it. */
  inside: boolean;
  /** Whether every line of the text beside the control ends short of it (none beside is clear). */
  clearOfText: boolean;
  /** How many line boxes of the text sit beside the control. */
  besideLines: number;
  /** Whether the first line of text is one of the lines beside the control. */
  firstLineBeside: boolean;
  /** The control's left edge less the right edge of the text beside it, in CSS pixels (null with none beside). */
  slack: number | null;
  /** The control's height, in CSS pixels. */
  height: number;
  /** The leading the first line of text is set in (its element's computed line-height), in CSS pixels. */
  firstLineHeight: number;
}

/**
 * One prompt to draw: its blocks (text, a reply's quote, or a run of entries
 * answering a response's tree), whether the bubble is opened, whether it is
 * the selected entry, and the feed text scale it is drawn at (1 unless told).
 */
export interface PromptCase {
  blocks: readonly ({ text: string } | { quote: string } | { entries: string })[];
  expanded: boolean;
  selected: boolean;
  scale?: number;
}

/** One final response to draw: its text, and the feed text scale it is drawn at (1 unless told). */
export interface ResponseCase {
  text: string;
  scale?: number;
}

/** What the test calls, on `window.bubbleCopy`. */
export interface BubbleCopyPage {
  /** Draw CASE's prompt, and measure its copy control. */
  measurePrompt(c: PromptCase): CopyGaps;
  /** Draw CASE's final response, and measure its copy control. */
  measureResponse(c: ResponseCase): CopyGaps;
}

// The zoom lands at once here rather than gliding, so every bubble is drawn
// and measured at the scale it names.
document.documentElement.style.transition = "none";

declare global {
  interface Window {
    bubbleCopy: BubbleCopyPage;
  }
}

/** Show BUBBLE alone in the feed, and measure its copy control against it. */
function measure(bubble: HTMLElement): CopyGaps {
  const item = document.createElement("div");
  item.className = "feed-item";
  item.append(bubble);
  document.getElementById("feed")!.replaceChildren(item);
  const control = bubble.querySelector(`.${BUBBLE_COPY_CLASS}`);
  if (control === null) throw new Error("bubble-copy page: the bubble drew no copy control");
  const b = bubble.getBoundingClientRect();
  const r = control.getBoundingClientRect();
  // Each text node's own line boxes: a range over the body would also
  // return every element it holds, each a full-width block.
  const lines: DOMRect[] = [];
  const walker = document.createTreeWalker(bubble.querySelector(".bubble-body")!, NodeFilter.SHOW_TEXT);
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    const range = document.createRange();
    range.selectNodeContents(node);
    lines.push(...[...range.getClientRects()].filter((line) => line.width > 0));
  }
  // A line beside the control is one whose middle is within the control's
  // height: a glyph box is taller than its ink, so a line wrapped below the
  // control may still graze its bottom edge.
  const isBeside = (line: DOMRect): boolean => (line.top + line.bottom) / 2 < r.bottom && (line.top + line.bottom) / 2 > r.top;
  const beside = lines.filter(isBeside);
  const firstText = document.createTreeWalker(bubble.querySelector(".bubble-body")!, NodeFilter.SHOW_TEXT, {
    acceptNode: (node) => (/\S/.test(node.textContent ?? "") ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_SKIP),
  }).nextNode();
  if (firstText?.parentElement == null) throw new Error("bubble-copy page: the bubble drew no text");
  const firstTop = Math.min(...lines.map((line) => line.top));
  const first = lines.filter((line) => line.top === firstTop);
  return {
    shown: r.width > 0 && r.height > 0,
    right: Math.round((b.right - r.right) * 100) / 100,
    top: Math.round((r.top - b.top) * 100) / 100,
    inside:
      r.left >= b.left + bubble.clientLeft &&
      r.right <= b.left + bubble.clientLeft + bubble.clientWidth &&
      r.top >= b.top + bubble.clientTop &&
      r.bottom <= b.top + bubble.clientTop + bubble.clientHeight,
    clearOfText: beside.every((line) => line.right <= r.left),
    besideLines: beside.length,
    firstLineBeside: first.length > 0 && first.every(isBeside),
    slack: beside.length === 0 ? null : Math.round((r.left - Math.max(...beside.map((line) => line.right))) * 100) / 100,
    height: Math.round(r.height * 100) / 100,
    firstLineHeight: Math.round(Number.parseFloat(getComputedStyle(firstText.parentElement).lineHeight) * 100) / 100,
  };
}

window.bubbleCopy = {
  measurePrompt(c) {
    applyFeedTextScale(c.scale ?? 1);
    const msg = create(FeedUserPromptSchema, {
      author: { label: "You" },
      result: {
        case: "success",
        value: {
          body: {
            blocks: c.blocks.map((b) =>
              "text" in b
                ? { block: { case: "text", value: { prose: servedProse(b.text) } } }
                : "entries" in b
                  ? { block: { case: "text", value: { prose: promptProse(b.entries) } } }
                  : { block: { case: "quote", value: { heading: "⟢ Replying", quoted: b.quote, closing: "⟢ My message" } } },
            ) as never,
          },
        },
      },
    });
    const bubble = drawFeedUserPrompt(msg);
    if (c.selected) bubble.classList.add(SELECTED_ENTRY_CLASS);
    if (c.expanded) bubble.querySelector(`.${BUBBLE_SCROLL_CLASS}`)?.classList.add(EXPANDED_CLASS);
    return measure(bubble);
  },

  measureResponse(c) {
    applyFeedTextScale(c.scale ?? 1);
    const bubble = drawFeedResponse(
      create(FeedResponseSchema, {
        finalAnswer: true,
        usage: { text: "12.3k", atMs: 1_000n, heat: { position: 0.5 } },
        result: { case: "success", value: { prose: { prose: servedProse(c.text) } } },
      }),
      pagedRowContext(),
    );
    return measure(bubble);
  },
};
