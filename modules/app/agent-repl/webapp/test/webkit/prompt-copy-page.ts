/**
 * THE PAGE THE WEBKIT PROMPT-COPY TEST DRIVES (prompt-copy.webkit.test.ts):
 * real user prompt bubbles, drawn by the real row module over the real
 * stylesheet, selected and folded or opened, with their copy control measured
 * against the bubble's own edges.
 */
import { create } from "@bufbuild/protobuf";
import { FeedUserPromptSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawFeedUserPrompt } from "../../src/feed/rows/user-prompt.js";
import { PROMPT_COPY_CLASS } from "../../src/feed/rows/blocks.js";
import { BUBBLE_SCROLL_CLASS } from "../../src/feed/bubble-scroll.js";
import { EXPANDED_CLASS } from "../../src/expand.js";
import { SELECTED_ENTRY_CLASS } from "../../src/feed/selected-entry.js";
import { servedProse } from "../prose.js";
import { installPageLogger } from "./page-log";

installPageLogger("prompt-copy");

/** One drawn bubble's copy control, as gaps from the bubble's border box. */
export interface CopyGaps {
  /** The control's right edge to the bubble's right edge, in CSS pixels. */
  right: number;
  /** The control's top edge to the bubble's top edge, in CSS pixels. */
  top: number;
  /** Whether the control is drawn whole inside the bubble's padding box, where nothing clips it. */
  inside: boolean;
  /** Whether every line of the prompt's text beside the control ends short of it (none beside is clear). */
  clearOfText: boolean;
  /** How many line boxes of the prompt's text sit beside the control. */
  besideLines: number;
}

/** One prompt to draw: its blocks, and whether the bubble is opened. */
export interface PromptCase {
  blocks: readonly ({ text: string } | { quote: string })[];
  expanded: boolean;
}

/** What the test calls, on `window.promptCopy`. */
export interface PromptCopyPage {
  /** Draw CASE's prompt selected, and measure its copy control. */
  measure(c: PromptCase): CopyGaps;
}

declare global {
  interface Window {
    promptCopy: PromptCopyPage;
  }
}

window.promptCopy = {
  measure(c) {
    const msg = create(FeedUserPromptSchema, {
      author: { label: "You" },
      result: {
        case: "success",
        value: {
          body: {
            blocks: c.blocks.map((b) =>
              "text" in b
                ? { block: { case: "text", value: { prose: servedProse(b.text) } } }
                : { block: { case: "quote", value: { heading: "⟢ Replying", quoted: b.quote, closing: "⟢ My message" } } },
            ) as never,
          },
        },
      },
    });
    const bubble = drawFeedUserPrompt(msg);
    bubble.classList.add(SELECTED_ENTRY_CLASS);
    if (c.expanded) bubble.querySelector(`.${BUBBLE_SCROLL_CLASS}`)?.classList.add(EXPANDED_CLASS);
    const item = document.createElement("div");
    item.className = "feed-item";
    item.append(bubble);
    document.getElementById("feed")!.replaceChildren(item);
    const control = bubble.querySelector(`.${PROMPT_COPY_CLASS}`);
    if (control === null) throw new Error("prompt-copy page: the prompt drew no copy control");
    const b = bubble.getBoundingClientRect();
    const r = control.getBoundingClientRect();
    // Each text node's own line boxes: a range over the body would also
    // return every element it holds, each a full-width block.
    const lines: DOMRect[] = [];
    const walker = document.createTreeWalker(bubble.querySelector(".bubble-body")!, NodeFilter.SHOW_TEXT);
    for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
      const range = document.createRange();
      range.selectNodeContents(node);
      lines.push(...range.getClientRects());
    }
    // A line beside the control is one whose middle is within the control's
    // height: a glyph box is taller than its ink, so a line wrapped below the
    // control may still graze its bottom edge.
    const beside = lines.filter((line) => line.width > 0 && (line.top + line.bottom) / 2 < r.bottom && (line.top + line.bottom) / 2 > r.top);
    return {
      right: Math.round((b.right - r.right) * 100) / 100,
      top: Math.round((r.top - b.top) * 100) / 100,
      inside:
        r.left >= b.left + bubble.clientLeft &&
        r.right <= b.left + bubble.clientLeft + bubble.clientWidth &&
        r.top >= b.top + bubble.clientTop &&
        r.bottom <= b.top + bubble.clientTop + bubble.clientHeight,
      clearOfText: beside.every((line) => line.right <= r.left),
      besideLines: beside.length,
    };
  },
};
