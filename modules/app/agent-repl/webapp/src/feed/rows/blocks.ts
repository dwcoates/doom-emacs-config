/**
 * blocks — the feed's DRAWN BLOCK VOCABULARY: text, an image, a block the
 * schema does not model, and a reply's quote of an earlier bubble.
 *
 * feed.proto declares these three as shared messages precisely because both
 * prompt kinds — what a person typed and what one agent addressed to another —
 * carry the same content vocabulary and differ only in their author line. They
 * therefore get ONE base function each here, and each prompt message's own
 * block oneof gets its own dedicated function in that message's file, both
 * delegating to these.
 *
 * THE ARM IS THE BLOCK, and an unset one is a malformed view rather than an
 * empty paragraph: a prompt that draws nothing where the person typed something
 * is worse than a loud refusal, because the reader cannot tell it happened.
 */
import { requireCase, unreachableArm } from "../../rpc/strict.js";
import { log } from "../../log.js";
import { markdownSlot, type SlotMark } from "../../bubble/body.js";
import { BUBBLE_QUOTE_CLASS, QUOTE_MARKER_CLASS, QUOTE_PARTS_CLASS } from "../../bubble/quote.js";
import { codeBlockHtml } from "../../code-block.js";
import { armName } from "../renderers.js";
import type {
  FeedImageBlock,
  FeedQuoteBlock,
  FeedTextBlock,
  FeedUnsupportedBlock,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";

/**
 * The shape both prompt kinds' block oneofs have. They are separate generated
 * types over the SAME three payload messages, so one switch draws either; the
 * per-message wrappers are what keep the call sites named after their own
 * message.
 */
export type PromptBlockArm =
  | { case: "text"; value: FeedTextBlock }
  | { case: "image"; value: FeedImageBlock }
  | { case: "unsupported"; value: FeedUnsupportedBlock }
  | { case: "quote"; value: FeedQuoteBlock }
  | { case: undefined; value?: undefined };

/**
 * A text block: a markdown slot the bubble's body paints, so a tree the person
 * typed wraps at the prompt bubble's own cap exactly as one in a response does.
 * MARK, when the prompt kind has one, is applied to every paint of the slot
 * (a user prompt's known slash commands).
 */
export function drawFeedTextBlock(block: FeedTextBlock, mark?: SlotMark): HTMLElement {
  log.info("drawing a prompt text block", {
    operation: "feed.draw-text-block",
    context: { characters: block.text.length, marked: mark !== undefined },
  });
  return markdownSlot("prompt-block prompt-block-text", block.text, mark);
}

/**
 * An image block. The `src` is the DAEMON's resolution of the record's
 * reference into something a browser can load — this end never resolves a host
 * path itself — and the alt text is resolved too and may legitimately be empty.
 */
export function drawFeedImageBlock(block: FeedImageBlock): HTMLElement {
  log.info("drawing a prompt image block", {
    operation: "feed.draw-image-block",
    context: { has_alt: block.alt !== "" },
  });
  const el = document.createElement("img");
  el.className = "prompt-block prompt-block-image";
  el.src = block.src;
  el.alt = block.alt;
  return el;
}

/**
 * A block this schema does not model, named rather than dropped: the reader is
 * told something was in the prompt that this build cannot draw, and the KIND is
 * what tells a maintainer which block to model next.
 */
export function drawFeedUnsupportedBlock(block: FeedUnsupportedBlock): HTMLElement {
  log.warn(`a prompt carries an unsupported block kind '${block.kind}'`, {
    operation: "feed.draw-unsupported-block",
    context: { kind: block.kind },
  });
  const el = document.createElement("div");
  el.className = "prompt-block prompt-block-unsupported";
  el.textContent = `unsupported block: ${block.kind}`;
  return el;
}

/**
 * A reply's quote of an earlier bubble, from the parts the daemon took it
 * apart into (src/bubble/quote.ts): the heading, shown folded and open alike,
 * then, only while the bubble is expanded, the heading's colon, the quoted
 * text as a code block (never as markdown, so a quoted tree is the code it was
 * fenced as), and the closing marker.
 */
export function drawFeedQuoteBlock(block: FeedQuoteBlock): HTMLElement {
  log.info("drawing a prompt quote block", {
    operation: "feed.draw-quote-block",
    context: { heading: block.heading, characters: block.quoted.length },
  });
  const el = document.createElement("div");
  el.className = `prompt-block prompt-block-quote ${QUOTE_PARTS_CLASS}`;
  const colon = expandOnly("span", ":");
  const heading = document.createElement("div");
  heading.className = QUOTE_MARKER_CLASS;
  heading.append(block.heading, colon);
  const quoted = expandOnly("div", "");
  quoted.innerHTML = codeBlockHtml(block.quoted, "");
  const closing = expandOnly("div", `${block.closing}:`);
  closing.classList.add(QUOTE_MARKER_CLASS);
  el.append(heading, quoted, closing);
  return el;
}

/** A TAG holding TEXT that the collapsed bubble hides (`BUBBLE_QUOTE_CLASS`). */
function expandOnly(tag: "span" | "div", text: string): HTMLElement {
  const el = document.createElement(tag);
  el.className = BUBBLE_QUOTE_CLASS;
  el.textContent = text;
  return el;
}

/**
 * One block of either prompt kind, by arm.
 *
 * PATH names the field this block came from, so a refusal points at the
 * producer's own field rather than at "a block somewhere". MARK is the text
 * blocks' (`drawFeedTextBlock`).
 */
export function drawPromptBlockArm(arm: PromptBlockArm, path: string, mark?: SlotMark): HTMLElement {
  const block = requireCase(arm, path);
  switch (block.case) {
    case "text":
      return drawFeedTextBlock(block.value, mark);
    case "image":
      return drawFeedImageBlock(block.value);
    case "unsupported":
      return drawFeedUnsupportedBlock(block.value);
    case "quote":
      return drawFeedQuoteBlock(block.value);
    default:
      return unreachableArm(path, armName(block));
  }
}
