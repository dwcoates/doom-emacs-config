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
import { expandedOnly } from "../../bubble/expanded-only.js";
import { requireCase, unreachableArm } from "../../rpc/strict.js";
import { log } from "../../log.js";
import { markdownSlot, type SlotMark } from "../../bubble/body.js";
import { BUBBLE_QUOTE_CLASS, QUOTE_MARKER_CLASS, QUOTE_PARTS_CLASS } from "../../bubble/quote.js";
import { codeBlockHtml } from "../../code-block.js";
import { boundCopyControl } from "../../copy-control.js";
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
 * What an image the webview cannot load is drawn as, in its place. Emacs's
 * composer says the same words: both are asserted against
 * proto/vocab/image-thumbnail.json.
 */
export const IMAGE_MISSING_TEXT = "Image no longer available";

/**
 * An image block. The `src` is the DAEMON's resolution of the record's
 * reference into something a browser can load — this end never resolves a host
 * path itself — and the alt text is resolved too and may legitimately be empty.
 *
 * It is drawn as a THUMBNAIL: every image, large or small, fills the same
 * fixed-height box (`.prompt-block-image`), so a screenshot never takes over
 * the feed and an icon is never a speck.
 *
 * AN IMAGE THAT CANNOT BE LOADED SAYS SO. A pasted image lives in its
 * workspace's worktree, and a deleted worktree takes it along; the load's own
 * failure is what reports that, whenever it happens, so the image is swapped
 * for {@link IMAGE_MISSING_TEXT} rather than left as a broken-image icon.
 */
/** The classes a drawn image and its lost-image note wear. */
export interface FeedImageClasses {
  readonly image: string;
  readonly missing: string;
}

/** The class every thumbnail the feed draws wears; a click on one opens it (feed-image.ts). */
export const FEED_IMAGE_CLASS = "prompt-block-image";

/** A thumbnail among a bubble's or a card's blocks. */
export const THUMBNAIL_CLASSES: FeedImageClasses = {
  image: `prompt-block ${FEED_IMAGE_CLASS}`,
  missing: "prompt-block prompt-block-image-missing",
};

export function drawFeedImageBlock(block: FeedImageBlock, classes: FeedImageClasses = THUMBNAIL_CLASSES): HTMLElement {
  log.info("drawing a prompt image block", {
    operation: "feed.draw-image-block",
    context: { has_alt: block.alt !== "" },
  });
  const el = document.createElement("img");
  el.className = classes.image;
  el.src = block.src;
  el.alt = block.alt;
  el.addEventListener("error", () => {
    log.info("a drawn image could not be loaded; it is drawn as no longer available", {
      operation: "feed.draw-image-block.missing",
      context: { src: block.src, alt: block.alt },
    });
    // The note takes the image's place AND any class added to it since (an
    // expanded-only mark), so it is hidden and shown exactly as the image was.
    const own = new Set(classes.image.split(" "));
    const missing = document.createElement("div");
    missing.className = classes.missing;
    for (const added of [...el.classList].filter((name) => !own.has(name))) missing.classList.add(added);
    missing.textContent = IMAGE_MISSING_TEXT;
    if (block.alt !== "") missing.title = block.alt;
    el.replaceWith(missing);
  });
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
      // A prompt's image is drawn only once its bubble is expanded.
      return expandedOnly(drawFeedImageBlock(block.value));
    case "unsupported":
      return drawFeedUnsupportedBlock(block.value);
    case "quote":
      return drawFeedQuoteBlock(block.value);
    default:
      return unreachableArm(path, armName(block));
  }
}

/**
 * One block of either prompt kind as the text its prompt's copy control
 * copies: a text block's markdown as the person wrote it, a quote as its
 * heading, quoted text and closing marker, and a block with no text as the
 * bracketed name of what it is.
 */
export function promptBlockCopyText(arm: PromptBlockArm, path: string): string {
  const block = requireCase(arm, path);
  switch (block.case) {
    case "text":
      return block.value.text;
    case "image":
      return block.value.alt === "" ? "[image]" : `[image: ${block.value.alt}]`;
    case "unsupported":
      return `[unsupported block: ${block.value.kind}]`;
    case "quote":
      return [`${block.value.heading}:`, block.value.quoted, `${block.value.closing}:`]
        .filter((part) => part !== "")
        .join("\n\n");
    default:
      return unreachableArm(path, armName(block));
  }
}

/** The class a prompt bubble's copy control wears, which places it in the bubble. */
export const PROMPT_COPY_CLASS = "prompt-copy";

/**
 * A PROMPT BUBBLE'S COPY CONTROL (owner request, 2026-10-20): the one copy
 * control (copy-control.ts), of kind `prompt`, copying the whole prompt, its
 * BLOCKS' copy text in order, a blank line apart. The bubble floats it at its
 * top right (the spec's `corner`), and the stylesheet shows it only while the
 * bubble is the selected entry. BLOCKS are either prompt kind's body blocks,
 * and PATH names their field.
 */
export function drawPromptCopyControl(blocks: readonly { block: PromptBlockArm }[], path: string): HTMLElement {
  const text = blocks.map(({ block }, index) => promptBlockCopyText(block, `${path}[${index}].block`)).join("\n\n");
  return boundCopyControl("prompt", PROMPT_COPY_CLASS, "Copy prompt", text);
}
