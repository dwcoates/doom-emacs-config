/**
 * user-prompt — the prompt bubble: what a person typed, or (on a subagent's
 * feed) the commission the bubble was given.
 *
 * THE LOOK IS THE EXISTING ONE, unchanged: the right-flushed `.bubble.user`
 * with its body in `.bubble-body`, the page-global shadow wave crossing its
 * background, and the shared 25-line cap the stylesheet puts on every bubble
 * body. Nothing about the message that carries prompts now suggests the bubble
 * should look different, so it does not.
 *
 * ONE ARM ON PURPOSE. `FeedUserPrompt.result` has a single `success` arm
 * because a user row is never in flight and never fails — but it is still a
 * oneof, so an unset one is a malformed view here rather than a bubble drawn
 * with no body.
 */
import { bubbleWaveStyle } from "../../breathing.js";
import { log } from "../../log.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedUserPrompt,
  FeedUserPromptAuthor,
  FeedUserPromptBlock,
  FeedUserPromptBody,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawPromptBlockArm } from "./blocks.js";

/** The path prefix every refusal from this file is reported under. */
const PATH = "FeedUserPrompt";

/**
 * The prompt bubble.
 *
 * The wave's PHASE is stamped inline, as it always has been: the feed rebuilds
 * a row's body wholesale and a fresh node restarts a CSS animation at 0%, so
 * without the negative delay every redraw would jump the wave back to the left
 * edge.
 *
 * WHETHER THE WAVE RUNS AT ALL IS NOT DECIDED HERE. A renderer is handed one
 * message and draws it; whether this prompt's turn is still in flight is a fact
 * about the FEED's rows, not about this message, so the feed marks the bubble
 * (`markWorkingPrompts` in feed-view.ts) and this stamps only the phase.
 */
export function drawFeedUserPrompt(msg: FeedUserPrompt): HTMLElement {
  log("debug", "drawing a user prompt row", {
    operation: "feed.draw-user-prompt",
    context: { arm: msg.result.case ?? "unset" },
  });
  const bubble = document.createElement("div");
  bubble.className = "bubble user";
  bubble.setAttribute("style", bubbleWaveStyle());
  bubble.append(drawFeedUserPromptAuthor(requireMessage(msg.author, `${PATH}.author`)));

  const result = requireCase(msg.result, `${PATH}.result`);
  switch (result.case) {
    case "success":
      bubble.append(
        drawFeedUserPromptBody(
          requireMessage(result.value.body, `${PATH}.success.body`),
          `${PATH}.success.body`,
        ),
      );
      return bubble;
    default:
      return unreachableArm(`${PATH}.result`, result.case);
  }
}

/**
 * The author label ("You", or the spawning agent's name under a bubble).
 *
 * Drawn as its own element rather than folded into the body, because it is the
 * bubble's own attribution and must not scroll away with the text when the
 * body hits its cap.
 */
export function drawFeedUserPromptAuthor(author: FeedUserPromptAuthor): HTMLElement {
  const el = document.createElement("span");
  el.className = "prompt-author";
  el.textContent = author.label;
  return el;
}

/**
 * The blocks the person composed, in order.
 *
 * `.bubble-body` is what the stylesheet caps at the shared 25-line budget and
 * scrolls past it, so the cap comes from reusing the existing class rather than
 * from anything measured here.
 */
export function drawFeedUserPromptBody(body: FeedUserPromptBody, path: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "bubble-body";
  body.blocks.forEach((block, index) => {
    el.append(drawFeedUserPromptBlock(block, `${path}.blocks[${index}]`));
  });
  return el;
}

/** One block of a user prompt — the shared drawn block vocabulary. */
export function drawFeedUserPromptBlock(block: FeedUserPromptBlock, path: string): HTMLElement {
  return drawPromptBlockArm(block.block, `${path}.block`);
}
