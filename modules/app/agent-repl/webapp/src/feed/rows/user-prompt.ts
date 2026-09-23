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
import { log } from "../../log.js";
import { drawBubble } from "../../bubble/draw.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedUserPrompt,
  FeedUserPromptBlock,
  FeedUserPromptBody,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawPromptBlockArm } from "./blocks.js";

/** The path prefix every refusal from this file is reported under. */
const PATH = "FeedUserPrompt";

/**
 * The prompt bubble.
 *
 * IT WAVES EXACTLY WHILE ITS ROW SAYS ITS TURN IS WORKING (`msg.working`, the
 * daemon's fact). The one bubble stamps both halves of the signal: the mark
 * the `bubble-wave` rule keys on, drawn from that flag verbatim, and the
 * wave's PHASE as a negative inline `animation-delay` — the feed rebuilds a
 * row's body wholesale and a fresh node restarts a CSS animation at 0%, so
 * without the delay every redraw would jump the band back to the left edge.
 */
export function drawFeedUserPrompt(msg: FeedUserPrompt): HTMLElement {
  log.info("drawing a user prompt row", {
    operation: "feed.draw-user-prompt",
    context: { arm: msg.result.case ?? "unset" },
  });
  // The author is still a required field on the wire (a message with none is
  // malformed, not merely unattributed) — validated but no longer drawn: the
  // bubble carries no "You" label or other attribution (owner ruling,
  // 2026-09-14).
  requireMessage(msg.author, `${PATH}.author`);

  const result = requireCase(msg.result, `${PATH}.result`);
  switch (result.case) {
    case "success":
      return drawBubble({
        role: "prompt",
        variant: "user",
        hooks: ["user"],
        working: msg.working,
        content: drawFeedUserPromptBody(
          requireMessage(result.value.body, `${PATH}.success.body`),
          `${PATH}.success.body`,
        ),
        capLines: "feed",
      }).bubble;
    default:
      return unreachableArm(`${PATH}.result`, result.case);
  }
}

/** The blocks the person composed, in order: the bubble's content. */
export function drawFeedUserPromptBody(body: FeedUserPromptBody, path: string): HTMLElement[] {
  return body.blocks.map((block, index) =>
    drawFeedUserPromptBlock(block, `${path}.blocks[${index}]`),
  );
}

/** One block of a user prompt — the shared drawn block vocabulary. */
export function drawFeedUserPromptBlock(block: FeedUserPromptBlock, path: string): HTMLElement {
  return drawPromptBlockArm(block.block, `${path}.block`);
}
