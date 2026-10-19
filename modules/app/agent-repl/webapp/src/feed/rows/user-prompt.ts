/**
 * user-prompt — the prompt bubble: what a person typed, or (on a subagent's
 * feed) the commission the bubble was given.
 *
 * THE LOOK IS THE EXISTING ONE, unchanged: the right-flushed `.bubble.user`
 * with its body in `.bubble-body`, and the page-global shadow wave crossing its
 * background. It folds with no line cap (`USER_PROMPT_CAP_LINES`): folded, it
 * shows the whole prompt and hides only a reply's quote.
 *
 * ONE ARM ON PURPOSE. `FeedUserPrompt.result` has a single `success` arm
 * because a user row is never in flight and never fails — but it is still a
 * oneof, so an unset one is a malformed view here rather than a bubble drawn
 * with no body.
 */
import { log } from "../../log.js";
import { promptWaves } from "../../breathing.js";
import { USER_PROMPT_CAP_LINES, drawBubble } from "../../bubble/draw.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedUserPrompt,
  FeedUserPromptBlock,
  FeedUserPromptBody,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { SlotMark } from "../../bubble/body.js";
import { drawPromptBlockArm } from "./blocks.js";
import { markKnownCommands } from "./known-commands.js";

/** The path prefix every refusal from this file is reported under. */
const PATH = "FeedUserPrompt";

/**
 * The prompt bubble.
 *
 * IT WAVES EXACTLY WHILE ITS ROW SAYS ITS TURN IS WORKING AND NOT WAITING ON THE
 * USER (`msg.working`, `msg.awaitingUser`, the daemon's facts, read by
 * `promptWaves`). The one bubble stamps both halves of the signal: the mark
 * the `bubble-wave` rule keys on, drawn from that flag verbatim, and the
 * wave's PHASE as a negative inline `animation-delay` — the feed rebuilds a
 * row's body wholesale and a fresh node restarts a CSS animation at 0%, so
 * without the delay every redraw would jump the band back to the left edge.
 */
export function drawFeedUserPrompt(msg: FeedUserPrompt, previous?: HTMLElement): HTMLElement {
  log.info("drawing a user prompt row", {
    operation: "feed.draw-user-prompt",
    context: { arm: msg.result.case ?? "unset", commands: msg.commands.length },
  });
  // The author is still a required field on the wire (a message with none is
  // malformed, not merely unattributed) — validated but no longer drawn: the
  // bubble carries no "You" label or other attribution (owner ruling,
  // 2026-09-14).
  requireMessage(msg.author, `${PATH}.author`);

  const result = requireCase(msg.result, `${PATH}.result`);
  switch (result.case) {
    case "success":
      // PREVIOUS, the row's last draw, is updated in place (drawBubble), so a
      // re-push never replaces the box a reader is scrolled inside.
      return drawBubble(
        {
          role: "prompt",
          variant: "user",
          hooks: ["user"],
          waving: promptWaves(msg),
          content: drawFeedUserPromptBody(
            requireMessage(result.value.body, `${PATH}.success.body`),
            `${PATH}.success.body`,
            knownCommandsMark(msg),
          ),
          capLines: USER_PROMPT_CAP_LINES,
        },
        previous,
      ).bubble;
    default:
      return unreachableArm(`${PATH}.result`, result.case);
  }
}

/**
 * The mark a prompt's text blocks take: every slash word naming one of the
 * known commands the row lists (`msg.commands`, the daemon's), marked as the
 * composer marks it. A row listing none takes no mark.
 */
export function knownCommandsMark(msg: FeedUserPrompt): SlotMark | undefined {
  if (msg.commands.length === 0) return undefined;
  const names = new Set(msg.commands.map((command) => command.name));
  return (rendered) => markKnownCommands(rendered, names);
}

/** The blocks the person composed, in order: the bubble's content. */
export function drawFeedUserPromptBody(
  body: FeedUserPromptBody,
  path: string,
  mark?: SlotMark,
): HTMLElement[] {
  return body.blocks.map((block, index) =>
    drawFeedUserPromptBlock(block, `${path}.blocks[${index}]`, mark),
  );
}

/** One block of a user prompt — the shared drawn block vocabulary. */
export function drawFeedUserPromptBlock(
  block: FeedUserPromptBlock,
  path: string,
  mark?: SlotMark,
): HTMLElement {
  return drawPromptBlockArm(block.block, `${path}.block`, mark);
}
